;;; agent-shell-cockpit-safety-test.el --- Native and filesystem regressions -*- lexical-binding: t; -*-
;; These tests use real native state and disposable repositories, no agents.
(require 'ert)
(require 'cl-lib)
(require 'agent-shell-cockpit)
(setq agent-shell-cockpit-refresh-interval nil
      agent-shell-inhibit-system-sleep nil)
(defun cockpit-safety-git (dir &rest args)
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (apply #'process-file "git" nil t nil args)) (error "%s" (buffer-string)))
      (string-trim (buffer-string)))))
(defmacro cockpit-safety-root (&rest body)
  (declare (indent 0))
  `(let* ((root (make-temp-file "cockpit-cockpit-safety-fixture-" t))
          (agent-shell-cockpit-workspace-directory (expand-file-name "workspaces" root))
          (agent-shell-cockpit-archive-directory nil)
          (workspace (agent-shell-cockpit-workspace-create :name "sample")))
     (unwind-protect (progn ,@body) (delete-directory root t))))
(defmacro cockpit-safety-tree (&rest body)
  (declare (indent 0))
  `(cockpit-safety-root
    (let* ((source (expand-file-name "source" root)) repository worktree)
      (make-directory source)
      (cockpit-safety-git source "init" "-q")
      (cockpit-safety-git source "config" "user.email" "review@example.invalid")
      (cockpit-safety-git source "config" "user.name" "Review fixture")
      (with-temp-file (expand-file-name "file" source) (insert "initial"))
      (cockpit-safety-git source "add" ".") (cockpit-safety-git source "commit" "-qm" "initial")
      (setq repository (agent-shell-cockpit-git-add-worktree :workspace workspace :source source :name "tree" :mode 'detached :ref "HEAD")
            worktree (agent-shell-cockpit-workspace-repository-path workspace repository))
      ,@body)))
(defun cockpit-safety-agent (directory)
  (let ((buffer (generate-new-buffer " *cockpit-safety-native*")))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode default-directory directory)
      (setq-local shell-maker--config 'fixture)
      (setq-local agent-shell--state
                  (agent-shell--make-state :buffer buffer :agent-config '((:identifier . fixture))))
      (setf (map-elt (map-elt agent-shell--state :session) :id) "saved"
            (map-elt (map-elt agent-shell--state :session) :model-id) "before")
      ;; Same hook setup as native agent-shell's mode initialization.
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (add-hook 'change-major-mode-hook #'agent-shell--clean-up nil t))
    buffer))
(defun cockpit-safety-settings (workspace)
  (map-elt (car (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'sessions)) 'settings))
(ert-deftest cockpit-safety-rearchive-captures-current-head ()
  (cockpit-safety-tree
   (setq workspace (agent-shell-cockpit-workspace-restore (agent-shell-cockpit-workspace-archive workspace)))
   (with-temp-file (expand-file-name "new" worktree) (insert "new commit"))
   (cockpit-safety-git worktree "add" ".") (cockpit-safety-git worktree "commit" "-qm" "after restore")
   (let ((head (cockpit-safety-git worktree "rev-parse" "HEAD")))
     (setq workspace (agent-shell-cockpit-workspace-restore (agent-shell-cockpit-workspace-archive workspace)))
     (should (equal head (cockpit-safety-git worktree "rev-parse" "HEAD"))))))
(ert-deftest cockpit-safety-native-kill-saves-before-unsubscribe ()
  (cockpit-safety-root
   (let ((buffer (cockpit-safety-agent (map-elt workspace 'root))))
     (agent-shell-cockpit-session-attach buffer workspace)
     (with-current-buffer buffer
       (setf (map-elt (map-elt agent-shell--state :session) :model-id) "after")
       (should (eq (car kill-buffer-hook) #'agent-shell-cockpit-session--finish)))
     (kill-buffer buffer)
     (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "after"))))))))
(ert-deftest cockpit-safety-visit-retains-return-target ()
  (cockpit-safety-root
   (save-window-excursion
     (let ((origin (current-buffer)) (buffer (cockpit-safety-agent (map-elt workspace 'root))))
       (unwind-protect
           (progn
             (with-current-buffer buffer (setq agent-shell-cockpit-session-return-buffer origin))
             (switch-to-buffer buffer)
             (agent-shell-cockpit-session-visit buffer)
             (should (eq agent-shell-cockpit-session-return-buffer origin))
             (agent-shell-cockpit-session-return)
             (should (eq (current-buffer) origin)))
         (kill-buffer buffer))))))
(ert-deftest cockpit-safety-restore-rejects-worktrees-symlink ()
  (cockpit-safety-tree
   (let* ((archive (agent-shell-cockpit-workspace-archive workspace))
          (container (agent-shell-cockpit-workspace-worktrees-path archive))
          (outside (expand-file-name "outside" root)))
     (make-directory outside)
     (delete-directory container)
     (make-symbolic-link outside container)
     (should-error (agent-shell-cockpit-workspace-restore archive) :type 'user-error)
     (should-not (file-exists-p (expand-file-name "tree/file" outside)))
     (should (file-directory-p (map-elt archive 'root))))))
(ert-deftest cockpit-safety-archive-checks-and-retargets-visiting-context ()
  (cockpit-safety-root
   (let* ((path (expand-file-name "context/note.txt" (map-elt workspace 'root))) buffer archive)
     (with-temp-file path (insert "on disk"))
     (setq buffer (find-file-noselect path))
     (unwind-protect
         (progn
           (with-current-buffer buffer (goto-char (point-max)) (insert " unsaved"))
           (should-error (agent-shell-cockpit-workspace-archive workspace) :type 'user-error)
           (should (file-exists-p path))
           (with-current-buffer buffer (save-buffer))
           (setq archive (agent-shell-cockpit-workspace-archive workspace))
           (should (equal (buffer-local-value 'buffer-file-name buffer)
                          (expand-file-name "context/note.txt" (map-elt archive 'root)))))
       (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))))
(ert-deftest cockpit-safety-native-cleanup-hook-order-on-mode-change ()
  (cockpit-safety-root
   (let ((buffer (cockpit-safety-agent (map-elt workspace 'root))))
     (unwind-protect
         (progn
           (agent-shell-cockpit-session-attach buffer workspace)
           (with-current-buffer buffer
             (setf (map-elt (map-elt agent-shell--state :session) :model-id) "after")
             (fundamental-mode))
           (agent-shell-cockpit-session--observe buffer)
           (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "after"))))))
       (kill-buffer buffer)))))
(ert-deftest cockpit-safety-native-confirmed-setter-persists ()
  (cockpit-safety-root
   (let ((buffer (cockpit-safety-agent (map-elt workspace 'root))))
     (unwind-protect
         (with-current-buffer buffer
           (agent-shell--save-config-options :state agent-shell--state
                                             :acp-config-options '[((id . "model") (category . "model") (type . "select") (currentValue . "before"))])
           (agent-shell-cockpit-session-attach buffer workspace)
           (cl-letf (((symbol-function 'agent-shell--send-request)
                      (lambda (&rest args) (funcall (plist-get args :on-success) nil)))
                     ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore))
             (agent-shell--set-session-config-option :config-id "model" :value "after"))
           (should (equal (agent-shell-cockpit-agent-shell-settings) '(((id . "model") (value . "after")))))
           (agent-shell-cockpit-session--observe buffer)
           (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "after"))))))
       (kill-buffer buffer))
     (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "after"))))))))
(ert-deftest cockpit-safety-preserve-checks-modified-visiting-buffer ()
  (cockpit-safety-tree
   (let* ((path (expand-file-name "local.txt" worktree)) buffer)
     (with-temp-file path (insert "on disk"))
     (setq buffer (find-file-noselect path))
     (unwind-protect
         (progn
           (with-current-buffer buffer (goto-char (point-max)) (insert " unsaved"))
           (cl-letf (((symbol-function 'agent-shell-cockpit-workspace-view--workspace) (lambda () workspace))
                     ((symbol-function 'agent-shell-cockpit-workspace-view--repository) (lambda () repository))
                     ((symbol-function 'completing-read-multiple) (lambda (&rest _) '("local.txt")))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'agent-shell-cockpit-refresh) #'ignore))
             (should-error (agent-shell-cockpit-workspace-view-preserve) :type 'user-error))
           (should (file-exists-p path))
           (should (buffer-modified-p buffer))
           (should (equal (buffer-local-value 'buffer-file-name buffer) path)))
       (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))))
(ert-deftest cockpit-safety-git-stderr-is-not-dirty ()
  (cockpit-safety-root
   (let* ((bin (expand-file-name "bin" root))
          (program (expand-file-name "git" bin)))
     (make-directory bin)
     (with-temp-file program (insert "#!/bin/sh\nprintf 'warning: fixture diagnostic\\n' >&2\nexit 0\n"))
     (set-file-modes program #o700)
     (let ((exec-path (cons bin exec-path)))
       (with-temp-buffer
         (agent-shell-cockpit-git-status root)
         (let ((process (plist-get (cdar agent-shell-cockpit-git--status-cache) :process)))
           (while (process-live-p process) (accept-process-output process 0.05)))
         ;; Process exit and its sentinel are dispatched separately.  Wait for
         ;; the sentinel to replace the pending cache entry before asserting.
         (let ((deadline (+ (float-time) 1)))
           (while (and (eq (agent-shell-cockpit-git-status root) 'pending)
                       (< (float-time) deadline))
             (accept-process-output nil 0.05)))
         (should (eq (agent-shell-cockpit-git-status root) 'clean)))))))
(ert-deftest cockpit-safety-lifecycle-rejects-other-host-with-same-pid ()
  (cockpit-safety-root
   (agent-shell-cockpit-store-update (map-elt workspace 'root)
                                     (lambda (fresh)
                                       (agent-shell-cockpit-store-set fresh 'operation
                                                                      `((type . "archive") (pid . ,(emacs-pid)) (host . "other-host.invalid") (destination . "/unused")))))
   (should-error (agent-shell-cockpit-lifecycle--claim workspace "archive" "/unused") :type 'user-error)))
(ert-deftest cockpit-safety-duplicate-native-session-cannot-overwrite-record ()
  (cockpit-safety-root
   (let ((first (cockpit-safety-agent (map-elt workspace 'root)))
         (second (cockpit-safety-agent (map-elt workspace 'root))))
     (unwind-protect
         (progn
           (agent-shell-cockpit-session-attach first workspace)
           (with-current-buffer first
             (setf (map-elt (map-elt agent-shell--state :session) :model-id) "chosen")
             (agent-shell--emit-event :event 'init-finished))
           (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "chosen")))))
           (should-error (agent-shell-cockpit-session-attach second workspace) :type 'user-error)
           (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "chosen")))))
           (should (= 1 (length (agent-shell-cockpit-session-live-buffers workspace)))))
       (kill-buffer first) (kill-buffer second)))))
(ert-deftest cockpit-safety-native-pipeline-protects-until-refusal-settles ()
  (cockpit-safety-root
   (let* ((buffer (cockpit-safety-agent (map-elt workspace 'root)))
          (saved '(((id . "model") (value . "chosen")) ((id . "effort") (value . "high"))))
          (pending nil) (requests nil))
     (unwind-protect
         (with-current-buffer buffer
           (agent-shell-cockpit-store-update (map-elt workspace 'root)
                                             (lambda (fresh) (agent-shell-cockpit-store-set fresh 'sessions
                                                                                            `(((agentId . "fixture") (sessionId . "saved") (settings . ,saved))))))
           (setf (map-elt agent-shell--state :agent-config)
                 (agent-shell-cockpit-agent-shell-resume-config '((:identifier . fixture)) saved)
                 (map-elt agent-shell--state :client)
                 '((:request-handlers . t) (:notification-handlers . t) (:error-handlers . t))
                 (map-elt agent-shell--state :initialized) t)
           (agent-shell--save-config-options :state agent-shell--state
                                             :acp-config-options '[((id . "model") (category . "model") (type . "select")
                                                                    (currentValue . "before") (options . [((value . "chosen") (name . "Chosen"))]))])
           (setq agent-shell-cockpit-session--restoring-settings t)
           (agent-shell-cockpit-session-attach buffer workspace)
           (cl-letf (((symbol-function 'shell-maker--current-request-id) (lambda () 0))
                     ((symbol-function 'agent-shell--send-request)
                      (lambda (&rest args) (push (plist-get args :request) requests) (setq pending args)))
                     ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
                     ((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore))
             (agent-shell--handle :shell-buffer buffer)
             (should (equal (cockpit-safety-settings workspace) saved))
             (funcall (plist-get pending :on-success)
                      '((configOptions . [((id . "model") (category . "model") (type . "select")
                                           (currentValue . "chosen") (options . [((value . "chosen") (name . "Chosen"))]))
                                          ((id . "effort") (type . "select") (currentValue . "low")
                                           (options . [((value . "high") (name . "High"))]))])))
             (agent-shell--emit-event :event 'session-title-changed)
             (should (equal (cockpit-safety-settings workspace) saved))
             (funcall (plist-get pending :on-failure) '((message . "fixture refusal")) nil)
             (should-not agent-shell-cockpit-session--restoring-settings)
             (should (= 2 (length requests)))
             (should (equal (cockpit-safety-settings workspace)
                            '(((id . "model") (value . "chosen")) ((id . "effort") (value . "low")))))))
       (with-current-buffer buffer (setf (map-elt agent-shell--state :client) nil))
       (kill-buffer buffer)))))
(ert-deftest cockpit-safety-native-reload-protects-snapshot-before-init ()
  (cockpit-safety-root
   (let* ((old (cockpit-safety-agent (map-elt workspace 'root))) created
          (agent-shell-prefer-viewport-interaction nil))
     (unwind-protect
         (progn
           (agent-shell-cockpit-session-attach old workspace)
           (with-current-buffer old
             (setf (map-elt (map-elt agent-shell--state :session) :model-id) "chosen")
             (agent-shell--emit-event :event 'init-finished))
           (cl-letf (((symbol-function 'agent-shell--active-requests-p) (lambda (_) nil))
                     ((symbol-function 'shell-maker-set-buffer-name) #'ignore)
                     ((symbol-function 'agent-shell--display-buffer) #'identity)
                     ((symbol-function 'agent-shell--start)
                      (lambda (&rest args)
                        (setq created (cockpit-safety-agent default-directory))
                        (with-current-buffer created
                          (setf (map-elt agent-shell--state :agent-config) (plist-get args :config))
                          (should (equal (funcall (map-elt (plist-get args :config) :default-config-options))
                                         '(("model" . "chosen")))))
                        created)))
             (agent-shell-cockpit-session-invoke old #'agent-shell-reload))
           (should (equal (cockpit-safety-settings workspace) '(((id . "model") (value . "chosen"))))))
       (when (buffer-live-p old) (kill-buffer old))
       (when (buffer-live-p created) (kill-buffer created))))))


(ert-deftest cockpit-safety-native-configuration-waits-for-confirmation ()
  (cockpit-safety-root
   (let* ((buffer (cockpit-safety-agent (map-elt workspace 'root)))
          (agent-shell-cockpit-agent--action-buffer buffer)
          pending requests)
     (unwind-protect
         (with-current-buffer buffer
           (agent-shell--save-config-options
            :state agent-shell--state
            :acp-config-options
            '[((id . "model") (category . "model") (name . "Model") (type . "select")
               (currentValue . "before")
               (options . [((value . "before") (name . "Before"))
                           ((value . "chosen") (name . "Chosen"))]))])
           (agent-shell-cockpit-session-attach buffer workspace)
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (_prompt choices &rest _)
                        (car (last choices))))
                     ((symbol-function 'acp-send-request)
                      (lambda (&rest args)
                        (setq pending args)
                        (push (plist-get args :request) requests)))
                     ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore))
             (should-error (agent-shell-cockpit-agent-set-model) :type 'user-error)
             (should-not requests)
             (setf (map-elt agent-shell--state :initialized) t)
             (agent-shell-cockpit-agent-set-model)
             (should (equal (map-nested-elt (car requests) '(:params configId)) "model"))
             (should (equal (map-nested-elt (car requests) '(:params value)) "chosen"))
             (should-error (agent-shell-cockpit-agent-set-thought) :type 'user-error)
             (should (equal (cockpit-safety-settings workspace)
                            '(((id . "model") (value . "before")))))
             (funcall (plist-get pending :on-success)
                      '((configOptions .
                                       [((id . "model") (category . "model") (name . "Model") (type . "select")
                                         (currentValue . "chosen"))
                                        ((id . "effort") (category . "thought_level") (name . "Reasoning") (type . "select")
                                         (currentValue . "low")
                                         (options . [((value . "low") (name . "Low"))
                                                     ((value . "high") (name . "High"))]))])))
             (should (equal (cdr (assoc 'value (car (cockpit-safety-settings workspace)))) "chosen"))
             (agent-shell-cockpit-agent-set-thought)
             (funcall (plist-get pending :on-failure) '((message . "refused")) nil)
             (should (equal (map-elt (cadr (cockpit-safety-settings workspace)) 'value) "low"))
             (agent-shell-cockpit-agent-set-thought)
             (funcall (plist-get pending :on-success) nil)
             (should (equal (map-elt (cadr (cockpit-safety-settings workspace)) 'value) "high"))
             (should (= (length requests) 3))
             (should (seq-every-p (lambda (request)
                                    (equal (map-elt request :method) "session/set_config_option"))
                                  requests))))
       (kill-buffer buffer)))))

(ert-deftest cockpit-safety-late-duplicate-detaches-without-saving ()
  (cockpit-safety-root
   (let ((first (cockpit-safety-agent (map-elt workspace 'root)))
         (second (cockpit-safety-agent (map-elt workspace 'root))))
     (unwind-protect
         (progn
           (dolist (buffer (list first second))
             (with-current-buffer buffer
               (setf (map-elt (map-elt agent-shell--state :session) :id) nil))
             (agent-shell-cockpit-session-attach buffer workspace))
           (with-current-buffer first
             (setf (map-elt (map-elt agent-shell--state :session) :id) "saved"
                   (map-elt (map-elt agent-shell--state :session) :model-id) "chosen")
             (agent-shell--emit-event :event 'init-session))
           (with-current-buffer second
             (setf (map-elt (map-elt agent-shell--state :session) :id) "saved")
             (agent-shell--emit-event :event 'init-session)
             (should-not agent-shell-cockpit-session-workspace-root)
             (should-not agent-shell-cockpit-session--settings-timer)
             (should-not agent-shell-cockpit-session--subscription))
           (should (equal (cockpit-safety-settings workspace)
                          '(((id . "model") (value . "chosen"))))))
       (kill-buffer first) (kill-buffer second)))))

(ert-deftest cockpit-safety-cleanup-releases-observer-after-save-error ()
  (cockpit-safety-root
   (let ((buffer (cockpit-safety-agent (map-elt workspace 'root))) timer)
     (agent-shell-cockpit-session-attach buffer workspace)
     (setq timer (buffer-local-value 'agent-shell-cockpit-session--settings-timer buffer))
     (cl-letf (((symbol-function 'agent-shell-cockpit-session--upsert-current)
                (lambda () (error "Fixture disk failure"))))
       (kill-buffer buffer))
     (should-not (memq timer timer-list)))))

(ert-deftest cockpit-safety-stale-removal-uses-current-source ()
  (cockpit-safety-tree
   (agent-shell-cockpit-store-update
    (map-elt workspace 'root)
    (lambda (fresh) (agent-shell-cockpit-store-set (car (map-elt fresh 'worktrees)) 'source temporary-file-directory)))
   (should-error (agent-shell-cockpit-git-remove-worktree workspace repository) :type 'user-error)
   (should (file-exists-p (expand-file-name "file" worktree)))))

(ert-deftest cockpit-safety-archive-delete-refuses-pending-operation ()
  (cockpit-safety-root
   (let ((archive (agent-shell-cockpit-workspace-archive workspace)))
     (agent-shell-cockpit-lifecycle--claim archive "restore" (map-elt workspace 'root))
     (should-error (agent-shell-cockpit-workspace-delete-archive archive) :type 'user-error)
     (should (file-directory-p (map-elt archive 'root))))))



(ert-deftest cockpit-safety-removal-recovers-after-final-write-failure ()
  (cockpit-safety-tree
   (let ((write (symbol-function 'agent-shell-cockpit-store-write)) (count 0))
     (cl-letf (((symbol-function 'agent-shell-cockpit-store-write)
                (lambda (&rest args)
                  (if (= (cl-incf count) 2) (error "Fixture final write failure")
                    (apply write args)))))
       (should-error (agent-shell-cockpit-git-remove-worktree workspace repository)))
     (should-not (file-exists-p worktree))
     (let ((entry (car (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'worktrees))))
       (should (equal (map-elt entry 'removed) "pending"))
       (should (equal (map-elt entry 'head)
                      (cockpit-safety-git source "rev-parse" (map-elt entry 'retention)))))
     (agent-shell-cockpit-git-remove-worktree workspace repository)
     (should-not (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'worktrees)))))

(ert-deftest cockpit-safety-add-write-failure-leaves-managed-worktree ()
  (cockpit-safety-root
   (let ((source (expand-file-name "source" root)))
     (make-directory source)
     (cockpit-safety-git source "init" "-q")
     (cockpit-safety-git source "config" "user.email" "test@example.invalid")
     (cockpit-safety-git source "config" "user.name" "Test")
     (cockpit-safety-git source "commit" "--allow-empty" "-qm" "Initial")
     (cl-letf (((symbol-function 'agent-shell-cockpit-store-write)
                (lambda (&rest _) (error "Fixture write failure"))))
       (should-error (agent-shell-cockpit-git-add-worktree
                      :workspace workspace :source source :name "tree" :mode 'detached)))
     (let ((repository (car (agent-shell-cockpit-workspace-active-worktrees workspace))))
       (should (equal (map-elt repository 'name) "tree"))
       (let ((retained (agent-shell-cockpit-git-remove-worktree workspace repository)))
         (should (map-elt retained 'retention))
         (should-not (file-exists-p (agent-shell-cockpit-workspace-repository-path workspace repository)))
         (should-not (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'worktrees)))))))

(ert-deftest cockpit-safety-git-timeout-releases-stderr-and-timer ()
  (cockpit-safety-root
   (let* ((bin (expand-file-name "bin" root)) (program (expand-file-name "git" bin)))
     (make-directory bin)
     (with-temp-file program (insert "#!/bin/sh\nexec sleep 30\n"))
     (set-file-modes program #o700)
     (let ((exec-path (cons bin exec-path)))
       (with-temp-buffer
         (agent-shell-cockpit-git-status root)
         (let* ((entry (car agent-shell-cockpit-git--status-cache))
                (process (plist-get (cdr entry) :process))
                (stderr (plist-get (cdr entry) :stderr))
                (timer (plist-get (cdr entry) :timer)))
           ;; Fire the real timeout callback without spending ten seconds idle.
           (funcall (timer--function timer))
           (accept-process-output process 0.05)
           (should-not (process-live-p process))
           (should-not (process-live-p stderr))
           (should-not (memq timer timer-list))
           (should (eq (agent-shell-cockpit-git-status root) 'unknown))))))))

(ert-deftest cockpit-safety-readiness-does-not-evaluate-launch-defaults ()
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:initialized . t) (:session . ((:id . "saved")))
                  (:agent-config . ((:default-model-id . ,(lambda () (error "Must not run")))))
                  (:active-requests . nil)))
    (should (agent-shell-cockpit-agent-shell-ready-p))
    (setf (map-elt agent-shell--state :active-requests)
          '(((:method . "session/set_config_option"))))
    (should-not (agent-shell-cockpit-agent-shell-ready-p))))

;;; agent-shell-cockpit-safety-test.el ends here
