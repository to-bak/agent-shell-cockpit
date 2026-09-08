;;; agent-shell-cockpit-integration-test.el --- Real native API tests -*- lexical-binding: t; -*-

;; Intentionally does not load the unit helper that provides a fake agent-shell.
(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require 'agent-shell-cockpit)

(ert-deftest cockpit-integration-restores-options-in-native-order-with-refusals ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local shell-maker--config 'fixture)
    (setq-local agent-shell--state
                `((:buffer . ,(current-buffer))
                  (:session . ((:id . "saved")))
                  (:config-options .
                   (((:id . "model") (:type . "select") (:category . "model")
                     (:current-value . "old")
                     (:options . (((:value . "chosen") (:name . "Chosen")))))))))
    (let ((settings '(((id . "model") (value . "chosen"))
                      ((id . "effort") (value . "high"))
                      ((id . "removed-option") (value . "gone"))))
          requests bodies finished)
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment)
                 (lambda (&rest args) (push (plist-get args :body) bodies)))
                ((symbol-function 'agent-shell--request-default-config-option)
                 (lambda (&rest args)
                   (push (plist-get args :option) requests)
                   ;; This model advertises reasoning only after it is selected.
                   (when (equal (plist-get args :option) "model")
                     (push '((:id . "effort") (:type . "select")
                             (:category . "thought_level") (:current-value . "low")
                             (:options . (((:value . "high") (:name . "High")))))
                           (map-elt agent-shell--state :config-options)))
                   (funcall (plist-get args :on-success)))))
        (let ((config (agent-shell-cockpit-agent-shell-resume-config nil settings)))
          (agent-shell--set-default-config-options
           :config-options (funcall (map-elt config :default-config-options))
           :on-options-set (lambda () (setq finished t))))
        (should finished)
        (should (equal (nreverse requests) '("model" "effort")))
        (should (seq-some (lambda (body)
                            (and body (string-match-p "removed-option" body))) bodies))))))

(defun cockpit-integration-agent (directory &optional session)
  "Create a native-state fixture in DIRECTORY with SESSION, without transport."
  (let ((buffer (generate-new-buffer " *cockpit native fixture*")))
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode default-directory directory)
      (setq-local shell-maker--config 'fixture)
      (setq-local agent-shell--state
                  `((:buffer . ,buffer) (:event-subscriptions . nil)
                    (:supports-session-fork . t)
                    (:agent-config . ((:identifier . fixture)))
                    (:session . ((:id . ,(or session (buffer-name buffer))))))))
    buffer))

(ert-deftest cockpit-integration-native-reload-and-fork-retain-membership ()
  (let* ((root (make-temp-file "cockpit-native-" t))
         (agent-shell-cockpit-workspace-directory root)
         (agent-shell-cockpit-refresh-interval nil)
         (agent-shell-prefer-viewport-interaction nil)
         (workspace (agent-shell-cockpit-workspace-create :name "native"))
         (directory (expand-file-name "worktrees/subdirectory" (map-elt workspace 'root)))
         (origin (current-buffer)) old created)
    (unwind-protect
        (progn
          (make-directory directory t)
          (setq old (cockpit-integration-agent directory))
          (agent-shell-cockpit-session-attach old workspace)
          (with-current-buffer old (setq agent-shell-cockpit-session-return-buffer origin))
          (cl-letf (((symbol-function 'agent-shell--active-requests-p) (lambda (_) nil))
                    ((symbol-function 'shell-maker-set-buffer-name) #'ignore)
                    ((symbol-function 'agent-shell--display-buffer) #'identity)
                    ((symbol-function 'agent-shell--start)
                     (lambda (&rest arguments)
                       (let ((buffer (cockpit-integration-agent
                                      default-directory (plist-get arguments :session-id))))
                         (push buffer created) buffer))))
            (agent-shell-cockpit-session-invoke old #'agent-shell-reload)
            (should-not (buffer-live-p old))
            (let ((replacement (car created)))
              (should (member replacement (agent-shell-cockpit-session-live-buffers workspace)))
              (should (eq (buffer-local-value 'agent-shell-cockpit-session-return-buffer replacement) origin))
              (should (file-equal-p (buffer-local-value 'default-directory replacement) directory))
              (agent-shell-cockpit-session-invoke replacement #'agent-shell-fork)
              (should (buffer-live-p replacement))
              (should (= 2 (length (agent-shell-cockpit-session-live-buffers workspace)))))))
      (dolist (buffer (cons old created)) (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest cockpit-integration-native-standalone-cwd-and-context ()
  (let* ((directory (make-temp-file "cockpit-standalone-" t))
         (target (agent-shell-cockpit-session-target directory))
         (agent-shell-prefer-viewport-interaction nil) created text strategy)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--auto-preferred-config)
                   (lambda () '((:identifier . fixture))))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest _)
                     (setq strategy agent-shell-session-strategy
                           created (cockpit-integration-agent (agent-shell-cwd)))))
                  ((symbol-function 'agent-shell--display-and-insert-context)
                   (lambda (_buffer input) (setq text input))))
          (let ((result (agent-shell-cockpit-session-start-target target "Only this context")))
            (should (eq result created))
            (should (eq strategy 'new))
            (should (equal text "Only this context"))
            (should (file-equal-p (buffer-local-value 'default-directory created) directory))
            (should (buffer-local-value 'agent-shell-cockpit-session-standalone-p created))
            (should (memq created (agent-shell-cockpit-session-unassigned-buffers)))
            (should-not (file-exists-p (agent-shell-cockpit-store-metadata-path directory)))))
      (when (buffer-live-p created) (kill-buffer created))
      (delete-directory directory t))))

(ert-deftest cockpit-integration-resume-honors-recorded-subdirectory ()
  (let* ((root (make-temp-file "cockpit-resume-" t))
         (agent-shell-cockpit-workspace-directory root)
         (workspace (agent-shell-cockpit-workspace-create :name "native"))
         (directory (expand-file-name "context" (map-elt workspace 'root)))
         (agent-shell-agent-configs '((:identifier fixture))) created)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-start)
                   (lambda (&rest _) (setq created (cockpit-integration-agent (agent-shell-cwd))))))
          (agent-shell-cockpit-session-resume
           workspace '((agentId . "fixture") (sessionId . "saved") (cwd . "context/")))
          (should (file-equal-p (buffer-local-value 'default-directory created) directory)))
      (when (buffer-live-p created) (kill-buffer created))
      (delete-directory root t))))


(ert-deftest cockpit-integration-dashboard-start-is-always-standalone ()
  (let ((agent-shell-cockpit-dashboard--origin-directory temporary-file-directory)
        launched)
    (cl-letf (((symbol-function 'agent-shell-cockpit-agent-launch)
               (lambda (&optional workspace directory)
                 (setq launched (list workspace directory)) 'agent))
              ((symbol-function 'agent-shell-cockpit-dashboard-selected-workspace)
               (lambda () (ert-fail "Dashboard launch must not select a workspace"))))
      (agent-shell-cockpit-start-agent)
      (should (equal launched (list nil temporary-file-directory))))))

(ert-deftest cockpit-integration-workspace-start-forces-new-session ()
  (let ((agent-shell-session-strategy 'prompt) strategy)
    (cl-letf (((symbol-function 'agent-shell-cockpit-session-start)
               (lambda (&rest _) (setq strategy agent-shell-session-strategy))))
      (agent-shell-cockpit-session-start-select 'workspace)
      (should (eq strategy 'new))
      (should (eq agent-shell-session-strategy 'prompt)))))

(ert-deftest cockpit-integration-new-strategy-skips-native-history-picker ()
  ;; Exercise the native post-connection decision with an agent that supports
  ;; history.  The process boundary is replaced; session routing is real.
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local shell-maker--config 'fixture)
    (setq-local agent-shell-session-strategy 'new)
    (setq-local agent-shell--state
                (agent-shell--make-state
                 :buffer (current-buffer) :agent-config '((:identifier . fixture))))
    (setf (map-elt agent-shell--state :supports-session-list) t
          (map-elt agent-shell--state :supports-session-load) t)
    (let (created)
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                ((symbol-function 'agent-shell--initiate-session-list-and-load)
                 (lambda (&rest _) (ert-fail "New launches must not enter history selection")))
                ((symbol-function 'agent-shell--initiate-new-session)
                 (lambda (&rest _) (setq created t))))
        (agent-shell--initiate-session :shell-buffer (current-buffer)
                                      :on-session-init #'ignore)
        (should created)))))

(ert-deftest cockpit-integration-view-menus-cover-cockpit-bindings ()
  (let* ((root (make-temp-file "cockpit-menu-" t))
         (agent-shell-cockpit-workspace-directory root)
         (agent-shell-cockpit-refresh-interval nil)
         (workspace (agent-shell-cockpit-workspace-create :name "menu")))
    (unwind-protect
        (dolist (mode '(agent-shell-cockpit-mode
                        agent-shell-cockpit-workspace-view-mode
                        agent-shell-cockpit-archive-view-mode))
          (with-temp-buffer
            (funcall mode)
            (setq-local agent-shell-cockpit-workspace-view--root (map-elt workspace 'root))
            (unwind-protect
                (progn
                  (agent-shell-cockpit-dispatch)
                  (map-keymap
                   (lambda (event command)
                     (when (and (symbolp command)
                                (or (string-prefix-p "agent-shell-cockpit-" (symbol-name command))
                                    (memq command '(next-line previous-line)))
                                (not (eq command 'agent-shell-cockpit-dispatch)))
                       (ert-info ((format "%s key %s" mode (key-description (vector event))))
                         (should (eq (lookup-key transient--transient-map (vector event)) command)))))
                   (current-local-map))
                  (dolist (pair '(("<return>" . agent-shell-cockpit-open)
                                  ("<tab>" . agent-shell-cockpit-toggle-section)))
                    (should (eq (lookup-key transient--transient-map (kbd (car pair))) (cdr pair)))))
              (transient-quit-all))))
      (delete-directory root t))))

(provide 'agent-shell-cockpit-integration-test)
;;; agent-shell-cockpit-integration-test.el ends here
