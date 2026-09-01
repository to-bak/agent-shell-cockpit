;;; agent-shell-cockpit-session-ui-test.el --- Session and UI tests -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest agent-shell-cockpit-session-attaches-and-persists-id ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (buffer (generate-new-buffer " *cockpit agent*"))
           (agent-shell-cockpit-test--buffers (list buffer)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory (map-elt workspace 'root)
                    agent-shell--state
                    '((:agent-config . ((:identifier . codex)))
                      (:session . ((:id . "session-1")
                                   (:title . "Build Alpha"))))))
            (agent-shell-cockpit-session-attach buffer workspace)
            (let* ((loaded (agent-shell-cockpit-store-read
                            (map-elt workspace 'root)))
                   (session (car (map-elt loaded 'sessions))))
              (should (equal (map-elt session 'agentId) "codex"))
              (should (equal (map-elt session 'sessionId) "session-1"))
              (should (equal (map-elt session 'title) "Build Alpha"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-shell-cockpit-session-refuses-outside-attachment ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create
                      :name "alpha" :title "Alpha"))
          (buffer (generate-new-buffer " *outside agent*")))
      (unwind-protect
          (with-current-buffer buffer
            (setq default-directory test-root)
            (should-error
             (agent-shell-cockpit-session-attach buffer workspace)
             :type 'user-error))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-shell-cockpit-session-resumes-with-stored-agent-and-id ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (session (list (cons 'agentId "codex")
                          (cons 'sessionId "session-1")
                          (cons 'title "Resume me")))
           (config '((:identifier . codex)))
           (buffer (generate-new-buffer " *resumed agent*"))
           received)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell--resolved-agent-configs)
                     (lambda () (list config)))
                    ((symbol-function 'agent-shell-start)
                     (lambda (&rest arguments)
                       (setq received arguments)
                       (with-current-buffer buffer
                         (setq default-directory (map-elt workspace 'root)
                               agent-shell--state
                               '((:agent-config . ((:identifier . codex)))
                                 (:session . ((:id . "session-1")
                                              (:title . "Resume me"))))))
                       buffer)))
            (should (eq (agent-shell-cockpit-session-resume workspace session)
                        buffer))
            (should (eq (plist-get received :config) config))
            (should (equal (plist-get received :session-id) "session-1")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-shell-cockpit-dashboard-renders-workspace-and-unassigned ()
  (agent-shell-cockpit-test-with-root
    (let* ((alpha (agent-shell-cockpit-workspace-create
                   :name "alpha" :title "Alpha"))
           (_beta (agent-shell-cockpit-workspace-create
                   :name "beta" :title "Beta"))
           (assigned (generate-new-buffer " *alpha agent*"))
           (unassigned (generate-new-buffer " *unassigned*")))
      (unwind-protect
          (let ((agent-shell-cockpit-test--buffers
                 (list assigned unassigned)))
            (with-current-buffer assigned
              (setq default-directory (map-elt alpha 'root)
                    agent-shell-cockpit-session-workspace-root
                    (map-elt alpha 'root)))
            (with-current-buffer unassigned
              (setq default-directory test-root))
            (with-temp-buffer
              (agent-shell-cockpit-mode)
              (let ((inhibit-read-only t))
                (agent-shell-cockpit-dashboard--render))
              (let* ((text (buffer-string))
                     (alpha-position (string-match "--- Alpha ---" text))
                     (agent-position (string-match "alpha agent" text))
                     (beta-position (string-match "--- Beta ---" text))
                     (unassigned-position
                      (string-match "Unassigned agents" text)))
                (should alpha-position)
                (should (< alpha-position agent-position beta-position
                           unassigned-position))
                (should (= (length
                            (agent-shell-cockpit-ui--row-positions)) 4))
                (goto-char (1+ agent-position))
                (should (eq (agent-shell-cockpit-ui-object-type-at-point)
                            'workspace-session)))))
        (dolist (buffer (list assigned unassigned))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest agent-shell-cockpit-create-workspace-prompts-only-for-name ()
  (agent-shell-cockpit-test-with-root
    (let ((read-count 0)
          opened)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (setq read-count (1+ read-count))
                   "alpha"))
                ((symbol-function 'agent-shell-cockpit-dashboard-refresh)
                 #'ignore)
                ((symbol-function 'dired-other-window)
                 (lambda (directory) (setq opened directory))))
        (agent-shell-cockpit-create-workspace))
      (should (= read-count 1))
      (should (equal opened
                     (expand-file-name "alpha/prompts/"
                                       agent-shell-cockpit-workspace-directory)))
      (should (equal
               (map-elt (agent-shell-cockpit-store-read
                         (expand-file-name
                          "alpha" agent-shell-cockpit-workspace-directory))
                        'title)
               "alpha")))))

(ert-deftest agent-shell-cockpit-dashboard-previews-workspace-detail ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (dashboard (generate-new-buffer " *cockpit dashboard preview*"))
           detail)
      (unwind-protect
          (with-current-buffer dashboard
            (agent-shell-cockpit-mode)
            (let ((inhibit-read-only t))
              (agent-shell-cockpit-dashboard--render))
            (goto-char (point-min))
            (search-forward "--- Alpha ---")
            (setq detail (agent-shell-cockpit-dashboard-preview-buffer))
            (should (buffer-live-p detail))
            (should (eq (current-buffer) dashboard))
            (with-current-buffer detail
              (should (derived-mode-p
                       'agent-shell-cockpit-workspace-view-mode))
              (should (equal agent-shell-cockpit-workspace-view--root
                             (map-elt workspace 'root)))
              (should (string-match-p "Agents" (buffer-string)))
              (should (string-match-p "prompt.org" (buffer-string)))
              (should (string-match-p "Repositories" (buffer-string)))))
        (when (buffer-live-p dashboard) (kill-buffer dashboard))
        (when (buffer-live-p detail) (kill-buffer detail))))))

(ert-deftest agent-shell-cockpit-workspace-detail-renders-repositories-and-history ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create
                      :name "alpha" :title "Alpha")))
      (agent-shell-cockpit-store-set
       workspace 'sessions
       (list (list (cons 'agentId "codex")
                   (cons 'sessionId "session-1")
                   (cons 'title "Historical session"))))
      (agent-shell-cockpit-store-write workspace)
      (with-temp-buffer
        (agent-shell-cockpit-workspace-view-mode)
        (setq agent-shell-cockpit-workspace-view--root
              (map-elt workspace 'root))
        (let ((inhibit-read-only t))
          (agent-shell-cockpit-workspace-view--render))
        (let* ((text (buffer-string))
               (agents (string-match "  Agents" text))
               (prompts (string-match "  Prompts" text))
               (repositories (string-match "  Repositories" text)))
          (should (< agents prompts repositories))
          (should (string-match-p "prompt.org" text))
          (should (string-match-p "Historical session" text))
          (should (string-match-p "\\[HISTORY\\]" text))
          (should-not (string-match-p "\\[IDLE\\]" text))
          (should (string-match-p "No repositories" text)))))))

(ert-deftest agent-shell-cockpit-workspace-detail-renders-unrecorded-live-agent ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (agent (generate-new-buffer " *new live agent*")))
      (unwind-protect
          (let ((agent-shell-cockpit-test--buffers (list agent)))
            (with-current-buffer agent
              (setq default-directory (map-elt workspace 'root)
                    agent-shell-cockpit-session-workspace-root
                    (map-elt workspace 'root))
              (setq-local agent-shell--state nil))
            (with-temp-buffer
              (agent-shell-cockpit-workspace-view-mode)
              (setq agent-shell-cockpit-workspace-view--root
                    (map-elt workspace 'root))
              (let ((inhibit-read-only t))
                (agent-shell-cockpit-workspace-view--render))
              (should (string-match-p "new live agent" (buffer-string)))
              (should (string-match-p "\\[READY\\]" (buffer-string)))))
        (when (buffer-live-p agent) (kill-buffer agent))))))

(ert-deftest agent-shell-cockpit-navigation-wraps ()
  (with-temp-buffer
    (agent-shell-cockpit-ui-mode)
    (let ((inhibit-read-only t))
      (dotimes (index 3)
        (let ((start (point)))
          (insert (format "row %d\n" index))
          (agent-shell-cockpit-ui-add-row-properties
           start (list 'agent-shell-cockpit-object index)))))
    (agent-shell-cockpit-ui-goto-first-row)
    (should (= (agent-shell-cockpit-next) 1))
    (should (= (agent-shell-cockpit-next) 2))
    (should (= (agent-shell-cockpit-next) 0))
    (should (= (agent-shell-cockpit-previous) 2))
    (agent-shell-cockpit-first)
    (should (= (agent-shell-cockpit-ui-object-at-point) 0))
    (agent-shell-cockpit-last)
    (should (= (agent-shell-cockpit-ui-object-at-point) 2))))

(ert-deftest agent-shell-cockpit-keeps-evil-motion-keys-free ()
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map))
    (dolist (key '("h" "j" "k" "l"))
      (should-not
       (memq (lookup-key map (kbd key))
             '(agent-shell-cockpit-next
               agent-shell-cockpit-previous
               agent-shell-cockpit-preview-next
               agent-shell-cockpit-preview-previous))))))

(ert-deftest agent-shell-cockpit-binds-context-help ()
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map))
    (should (eq (lookup-key map (kbd "?"))
                'agent-shell-cockpit-help))))

(ert-deftest agent-shell-cockpit-provides-evil-safe-action-bindings ()
  (dolist (binding '(("C-c C-n" . agent-shell-cockpit-create-workspace)
                     ("C-c C-s" . agent-shell-cockpit-start-agent)
                     ("C-c C-e" . agent-shell-cockpit-edit-prompt)
                     ("C-c C-a" . agent-shell-cockpit-attach-session)
                     ("C-c C-x" . agent-shell-cockpit-archive-workspace)))
    (should (eq (lookup-key agent-shell-cockpit-mode-map (kbd (car binding)))
                (cdr binding))))
  (dolist (binding '(("C-c C-a" . agent-shell-cockpit-add-worktree)
                     ("C-c C-d" . agent-shell-cockpit-remove-worktree)
                     ("C-c C-r" . agent-shell-cockpit-resume-session)
                     ("C-c C-b" . agent-shell-cockpit-workspace-view-back)))
    (should (eq (lookup-key agent-shell-cockpit-workspace-view-mode-map
                            (kbd (car binding)))
                (cdr binding)))))

(ert-deftest agent-shell-cockpit-refresh-restores-unselected-window-point ()
  (agent-shell-cockpit-test-with-root
    (agent-shell-cockpit-workspace-create :name "alpha" :title "Alpha")
    (let* ((agent-shell-cockpit-refresh-interval nil)
           (cockpit (generate-new-buffer " *cockpit window*"))
           (other (generate-new-buffer " *other window*"))
           (cockpit-window (selected-window)))
      (unwind-protect
          (progn
            (delete-other-windows cockpit-window)
            (set-window-buffer cockpit-window cockpit)
            (with-current-buffer cockpit
              (agent-shell-cockpit-mode)
              (agent-shell-cockpit-dashboard-refresh)
              (set-window-point cockpit-window (+ (point) 5)))
            (let ((other-window (split-window cockpit-window nil 'right)))
              (set-window-buffer other-window other)
              (select-window other-window)
              (with-current-buffer cockpit
                ;; An unselected window has a separate `window-point'.
                (goto-char (point-max))
                (agent-shell-cockpit-dashboard-refresh))
              (with-current-buffer cockpit
                (should (< (window-point cockpit-window) (point-max)))
                (should (get-text-property
                         (window-point cockpit-window)
                         'agent-shell-cockpit-row-key))
                (let* ((position (window-point cockpit-window))
                       (key (get-text-property
                             position 'agent-shell-cockpit-row-key))
                       (start (agent-shell-cockpit-ui--row-start-at
                               position key)))
                  (should (= (- position start) 5))))))
        (when (window-live-p cockpit-window)
          (select-window cockpit-window)
          (delete-other-windows cockpit-window))
        (dolist (buffer (list cockpit other))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'agent-shell-cockpit-session-ui-test)

;;; agent-shell-cockpit-session-ui-test.el ends here
