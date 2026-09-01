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

(ert-deftest agent-shell-cockpit-session-returns-to-origin-buffer ()
  (let ((origin (generate-new-buffer " *cockpit origin*"))
        (agent (generate-new-buffer " *cockpit visited agent*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (agent-shell-cockpit-session-visit agent)
          (should (eq (current-buffer) agent))
          (should agent-shell-cockpit-session-mode)
          (should (eq (lookup-key agent-shell-cockpit-session-mode-map
                                  (kbd "C-c C-b"))
                      'agent-shell-cockpit-session-return))
          (agent-shell-cockpit-session-return)
          (should (eq (current-buffer) origin)))
      (dolist (buffer (list origin agent))
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
                     (alpha-position (string-match "^alpha" text))
                     (agent-position (string-match "alpha agent" text))
                     (beta-position (string-match "^Beta" text))
                     (unassigned-position
                      (string-match "unassigned" text)))
                (should alpha-position)
                (should (< agent-position unassigned-position
                           alpha-position beta-position))
                (should (= (apply #'+
                                  (mapcar
                                   (lambda (section)
                                     (length (oref section children)))
                                   (oref magit-root-section children)))
                           4))
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

(ert-deftest agent-shell-cockpit-workspace-detail-buffer-renders ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (detail (agent-shell-cockpit-workspace-view-buffer workspace)))
      (unwind-protect
          (with-current-buffer detail
            (should (derived-mode-p
                     'agent-shell-cockpit-workspace-view-mode))
            (should (equal agent-shell-cockpit-workspace-view--root
                           (map-elt workspace 'root)))
            (should (equal default-directory (map-elt workspace 'root)))
            (should (string-match-p "Agents" (buffer-string)))
            (should (string-match-p "No prompts" (buffer-string)))
            (should (string-match-p "Repositories" (buffer-string))))
        (when (buffer-live-p detail) (kill-buffer detail))))))

(ert-deftest agent-shell-cockpit-dashboard-groups-collapse-but-rows-are-flat ()
  (agent-shell-cockpit-test-with-root
    (agent-shell-cockpit-workspace-create :name "alpha" :title "Alpha")
    (with-temp-buffer
      (agent-shell-cockpit-mode)
      (agent-shell-cockpit-dashboard-refresh)
      (let* ((workspaces (cadr (oref magit-root-section children)))
             (workspace (car (oref workspaces children))))
        (should (oref workspaces content))
        (should-not (oref workspace content))
        (goto-char (oref workspaces start))
        (agent-shell-cockpit-toggle-section)
        (should (oref workspaces hidden))
        (agent-shell-cockpit-dashboard-refresh)
        (setq workspaces (cadr (oref magit-root-section children)))
        (should (oref workspaces hidden))))))

(ert-deftest agent-shell-cockpit-dashboard-workspace-summary-uses-icons ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create
                      :name "alpha" :title "Alpha")))
      (cl-letf (((symbol-function 'agent-shell-cockpit-ui-icon)
                 (lambda (kind) (format "[%s]" kind))))
        (should
         (equal (substring-no-properties
                 (agent-shell-cockpit-dashboard--workspace-summary workspace))
                "0 [agent]  0 [prompt]  0 [repository]"))))))

(ert-deftest agent-shell-cockpit-archives-open-in-history-buffer ()
  (agent-shell-cockpit-test-with-root
    (agent-shell-cockpit-workspace-archive
     (agent-shell-cockpit-workspace-create :name "alpha" :title "Alpha"))
    (with-temp-buffer
      (agent-shell-cockpit-mode)
      (agent-shell-cockpit-dashboard-refresh)
      (should-not (string-match-p "Archived workspaces" (buffer-string)))
      (agent-shell-cockpit-archive-view-mode)
      (agent-shell-cockpit-archive-view-refresh)
      (should (string-match-p "Archives in"
                              (agent-shell-cockpit-ui-header-context)))
      (should (string-match-p "alpha" (buffer-string)))
      (should (string-match-p "[[:xdigit:]]\\{8\\} \\* alpha"
                              (buffer-string)))
      (should (string-match-p "0 agents  [0-9]+ \\(second\\|minute\\|hour\\|day\\|week\\|month\\|year\\)s?"
                              (buffer-string)))
      (let ((workspace (car (oref magit-root-section children))))
        (should (eq (oref workspace kind) 'archived-workspace))
        (should-not (oref workspace content))))))

(ert-deftest agent-shell-cockpit-archive-view-permanently-deletes-workspace ()
  (agent-shell-cockpit-test-with-root
    (let* ((archived
            (agent-shell-cockpit-workspace-archive
             (agent-shell-cockpit-workspace-create
              :name "alpha" :title "Alpha")))
           (root (map-elt archived 'root)))
      (with-temp-buffer
        (agent-shell-cockpit-archive-view-mode)
        (agent-shell-cockpit-archive-view-refresh)
        (goto-char (point-min))
        (search-forward "alpha")
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (agent-shell-cockpit-archive-view-delete))
        (should-not (file-exists-p root))
        (should (string-match-p "No archived workspaces"
                                (buffer-string)))))))

(ert-deftest agent-shell-cockpit-dashboard-context-predicates-accept-agent-row ()
  (agent-shell-cockpit-test-with-root
    (let* ((agent (generate-new-buffer " *predicate agent*"))
           (agent-shell-cockpit-test--buffers (list agent)))
      (unwind-protect
          (progn
            (with-current-buffer agent
              (setq default-directory test-root))
            (with-temp-buffer
              (agent-shell-cockpit-mode)
              (agent-shell-cockpit-dashboard-refresh)
              (goto-char (point-min))
              (search-forward "predicate agent")
              (should (eq (agent-shell-cockpit-ui-object-at-point) agent))
              (should-not
               (agent-shell-cockpit-dashboard--invalid-at-point-p))
              (should
               (agent-shell-cockpit-dashboard--live-agent-at-point-p))))
        (when (buffer-live-p agent) (kill-buffer agent))))))

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
               (agents (string-match "^Agents" text))
               (prompts (string-match "^Prompts" text))
               (repositories (string-match "^Repositories" text)))
          (should (< agents prompts repositories))
          (should (string-match-p "No prompts" text))
          (should (string-match-p "Historical session" text))
          (should (string-match-p "● history" text))
          (should-not (string-match-p "● idle" text))
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
              (goto-char (point-min))
              (should (re-search-forward "● ready" nil t))
              (should (eq (get-text-property (match-beginning 0)
                                             'font-lock-face)
                          'agent-shell-cockpit-status-ready))))
        (when (buffer-live-p agent) (kill-buffer agent))))))

(ert-deftest agent-shell-cockpit-workspace-flattens-multiline-agent-title ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (agent (generate-new-buffer " *multiline agent*")))
      (unwind-protect
          (let ((agent-shell-cockpit-test--buffers (list agent)))
            (with-current-buffer agent
              (setq default-directory (map-elt workspace 'root)
                    agent-shell-cockpit-session-workspace-root
                    (map-elt workspace 'root)
                    agent-shell--state
                    '((:session . ((:title . "First line\nSecond line\nThird")))))
              (insert "Earlier output\nLatest agent answer")
              (let ((start (- (point) (length "Latest agent answer"))))
                (add-text-properties
                 start (point)
                 '(font-lock-face font-lock-keyword-face
                   keymap ignored-preview-keymap
                   agent-shell-ui-state ignored-preview-state))))
            (with-temp-buffer
              (agent-shell-cockpit-workspace-view-mode)
              (setq agent-shell-cockpit-workspace-view--root
                    (map-elt workspace 'root))
              (agent-shell-cockpit-workspace-view-refresh)
              (let ((text (buffer-string)))
                (should (string-match-p "First line Second line Third" text))
                (should-not (string-match-p "^Second line" text)))
              (let* ((agents (car (oref magit-root-section children)))
                     (section (car (oref agents children))))
                (should (oref agents content))
                (should (oref section content))
                (should-not (string-match-p "Latest agent answer"
                                            (buffer-string)))
                (magit-section-show section)
                (should (string-match-p "Preview" (buffer-string)))
                (should (string-match-p "Latest agent answer"
                                        (buffer-string)))
                (goto-char (point-min))
                (search-forward "Latest agent answer")
                (should
                 (equal (get-text-property (1- (point)) 'font-lock-face)
                        '(agent-shell-cockpit-agent-preview
                          font-lock-keyword-face)))
                (should-not (get-text-property (1- (point)) 'keymap))
                (should-not
                 (get-text-property (1- (point)) 'agent-shell-ui-state)))))
        (when (buffer-live-p agent) (kill-buffer agent))))))

(ert-deftest agent-shell-cockpit-allows-pending-permission-without-visiting-agent ()
  (let ((agent (generate-new-buffer " *cockpit permission agent*"))
        (origin (current-buffer))
        allowed)
    (unwind-protect
        (progn
          (with-current-buffer agent
            (insert "Allow (y)")
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "y")
                          (lambda () (interactive) (setq allowed t)))
              (add-text-properties
               (- (point) 2) (1- (point))
               (list 'agent-shell-permission-button t 'keymap map))))
          (agent-shell-cockpit-session-allow-once agent)
          (should allowed)
          (should (eq (current-buffer) origin)))
      (when (buffer-live-p agent) (kill-buffer agent)))))

(ert-deftest agent-shell-cockpit-refuses-allow-without-pending-permission ()
  (let ((agent (generate-new-buffer " *cockpit no permission agent*")))
    (unwind-protect
        (should-error (agent-shell-cockpit-session-allow-once agent)
                      :type 'user-error)
      (when (buffer-live-p agent) (kill-buffer agent)))))

(ert-deftest agent-shell-cockpit-prompt-expands-file-inline ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (prompt (expand-file-name
                    "prompt.org"
                    (agent-shell-cockpit-workspace-prompts-path workspace))))
      (with-temp-file prompt
        (insert "#+TITLE: Alpha\n\nInline prompt body\n"))
      (with-temp-buffer
        (agent-shell-cockpit-workspace-view-mode)
        (setq agent-shell-cockpit-workspace-view--root
              (map-elt workspace 'root))
        (agent-shell-cockpit-workspace-view-refresh)
        (let* ((prompts (cadr (oref magit-root-section children)))
               (section (car (oref prompts children))))
          (should (oref section hidden))
          (should-not (string-match-p "Inline prompt body" (buffer-string)))
          (magit-section-show section)
          (should (string-match-p "Inline prompt body" (buffer-string)))
          (should (string-match-p "File:" (buffer-string)))
          (goto-char (point-min))
          (search-forward "Inline prompt body")
          (should (eq (get-text-property (1- (point)) 'font-lock-face)
                      'agent-shell-cockpit-prompt-preview))
          (goto-char (point-min))
          (search-forward "TITLE")
          (should
           (seq-some
            (lambda (overlay)
              (let ((face (overlay-get overlay 'face)))
                (or (eq face 'org-document-info-keyword)
                    (and (listp face)
                         (memq 'org-document-info-keyword face)))))
            (overlays-at (1- (point))))))))))

(ert-deftest agent-shell-cockpit-refresh-installs-visibility-indicators ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create
                      :name "alpha" :title "Alpha")))
      (with-temp-file
          (expand-file-name
           "prompt.org"
           (agent-shell-cockpit-workspace-prompts-path workspace))
        (insert "#+TITLE: Alpha\n"))
      (with-temp-buffer
        (agent-shell-cockpit-workspace-view-mode)
        (setq agent-shell-cockpit-workspace-view--root
              (map-elt workspace 'root))
        (agent-shell-cockpit-workspace-view-refresh)
        (let* ((prompts (cadr (oref magit-root-section children)))
               (prompt (car (oref prompts children))))
          (should (oref prompts content))
          (should (oref prompt hidden))
          (should (seq-some
                   (lambda (overlay)
                     (overlay-get overlay 'magit-vis-indicator))
                   (overlays-in (oref prompt start)
                                (1+ (oref prompt content))))))))))

(ert-deftest agent-shell-cockpit-navigation-uses-magit-sections ()
  (with-temp-buffer
    (agent-shell-cockpit-ui-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section
          (agent-shell-cockpit-section 'root nil :kind 'root)
        (dotimes (index 3)
          (magit-insert-section
              (agent-shell-cockpit-section index nil
                                           :kind 'test :object index)
            (magit-insert-heading (format "row %d" index))))))
    (agent-shell-cockpit-ui-goto-first-row)
    (should (= (agent-shell-cockpit-next) 1))
    (should (= (agent-shell-cockpit-next) 2))
    (should (= (agent-shell-cockpit-previous) 1))
    (agent-shell-cockpit-first)
    (should (= (agent-shell-cockpit-ui-object-at-point) 0))
    (agent-shell-cockpit-last)
    (should (= (agent-shell-cockpit-ui-object-at-point) 2))))

(ert-deftest agent-shell-cockpit-keeps-evil-motion-keys-free ()
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map
                     agent-shell-cockpit-archive-view-mode-map))
    (dolist (key '("h" "j" "k" "l"))
      (should-not
       (memq (lookup-key map (kbd key))
             '(agent-shell-cockpit-next
               agent-shell-cockpit-previous))))))

(ert-deftest agent-shell-cockpit-binds-transient-dispatch ()
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map
                     agent-shell-cockpit-archive-view-mode-map))
    (should (eq (lookup-key map (kbd "?"))
                'agent-shell-cockpit-dispatch))))

(ert-deftest agent-shell-cockpit-installs-branded-header-line ()
  (dolist (mode '(agent-shell-cockpit-mode
                  agent-shell-cockpit-workspace-view-mode
                  agent-shell-cockpit-archive-view-mode))
    (with-temp-buffer
      (funcall mode)
      (should (equal header-line-format
                     agent-shell-cockpit-ui-header-line-format)))))

(ert-deftest agent-shell-cockpit-provides-magit-style-keymap ()
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map
                     agent-shell-cockpit-archive-view-mode-map))
    (dolist (binding '(("TAB" . agent-shell-cockpit-toggle-section)
                       ("RET" . agent-shell-cockpit-open)
                       ("?" . agent-shell-cockpit-dispatch)
                       ("g" . agent-shell-cockpit-refresh)
                       ("<down>" . agent-shell-cockpit-next)
                       ("<up>" . agent-shell-cockpit-previous)))
      (should (eq (lookup-key map (kbd (car binding))) (cdr binding)))))
  (should (eq (lookup-key agent-shell-cockpit-workspace-view-mode-map (kbd "b"))
              'agent-shell-cockpit-workspace-view-back))
  (should (eq (lookup-key agent-shell-cockpit-mode-map (kbd "l"))
              'agent-shell-cockpit-archive-dispatch))
  (should (eq (lookup-key agent-shell-cockpit-archive-view-mode-map (kbd "D"))
              'agent-shell-cockpit-archive-view-delete))
  (dolist (map (list agent-shell-cockpit-mode-map
                     agent-shell-cockpit-workspace-view-mode-map
                     agent-shell-cockpit-archive-view-mode-map))
    (dolist (key '("<backtab>" "C-c TAB" "C-<tab>" "M-<tab>"))
      (should (eq (lookup-key map (kbd key)) 'ignore)))))

(ert-deftest agent-shell-cockpit-navigation-does-not-update-other-windows ()
  (let ((agent-shell-cockpit-refresh-interval nil)
        (cockpit (generate-new-buffer " *cockpit navigation*"))
        (other (generate-new-buffer " *cockpit existing right*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) cockpit)
          (with-current-buffer cockpit
            (agent-shell-cockpit-ui-mode)
            (let ((inhibit-read-only t))
              (magit-insert-section
                  (agent-shell-cockpit-section 'root nil :kind 'root)
                (dotimes (index 2)
                  (magit-insert-section
                      (agent-shell-cockpit-section
                       index nil :kind 'test :object index)
                    (magit-insert-heading (format "row %d" index)))))))
            (agent-shell-cockpit-ui-goto-first-row))
          (let ((other-window (split-window-right)))
            (set-window-buffer other-window other)
            (select-window (get-buffer-window cockpit))
            (with-current-buffer cockpit
              (agent-shell-cockpit-ui-goto-first-row)
              (set-window-point (selected-window) (point))
              (should (= (agent-shell-cockpit-next) 1))
              (should (= (agent-shell-cockpit-previous) 0)))
            (should (eq (window-buffer other-window) other))
            (should (= (length (window-list)) 2))))
      (delete-other-windows)
      (dolist (buffer (list cockpit other))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

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
                (should (magit-section-at (window-point cockpit-window)))
                (let* ((position (window-point cockpit-window))
                       (section (magit-section-at position))
                       (start (oref section start)))
                  (should (= (- position start) 5))))))
        (when (window-live-p cockpit-window)
          (select-window cockpit-window)
          (delete-other-windows cockpit-window))
        (dolist (buffer (list cockpit other))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'agent-shell-cockpit-session-ui-test)

;;; agent-shell-cockpit-session-ui-test.el ends here
