;;; agent-shell-cockpit-regression-test.el --- Core regressions -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest cockpit-store-rejects-stale-writes ()
  (agent-shell-cockpit-test-with-root
   (let* ((first (agent-shell-cockpit-workspace-create :name "alpha"))
          (root (map-elt first 'root))
          (stale (agent-shell-cockpit-store-read root)))
     (agent-shell-cockpit-store-update root
				       (lambda (fresh) (agent-shell-cockpit-store-set fresh 'displayTitle "New title")))
     (should-error (agent-shell-cockpit-store-write stale) :type 'user-error)
     (should (equal (map-elt (agent-shell-cockpit-store-read root) 'title) "New title")))))

(ert-deftest cockpit-permission-captured-choice-rejects-replaced-request ()
  (with-temp-buffer
    (let ((map (make-sparse-keymap)) invoked)
      (define-key map (kbd "RET") (lambda () (interactive) (setq invoked t)))
      (insert (propertize "Allow" 'agent-shell-permission-button t 'keymap map))
      (let ((choice (car (agent-shell-cockpit-agent-shell-permission-choices (current-buffer)))))
        (remove-text-properties (point-min) (point-max) '(agent-shell-permission-button nil))
        (should-error (agent-shell-cockpit-agent-shell-invoke-choice (current-buffer) choice) :type 'user-error)
        (should-not invoked)))))

(ert-deftest cockpit-preview-cache-avoids-repeat-fontification ()
  (let ((path (make-temp-file "cockpit-preview-")) (count 0))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-shell-cockpit-workspace-view--fontified-file-uncached)
                     (lambda (_) (cl-incf count) "text")))
            (dotimes (_ 10) (agent-shell-cockpit-workspace-view--fontified-file path))
            (should (= count 1))
            (with-temp-file path (insert "changed"))
            (agent-shell-cockpit-workspace-view--fontified-file path)
            (should (= count 2))))
      (delete-file path))))


(ert-deftest cockpit-worktrees-directory-survives-customization-change ()
  (agent-shell-cockpit-test-with-root
   (let* ((agent-shell-cockpit-worktrees-directory-name "checkouts")
          (workspace (agent-shell-cockpit-workspace-create :name "alpha"))
          (root (map-elt workspace 'root)))
     (let ((agent-shell-cockpit-worktrees-directory-name "worktrees"))
       (should (equal (agent-shell-cockpit-workspace-worktrees-path
                       (agent-shell-cockpit-store-read root))
                      (expand-file-name "checkouts" root)))))))

(ert-deftest cockpit-workspace-launch-selects-instructions ()
  (let ((workspace '((root . "/unused/"))) selected)
    (cl-letf (((symbol-function 'agent-shell-cockpit-workspace-view--workspace)
               (lambda () workspace))
              ((symbol-function 'agent-shell-cockpit-agent-launch)
               (lambda (value) (setq selected value) (current-buffer))))
      (agent-shell-cockpit-workspace-view-start-agent)
      (should (eq selected workspace)))))

(provide 'agent-shell-cockpit-regression-test)


(ert-deftest cockpit-refresh-preserves-narrowed-section ()
  (with-temp-buffer
    (agent-shell-cockpit-ui-mode)
    (cl-labels ((render ()
                  (erase-buffer)
                  (magit-insert-section (root)
                    (dolist (name '("First" "Second" "Third"))
                      (magit-insert-section (agent-shell-cockpit-section name nil :kind 'row)
                        (magit-insert-heading name))))))
      (agent-shell-cockpit-ui-refresh-buffer #'render)
      (let ((section (cadr (oref magit-root-section children))))
        (narrow-to-region (oref section start) (oref section end)))
      (agent-shell-cockpit-ui-refresh-buffer #'render)
      (should (buffer-narrowed-p))
      (should (equal (buffer-string) "Second\n")))))

(ert-deftest cockpit-permission-choice-invokes-native-ret-and-preserves-point ()
  (with-temp-buffer
    (let ((map (make-sparse-keymap)) invoked)
      (define-key map (kbd "RET") (lambda () (interactive) (setq invoked t)))
      (insert "Before\n" (propertize "Allow" 'agent-shell-permission-button t 'keymap map))
      (goto-char (point-min))
      (let ((choice (car (agent-shell-cockpit-agent-shell-permission-choices (current-buffer)))))
        (agent-shell-cockpit-agent-shell-invoke-choice (current-buffer) choice))
      (should invoked)
      (should (= (point) (point-min))))))


(ert-deftest cockpit-metadata-validates-identities-and-lifecycle-fields ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create :name "alpha")))
      (dolist (fields
               '((sessions . (((agentId . "a") (sessionId . "s"))
                              ((agentId . "a") (sessionId . "s"))))
                 (sessions . (((agentId . "a") (sessionId . "s") (cwd . "../outside"))))
                 (sessions . (((agentId . "a") (sessionId . "s") (displayId . 42))))
                 (worktrees . (((name . "same")) ((name . "same"))))
                 (operation . ((type . "unknown") (pid . 1) (host . "host") (destination . "/tmp")))))
        (let ((copy (copy-tree workspace)))
          (agent-shell-cockpit-store-set copy (car fields) (cdr fields))
          (should-error (agent-shell-cockpit-store--validate copy (map-elt copy 'root)))))
      (agent-shell-cockpit-store-set workspace 'worktrees '(((name . "tree") (runtime-cache . "omit"))))
      (agent-shell-cockpit-store-write workspace)
      (should-not (map-elt (car (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'worktrees))
                          'runtime-cache)))))

(ert-deftest cockpit-preview-respects-suppressed-and-existing-user-windows ()
  (save-window-excursion
    (let ((agent (generate-new-buffer " *preview fixture*")))
      (unwind-protect
          (with-temp-buffer
            (agent-shell-cockpit-ui-mode)
            (cl-letf (((symbol-function 'agent-shell-cockpit-agent-live-at-point-p) (lambda () t))
                      ((symbol-function 'agent-shell-cockpit-ui-object-at-point) (lambda () agent))
                      ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
              (agent-shell-cockpit-agent-preview-update)
              (should-not agent-shell-cockpit-agent--preview-window))
            (let ((window (selected-window)))
              (set-window-buffer window agent)
              (with-current-buffer agent (insert "first\nlast\n"))
              (set-window-point window 1)
              (cl-letf (((symbol-function 'agent-shell-cockpit-agent-live-at-point-p) (lambda () t))
                        ((symbol-function 'agent-shell-cockpit-ui-object-at-point) (lambda () agent))
                        ((symbol-function 'display-buffer) (lambda (&rest _) window)))
                (agent-shell-cockpit-agent-preview-update)
                (should (= (window-point window) 1))
                (agent-shell-cockpit-agent-preview-close)
                (should (eq (window-buffer window) agent)))))
        (kill-buffer agent)))))

;;; agent-shell-cockpit-regression-test.el ends here

(ert-deftest cockpit-refresh-keeps-top-metadata-visible ()
  (save-window-excursion
    (with-temp-buffer
      (agent-shell-cockpit-ui-mode)
      (switch-to-buffer (current-buffer))
      (cl-labels ((render ()
                    (erase-buffer)
                    (magit-insert-section (root)
                      (insert "Workspace: Tetris\nRoot: /demo/tetris/\n\n")
                      (magit-insert-section (agents)
                        (magit-insert-heading "Agents")
                        (insert "An agent\n")))))
        (agent-shell-cockpit-ui-refresh-buffer #'render)
        (should (= (window-start) (point-min)))
        (goto-char (point-min)) (forward-line 1)
        (set-window-start (selected-window) (point) t)
        (agent-shell-cockpit-ui-refresh-buffer #'render)
        (should (= (window-start) (save-excursion (goto-char (point-min)) (forward-line 1) (point))))))))

(ert-deftest cockpit-metadata-writes-unicode-as-utf8 ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create :name "unicode"))
           (root (map-elt workspace 'root))
           (coding-system-for-write 'raw-text))
      (agent-shell-cockpit-store-update
       root (lambda (fresh) (agent-shell-cockpit-store-set fresh 'displayTitle "Tetris — first playable")))
      (should (equal (map-elt (agent-shell-cockpit-store-read root) 'title)
                     "Tetris — first playable")))))
