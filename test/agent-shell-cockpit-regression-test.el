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

(ert-deftest cockpit-permission-missing-key-cannot-fall-through ()
  (with-temp-buffer
    (insert (propertize "Permission" 'agent-shell-permission-button t 'keymap (make-sparse-keymap)))
    (should-not (agent-shell-cockpit-agent-shell-permission-action-available-p (current-buffer) "y"))
    (should-error (agent-shell-cockpit-agent-shell-permission-action (current-buffer) "y") :type 'user-error)))

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
  (let ((workspace '((root . "/unused/"))) selected configured)
    (cl-letf (((symbol-function 'agent-shell-cockpit-dashboard-selected-workspace)
               (lambda () workspace))
              ((symbol-function 'agent-shell-cockpit-instructions-launch)
               (lambda (value) (setq selected value) (current-buffer)))
              ((symbol-function 'agent-shell-cockpit-agent-configure)
               (lambda (buffer) (setq configured buffer))))
      (agent-shell-cockpit-start-agent)
      (should (eq selected workspace))
      (should (eq configured (current-buffer)))
      (setq configured nil)
      (agent-shell-cockpit-start-agent-defaults)
      (should-not configured))))

(provide 'agent-shell-cockpit-regression-test)
;;; agent-shell-cockpit-regression-test.el ends here
