;;; package-smoke-child.el --- Fresh autoload entry smoke test -*- lexical-binding: t; -*-

(dolist (command '(agent-shell-cockpit agent-shell-cockpit-insert-instruction
                   agent-shell-cockpit-visit-instruction))
  (unless (autoloadp (symbol-function command))
    (error "Command did not autoload: %s" command)))
(when (featurep 'agent-shell-cockpit) (error "Package loaded before its entry point"))
(defvar agent-shell-cockpit-workspace-directory)
(defvar agent-shell-cockpit-refresh-interval)

(let* ((root (make-temp-file "cockpit-smoke-workspaces-" t))
       (agent-shell-cockpit-workspace-directory root)
       (agent-shell-cockpit-refresh-interval nil))
  (unwind-protect
      (progn
        (agent-shell-cockpit)
        (unless (derived-mode-p 'agent-shell-cockpit-mode) (error "Dashboard did not open"))
        (let ((workspace (agent-shell-cockpit-workspace-create :name "installed")))
          (agent-shell-cockpit-workspace-view workspace)
          (unless (derived-mode-p 'agent-shell-cockpit-workspace-view-mode) (error "Workspace did not open"))
          (agent-shell-cockpit-archives)
          (unless (derived-mode-p 'agent-shell-cockpit-archive-view-mode) (error "Archive did not open")))
        (princ "Installed package: dashboard, workspace and archive autoload smoke passed.\n"))
    (dolist (buffer (buffer-list))
      (when (with-current-buffer buffer (derived-mode-p 'agent-shell-cockpit-ui-mode)) (kill-buffer buffer)))
    (delete-directory root t)))

;;; package-smoke-child.el ends here
