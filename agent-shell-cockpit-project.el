;;; agent-shell-cockpit-project.el --- project.el workspace integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Treat a cockpit workspace and all of its worktrees as one project.el
;; project.

;;; Code:

(require 'project)
(require 'map)
(require 'seq)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)

(defun agent-shell-cockpit-project--workspace-root (directory)
  "Return the cockpit workspace root containing DIRECTORY."
  (when-let* ((root
              (locate-dominating-file
               directory
               (lambda (candidate)
                 (file-readable-p
                  (agent-shell-cockpit-store-metadata-path candidate))))))
    (condition-case nil
        (let ((workspace (agent-shell-cockpit-store-read root)))
          (when (equal (map-elt workspace 'state) "active")
            (map-elt workspace 'root)))
      (error nil))))

(defun agent-shell-cockpit-project-find (directory)
  "Return a cockpit project containing DIRECTORY, or nil."
  (when-let* ((root (agent-shell-cockpit-project--workspace-root directory)))
    (list 'agent-shell-cockpit root)))

(cl-defmethod project-root ((project (head agent-shell-cockpit)))
  "Return PROJECT's workspace root."
  (cadr project))

(cl-defmethod project-name ((project (head agent-shell-cockpit)))
  "Return PROJECT's workspace name."
  (file-name-nondirectory (directory-file-name (project-root project))))

(cl-defmethod project-files ((project (head agent-shell-cockpit)) &optional _dirs)
  "Return files aggregated from all active worktrees in PROJECT."
  (let ((workspace (agent-shell-cockpit-store-read (project-root project))))
    (apply #'append
           (mapcar
            (lambda (repository)
              (agent-shell-cockpit-git-files
               (agent-shell-cockpit-workspace-repository-path
                workspace repository)))
            (agent-shell-cockpit-workspace-active-repositories workspace)))))

(defun agent-shell-cockpit-project-enable ()
  "Enable cockpit workspace discovery in project.el."
  (add-hook 'project-find-functions #'agent-shell-cockpit-project-find))

(defun agent-shell-cockpit-project-disable ()
  "Disable cockpit workspace discovery in project.el."
  (remove-hook 'project-find-functions #'agent-shell-cockpit-project-find))

(provide 'agent-shell-cockpit-project)

;;; agent-shell-cockpit-project.el ends here
