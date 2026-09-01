;;; agent-shell-cockpit-workspace-view.el --- Cockpit workspace detail -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render and operate a single cockpit workspace.

;;; Code:

(require 'dired)
(require 'map)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(defvar agent-shell-cockpit--buffer)
(declare-function agent-shell-cockpit "agent-shell-cockpit-dashboard")

(defvar-local agent-shell-cockpit-workspace-view--root nil
  "Workspace root displayed in the current detail buffer.")

(defun agent-shell-cockpit-workspace-view--workspace ()
  "Return the workspace displayed in the current detail buffer."
  (condition-case nil
      (agent-shell-cockpit-store-read agent-shell-cockpit-workspace-view--root)
    (error (user-error "Workspace no longer exists"))))

(defun agent-shell-cockpit-workspace-view--insert-row
    (label details type object &optional status)
  "Insert row LABEL and DETAILS representing TYPE and OBJECT.
When STATUS is non-nil, prefix the row with an agent status badge."
  (let ((start (point)))
    (insert "  ")
    (when status
      (insert (agent-shell-cockpit-ui-status-badge status) "  "))
    (insert (propertize label 'face 'font-lock-function-name-face)
            "\n" (make-string (if status 23 4) ?\s)
            (propertize details 'face 'agent-shell-cockpit-secondary) "\n")
    (agent-shell-cockpit-ui-add-row-properties
     start (list 'agent-shell-cockpit-object-type type
                 'agent-shell-cockpit-object object))))

(defun agent-shell-cockpit-workspace-view--live-session-for (session workspace)
  "Return live buffer matching SESSION in WORKSPACE."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (equal (agent-shell-cockpit-session--identifier)
                   (map-elt session 'agentId))
            (equal (agent-shell-cockpit-session--session-id)
                   (map-elt session 'sessionId)))))
   (agent-shell-cockpit-session-live-buffers workspace)))

(defun agent-shell-cockpit-workspace-view--render ()
  "Render the current workspace detail buffer."
  (let* ((workspace (agent-shell-cockpit-workspace-view--workspace))
         (repositories
          (agent-shell-cockpit-workspace-active-repositories workspace))
         (sessions (map-elt workspace 'sessions))
         (prompts (agent-shell-cockpit-workspace-prompt-paths workspace))
         (live-sessions
          (agent-shell-cockpit-session-live-buffers workspace))
         (history
          (seq-remove
           (lambda (session)
             (agent-shell-cockpit-workspace-view--live-session-for
              session workspace))
           sessions)))
    (erase-buffer)
    (insert "\n  "
            (propertize (map-elt workspace 'title)
                        'face 'agent-shell-cockpit-title)
            "\n  "
            (propertize
             (abbreviate-file-name (map-elt workspace 'root))
             'face 'agent-shell-cockpit-secondary)
            "\n\n")
    (agent-shell-cockpit-ui-insert-heading
     "  Agents" (+ (length live-sessions) (length history)))
    (if (or live-sessions history)
        (progn
          (dolist (live live-sessions)
            (agent-shell-cockpit-workspace-view--insert-row
             (with-current-buffer live
               (agent-shell-cockpit-session--title))
             (with-current-buffer live
               (format "%s · %s"
                       (or (agent-shell-cockpit-session--identifier) "agent")
                       (or (agent-shell-cockpit-session--session-id)
                           "starting")))
             'live-session live
             (agent-shell-cockpit-session-status live)))
          (dolist (session history)
            (agent-shell-cockpit-workspace-view--insert-row
             (or (map-elt session 'title) (map-elt session 'sessionId))
             (format "%s · %s" (map-elt session 'agentId)
                     (map-elt session 'sessionId))
             'session-history session 'history)))
      (insert (propertize "  No recorded agents. Use ca to start one.\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert "\n")
    (agent-shell-cockpit-ui-insert-heading "  Prompts" (length prompts))
    (if prompts
        (dolist (prompt prompts)
          (agent-shell-cockpit-workspace-view--insert-row
           (agent-shell-cockpit-workspace-prompt-name workspace prompt)
           prompt 'prompt prompt))
      (insert (propertize "  No prompts. Use ce to create one.\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert "\n")
    (agent-shell-cockpit-ui-insert-heading "  Repositories"
                                           (length repositories))
    (if repositories
        (dolist (repository repositories)
          (agent-shell-cockpit-workspace-view--insert-row
           (map-elt repository 'name)
           (agent-shell-cockpit-git-description
            (agent-shell-cockpit-workspace-repository-path
             workspace repository))
           'repository repository))
      (insert (propertize "  No repositories. Use cr to add one.\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert "\n  "
            (propertize
             "Evil: j/k move · C-j/C-k preview · TAB preview · RET open · ? help"
             'face 'agent-shell-cockpit-secondary)
            "\n")))

(defun agent-shell-cockpit-workspace-view-refresh ()
  "Refresh the current workspace detail view."
  (let* ((window (get-buffer-window (current-buffer) t))
         (saved-position (if (window-live-p window)
                             (window-point window)
                           (point)))
         (saved (agent-shell-cockpit-ui-capture-position saved-position))
        (inhibit-read-only t))
    (agent-shell-cockpit-workspace-view--render)
    (agent-shell-cockpit-ui-restore-position saved)
    (when (window-live-p window)
      (set-window-point window (point)))))

(defun agent-shell-cockpit-workspace-view-open ()
  "Open the workspace detail item at point."
  (let ((workspace (agent-shell-cockpit-workspace-view--workspace))
        (type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ('prompt (find-file object))
      ('repository
       (dired (agent-shell-cockpit-workspace-repository-path workspace object)))
      ('live-session (agent-shell-cockpit-ui-show-right object t))
      ('session-history
       (agent-shell-cockpit-session-resume workspace object))
      (_ (user-error "Point is not on a workspace item")))))

(defun agent-shell-cockpit-workspace-view-preview-buffer ()
  "Return a preview buffer for the workspace detail item at point."
  (let ((workspace (agent-shell-cockpit-workspace-view--workspace))
        (type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ('prompt (find-file-noselect object))
      ('repository
       (dired-noselect
        (agent-shell-cockpit-workspace-repository-path workspace object)))
      ('live-session object)
      (_ nil))))

(defun agent-shell-cockpit-workspace-view-edit-prompt ()
  "Select and edit a prompt in the displayed workspace."
  (interactive)
  (agent-shell-cockpit-workspace-edit-prompt
   (agent-shell-cockpit-workspace-view--workspace)))

(defun agent-shell-cockpit-workspace-view--source-directory ()
  "Prompt for a local source repository directory."
  (let* ((roots (seq-filter #'file-directory-p
                            (project-known-project-roots)))
         (manual "[Choose another directory]")
         (choice (completing-read "Source repository: "
                                  (cons manual roots) nil t)))
    (if (equal choice manual)
        (read-directory-name "Source repository: ")
      choice)))

(defun agent-shell-cockpit-add-worktree ()
  "Add a Git worktree using a Magit-like branch and start-point flow."
  (interactive)
  (let* ((workspace (agent-shell-cockpit-workspace-view--workspace))
         (source (agent-shell-cockpit-workspace-view--source-directory))
         (default-name
          (file-name-nondirectory (directory-file-name source)))
         (name (read-string "Worktree name: " default-name))
         (checkout-choices '(("Create new branch" . new)
                             ("Checkout existing branch" . existing)
                             ("Checkout detached ref" . detached)))
         (checkout
          (completing-read "Worktree checkout: " checkout-choices nil t nil
                           nil "Create new branch"))
         (mode (cdr (assoc checkout checkout-choices)))
         (start-points (agent-shell-cockpit-git-starting-points source))
         (default-start
          (agent-shell-cockpit-git-default-starting-point source))
         branch ref)
    (pcase mode
      ('new
       (setq branch (read-string "New branch name: "
                                 (map-elt workspace 'name))
             ref (completing-read "Start branch from: " start-points nil t
                                  nil nil default-start)))
      ('existing
       (setq branch
             (completing-read "Checkout existing branch: "
                              (agent-shell-cockpit-git-branches source) nil t)))
      ('detached
       (setq ref (completing-read "Detach at: " start-points nil t
                                  nil nil default-start))))
    (agent-shell-cockpit-git-add-worktree
     :workspace workspace :source source :name name :mode mode
     :ref ref :branch branch)
    (agent-shell-cockpit-workspace-view-refresh)))

(defun agent-shell-cockpit-remove-worktree ()
  "Remove the clean worktree at point."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'repository)
    (user-error "Point is not on a repository"))
  (let ((workspace (agent-shell-cockpit-workspace-view--workspace))
        (repository (agent-shell-cockpit-ui-object-at-point)))
    (when (yes-or-no-p (format "Remove worktree %s? "
                               (map-elt repository 'name)))
      (agent-shell-cockpit-git-remove-worktree workspace repository)
      (agent-shell-cockpit-workspace-view-refresh))))

(defun agent-shell-cockpit-workspace-view-start-agent ()
  "Start the default agent in the displayed workspace."
  (interactive)
  (agent-shell-cockpit-session-start-default
   (agent-shell-cockpit-workspace-view--workspace)))

(defun agent-shell-cockpit-workspace-view-start-agent-select ()
  "Choose and start an agent in the displayed workspace."
  (interactive)
  (agent-shell-cockpit-session-start-select
   (agent-shell-cockpit-workspace-view--workspace)))

(defun agent-shell-cockpit-resume-session ()
  "Resume the session-history row at point."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'session-history)
    (user-error "Point is not on resumable session history"))
  (agent-shell-cockpit-session-resume
   (agent-shell-cockpit-workspace-view--workspace)
   (agent-shell-cockpit-ui-object-at-point)))

(defun agent-shell-cockpit-workspace-view-archive ()
  "Archive the displayed workspace after confirmation."
  (interactive)
  (let ((workspace (agent-shell-cockpit-workspace-view--workspace)))
    (when (yes-or-no-p (format "Archive workspace %s? "
                               (map-elt workspace 'title)))
      (agent-shell-cockpit-workspace-archive workspace)
      (kill-buffer (current-buffer))
      (when (buffer-live-p agent-shell-cockpit--buffer)
        (switch-to-buffer agent-shell-cockpit--buffer)
        (agent-shell-cockpit-refresh)))))

(defun agent-shell-cockpit-workspace-view-kill-session ()
  "Kill the live session at point."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'live-session)
    (user-error "Point is not on a live session"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (when (yes-or-no-p (format "Kill agent session %s? " (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))
      (agent-shell-cockpit-workspace-view-refresh))))

(defun agent-shell-cockpit-workspace-view-back ()
  "Return to the cockpit dashboard."
  (interactive)
  (if (buffer-live-p agent-shell-cockpit--buffer)
      (switch-to-buffer agent-shell-cockpit--buffer)
    (agent-shell-cockpit)))

(defvar-keymap agent-shell-cockpit-workspace-view-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "e" #'agent-shell-cockpit-workspace-view-edit-prompt
  "+" #'agent-shell-cockpit-add-worktree
  "-" #'agent-shell-cockpit-remove-worktree
  "c" #'agent-shell-cockpit-workspace-view-start-agent
  "C" #'agent-shell-cockpit-workspace-view-start-agent-select
  "R" #'agent-shell-cockpit-resume-session
  "A" #'agent-shell-cockpit-workspace-view-archive
  "x" #'agent-shell-cockpit-workspace-view-kill-session
  "b" #'agent-shell-cockpit-workspace-view-back
  "C-c C-e" #'agent-shell-cockpit-workspace-view-edit-prompt
  "C-c C-a" #'agent-shell-cockpit-add-worktree
  "C-c C-d" #'agent-shell-cockpit-remove-worktree
  "C-c C-s" #'agent-shell-cockpit-workspace-view-start-agent
  "C-c C-c" #'agent-shell-cockpit-workspace-view-start-agent-select
  "C-c C-r" #'agent-shell-cockpit-resume-session
  "C-c C-x" #'agent-shell-cockpit-workspace-view-archive
  "C-c C-k" #'agent-shell-cockpit-workspace-view-kill-session
  "C-c C-b" #'agent-shell-cockpit-workspace-view-back)

(define-derived-mode agent-shell-cockpit-workspace-view-mode
  agent-shell-cockpit-ui-mode "Cockpit-Workspace"
  "Major mode for a cockpit workspace detail view."
  (setq-local agent-shell-cockpit-ui--refresh-function
              #'agent-shell-cockpit-workspace-view-refresh
              agent-shell-cockpit-ui--open-function
              #'agent-shell-cockpit-workspace-view-open
              agent-shell-cockpit-ui--preview-function
              #'agent-shell-cockpit-workspace-view-preview-buffer))

(defun agent-shell-cockpit-workspace-view-buffer (workspace)
  "Return a rendered detail buffer for WORKSPACE without displaying it."
  (let ((buffer (get-buffer-create
                 (format "*Cockpit: %s*" (map-elt workspace 'name)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-cockpit-workspace-view-mode)
        (agent-shell-cockpit-workspace-view-mode))
      (setq agent-shell-cockpit-workspace-view--root
            (map-elt workspace 'root))
      (agent-shell-cockpit-workspace-view-refresh))
    buffer))

(defun agent-shell-cockpit-workspace-view (workspace)
  "Open detail view for WORKSPACE."
  (switch-to-buffer
   (agent-shell-cockpit-workspace-view-buffer workspace)))

(provide 'agent-shell-cockpit-workspace-view)

;;; agent-shell-cockpit-workspace-view.el ends here
