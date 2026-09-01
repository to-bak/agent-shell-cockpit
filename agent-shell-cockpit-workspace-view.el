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
(require 'transient)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(defvar agent-shell-cockpit--buffer)
(declare-function agent-shell-cockpit "agent-shell-cockpit-dashboard")
(defvar-local agent-shell-cockpit-workspace-view--root nil
  "Workspace root displayed in the current detail buffer.")

(defcustom agent-shell-cockpit-workspace-sections-hook
  '(agent-shell-cockpit-workspace-insert-agents
    agent-shell-cockpit-workspace-insert-prompts
    agent-shell-cockpit-workspace-insert-repositories)
  "Hook of functions that insert workspace detail sections.
Each function receives WORKSPACE, LIVE-SESSIONS, HISTORY, PROMPTS,
and REPOSITORIES."
  :type 'hook
  :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-workspace-view--workspace ()
  "Return the workspace displayed in the current detail buffer."
  (condition-case nil
      (agent-shell-cockpit-store-read agent-shell-cockpit-workspace-view--root)
    (error (user-error "Workspace no longer exists"))))

(defun agent-shell-cockpit-workspace-view--insert-row
    (label details type object &optional status non-collapsible)
  "Insert row LABEL and DETAILS representing TYPE and OBJECT.
When STATUS is non-nil, append a colored agent status label.
When NON-COLLAPSIBLE is non-nil, omit the expandable detail body."
  (let ((identity
         (pcase type
           ('live-session object)
           ('session-history (map-elt object 'sessionId))
           ('repository (map-elt object 'name))
           (_ object))))
    (magit-insert-section
        (agent-shell-cockpit-section identity t :kind type :object object)
      (let ((heading
             (concat (propertize
                      (agent-shell-cockpit-ui-one-line
                       label (- agent-shell-cockpit-summary-width
                                (if status 12 0)))
                      'face 'default)
                     (if status
                         (agent-shell-cockpit-ui-status-label status)
                       ""))))
        (if non-collapsible
            (insert heading ?\n)
          (magit-insert-heading heading)
          (magit-insert-section-body
            (pcase type
              ('prompt
               (agent-shell-cockpit-workspace-view--insert-prompt-body object))
              ('live-session
               (agent-shell-cockpit-ui-insert-agent-preview object))
              (_
               (if (listp details)
                   (dolist (detail details)
                     (agent-shell-cockpit-ui-insert-detail
                      (car detail) (cdr detail)))
                 (insert "  "
                         (propertize
                          (agent-shell-cockpit-ui-one-line details 120)
                          'face 'agent-shell-cockpit-secondary)
                         "\n"))))))))))

(defun agent-shell-cockpit-workspace-view--insert-prompt-body (path)
  "Insert prompt file PATH as a section body."
  (agent-shell-cockpit-ui-insert-detail
   "File" (abbreviate-file-name path))
  (condition-case error-data
      (let ((start (point)))
        (goto-char (+ start (cadr (insert-file-contents path))))
        (unless (bolp)
          (insert ?\n))
        (let ((end (copy-marker (point) t)))
          (indent-rigidly start end 2)
          (add-text-properties
           start end '(font-lock-face agent-shell-cockpit-prompt-preview))
          (set-marker end nil)))
    (file-error
     (insert "  "
             (propertize (error-message-string error-data) 'face 'error)
             "\n"))))

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

(defun agent-shell-cockpit-workspace-insert-agents
    (_workspace live-sessions history _prompts _repositories)
  "Insert agent sections from LIVE-SESSIONS and HISTORY."
  (magit-insert-section
      (agent-shell-cockpit-section 'agents nil :kind 'group)
    (insert (propertize
             (format "Agents (%d)" (+ (length live-sessions)
                                       (length history)))
             'font-lock-face 'magit-section-heading)
            ?\n)
    (if (or live-sessions history)
        (progn
          (dolist (live live-sessions)
            (agent-shell-cockpit-workspace-view--insert-row
             (with-current-buffer live
               (agent-shell-cockpit-session--title))
             nil 'live-session live
             (agent-shell-cockpit-session-status live)))
          (dolist (session history)
            (agent-shell-cockpit-workspace-view--insert-row
             (or (map-elt session 'title)
                 (map-elt session 'sessionId))
             `(("Agent" . ,(map-elt session 'agentId))
               ("Session" . ,(map-elt session 'sessionId)))
             'session-history session 'history t)))
      (insert (propertize "No recorded agents\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert ?\n)))

(defun agent-shell-cockpit-workspace-insert-prompts
    (workspace _live-sessions _history prompts _repositories)
  "Render prompt-file sections for WORKSPACE.
PROMPTS is the list of prompt files to render."
  (magit-insert-section
      (agent-shell-cockpit-section 'prompts nil :kind 'group)
    (insert (propertize (format "Prompts (%d)" (length prompts))
                        'font-lock-face 'magit-section-heading)
            ?\n)
    (if prompts
        (dolist (prompt prompts)
          (agent-shell-cockpit-workspace-view--insert-row
           (agent-shell-cockpit-workspace-prompt-name workspace prompt)
           (abbreviate-file-name prompt) 'prompt prompt))
      (insert (propertize "No prompts\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert ?\n)))

(defun agent-shell-cockpit-workspace-insert-repositories
    (workspace _live-sessions _history _prompts repositories)
  "Insert REPOSITORIES for WORKSPACE."
  (magit-insert-section
      (agent-shell-cockpit-section 'repositories nil :kind 'group)
    (insert (propertize (format "Repositories (%d)" (length repositories))
                        'font-lock-face 'magit-section-heading)
            ?\n)
    (if repositories
        (dolist (repository repositories)
          (agent-shell-cockpit-workspace-view--insert-row
           (map-elt repository 'name)
           (let ((path (agent-shell-cockpit-workspace-repository-path
                        workspace repository)))
             `(("Path" . ,(abbreviate-file-name path))
               ("Status" . ,(agent-shell-cockpit-git-description path))))
           'repository repository))
      (insert (propertize "No repositories\n"
                          'face 'agent-shell-cockpit-secondary)))
    (insert ?\n)))

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
    (setq default-directory
          (file-name-as-directory (map-elt workspace 'root)))
    (erase-buffer)
    (magit-insert-section
        (agent-shell-cockpit-section
         (map-elt workspace 'root) nil :kind 'root :object workspace)
      (agent-shell-cockpit-ui-insert-header
       "Workspace" (map-elt workspace 'title))
      (agent-shell-cockpit-ui-insert-header
       "Root" (abbreviate-file-name (map-elt workspace 'root)))
      (insert ?\n)
      (magit-run-section-hook 'agent-shell-cockpit-workspace-sections-hook
                              workspace live-sessions history prompts
                              repositories))))

(defun agent-shell-cockpit-workspace-view-refresh ()
  "Refresh the current workspace detail view."
  (agent-shell-cockpit-ui-refresh-buffer
   #'agent-shell-cockpit-workspace-view--render))

(defun agent-shell-cockpit-workspace-view-open ()
  "Open the workspace detail item at point."
  (let ((workspace (agent-shell-cockpit-workspace-view--workspace))
        (type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ((or 'root 'group)
       nil)
      ('prompt (find-file object))
      ('repository
       (dired (agent-shell-cockpit-workspace-repository-path
               workspace object)))
      ('live-session (agent-shell-cockpit-session-visit object))
      ('session-history
       (switch-to-buffer
        (agent-shell-cockpit-session-resume workspace object)))
      (_ (user-error "Point is not on a workspace item")))))

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

(defun agent-shell-cockpit-workspace-view-allow-once ()
  "Allow the live agent's latest pending permission request once."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'live-session)
    (user-error "Point is not on a live agent"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (agent-shell-cockpit-session-allow-once buffer)
    (agent-shell-cockpit-workspace-view-refresh)))

(defun agent-shell-cockpit-workspace-view-back ()
  "Return to the cockpit dashboard."
  (interactive)
  (if (buffer-live-p agent-shell-cockpit--buffer)
      (switch-to-buffer agent-shell-cockpit--buffer)
    (agent-shell-cockpit)))

(defun agent-shell-cockpit-workspace-view--type-at-point-p (type)
  "Return non-nil when the Cockpit section at point has TYPE."
  (eq (agent-shell-cockpit-ui-object-type-at-point) type))

(transient-define-prefix agent-shell-cockpit-workspace-view-dispatch ()
  "Invoke a Cockpit workspace command from the available commands."
  ["Workspace and agent commands"
   [("s" "Start agent" agent-shell-cockpit-workspace-view-start-agent)
    ("S" "Start selected agent"
     agent-shell-cockpit-workspace-view-start-agent-select)
    ("r" "Resume agent" agent-shell-cockpit-resume-session
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'session-history)))
    ("K" "Kill agent" agent-shell-cockpit-workspace-view-kill-session
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'live-session)))
    ("Y" "Allow permission once" agent-shell-cockpit-workspace-view-allow-once
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'live-session)))]
   [("e" "Edit prompt" agent-shell-cockpit-workspace-view-edit-prompt)
    ("+" "Add repository" agent-shell-cockpit-add-worktree)
    ("-" "Remove repository" agent-shell-cockpit-remove-worktree
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p 'repository)))
    ("A" "Archive workspace" agent-shell-cockpit-workspace-view-archive)]
   [("b" "Return to dashboard" agent-shell-cockpit-workspace-view-back)]]
  ["Essential commands"
   [("g" "       Refresh current buffer" agent-shell-cockpit-refresh)
    ("q" "       Bury current buffer" agent-shell-cockpit-quit)
    ("<tab>" "   Toggle section at point" agent-shell-cockpit-toggle-section)
    ("<return>" "Visit thing at point" agent-shell-cockpit-open)]
   [("n" "       Next section" agent-shell-cockpit-next)
    ("p" "       Previous section" agent-shell-cockpit-previous)
    ("C-x m" "Show all key bindings" describe-mode)]])

(defvar-keymap agent-shell-cockpit-workspace-view-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "s" #'agent-shell-cockpit-workspace-view-start-agent
  "S" #'agent-shell-cockpit-workspace-view-start-agent-select
  "r" #'agent-shell-cockpit-resume-session
  "K" #'agent-shell-cockpit-workspace-view-kill-session
  "Y" #'agent-shell-cockpit-workspace-view-allow-once
  "e" #'agent-shell-cockpit-workspace-view-edit-prompt
  "+" #'agent-shell-cockpit-add-worktree
  "-" #'agent-shell-cockpit-remove-worktree
  "A" #'agent-shell-cockpit-workspace-view-archive
  "b" #'agent-shell-cockpit-workspace-view-back)

(define-derived-mode agent-shell-cockpit-workspace-view-mode
  agent-shell-cockpit-ui-mode "Cockpit-Workspace"
  "Major mode for a cockpit workspace detail view."
  (setq-local agent-shell-cockpit-ui--refresh-function
              #'agent-shell-cockpit-workspace-view-refresh
              agent-shell-cockpit-ui--open-function
              #'agent-shell-cockpit-workspace-view-open
              agent-shell-cockpit-ui--dispatch-function
              #'agent-shell-cockpit-workspace-view-dispatch))

(defun agent-shell-cockpit-workspace-view-buffer (workspace)
  "Return a rendered detail buffer for WORKSPACE without displaying it."
  (let ((buffer (get-buffer-create
                 (format "*Cockpit: %s*" (map-elt workspace 'name)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-cockpit-workspace-view-mode)
        (agent-shell-cockpit-workspace-view-mode))
      (setq agent-shell-cockpit-workspace-view--root
            (map-elt workspace 'root)
            default-directory
            (file-name-as-directory (map-elt workspace 'root)))
      (agent-shell-cockpit-workspace-view-refresh))
    buffer))

(defun agent-shell-cockpit-workspace-view (workspace)
  "Open detail view for WORKSPACE."
  (switch-to-buffer
   (agent-shell-cockpit-workspace-view-buffer workspace)))

(provide 'agent-shell-cockpit-workspace-view)

;;; agent-shell-cockpit-workspace-view.el ends here
