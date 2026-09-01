;;; agent-shell-cockpit-dashboard.el --- Cockpit workspace dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render and operate the top-level cockpit workspace dashboard.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(declare-function agent-shell-cockpit-workspace-view "agent-shell-cockpit-workspace-view")
(declare-function agent-shell-cockpit-archives "agent-shell-cockpit-archive-view")
(defvar agent-shell-cockpit--buffer nil
  "Live cockpit dashboard buffer.")

(defcustom agent-shell-cockpit-dashboard-sections-hook
  '(agent-shell-cockpit-dashboard-insert-agents
    agent-shell-cockpit-dashboard-insert-workspaces)
  "Hook of functions that insert top-level dashboard sections.
Each function receives WORKSPACES and AGENTS."
  :type 'hook
  :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-dashboard--sorted-workspaces ()
  "Return active workspaces sorted by title."
  (sort (agent-shell-cockpit-store-discover)
        (lambda (left right)
          (string-lessp (or (map-elt left 'title) (map-elt left 'name))
                        (or (map-elt right 'title) (map-elt right 'name))))))

(defun agent-shell-cockpit-dashboard--sorted-sessions (buffers)
  "Return live agent BUFFERS in stable name order."
  (sort (copy-sequence buffers)
        (lambda (left right)
          (string-lessp (buffer-name left) (buffer-name right)))))

(defun agent-shell-cockpit-dashboard--insert-workspace (workspace)
  "Insert non-collapsible WORKSPACE row."
  (let* ((title (agent-shell-cockpit-ui-one-line
                 (or (map-elt workspace 'title) (map-elt workspace 'name))
                 23))
         (invalid (eq (map-elt workspace 'kind) 'invalid)))
    (magit-insert-section
        (agent-shell-cockpit-section (map-elt workspace 'root) t
                                     :kind 'workspace :object workspace)
      (insert
       (concat (propertize (format "%-24s" title)
                           'face (if invalid 'error 'default))
               (if invalid
                   (concat (agent-shell-cockpit-ui-status-label 'invalid)
                           "  "
                           (propertize
                            (agent-shell-cockpit-ui-one-line
                             (map-elt workspace 'error) 50)
                            'face 'error))
                 (agent-shell-cockpit-dashboard--workspace-summary
                  workspace)))
       ?\n))))

(defun agent-shell-cockpit-dashboard--workspace-summary (workspace)
  "Return an icon-based resource summary for WORKSPACE."
  (let ((agents (length
                 (agent-shell-cockpit-session-live-buffers workspace)))
        (prompts (length
                  (agent-shell-cockpit-workspace-prompt-paths workspace)))
        (repositories
         (length
          (agent-shell-cockpit-workspace-active-repositories workspace))))
    (propertize
     (format "%d %s  %d %s  %d %s"
             agents (agent-shell-cockpit-ui-icon 'agent)
             prompts (agent-shell-cockpit-ui-icon 'prompt)
             repositories (agent-shell-cockpit-ui-icon 'repository))
     'face 'agent-shell-cockpit-secondary)))

(defun agent-shell-cockpit-dashboard--insert-session (buffer &optional workspace)
  "Insert live agent BUFFER with a preview for optional WORKSPACE."
  (let* ((status (agent-shell-cockpit-session-status buffer))
         (directory (buffer-local-value 'default-directory buffer))
         (kind (if workspace 'workspace-session 'session))
         (name (buffer-name buffer)))
    (magit-insert-section
        (agent-shell-cockpit-section buffer t :kind kind :object buffer)
      (magit-insert-heading
        (concat (propertize
                 (format "%-28s"
                         (string-trim name "\\*+" "\\*+"))
                 'face 'default)
                (agent-shell-cockpit-ui-status-label status)
                "  "
                (propertize
                 (if workspace
                     (agent-shell-cockpit-ui-one-line
                      (map-elt workspace 'title) 24)
                   (agent-shell-cockpit-ui-one-line
                    (abbreviate-file-name directory) 24))
                 'face 'agent-shell-cockpit-secondary)))
      (magit-insert-section-body
        (agent-shell-cockpit-ui-insert-agent-preview buffer)))))

(defun agent-shell-cockpit-dashboard-insert-agents (_workspaces agents)
  "Insert the live AGENTS section."
  (magit-insert-section
      (agent-shell-cockpit-section 'live-agents nil :kind 'group)
    (magit-insert-heading
      (propertize (format "Agents (%d)" (length agents))
                  'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
      (if agents
          (dolist (buffer
                   (agent-shell-cockpit-dashboard--sorted-sessions agents))
            (agent-shell-cockpit-dashboard--insert-session
             buffer (agent-shell-cockpit-session-workspace buffer)))
        (insert (propertize "No live agents\n"
                            'face 'agent-shell-cockpit-secondary)))
      (insert ?\n))))

(defun agent-shell-cockpit-dashboard-insert-workspaces (workspaces _agents)
  "Insert the active WORKSPACES section."
  (magit-insert-section
      (agent-shell-cockpit-section 'workspaces nil :kind 'group)
    (magit-insert-heading
      (propertize (format "Workspaces (%d)" (length workspaces))
                  'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
      (if workspaces
          (dolist (workspace workspaces)
            (agent-shell-cockpit-dashboard--insert-workspace workspace))
        (insert (propertize "No workspaces\n"
                            'face 'agent-shell-cockpit-secondary)))
      (insert ?\n))))

(defun agent-shell-cockpit-dashboard--render ()
  "Render the current cockpit dashboard."
  (let ((workspaces (agent-shell-cockpit-dashboard--sorted-workspaces))
        (unassigned (agent-shell-cockpit-session-unassigned-buffers)))
    (erase-buffer)
    (let ((agents
           (append
            (seq-mapcat #'agent-shell-cockpit-session-live-buffers workspaces)
            unassigned)))
      (magit-insert-section
          (agent-shell-cockpit-section 'dashboard nil :kind 'root)
        (agent-shell-cockpit-ui-insert-header
         "Cockpit" (abbreviate-file-name agent-shell-cockpit-workspace-directory))
        (insert ?\n)
        (magit-run-section-hook 'agent-shell-cockpit-dashboard-sections-hook
                                workspaces agents)))))

(defun agent-shell-cockpit-dashboard-refresh ()
  "Refresh the dashboard while retaining a useful row position."
  (agent-shell-cockpit-ui-refresh-buffer
   #'agent-shell-cockpit-dashboard--render))

(defun agent-shell-cockpit-dashboard-open ()
  "Open the dashboard item at point."
  (let ((type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ((or 'root 'group)
       nil)
      ('workspace
         (if (eq (map-elt object 'kind) 'invalid)
           (find-file (agent-shell-cockpit-store-metadata-path
                       (map-elt object 'root)))
         (agent-shell-cockpit-workspace-view object)))
      ((or 'session 'workspace-session)
       (agent-shell-cockpit-session-visit object))
      (_ (user-error "Point is not on a cockpit item")))))

(defun agent-shell-cockpit-dashboard-selected-workspace ()
  "Return workspace at point or signal a user error."
  (let* ((type (agent-shell-cockpit-ui-object-type-at-point))
         (object (agent-shell-cockpit-ui-object-at-point))
         (workspace
          (pcase type
            ('workspace object)
            ('workspace-session
             (agent-shell-cockpit-session-workspace object)))))
    (unless (and workspace (eq (map-elt workspace 'kind) 'workspace))
      (user-error "Point is not on an active workspace"))
    workspace))

(defun agent-shell-cockpit-create-workspace ()
  "Interactively create a cockpit workspace."
  (interactive)
  (let* ((name (read-string "Workspace directory name: "))
         (workspace (agent-shell-cockpit-workspace-create
                     :name name)))
    (agent-shell-cockpit-dashboard-refresh)
    (dired-other-window
     (agent-shell-cockpit-workspace-prompts-path workspace))))

(defun agent-shell-cockpit-edit-prompt ()
  "Select and edit a prompt in the selected workspace."
  (interactive)
  (agent-shell-cockpit-workspace-edit-prompt
   (agent-shell-cockpit-dashboard-selected-workspace)))

(defun agent-shell-cockpit-start-agent ()
  "Start the default agent in the selected workspace."
  (interactive)
  (agent-shell-cockpit-session-start-default
   (agent-shell-cockpit-dashboard-selected-workspace)))

(defun agent-shell-cockpit-start-agent-select ()
  "Choose and start an agent in the selected workspace."
  (interactive)
  (agent-shell-cockpit-session-start-select
   (agent-shell-cockpit-dashboard-selected-workspace)))

(defalias 'agent-shell-cockpit-create #'agent-shell-cockpit-start-agent)
(defalias 'agent-shell-cockpit-create-new #'agent-shell-cockpit-start-agent-select)

(defun agent-shell-cockpit-attach-session ()
  "Attach the selected unassigned session to a compatible workspace."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'session)
    (user-error "Point is not on an unassigned session"))
  (let* ((buffer (agent-shell-cockpit-ui-object-at-point))
         (workspaces
          (seq-filter (lambda (workspace)
                        (eq (map-elt workspace 'kind) 'workspace))
                      (agent-shell-cockpit-store-discover)))
         (choices (mapcar (lambda (workspace)
                            (cons (map-elt workspace 'title) workspace))
                          workspaces))
         (workspace (cdr (assoc (completing-read "Attach to workspace: "
                                                 choices nil t)
                                choices))))
    (agent-shell-cockpit-session-attach buffer workspace)
    (agent-shell-cockpit-dashboard-refresh)))

(defun agent-shell-cockpit-archive-workspace ()
  "Archive the selected active workspace after confirmation."
  (interactive)
  (let ((workspace (agent-shell-cockpit-dashboard-selected-workspace)))
    (when (yes-or-no-p (format "Archive workspace %s? "
                               (map-elt workspace 'title)))
      (agent-shell-cockpit-workspace-archive workspace)
      (agent-shell-cockpit-dashboard-refresh))))

(defun agent-shell-cockpit-kill ()
  "Kill the live agent session at point after confirmation."
  (interactive)
  (unless (memq (agent-shell-cockpit-ui-object-type-at-point)
                '(session workspace-session))
    (user-error "Point is not on a live agent session"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (when (yes-or-no-p (format "Kill agent session %s? " (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))
      (agent-shell-cockpit-dashboard-refresh))))

(defun agent-shell-cockpit-repair-workspace ()
  "Repair invalid workspace metadata at point after confirmation."
  (interactive)
  (let ((record (agent-shell-cockpit-ui-object-at-point)))
    (unless (and (listp record)
                 (eq (map-elt record 'kind) 'invalid))
      (user-error "Point is not on invalid workspace metadata"))
    (when (yes-or-no-p
           "Back up and rebuild metadata without repository/session history? ")
      (agent-shell-cockpit-workspace-repair record)
      (agent-shell-cockpit-dashboard-refresh))))

(defun agent-shell-cockpit-dashboard--workspace-at-point-p ()
  "Return non-nil when point represents an active workspace."
  (ignore-errors (agent-shell-cockpit-dashboard-selected-workspace) t))

(defun agent-shell-cockpit-dashboard--unassigned-at-point-p ()
  "Return non-nil when point represents an unassigned agent."
  (eq (agent-shell-cockpit-ui-object-type-at-point) 'session))

(defun agent-shell-cockpit-dashboard--live-agent-at-point-p ()
  "Return non-nil when point represents any live agent."
  (memq (agent-shell-cockpit-ui-object-type-at-point)
        '(session workspace-session)))

(defun agent-shell-cockpit-dashboard-allow-once ()
  "Allow the live agent's latest pending permission request once."
  (interactive)
  (unless (agent-shell-cockpit-dashboard--live-agent-at-point-p)
    (user-error "Point is not on a live agent"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (agent-shell-cockpit-session-allow-once buffer)
    (agent-shell-cockpit-dashboard-refresh)))

(defun agent-shell-cockpit-dashboard--invalid-at-point-p ()
  "Return non-nil when point represents invalid workspace metadata."
  (let ((object (agent-shell-cockpit-ui-object-at-point)))
    (and (listp object)
         (eq (map-elt object 'kind) 'invalid))))

(transient-define-prefix agent-shell-cockpit-archive-dispatch ()
  "Invoke an archive command."
  [["Archives"
    ("l" "Browse archived workspaces" agent-shell-cockpit-archives)]])

(transient-define-prefix agent-shell-cockpit-dashboard-dispatch ()
  "Invoke a Cockpit dashboard command from the available commands."
  ["Workspace and agent commands"
   [("w" "Create workspace" agent-shell-cockpit-create-workspace)
    ("e" "Edit prompt" agent-shell-cockpit-edit-prompt
     :inapt-if-not agent-shell-cockpit-dashboard--workspace-at-point-p)
    ("A" "Archive workspace" agent-shell-cockpit-archive-workspace
     :inapt-if-not agent-shell-cockpit-dashboard--workspace-at-point-p)
    ("E" "Repair metadata" agent-shell-cockpit-repair-workspace
     :inapt-if-not agent-shell-cockpit-dashboard--invalid-at-point-p)]
   [("s" "Start agent" agent-shell-cockpit-start-agent
     :inapt-if-not agent-shell-cockpit-dashboard--workspace-at-point-p)
    ("S" "Start selected agent" agent-shell-cockpit-start-agent-select
     :inapt-if-not agent-shell-cockpit-dashboard--workspace-at-point-p)
    ("a" "Attach agent" agent-shell-cockpit-attach-session
     :inapt-if-not agent-shell-cockpit-dashboard--unassigned-at-point-p)
    ("Y" "Allow permission once" agent-shell-cockpit-dashboard-allow-once
     :inapt-if-not agent-shell-cockpit-dashboard--live-agent-at-point-p)
    ("K" "Kill agent" agent-shell-cockpit-kill
     :inapt-if-not agent-shell-cockpit-dashboard--live-agent-at-point-p)]
   [("l" "Archives" agent-shell-cockpit-archive-dispatch)]]
  ["Essential commands"
   [("g" "       Refresh current buffer" agent-shell-cockpit-refresh)
    ("q" "       Bury current buffer" agent-shell-cockpit-quit)
    ("<tab>" "   Toggle section at point" agent-shell-cockpit-toggle-section)
    ("<return>" "Visit thing at point" agent-shell-cockpit-open)]
   [("n" "       Next section" agent-shell-cockpit-next)
    ("p" "       Previous section" agent-shell-cockpit-previous)
    ("C-x m" "Show all key bindings" describe-mode)]])

(defvar-keymap agent-shell-cockpit-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "w" #'agent-shell-cockpit-create-workspace
  "e" #'agent-shell-cockpit-edit-prompt
  "s" #'agent-shell-cockpit-start-agent
  "S" #'agent-shell-cockpit-start-agent-select
  "a" #'agent-shell-cockpit-attach-session
  "A" #'agent-shell-cockpit-archive-workspace
  "Y" #'agent-shell-cockpit-dashboard-allow-once
  "K" #'agent-shell-cockpit-kill
  "l" #'agent-shell-cockpit-archive-dispatch)

(define-derived-mode agent-shell-cockpit-mode agent-shell-cockpit-ui-mode
  "Agent-Cockpit"
  "Major mode for the agent-shell cockpit workspace dashboard."
  (setq-local agent-shell-cockpit-ui--refresh-function
              #'agent-shell-cockpit-dashboard-refresh
              agent-shell-cockpit-ui--open-function
              #'agent-shell-cockpit-dashboard-open
              agent-shell-cockpit-ui--dispatch-function
              #'agent-shell-cockpit-dashboard-dispatch))

;;;###autoload
(defun agent-shell-cockpit ()
  "Open the agent-shell cockpit workspace dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-cockpit-buffer-name)))
    (setq agent-shell-cockpit--buffer buffer)
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'agent-shell-cockpit-mode)
      (agent-shell-cockpit-mode))
    (agent-shell-cockpit-dashboard-refresh)))

(provide 'agent-shell-cockpit-dashboard)

;;; agent-shell-cockpit-dashboard.el ends here
