;;; agent-shell-cockpit-dashboard.el --- Cockpit workspace dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render and operate the top-level cockpit workspace dashboard.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(declare-function agent-shell-cockpit-workspace-view "agent-shell-cockpit-workspace-view")
(declare-function agent-shell-cockpit-workspace-view-buffer
                  "agent-shell-cockpit-workspace-view")

(defvar agent-shell-cockpit--buffer nil
  "Live cockpit dashboard buffer.")

(defvar-local agent-shell-cockpit-dashboard-show-archived nil
  "Non-nil when the dashboard includes archived workspaces.")

(defconst agent-shell-cockpit-dashboard--ascii-art
  '("   .---------------."
    "  / .-----------.  \\"
    " / /   _     _    \\ \\"
    "| |   (_)   (_)    | |"
    "| |  .-------.     | |"
    " \\ \\ '-------'    / /"
    "  '---------------'"))

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

(defun agent-shell-cockpit-dashboard--insert-workspace (workspace archived)
  "Insert a group heading for WORKSPACE, marked ARCHIVED when non-nil."
  (let* ((title (or (map-elt workspace 'title) (map-elt workspace 'name)))
         (start (point)))
    (insert "  "
            (propertize (format "--- %s%s ---"
                                title
                                (if (eq (map-elt workspace 'kind) 'invalid)
                                    " (INVALID)" ""))
                        'face (if (eq (map-elt workspace 'kind) 'invalid)
                                  'error
                                'agent-shell-cockpit-heading))
            "\n")
    (when (eq (map-elt workspace 'kind) 'invalid)
      (insert "      "
              (propertize (map-elt workspace 'error)
                          'face 'agent-shell-cockpit-secondary)
              "\n"))
    (agent-shell-cockpit-ui-add-row-properties
     start (list 'agent-shell-cockpit-object-type
                 (if archived 'archived-workspace 'workspace)
                 'agent-shell-cockpit-object workspace))))

(defun agent-shell-cockpit-dashboard--insert-session (buffer &optional workspace)
  "Insert live agent BUFFER, associated with optional WORKSPACE."
  (let ((status (agent-shell-cockpit-session-status buffer))
        (directory (buffer-local-value 'default-directory buffer))
        (start (point)))
    (insert "  " (agent-shell-cockpit-ui-status-badge status) "  "
            (propertize (string-trim (buffer-name buffer) "\\*+" "\\*+")
                        'face 'font-lock-function-name-face)
            "\n" (make-string 23 ?\s))
    (insert (propertize (abbreviate-file-name directory)
                        'face 'agent-shell-cockpit-secondary))
    (insert "\n")
    (agent-shell-cockpit-ui-add-row-properties
     start (list 'agent-shell-cockpit-object-type
                 (if workspace 'workspace-session 'session)
                 'agent-shell-cockpit-object buffer))))

(defun agent-shell-cockpit-dashboard--render ()
  "Render the current cockpit dashboard."
  (let ((workspaces (agent-shell-cockpit-dashboard--sorted-workspaces))
        (unassigned (agent-shell-cockpit-session-unassigned-buffers))
        (archived (when agent-shell-cockpit-dashboard-show-archived
                    (agent-shell-cockpit-store-discover t))))
    (erase-buffer)
    (insert "\n")
    (dolist (line agent-shell-cockpit-dashboard--ascii-art)
      (insert "  " (propertize line 'face 'agent-shell-cockpit-title) "\n"))
    (let ((agent-count
           (+ (length unassigned)
              (seq-reduce
               (lambda (count workspace)
                 (+ count (length
                           (agent-shell-cockpit-session-live-buffers workspace))))
               workspaces 0))))
      (insert "\n  "
            (propertize
             (format "%d active workspace%s · %d live agent%s"
                     (length workspaces) (if (= (length workspaces) 1) "" "s")
                     agent-count (if (= agent-count 1) "" "s"))
             'face 'agent-shell-cockpit-secondary)
            "\n\n"))
    (agent-shell-cockpit-ui-insert-heading "  Workspaces"
                                           (length workspaces))
    (if workspaces
        (dolist (workspace workspaces)
          (agent-shell-cockpit-dashboard--insert-workspace workspace nil)
          (when (eq (map-elt workspace 'kind) 'workspace)
            (dolist (buffer
                     (agent-shell-cockpit-dashboard--sorted-sessions
                      (agent-shell-cockpit-session-live-buffers workspace)))
              (agent-shell-cockpit-dashboard--insert-session buffer workspace))))
      (insert (propertize "  No workspaces. Use cw to create one.\n"
                          'face 'agent-shell-cockpit-secondary)))
    (when unassigned
      (insert "\n")
      (agent-shell-cockpit-ui-insert-heading "  Unassigned agents"
                                             (length unassigned))
      (dolist (buffer (agent-shell-cockpit-dashboard--sorted-sessions
                       unassigned))
        (agent-shell-cockpit-dashboard--insert-session buffer)))
    (when agent-shell-cockpit-dashboard-show-archived
      (insert "\n")
      (agent-shell-cockpit-ui-insert-heading "  Archived workspaces"
                                             (length archived))
      (dolist (workspace archived)
        (agent-shell-cockpit-dashboard--insert-workspace workspace t)))
    (insert "\n  "
            (propertize
             "Evil: j/k move · C-j/C-k preview · TAB/RET open · ? help"
             'face 'agent-shell-cockpit-secondary)
            "\n")))

(defun agent-shell-cockpit-dashboard-refresh ()
  "Refresh the dashboard while retaining a useful row position."
  (let* ((window (get-buffer-window (current-buffer) t))
         (saved-position (if (window-live-p window)
                             (window-point window)
                           (point)))
         (saved (agent-shell-cockpit-ui-capture-position saved-position))
        (inhibit-read-only t))
    (agent-shell-cockpit-dashboard--render)
    (agent-shell-cockpit-ui-restore-position saved)
    (when (window-live-p window)
      (set-window-point window (point)))))

(defun agent-shell-cockpit-dashboard-open ()
  "Open the dashboard item at point."
  (let ((type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ((or 'workspace 'archived-workspace)
       (if (eq (map-elt object 'kind) 'invalid)
           (find-file (agent-shell-cockpit-store-metadata-path
                       (map-elt object 'root)))
         (agent-shell-cockpit-workspace-view object)))
      ((or 'session 'workspace-session)
       (agent-shell-cockpit-ui-show-right object t))
      (_ (user-error "Point is not on a cockpit item")))))

(defun agent-shell-cockpit-dashboard-preview-buffer ()
  "Return a preview buffer for the dashboard item at point."
  (let ((type (agent-shell-cockpit-ui-object-type-at-point))
        (object (agent-shell-cockpit-ui-object-at-point)))
    (pcase type
      ((or 'session 'workspace-session) object)
      ((or 'workspace 'archived-workspace)
       (when (eq (map-elt object 'kind) 'workspace)
         (agent-shell-cockpit-workspace-view-buffer object)))
      (_ nil))))

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

(defun agent-shell-cockpit-dashboard-toggle-archived ()
  "Toggle archived workspaces in the dashboard."
  (interactive)
  (setq agent-shell-cockpit-dashboard-show-archived
        (not agent-shell-cockpit-dashboard-show-archived))
  (agent-shell-cockpit-dashboard-refresh))

(defun agent-shell-cockpit-repair-workspace ()
  "Repair invalid workspace metadata at point after confirmation."
  (interactive)
  (let ((record (agent-shell-cockpit-ui-object-at-point)))
    (unless (eq (map-elt record 'kind) 'invalid)
      (user-error "Point is not on invalid workspace metadata"))
    (when (yes-or-no-p
           "Back up and rebuild metadata without repository/session history? ")
      (agent-shell-cockpit-workspace-repair record)
      (agent-shell-cockpit-dashboard-refresh))))

(defvar-keymap agent-shell-cockpit-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "w" #'agent-shell-cockpit-create-workspace
  "c" #'agent-shell-cockpit-start-agent
  "C" #'agent-shell-cockpit-start-agent-select
  "e" #'agent-shell-cockpit-edit-prompt
  "a" #'agent-shell-cockpit-attach-session
  "A" #'agent-shell-cockpit-archive-workspace
  "E" #'agent-shell-cockpit-repair-workspace
  "v" #'agent-shell-cockpit-dashboard-toggle-archived
  "x" #'agent-shell-cockpit-kill
  "C-c C-n" #'agent-shell-cockpit-create-workspace
  "C-c C-s" #'agent-shell-cockpit-start-agent
  "C-c C-c" #'agent-shell-cockpit-start-agent-select
  "C-c C-e" #'agent-shell-cockpit-edit-prompt
  "C-c C-a" #'agent-shell-cockpit-attach-session
  "C-c C-x" #'agent-shell-cockpit-archive-workspace
  "C-c C-v" #'agent-shell-cockpit-dashboard-toggle-archived
  "C-c C-r" #'agent-shell-cockpit-repair-workspace
  "C-c C-k" #'agent-shell-cockpit-kill)

(define-derived-mode agent-shell-cockpit-mode agent-shell-cockpit-ui-mode
  "Agent-Cockpit"
  "Major mode for the agent-shell cockpit workspace dashboard."
  (setq-local agent-shell-cockpit-ui--refresh-function
              #'agent-shell-cockpit-dashboard-refresh
              agent-shell-cockpit-ui--open-function
              #'agent-shell-cockpit-dashboard-open
              agent-shell-cockpit-ui--preview-function
              #'agent-shell-cockpit-dashboard-preview-buffer))

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
