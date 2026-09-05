;;; agent-shell-cockpit-workspace-view.el --- Cockpit workspace detail -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render and operate a single cockpit workspace.

;;; Code:

(require 'dired)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-cockpit-agent)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-skills)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(defvar agent-shell-cockpit--buffer)
(declare-function agent-shell-cockpit "agent-shell-cockpit-dashboard")
(defvar-local agent-shell-cockpit-workspace-view--root nil
  "Workspace root displayed in the current detail buffer.")

(defcustom agent-shell-cockpit-repository-open-function #'dired
  "Function used to open a repository from a workspace view.
The function receives the repository directory as its sole argument."
  :type 'function
  :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-read-source-directory ()
  "Prompt for and return a local source repository directory."
  (read-directory-name "Source repository: "))

(defcustom agent-shell-cockpit-repository-source-function
  #'agent-shell-cockpit-read-source-directory
  "Function used to choose a source repository for a new worktree.
The function is called without arguments and must return a directory."
  :type 'function
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-workspace-sections-hook
  '(agent-shell-cockpit-workspace-insert-agents
    agent-shell-cockpit-workspace-insert-context
    agent-shell-cockpit-workspace-insert-repositories)
  "Hook of functions that insert workspace detail sections.
Each function receives WORKSPACE, LIVE-SESSIONS, HISTORY, CONTEXTS,
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
              ('context-file
               (agent-shell-cockpit-workspace-view--insert-context-body object))
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

(defun agent-shell-cockpit-workspace-view--insert-context-body (path)
  "Insert context file PATH as a section body."
  (agent-shell-cockpit-ui-insert-detail
   "File" (abbreviate-file-name path))
  (condition-case error-data
      (let ((start (point)))
        (insert (agent-shell-cockpit-workspace-view--fontified-file path))
        (unless (bolp)
          (insert ?\n))
        (let ((end (copy-marker (point) t)))
          (indent-rigidly start end 2)
          (agent-shell-cockpit-workspace-view--apply-fontified-faces
           start end)
          (set-marker end nil)))
    (file-error
     (insert "  "
             (propertize (error-message-string error-data) 'face 'error)
             "\n"))))

(defun agent-shell-cockpit-workspace-view--fontified-file (path)
  "Return PATH's contents fontified using its normal major mode.
File-local variables and mode hooks are not run.  Only face properties
are retained, with the context-preview face layered underneath them."
  (let ((source (generate-new-buffer "cockpit-context-preview")))
    (unwind-protect
        (with-current-buffer source
          (let ((buffer-file-name path)
                (default-directory (file-name-directory path))
                (enable-local-eval nil)
                (enable-local-variables nil)
                (inhibit-message t))
            (insert-file-contents path)
            ;; Like Magit's blob buffers, use the file's normal major mode as
            ;; an isolated source of syntax faces.  Delayed hooks disappear
            ;; with this temporary buffer instead of running user code.
            (condition-case nil
                (progn
                  (delay-mode-hooks (normal-mode t))
                  (font-lock-ensure))
              (error
               ;; A missing tree-sitter grammar or broken third-party mode
               ;; should degrade to an unfontified preview, not abort refresh.
               (remove-list-of-text-properties
                (point-min) (point-max) '(face font-lock-face))))
            (let ((text (buffer-substring (point-min) (point-max))))
              (agent-shell-cockpit-workspace-view--sanitize-fontified-text
               text)
              text)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun agent-shell-cockpit-workspace-view--sanitize-fontified-text (text)
  "Remove all properties except syntax faces from TEXT."
  (let ((position 0)
        (length (length text)))
    (while (< position length)
      (let* ((next (next-property-change position text length))
             (face (get-text-property position 'face text))
             (font-lock-face
              (get-text-property position 'font-lock-face text)))
        (set-text-properties position next nil text)
        (when face
          (put-text-property position next 'face face text))
        (when font-lock-face
          (put-text-property
           position next 'font-lock-face font-lock-face text))
        (setq position next))))
  text)

(defun agent-shell-cockpit-workspace-view--apply-fontified-faces (start end)
  "Turn source faces between START and END into display overlays.
This follows Magit's diff preview approach, keeping source syntax faces
outside the font-lock lifecycle of the cockpit buffer."
  (let ((position start))
    (while (< position end)
      (let* ((next (next-property-change position nil end))
             (face (get-text-property position 'face))
             (font-lock-face (get-text-property position 'font-lock-face))
             (source-face
              (cond
               ((and face font-lock-face) (list face font-lock-face))
               (face face)
               (font-lock-face font-lock-face))))
        (when source-face
          (let ((overlay (make-overlay position next nil t)))
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'face source-face)))
        (remove-list-of-text-properties
         position next '(face font-lock-face))
        (setq position next))))
  (add-text-properties
   start end '(font-lock-face agent-shell-cockpit-context-preview)))

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
    (workspace live-sessions history _contexts _repositories)
  "Insert WORKSPACE agent sections from LIVE-SESSIONS and HISTORY."
  (magit-insert-section
      (agent-shell-cockpit-section 'agents nil :kind 'group)
    (magit-insert-heading
      (propertize
       (format "Agents (%d)" (+ (length live-sessions)
                                 (length history)))
       'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
      (if (or live-sessions history)
          (progn
            (dolist (live live-sessions)
              (agent-shell-cockpit-agent-insert-live
               live workspace 'live-session))
            (dolist (session history)
              (agent-shell-cockpit-agent-insert-history
               session workspace)))
        (insert (propertize "No recorded agents\n"
                            'face 'agent-shell-cockpit-secondary)))
      (insert ?\n))))

(defun agent-shell-cockpit-workspace-insert-context
    (workspace _live-sessions _history contexts _repositories)
  "Render context-file sections for WORKSPACE.
CONTEXTS is the list of context files to render."
  (magit-insert-section
      (agent-shell-cockpit-section 'contexts nil :kind 'group)
    (magit-insert-heading
      (propertize (format "Context (%d)" (length contexts))
                  'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
      (if contexts
          (dolist (context contexts)
            (agent-shell-cockpit-workspace-view--insert-row
             (agent-shell-cockpit-workspace-context-name workspace context)
             (abbreviate-file-name context) 'context-file context))
        (insert (propertize "No context files\n"
                            'face 'agent-shell-cockpit-secondary)))
      (insert ?\n))))

(defun agent-shell-cockpit-workspace-insert-repositories
    (workspace _live-sessions _history _contexts repositories)
  "Insert REPOSITORIES for WORKSPACE."
  (magit-insert-section
      (agent-shell-cockpit-section 'repositories nil :kind 'group)
    (magit-insert-heading
      (propertize (format "Repositories (%d)" (length repositories))
                  'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
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
      (insert ?\n))))

(defun agent-shell-cockpit-workspace-view--render ()
  "Render the current workspace detail buffer."
  (let* ((workspace (agent-shell-cockpit-workspace-view--workspace))
         (repositories
          (agent-shell-cockpit-workspace-active-repositories workspace))
         (sessions (map-elt workspace 'sessions))
         (contexts (agent-shell-cockpit-workspace-context-paths workspace))
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
                              workspace live-sessions history contexts
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
      ('context-file (find-file object))
      ('repository
       (funcall agent-shell-cockpit-repository-open-function
                (agent-shell-cockpit-workspace-repository-path
                 workspace object)))
      ('live-session (agent-shell-cockpit-session-visit object))
      ('session-history
       (switch-to-buffer
        (agent-shell-cockpit-session-resume workspace object)))
      (_ (user-error "Point is not on a workspace item")))))

(defun agent-shell-cockpit-workspace-view-edit-context ()
  "Select and edit a context file in the displayed workspace."
  (interactive)
  (agent-shell-cockpit-workspace-edit-context
   (agent-shell-cockpit-workspace-view--workspace)))

(defun agent-shell-cockpit-workspace-view--source-directory ()
  "Prompt for a local source repository directory."
  (funcall agent-shell-cockpit-repository-source-function))

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
  "Configure launch skills, then choose and start an agent."
  (interactive)
  (agent-shell-cockpit-skills-launch
   (agent-shell-cockpit-workspace-view--workspace)))

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

(defun agent-shell-cockpit-workspace-view-forget-session ()
  "Forget the historical session at point after confirmation."
  (interactive)
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'session-history)
    (user-error "Point is not on historical session"))
  (let* ((workspace (agent-shell-cockpit-workspace-view--workspace))
         (session (agent-shell-cockpit-ui-object-at-point))
         (name (or (map-elt session 'title)
                   (map-elt session 'sessionId))))
    (when (y-or-n-p (format "Discard historical session %s? " name))
      (agent-shell-cockpit-session-forget workspace session)
      (agent-shell-cockpit-workspace-view-refresh))))

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
    ("a" "Agent actions" agent-shell-cockpit-agent-actions
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'live-session)))
    ("K" "Kill agent" agent-shell-cockpit-agent-kill
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'live-session)))
    ("x" "Discard history" agent-shell-cockpit-workspace-view-forget-session
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p
        'session-history)))]
   [("e" "Edit context" agent-shell-cockpit-workspace-view-edit-context)
    ("+" "Add repository" agent-shell-cockpit-add-worktree)
    ("-" "Remove repository" agent-shell-cockpit-remove-worktree
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-workspace-view--type-at-point-p 'repository)))
    ("A" "Archive workspace" agent-shell-cockpit-workspace-view-archive)
    ("b" "Return to dashboard" agent-shell-cockpit-workspace-view-back)]]
  ["Essential commands"
   [("r" "       Refresh current buffer" agent-shell-cockpit-refresh)
    ("q" "       Bury current buffer" agent-shell-cockpit-quit)
    ("<tab>" "   Toggle section at point" agent-shell-cockpit-toggle-section)
    ("<return>" "Visit thing at point" agent-shell-cockpit-open)]
   [("n" "       Next section" agent-shell-cockpit-next)
    ("p" "       Previous section" agent-shell-cockpit-previous)]])

(defvar-keymap agent-shell-cockpit-workspace-view-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "s" #'agent-shell-cockpit-workspace-view-start-agent
  "a" #'agent-shell-cockpit-agent-actions
  "x" #'agent-shell-cockpit-workspace-view-forget-session
  "K" #'agent-shell-cockpit-agent-kill
  "e" #'agent-shell-cockpit-workspace-view-edit-context
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
              #'agent-shell-cockpit-workspace-view-dispatch)
  (agent-shell-cockpit-agent-preview-mode 1))

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
  (let* ((origin (current-buffer))
         (buffer (agent-shell-cockpit-workspace-view-buffer workspace))
         (return-buffer
          (cond
           ((with-current-buffer origin
              (derived-mode-p 'agent-shell-cockpit-ui-mode))
            origin)
           ((buffer-live-p agent-shell-cockpit--buffer)
            agent-shell-cockpit--buffer)
           ((with-current-buffer origin
              (not (derived-mode-p 'agent-shell-mode)))
            origin))))
    (with-current-buffer buffer
      (setq agent-shell-cockpit-ui-return-buffer return-buffer))
    (switch-to-buffer buffer)))

(provide 'agent-shell-cockpit-workspace-view)

;;; agent-shell-cockpit-workspace-view.el ends here
