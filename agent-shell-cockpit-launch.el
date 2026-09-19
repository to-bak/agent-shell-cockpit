;;; agent-shell-cockpit-launch.el --- Ordered launch checklist -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compose agent launches from declarative instructions and workspace files.

;;; Code:

(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-instructions)
(require 'agent-shell-cockpit-session)
(require 'transient)

(defvar agent-shell-cockpit-worktree-open-function)

(defvar-local agent-shell-cockpit-launch--target nil)
(defvar-local agent-shell-cockpit-launch--items nil)
(defvar-local agent-shell-cockpit-launch--worktrees nil)

(defun agent-shell-cockpit-launch--workspace ()
  "Return the launch workspace, if any."
  (map-elt agent-shell-cockpit-launch--target 'workspace))

(defun agent-shell-cockpit-launch--initial-items (workspace)
  "Build fresh catalog entries and selected context files for WORKSPACE."
  (let* ((catalog (agent-shell-cockpit-instructions--catalog workspace))
         (ids (delete-dups (append agent-shell-cockpit-default-instructions
                                  (mapcar #'car catalog)))))
    (let ((items
           (delq nil
                 (mapcar (lambda (id)
                           (when-let* ((entry (assq id catalog)))
                             (list :id id :title (plist-get (cdr entry) :title)
                                   :source (plist-get (cdr entry) :source)
                                   :enabled (and (memq id agent-shell-cockpit-default-instructions) t))))
                         ids))))
      (when workspace
        (dolist (file (agent-shell-cockpit-workspace-context-paths workspace))
          (let* ((root (map-elt workspace 'root))
                 (existing
                  (seq-find (lambda (item)
                              (let ((source (plist-get item :source)))
                                (and (eq (car source) 'file) (stringp (cadr source))
                                     (equal (expand-file-name (cadr source) root) file)))) items)))
            (if existing
                (setf (plist-get existing :enabled) t)
              (let ((relative (file-relative-name file root)))
                (setq items (append items
                                    (list (list :id (make-symbol "context") :title relative
                                                :source (list 'file relative) :enabled t)))))))))
      items)))

(defun agent-shell-cockpit-launch--row (item kind)
  "Insert checklist ITEM of KIND as a Magit section."
  (magit-insert-section
      (agent-shell-cockpit-section (plist-get item :id) nil :kind kind :object item)
    (insert (format "  [%s] %s%s\n" (if (plist-get item :enabled) "x" " ")
                    (if (eq kind 'launch-item)
                        (format "%s · " (car (plist-get item :source))) "")
                    (plist-get item :title)))))

(defun agent-shell-cockpit-launch--render ()
  "Render the current ordered launch checklist."
  (erase-buffer)
  (magit-insert-section (agent-shell-cockpit-section 'launch nil :kind 'root)
    (agent-shell-cockpit-ui-insert-header
     "Launch" (or (map-elt (agent-shell-cockpit-launch--workspace) 'title)
                  (map-elt agent-shell-cockpit-launch--target 'directory)))
    (insert "\nSPC toggle · M-k/M-j reorder · a add file · RET inspect · p preview · s start\n\n")
    (when (agent-shell-cockpit-launch--workspace)
      (magit-insert-section (agent-shell-cockpit-section 'worktrees nil :kind 'group)
        (magit-insert-heading "Worktrees")
        (dolist (item agent-shell-cockpit-launch--worktrees)
          (agent-shell-cockpit-launch--row item 'launch-worktree))
        (insert "\n")))
    (magit-insert-section (agent-shell-cockpit-section 'items nil :kind 'group)
      (magit-insert-heading "Launch items (message order)")
      (dolist (item agent-shell-cockpit-launch--items)
        (agent-shell-cockpit-launch--row item 'launch-item))
      (insert "\n"))))

(defun agent-shell-cockpit-launch-refresh ()
  "Refresh the checklist, preserving its selection and order."
  (interactive)
  (when-let* ((workspace (agent-shell-cockpit-launch--workspace))
              ((file-directory-p (map-elt workspace 'root))))
    (dolist (entry (agent-shell-cockpit-workspace-active-worktrees workspace))
      (let ((name (map-elt entry 'name)))
        (when (and (file-directory-p (agent-shell-cockpit-workspace-repository-path workspace entry))
                   (not (seq-find (lambda (item) (equal (plist-get item :id) name))
                                  agent-shell-cockpit-launch--worktrees)))
          (setq agent-shell-cockpit-launch--worktrees
                (append agent-shell-cockpit-launch--worktrees
                        (list (list :id name :title name :enabled nil))))))))
  (agent-shell-cockpit-ui-refresh-buffer #'agent-shell-cockpit-launch--render))

(defun agent-shell-cockpit-launch-toggle ()
  "Toggle the worktree or launch item at point."
  (interactive)
  (unless (memq (agent-shell-cockpit-ui-object-type-at-point) '(launch-item launch-worktree))
    (user-error "Select a checklist item"))
  (let ((item (agent-shell-cockpit-ui-object-at-point)))
    (setf (plist-get item :enabled) (not (plist-get item :enabled))))
  (agent-shell-cockpit-launch-refresh))

(defun agent-shell-cockpit-launch--move (offset)
  "Move the launch item at point by OFFSET places."
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point) 'launch-item)
    (user-error "Select a launch item to reorder"))
  (let* ((item (agent-shell-cockpit-ui-object-at-point))
         (items (vconcat agent-shell-cockpit-launch--items))
         (index (cl-position item items :test #'eq))
         (other (+ index offset)))
    (when (and (>= other 0) (< other (length items)))
      (cl-rotatef (aref items index) (aref items other))
      (setq agent-shell-cockpit-launch--items (append items nil))
      (agent-shell-cockpit-launch-refresh))))

(defun agent-shell-cockpit-launch-move-up ()
  "Move the current launch item up."
  (interactive)
  (agent-shell-cockpit-launch--move -1))

(defun agent-shell-cockpit-launch-move-down ()
  "Move the current launch item down."
  (interactive)
  (agent-shell-cockpit-launch--move 1))

(defun agent-shell-cockpit-launch-add-file (file)
  "Append FILE as an enabled read item."
  (interactive
   (list (read-file-name "Read file: "
                         (if-let* ((workspace (agent-shell-cockpit-launch--workspace)))
                             (expand-file-name agent-shell-cockpit-context-directory-name
                                               (map-elt workspace 'root))
                           default-directory)
                         nil t)))
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "Choose a readable file"))
  (setq agent-shell-cockpit-launch--items
        (append agent-shell-cockpit-launch--items
                (list (list :id (make-symbol "file") :title (file-relative-name file default-directory)
                            :source (list 'file (expand-file-name file)) :enabled t))))
  (agent-shell-cockpit-launch-refresh))

(defun agent-shell-cockpit-launch-inspect ()
  "Inspect the selected instruction source or worktree."
  (interactive)
  (let* ((item (agent-shell-cockpit-ui-object-at-point))
         (source (plist-get item :source))
         (workspace (agent-shell-cockpit-launch--workspace)))
    (pcase (agent-shell-cockpit-ui-object-type-at-point)
      ('launch-worktree
       (funcall agent-shell-cockpit-worktree-open-function
                (agent-shell-cockpit-workspace-repository-path
                 workspace `((name . ,(plist-get item :id))))))
      ('launch-item
       (if (memq (car source) '(literal skill))
           (message "%s" (cadr source))
         (let ((visit (plist-get (agent-shell-cockpit-instructions--adapter (car source)) :visit)))
           (unless visit (user-error "This adapter cannot visit its source"))
           (funcall visit source workspace))))
      (_ (user-error "Select a checklist item")))))

(defun agent-shell-cockpit-launch-render ()
  "Return the selected worktree scope and ordered initial message."
  (let* ((workspace (agent-shell-cockpit-launch--workspace))
         (selected (seq-filter (lambda (item) (plist-get item :enabled))
                               agent-shell-cockpit-launch--items))
         (agent-shell-cockpit-instructions
          (mapcar (lambda (item) (list (plist-get item :id)
                                      :title (plist-get item :title)
                                      :source (plist-get item :source)))
                  (seq-remove (lambda (item) (eq (plist-get item :id) 'cockpit)) selected)))
         (text (agent-shell-cockpit-instructions-render
                (mapcar (lambda (item) (plist-get item :id)) selected) workspace))
         (paths
          (when workspace
            (mapcar
             (lambda (item)
               (let ((path (agent-shell-cockpit-workspace-repository-path
                            workspace `((name . ,(plist-get item :id))))))
                 (unless (file-directory-p path) (user-error "Selected worktree is missing: %s" path))
                 (file-relative-name path (map-elt workspace 'root))))
             (seq-filter (lambda (item) (plist-get item :enabled))
                         agent-shell-cockpit-launch--worktrees)))))
    (string-join
     (delq nil
           (list (when workspace
                   (if paths
                       (concat "Work only in these selected worktrees unless asked otherwise:\n"
                               (mapconcat (lambda (path) (concat "- " path)) paths "\n"))
                     "No worktrees are assigned to this agent. Do not edit repository checkouts unless asked."))
                 text)) "\n\n")))

(defun agent-shell-cockpit-launch-preview ()
  "Preview the exact assembled initial message without starting an agent."
  (interactive)
  (let ((text (agent-shell-cockpit-launch-render)))
    (with-current-buffer (get-buffer-create "*Cockpit launch preview*")
      (let ((inhibit-read-only t)) (erase-buffer) (insert text))
      (special-mode)
      (display-buffer (current-buffer)))))

(defun agent-shell-cockpit-launch-start ()
  "Validate the checklist and start an agent with the assembled message."
  (interactive)
  (let* ((text (agent-shell-cockpit-launch-render))
         (target (agent-shell-cockpit-session-target
                  (map-elt agent-shell-cockpit-launch--target 'directory)
                  (agent-shell-cockpit-launch--workspace)))
         (selection (mapcar (lambda (item) (plist-get item :id))
                            (seq-filter (lambda (item) (plist-get item :enabled))
                                        agent-shell-cockpit-launch--worktrees)))
         (origin agent-shell-cockpit-ui-return-buffer)
         (buffer (agent-shell-cockpit-session-start-target target text)))
    (with-current-buffer buffer
      (setq agent-shell-cockpit-session-selected-worktrees selection
            agent-shell-cockpit-session-return-buffer origin)
      (agent-shell-cockpit-session--upsert-current))))

(transient-define-prefix agent-shell-cockpit-launch-dispatch ()
  "Prepare and launch an agent."
  [[("SPC" "Toggle" agent-shell-cockpit-launch-toggle)
    ("u" "Move up" agent-shell-cockpit-launch-move-up)
    ("d" "Move down" agent-shell-cockpit-launch-move-down)
    ("a" "Add file" agent-shell-cockpit-launch-add-file)]
   [("RET" "Inspect source" agent-shell-cockpit-launch-inspect)
    ("p" "Preview message" agent-shell-cockpit-launch-preview)
    ("s" "Start agent" agent-shell-cockpit-launch-start)
    ("q" "Back" agent-shell-cockpit-quit)]])

(defvar-keymap agent-shell-cockpit-launch-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "SPC" #'agent-shell-cockpit-launch-toggle
  "RET" #'agent-shell-cockpit-launch-inspect
  "M-<up>" #'agent-shell-cockpit-launch-move-up
  "M-<down>" #'agent-shell-cockpit-launch-move-down
  "M-k" #'agent-shell-cockpit-launch-move-up
  "M-j" #'agent-shell-cockpit-launch-move-down
  "a" #'agent-shell-cockpit-launch-add-file
  "p" #'agent-shell-cockpit-launch-preview
  "s" #'agent-shell-cockpit-launch-start
  "?" #'agent-shell-cockpit-launch-dispatch
  "q" #'agent-shell-cockpit-quit)

(define-derived-mode agent-shell-cockpit-launch-mode agent-shell-cockpit-ui-mode "Cockpit Launch"
  "Prepare an agent using an ordered checklist."
  (setq-local agent-shell-cockpit-ui--refresh-function #'agent-shell-cockpit-launch-refresh
              agent-shell-cockpit-ui--open-function #'agent-shell-cockpit-launch-inspect
              agent-shell-cockpit-ui--dispatch-function #'agent-shell-cockpit-launch-dispatch))

(defun agent-shell-cockpit-launch (&optional workspace directory)
  "Prepare a fresh checklist in WORKSPACE or standalone DIRECTORY."
  (let* ((target (agent-shell-cockpit-session-target
                  (or (map-elt workspace 'root) directory default-directory) workspace))
         (origin (current-buffer))
         (buffer (get-buffer-create
                  (format "*Cockpit launch: %s*" (map-elt target 'directory)))))
    (when-let* ((preview (get-buffer "*Cockpit launch preview*")))
      (with-current-buffer preview
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Press p in the launch checklist to preview this new prompt.\n"))))
    (with-current-buffer buffer
      ;; Reusing the window must not reuse a previous launch's choices or files.
      (agent-shell-cockpit-launch-mode)
      (setq agent-shell-cockpit-launch--items (agent-shell-cockpit-launch--initial-items workspace)
              agent-shell-cockpit-launch--worktrees
              (when workspace
                (mapcar (lambda (entry)
                          (list :id (map-elt entry 'name) :title (map-elt entry 'name) :enabled t))
                        (seq-filter
                         (lambda (entry) (file-directory-p (agent-shell-cockpit-workspace-repository-path workspace entry)))
                         (agent-shell-cockpit-workspace-active-worktrees workspace)))))
      (setq agent-shell-cockpit-launch--target target
            agent-shell-cockpit-ui-return-buffer origin
            default-directory (map-elt target 'directory))
      (agent-shell-cockpit-launch-refresh))
    (pop-to-buffer buffer)))

(provide 'agent-shell-cockpit-launch)
;;; agent-shell-cockpit-launch.el ends here
