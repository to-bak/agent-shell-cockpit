;;; agent-shell-cockpit-ui.el --- Shared cockpit user interface -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared faces, navigation, timers, and preview window behavior.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(defcustom agent-shell-cockpit-buffer-name "*Agent Shell Cockpit*"
  "Name of the cockpit dashboard buffer."
  :type 'string
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-refresh-interval 2
  "Seconds between visible cockpit refreshes, or nil to disable."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-width 0.5
  "Width of the cockpit window when previewing another buffer."
  :type '(choice float integer)
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-title
  '((t :inherit (error fixed-pitch) :weight bold :height 1.7))
  "Face for the cockpit title."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-heading '((t :inherit bold))
  "Face for cockpit headings."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-secondary '((t :inherit shadow))
  "Face for secondary cockpit text."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-attention
  '((t :inherit error :weight bold :box (:line-width (1 . -1))))
  "Face for items needing attention."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-working
  '((t :inherit warning :weight bold :box (:line-width (1 . -1))))
  "Face for working items."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-ready
  '((t :inherit success :weight bold :box (:line-width (1 . -1))))
  "Face for ready items."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-muted
  '((t :inherit shadow :box (:line-width (1 . -1))))
  "Face for idle and unknown items."
  :group 'agent-shell-cockpit)

(defconst agent-shell-cockpit-ui--status-spec
  '((attention "[NEEDS ATTENTION]" agent-shell-cockpit-status-attention 0)
    (working "[WORKING]" agent-shell-cockpit-status-working 1)
    (ready "[READY]" agent-shell-cockpit-status-ready 2)
    (idle "[IDLE]" agent-shell-cockpit-status-muted 3)
    (history "[HISTORY]" agent-shell-cockpit-status-muted 4)
    (starting "[STARTING]" agent-shell-cockpit-status-muted 5)
    (invalid "[INVALID]" agent-shell-cockpit-status-attention 6)))

(defvar-local agent-shell-cockpit-ui--refresh-function nil)
(defvar-local agent-shell-cockpit-ui--open-function nil)
(defvar-local agent-shell-cockpit-ui--preview-function nil)
(defvar-local agent-shell-cockpit-ui--refresh-timer nil)

(defconst agent-shell-cockpit-ui--dashboard-commands
  '((agent-shell-cockpit-open . "Open selected workspace or agent")
    (agent-shell-cockpit-preview . "Preview selected item")
    (agent-shell-cockpit-preview-next . "Move down and preview")
    (agent-shell-cockpit-preview-previous . "Move up and preview")
    (agent-shell-cockpit-create-workspace . "Create workspace")
    (agent-shell-cockpit-start-agent . "Start default agent")
    (agent-shell-cockpit-start-agent-select . "Choose and start agent")
    (agent-shell-cockpit-edit-prompt . "Edit workspace prompt")
    (agent-shell-cockpit-attach-session . "Attach unassigned agent")
    (agent-shell-cockpit-archive-workspace . "Archive workspace")
    (agent-shell-cockpit-dashboard-toggle-archived . "Toggle archived workspaces")
    (agent-shell-cockpit-repair-workspace . "Repair workspace metadata")
    (agent-shell-cockpit-kill . "Kill selected live agent")
    (agent-shell-cockpit-refresh . "Refresh view")
    (quit-window . "Quit view"))
  "Commands shown by `agent-shell-cockpit-help' in the dashboard.")

(defconst agent-shell-cockpit-ui--workspace-commands
  '((agent-shell-cockpit-open . "Open selected item")
    (agent-shell-cockpit-preview . "Preview selected item")
    (agent-shell-cockpit-preview-next . "Move down and preview")
    (agent-shell-cockpit-preview-previous . "Move up and preview")
    (agent-shell-cockpit-workspace-view-edit-prompt . "Edit or create prompt")
    (agent-shell-cockpit-add-worktree . "Add repository worktree")
    (agent-shell-cockpit-remove-worktree . "Remove selected worktree")
    (agent-shell-cockpit-workspace-view-start-agent . "Start default agent")
    (agent-shell-cockpit-workspace-view-start-agent-select . "Choose agent")
    (agent-shell-cockpit-resume-session . "Resume historical session")
    (agent-shell-cockpit-workspace-view-archive . "Archive workspace")
    (agent-shell-cockpit-workspace-view-kill-session . "Kill live agent")
    (agent-shell-cockpit-refresh . "Refresh view")
    (agent-shell-cockpit-workspace-view-back . "Return to dashboard")
    (quit-window . "Quit view"))
  "Commands shown by `agent-shell-cockpit-help' in a workspace view.")

(defun agent-shell-cockpit-help ()
  "Display all commands available for the current Cockpit view."
  (interactive)
  (let* ((workspace-view
          (derived-mode-p 'agent-shell-cockpit-workspace-view-mode))
         (commands (if workspace-view
                       agent-shell-cockpit-ui--workspace-commands
                     agent-shell-cockpit-ui--dashboard-commands))
         (title (if workspace-view "Workspace commands" "Dashboard commands"))
         (rows
          (mapcar
           (lambda (entry)
             (let* ((command (car entry))
                    (key (where-is-internal command nil t)))
               (list (if key (key-description key) "M-x")
                     (cdr entry) command)))
           commands)))
    (with-help-window "*Agent Shell Cockpit Help*"
      (princ (format "%s\n%s\n\n" title (make-string (length title) ?=)))
      (dolist (row rows)
        (princ (format "%-12s %-38s %s\n"
                       (nth 0 row) (nth 1 row) (nth 2 row)))))))

(defun agent-shell-cockpit-ui-status-badge (status)
  "Return a propertized badge for STATUS."
  (let ((spec (assq status agent-shell-cockpit-ui--status-spec)))
    (propertize (format " %-17s " (or (nth 1 spec) "[UNKNOWN]"))
                'face (or (nth 2 spec) 'agent-shell-cockpit-status-muted))))

(defun agent-shell-cockpit-ui-status-rank (status)
  "Return display rank for STATUS."
  (or (nth 3 (assq status agent-shell-cockpit-ui--status-spec)) 99))

(defun agent-shell-cockpit-ui-insert-heading (title &optional count)
  "Insert section TITLE and optional COUNT."
  (insert (propertize title 'face 'agent-shell-cockpit-heading))
  (when count
    (insert (propertize (format "  %d" count)
                        'face 'agent-shell-cockpit-secondary)))
  (insert "\n\n"))

(defun agent-shell-cockpit-ui-add-row-properties (start properties)
  "Make text from START to point a navigable row with PROPERTIES."
  (let* ((type (plist-get properties 'agent-shell-cockpit-object-type))
         (object (plist-get properties 'agent-shell-cockpit-object))
         (identity (cond
                    ((bufferp object) object)
                    ((listp object)
                     (or (map-elt object 'id)
                         (map-elt object 'sessionId)
                         (map-elt object 'root)
                         (map-elt object 'name)))
                    (t object))))
    (add-text-properties
     start (point)
     (append (list 'agent-shell-cockpit-row t
                   'agent-shell-cockpit-row-key (cons type identity)
                   'mouse-face 'highlight)
             properties))))

(defun agent-shell-cockpit-ui-object-at-point ()
  "Return the cockpit object represented at point."
  (get-text-property (point) 'agent-shell-cockpit-object))

(defun agent-shell-cockpit-ui-object-type-at-point ()
  "Return the cockpit object type represented at point."
  (get-text-property (point) 'agent-shell-cockpit-object-type))

(defun agent-shell-cockpit-ui-row-key-at-point ()
  "Return the stable identity of the cockpit row at point."
  (get-text-property (point) 'agent-shell-cockpit-row-key))

(defun agent-shell-cockpit-ui-restore-row (key)
  "Move to the row identified by KEY and return non-nil when found."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (when (equal (get-text-property position 'agent-shell-cockpit-row-key)
                   key)
        (setq found position))
      (setq position
            (or (next-single-property-change
                 position 'agent-shell-cockpit-row-key nil (point-max))
                (point-max))))
    (when found (goto-char found))))

(defun agent-shell-cockpit-ui--row-start-at (position key)
  "Return the row start at POSITION having row KEY."
  (seq-find
   (lambda (candidate)
     (and (<= candidate position)
          (equal (get-text-property
                  candidate 'agent-shell-cockpit-row-key)
                 key)))
   (reverse (agent-shell-cockpit-ui--row-positions))))

(defun agent-shell-cockpit-ui-capture-position (&optional position)
  "Capture POSITION so it can be restored after rendering.
The offset within a navigable row is retained as well as its identity."
  (let* ((position (min (or position (point)) (point-max)))
         (had-rows (agent-shell-cockpit-ui--row-positions))
         (key (and (< position (point-max))
                   (get-text-property
                    position 'agent-shell-cockpit-row-key)))
         (row-start (and key
                         (agent-shell-cockpit-ui--row-start-at position key))))
    (list :point position :had-rows (and had-rows t) :row-key key
          :row-offset (and row-start (- position row-start)))))

(defun agent-shell-cockpit-ui-restore-position (state)
  "Restore point from captured position STATE after rendering."
  (let ((key (plist-get state :row-key))
        (offset (plist-get state :row-offset))
        restored)
    (when key
      (setq restored (agent-shell-cockpit-ui-restore-row key))
      (when restored
        (let* ((start (point))
               (end (or (next-single-property-change
                         start 'agent-shell-cockpit-row-key nil (point-max))
                        (point-max))))
          (goto-char (min (+ start (or offset 0))
                          (max start (1- end)))))))
    (unless restored
      (if (plist-get state :had-rows)
          (goto-char (min (or (plist-get state :point) (point-min))
                          (point-max)))
        (agent-shell-cockpit-ui-goto-first-row)))))

(defun agent-shell-cockpit-ui--row-positions ()
  "Return the start position of every navigable row."
  (let ((position (point-min)) positions)
    (while (< position (point-max))
      (when (get-text-property position 'agent-shell-cockpit-row)
        (push position positions))
      (setq position
            (or (next-single-property-change
                 position 'agent-shell-cockpit-object nil (point-max))
                (point-max))))
    (nreverse positions)))

(defun agent-shell-cockpit-ui-goto-first-row ()
  "Move point to the first navigable cockpit row."
  (when-let* ((position (car (agent-shell-cockpit-ui--row-positions))))
    (goto-char position)))

(defun agent-shell-cockpit-first ()
  "Move point to the first navigable cockpit row."
  (interactive)
  (agent-shell-cockpit-ui-goto-first-row))

(defun agent-shell-cockpit-last ()
  "Move point to the last navigable cockpit row."
  (interactive)
  (when-let* ((position (car (last (agent-shell-cockpit-ui--row-positions)))))
    (goto-char position)))

(defun agent-shell-cockpit-next ()
  "Move to the next cockpit row, wrapping at the end."
  (interactive)
  (agent-shell-cockpit-ui--move 1))

(defun agent-shell-cockpit-previous ()
  "Move to the previous cockpit row, wrapping at the beginning."
  (interactive)
  (agent-shell-cockpit-ui--move -1))

(defun agent-shell-cockpit-ui--move (step)
  "Move STEP rows and return the selected object."
  (let ((positions (agent-shell-cockpit-ui--row-positions)))
    (unless positions
      (user-error "No cockpit items"))
    (let* ((current (seq-position
                     positions (point)
                     (lambda (row point)
                       (and (<= row point)
                            (or (= row (car (last positions)))
                                (< point (or (seq-find (lambda (candidate)
                                                        (> candidate row))
                                                      positions)
                                             (point-max))))))))
           (target (nth (mod (+ (or current (if (> step 0) -1 0)) step)
                             (length positions))
                        positions)))
      (goto-char target)
      (agent-shell-cockpit-ui-object-at-point))))

(defun agent-shell-cockpit-refresh ()
  "Refresh the current cockpit buffer."
  (interactive)
  (unless agent-shell-cockpit-ui--refresh-function
    (user-error "This buffer cannot be refreshed"))
  (funcall agent-shell-cockpit-ui--refresh-function))

(defun agent-shell-cockpit-open ()
  "Open the cockpit item at point."
  (interactive)
  (unless agent-shell-cockpit-ui--open-function
    (user-error "This buffer cannot open items"))
  (funcall agent-shell-cockpit-ui--open-function))

(defun agent-shell-cockpit-preview ()
  "Preview the cockpit item at point without leaving the cockpit."
  (interactive)
  (unless agent-shell-cockpit-ui--preview-function
    (user-error "This buffer cannot preview items"))
  (when-let* ((buffer (funcall agent-shell-cockpit-ui--preview-function)))
    (agent-shell-cockpit-ui-show-right buffer nil)))

(defun agent-shell-cockpit-preview-next ()
  "Move to and preview the next cockpit item."
  (interactive)
  (agent-shell-cockpit-next)
  (agent-shell-cockpit-preview))

(defun agent-shell-cockpit-preview-previous ()
  "Move to and preview the previous cockpit item."
  (interactive)
  (agent-shell-cockpit-previous)
  (agent-shell-cockpit-preview))

(defun agent-shell-cockpit-ui-show-right (buffer select)
  "Show BUFFER to the right of the current window.
When SELECT is non-nil, select the preview window."
  (let* ((cockpit-window (selected-window))
         (width (if (integerp agent-shell-cockpit-width)
                    agent-shell-cockpit-width
                  (max window-min-width
                       (floor (* (window-total-width cockpit-window)
                                 agent-shell-cockpit-width)))))
         (target-window
          (or (window-in-direction 'right cockpit-window)
              (split-window cockpit-window width 'right))))
    (set-window-buffer target-window buffer)
    (when select (select-window target-window))
    target-window))

(defun agent-shell-cockpit-ui--timer-refresh (buffer)
  "Refresh visible cockpit BUFFER."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when agent-shell-cockpit-ui--refresh-function
        (funcall agent-shell-cockpit-ui--refresh-function)))))

(defun agent-shell-cockpit-ui--stop-timer ()
  "Stop the current cockpit refresh timer."
  (when (timerp agent-shell-cockpit-ui--refresh-timer)
    (cancel-timer agent-shell-cockpit-ui--refresh-timer)
    (setq agent-shell-cockpit-ui--refresh-timer nil)))

(defvar-keymap agent-shell-cockpit-ui-mode-map
  :doc "Shared keymap for cockpit views."
  "TAB" #'agent-shell-cockpit-open
  "RET" #'agent-shell-cockpit-open
  "C-j" #'agent-shell-cockpit-preview-next
  "C-k" #'agent-shell-cockpit-preview-previous
  "n" #'agent-shell-cockpit-next
  "p" #'agent-shell-cockpit-previous
  "r" #'agent-shell-cockpit-refresh
  "?" #'agent-shell-cockpit-help
  "q" #'quit-window)

(define-derived-mode agent-shell-cockpit-ui-mode special-mode "Agent-Cockpit"
  "Base mode for agent-shell cockpit views."
  (setq-local truncate-lines t cursor-type 'hbar buffer-read-only t)
  (hl-line-mode 1)
  (add-hook 'kill-buffer-hook #'agent-shell-cockpit-ui--stop-timer nil t)
  (agent-shell-cockpit-ui--stop-timer)
  (setq agent-shell-cockpit-ui--refresh-timer
        (when agent-shell-cockpit-refresh-interval
          (run-with-timer agent-shell-cockpit-refresh-interval
                          agent-shell-cockpit-refresh-interval
                          #'agent-shell-cockpit-ui--timer-refresh
                          (current-buffer)))))

(provide 'agent-shell-cockpit-ui)

;;; agent-shell-cockpit-ui.el ends here
