;;; agent-shell-cockpit-ui.el --- Shared cockpit user interface -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared faces, Magit-style navigation, and timers.

;;; Code:

(require 'map)
(require 'seq)

(defvar agent-shell-cockpit-ui--event-timer nil)
(require 'magit-section)
(require 'subr-x)
(require 'transient)
(require 'nerd-icons nil t)

(declare-function agent-shell-cockpit-store-archive-directory
                  "agent-shell-cockpit-store")
(declare-function agent-shell-cockpit-agent-preview-close
                  "agent-shell-cockpit-agent")

(defcustom agent-shell-cockpit-buffer-name "*Agent Shell Cockpit*"
  "Name of the cockpit dashboard buffer."
  :type 'string
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-refresh-interval 2
  "Seconds between visible cockpit refreshes, or nil to disable."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-summary-width 72
  "Maximum display width of a Cockpit row heading."
  :type 'integer
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-brand
  '((t :inherit magit-section-heading :weight bold
       :box (:line-width (1 . -1))))
  "Face for the Cockpit badge in buffer header lines."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-secondary '((t :inherit shadow))
  "Face for secondary cockpit text."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-attention
  '((((class color) (background dark))
     :foreground "#ff6c6b" :weight bold)
    (((class color) (background light))
     :foreground "#b00020" :weight bold)
    (t :inherit error :weight bold))
  "Face for items needing attention."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-working
  '((((class color) (background dark))
     :foreground "#ECBE7B" :weight semi-bold)
    (((class color) (background light))
     :foreground "#9a6700" :weight semi-bold)
    (t :inherit warning))
  "Face for working items."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-ready
  '((((class color) (background dark))
     :foreground "#98be65" :weight semi-bold)
    (((class color) (background light))
     :foreground "#1a7f37" :weight semi-bold)
    (t :inherit success :weight semi-bold))
  "Face for ready items."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-status-muted
  '((((class color) (background dark)) :foreground "#7f849c")
    (((class color) (background light)) :foreground "#6e7781")
    (t :inherit shadow))
  "Face for idle and unknown items."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-context-preview
  '((t :inherit (fixed-pitch magit-section-highlight) :extend t))
  "Face for expanded context file contents."
  :group 'agent-shell-cockpit)

(defconst agent-shell-cockpit-ui--status-spec
  '((attention "attention" agent-shell-cockpit-status-attention)
    (working "working" agent-shell-cockpit-status-working)
    (ready "ready" agent-shell-cockpit-status-ready)
    (idle "idle" agent-shell-cockpit-status-muted)
    (history "history" agent-shell-cockpit-status-muted)
    (starting "starting" agent-shell-cockpit-status-muted)
    (invalid "invalid" agent-shell-cockpit-status-attention)))

(defvar-local agent-shell-cockpit-ui--refresh-function nil)
(defvar-local agent-shell-cockpit-ui--open-function nil)
(defvar-local agent-shell-cockpit-ui--dispatch-function nil)
(defvar-local agent-shell-cockpit-ui--refresh-timer nil)
(defvar-local agent-shell-cockpit-ui-return-buffer nil
  "Logical parent buffer returned to when quitting the current Cockpit view.")

(defconst agent-shell-cockpit-ui-header-line-format
  '(" "
    (:eval (propertize " COCKPIT " 'face 'agent-shell-cockpit-brand))
    "  "
    (:eval
     (propertize (agent-shell-cockpit-ui-header-context)
                 'face 'agent-shell-cockpit-secondary))
    "  "
    (:eval (propertize "? commands" 'face 'agent-shell-cockpit-secondary)))
  "Header line shared by all Cockpit views.")

(defclass agent-shell-cockpit-section (magit-section)
  ((kind :initarg :kind :initform nil)
   (object :initarg :object :initform nil))
  "Section representing a Cockpit group or domain object.")

(defun agent-shell-cockpit-ui-status-label (status)
  "Return a compact colored text label for STATUS."
  (let ((spec (assq status agent-shell-cockpit-ui--status-spec)))
    (propertize (format "  ● %s" (or (nth 1 spec) "unknown"))
                'font-lock-face
                (or (nth 2 spec) 'agent-shell-cockpit-status-muted))))

(defvar-local agent-shell-cockpit-ui-workspace-heading nil
  "Workspace identity retained in the fixed header line.")

(defun agent-shell-cockpit-ui-header-context ()
  "Return the short context displayed in the Cockpit header line."
  (cond
   ((derived-mode-p 'agent-shell-cockpit-archive-view-mode)
    (format "Archives in %s"
            (abbreviate-file-name
             (agent-shell-cockpit-store-archive-directory))))
   ((derived-mode-p 'agent-shell-cockpit-workspace-view-mode)
    (or agent-shell-cockpit-ui-workspace-heading
        (format "Workspace · %s"
                (string-remove-prefix
                 "Cockpit: " (string-trim (buffer-name) "\\*+" "\\*+")))))
   (t "Dashboard")))

(defun agent-shell-cockpit-ui-insert-header (label value)
  "Insert a Magit-style header line with LABEL and VALUE."
  (insert (propertize (format "%-11s" (concat label ":"))
                      'face 'magit-section-heading)
          (agent-shell-cockpit-ui-one-line value) "\n"))

(defun agent-shell-cockpit-ui-one-line (value &optional width)
  "Return VALUE as one line, truncated to WIDTH display columns.
WIDTH defaults to `agent-shell-cockpit-summary-width'."
  (let* ((limit (or width agent-shell-cockpit-summary-width))
         (text (string-trim
                (replace-regexp-in-string
                 "[[:space:]\n\r]+" " " (format "%s" (or value ""))))))
    (if (> (string-width text) limit)
        (concat (truncate-string-to-width text (1- limit)) "…")
      text)))

(defun agent-shell-cockpit-ui-icon (kind)
  "Return a compact icon representing KIND.
Use Nerd Icons when available, with portable glyphs as a fallback."
  (pcase kind
    ('agent
     (if (fboundp 'nerd-icons-mdicon)
         (nerd-icons-mdicon "nf-md-robot"
                            :face 'agent-shell-cockpit-secondary)
       "●"))
    ('context
     (if (fboundp 'nerd-icons-mdicon)
         (nerd-icons-mdicon "nf-md-file_document_edit_outline"
                            :face 'agent-shell-cockpit-secondary)
       "✎"))
    ('repository
     (if (fboundp 'nerd-icons-codicon)
         (nerd-icons-codicon "nf-cod-git_merge"
                             :face 'agent-shell-cockpit-secondary)
       "⑂"))
    (_ "•")))

(defun agent-shell-cockpit-ui-insert-detail (label value)
  "Insert an indented detail line with LABEL and VALUE."
  (insert "  "
          (propertize (format "%-12s" (concat label ":"))
                      'face 'agent-shell-cockpit-secondary)
          (agent-shell-cockpit-ui-one-line value 120)
          "\n"))

(defun agent-shell-cockpit-ui-object-at-point ()
  "Return the cockpit object represented at point."
  (let ((section (magit-current-section)))
    (when (cl-typep section 'agent-shell-cockpit-section)
      (oref section object))))

(defun agent-shell-cockpit-ui-object-type-at-point ()
  "Return the cockpit object type represented at point."
  (let ((section (magit-current-section)))
    (when (cl-typep section 'agent-shell-cockpit-section)
      (oref section kind))))

(defun agent-shell-cockpit-ui-capture-position (&optional position keep-header)
  "Capture POSITION so it can be restored after rendering.
The offset within a navigable row is retained as well as its identity.
KEEP-HEADER preserves positions before the first section for window scrolling."
  (let* ((position (min (or position (point)) (point-max)))
         (section (magit-section-at position)))
    (list :header-offset (and keep-header
                              (or (null section) (eq section magit-root-section))
                              (- position (point-min)))
          :section-ident (and section (magit-section-ident section))
          :section-offset (and section (- position (oref section start))))))

(defun agent-shell-cockpit-ui-restore-position (state)
  "Restore point from captured position STATE after rendering."
  (let* ((ident (plist-get state :section-ident))
         (section (and ident magit-root-section
                       (magit-get-section ident))))
    (cond
     ((plist-get state :header-offset)
      (goto-char (min (point-max) (+ (point-min) (plist-get state :header-offset)))))
     (section
      (goto-char (min (+ (oref section start)
                         (or (plist-get state :section-offset) 0))
                      (max (oref section start) (1- (oref section end))))))
     ((and magit-root-section (oref magit-root-section children))
      (goto-char (oref (car (oref magit-root-section children)) start)))
     (t (goto-char (point-min))))))

(defun agent-shell-cockpit-ui-refresh-buffer (render-function)
  "Refresh the current buffer using RENDER-FUNCTION.
Preserve section visibility and the position of point."
  (let ((restriction (when (buffer-narrowed-p)
                       (list (agent-shell-cockpit-ui-capture-position (point-min))
                             (agent-shell-cockpit-ui-capture-position
                              (max (point-min) (1- (point-max))))))))
    (save-restriction
      (widen)
      (let* ((saved (agent-shell-cockpit-ui-capture-position))
             (windows (mapcar (lambda (window)
                                (list window
                                      (agent-shell-cockpit-ui-capture-position (window-point window))
                                      (agent-shell-cockpit-ui-capture-position (window-start window) t)
                                      (window-hscroll window)))
                              (get-buffer-window-list (current-buffer) nil t)))
             (inhibit-read-only t))
        (funcall render-function)
        (let ((magit-section-cache-visibility nil))
          (magit-section-show magit-root-section))
        (dolist (state windows)
          (when (window-live-p (car state))
            (agent-shell-cockpit-ui-restore-position (nth 2 state))
            (set-window-start (car state) (point) t)
            (agent-shell-cockpit-ui-restore-position (nth 1 state))
            (set-window-point (car state) (point))
            (set-window-hscroll (car state) (nth 3 state))))
        (agent-shell-cockpit-ui-restore-position saved)))
    (when restriction
      (save-excursion
        (widen)
        (agent-shell-cockpit-ui-restore-position (car restriction))
        (let ((start (point)))
          (agent-shell-cockpit-ui-restore-position (cadr restriction))
          (narrow-to-region start (max start (min (point-max) (1+ (point))))))))))

(defun agent-shell-cockpit-ui-goto-first-row ()
  "Move point to the first top-level Cockpit section."
  (when-let* ((section (and magit-root-section
                            (car (oref magit-root-section children)))))
    (goto-char (oref section start))))

(defun agent-shell-cockpit-first ()
  "Move point to the first navigable cockpit row."
  (interactive)
  (agent-shell-cockpit-ui-goto-first-row))

(defun agent-shell-cockpit-last ()
  "Move point to the last visible Cockpit section."
  (interactive)
  (when magit-root-section
    (let ((section magit-root-section))
      (while (and (not (oref section hidden)) (oref section children))
        (setq section (car (last (oref section children)))))
      (unless (eq section magit-root-section)
        (goto-char (oref section start))))))

(defun agent-shell-cockpit-next ()
  "Move to the next visible Cockpit section."
  (interactive)
  (magit-section-forward)
  (agent-shell-cockpit-ui-object-at-point))

(defun agent-shell-cockpit-previous ()
  "Move to the previous visible Cockpit section."
  (interactive)
  (magit-section-backward)
  (agent-shell-cockpit-ui-object-at-point))

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

(defun agent-shell-cockpit-toggle-section ()
  "Toggle the section at point when it has expandable content."
  (interactive)
  (let ((section (magit-current-section)))
    (when (and section (oref section content))
      (magit-section-toggle section))))

(defun agent-shell-cockpit-dispatch ()
  "Show the Magit-style command dispatcher for the current view."
  (interactive)
  (unless agent-shell-cockpit-ui--dispatch-function
    (user-error "This Cockpit view has no dispatcher"))
  (funcall agent-shell-cockpit-ui--dispatch-function))

(defun agent-shell-cockpit-quit ()
  "Return to the logical parent of the current Cockpit view."
  (interactive)
  (when (fboundp 'agent-shell-cockpit-agent-preview-close)
    (agent-shell-cockpit-agent-preview-close))
  (if (and (buffer-live-p agent-shell-cockpit-ui-return-buffer)
           (not (eq agent-shell-cockpit-ui-return-buffer
                    (current-buffer))))
      (switch-to-buffer agent-shell-cockpit-ui-return-buffer)
    (quit-window)))

(defun agent-shell-cockpit-ui--timer-refresh (buffer)
  "Refresh visible cockpit BUFFER."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when agent-shell-cockpit-ui--refresh-function
        (condition-case err
            (funcall agent-shell-cockpit-ui--refresh-function)
          (error (message "Cockpit refresh: %s" (error-message-string err))))))))

(defun agent-shell-cockpit-ui--stop-timer ()
  "Stop the current cockpit refresh timer."
  (when (timerp agent-shell-cockpit-ui--refresh-timer)
    (cancel-timer agent-shell-cockpit-ui--refresh-timer)
    (setq agent-shell-cockpit-ui--refresh-timer nil))
  (unless (seq-some (lambda (buffer)
                      (and (not (eq buffer (current-buffer)))
                           (with-current-buffer buffer (derived-mode-p 'agent-shell-cockpit-ui-mode))))
                    (buffer-list))
    (when (timerp agent-shell-cockpit-ui--event-timer)
      (cancel-timer agent-shell-cockpit-ui--event-timer)
      (setq agent-shell-cockpit-ui--event-timer nil))))

(defvar-keymap agent-shell-cockpit-ui-mode-map
  :doc "Shared keymap for Magit-style Cockpit views."
  :parent magit-section-mode-map
  "RET" #'agent-shell-cockpit-open
  "TAB" #'agent-shell-cockpit-toggle-section
  "M-<" #'agent-shell-cockpit-first
  "M->" #'agent-shell-cockpit-last
  ;; These come from `magit-section-mode-map', but Cockpit only uses the
  ;; single-section `TAB' toggle and never cycles the whole section tree.
  "<backtab>" #'ignore
  "C-c TAB" #'ignore
  "C-<tab>" #'ignore
  "M-<tab>" #'ignore
  "<down>" #'agent-shell-cockpit-next
  "<up>" #'agent-shell-cockpit-previous
  "C-n" #'next-line
  "C-p" #'previous-line
  "n" #'agent-shell-cockpit-next
  "p" #'agent-shell-cockpit-previous
  "r" #'agent-shell-cockpit-refresh
  "?" #'agent-shell-cockpit-dispatch
  "q" #'agent-shell-cockpit-quit)

(define-derived-mode agent-shell-cockpit-ui-mode magit-section-mode
  "Agent-Cockpit"
  "Base mode for agent-shell cockpit views."
  (setq-local truncate-lines t buffer-read-only t
              magit-section-preserve-visibility nil
              header-line-format agent-shell-cockpit-ui-header-line-format)
  (setq-local revert-buffer-function (lambda (&rest _) (agent-shell-cockpit-refresh)))
  (add-hook 'change-major-mode-hook #'agent-shell-cockpit-ui--stop-timer nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-cockpit-ui--stop-timer nil t)
  (agent-shell-cockpit-ui--stop-timer)
  (setq agent-shell-cockpit-ui--refresh-timer
        (when agent-shell-cockpit-refresh-interval
          (run-with-timer agent-shell-cockpit-refresh-interval
                          agent-shell-cockpit-refresh-interval
                          #'agent-shell-cockpit-ui--timer-refresh
                          (current-buffer)))))

(defun agent-shell-cockpit-ui--refresh-visible ()
  "Refresh visible Cockpit views after a burst of state changes."
  (setq agent-shell-cockpit-ui--event-timer nil)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer (derived-mode-p 'agent-shell-cockpit-ui-mode))
      (agent-shell-cockpit-ui--timer-refresh buffer))))

(defun agent-shell-cockpit-ui-schedule-refresh (&rest _)
  "Coalesce session and Git events into a visible-view refresh."
  (when (and (not (timerp agent-shell-cockpit-ui--event-timer))
             (seq-some (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (derived-mode-p 'agent-shell-cockpit-ui-mode)))
                       (mapcan (lambda (frame) (window-list frame 'nomini)) (frame-list))))
    (setq agent-shell-cockpit-ui--event-timer
          (run-with-idle-timer 0.1 nil #'agent-shell-cockpit-ui--refresh-visible))))

(add-hook 'agent-shell-cockpit-session-change-hook #'agent-shell-cockpit-ui-schedule-refresh)

(provide 'agent-shell-cockpit-ui)

;;; agent-shell-cockpit-ui.el ends here
