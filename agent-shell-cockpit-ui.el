;;; agent-shell-cockpit-ui.el --- Shared cockpit user interface -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared faces, Magit-style navigation, and timers.

;;; Code:

(require 'map)
(require 'magit-section)
(require 'subr-x)
(require 'transient)
(require 'nerd-icons nil t)

(declare-function agent-shell-cockpit-store-archive-directory
                  "agent-shell-cockpit-store")

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

(defcustom agent-shell-cockpit-agent-preview-lines 12
  "Maximum number of recent agent-buffer lines shown in an expanded row."
  :type 'natnum
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-title
  '((t :inherit magit-section-heading))
  "Face for Cockpit buffer titles."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-brand
  '((t :inherit magit-section-heading :weight bold
       :box (:line-width (1 . -1))))
  "Face for the Cockpit badge in buffer header lines."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-heading '((t :inherit magit-section-heading))
  "Face for cockpit headings."
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

(defface agent-shell-cockpit-prompt-preview
  '((t :inherit (fixed-pitch magit-section-highlight) :extend t))
  "Face for expanded prompt file contents."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-agent-preview
  '((((class color) (background dark))
     :inherit fixed-pitch :background "#3c3836" :extend t)
    (((class color) (background light))
     :inherit fixed-pitch :background "#f0f1f2" :extend t)
    (t :inherit (fixed-pitch magit-section-highlight) :extend t))
  "Face for an expanded live agent-buffer snapshot."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-agent-preview-heading
  '((((class color) (background dark))
     :background "#504945" :weight semi-bold :extend t)
    (((class color) (background light))
     :background "#d8dee4" :weight semi-bold :extend t)
    (t :inherit magit-section-heading :extend t))
  "Face for the heading above a live agent-buffer snapshot."
  :group 'agent-shell-cockpit)

(defconst agent-shell-cockpit-ui--status-spec
  '((attention "attention" agent-shell-cockpit-status-attention 0)
    (working "working" agent-shell-cockpit-status-working 1)
    (ready "ready" agent-shell-cockpit-status-ready 2)
    (idle "idle" agent-shell-cockpit-status-muted 3)
    (history "history" agent-shell-cockpit-status-muted 4)
    (starting "starting" agent-shell-cockpit-status-muted 5)
    (invalid "invalid" agent-shell-cockpit-status-attention 6)))

(defvar-local agent-shell-cockpit-ui--refresh-function nil)
(defvar-local agent-shell-cockpit-ui--open-function nil)
(defvar-local agent-shell-cockpit-ui--dispatch-function nil)
(defvar-local agent-shell-cockpit-ui--refresh-timer nil)

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

(defun agent-shell-cockpit-help ()
  "Show the command dispatcher for the current Cockpit view."
  (interactive)
  (agent-shell-cockpit-dispatch))

(defun agent-shell-cockpit-ui-status-label (status)
  "Return a compact colored text label for STATUS."
  (let ((spec (assq status agent-shell-cockpit-ui--status-spec)))
    (propertize (format "  ● %s" (or (nth 1 spec) "unknown"))
                'font-lock-face
                (or (nth 2 spec) 'agent-shell-cockpit-status-muted))))

(defun agent-shell-cockpit-ui-header-context ()
  "Return the short context displayed in the Cockpit header line."
  (cond
   ((derived-mode-p 'agent-shell-cockpit-archive-view-mode)
    (format "Archives in %s"
            (abbreviate-file-name
             (agent-shell-cockpit-store-archive-directory))))
   ((derived-mode-p 'agent-shell-cockpit-workspace-view-mode)
    (format "Workspace · %s"
            (string-remove-prefix
             "Cockpit: " (string-trim (buffer-name) "\\*+" "\\*+"))))
   (t "Dashboard")))

(defun agent-shell-cockpit-ui-insert-header (label value)
  "Insert a Magit-style header line with LABEL and VALUE."
  (insert (propertize (format "%-11s" (concat label ":"))
                      'face 'magit-section-heading)
          (agent-shell-cockpit-ui-one-line value) "\n"))

(defun agent-shell-cockpit-ui-status-rank (status)
  "Return display rank for STATUS."
  (or (nth 3 (assq status agent-shell-cockpit-ui--status-spec)) 99))

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
    ('prompt
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

(defun agent-shell-cockpit-ui--buffer-tail (buffer)
  "Return a safely styled recent tail of live BUFFER.
Only `font-lock-face' is copied; interactive and internal buffer text
properties are deliberately discarded."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward "\n\r\t ")
          (let ((end (point)))
            (forward-line (- agent-shell-cockpit-agent-preview-lines))
            (let* ((source (buffer-substring (point) end))
                   (text (substring-no-properties source))
                   (position 0)
                   (length (length source)))
              (while (< position length)
                (let ((next (or (next-single-property-change
                                 position 'font-lock-face source)
                                length))
                      (face (get-text-property
                             position 'font-lock-face source)))
                  (when face
                    (put-text-property
                     position next 'font-lock-face face text))
                  (setq position next)))
              (string-trim text))))))))

(defun agent-shell-cockpit-ui--add-preview-background (start end)
  "Add the Cockpit preview background from START to END.
Preserve any existing agent-shell foreground and emphasis faces."
  (let ((position start))
    (while (< position end)
      (let* ((existing (get-text-property position 'font-lock-face))
             (next (or (next-single-property-change
                        position 'font-lock-face nil end)
                       end))
             (combined
              (cond
               ((null existing) 'agent-shell-cockpit-agent-preview)
               ((and (listp existing) (not (keywordp (car existing))))
                (cons 'agent-shell-cockpit-agent-preview existing))
               (t (list 'agent-shell-cockpit-agent-preview existing)))))
        (put-text-property position next 'font-lock-face combined)
        (setq position next)))))

(defun agent-shell-cockpit-ui-insert-agent-preview (buffer)
  "Insert a bounded snapshot of live agent BUFFER."
  (let ((heading-start (point)))
    (insert "  Preview" ?\n)
    (add-text-properties
     heading-start (point)
     '(font-lock-face agent-shell-cockpit-agent-preview-heading)))
  (let ((text (agent-shell-cockpit-ui--buffer-tail buffer))
        (start (point)))
    (insert (if (string-empty-p (or text "")) "No output yet" text) ?\n)
    (indent-rigidly start (point) 2)
    (agent-shell-cockpit-ui--add-preview-background start (point))))

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

(defun agent-shell-cockpit-ui-capture-position (&optional position)
  "Capture POSITION so it can be restored after rendering.
The offset within a navigable row is retained as well as its identity."
  (let* ((position (min (or position (point)) (point-max)))
         (section (magit-section-at position)))
    (list :point position
          :section-ident (and section (magit-section-ident section))
          :section-offset (and section (- position (oref section start))))))

(defun agent-shell-cockpit-ui-restore-position (state)
  "Restore point from captured position STATE after rendering."
  (let* ((ident (plist-get state :section-ident))
         (section (and ident magit-root-section
                       (magit-get-section ident))))
    (cond
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
  (let* ((window (get-buffer-window (current-buffer) t))
         (saved-position (if (window-live-p window)
                             (window-point window)
                           (point)))
         (saved (agent-shell-cockpit-ui-capture-position saved-position))
         (inhibit-read-only t))
    (funcall render-function)
    ;; This is also how Magit's refresh lifecycle installs the configured
    ;; expand/collapse indicators and reapplies each child's visibility.
    (let ((magit-section-cache-visibility nil))
      (magit-section-show magit-root-section))
    (agent-shell-cockpit-ui-restore-position saved)
    (when (window-live-p window)
      (set-window-point window (point)))))

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
  "Close the Cockpit view."
  (interactive)
  (quit-window))

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
  :doc "Shared keymap for Magit-style Cockpit views."
  :parent magit-section-mode-map
  "RET" #'agent-shell-cockpit-open
  "TAB" #'agent-shell-cockpit-toggle-section
  ;; These come from `magit-section-mode-map', but Cockpit only uses the
  ;; single-section `TAB' toggle and never cycles the whole section tree.
  "<backtab>" #'ignore
  "C-c TAB" #'ignore
  "C-<tab>" #'ignore
  "M-<tab>" #'ignore
  "<down>" #'agent-shell-cockpit-next
  "<up>" #'agent-shell-cockpit-previous
  "C-n" #'agent-shell-cockpit-next
  "C-p" #'agent-shell-cockpit-previous
  "n" #'agent-shell-cockpit-next
  "p" #'agent-shell-cockpit-previous
  "g" #'agent-shell-cockpit-refresh
  "?" #'agent-shell-cockpit-dispatch
  "q" #'agent-shell-cockpit-quit)

(define-derived-mode agent-shell-cockpit-ui-mode magit-section-mode
  "Agent-Cockpit"
  "Base mode for agent-shell cockpit views."
  (setq-local truncate-lines t buffer-read-only t
              magit-section-preserve-visibility nil
              header-line-format agent-shell-cockpit-ui-header-line-format)
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
