;;; agent-shell-cockpit-consult.el --- Optional instruction previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Set `agent-shell-cockpit-instructions-read-function' to
;; `agent-shell-cockpit-consult-read' to opt into ordered Consult selection.
;; Previews render literals and references, never referenced file contents.

;;; Code:

(require 'agent-shell-cockpit-instructions)

(declare-function consult--read "consult")

(defun agent-shell-cockpit-consult-read (&optional workspace single)
  "Select ordered instructions for WORKSPACE with previews.
When SINGLE is non-nil, select one instruction for visiting instead.
RET adds a candidate; Done accepts the ordered list, Remove last undoes
the last addition, and Clear removes all selections.  Quit cancels."
  (unless (require 'consult nil t)
    (user-error "Install Consult to use the Cockpit Consult picker"))
  (let* ((catalog (agent-shell-cockpit-instructions--catalog workspace))
         (selected (unless single
                     (seq-filter (lambda (id) (assq id catalog))
                                 agent-shell-cockpit-default-instructions)))
         (preview (generate-new-buffer " *Cockpit instruction preview*"))
         (window (selected-window))
         done)
    (unwind-protect
        (save-window-excursion
          (while (and catalog (not done))
            (let* ((candidates
                    (mapcar (lambda (entry)
                              (cons (format "%s — %s" (car entry)
                                            (plist-get (cdr entry) :title))
                                    (car entry)))
                            (seq-remove (lambda (entry) (memq (car entry) selected)) catalog)))
                   (choice
                    (consult--read
                     (append (unless single '("Done" "Remove last" "Clear"))
                             (mapcar #'car candidates))
                     :prompt (if single "Visit instruction: "
                               (format "Instructions [%s]: "
                                       (mapconcat #'symbol-name selected " → ")))
                     :require-match t :sort nil
                     :preview-key '(:debounce 0.15 any)
                     :state
                     (lambda (action candidate)
                       (when (and (eq action 'preview) candidate (window-live-p window))
                         (let* ((id (cdr (assoc candidate candidates)))
                                (ids (cond (single (and id (list id)))
                                           ((equal candidate "Clear") nil)
                                           ((equal candidate "Remove last") (butlast selected))
                                           (id (append selected (list id)))
                                           (t selected))))
                           (with-current-buffer preview
                             (let ((inhibit-read-only t))
                               (erase-buffer)
                               (insert (condition-case err
                                           (or (agent-shell-cockpit-instructions-render ids workspace)
                                               "No instructions selected.")
                                         (error (format "Preview unavailable: %s"
                                                        (error-message-string err)))))
                               (goto-char (point-min)))
                             (special-mode))
                           (set-window-buffer window preview)))))))
              (cond ((equal choice "Done") (setq done t))
                    ((equal choice "Remove last") (setq selected (butlast selected)))
                    ((equal choice "Clear") (setq selected nil))
                    (t (setq selected (append selected (list (cdr (assoc choice candidates))))
                             done single)))))
          selected)
      (kill-buffer preview))))

(provide 'agent-shell-cockpit-consult)
;;; agent-shell-cockpit-consult.el ends here
