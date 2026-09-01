;;; agent-shell-cockpit-archive-view.el --- Archived Cockpit workspaces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render archived workspaces as a flat Magit-style history buffer.

;;; Code:

(require 'dired)
(require 'map)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-workspace)

(defvar agent-shell-cockpit--buffer)
(declare-function agent-shell-cockpit "agent-shell-cockpit-dashboard")

(defcustom agent-shell-cockpit-archive-buffer-name "*Cockpit Archives*"
  "Name of the Cockpit archive history buffer."
  :type 'string
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-archive-id
  '((t :inherit shadow))
  "Face for abbreviated archive UUIDs."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-archive-count
  '((t :inherit font-lock-constant-face))
  "Face for recorded-agent counts in archive rows."
  :group 'agent-shell-cockpit)

(defface agent-shell-cockpit-archive-date
  '((t :inherit shadow))
  "Face for relative dates in archive rows."
  :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-archive-view--time (workspace)
  "Return the archive time for WORKSPACE.
Use metadata modification time for archives created before timestamps were
stored explicitly."
  (or (when-let* ((seconds (map-elt workspace 'archivedAt)))
        (seconds-to-time seconds))
      (when-let* ((attributes
                   (file-attributes
                    (agent-shell-cockpit-store-metadata-path
                     (map-elt workspace 'root)))))
        (file-attribute-modification-time attributes))
      (seconds-to-time 0)))

(defun agent-shell-cockpit-archive-view--age (time)
  "Return a compact human-readable age for TIME."
  (let* ((seconds (max 0 (- (float-time) (float-time time))))
         (spec (cond
                ((< seconds 60) (list 1 "second"))
                ((< seconds 3600) (list 60 "minute"))
                ((< seconds 86400) (list 3600 "hour"))
                ((< seconds 604800) (list 86400 "day"))
                ((< seconds 2629800) (list 604800 "week"))
                ((< seconds 31557600) (list 2629800 "month"))
                (t (list 31557600 "year"))))
         (count (max 1 (round (/ seconds (car spec)))))
         (unit (cadr spec)))
    (format "%d %s%s" count unit (if (= count 1) "" "s"))))

(defun agent-shell-cockpit-archive-view--identifier (workspace)
  "Return a short stable identifier for archived WORKSPACE."
  (let ((directory
         (file-name-nondirectory
          (directory-file-name (map-elt workspace 'root)))))
    (if (string-match
         "--\\([[:xdigit:]]\\{8\\}\\)-[[:xdigit:]-]\\{27\\}\\'"
         directory)
        (match-string 1 directory)
      (substring (secure-hash 'sha1 directory) 0 8))))

(defun agent-shell-cockpit-archive-view--workspaces ()
  "Return archived workspaces sorted newest first."
  (sort (agent-shell-cockpit-store-discover t)
        (lambda (left right)
          (let ((left-time (agent-shell-cockpit-archive-view--time left))
                (right-time (agent-shell-cockpit-archive-view--time right)))
            (if (time-equal-p left-time right-time)
                (string-lessp
                 (or (map-elt left 'title) (map-elt left 'name))
                 (or (map-elt right 'title) (map-elt right 'name)))
              (time-less-p right-time left-time))))))

(defun agent-shell-cockpit-archive-view--insert-workspace (workspace)
  "Insert a flat history row for archived WORKSPACE."
  (let ((title (or (map-elt workspace 'title) (map-elt workspace 'name)))
        (root (map-elt workspace 'root))
        (invalid (eq (map-elt workspace 'kind) 'invalid)))
    (magit-insert-section
        (agent-shell-cockpit-section root nil
                                     :kind 'archived-workspace
                                     :object workspace)
      (insert (propertize
               (format "%-8s" (agent-shell-cockpit-archive-view--identifier
                               workspace))
               'font-lock-face 'agent-shell-cockpit-archive-id)
              " * "
              (propertize
               (agent-shell-cockpit-ui-one-line title 80)
               'font-lock-face (if invalid 'error 'default))
              (propertize " " 'display '(space :align-to (- right 24)))
              (propertize
               (if invalid
                   "invalid"
                 (format "%d agent%s"
                         (length (map-elt workspace 'sessions))
                         (if (= (length (map-elt workspace 'sessions)) 1)
                             "" "s")))
               'font-lock-face 'agent-shell-cockpit-archive-count)
              "  "
              (propertize
               (agent-shell-cockpit-archive-view--age
                (agent-shell-cockpit-archive-view--time workspace))
               'font-lock-face 'agent-shell-cockpit-archive-date)
              ?\n))))

(defun agent-shell-cockpit-archive-view--render ()
  "Render the archive history buffer."
  (let ((workspaces (agent-shell-cockpit-archive-view--workspaces)))
    (erase-buffer)
    (magit-insert-section
        (agent-shell-cockpit-section 'archives nil :kind 'root)
      (if workspaces
          (dolist (workspace workspaces)
            (agent-shell-cockpit-archive-view--insert-workspace workspace))
        (insert (propertize "No archived workspaces\n"
                            'face 'agent-shell-cockpit-secondary))))))

(defun agent-shell-cockpit-archive-view-refresh ()
  "Refresh the Cockpit archive history buffer."
  (interactive)
  (agent-shell-cockpit-ui-refresh-buffer
   #'agent-shell-cockpit-archive-view--render))

(defun agent-shell-cockpit-archive-view--selected-workspace ()
  "Return the archived workspace at point or signal a user error."
  (unless (eq (agent-shell-cockpit-ui-object-type-at-point)
              'archived-workspace)
    (user-error "Point is not on an archived workspace"))
  (agent-shell-cockpit-ui-object-at-point))

(defun agent-shell-cockpit-archive-view-open ()
  "Open the archived workspace directory at point."
  (interactive)
  (dired (map-elt (agent-shell-cockpit-archive-view--selected-workspace)
                  'root)))

(defun agent-shell-cockpit-archive-view-delete ()
  "Permanently delete the archived workspace at point after confirmation."
  (interactive)
  (let* ((workspace (agent-shell-cockpit-archive-view--selected-workspace))
         (name (or (map-elt workspace 'title) (map-elt workspace 'name)))
         (root (map-elt workspace 'root)))
    (when (yes-or-no-p
           (format (concat "Permanently delete archived workspace %s and "
                           "all files?  This cannot be undone. ")
                   name))
      (agent-shell-cockpit-workspace-delete-archive workspace)
      (when-let* ((detail (get-buffer (format "*Cockpit: %s*"
                                               (map-elt workspace 'name)))))
        (kill-buffer detail))
      (agent-shell-cockpit-archive-view-refresh)
      (message "Deleted archived workspace %s (%s)" name root))))

(defun agent-shell-cockpit-archive-view-back ()
  "Return to the Cockpit dashboard."
  (interactive)
  (if (buffer-live-p agent-shell-cockpit--buffer)
      (switch-to-buffer agent-shell-cockpit--buffer)
    (agent-shell-cockpit)))

(defun agent-shell-cockpit-archive-view--workspace-at-point-p ()
  "Return non-nil when point represents an archived workspace."
  (eq (agent-shell-cockpit-ui-object-type-at-point) 'archived-workspace))

(transient-define-prefix agent-shell-cockpit-archive-view-dispatch ()
  "Invoke a Cockpit archive command from the available commands."
  ["Archive commands"
   [("D" "Permanently delete workspace" agent-shell-cockpit-archive-view-delete
     :inapt-if-not agent-shell-cockpit-archive-view--workspace-at-point-p)
    ("b" "Return to dashboard" agent-shell-cockpit-archive-view-back)]]
  ["Essential commands"
   [("g" "       Refresh current buffer" agent-shell-cockpit-refresh)
    ("q" "       Bury current buffer" agent-shell-cockpit-quit)
    ("<return>" "Visit thing at point" agent-shell-cockpit-open
     :inapt-if-not agent-shell-cockpit-archive-view--workspace-at-point-p)]
   [("n" "       Next section" agent-shell-cockpit-next)
    ("p" "       Previous section" agent-shell-cockpit-previous)
    ("C-x m" "Show all key bindings" describe-mode)]])

(defvar-keymap agent-shell-cockpit-archive-view-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "D" #'agent-shell-cockpit-archive-view-delete
  "b" #'agent-shell-cockpit-archive-view-back)

(define-derived-mode agent-shell-cockpit-archive-view-mode
  agent-shell-cockpit-ui-mode "Cockpit-Archives"
  "Major mode for archived Cockpit workspace history."
  (setq-local agent-shell-cockpit-ui--refresh-function
              #'agent-shell-cockpit-archive-view-refresh
              agent-shell-cockpit-ui--open-function
              #'agent-shell-cockpit-archive-view-open
              agent-shell-cockpit-ui--dispatch-function
              #'agent-shell-cockpit-archive-view-dispatch))

(defun agent-shell-cockpit-archives ()
  "Open the Cockpit archive history buffer."
  (interactive)
  (let ((buffer (get-buffer-create agent-shell-cockpit-archive-buffer-name)))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'agent-shell-cockpit-archive-view-mode)
      (agent-shell-cockpit-archive-view-mode))
    (agent-shell-cockpit-archive-view-refresh)))

(provide 'agent-shell-cockpit-archive-view)

;;; agent-shell-cockpit-archive-view.el ends here
