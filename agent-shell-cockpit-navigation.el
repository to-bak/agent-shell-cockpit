;;; agent-shell-cockpit-navigation.el --- Workspace switching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Flat workspace switching and contextual entry into Cockpit.

;;; Code:

(require 'agent-shell-cockpit-workspace-view)
(require 'agent-shell-cockpit-agents-view)
(require 'transient)
(require 'savehist)

(defvar agent-shell-cockpit-recent-workspaces nil
  "Workspace roots in most recently visited order.")
(add-to-list 'savehist-additional-variables 'agent-shell-cockpit-recent-workspaces)

(defun agent-shell-cockpit-create-workspace ()
  "Create a workspace and open its view."
  (interactive)
  (agent-shell-cockpit-workspace-view
   (agent-shell-cockpit-workspace-create :name (read-string "Workspace directory name: "))))

(defun agent-shell-cockpit-navigation--invalid-workspace ()
  "Select invalid workspace metadata, including archived workspaces."
  (let* ((records (seq-filter (lambda (record) (eq (map-elt record 'kind) 'invalid))
                              (append (agent-shell-cockpit-store-discover)
                                      (agent-shell-cockpit-store-discover t))))
         (choices (mapcar (lambda (record)
                           (cons (format "%s — %s" (map-elt record 'root) (map-elt record 'error)) record))
                         records)))
    (unless choices (user-error "No invalid workspace metadata"))
    (cdr (assoc (completing-read "Workspace metadata: " choices nil t) choices))))

(defun agent-shell-cockpit-inspect-metadata ()
  "Visit an invalid workspace's metadata for manual inspection."
  (interactive)
  (find-file (agent-shell-cockpit-store-metadata-path
              (map-elt (agent-shell-cockpit-navigation--invalid-workspace) 'root))))

(defun agent-shell-cockpit-repair-workspace ()
  "Select and repair invalid workspace metadata after confirmation."
  (interactive)
  (let ((record (agent-shell-cockpit-navigation--invalid-workspace)))
    (when (yes-or-no-p
           (format "Back up and rebuild %s without repository/session history? " (map-elt record 'root)))
      (agent-shell-cockpit-workspace-repair record)
      (agent-shell-cockpit-workspace-view (agent-shell-cockpit-store-read (map-elt record 'root))))))

(defun agent-shell-cockpit-navigation-workspaces ()
  "Return valid active workspaces, most recently visited first."
  (let ((records (seq-filter (lambda (record) (eq (map-elt record 'kind) 'workspace))
                             (agent-shell-cockpit-store-discover))))
    (sort records
          (lambda (left right)
            (< (or (cl-position (map-elt left 'root) agent-shell-cockpit-recent-workspaces :test #'equal)
                   most-positive-fixnum)
               (or (cl-position (map-elt right 'root) agent-shell-cockpit-recent-workspaces :test #'equal)
                   most-positive-fixnum))))))

(defun agent-shell-cockpit-switch-workspace ()
  "Select a workspace directly using completion."
  (interactive)
  (let* ((workspaces (agent-shell-cockpit-navigation-workspaces))
         (choices (mapcar (lambda (workspace)
                           (cons (format "%s (%s)" (map-elt workspace 'title)
                                         (map-elt workspace 'name)) workspace)) workspaces))
         (completion-extra-properties
          `(:display-sort-function identity
            :annotation-function
            ,(lambda (name)
               (let* ((workspace (cdr (assoc name choices)))
                      (agents (agent-shell-cockpit-session-live-buffers workspace))
                      (waiting (seq-count
                                (lambda (buffer)
                                  (eq (agent-shell-cockpit-session-status buffer) 'attention)) agents)))
                 (format "  %d agents · %d waiting" (length agents) waiting))))))
    (if choices
        (agent-shell-cockpit-workspace-view
         (cdr (assoc (completing-read "Workspace: " choices nil t) choices)))
      (call-interactively #'agent-shell-cockpit-create-workspace))))

(defun agent-shell-cockpit-navigation-open ()
  "Open the contextual workspace, a recent workspace, or the picker."
  (let* ((workspaces (agent-shell-cockpit-navigation-workspaces))
         (associated (or agent-shell-cockpit-session-workspace-root
                         (and (boundp 'agent-shell-cockpit-workspace-view--root)
                              agent-shell-cockpit-workspace-view--root)))
         (workspace
          (or (seq-find (lambda (record) (equal associated (map-elt record 'root))) workspaces)
              (seq-find (lambda (record) (file-in-directory-p default-directory (map-elt record 'root))) workspaces)
              (seq-find (lambda (record) (member (map-elt record 'root) agent-shell-cockpit-recent-workspaces)) workspaces))))
    (if workspace (agent-shell-cockpit-workspace-view workspace)
      (agent-shell-cockpit-switch-workspace))))

(transient-define-prefix agent-shell-cockpit-workspace-dispatch ()
  "Switch, create, or maintain workspaces."
  [[("b" "Switch workspace" agent-shell-cockpit-switch-workspace)
    ("c" "Create workspace" agent-shell-cockpit-create-workspace)
    ("a" "All agents" agent-shell-cockpit-all-agents)
    ("l" "Archives" agent-shell-cockpit-archives)]
   [("s" "Start standalone agent" agent-shell-cockpit-start-agent)
    ("+" "Attach unassigned agent" agent-shell-cockpit-attach-session)
    ("i" "Inspect invalid metadata" agent-shell-cockpit-inspect-metadata)
    ("E" "Repair invalid metadata" agent-shell-cockpit-repair-workspace)]])

(provide 'agent-shell-cockpit-navigation)
;;; agent-shell-cockpit-navigation.el ends here
