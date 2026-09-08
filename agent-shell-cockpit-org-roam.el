;;; agent-shell-cockpit-org-roam.el --- Optional Org-roam instructions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Explicitly require this module to register the org-roam instruction adapter.
;; Core Cockpit never loads it.  Org-roam is required only when using it.

;;; Code:

(require 'agent-shell-cockpit-instructions)

(defvar org-roam-directory)
(declare-function org-roam-node-from-id "org-roam-node")
(declare-function org-roam-node-visit "org-roam-node")

(defun agent-shell-cockpit-org-roam--load ()
  "Load the optional Org-roam dependency or report a useful error."
  (unless (require 'org-roam nil t)
    (user-error "Install Org-roam to use its Cockpit instruction adapter")))

(defun agent-shell-cockpit-org-roam--reference (source _workspace)
  "Return the stable Org-roam ID reference described by SOURCE."
  (agent-shell-cockpit-org-roam--load)
  (unless (and (= (length source) 2) (stringp (cadr source)))
    (user-error "Org-roam instructions require one node ID string"))
  (concat "id:" (agent-shell-cockpit-instructions--text (cadr source))))

(defun agent-shell-cockpit-org-roam--bootstrap (_workspace)
  "Describe how to resolve nodes in the configured Org-roam directory."
  (agent-shell-cockpit-org-roam--load)
  (format
   (concat "Org-roam directory: %s\n"
           "Resolve id: links by searching this directory's Org files for the exact :ID: property. "
           "Read the containing entry, or the whole file for a file-level ID. "
           "Link labels are descriptive, not filenames. Follow relevant id: and file: links; "
           "resolve relative file links from the containing file. "
           "Do not modify instruction sources unless explicitly asked. "
           "If an ID cannot be found, report it rather than guessing.")
   (expand-file-name org-roam-directory)))

(defun agent-shell-cockpit-org-roam--visit (source workspace)
  "Visit the Org-roam node described by SOURCE in WORKSPACE."
  (agent-shell-cockpit-org-roam--reference source workspace)
  (let ((node (org-roam-node-from-id (cadr source))))
    (unless node (user-error "Org-roam node not found: %s" (cadr source)))
    (org-roam-node-visit node)))

(agent-shell-cockpit-register-instruction-adapter
 'org-roam :reference #'agent-shell-cockpit-org-roam--reference
 :bootstrap #'agent-shell-cockpit-org-roam--bootstrap
 :visit #'agent-shell-cockpit-org-roam--visit)

(provide 'agent-shell-cockpit-org-roam)
;;; agent-shell-cockpit-org-roam.el ends here
