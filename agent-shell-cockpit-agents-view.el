;;; agent-shell-cockpit-agents-view.el --- Cross-workspace agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A dedicated live-agent view, independent of workspace navigation.

;;; Code:

(require 'agent-shell-cockpit-agent)
(require 'agent-shell-cockpit-ui)
(require 'transient)

(declare-function agent-shell-cockpit-workspace-view "agent-shell-cockpit-workspace-view")
(declare-function agent-shell-cockpit-workspace-dispatch "agent-shell-cockpit-navigation")
(declare-function agent-shell-cockpit-archives "agent-shell-cockpit-archive-view")

(defcustom agent-shell-cockpit-agents-view-buffer-name "*Cockpit: All agents*"
  "Name of the cross-workspace live-agent buffer."
  :type 'string :group 'agent-shell-cockpit)

(defvar agent-shell-cockpit-agents-view--buffer nil
  "Live cross-workspace agent view buffer.")

(defun agent-shell-cockpit-agents-view--render ()
  "Render live agents, without a workspace directory listing."
  (let ((agents (agent-shell-cockpit-agent-sort-buffers
                 (seq-filter #'buffer-live-p (agent-shell-buffers)))))
    (erase-buffer)
    (magit-insert-section (agent-shell-cockpit-section 'all-agents nil :kind 'root)
      (agent-shell-cockpit-ui-insert-header "All agents" "Across all workspaces")
      (insert "\n")
      (magit-insert-section (agent-shell-cockpit-section 'live-agents nil :kind 'group)
        (magit-insert-heading
         (propertize (format "Agents (%d) · %d need attention" (length agents)
                             (seq-count (lambda (buffer)
                                          (eq (agent-shell-cockpit-session-status buffer) 'attention)) agents))
                     'font-lock-face 'magit-section-heading))
        (magit-insert-section-body
         (if agents
             (dolist (buffer agents)
               (let ((workspace (agent-shell-cockpit-session-workspace buffer)))
                 (agent-shell-cockpit-agent-insert-live
                  buffer workspace (if workspace 'workspace-session 'session) t)))
           (insert (propertize "No live agents\n" 'face 'agent-shell-cockpit-secondary)))
         (insert "\n"))))))

(defun agent-shell-cockpit-agents-view-refresh ()
  "Refresh the all-agents view while preserving point and section state."
  (agent-shell-cockpit-ui-refresh-buffer #'agent-shell-cockpit-agents-view--render))

(defun agent-shell-cockpit-agents-view-open ()
  "Visit the live agent at point."
  (when (agent-shell-cockpit-agent-live-at-point-p)
    (agent-shell-cockpit-session-visit (agent-shell-cockpit-agent-buffer-at-point))))

(defun agent-shell-cockpit-agents-view-workspace ()
  "Open the workspace belonging to the agent at point."
  (interactive)
  (if-let* ((workspace (agent-shell-cockpit-session-workspace
                        (agent-shell-cockpit-agent-buffer-at-point))))
      (agent-shell-cockpit-workspace-view workspace)
    (user-error "This agent has no workspace")))

(defun agent-shell-cockpit-start-agent ()
  "Prepare a standalone native agent in a chosen directory."
  (interactive)
  (agent-shell-cockpit-agent-launch
   nil (read-directory-name "Agent directory: " default-directory nil t)))

(defun agent-shell-cockpit-attach-session ()
  "Attach an unassigned agent to a compatible workspace."
  (interactive)
  (let* ((at-point (and (agent-shell-cockpit-agent-live-at-point-p)
                        (agent-shell-cockpit-agent-buffer-at-point)))
         (unassigned (agent-shell-cockpit-session-unassigned-buffers))
         (buffer (if (memq at-point unassigned) at-point
                   (unless unassigned (user-error "No unassigned agents"))
                   (get-buffer (completing-read "Attach agent: " (mapcar #'buffer-name unassigned) nil t))))
         (workspaces (seq-filter (lambda (record) (eq (map-elt record 'kind) 'workspace))
                                 (agent-shell-cockpit-store-discover)))
         (choices (mapcar (lambda (workspace)
                           (cons (format "%s — %s" (map-elt workspace 'title) (map-elt workspace 'root)) workspace))
                         workspaces)))
    (unless choices (user-error "Create a workspace first"))
    (agent-shell-cockpit-session-attach
     buffer (cdr (assoc (completing-read "Attach to workspace: " choices nil t) choices)))
    (agent-shell-cockpit-refresh)))

(transient-define-prefix agent-shell-cockpit-agents-view-dispatch ()
  "Show live-agent actions and workspace navigation."
  [[("s" "Start standalone agent" agent-shell-cockpit-start-agent)
    ("S" "Start standalone agent" agent-shell-cockpit-start-agent)
    ("+" "Attach unassigned agent" agent-shell-cockpit-attach-session)
    ("o" "Visit agent's workspace" agent-shell-cockpit-agents-view-workspace)]
   [("a" "Agent actions" agent-shell-cockpit-agent-actions :inapt-if-not agent-shell-cockpit-agent-live-at-point-p)
    ("K" "Kill agent" agent-shell-cockpit-agent-kill :inapt-if-not agent-shell-cockpit-agent-live-at-point-p)
    ("v" "Preview agent" agent-shell-cockpit-agent-preview)
    ("]" "Next attention" agent-shell-cockpit-next-attention)]
   [("b" "Workspaces" agent-shell-cockpit-workspace-dispatch)
    ("l" "Archives" agent-shell-cockpit-archives)
    ("I" "Visit instruction" agent-shell-cockpit-visit-instruction)]]
  ["Navigation"
   [("r" "Refresh" agent-shell-cockpit-refresh)
    ("q" "Return / bury buffer" agent-shell-cockpit-quit)
    ("TAB" "Toggle section" agent-shell-cockpit-toggle-section)
    ("RET" "Visit agent" agent-shell-cockpit-open)]
   [("n" "Next section" agent-shell-cockpit-next)
    ("p" "Previous section" agent-shell-cockpit-previous)
    ("M-<" "First section" agent-shell-cockpit-first)
    ("M->" "Last section" agent-shell-cockpit-last)]
   [("<down>" "Next section" agent-shell-cockpit-next)
    ("<up>" "Previous section" agent-shell-cockpit-previous)
    ("C-n" "Next line" next-line)
    ("C-p" "Previous line" previous-line)]
   [("j" "Next section (Evil)" agent-shell-cockpit-next :if (lambda () (bound-and-true-p evil-local-mode)))
    ("k" "Previous section (Evil)" agent-shell-cockpit-previous :if (lambda () (bound-and-true-p evil-local-mode)))]]
  [:hide (lambda () t)
   ("<tab>" "Toggle section" agent-shell-cockpit-toggle-section)
   ("<return>" "Visit agent" agent-shell-cockpit-open)])

(defvar-keymap agent-shell-cockpit-agents-view-mode-map
  :parent agent-shell-cockpit-ui-mode-map
  "s" #'agent-shell-cockpit-start-agent
  "S" #'agent-shell-cockpit-start-agent
  "+" #'agent-shell-cockpit-attach-session
  "o" #'agent-shell-cockpit-agents-view-workspace
  "a" #'agent-shell-cockpit-agent-actions
  "K" #'agent-shell-cockpit-agent-kill
  "v" #'agent-shell-cockpit-agent-preview
  "]" #'agent-shell-cockpit-next-attention
  "I" #'agent-shell-cockpit-visit-instruction
  "b" #'agent-shell-cockpit-workspace-dispatch
  "l" #'agent-shell-cockpit-archives)

(define-derived-mode agent-shell-cockpit-agents-view-mode agent-shell-cockpit-ui-mode "Cockpit Agents"
  "Inspect agents across workspaces without a workspace listing."
  (setq-local agent-shell-cockpit-ui--refresh-function #'agent-shell-cockpit-agents-view-refresh
              agent-shell-cockpit-ui--open-function #'agent-shell-cockpit-agents-view-open
              agent-shell-cockpit-ui--dispatch-function #'agent-shell-cockpit-agents-view-dispatch)
  (agent-shell-cockpit-agent-preview-mode 1))

(defun agent-shell-cockpit-all-agents ()
  "Open the cross-workspace live-agent view."
  (interactive)
  (let ((origin (current-buffer))
        (directory default-directory)
        (buffer (get-buffer-create agent-shell-cockpit-agents-view-buffer-name)))
    (agent-shell-cockpit-agent-preview-close)
    (setq agent-shell-cockpit-agents-view--buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-cockpit-agents-view-mode)
        (agent-shell-cockpit-agents-view-mode))
      (unless (eq origin buffer)
        (setq agent-shell-cockpit-ui-return-buffer origin
              default-directory directory))
      (agent-shell-cockpit-agents-view-refresh))
    (switch-to-buffer buffer)))

(provide 'agent-shell-cockpit-agents-view)
;;; agent-shell-cockpit-agents-view.el ends here
