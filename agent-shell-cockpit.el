;;; agent-shell-cockpit.el --- Workspace cockpit for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; Author: to-bak
;; Assisted-by: Codex:GPT-6
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (agent-shell "0.75.2")
;;                    (magit-section "4.0.0") (transient "0.7.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/to-bak/agent-shell-cockpit
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Manage durable multi-repository workspaces and their agent-shell sessions.
;; Invoke `agent-shell-cockpit' to open your current workspace.

;;; Code:

(defgroup agent-shell-cockpit nil
  "Workspace and session management for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-cockpit-")

(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-instructions)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-agent)
(require 'agent-shell-cockpit-agents-view)
(require 'agent-shell-cockpit-workspace-view)
(require 'agent-shell-cockpit-archive-view)
(require 'agent-shell-cockpit-navigation)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(defun agent-shell-cockpit--evil-setup ()
  "Install Cockpit's Magit-style bindings in Evil motion state."
  (evil-set-initial-state 'agent-shell-cockpit-agents-view-mode 'motion)
  (evil-set-initial-state 'agent-shell-cockpit-workspace-view-mode 'motion)
  (evil-set-initial-state 'agent-shell-cockpit-archive-view-mode 'motion)
  (evil-set-initial-state 'agent-shell-cockpit-launch-mode 'motion)
  (evil-define-key* 'motion agent-shell-cockpit-launch-mode-map
    (kbd "SPC") #'agent-shell-cockpit-launch-toggle
    (kbd "RET") #'agent-shell-cockpit-launch-inspect
    (kbd "TAB") #'agent-shell-cockpit-toggle-section
    (kbd "M-<up>") #'agent-shell-cockpit-launch-move-up
    (kbd "M-<down>") #'agent-shell-cockpit-launch-move-down
    (kbd "M-k") #'agent-shell-cockpit-launch-move-up
    (kbd "M-j") #'agent-shell-cockpit-launch-move-down
    "j" #'agent-shell-cockpit-next "k" #'agent-shell-cockpit-previous
    "a" #'agent-shell-cockpit-launch-add-file
    "p" #'agent-shell-cockpit-launch-preview
    "s" #'agent-shell-cockpit-launch-start
    "?" #'agent-shell-cockpit-launch-dispatch
    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'motion agent-shell-cockpit-agents-view-mode-map
    (kbd "I") #'agent-shell-cockpit-visit-instruction
                    (kbd "TAB") #'agent-shell-cockpit-toggle-section
                    (kbd "RET") #'agent-shell-cockpit-open
                    "?" #'agent-shell-cockpit-dispatch
                    "j" #'agent-shell-cockpit-next
                    "k" #'agent-shell-cockpit-previous
                    "n" #'agent-shell-cockpit-next
                    "p" #'agent-shell-cockpit-previous
                    "r" #'agent-shell-cockpit-refresh
                    "v" #'agent-shell-cockpit-agent-preview
                    "o" #'agent-shell-cockpit-agents-view-workspace
                    "s" #'agent-shell-cockpit-start-agent
                    "S" #'agent-shell-cockpit-start-agent
                    "]" #'agent-shell-cockpit-next-attention
                    "+" #'agent-shell-cockpit-attach-session
                    "a" #'agent-shell-cockpit-agent-actions
                    "K" #'agent-shell-cockpit-agent-kill
                    "l" #'agent-shell-cockpit-archives
                    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'motion agent-shell-cockpit-workspace-view-mode-map
    (kbd "I") #'agent-shell-cockpit-visit-instruction
                    (kbd "TAB") #'agent-shell-cockpit-toggle-section
                    (kbd "RET") #'agent-shell-cockpit-open
                    "?" #'agent-shell-cockpit-dispatch
                    "j" #'agent-shell-cockpit-next
                    "k" #'agent-shell-cockpit-previous
                    "n" #'agent-shell-cockpit-next
                    "p" #'agent-shell-cockpit-previous
                    "r" #'agent-shell-cockpit-refresh
                    "s" #'agent-shell-cockpit-workspace-view-start-agent
                    "S" #'agent-shell-cockpit-workspace-view-start-agent
                    "R" #'agent-shell-cockpit-workspace-view-recover
                    "t" #'agent-shell-cockpit-workspace-view-title
                    "v" #'agent-shell-cockpit-agent-preview
                    "c" #'agent-shell-cockpit-workspace-view-add-context
                    "P" #'agent-shell-cockpit-workspace-view-preserve
                    "]" #'agent-shell-cockpit-next-attention
                    "a" #'agent-shell-cockpit-agent-actions
                    "x" #'agent-shell-cockpit-workspace-view-forget-session
                    "o" #'agent-shell-cockpit-agents-view-workspace
                    "K" #'agent-shell-cockpit-agent-kill
                    "e" #'agent-shell-cockpit-workspace-view-edit-context
                    "+" #'agent-shell-cockpit-add-worktree
                    "-" #'agent-shell-cockpit-remove-worktree
                    "A" #'agent-shell-cockpit-workspace-view-archive
                    "w" #'agent-shell-cockpit-create-workspace
                    "l" #'agent-shell-cockpit-archives
                    "E" #'agent-shell-cockpit-repair-workspace
                    "b" #'agent-shell-cockpit-workspace-dispatch
                    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'motion agent-shell-cockpit-archive-view-mode-map
                    (kbd "TAB") #'agent-shell-cockpit-toggle-section
                    (kbd "RET") #'agent-shell-cockpit-open
                    "?" #'agent-shell-cockpit-dispatch
                    "j" #'agent-shell-cockpit-next
                    "k" #'agent-shell-cockpit-previous
                    "n" #'agent-shell-cockpit-next
                    "p" #'agent-shell-cockpit-previous
                    "r" #'agent-shell-cockpit-refresh
                    "D" #'agent-shell-cockpit-archive-view-delete
                    "R" #'agent-shell-cockpit-archive-view-restore
                    "b" #'agent-shell-cockpit-workspace-dispatch
                    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'normal agent-shell-cockpit-session-mode-map
                    "q" #'agent-shell-cockpit-session-return)
  (dolist (map (list agent-shell-cockpit-agents-view-mode-map
                    agent-shell-cockpit-workspace-view-mode-map
                    agent-shell-cockpit-archive-view-mode-map))
    (evil-define-key* 'motion map
      "b" #'agent-shell-cockpit-workspace-dispatch)))

(add-hook 'evil-mode-hook #'agent-shell-cockpit--evil-setup)
(when (featurep 'evil) (agent-shell-cockpit--evil-setup))

;;;###autoload
(defun agent-shell-cockpit ()
  "Open the current or most recently visited Cockpit workspace."
  (interactive)
  (agent-shell-cockpit-navigation-open))

(dolist (map (list agent-shell-cockpit-agents-view-mode-map
                   agent-shell-cockpit-workspace-view-mode-map
                   agent-shell-cockpit-archive-view-mode-map))
  (keymap-set map "b" #'agent-shell-cockpit-workspace-dispatch))

(provide 'agent-shell-cockpit)

;;; agent-shell-cockpit.el ends here
