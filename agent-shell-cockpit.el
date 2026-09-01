;;; agent-shell-cockpit.el --- Workspace cockpit for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; Author: to-bak
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.71.3")
;;                    (magit-section "4.0.0") (transient "0.7.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/to-bak/agent-shell-cockpit
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Manage durable multi-repository workspaces and their agent-shell sessions.
;; Invoke `agent-shell-cockpit' to open the dashboard.

;;; Code:

(defgroup agent-shell-cockpit nil
  "Workspace and session management for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-cockpit-")

(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-project)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-dashboard)
(require 'agent-shell-cockpit-workspace-view)
(require 'agent-shell-cockpit-archive-view)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(defun agent-shell-cockpit--evil-setup ()
  "Install Cockpit's Magit-style bindings in Evil motion state."
  (evil-set-initial-state 'agent-shell-cockpit-mode 'motion)
  (evil-set-initial-state 'agent-shell-cockpit-workspace-view-mode 'motion)
  (evil-set-initial-state 'agent-shell-cockpit-archive-view-mode 'motion)
  (evil-define-key* 'motion agent-shell-cockpit-mode-map
    (kbd "TAB") #'agent-shell-cockpit-toggle-section
    (kbd "RET") #'agent-shell-cockpit-open
    "?" #'agent-shell-cockpit-dispatch
    "j" #'agent-shell-cockpit-next
    "k" #'agent-shell-cockpit-previous
    "n" #'agent-shell-cockpit-next
    "p" #'agent-shell-cockpit-previous
    "g" #'agent-shell-cockpit-refresh
    "w" #'agent-shell-cockpit-create-workspace
    "e" #'agent-shell-cockpit-edit-prompt
    "s" #'agent-shell-cockpit-start-agent
    "S" #'agent-shell-cockpit-start-agent-select
    "a" #'agent-shell-cockpit-attach-session
    "A" #'agent-shell-cockpit-archive-workspace
    "Y" #'agent-shell-cockpit-dashboard-allow-once
    "K" #'agent-shell-cockpit-kill
    "l" #'agent-shell-cockpit-archive-dispatch
    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'motion agent-shell-cockpit-workspace-view-mode-map
    (kbd "TAB") #'agent-shell-cockpit-toggle-section
    (kbd "RET") #'agent-shell-cockpit-open
    "?" #'agent-shell-cockpit-dispatch
    "j" #'agent-shell-cockpit-next
    "k" #'agent-shell-cockpit-previous
    "n" #'agent-shell-cockpit-next
    "p" #'agent-shell-cockpit-previous
    "g" #'agent-shell-cockpit-refresh
    "s" #'agent-shell-cockpit-workspace-view-start-agent
    "S" #'agent-shell-cockpit-workspace-view-start-agent-select
    "r" #'agent-shell-cockpit-resume-session
    "Y" #'agent-shell-cockpit-workspace-view-allow-once
    "K" #'agent-shell-cockpit-workspace-view-kill-session
    "e" #'agent-shell-cockpit-workspace-view-edit-prompt
    "+" #'agent-shell-cockpit-add-worktree
    "-" #'agent-shell-cockpit-remove-worktree
    "A" #'agent-shell-cockpit-workspace-view-archive
    "b" #'agent-shell-cockpit-workspace-view-back
    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'motion agent-shell-cockpit-archive-view-mode-map
    (kbd "TAB") #'agent-shell-cockpit-toggle-section
    (kbd "RET") #'agent-shell-cockpit-open
    "?" #'agent-shell-cockpit-dispatch
    "j" #'agent-shell-cockpit-next
    "k" #'agent-shell-cockpit-previous
    "n" #'agent-shell-cockpit-next
    "p" #'agent-shell-cockpit-previous
    "g" #'agent-shell-cockpit-refresh
    "D" #'agent-shell-cockpit-archive-view-delete
    "b" #'agent-shell-cockpit-archive-view-back
    "q" #'agent-shell-cockpit-quit)
  (evil-define-key* 'normal agent-shell-cockpit-session-mode-map
    "q" #'agent-shell-cockpit-session-return))

(with-eval-after-load 'evil
  (agent-shell-cockpit--evil-setup))

(agent-shell-cockpit-project-enable)

(provide 'agent-shell-cockpit)

;;; agent-shell-cockpit.el ends here
