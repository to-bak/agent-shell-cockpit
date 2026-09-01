;;; agent-shell-cockpit.el --- Workspace cockpit for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; Author: to-bak
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.71.3"))
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

(agent-shell-cockpit-project-enable)

(provide 'agent-shell-cockpit)

;;; agent-shell-cockpit.el ends here
