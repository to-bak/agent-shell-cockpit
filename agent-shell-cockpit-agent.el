;;; agent-shell-cockpit-agent.el --- Shared Cockpit agent UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared agent rows, native agent-shell actions, and point-driven previews.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-ui)
(require 'agent-shell-cockpit-instructions)

(declare-function agent-shell-cockpit-refresh "agent-shell-cockpit-ui")

(declare-function agent-shell-clear-buffer "agent-shell")
(declare-function agent-shell-copy-last-output "agent-shell")
(declare-function agent-shell-copy-session-id "agent-shell")
(declare-function agent-shell-cycle-session-mode "agent-shell")
(declare-function agent-shell-fork "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-open-transcript "agent-shell")
(declare-function agent-shell-reload "agent-shell")
(declare-function agent-shell-prompt-steer "agent-shell-prompt-queue")
(declare-function agent-shell-rename-buffer "agent-shell")
(declare-function agent-shell-set-session-config-option "agent-shell")
(declare-function agent-shell-set-session-mode "agent-shell")
(declare-function agent-shell-set-session-model "agent-shell")
(declare-function agent-shell-set-session-thought-level "agent-shell")
(declare-function agent-shell-show-usage "agent-shell-usage")

(defcustom agent-shell-cockpit-agent-preview-width 0.4
  "Width of the temporary right-side agent preview window."
  :type 'number
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-agent-preview-behavior 'delayed
  "Whether agent previews are automatic, manual, or disabled."
  :type '(choice (const delayed) (const manual) (const off))
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-agent-preview-delay 0.3
  "Idle seconds before showing the agent at point."
  :type 'number :group 'agent-shell-cockpit)

(defvar-local agent-shell-cockpit-agent--preview-timer nil)
(defvar-local agent-shell-cockpit-agent--preview-pinned nil)

(defvar agent-shell-cockpit-agent--action-buffer nil
  "Live agent buffer targeted by the active action menu.")

(defvar-local agent-shell-cockpit-agent--preview-window nil
  "Temporary side window owned by the current Cockpit buffer.")

(defvar-local agent-shell-cockpit-agent--preview-buffer nil
  "Agent currently previewed from the current Cockpit buffer.")

(defun agent-shell-cockpit-agent-launch (&optional workspace directory)
  "Choose instructions and start an agent in WORKSPACE or standalone DIRECTORY."
  (let ((text (agent-shell-cockpit-instructions-render
               (agent-shell-cockpit-instructions-read workspace) workspace)))
    (if workspace
        (agent-shell-cockpit-session-start-select workspace text)
      (agent-shell-cockpit-session-start-target
       (agent-shell-cockpit-session-target (or directory default-directory)) text))))

(defun agent-shell-cockpit-agent-live-at-point-p ()
  "Return non-nil when point is on a live agent row."
  (memq (agent-shell-cockpit-ui-object-type-at-point)
        '(session workspace-session live-session)))

(defun agent-shell-cockpit-agent-buffer-at-point ()
  "Return the live agent buffer at point or signal a user error."
  (unless (agent-shell-cockpit-agent-live-at-point-p)
    (user-error "Point is not on a live agent"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (unless (buffer-live-p buffer)
      (user-error "Agent buffer is no longer live"))
    buffer))

(defun agent-shell-cockpit-agent--identifier-name (identifier)
  "Return a readable agent name for IDENTIFIER."
  (capitalize
   (replace-regexp-in-string "[-_]+" " " (or identifier "agent"))))

(defun agent-shell-cockpit-agent--name (agent-id session-id workspace)
  "Return a stable display name for AGENT-ID and SESSION-ID in WORKSPACE."
  (let* ((record (seq-find (lambda (entry)
                             (and (equal (map-elt entry 'agentId) agent-id)
                                  (equal (map-elt entry 'sessionId) session-id)))
                           (map-elt workspace 'sessions)))
         (id (or (map-elt record 'displayId)
                 (and session-id (substring (secure-hash 'sha256 (concat agent-id ":" session-id)) 0 6))
                 "new")))
    (format "%s · %s" (agent-shell-cockpit-agent--identifier-name agent-id) id)))

(defun agent-shell-cockpit-agent--status-column (status)
  "Return a fixed-width status column for STATUS."
  (let* ((label (string-trim-left
                 (agent-shell-cockpit-ui-status-label status)))
         (padding (max 1 (- 13 (string-width label)))))
    (concat label (make-string padding ?\s))))

(defun agent-shell-cockpit-agent--workspace-tag (workspace)
  "Return a compact tag for WORKSPACE, or the unassigned tag."
  (propertize
   (format "[%-12s] "
           (agent-shell-cockpit-ui-one-line
            (if workspace
                (or (map-elt workspace 'title) (map-elt workspace 'name))
              "unassigned")
            12))
   'face 'agent-shell-cockpit-secondary))

(defun agent-shell-cockpit-agent--heading
    (status name title &optional workspace-tag)
  "Return an agent heading from STATUS, NAME, TITLE, and WORKSPACE-TAG."
  (concat
   (agent-shell-cockpit-agent--status-column status)
   (or workspace-tag "")
   (propertize (format "%-18s "
                       (agent-shell-cockpit-ui-one-line name 17))
               'face 'default)
   (propertize (agent-shell-cockpit-ui-one-line title 42)
               'face 'agent-shell-cockpit-secondary)))

(defun agent-shell-cockpit-agent-insert-live
    (buffer workspace kind &optional show-workspace)
  "Insert live BUFFER in WORKSPACE as section KIND.
When SHOW-WORKSPACE is non-nil, include a workspace tag."
  (let* ((agent-id (with-current-buffer buffer
                     (agent-shell-cockpit-session--identifier)))
         (session-id (with-current-buffer buffer
                       (agent-shell-cockpit-session--session-id)))
         (record (and workspace
                      (agent-shell-cockpit-session-record buffer workspace)))
         (title (or (and record (map-elt record 'title))
                    (with-current-buffer buffer
                      (agent-shell-cockpit-session--title))))
         (name (agent-shell-cockpit-agent--name
                agent-id session-id workspace)))
    (magit-insert-section
     (agent-shell-cockpit-section buffer t :kind kind :object buffer)
     (magit-insert-heading
      (agent-shell-cockpit-agent--heading
       (agent-shell-cockpit-session-status buffer) name title
       (and show-workspace
            (agent-shell-cockpit-agent--workspace-tag
             (or workspace (when (buffer-local-value 'agent-shell-cockpit-session-standalone-p buffer)
                             '((title . "standalone"))))))))
     (magit-insert-section-body
      (agent-shell-cockpit-ui-insert-detail "Agent" (or agent-id "unknown"))
      (when session-id
        (agent-shell-cockpit-ui-insert-detail "Session" session-id))
      (agent-shell-cockpit-ui-insert-detail
       "Directory"
       (abbreviate-file-name
        (buffer-local-value 'default-directory buffer)))))))

(defun agent-shell-cockpit-agent-insert-history (session workspace)
  "Insert historical SESSION from WORKSPACE."
  (let* ((agent-id (map-elt session 'agentId))
         (session-id (map-elt session 'sessionId))
         (name (agent-shell-cockpit-agent--name
                agent-id session-id workspace)))
    (magit-insert-section
     (agent-shell-cockpit-section session-id t
                                  :kind 'session-history
                                  :object session)
     (magit-insert-heading
      (agent-shell-cockpit-agent--heading
       'history name (or (map-elt session 'title) session-id)))
     (magit-insert-section-body
      (agent-shell-cockpit-ui-insert-detail "Agent" agent-id)
      (agent-shell-cockpit-ui-insert-detail "Session" session-id)))))

(defun agent-shell-cockpit-agent-preview-close ()
  "Close the temporary agent preview owned by the current Cockpit buffer."
  (when (timerp agent-shell-cockpit-agent--preview-timer)
    (cancel-timer agent-shell-cockpit-agent--preview-timer))
  (setq agent-shell-cockpit-agent--preview-timer nil
        agent-shell-cockpit-agent--preview-pinned nil)
  (when (and (window-live-p agent-shell-cockpit-agent--preview-window)
             (eq (window-parameter agent-shell-cockpit-agent--preview-window 'agent-shell-cockpit-preview)
                 (current-buffer))
             (eq (window-buffer agent-shell-cockpit-agent--preview-window)
                 agent-shell-cockpit-agent--preview-buffer))
    (quit-restore-window agent-shell-cockpit-agent--preview-window 'bury))
  (setq agent-shell-cockpit-agent--preview-window nil
        agent-shell-cockpit-agent--preview-buffer nil)
  (unless (seq-some (lambda (buffer)
                      (buffer-local-value 'agent-shell-cockpit-agent--preview-window buffer))
                    (buffer-list))
    (remove-hook 'window-state-change-functions #'agent-shell-cockpit-agent--preview-window-change)))

(defun agent-shell-cockpit-agent-preview-update ()
  "Preview the live agent at point in a temporary right-side window."
  (let ((buffer (and (agent-shell-cockpit-agent-live-at-point-p)
                     (agent-shell-cockpit-ui-object-at-point))))
    (if (not (buffer-live-p buffer))
        (agent-shell-cockpit-agent-preview-close)
      (unless (and (eq buffer agent-shell-cockpit-agent--preview-buffer)
                   (window-live-p agent-shell-cockpit-agent--preview-window)
                   (eq (window-buffer agent-shell-cockpit-agent--preview-window) buffer)
                   (eq (window-parameter agent-shell-cockpit-agent--preview-window
                                         'agent-shell-cockpit-preview) (current-buffer)))
        (agent-shell-cockpit-agent-preview-close)
        (let ((window
               (display-buffer
                buffer
                `((display-buffer-in-side-window) (side . right)
                  (slot . 99)
                  (window-width . ,agent-shell-cockpit-agent-preview-width)
                  (window-parameters
                   . ((agent-shell-cockpit-preview . ,(current-buffer))))))))
          (when (and (window-live-p window)
                     (eq (window-parameter window 'agent-shell-cockpit-preview)
                         (current-buffer)))
            (add-hook 'window-state-change-functions #'agent-shell-cockpit-agent--preview-window-change)
            (setq agent-shell-cockpit-agent--preview-window window
                  agent-shell-cockpit-agent--preview-buffer buffer)
            (set-window-point window
                              (with-current-buffer buffer (point-max)))))))))

(defun agent-shell-cockpit-agent-preview ()
  "Manually show and pin the agent preview, or close a pinned preview."
  (interactive)
  (if agent-shell-cockpit-agent--preview-pinned
      (agent-shell-cockpit-agent-preview-close)
    (agent-shell-cockpit-agent-preview-update)
    (setq agent-shell-cockpit-agent--preview-pinned
          (window-live-p agent-shell-cockpit-agent--preview-window))))

(defun agent-shell-cockpit-agent--preview-schedule ()
  "Schedule a preview only after point settles on a live agent."
  (when (timerp agent-shell-cockpit-agent--preview-timer)
    (cancel-timer agent-shell-cockpit-agent--preview-timer))
  (unless agent-shell-cockpit-agent--preview-pinned
    (when (eq agent-shell-cockpit-agent-preview-behavior 'delayed)
      (let ((owner (current-buffer)) (position (point)))
        (setq agent-shell-cockpit-agent--preview-timer
              (run-with-idle-timer
               agent-shell-cockpit-agent-preview-delay nil
               (lambda ()
                 (when (and (buffer-live-p owner) (eq (window-buffer (selected-window)) owner))
                   (with-current-buffer owner
                     (when (= position (point))
                       (agent-shell-cockpit-agent-preview-update)))))))))))

(defun agent-shell-cockpit-agent--preview-window-change (&rest _)
  "Close orphaned previews when their owning view is no longer visible."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (window-live-p agent-shell-cockpit-agent--preview-window)
                 (not (get-buffer-window buffer t)))
        (agent-shell-cockpit-agent-preview-close)))))

(define-minor-mode agent-shell-cockpit-agent-preview-mode
  "Preview the live agent at point in a right-side window."
  :lighter nil
  (if agent-shell-cockpit-agent-preview-mode
      (progn
        (add-hook 'post-command-hook
                  #'agent-shell-cockpit-agent--preview-schedule nil t)
        (add-hook 'change-major-mode-hook #'agent-shell-cockpit-agent-preview-close nil t)
        (add-hook 'kill-buffer-hook
                  #'agent-shell-cockpit-agent-preview-close nil t))
    (remove-hook 'post-command-hook
                 #'agent-shell-cockpit-agent--preview-schedule t)
    (remove-hook 'kill-buffer-hook
                 #'agent-shell-cockpit-agent-preview-close t)
    (remove-hook 'change-major-mode-hook
                 #'agent-shell-cockpit-agent-preview-close t)
    (agent-shell-cockpit-agent-preview-close)))

(defun agent-shell-cockpit-agent--invoke (command)
  "Invoke native agent-shell COMMAND in the selected agent buffer."
  (unless (buffer-live-p agent-shell-cockpit-agent--action-buffer)
    (user-error "Selected agent buffer is no longer live"))
  (agent-shell-cockpit-agent-preview-close)
  (agent-shell-cockpit-session-invoke
   agent-shell-cockpit-agent--action-buffer command))

(defun agent-shell-cockpit-agent--configure-option (command)
  "Run native option COMMAND and persist its confirmed result."
  (let ((buffer agent-shell-cockpit-agent--action-buffer))
    (unless (buffer-live-p buffer) (user-error "Agent buffer is no longer live"))
    (with-current-buffer buffer
      (when (agent-shell-cockpit-agent-shell-configuration-pending-p)
        (user-error "Wait for the pending setting change before choosing another"))
      (unless (agent-shell-cockpit-agent-shell-ready-p)
        (user-error "Initialization is incomplete; visit the agent buffer to inspect its connection"))
      (funcall command
               (lambda (&rest _)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (agent-shell-cockpit-session--observe buffer)
                     (run-hooks 'agent-shell-cockpit-session-change-hook))
                   (message "Agent setting confirmed")))))))

(defmacro agent-shell-cockpit-agent--define-action (name command)
  "Define Cockpit action NAME delegating to native COMMAND."
  `(defun ,name ()
     ,(format "Run `%s' in the selected agent buffer." command)
     (interactive)
     (agent-shell-cockpit-agent--invoke #',command)))

(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-interrupt agent-shell-interrupt)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-fork agent-shell-fork)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-reload agent-shell-reload)
(defun agent-shell-cockpit-agent-cycle-mode ()
  "Run `agent-shell-cycle-session-mode' after initialization."
  (interactive)
  (agent-shell-cockpit-agent--configure-option #'agent-shell-cycle-session-mode))
(defun agent-shell-cockpit-agent-set-mode ()
  "Run `agent-shell-set-session-mode' after initialization."
  (interactive)
  (agent-shell-cockpit-agent--configure-option #'agent-shell-set-session-mode))
(defun agent-shell-cockpit-agent-set-model ()
  "Run `agent-shell-set-session-model' after initialization."
  (interactive)
  (agent-shell-cockpit-agent--configure-option #'agent-shell-set-session-model))
(defun agent-shell-cockpit-agent-set-thought ()
  "Run `agent-shell-set-session-thought-level' after initialization."
  (interactive)
  (agent-shell-cockpit-agent--configure-option #'agent-shell-set-session-thought-level))
(defun agent-shell-cockpit-agent-set-option ()
  "Run `agent-shell-set-session-config-option' after initialization."
  (interactive)
  (agent-shell-cockpit-agent--configure-option #'agent-shell-set-session-config-option))
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-usage agent-shell-show-usage)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-copy-session-id agent-shell-copy-session-id)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-copy-output agent-shell-copy-last-output)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-open-transcript agent-shell-open-transcript)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-clear agent-shell-clear-buffer)

(defun agent-shell-cockpit-agent-kill ()
  "Kill the live agent at point after confirmation."
  (interactive)
  (let ((buffer (agent-shell-cockpit-agent-buffer-at-point)))
    (when (yes-or-no-p (format "Kill agent session %s? "
                               (buffer-name buffer)))
      (with-current-buffer buffer
        (kill-buffer buffer))
      (agent-shell-cockpit-refresh))))

(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-steer agent-shell-prompt-steer)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-rename agent-shell-rename-buffer)

(transient-define-prefix agent-shell-cockpit-agent-actions-menu ()
  "Run native commands in a live agent-shell buffer."
  [["Request"
    ("p" "Permission choices" agent-shell-cockpit-agent-permissions)]
   ["Control"
    ("s" "Steer" agent-shell-cockpit-agent-steer)
    ("r" "Rename buffer" agent-shell-cockpit-agent-rename)
    ("i" "Interrupt" agent-shell-cockpit-agent-interrupt)
    ("f" "Fork" agent-shell-cockpit-agent-fork)
    ("R" "Reload" agent-shell-cockpit-agent-reload)]
   ["Session"
    ("m" "Cycle mode" agent-shell-cockpit-agent-cycle-mode)
    ("M" "Set mode" agent-shell-cockpit-agent-set-mode)
    ("v" "Set model" agent-shell-cockpit-agent-set-model)
    ("t" "Set thought level" agent-shell-cockpit-agent-set-thought)
    ("o" "Set option" agent-shell-cockpit-agent-set-option)]
   ["Inspect"
    ("u" "Usage" agent-shell-cockpit-agent-usage)
    ("c" "Copy session ID" agent-shell-cockpit-agent-copy-session-id)
    ("w" "Copy last output" agent-shell-cockpit-agent-copy-output)
    ("T" "Open transcript" agent-shell-cockpit-agent-open-transcript)
    ("C" "Clear buffer" agent-shell-cockpit-agent-clear)]])

(defun agent-shell-cockpit-agent-permissions ()
  "Choose an actual native permission option, validating it before invocation."
  (interactive)
  (let* ((buffer agent-shell-cockpit-agent--action-buffer)
         (choices (agent-shell-cockpit-agent-shell-permission-choices buffer))
         (table (cl-loop for choice in choices for index from 1
                         collect (cons (format "%d. %s" index (car choice)) choice))))
    (unless choices (user-error "Agent has no pending permission choices"))
    (agent-shell-cockpit-agent-shell-invoke-choice
     buffer (cdr (assoc (completing-read "Native permission: " table nil t) table)))
    (agent-shell-cockpit-refresh)))

(defun agent-shell-cockpit-agent-actions ()
  "Open the native action menu for the live agent at point."
  (interactive)
  (setq agent-shell-cockpit-agent--action-buffer
        (agent-shell-cockpit-agent-buffer-at-point))
  (transient-setup 'agent-shell-cockpit-agent-actions-menu))

(defun agent-shell-cockpit-next-attention ()
  "Move to the next agent needing attention, wrapping within this view."
  (interactive)
  (let ((start (point)) positions)
    (cl-labels ((collect (section)
                  (when (and (cl-typep section 'agent-shell-cockpit-section)
                             (memq (oref section kind) '(session workspace-session live-session))
                             (buffer-live-p (oref section object))
                             (eq (agent-shell-cockpit-session-status (oref section object)) 'attention))
                    (push (oref section start) positions))
                  (mapc #'collect (oref section children))))
               (when magit-root-section (collect magit-root-section)))
    (setq positions (sort positions #'<))
    (unless positions (user-error "No agents need attention in this view"))
    (goto-char (or (seq-find (lambda (position) (> position start)) positions)
                   (car positions)))
    (magit-section-show (magit-current-section))))

(provide 'agent-shell-cockpit-agent)

;;; agent-shell-cockpit-agent.el ends here
