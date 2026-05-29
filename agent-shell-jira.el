;;; agent-shell-jira.el --- Jira helpers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.52.1") (jira "2.21.0"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Local helpers around `jira.el' for querying and opening Jira issues.

;;; Code:

(require 'tabulated-list)
(require 'tablist)
(require 'subr-x)

(require 'jira)
(require 'jira-utils)
(require 'agent-shell)
(require 'agent-shell-utils)
(require 'agent-shell-fanout)

(defvar jira-detail--current-key nil)
(defvar jira-issues-key-summary-map)

(defun agent-shell-jira--agent-task (issue-id issue-title prompt?)
  "Build the agent investigation task for ISSUE-ID and ISSUE-TITLE."
  (let ((default (format
                  (concat
                   "Investigate issue %s: %s\n\n"
                   "Please inspect the Jira issue, any failing job context, identify "
                   "if any work on it was already done in this git repository. "
                   "If work was done, double check it against the issue context "
                   "and report your conclusions. Otherwise, figure out if the issue may "
                   "be stale and no longer applicable, and if not, try to find the root "
                   "cause in this repository, and propose a focused fix "
                   "with validation if feasible, but WITHOUT doing any code changes "
                   "for now.")
                  issue-id issue-title)))
    (if prompt?
        (read-from-minibuffer "Prompt: " default)
      default)))

(defun agent-shell-jira--worktree-title (issue-id issue-title)
  "Return a compact worktree title for ISSUE-ID using ISSUE-TITLE."
  (let ((title (string-trim
                (replace-regexp-in-string "\\s-+" " " (or issue-title "")))))
    (if (string-empty-p title)
        issue-id
      (when (> (length title) 18)
        (setq title (concat (substring title 0 15) "...")))
      (format "%s-%s" title issue-id))))

(defun agent-shell-jira--explicitly-marked-issue-ids ()
  "Return explicitly marked Jira issue ids in the current tabulated list."
  (if (derived-mode-p 'jira-detail-mode)
      (list jira-detail--current-key)
    (let (issue-ids)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((issue-id (tabulated-list-get-id))
                (mark-state (and (fboundp 'tablist-get-mark-state)
                                 (tablist-get-mark-state))))
            (when (and issue-id mark-state
                       (not (eq (car mark-state) ?\s)))
              (push issue-id issue-ids)))
          (forward-line 1)))
      (nreverse issue-ids))))

;;;###autoload
(defun agent-shell-jira-issues-investigate-marked-with-agent (&optional prompt?)
  "Start worktree-backed agent investigations for marked Jira issues.

If no issues are marked in `*Jira Issues*', emit a message and do nothing."
  (interactive "P")
  (let ((issue-ids (agent-shell-jira--explicitly-marked-issue-ids)))
    (if (null issue-ids)
        (message "No Jira issues are marked")
      (let ((project-root (expand-file-name
                           (read-directory-name "Project root for agent worktrees: " default-directory nil t))))
        (agent-shell-fanout-worktrees
         (mapcar (lambda (issue-id)
                   (let ((issue-title (or (gethash issue-id jira-issues-key-summary-map)
                                          "")))
                     (cons (agent-shell-jira--worktree-title issue-id issue-title)
                           (agent-shell-jira--agent-task issue-id issue-title prompt?))))
                 issue-ids)
         project-root)))))

(provide 'agent-shell-jira)

;;; timfel-jira-extensions.el ends here
