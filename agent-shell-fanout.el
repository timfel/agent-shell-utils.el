;;; agent-shell-fanout.el --- Fan out agent-shell sessions -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.55.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Commands for creating or resuming multiple `agent-shell' sessions, usually
;; one per Git worktree.  `agent-shell-fanout-worktrees' accepts task specs and
;; starts one shell per task.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-utils)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell--dot-subdir "agent-shell")

(defcustom agent-shell-fanout-planning-request
  "Go into planning mode"
  "First line prefixed to each fan-out agent task.

Set to an empty string to queue task text without a planning preface."
  :type 'string
  :group 'agent-shell-utils)

(defcustom agent-shell-fanout-worktree-cleanup-age-days 14
  "Offer to remove fan-out worktrees untouched for this many days.

Set to nil to disable stale worktree cleanup."
  :type '(choice (const :tag "Disable cleanup" nil) integer)
  :group 'agent-shell-utils)

(defcustom agent-shell-fanout-adjacent-repository-names nil
  "Adjacent repository directory names included in multi-repo worktrees.

Each name is resolved as a sibling of the selected repository root.  The main
repository is always included."
  :type '(repeat string)
  :group 'agent-shell-utils)

(defcustom agent-shell-fanout-repositories-function
  #'agent-shell-fanout-default-repositories
  "Function called with a repository root and returning roots to fan out.

The default includes the main repository and any configured adjacent
repositories from `agent-shell-fanout-adjacent-repository-names'."
  :type 'function
  :group 'agent-shell-utils)

(defvar-local agent-shell-fanout-worktree-parent nil
  "Parent directory for the current fan-out worktree set.")

(defun agent-shell-fanout--initial-request (task)
  "Return the initial queued request for TASK."
  (let ((planning-request
         (string-trim (or agent-shell-fanout-planning-request "")))
        (task (string-trim (or task ""))))
    (cond
     ((string-empty-p task) nil)
     ((string-empty-p planning-request) task)
     (t (concat planning-request "\n" task)))))

(defun agent-shell-fanout--worktree-base-ref (git-root)
  "Return the best available base ref used for new worktrees in GIT-ROOT."
  (let ((default-directory (file-name-as-directory git-root)))
    (cond
     ((zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                           "refs/remotes/origin/master"))
      "origin/master")
     ((zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                           "refs/remotes/origin/main"))
      "origin/main")
     ((zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                           "refs/heads/master"))
      "master")
     ((zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                           "refs/heads/main"))
      "main")
     ((zerop (process-file "git" nil nil nil "rev-parse" "--verify" "--quiet"
                           "HEAD"))
      "HEAD"))))

(defun agent-shell-fanout--worktrees-base-dir (repo-root)
  "Return the fan-out worktrees base directory for REPO-ROOT."
  (let ((default-directory (file-name-as-directory repo-root))
        (agent-shell-cwd-function (lambda () repo-root)))
    (agent-shell--dot-subdir "worktrees")))

(defun agent-shell-fanout--directory-latest-mtime (directory max-depth)
  "Return latest modification time below DIRECTORY, descending MAX-DEPTH levels."
  (let ((latest (file-attribute-modification-time
                 (file-attributes directory))))
    (cl-labels ((scan (dir depth)
                  (when (and (> depth 0) (file-directory-p dir))
                    (dolist (entry (directory-files
                                    dir t directory-files-no-dot-files-regexp))
                      (when-let* ((attrs (file-attributes entry))
                                  (mtime
                                   (file-attribute-modification-time attrs)))
                        (when (time-less-p latest mtime)
                          (setq latest mtime))
                        (when (eq t (file-attribute-type attrs))
                          (scan entry (1- depth))))))))
      (scan directory max-depth))
    latest))

(defun agent-shell-fanout--worktree-parent-in-use-p (directory)
  "Return non-nil when an `agent-shell' buffer is rooted below DIRECTORY."
  (seq-some (lambda (buffer)
              (with-current-buffer buffer
                (file-in-directory-p default-directory directory)))
            (agent-shell-buffers)))

(defun agent-shell-fanout--cleanup-stale-worktrees (base-dir)
  "Offer to remove fan-out worktree parents under BASE-DIR that look stale."
  (when (and agent-shell-fanout-worktree-cleanup-age-days
             (file-directory-p base-dir))
    (let* ((cutoff (time-subtract
                    (current-time)
                    (days-to-time
                     agent-shell-fanout-worktree-cleanup-age-days)))
           (worktree-parents
            (seq-filter #'file-directory-p
                        (directory-files
                         base-dir t directory-files-no-dot-files-regexp))))
      (dolist (worktree-parent worktree-parents)
        (let ((last-touched
               (agent-shell-fanout--directory-latest-mtime worktree-parent 3)))
          (when (and (time-less-p last-touched cutoff)
                     (not (agent-shell-fanout--worktree-parent-in-use-p
                           worktree-parent))
                     (yes-or-no-p
                      (format "Remove stale agent worktree %s (last touched %s)? "
                              worktree-parent
                              (format-time-string
                               "%Y-%m-%d %H:%M" last-touched))))
            (delete-directory worktree-parent t nil)))))))

(defun agent-shell-fanout-default-repositories (repo-root)
  "Return repository roots to fan out for REPO-ROOT."
  (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
         (parent (file-name-directory (directory-file-name repo-root))))
    (cons
     repo-root
     (thread-last
       agent-shell-fanout-adjacent-repository-names
       (seq-map (lambda (name) (expand-file-name name parent)))
       (seq-filter #'file-directory-p)
       (seq-filter (lambda (path) (not (file-equal-p repo-root path))))))))

(defun agent-shell-fanout--worktree-create (git-root parent-folder branch base-ref)
  "Create BRANCH worktree from GIT-ROOT under PARENT-FOLDER.

Return the new worktree directory on success.  If the directory already exists
and is not currently used by an `agent-shell' buffer, return it for reuse."
  (let ((worktree-dir
         (expand-file-name
          (file-name-nondirectory (directory-file-name git-root))
          parent-folder)))
    (make-directory parent-folder t)
    (let ((default-directory (file-name-as-directory git-root)))
      (process-file "git" nil nil nil "worktree" "prune")
      (if (zerop (process-file "git" nil nil nil
                               "worktree" "add" "-b" branch
                               worktree-dir base-ref))
          worktree-dir
        (when (and (file-exists-p worktree-dir)
                   (not (seq-some
                         (lambda (buffer)
                           (file-in-directory-p
                            (with-current-buffer buffer default-directory)
                            worktree-dir))
                         (agent-shell-buffers))))
          worktree-dir)))))

(defun agent-shell-fanout--worktrees-create-with-suffix
    (repo-roots base-dir slug suffix base-ref)
  "Create related worktrees under BASE-DIR with SLUG and optional SUFFIX."
  (let* ((parent-folder
          (expand-file-name
           (if suffix (format "%s-%02d" slug suffix) slug)
           base-dir))
         (branch
          (format "agent-shell/%s"
                  (file-name-nondirectory
                   (directory-file-name parent-folder)))))
    (let ((created-worktrees
           (seq-filter
            #'cdr
            (seq-map
             (lambda (repo-root)
               (cons repo-root
                     (agent-shell-fanout--worktree-create
                      repo-root parent-folder branch base-ref)))
             repo-roots))))
      (if (= (seq-length created-worktrees) (seq-length repo-roots))
          created-worktrees
        (mapc (lambda (repo-root-and-new-worktree-dir)
                (let ((default-directory
                       (car repo-root-and-new-worktree-dir)))
                  (process-file "git" nil nil nil
                                "worktree" "remove"
                                (cdr repo-root-and-new-worktree-dir))
                  (process-file "git" nil nil nil
                                "branch" "-d" branch)))
              created-worktrees)
        (agent-shell-fanout--worktrees-create-with-suffix
         repo-roots base-dir slug (1+ (or suffix 0)) base-ref)))))

(defun agent-shell-fanout--worktrees-create (repo-root title)
  "Create or reuse an agent-shell worktree below REPO-ROOT for TITLE."
  (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
         (repo-roots (funcall agent-shell-fanout-repositories-function
                              repo-root))
         (base-dir (agent-shell-fanout--worktrees-base-dir repo-root))
         (slug (thread-last
                 (if (string-match "\\`[A-Z][A-Z]+-[0-9]+\\b" title)
                     (match-string 0 title)
                   title)
                 (downcase)
                 (replace-regexp-in-string "[^[:alnum:]]+" "-")
                 (replace-regexp-in-string "\\`-+\\|-+\\'" "")))
         (slug (if (string-empty-p slug) "task" slug))
         (base-ref (agent-shell-fanout--worktree-base-ref repo-root)))
    (unless base-ref
      (user-error "Could not find a usable base ref in %s" repo-root))
    (when-let ((created-worktrees
                (agent-shell-fanout--worktrees-create-with-suffix
                 repo-roots base-dir slug nil base-ref)))
      (cdr (car created-worktrees)))))

(defun agent-shell-fanout--repo-root (directory)
  "Return the Git repository root for DIRECTORY, or nil."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name directory))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun agent-shell-fanout--preferred-config (&optional prompt)
  "Return the preferred `agent-shell' config.

When PROMPT is non-nil, prompt for an agent config if no preferred config is
available.  Programmatic fan-out calls should not unexpectedly enter the
minibuffer."
  (let ((config
         (cond
          ((null agent-shell-preferred-agent-config) nil)
          ((symbolp agent-shell-preferred-agent-config)
           (seq-find
            (lambda (config)
              (if (symbolp config)
                  (setq config (funcall config)))
              (eq (map-elt config :identifier)
                  agent-shell-preferred-agent-config))
            agent-shell-agent-configs))
          ((listp agent-shell-preferred-agent-config)
           agent-shell-preferred-agent-config))))
    (cond
     (config (if (symbolp config) (funcall config) (copy-alist config)))
     (prompt
      (copy-alist (agent-shell-select-config :prompt "Agent config: ")))
     (t nil))))

(defun agent-shell-fanout--task-spec-title (title-or-dir)
  "Return a display title for TITLE-OR-DIR."
  (if (file-name-absolute-p title-or-dir)
      (string-join
       (last (split-string (directory-file-name title-or-dir) "/" t) 2)
       "-")
    title-or-dir))

(defun agent-shell-fanout--apply-dir-locals (buffer worktree-parent)
  "Apply dir-local variables for BUFFER and persist WORKTREE-PARENT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (hack-dir-local-variables-non-file-buffer)
      (unless (local-variable-p 'agent-shell-fanout-worktree-parent)
        (add-dir-local-variable
         'agent-shell-mode 'agent-shell-fanout-worktree-parent
         worktree-parent
         (expand-file-name dir-locals-file worktree-parent))
        (when (and (buffer-file-name)
                   (string-suffix-p dir-locals-file (buffer-file-name)))
          (save-buffer)
          (kill-buffer))
        (add-to-list 'safe-local-variable-values
                     (cons 'agent-shell-fanout-worktree-parent
                           worktree-parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (hack-dir-local-variables-non-file-buffer)))))))

;;;###autoload
(defun agent-shell-fanout-worktrees
    (task-specs &optional directory session-strategy)
  "Create one worktree-backed `agent-shell' per entry in TASK-SPECS.

TASK-SPECS is an alist of (TITLE . TASK) pairs.  If a TITLE is an absolute
directory path, the final path elements are used as TITLE and the working
directory is that absolute path.  Other specs create or reuse worktrees below
DIRECTORY's Git repository, rename each shell buffer to TITLE, and queue TASK.
When DIRECTORY is nil, use `default-directory'."
  (interactive
   (list (list (cons (read-string "Task title: ")
                     (read-string "Initial request: ")))))
  (let* ((titles (mapcar #'car task-specs))
         (needs-repo-root (not (seq-every-p #'file-name-absolute-p titles)))
         (directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (agent-shell-session-strategy (or session-strategy 'new))
         (config (agent-shell-fanout--preferred-config
                  (called-interactively-p 'interactive)))
         (repo-root (when needs-repo-root
                      (agent-shell-fanout--repo-root directory))))
    (when (and needs-repo-root (not repo-root))
      (user-error "Not inside a git repository: %s" directory))
    (when (seq-some #'string-blank-p titles)
      (user-error "Empty title for fanout: %s" titles))
    (unless config
      (user-error "No preferred agent-shell config is available"))
    (when needs-repo-root
      (agent-shell-fanout--cleanup-stale-worktrees
       (agent-shell-fanout--worktrees-base-dir repo-root)))
    (cl-loop for (title-or-dir . task) in task-specs
             for i from 3 by 3
             do
             (let* ((title-is-dir (file-name-absolute-p title-or-dir))
                    (title (agent-shell-fanout--task-spec-title title-or-dir))
                    (worktree-dir
                     (file-name-as-directory
                      (expand-file-name
                       (if title-is-dir
                           title-or-dir
                         (agent-shell-fanout--worktrees-create
                          repo-root title)))))
                    (config (copy-alist config))
                    (default-directory worktree-dir)
                    (prev-transcripts
                     (ignore-errors
                       (directory-files
                        (file-name-parent-directory
                         (funcall agent-shell-transcript-file-path-function))
                        nil "\\.md$"))))
               (run-with-timer
                i nil
                (lambda (worktree-dir config task)
                  (let ((default-directory worktree-dir)
                        (agent-shell-session-strategy
                         (or session-strategy 'new))
                        (agent-shell-cwd-function
                         (lambda () worktree-dir)))
                    (when-let ((shell-buffer
                                (agent-shell-start :config config)))
                      (run-with-timer
                       3 nil
                       #'agent-shell-fanout--apply-dir-locals
                       shell-buffer
                       (file-name-parent-directory
                        (directory-file-name worktree-dir)))
                      (when task
                        (run-with-timer
                         (+ 3 (random 4)) nil
                         (lambda (buffer initial-request)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (agent-shell-queue-request initial-request))))
                         shell-buffer
                         (agent-shell-fanout--initial-request task))))))
                worktree-dir
                config
                (if (or (not task)
                        (string-blank-p task)
                        prev-transcripts)
                    nil
                  task))))))

;;;###autoload
(defun agent-shell-fanout-cleanup-worktree ()
  "Delete the current fan-out worktree parent after confirmation."
  (interactive)
  (let ((worktree-parent nil))
    (when (or (boundp 'agent-shell-fanout-worktree-parent)
              (local-variable-p 'agent-shell-fanout-worktree-parent))
      (setq worktree-parent agent-shell-fanout-worktree-parent))
    (when (and worktree-parent
               (yes-or-no-p (format "Delete %s? " worktree-parent)))
      (kill-buffer)
      (delete-directory worktree-parent t nil))))

(provide 'agent-shell-fanout)

;;; agent-shell-fanout.el ends here
