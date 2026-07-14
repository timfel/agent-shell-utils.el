;;; agent-shell-bwrap.el --- Bubblewrap command prefix for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.55.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional `bwrap' and `systemd-run' command prefix for `agent-shell'
;; processes.  Enable with `agent-shell-bwrap-mode' or set
;; `agent-shell-command-prefix' to `agent-shell-bwrap-command-prefix'.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-utils)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar agent-shell-bwrap-enabled t
  "When non-nil, use `bwrap' when it is available.")

(defcustom agent-shell-bwrap-cpu-limit 4
  "Maximum CPU quota, in CPUs, to request through systemd."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-bwrap-memory-limit-gb 32
  "Maximum memory limit, in GiB, to request through systemd."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-bwrap-memory-fraction 0.8
  "Fraction of host memory to request through systemd."
  :type 'number
  :group 'agent-shell-utils)

(defcustom agent-shell-bwrap-temp-prefix "/tmp/agent-shell-session"
  "Prefix used when creating per-session temporary directories."
  :type 'string
  :group 'agent-shell-utils)

(defcustom agent-shell-bwrap-cleanup-temp-days 6
  "Delete stale temp directories older than this many days.

Set to nil to disable cleanup."
  :type '(choice (const :tag "Disable cleanup" nil) integer)
  :group 'agent-shell-utils)

(defcustom agent-shell-bwrap-dirs
  '(("./" . w)
    ("~/.cache" . w)
    ("~/.cline" . w)
    ("~/.codex" . w)
    ("~/.config/goose" . w)
    ("~/.local/share/goose" . w)
    ("~/.local/state/goose" . w)
    ("~/.local/share/opencode" . w)
    ("~/.eclipse" . w)
    ("~/.gradle" . w)
    ("~/.hermes" . w)
    ("~/.m2" . w)
    ("~/.mx" . w)
    ("~/.npm" . w)
    ("~/.opencode" . w)
    ("~/.pi" . w)
    ("~/dev/.metadata" . w)
    ("~/dev/ci-overlays/.git" . w)
    ("~/dev/graal/.git" . w)
    ("~/dev/graal-enterprise/.git" . w)
    ("~/dev/graalpython/.git" . w)
    ("~/dev/eclipse" . w)
    ("~/dev/mx" . w)
    ("../graal" . w)
    ("../graalos" . w)
    ("../graalos-image-builder" . w)
    ("../graal-enterprise" . w)
    ("../ci-overlays" . w)

    ("~/.codex/config.toml" . w)
    ("~/.config/goose/config.yaml" . w)
    ("~/.config/goose/adversary.md" . w)
    ("~/.config/opencode/opencode.jsonc" . w)
    ("~/.cline/data/globalState.json" . w)
    ("~/.cline/data/settings/cline_mcp_settings.json" . w)

    ("~/.agents" . r)
    ("~/.bun" . r)
    ("~/.bundle" . r)
    ("~/.cargo" . r)
    ("~/.config" . r)
    ("~/.docker" . r)
    ("~/.emacs.d" . r)
    ("~/.gitconfig" . r)
    ("~/.gitignore" . r)
    ("~/.local" . r)
    ("~/.npmrc" . r)
    ("~/.nvm" . r)
    ("~/.ol" . r)
    ("~/.ssh" . r)
    ("~/.pyenv" . r)
    ("~/.rustup" . r)
    ("~/.sdkman" . r)
    ("~/dotfiles" . r)
    ("~/dev" . r)

    ("~/.config/mc" . nil)
    ("~/.config/onedrive" . nil)
    ("~/.config/pulse" . nil)
    ("~/.config/rclone" . nil))
  "Paths that `agent-shell' bwrap sessions should bind."
  :type '(repeat file)
  :group 'agent-shell-utils)

(defvar agent-shell-bwrap--previous-command-prefix nil)

(defun agent-shell-bwrap--git-common-root (directory)
  "Return the Git common root for DIRECTORY, or DIRECTORY on failure."
  (let ((default-directory
         (file-name-as-directory (expand-file-name directory))))
    (with-temp-buffer
      (if (zerop (process-file "git" nil t nil "rev-parse" "--git-common-dir"))
          (let ((gitdir (string-trim (buffer-string))))
            (if (string-empty-p gitdir)
                directory
              (file-name-directory
               (directory-file-name
                (expand-file-name gitdir default-directory)))))
        directory))))

(defun agent-shell-bwrap--memory-limit ()
  "Return the systemd memory limit string, or nil when unavailable."
  (when-let* ((total-kib (ignore-errors (car (memory-info))))
              (total-gib (/ total-kib 1024.0 1024.0))
              (limit (max 1
                          (min agent-shell-bwrap-memory-limit-gb
                               (floor (* agent-shell-bwrap-memory-fraction
                                         total-gib))))))
    (format "MemoryMax=%dG" limit)))

(defun agent-shell-bwrap--systemd-prefix ()
  "Return the optional systemd command prefix."
  (when (executable-find "systemd-run")
    (let ((num-cpus (max 1 (min agent-shell-bwrap-cpu-limit
                                (/ (max 1 (num-processors)) 2)))))
      (append
       `("systemd-run"
         "--user"
         "--scope"
         "-p"
         ,(format "CPUQuota=%d00%%" num-cpus))
       (when-let ((memory (agent-shell-bwrap--memory-limit)))
         (list "-p" memory))
       '("--")))))

(defun agent-shell-bwrap--cleanup-temp-dirs ()
  "Delete stale temp directories created by this package."
  (when agent-shell-bwrap-cleanup-temp-days
    (let* ((directory (file-name-directory agent-shell-bwrap-temp-prefix))
           (prefix (file-name-nondirectory agent-shell-bwrap-temp-prefix))
           (cutoff (time-subtract
                    (current-time)
                    (days-to-time agent-shell-bwrap-cleanup-temp-days))))
      (when (file-directory-p directory)
        (dolist (path (directory-files directory t
                                       (concat "\\`" (regexp-quote prefix))))
          (when (and (file-directory-p path)
                     (time-less-p
                      (file-attribute-modification-time
                       (file-attributes path))
                      cutoff))
            (ignore-errors
              (message "Cleaning old temporary directory %s" path)
              (delete-directory path t))))))))

;;;###autoload
(defun agent-shell-bwrap-command-prefix (_buffer)
  "Return a command prefix for `agent-shell'.

The prefix contains a `systemd-run' wrapper when configured and available.
When `agent-shell-bwrap-enabled' is non-nil and `bwrap' exists, the prefix
also creates a bubblewrap filesystem view."
  (let ((prefix (agent-shell-bwrap--systemd-prefix)))
    (if (not (and (executable-find "bwrap") agent-shell-bwrap-enabled))
        prefix
      (agent-shell-bwrap--cleanup-temp-dirs)
      (let* ((tmpdir (make-temp-file
                      agent-shell-bwrap-temp-prefix t
                      (replace-regexp-in-string
                       "[^[:alnum:]]" ""
                       (or default-directory "agent-shell")))))
        (append
         prefix
         `("bwrap" "--die-with-parent" "--new-session"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--tmpfs" ,(getenv "HOME"))
         (thread-last
           (seq-map (lambda (e) (cons (expand-file-name (car e)) (cdr e))) agent-shell-bwrap-dirs)
           (seq-sort (lambda (e1 e2) (string-lessp (car e1) (car e2))))
           (seq-filter (lambda (e) (file-exists-p (car e))))
           (seq-mapcat (lambda (e)
                         (let ((p (car e))
                               (m (cdr e)))
                           (cond
                            ((eq m 'w) (list "--bind" (file-truename p) (file-truename p)
                                             "--bind" p p))
                            ((eq m 'r) (list "--ro-bind" (file-truename p) (file-truename p)
                                             "--ro-bind" p p))
                            (t (list "--tmpfs" p)))))))
         `("--proc" "/proc"
           "--dev" "/dev"
           "--chdir" ,default-directory
           "--setenv" "HTTP_PROXY" ,(or (getenv "HTTP_PROXY") "")
           "--setenv" "HTTPS_PROXY" ,(or (getenv "HTTPS_PROXY") "")
           "--setenv" "NO_PROXY" ,(or (getenv "NO_PROXY") "")
           "--setenv" "HOME" ,(getenv "HOME")
           "--setenv" "TMPDIR" ,tmpdir
           "--setenv" "XDG_CACHE_INNER" ,(expand-file-name ".agent-shell/xdgcache")
           "--setenv" "XDG_STATE_INNER" ,(expand-file-name ".agent-shell/xdgstate")
           "--setenv" "XDG_RUNTIME_INNER" ,(expand-file-name ".agent-shell/xdgruntime")
           "--"))))))

;;;###autoload
(define-minor-mode agent-shell-bwrap-mode
  "Use `agent-shell-bwrap-command-prefix' for `agent-shell' commands."
  :global t
  :lighter " AS-Bwrap"
  :group 'agent-shell-utils
  (if agent-shell-bwrap-mode
      (progn
        (setq agent-shell-bwrap--previous-command-prefix
              agent-shell-command-prefix)
        (setq agent-shell-command-prefix
              #'agent-shell-bwrap-command-prefix))
    (when (eq agent-shell-command-prefix
              #'agent-shell-bwrap-command-prefix)
      (setq agent-shell-command-prefix
            agent-shell-bwrap--previous-command-prefix))))

(provide 'agent-shell-bwrap)

;;; agent-shell-bwrap.el ends here
