;;; agent-shell-nono.el --- Nono profile launcher for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.55.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Launch agents using nono JSON profiles, optionally within a systemd
;; resource-limited scope.  Linux sessions get a private tmpfs /tmp when
;; bubblewrap is available.  Select a profile for new sessions with
;; `agent-shell-nono-select-profile' and enable `agent-shell-nono-mode'.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-utils)
(require 'subr-x)

(defcustom agent-shell-nono-profile-directory
  (expand-file-name "nono/" user-emacs-directory)
  "Directory containing your nono JSON profiles.
A missing directory or profile is an error; no alternative policy is used."
  :type 'directory
  :group 'agent-shell-utils)

(defcustom agent-shell-nono-profile "developer.json"
  "Profile filename in `agent-shell-nono-profile-directory' for new sessions.
Each agent buffer remembers its profile path on first launch.  Selecting
a different default does not change that buffer's subsequent launches.
Nono reads the selected policy file at each process launch."
  :type 'string
  :group 'agent-shell-utils)

(defcustom agent-shell-nono-enabled t
  "When non-nil, require nono to sandbox agent processes.
Linux sessions also get a private tmpfs /tmp when bubblewrap is available.
Missing nono is an error.  Explicitly setting this to nil leaves only the
optional systemd resource limits."
  :type 'boolean
  :group 'agent-shell-utils)

(defcustom agent-shell-nono-cpu-limit 4
  "Maximum CPU quota, in CPUs, to request through systemd."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-nono-memory-limit-gb 32
  "Maximum memory limit, in GiB, to request through systemd."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-nono-memory-fraction 0.8
  "Fraction of host memory to request through systemd."
  :type 'number
  :group 'agent-shell-utils)

(defvar-local agent-shell-nono--session-profile nil
  "Absolute nono profile path selected for this agent buffer.")

(defvar agent-shell-nono--previous-command-prefix nil)

(defun agent-shell-nono--profile-file (name)
  "Resolve and validate profile filename NAME without interpreting its policy."
  (unless (and (stringp name) (equal name (file-name-nondirectory name))
               (string-suffix-p ".json" name))
    (user-error "Expected a profile filename such as developer.json: %S" name))
  (let ((file (expand-file-name name agent-shell-nono-profile-directory)))
    (when (file-remote-p file)
      (user-error "Nono profiles must be local files"))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Nono profile is missing or unreadable: %s" file))
    file))

;;;###autoload
(defun agent-shell-nono-select-profile (name)
  "Select nono profile NAME for future agent buffers.
Existing agent buffers keep their selected profile.  Use Customize to
persist the default across Emacs restarts."
  (interactive
   (list (completing-read
          "Nono profile for new agents: "
          (directory-files agent-shell-nono-profile-directory nil "\\.json\\'")
          nil t nil nil agent-shell-nono-profile)))
  (agent-shell-nono--profile-file name)
  (setq-default agent-shell-nono-profile name)
  (message "New agent buffers will use nono profile %s" name))

(defun agent-shell-nono--memory-limit ()
  "Return the systemd memory limit string, or nil when unavailable."
  (when-let* ((total-kib (ignore-errors (car (memory-info))))
              (total-gib (/ total-kib 1024.0 1024.0))
              (limit (max 1
                          (min agent-shell-nono-memory-limit-gb
                               (floor (* agent-shell-nono-memory-fraction
                                         total-gib))))))
    (format "MemoryMax=%dG" limit)))

(defun agent-shell-nono--systemd-prefix ()
  "Return the optional systemd resource-limit prefix."
  (when (executable-find "systemd-run" t)
    (let ((num-cpus (max 1 (min agent-shell-nono-cpu-limit
                                (/ (max 1 (num-processors)) 2)))))
      (append
       `("systemd-run" "--user" "--scope" "--quiet"
         "-p" ,(format "CPUQuota=%d00%%" num-cpus))
       (when-let* ((memory (agent-shell-nono--memory-limit)))
         (list "-p" memory))
       '("--")))))

(defun agent-shell-nono--tmpfs-prefix (nono profile)
  "Return an optional Linux mount wrapper for NONO and PROFILE.
Return nil when bubblewrap is unavailable or on other platforms.
The private /tmp must exist before nono installs its filesystem policy.
Reject launch inputs that would be hidden by the mount rather than
exposing any part of the host's /tmp inside it."
  (when (eq system-type 'gnu/linux)
    (if-let* ((bwrap (executable-find "bwrap")))
        (let ((tmp (file-name-as-directory (file-truename "/tmp"))))
          (dolist (path (list nono profile default-directory))
            (when (or (string-prefix-p "/tmp/" (file-name-as-directory (expand-file-name path)))
                      (string-prefix-p tmp (file-name-as-directory (file-truename path))))
              (user-error "Private /tmp would hide this launch path; move it outside /tmp: %s" path)))
          (list bwrap "--die-with-parent"
                "--bind" "/" "/"
                "--dev-bind" "/dev" "/dev"
                "--perms" "1777" "--tmpfs" "/tmp"
                "--setenv" "TMPDIR" "/tmp"
                "--setenv" "TMP" "/tmp"
                "--setenv" "TEMP" "/tmp"
                "--"))
      (message "Bubblewrap not found; nono will use the host's temporary directories")
      nil)))

;;;###autoload
(defun agent-shell-nono-command-prefix (buffer)
  "Return a nono launch prefix for agent BUFFER, with optional systemd limits.
Local Linux sessions get a private tmpfs /tmp when bubblewrap is available.
Use the buffer's directory as nono's WORKDIR.  Remote processes are left
unwrapped.  Missing nono/profiles are errors when sandboxing is enabled.
Nono validates and enforces the selected JSON policy in either case."
  (with-current-buffer buffer
    (if (file-remote-p default-directory)
        (progn
          (message "The nono prefix supports local agent processes only")
          nil)
      (let ((prefix (agent-shell-nono--systemd-prefix)))
        (if (not agent-shell-nono-enabled)
            prefix
          (let* ((nono (or (executable-find "nono")
                           (user-error "Nono is required; activate it with mise use -g nono@0.78.0")))
                 (profile (or agent-shell-nono--session-profile
                              (agent-shell-nono--profile-file agent-shell-nono-profile))))
            (unless (and (file-regular-p profile) (file-readable-p profile))
              (user-error "Nono session profile is missing or unreadable: %s" profile))
            (let ((mount-prefix (agent-shell-nono--tmpfs-prefix nono profile)))
              (setq agent-shell-nono--session-profile profile)
              (append prefix mount-prefix
                      (list nono "--silent" "run" "--profile" profile
                            "--workdir" (expand-file-name default-directory)
                            "--allow-cwd" "--no-rollback" "--no-rollback-prompt" "--startup-timeout" "0"
                            "--")))))))))

;;;###autoload
(define-minor-mode agent-shell-nono-mode
  "Launch agents with nono and `agent-shell-nono-command-prefix'."
  :global t
  :lighter " AS-Nono"
  :group 'agent-shell-utils
  (if agent-shell-nono-mode
      (unless (eq agent-shell-command-prefix #'agent-shell-nono-command-prefix)
        (setq agent-shell-nono--previous-command-prefix agent-shell-command-prefix
              agent-shell-command-prefix #'agent-shell-nono-command-prefix))
    (when (eq agent-shell-command-prefix #'agent-shell-nono-command-prefix)
      (setq agent-shell-command-prefix agent-shell-nono--previous-command-prefix))))

(provide 'agent-shell-nono)

;;; agent-shell-nono.el ends here
