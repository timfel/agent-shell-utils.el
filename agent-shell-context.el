;;; agent-shell-context.el --- Additional context sources for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.52.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional `agent-shell-context-sources' for recent Emacs buffers, built-in VC
;; diffs, and Magit diffs.  Enable all configured sources with
;; `agent-shell-context-mode', or add individual functions to
;; `agent-shell-context-sources'.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-utils)
(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'project)
(require 'ring)
(require 'seq)
(require 'server)
(require 'subr-x)
(require 'vc)
(require 'vc-dir)

(declare-function magit-buffer-revision "magit-mode")
(declare-function magit-current-section "magit-section")
(declare-function magit-diff-file-header "magit-diff")
(declare-function magit-diff-hunk-region-patch "magit-diff")
(declare-function magit-file-section-p "magit-diff")
(declare-function magit-hunk-section-p "magit-diff")
(declare-function magit-section-internal-region-p "magit-section")

(defcustom agent-shell-context-buffer-limit 5
  "Maximum number of recently used buffers to include."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-context-lines-around-point 10
  "How many lines of context to include before and after point."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-context-shell-output-lines 20
  "How many trailing lines of shell output to include."
  :type 'integer
  :group 'agent-shell-utils)

(defcustom agent-shell-context-special-buffer-regexps
  '("\\`\\*Backtrace\\*\\'"
    "\\`\\*Jira.*"
    "\\`\\*ci-dashboard\\*\\'")
  "Buffer name regexps that should always be eligible for context collection.

These buffers are considered even when they are not part of the target
`agent-shell' buffer's project."
  :type '(repeat regexp)
  :group 'agent-shell-utils)

(defcustom agent-shell-context-include-emacsclient-instructions t
  "When non-nil, include an `emacsclient' command in Emacs context."
  :type 'boolean
  :group 'agent-shell-utils)

(defcustom agent-shell-context-extra-sources
  '(agent-shell-context-magit-source
    agent-shell-context-vc-source
    agent-shell-context-emacs-source)
  "Additional context source functions installed by `agent-shell-context-mode'."
  :type '(repeat function)
  :group 'agent-shell-utils)

(defvar agent-shell-context-sent-states (make-hash-table :test 'equal)
  "Stores the last sent state for buffers keyed by buffer name.")

(defvar agent-shell-context-last-xref-summary nil
  "Stores the last sent xref summary.")

(defun agent-shell-context--project-root (buffer)
  "Return BUFFER's project root, or nil when none can be determined."
  (with-current-buffer buffer
    (let ((directory
           (cond
            ((and (derived-mode-p 'vterm-mode)
                  (boundp 'vterm--process)
                  (process-live-p vterm--process))
             (ignore-errors
               (file-truename
                (format "/proc/%d/cwd/" (process-id vterm--process)))))
            (default-directory
             (expand-file-name default-directory))
            (t nil))))
      (or (when-let ((project (and directory
                                   (project-current nil directory))))
            (expand-file-name (project-root project)))
          directory))))

(defun agent-shell-context--eligible-buffer-p (buffer project-root)
  "Return non-nil when BUFFER should be considered for PROJECT-ROOT."
  (let ((name (buffer-name buffer)))
    (and name
         (not (string-prefix-p " " name))
         (not (with-current-buffer buffer
                (derived-mode-p 'agent-shell-mode
                                'agent-shell-viewport-edit-mode
                                'agent-shell-viewport-view-mode
                                'agent-shell-diff-mode)))
         (or (seq-some (lambda (regexp)
                         (string-match-p regexp name))
                       agent-shell-context-special-buffer-regexps)
             (when-let* ((root project-root)
                         (buffer-root
                          (agent-shell-context--project-root buffer)))
               (string-prefix-p (file-name-as-directory root)
                                (file-name-as-directory buffer-root)))))))

(defun agent-shell-context--buffer-state (buffer)
  "Return a compact state object for BUFFER."
  (with-current-buffer buffer
    (list (point)
          (buffer-chars-modified-tick)
          (point-max))))

(defun agent-shell-context--tail-lines (start end line-count)
  "Return up to LINE-COUNT trailing lines between START and END."
  (save-excursion
    (goto-char end)
    (forward-line (- line-count))
    (buffer-substring-no-properties (max start (point)) end)))

(defun agent-shell-context--lines-around-point ()
  "Return a snippet around point in the current buffer."
  (save-excursion
    (let* ((start-line (max 1 (- (line-number-at-pos)
                                 agent-shell-context-lines-around-point)))
           (end-line (+ (line-number-at-pos)
                        agent-shell-context-lines-around-point))
           (start-pos (progn
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (point)))
           (end-pos (progn
                      (goto-char (point-min))
                      (forward-line end-line)
                      (point))))
      (buffer-substring-no-properties start-pos end-pos))))

(defun agent-shell-context--ring-most-recent (ring-symbol)
  "Return the newest entry in RING-SYMBOL, or nil when unavailable."
  (when (and (boundp ring-symbol)
             (ring-p (symbol-value ring-symbol))
             (> (ring-length (symbol-value ring-symbol)) 0))
    (string-trim (ring-ref (symbol-value ring-symbol) 0))))

(defun agent-shell-context--guess-last-command-from-output (text)
  "Heuristically extract the last shell command from TEXT."
  (let ((lines (reverse (split-string text "\n" t "[ \t]+"))))
    (seq-some
     (lambda (line)
       (when (string-match
              "^[^#$%>\n]*\\(?:[$#%>]\\)\\s-*\\(.+\\)$"
              line)
         (string-trim (match-string 1 line))))
     lines)))

(defun agent-shell-context--last-shell-command ()
  "Return the most recent shell command for the current shell buffer."
  (cond
   ((derived-mode-p 'eshell-mode)
    (or (agent-shell-context--ring-most-recent 'eshell-history-ring)
        (agent-shell-context--guess-last-command-from-output
         (agent-shell-context--tail-lines
          (point-min) (point-max)
          (* 3 agent-shell-context-shell-output-lines)))))
   ((derived-mode-p 'comint-mode 'shell-mode 'term-mode)
    (or (agent-shell-context--ring-most-recent 'comint-input-ring)
        (agent-shell-context--guess-last-command-from-output
         (agent-shell-context--tail-lines
          (point-min) (point-max)
          (* 3 agent-shell-context-shell-output-lines)))))
   ((derived-mode-p 'vterm-mode)
    (agent-shell-context--guess-last-command-from-output
     (agent-shell-context--tail-lines
      (point-min) (point-max)
      (* 3 agent-shell-context-shell-output-lines))))
   (t nil)))

(defun agent-shell-context--shell-snippet ()
  "Return shell context for the current buffer."
  (let* ((output (string-trim-right
                  (agent-shell-context--tail-lines
                   (point-min) (point-max)
                   agent-shell-context-shell-output-lines)))
         (command (agent-shell-context--last-shell-command))
         (command-already-present
          (and command
               (not (string-empty-p output))
               (string-match-p (regexp-quote command) output))))
    (string-join
     (delq nil
           (list (when (and command (not command-already-present))
                   (format "Last command: %s" command))
                 (unless (string-empty-p output)
                   (format "Recent output:\n%s" output))))
     "\n\n")))

(defun agent-shell-context--buffer-snippet (buffer)
  "Extract a concise snippet from BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p 'vterm-mode 'term-mode 'eshell-mode
                        'comint-mode 'shell-mode)
        (agent-shell-context--shell-snippet)
      (agent-shell-context--lines-around-point))))

(defun agent-shell-context--format-xref-history (history-list limit project-root)
  "Format up to LIMIT items from HISTORY-LIST into a readable path.

Return nil unless at least one xref target buffer would qualify for inclusion
under PROJECT-ROOT."
  (let ((items (seq-take history-list limit))
        (path '())
        (has-eligible-buffer nil))
    (dolist (item items)
      (let ((marker (cond
                     ((markerp item) item)
                     ((and (consp item) (markerp (car item))) (car item))
                     ((and (consp item) (markerp (cdr item))) (cdr item))
                     (t nil))))
        (when (and marker (marker-buffer marker))
          (let ((buffer (marker-buffer marker)))
            (when (agent-shell-context--eligible-buffer-p buffer project-root)
              (setq has-eligible-buffer t))
            (with-current-buffer buffer
              (save-excursion
                (goto-char marker)
                (push (format "%s (%s)"
                              (or (thing-at-point 'symbol t)
                                  (format "[%d]" (line-number-at-pos)))
                              (buffer-name))
                      path)))))))
    (when has-eligible-buffer
      (string-join (reverse path) " -> "))))

(defun agent-shell-context--emacsclient-command ()
  "Return an `emacsclient' command for the current server, or nil."
  (when (and agent-shell-context-include-emacsclient-instructions
             (boundp 'server-process)
             (process-live-p server-process))
    (string-join
     `("emacsclient"
       ,(cond
         ((or server-use-tcp
              (memq system-type '(windows-nt ms-dos cygwin)))
          (format "--server-file=%s"
                  (shell-quote-argument
                   (expand-file-name server-name server-auth-dir))))
         (t
          (format "--socket-name=%s"
                  (shell-quote-argument
                   (expand-file-name server-name server-socket-dir)))))
       ,(if (eq system-type 'windows-nt)
            "--eval '(load-file PATH-TO-TEMP-ELISP-SCRIPT)'"
          ""))
     " ")))

;;;###autoload
(defun agent-shell-context-emacs-source ()
  "Gather recent Emacs context that has not already been sent."
  (let* ((context-parts '())
         (buffers-found 0)
         (project-root (agent-shell-context--project-root (current-buffer)))
         (emacsclient-command (agent-shell-context--emacsclient-command)))
    (catch 'agent-shell-context-limit-reached
      (dolist (buffer (seq-subseq (buffer-list)
                                  0
                                  (min (seq-length (buffer-list))
                                       (* 2 agent-shell-context-buffer-limit))))
        (when (agent-shell-context--eligible-buffer-p buffer project-root)
          (let* ((name (buffer-name buffer))
                 (state (agent-shell-context--buffer-state buffer))
                 (last-state (gethash name agent-shell-context-sent-states)))
            (unless (equal state last-state)
              (let ((snippet (string-trim
                              (agent-shell-context--buffer-snippet buffer))))
                (unless (string-empty-p snippet)
                  (push (format "### Buffer: %s\n```\n%s\n```" name snippet)
                        context-parts)
                  (puthash name state agent-shell-context-sent-states)
                  (setq buffers-found (1+ buffers-found))
                  (when (>= buffers-found agent-shell-context-buffer-limit)
                    (throw 'agent-shell-context-limit-reached nil)))))))))
    (let* ((current-xref (and (boundp 'xref--history)
                              (seq-take xref--history 5)))
           (xref-summary (and current-xref
                              (agent-shell-context--format-xref-history
                               current-xref 5 project-root))))
      (when (and xref-summary
                 (not (string-empty-p xref-summary))
                 (not (equal xref-summary
                             agent-shell-context-last-xref-summary)))
        (push (format "### Recent Navigation (Xref)\n%s" xref-summary)
              context-parts)
        (setq agent-shell-context-last-xref-summary xref-summary)))
    (when context-parts
      (concat
       "\n\n[USER ENVIRONMENT CONTEXT - EMACS STATE]\n"
       (if emacsclient-command
           (concat
            "First evaluate whether contextual Emacs buffers may be relevant "
            "to this prompt. If so, you may use `" emacsclient-command "` "
            "to query buffer contents and key bindings before working on the "
            "task. Some buffers expose logs, issue comments, history, TODOs, "
            "or project-specific actions.\n"
            (if (eq system-type 'windows-nt)
                (concat
                 "Write temporary Elisp scripts and load them with "
                 "`emacsclient --eval' to avoid shell quoting issues.\n\n")
              "\n"))
         "\n")
       (mapconcat #'identity (nreverse context-parts) "\n\n")
       "[END CONTEXT]\n"))))

;;;###autoload
(defalias 'agent-shell-context-source #'agent-shell-context-emacs-source)

(defun agent-shell-context-vc--stringify-list (values)
  "Return VALUES as a comma-separated string."
  (mapconcat (lambda (value) (format "%s" value)) values ", "))

(defun agent-shell-context-vc--format-context (backend revisions files patch)
  "Format VC diff context for `agent-shell'."
  (string-join
   (delq nil
         (list "[VC DIFF CONTEXT]"
               (when backend
                 (format "Backend: %s" backend))
               (when revisions
                 (format "Revisions: %s"
                         (agent-shell-context-vc--stringify-list revisions)))
               (when files
                 (format "Files: %s"
                         (agent-shell-context-vc--stringify-list files)))
               ""
               "```diff"
               patch
               "```"
               "[END CONTEXT]"))
   "\n"))

(defun agent-shell-context-vc--substring (bounds)
  "Return the buffer substring for BOUNDS."
  (buffer-substring-no-properties (car bounds) (cadr bounds)))

(defun agent-shell-context-vc--file-header-before (position)
  "Return the current diff file header before POSITION."
  (save-excursion
    (goto-char position)
    (let* ((hunk-start (car (diff-bounds-of-hunk)))
           (file-start (car (diff-bounds-of-file))))
      (buffer-substring-no-properties file-start hunk-start))))

(defun agent-shell-context-vc--diff-mode-patch ()
  "Return the current `diff-mode' file, hunk, or region patch."
  (when (derived-mode-p 'diff-mode)
    (condition-case nil
        (string-trim-right
         (cond
          ((region-active-p)
           (let ((beg (region-beginning))
                 (end (region-end)))
             (concat (agent-shell-context-vc--file-header-before beg)
                     (buffer-substring-no-properties
                      (car (diff-bounds-of-hunk)) end))))
          (t
           (let* ((pos (point))
                  (hunk-bounds (ignore-errors (diff-bounds-of-hunk)))
                  (file-bounds (diff-bounds-of-file)))
             (if (and hunk-bounds (<= (car hunk-bounds) pos))
                 (concat (agent-shell-context-vc--file-header-before pos)
                         (agent-shell-context-vc--substring hunk-bounds))
               (agent-shell-context-vc--substring file-bounds))))))
      (error nil))))

(defun agent-shell-context-vc--tracked-files (backend files)
  "Return FILES that BACKEND can diff."
  (seq-filter
   (lambda (file)
     (condition-case nil
         (not (eq (vc-call-backend backend 'state file) 'unregistered))
       (error nil)))
   files))

(defun agent-shell-context-vc--vc-dir-files ()
  "Return the marked vc-dir files or the file at point."
  (when (derived-mode-p 'vc-dir-mode)
    (or (vc-dir-marked-files)
        (list (vc-dir-current-file)))))

(defun agent-shell-context-vc--vc-dir-patch ()
  "Return a synchronous VC diff for the current `vc-dir' selection."
  (when (derived-mode-p 'vc-dir-mode)
    (when-let* ((backend (or vc-dir-backend (vc-deduce-backend)))
                (files (agent-shell-context-vc--tracked-files
                        backend
                        (agent-shell-context-vc--vc-dir-files))))
      (when files
        (condition-case nil
            (with-temp-buffer
              (let ((default-directory (or (ignore-errors (vc-root-dir))
                                           default-directory)))
                (vc-call-backend backend 'diff files nil nil
                                 (current-buffer) nil)
                (string-trim-right (buffer-string))))
          (error nil))))))

;;;###autoload
(defun agent-shell-context-vc-source ()
  "Return the current built-in VC diff as `agent-shell' context, or nil."
  (let* ((patch (or (agent-shell-context-vc--diff-mode-patch)
                    (agent-shell-context-vc--vc-dir-patch)))
         (backend (cond
                   ((derived-mode-p 'diff-mode) diff-vc-backend)
                   ((derived-mode-p 'vc-dir-mode) vc-dir-backend)))
         (revisions (when (derived-mode-p 'diff-mode)
                      diff-vc-revisions))
         (files (cond
                 ((derived-mode-p 'diff-mode)
                  (ignore-errors (cadr (diff-vc-deduce-fileset))))
                 ((derived-mode-p 'vc-dir-mode)
                  (agent-shell-context-vc--vc-dir-files)))))
    (when (and patch (not (string-empty-p patch)))
      (agent-shell-context-vc--format-context backend revisions files patch))))

(defun agent-shell-context-magit--available-p ()
  "Return non-nil when Magit diff support is available."
  (and (require 'magit-mode nil t)
       (require 'magit-section nil t)
       (require 'magit-diff nil t)))

(defun agent-shell-context-magit--slot (section slot)
  "Return SECTION's SLOT value."
  (eieio-oref section slot))

(defun agent-shell-context-magit--hunk-text (section)
  "Return the diff hunk text for SECTION."
  (buffer-substring-no-properties
   (agent-shell-context-magit--slot section 'start)
   (agent-shell-context-magit--slot section 'end)))

(defun agent-shell-context-magit--file-context (section)
  "Return full file diff context for SECTION."
  (concat
   (magit-diff-file-header section)
   (mapconcat #'agent-shell-context-magit--hunk-text
              (delq nil
                    (mapcar (lambda (child)
                              (when (magit-hunk-section-p child)
                                child))
                            (agent-shell-context-magit--slot
                             section 'children)))
              "")))

(defun agent-shell-context-magit--region-context (section)
  "Return diff context for SECTION.

Use the active internal hunk region when present, otherwise include the full
hunk or file."
  (cond
   ((magit-file-section-p section)
    (agent-shell-context-magit--file-context section))
   ((and (region-active-p)
         (magit-section-internal-region-p section))
    (concat (magit-diff-file-header section)
            (magit-diff-hunk-region-patch section)))
   (t
    (concat (magit-diff-file-header section)
            (agent-shell-context-magit--hunk-text section)))))

(defun agent-shell-context-magit--selected-section ()
  "Return the current hunk or file section, or nil when none is selected.

If the region is active, it has to stay inside a single hunk body."
  (when-let ((section (magit-current-section)))
    (cond
     ((and (magit-hunk-section-p section)
           (or (not (region-active-p))
               (magit-section-internal-region-p section)))
      section)
     ((and (magit-file-section-p section)
           (not (region-active-p)))
      section))))

(defun agent-shell-context-magit--format-context (commit-sha patch)
  "Format Magit diff context for `agent-shell'."
  (string-join
   (delq nil
         (list "[MAGIT DIFF CONTEXT]"
               (when commit-sha
                 (format "Commit-ish: %s" commit-sha))
               ""
               "```diff"
               patch
               "```"
               "[END CONTEXT]"))
   "\n"))

;;;###autoload
(defun agent-shell-context-magit-source ()
  "Return the current Magit diff section as `agent-shell' context, or nil."
  (when (agent-shell-context-magit--available-p)
    (when-let* ((section (agent-shell-context-magit--selected-section))
                (patch (string-trim-right
                        (agent-shell-context-magit--region-context section))))
      (unless (string-empty-p patch)
        (agent-shell-context-magit--format-context
         (magit-buffer-revision) patch)))))

(defun agent-shell-context--install-sources (sources)
  "Install context SOURCES into `agent-shell-context-sources'.

Sources are inserted before the built-in `line' fallback when present."
  (let* ((sources (delete-dups (copy-sequence sources)))
         (base (seq-remove (lambda (source) (memq source sources))
                           agent-shell-context-sources))
         (line-index (cl-position 'line base))
         (prefix (if line-index (seq-subseq base 0 line-index) base))
         (suffix (when line-index (seq-subseq base line-index))))
    (setq agent-shell-context-sources
          (append prefix sources suffix))))

(defun agent-shell-context--remove-sources (sources)
  "Remove context SOURCES from `agent-shell-context-sources'."
  (setq agent-shell-context-sources
        (seq-remove (lambda (source) (memq source sources))
                    agent-shell-context-sources)))

;;;###autoload
(define-minor-mode agent-shell-context-mode
  "Enable additional context sources for `agent-shell'."
  :global t
  :lighter " AS-Ctx"
  :group 'agent-shell-utils
  (if agent-shell-context-mode
      (agent-shell-context--install-sources agent-shell-context-extra-sources)
    (agent-shell-context--remove-sources agent-shell-context-extra-sources)))

(provide 'agent-shell-context)

;;; agent-shell-context.el ends here
