;;; agent-shell-ralph.el --- Keep agent-shell sessions moving -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.55.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Buffer-local continuation rules and retry helpers for `agent-shell'.
;;
;; Use `agent-shell-ralph-setup' to configure and enable a continuation rule in
;; the current `agent-shell' buffer.  Use
;; `agent-shell-ralph-rate-limit-retry-mode' in `agent-shell-mode-hook' to retry
;; a standard continuation prompt after transient rate-limit errors.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-utils)
(require 'map)
(require 'subr-x)

(defcustom agent-shell-ralph-rate-limit-retry-prompt
  "We got interrupted, continue."
  "Prompt queued by `agent-shell-ralph-rate-limit-retry-mode'."
  :type 'string
  :group 'agent-shell-utils)

(defcustom agent-shell-ralph-rate-limit-message-regexp
  (rx (or "429 Too Many Requests"
          "Too Many Requests"
          "rate limit"
          "rate-limit"
          "ResponseTooManyFailedAttempts"
          "exceeded retry limit"))
  "Regexp matching transient rate-limit error messages."
  :type 'regexp
  :group 'agent-shell-utils)

(defvar agent-shell-ralph--type-history nil)
(defvar agent-shell-ralph--trigger-history nil)
(defvar agent-shell-ralph--shell-history nil)
(defvar agent-shell-ralph--elisp-history nil)
(defvar agent-shell-ralph--prompt-history nil)

(defvar agent-shell-ralph-mode)

(defvar-local agent-shell-ralph--check-type nil
  "Continuation check type for the current buffer.

The value is either the symbol `shell' or `elisp'.")

(defvar-local agent-shell-ralph--check-form nil
  "Continuation check form for the current buffer.

For `shell' checks this is a shell command string.  For `elisp' checks this is
a string containing a form to read and evaluate.")

(defvar-local agent-shell-ralph--trigger 'success
  "Outcome that should inject the continuation prompt.

The value is either `success' or `failure'.")

(defvar-local agent-shell-ralph--prompt nil
  "Prompt to inject into the current `agent-shell' buffer.")

(defvar-local agent-shell-ralph--subscription nil
  "Event subscription token for the current continuation mode.")

(defvar-local agent-shell-ralph--last-result nil
  "Most recent continuation check result plist.")

(defvar-local agent-shell-ralph-rate-limit-retry-mode--subscription-token nil
  "Event subscription token for rate-limit retry mode.")

(defun agent-shell-ralph--ready-p ()
  "Return non-nil when the current `agent-shell' buffer is ready for input."
  (eq (agent-shell-status) 'ready))

(defun agent-shell-ralph--ensure-agent-shell ()
  "Fail unless the current buffer is an `agent-shell' buffer."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "This command only works in an agent-shell buffer")))

(defun agent-shell-ralph--read-check-type ()
  "Read a continuation check type."
  (intern
   (completing-read
    "Continue when checking via: "
    '("shell" "elisp")
    nil t nil 'agent-shell-ralph--type-history
    (when agent-shell-ralph--check-type
      (symbol-name agent-shell-ralph--check-type)))))

(defun agent-shell-ralph--read-trigger ()
  "Read which outcome should inject the continuation prompt."
  (intern
   (completing-read
    "Inject prompt on: "
    '("success" "failure")
    nil t nil 'agent-shell-ralph--trigger-history
    (symbol-name agent-shell-ralph--trigger))))

(defun agent-shell-ralph--read-check-form (check-type)
  "Read a continuation check for CHECK-TYPE."
  (pcase check-type
    ('shell
     (read-shell-command "Shell command: "
                         (or (and (eq agent-shell-ralph--check-type 'shell)
                                  agent-shell-ralph--check-form)
                             nil)
                         'agent-shell-ralph--shell-history))
    ('elisp
     (read-from-minibuffer
      "Elisp form: "
      (or (and (eq agent-shell-ralph--check-type 'elisp)
               agent-shell-ralph--check-form)
          nil)
      read-expression-map nil
      'agent-shell-ralph--elisp-history))
    (_
     (user-error "Unsupported continuation check type: %S" check-type))))

(defun agent-shell-ralph--describe-rule ()
  "Return a short description of the current continuation rule."
  (format "%s %s -> %s"
          (capitalize (symbol-name agent-shell-ralph--trigger))
          (pcase agent-shell-ralph--check-type
            ('shell "shell")
            ('elisp "elisp")
            (_ "check"))
          (string-trim (or agent-shell-ralph--prompt ""))))

;;;###autoload
(defun agent-shell-ralph-configure ()
  "Configure the continuation rule for the current `agent-shell' buffer."
  (interactive)
  (agent-shell-ralph--ensure-agent-shell)
  (let* ((check-type (agent-shell-ralph--read-check-type))
         (check-form (string-trim
                      (agent-shell-ralph--read-check-form check-type)))
         (trigger (agent-shell-ralph--read-trigger))
         (prompt (string-trim
                  (read-string "Injected prompt: "
                               agent-shell-ralph--prompt
                               'agent-shell-ralph--prompt-history))))
    (when (string-empty-p check-form)
      (user-error "Continuation check cannot be empty"))
    (when (string-empty-p prompt)
      (user-error "Injected prompt cannot be empty"))
    (setq-local agent-shell-ralph--check-type check-type
                agent-shell-ralph--check-form check-form
                agent-shell-ralph--trigger trigger
                agent-shell-ralph--prompt prompt)
    (message "agent-shell ralph rule configured: %s"
             (agent-shell-ralph--describe-rule))))

(defun agent-shell-ralph--eval-shell (command)
  "Run COMMAND in the current buffer's `default-directory'."
  (with-temp-buffer
    (let ((status (call-process shell-file-name nil (current-buffer) nil
                                shell-command-switch command))
          (output nil))
      (setq output (string-trim (buffer-string)))
      (list :success (and (integerp status) (zerop status))
            :status status
            :output output))))

(defun agent-shell-ralph--eval-elisp (form-string)
  "Evaluate FORM-STRING and return a continuation result plist."
  (condition-case err
      (let ((value (eval (read form-string) t)))
        (list :success (not (null value))
              :status value
              :output (string-trim (format "%S" value))))
    (error
     (list :success nil
           :status 'error
           :output (error-message-string err)))))

(defun agent-shell-ralph--evaluate ()
  "Evaluate the current continuation rule."
  (pcase agent-shell-ralph--check-type
    ('shell
     (agent-shell-ralph--eval-shell agent-shell-ralph--check-form))
    ('elisp
     (agent-shell-ralph--eval-elisp agent-shell-ralph--check-form))
    (_
     (user-error "No continuation rule configured in %s" (buffer-name)))))

(defun agent-shell-ralph--should-inject-p (result)
  "Return non-nil when RESULT matches the configured trigger."
  (eq (if (plist-get result :success) 'success 'failure)
      agent-shell-ralph--trigger))

(defun agent-shell-ralph--maybe-inject (&optional source)
  "Evaluate the continuation rule and queue a prompt when it matches.

SOURCE is a short string used for status messages."
  (when (and agent-shell-ralph-mode
             (agent-shell-ralph--ready-p))
    (let ((result (agent-shell-ralph--evaluate)))
      (setq-local agent-shell-ralph--last-result result)
      (when (agent-shell-ralph--should-inject-p result)
        (agent-shell-prompt-queue agent-shell-ralph--prompt)
        (message "agent-shell ralph queued in %s (%s: %s)"
                 (buffer-name)
                 (or source "manual")
                 (or (plist-get result :output)
                     (plist-get result :status)))
        t))))

(defun agent-shell-ralph--subscribe ()
  "Subscribe to the current buffer's turn completion events."
  (unless agent-shell-ralph--subscription
    (let ((buffer (current-buffer)))
      (setq-local
       agent-shell-ralph--subscription
       (agent-shell-subscribe-to
        :shell-buffer buffer
        :event 'turn-complete
        :on-event (lambda (_event)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (agent-shell-ralph--maybe-inject
                         "turn-complete")))))))))

(defun agent-shell-ralph--unsubscribe ()
  "Remove the current continuation mode subscription."
  (when agent-shell-ralph--subscription
    (agent-shell-unsubscribe
     :subscription agent-shell-ralph--subscription)
    (setq-local agent-shell-ralph--subscription nil)))

;;;###autoload
(define-minor-mode agent-shell-ralph-mode
  "Keep the current `agent-shell' moving while a local rule matches.

When enabled, the current buffer stores a single continuation rule.  At the end
of each turn, the rule is evaluated.  If it matches the configured trigger,
`agent-shell-prompt-queue' is called with the configured prompt."
  :lighter " Ralph"
  :group 'agent-shell-utils
  (if agent-shell-ralph-mode
      (condition-case err
          (progn
            (agent-shell-ralph--ensure-agent-shell)
            (unless (and agent-shell-ralph--check-type
                         agent-shell-ralph--check-form
                         agent-shell-ralph--prompt)
              (agent-shell-ralph-configure))
            (agent-shell-ralph--subscribe)
            (when (agent-shell-ralph--ready-p)
              (agent-shell-ralph--maybe-inject "enable")))
        (quit
         (setq agent-shell-ralph-mode nil))
        (error
         (setq agent-shell-ralph-mode nil)
         (signal (car err) (cdr err))))
    (agent-shell-ralph--unsubscribe)))

(defun agent-shell-ralph--rate-limit-event-p (event)
  "Return non-nil when EVENT looks like a rate-limit error."
  (let* ((data (or (map-elt event :data) event))
         (code (map-elt data :code))
         (text (map-elt data :message)))
    (or (equal code 429)
        (and text
             (string-match-p agent-shell-ralph-rate-limit-message-regexp
                             text)))))

(defun agent-shell-ralph--handle-rate-limit (event buffer)
  "Handle possible rate-limit EVENT for BUFFER."
  (when (and (buffer-live-p buffer)
             (agent-shell-ralph--rate-limit-event-p event))
    (let ((delay (+ 30 (random (1+ 30)))))
      (message "Rate limit in %s, scheduling a retry in %s seconds"
               buffer delay)
      (run-with-timer
       delay nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (if (agent-shell-ralph--ready-p)
                 (agent-shell-prompt-queue
                  agent-shell-ralph-rate-limit-retry-prompt)
               (message "Agent shell %s is %s after interruption; not queuing"
                        buf (agent-shell-status))))))
       buffer))))

;;;###autoload
(define-minor-mode agent-shell-ralph-rate-limit-retry-mode
  "Retry `agent-shell' requests after transient rate-limit errors."
  :global nil
  :lighter " RL-Retry"
  :group 'agent-shell-utils
  (if agent-shell-ralph-rate-limit-retry-mode
      (run-with-idle-timer
       5
       nil
       (lambda (b)
         (when (buffer-live-p b)
           (with-current-buffer b
             (when agent-shell-ralph-rate-limit-retry-mode
               (agent-shell-ralph--ensure-agent-shell)
               (setq agent-shell-ralph-rate-limit-retry-mode--subscription-token
                     (agent-shell-subscribe-to
                      :shell-buffer b
                      :event 'error
                      :on-event
                      (lambda (event)
                        (agent-shell-ralph--handle-rate-limit event b))))))))
       (current-buffer))
    (when agent-shell-ralph-rate-limit-retry-mode--subscription-token
      (agent-shell-unsubscribe
       :subscription agent-shell-ralph-rate-limit-retry-mode--subscription-token)
      (setq agent-shell-ralph-rate-limit-retry-mode--subscription-token nil))))

(provide 'agent-shell-ralph)

;;; agent-shell-ralph.el ends here
