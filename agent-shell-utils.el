;;; agent-shell-utils.el --- Utilities for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.52.1"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared customization group for optional agent-shell utility modules.

;;; Code:

(require 'agent-shell)

(defgroup agent-shell-utils nil
  "Optional utilities for `agent-shell'."
  :group 'agent-shell
  :prefix "agent-shell-")

;;;###autoload
(defun agent-shell-utils-unstick ()
  "Force the current idle `agent-shell' buffer back to prompt-ready state."
  (interactive)
  (when (map-elt agent-shell--state :active-requests)
    (user-error "Agent still has active requests; not forcing prompt"))
  (unless shell-maker--config
    (user-error "No shell-maker config in this buffer"))
  (let ((shell-maker--buffer-name-override (buffer-name)))
    (shell-maker-finish-output :config shell-maker--config :success t))
  (when (fboundp 'agent-shell--emit-event)
    (agent-shell--emit-event :event 'prompt-ready)))

(provide 'agent-shell-utils)

;;; agent-shell-utils.el ends here
