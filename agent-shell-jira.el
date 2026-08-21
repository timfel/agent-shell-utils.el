;;; agent-shell-jira.el --- Jira helpers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff
;; URL: https://github.com/timfel/agent-shell-utils
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.58.1") (jira "2.21.0"))
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Local helpers around `jira.el' for querying and opening Jira issues,
;; including `agent-shell' rendering integration for Jira issue links.

;;; Code:

(require 'tabulated-list)
(require 'tablist)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'jira)
(require 'jira-utils)
(require 'agent-shell)
(require 'agent-shell-markdown)
(require 'agent-shell-utils)
(require 'agent-shell-fanout)

(defvar jira-detail--current-key nil)
(defvar jira-issues-key-summary-map)
(defvar agent-shell-markdown-render-function)

(declare-function agent-shell-markdown--open-link "agent-shell-markdown" (url))

(defcustom agent-shell-jira-issue-regexp "\\bGR\\(AALOS\\)?-[0-9]+\\b"
  "Regexp matching Jira issue keys handled by `agent-shell-jira'."
  :type 'regexp
  :group 'agent-shell-utils)

(defun agent-shell-jira--issue-key (text)
  "Return a Jira issue key extracted from TEXT, or nil.

Plain issue keys such as \"GR-123\" are accepted directly. URLs are only
treated as Jira links when they contain both \"jira\" and a Jira issue key."
  (when (stringp text)
    (let ((case-fold-search t))
      (cond
       ((and (string-match-p "\\`https?://" text)
             (string-match-p "jira" text)
             (string-match agent-shell-jira-issue-regexp text))
        (match-string 0 text))
       ((and (string-match-p (concat "\\`" agent-shell-jira-issue-regexp "\\'") text)
             text)
        text)))))

(defun agent-shell-jira-open-issue (text)
  "Open the Jira issue identified by TEXT with `jira-detail-show-issue'."
  (interactive "sJira issue or URL: ")
  (let ((issue-key (agent-shell-jira--issue-key text)))
    (unless issue-key
      (user-error "No Jira issue key found in %S" text))
    (unless (require 'jira-detail nil t)
      (user-error "jira-detail.el is not available"))
    (cond
     ((fboundp 'jira-detail-show-issue)
      (jira-detail-show-issue issue-key))
     ((and (fboundp 'jira-api-call)
           (fboundp 'jira-detail--issue))
      (jira-api-call
       "GET" (concat "issue/" issue-key)
       :callback (lambda (data _response)
                   (jira-detail--issue issue-key data))))
     (t
      (user-error "jira-detail is loaded, but no issue-opening entrypoint is available")))))

(defun agent-shell-jira--ret-action-at-point ()
  "Return the command bound to `RET' by an overlay/text keymap at point."
  (let ((maps (delq nil
                    (list (get-char-property (point) 'keymap)
                          (get-text-property (point) 'keymap)
                          (and (> (point) (point-min))
                               (get-char-property (1- (point)) 'keymap))
                          (and (> (point) (point-min))
                               (get-text-property (1- (point)) 'keymap))))))
    (seq-some (lambda (map)
                (when (keymapp map)
                  (let ((binding (lookup-key map (kbd "RET"))))
                    (and binding
                         (not (integerp binding))
                         (commandp binding)
                         binding))))
              maps)))

(defun agent-shell-jira-return-dwim ()
  "Activate an actionable item at point, otherwise insert a newline."
  (interactive)
  (cond
   ((button-at (point))
    (push-button (point)))
   ((and (> (point) (point-min))
         (button-at (1- (point))))
    (push-button (1- (point))))
   ((if-let* ((action (agent-shell-jira--ret-action-at-point)))
        (progn
          (call-interactively action)
          t)
      nil))
   (t
    (call-interactively #'newline))))

(defun agent-shell-jira--hidden-p (pos)
  "Return non-nil when POS is currently hidden by another overlay."
  (get-char-property pos 'invisible))

(defun agent-shell-jira--clickable-text-p (start end)
  "Return non-nil when a clickable text property already covers START END."
  (and (< start end)
       (let ((pos start)
             found)
         (while (and (< pos end) (not found))
           (setq found (get-char-property pos 'keymap)
                 pos (1+ pos)))
         found)))

(defun agent-shell-jira--range-contains-p (start end range)
  "Return non-nil when RANGE fully contains START END."
  (and (<= (car range) start)
       (<= end (cdr range))))

(defun agent-shell-jira--markdown-avoid-ranges (context)
  "Return Markdown regions in CONTEXT that Jira links must avoid."
  (append
   (mapcar (lambda (source-block)
             (let ((block (map-elt source-block :block)))
               (cons (map-elt block :start)
                     (map-elt block :end))))
           (map-elt context :source-blocks))
   (map-elt context :inline-code-ranges)))

(defun agent-shell-jira--in-markdown-link-p (start end)
  "Return non-nil when START END is inside a Markdown link span."
  (save-excursion
    (goto-char (point-min))
    (catch 'inside-link
      (while (re-search-forward
              (rx (optional "!") "["
                  (one-or-more (not (any "]\n"))) "]"
                  "("
                  (one-or-more (not (any ")\n"))) ")")
              nil t)
        (when (and (<= (match-beginning 0) start)
                   (<= end (match-end 0)))
          (throw 'inside-link t)))
      nil)))

(defun agent-shell-jira--keymap (issue-key)
  "Return a keymap that opens ISSUE-KEY in Jira."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda ()
                  (interactive)
                  (agent-shell-jira-open-issue issue-key)))
    (define-key map (kbd "RET")
                (lambda ()
                  (interactive)
                  (agent-shell-jira-open-issue issue-key)))
    map))

(defun agent-shell-jira--face-at (pos)
  "Return a face list for a Jira link at POS that preserves styling."
  (delete-dups
   (append (ensure-list (get-char-property pos 'face))
           '(link))))

(defun agent-shell-jira--put-properties (start end issue-key)
  "Attach Jira link affordances for ISSUE-KEY over START END."
  (let ((face (agent-shell-jira--face-at start))
        (keymap (agent-shell-jira--keymap issue-key)))
    (add-text-properties
     start end
     `(face ,face
            font-lock-face ,face
            mouse-face highlight
            help-echo ,(format "Open Jira issue %s" issue-key)
            follow-link t
            keymap ,keymap
            agent-shell-jira-issue ,issue-key))))

(defun agent-shell-jira--skip-region-p (start end &optional avoid-ranges)
  "Return non-nil when Jira affordances should not be added over START END."
  (or (seq-some (lambda (range)
                  (agent-shell-jira--range-contains-p start end range))
                avoid-ranges)
      (agent-shell-jira--in-markdown-link-p start end)
      (agent-shell-jira--hidden-p start)
      (get-char-property start 'agent-shell-markdown-frozen)
      (agent-shell-jira--clickable-text-p start end)))

(defun agent-shell-jira-add-link-properties-in-region
    (start end &optional avoid-ranges)
  "Add Jira click targets across visible plain issue keys in START END.

AVOID-RANGES contains Markdown regions, such as code blocks and inline
code spans, that must not receive Jira affordances."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward agent-shell-jira-issue-regexp nil t)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0))
              (issue-key (match-string-no-properties 0)))
          (unless (agent-shell-jira--skip-region-p
                   match-start match-end avoid-ranges)
            (agent-shell-jira--put-properties match-start match-end issue-key)))))))

(defun agent-shell-jira--open-link-around (original-fn url)
  "Open Jira-like URL via Jira package, else delegate to ORIGINAL-FN."
  (if-let* ((issue-key (agent-shell-jira--issue-key url)))
      (agent-shell-jira-open-issue issue-key)
    (funcall original-fn url)))

(defun agent-shell-jira--render-issue-links (context)
  "Add Jira affordances during the Markdown renderer pass.

The renderer narrows to the streaming region before calling this hook.
CONTEXT supplies ranges for fenced blocks and inline code, which must
remain ordinary text even when they contain Jira-looking strings."
  (agent-shell-jira-add-link-properties-in-region
   (point-min) (point-max)
   (agent-shell-jira--markdown-avoid-ranges context)))

(defun agent-shell-jira--render-custom-markdown-around (original-fn &rest args)
  "Preserve Jira links for custom Markdown renderers.

The normal in-place renderer runs `agent-shell-jira--render-issue-links'
itself.  A custom renderer does not, so decorate its rendered region after
it returns, matching the old integration path."
  (prog1 (apply original-fn args)
    (unless (eq agent-shell-markdown-render-function
               #'agent-shell-markdown-replace-markup)
      (agent-shell-jira-add-link-properties-in-region
       (point-min) (point-max)))))

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

(unless (advice-member-p #'agent-shell-jira--open-link-around
                         'agent-shell-markdown--open-link)
  (advice-add 'agent-shell-markdown--open-link :around
              #'agent-shell-jira--open-link-around))

(add-hook 'agent-shell-markdown-render-functions
          #'agent-shell-jira--render-issue-links)

(unless (advice-member-p #'agent-shell-jira--render-custom-markdown-around
                         'agent-shell--render-markdown)
  (advice-add 'agent-shell--render-markdown :around
              #'agent-shell-jira--render-custom-markdown-around))

(provide 'agent-shell-jira)

;;; timfel-jira-extensions.el ends here
