# agent-shell-utils

Small optional utilities for
[agent-shell](https://github.com/xenodium/agent-shell).

The package is split into separately loadable features:

- `agent-shell-bwrap.el`: run agent commands through `systemd-run` and
  `bwrap`.
- `agent-shell-context.el`: add context sources for recent Emacs buffers,
  built-in VC diffs, and Magit diffs.
- `agent-shell-fanout.el`: start or resume multiple `agent-shell` sessions,
  usually one per Git worktree.
- `agent-shell-ralph.el`: keep a session moving with buffer-local continuation
  rules, retry after rate limits, and force an idle prompt-ready state.
- `agent-shell-jira.el`: Integration with jira.el to launch agents to investigate
  issues.

All customization lives under the `agent-shell-utils` Custom group.

## Installation

With Emacs 30 or `package-vc`:

```elisp
(package-vc-install
 '(agent-shell-utils
   :url "https://github.com/timfel/agent-shell-utils.git"
   :branch "main"))
```

With `use-package` and built-in VC support:

```elisp
(use-package agent-shell-utils
  :vc (:url "https://github.com/timfel/agent-shell-utils.git"
       :rev :newest)
  :defer t)
```

With straight.el:

```elisp
(straight-use-package
 '(agent-shell-utils
   :type git
   :host github
   :repo "timfel/agent-shell-utils"))
```

## Bubblewrap

Enable globally:

```elisp
(use-package agent-shell-bwrap
  :after agent-shell
  :config
  (agent-shell-bwrap-mode 1))
```

Or configure `agent-shell` directly:

```elisp
(require 'agent-shell-bwrap)
(setq agent-shell-command-prefix #'agent-shell-bwrap-command-prefix)
```

Useful options:

- `agent-shell-bwrap-write-dirs`
- `agent-shell-bwrap-read-dirs`
- `agent-shell-bwrap-hidden-dirs`
- `agent-shell-bwrap-extra-write-files`
- `agent-shell-bwrap-use-systemd-run`

## Context Sources

Enable the extra context sources globally:

```elisp
(use-package agent-shell-context
  :after agent-shell
  :config
  (agent-shell-context-mode 1))
```

This installs the functions from `agent-shell-context-extra-sources` into
`agent-shell-context-sources`, before the built-in `line` fallback.

To enable only selected sources:

```elisp
(require 'agent-shell-context)
(setq agent-shell-context-sources
      '(files region error
              agent-shell-context-magit-source
              agent-shell-context-vc-source
              agent-shell-context-emacs-source
              line))
```

Useful options:

- `agent-shell-context-buffer-limit`
- `agent-shell-context-lines-around-point`
- `agent-shell-context-shell-output-lines`
- `agent-shell-context-special-buffer-regexps`
- `agent-shell-context-extra-sources`

Magit support is optional. If Magit is not installed,
`agent-shell-context-magit-source` returns nil.

## Fan-out

Load the feature and call `agent-shell-fanout-worktrees` with task specs:

```elisp
(require 'agent-shell-fanout)

(agent-shell-fanout-worktrees
 '(("fix-parser" . "Investigate and fix the parser failure.")
   ("add-tests" . "Add tests for the new parser behavior."))
 "/path/to/repo")
```

Each spec is a `(TITLE . TASK)` pair. Non-absolute titles create or reuse Git
worktrees below the current repository's `agent-shell` transcript directory.
Absolute titles are treated as existing directories and start sessions there.

Enable the Dired helper key (`C-x a i`):

```elisp
(use-package agent-shell-fanout
  :after (agent-shell dired)
  :config
  (agent-shell-fanout-dired-mode 1))
```

Useful options:

- `agent-shell-fanout-planning-request`
- `agent-shell-fanout-worktree-cleanup-age-days`
- `agent-shell-fanout-adjacent-repository-names`
- `agent-shell-fanout-repositories-function`

## Ralph

`agent-shell-ralph-mode` keeps one `agent-shell` buffer moving while a local
condition matches. Configure and enable it in an `agent-shell` buffer:

```elisp
(require 'agent-shell-ralph)
M-x agent-shell-ralph-setup
```

The setup command asks for:

- a shell command or Elisp form to check
- whether success or failure should trigger continuation
- the prompt to queue when the rule matches

Run the configured rule manually:

```elisp
M-x agent-shell-ralph-run-now
```

Enable retry after rate-limit errors:

```elisp
(use-package agent-shell-ralph
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-ralph-rate-limit-retry-mode))
```

Force an idle `agent-shell` buffer back to prompt-ready state:

```elisp
M-x agent-shell-ralph-unstick
```

Useful options:

- `agent-shell-ralph-check-on-enable`
- `agent-shell-ralph-rate-limit-retry-prompt`
- `agent-shell-ralph-rate-limit-retry-delay-min`
- `agent-shell-ralph-rate-limit-retry-delay-max`
- `agent-shell-ralph-rate-limit-message-regexp`

## Minimal Setup

```elisp
(use-package agent-shell-bwrap
  :after agent-shell
  :config
  (agent-shell-bwrap-mode 1))

(use-package agent-shell-context
  :after agent-shell
  :config
  (agent-shell-context-mode 1))

(use-package agent-shell-fanout
  :after (agent-shell dired)
  :config
  (agent-shell-fanout-dired-mode 1))

(use-package agent-shell-ralph
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-ralph-rate-limit-retry-mode))
```
