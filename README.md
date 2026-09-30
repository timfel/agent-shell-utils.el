# agent-shell-utils

Small optional utilities for
[agent-shell](https://github.com/xenodium/agent-shell).

The package is split into separately loadable features:

- `agent-shell-nono.el`: launch agents with a selected nono JSON profile
  and optional `systemd-run` resource limits.
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

## Nono sandbox profiles

Install nono using mise. In your global `mise.toml`:

```toml
[tools]
nono = { version = "0.78.0", os = ["linux", "macos"] }
```

Then run `mise install nono` and ensure `nono` is on Emacs's `exec-path`.
The version is pinned to the release used for the integration tests.
On Linux, optionally install your distribution's `bubblewrap` package for
a private `/tmp` mount. Like `systemd-run`, `bwrap` is used when available.

Enable globally:

```elisp
(use-package agent-shell-nono
  :after agent-shell
  :config
  (agent-shell-nono-mode 1))
```

Or configure `agent-shell` directly:

```elisp
(require 'agent-shell-nono)
(setq agent-shell-command-prefix #'agent-shell-nono-command-prefix)
```

Use **`M-x agent-shell-nono-select-profile`** to select the default for new
agent buffers. Existing buffers retain their selected profile path. Nono
reads that file at each launch, so editing a profile changes subsequent
launches using it, not processes already running.

Create your profiles in `~/.emacs.d/nono/`, or set
`agent-shell-nono-profile-directory` to another directory. The initial
selection is `developer.json`; set `agent-shell-nono-profile` to use a
different filename. See [nono's profile documentation](https://nono.sh/docs/cli/features/profile-authoring)
for policy configuration and inheritance.

```sh
nono profile validate ~/.emacs.d/nono/developer.json
nono profile show ~/.emacs.d/nono/developer.json
```

Missing nono or a missing profile is an error, never an automatic
unsandboxed fallback. `--allow-cwd` acknowledges the workspace access
specified in the profile without prompting on ACP's standard input.

The agent buffer's `default-directory` is passed as `--workdir`, so profile
paths such as `$WORKDIR/../graal` refer to siblings of that directory, not
of the JSON file. Git worktrees may also need an explicit grant for their
shared Git metadata outside the workspace.

### Private temporary files on Linux

When `bwrap` is available, every sandboxed local Linux launch gets a fresh
tmpfs mounted at `/tmp`, with mode `1777`. The launcher sets `TMPDIR`, `TMP`
and `TEMP` to `/tmp`. Host `/tmp` contents are hidden, and temporary data
disappears when the session's processes exit. Other platforms are unchanged;
remote launches are not wrapped.

Without `bwrap`, the launcher reports that temporary directories remain
host-backed and runs nono without the mount wrapper. Nono still enforces
the selected profile; the temporary-directory environment is left unchanged.

The launch order is:

```text
systemd-run (if available) → bwrap (if available, Linux) → nono → agent
```

The mount is created **before** nono applies filesystem permissions.
Your profile must allow the temporary-file operations you need. For a
profile that excludes nono's default temporary-directory grants, add this
Linux-only entry to `filesystem.allow`:

```json
{ "path": "/tmp", "when": "linux" }
```

With the wrapper, this grants access to the private mount. **Without
bubblewrap, the same grant permits access to the host's `/tmp`.** It does
not require enabling broader host-temp grants. `/var/tmp` and the home
directory are not replaced. Tmpfs memory is charged to the systemd scope
when systemd resource limits are available.

When using bubblewrap, keep the nono executable, selected profile and
workspace outside `/tmp`, including symlink targets: the launcher rejects
paths that the mount would hide. If an installed wrapper fails at runtime,
the launch fails rather than retrying without it.

### Launcher options

- `agent-shell-nono-profile-directory`
- `agent-shell-nono-profile`
- `agent-shell-nono-enabled` (explicitly setting nil disables sandboxing)
- `agent-shell-nono-cpu-limit`
- `agent-shell-nono-memory-limit-gb`
- `agent-shell-nono-memory-fraction`

Resource limits are applied through `systemd-run --user --scope` when it
is available. No resource-limit guarantee is made on systems without it.
Live sessions are not reconfigured by loading this module.

### Tests

With agent-shell installed in Emacs's normal package directory:

```sh
emacs -Q --batch \
  --eval '(progn (require (quote package)) (package-initialize))' \
  -L . -l tests/agent-shell-nono-tests.el -f ert-run-tests-batch-and-exit
```

Set `AGENT_SHELL_NONO_TEST_BINARY` to an absolute nono executable path to
also test profile validation and ACP stdin/stdout with temporary test
configuration and an isolated HOME. Linux integration tests also verify
that `/tmp` is writable tmpfs, hides host files, and starts fresh on each
launch. The test executable and fixtures must be outside `/tmp`. Tests do
not use your personal profiles or make external network requests.

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

## Agent Shell Dashboard

A simple dashboard tailored to tracking agent-shells across worktrees and
common directories and associating them with Jira issues and Bitbucket PRs.

## Minimal Setup

```elisp
(use-package agent-shell-nono
  :after agent-shell
  :config
  (agent-shell-nono-mode 1))

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
