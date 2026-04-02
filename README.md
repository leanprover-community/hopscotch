# `hopscotch`
`hopscotch` is an interactive validation tool meant to test downstream Lean 4 projects against a range of dependency commits. It is designed to be run by hand, one session at a time: it hops through a sequence of versions and stops when landing on a failing one — just like tossing a stone and having to pick it up before you can continue. You fix the issue, then resume hopping onward. Each project directory holds at most one active session; to test a different range you clear the state and start a new one.

The primary use case is checking whether a downstream project (one that depends on mathlib) still builds across a range of mathlib commits, and finding exactly which commit introduced a regression.

## Exit codes

| Code | Meaning |
|------|---------|
| `0` | Session completed with no failures (all commits/toolchains passed). |
| `1` | A failure boundary was found (the tool ran correctly; the downstream failed). |
| `2` | An unexpected error in the tool itself (bad arguments, network error, corrupt state, etc.). |

## Getting a binary

Pre-built binaries for Linux (x86_64), macOS (x86_64 and arm64), and Windows (x86_64) are attached to each [GitHub release](https://github.com/leanprover-community/hopscotch/releases/latest). Download the one for your platform and place it somewhere on your `PATH`.

## Building from source

To build from source, run the following from the repo root:

```bash
lake build hopscotch
```

The binary will be located at `.lake/build/bin/hopscotch`. You can add it to your `PATH` or invoke it directly.

## Modes

`hopscotch` has two subcommands:

- **`dep`** — test a range of dependency commits against the downstream project.
- **`toolchain`** — test a list of Lean toolchain strings against the downstream project.

Within each subcommand, two execution strategies are available, selected with `--scan-mode [linear|bisect]`:

### Bisect (default)

Binary-searches a commit list to find the exact first failing commit. Useful when the commit range is large and you just want to identify the breaking commit quickly.

Bisect validates that the last commit in the range actually fails before starting, but does **not** verify the first commit (it is assumed to be good — typically the currently-pinned rev). The search narrows until `knownGoodIndex` and `knownBadIndex` are adjacent — the commit at `knownBadIndex` is reported as the first failure. If the good endpoint was never actually probed during the search, the summary notes this.

### Linear (`--scan-mode linear`)

Steps linearly through the commit list from oldest to newest. Stops at the first failure. Best for:

- Checking whether a dependency update is safe at all (namely, the list can have a single item)
- Walking forward through known-good commits until the project breaks
- Resuming and fixing issues step by step

## The `dep` subcommand in detail

```
hopscotch dep <dependency-name> [OPTIONS]
```

For each commit in the list, `hopscotch` runs two phases:

- **Bump**:
  - rewrites the `rev` field (or `@ "rev"` annotation) for `<dependency-name>` in the downstream's lakefile to the target commit. Both `lakefile.toml` and `lakefile.lean` are supported; when both exist, `lakefile.lean` takes precedence (matching Lake's own resolution order).
  - then runs `lake update <dependency-name>` to fetch it and update `lake-manifest.json`.
- **Build**: runs `lake build` to verify the downstream compiles against the bumped dependency.

Both phases are recorded separately in the persisted state and in the log files, so a failure at the bump phase (e.g. a network error or a broken `lake update`) is distinguishable from a failure at the build phase (a compile error in the downstream).

If any step fails, `hopscotch` stops, records the failure, and exits with code `1` (see exit codes above).

### Options

| Flag | Description |
|------|-------------|
| `--scan-mode [linear\|bisect]` | Select the scan strategy. `bisect` (default) binary-searches the range; `linear` steps through from oldest to newest. |
| `--from REF` | First commit to test (exclusive — this commit itself is not tested). Defaults to the SHA in `lake-manifest.json`, falling back to the `rev` field in the lakefile. |
| `--to REF` | Last commit to test (inclusive). Defaults to the tip of the default branch (`main` or `master`). |
| `--commits-file PATH` | Load commit list from a file (one SHA per line) instead of fetching from GitHub. Mutually exclusive with `--to`. |
| `--git-url URL` | Override the git URL for the dependency (default: read from the lakefile's `require` block, or looked up via [Reservoir](https://reservoir.lean-lang.org) for Reservoir-style dependencies). |
| `--project-dir DIR` | Path to the downstream project (default: current directory). |
| `--quiet` | Suppress lake command output. |
| `--allow-dirty-workspace` | Skip git-cleanliness checks (both the linear-mode resume check and the bisect session-start check). |
| `--config-file PATH` | Load options from a JSON config file. CLI flags take precedence. |

`--from` or `--git-url` can be passed without `--to`; in that case `--to` defaults to the tip of the default branch.

### Config file

Options can be stored in a JSON file and passed with `--config-file`:

```json
{
  "fromRef": "abc123def456",
  "gitUrl": "https://github.com/leanprover-community/mathlib4",
  "projectDir": "/path/to/my-project",
  "allowDirtyWorkspace": false
}
```

CLI flags override config file values.

### Git check and `--allow-dirty-workspace`

When you resume after a failure, `hopscotch` does something important: once the previously-failing commit now passes, **before advancing to the next commit** it checks whether the project's working tree is clean (`git status --porcelain`).

The rationale is straightforward. When you fix a build error after a failure, that fix lives in your project's source files. As soon as `hopscotch` bumps to the next dependency commit it overwrites the lakefile and runs `lake update`, but it does not touch your source files — so the fix survives. However, if you forget to commit the fix, it remains as an uncommitted change sitting in your working tree alongside a dependency that has now moved on. The git check is added as a guardrail to ensures your fix is properly recorded before the session advances. This keeps the git history clear, with focused fixes associated with particular regressions in the dependency.

If the tree is dirty at that point, `hopscotch` blocks advancement and exits with a `gitCheck` failure:

```
Resume blocked: commit the fix for c3d4e5f6 or rerun with --allow-dirty-workspace
```

The correct response is to commit your changes and then rerun the same command. `hopscotch` will see the clean tree, advance past the fixed commit, and continue.

If git is not available in the project directory (e.g. it is not a git repo), the check is skipped with a warning rather than blocking the run.

**`--allow-dirty-workspace`** bypasses the linear-mode resume check entirely. Use this when you have deliberately left changes uncommitted — for example, you are iterating on a fix and want to test it against several consecutive commits before committing.

Bisect mode has its own git check, run once at session start (including on resume). The concern there is slightly different: bisect restores `lean-toolchain` before each probe but does not restore source files, so uncommitted changes persist invisibly across every probe in the search and can silently skew every result. If the tree is dirty at the start of a bisect run, `hopscotch` refuses to proceed:

```
bisect requires a clean working tree; commit or stash your changes first, or pass --allow-dirty-workspace
```

Pass `--allow-dirty-workspace` to override this too, if you know what you are doing.

### Persistent state and sessions

`hopscotch` is designed for local, interactive use: you run it, it stops on a failure, you fix things, and you run it again. To support this, all run state is written to `.lake/hopscotch/` inside the project directory after every significant step:

- `state.json` — restartable state (committed index, current stage, bisect window, etc.)
- `summary.md` — human-readable outcome
- `logs/` — per-step build logs (e.g. `3-abc12345-build.log`)

If `hopscotch` is interrupted at any point, rerunning the same command resumes from exactly where it left off without repeating completed steps.

Each project directory holds **one active session at a time**. Once a session has `completed` or `failed`, `hopscotch` will not start a new run — it will just report the existing result. To test a different commit range, or to re-run from scratch, you must first clear the session state:

```bash
rm -rf MyProject/.lake/hopscotch
```

## Step-by-step example: updating a mathlib downstream

Suppose you maintain `MyProject`, a Lean 4 project that depends on mathlib. mathlib has released a batch of new commits and you want to find the exact commit where your project breaks. Because mathlib moves fast — dozens of commits a day — you almost always want bisect: it identifies the first bad commit with roughly log₂(N) probes instead of N.

### 1. Check your downstream's current pin

Your lakefile can be either `lakefile.toml` or `lakefile.lean`; `hopscotch` handles both.

```
$ cat MyProject/lakefile.toml | grep -A3 mathlib
[[require]]
name = "mathlib"
git = "https://github.com/leanprover-community/mathlib4"
rev = "a1b2c3d4"   # currently pinned here
```

### 2. Run bisect to find the breaking commit

Bisect is the default — no `--scan-mode` flag needed:

```bash
hopscotch dep mathlib \
  --project-dir ./MyProject \
  --from a1b2c3d4 \
  --to origin/master
```

`hopscotch` fetches the commit range from GitHub (set `GITHUB_TOKEN` to avoid rate limits), validates that the last commit in the range actually fails, then binary-searches. Note that `--from` is exclusive — `a1b2c3d4` itself is not tested.

```
[2026-03-31T12:00:00Z] Validating last commit e5f6a7b8 (47/47) — must fail to proceed
[2026-03-31T12:00:01Z] Running lake update mathlib
[2026-03-31T12:01:30Z] Finished lake update mathlib (log file: .lake/hopscotch/logs/46-e5f6a7b8cd90-bump.log)
[2026-03-31T12:01:30Z] Running lake build
[2026-03-31T12:05:00Z] Finished lake build (log file: .lake/hopscotch/logs/46-e5f6a7b8cd90-build.log)   ← fails ✓
[2026-03-31T12:05:00Z] Probing commit 6c7d8e9f (24/47)
[2026-03-31T12:05:01Z] Running lake update mathlib
…
[2026-03-31T12:10:00Z] Finished lake build (log file: .lake/hopscotch/logs/23-6c7d8e9fab12-build.log)   ← passes
[2026-03-31T12:10:00Z] Probing commit d1e2f3a4 (36/47)
…
[2026-03-31T12:15:00Z] Finished lake build (log file: …)   ← fails
[2026-03-31T12:15:00Z] Probing commit 7b8c9d0e (30/47)
…
[2026-03-31T12:20:00Z] Finished lake build (log file: …)   ← passes
[2026-03-31T12:20:00Z] Probing commit 4f5a6b7c (33/47)
…
[2026-03-31T12:25:00Z] Finished lake build (log file: …)   ← fails
[2026-03-31T12:25:00Z] Probing commit 9e0f1a2b (32/47)
…
[2026-03-31T12:30:00Z] Finished lake build (log file: …)   ← passes
[2026-03-31T12:30:00Z] knownGood (32) and knownBad (33) are adjacent — boundary found.
```

Six probes instead of 47.

### 3. Examine the failure boundary

The summary records the exact boundary:

```bash
cat MyProject/.lake/hopscotch/summary.md
# Bisect result
Last known good: 9e0f1a2b (32/47)
First known bad: 4f5a6b7c (33/47)
```

Inspect the build log for the bad commit:

```bash
cat MyProject/.lake/hopscotch/logs/32-4f5a6b7cab12-build.log
# MyProject/Foo.lean:12:5: error: unknown identifier 'Mathlib.SomeRenamedLemma'
```

### 4. Fix the issue in your project

Edit `MyProject/Foo.lean` to use the updated API. With the lakefile still pinned to `4f5a6b7c` from the last probe, verify the fix locally:

```bash
cd MyProject && lake build
```

### 5. Commit and start a fresh session

Bisect's goal is identification, not incremental repair — once it finds the boundary the session is complete. Commit your fix, clear the state, and run again to check whether the rest of the range is clean:

```bash
cd MyProject && git add -p && git commit -m "fix: update to renamed Mathlib.SomeRenamedLemma"
rm -rf MyProject/.lake/hopscotch

hopscotch dep mathlib \
  --project-dir ./MyProject \
  --from a1b2c3d4 \
  --to origin/master
```

If there is only one regression in the range this second run will complete with all commits passing. If there are multiple regressions, bisect will find the next boundary and you repeat.

### Aside: Linear mode for incremental iteration

`--scan-mode linear` steps through commits oldest-to-newest and stops at the first failure. It is useful when:

- you are testing a single commit (`--from <last-good> --to <candidate>`)
- you prefer to fix each regression before moving to the next, letting you accumulate fixes across a long walk

```bash
hopscotch dep mathlib \
  --project-dir ./MyProject \
  --from a1b2c3d4 \
  --to origin/master \
  --scan-mode linear
```

Because linear mode serializes state after each step, rerunning the same command after a failure resumes from exactly where it stopped. Once the previously-failing commit passes, `hopscotch` checks that the working tree is clean before advancing — ensuring every fix is committed before the session moves on. Pass `--allow-dirty-workspace` to skip this check when you are iterating on a fix across several commits before committing.

### Using a commits file

If you already have a specific list of commits to test (e.g. from a CI artifact or a Reservoir nightly list), skip the GitHub range fetch:

```bash
echo -e "abc123\ndef456\nghi789" > commits.txt
hopscotch dep mathlib \
  --project-dir ./MyProject \
  --commits-file commits.txt
```

## The `toolchain` subcommand in detail

```
hopscotch toolchain --toolchains-file PATH [OPTIONS]
```

For each toolchain in the file, `hopscotch` runs two phases:

- **Bump**: writes the toolchain string to the downstream's `lean-toolchain` file.
- **Build**: runs `lake build` (via `elan run <toolchain> lake build`) to verify the downstream compiles with that toolchain.

Unlike `dep`, the toolchain subcommand does not touch the lakefile or run `lake update`. It only rewrites `lean-toolchain` before each probe.

### Options

| Flag | Description |
|------|-------------|
| `--toolchains-file PATH` | **(Required)** Path to a file listing toolchain strings, one per line. |
| `--scan-mode [linear\|bisect]` | Select the scan strategy. `bisect` (default) binary-searches the range; `linear` steps through from oldest to newest. |
| `--project-dir DIR` | Path to the downstream project (default: current directory). |
| `--quiet` | Suppress lake command output. |
| `--allow-dirty-workspace` | Skip git-cleanliness checks (same semantics as for `dep`). |
| `--config-file PATH` | Load options from a JSON config file. CLI flags take precedence. |

### Config file

```json
{
  "toolchainsFile": "/path/to/toolchains.txt",
  "projectDir": "/path/to/my-project"
}
```

For a step-by-step walkthrough, see [docs/toolchain-example.md](toolchain-example.md).

## Notes

- `hopscotch` uses `elan run <toolchain> lake` so it always respects the downstream's pinned `lean-toolchain`, not its own.
- Set `GITHUB_TOKEN` in your environment to increase GitHub API rate limits when fetching large commit ranges.
- To start a new session after a completed or failed run, delete `.lake/hopscotch/` entirely.
