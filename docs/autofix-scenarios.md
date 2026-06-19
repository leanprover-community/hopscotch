# Automated fixes: scenario guide

This guide enumerates every scenario hopscotch's automated-fix detection considers, how each is handled, and what a user or CI workflow (such as a downstream-reports updater) should do with the result. For concrete minimal timelines of each case, see [autofix-examples.md](autofix-examples.md).

## The model

A run does not modify the downstream workspace. When a run concludes, detection runs once and records two kinds of results, surfaced in `summary.md` and `results.json`:

| Result | Meaning | When recorded | Applied by |
|---|---|---|---|
| Proposal (`proposedFixes`) | A migration that repairs the failure boundary | Only when the run stopped (exit 1) | `hopscotch fix apply` |
| Advisory (`deprecatedImports`) | An import that works today but resolves through a live `deprecated_module` shim and will break when the shim is deleted upstream | Any conclusion, including fully-green runs | `hopscotch fix apply` (skip with `--no-advisories`) |

Multi-breakage ranges are handled iteratively: each run reports one reproducible boundary plus its fix when one is known. Apply the fix, commit, re-run, and repeat. The CI that applies the fix validates the repair — hopscotch does not pre-validate a proposal by building.

## What upstream actually does (mathlib survey, June 2026)

The detection scenarios mirror the `deprecated_module` lifecycle observed across all 164 shims on mathlib master (plus 42 already deleted):

1. **Two-step deprecation** (69% of shims): a PR deletes `Mathlib/Foo/Bar.lean` (split or move), and a later commit re-adds it as a shim re-exporting the new location(s). Median window between the two: 42 mathlib commits (p75: 253; max observed: 2065, roughly two months). Every commit inside the window breaks any downstream `import Mathlib.Foo.Bar`.
2. **In-place conversion** (31%): the file becomes a shim in a single commit. Plain builds do not break; only deprecation warnings fire.
3. **Shim cleanup**: roughly 6 months after deprecation, shims are bulk-deleted ("chore: delete >6 month old deprecated modules"). A downstream that ignored the warnings breaks at the cleanup commit, and the shim now exists only in the past.
4. **Empty shims**: some shims re-export nothing (`deprecated_module "Upstreamed to core"`). The correct migration is to delete the import.
5. **Code-carrying shims** (~3%): the shim still defines declarations (usually `@[deprecated]` compatibility aliases). An import rewrite drops those, so the migration may be partial.

## Detection inputs

Detection (`ModuleDeprecation.detect`) runs against the boundary commit (the failing commit when stopped; the newest range commit on a green conclusion) and uses the following, all offline unless noted:

- The downstream's import set (comment-aware scan of every `.lean` file, prefiltered by the dependency's top-level module roots; non-dependency imports have no cost).
- The dependency tree at the boundary and at the newest range commit (one `git ls-tree` each), plus the shim paths at the newest commit (one `git grep`).
- The build log: the failing log at a stopped boundary, or the last successful build log on a green conclusion. The green build already ran `linter.deprecated.module`, so its warnings are available in the captured log. Logs are parsed against the exact message Lean core emits (`Lean.formatDeprecatedModuleWarning`), including the verbatim replacement import lines — but logs are a hint only. Shim file content is the source of truth wherever it is reachable.

## Scenario catalog

For each dependency module `M` the downstream imports (path `P`):

### M is missing at the boundary commit (the import is broken → proposal track)

| # | Situation | Resolution | Result |
|---|---|---|---|
| 1 | The shim exists at the newest range commit (the deprecation window closed inside the range) | Read the shim blob at the newest commit | Proposal: `import M → shim's imports` |
| 2 | Missing at the newest commit too, and the blob just before the deletion commit is a shim (the ~6-month shim cleanup: the shim only exists in the past) | `git show <deletion>^:P` | Proposal from the pre-deletion shim |
| 3 | Pre-deletion blob is a real module, but git detects the deletion as a rename (mechanical move, no shim ever) | Rename target at the deletion commit | Proposal: `import M → renamed module` |
| 4 | None of the above, but the shim has landed on the dependency's default branch beyond the range | `raw.githubusercontent.com` at `master`/`main` (the only network step; requires a recognizable GitHub URL in the lakefile) | Proposal from the upstream shim |
| 5 | The resolved shim re-exports nothing (empty shim, "Upstreamed to core") | Any of the shim sources (1, 2, 4) | Proposal with `newModules = []`: remove the import |
| 6 | No source yields a shim or rename (genuine removal) | — | No proposal. A note recorded in the summary and in `results.json` (`detectionNotes`) explains the module was deleted with no replacement. The boundary stands as a real culprit. |

Every blob source must actually contain a top-level `deprecated_module` command (`isDeprecatedModuleShim`, comment-aware) before its imports are trusted. A real module's imports are never mistaken for replacements.

### M is present at the boundary commit (the import resolves)

| # | Situation | Result |
|---|---|---|
| 7 | M is a shim at the newest range commit, and the build log does not warn about it | Advisory: builds today, breaks at the upstream cleanup. Migration is optional. |
| 8 | Same, but the failing boundary log carries the toolchain's deprecation warning for M (warnings-as-error: the warning itself failed the build) | Promoted to a proposal — the warning is the breakage |
| 9 | M is warned in the log but the shim is already gone at the newest commit (cleaned up later in the range) | Shim read at the boundary commit itself → proposal |
| 10 | M resolves to a real (non-shim) module | Nothing — the import is healthy |

### Log-only scenarios (any conclusion)

| # | Situation | Result |
|---|---|---|
| 11 | The log warns about a deprecated module outside the inspected dependency (e.g. a Batteries shim) — invisible to the tree scan | Advisory with the replacement imports the toolchain itself printed |
| 12 | Green conclusion: a warned, shim-routed import was considered for promotion by detection, but nothing actually failed | Folded into the advisories (a green run records no proposals) |

### Migration quality flags

| Flag | Meaning | Effect |
|---|---|---|
| `newModules = []` | The shim re-exports nothing | `fix apply` deletes the import line |
| `shimHasDeclarations = true` | The source shim also defines declarations (compat aliases) that an import rewrite drops | Rendered with a `[partial]` marker; `fix apply` skips these advisories with a message (rewriting could regress a working build); proposals still apply (the build was already broken) |

## What a run reports

| Conclusion | `proposedFixes` | `deprecatedImports` | Exit code |
|---|---|---|---|
| Stopped at a repairable boundary | the migrations | shim-routed working imports | 1 |
| Stopped at a genuine removal or unrelated culprit | empty | shim-routed working imports | 1 |
| Fully successful | always empty | advisories (including any considered for promotion on green runs) | 0 |

Exit codes are unchanged by detection. Automation should branch on the JSON fields, not the exit code. A resumed session clears both fields before retrying; they are recomputed at the next conclusion.

## Consumer playbook (CI updater, e.g. downstream-reports)

1. Exit 1, `proposedFixes` non-empty: open a "fix breaking changes" PR. Run `hopscotch fix apply --no-advisories --from results.json` for a surgical, break-only PR (drop the flag to also fold in deprecation hygiene), commit, and bump the rev. The PR's CI validates the repair. The next scheduled run searches past the repaired breakage.
2. Exit 1, `proposedFixes` empty: a genuine breaking change. Open an issue with `firstFailingCommit`, the culprit log, and any `detectionNotes` (e.g. "deleted with no replacement shim").
3. Exit 0, `deprecatedImports` non-empty: optional hygiene PR. Run `hopscotch fix apply` to migrate the clean entries. Partial ones are skipped with a message and listed for human review. This keeps the downstream off shims before the upstream cleanup occurs.
4. `hopscotch fix list` inspects both lists. `hopscotch fix revert` restores every original from the backup store (`.lake/hopscotch/autofix-backups/`, which mirrors project-relative paths and is itself the record of what `apply` touched).
5. Bisect sessions are terminal once stopped. Run `hopscotch clean` (or start a fresh CI checkout) before re-running after a fix. Linear sessions resume in place.

## Known limitations

- **Refactor-renames**: when upstream redefined the content (e.g. mathlib's semicontinuity refactor), the old declaration names may not exist anywhere. The import rewrite is necessary but not sufficient; the fix PR's CI fails on the remaining references and a human must adapt the code.
- **Warnings-as-error regimes** (a downstream whose build runs lake with `--wfail`, or the stricter `--iofail`; both are sugar for `--fail-level`): deprecation warnings fail builds with exit 1 while the warning block stays verbatim in the captured log, including on cached rebuilds where lake replays the logged warning and still fails (verified against lake v4.31.0-rc1; the message format matches `deprecationWarnings` in all three shapes: plain, custom-message, and empty shim with zero replacement lines). Shim-routed imports surface as proposals at the commit that introduced the deprecation (scenarios 8–9) rather than as advisories. Failure profiles become monotone, so bisects converge on the deprecating commit deterministically. Deprecated declaration warnings also fail builds, so an import rewrite alone may not turn the re-run green when the code still references `@[deprecated]` alias names. Migrations flagged `[partial]` are guaranteed insufficient there, and even clean ones may need the declaration renames in the same PR.
- **Non-monotone failure ranges** (deprecation window and an unrelated culprit in one range): the bisect converges on whichever pass-to-fail boundary it finds first. The iterative protocol reaches the other one on the next run.
- **Network degradation**: scenario 4 is the only network-dependent path. On a locked-down runner it degrades to scenario 6 (a plain culprit report, no proposal).
- **Log-format dependence**: scenarios 8, 9, 11, and 12 depend on Lean core's warning text. A format change degrades them silently to the tree-scan results, never to wrong output, since logs only add candidates whose content is re-verified where reachable.
