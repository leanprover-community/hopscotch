# Automated fixes: end-to-end examples

Minimal walkthroughs of a `hopscotch dep` run for each way the failure boundary can land relative to a module deprecation. The systematic catalog is in [autofix-scenarios.md](autofix-scenarios.md); here each case is shown as a concrete timeline.

**Shared setup:** The downstream project imports one module from the dependency `demo`:

```lean
-- Main.lean
import Demo.Old
```

The dependency timeline is written oldest to newest. `c0` is always the pinned, known-good baseline (`--from`), and the run is:

```bash
hopscotch dep demo --to <newest>
```

`Demo.Old → Demo.New` below denotes a `deprecated_module` shim file whose content is:

```lean
module

public import Demo.New

deprecated_module (since := "…")
```

---

## 1. Culprit is the deletion; the shim lands later in the range

The classic two-step deprecation, fully contained in the tested range.

```
c0 ── c1 ──────────────── c2 ─────────────── c3
good  deletes Demo/Old    re-adds Demo.Old   (newest)
      adds Demo/New       as shim → Demo.New
```

`Main.lean` fails to build at `c1` (unknown module). The bisect converges on `c1`; every commit from `c1` onward fails until `c2`, and the search does not need to proceed past it.

**Report:** exit `1`, `firstFailingCommit = c1`, and because the shim is readable at the newest range commit (from the local checkout):

```json
"proposedFixes": [
  { "fixId": "module-deprecation", "oldModule": "Demo.Old",
    "newModules": ["Demo.New"], "shimHasDeclarations": false }
]
```

**You do:** Run `hopscotch fix apply`, commit the rewritten import, and re-run. The resumed or next run passes `c1` and continues.

---

## 2. Culprit is the deletion; the shim exists only beyond the range

Same deletion, but the range ends before the shim was ever added.

```
c0 ── c1 ─────────────── c2          …future: shim added on master
good  deletes Demo/Old   (newest)
```

Boundary is again `c1`. The shim is not at `c2` and not in the range's history, so resolution falls back to fetching `Demo/Old.lean` from the dependency's default branch on `raw.githubusercontent.com`:

- **Shim already on master:** same proposal as example 1. This is the only network-dependent case.
- **No shim anywhere** (or no network): no proposal. The report is a plain culprit, which is correct. At this moment, the deletion is an unshimmed breaking change. A note in the summary and in `results.json` (`detectionNotes`) records that no replacement was found. Re-running after upstream adds the shim upgrades this to example 1.

---

## 3. Culprit is the shim cleanup (months after the deprecation)

The downstream ignored the deprecation warnings; upstream eventually deletes the shim.

```
c0 ─────────────────────── c1
good                       "chore: delete >6 month old
(Demo.Old is already        deprecated modules"
 a shim → Demo.New)         deletes Demo/Old
```

Boundary is `c1`, and the shim now exists only in the past. Resolution reads the blob just before the deletion (`git show c1^:Demo/Old.lean`) — still a shim, still naming `Demo.New` — fully offline.

**Report:** exit `1`, `firstFailingCommit = c1`, proposal `Demo.Old → Demo.New`. Same consumer flow as example 1.

A variant: if the shim re-exported nothing (`deprecated_module "Upstreamed to core"`), the proposal has `newModules = []`, and `fix apply` removes the import line.

---

## 4. Culprit is unrelated; the deprecation never breaks anything

The deprecation here is an in-place conversion (no deletion window), and something else breaks the build.

```
c0 ── c1 ─────────────────── c2
good  converts Demo/Old      deletes Demo/Other     ← downstream also
      into a shim → Demo.New (genuine API removal)    imports Demo.Other
```

`import Demo.Old` keeps resolving at every commit (via the shim, with a warning). The bisect converges on `c2` for the `Demo.Other` removal, for which no shim exists.

**Report:** exit `1`, `firstFailingCommit = c2`. No proposal for the culprit (a human must adapt the code), but the deprecation is still surfaced:

```json
"proposedFixes": [],
"deprecatedImports": [
  { "fixId": "module-deprecation", "oldModule": "Demo.Old",
    "newModules": ["Demo.New"], "shimHasDeclarations": false }
]
```

**You do:** fix `Demo.Other` usage by hand. Migrate `Demo.Old` whenever convenient; it builds today, but see example 6 for its future.

---

## 5. No culprit at all: the green run as a deprecation audit

```
c0 ── c1 ─────────────────── c2
good  converts Demo/Old      unrelated change
      into a shim → Demo.New (newest)
```

Every commit builds. The run exits `0`, `status = fullySuccessful`, and the conclusion still inspects the imports and the last successful build log, where the toolchain printed its deprecation warning:

```json
"proposedFixes": [],
"deprecatedImports": [
  { "fixId": "module-deprecation", "oldModule": "Demo.Old",
    "newModules": ["Demo.New"], "shimHasDeclarations": false }
]
```

**You do (optionally):** Run `hopscotch fix apply --advisories` to migrate now, for example folded into the same green bump PR, so example 3 never happens to you. Advisories whose shim still defines compatibility aliases are skipped with a message (`shimHasDeclarations = true`); rewriting those could regress a working build.

---

## 6. Warnings-as-error: the deprecation itself is the culprit

Same timeline as example 5, but the downstream's build fails on warnings (lake's `--wfail`/`--iofail`).

```
c0 ── c1 ─────────────────── c2
good  converts Demo/Old      (newest)
      into a shim → Demo.New
      ⚠ warning now fails the build
```

The deprecation warning makes `c1` fail, so the bisect converges on `c1`. Detection sees the module present at the boundary but warned in the failing log, so the would-be advisory is promoted to a proposal:

**Report:** exit `1`, `firstFailingCommit = c1`, proposal `Demo.Old → Demo.New`.

**You do:** Run `hopscotch fix apply`, commit, and re-run. In this regime, deprecated declaration warnings also fail builds. If your code uses `@[deprecated]` alias names, expect the follow-up build to request those renames as well.

---

## 7. Two breakages in one range: the iterative protocol

Examples 1 and 4 combined — a deprecation window and a genuine break.

```
c0 ── c1 ──────────── c2 ────────── c3 ─────────── c4
good  deletes Demo/Old shim → New   unrelated      deletes Demo/Other
```

- **Run 1** stops at the first breaking change: `firstFailingCommit = c1`, proposal `Demo.Old → Demo.New`. The workspace is untouched, so this boundary reproduces on a clean checkout.
- **You:** Run `hopscotch fix apply` and commit.
- **Run 2** proceeds past the repaired window and stops at `c4`: a genuine removal, no proposal. Human work is needed.

Each run reports one reproducible boundary (plus its fix, when known). Apply, commit, re-run, and repeat. Bisect sessions are terminal once stopped, so run `hopscotch clean` first before starting run 2 (a fresh CI checkout does this automatically). Linear sessions resume in place.
