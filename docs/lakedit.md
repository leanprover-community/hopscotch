# `lakedit`

`lakedit` is a small companion CLI, built alongside `hopscotch` in the same repo, that rewrites a lakefile `require` block — either pinning it to a specific git revision or pointing it at a local path. The typical use case is testing a local checkout of a dependency, or manually advancing a pin, before running `hopscotch` to find a regression across a commit range.

## Building

```bash
lake build lakedit
# binary at .lake/build/bin/lakedit
```

## Synopsis

```
lakedit set <dep-name> (--path PATH | --rev SHA) [--project-dir DIR] [--quiet]
```

`lakedit` does not run `lake update` or `lake build`. It is a lakefile editor; you run those yourself after.

## Subcommands

Only `set` is implemented. `--help` and `--version` are also recognized.

### `set`

```
lakedit set <dep-name> (--path PATH | --rev SHA) [--project-dir DIR] [--quiet]
```

Rewrites the `require` block for `<dep-name>` and exits. Exactly one of `--path` or `--rev` is required. Both `lakefile.toml` and `lakefile.lean` are supported; when both exist, `lakefile.lean` takes precedence (matching Lake's own resolution order).

**Options**

| Flag | Description |
|------|-------------|
| `--path PATH` | Rewrite the dependency to use a local filesystem path. |
| `--rev SHA` | Rewrite the pinned revision (git SHA, branch, or tag). |
| `--project-dir DIR` | Directory containing the lakefile (default: `.`). |
| `--quiet` | Suppress the confirmation message printed to stdout. |

## Behavior

### `--rev`: pinning a revision

`--rev` rewrites the revision field in-place, leaving the git URL and all other fields untouched.

**`lakefile.toml`** — rewrites (or inserts) the `rev = "..."` line:

```toml
# before
[[require]]
name = "mathlib"
git = "https://github.com/leanprover-community/mathlib4"
rev = "old_sha"

# after: lakedit set mathlib --rev new_sha
[[require]]
name = "mathlib"
git = "https://github.com/leanprover-community/mathlib4"
rev = "new_sha"
```

**`lakefile.lean`** — rewrites the `@ "rev"` annotation:

```lean
-- before
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "old_sha"

-- after: lakedit set mathlib --rev new_sha
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "new_sha"
```

If the dependency has no existing revision annotation, one is inserted.

### `--path`: switching to a local source

`--path` drops the `git`, `rev`, `path`, and `source` fields and replaces them with a `path` source.

**`lakefile.toml`**:

```toml
# before
[[require]]
name = "mathlib"
scope = "leanprover-community"
git = "https://github.com/leanprover-community/mathlib4"
rev = "abc123"

# after: lakedit set mathlib --path /local/mathlib4
[[require]]
name = "mathlib"
path = "/local/mathlib4"
scope = "leanprover-community"
```

- `git`, `rev`, `path`, and `source` fields are removed; `name`, `scope`, comments, and all other fields are preserved.
- `path` is inserted immediately after the `name` line.
- CRLF line endings are preserved.

**`lakefile.lean`**:

```lean
-- before
require batteries from git "https://github.com/leanprover-community/batteries.git" @ "abc123"

-- after: lakedit set batteries --path /local/batteries
require batteries from "/local/batteries"
```

- The entire `require` block (header line + any continuation lines) is collapsed to a single-line declaration.
- Scoped dependencies (`require "scope" / "name" from git ...`) are rewritten to the matching scoped form (`require "scope" / "name" from "path"`).
- The `with` clause is not supported in `--path` mode; `lakedit` exits with an error if one is present. Edit the file manually in that case.

### Path normalization

`lakedit` passes the `--path` value through unchanged. Lake interprets path dependencies relative to the requiring package's directory, so pass either an absolute path or a path relative to the project directory.

## Exit codes

| Code | Meaning |
|------|---------|
| `0` | Rewrite succeeded. |
| `2` | Error: bad arguments, dependency not found in lakefile, malformed lakefile, or I/O failure. |

## Example workflows

### Test a local checkout

Point `mathlib` at a local checkout, build, and then restore:

```bash
cd MyProject
lakedit set mathlib --path /local/mathlib4
lake update mathlib
lake build
git checkout lakefile.toml   # or lakefile.lean
```

### Advance a pin manually

Pin a dependency to a specific commit without fetching a range from GitHub:

```bash
lakedit set mathlib --rev abc123def456
lake update mathlib
lake build
```

### Use as the starting point for a `hopscotch` run

Leave the lakefile at the desired `--from` revision and invoke `hopscotch dep` normally; it will pick up the current pin as the lower bound of the commit range.
