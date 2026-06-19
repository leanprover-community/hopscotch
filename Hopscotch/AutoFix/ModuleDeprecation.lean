import Hopscotch.AutoFix.Framework
import Hopscotch.AutoFix.State
import Hopscotch.State
import Hopscotch.LakefileProcessor
import Hopscotch.GitHub
import Hopscotch.Util

/-!
# Module-deprecation fix

Bisecting a downstream project against a fast-moving dependency (mathlib in
particular) sometimes lands on a commit that fails to build for a reason
unrelated to the regression being hunted. The common case is a module
deprecation done in two steps:

1. A PR deletes `Mathlib/Foo/Bar.lean` (the module is split/moved).
2. A later PR re-adds `Mathlib/Foo/Bar.lean` as a `deprecated_module` shim that
   re-exports the new location(s).

For every commit between those two PRs, a downstream `import Mathlib.Foo.Bar`
fails with an unknown-module error. The deletion commit a bisect converges on is
a real breaking change for the downstream, but one with a known mechanical
repair worth reporting alongside the boundary.

The concrete `module-deprecation` fix, built on the `Hopscotch.AutoFix`
framework. The pure string helpers are factored out so they can be unit-tested
without any filesystem or network access.
-/

namespace Hopscotch.AutoFix

open Hopscotch
open Hopscotch.State

namespace ModuleDeprecation

/-- The fix identifier recorded on every migration this fix produces. -/
def fixId : String := "module-deprecation"

/-- Convert a dotted module name to its dependency-relative file path:
    `"Mathlib.Foo.Bar"` → `"Mathlib/Foo/Bar.lean"`. -/
def moduleToRelPath (name : String) : String :=
  (String.intercalate "/" (name.splitOn ".")) ++ ".lean"

/-- Convert a dependency-relative `.lean` file path to a dotted module name:
    `"Mathlib/Foo/Bar.lean"` → `some "Mathlib.Foo.Bar"`. Returns `none` for
    non-`.lean` paths. -/
def relPathToModule? (path : String) : Option String :=
  if path.endsWith ".lean" then
    let stem := (path.dropEnd 5).copy  -- strip ".lean"
    some (String.intercalate "." (stem.splitOn "/"))
  else
    none

/-- The import keywords recognized at the start of a (trimmed) import line, in
    longest-first order so that `public import` is matched before `import`. -/
private def importKeywords : List String :=
  ["public import ", "private import ", "import "]

/-- If `line` is an `import` of `oldModule`, return `(leadingWhitespace, keyword, trailing)`
    where `keyword` is the matched import keyword (e.g. `"import "`) and `trailing`
    is anything after the module name (usually empty or a comment). The match is
    exact on the module token (delimited by whitespace), so `import Foo.Bar` does
    not match `oldModule = "Foo"` nor `import Foo.BarBaz`. -/
def matchImportOf? (line oldModule : String) : Option (String × String × String) :=
  let leading := (line.takeWhile (fun c => c == ' ' || c == '\t')).copy
  let rest := (line.drop leading.length).copy
  -- Exactly one import keyword can prefix a given line (`public import …` does not
  -- also start with `import …`), so `findSome?` cleanly yields the single match.
  importKeywords.findSome? fun kw =>
    if rest.startsWith kw then
      let after := (rest.drop kw.length).copy
      let token := (after.takeWhile (fun c => !c.isWhitespace)).copy
      if token == oldModule then
        let trailing := (after.drop token.length).copy
        some (leading, kw, trailing)
      else
        none  -- an import line, but not of oldModule
    else
      none

/-- Number of (possibly overlapping-free) occurrences of `pat` in `s`. -/
private def countOccurrences (s pat : String) : Nat :=
  (s.splitOn pat).length - 1

/-- The lines of a Lean file that lie outside `/- … -/` block comments, so that
    docstring text starting with `import` or `deprecated_module` cannot be
    mistaken for the real thing. (A line that *opens* a comment is still
    yielded: its prefix is genuine code.) -/
private def topLevelLines (contents : String) : Array String := Id.run do
  let mut out : Array String := #[]
  let mut depth : Nat := 0
  for line in contents.splitOn "\n" do
    if depth == 0 then
      out := out.push line
    depth := (depth + countOccurrences line "/-") - countOccurrences line "-/"
  return out

/-- Extract the dotted module names imported by a `deprecated_module` shim (or any
    Lean file): the targets of every `import` / `public import` / `private import`
    line outside block comments. Lines like `module`, `deprecated_module …`,
    comments and blanks are ignored. -/
def parseShimImports (contents : String) : Array String := Id.run do
  let mut mods : Array String := #[]
  for rawLine in topLevelLines contents do
    let trimmed := rawLine.trimAscii.copy
    for kw in importKeywords do
      if trimmed.startsWith kw then
        let token := ((trimmed.drop kw.length).takeWhile (fun c => !c.isWhitespace)).copy
        unless token.isEmpty do
          mods := mods.push token
        break
  return mods

/-- `true` when `contents` is a `deprecated_module` shim: the command appears at
    column 0 outside block comments (it is a top-level Lean command). Required
    before trusting a blob's imports as migration targets — a *real* module's
    imports are not replacements. -/
def isDeprecatedModuleShim (contents : String) : Bool :=
  (topLevelLines contents).any (·.startsWith "deprecated_module")

/-- Declaration-introducing keywords (after modifiers) used to spot shims that
    still *define* things — typically `@[deprecated]` compatibility aliases. -/
private def declKeywords : List String :=
  ["def ", "theorem ", "lemma ", "instance ", "abbrev ", "alias ",
   "structure ", "inductive ", "opaque ", "@["]

/-- Leading declaration modifiers to skip before checking for a declaration
    keyword (`public import` must not be mistaken for a declaration). -/
private def declModifiers : List String :=
  ["private ", "protected ", "public ", "noncomputable ", "scoped ", "unsafe ", "partial "]

/-- `true` when a shim's body defines declarations beyond its imports and the
    `deprecated_module` command itself — a sign that compatibility aliases live
    *in the shim*, which an import rewrite would drop (the migration is partial). -/
partial def shimDefinesDeclarations (contents : String) : Bool :=
  (topLevelLines contents).any fun line =>
    let rec strip (s : String) : String :=
      match declModifiers.find? (s.startsWith ·) with
      | some m => strip ((s.drop m.length).copy)
      | none => s
    let stripped := strip line.trimAscii.copy
    declKeywords.any (stripped.startsWith ·)

/-- Collect the `import M` lines starting at line `i` (skipping blanks); returns
    the module tokens and the index of the first line past the block. -/
private partial def collectWarningImports (lines : Array String) (i : Nat)
    (acc : Array String) : Array String × Nat :=
  if h : i < lines.size then
    let l := lines[i].trimAscii.copy
    if l.isEmpty then
      collectWarningImports lines (i + 1) acc
    else if l.startsWith "import " then
      let tok := ((l.drop "import ".length).takeWhile (fun c => !c.isWhitespace)).copy
      collectWarningImports lines (i + 1) (if tok.isEmpty then acc else acc.push tok)
    else
      (acc, i)
  else
    (acc, i)

private partial def deprecationWarningsGo (lines : Array String) (i : Nat)
    (out : Array (String × Array String)) : Array (String × Array String) :=
  if h : i < lines.size then
    match lines[i].splitOn "'" with
    | _ :: name :: rest :: _ =>
        if rest.trimAscii.copy.startsWith "has been deprecated: please replace this import by" then
          let (repls, j) := collectWarningImports lines (i + 1) #[]
          let out := if name.isEmpty || out.any (·.1 == name) then out else out.push (name, repls)
          deprecationWarningsGo lines j out
        else
          deprecationWarningsGo lines (i + 1) out
    | _ => deprecationWarningsGo lines (i + 1) out
  else
    out

/-- The `deprecated_module` warnings in a build log, as
    `(module, replacement modules)` pairs. Matches the message Lean core emits on
    importing a deprecated module (`Lean.formatDeprecatedModuleWarning`):
    `'{mod}' has been deprecated: please replace this import by` followed by the
    verbatim `import {repl}` lines. A hint only — when the shim blob is reachable
    it remains the source of truth; the log-carried replacements are used for
    modules outside the inspected dependency (e.g. another package's shims). -/
def deprecationWarnings (log : String) : Array (String × Array String) :=
  deprecationWarningsGo ((log.splitOn "\n").toArray) 0 #[]

/-- Rewrite every `import oldModule` line in `contents` to import `newModules`
    instead, preserving each original line's indentation and import keyword.
    Empty `newModules` removes the matching import lines (the migration for a
    shim that re-exports nothing). Returns `none` when nothing matched (so
    callers can detect a no-op). -/
def rewriteImports (contents oldModule : String) (newModules : Array String) : Option String := Id.run do
  let lines := (contents.splitOn "\n").toArray
  let mut out : Array String := #[]
  let mut changed := false
  for line in lines do
    match matchImportOf? line oldModule with
    | some (leading, kw, trailing) =>
        changed := true
        let mut first := true
        for m in newModules do
          -- Keep any trailing comment on the first replacement line only.
          let suffix := if first then trailing else ""
          out := out.push s!"{leading}{kw}{m}{suffix}"
          first := false
    | none =>
        out := out.push line
  if changed then
    return some (String.intercalate "\n" out.toList)
  else
    return none

end ModuleDeprecation

/-! ### IO support for the module-deprecation fix -/

namespace ModuleDeprecation

open Hopscotch.AutoFix (FixContext DetectResult Fix)

/-- Recursively collect `.lean` files under `root`, skipping `.lake`, `.git`, and
    any dotfile directory. Declared `partial`: termination over the filesystem
    cannot be proven structurally. -/
partial def collectLeanFiles (root : System.FilePath) : IO (Array System.FilePath) := do
  let mut acc : Array System.FilePath := #[]
  unless ← root.pathExists do return acc
  for entry in ← root.readDir do
    let name := entry.fileName
    if ← entry.path.isDir then
      if name == ".lake" || name == ".git" || name.startsWith "." then
        continue
      acc := acc ++ (← collectLeanFiles entry.path)
    else if name.endsWith ".lean" then
      acc := acc.push entry.path
  return acc

/-- Project-relative path string for `file` under `projectDir` (best effort:
    strips the `projectDir` prefix, otherwise uses the file name). -/
private def relPathString (projectDir file : System.FilePath) : String :=
  let p := file.toString
  let root := projectDir.toString
  if p.startsWith (root ++ "/") then (p.drop (root.length + 1)).copy
  -- Only strip a bare `root` prefix when it already ends in a separator; otherwise
  -- a sibling like `/a/projX/f.lean` under root `/a/proj` would be mangled.
  else if root.endsWith "/" && p.startsWith root then (p.drop root.length).copy
  else file.fileName.getD p

/--
Apply one migration to every downstream source file: rewrite `import oldModule`
to the migration's `newModules`. Backs up each file (once) before its first
rewrite. Returns the project-relative paths of the files that changed.
-/
def applyMigrationToWorkspace (paths : Paths) (projectDir : System.FilePath)
    (m : ModuleMigration) : IO (Array String) := do
  let files ← collectLeanFiles projectDir
  let mut changedFiles : Array String := #[]
  for file in files do
    let contents ← IO.FS.readFile file
    match rewriteImports contents m.oldModule m.newModules with
    | some rewritten =>
        let rel := relPathString projectDir file
        backupFileOnce paths rel contents
        IO.FS.writeFile file rewritten
        changedFiles := changedFiles.push rel
    | none => pure ()
  return changedFiles

/-- Resolve `(owner, repo)` for the dependency from the downstream lakefile, if it
    is a recognized GitHub git dependency. -/
private def resolveRepo? (projectDir : System.FilePath) (dependencyName : String)
    : IO (Option (String × String)) := do
  match ← LakefileProcessor.readGitUrlAny projectDir dependencyName with
  | some url => return GitHub.parseRepoId url
  | none => return none

/-- Locate the dependency's checkout under `.lake/packages/<bareName>`. Returns
    `none` when the directory does not exist. -/
private def resolveDepCheckout? (projectDir : System.FilePath) (dependencyName : String)
    : IO (Option System.FilePath) := do
  let bareName := match dependencyName.splitOn "/" with
    | [_, name] => name
    | _ => dependencyName
  let dir := projectDir / ".lake" / "packages" / bareName
  if ← dir.pathExists then return some dir else return none

/-- The dependency-relative `.lean` paths present in the tree at `ref`, or `none`
    when the object is unavailable. One `ls-tree` per ref makes all existence
    checks local array lookups. -/
private def gitLeanFilesAt? (depDir : System.FilePath) (ref : String)
    : IO (Option (Array String)) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", depDir.toString, "ls-tree", "-r", "--name-only", ref]
    env := secretScrubEnv
  }
  if result.exitCode != 0 then return none
  return some (((result.stdout.splitOn "\n").filter (·.endsWith ".lean")).toArray)

/-- Paths of the `deprecated_module` shims present at `ref`, via one `git grep`.
    A cheap pre-index: the blob is still read and `isDeprecatedModuleShim`-checked
    before its imports are trusted. Returns `#[]` when there are none (or on any
    git error). -/
private def gitShimPathsAt (depDir : System.FilePath) (ref : String)
    : IO (Array String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", depDir.toString, "grep", "-lE", "^deprecated_module", ref, "--", "*.lean"]
    env := secretScrubEnv
  }
  if result.exitCode != 0 then return #[]
  -- Output lines are `<ref>:<path>`.
  let mut out : Array String := #[]
  for line in result.stdout.splitOn "\n" do
    match line.splitOn ":" with
    | _ :: rest =>
        let p := String.intercalate ":" rest
        if p.endsWith ".lean" then out := out.push p
    | [] => pure ()
  return out

/-- The commit that deleted `path` within `baseline..upTo`, if any. -/
private def gitDeletionCommit? (depDir : System.FilePath) (baseline upTo path : String)
    : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", depDir.toString, "log", "-1", "--format=%H", "--diff-filter=D",
              s!"{baseline}..{upTo}", "--", path]
    env := secretScrubEnv
  }
  if result.exitCode != 0 then return none
  let sha := result.stdout.trimAscii.copy
  return if sha.isEmpty then none else some sha

/-- The rename target of `path` in `commit`, when git's similarity detection sees
    the deletion as a rename (content moved mostly intact). -/
private def gitRenameTarget? (depDir : System.FilePath) (commit path : String)
    : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", depDir.toString, "show", "-M", "--format=", "--name-status", commit]
    env := secretScrubEnv
  }
  if result.exitCode != 0 then return none
  for line in result.stdout.splitOn "\n" do
    match line.splitOn "\t" with
    | status :: old :: new :: _ =>
        if status.startsWith "R" && old == path then return some new
    | _ => pure ()
  return none

/-- Read `ref:path` from the local dependency checkout via `git show`. Returns
    `none` when the object is absent (e.g. the commit was never fetched). This is
    the offline, network-free path used whenever the shim happens to live in the
    clone's history. -/
private def gitShow? (depDir : System.FilePath) (ref path : String) : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", depDir.toString, "show", s!"{ref}:{path}"]
    env := secretScrubEnv
  }
  if result.exitCode == 0 then return some result.stdout else return none

/-- Fetch the raw contents of `path` from `owner/repo` at the given `ref` via
    `raw.githubusercontent.com` (public, unauthenticated). Returns `none` when the
    file is absent (404 pages are short HTML; we treat any "404: Not Found" body or
    curl failure as absent). -/
private def fetchRaw? (owner repo ref path : String) : IO (Option String) := do
  let url := s!"https://raw.githubusercontent.com/{owner}/{repo}/{ref}/{path}"
  let result ← IO.Process.output {
    cmd := "curl"
    args := #["-sL", "-w", "\n%{http_code}", url]
  }
  if result.exitCode != 0 then return none
  -- The body has the HTTP status appended on the last line (from -w).
  let body := result.stdout
  let lines := body.splitOn "\n"
  match lines.reverse with
  | code :: revBodyLines =>
      if code.trimAscii.copy == "200" then
        return some (String.intercalate "\n" revBodyLines.reverse)
      else
        return none
  | [] => return none

/-- Modules re-exported by a `deprecated_module` shim, excluding any pathological
    self-import. (Lean core derives its own deprecation-warning replacement list
    the same way: the shim module's imports.) -/
private def shimReplacements (oldModule contents : String) : Array String :=
  (parseShimImports contents).filter (· != oldModule)

/-- Resolve the replacements for `oldModule`, whose file is missing at the
    boundary commit. Sources in order — each blob source requires the content to
    actually be a `deprecated_module` shim before its imports are trusted:

    1. the blob at the newest range commit (offline; the window closed in range);
    2. the blob just *before* the deletion (offline; covers the upstream
       shim-cleanup deletions, where the shim only exists in the past);
    3. a rename target at the deletion commit (offline; mechanical move, no shim);
    4. `raw.githubusercontent.com` at the default branch (network; the shim
       landed beyond the range).

    Returns the replacements plus whether the source shim also defines
    declarations (a partial fix; see `ModuleMigration.shimHasDeclarations`).
    `some (#[], _)` means the shim re-exports nothing — drop the import. `none`
    means a genuine removal: propose nothing. -/
private def resolveMissing (depDir : System.FilePath) (repo? : Option (String × String))
    (baseline newestRef oldModule path : String) : IO (Option (Array String × Bool)) := do
  -- 1. Newest range commit.
  if let some blob ← gitShow? depDir newestRef path then
    if isDeprecatedModuleShim blob then
      return some (shimReplacements oldModule blob, shimDefinesDeclarations blob)
  -- 2 & 3. The deletion commit's parent blob, then its rename detection.
  if let some d ← gitDeletionCommit? depDir baseline newestRef path then
    if let some pre ← gitShow? depDir s!"{d}^" path then
      if isDeprecatedModuleShim pre then
        return some (shimReplacements oldModule pre, shimDefinesDeclarations pre)
    if let some target ← gitRenameTarget? depDir d path then
      if let some m := relPathToModule? target then return some (#[m], false)
  -- 4. Raw fetch at the default branch.
  if let some (owner, repo) := repo? then
    for ref in ["master", "main"] do
      if let some blob ← fetchRaw? owner repo ref path then
        if isDeprecatedModuleShim blob then
          return some (shimReplacements oldModule blob, shimDefinesDeclarations blob)
  return none

/-- Tree-lookup detection: resolve every dependency module the downstream imports
    against the dependency tree at the boundary commit and at the newest range
    commit.

    - Missing at the boundary → proposal, when `resolveMissing` can name the
      replacement (or name "remove the import" for an empty shim); otherwise a
      genuine-removal note.
    - Present but resolving through a live `deprecated_module` shim → advisory
      (builds today, breaks at the upstream shim cleanup), promoted to a proposal
      when the build log carries the toolchain's deprecation warning for it (the
      warnings-as-error regime, where the warning itself failed the build). -/
private def detect (ctx : FixContext) : IO DetectResult := do
  let some depDir ← resolveDepCheckout? ctx.projectDir ctx.dependencyName
    | return {}
  let newestRef := ctx.items.back?.getD ctx.currentCommit
  let some treeNew ← gitLeanFilesAt? depDir newestRef
    | return {}
  let some treeCur ←
      (if ctx.currentCommit == newestRef then pure (some treeNew)
       else gitLeanFilesAt? depDir ctx.currentCommit)
    | return {}
  let shimsNew ← gitShimPathsAt depDir newestRef
  -- Top-level module roots of the dependency (e.g. `Mathlib`), to skip imports
  -- that cannot belong to it (Lean, Std, the downstream's own modules) without
  -- paying a git query each. Drawn from the baseline tree as well as the
  -- boundary/newest trees, so a root whose files were all deleted in the range
  -- is still recognized (its imports are exactly the broken ones).
  let treeBase := (← gitLeanFilesAt? depDir ctx.baseline).getD #[]
  let mut roots : Array String := #[]
  for p in treeNew ++ treeCur ++ treeBase do
    let root := (p.splitOn "/").headD p
    let root := if root.endsWith ".lean" then (root.dropEnd 5).copy else root
    unless roots.contains root do roots := roots.push root
  -- The downstream's import set.
  let mut imported : Array String := #[]
  for f in ← collectLeanFiles ctx.projectDir do
    for m in parseShimImports (← IO.FS.readFile f) do
      unless imported.contains m do imported := imported.push m
  -- Toolchain deprecation warnings in the boundary's failing log (warnings-as-error
  -- regime) or in the last successful build's log (green-run audit).
  let warnings ←
    match ctx.buildLogPath with
    | some lp =>
        try
          pure (deprecationWarnings (← IO.FS.readFile lp))
        catch _ =>
          pure (#[] : Array (String × Array String))
    | none => pure #[]
  let warned := warnings.map (·.1)
  -- `repo?` is only needed for the network fallback; a missing URL is not fatal.
  let repo? ← resolveRepo? ctx.projectDir ctx.dependencyName
  let mut migrations : Array ModuleMigration := #[]
  let mut advisories : Array ModuleMigration := #[]
  let mut notes : Array String := #[]
  for m in imported do
    let root := (m.splitOn ".").headD m
    unless roots.contains root do continue
    let p := moduleToRelPath m
    if !treeCur.contains p then
      -- Broken at the boundary commit.
      match ← resolveMissing depDir repo? ctx.baseline newestRef m p with
      | some (newModules, hasDecls) =>
          migrations := migrations.push
            { fixId := fixId, oldModule := m, newModules, shimHasDeclarations := hasDecls }
      | none =>
          notes := notes.push
            s!"{m} is missing in the dependency at the boundary commit and no \
               replacement/deprecation shim was found; leaving it as a genuine failure"
    else if shimsNew.contains p then
      -- Resolves today, but through a deprecation shim at the newest range commit.
      if let some blob ← gitShow? depDir newestRef p then
        if isDeprecatedModuleShim blob then
          let mig : ModuleMigration :=
            { fixId := fixId, oldModule := m, newModules := shimReplacements m blob
              shimHasDeclarations := shimDefinesDeclarations blob }
          if warned.contains m then
            migrations := migrations.push mig
          else
            advisories := advisories.push mig
    else if warned.contains m then
      -- The warning fired at the boundary but the shim is gone at the newest
      -- commit (cleaned up later in range): read it at the boundary itself.
      if let some blob ← gitShow? depDir ctx.currentCommit p then
        if isDeprecatedModuleShim blob then
          migrations := migrations.push
            { fixId := fixId, oldModule := m, newModules := shimReplacements m blob
              shimHasDeclarations := shimDefinesDeclarations blob }
  -- Deprecation warnings for modules outside this dependency's tree (e.g. another
  -- package's shims): the toolchain printed the replacements itself, so surface
  -- them as advisories verbatim.
  for (m, repls) in warnings do
    let seen := migrations.any (·.oldModule == m) || advisories.any (·.oldModule == m)
    unless seen do
      advisories := advisories.push { fixId := fixId, oldModule := m, newModules := repls }
  return { migrations, advisories, notes }

end ModuleDeprecation

/-- The module-deprecation automated fix. -/
def moduleDeprecationFix : Fix := {
  id := ModuleDeprecation.fixId
  description :=
    "Repair downstream imports of dependency modules that were deleted or \
     deprecated (deprecated_module shims): propose the shim's replacement \
     imports, and flag still-working imports that resolve through a shim."
  detect := ModuleDeprecation.detect
  applyOne := ModuleDeprecation.applyMigrationToWorkspace
}

end Hopscotch.AutoFix
