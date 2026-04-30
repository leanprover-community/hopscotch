import Lean.Data.Json
import Hopscotch.Util

namespace Hopscotch.LakefileProcessor

/-- Preserve CRLF (or CR-only) formatting when rewriting a `lakefile.toml`. -/
private def preferredNewline (contents : String) : String :=
  if contents.contains "\r\n" then "\r\n"
  else if contents.contains "\r" then "\r"
  else "\n"

/-- Detect one `[[require]]` section header after trimming indentation. -/
private def isRequireHeader (line : String) : Bool :=
  line.trimAscii.copy == "[[require]]"

/-- Detect any TOML section header (`[table]` or `[[array-of-tables]]`) after trimming indentation. -/
private def isTomlSectionHeader (line : String) : Bool :=
  line.trimAscii.startsWith "["

/-- Read `name = "..."` or `rev = "..."` values from a trimmed TOML assignment line.
    Splits on `"` to extract the value; does not handle escaped quotes inside values. -/
private def quotedAssignmentValue? (key : String) (line : String) : Option String :=
  let trimmed := line.trimAscii.copy
  let doubleQuote := "\""
  let assignmentPrefix := key ++ " = " ++ doubleQuote
  if trimmed.startsWith assignmentPrefix then
    match (trimmed.drop assignmentPrefix.length |>.copy).splitOn doubleQuote with
    | value :: _ => some value
    | [] => none
  else
    none

/-- Replace only the quoted revision value while preserving indentation and any suffix. -/
private def rewriteRevLine (line : String) (rev : String) : Except String String :=
  let indent := (line.takeWhile fun ch => ch == ' ' || ch == '\t').copy
  let body := line.drop indent.length |>.copy
  let doubleQuote := "\""
  let revPrefix := "rev = " ++ doubleQuote
  if !body.startsWith revPrefix then
    .error "internal error: attempted to rewrite a non-rev line"
  else
    match (body.drop revPrefix.length |>.copy).splitOn doubleQuote with
    | _ :: suffixParts =>
        -- suffixParts is everything after the closing quote; rejoining it preserves inline TOML comments.
        let suffix := String.intercalate doubleQuote suffixParts
        .ok s!"{indent}{revPrefix}{rev}{doubleQuote}{suffix}"
    | [] =>
        .error "internal error: malformed rev line"

/--
Scan `lines` and collect the `(contentStart, contentEnd)` range for every
`[[require]]` block whose `name` field matches `dependencyName`.

`contentStart` is the line index immediately after the `[[require]]` header;
`contentEnd` is the exclusive end index (the next `[[require]]` header or
`lines.size`). This shared primitive is used by both the rewrite and read paths.
-/
private def findNamedRequireBlocks (lines : Array String) (dependencyName : String)
    : Array (Nat × Nat) := Id.run do
  let mut blocks : Array (Nat × Nat) := #[]
  let mut i := 0
  while i < lines.size do
    if isRequireHeader lines[i]! then
      -- Scan forward to find the end of this block (next header or EOF).
      let blockStart := i + 1
      let mut blockEnd := lines.size
      let mut j := blockStart
      while j < lines.size do
        if isTomlSectionHeader lines[j]! then
          blockEnd := j
          break
        j := j + 1
      -- Record this block if its `name` field matches.
      let hasName := (lines.extract blockStart blockEnd).any
        fun line => quotedAssignmentValue? "name" line == some dependencyName
      if hasName then
        -- Decrement blockEnd while it's whitespace or newline
        while blockEnd > blockStart && lines[blockEnd - 1]!.isEmpty do
          blockEnd := blockEnd - 1
        blocks := blocks.push (blockStart, blockEnd)
      i := blockEnd
    else
      i := i + 1
  blocks

/--
Find the first `rev` line inside the unique `[[require]]` block named
`dependencyName`.

Returns `some index` when the block contains a `rev` entry, or `none` when
the block exists but has no `rev`. Throws when the block is absent or when
multiple matching blocks exist.
-/
private def findDependencyRevLine (lines : Array String) (dependencyName : String)
    : Except String (Option Nat) := do
  let blocks := findNamedRequireBlocks lines dependencyName
  if blocks.isEmpty then
    throw s!"lakefile.toml does not contain a [[require]] block named '{dependencyName}'"
  if blocks.size > 1 then
    throw s!"lakefile.toml contains multiple [[require]] blocks named '{dependencyName}'"
  let (blockStart, blockEnd) := blocks[0]!
  match (lines.extract blockStart blockEnd).findIdx? fun line =>
      (quotedAssignmentValue? "rev" line).isSome with
  | some relIdx => return some (blockStart + relIdx)
  | none => return none

/--
Find the line index before which a new `rev` entry should be inserted for
the named `[[require]]` block. Returns the exclusive content-end of the block
(next `[[require]]` header, or `lines.size`).
-/
private def findRevInsertionPoint (lines : Array String) (dependencyName : String)
    : Except String Nat := do
  let blocks := findNamedRequireBlocks lines dependencyName
  if blocks.isEmpty then
    throw s!"lakefile.toml does not contain a [[require]] block named '{dependencyName}'"
  return blocks[0]!.2

/--
Rewrite the first `rev` line that appears after a matching dependency `name`, or insert
a new `rev` entry at the end of the block when none exists.
-/
def rewriteContents (contents : String) (dependencyName : String) (rev : String) : Except String String := do
  let newline := preferredNewline contents
  let lines := (contents.splitOn newline).toArray
  match ← findDependencyRevLine lines dependencyName with
  | some revLineIndex =>
      let rewrittenLine ← rewriteRevLine lines[revLineIndex]! rev
      let rewrittenLines := lines.mapIdx fun index line =>
        if index == revLineIndex then rewrittenLine else line
      return String.intercalate newline rewrittenLines.toList
  | none =>
      let insertAt ← findRevInsertionPoint lines dependencyName
      let revLine := s!"rev = \"{rev}\""
      let newLines := lines.toList.take insertAt ++ [revLine] ++ lines.toList.drop insertAt
      return String.intercalate newline newLines

/-- Rewrite a dependency `rev` inside a `lakefile.toml` on disk. -/
def rewriteFile (path : System.FilePath) (dependencyName : String) (rev : String) : IO Unit := do
  let contents ← IO.FS.readFile path
  let rewritten ← IO.ofExcept <| rewriteContents contents dependencyName rev
  IO.FS.writeFile path rewritten

/--
Read a quoted field value from the `[[require]]` block named `dependencyName`.

Returns `none` when the dependency block is not found or when the block does not
contain the requested field. Does not throw.

Unlike `findDependencyRevLine` (used by `rewriteFile`), this function does not
reject duplicate blocks — if multiple blocks share the same name, only the first
one is checked.
-/
private def readFieldValue (lines : Array String) (dependencyName : String) (field : String)
    : Option String := Id.run do
  let blocks := findNamedRequireBlocks lines dependencyName
  if blocks.isEmpty then return none
  let (blockStart, blockEnd) := blocks[0]!
  for line in lines.extract blockStart blockEnd do
    if let some value := quotedAssignmentValue? field line then
      return some value
  return none

private def readFieldFromFile (path : System.FilePath) (dependencyName field : String)
    : IO (Option String) := do
  let contents ← IO.FS.readFile path
  let lines := (contents.splitOn (preferredNewline contents)).toArray
  return readFieldValue lines dependencyName field

/--
Read the `git` URL from the `[[require]]` block named `dependencyName` in a
`lakefile.toml`, returning `none` when the block or field is absent.
-/
def readGitUrl (path : System.FilePath) (dependencyName : String) : IO (Option String) :=
  readFieldFromFile path dependencyName "git"

/--
Read the `scope` from the `[[require]]` block named `dependencyName` in a
`lakefile.toml`, returning `none` when the block or field is absent.
-/
def readScope (path : System.FilePath) (dependencyName : String) : IO (Option String) :=
  readFieldFromFile path dependencyName "scope"

/--
Read the pinned `rev` from the `[[require]]` block named `dependencyName` in a
`lakefile.toml`, returning `none` when the block or field is absent.
-/
def readPinnedRev (path : System.FilePath) (dependencyName : String) : IO (Option String) :=
  readFieldFromFile path dependencyName "rev"

/-! ## `lakefile.lean` support

`lakefile.lean` uses Lean DSL `require` declarations instead of TOML `[[require]]` blocks.
The relevant syntax is:

    require depName (@ git? term)? (from git "url" (@ "rev")? (/ subdir)?)? (with term)?

We only care about the git-source form (`from git "url" @ "rev"`).
Parsing is line-based; no Lean AST parser is involved.
-/

/--
Returns `true` when a line is a continuation of the preceding `require` statement.

A continuation line is one that belongs to the same `require` declaration rather
than starting a new top-level item. Blank lines and comment lines end the block.
Lines with leading whitespace, or that start with `@`, `"`, `/`, or `(`, continue
the declaration.
-/
private def isLeanContinuationLine (line : String) : Bool :=
  if line.isEmpty then false
  else
    let trimmed := line.trimAscii
    if trimmed.isEmpty then false
    else if trimmed.startsWith "--" || trimmed.startsWith "/-" then false
    else if line.front == ' ' || line.front == '\t' then true
    else trimmed.startsWith "@" || trimmed.startsWith "\"" ||
         trimmed.startsWith "/" || trimmed.startsWith "("

/--
Build the set of line-start patterns that identify a `require` declaration for `depName`.

Bare name `"batteries"` matches `require batteries`.
Scoped name `"leanprover-community/batteries"` matches the Lake DSL form
`require "leanprover-community" / "batteries"` (with flexible whitespace around `/`).
-/
private def leanRequirePrefixes (depName : String) : Array String :=
  match depName.splitOn "/" with
  | [scope, name] =>
      -- Scoped name: `require "scope" / "name"` with common whitespace variants around `/`.
      #[ "require \"" ++ scope ++ "\" / \"" ++ name ++ "\""
       , "require \"" ++ scope ++ "\"/\"" ++ name ++ "\""
       ]
  | _ =>
      -- Bare name: `require depName` or `require "depName"` (quoted form is also valid Lake DSL).
      #["require " ++ depName, "require \"" ++ depName ++ "\""]

/--
Scan `lines` and collect every `require` block whose dependency name equals `depName`.

Returns an array of `(headerLineIndex, blockLines)` pairs, where `blockLines`
contains the header line and all continuation lines that follow it.

For a bare name like `"batteries"`, the match is `require batteries` followed by
a space, `@`, or end-of-string (preventing `"batteries"` from falsely matching
`"batteriesExtra"`). For a scoped name like `"leanprover-community/batteries"`,
the match is `require "leanprover-community" / "batteries"`.
-/
private def findLeanRequireBlocks (lines : Array String) (depName : String)
    : Array (Nat × Array String) := Id.run do
  let mut blocks : Array (Nat × Array String) := #[]
  let prefixes := leanRequirePrefixes depName
  let mut i := 0
  while i < lines.size do
    let trimmed := lines[i]!.trimAscii
    -- Check if this line is a require header for depName.
    let isMatch := prefixes.any fun pfx =>
      trimmed == pfx ||
      trimmed.startsWith (pfx ++ " ") ||
      trimmed.startsWith (pfx ++ "@")
    if isMatch then
      -- Collect continuation lines.
      let mut blockLines : Array String := #[lines[i]!]
      let mut j := i + 1
      while j < lines.size && isLeanContinuationLine lines[j]! do
        blockLines := blockLines.push lines[j]!
        j := j + 1
      blocks := blocks.push (i, blockLines)
      i := j
    else
      i := i + 1
  blocks

/--
Extract the git URL from `from git "url"` in the joined block text.
Returns `none` when no `from git` source is present.
-/
private def leanExtractGitUrl (blockLines : Array String) : Option String :=
  let text := String.intercalate " " blockLines.toList
  -- Find text after `from git` (which may be followed by varying whitespace before the `"`).
  match (text.splitOn "from git").drop 1 with
  | [] => none
  | afterFromGit :: _ =>
      -- afterFromGit looks like: `  "url" @ "rev"` — skip to first `"`, then take until next `"`.
      match afterFromGit.splitOn "\"" with
      | _ :: url :: _ => some url
      | _ => none

/--
Extract the git revision from `@ "rev"` that appears after `from git "url"`
in the joined block text. Returns `none` when no git URL is present or when
no `@ "..."` follows the URL.
-/
private def leanExtractGitRev (blockLines : Array String) : Option String :=
  let text := String.intercalate " " blockLines.toList
  -- Case 1: `from git "url" @ "rev"` — extract rev after the URL.
  match (text.splitOn "from git").drop 1 with
  | afterFromGit :: _ =>
      -- afterFromGit: `  "url" @ "rev"`. Split on `"` to get: ["  ", "url", " @ ", "rev", ""].
      -- Skip the leading whitespace piece and the URL piece; rejoin the rest with `"`.
      match afterFromGit.splitOn "\"" with
      | _ :: _ :: rest =>
          let postUrl := String.intercalate "\"" rest
          -- postUrl looks like ` @ "rev"`. Find `@ "` and extract the rev.
          match (postUrl.splitOn "@ \"").drop 1 with
          | afterAt :: _ =>
              match afterAt.splitOn "\"" with
              | rev :: _ => some rev
              | [] => none
          | [] => none
      | _ => none
  | [] =>
      -- Case 2: `@ git "rev"` — Reservoir package with a git revision.
      match (text.splitOn "@ git \"").drop 1 with
      | afterAtGit :: _ =>
          match afterAtGit.splitOn "\"" with
          | rev :: _ => some rev
          | [] => none
      | [] => none

/--
Read the git URL from the `require depName` block in a `lakefile.lean`,
returning `none` when the block or git source is absent.
-/
def readLeanGitUrl (path : System.FilePath) (depName : String) : IO (Option String) := do
  let contents ← IO.FS.readFile path
  let lines := (contents.splitOn (preferredNewline contents)).toArray
  let blocks := findLeanRequireBlocks lines depName
  if blocks.isEmpty then return none
  let (_, blockLines) := blocks[0]!
  return leanExtractGitUrl blockLines

/--
Read the pinned revision from the `require depName` block in a `lakefile.lean`,
returning `none` when the block, git source, or `@ "rev"` annotation is absent.
-/
def readLeanPinnedRev (path : System.FilePath) (depName : String) : IO (Option String) := do
  let contents ← IO.FS.readFile path
  let lines := (contents.splitOn (preferredNewline contents)).toArray
  let blocks := findLeanRequireBlocks lines depName
  if blocks.isEmpty then return none
  let (_, blockLines) := blocks[0]!
  return leanExtractGitRev blockLines

/--
Extract the scope from a scoped dependency name.

In `lakefile.lean`, the scope is encoded in the require syntax as
`require "scope" / "name"`. When the caller passes `"scope/name"` as the
dependency name, the scope is the part before `/`. Returns `none` for bare
(unscoped) names.
-/
def readLeanScope (_ : System.FilePath) (depName : String) : IO (Option String) := do
  match depName.splitOn "/" with
  | [scope, _] => return some scope
  | _ => return none

/--
Rewrite `@ "old"` → `@ "new"` in a line, preserving leading whitespace and
any content after the closing quote (e.g. a trailing `with` clause or comment).
-/
private def rewriteAtRevLine (line : String) (rev : String) : Except String String :=
  let doubleQuote := "\""
  -- Determine which marker is present: `@ git "` (Reservoir git rev) or `@ "` (git source rev).
  let atGitMarker := "@ git \""
  let atMarker := "@ \""
  let marker := if line.contains atGitMarker then atGitMarker else atMarker
  match line.splitOn marker with
  | [before, afterAt] =>
      match afterAt.splitOn doubleQuote with
      | _ :: suffixParts =>
          let suffix := String.intercalate doubleQuote suffixParts
          .ok s!"{before}{marker}{rev}{doubleQuote}{suffix}"
      | [] =>
          .error "internal error: malformed @ rev annotation"
  | _ =>
      .error "internal error: line does not contain exactly one rev annotation"

/--
Find the absolute line index of the `@ "rev"` annotation within the require block.

Scans block lines starting at `headerIdx`. Once `from git "` is seen, looks for
`@ "` on the same line (after the URL's closing quote) or on subsequent lines.
Returns `none` when the block has no git URL or no rev annotation.
-/
private def findLeanRevLine (lines : Array String) (headerIdx blockLen : Nat) : Option Nat :=
  Id.run do
  -- First check if the block uses `from git` (explicit source) vs `@ git "rev"` (Reservoir).
  let blockHasFromGit := (List.range blockLen).any fun i =>
    lines[headerIdx + i]!.contains "from git"
  if blockHasFromGit then
    -- `from git "url" @ "rev"` pattern: find `@ "` after the URL.
    let mut seenGitUrl := false
    for i in List.range blockLen do
      let lineIdx := headerIdx + i
      let line := lines[lineIdx]!
      if !seenGitUrl then
        if line.contains "from git" then
          seenGitUrl := true
          -- Check if `@ "` also appears after the URL's closing quote on this line.
          let afterFromGit := match line.splitOn "from git" with
            | _ :: rest => String.intercalate "from git" rest
            | _ => ""
          let postUrl := match (afterFromGit.splitOn "\"").drop 1 with
            | _ :: rest => String.intercalate "\"" rest
            | _ => ""
          if postUrl.contains "@ \"" then
            return some lineIdx
      else
        let trimmed := line.trimAscii
        if trimmed.startsWith "@ \"" || line.contains " @ \"" then
          return some lineIdx
    none
  else
    -- `@ git "rev"` pattern (Reservoir with git revision): find the line with `@ git "`.
    for i in List.range blockLen do
      let lineIdx := headerIdx + i
      let line := lines[lineIdx]!
      if line.contains "@ git \"" then
        return some lineIdx
    none

/--
Rewrite the pinned revision in a `lakefile.lean` contents string, or insert
a new `@ "rev"` annotation when the dependency has no existing revision.
-/
def rewriteLeanContents (contents : String) (depName : String) (rev : String)
    : Except String String := do
  let newline := preferredNewline contents
  let lines := (contents.splitOn newline).toArray
  let blocks := findLeanRequireBlocks lines depName
  if blocks.isEmpty then
    throw s!"lakefile.lean does not contain a require block named '{depName}'"
  if blocks.size > 1 then
    throw s!"lakefile.lean contains multiple require blocks named '{depName}'"
  let (headerIdx, blockLines) := blocks[0]!
  let blockText := String.intercalate " " blockLines.toList
  if !blockText.contains "from git" && !blockText.contains "@ git \"" then
    throw s!"'{depName}' in lakefile.lean is not a git dependency; cannot pin a revision"
  match findLeanRevLine lines headerIdx blockLines.size with
  | some revLineIdx =>
      let rewrittenLine ← rewriteAtRevLine lines[revLineIdx]! rev
      let rewrittenLines := lines.mapIdx fun idx line =>
        if idx == revLineIdx then rewrittenLine else line
      return String.intercalate newline rewrittenLines.toList
  | none =>
      -- No existing @ "rev"; append one after the last line of the block.
      let insertAt := headerIdx + blockLines.size
      let revLine := s!"  @ \"{rev}\""
      let newLines := lines.toList.take insertAt ++ [revLine] ++ lines.toList.drop insertAt
      return String.intercalate newline newLines

/-- Rewrite a dependency revision inside a `lakefile.lean` on disk. -/
def rewriteLeanFile (path : System.FilePath) (depName : String) (rev : String) : IO Unit := do
  let contents ← IO.FS.readFile path
  let rewritten ← IO.ofExcept <| rewriteLeanContents contents depName rev
  IO.FS.writeFile path rewritten

/-! ## Auto-detection: unified `*Any` functions

These functions detect whether the project uses `lakefile.lean` or `lakefile.toml`
and delegate to the appropriate format. `lakefile.lean` takes precedence when both
exist, matching Lake's own resolution order.
-/

private def hasLakeLean (projectDir : System.FilePath) : IO Bool :=
  (projectDir / "lakefile.lean").pathExists

/--
Read the git URL for `depName` from whichever lakefile format the project uses.
-/
def readGitUrlAny (projectDir : System.FilePath) (depName : String) : IO (Option String) := do
  if ← hasLakeLean projectDir then
    readLeanGitUrl (projectDir / "lakefile.lean") depName
  else
    readGitUrl (projectDir / "lakefile.toml") depName

/--
Read the pinned revision for `depName` from whichever lakefile format the project uses.
-/
def readPinnedRevAny (projectDir : System.FilePath) (depName : String) : IO (Option String) := do
  if ← hasLakeLean projectDir then
    readLeanPinnedRev (projectDir / "lakefile.lean") depName
  else
    readPinnedRev (projectDir / "lakefile.toml") depName

/--
Read the resolved revision for `depName` from `lake-manifest.json`.

`lake build` uses the revision recorded in the manifest rather than the `rev` field in
the lakefile, so this gives a more accurate lower bound for the commit range when the
lakefile pins a branch name such as `master` instead of a concrete SHA.

Returns `none` if the manifest file does not exist or the package is not listed.
-/
def readManifestRev (projectDir : System.FilePath) (depName : String) : IO (Option String) := do
  let manifestPath := projectDir / "lake-manifest.json"
  if !(← System.FilePath.pathExists manifestPath) then return none
  let contents ← IO.FS.readFile manifestPath
  let json ← IO.ofExcept <| Lean.Json.parse contents
  -- The manifest `name` field is the bare package name (no scope prefix).
  let pkgName := match depName.splitOn "/" with
    | [_, name] => name
    | _ => depName
  let packages ← IO.ofExcept <| json.getObjValAs? (Array Lean.Json) "packages"
  for pkg in packages do
    if let .ok name := pkg.getObjValAs? String "name" then
      if name == pkgName then
        return (pkg.getObjValAs? String "rev").toOption
  return none

/--
Read the scope for `depName` from whichever lakefile format the project uses.
Returns `none` for `lakefile.lean` projects (scope is not part of that syntax).
-/
def readScopeAny (projectDir : System.FilePath) (depName : String) : IO (Option String) := do
  if ← hasLakeLean projectDir then
    readLeanScope (projectDir / "lakefile.lean") depName
  else
    readScope (projectDir / "lakefile.toml") depName

/--
Rewrite the pinned revision for `depName` in whichever lakefile format the project uses.
-/
def rewriteAny (projectDir : System.FilePath) (depName : String) (rev : String) : IO Unit := do
  if ← hasLakeLean projectDir then
    rewriteLeanFile (projectDir / "lakefile.lean") depName rev
  else
    rewriteFile (projectDir / "lakefile.toml") depName rev

/-! ## Path-source rewriting

These functions rewrite a dependency to use a local `path = "..."` source
(TOML) or `from "path"` source (Lean lakefile), dropping any existing git/rev
fields. The user is responsible for the path value — it is passed through
unchanged, since Lake interprets it relative to the requiring package.
-/

/--
Rewrite the `[[require]]` block for `depName` to use a local `path = "..."` source.

Drops any existing `git`, `rev`, `path`, or `source` fields and inserts
`path = "..."` immediately after the `name` field (or at the end of the block
when no `name` line is present). All other fields and comment lines are kept.
-/
def setPathContents (contents : String) (depName : String) (path : String) : Except String String := do
  let newline := preferredNewline contents
  let lines := (contents.splitOn newline).toArray
  let blocks := findNamedRequireBlocks lines depName
  if blocks.isEmpty then
    throw s!"lakefile.toml does not contain a [[require]] block named '{depName}'"
  if blocks.size > 1 then
    throw s!"lakefile.toml contains multiple [[require]] blocks named '{depName}'"
  let (blockStart, blockEnd) := blocks[0]!
  let isSourceField (line : String) : Bool :=
    (quotedAssignmentValue? "git" line).isSome ||
    (quotedAssignmentValue? "rev" line).isSome ||
    (quotedAssignmentValue? "path" line).isSome ||
    line.trimAscii.startsWith "source ="
  let blockLines := (lines.extract blockStart blockEnd).filter (fun line => !isSourceField line)
  let pathLine := s!"path = \"{path}\""
  let nameLineIdx := blockLines.findIdx? (fun line => (quotedAssignmentValue? "name" line).isSome)
  let newBlockLines := match nameLineIdx with
    | some idx => blockLines.toList.take (idx + 1) ++ [pathLine] ++ blockLines.toList.drop (idx + 1)
    | none => blockLines.toList ++ [pathLine]
  let newLines := lines.toList.take blockStart ++ newBlockLines ++ lines.toList.drop blockEnd
  return String.intercalate newline newLines

/-- Rewrite a dependency to use a local path inside a `lakefile.toml` on disk. -/
def setPathFile (filePath : System.FilePath) (depName : String) (localPath : String) : IO Unit := do
  let contents ← IO.FS.readFile filePath
  let rewritten ← IO.ofExcept <| setPathContents contents depName localPath
  IO.FS.writeFile filePath rewritten

/--
Rewrite the `require depName` block in a `lakefile.lean` to use a local
`from "path"` source.

Replaces the entire block (header + continuation lines) with a single-line
`require <depName> from "<path>"`. Throws when the block is absent, when
multiple blocks exist, or when a `with` clause is present (unsupported in v1).
-/
def setLeanPathContents (contents : String) (depName : String) (path : String)
    : Except String String := do
  let newline := preferredNewline contents
  let lines := (contents.splitOn newline).toArray
  let blocks := findLeanRequireBlocks lines depName
  if blocks.isEmpty then
    throw s!"lakefile.lean does not contain a require block named '{depName}'"
  if blocks.size > 1 then
    throw s!"lakefile.lean contains multiple require blocks named '{depName}'"
  let (headerIdx, blockLines) := blocks[0]!
  let blockText := String.intercalate " " blockLines.toList
  if blockText.contains " with " then
    throw s!"'{depName}' in lakefile.lean has a 'with' clause; manual editing required"
  let newHeader := match depName.splitOn "/" with
    | [scope, name] => s!"require \"{scope}\" / \"{name}\" from \"{path}\""
    | _ => s!"require {depName} from \"{path}\""
  let blockLen := blockLines.size
  let newLines := lines.toList.take headerIdx ++ [newHeader] ++ lines.toList.drop (headerIdx + blockLen)
  return String.intercalate newline newLines

/-- Rewrite a dependency to use a local path inside a `lakefile.lean` on disk. -/
def setLeanPathFile (filePath : System.FilePath) (depName : String) (localPath : String) : IO Unit := do
  let contents ← IO.FS.readFile filePath
  let rewritten ← IO.ofExcept <| setLeanPathContents contents depName localPath
  IO.FS.writeFile filePath rewritten

/--
Rewrite the dependency `depName` to use a local path in whichever lakefile
format the project uses. Prefers `lakefile.lean` when both exist (matching
Lake's own resolution order).
-/
def setPathAny (projectDir : System.FilePath) (depName : String) (localPath : String) : IO Unit := do
  if ← hasLakeLean projectDir then
    setLeanPathFile (projectDir / "lakefile.lean") depName localPath
  else
    setPathFile (projectDir / "lakefile.toml") depName localPath

end Hopscotch.LakefileProcessor
