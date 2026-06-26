import Hopscotch.Runner
import Hopscotch.State
import Hopscotch.FixCommand
import Hopscotch.Util

namespace Hopscotch.CLI

open Hopscotch
open Hopscotch.State
open Lean

def versionString : String := "hopscotch 1.4.0"

/-- Shared usage text for `hopscotch dep`. -/
def depUsage : String :=
  "usage: hopscotch dep <dependency-name> " ++
  "[--commits-file PATH | --to REF [--from REF]] " ++
  "[--git-url URL] [--project-dir DIR] [--quiet] [--allow-dirty-workspace] " ++
  "[--keep-last-good] [--test] [--lint] " ++
  "[--build-args ARGS] [--test-args ARGS] [--lint-args ARGS] " ++
  "[--no-auto-fix] [--scan-mode [linear|bisect]] [--results-json PATH] [--config-file PATH]"

/-- Shared usage text for `hopscotch toolchain`. -/
def toolchainUsage : String :=
  "usage: hopscotch toolchain --toolchains-file PATH " ++
  "[--project-dir DIR] [--quiet] [--allow-dirty-workspace] " ++
  "[--keep-last-good] [--test] [--lint] " ++
  "[--build-args ARGS] [--test-args ARGS] [--lint-args ARGS] " ++
  "[--scan-mode [linear|bisect]] [--results-json PATH] [--config-file PATH]"

/-- Shared usage text for `hopscotch fix`. -/
def fixUsage : String :=
  "usage: hopscotch fix <apply|revert|list> [--project-dir DIR] [--from RESULTS_JSON] [--no-advisories]"

/-- Shared usage text for `hopscotch continue`. -/
def continueUsage : String :=
  "usage: hopscotch continue [--project-dir DIR] [--quiet] [--allow-dirty-workspace] " ++
  "[--keep-last-good] [--no-auto-fix] [--results-json PATH]"

def helpText : String :=
  "hopscotch — binary-search or linearly scan dependency commits to find a regression\n\n" ++
  "usage: hopscotch <subcommand> [OPTIONS]\n\n" ++
  "Subcommands:\n" ++
  "  dep <name>   Bisect/scan a dependency's commit range\n" ++
  "  toolchain    Bisect/scan a list of toolchain strings\n" ++
  "  continue     Resume the stored session using its saved options\n" ++
  "  fix          Apply/list/revert automated fixes recorded by a run\n" ++
  "  clean        Remove session state (.lake/hopscotch/)\n\n" ++
  "Global flags:\n" ++
  "  --help, -h   Show this help text\n" ++
  "  --version    Print version and exit\n\n" ++
  depUsage ++ "\n\n" ++ toolchainUsage ++ "\n\n" ++
  continueUsage ++ "\n\n" ++
  fixUsage ++ "\n\n" ++
  "usage: hopscotch clean [--project-dir DIR]"

/-- Config file options for the `dep` subcommand. All fields are optional;
    explicit CLI flags take precedence over values set here. -/
private structure DepConfigFile where
  allowDirtyWorkspace : Option Bool := none
  keepLastGood : Option Bool := none
  test : Option Bool := none
  lint : Option Bool := none
  buildArgs : Option (Array String) := none
  testArgs : Option (Array String) := none
  lintArgs : Option (Array String) := none
  projectDir : Option String := none
  gitUrl : Option String := none
  fromRef : Option String := none
  autoFix : Option Bool := none
  deriving FromJson, Inhabited

/-- Config file options for the `toolchain` subcommand. All fields are optional;
    explicit CLI flags take precedence over values set here. -/
private structure ToolchainConfigFile where
  projectDir : Option String := none
  toolchainsFile : Option String := none
  test : Option Bool := none
  lint : Option Bool := none
  buildArgs : Option (Array String) := none
  testArgs : Option (Array String) := none
  lintArgs : Option (Array String) := none
  deriving FromJson, Inhabited

/-- Split a `--*-args` value into individual arguments on whitespace. No shell-style
    quoting; use the config file's array form for an argument that itself contains spaces. -/
private def splitArgs (s : String) : Array String := Id.run do
  let mut out : Array String := #[]
  let mut cur : String := ""
  for c in s.toList do
    if c.isWhitespace then
      unless cur.isEmpty do
        out := out.push cur
        cur := ""
    else
      cur := cur.push c
  unless cur.isEmpty do out := out.push cur
  return out

/-- Load and parse a JSON config file, returning a default value when no file is given. -/
private def loadConfigFile {α : Type _} [Lean.FromJson α] [Inhabited α]
    (path : Option System.FilePath) : IO α :=
  match path with
  | none => return default
  | some p => readJsonFile p

/-- Assemble the run strategy for a `kind`/`scope`/`opts` triple. The single place that
    maps the persisted strategy shape to a `RunStrategy`, shared by `dep`, `toolchain`, and
    `continue` so they can't drift. `scope` is the dependency name for `.dep` (ignored for
    `.toolchain`, whose scope is fixed).

    INTERNAL: HOPSCOTCH_SKIP_BUILD drops the verify steps entirely, leaving only the
    `lake update` bump — for callers that handle build validation separately (e.g. a
    bump-to-latest action in update-only mode). It applies to `.dep` only (toolchain runs
    have never consulted it); not part of the public CLI and may change without notice. -/
private def mkRunStrategy (kind : StrategyKind) (scope : String)
    (opts : Runner.VerifyOptions) : IO Runner.RunStrategy := do
  match kind with
  | .toolchain => return Runner.toolchainStrategy "lake" opts
  | .dep =>
      let base := Runner.lakefileStrategy scope "lake" opts
      match ← IO.getEnv "HOPSCOTCH_SKIP_BUILD" with
      | some "true" => return { base with verify := #[] }
      | _           => return base

-- ---------------------------------------------------------------------------
-- dep subcommand
-- ---------------------------------------------------------------------------

private structure DepParseState where
  dependencyName : String
  configFile : Option System.FilePath := none
  commitsFile : Option System.FilePath := none
  toRef : Option String := none
  fromRef : Option String := none
  gitUrl : Option String := none
  projectDir : Option System.FilePath := none
  runMode : RunMode := .bisect
  quiet : Bool := false
  allowDirtyWorkspace : Option Bool := none
  keepLastGood : Option Bool := none
  test : Option Bool := none
  lint : Option Bool := none
  buildArgs : Option (Array String) := none
  testArgs : Option (Array String) := none
  lintArgs : Option (Array String) := none
  resultsJsonPath : Option System.FilePath := none
  autoFix : Option Bool := none

private def parseDepOptions (state : DepParseState) (args : List String) : IO DepParseState := do
  match args with
  | [] =>
      return state
  | "--config-file" :: path :: rest =>
      parseDepOptions { state with configFile := some (System.FilePath.mk path) } rest
  | "--commits-file" :: path :: rest =>
      parseDepOptions { state with commitsFile := some (System.FilePath.mk path) } rest
  | "--to" :: ref :: rest =>
      parseDepOptions { state with toRef := some ref } rest
  | "--from" :: ref :: rest =>
      parseDepOptions { state with fromRef := some ref } rest
  | "--git-url" :: url :: rest =>
      parseDepOptions { state with gitUrl := some url } rest
  | "--project-dir" :: projectDir :: rest =>
      parseDepOptions { state with projectDir := some (System.FilePath.mk projectDir) } rest
  | "--results-json" :: path :: rest =>
      parseDepOptions { state with resultsJsonPath := some (System.FilePath.mk path) } rest
  | "--quiet" :: rest =>
      parseDepOptions { state with quiet := true } rest
  | "--allow-dirty-workspace" :: rest =>
      parseDepOptions { state with allowDirtyWorkspace := some true } rest
  | "--keep-last-good" :: rest =>
      parseDepOptions { state with keepLastGood := some true } rest
  | "--no-auto-fix" :: rest =>
      parseDepOptions { state with autoFix := some false } rest
  | "--auto-fix" :: rest =>
      parseDepOptions { state with autoFix := some true } rest
  | "--test" :: rest =>
      parseDepOptions { state with test := some true } rest
  | "--lint" :: rest =>
      parseDepOptions { state with lint := some true } rest
  | "--build-args" :: value :: rest =>
      parseDepOptions { state with buildArgs := some (splitArgs value) } rest
  | "--test-args" :: value :: rest =>
      parseDepOptions { state with testArgs := some (splitArgs value) } rest
  | "--lint-args" :: value :: rest =>
      parseDepOptions { state with lintArgs := some (splitArgs value) } rest
  | "--scan-mode" :: "bisect" :: rest =>
      parseDepOptions { state with runMode := .bisect } rest
  | "--scan-mode" :: "linear" :: rest =>
      parseDepOptions { state with runMode := .linear } rest
  | _ =>
      throw <| IO.userError depUsage

private def buildDepConfig (state : DepParseState) : IO Runner.Config := do
  let cfg : DepConfigFile ← loadConfigFile state.configFile
  let allowDirtyWorkspace := (state.allowDirtyWorkspace <|> cfg.allowDirtyWorkspace).getD false
  let keepLastGood := (state.keepLastGood <|> cfg.keepLastGood).getD false
  let autoFixEnabled := (state.autoFix <|> cfg.autoFix).getD true
  let runTest := (state.test <|> cfg.test).getD false
  let runLint := (state.lint <|> cfg.lint).getD false
  let buildArgs := (state.buildArgs <|> cfg.buildArgs).getD #[]
  let testArgs := (state.testArgs <|> cfg.testArgs).getD #[]
  let lintArgs := (state.lintArgs <|> cfg.lintArgs).getD #[]
  let projectDir := (state.projectDir <|> cfg.projectDir.map System.FilePath.mk).getD "."
  let fromRef := state.fromRef <|> cfg.fromRef
  let gitUrl := state.gitUrl <|> cfg.gitUrl
  let itemSource ←
    match state.commitsFile, state.toRef with
    | some path, none =>
        if state.fromRef.isSome then
          throw <| IO.userError "--from is only valid when --to is also specified"
        if state.gitUrl.isSome then
          throw <| IO.userError "--git-url is only valid when --to is also specified"
        pure <| Runner.ItemSource.file path
    | none, some toRef =>
        pure <| Runner.ItemSource.range (some toRef) fromRef gitUrl
    | some _, some _ =>
        throw <| IO.userError
          s!"--commits-file and --to cannot both be specified\n{depUsage}"
    | none, none =>
        -- No --commits-file and no --to: valid when --from or --git-url is present;
        -- --to defaults to the tip of the default branch at runtime.
        if fromRef.isNone && gitUrl.isNone then
          throw <| IO.userError depUsage
        pure <| Runner.ItemSource.range none fromRef gitUrl
  let verifyOpts : Runner.VerifyOptions :=
    { runTest, runLint, buildArgs, testArgs, lintArgs }
  let strategy ← mkRunStrategy .dep state.dependencyName verifyOpts
  return {
    itemSource := itemSource
    projectDir := projectDir
    runMode := state.runMode
    quiet := state.quiet
    allowDirtyWorkspace := allowDirtyWorkspace
    keepLastGood := keepLastGood
    resultsJsonPath := state.resultsJsonPath
    autoFixes := if autoFixEnabled then Hopscotch.AutoFix.standardAutoFixes else #[]
    strategy := strategy
  }

private def parseDep (args : List String) : IO Runner.Config := do
  match args with
  | dependencyName :: rest =>
      let state ← parseDepOptions { dependencyName := dependencyName } rest
      buildDepConfig state
  | [] =>
      throw <| IO.userError depUsage

-- ---------------------------------------------------------------------------
-- toolchain subcommand
-- ---------------------------------------------------------------------------

private structure ToolchainParseState where
  toolchainsFile : Option System.FilePath := none
  projectDir : Option System.FilePath := none
  runMode : RunMode := .bisect
  quiet : Bool := false
  allowDirtyWorkspace : Bool := false
  keepLastGood : Bool := false
  test : Option Bool := none
  lint : Option Bool := none
  buildArgs : Option (Array String) := none
  testArgs : Option (Array String) := none
  lintArgs : Option (Array String) := none
  resultsJsonPath : Option System.FilePath := none
  configFile : Option System.FilePath := none

private def parseToolchainOptions (state : ToolchainParseState)
    (args : List String) : IO ToolchainParseState := do
  match args with
  | [] =>
      return state
  | "--config-file" :: path :: rest =>
      parseToolchainOptions { state with configFile := some (System.FilePath.mk path) } rest
  | "--toolchains-file" :: path :: rest =>
      parseToolchainOptions { state with toolchainsFile := some (System.FilePath.mk path) } rest
  | "--project-dir" :: projectDir :: rest =>
      parseToolchainOptions { state with projectDir := some (System.FilePath.mk projectDir) } rest
  | "--results-json" :: path :: rest =>
      parseToolchainOptions { state with resultsJsonPath := some (System.FilePath.mk path) } rest
  | "--quiet" :: rest =>
      parseToolchainOptions { state with quiet := true } rest
  | "--allow-dirty-workspace" :: rest =>
      parseToolchainOptions { state with allowDirtyWorkspace := true } rest
  | "--keep-last-good" :: rest =>
      parseToolchainOptions { state with keepLastGood := true } rest
  | "--test" :: rest =>
      parseToolchainOptions { state with test := some true } rest
  | "--lint" :: rest =>
      parseToolchainOptions { state with lint := some true } rest
  | "--build-args" :: value :: rest =>
      parseToolchainOptions { state with buildArgs := some (splitArgs value) } rest
  | "--test-args" :: value :: rest =>
      parseToolchainOptions { state with testArgs := some (splitArgs value) } rest
  | "--lint-args" :: value :: rest =>
      parseToolchainOptions { state with lintArgs := some (splitArgs value) } rest
  | "--scan-mode" :: "bisect" :: rest =>
      parseToolchainOptions { state with runMode := .bisect } rest
  | "--scan-mode" :: "linear" :: rest =>
      parseToolchainOptions { state with runMode := .linear } rest
  | _ =>
      throw <| IO.userError toolchainUsage

private def buildToolchainConfig (state : ToolchainParseState) : IO Runner.Config := do
  let cfg : ToolchainConfigFile ← loadConfigFile state.configFile
  let projectDir := (state.projectDir <|> cfg.projectDir.map System.FilePath.mk).getD "."
  let runTest := (state.test <|> cfg.test).getD false
  let runLint := (state.lint <|> cfg.lint).getD false
  let buildArgs := (state.buildArgs <|> cfg.buildArgs).getD #[]
  let testArgs := (state.testArgs <|> cfg.testArgs).getD #[]
  let lintArgs := (state.lintArgs <|> cfg.lintArgs).getD #[]
  let toolchainsFile ←
    match state.toolchainsFile <|> cfg.toolchainsFile.map System.FilePath.mk with
    | some path => pure path
    | none => throw <| IO.userError s!"--toolchains-file is required\n{toolchainUsage}"
  let verifyOpts : Runner.VerifyOptions :=
    { runTest, runLint, buildArgs, testArgs, lintArgs }
  let strategy ← mkRunStrategy .toolchain "toolchain" verifyOpts
  return {
    itemSource := Runner.ItemSource.file toolchainsFile
    projectDir := projectDir
    runMode := state.runMode
    quiet := state.quiet
    allowDirtyWorkspace := state.allowDirtyWorkspace
    keepLastGood := state.keepLastGood
    resultsJsonPath := state.resultsJsonPath
    strategy := strategy
  }

private def parseToolchain (args : List String) : IO Runner.Config := do
  let state ← parseToolchainOptions {} args
  buildToolchainConfig state

-- ---------------------------------------------------------------------------
-- clean subcommand
-- ---------------------------------------------------------------------------

private def parseCleanOptions (projectDir : Option System.FilePath)
    (args : List String) : IO System.FilePath := do
  match args with
  | [] => return projectDir.getD "."
  | "--project-dir" :: dir :: rest =>
      parseCleanOptions (some (System.FilePath.mk dir)) rest
  | _ =>
      throw <| IO.userError "usage: hopscotch clean [--project-dir DIR]"

-- ---------------------------------------------------------------------------
-- fix subcommand
-- ---------------------------------------------------------------------------

private def parseFixOptions (config : FixCommand.Config)
    (args : List String) : IO FixCommand.Config := do
  match args with
  | [] => return config
  | "--project-dir" :: dir :: rest =>
      parseFixOptions { config with projectDir := System.FilePath.mk dir } rest
  | "--from" :: path :: rest =>
      parseFixOptions { config with fromPath := some (System.FilePath.mk path) } rest
  | "--no-advisories" :: rest =>
      parseFixOptions { config with includeAdvisories := false } rest
  | _ =>
      throw <| IO.userError fixUsage

private def parseFix (args : List String) : IO FixCommand.Config := do
  match args with
  | actionStr :: rest =>
      let action ←
        match actionStr with
        | "apply"  => pure FixCommand.Action.apply
        | "revert" => pure FixCommand.Action.revert
        | "list"   => pure FixCommand.Action.list
        | _        => throw <| IO.userError fixUsage
      parseFixOptions { action := action } rest
  | [] =>
      throw <| IO.userError fixUsage

-- ---------------------------------------------------------------------------
-- continue subcommand
-- ---------------------------------------------------------------------------

private structure ContinueParseState where
  projectDir : Option System.FilePath := none
  quiet : Bool := false
  allowDirtyWorkspace : Bool := false
  keepLastGood : Bool := false
  resultsJsonPath : Option System.FilePath := none
  autoFix : Bool := true

private def parseContinueOptions (state : ContinueParseState)
    (args : List String) : IO ContinueParseState := do
  match args with
  | [] => return state
  | "--project-dir" :: dir :: rest =>
      parseContinueOptions { state with projectDir := some (System.FilePath.mk dir) } rest
  | "--results-json" :: path :: rest =>
      parseContinueOptions { state with resultsJsonPath := some (System.FilePath.mk path) } rest
  | "--quiet" :: rest =>
      parseContinueOptions { state with quiet := true } rest
  | "--allow-dirty-workspace" :: rest =>
      parseContinueOptions { state with allowDirtyWorkspace := true } rest
  | "--keep-last-good" :: rest =>
      parseContinueOptions { state with keepLastGood := true } rest
  | "--no-auto-fix" :: rest =>
      parseContinueOptions { state with autoFix := false } rest
  | "--auto-fix" :: rest =>
      parseContinueOptions { state with autoFix := true } rest
  | _ =>
      throw <| IO.userError continueUsage

/-- Reconstruct a run `Config` from the stored session so it can be resumed without
    re-specifying the original options. The item list, run mode, and verify
    configuration come from `state.json`; only the "how to run" flags (project dir,
    quiet, dirty-workspace, keep-last-good, auto-fix, results mirror) come from the
    `continue` invocation, since none of those affect what pass/fail means. -/
private def buildContinueConfig (st : ContinueParseState) : IO Runner.Config := do
  let projectDir := st.projectDir.getD "."
  let paths ← State.mkPaths projectDir
  -- `Runner.run` re-reads this state below (its resume path), so `state.json` is parsed
  -- twice per continue. Negligible for a one-shot command on a tiny file, and it keeps
  -- continue on the shared run path rather than a bespoke resume entry point.
  let some persisted ← State.load? paths
    | throw <| IO.userError
        s!"no hopscotch session found at {paths.statePath}; \
           run `hopscotch dep` or `hopscotch toolchain` first"
  let some spec := persisted.strategySpec
    | throw <| IO.userError
        "this session was created by an older hopscotch and can't be continued; \
         re-run the original dep/toolchain command (or `hopscotch clean` and start over)"
  let verifyOpts : Runner.VerifyOptions :=
    { runTest := spec.runTest, runLint := spec.runLint
      buildArgs := spec.buildArgs, testArgs := spec.testArgs, lintArgs := spec.lintArgs }
  let strategy ← mkRunStrategy spec.kind persisted.strategyScope verifyOpts
  return {
    -- A range source with no refs makes `Runner.run` resume from the stored item list
    -- (state exists) rather than re-resolving it; the original source no longer matters.
    itemSource := Runner.ItemSource.range none none none
    projectDir := projectDir
    runMode := persisted.runMode
    quiet := st.quiet
    allowDirtyWorkspace := st.allowDirtyWorkspace
    keepLastGood := st.keepLastGood
    resultsJsonPath := st.resultsJsonPath
    -- Auto-fix detection is a `dep`-only feature (toolchain runs never enable it), so a
    -- continued toolchain session matches the original by leaving it off regardless of
    -- `--no-auto-fix`/`--auto-fix`.
    autoFixes := if st.autoFix && spec.kind == .dep then Hopscotch.AutoFix.standardAutoFixes else #[]
    strategy := strategy
  }

private def parseContinue (args : List String) : IO Runner.Config := do
  buildContinueConfig (← parseContinueOptions {} args)

-- ---------------------------------------------------------------------------
-- Entrypoint
-- ---------------------------------------------------------------------------

/-- The action to perform, parsed from CLI arguments. -/
inductive Command where
  | run     (config : Runner.Config)
  | fix     (config : FixCommand.Config)
  | clean   (projectDir : System.FilePath)
  | version
  | help

/-- Parse CLI arguments into a `Command`, dispatching on the subcommand. -/
def parseArgs (args : List String) : IO Command := do
  match args with
  | "--version" :: _    => return .version
  | "--help" :: _       => return .help
  | "-h" :: _           => return .help
  | "dep" :: rest       => return .run (← parseDep rest)
  | "toolchain" :: rest => return .run (← parseToolchain rest)
  | "continue" :: rest  => return .run (← parseContinue rest)
  | "fix" :: rest       => return .fix (← parseFix rest)
  | "clean" :: rest     =>
      let projectDir ← parseCleanOptions none rest
      return .clean projectDir
  | _ =>
      throw <| IO.userError helpText

end Hopscotch.CLI
