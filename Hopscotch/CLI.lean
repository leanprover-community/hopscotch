import Hopscotch.Runner
import Hopscotch.State
import Hopscotch.Util

namespace Hopscotch.CLI

open Hopscotch
open Hopscotch.State
open Lean

/-- Shared usage text for `hopscotch dep`. -/
def depUsage : String :=
  "usage: hopscotch dep <dependency-name> " ++
  "[--commits-file PATH | --to REF [--from REF]] " ++
  "[--git-url URL] [--project-dir DIR] [--quiet] [--allow-dirty-workspace] " ++
  "[--scan-mode [linear|bisect]] [--config-file PATH]"

/-- Shared usage text for `hopscotch toolchain`. -/
def toolchainUsage : String :=
  "usage: hopscotch toolchain --toolchains-file PATH " ++
  "[--project-dir DIR] [--quiet] [--allow-dirty-workspace] " ++
  "[--scan-mode [linear|bisect]] [--config-file PATH]"

/-- Config file options for the `dep` subcommand. All fields are optional;
    explicit CLI flags take precedence over values set here. -/
private structure DepConfigFile where
  allowDirtyWorkspace : Option Bool := none
  projectDir : Option String := none
  gitUrl : Option String := none
  fromRef : Option String := none
  deriving FromJson, Inhabited

/-- Config file options for the `toolchain` subcommand. All fields are optional;
    explicit CLI flags take precedence over values set here. -/
private structure ToolchainConfigFile where
  projectDir : Option String := none
  toolchainsFile : Option String := none
  deriving FromJson, Inhabited

/-- Load and parse a JSON config file, returning a default value when no file is given. -/
private def loadConfigFile {α : Type _} [Lean.FromJson α] [Inhabited α]
    (path : Option System.FilePath) : IO α :=
  match path with
  | none => return default
  | some p => readJsonFile p

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
  | "--quiet" :: rest =>
      parseDepOptions { state with quiet := true } rest
  | "--allow-dirty-workspace" :: rest =>
      parseDepOptions { state with allowDirtyWorkspace := some true } rest
  | "--scan-mode" :: "bisect" :: rest =>
      parseDepOptions { state with runMode := .bisect } rest
  | "--scan-mode" :: "linear" :: rest =>
      parseDepOptions { state with runMode := .linear } rest
  | _ =>
      throw <| IO.userError depUsage

private def buildDepConfig (state : DepParseState) : IO Runner.Config := do
  let cfg : DepConfigFile ← loadConfigFile state.configFile
  let allowDirtyWorkspace := (state.allowDirtyWorkspace <|> cfg.allowDirtyWorkspace).getD false
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
  return {
    itemSource := itemSource
    projectDir := projectDir
    runMode := state.runMode
    quiet := state.quiet
    allowDirtyWorkspace := allowDirtyWorkspace
    strategy := Runner.lakefileStrategy state.dependencyName "lake"
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
  | "--quiet" :: rest =>
      parseToolchainOptions { state with quiet := true } rest
  | "--allow-dirty-workspace" :: rest =>
      parseToolchainOptions { state with allowDirtyWorkspace := true } rest
  | "--scan-mode" :: "bisect" :: rest =>
      parseToolchainOptions { state with runMode := .bisect } rest
  | "--scan-mode" :: "linear" :: rest =>
      parseToolchainOptions { state with runMode := .linear } rest
  | _ =>
      throw <| IO.userError toolchainUsage

private def buildToolchainConfig (state : ToolchainParseState) : IO Runner.Config := do
  let cfg : ToolchainConfigFile ← loadConfigFile state.configFile
  let projectDir := (state.projectDir <|> cfg.projectDir.map System.FilePath.mk).getD "."
  let toolchainsFile ←
    match state.toolchainsFile <|> cfg.toolchainsFile.map System.FilePath.mk with
    | some path => pure path
    | none => throw <| IO.userError s!"--toolchains-file is required\n{toolchainUsage}"
  return {
    itemSource := Runner.ItemSource.file toolchainsFile
    projectDir := projectDir
    runMode := state.runMode
    quiet := state.quiet
    allowDirtyWorkspace := state.allowDirtyWorkspace
    strategy := Runner.toolchainStrategy "lake"
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
-- Entrypoint
-- ---------------------------------------------------------------------------

/-- The action to perform, parsed from CLI arguments. -/
inductive Command where
  | run   (config : Runner.Config)
  | clean (projectDir : System.FilePath)

/-- Parse CLI arguments into a `Command`, dispatching on the subcommand. -/
def parseArgs (args : List String) : IO Command := do
  match args with
  | "dep" :: rest       => return .run (← parseDep rest)
  | "toolchain" :: rest => return .run (← parseToolchain rest)
  | "clean" :: rest     =>
      let projectDir ← parseCleanOptions none rest
      return .clean projectDir
  | _ =>
      throw <| IO.userError
        ("usage: hopscotch <subcommand> [OPTIONS]\n" ++
         "Subcommands: dep, toolchain, clean\n\n" ++
         depUsage ++ "\n" ++ toolchainUsage ++ "\n" ++
         "usage: hopscotch clean [--project-dir DIR]")

end Hopscotch.CLI
