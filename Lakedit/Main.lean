import Lakedit.CLI
import Hopscotch.LakefileProcessor

open Lakedit.CLI
open Hopscotch.LakefileProcessor

def main (args : List String) : IO UInt32 := do
  try
    let cmd ← parse args
    match cmd with
    | .help =>
        IO.println helpText
        pure 0
    | .version =>
        IO.println versionString
        pure 0
    | .set depName (.path p) projectDir quiet =>
        setPathAny projectDir depName p.toString
        if !quiet then
          IO.println s!"Set '{depName}' → path \"{p}\""
        pure 0
    | .set depName (.rev sha) projectDir quiet =>
        rewriteAny projectDir depName sha
        if !quiet then
          IO.println s!"Set '{depName}' → rev \"{sha}\""
        pure 0
  catch e =>
    IO.eprintln e.toString
    pure 2
