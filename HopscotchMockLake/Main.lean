import HopscotchTestLib.MockLake

open HopscotchTestLib

/-- Test-only mock `lake` executable used by the IO-heavy runner tests. -/
def main (args : List String) : IO UInt32 :=
  runMockLake args
