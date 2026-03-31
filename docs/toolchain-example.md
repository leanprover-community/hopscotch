# Step-by-step example: testing across a range of Lean toolchains

Suppose you want to check which nightly Lean releases your project `MyProject` still builds against.

## 1. Prepare a toolchains file

```
$ cat toolchains.txt
leanprover/lean4:v4.14.0
leanprover/lean4:v4.15.0
leanprover/lean4:v4.16.0
leanprover/lean4:nightly-2025-03-01
leanprover/lean4:nightly-2025-03-15
```

Each line is a toolchain string exactly as it would appear in `lean-toolchain`.

## 2. Run linear mode

```bash
hopscotch toolchain \
  --toolchains-file toolchains.txt \
  --project-dir ./MyProject
```

`hopscotch` steps through the list from top to bottom:

```
[1/5] leanprover/lean4:v4.14.0      write lean-toolchain … ok  lake build … ok
[2/5] leanprover/lean4:v4.15.0      write lean-toolchain … ok  lake build … ok
[3/5] leanprover/lean4:v4.16.0      write lean-toolchain … ok  lake build … FAILED
```

The run stops. Check `.lake/hopscotch/logs/2-leanprover-lean4-v4.16.0-build.log` for the full build output.

## 3. Examine the failure

```bash
cat MyProject/.lake/hopscotch/logs/2-leanprover-lean4-v4.16.0-build.log
# MyProject/Foo.lean:7:3: error: unknown identifier 'someRenamedDecl'
```

## 4. Fix the issue and resume

Edit `MyProject/Foo.lean` to use the updated API. After fixing:

```bash
cd MyProject && lake build   # verify the fix works at the failing toolchain
```

Then rerun the same command. `hopscotch` retries the failed toolchain and continues from there:

```
[3/5] leanprover/lean4:v4.16.0           write lean-toolchain … ok  lake build … ok   ← now passes
[4/5] leanprover/lean4:nightly-2025-03-01  write lean-toolchain … ok  lake build … ok
[5/5] leanprover/lean4:nightly-2025-03-15  write lean-toolchain … ok  lake build … ok
Status: completed
```

Repeat steps 3–4 for each failure until the full list passes.

## 5. Pinpoint a breaking toolchain with bisect

If the list is long and you just want to identify the first failing toolchain quickly:

```bash
hopscotch toolchain \
  --toolchains-file toolchains.txt \
  --project-dir ./MyProject \
  --scan-mode bisect
```

`hopscotch` binary-searches the list, probing roughly log₂(N) toolchains instead of all N:

```
[bisect] verifying bad endpoint leanprover/lean4:nightly-2025-03-15 … FAILED ✓
[bisect] probing mid leanprover/lean4:v4.16.0 … FAILED
[bisect] probing mid leanprover/lean4:v4.15.0 … ok
[bisect] boundary found: first failing toolchain is leanprover/lean4:v4.16.0
```

The summary at `.lake/hopscotch/summary.md` records the last known-good toolchain and the first known-bad one.
