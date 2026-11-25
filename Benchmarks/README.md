# LispKit Benchmarks

This directory contains a small benchmark suite for the LispKit compiler and virtual
machine, a runner that times the benchmarks and compares results against a saved
baseline, and a profiling helper based on the macOS `sample` tool. None of this is
part of the Swift package or the Xcode project.

## Benchmarks

Each benchmark is a Scheme program that loads `harness.scm` and calls `run-bench`, which
prints one line per measurement: `<label>: <milliseconds> ms  result=<value>`. With the
release build from November 2026 (commit `d3dc8ae`) on an Apple M4, each takes between
0.2 and 5 seconds.

| Benchmark | What it exercises |
|---|---|
| `fib` | Non-tail calls of a global procedure, generic arithmetic (fib 27) |
| `tak` | Call-heavy recursion with three arguments and comparisons |
| `loop` | Named-let loop |
| `doloop` | `do` loop with `set!` of a local variable (mutable locals, branches) |
| `global` | Reading and writing a global variable (`push_global`/`set_global`) |
| `closures` | Closure creation, captured mutable variables, composition |
| `records` | Record allocation, accessors, and modifiers |
| `apply` | Variadic procedures, `apply`, named let entered on every call |
| `lists` | Library list procedures: `iota`, `map`, `filter`, `fold-left`, `reverse` |
| `listrec` | User-defined non-tail-recursive `map`; deep stacks, periodic GC |
| `sieve` | Vector access in nested `do` loops |
| `mandel` | Flonum arithmetic through generic operators |
| `strings` | `number->string`, `string-append`, `string-ref` |
| `hashtable` | `equal?` hashtable insertion and lookup |
| `callcc` | Continuation capture and invocation |
| `nqueens` | List manipulation with internal definitions |

`generic-vs-fx.scm` is not part of the default suite. It runs fib, tak, a named-let loop, a
`do` loop, sieve, and mandelbrot once with generic arithmetic and once with the
fixnum/flonum operations (`fx+`, `fl*`, ...) that the compiler turns into VM instructions.
The difference is an upper bound for the gain of compiling generic arithmetic into
instructions with fast paths.

## Running benchmarks

Build a release binary first:

```
swift build -c release -Xswiftc "-D" -Xswiftc "SPM"
```

Then, from the repository root:

```
Benchmarks/run.py                       # full suite
Benchmarks/run.py fib sieve             # selected benchmarks
Benchmarks/run.py generic-vs-fx         # generic vs. specialized arithmetic
Benchmarks/run.py -n 3                  # fastest of three runs per benchmark
Benchmarks/run.py -c Benchmarks/results/baseline-2026-11-25.tsv
Benchmarks/run.py -o Benchmarks/results/my-change.tsv
```

Options:

- `-b BINARY`: the `LispKitRepl` binary (default: `.build/release/LispKitRepl`), e.g. to
  compare builds kept side by side
- `-r DIR`: the resources directory (default: `Sources/LispKit/Resources`)
- `-n N`: run each benchmark N times and report the fastest run
- `-o FILE`: save the results as a TSV file, with date, commit, machine, and binary in
  comment lines
- `-c FILE`: compare against a saved TSV file; prints the change per benchmark and the
  geometric mean of all ratios
- `-t SECONDS`: timeout per benchmark run (default: 600)

Timings vary by about ±3–5% between runs. Use `-n 3` for comparisons, and don't run
benchmarks while a build is running.

## Profiling

`profile.py` runs a benchmark repeatedly in one process, samples the main thread with
`sample`, and prints the self time by category (exclusivity checks, `Expr` copies,
retain/release, array checks, enum payload copies, GC, ...) plus the functions with the
most self time and the most inclusive time:

```
Benchmarks/profile.py fib
Benchmarks/profile.py -s 8 --top 40 sieve
Benchmarks/profile.py --analyze /path/to/fib.sample --callers doDecrementSlow
```

`--callers SYMBOL` lists the nearest non-runtime callers of the functions matching
`SYMBOL`, e.g. to find out which code triggers slow-path reference counting. The raw
`sample` output is kept in a temporary directory, and its path is printed at the end.

## Results

`results/` holds saved runs. `baseline-2026-11-25.tsv` is the state before any
performance work: commit `d3dc8ae`, release build, Apple M4. `step1-runtime-2026-11-25.tsv`
is the state after removing runtime overheads in the virtual machine (exclusivity checks,
reference counting of cached context objects, the object pool's free list, and instructions
that need reference counting): 38% faster on average (geometric mean).
`step2-intrinsics-2026-11-25.tsv` is the state after compiling calls of generic arithmetic
procedures, numeric comparisons, `vector-ref`, and `vector-set!` into instructions with fast
paths for fixnums and flonums. The suite is now 64% faster than the baseline on average, and
generic arithmetic is as fast as the fixnum/flonum-specific operations. `listrec` got slightly
slower in this step: garbage collection is triggered after a fixed number of instructions, so
it runs more often relative to the work done when fewer instructions are needed (68% of the
time of `listrec` is spent in the garbage collector).
`step3-loops-2026-11-25.tsv` is the state after compiling named `let` forms in tail position
into loops and storing mutable local variables on the stack if no closure captures them and
no procedure call within their scope can capture a continuation. The suite is now 72% faster
than the baseline on average (21% faster than after step 2). `listrec` gained the most
(-72%): its loops no longer allocate a closure and a box, so the garbage collector hardly
runs anymore.
