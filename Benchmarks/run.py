#!/usr/bin/env python3
#
# run.py
# LispKit benchmarks
#
# Runs the LispKit benchmark suite with a LispKitRepl binary and prints a table of
# run times. Results can be saved as a TSV file and compared against a previously
# saved TSV file (e.g. one of the files in `results/`).
#
# Usage examples (from the repository root):
#   Benchmarks/run.py                                  # run the default suite
#   Benchmarks/run.py fib tak                          # run selected benchmarks
#   Benchmarks/run.py -n 3 -o Benchmarks/results/x.tsv # best of 3, save results
#   Benchmarks/run.py -c Benchmarks/results/baseline-2026-11-25.tsv
#   Benchmarks/run.py generic-vs-fx                    # generic vs. fx/fl arithmetic
#
# Author: Matthias Zenger
# Copyright © 2026 Matthias Zenger. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import argparse
import datetime
import os
import platform
import re
import subprocess
import sys

BENCH_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(BENCH_DIR)

# The default suite, in the order in which benchmarks are run
DEFAULT_SUITE = [
  "fib", "tak", "loop", "doloop", "global", "closures", "records", "apply",
  "lists", "listrec", "sieve", "mandel", "strings", "hashtable", "callcc", "nqueens"
]

RESULT_LINE = re.compile(r"^(.+?): (\d+) ms  result=(.*)$")


def run_benchmark(binary, resources, name, timeout):
  """Runs benchmark `name` once and returns a list of (label, ms, result) tuples."""
  path = os.path.join(BENCH_DIR, name + ".scm")
  proc = subprocess.run([binary, "-r", resources, "--lispkit", path],
                        cwd=BENCH_DIR, capture_output=True, text=True, timeout=timeout)
  results = []
  for line in proc.stdout.splitlines():
    m = RESULT_LINE.match(line.strip())
    if m:
      results.append((m.group(1), int(m.group(2)), m.group(3)))
  if not results:
    out = (proc.stdout + proc.stderr).strip().splitlines()
    raise RuntimeError("no result (exit code %d): %s" % (proc.returncode, " | ".join(out[-3:])))
  return results


def load_tsv(path):
  """Loads a results file: lines `label<TAB>ms[<TAB>result]`; lines starting with # are ignored."""
  res = {}
  with open(path) as f:
    for line in f:
      if line.startswith("#") or not line.strip():
        continue
      cols = line.rstrip("\n").split("\t")
      res[cols[0]] = int(cols[1])
  return res


def machine():
  try:
    cpu = subprocess.run(["sysctl", "-n", "machdep.cpu.brand_string"],
                         capture_output=True, text=True).stdout.strip()
  except OSError:
    cpu = ""
  return "%s, macOS %s" % (cpu or platform.machine(), platform.mac_ver()[0])


def git_commit():
  try:
    return subprocess.run(["git", "-C", REPO_DIR, "log", "-1", "--format=%h"],
                          capture_output=True, text=True).stdout.strip()
  except OSError:
    return "?"


def main():
  ap = argparse.ArgumentParser(description="Run the LispKit benchmark suite.")
  ap.add_argument("benchmarks", nargs="*",
                  help="benchmark names (file names without .scm); default: the full suite")
  ap.add_argument("-b", "--binary", default=os.path.join(REPO_DIR, ".build/release/LispKitRepl"),
                  help="LispKitRepl binary (default: .build/release/LispKitRepl)")
  ap.add_argument("-r", "--resources", default=os.path.join(REPO_DIR, "Sources/LispKit/Resources"),
                  help="LispKit resources directory (default: Sources/LispKit/Resources)")
  ap.add_argument("-n", "--repeat", type=int, default=1,
                  help="run each benchmark n times and report the fastest run")
  ap.add_argument("-o", "--output", help="save results to this TSV file")
  ap.add_argument("-c", "--compare", help="compare against results in this TSV file")
  ap.add_argument("-t", "--timeout", type=int, default=600,
                  help="timeout per benchmark run in seconds (default: 600)")
  args = ap.parse_args()

  if not os.path.isfile(args.binary):
    sys.exit("binary not found: %s\nbuild it with: swift build -c release -Xswiftc \"-D\" -Xswiftc \"SPM\""
             % args.binary)
  names = args.benchmarks or DEFAULT_SUITE
  for name in names:
    if not os.path.isfile(os.path.join(BENCH_DIR, name + ".scm")):
      sys.exit("unknown benchmark: %s" % name)
  baseline = load_tsv(args.compare) if args.compare else None

  rows = []
  for name in names:
    best = {}
    try:
      for _ in range(args.repeat):
        for label, ms, result in run_benchmark(args.binary, args.resources, name, args.timeout):
          if label not in best or ms < best[label][0]:
            best[label] = (ms, result)
    except (RuntimeError, subprocess.TimeoutExpired) as e:
      print("%-20s FAILED: %s" % (name, e), flush=True)
      continue
    for label, (ms, result) in best.items():
      rows.append((label, ms, result))
      line = "%-20s %8d ms" % (label, ms)
      if baseline is not None:
        if label in baseline:
          base = baseline[label]
          line += "   baseline %8d ms   %+6.1f%%" % (base, 100.0 * (ms - base) / base if base else 0.0)
        else:
          line += "   baseline        -"
      print(line + "   result=" + result, flush=True)

  if baseline is not None:
    common = [(ms, baseline[label]) for label, ms, _ in rows if label in baseline and baseline[label] > 0]
    if common:
      ratio = 1.0
      for ms, base in common:
        ratio *= max(ms, 1) / base
      ratio **= 1.0 / len(common)
      print("geometric mean vs. baseline: %+.1f%% (%d benchmarks)" % (100.0 * (ratio - 1.0), len(common)))

  if args.output:
    with open(args.output, "w") as f:
      f.write("# LispKit benchmark results\n")
      f.write("# date: %s\n" % datetime.date.today().isoformat())
      f.write("# commit: %s\n" % git_commit())
      f.write("# machine: %s\n" % machine())
      f.write("# binary: %s\n" % os.path.relpath(args.binary, REPO_DIR))
      f.write("# repeat: %d (fastest run)\n" % args.repeat)
      for label, ms, result in rows:
        f.write("%s\t%d\t%s\n" % (label, ms, result))
    print("results saved to %s" % args.output)


if __name__ == "__main__":
  main()
