#!/usr/bin/env python3
#
# profile.py
# LispKit benchmarks
#
# Profiles a benchmark with the macOS `sample` tool and summarizes where the main thread
# spends its time: a breakdown by category (exclusivity checks, Expr copies,
# retain/release, array checks, GC, ...), the functions with the most self time, and the
# functions with the most inclusive time. Existing `sample` output files can be analyzed
# with `--analyze`.
#
# Usage examples (from the repository root):
#   Benchmarks/profile.py fib                     # profile benchmark fib for 4 seconds
#   Benchmarks/profile.py -s 8 --top 40 sieve
#   Benchmarks/profile.py --analyze /path/to/file.sample
#   Benchmarks/profile.py --analyze /path/to/file.sample --callers doDecrementSlow
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
import collections
import os
import re
import subprocess
import sys
import tempfile
import time

BENCH_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(BENCH_DIR)

NODE_LINE = re.compile(r"^([\s+!:|]*)(\d+) (.*?)(\s+\(in [^)]*\).*)?$")

# Categories for self time; the first matching pattern wins
CATEGORIES = [
  ("exclusivity checks", r"beginAccess|endAccess"),
  ("Expr copy/destroy", r"of Expr|for Expr"),
  ("enum payload copies", r"Procedure\.Kind|Procedure\.Implementation|of Instruction|of Type\b|for Type\b"),
  ("retain/release", r"swift_retain|swift_release|RefCount|bridgeObject|unowned|swift_tryRetain|weak"),
  ("array CoW/bounds", r"ContiguousArray|isUniquelyReferenced|_ArrayBuffer|checkSubscript"),
  ("malloc/free", r"malloc|free|swift_allocObject|swift_deallocObject|swift_slowAlloc|swift_slowDealloc"),
  ("locks", r"[Ll]ock"),
]


def parse_main_thread(path):
  """Parses the call graph of the main thread of a `sample` output file into a tree."""
  with open(path) as f:
    lines = f.read().split("\n")
  start = next((i for i, l in enumerate(lines)
                if "Thread_" in l and "com.apple.main-thread" in l), None)
  if start is None:
    sys.exit("no main thread call graph found in %s" % path)
  root = {"name": "ROOT", "cnt": 0, "kids": []}
  stack = [(-1, root)]
  for line in lines[start + 1:]:
    if not line.strip() or re.match(r"^\s+\d+ Thread_", line):
      break
    m = NODE_LINE.match(line)
    if not m:
      continue
    depth = len(m.group(1))
    name = re.sub(r"\s+\+\s+\d+.*$", "", m.group(3).strip())
    name = re.sub(r"\s*\[0x.*$", "", name)
    node = {"name": name, "cnt": int(m.group(2)), "kids": []}
    while stack[-1][0] >= depth:
      stack.pop()
    stack[-1][1]["kids"].append(node)
    stack.append((depth, node))
  return root


def category(name):
  for cat, pat in CATEGORIES:
    if re.search(pat, name):
      return cat
  if name == "VirtualMachine.execute()":
    return "VM.execute (self)"
  if name.startswith("VirtualMachine.invoke"):
    return "VM.invoke (self)"
  return "other"


def analyze(path, top, callers):
  root = parse_main_thread(path)
  self_time = collections.Counter()
  incl_time = collections.Counter()
  cats = collections.Counter()
  caller_counts = collections.Counter()

  def walk(node, ancestors, in_gc, path_names):
    s = node["cnt"] - sum(k["cnt"] for k in node["kids"])
    self_time[node["name"]] += s
    if node["name"] not in ancestors:
      incl_time[node["name"]] += node["cnt"]
    gc = in_gc or "collectGarbage" in node["name"]
    cats["GC (mark/sweep)" if gc else category(node["name"])] += s
    if callers and callers in node["name"]:
      chain = [p for p in path_names
               if not re.search(r"swift_|DYLD|RefCount|outlined|destroy|WithCopy|WithTake|deduplicated", p)]
      caller_counts[" <- ".join(reversed(chain[-2:]))] += node["cnt"]
      return
    for kid in node["kids"]:
      walk(kid, ancestors | {node["name"]}, gc, path_names + [node["name"]])

  for kid in root["kids"]:
    walk(kid, set(), False, [])
  total = sum(k["cnt"] for k in root["kids"]) or 1

  print("main thread samples: %d" % total)
  print("\n== Self time by category ==")
  for cat, c in cats.most_common():
    print("%6d %5.1f%%  %s" % (c, 100.0 * c / total, cat))
  print("\n== Self time (top %d) ==" % top)
  for name, c in self_time.most_common(top):
    print("%6d %5.1f%%  %s" % (c, 100.0 * c / total, name[:110]))
  print("\n== Inclusive time (top %d, excluding frames present in every sample) ==" % top)
  for name, c in [(n, c) for n, c in incl_time.most_common() if c < total][:top]:
    print("%6d %5.1f%%  %s" % (c, 100.0 * c / total, name[:110]))
  if callers:
    print("\n== Callers of functions matching '%s' ==" % callers)
    for chain, c in caller_counts.most_common(top):
      print("%6d %5.1f%%  %s" % (c, 100.0 * c / total, chain[:160]))


def profile(args):
  if not os.path.isfile(args.binary):
    sys.exit("binary not found: %s" % args.binary)
  bench = os.path.join(BENCH_DIR, args.benchmark + ".scm")
  if not os.path.isfile(bench):
    sys.exit("unknown benchmark: %s" % args.benchmark)
  outdir = tempfile.mkdtemp(prefix="lispkit-profile-")
  # Driver that runs the benchmark repeatedly so that it outlasts the sampling period
  driver = os.path.join(outdir, "driver.scm")
  with open(driver, "w") as f:
    for _ in range(args.iterations):
      f.write("(load \"%s\")\n" % bench)
  sample_file = os.path.join(outdir, args.benchmark + ".sample")
  proc = subprocess.Popen([args.binary, "-r", args.resources, "--lispkit", driver],
                          cwd=BENCH_DIR, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
  try:
    time.sleep(args.warmup)
    if proc.poll() is not None:
      sys.exit("benchmark finished before sampling started; increase --iterations")
    subprocess.run(["sample", str(proc.pid), str(args.seconds), "-f", sample_file],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
  finally:
    proc.kill()
    proc.wait()
  if not os.path.isfile(sample_file):
    sys.exit("sampling failed")
  analyze(sample_file, args.top, args.callers)
  print("\nraw sample output: %s" % sample_file)


def main():
  ap = argparse.ArgumentParser(description="Profile a LispKit benchmark with `sample`.")
  ap.add_argument("benchmark", nargs="?", help="benchmark name (file name without .scm)")
  ap.add_argument("--analyze", metavar="FILE", help="analyze an existing `sample` output file")
  ap.add_argument("-b", "--binary", default=os.path.join(REPO_DIR, ".build/release/LispKitRepl"),
                  help="LispKitRepl binary (default: .build/release/LispKitRepl)")
  ap.add_argument("-r", "--resources", default=os.path.join(REPO_DIR, "Sources/LispKit/Resources"),
                  help="LispKit resources directory (default: Sources/LispKit/Resources)")
  ap.add_argument("-s", "--seconds", type=int, default=4, help="sampling duration (default: 4)")
  ap.add_argument("-w", "--warmup", type=float, default=1.5,
                  help="seconds to wait before sampling starts (default: 1.5)")
  ap.add_argument("-i", "--iterations", type=int, default=20,
                  help="how often the benchmark is run in one process (default: 20)")
  ap.add_argument("--top", type=int, default=25, help="number of functions listed (default: 25)")
  ap.add_argument("--callers", metavar="SYMBOL",
                  help="also list the callers of functions whose name contains SYMBOL")
  args = ap.parse_args()
  if args.analyze:
    analyze(args.analyze, args.top, args.callers)
  elif args.benchmark:
    profile(args)
  else:
    ap.error("specify a benchmark or --analyze FILE")


if __name__ == "__main__":
  main()
