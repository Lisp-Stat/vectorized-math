#!/usr/bin/env bash
# Usage: bench/run.sh TAG    runs the benchmark on SBCL and CCL, results in bench/results/TAG-{sbcl,ccl}.tsv
# (extra environment: BENCH_SIZES, BENCH_FILTER, BENCH_SECTIONS, see bench.lisp).  Run it on a quiet machine.
set -u
tag=${1:?usage: run.sh TAG}
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$here/results"
rm -f "$here/results/$tag-sbcl.tsv" "$here/results/$tag-ccl.tsv"
BENCH_OUT="$here/results/$tag-sbcl.tsv" sbcl --dynamic-space-size 8192 --no-userinit --non-interactive --load "$here/bench.lisp" >"$here/results/$tag-sbcl.log" 2>&1
BENCH_OUT="$here/results/$tag-ccl.tsv" ccl --no-init --batch --load "$here/bench.lisp" --eval '(ccl:quit)' </dev/null >"$here/results/$tag-ccl.log" 2>&1
