#!/usr/bin/env bash
# Demonstrates that MKL can be moved anywhere: copies the minimal set of MKL shared objects
# to a scratch directory, starts a fresh image with VM_MKL_LIBRARY pointing nowhere and
# LD_LIBRARY_PATH unset, calls (vm:load-mkl <relocated libmkl_rt>) and uses LD_DEBUG=libs
# to check that every MKL/OpenMP library the process loaded came from the scratch directory.
# Usage: relocate-mkl.sh [SCRATCH-DIR] [MKL-LIB-DIR] [IOMP5-DIR]
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
dest=$(realpath -m "${1:-${TMPDIR:-/tmp}/mkl-relocated}")
mklib=${2:-/opt/intel/oneapi/mkl/latest/lib}
iomp=${3:-/opt/intel/oneapi/compiler/latest/lib}
# Minimal set for the default threading layer (Intel OpenMP) on a CPU with AVX2.  On other CPUs
# replace libmkl_avx2/libmkl_vml_avx2 by the matching pair (avx512, avx10, mc3, def, ...); it is
# simplest to copy every libmkl_*avx*, libmkl_*mc* and libmkl_*def* file for portability.
files="libmkl_rt.so.3 libmkl_core.so.3 libmkl_intel_lp64.so.3 libmkl_intel_thread.so.3 libmkl_avx2.so.3 libmkl_vml_avx2.so.3"
mkdir -p "$dest"
for f in $files; do cp -L "$mklib/$f" "$dest/"; done
cp -L "$iomp/libiomp5.so" "$dest/"
echo "copied to $dest:"; ls -l "$dest"
du -cb "$dest"/* | tail -1 | awk '{printf "total %d bytes (%.0f MB)\n", $1, $1/1048576}'
export VM_SYSTEM_DIR="$(cd "$here/.." && pwd)"
export RELOCATED_MKL="$dest/libmkl_rt.so.3"
export VM_MKL_LIBRARY=/nonexistent/libmkl_rt.so
unset LD_LIBRARY_PATH
rc=0
for impl in sbcl ccl; do
  echo "===== $impl"
  case $impl in
    sbcl) out=$(LD_DEBUG=libs sbcl --no-userinit --non-interactive --load "$here/relocate-mkl.lisp" 2>&1) ;;
    ccl)  out=$(LD_DEBUG=libs ccl --no-init --batch --load "$here/relocate-mkl.lisp" --eval '(ccl:quit)' </dev/null 2>&1) ;;
  esac
  grep -E "^RESULT" <<<"$out"
  # every MKL/OpenMP library initialised must come from $dest
  loaded=$(grep -E "calling init: .*(libmkl|libiomp)" <<<"$out" | sed 's/.*calling init: //' | sort -u)
  echo "$loaded"
  if grep -qv "^$dest/" <<<"$loaded"; then echo "FAIL: library loaded from outside $dest"; rc=1; else echo "PASS: all MKL libraries came from $dest"; fi
done
exit $rc
