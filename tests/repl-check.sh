#!/usr/bin/env bash
# Pipes tests/repl-session.txt into a real REPL (stdin, not --load) of the given
# implementation and checks that the REPL variables + ++ +++ work in a package that
# shadowing-imports (vm:shadowed-symbols): + ++ +++ - * ** *** / // /// and
# the operators and the type wildcard in that package; also boundp/symbol-value of the names and
# a global setf of CL's variable in another thread (the Swank and Jupyter case).  Usage: repl-check.sh ccl|sbcl
set -uo pipefail
impl=${1:?usage: repl-check.sh ccl|sbcl}
here=$(cd "$(dirname "$0")" && pwd)
export VM_SYSTEM_DIR="$(cd "$here/.." && pwd)"
case "$impl" in
  ccl)  out=$(ccl --no-init < "$here/repl-session.txt" 2>&1) ;;
  sbcl) out=$(sbcl --noinform --no-userinit < "$here/repl-session.txt" 2>&1) ;;
  *) echo "unknown implementation: $impl" >&2; exit 2 ;;
esac
echo "$out"
echo "----- checks ($impl) -----"
fail=0
check () {
  if grep -qF -- "$1" <<<"$out"; then echo "PASS  $1"; else echo "FAIL  $1"; fail=1; fi
}
check 'CHECK-SHADOWED T'
check 'CHECK-HISTORY T'
check 'CHECK-VALUES T'
check 'CHECK-PREV-FORM T'
check 'CHECK-PREV-VALUE T'
check 'CHECK-CALL T'
check 'CHECK-LET-VARIABLE T'
check 'CHECK-ARRAY T'
check 'CHECK-SETQ T'
check 'CHECK-SLASH T'
check 'CHECK-SLASHES T'
check 'CHECK-MINUS T'
check 'CHECK-STARS-WITH-OPS T'
check 'CHECK-ALL-OPERATORS T'
check 'CHECK-ARRAY-OPERATORS T'
check 'CHECK-INCF-DECF T'
check 'CHECK-DIVISION T'
check 'CHECK-INTEGER T'
check 'CHECK-TYPE-NAMES T'
check 'CHECK-WILDCARD T'
check 'CHECK-SETQ-MINUS T'
check 'CHECK-BOUNDP T'
check 'CHECK-SYMBOL-VALUE T'
check 'CHECK-GLOBAL-SETF-OTHER-THREAD T'
# the path chosen for the REPL variables: aliases on CCL, symbol macros on SBCL
# (or wherever VM_REPL_VARIABLES=symbol-macro forces them)
if [ "$impl" = sbcl ] || [ "${VM_REPL_VARIABLES:-}" = symbol-macro ]; then
  check 'CHECK-MODE :SYMBOL-MACRO'
elif grep -qF 'CHECK-MODE :NATIVE' <<<"$out"; then
  check 'CHECK-MODE :NATIVE'
else
  check 'CHECK-MODE :DOWNSTREAM'
fi
exit $fail
