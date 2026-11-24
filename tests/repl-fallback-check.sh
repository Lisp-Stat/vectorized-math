#!/usr/bin/env bash
# Checks the fallback of src/repl-variables.lisp on CCL: with VM_REPL_VARIABLES=fail-self-test the
# self-test fails, everything installed must be rolled back, one warning is signalled and
# VM:+ VM:- VM:* VM:/ are symbol macros of the CL REPL variables (mode :SYMBOL-MACRO).
# With VM_REPL_VARIABLES=symbol-macro the fallback is chosen without a warning.
# Usage: repl-fallback-check.sh [ccl-command]   (default: ccl; run on a CCL where the mode would be
# :native or :downstream)
set -uo pipefail
ccl=${1:-ccl}
here=$(cd "$(dirname "$0")" && pwd)
sys="$(cd "$here/.." && pwd)"
script=$(mktemp --suffix=.lisp)
trap 'rm -f "$script"' EXIT
cat > "$script" <<LISP
(load "~/quicklisp/setup.lisp")
(push #p"$sys/" asdf:*central-registry*)
(defvar *warnings* nil)
(handler-bind ((warning (lambda (c)
                          (when (search "REPL variables" (princ-to-string c)) (push c *warnings*)))))
  (asdf:load-system :vectorized-math))
(defpackage :fb-user (:use :cl) (:shadowing-import-from :vm . #.(mapcar #'symbol-name (vm:shadowed-symbols))))
(in-package :fb-user)
(format t "CHECK-MODE ~S~%" vm:*repl-variable-mode*)
(format t "CHECK-WARNINGS ~S~%" (length cl-user::*warnings*))
(format t "CHECK-NOT-BOUND ~S~%" (not (boundp 'vm:*)))
(format t "CHECK-NOT-SPECIAL ~S~%" (notany (lambda (s) (logbitp ccl::\$sym_vbit_special (ccl::%symbol-bits s))) (list 'vm:+ 'vm:- 'vm:* 'vm:/)))
(format t "CHECK-NO-FLAG ~S~%" (notany (lambda (s) (logbitp ccl::\$sym_vbit_indirect (ccl::%symbol-bits s))) (list 'vm:+ 'vm:- 'vm:* 'vm:/)))
(format t "CHECK-ORIGINALS ~S~%" (and (null (ignore-errors (symbol-value (find-symbol "*ORIGINAL-DEFINITIONS*" "VM.IMPL"))))
                                      (every (lambda (n) (not (search "VM.IMPL" (princ-to-string (ccl::function-name (fdefinition n))))))
                                             '(ccl::%sym-value ccl::%set-sym-value ccl::%sym-global-value ccl::%set-sym-global-value ccl::%symbol-binding-address cl:makunbound ccl::x862-symbol-value-cell ccl::nx1-note-vcell-ref))))
(format t "CHECK-LET ~S~%" (equal (list (let ((cl:* 5)) *) (funcall (compile nil '(lambda () (let ((cl:* 6)) vm:*))))) '(5 6)))
LISP
out=$("$ccl" --no-init --batch --load "$script" </dev/null 2>&1)
echo "$out" | grep -v '^;' | head -40
fail=0
check () { if grep -qF -- "$1" <<<"$out"; then echo "PASS  $1"; else echo "FAIL  $1"; fail=1; fi; }
echo "----- checks (fallback, $ccl) -----"
check 'CHECK-MODE :SYMBOL-MACRO'
check 'CHECK-NOT-BOUND T'
check 'CHECK-NOT-SPECIAL T'
check 'CHECK-NO-FLAG T'
check 'CHECK-ORIGINALS T'
check 'CHECK-LET T'
# the warning is signalled once by the decision; a compile-file of the system (a cold fasl cache)
# makes CCL's compiler report it a second time
n=$(sed -n 's/^CHECK-WARNINGS \([0-9]*\)$/\1/p' <<<"$out" | head -1)
if [ "${VM_REPL_VARIABLES:-}" = symbol-macro ]; then want=0; else want=1; fi
if { [ "$want" = 0 ] && [ "${n:-x}" = 0 ]; } || { [ "$want" = 1 ] && [ "${n:-0}" -ge 1 ]; }; then echo "PASS  warnings: $n"; else echo "FAIL  warnings: ${n:-none}"; fail=1; fi
exit $fail
