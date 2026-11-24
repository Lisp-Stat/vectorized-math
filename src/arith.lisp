;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; VM:+ VM:- VM:* VM:/ VM:1+ VM:1- VM:INCF VM:DECF.
;;; This package does not shadow anything: + here is CL:+, VM:+ is written qualified.
;;;
;;; The four n-ary operators share one engine: fold left pairwise with BINARY-ARRAYS
;;; (shape.lisp), one operand with an arithmetic meaning of its own (identity for + and
;;; *, negation for -, reciprocal for /), and the :INTO output argument.

(in-package #:vm.impl)

(defun variable-info (symbol env)
  "CLtL2 VARIABLE-INFORMATION: (values kind local-p alist)."
  #+ccl (ccl:variable-information symbol env)
  #+sbcl (sb-cltl2:variable-information symbol env)
  #-(or ccl sbcl) (values nil nil nil))

(defun scalar-function (opname)
  (cond ((string= opname "+") #'+)
        ((string= opname "-") #'-)
        ((string= opname "*") #'*)
        ((string= opname "/") #'/)
        (t (error "unknown operation ~S" opname))))

(defun arith-step (opname acc o)
  "One left-fold step: ACC OPNAME O (both operands)."
  (if (or (operand-array-p acc) (operand-array-p o))
      (binary-arrays opname acc o)
      (classify-scalar (funcall (scalar-function opname) (operand-value acc) (operand-value o)))))

(defun unary-array (opname o)
  "(OPNAME x) for an array operand: x for + and *, -x, 1/x."
  (cond ((string= opname "-") (negate-array o))
        ((string= opname "/") (binary-arrays "/" (classify-scalar 1) o))
        (t (copy-operand-array o))))

(defun replay-scalar-error (function ops)
  "Call FUNCTION (a CL function) on OPS with every array that can hold numbers replaced by the
number 2, so that CL signals its own error, for the argument and in the order it does (datum,
expected type), about the operand that is not a number (a string, say).  Returns when CL does not
signal."
  (apply function
         (mapcar (lambda (x)
                   (if (and (arrayp x)
                            (handler-case (progn (classify x) t) (type-error () nil)))
                       2
                       x))
                 ops)))

(defmacro with-cl-error-replay ((function ops) &body body)
  "BODY, except that a type error about an array that cannot hold numbers is replaced by the error
the CL FUNCTION signals for OPS (see REPLAY-SCALAR-ERROR): the offending operand is the datum,
whatever numeric arrays precede it."
  `(handler-case (progn ,@body)
     (type-error (c)
       (when (arrayp (type-error-datum c)) (replay-scalar-error ,function ,ops))
       (error c))))

(defun arith-arrays (opname args)
  "OPNAME of ARGS, at least one an array.  Returns a number or a fresh simple array."
  (let* ((ops (mapcar #'classify args))     ; signals type-errors in argument order
         (acc (first ops)))
    (if (null (rest ops))
        (setf acc (unary-array opname acc))
        (dolist (o (rest ops)) (setf acc (arith-step opname acc o))))
    (operand-value acc)))

(defun into-position (args)
  "If ARGS ends in :INTO OUT preceded by at least one operand, the number of operands."
  (let ((n (length args)))
    (and (>= n 3) (eq (nth (- n 2) args) :into) (- n 2))))

(defun arith-operator (opname scalar-function args)
  "The body of the n-ary operators.  SCALAR-FUNCTION is the CL function for OPNAME."
  ;; Scalar fast path: no array anywhere (an :INTO destination is an array, so it takes the slow path).
  ;; With no arguments this signals CL's own error for (-) and (/).
  (if (notany #'arrayp args)
      (apply scalar-function args)
      (let ((nops (into-position args)))
        (if nops
            (let* ((out (car (last args)))
                   (ops (subseq args 0 nops)))
              (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
              (store-into (if (some #'arrayp ops)
                              (with-cl-error-replay (scalar-function ops) (arith-arrays opname ops))
                              (apply scalar-function ops))
                          out))
            (with-cl-error-replay (scalar-function args) (arith-arrays opname args))))))

(defmacro define-arith-operator (name opname scalar-function doc)
  `(defun ,name (&rest args)
     ,doc
     (declare (dynamic-extent args))
     (arith-operator ,opname #',scalar-function args)))

(define-arith-operator vm:+ "+" +
  "Sum like + for numbers; elementwise with R recycling when any argument is an array.
The last two arguments may be :INTO OUT, in which case the sum is stored in OUT (recycled or
truncated to its size, with a VM:RECYCLING-WARNING) and OUT is returned.")

(define-arith-operator vm:- "-" -
  "Difference like - for numbers; elementwise with R recycling when any argument is an array.
(- array) negates, (- a b c ...) folds from the left (a-b)-c.  :INTO OUT as for VM:+.")

(define-arith-operator vm:* "*" *
  "Product like * for numbers; elementwise with R recycling when any argument is an array.
:INTO OUT as for VM:+.")

(define-arith-operator vm:/ "/" /
  "Quotient like / for numbers; elementwise with R recycling when any argument is an array.
(/ array) is the reciprocal.  Integer and rational operands give exact rationals in an array
of element type T; float arrays give IEEE Inf and NaN (no signal), integer division by zero
signals DIVISION-BY-ZERO as in CL.  :INTO OUT as for VM:+.")

(defun vm:1+ (x)
  "(1+ x) for numbers; adds 1 to every element of an array."
  (if (arrayp x)
      (locally (declare (notinline vm:+)) (vm:+ x 1))   ; the compiler macro is defined later
      (1+ x)))

(defun vm:1- (x)
  "(1- x) for numbers; subtracts 1 from every element of an array."
  (if (arrayp x)
      (locally (declare (notinline vm:-)) (vm:- x 1))
      (1- x)))

;;; --- INCF and DECF ---
;;; CL's semantics: the subforms of the place are evaluated once, left to right, then
;;; DELTA, then the old value is read (the order CCL's and SBCL's INCF use; the host
;;; DEFINE-MODIFY-MACROs differ on this, which is why these are written out), then the
;;; new value is stored.  The sum is VM:+ / VM:- so that arrays can be incremented.

(defun modify-macro-expansion (function place delta env)
  (multiple-value-bind (temps vals stores setter getter) (get-setf-expansion place env)
    (let* ((plain (or (numberp delta)
                      (and (symbolp delta) (not (null delta)) (not (keywordp delta))
                           (member (variable-info delta env) '(:lexical :special)))))
           (dv (if plain delta (gensym "DELTA"))))
      `(let* (,@(mapcar #'list temps vals)
              ,@(unless plain `((,dv ,delta)))
              (,(first stores) (,function ,getter ,dv))
              ,@(mapcar (lambda (s) `(,s nil)) (rest stores)))
         ,setter))))

(defmacro vm:incf (place &optional (delta 1) &environment env)
  "Like INCF, but adds with VM:+ so that arrays (and array elements) can be incremented elementwise."
  (modify-macro-expansion 'vm:+ place delta env))

(defmacro vm:decf (place &optional (delta 1) &environment env)
  "Like DECF, but subtracts with VM:-."
  (modify-macro-expansion 'vm:- place delta env))

(defun shadowed-symbols ()
  "The list of VM operator symbols a user package should shadowing-import."
  (list 'vm:+ 'vm:- 'vm:* 'vm:/ 'vm:1+ 'vm:1- 'vm:incf 'vm:decf
        'vm:exp 'vm:expt 'vm:log 'vm:sqrt 'vm:isqrt 'vm:sin 'vm:cos 'vm:tan 'vm:asin 'vm:acos
        'vm:atan 'vm:sinh 'vm:cosh 'vm:tanh 'vm:asinh 'vm:acosh 'vm:atanh 'vm:cis 'vm:abs
        'vm:signum 'vm:conjugate 'vm:phase 'vm:realpart 'vm:imagpart 'vm:complex
        'vm:= 'vm:/= 'vm:< 'vm:> 'vm:<= 'vm:>= 'vm:zerop 'vm:plusp 'vm:minusp 'vm:evenp 'vm:oddp
        'vm:max 'vm:min
        'vm:floor 'vm:ceiling 'vm:truncate 'vm:round 'vm:ffloor 'vm:fceiling 'vm:ftruncate 'vm:fround
        'vm:mod 'vm:rem 'vm:gcd 'vm:lcm
        'vm:logand 'vm:logior 'vm:logxor 'vm:logeqv 'vm:lognand 'vm:lognor 'vm:logandc1 'vm:logandc2
        'vm:logorc1 'vm:logorc2 'vm:lognot 'vm:logcount 'vm:integer-length 'vm:ash 'vm:logbitp 'vm:logtest
        'vm:float 'vm:rational 'vm:rationalize 'vm:numerator 'vm:denominator 'vm:scale-float 'vm:float-sign))

;;; --- REPL variables ---
;;; + ++ +++ is the REPL's history of forms (likewise - * / ...).  A package that shadows
;;; them would lose it, so each is also a symbol macro for the CL variable (a different
;;; namespace from the function).  (spike S1: .agent-work/spikes/vm-shadowing/REPORT.md)
(define-symbol-macro vm:+ cl:+)
(define-symbol-macro vm:- cl:-)
(define-symbol-macro vm:* cl:*)
(define-symbol-macro vm:/ cl:/)

;;; --- method combination ---
;;; (defgeneric f (x) (:method-combination +)) in a package shadowing VM:+
(define-method-combination vm:+ :identity-with-one-argument t)
