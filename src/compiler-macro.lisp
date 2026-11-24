;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Compiler macros for VM:+ VM:- VM:* VM:/ VM:1+ VM:1- so that shadowing does not slow
;;; down scalar code.  One generator (DEFINE-OPERATOR-COMPILER-MACRO), per-operator arity rules.
;;;   * every argument known to be a number at compile time -> (cl:op ...)
;;;   * otherwise an inline NUMBERP test on the arguments not known to be numbers,
;;;     CL:OP when they all are, the array path (the operator's -GENERAL function) otherwise.
;;; Arguments are evaluated once, left to right.  Wrong argument counts are declined (the
;;; form is returned unchanged), so the runtime error is the one the function gives, which
;;; is the one CL gives: (vm:-) and (vm:/) are not rewritten.

(in-package #:vm.impl)

(defun known-number-type-p (type)
  (and type
       (handler-case (values (subtypep type 'number))
         (error () nil))))

(defun declared-variable-type (symbol env)
  "The declared type of variable SYMBOL in ENV, or NIL when none or the symbol is a symbol macro."
  (multiple-value-bind (kind local alist) (variable-info symbol env)
    (declare (ignore local))
    (and (member kind '(:lexical :special))
         (cdr (assoc 'type alist)))))

(defun known-number-form-p (form env)
  "True when FORM certainly evaluates to a number, judging from the form alone."
  (typecase form
    (number t)
    (null nil)
    (keyword nil)
    (symbol
     (or (and (constantp form env) (boundp form) (numberp (symbol-value form)))
         (known-number-type-p (declared-variable-type form env))))
    (cons
     (case (car form)
       (the (and (consp (cdr form)) (known-number-type-p (cadr form))))
       ((vm:+ vm:*) (every (lambda (f) (known-number-form-p f env)) (cdr form)))
       ((vm:- vm:/) (and (cdr form) (every (lambda (f) (known-number-form-p f env)) (cdr form))))
       ((vm:1+ vm:1-) (and (cdr form) (null (cddr form)) (known-number-form-p (cadr form) env)))
       ;; the operators of transcendental.lisp return a number for number arguments
       ((vm:exp vm:sqrt vm:sin vm:cos vm:tan vm:asin vm:acos vm:sinh vm:cosh vm:tanh vm:asinh
         vm:acosh vm:atanh vm:cis vm:abs vm:signum vm:conjugate vm:phase vm:realpart vm:imagpart
         vm:isqrt vm:expt vm:log vm:atan vm:complex vm:max vm:min
         ;; division.lisp: the first value is a number
         vm:floor vm:ceiling vm:truncate vm:round vm:ffloor vm:fceiling vm:ftruncate vm:fround
         vm:mod vm:rem vm:gcd vm:lcm
         ;; integer.lisp: a number for number arguments
         vm:logand vm:logior vm:logxor vm:logeqv vm:lognand vm:lognor vm:logandc1 vm:logandc2
         vm:logorc1 vm:logorc2 vm:lognot vm:logcount vm:integer-length vm:ash
         vm:float vm:rational vm:rationalize vm:numerator vm:denominator vm:scale-float vm:float-sign)
        (and (cdr form) (every (lambda (f) (known-number-form-p f env)) (cdr form))))
       (t nil)))
    (t nil)))

(defun simple-form-p (form env)
  "True when evaluating FORM has no side effects and its value cannot be changed by
evaluating another argument form (constants, plain variables)."
  (typecase form
    (symbol (or (null form) (keywordp form)
                (multiple-value-bind (kind) (variable-info form env)
                  (member kind '(:lexical :special :constant)))))
    (cons (eq (car form) 'quote))
    (t t)))

(defun number-test (var)
  "Inline test that the value of variable VAR is a number.  CCL compiles NUMBERP as a
full call, so test the common representations inline first."
  #+ccl `(or (typep ,var 'fixnum) (typep ,var 'double-float) (numberp ,var))
  #-ccl `(numberp ,var))

(defun general-call (function args)
  (declare (dynamic-extent args))
  (apply function args))

(defun expand-operator-call (cl-op general form args env)
  "The expansion of an operator call whose arguments ARGS are all evaluated, once, left to right."
  (declare (ignore form))
  (if (every (lambda (a) (known-number-form-p a env)) args)
      `(,cl-op ,@args)
      (let* ((names '()) (bindings '()) (tests '()))
        (loop for a in args for i from 1
              for later = (nthcdr i args)
              do (let* ((known (known-number-form-p a env))
                        (direct (and (symbolp a) (not (null a)) (not (keywordp a))
                                     (every (lambda (l) (simple-form-p l env)) later)
                                     (simple-form-p a env)))
                        (const (or (numberp a) (and (atom a) (not (symbolp a))))))
                   (cond ((or direct const)
                          (push a names))
                         (t (let ((g (gensym "A")))
                              (push g names)
                              (push (list g a) bindings))))
                   (unless known (push (number-test (first names)) tests))))
        (setf names (nreverse names) bindings (nreverse bindings) tests (nreverse tests))
        (let ((body `(if (and ,@tests) (,cl-op ,@names) (,general ,@names))))
          ;; SBCL: when a compile-time array type is known for an argument the numeric
          ;; arm is dead code, but its result type may still clash with the context and
          ;; produce a spurious style-warning.  Only our own variables are inside.
          #+sbcl (setf body `(locally (declare (sb-ext:muffle-conditions style-warning)) ,body))
          (if bindings `(let* ,bindings ,body) body)))))

(defmacro define-operator-compiler-macro (name cl-op general &key (min-args 1) max-args zero-form)
  "Define the function GENERAL (the array path: NAME without the compiler macro) and the
compiler macro of NAME.  A call with no arguments expands to ZERO-FORM when given, and a
call with fewer than MIN-ARGS or more than MAX-ARGS arguments is left alone."
  `(progn
     (defun ,general (&rest args)
       (declare (dynamic-extent args))
       (general-call #',name args))
     (define-compiler-macro ,name (&whole form &rest args &environment env)
       (cond
         ;; (funcall #'op ...): not handled
         ((not (eq (car form) ',name)) form)
         ;; :INTO destination: always the array path
         ((member :into args) form)
         ((null args) ,(if zero-form zero-form 'form))
         ((< (length args) ,min-args) form)
         ,@(when max-args `(((> (length args) ,max-args) form)))
         (t (expand-operator-call ',cl-op ',general form args env))))))

(define-operator-compiler-macro vm:+ + plus-general :zero-form 0)
(define-operator-compiler-macro vm:* * times-general :zero-form 1)
(define-operator-compiler-macro vm:- - minus-general)
(define-operator-compiler-macro vm:/ / divide-general)
(define-operator-compiler-macro vm:1+ 1+ one-plus-general :min-args 1 :max-args 1)
(define-operator-compiler-macro vm:1- 1- one-minus-general :min-args 1 :max-args 1)

;;; exp ... complex (transcendental.lisp)
(macrolet ((def (&rest specs)
             `(progn
                ,@(loop for (name cl-op general min max) in specs
                        collect `(define-operator-compiler-macro ,name ,cl-op ,general
                                   :min-args ,min :max-args ,max)))))
  (def (vm:exp exp exp-general 1 1) (vm:sin sin sin-general 1 1) (vm:cos cos cos-general 1 1)
       (vm:tan tan tan-general 1 1) (vm:sinh sinh sinh-general 1 1) (vm:cosh cosh cosh-general 1 1)
       (vm:tanh tanh tanh-general 1 1) (vm:asinh asinh asinh-general 1 1)
       (vm:acosh acosh acosh-general 1 1) (vm:atanh atanh atanh-general 1 1)
       (vm:asin asin asin-general 1 1) (vm:acos acos acos-general 1 1)
       (vm:sqrt sqrt sqrt-general 1 1) (vm:isqrt isqrt isqrt-general 1 1)
       (vm:cis cis cis-general 1 1) (vm:abs abs abs-general 1 1)
       (vm:signum signum signum-general 1 1) (vm:conjugate conjugate conjugate-general 1 1)
       (vm:phase phase phase-general 1 1) (vm:realpart realpart realpart-general 1 1)
       (vm:imagpart imagpart imagpart-general 1 1)
       (vm:atan atan atan-general 1 2) (vm:log log log-general 1 2)
       (vm:complex complex complex-general 1 2) (vm:expt expt expt-general 2 2)))

;;; comparisons, predicates, max and min (compare.lisp)
(macrolet ((def (&rest specs)
             `(progn
                ,@(loop for (name cl-op general min max) in specs
                        collect `(define-operator-compiler-macro ,name ,cl-op ,general
                                   :min-args ,min :max-args ,max)))))
  (def (vm:= = num-eq-general 1 nil) (vm:/= /= num-ne-general 1 nil)
       (vm:< < lt-general 1 nil) (vm:> > gt-general 1 nil)
       (vm:<= <= le-general 1 nil) (vm:>= >= ge-general 1 nil)
       (vm:max max max-general 1 nil) (vm:min min min-general 1 nil)
       (vm:zerop zerop zerop-general 1 1) (vm:plusp plusp plusp-general 1 1)
       (vm:minusp minusp minusp-general 1 1) (vm:evenp evenp evenp-general 1 1)
       (vm:oddp oddp oddp-general 1 1)))

;;; division and rounding, gcd and lcm (division.lisp).  The two-valued operators expand to the CL
;;; call, so both values pass through.
(macrolet ((def (&rest specs)
             `(progn
                ,@(loop for (name cl-op general min max) in specs
                        collect `(define-operator-compiler-macro ,name ,cl-op ,general
                                   :min-args ,min :max-args ,max)))))
  (def (vm:floor floor floor-general 1 2) (vm:ceiling ceiling ceiling-general 1 2)
       (vm:truncate truncate truncate-general 1 2) (vm:round round round-general 1 2)
       (vm:ffloor ffloor ffloor-general 1 2) (vm:fceiling fceiling fceiling-general 1 2)
       (vm:ftruncate ftruncate ftruncate-general 1 2) (vm:fround fround fround-general 1 2)
       (vm:mod mod mod-general 2 2) (vm:rem rem rem-general 2 2)))
(define-operator-compiler-macro vm:gcd gcd gcd-general :min-args 1 :zero-form 0)
(define-operator-compiler-macro vm:lcm lcm lcm-general :min-args 1 :zero-form 1)

;;; integer, bitwise and conversion operators (integer.lisp).  The zero-argument forms of the n-ary
;;; bitwise operators are their identities.
(define-operator-compiler-macro vm:logand logand logand-general :min-args 1 :zero-form -1)
(define-operator-compiler-macro vm:logeqv logeqv logeqv-general :min-args 1 :zero-form -1)
(define-operator-compiler-macro vm:logior logior logior-general :min-args 1 :zero-form 0)
(define-operator-compiler-macro vm:logxor logxor logxor-general :min-args 1 :zero-form 0)
(macrolet ((def (&rest specs)
             `(progn
                ,@(loop for (name cl-op general min max) in specs
                        collect `(define-operator-compiler-macro ,name ,cl-op ,general
                                   :min-args ,min :max-args ,max)))))
  (def (vm:lognand lognand lognand-general 2 2) (vm:lognor lognor lognor-general 2 2)
       (vm:logandc1 logandc1 logandc1-general 2 2) (vm:logandc2 logandc2 logandc2-general 2 2)
       (vm:logorc1 logorc1 logorc1-general 2 2) (vm:logorc2 logorc2 logorc2-general 2 2)
       (vm:lognot lognot lognot-general 1 1) (vm:logcount logcount logcount-general 1 1)
       (vm:integer-length integer-length integer-length-general 1 1) (vm:ash ash ash-general 2 2)
       (vm:logbitp logbitp logbitp-general 2 2) (vm:logtest logtest logtest-general 2 2)
       (vm:float float float-general 1 2) (vm:rational rational rational-general 1 1)
       (vm:rationalize rationalize rationalize-general 1 1) (vm:numerator numerator numerator-general 1 1)
       (vm:denominator denominator denominator-general 1 1)
       (vm:scale-float scale-float scale-float-general 2 2) (vm:float-sign float-sign float-sign-general 1 2)))
