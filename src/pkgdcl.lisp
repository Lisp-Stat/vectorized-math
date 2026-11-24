;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Two packages:
;;;   VM       the exported interface only.  It shadows the CL operators it replaces
;;;            (+ - * / 1+ 1- incf decf exp expt log sqrt isqrt sin cos tan asin acos atan
;;;            sinh cosh tanh asinh acosh atanh cis abs signum conjugate phase realpart imagpart
;;;            complex = /= < > <= >= zerop plusp minusp evenp oddp max min floor ceiling truncate round ffloor
;;;            fceiling ftruncate fround mod rem gcd lcm logand logior logxor logeqv lognand lognor logandc1 logandc2
;;;            logorc1 logorc2 lognot logcount integer-length ash logbitp logtest float rational rationalize numerator
;;;            denominator scale-float float-sign), and contains no code of its own.
;;;   VM.IMPL  all the code.  It uses CL and shadows NOTHING, so inside the
;;;            implementation + - * / ... always mean the CL functions; the vm operators
;;;            are defined (and, when needed, called) by qualified name: (defun vm:+ ...).

(uiop:define-package #:vm
  (:use)
  (:shadow #:+ #:- #:* #:/ #:1+ #:1- #:incf #:decf
           #:exp #:expt #:log #:sqrt #:isqrt #:sin #:cos #:tan #:asin #:acos #:atan #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh #:cis #:abs #:signum #:conjugate #:phase #:realpart #:imagpart #:complex
           #:= #:/= #:< #:> #:<= #:>= #:zerop #:plusp #:minusp #:evenp #:oddp #:max #:min
           #:floor #:ceiling #:truncate #:round #:ffloor #:fceiling #:ftruncate #:fround #:mod #:rem #:gcd #:lcm
           #:logand #:logior #:logxor #:logeqv #:lognand #:lognor #:logandc1 #:logandc2 #:logorc1 #:logorc2
           #:lognot #:logcount #:integer-length #:ash #:logbitp #:logtest
           #:float #:rational #:rationalize #:numerator #:denominator #:scale-float #:float-sign)
  (:export
   ;; operators
   #:+ #:- #:* #:/ #:1+ #:1- #:incf #:decf
   #:exp #:expt #:log #:sqrt #:isqrt #:sin #:cos #:tan #:asin #:acos #:atan #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh #:cis #:abs #:signum #:conjugate #:phase #:realpart #:imagpart #:complex
   #:= #:/= #:< #:> #:<= #:>= #:zerop #:plusp #:minusp #:evenp #:oddp #:max #:min
   #:floor #:ceiling #:truncate #:round #:ffloor #:fceiling #:ftruncate #:fround #:mod #:rem #:gcd #:lcm
           #:logand #:logior #:logxor #:logeqv #:lognand #:lognor #:logandc1 #:logandc2 #:logorc1 #:logorc2
           #:lognot #:logcount #:integer-length #:ash #:logbitp #:logtest
           #:float #:rational #:rationalize #:numerator #:denominator #:scale-float #:float-sign
   #:shadowed-symbols
   ;; configuration
   #:cutoff
   #:find-kernel
   #:register-kernel
   #:*mkl-available*
   #:*mkl-library*
   #:*repl-variable-mode*
   #:load-mkl
   ;; conditions
   #:non-conformable-arrays
   #:recycling-warning
   #:output-type-error)
  (:documentation "Vectorized replacements for Common Lisp numeric operators.

Numbers behave exactly as in CL.  When any operand is an array the operator is
applied elementwise with R-style recycling (column-major emulation for arrays of
rank 2 or more).  Float and complex float arrays use Intel MKL Vector Math when
available, typed Lisp loops otherwise.  A package that shadowing-imports the
symbols returned by VM:SHADOWED-SYMBOLS gets the new behaviour."))

(uiop:define-package #:vm.impl
  (:use #:cl)
  ;; Non-operator exports of VM are defined here, by plain name.
  (:import-from #:vm
                #:shadowed-symbols #:cutoff #:find-kernel #:register-kernel #:*mkl-available*
                #:*mkl-library* #:load-mkl #:*repl-variable-mode*
                #:non-conformable-arrays #:recycling-warning #:output-type-error)
  (:documentation "Implementation of the VM package.  Uses CL, shadows nothing: the vm
operators are defined by qualified name, e.g. (defun vm:+ ...)."))
