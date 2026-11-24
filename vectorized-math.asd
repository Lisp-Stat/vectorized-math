;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ASDF -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "vectorized-math"
  :name "Vectorized math"
  :version     "1.0.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Integrated vectorized mathematical operators for Common Lisp"
  :description "Vectorized replacements for Common Lisp numeric operators: exact CL for numbers, elementwise (MKL-backed) for arrays."
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/vectorized-math.git")
  :bug-tracker "https://github.com/Lisp-Stat/vectorized-math/issues"
  :depends-on (#:cffi #+sbcl (:require :sb-cltl2))
  :in-order-to ((test-op (test-op "vectorized-math/tests")))
  :components ((:static-file #:LICENSE)
	       (:module "src"
		:serial t
		:components ((:file "pkgdcl")
			     (:file "repl-variables")
			     (:file "wildcard-hook")
			     (:file "conditions")
			     (:file "ffi")
			     (:file "types")
			     (:file "kernels")
			     (:file "shape")
			     (:file "output")
			     (:file "arith")
			     (:file "elementwise")
			     (:file "transcendental")
			     (:file "compare")
			     (:file "division")
			     (:file "integer")
			     (:file "compiler-macro")))))

;;; Several test files expand into large functions that take SBCL 150-270 MB each to compile.
;;; Compiled one after another without a full collection, the garbage accumulates in the older
;;; generations and SBCL's default 1 GB heap runs out, so collect before compiling each file.
(defclass test-source-file (cl-source-file) ())

(defmethod perform :before ((o compile-op) (c test-source-file))
  #+sbcl (uiop:symbol-call :sb-ext :gc :full t))

(defsystem "vectorized-math/tests"
  :description "Unit tests for vectorized-math."
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on (#:vectorized-math
	       #:clunit2
	       #:bordeaux-threads)
  :pathname #P"tests/"
  :default-component-class test-source-file
  :serial t
  :components ((:file "setup")
	       (:file "scalar")
	       (:file "shapes")
	       (:file "contagion")
	       (:file "into")
	       (:file "backends")
	       (:file "compiler-macro")
	       (:file "compat")
	       (:file "gaps")
	       (:file "ops")
	       (:file "ops-backends")
	       (:file "transcendental")
	       (:file "compare")
	       (:file "division")
	       (:file "integer")
	       (:file "repl-variables")
	       (:file "wildcard")
	       (:file "typenames"))
  :perform (test-op (o s)
	     (symbol-call :vm-tests :run)))
