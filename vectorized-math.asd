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

(defsystem "vectorized-math/tests"
  :description "Unit tests for vectorized-math."
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on (#:vectorized-math
	       #:clunit2
	       #:bordeaux-threads)
  :pathname #P"tests/"
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
	       (:file "wildcard")
	       (:file "typenames"))
  :perform (test-op (o s)
	     (symbol-call :vm-tests :run)))
