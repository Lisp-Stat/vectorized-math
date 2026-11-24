;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; exp expt log sqrt isqrt sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh cis
;;; abs signum conjugate phase realpart imagpart complex (src/transcendental.lisp).
;;; In this package exp, sqrt, ... are the CL functions: the VM operators are written vm:op.
(in-package #:vm-tests)

(defsuite transcendental-tests (vm-tests))

;;; ---------------------------------------------------------------------------
;;; the operators

(defparameter *real-unary-ops*
  ;; (name vm-fn cl-fn sampler-low sampler-high)  the sampler range is inside the real domain
  (list (list "exp" #'vm:exp #'cl:exp -20d0 20d0)
        (list "log" #'vm:log #'cl:log 1d-5 1d5)
        (list "sqrt" #'vm:sqrt #'cl:sqrt 0d0 1d6)
        (list "sin" #'vm:sin #'cl:sin -100d0 100d0)
        (list "cos" #'vm:cos #'cl:cos -100d0 100d0)
        (list "tan" #'vm:tan #'cl:tan -1.5d0 1.5d0)
        (list "asin" #'vm:asin #'cl:asin -0.999d0 0.999d0)
        (list "acos" #'vm:acos #'cl:acos -0.999d0 0.999d0)
        (list "atan" #'vm:atan #'cl:atan -1000d0 1000d0)
        (list "sinh" #'vm:sinh #'cl:sinh -20d0 20d0)
        (list "cosh" #'vm:cosh #'cl:cosh -20d0 20d0)
        (list "tanh" #'vm:tanh #'cl:tanh -20d0 20d0)
        (list "asinh" #'vm:asinh #'cl:asinh -1000d0 1000d0)
        (list "acosh" #'vm:acosh #'cl:acosh 1.0001d0 1d5)
        (list "atanh" #'vm:atanh #'cl:atanh -0.999d0 0.999d0)))

(defparameter *domain-op-names* '("log" "sqrt" "asin" "acos" "acosh" "atanh"))

(defun uop-name (op) (first op))
(defun uop-vm (op) (second op))
(defun uop-cl (op) (third op))
(defun uop (name) (find name *real-unary-ops* :key #'first :test #'string=))

;;; deterministic pseudo-random numbers (xorshift)
(defvar *rs* 88172645463325252)
(defun rnd-reset (&optional (seed 88172645463325252)) (setf *rs* seed))
(defun rnd-next ()
  (let ((x *rs*))
    (setf x (logand (logxor x (ash x 13)) #xFFFFFFFFFFFFFFFF))
    (setf x (logxor x (ash x -7)))
    (setf x (logand (logxor x (ash x 17)) #xFFFFFFFFFFFFFFFF))
    (setf *rs* x)))
(defun rnd-unit () (/ (float (ldb (byte 52 8) (rnd-next)) 1d0) (float (expt 2 52) 1d0)))
(defun rnd-range (lo hi) (+ lo (* (- hi lo) (rnd-unit))))

(defun float-array-of (type list)
  (make-arr (ecase type (:single 'single-float) (:double 'double-float)
              (:csingle '(complex single-float)) (:cdouble '(complex double-float)))
            (length list) list))

(defun convert-sample (type x)
  (ecase type
    (:double x)
    (:single (float x 1f0))
    (:cdouble (complex x (rnd-range -3d0 3d0)))
    (:csingle (complex (float x 1f0) (float (rnd-range -3d0 3d0) 1f0)))))

(defun convert-real (type x)
  (if (eq type :single) (float x 1f0) x))

(defun random-real-array (type n lo hi)
  (float-array-of type (loop repeat n collect (convert-sample type (rnd-range lo hi)))))

;;; --- ulp measurement ---

(defun float-eps-of (x)
  (typecase x
    (single-float single-float-epsilon) (double-float double-float-epsilon)
    (complex (float-eps-of (realpart x))) (t 1d-16)))

(defun ulp-diff (a b)
  "Distance between A (computed) and B (reference) in units of the last place of B (reals), or in
units of EPSILON * |B| for complex numbers.  NaN matches NaN; Inf matches the same Inf."
  (cond ((and (complexp a) (complexp b))
         (let ((za (nanp a)) (zb (nanp b)))
           (cond ((and za zb) 0)
                 ((or za zb) 1d300)
                 ((or (vm.impl::with-masked-float-traps (or (> (abs (realpart b)) 1d300)
                                                          (> (abs (imagpart b)) 1d300)
                                                          (> (abs (realpart a)) 1d300)
                                                          (> (abs (imagpart a)) 1d300)))
                      (not (and (finite-p (realpart a)) (finite-p (imagpart a))
                                (finite-p (realpart b)) (finite-p (imagpart b)))))
                  (if (and (same-bits-p (realpart a) (realpart b)) (same-bits-p (imagpart a) (imagpart b)))
                      0 1d300))
                 (t (let ((d (abs (- (complex (float (realpart a) 1d0) (float (imagpart a) 1d0))
                                     (complex (float (realpart b) 1d0) (float (imagpart b) 1d0)))))
                          (m (abs (complex (float (realpart b) 1d0) (float (imagpart b) 1d0)))))
                      (if (zerop d) 0
                          (/ d (cl:max m least-positive-normalized-double-float) (float-eps-of b))))))))
        ((or (complexp a) (complexp b)) 1d300)
        ((and (nanp a) (nanp b)) 0)
        ((or (nanp a) (nanp b)) 1d300)
        ((= a b) 0)
        ((or (not (finite-p a)) (not (finite-p b))) 1d300)
        (t (let* ((ra (rational a)) (rb (rational b))
                  (e (if (zerop b) -1074 (nth-value 1 (integer-decode-float b))))
                  (ulp (expt 2 (if (typep b 'single-float) (cl:max e -149) (cl:max e -1074)))))
             (float (/ (abs (- ra rb)) ulp) 1d0)))))

(defun finite-p (x)
  (and (not (nanp x))
       (vm.impl::with-masked-float-traps
         (< (abs x) (if (typep x 'single-float) most-positive-single-float most-positive-double-float)))))

(defvar *ulp-table* (make-hash-table :test 'equal))
(defun note-ulp (op type max)
  (let ((old (gethash (list op type) *ulp-table* -1)))
    (when (> max old) (setf (gethash (list op type) *ulp-table*) max))))

(defun max-ulp (vm-array cl-values)
  (let ((m 0))
    (loop for i from 0 for c in cl-values
          do (setf m (cl:max m (ulp-diff (row-major-aref vm-array i) c))))
    m))

(defun cl-elementwise (cl-fn array)
  "CL-FN applied to every element of ARRAY with traps masked: the reference."
  (loop for x in (contents array) collect (masked-cl cl-fn x)))

(defun result-et (a) (array-element-type a))

;;; ---------------------------------------------------------------------------
;;; scalars

(defparameter *tc-corpus*
  (append *ops-corpus*
          (list 2 3 4 9 100 -9 0.5d0 -0.5d0 1d0 -1d0 1f0 -1f0 100d0 -100d0 0.25f0 -2.5f0 10d0 1d-10
                #c(0 1) #c(0d0 1d0) #c(-1 0) #c(3 4) #c(3d0 4d0) #c(0.5f0 -0.5f0) 1/4 -1/4 8 27)))

(defparameter *all-ops*
  (append *real-unary-ops*
          (list (list "isqrt" #'vm:isqrt #'cl:isqrt) (list "cis" #'vm:cis #'cl:cis)
                (list "abs" #'vm:abs #'cl:abs) (list "signum" #'vm:signum #'cl:signum)
                (list "conjugate" #'vm:conjugate #'cl:conjugate) (list "phase" #'vm:phase #'cl:phase)
                (list "realpart" #'vm:realpart #'cl:realpart) (list "imagpart" #'vm:imagpart #'cl:imagpart))))

(deftest tc-scalar-one-arg-identity (transcendental-tests)
  "Every unary operator on every corpus element: same value, type or condition as CL."
  (dolist (op *all-ops*)
    (let ((bad '()))
      (dolist (x (append *tc-corpus* (list 'a "s" nil :k '(1 2))))
        (multiple-value-bind (v c) (call-pair (uop-vm op) (uop-cl op) (list x))
          (unless (same-result-p v c) (push (list (uop-name op) x v c) bad))))
      (assert-equal nil bad))))

(defparameter *binary-tc-ops*
  (list (list "expt" #'vm:expt #'cl:expt) (list "log" #'vm:log #'cl:log)
        (list "atan" #'vm:atan #'cl:atan) (list "complex" #'vm:complex #'cl:complex)))

(defun huge-integer-p (x) (and (integerp x) (> (abs x) 1000)))

(deftest tc-scalar-two-arg-identity (transcendental-tests)
  "expt, two-argument log and atan, and complex over every pair of the corpus (EXPT of huge
integer powers is skipped: CL would compute for ever)."
  (dolist (op *binary-tc-ops*)
    (let ((bad '()))
      (dolist (x *tc-corpus*)
        (dolist (y *tc-corpus*)
          (unless (and (string= (uop-name op) "expt") (huge-integer-p y))
            (multiple-value-bind (v c) (call-pair (uop-vm op) (uop-cl op) (list x y))
              (unless (same-result-p v c) (push (list (uop-name op) x y v c) bad))))))
      (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 5 (length bad)))))
      (assert-equal nil bad))))

(deftest tc-scalar-arities (transcendental-tests)
  "No arguments, too many arguments, optional arguments: the very condition CL signals."
  (dolist (op (append *all-ops* *binary-tc-ops*))
    (dolist (args (list '() '(1 2 3) '(1 2 3 4) '(2d0 3d0 4d0)))
      (multiple-value-bind (v c) (call-pair (uop-vm op) (uop-cl op) args)
        (assert-true (same-result-p v c))))
    (dolist (args (list '(2) '(2 3) '(2d0 3d0) '(8 2) '(1/2 2)))
      (multiple-value-bind (v c) (call-pair (uop-vm op) (uop-cl op) args)
        (assert-true (same-result-p v c))))))

(deftest tc-scalar-non-numbers (transcendental-tests)
  (dolist (op (append *all-ops* *binary-tc-ops*))
    (dolist (args (list '(a) '(1 a) '(a 1) '(nil) '((1 2)) '(1.5 :foo)))
      (multiple-value-bind (v c) (call-pair (uop-vm op) (uop-cl op) args)
        (assert-true (same-result-p v c))))))

;;; ---------------------------------------------------------------------------
;;; result element types for each input element type

(defparameter *int-array-types*
  '((unsigned-byte 8) (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64) fixnum (unsigned-byte 64)))

(deftest tc-result-types-float-arrays (transcendental-tests)
  "Float arrays keep their float format; complex float arrays stay complex of the format."
  (dolist (op *real-unary-ops*)
    (rnd-reset)
    (destructuring-bind (name vm-fn cl-fn lo hi) op
      (declare (ignore cl-fn))
      (dolist (type '(:single :double))
        (let ((r (funcall vm-fn (random-real-array type 5 lo hi))))
          (assert-equal (if (eq type :single) 'single-float 'double-float) (result-et r) name)))
      (dolist (type '(:csingle :cdouble))
        (let ((r (funcall vm-fn (random-real-array type 5 (cl:max lo -0.9d0) (cl:min hi 0.9d0)))))
          (assert-equal (if (eq type :csingle) '(complex single-float) '(complex double-float)) (result-et r)
                        name)))))
  ;; complex-part operators
  (dolist (type '(:single :double))
    (let* ((et (if (eq type :single) 'single-float 'double-float))
           (cet `(complex ,et))
           (x (random-real-array type 4 -2d0 2d0))
           (z (random-real-array (if (eq type :single) :csingle :cdouble) 4 -2d0 2d0)))
      (assert-equal et (result-et (vm:abs x)))
      (assert-equal et (result-et (vm:abs z)))
      (assert-equal et (result-et (vm:phase x)))
      (assert-equal et (result-et (vm:phase z)))
      (assert-equal et (result-et (vm:signum x)))
      (assert-equal cet (result-et (vm:signum z)))
      (assert-equal et (result-et (vm:realpart x)))
      (assert-equal et (result-et (vm:realpart z)))
      (assert-equal et (result-et (vm:imagpart x)))
      (assert-equal et (result-et (vm:imagpart z)))
      (assert-equal et (result-et (vm:conjugate x)))
      (assert-equal cet (result-et (vm:conjugate z)))
      (assert-equal cet (result-et (vm:cis x)))
      (assert-equal cet (result-et (vm:complex x)))
      (assert-equal cet (result-et (vm:complex x x)))
      (assert-equal et (result-et (vm:atan x x)))
      (assert-equal et (result-et (vm:expt (vm:abs x) (vm:abs x))))
      (assert-equal et (result-et (vm:expt x 3)))
      (assert-equal et (result-et (vm:log (vm:abs x) (vm:abs x)))))))

(defun expected-et-of-values (values)
  "The element type the generic path gives to results VALUES (CL's own per-element results)."
  (cond ((every (lambda (v) (typep v 'single-float)) values) 'single-float)
        ((every (lambda (v) (typep v '(or single-float (complex single-float)))) values)
         '(complex single-float))
        (t t)))

(deftest tc-result-types-integer-arrays (transcendental-tests)
  "Irrational functions of integers: the arrays hold what CL returns per element (single-floats;
complex single-floats when any element is complex; CCL's SQRT of a perfect square is exact, so
T); abs signum conjugate realpart integer arrays; imagpart zeros."
  (dolist (et *int-array-types*)
    (let ((a (make-arr et 4 '(1 2 3 4))))
      (dolist (op (remove "atanh" *real-unary-ops* :key #'first :test #'string=)) ; (atanh 1): CL's integer error
        (let* ((r (funcall (uop-vm op) a))
               (refs (cl-elementwise (uop-cl op) a)))
          (assert-equal (expected-et-of-values refs) (result-et r) (uop-name op))
          (dotimes (i 4)
            (assert-true (= (row-major-aref r i) (nth i refs)) (uop-name op) i))))
      (dolist (name '("abs" "signum" "conjugate" "realpart" "imagpart"))
        (let ((r (funcall (uop-vm (find name *all-ops* :key #'first :test #'string=)) a)))
          (assert-equal '(signed-byte 64) (result-et r) name)))
      (assert-equal '(0 0 0 0) (contents (vm:imagpart a)))
      (assert-equal '(1 2 3 4) (contents (vm:realpart a)))
      (assert-equal '(1 2 3 4) (contents (vm:conjugate a)))
      (assert-equal '(1 1 1 1) (contents (vm:signum a)))
      (assert-equal 'single-float (result-et (vm:phase a)))
      (assert-equal '(complex single-float) (result-et (vm:cis a)))
      (assert-equal '(2 2 2 3) (contents (vm:isqrt (make-arr et 4 '(4 5 8 10)))))
      (assert-equal '(signed-byte 64) (result-et (vm:isqrt a)))
      (assert-equal '(signed-byte 64) (result-et (vm:expt a 2)))
      (assert-equal '(1 4 9 16) (contents (vm:expt a 2))))))

(deftest tc-result-types-t-and-rational-arrays (transcendental-tests)
  (let* ((r (varr 1/2 1/4 3)))
    (assert-equal 'single-float (result-et (vm:exp r)))
    (assert-equal (expected-et-of-values (cl-elementwise #'cl:sqrt r)) (result-et (vm:sqrt r)))
    (assert-equal 'single-float (result-et (vm:atan r)))
    (assert-equal t (result-et (vm:abs r)))
    (assert-equal '(1/2 1/4 3) (contents (vm:abs r)))
    (assert-equal '(1/2 1/4 3) (contents (vm:abs (varr -1/2 1/4 3))))
    (assert-equal t (result-et (vm:signum r)))
    (assert-equal '(0 0 0) (contents (vm:imagpart r)))
    (assert-equal t (result-et (vm:expt r 2)))
    (assert-equal '(1/4 1/16 9) (contents (vm:expt r 2)))
    (assert-equal '(2 4 1/3) (contents (vm:expt r -1))))
  ;; out-of-domain integers: a negative integer makes the result complex
  (let* ((a (iarr '(signed-byte 64) 4 -4 9)) (r (vm:sqrt a)) (refs (cl-elementwise #'cl:sqrt a)))
    (assert-equal (expected-et-of-values refs) (result-et r))
    (assert-true (every #'= refs (contents r)))
    (assert-true (some #'complexp (contents r))))
  (let* ((a (iarr '(signed-byte 64) 1 -1)) (r (vm:log a)))
    (assert-equal '(complex single-float) (result-et r))
    (assert-true (every #'= (list (cl:log 1) (cl:log -1)) (contents r))))
  (assert-equal 'single-float (result-et (vm:log (iarr '(signed-byte 64) 1 2))))
  ;; mixed T arrays
  (let ((r (vm:abs (varr -1 2.5d0 #c(3 4)))))
    (assert-equal t (result-et r))
    (assert-true (every #'= (list 1 2.5d0 5) (contents r))))
  (assert-equal 'double-float (result-et (vm:exp (varr 1d0 2d0)))))

(deftest tc-complex-operator-types (transcendental-tests)
  ;; (complex 1 0) is 1: element type T
  (let ((r (vm:complex (iarr '(signed-byte 64) 1 2 3) 0)))
    (assert-equal t (result-et r))
    (assert-equal (list 1 2 3) (contents r))
    (assert-true (every #'integerp (contents r))))
  (let ((r (vm:complex (iarr '(signed-byte 64) 1 2 3) (iarr '(signed-byte 64) 0 5 0))))
    (assert-equal t (result-et r))
    (assert-equal (list 1 #c(2 5) 3) (contents r)))
  (assert-equal '(complex double-float) (result-et (vm:complex (iarr '(signed-byte 64) 1 2) (darr 1d0 2d0))))
  (assert-equal (list #c(1d0 1d0) #c(2d0 2d0))
                (contents (vm:complex (iarr '(signed-byte 64) 1 2) (darr 1d0 2d0))))
  (assert-equal '(complex double-float) (result-et (vm:complex (sarr 1f0) (darr 1d0))))
  (assert-equal (list #c(1d0 1d0)) (contents (vm:complex (sarr 1f0) (darr 1d0))))
  ;; one argument
  (assert-equal '(signed-byte 64) (result-et (vm:complex (iarr '(signed-byte 64) 1 2))))
  (assert-equal '(1 2) (contents (vm:complex (iarr '(signed-byte 64) 1 2))))
  (assert-equal (list #c(1d0 0d0)) (contents (vm:complex (darr 1d0))))
  ;; complex arguments are a type error, as in CL
  (assert-true (typep (condition-of (vm:complex (cdarr #c(1d0 1d0)) (darr 1d0))) 'type-error))
  (assert-true (typep (condition-of (vm:complex (cdarr #c(1d0 1d0)))) 'type-error)))

;;; ---------------------------------------------------------------------------
;;; MKL (the array path) against CL, element by element

(defparameter *ulp-tolerance* 4d0 "Real elementwise functions, in ulps of the CL result.")
(defparameter *complex-tolerance* 64d0 "Complex results, in units of epsilon * |result|.")
(defparameter *sizes* '(1 2 7 255 256 257 1000))

(deftest tc-real-unary-against-cl (transcendental-tests)
  "Within *ulp-tolerance* ulps of cl:op per element, single and double, at several sizes.
The maximum observed per operator and type goes to the ulp table."
  (let ((bad '()))
    (dolist (op *real-unary-ops*)
      (dolist (type '(:single :double))
        (rnd-reset)
        (let ((max 0))
          (dolist (n *sizes*)
            (let* ((a (random-real-array type n (fourth op) (fifth op)))
                   (r (funcall (uop-vm op) a))
                   (cl-values (cl-elementwise (uop-cl op) a)))
              (assert-true (typep r 'simple-array))
              (setf max (cl:max max (max-ulp r cl-values)))))
          (note-ulp (uop-name op) type max)
          (when (> max *ulp-tolerance*) (push (list (uop-name op) type max) bad)))))
    (assert-equal nil bad)))

(defun rand-complex-array (type n lo hi)
  (float-array-of type (loop repeat n collect
                         (let ((x (rnd-range lo hi)) (y (rnd-range lo hi)))
                           (ecase type
                             (:cdouble (complex x y))
                             (:csingle (complex (float x 1f0) (float y 1f0))))))))

(deftest tc-complex-unary-against-cl (transcendental-tests)
  "Complex float arrays (MKL vz/vc kernels) against cl:op per element, away from the branch cuts."
  (let ((bad '()))
    (dolist (op *real-unary-ops*)
      (dolist (type '(:csingle :cdouble))
        (rnd-reset 12345)
        (let ((max 0))
          (dolist (n '(1 5 300))
            (let* ((a (rand-complex-array type n 0.1d0 3d0))   ; first quadrant: no cuts
                   (r (funcall (uop-vm op) a))
                   (cl-values (cl-elementwise (uop-cl op) a)))
              (assert-equal (array-element-type a) (result-et r) (uop-name op))
              (setf max (cl:max max (max-ulp r cl-values)))))
          (note-ulp (uop-name op) type max)
          (when (> max *complex-tolerance*) (push (list (uop-name op) type max) bad)))))
    (assert-equal nil bad)))

(deftest tc-complex-unary-negative-quadrants (transcendental-tests)
  "Complex arrays with all four signs of the parts (still off the real and imaginary axes)."
  (let ((bad '()))
    (dolist (op *real-unary-ops*)
      (unless (member (uop-name op) '("tan" "tanh") :test #'string=)   ; poles: large cancellation
        (rnd-reset 777)
        (let* ((a (rand-complex-array :cdouble 400 -3d0 3d0))
               (r (funcall (uop-vm op) a))
               (cl-values (cl-elementwise (uop-cl op) a))
               (m (max-ulp r cl-values)))
          (note-ulp (uop-name op) :cdouble-all-quadrants m)
          (when (> m *complex-tolerance*) (push (list (uop-name op) m) bad)))))
    (assert-equal nil bad)))

(defun check-unary-pair (name vm-fn cl-fn a tol)
  (let* ((r (funcall vm-fn a)) (cl-values (cl-elementwise cl-fn a)))
    (declare (ignorable name))
    (<= (max-ulp r cl-values) tol)))

(deftest tc-complex-part-operators-against-cl (transcendental-tests)
  (rnd-reset 99)
  (dolist (type '(:single :double :csingle :cdouble))
    (let ((a (if (member type '(:single :double))
                 (random-real-array type 300 -50d0 50d0)
                 (rand-complex-array type 300 -50d0 50d0))))
      (dolist (name '("abs" "signum" "conjugate" "realpart" "imagpart" "phase"))
        (let* ((op (find name *all-ops* :key #'first :test #'string=))
               (r (funcall (uop-vm op) a))
               (m (max-ulp r (cl-elementwise (uop-cl op) a))))
          (note-ulp name type m)
          (assert-true (<= m (if (and (member type '(:csingle :cdouble)) (member name '("abs" "phase" "signum") :test #'string=))
                                 *complex-tolerance* 0))
                       name type m)))))
  ;; cis of real arrays
  (dolist (type '(:single :double))
    (let* ((a (random-real-array type 300 -50d0 50d0))
           (r (vm:cis a)) (m (max-ulp r (cl-elementwise #'cl:cis a))))
      (note-ulp "cis" type m)
      (assert-true (<= m *complex-tolerance*)))))

(deftest tc-abs-signum-special-values (transcendental-tests)
  "abs, signum, phase, imagpart etc. on zeros, infinities, NaN: the bits CL gives per element."
  (let* ((specials (list 0d0 -0d0 1d0 -1d0 (inf+) (inf-) (nan) 1d-310 -1d-310 5d-324))
         (a (apply #'darr specials)))
    (dolist (name '("abs" "signum" "conjugate" "realpart" "imagpart" "phase"))
      (let* ((op (find name *all-ops* :key #'first :test #'string=))
             (r (funcall (uop-vm op) a))
             (c (cl-elementwise (uop-cl op) a)))
        (loop for x in c for v in (contents r)
              do (assert-true (same-bits-p v x) name v x))))
    (let* ((ss (mapcar (lambda (x) (float x 1f0)) (list 0d0 -0d0 1d0 -1d0 1d-30)))
           (a (apply #'sarr (append ss (list (single-inf+) (single-nan))))))
      (dolist (name '("abs" "signum" "phase" "imagpart"))
        (let* ((op (find name *all-ops* :key #'first :test #'string=))
               (r (funcall (uop-vm op) a)) (c (cl-elementwise (uop-cl op) a)))
          (loop for x in c for v in (contents r) do (assert-true (same-bits-p v x) name v x)))))))

;;; ---------------------------------------------------------------------------
;;; Out-of-domain real arrays give complex arrays

(defparameter *boundary-values*
  (list 0d0 -0d0 1d0 -1d0 0.5d0 -0.5d0 2d0 -2d0 1.0000000001d0 -1.0000000001d0 0.9999999999d0
        100d0 -100d0 1d-300 -1d-300 5d-324 -5d-324 1d300))

(defun special-doubles ()
  (append *boundary-values* (list (inf+) (inf-) (nan))))

(defun specials-of (type)
  (if (eq type :single)
      (mapcar (lambda (x) (cond ((nanp x) (single-nan))
                                ((and (not (finite-p x)) (plusp x)) (single-inf+))
                                ((not (finite-p x)) (- (single-inf+)))
                                ((> (abs x) 1d38) (float (signum x) 1f0))
                                ((and (/= x 0) (< (abs x) 1d-37)) (if (minusp x) -1f-30 1f-30))
                                (t (float x 1f0))))
              (special-doubles))
      (special-doubles)))

(defun cl-complexp-p (cl-fn x)
  (complexp (masked-cl cl-fn x)))

(defun safe-cl (cl-fn x)
  "CL-FN of X with traps masked, or :error when CL signals anyway."
  (handler-case (masked-cl cl-fn x) (error () :error)))

(deftest tc-out-of-domain-promotes-whole-array (transcendental-tests)
  "For sqrt log asin acos acosh atanh: one out-of-domain element makes the result a complex array
whose elements are cl:op's (within tolerance); in-domain arrays stay real.  Elements: boundary
values, NaN, Inf, -0.0 each placed in an otherwise in-domain array."
  (dolist (name *domain-op-names*)
    (let ((op (uop name)))
      (dolist (type '(:single :double))
        (let ((bad '()))
          (dolist (v (specials-of type))
            (let* ((safe (convert-sample type 0.5d0))
                   (safe (if (complexp safe) (realpart safe) safe))
                   (ref (safe-cl (uop-cl op) v)))
              (unless (eq ref :error)
                (dolist (arr (list (float-array-of type (list safe v safe))
                                   (float-array-of type (list v safe))
                                   (float-array-of type (list v))
                                   (float-array-of type (append (make-list 300 :initial-element safe) (list v)))))
                  (let* ((r (funcall (uop-vm op) arr))
                         (refs (cl-elementwise (uop-cl op) arr))
                         (expect-complex (some #'complexp refs))
                         (is-complex (typep (result-et r) 'cons)))
                    (unless (eq expect-complex is-complex)
                      (push (list name type :complexness v expect-complex is-complex) bad))
                    (let ((m (max-ulp r (if expect-complex
                                            (mapcar (lambda (x) (if (complexp x) x (complex x (float 0 x)))) refs)
                                            refs))))
                      (when (> m *complex-tolerance*)
                        (push (list name type :value v m (contents r) refs) bad))))))))
          (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 6 (length bad)))))
          (assert-equal nil bad))))))

(deftest tc-out-of-domain-mixed-arrays (transcendental-tests)
  "Random arrays straddling the domain boundary of every domain operator: whole-result complex
with every element equal to cl:op's; completely in-domain arrays real."
  (dolist (name *domain-op-names*)
    (let ((op (uop name)))
      (dolist (type '(:single :double))
        (rnd-reset 4242)
        (let* ((lo (if (string= name "acosh") 0d0 -3d0)) (hi 3d0))
          (dolist (n '(2 9 300))
            (let* ((a (random-real-array type n lo hi))
                   (r (funcall (uop-vm op) a))
                   (refs (cl-elementwise (uop-cl op) a))
                   (cx (some #'complexp refs)))
              (assert-equal (if cx t nil) (typep (result-et r) 'cons) name type n)
              (assert-true (<= (max-ulp r (mapcar (lambda (x) (if (and cx (not (complexp x))) (complex x (float 0 x)) x))
                                                  refs))
                               *complex-tolerance*)
                           name type n))))))))

(deftest tc-domain-scan-agrees-with-cl (transcendental-tests)
  "The complexness the pre-scan decides equals cl:op's for each single element (boundary values,
NaN, Inf, -0.0), for every domain operator, in arrays of one element."
  (let ((bad '()))
    (dolist (name *domain-op-names*)
      (let ((op (uop name)))
        (dolist (type '(:single :double))
          (dolist (v (specials-of type))
            (let ((ref (safe-cl (uop-cl op) v)))
              (unless (eq ref :error)
                (let* ((r (funcall (uop-vm op) (float-array-of type (list v))))
                       (is-complex (typep (result-et r) 'cons)))
                  (unless (eq is-complex (complexp ref))
                    (push (list name type v :cl ref :vm (contents r)) bad)))))))))
    (when bad (format t "~&~{~S~%~}" bad))
    (assert-equal nil bad)))

(deftest tc-non-domain-ops-never-promote (transcendental-tests)
  (dolist (name '("exp" "sin" "cos" "tan" "atan" "sinh" "cosh" "tanh" "asinh"))
    (let* ((op (uop name)) (a (apply #'darr (special-doubles))))
      (assert-equal 'double-float (result-et (funcall (uop-vm op) a)) name))))

(deftest tc-expt-out-of-domain (transcendental-tests)
  "Negative real base with a non-integer power: complex array; elements are cl:expt's."
  (dolist (type '(:single :double))
    (let* ((vals (mapcar (lambda (x) (convert-real type x)) '(-8d0 2d0 -2d0 9d0 -0.5d0 0.5d0)))
           (base (float-array-of type vals))
           (power (float-array-of type (mapcar (lambda (x) (convert-real type x)) '(2d0 0.5d0 3d0 0.5d0 1.5d0 2d0))))
           (r (vm:expt base power))
           (refs (loop for b in vals for p in (mapcar (lambda (x) (convert-real type x)) '(2d0 0.5d0 3d0 0.5d0 1.5d0 2d0))
                       collect (masked-cl #'cl:expt b p))))
      (assert-true (some #'complexp refs))
      (assert-equal t (typep (result-et r) 'cons))
      (assert-true (<= (max-ulp r (mapcar (lambda (x) (if (complexp x) x (complex x (float 0 x)))) refs))
                       *complex-tolerance*))))
  ;; all in domain: real
  (let ((r (vm:expt (darr 2d0 3d0) (darr 0.5d0 2d0))))
    (assert-equal 'double-float (result-et r)))
  ;; integer-valued powers of negative bases: CL's own answer per element
  (let* ((b (darr -8d0 -2d0 3d0)) (p (darr 2d0 3d0 2d0))
         (r (vm:expt b p)) (refs (cl-elementwise2 #'cl:expt b p)))
    (assert-true (<= (max-ulp r (mapcar (lambda (x) (if (and (complexp (first (contents r))) (not (complexp x))) (complex x 0d0) x)) refs))
                     *complex-tolerance*))))

(defun cl-elementwise2 (cl-fn a b)
  (loop for x in (contents a) for y in (contents b) collect (masked-cl cl-fn x y)))

;;; ---------------------------------------------------------------------------
;;; IEEE results, no signal

(deftest tc-ieee-no-signal (transcendental-tests)
  (let ((big (darr 1000d0 -1000d0 1d300)))
    (assert-equal (list (inf+) 0d0 (inf+)) (contents (vm:exp big)))
    (assert-equal (list (inf+) (inf+) (inf+)) (contents (vm:cosh big)))
    (assert-equal (list (inf+) (inf-) (inf+)) (contents (vm:sinh big))))
  (assert-equal (list (inf-)) (contents (vm:log (darr 0d0))))
  (assert-equal (list (inf-) 0d0) (contents (vm:log (darr 0d0 1d0))))
  (assert-true (nanp (first (contents (vm:sqrt (darr (nan)))))))
  (assert-equal (list (- (single-inf+))) (contents (vm:log (sarr 0f0))))
  (assert-equal (list (single-inf+)) (contents (vm:exp (sarr 1000f0))))
  ;; atanh of +-1, expt of zero to a negative power, atan2 of zeros
  (assert-equal (list (inf+) (inf-)) (contents (vm:atanh (darr 1d0 -1d0))))
  (assert-equal (list (inf+)) (contents (vm:expt (darr 0d0) (darr -1d0))))
  (assert-equal (list (inf+)) (contents (vm:expt (darr 0d0) -1d0)))
  (assert-equal (list 0d0 pi) (mapcar (lambda (x) (if (zerop x) 0d0 x)) (contents (vm:atan (darr 0d0 0d0) (darr 1d0 -1d0)))))
  (assert-equal (list (inf+)) (contents (vm:expt (darr 10d0) 400)))
  (assert-equal (list (inf+)) (contents (vm:expt (darr 1d300) (darr 2d0))))
  (assert-equal (list (masked-cl #'cl:log #c(0d0 0d0))) (contents (vm:log (cdarr #c(0d0 0d0)))))
  ;; MXCSR is clean afterwards: a scalar overflow still signals the right condition
  (assert-true (typep (condition-of (opaque (cl:* (big-double) (big-double)))) 'floating-point-overflow)))

;;; ---------------------------------------------------------------------------
;;; isqrt

(deftest tc-isqrt (transcendental-tests)
  (assert-equal '(0 1 1 1 2 2 3) (contents (vm:isqrt (iarr '(unsigned-byte 8) 0 1 2 3 4 8 9))))
  (assert-equal (list (expt 10 15)) (contents (vm:isqrt (varr (expt 10 30)))))
  ;; not integers: the type error CL signals
  (dolist (a (list (darr 4d0) (varr 4 2.0) (varr 1/4) (cdarr #c(1d0 0d0))))
    (let* ((bad-element (find-if-not #'integerp (contents a)))
           (v (condition-of (vm:isqrt a))) (c (condition-of (cl:isqrt bad-element))))
      (assert-true (typep v 'type-error))
      (assert-equal (type-error-datum c) (type-error-datum v))))
  (assert-true (typep (condition-of (vm:isqrt (iarr '(signed-byte 64) 4 -4))) 'type-error))
  (assert-equal '(signed-byte 64) (result-et (vm:isqrt (iarr '(signed-byte 64)))))
  (assert-equal 0 (length (vm:isqrt (iarr '(signed-byte 64))))))

;;; ---------------------------------------------------------------------------
;;; integers: abs signum, generic path

(deftest tc-integer-abs-signum (transcendental-tests)
  (let ((a (iarr '(signed-byte 64) -5 0 7 -1)))
    (assert-equal '(5 0 7 1) (contents (vm:abs a)))
    (assert-equal '(-1 0 1 -1) (contents (vm:signum a)))
    (assert-equal '(signed-byte 64) (result-et (vm:abs a))))
  ;; the most negative (signed-byte 64): abs overflows to a bignum: element type T
  (let* ((m (- (expt 2 63))) (a (iarr '(signed-byte 64) m 5)) (r (vm:abs a)))
    (assert-equal (list (expt 2 63) 5) (contents r))
    (assert-equal t (result-et r))
    (assert-equal '(signed-byte 64) (result-et (vm:abs (iarr '(signed-byte 64) (1+ m) 5))))
    (assert-equal (list -1 1) (contents (vm:signum a)))
    (assert-equal '(signed-byte 64) (result-et (vm:signum a))))
  ;; unsigned 64 beyond signed range
  (let ((a (iarr '(unsigned-byte 64) (1- (expt 2 64)) 3)))
    (assert-equal (list (1- (expt 2 64)) 3) (contents (vm:abs a)))
    (assert-equal '(1 1) (contents (vm:signum a))))
  ;; bignums
  (assert-equal (list (expt 10 30) 5) (contents (vm:abs (varr (- (expt 10 30)) -5))))
  (assert-equal (list (expt 2 100)) (contents (vm:expt (varr 2) 100)))
  (assert-equal (list (expt 2 100) 3) (contents (vm:expt (varr 2 3) (varr 100 1)))))

;;; ---------------------------------------------------------------------------
;;; expt: special cases agree with cl:expt per element

(defparameter *expt-corpus*
  (list 0 1 -1 2 -2 3 0.5d0 -0.5d0 2d0 -2d0 0d0 1/2 -1/2 1.5f0 #c(1 1) 10))

(deftest tc-expt-special-cases-per-element (transcendental-tests)
  "Arrays built from corpus values: each element of (expt a b) is cl:expt of the elements, for
integer, rational and T arrays (generic path) and float arrays with integer powers."
  (let ((bad '()))
    (dolist (x '(0 1 -1 2 -3 5 -8))
      (dolist (y '(0 1 2 3 -1 -2 5))
        (unless (and (zerop x) (minusp y))
          (let* ((r (vm:expt (iarr '(signed-byte 64) x x) (iarr '(signed-byte 64) y y)))
                 (c (cl:expt x y)))
            (unless (every (lambda (e) (eql e c)) (contents r)) (push (list :int x y (contents r) c) bad))))))
    ;; (expt x 0) is 1 of x's type
    (dolist (x (list 2d0 2f0 #c(1d0 1d0) #c(1f0 1f0) 0d0 -3d0))
      (let* ((a (make-arr (cond ((typep x 'double-float) 'double-float) ((typep x 'single-float) 'single-float)
                                ((typep (realpart x) 'double-float) '(complex double-float))
                                (t '(complex single-float)))
                          2 (list x x)))
             (r (vm:expt a 0)))
        (unless (every (lambda (e) (eql e (cl:expt x 0))) (contents r)) (push (list :zero-power x (contents r)) bad))))
    ;; float base, integer powers (CL's repeated multiplication)
    (dolist (x (list 2d0 -2d0 0.1d0 1.0000001d0 0d0 -0d0))
      (dolist (y '(0 1 2 3 7 -1 -3 100))
        (unless (and (zerop x) (minusp y))
          (let* ((r (vm:expt (darr x x) y)) (c (masked-cl #'cl:expt x y)))
            (unless (every (lambda (e) (eql e c)) (contents r)) (push (list :float-int x y (contents r) c) bad))))))
    ;; corpus pairs through the T-array path
    (dolist (x *expt-corpus*)
      (dolist (y *expt-corpus*)
        (let ((c (handler-case (masked-cl #'cl:expt x y) (error () :error))))
          (unless (eq c :error)
            (let ((r (handler-case (vm:expt (varr x x) (varr y y)) (error () :error))))
              (unless (and (arrayp r)
                           (every (lambda (e) (and (= e c) (equal (type-of e) (type-of c)))) (contents r)))
                (push (list :generic x y (and (arrayp r) (contents r)) c) bad)))))))
    (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 8 (length bad)))))
    (assert-equal nil bad)))

(deftest tc-expt-float-against-cl (transcendental-tests)
  (let ((bad '()))
    (dolist (type '(:single :double))
      (rnd-reset 31337)
      (dolist (n '(1 5 300))
        (let* ((a (random-real-array type n 0.1d0 20d0))
               (b (random-real-array type n -5d0 5d0))
               (r (vm:expt a b))
               (m (max-ulp r (cl-elementwise2 #'cl:expt a b))))
          (note-ulp "expt" type m)
          (when (> m *ulp-tolerance*) (push (list type n m) bad)))))
    ;; complex
    (dolist (type '(:csingle :cdouble))
      (rnd-reset 424242)
      (let* ((a (rand-complex-array type 100 0.1d0 3d0))
             (b (rand-complex-array type 100 0.1d0 2d0))
             (r (vm:expt a b))
             (m (max-ulp r (cl-elementwise2 #'cl:expt a b))))
        (note-ulp "expt" type m)
        (when (> m *complex-tolerance*) (push (list type m) bad))))
    (assert-equal nil bad)))

(deftest tc-atan2-log2-against-cl (transcendental-tests)
  (let ((bad '()))
    (dolist (type '(:single :double))
      (rnd-reset 2718)
      (dolist (n '(1 5 300))
        (let* ((y (random-real-array type n -50d0 50d0)) (x (random-real-array type n -50d0 50d0))
               (r (vm:atan y x)) (m (max-ulp r (cl-elementwise2 #'cl:atan y x))))
          (note-ulp "atan2" type m)
          (when (> m *ulp-tolerance*) (push (list :atan type n m) bad)))
        (let* ((x (random-real-array type n 0.01d0 50d0)) (b (random-real-array type n 1.1d0 20d0))
               (r (vm:log x b)) (m (max-ulp r (cl-elementwise2 #'cl:log x b))))
          (note-ulp "log2" type m)
          (assert-equal (if (eq type :single) 'single-float 'double-float) (result-et r))
          (when (> m (* 2 *ulp-tolerance*)) (push (list :log2 type n m) bad)))))
    (assert-equal nil bad)))

(deftest tc-log2-domain-and-special-cases (transcendental-tests)
  ;; negative x or base: complex, each element cl:log's
  (let* ((x (darr 8d0 -8d0 2d0)) (b (darr 2d0 2d0 10d0))
         (r (vm:log x b)) (refs (cl-elementwise2 #'cl:log x b)))
    (assert-equal '(complex double-float) (result-et r))
    (assert-true (<= (max-ulp r (mapcar (lambda (e) (if (complexp e) e (complex e 0d0))) refs)) *complex-tolerance*)))
  (let* ((x (darr 8d0 2d0)) (b (darr 2d0 -10d0))
         (r (vm:log x b)) (refs (cl-elementwise2 #'cl:log x b)))
    (assert-equal '(complex double-float) (result-et r))
    (assert-true (<= (max-ulp r (mapcar (lambda (e) (if (complexp e) e (complex e 0d0))) refs)) *complex-tolerance*)))
  ;; base 0: CL's special case, per element
  (let* ((x (darr 8d0 2d0)) (b (darr 0d0 2d0)) (r (vm:log x b)))
    (assert-equal (cl-elementwise2 #'cl:log x b) (contents r)))
  ;; integer / mixed operands: CL per element
  (assert-equal (list (cl:log 8 2) (cl:log 4 2)) (contents (vm:log (iarr '(signed-byte 64) 8 4) 2)))
  (assert-equal (list (cl:log 8d0 2) (cl:log 4d0 2)) (contents (vm:log (darr 8d0 4d0) 2)))
  (assert-equal (list (cl:log 8 2d0)) (contents (vm:log (iarr '(signed-byte 64) 8) 2d0)))
  (assert-equal (list (cl:log 8d0 2)) (contents (vm:log (darr 8d0) 2)))
  ;; scalar base and array number, array base and scalar number
  (assert-true (<= (max-ulp (vm:log 100d0 (darr 10d0 100d0)) (cl-elementwise (lambda (b) (cl:log 100d0 b)) (darr 10d0 100d0)))
                   8)))

(deftest tc-complex-atan-cis-phase-values (transcendental-tests)
  (assert-equal (list (cl:atan 1d0 2d0) (cl:atan -1d0 -2d0)) (contents (vm:atan (darr 1d0 -1d0) (darr 2d0 -2d0))))
  (assert-equal (list (cl:atan 1 2)) (contents (vm:atan (iarr '(signed-byte 64) 1) 2)))
  (assert-equal (list (cl:atan 1d0 2)) (contents (vm:atan (darr 1d0) 2)))
  (assert-equal (list (cl:atan 3 2d0)) (contents (vm:atan 3 (darr 2d0))))
  (assert-equal (list (cl:phase -1) (cl:phase 1)) (contents (vm:phase (iarr '(signed-byte 64) -1 1))))
  (assert-equal (list (cl:phase -1d0) (cl:phase 0d0) (cl:phase -0d0)) (contents (vm:phase (darr -1d0 0d0 -0d0))))
  ;; atan with a complex argument is an error, as in CL
  (assert-true (typep (condition-of (vm:atan (cdarr #c(1d0 1d0)) (darr 1d0))) 'error))
  (assert-true (typep (condition-of (vm:cis (cdarr #c(1d0 1d0)))) 'error)))

;;; ---------------------------------------------------------------------------
;;; shapes, recycling, :into of the binary forms

(defun reference-pairs (cl-fn a b dims)
  "The element list of CL-FN applied pairwise to A and B (row-major arrays or numbers), with
R recycling for vectors and column-major recycling of a vector over a matrix."
  (declare (ignorable dims))
  (let* ((na (if (arrayp a) (array-total-size a) 1)) (nb (if (arrayp b) (array-total-size b) 1))
         (n (cl:max na nb)))
    (loop for i below n
          collect (masked-cl cl-fn (if (arrayp a) (row-major-aref a (mod i na)) a)
                             (if (arrayp b) (row-major-aref b (mod i nb)) b)))))

(deftest tc-binary-shapes-vectors (transcendental-tests)
  "Vectors of different lengths recycle (with the warning), scalars broadcast, for every binary form."
  (dolist (op (list (list "expt" #'vm:expt #'cl:expt 0.5d0 3d0) (list "atan" #'vm:atan #'cl:atan -5d0 5d0)
                    (list "log" #'vm:log #'cl:log 1.5d0 9d0)))
    (destructuring-bind (name vm-fn cl-fn lo hi) op
      (rnd-reset 55)
      (let ((x (random-real-array :double 6 lo hi)) (y (random-real-array :double 3 lo hi))
            (z (random-real-array :double 1 lo hi)) (long (random-real-array :double 7 lo hi)))
        (dolist (pair (list (list x y) (list y x) (list x z) (list z x) (list x 2d0) (list 2d0 x)
                            (list x x) (list y long)))
          (multiple-value-bind (r ws) (collect-warnings (lambda () (funcall vm-fn (first pair) (second pair))))
            (let ((refs (reference-pairs cl-fn (first pair) (second pair) nil)))
              (assert-equal (length refs) (length r) name)
              (assert-true (<= (max-ulp r (mapcar (lambda (e) (if (and (typep (result-et r) 'cons) (not (complexp e))) (complex e 0d0) e)) refs))
                               (* 4 *complex-tolerance*))
                           name)
              (assert-equal (if (and (arrayp (first pair)) (arrayp (second pair))
                                     (not (zerop (mod (cl:max (length (first pair)) (length (second pair)))
                                                      (cl:min (length (first pair)) (length (second pair)))))))
                                1 0)
                            (length (recycling-warnings ws))))))))))

(deftest tc-binary-shapes-matrices (transcendental-tests)
  "A matrix and a vector: column-major recycling; two matrices must conform."
  (dolist (op (list (list "expt" #'vm:expt #'cl:expt) (list "atan" #'vm:atan #'cl:atan)
                    (list "log" #'vm:log #'cl:log) (list "complex" #'vm:complex #'cl:complex)))
    (destructuring-bind (name vm-fn cl-fn) op
      (rnd-reset 66)
      (let* ((m (make-arr 'double-float '(3 2) (loop repeat 6 collect (rnd-range 1.5d0 5d0))))
             (v (random-real-array :double 3 1.5d0 5d0))
             (m2 (make-arr 'double-float '(3 2) (loop repeat 6 collect (rnd-range 1.5d0 5d0)))))
        (dolist (pair (list (list m v) (list v m) (list m 2d0) (list 2d0 m) (list m m2)))
          (let* ((r (funcall vm-fn (first pair) (second pair))) (a (first pair)) (b (second pair))
                 (dims (array-dimensions (if (and (arrayp a) (> (array-rank a) 1)) a b))))
            (assert-equal dims (array-dimensions r) name)
            ;; reference: element with row-major index i has column-major index c; the vector takes c mod length
            (dotimes (i 6)
              (let* ((c (vm.impl::colmajor-order-index dims i))
                     (x (if (arrayp a) (if (> (array-rank a) 1) (row-major-aref a i) (aref a (mod c (length a)))) a))
                     (y (if (arrayp b) (if (> (array-rank b) 1) (row-major-aref b i) (aref b (mod c (length b)))) b))
                     (ref (masked-cl cl-fn x y)))
                (assert-true (<= (ulp-diff (row-major-aref r i) ref) 16) name pair i)))))
        (assert-true (typep (condition-of (funcall vm-fn m (make-arr 'double-float '(2 3) (loop repeat 6 collect 2d0))))
                            'vm:non-conformable-arrays))
        (assert-true (typep (condition-of (funcall vm-fn m (random-real-array :double 7 1d0 2d0)))
                            'vm:non-conformable-arrays))))))

(deftest tc-into (transcendental-tests)
  (let ((out (make-array 3 :element-type 'double-float :initial-element 0d0)) (a (darr 0d0 1d0 2d0)))
    (assert-true (eq out (vm:exp a :into out)))
    (assert-true (<= (max-ulp out (cl-elementwise #'cl:exp a)) 2))
    (assert-equal (list 0d0 1d0 2d0) (contents a))
    (vm:sqrt (darr 4d0 9d0 16d0) :into out)
    (assert-equal (list 2d0 3d0 4d0) (contents out))
    (vm:expt (darr 2d0 3d0 4d0) 2 :into out)
    (assert-equal (list 4d0 9d0 16d0) (contents out))
    (vm:atan (darr 0d0 0d0 0d0) (darr 1d0 1d0 1d0) :into out)
    (assert-equal (list 0d0 0d0 0d0) (contents out))
    (vm:log (darr 4d0 8d0 16d0) 2d0 :into out)
    (assert-true (<= (max-ulp out (list 2d0 3d0 4d0)) 2))
    (vm:abs (darr -1d0 -2d0 3d0) :into out)
    (assert-equal (list 1d0 2d0 3d0) (contents out))
    ;; in place
    (let ((b (darr 1d0 4d0 9d0)))
      (vm:sqrt b :into b)
      (assert-equal (list 1d0 2d0 3d0) (contents b)))
    ;; scalar operands with :into fill the array
    (vm:sqrt 4d0 :into out)
    (assert-equal (list 2d0 2d0 2d0) (contents out))
    (vm:expt 2 3 :into out)
    (assert-equal (list 8d0 8d0 8d0) (contents out))
    ;; widening single -> double is allowed, narrowing is an error
    (vm:exp (sarr 0f0 0f0 0f0) :into out)
    (assert-equal (list 1d0 1d0 1d0) (contents out))
    (assert-true (typep (condition-of (vm:exp (darr 0d0) :into (make-array 1 :element-type 'single-float)))
                        'vm:output-type-error))
    ;; out-of-domain result into a real array: error; into a complex array: fine
    (assert-true (typep (condition-of (vm:sqrt (darr -4d0 4d0 9d0) :into out)) 'vm:output-type-error))
    (let ((zout (make-array 3 :element-type '(complex double-float))))
      (vm:sqrt (darr -4d0 4d0 9d0) :into zout)
      (assert-equal (list #c(0d0 2d0) #c(2d0 0d0) #c(3d0 0d0)) (contents zout)))
    ;; :into must be an array; arity errors still the CL ones
    (assert-true (typep (condition-of (vm:exp (darr 1d0) :into 5)) 'type-error))
    (assert-true (typep (condition-of (vm:exp (darr 1d0) (darr 1d0))) 'program-error))
    (assert-true (typep (condition-of (vm:expt (darr 1d0))) 'program-error))
    (assert-true (typep (condition-of (vm:abs (darr 1d0) :into)) 'program-error))))

(deftest tc-no-mutation (transcendental-tests)
  (rnd-reset 1)
  (dolist (op (append *all-ops* *binary-tc-ops*))
    (dolist (type '(:single :double :csingle :cdouble))
      (let* ((a (if (member type '(:single :double))
                    (random-real-array type 50 0.1d0 0.9d0) (rand-complex-array type 50 0.1d0 0.9d0)))
             (copy (copy-arr a)))
        (ignore-errors (funcall (uop-vm op) a))
        (when (member (uop-name op) '("expt" "log" "atan" "complex") :test #'string=)
          (ignore-errors (funcall (uop-vm op) a a))
          (ignore-errors (funcall (uop-vm op) a 2)))
        (assert-true (same-contents-p a copy) (uop-name op) type)))
    ;; integer arrays too
    (let* ((a (iarr '(signed-byte 64) 3 -4 5)) (copy (copy-arr a)))
      (ignore-errors (funcall (uop-vm op) a))
      (assert-true (same-contents-p a copy) (uop-name op)))))

;;; ---------------------------------------------------------------------------
;;; backend independence of the array length, and the MKL-less path

(defun prefix-bits-equal-p (vm-fn make-array-fn)
  "The result for the first K elements of a long array has the same bits as the first K
elements of the result for the whole array (K at the sizes where backends could switch)."
  (let* ((big (funcall make-array-fn 100000)) (rbig (funcall vm-fn big)) (ok t))
    (dolist (k '(1 2 3 31 255 256 257 1023 1024 1025 2047 2048 2049 4096))
      (let* ((sub (subseq big 0 k)) (r (funcall vm-fn sub)))
        (loop for i below k
              unless (same-bits-p (row-major-aref r i) (row-major-aref rbig i))
                do (setf ok nil) (format t "~&prefix mismatch k=~D i=~D ~S ~S~%" k i
                                         (row-major-aref r i) (row-major-aref rbig i)))))
    ok))

(deftest tc-backend-independent-of-length (transcendental-tests)
  "The same element has the same bits whatever the array length (no size cutoff in the
transcendental operators): single/double/complex, every operator."
  (dolist (op (append *real-unary-ops* (list (list "abs" #'vm:abs nil -50d0 50d0)
                                             (list "phase" #'vm:phase nil -50d0 50d0)
                                             (list "cis" #'vm:cis nil -50d0 50d0))))
    (dolist (type '(:single :double :csingle :cdouble))
      (unless (and (member type '(:csingle :cdouble)) (string= (uop-name op) "cis"))
        (let ((lo (fourth op)) (hi (fifth op)))
          (rnd-reset 2024)
          (let ((maker (lambda (n)
                         (if (member type '(:single :double))
                             (random-real-array type n lo hi)
                             (rand-complex-array type n (cl:max lo -3d0) (cl:min hi 3d0))))))
            (assert-true (prefix-bits-equal-p (uop-vm op) maker) (uop-name op) type))))))
  ;; the binary kernels, MKL at every size
  (dolist (type '(:single :double))
    (rnd-reset 2025)
    (let ((x (random-real-array type 100000 0.1d0 9d0)) (y (random-real-array type 100000 0.1d0 3d0)))
      (dolist (fn (list #'vm:expt #'vm:atan #'vm:log))
        (let ((rbig (funcall fn x y)))
          (dolist (k '(1 2 255 256 257 1025 2049))
            (let ((r (funcall fn (subseq x 0 k) (subseq y 0 k))))
              (assert-true (loop for i below k always (same-bits-p (aref r i) (aref rbig i)))
                           fn type k))))))))

(deftest tc-out-of-domain-element-independent-of-array (transcendental-tests)
  "An element's value does not depend on the other elements: the in-domain elements of a promoted
array are the real results, the out-of-domain ones the complex kernel's, at any length."
  (rnd-reset 8)
  (dolist (name '("sqrt" "log" "asin" "acos"))
    (let* ((op (uop name)) (n 3000)
           (a (random-real-array :double n -3d0 3d0))
           (r (funcall (uop-vm op) a)))
      (assert-equal '(complex double-float) (result-et r))
      (dolist (k '(1 2 3 255 256 257 1000))
        (let ((sub (funcall (uop-vm op) (subseq a 0 k))))
          (loop for i below k
                do (assert-true (or (same-bits-p (row-major-aref sub i) (row-major-aref r i))
                                    ;; a sub-array without out-of-domain elements is real
                                    (not (typep (result-et sub) 'cons)))
                                name k i)))))))

(deftest tc-lisp-path-without-mkl-agrees-with-cl (transcendental-tests)
  "With *MKL-AVAILABLE* false everything runs in Lisp loops; per element it is exactly cl:op
(same bits), including out-of-domain promotion."
  (let ((bad '()) (vm:*mkl-available* nil))
    (dolist (op *real-unary-ops*)
      (dolist (type '(:single :double :csingle :cdouble))
        (rnd-reset 6)
        (let* ((a (if (member type '(:single :double))
                      (random-real-array type 400 (if (string= (uop-name op) "acosh") 0.1d0 -3d0) 3d0)
                      (rand-complex-array type 400 0.1d0 3d0)))
               (r (funcall (uop-vm op) a))
               (refs (cl-elementwise (uop-cl op) a))
               (cx (some #'complexp refs)))
          (loop for i from 0 for c in refs
                for want = (if (and cx (not (complexp c))) (complex c (float 0 c)) c)
                unless (same-bits-p (row-major-aref r i) want)
                  do (push (list (uop-name op) type i (row-major-aref r i) want) bad)))))
    (dolist (name '("abs" "signum" "phase" "realpart" "imagpart" "conjugate"))
      (let ((op (find name *all-ops* :key #'first :test #'string=)))
        (dolist (type '(:single :double :csingle :cdouble))
          (rnd-reset 7)
          (let* ((a (if (member type '(:single :double))
                        (random-real-array type 100 -30d0 30d0) (rand-complex-array type 100 -30d0 30d0)))
                 (r (funcall (uop-vm op) a)) (refs (cl-elementwise (uop-cl op) a)))
            (loop for i from 0 for c in refs
                  unless (same-bits-p (row-major-aref r i) c)
                    do (push (list name type i (row-major-aref r i) c) bad))))))
    (dolist (type '(:single :double))
      (rnd-reset 9)
      (let* ((a (random-real-array type 100 -30d0 30d0)) (b (random-real-array type 100 -4d0 4d0))
             (r (vm:atan a b)))
        (loop for i from 0 for c in (cl-elementwise2 #'cl:atan a b)
              unless (same-bits-p (row-major-aref r i) c) do (push (list :atan type i) bad))
        (let* ((p (random-real-array type 100 0.5d0 5d0)) (e (random-real-array type 100 -3d0 3d0))
               (r (vm:expt p e)))
          (loop for i from 0 for c in (cl-elementwise2 #'cl:expt p e)
                unless (same-bits-p (row-major-aref r i) c) do (push (list :expt type i (row-major-aref r i) c) bad)))
        (let* ((p (random-real-array type 100 -3d0 5d0)) (e (random-real-array type 100 -3d0 3d0))
               (r (vm:expt p e)) (refs (cl-elementwise2 #'cl:expt p e)))
          (loop for i from 0 for c in refs
                unless (same-bits-p (row-major-aref r i) (if (complexp c) c (complex c (float 0 c))))
                  do (push (list :expt-neg type i (row-major-aref r i) c) bad)))))
    (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 8 (length bad)))))
    (assert-equal nil bad)))

;;; ---------------------------------------------------------------------------
;;; sqrt: correctly rounded

(deftest tc-sqrt-correctly-rounded (transcendental-tests)
  "Real sqrt of single and double arrays is bit-identical to cl:sqrt (never MKL's v?Sqrt, which
differs from the correctly rounded value in the last bit for about 1% of doubles)."
  (dolist (type '(:single :double))
    (rnd-reset 123)
    (dolist (hi '(1d6 1d-3 1d30))
      (let* ((a (random-real-array type 20000 0d0 hi))
             (r (vm:sqrt a)))
        (assert-equal (array-element-type a) (array-element-type r))
        (loop for i below 20000
              do (assert-true (same-bits-p (aref r i) (masked-cl #'cl:sqrt (aref a i))) type hi i))))))

(deftest tc-sqrt-special-values (transcendental-tests)
  "Real sqrt of zeros, subnormals, the extremes, infinity and NaN is what cl:sqrt gives, bit for bit
(on CCL the kernel calls the C library's sqrt/sqrtf instead of CL:SQRT)."
  (dolist (type '(:double :single))
    (let* ((vals (if (eq type :double)
                     (list 0d0 -0d0 1d0 4d0 2d0 0.5d0 least-positive-double-float (* 3 least-positive-double-float)
                           least-positive-normalized-double-float (cl:* 0.9d0 least-positive-normalized-double-float)
                           most-positive-double-float (cl:- most-positive-double-float 1d292)
                           1d300 1d-300 (cl:+ 1d0 double-float-epsilon) (cl:- 1d0 double-float-epsilon)
                           vm.impl::*dinf* vm.impl::*dnan*)
                     (list 0f0 -0f0 1f0 4f0 2f0 0.5f0 least-positive-single-float (* 3 least-positive-single-float)
                           least-positive-normalized-single-float (cl:* 0.9f0 least-positive-normalized-single-float)
                           most-positive-single-float 1f30 1f-30 (cl:+ 1f0 single-float-epsilon)
                           (cl:- 1f0 single-float-epsilon)
                           vm.impl::*sinf* vm.impl::*snan*)))
           (a (float-array-of type vals))
           (r (vm:sqrt a)))
      (assert-equal (array-element-type a) (array-element-type r))
      (loop for x in vals for i from 0
            do (assert-true (same-bits-p (aref r i) (masked-cl #'cl:sqrt x)) type x (aref r i))))))

;;; ---------------------------------------------------------------------------
;;; non-simple and odd arrays

(deftest tc-non-simple-and-odd-arrays (transcendental-tests)
  (let* ((base (darr 1d0 4d0 9d0 16d0))
         (disp (make-array 2 :element-type 'double-float :displaced-to base :displaced-index-offset 1))
         (fp (make-array 4 :element-type 'double-float :fill-pointer 2 :initial-contents '(1d0 4d0 9d0 16d0))))
    (assert-equal (list 2d0 3d0) (contents (vm:sqrt disp)))
    (assert-equal (list 1d0 2d0) (contents (vm:sqrt fp)))
    (assert-equal '(2 2) (array-dimensions (vm:sqrt (make-array '(2 2) :element-type 'double-float :initial-element 4d0))))
    (assert-equal nil (array-dimensions (vm:sqrt (make-array nil :element-type 'double-float :initial-element 4d0))))
    (assert-equal 0 (length (vm:sqrt (darr))))
    (assert-equal 'double-float (result-et (vm:sqrt (darr))))
    (assert-equal '(complex double-float) (result-et (vm:exp (cdarr))))
    (assert-equal 'single-float (result-et (vm:exp (iarr '(signed-byte 64)))))
    (assert-equal '(signed-byte 64) (result-et (vm:abs (iarr '(signed-byte 64)))))
    (assert-equal 0 (length (vm:expt (darr) 2)))
    (assert-equal 0 (length (vm:atan (darr) (darr)))))
  ;; arrays that cannot hold numbers
  (dolist (op (list #'vm:exp #'vm:abs #'vm:sqrt #'vm:signum #'vm:phase #'vm:conjugate))
    (assert-true (typep (condition-of (funcall op "abc")) 'type-error))
    (assert-true (typep (condition-of (funcall op (varr 1 'a))) 'type-error)))
  ;; a type is still a type
  (assert-true (typep #c(1d0 2d0) 'vm:complex))
  (assert-true (typep #c(1d0 2d0) '(vm:complex double-float)))
  (assert-false (typep 1d0 'vm:complex)))

;;; ---------------------------------------------------------------------------
;;; the ulp table (run last: it prints what the other tests recorded)

(deftest tc-compiler-macros (transcendental-tests)
  "The compiler macros of the new operators: numbers as CL, arrays through the general function,
evaluation order, wrong arities left to the runtime error, :into."
  (flet ((c (form &optional (args '(a))) (compile nil `(lambda ,args ,form))))
    ;; declared numbers: CL's value
    (assert-equal (cl:sqrt 2d0) (funcall (c '(vm:sqrt a)) 2d0))
    (assert-equal (cl:sqrt 2d0) (funcall (compile nil '(lambda (a) (declare (double-float a)) (vm:sqrt a))) 2d0))
    (assert-equal (cl:abs -3) (funcall (compile nil '(lambda (a) (declare (fixnum a)) (vm:abs a))) -3))
    (assert-equal (cl:expt 2d0 3) (funcall (c '(vm:expt a 3)) 2d0))
    (assert-equal (cl:log 8 2) (funcall (c '(vm:log a 2)) 8))
    (assert-equal (cl:atan 1 2) (funcall (c '(vm:atan a 2)) 1))
    (assert-equal (cl:complex 1 2) (funcall (c '(vm:complex a 2)) 1))
    ;; arrays through the same compiled code
    (assert-equal (list 2d0 3d0) (contents (funcall (c '(vm:sqrt a)) (darr 4d0 9d0))))
    (assert-equal (list 4d0 9d0) (contents (funcall (c '(vm:expt a 2)) (darr 2d0 3d0))))
    (assert-equal (list 1d0 2d0) (contents (funcall (c '(vm:abs a)) (darr -1d0 2d0))))
    (assert-equal (list 1d0 2d0) (contents (funcall (compile nil '(lambda (a) (vm:abs (the (simple-array double-float (*)) a))))
                                                  (darr -1d0 2d0))))
    ;; :into
    (let ((out (darr 0d0 0d0)))
      (funcall (c '(vm:sqrt a :into o) '(a o)) (darr 4d0 9d0) out)
      (assert-equal (list 2d0 3d0) (contents out)))
    ;; left-to-right, once
    (let ((trace '()))
      (flet ((note (x) (push x trace) x))
        (declare (notinline note))
        (assert-equal 8 (funcall (compile nil '(lambda (note) (vm:expt (funcall note 2) (funcall note 3)))) #'note))
        (assert-equal '(3 2) trace)))
    ;; wrong argument counts: the runtime error, as in CL
    (dolist (form '((vm:exp) (vm:exp 1 2) (vm:expt 1) (vm:expt 1 2 3) (vm:log) (vm:log 1 2 3) (vm:atan 1 2 3) (vm:complex)
                    (vm:abs) (vm:isqrt 4 5)))
      (let ((v (condition-of (funcall (compile nil `(lambda () ,form)))))
            (cform (cons (find-symbol (symbol-name (first form)) :cl) (rest form))))
        (assert-true (typep v 'program-error) form)
        (assert-true (typep (condition-of (funcall (compile nil `(lambda () ,cform)))) 'program-error) form)))
    ;; non-numbers
    (assert-true (typep (condition-of (funcall (c '(vm:sqrt a)) 'x)) 'type-error))
    (assert-true (typep (condition-of (funcall (c '(vm:abs a)) "abc")) 'type-error))))

(deftest tc-funcall-apply-and-symbol-identity (transcendental-tests)
  (assert-equal (cl:sqrt 4d0) (funcall #'vm:sqrt 4d0))
  (assert-equal (cl:expt 2 10) (apply #'vm:expt '(2 10)))
  (assert-equal (list 2d0) (contents (funcall 'vm:sqrt (darr 4d0))))
  (assert-equal (list (cl:atan 1d0 0d0)) (contents (apply #'vm:atan (list (darr 1d0) 0d0 :into (darr 5d0))))))

(deftest tc-shadowing-package-end-to-end (transcendental-tests)
  "In a package that shadows every operator (including COMPLEX, which is also a type and a class)."
  (let ((p (make-user-package)))
    (flet ((ev (string) (eval-in p string)))
      (assert-true (eq (find-symbol "SQRT" p) 'vm:sqrt))
      (assert-true (eq (find-symbol "COMPLEX" p) 'vm:complex))
      (assert-equal 3 (ev "(abs -3)"))
      (assert-equal 2d0 (ev "(sqrt 4d0)"))
      (assert-equal #c(1 2) (ev "(complex 1 2)"))
      (assert-equal 1 (ev "(complex 1 0)"))
      (assert-equal '(2d0 3d0) (coerce (ev "(sqrt (make-array 2 :element-type 'double-float :initial-contents '(4d0 9d0)))") 'list))
      (assert-equal '(#c(0d0 1d0) #c(2d0 0d0))
                    (coerce (ev "(sqrt (make-array 2 :element-type 'double-float :initial-contents '(-1d0 4d0)))") 'list))
      ;; the type and the class named COMPLEX
      (assert-true (ev "(typep #c(1 2) 'complex)"))
      (assert-false (ev "(typep 1 'complex)"))
      (assert-true (ev "(typep #c(1d0 2d0) '(complex double-float))"))
      (assert-false (ev "(typep #c(1 2) '(complex double-float))"))
      (assert-true (ev "(typep #c(1d0 2d0) '(complex *))"))
      (assert-equal #c(1d0 0d0) (ev "(coerce 1 '(complex double-float))"))
      (assert-true (ev "(subtypep 'complex 'number)"))
      (assert-equal :c (ev "(progn (defgeneric ccm-foo (x)) (defmethod ccm-foo ((x complex)) :c) (ccm-foo #c(1 2)))"))
      (assert-equal :c (ev "(progn (defmethod ccm-foo ((x (eql 5))) :five) (ccm-foo #c(1d0 2d0)))"))
      (assert-equal '(complex double-float)
                    (ev "(array-element-type (make-array 2 :element-type '(complex double-float)))"))
      (assert-equal 'double-float (ev "(upgraded-complex-part-type 'double-float)"))
      (assert-equal "COMPLEX" (ev "(string (class-name (find-class 'complex)))"))
      ;; REPL variables and scalar code are untouched
      (assert-equal 4d0 (ev "(let ((x 2d0)) (declare (double-float x)) (expt x 2))")))))

(defun report-ulp-table ()
  (let ((rows '()))
    (maphash (lambda (k v) (push (cons k v) rows)) *ulp-table*)
    (setf rows (sort rows (lambda (a b) (string< (format nil "~A ~A" (caar a) (cadar a))
                                                 (format nil "~A ~A" (caar b) (cadar b))))))
    (format t "~&ULP-TABLE (max observed vs cl:op; reals in ulps, complex in eps*|z| units)~%")
    (dolist (r rows)
      (format t "ULP ~10A ~26A ~,2F~%" (caar r) (cadar r) (cdr r)))))
