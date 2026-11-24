;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Division and rounding: FLOOR CEILING TRUNCATE ROUND FFLOOR FCEILING FTRUNCATE FROUND MOD REM
;;; GCD LCM.  The reference is always CL's own function (cl:floor ...) applied per element.
;;; In this package + - * / 1+ 1- are the VM operators: CL's are written cl:+ etc.
(in-package #:vm-tests)

(defsuite division-tests (vm-tests))

(defparameter *div-names* '("floor" "ceiling" "truncate" "round" "ffloor" "fceiling" "ftruncate" "fround" "mod" "rem"))
(defparameter *int-q-names* '("floor" "ceiling" "truncate" "round"))
(defparameter *float-q-names* '("ffloor" "fceiling" "ftruncate" "fround"))
(defparameter *one-valued-names* '("mod" "rem"))

(defun vm-op (name) (symbol-function (find-symbol (string-upcase name) :vm)))
(defun cl-op (name) (symbol-function (find-symbol (string-upcase name) :cl)))

;;; --- outcomes: values, or a description of the condition ---

(defun describe-condition (c)
  (typecase c
    (type-error (list (type-of c) (if (arrayp (type-error-datum c)) :array (type-error-datum c))
                      (type-error-expected-type c)))
    (arithmetic-error (list (type-of c) (arithmetic-error-operation c)
                            (mapcar (lambda (x) (if (arrayp x) :array x)) (arithmetic-error-operands c))))
    (t (list (type-of c)))))

(defun outcome (thunk)
  (handler-case (cons :values (multiple-value-list (funcall thunk)))
    (error (c) (cons :condition (describe-condition c)))))

(defun same-thing-p (a b)
  (cond ((and (consp a) (consp b)) (and (same-thing-p (car a) (car b)) (same-thing-p (cdr a) (cdr b))))
        (t (same-bits-p a b))))

(defun same-outcome-p (a b) (same-thing-p a b))

(defmacro with-masked (&body body)
  `(vm.impl::with-masked-float-traps ,@body))

;;; --- the corpora ---

(defparameter *int64-corpus*
  '(0 1 -1 2 -2 3 -3 5 7 -7 10 -10 100 255 -1000 123456789 -123456789 1000000007
    4611686018427387903 -4611686018427387904 9223372036854775807 -9223372036854775808))

(defun double-corpus ()
  (append '(0d0 -0d0 0.1d0 -0.1d0 0.5d0 -0.5d0 1.5d0 -1.5d0 2.5d0 -2.5d0 3.5d0 -3.5d0 0.3d0 0.7d0
            1d0 -1d0 2d0 -2d0 3d0 -3d0 7.25d0 -7.25d0 10d0 100.75d0
            1d10 1d15 4503599627370495.5d0 4503599627370496d0 9007199254740991d0 9007199254740992d0
            1d17 3.9d18 4.2d18 -4.2d18 1d19 1d22 1d300 -1d300 1d-300 5d-324 -5d-324
            1.7976931348623157d308 2.220446049250313d-16)
          (loop for e in '(20 40 51 52) append (list (cl:+ (cl:expt 2d0 e) 0.5d0) (cl:- (cl:+ (cl:expt 2d0 e) 0.5d0))))))

(defun single-corpus ()
  '(0f0 -0f0 0.1f0 -0.1f0 0.5f0 -0.5f0 1.5f0 -1.5f0 2.5f0 -2.5f0 3.5f0 0.3f0 1f0 -1f0 2f0 3f0 -3f0 7.25f0
    16777215f0 16777216f0 4194304.5f0 8388608.5f0 1f10 1f18 4.2f18 1f30 3.4028235f38 1f-30 1.4012985f-45 -1f10))

(defparameter *ratio-corpus* '(1/2 -1/2 1/3 -1/3 7/2 -7/2 5/2 -5/2 2/3 22/7 1/100000000000000000000))

(defparameter *bignum-corpus*
  (list (expt 2 64) (- (expt 2 64)) (expt 10 30) (- (expt 10 30)) (1+ (expt 2 63)) (- (1- (- (expt 2 63))) 1)
        (expt 3 50)))

(defun sb64-p (x) (typep x '(signed-byte 64)))

(defun fits-et-p (et x)
  (typep x et))

;;; --- one case: operation NAME on arrays of element types A-ET and B-ET built from the pairs ---

(defun ref-ok-pairs (name xs ys)
  "The pairs (x y . values) of XS x YS for which CL's NAME returns (traps as they are, unmasked)."
  (let ((f (cl-op name)) (out '()))
    (dolist (x xs (nreverse out))
      (dolist (y ys)
        (let ((o (outcome (lambda () (funcall f x y)))))
          (when (eq (car o) :values) (push (list* x y (cdr o)) out)))))))

(defun all-p (pred list) (every pred list))

(defun contagion-class (et)
  (cond ((eq et 'double-float) :double)
        ((eq et 'single-float) :single)
        ((eq et t) :t)
        ((subtypep et 'integer) :int)
        (t :other)))

(defun expected-element-types (name a-et b-et qs rs)
  "(values quotient-et remainder-et) the results must have for operand element types A-ET, B-ET and the
reference quotient and remainder lists."
  (let* ((ca (contagion-class a-et)) (cb (contagion-class b-et))
         (any-t (or (eq ca :t) (eq cb :t)))
         (tightest (lambda (vals)
                     (cond ((null vals) t)
                           ((all-p (lambda (v) (typep v 'double-float)) vals) 'double-float)
                           ((all-p (lambda (v) (typep v 'single-float)) vals) 'single-float)
                           (t t))))
         (rcontagion (cond (any-t (funcall tightest rs))
                           ((or (eq ca :double) (eq cb :double)) 'double-float)
                           ((or (eq ca :single) (eq cb :single)) 'single-float)
                           ((and (subtypep a-et 'integer) (subtypep b-et 'integer))
                            (if (all-p #'sb64-p rs) '(signed-byte 64) t))
                           (t t))))
    (values (cond ((member name *int-q-names* :test #'string=)
                   (if (all-p #'sb64-p qs) '(signed-byte 64) t))
                  ((member name *float-q-names* :test #'string=)
                   (cond (any-t (funcall tightest qs))
                         ((member rcontagion '(double-float single-float)) rcontagion)
                         (t 'single-float)))
                  (t nil))
            rcontagion)))

(defun scalar-et (x)
  "The element type that stands for the scalar X in the expectations."
  (typecase x (double-float 'double-float) (single-float 'single-float) (integer 'integer) (t t)))

(defun eq-et (a b) (equal (upgraded-array-element-type a) (upgraded-array-element-type b)))

(defun check-pairs (name a-et b-et pairs &key scalar-b scalar-a typed-key)
  "Apply the VM operator NAME to arrays built from PAIRS ((x y . values) ...) and compare every
element of both results, and the element types, with CL's values.  A list of mismatch descriptions."
  (let* ((n (length pairs))
         (xs (mapcar #'first pairs)) (ys (mapcar #'second pairs))
         (qs (mapcar #'third pairs)) (rs (mapcar (lambda (p) (if (member name *one-valued-names* :test #'string=)
                                                                 (third p) (fourth p)))
                                                 pairs))
         (one (member name *one-valued-names* :test #'string=))
         (bad '()))
    (when scalar-b
      ;; array op scalar: the scalar is the first y, and the reference is recomputed
      (let ((y (first ys)) (f (cl-op name)))
        (setf pairs (loop for x in xs
                          collect (let ((v (outcome (lambda () (funcall f x y)))))
                                    (and (eq (car v) :values) (or (null typed-key) (ordinary-pair-p name typed-key x y))
                                         (list* x y (cdr v))))))
        (setf pairs (remove nil pairs) n (length pairs)
              xs (mapcar #'first pairs) ys (mapcar #'second pairs)
              qs (mapcar #'third pairs)
              rs (mapcar (lambda (p) (if one (third p) (fourth p))) pairs))))
    (when scalar-a
      (let ((x (first xs)) (f (cl-op name)))
        (setf pairs (loop for y in ys
                          collect (let ((v (outcome (lambda () (funcall f x y)))))
                                    (and (eq (car v) :values) (or (null typed-key) (ordinary-pair-p name typed-key x y))
                                         (list* x y (cdr v))))))
        (setf pairs (remove nil pairs) n (length pairs)
              xs (mapcar #'first pairs) ys (mapcar #'second pairs)
              qs (mapcar #'third pairs)
              rs (mapcar (lambda (p) (if one (third p) (fourth p))) pairs))))
    (when (zerop n) (return-from check-pairs nil))
    (let* ((a (make-arr a-et n xs)) (b (make-arr b-et n ys))
           (arg-a (if scalar-a (first xs) a)) (arg-b (if scalar-b (first ys) b))
           (a-et (if scalar-a (scalar-et (first xs)) a-et))
           (b-et (if scalar-b (scalar-et (first ys)) b-et))
           (vals (multiple-value-list (funcall (vm-op name) arg-a arg-b))))
      (multiple-value-bind (qet ret) (expected-element-types name a-et b-et (if one nil qs) rs)
        (if one
            (progn
              (unless (= (length vals) 1) (push (list :value-count (length vals)) bad))
              (let ((r (first vals)))
                (unless (eq-et (array-element-type r) ret)
                  (push (list :r-type (array-element-type r) :expected ret) bad))
                (loop for i below n for e in rs
                      unless (same-bits-p (row-major-aref r i) e)
                        do (push (list :r i (nth i xs) (nth i ys) (row-major-aref r i) e) bad))))
            (progn
              (unless (= (length vals) 2) (push (list :value-count (length vals)) bad))
              (destructuring-bind (q r) vals
                (unless (eq-et (array-element-type q) qet)
                  (push (list :q-type (array-element-type q) :expected qet) bad))
                (unless (eq-et (array-element-type r) ret)
                  (push (list :r-type (array-element-type r) :expected ret) bad))
                (loop for i below n for e in qs for f in rs
                      do (unless (same-bits-p (row-major-aref q i) e)
                           (push (list :q i (nth i xs) (nth i ys) (row-major-aref q i) e) bad))
                         (unless (same-bits-p (row-major-aref r i) f)
                           (push (list :r i (nth i xs) (nth i ys) (row-major-aref r i) f) bad)))))))
      (when bad (setf bad (cons (list name a-et b-et (if scalar-a :scalar-a (if scalar-b :scalar-b :arrays)))
                                (subseq (nreverse bad) 0 (min 4 (length bad))))))
      bad)))

(defun ordinary-pair-p (name key x y)
  "True when the typed kernel of KEY accepts the pair (x, y) for the operation NAME."
  (if (eq key :signed-byte-64)
      (and (/= y 0) (not (and (= y -1) (= x -9223372036854775808))))
      (and (/= y 0)
           (< (abs (cl:/ (rational x) (rational y)))
              (cond ((member name *int-q-names* :test #'string=) 4d18)
                    ((eq key :single-float) (rational most-positive-single-float))
                    (t (rational most-positive-double-float))))
           (<= (abs y) (if (eq key :single-float) most-positive-single-float most-positive-double-float)))))

(defun product-pairs (name xs ys &optional typed-key)
  (let ((pairs (ref-ok-pairs name xs ys)))
    (if typed-key
        (remove-if-not (lambda (p) (ordinary-pair-p name typed-key (first p) (second p))) pairs)
        pairs)))

(defun run-case (a-et b-et xs ys &key (names *div-names*) typed-key)
  "Check every operation on arrays, array-scalar and scalar-array shapes; the list of all mismatches.
TYPED-KEY: only pairs the typed kernel of that key accepts are used, and it must be the kernel that runs."
  (let ((bad '()))
    (dolist (name names)
      (let ((pairs (product-pairs name xs ys typed-key)))
        (when pairs
          (let* ((results '())
                 (thunk (lambda ()
                          (let ((r (check-pairs name a-et b-et pairs)))
                            (when r (push r results)))
                          ;; scalar divisor: the first y; scalar number: the first x
                          (let ((r (check-pairs name a-et b-et pairs :scalar-b t :typed-key typed-key)))
                            (when r (push r results)))
                          (let ((r (check-pairs name a-et b-et pairs :scalar-a t :typed-key typed-key)))
                            (when r (push r results)))))
                 (trace (if typed-key
                            (traced-kernel-results name typed-key thunk)
                            (progn (funcall thunk) nil))))
            (dolist (r results) (push r bad))
            (when (and typed-key (not (and trace (every #'identity trace))))
              (push (list :not-typed name typed-key trace) bad))))))
    (nreverse bad)))

;;; ---------------------------------------------------------------------------
;;; scalars

(deftest division-scalars-identical (division-tests)
  "Numbers: the same values, types and conditions as CL, for every operator, with the traps as they
are and masked; including arity."
  (let* ((corpus (append *int64-corpus* *bignum-corpus* *ratio-corpus*
                         '(0d0 -0d0 0.5d0 1.5d0 2.5d0 -2.5d0 3d0 0.1d0 1d300 -1d300 1d-300
                           4503599627370496d0 1d19 1.7976931348623157d308
                           0f0 -0f0 0.5f0 1.5f0 2.5f0 3f0 16777216f0 1f30)
                         (list (inf+) (inf-) (nan) (single-inf+) (single-nan) #c(1 2) #c(1d0 2d0) 'a "x")))
         (bad '()))
    (dolist (name *div-names*)
      (let ((v (vm-op name)) (c (cl-op name)))
        (dolist (x corpus)
          (dolist (y corpus)
            (unless (same-outcome-p (outcome (lambda () (funcall v x y))) (outcome (lambda () (funcall c x y))))
              (push (list name x y) bad))
            (unless (same-outcome-p (outcome (lambda () (with-masked (funcall v x y))))
                                    (outcome (lambda () (with-masked (funcall c x y)))))
              (push (list :masked name x y) bad)))
          (unless (member name *one-valued-names* :test #'string=)
            (unless (same-outcome-p (outcome (lambda () (funcall v x))) (outcome (lambda () (funcall c x))))
              (push (list name x) bad))))))
    (assert-equal nil (subseq bad 0 (min 6 (length bad))))))

(deftest division-arity (division-tests)
  (dolist (name *div-names*)
    (let ((v (vm-op name)) (c (cl-op name)))
      (dolist (args '(() (1) (1 2) (1 2 3) (1.5d0 2 3 4)))
        (assert-true (same-outcome-p (outcome (lambda () (apply v args))) (outcome (lambda () (apply c args))))))
      ;; with an array the arity errors are CL's too
      (let ((a (darr 1d0 2d0)))
        (dolist (args (list (list a 1 2) (list 1 a 2) (list a 2 3 4)))
          (assert-true (typep (condition-of (apply v args)) 'program-error)))
        (when (member name *one-valued-names* :test #'string=)
          (assert-true (typep (condition-of (funcall v a)) 'program-error)))
        (when (member name *int-q-names* :test #'string=)
          (assert-true (typep (condition-of (apply v (list a 1 2))) 'program-error))))))
  (dolist (name '("gcd" "lcm"))
    (let ((v (vm-op name)) (c (cl-op name)))
      (dolist (args '(() (7) (-7) (4 6) (4 6 10) (0 0) (0 5) (-4 6) (1.5d0) (1.5d0 2) (4 1.5d0) (a 2)))
        (assert-true (same-outcome-p (outcome (lambda () (apply v args))) (outcome (lambda () (apply c args)))))))))

(deftest division-multiple-values-scalar (division-tests)
  (assert-equal '(3 1) (multiple-value-list (vm:floor 7 2)))
  (assert-equal '(-4 1) (multiple-value-list (vm:floor -7 2)))
  (assert-equal '(4 -1) (multiple-value-list (vm:ceiling 7 2)))
  (assert-equal '(-3 -1) (multiple-value-list (vm:truncate -7 2)))
  (assert-equal '(4 -1) (multiple-value-list (vm:round 7 2)))
  (assert-equal '(2 1/2) (multiple-value-list (vm:round 5/2 1)))
  (assert-equal 1 (vm:mod -7 4))
  (assert-equal -3 (vm:rem -7 4))
  (assert-equal 6 (vm:gcd 12 18 30))
  (assert-equal 36 (vm:lcm 12 18)))

;;; ---------------------------------------------------------------------------
;;; arrays against cl: per element

(deftest division-typed-kernels-match-cl (division-tests)
  "Only pairs the typed kernels accept: the typed kernels themselves run, and agree with CL."
  (assert-equal nil (run-case '(signed-byte 64) '(signed-byte 64) *int64-corpus* *int64-corpus* :typed-key :signed-byte-64))
  (assert-equal nil (run-case 'double-float 'double-float (double-corpus) (double-corpus) :typed-key :double-float))
  (assert-equal nil (run-case 'single-float 'single-float (single-corpus) (single-corpus) :typed-key :single-float))
  (assert-equal nil (run-case 'double-float 'single-float (double-corpus) (single-corpus) :typed-key :double-float))
  (assert-equal nil (run-case 'single-float 'double-float (single-corpus) (double-corpus) :typed-key :double-float))
  (let ((small '(0 1 -1 2 -2 3 7 -7 10 255 -1000 16777216 -16777216 9007199254740992 -9007199254740992)))
    (assert-equal nil (run-case 'double-float '(signed-byte 64) (double-corpus) small :typed-key :double-float))
    (assert-equal nil (run-case '(signed-byte 64) 'double-float small (double-corpus) :typed-key :double-float))
    (assert-equal nil (run-case 'single-float '(signed-byte 64) (single-corpus) '(0 1 -1 2 3 -7 255 16777216 -16777216)
                                :typed-key :single-float))))

(deftest division-integer-arrays (division-tests)
  (assert-equal nil (run-case '(signed-byte 64) '(signed-byte 64) *int64-corpus* *int64-corpus*))
  (assert-equal nil (run-case '(signed-byte 32) '(signed-byte 8) '(0 1 -1 2 100 -100 2000000 -2000000000 2147483647)
                              '(1 -1 2 3 -5 7 127 -128)))
  (assert-equal nil (run-case '(unsigned-byte 8) '(unsigned-byte 16) '(0 1 2 3 7 100 255) '(1 2 3 255 1000 65535)))
  (assert-equal nil (run-case 'fixnum '(unsigned-byte 2) '(0 -1 5 1000 -1000 576460752303423487 -576460752303423488) '(1 2 3))))

(deftest division-integer-arrays-beyond-64-bits (division-tests)
  "Operands that do not fit (signed-byte 64) and results that do not: generic path, element type T."
  (assert-equal nil (run-case '(unsigned-byte 64) '(signed-byte 64) '(0 1 2 9223372036854775808 18446744073709551615)
                              '(1 -1 2 3 -5 7 9223372036854775807)))
  (assert-equal nil (run-case t t (append *int64-corpus* *bignum-corpus*) (append *int64-corpus* *bignum-corpus*)))
  (assert-equal nil (run-case t '(signed-byte 64) (append *int64-corpus* *bignum-corpus*) *int64-corpus*))
  ;; the most negative / -1: the quotient does not fit
  (let ((q (vm:floor (iarr '(signed-byte 64) -9223372036854775808 5) -1)))
    (assert-true (array-is q t 2 '(9223372036854775808 -5))))
  (let ((q (vm:truncate (iarr '(signed-byte 64) -9223372036854775808) (iarr '(signed-byte 64) -1))))
    (assert-true (array-is q t 1 '(9223372036854775808)))))

(deftest division-double-arrays (division-tests)
  (assert-equal nil (run-case 'double-float 'double-float (double-corpus) (double-corpus)))
  ;; the same data in T arrays takes the generic path: the same result
  (assert-equal nil (run-case t t (double-corpus) (double-corpus))))

(deftest division-single-arrays (division-tests)
  (assert-equal nil (run-case 'single-float 'single-float (single-corpus) (single-corpus)))
  (assert-equal nil (run-case t t (single-corpus) (single-corpus))))

(deftest division-mixed-float-arrays (division-tests)
  (assert-equal nil (run-case 'single-float 'double-float (single-corpus) (double-corpus)))
  (assert-equal nil (run-case 'double-float 'single-float (double-corpus) (single-corpus))))

(deftest division-float-integer-mixes (division-tests)
  "Floats with integers: within the exact range the typed kernels, beyond it the generic path."
  (let ((small '(0 1 -1 2 -2 3 7 -7 10 255 -1000 16777216 -16777216 9007199254740992 -9007199254740992))
        (all *int64-corpus*))
    (assert-equal nil (run-case 'double-float '(signed-byte 64) (double-corpus) small))
    (assert-equal nil (run-case 'double-float '(signed-byte 64) (double-corpus) all))
    (assert-equal nil (run-case '(signed-byte 64) 'double-float all (double-corpus)))
    (assert-equal nil (run-case 'single-float '(signed-byte 64) (single-corpus) small))
    (assert-equal nil (run-case 'single-float '(signed-byte 64) (single-corpus) all))
    (assert-equal nil (run-case '(signed-byte 64) 'single-float all (single-corpus)))
    (assert-equal nil (run-case 'double-float '(unsigned-byte 8) (double-corpus) '(1 2 3 255)))
    (assert-equal nil (run-case 'single-float '(unsigned-byte 8) (single-corpus) '(1 2 3 255)))))

(deftest division-rationals-and-generic (division-tests)
  (let ((mixed (append *int64-corpus* *bignum-corpus* *ratio-corpus*
                       '(0.5d0 -2.5d0 3d0 1d300 0.1f0 2.5f0 1f10))))
    (assert-equal nil (run-case t t mixed mixed))
    (assert-equal nil (run-case t 'double-float mixed (double-corpus)))
    (assert-equal nil (run-case 'double-float t (double-corpus) mixed))
    (assert-equal nil (run-case t '(signed-byte 64) mixed *int64-corpus*))))

(deftest division-ties-and-large-floats (division-tests)
  "ROUND rounds to even at every magnitude (not half away from zero); large floats."
  (let ((xs (loop for e from 0 to 52
                  append (let ((h (cl:+ (cl:expt 2d0 e) 0.5d0)))
                           (list h (cl:- h) (cl:+ h 1d0) (cl:- (cl:+ h 1d0))))))
        (ys '(1d0 -1d0 2d0 0.5d0 3d0)))
    (assert-equal nil (run-case 'double-float 'double-float xs ys)))
  (assert-equal '(0 2 2 4 0 -2 -2 -4)
                (contents (vm:round (darr 0.5d0 1.5d0 2.5d0 3.5d0 -0.5d0 -1.5d0 -2.5d0 -3.5d0))))
  (assert-equal '(0 2 2 4 0 -2 -2 -4)
                (contents (vm:round (sarr 0.5f0 1.5f0 2.5f0 3.5f0 -0.5f0 -1.5f0 -2.5f0 -3.5f0))))
  (assert-equal '(2 1 2 -2) (contents (vm:round (iarr '(signed-byte 64) 3 5 7 -3) (iarr '(signed-byte 64) 2 4 4 2))))
  ;; the bignum quotient of a large float: element type T
  (let ((q (vm:floor (darr 1d300))))
    (assert-true (array-is q t 1 (list (cl:floor 1d300))))
    (assert-true (typep (aref q 0) 'bignum)))
  (assert-true (array-is (vm:floor (darr 1d300 3d0)) t 2 (list (cl:floor 1d300) 3)))
  (assert-true (array-is (vm:floor (darr 1d18 3d0)) '(signed-byte 64) 2 (list (cl:floor 1d18) 3))))

(defun check-one-argument (name et xs)
  (let* ((f (cl-op name))
         (ok (remove-if-not (lambda (x) (eq (car (outcome (lambda () (funcall f x)))) :values)) xs))
         (n (length ok)))
    (when (plusp n)
      (let* ((a (make-arr et n ok))
             (vals (multiple-value-list (funcall (vm-op name) a)))
             (bad '()))
        (loop for x in ok for i from 0
              do (let ((ref (multiple-value-list (funcall f x))))
                   (unless (and (same-bits-p (row-major-aref (first vals) i) (first ref))
                                (same-bits-p (row-major-aref (second vals) i) (second ref)))
                     (push (list name et x (row-major-aref (first vals) i) (row-major-aref (second vals) i) ref) bad))))
        (nreverse bad)))))

(deftest division-one-argument (division-tests)
  "(floor v) is (floor v 1): the same values as CL's one-argument call for every element."
  (let ((bad '()))
    (dolist (name (append *int-q-names* *float-q-names*))
      (dolist (case (list (list 'double-float (double-corpus)) (list 'single-float (single-corpus))
                          (list '(signed-byte 64) *int64-corpus*) (list t (append *int64-corpus* *bignum-corpus* *ratio-corpus* (double-corpus)))))
        (let ((r (check-one-argument name (first case) (second case))))
          (when r (setf bad (append bad (subseq r 0 (min 2 (length r)))))))))
    (assert-equal nil (subseq bad 0 (min 4 (length bad))))))

(deftest division-negative-divisors (division-tests)
  (assert-equal '(-4 -3 3 3) (contents (vm:floor (iarr '(signed-byte 64) 7 7 -7 -7) (iarr '(signed-byte 64) -2 -3 -2 -2))))
  (assert-equal nil (run-case '(signed-byte 64) '(signed-byte 64) '(7 -7 0 5 -5 6 -6) '(-2 -3 -1 2 3 -6 -4)))
  (assert-equal nil (run-case 'double-float 'double-float '(7d0 -7d0 0d0 5.5d0 -5.5d0 6d0 -6d0 0.2d0) '(-2d0 -3d0 -1d0 2d0 3d0 -6d0 -0.4d0 0.1d0))))

(deftest division-random-differential (division-tests)
  "A few thousand random pairs per type against CL."
  (let* ((next (lcg-stream 20260101))
         (ints (loop repeat 300 collect (cl:- (mod (funcall next) 2000001) 1000000)))
         (bigints (loop repeat 100 collect (cl:* (cl:- (mod (funcall next) 2001) 1000)
                                                 (mod (funcall next) 1000000007) (mod (funcall next) 1000003))))
         (ds (loop repeat 300 collect (cl:* (cl:/ (cl:- (mod (funcall next) 20001) 10000) 7d0)
                                            (cl:expt 10d0 (cl:- (mod (funcall next) 9) 3)))))
         (ss (loop repeat 200 collect (cl:* (cl:/ (cl:- (mod (funcall next) 20001) 10000) 7f0)
                                            (cl:expt 10f0 (cl:- (mod (funcall next) 7) 3))))))
    (assert-equal nil (run-case '(signed-byte 64) '(signed-byte 64) (append ints bigints) (append ints bigints)))
    (assert-equal nil (run-case 'double-float 'double-float ds ds))
    (assert-equal nil (run-case 'single-float 'single-float ss ss))
    (assert-equal nil (run-case 'double-float '(signed-byte 64) ds ints))))

;;; ---------------------------------------------------------------------------
;;; which backend runs

(defvar *kernel-results* nil)

(defun traced-kernel-results (name key thunk)
  "Call THUNK with the kernels (NAME KEY :aa :as :sa) traced; the list of their return values."
  (let ((origs (loop for v in '(:aa :as :sa) collect (cons v (vm:find-kernel name key v))))
        (results '()))
    (unwind-protect
         (progn
           (dolist (o origs)
             (let ((f (cdr o)))
               (vm:register-kernel name key (car o)
                                   (lambda (&rest args) (let ((r (apply f args))) (push (and r t) results) r)))))
           (funcall thunk))
      (dolist (o origs) (vm:register-kernel name key (car o) (cdr o))))
    (nreverse results)))

(deftest division-typed-kernels-run (division-tests)
  "Ordinary elements go through the typed kernel (one backend per type); a rejected element sends the
whole call to the generic path, with the same result."
  (dolist (name *div-names*)
    (flet ((typed (key a b)
             (traced-kernel-results name key (lambda () (ignore-errors (funcall (vm-op name) a b))))))
      (assert-equal '(t) (typed :signed-byte-64 (iarr '(signed-byte 64) 7 -9 11) (iarr '(signed-byte 64) 2 4 -3)))
      (assert-equal '(t) (typed :double-float (darr 7.5d0 -9.25d0 11d0) (darr 2d0 4d0 -3d0)))
      (assert-equal '(t) (typed :single-float (sarr 7.5f0 -9.25f0 11f0) (sarr 2f0 4f0 -3f0)))
      (assert-equal '(t) (typed :double-float (darr 7.5d0 -9.25d0 11d0) 2))
      (assert-equal '(t) (typed :double-float 2 (darr 7.5d0 -9.25d0 11d0)))
      (assert-equal '(t) (typed :double-float (sarr 7.5f0 -9.25f0 11f0) (darr 2d0 4d0 -3d0)))
      ;; a zero divisor, a non-finite number, a huge quotient: the kernel gives up (for the integer
      ;; family; the F family and MOD / REM give up on the non-finite and zero ones as well)
      (assert-equal '(nil) (typed :signed-byte-64 (iarr '(signed-byte 64) 7 9) 0))
      (assert-equal '(nil) (typed :double-float (darr 7d0 (inf+)) 2d0))
      (assert-equal '(nil) (typed :double-float (darr 7d0 (nan)) 2d0))
      (assert-equal '(nil) (typed :double-float (darr 7d0 1d0) (darr 2d0 0d0)))
      (assert-equal '(nil) (typed :double-float (darr 7d0 1d0) (inf+)))
      (assert-equal '(nil) (typed :single-float (sarr 7f0 1f0) (single-inf+))))
    ;; T arrays have no typed kernel
    (assert-equal '() (traced-kernel-results name :double-float
                                             (lambda () (funcall (vm-op name) (varr 7.5d0 1d0) (varr 2d0 3d0)))))))

(deftest division-huge-quotients-fall-back (division-tests)
  (assert-equal '(nil) (traced-kernel-results "floor" :double-float (lambda () (vm:floor (darr 1d22 1d0) 3d0))))
  (assert-true (array-is (vm:floor (darr 1d22 1d0) 3d0) t 2 (list (cl:floor 1d22 3d0) 0)))
  ;; the F family has no integer quotient: huge ones are ordinary
  (assert-equal '(t) (traced-kernel-results "ffloor" :double-float (lambda () (vm:ffloor (darr 1d22 1d0) 3d0)))))

(deftest division-integer-overflow-in-kernel (division-tests)
  (assert-equal '(nil) (traced-kernel-results "floor" :signed-byte-64
                                              (lambda () (vm:floor (iarr '(signed-byte 64) -9223372036854775808) -1))))
  (assert-equal '(nil) (traced-kernel-results "mod" :signed-byte-64
                                              (lambda () (vm:mod (iarr '(signed-byte 64) -9223372036854775808) -1))))
  (assert-equal 0 (aref (vm:mod (iarr '(signed-byte 64) -9223372036854775808) -1) 0)))

;;; ---------------------------------------------------------------------------
;;; zero divisors and non-finite values

(deftest division-integer-by-zero (division-tests)
  (dolist (name *div-names*)
    (dolist (args (list (list (iarr '(signed-byte 64) 1 2) 0)
                        (list (iarr '(signed-byte 64) 1 2) (iarr '(signed-byte 64) 1 0))
                        (list (varr 1 2 (expt 10 30)) 0)
                        (list 5 (iarr '(signed-byte 64) 0 1))))
      (let ((o (outcome (lambda () (apply (vm-op name) args)))))
        (assert-equal :condition (car o))
        (assert-equal 'division-by-zero (second o)))
      ;; also with the traps masked
      (let ((o (outcome (lambda () (with-masked (apply (vm-op name) args))))))
        (assert-equal 'division-by-zero (second o)))))
  (dolist (name '("gcd" "lcm")) (assert-equal :values (car (outcome (lambda () (funcall (vm-op name) (iarr 'fixnum 0) 0)))))))

;;; ---------------------------------------------------------------------------
;;; zero divisors and non-finite values

(defun special-element-values ()
  (list 0d0 -0d0 1d0 -1d0 2.5d0 -2.5d0 1d300 (inf+) (inf-) (nan) 5d-324))

(defun call-maybe-masked (masked thunk)
  (if masked (with-masked (funcall thunk)) (funcall thunk)))

(deftest division-float-int-family-matches-scalar-cl (division-tests)
  "FLOOR CEILING TRUNCATE ROUND of float arrays: for every pair, including zero divisors, infinities and
NaN, the array call signals exactly the condition CL signals for the scalars, or returns what CL returns;
with the traps as they are and masked."
  (let ((bad '()) (vals (special-element-values)))
    (dolist (name *int-q-names*)
      (dolist (x vals)
        (dolist (y (append vals '(0 3 -2)))
          (dolist (masked '(nil t))
            (let ((s (outcome (lambda ()
                                (call-maybe-masked masked
                                                   (lambda () (multiple-value-bind (q r) (funcall (cl-op name) x y)
                                                                (list q r)))))))
                  (a (outcome (lambda ()
                                (call-maybe-masked masked
                                                   (lambda () (multiple-value-bind (q r) (funcall (vm-op name) (darr x) y)
                                                                (list (aref q 0) (aref r 0)))))))))
              (unless (same-outcome-p s a) (push (list name x y masked s a) bad)))))))
    (assert-equal nil (subseq bad 0 (min 4 (length bad))))))

(deftest division-float-with-unconvertible-integer (division-tests)
  "A float element with an integer too big for the float format: CL's own error (or value), exactly,
with the traps as they are and masked, for every operator."
  (let ((bad '()) (big (expt 10 400)))
    (dolist (name *div-names*)
      (dolist (pair (list (list 1d0 big) (list 1d0 (- big)) (list big 1d0) (list 1f0 big) (list 1d0 (expt 10 30))))
        (destructuring-bind (x y) pair
          (dolist (masked '(nil t))
            (flet ((cl-side (a b) (call-maybe-masked masked (lambda () (multiple-value-list (funcall (cl-op name) a b)))))
                   (vm-side (a b)
                     (call-maybe-masked masked
                                        (lambda () (mapcar (lambda (r) (if (arrayp r) (aref r 0) r))
                                                           (multiple-value-list (funcall (vm-op name) a b)))))))
              (let ((c (outcome (lambda () (cl-side x y))))
                    (a (outcome (lambda () (vm-side (if (floatp x) (if (typep x 'single-float) (sarr x) (darr x)) (vector x))
                                                    (if (floatp y) (if (typep y 'single-float) (sarr y) (darr y)) y))))))
                ;; only the signalling cases and the unmasked values are comparable: masked F-family
                ;; results are IEEE by definition, so compare the conditions only there
                ;; The F family, MOD and REM convert the integer to the float format first (a float
                ;; division, no signal), so a signal of CL's own function for the pair (SBCL's
                ;; (rem 1f0 (expt 10 30)) overflows inside CL) does not occur when the conversion works.
                (when (and (or (eq (car c) :condition) (not masked))
                           (not (and (not (member name *int-q-names* :test #'string=))
                                     (eq (car c) :condition) (eq (car a) :values))))
                  (unless (same-outcome-p c a) (push (list name x y masked c a) bad)))))))))
    (assert-equal nil (subseq bad 0 (min 4 (length bad))))))

(deftest division-float-int-family-both-values-and-arrays (division-tests)
  "The conditions come out for any element, and for arrays on either side."
  (dolist (name *int-q-names*)
    (let ((f (vm-op name)))
      (dolist (args (list (list (darr 1d0 2d0) 0d0) (list (darr 1d0 2d0) (darr 1d0 0d0)) (list 1d0 (darr 0d0))
                          (list (sarr 1f0) 0f0) (list (darr (inf+)) 1d0) (list (darr 1d0) (inf+)) (list (darr (nan)) 1d0)
                          (list (darr 1d300) (darr 1d-300))))
        (let ((o (outcome (lambda () (apply f args)))) (r (outcome (lambda () (apply (cl-op name) (mapcar (lambda (x) (if (arrayp x) (aref x 0) x)) args))))))
          ;; element 0 is the offending one in the single-element cases and the first of two
          (when (and (every (lambda (x) (or (not (arrayp x)) (= 1 (length x)))) args))
            (assert-true (or (eq (car o) :condition) (eq (car r) :values)))
            (assert-true (same-outcome-p (cons (car o) (if (eq (car o) :values) (list (list (aref (first (cdr o)) 0) (aref (second (cdr o)) 0))) (cdr o)))
                                         (if (eq (car r) :values) (list :values (cdr r)) r)))))))))

(defun dbl (x) (case x (:inf (inf+)) (:-inf (inf-)) (:nan (nan)) (t (float x 1d0))))
(defun sgl (x) (case x (:inf (single-inf+)) (:-inf (cl:- (single-inf+))) (:nan (single-nan)) (t (float x 1f0))))

(defparameter *special-table*
  ;; (name x y quotient remainder): the documented IEEE results; :inf :-inf :nan; the quotient of the
  ;; one-valued MOD and REM is ignored
  '(("ffloor" 1 0 :inf :nan) ("ffloor" -1 0 :-inf :nan) ("ffloor" 0 0 :nan :nan) ("ffloor" 1 -0d0 :-inf :nan)
    ("ffloor" :inf 2 :inf :nan) ("ffloor" :-inf 2 :-inf :nan) ("ffloor" :inf :inf :nan :nan)
    ("ffloor" :-inf :inf :nan :nan) ("ffloor" :nan 2 :nan :nan) ("ffloor" 2 :nan :nan :nan)
    ("ffloor" 5 :inf 0d0 5) ("ffloor" -5 :inf -1 :inf) ("ffloor" 5 :-inf -1 :-inf) ("ffloor" -5 :-inf 0d0 -5)
    ("ffloor" 0 :inf 0d0 0d0) ("ffloor" -0d0 :inf -0d0 -0d0)
    ("fceiling" 5 :inf 1 :-inf) ("fceiling" -5 :inf -0d0 -5) ("fceiling" 5 :-inf -0d0 5) ("fceiling" -5 :-inf 1 :inf)
    ("fceiling" 1 0 :inf :nan) ("fceiling" :inf 1 :inf :nan)
    ("ftruncate" 5 :inf 0d0 5) ("ftruncate" -5 :inf -0d0 -5) ("ftruncate" -5 :-inf 0d0 -5) ("ftruncate" 3 0 :inf :nan)
    ("fround" 5 :inf 0d0 5) ("fround" -5 :inf -0d0 -5) ("fround" -3 0 :-inf :nan) ("fround" :nan :nan :nan :nan)
    ("mod" 5 :inf nil 5) ("mod" -5 :inf nil :inf) ("mod" 5 :-inf nil :-inf) ("mod" -5 :-inf nil -5)
    ("mod" 5 0 nil :nan) ("mod" :inf 3 nil :nan) ("mod" 3 :nan nil :nan) ("mod" :nan 3 nil :nan)
    ("rem" 5 :inf nil 5) ("rem" -5 :inf nil -5) ("rem" 5 0 nil :nan) ("rem" :-inf 3 nil :nan)
    ("rem" 3 :nan nil :nan) ("rem" 0 0 nil :nan)))

(deftest division-ieee-special-values (division-tests)
  "Float arrays, F family and MOD, REM: no signal, the IEEE results documented in IEEE-DIVISION-SPECIAL."
  (let ((bad '()))
    (dolist (row *special-table*)
      (destructuring-bind (name x y q r) row
        (dolist (type '(:double :single))
          (let* ((conv (if (eq type :double) #'dbl #'sgl))
                 (mk (if (eq type :double) #'darr #'sarr))
                 (f (vm-op name)) (one (member name *one-valued-names* :test #'string=)))
            ;; array / array, array / scalar (float and integer divisors), scalar / array
            (dolist (form (list (lambda () (funcall f (funcall mk (funcall conv x)) (funcall mk (funcall conv y))))
                                (lambda () (funcall f (funcall mk (funcall conv x)) (funcall conv y)))
                                (lambda () (funcall f (funcall conv x) (funcall mk (funcall conv y))))
                                (when (and (integerp y) (zerop y))
                                  (lambda () (funcall f (funcall mk (funcall conv x)) 0)))))
              (when form
                (let ((o (outcome (lambda () (multiple-value-list (funcall form))))))
                  (flet ((expect (value expected)
                           (let ((e (funcall conv expected)))
                             (unless (same-bits-p value e) (push (list row type value e) bad)))))
                    (if (eq (car o) :values)
                        (let ((vals (cadr o)))
                          (if one
                              (expect (aref (first vals) 0) r)
                              (progn (expect (aref (first vals) 0) q) (expect (aref (second vals) 0) r)))
                              (dolist (v vals) (unless (eq (array-element-type v) (if (eq type :double) 'double-float 'single-float))
                                                 (push (list row :type (array-element-type v)) bad))))
                        (push (list row type :signalled o) bad))))))))))
    (assert-equal nil (subseq bad 0 (min 5 (length bad))))))

(deftest division-ieee-overflowing-quotient (division-tests)
  "Finite x, y whose quotient overflows: quotient Inf, remainder exact (rational arithmetic)."
  (let ((bad '()))
    (dolist (pair '((1d300 1d-300) (-1d300 1d-300) (1d300 -1d-300) (1.7976931348623157d308 5d-324) (-1.5d308 3d-10)))
      (destructuring-bind (x y) pair
        (dolist (name '("ffloor" "fceiling" "ftruncate" "fround"))
          (multiple-value-bind (q r) (funcall (vm-op name) (darr x) (darr y))
            (let* ((mode (cond ((string= name "ffloor") #'floor) ((string= name "fceiling") #'ceiling)
                               ((string= name "ftruncate") #'truncate) (t #'round)))
                   (er (float (nth-value 1 (funcall mode (rational x) (rational y))) 1d0))
                   (exq (if (eql (float-sign x) (float-sign y)) (inf+) (inf-))))
              (unless (and (same-bits-p (aref q 0) exq) (same-bits-p (aref r 0) er))
                (push (list name x y (aref q 0) (aref r 0) exq er) bad)))))
        (dolist (name '("mod" "rem"))
          (let* ((r (aref (funcall (vm-op name) (darr x) (darr y)) 0))
                 (er (float (nth-value 1 (funcall (if (string= name "mod") #'floor #'truncate) (rational x) (rational y))) 1d0)))
            (unless (same-bits-p r er) (push (list name x y r er) bad))))))
    (assert-equal nil (subseq bad 0 (min 5 (length bad))))))

(deftest division-f-family-of-integers-and-rationals (division-tests)
  "Integers and rationals: single-float quotients as CL; a zero divisor still signals."
  (assert-true (array-is (vm:ffloor (iarr '(signed-byte 64) 7 -7) 2) 'single-float 2 (list (cl:ffloor 7 2) (cl:ffloor -7 2))))
  (assert-true (array-is (nth-value 1 (vm:ffloor (iarr '(signed-byte 64) 7 -7) 2)) '(signed-byte 64) 2 '(1 1)))
  (assert-true (array-is (vm:ffloor (varr 7/2 -7/2) 1) 'single-float 2 (list (cl:ffloor 7/2 1) (cl:ffloor -7/2 1))))
  (assert-equal 'division-by-zero (second (outcome (lambda () (vm:ffloor (iarr 'fixnum 1) 0)))))
  (assert-equal 'division-by-zero (second (outcome (lambda () (vm:mod (iarr 'fixnum 1) 0)))))
  ;; a float scalar zero with integers: a float division, IEEE
  (multiple-value-bind (q r) (vm:ffloor (iarr 'fixnum 1) 0d0)
    (assert-true (array-is q 'double-float 1 (list (inf+))))
    (assert-true (nanp (aref r 0)))))

;;; ---------------------------------------------------------------------------
;;; result types, empty arrays

(defmacro types (form)
  `(mapcar (lambda (a) (array-element-type a)) (multiple-value-list ,form)))

(deftest division-result-element-types (division-tests)
  (progn
    (assert-equal '((signed-byte 64) (signed-byte 64)) (types (vm:floor (iarr '(signed-byte 64) 7) 2)))
    (assert-equal '((signed-byte 64) double-float) (types (vm:floor (darr 7d0) 2)))
    (assert-equal '((signed-byte 64) single-float) (types (vm:floor (sarr 7f0) 2)))
    (assert-equal '((signed-byte 64) double-float) (types (vm:floor (sarr 7f0) 2d0)))
    (assert-equal '((signed-byte 64) single-float) (types (vm:floor 7f0 (iarr 'fixnum 2))))
    (assert-equal '(double-float double-float) (types (vm:ffloor (darr 7d0) 2)))
    (assert-equal '(single-float (signed-byte 64)) (types (vm:ffloor (iarr 'fixnum 7) 2)))
    (assert-equal '(single-float t) (types (vm:ffloor (varr 7/2) 2)))
    (assert-equal '((signed-byte 64) t) (types (vm:floor (varr 7/2) 2)))
    (assert-equal '((signed-byte 64) t) (types (vm:floor (varr 7) 2)))
    (assert-equal '((signed-byte 64) t) (types (vm:floor (varr (expt 10 30)) (expt 10 20))))
    (assert-equal '((signed-byte 64)) (types (vm:mod (iarr 'fixnum 7) 2)))
    (assert-equal '(double-float) (types (vm:mod (darr 7d0) 2)))
    (assert-equal '(t) (types (vm:mod (varr 7/2) 2)))))

(deftest division-empty-arrays (division-tests)
  (progn
    (assert-equal '((signed-byte 64) (signed-byte 64)) (types (vm:floor (make-array 0 :element-type '(signed-byte 64)) 2)))
    (assert-equal '((signed-byte 64) double-float) (types (vm:floor (make-array 0 :element-type 'double-float) 2)))
    (assert-equal '(double-float double-float) (types (vm:ffloor (make-array 0 :element-type 'double-float) 2d0)))
    (assert-equal '(single-float (signed-byte 64)) (types (vm:ffloor (make-array 0 :element-type '(signed-byte 64)) 2)))
    (assert-equal '((signed-byte 64) (signed-byte 64)) (types (vm:truncate (make-array 0 :element-type '(unsigned-byte 64)) 3)))
    (assert-equal '(single-float) (types (vm:mod 2 (make-array 0 :element-type 'single-float))))
    (assert-equal '((signed-byte 64)) (types (vm:rem (make-array 0 :element-type 'fixnum) (make-array 0 :element-type 'fixnum))))
    (assert-equal '(0) (array-dimensions (vm:mod (make-array 0 :element-type 'fixnum) (make-array 0 :element-type 'fixnum))))
    ;; no element, no error: not even for a zero divisor
    (assert-true (array-is (vm:mod (make-array 0 :element-type 'fixnum) 0) '(signed-byte 64) 0 '()))
    (assert-equal '(2 0) (array-dimensions (vm:floor (make-array '(2 0) :element-type 'double-float) 3)))))

;;; ---------------------------------------------------------------------------
;;; shapes

(defun ref-recycle-pairs (adims bdims)
  "The list of (ia . ib) flat (row-major) index pairs of the R recycling rules for operands of the
dimensions ADIMS, BDIMS, in row-major order of the result, with the result dimensions."
  (let* ((na (reduce #'cl:* adims)) (nb (reduce #'cl:* bdims))
         (ra (length adims)) (rb (length bdims)))
    (cond ((and (>= ra 2) (>= rb 2))
           (values (loop for i below na collect (cons i i)) adims))
          ((or (>= ra 2) (>= rb 2))
           (let* ((dims (if (>= ra 2) adims bdims)) (n (reduce #'cl:* dims)) (m (if (>= ra 2) nb na)))
             (values
              (loop for r below n
                    collect (let ((c (colmajor-index dims r)))
                              (if (>= ra 2) (cons r (mod c m)) (cons (mod c m) r))))
              dims)))
          (t (let ((n (max na nb)))
               (values (loop for r below n collect (cons (mod r na) (mod r nb))) (list n)))))))

(defun colmajor-index (dims r)
  (vm.impl::colmajor-order-index dims r))

(defun check-shapes (name a b)
  "Compare (NAME A B) for the arrays A, B with the per-element CL results under the R rules."
  (multiple-value-bind (pairs dims) (ref-recycle-pairs (array-dimensions a) (array-dimensions b))
    (let* ((one (member name *one-valued-names* :test #'string=))
           (vals (multiple-value-list (handler-bind ((vm:recycling-warning #'muffle-warning))
                                        (funcall (vm-op name) a b))))
           (bad '()))
      (dolist (v vals)
        (unless (equal (array-dimensions v) dims) (push (list :dims (array-dimensions v) dims) bad)))
      (loop for (ia . ib) in pairs for r from 0
            do (let* ((ref (multiple-value-list (funcall (cl-op name) (row-major-aref a ia) (row-major-aref b ib)))))
                 (if one
                     (unless (same-bits-p (row-major-aref (first vals) r) (first ref)) (push (list :r r) bad))
                     (progn (unless (same-bits-p (row-major-aref (first vals) r) (first ref)) (push (list :q r) bad))
                            (unless (same-bits-p (row-major-aref (second vals) r) (second ref)) (push (list :r r) bad))))))
      (when bad (list name (array-dimensions a) (array-dimensions b) (subseq bad 0 (min 3 (length bad))))))))

(deftest division-shapes-and-recycling (division-tests)
  (let ((bad '()))
    (dolist (name *div-names*)
      (flet ((try (a b) (let ((r (check-shapes name a b))) (when r (push r bad)))))
        (dolist (et '((signed-byte 64) double-float))
          (flet ((vec (n &optional (off 0)) (make-arr et n (loop for i below n collect (if (eq et 'double-float)
                                                                                         (float (cl:+ 7 (cl:* 3 (cl:- i 5)) off) 1d0)
                                                                                         (cl:+ 7 (cl:* 3 (cl:- i 5)) off)))))
                 (divs (n) (make-arr et n (loop for i below n collect (let ((v (cl:+ 2 (mod (cl:* i 5) 7)))) (if (eq et 'double-float) (float (if (oddp i) (cl:- v) v) 1d0) (if (oddp i) (cl:- v) v)))))))
            ;; equal length, a multiple, not a multiple (warning), one element, 16 and more (blocks)
            (try (vec 6) (divs 6)) (try (vec 6) (divs 2)) (try (divs 2) (vec 6)) (try (vec 5) (divs 2))
            (try (vec 1) (divs 6)) (try (vec 6) (divs 1))
            (try (vec 40) (divs 16)) (try (divs 16) (vec 40)) (try (vec 40) (divs 17)) (try (vec 32) (divs 16))
            ;; matrices with vectors: rows path (the vector as long as the first dimension, long rows),
            ;; column-major path, equal matrices
            (let ((m (make-arr et '(3 6) (loop for i below 18 collect (if (eq et 'double-float) (float (cl:+ 3 i) 1d0) (cl:+ 3 i)))))
                  (m2 (make-arr et '(2 20) (loop for i below 40 collect (if (eq et 'double-float) (float (if (oddp i) (cl:- (cl:1+ i)) (cl:1+ i)) 1d0) (if (oddp i) (cl:- (cl:1+ i)) (cl:1+ i)))))))
              (try m (divs 3)) (try (divs 3) m) (try m (divs 2)) (try (divs 6) m) (try m (divs 18)) (try m (divs 1))
              (try m2 (divs 2)) (try (divs 2) m2) (try m2 (divs 3)) (try m m)
              (try (make-arr et '(3 6) (loop for i below 18 collect (if (eq et 'double-float) (float (cl:+ 1 (mod i 4)) 1d0) (cl:+ 1 (mod i 4))))) m))))))
    (assert-equal nil (subseq bad 0 (min 3 (length bad))))))

(deftest division-recycling-warnings-and-errors (division-tests)
  (with-warnings (r ws) (vm:floor (iarr 'fixnum 1 2 3) (iarr 'fixnum 2 5))
    (assert-equal 1 (length (recycling-warnings ws))))
  (with-warnings (r ws) (vm:mod (iarr 'fixnum 1 2 3 4) (iarr 'fixnum 2 5))
    (assert-equal 0 (length (recycling-warnings ws))))
  ;; non-conformable
  (assert-true (typep (condition-of (vm:floor (make-arr 'double-float '(2 2) '(1d0 2d0 3d0 4d0))
                                              (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0))))
                      'vm:non-conformable-arrays))
  (assert-true (typep (condition-of (vm:mod (darr 1d0 2d0 3d0) (make-arr 'double-float '(1 2) '(1d0 2d0))))
                      'vm:non-conformable-arrays))
  ;; rank 0, and a vector with a fill pointer
  (let ((r0 (make-array nil :element-type 'double-float :initial-element 7.5d0)))
    (multiple-value-bind (q r) (vm:floor r0 2d0)
      (assert-equal nil (array-dimensions q))
      (assert-equal '(3 1.5d0) (list (aref q) (aref r)))))
  (let ((fp (make-array 4 :element-type 'double-float :initial-contents '(7d0 8d0 9d0 10d0) :fill-pointer 2)))
    (assert-equal '(3 4) (contents (vm:floor fp 2d0)))))

;;; ---------------------------------------------------------------------------
;;; :into, mutation

(deftest division-into-quotient-only (division-tests)
  (let* ((a (iarr '(signed-byte 64) 7 -7 9 10)) (b (iarr '(signed-byte 64) 2 2 4 3))
         (a0 (copy-arr a)) (b0 (copy-arr b))
         (out (make-array 4 :element-type '(signed-byte 64) :initial-element 99)))
    (multiple-value-bind (q r) (vm:floor a b :into out)
      (assert-true (eq q out))
      (assert-true (array-is out '(signed-byte 64) 4 '(3 -4 2 3)))
      (assert-true (not (eq r out)))
      (assert-true (array-is r '(signed-byte 64) 4 '(1 1 1 1)))
      (setf (aref r 0) 55)
      (assert-equal 3 (aref out 0)))
    (assert-true (same-contents-p a a0))
    (assert-true (same-contents-p b b0)))
  ;; one argument
  (let ((out (make-array 3 :element-type '(signed-byte 64) :initial-element 0)))
    (multiple-value-bind (q r) (vm:round (darr 0.5d0 1.5d0 2.6d0) :into out)
      (assert-true (eq q out))
      (assert-true (array-is out '(signed-byte 64) 3 '(0 2 3)))
      (assert-true (array-is r 'double-float 3 (list 0.5d0 -0.5d0 (cl:- 2.6d0 3)))))))

(deftest division-into-conversions-and-errors (division-tests)
  ;; integer quotient into a float or T array
  (let ((out (make-array 2 :element-type 'double-float :initial-element 0d0)))
    (vm:floor (iarr 'fixnum 7 9) 2 :into out)
    (assert-true (array-is out 'double-float 2 '(3d0 4d0))))
  (let ((out (make-array 2)))
    (vm:floor (darr 1d300 7d0) 3d0 :into out)
    (assert-equal (list (cl:floor 1d300 3d0) 2) (contents out)))
  ;; recycling into a longer array, truncation with a warning
  (let ((out (make-array 5 :element-type '(signed-byte 64) :initial-element 0)))
    (vm:floor (iarr 'fixnum 7 9) 2 :into out)
    (assert-equal '(3 4 3 4 3) (contents out)))
  (let ((out (make-array 1 :element-type '(signed-byte 64) :initial-element 0)))
    (with-warnings (r ws) (vm:floor (iarr 'fixnum 7 9) 2 :into out)
      (assert-equal 1 (length (recycling-warnings ws))))
    (assert-equal '(3) (contents out)))
  ;; the F family: a float quotient into a narrower float array is an error and leaves OUT alone
  (let* ((out (make-array 2 :element-type 'single-float :initial-element 0f0)) (before (copy-arr out)))
    (assert-true (typep (condition-of (vm:ffloor (darr 7d0 9d0) 2d0 :into out)) 'vm:output-type-error))
    (assert-true (same-contents-p out before)))
  (let ((out (make-array 2 :element-type 'double-float :initial-element 0d0)))
    (vm:ffloor (sarr 7f0 9f0) 2f0 :into out)
    (assert-true (array-is out 'double-float 2 '(3d0 4d0))))
  ;; MOD, REM, GCD, LCM: the result into OUT
  (let ((out (make-array 3 :element-type '(signed-byte 64) :initial-element 0)))
    (assert-true (eq out (vm:mod (iarr 'fixnum 7 -7 9) 4 :into out)))
    (assert-equal '(3 1 1) (contents out))
    (assert-true (eq out (vm:rem (iarr 'fixnum 7 -7 9) 4 :into out)))
    (assert-equal '(3 -3 1) (contents out))
    (assert-true (eq out (vm:gcd (iarr 'fixnum 12 -18 9) 6 :into out)))
    (assert-equal '(6 6 3) (contents out))
    (assert-true (eq out (vm:lcm (iarr 'fixnum 4 -6 9) 6 :into out)))
    (assert-equal '(12 6 18) (contents out)))
  ;; :into with numbers only: the quotient is stored everywhere, the remainder is the second value
  (let ((out (make-array 2 :element-type '(signed-byte 64) :initial-element 0)))
    (multiple-value-bind (q r) (vm:floor 7 2 :into out)
      (assert-true (eq q out))
      (assert-equal '(3 3) (contents out))
      (assert-equal 1 r)))
  (assert-true (typep (condition-of (vm:floor (darr 1d0) 1d0 :into 5)) 'type-error)))

(deftest division-no-mutation (division-tests)
  (dolist (name *div-names*)
    (let* ((a (darr 7.5d0 -3.25d0 9d0 1d0)) (b (darr 2d0 0.5d0 -4d0 3d0)) (a0 (copy-arr a)) (b0 (copy-arr b))
           (m (make-arr 'double-float '(2 2) '(7.5d0 -3.25d0 9d0 1d0))) (m0 (copy-arr m)))
      (funcall (vm-op name) a b) (funcall (vm-op name) a 2d0) (funcall (vm-op name) 2d0 b)
      (funcall (vm-op name) m (darr 3d0 2d0)) (funcall (vm-op name) (varr 7 8) (darr 3d0 2d0))
      (assert-true (same-contents-p a a0)) (assert-true (same-contents-p b b0)) (assert-true (same-contents-p m m0))))
  (let* ((a (iarr '(signed-byte 64) 7 8 9)) (a0 (copy-arr a)))
    (vm:floor a 2) (vm:gcd a 4) (vm:lcm a 4)
    (assert-true (same-contents-p a a0))))

;;; ---------------------------------------------------------------------------
;;; conditions for operands that are not numbers

(deftest division-type-errors (division-tests)
  (dolist (name *div-names*)
    (let ((v (vm-op name)) (c (cl-op name)))
      ;; a complex number or array
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (cdarr #c(1d0 2d0)) 2)))
                                   (outcome (lambda () (funcall c #c(1d0 2d0) 2)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v #c(1d0 2d0) (darr 2d0))))
                                   (outcome (lambda () (funcall c #c(1d0 2d0) 2d0)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (darr 1d0 2d0) #c(1 2))))
                                   (outcome (lambda () (funcall c 1d0 #c(1 2))))))
      ;; a string, a symbol
      (assert-equal :condition (car (outcome (lambda () (funcall v (darr 1d0) "abc")))))
      (assert-equal :condition (car (outcome (lambda () (funcall v "abc" (darr 1d0))))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (darr 1d0) 'x)))
                                   (outcome (lambda () (funcall c 2 'x)))))
      (assert-true (typep (condition-of (funcall v "abc" (iarr 'fixnum 1))) 'type-error))
      ;; T arrays holding non-numbers or complex numbers
      (assert-true (typep (condition-of (funcall v (varr 1 "x") 2)) 'type-error))
      (assert-true (typep (condition-of (funcall v (varr 1 #c(1 2)) 2)) 'type-error))
      (assert-equal (second (outcome (lambda () (funcall c #c(1 2) 2))))
                    (second (outcome (lambda () (funcall v (varr 1 #c(1 2)) 2))))))))

;;; ---------------------------------------------------------------------------
;;; gcd and lcm

(deftest gcd-lcm-arrays (division-tests)
  (let ((ints (append *int64-corpus* *bignum-corpus*)) (bad '()))
    (dolist (name '("gcd" "lcm"))
      (dolist (et '(((signed-byte 64) (signed-byte 64)) (t t) (t (signed-byte 64)) ((unsigned-byte 8) (signed-byte 32))))
        (let* ((xs (remove-if-not (lambda (x) (typep x (first et))) ints))
               (ys (remove-if-not (lambda (x) (typep x (second et))) ints))
               (pairs (loop for x in xs append (loop for y in ys collect (cons x y)))))
          (let* ((a (make-arr (first et) (length pairs) (mapcar #'car pairs)))
                 (b (make-arr (second et) (length pairs) (mapcar #'cdr pairs)))
                 (r (funcall (vm-op name) a b))
                 (ref (mapcar (lambda (p) (funcall (cl-op name) (car p) (cdr p))) pairs)))
            (loop for i from 0 for e in ref
                  unless (eql (aref r i) e) do (push (list name et i (aref r i) e) bad))
            (let ((expected (if (and (not (member t et)) (every #'sb64-p ref)) '(signed-byte 64)
                                (if (member t et) (if (every #'sb64-p ref) t t) t))))
              (declare (ignorable expected))
              (unless (or (eq-et (array-element-type r) '(signed-byte 64)) (eq-et (array-element-type r) t))
                (push (list name et :type (array-element-type r)) bad)))))))
    (assert-equal nil (subseq bad 0 (min 4 (length bad))))))

(deftest gcd-lcm-element-types-and-overflow (division-tests)
  (flet ((et (a) (array-element-type a)))
    (assert-equal '(signed-byte 64) (et (vm:gcd (iarr 'fixnum 12 18) (iarr 'fixnum 8 27))))
    (assert-equal '(6 9) (contents (vm:gcd (iarr 'fixnum 12 18) (iarr 'fixnum 6 27))))
    ;; gcd of the most negative with itself or 0: 2^63 does not fit
    (let ((r (vm:gcd (iarr '(signed-byte 64) -9223372036854775808 -9223372036854775808 4)
                     (iarr '(signed-byte 64) -9223372036854775808 0 6))))
      (assert-equal t (et r))
      (assert-equal '(9223372036854775808 9223372036854775808 2) (contents r)))
    ;; lcm overflow
    (let ((r (vm:lcm (iarr '(signed-byte 64) 4611686018427387904 3 2) (iarr '(signed-byte 64) 3 5 7))))
      (assert-equal t (et r))
      (assert-equal (list (cl:lcm 4611686018427387904 3) 15 14) (contents r)))
    (assert-equal '(signed-byte 64) (et (vm:lcm (iarr '(signed-byte 64) 4 6) (iarr '(signed-byte 64) 6 10))))
    (assert-equal '(12 30) (contents (vm:lcm (iarr '(signed-byte 64) 4 6) (iarr '(signed-byte 64) 6 10))))
    ;; one argument: the absolute values; the most negative does not fit
    (assert-equal '(5 0 7) (contents (vm:gcd (iarr 'fixnum -5 0 7))))
    (assert-equal '(5 0 7) (contents (vm:lcm (iarr 'fixnum -5 0 7))))
    (assert-equal '(signed-byte 64) (et (vm:gcd (iarr 'fixnum -5 0 7))))
    (let ((r (vm:gcd (iarr '(signed-byte 64) -9223372036854775808))))
      (assert-equal t (et r))
      (assert-equal '(9223372036854775808) (contents r)))
    ;; n-ary, with scalars
    (assert-equal '(2 6 2) (contents (vm:gcd (iarr 'fixnum 12 18 8) 30 (iarr 'fixnum 4 6 2))))
    (assert-equal '(60 60 120) (contents (vm:lcm (iarr 'fixnum 12 20 8) 30 (iarr 'fixnum 4 6 2))))
    (assert-equal '(6) (contents (vm:gcd 12 18 (iarr 'fixnum 30))))
    ;; shapes: recycling
    (assert-equal '(2 6 2 6) (contents (vm:gcd (iarr 'fixnum 4 6 8 12) (iarr 'fixnum 2 6))))
    (with-warnings (r ws) (vm:gcd (iarr 'fixnum 4 6 8) (iarr 'fixnum 2 6))
      (assert-equal 1 (length (recycling-warnings ws))))
    ;; empty
    (assert-true (array-is (vm:gcd (make-array 0 :element-type 'fixnum) 4) '(signed-byte 64) 0 '()))
    (assert-true (typep (condition-of (vm:lcm (make-array 0 :element-type 'double-float) 4)) 'type-error))))

(deftest gcd-lcm-errors (division-tests)
  (dolist (name '("gcd" "lcm"))
    (let ((v (vm-op name)) (c (cl-op name)))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (darr 1.5d0 2d0) 2)))
                                   (outcome (lambda () (funcall c 1.5d0 2)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v 2 (darr 1.5d0))))
                                   (outcome (lambda () (funcall c 2 1.5d0)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (iarr 'fixnum 4) 1.5d0)))
                                   (outcome (lambda () (funcall c 4 1.5d0)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (darr 6d0))))
                                   (outcome (lambda () (funcall c 6d0)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (varr 1/2 3) 2)))
                                   (outcome (lambda () (funcall c 1/2 2)))))
      (assert-true (same-outcome-p (outcome (lambda () (funcall v (cdarr #c(1d0 2d0)) 2)))
                                   (outcome (lambda () (funcall c #c(1d0 2d0) 2)))))
      (assert-equal :condition (car (outcome (lambda () (funcall v (iarr 'fixnum 4) "x")))))
      (assert-equal :condition (car (outcome (lambda () (funcall v (iarr 'fixnum 4) 'x))))))))

;;; ---------------------------------------------------------------------------
;;; the operators in a shadowing package: compiler macros and the MOD type

(deftest division-compiler-macro-shapes (division-tests)
  (flet ((expand (form &optional env) (funcall (compiler-macro-function (car form)) form env)))
    (assert-equal '(cl:floor 7 2) (expand '(vm:floor 7 2)))
    (assert-equal '(cl:mod 7 2) (expand '(vm:mod 7 2)))
    (assert-equal '(cl:gcd 12 18 30) (expand '(vm:gcd 12 18 30)))
    (assert-equal 0 (expand '(vm:gcd)))
    (assert-equal 1 (expand '(vm:lcm)))
    (let ((f '(vm:floor a b :into out))) (assert-true (eq f (expand f))))
    (let ((f '(vm:floor))) (assert-true (eq f (expand f))))
    (let ((f '(vm:floor a b c))) (assert-true (eq f (expand f))))
    (let ((f '(vm:mod a))) (assert-true (eq f (expand f))))
    (assert-equal 'if (first (let ((e (expand '(vm:floor a b))))
                                (unwrap-locally (if (eq (car e) 'let*) (third e) e)))))))

(deftest division-compiled-calls (division-tests)
  (let ((f (compile nil '(lambda (x y) (vm:floor x y))))
        (g (compile nil '(lambda (x y) (declare (type (signed-byte 32) x y)) (vm:floor x y))))
        (h (compile nil '(lambda (x y) (multiple-value-list (vm:round x y))))))
    (assert-equal '(3 1) (multiple-value-list (funcall f 7 2)))
    (assert-equal '(3 1) (multiple-value-list (funcall g 7 2)))
    (assert-equal '(4 -1) (funcall h 7 2))
    (let ((r (multiple-value-list (funcall f (darr 7d0 8d0) 2))))
      (assert-true (array-is (first r) '(signed-byte 64) 2 '(3 4)))
      (assert-true (array-is (second r) 'double-float 2 '(1d0 0d0))))
    (assert-equal 'division-by-zero (second (outcome (lambda () (funcall f 1 0)))))
    (assert-equal 'division-by-zero (second (outcome (lambda () (funcall f (iarr 'fixnum 1) 0)))))))

(deftest division-mod-type-specifier-in-shadowing-package (division-tests)
  "(mod n) is a type specifier as well as an operator: in a package shadowing MOD it still names the type."
  (let ((p (make-user-package)))
    (assert-true (eval-in p "(typep 5 '(mod 10))"))
    (assert-false (eval-in p "(typep 10 '(mod 10))"))
    (assert-true (eval-in p "(subtypep '(mod 256) '(unsigned-byte 8))"))
    (assert-true (eval-in p "(funcall (compile nil '(lambda (x) (declare (type (mod 10) x)) (mod (+ x 7) 5))) 4)"))
    (assert-equal 1 (eval-in p "(funcall (compile nil '(lambda (x) (declare (type (mod 10) x)) (mod (+ x 7) 5))) 4)"))
    (assert-equal '(3 1) (eval-in p "(multiple-value-list (floor 7 2))"))
    (assert-equal 6 (eval-in p "(gcd 12 18)"))))

;;; ---------------------------------------------------------------------------
;;; the non-consing float kernels

(deftest division-fast-kernels-agree-with-cl (division-tests)
  "SBCL: the kernels call CL's function open-coded; CCL: they repeat CCL's own algorithm inline.  Either
way they must give what CL's function gives.  Load time: the self-test accepted every fast kernel.  Here:
a corpus of 4000 pairs (exact ties and multiples, signed zeros, tiny and huge quotients) per float type
through every two-valued and one-valued operator, EQL to CL's result element by element.  The much larger
version of this check is tests/diff-kernels.lisp."
  (when (boundp 'vm.impl::*fast-division-failures*)
    (assert-equal nil (symbol-value 'vm.impl::*fast-division-failures*)))
  (dolist (type '(:double-float :single-float))
    (multiple-value-bind (xs ys) (vm.impl::fast-division-corpus type 4000)
      (dolist (name *div-names*)
        (let* ((results (multiple-value-list (funcall (vm-op name) xs ys)))
               (bad 0))
          (dotimes (i (length xs))
            (let* ((x (aref xs i)) (y (aref ys i))
                   (ref (multiple-value-list (masked-cl (cl-op name) x y))))
              (unless (every (lambda (r e) (same-bits-p (row-major-aref r i) e)) results ref)
                (incf bad))))
          (assert-equal 0 bad name type))))))
