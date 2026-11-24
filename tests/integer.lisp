;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Integer, bitwise and conversion operators: LOGAND LOGIOR LOGXOR LOGEQV LOGNAND LOGNOR LOGANDC1 LOGANDC2
;;; LOGORC1 LOGORC2 LOGNOT LOGCOUNT INTEGER-LENGTH ASH LOGBITP LOGTEST FLOAT RATIONAL RATIONALIZE NUMERATOR
;;; DENOMINATOR SCALE-FLOAT FLOAT-SIGN.  The reference is always CL's own function (cl:logand ...) applied
;;; per element.  Helpers (OUTCOME, SAME-THING-P, the corpora ...) come from tests/division.lisp.
(in-package #:vm-tests)

(defsuite integer-tests (vm-tests))

;;; ---------------------------------------------------------------------------
;;; the operators: name, arity range, kind of result

(defparameter *nary-bitwise* '("logand" "logior" "logxor" "logeqv"))
(defparameter *binary-bitwise* '("lognand" "lognor" "logandc1" "logandc2" "logorc1" "logorc2"))
(defparameter *unary-int* '("lognot" "logcount" "integer-length"))
(defparameter *bit-ops* '("logbitp" "logtest"))
(defparameter *masked-ops* '("float" "scale-float" "float-sign")
  "Operators that run their array elements with the float traps masked.")
(defparameter *all-int-ops* (append *nary-bitwise* *binary-bitwise* *unary-int* '("ash") *bit-ops*))
(defparameter *all-conversion-ops* '("float" "rational" "rationalize" "numerator" "denominator" "scale-float" "float-sign"))

(defun masked-op-p (name) (member name *masked-ops* :test #'string=))

;;; ---------------------------------------------------------------------------
;;; corpora

(defun int-corpus ()
  (remove-duplicates
   (append *int64-corpus* *bignum-corpus*
           '(4 6 8 15 16 17 31 32 33 63 64 65 -64 -65 -63 255 256 -255 65535 65536 4294967295 4294967296
             -4294967296 -4294967297 18446744073709551615 9223372036854775808 -9223372036854775809
             1180591620717411303424 -1180591620717411303424))))

(defun small-int-corpus ()
  (list 0 1 -1 2 -2 3 5 -5 7 8 -8 100 -100 255 -256 65535 123456789 -123456789
        4611686018427387903 -4611686018427387904 9223372036854775807 -9223372036854775808
        9223372036854775808 18446744073709551615 -9223372036854775809 (expt 2 70) (- (expt 3 50))))

(defun shift-corpus ()
  (list 0 1 2 3 5 31 32 62 63 64 65 100 200 -1 -2 -3 -5 -31 -32 -62 -63 -64 -65 -100 -200 -1000
        (- (expt 2 62)) (- (expt 2 70))))

(defun index-corpus () (list 0 1 2 5 31 32 62 63 64 65 100 1000 (expt 2 62) (expt 2 70)))

(defun float-specials ()
  (list (inf+) (inf-) (nan) (single-inf+) (- (single-inf+)) (single-nan)))

(defun not-numbers () (list "a" nil 'x '(1) #\a))

(defun complex-corpus () (list #c(1 2) #c(1d0 2d0) #c(1.5f0 -2.5f0) #c(1/2 3/4)))

(defun real-corpus ()
  (append (int-corpus) *ratio-corpus* (double-corpus) (single-corpus)))

(defun scale-corpus ()
  (list 0 1 -1 2 10 -10 100 -100 127 128 -149 -150 1000 1023 1024 -1074 -1075 -1100 2000 -2000
        (expt 2 31) (- (expt 2 31)) (expt 2 62) (- (expt 2 62)) (expt 2 70) (- (expt 2 70))))

;;; ---------------------------------------------------------------------------
;;; outcomes

(defun call-op (fn args masked)
  (outcome (lambda () (if masked (with-masked (apply fn args)) (apply fn args)))))

(defun scalar-mismatches (name rows)
  "Rows (argument lists) for which VM's NAME and CL's NAME differ in values or conditions, with the
traps as they are and masked.  Wrong arities included."
  (let ((vm (vm-op name)) (cl (cl-op name)) (bad '()))
    (dolist (masked '(nil t))
      (dolist (row rows)
        (let ((a (call-op vm row masked)) (b (call-op cl row masked)))
          (unless (same-outcome-p a b) (push (list name masked row a b) bad)))))
    (subseq (nreverse bad) 0 (min 5 (length bad)))))

(defun unary-rows (values) (mapcar #'list values))

(defun pair-rows (xs ys) (loop for x in xs append (loop for y in ys collect (list x y))))

(defun wrong-arity-rows (min max)
  (append (when (plusp min) (list (loop repeat (1- min) collect 1)))
          (list (loop repeat (1+ max) collect 1))))

;;; ---------------------------------------------------------------------------
;;; scalars: identical to CL (values, conditions)

(deftest integer-scalar-unary-identity (integer-tests)
  (let ((vals (append (int-corpus) *ratio-corpus* (double-corpus) (single-corpus) (float-specials)
                      (complex-corpus) (not-numbers))))
    (dolist (name (append *unary-int* '("rational" "rationalize" "numerator" "denominator" "float" "float-sign")))
      (assert-equal nil (scalar-mismatches name (unary-rows vals))))))

(deftest integer-scalar-nary-bitwise-identity (integer-tests)
  (let ((ints (small-int-corpus)))
    (dolist (name *nary-bitwise*)
      (assert-equal nil (scalar-mismatches name (pair-rows (append ints '(1.5 "a" 1/2 #c(1 2))) ints)))
      (assert-equal nil (scalar-mismatches name (list '() (list 5) (list 5 3 6) (list -1 7 12 255 4) (list "a") (list 1.5)
                                                      (list 1 2 'x) (list 1 2 3 4 5 6 7 8)))))))

(deftest integer-scalar-binary-identity (integer-tests)
  (let ((ints (small-int-corpus)))
    (dolist (name *binary-bitwise*)
      (assert-equal nil (scalar-mismatches name (pair-rows (append ints '(1.5 "a" 1/2)) ints))))
    (assert-equal nil (scalar-mismatches "logtest" (pair-rows (append ints '(1.5 "a" 1/2)) ints)))
    (assert-equal nil (scalar-mismatches "ash" (pair-rows (append ints '(1.5 "a" 1/2)) (append (shift-corpus) '(1.5 "a")))))
    (assert-equal nil (scalar-mismatches "logbitp" (pair-rows (append (index-corpus) '(-1 -64 1.5 "a")) (append ints '(1.5 "a")))))))

(deftest integer-scalar-conversion-identity (integer-tests)
  (let ((reals (append (int-corpus) *ratio-corpus* (double-corpus) (single-corpus) (float-specials))))
    (assert-equal nil (scalar-mismatches "float" (pair-rows (append reals (complex-corpus) '("a" nil))
                                                            (append (list 1f0 1d0 0f0 -2.5d0 0d0 (inf+) (single-inf+) 1 1/2 #c(1d0 1d0) "a")))))
    (assert-equal nil (scalar-mismatches "scale-float" (pair-rows (append (double-corpus) (single-corpus) (float-specials)
                                                                          (list 1 1/2 "a"))
                                                                  (append (scale-corpus) (list 1.5 "a")))))
    (assert-equal nil (scalar-mismatches "float-sign" (pair-rows (append (double-corpus) (single-corpus) (float-specials) (list 1 "a"))
                                                                 (append (list 1f0 -1d0 0d0 -0d0 2.5f0 (nan) (inf+)) (list 1 "a")))))))

(deftest integer-scalar-arities (integer-tests)
  "A wrong number of arguments signals the same kind of error as CL's function."
  (dolist (spec '(("logand" 0 1000) ("logior" 0 1000)
                  ("lognand" 2 2) ("lognor" 2 2) ("logandc1" 2 2) ("logandc2" 2 2) ("logorc1" 2 2) ("logorc2" 2 2)
                  ("lognot" 1 1) ("logcount" 1 1) ("integer-length" 1 1) ("ash" 2 2) ("logbitp" 2 2) ("logtest" 2 2)
                  ("float" 1 2) ("rational" 1 1) ("rationalize" 1 1) ("numerator" 1 1) ("denominator" 1 1)
                  ("scale-float" 2 2) ("float-sign" 1 2)))
    (destructuring-bind (name min max) spec
      (let ((vm (vm-op name)) (cl (cl-op name)))
        (flet ((kind (f args) (let ((o (outcome (lambda () (apply f args)))))
                                (if (eq (car o) :condition) (type-of-condition-kind o) :values))))
          (when (plusp min)
            (assert-equal (kind cl (loop repeat (1- min) collect 1)) (kind vm (loop repeat (1- min) collect 1)))
            (assert-true (eq :program-error (kind vm (loop repeat (1- min) collect 1)))))
          (when (< max 10)
            (assert-equal (kind cl (loop repeat (1+ max) collect 1)) (kind vm (loop repeat (1+ max) collect 1)))
            (assert-true (eq :program-error (kind vm (loop repeat (1+ max) collect 1))))))))))

(defun type-of-condition-kind (outcome)
  "A coarse classification of the condition description OUTCOME: the arity errors of the two
implementations are different condition classes of the same kind."
  (let ((type (first (cdr outcome))))
    (if (subtypep type 'program-error) :program-error type)))

(deftest integer-scalar-identity-0-and-1-args (integer-tests)
  (assert-equal -1 (funcall (vm-op "logand")))
  (assert-equal -1 (funcall (vm-op "logeqv")))
  (assert-equal 0 (funcall (vm-op "logior")))
  (assert-equal 0 (funcall (vm-op "logxor")))
  (dolist (name *nary-bitwise*)
    (assert-equal 5 (funcall (vm-op name) 5))))

;;; ---------------------------------------------------------------------------
;;; arrays, element by element

(defparameter *int-ets* '((signed-byte 64) (unsigned-byte 64) (signed-byte 8) (unsigned-byte 8) fixnum (signed-byte 32) t))

(defun tightest-et (vals)
  "The element type the results VALS must have."
  (cond ((null vals) :empty)
        ((every #'integerp vals) (if (every #'sb64-p vals) '(signed-byte 64) t))
        ((every (lambda (v) (typep v 'double-float)) vals) 'double-float)
        ((every (lambda (v) (typep v 'single-float)) vals) 'single-float)
        (t t)))

(defun row-fits-p (row ets)
  (every (lambda (x et) (or (eq et :scalar) (typep x et))) row ets))

(defun check-array-rows (name ets rows &key scalar-pos)
  "Apply the VM operator NAME to arrays of element types ETS (one per argument) built column-wise from ROWS,
and compare every element, and the element type, with CL's values.  SCALAR-POS: the argument at that
position is the scalar taken from the first row.  A list of mismatch descriptions."
  (let* ((vm (vm-op name)) (cl (cl-op name)) (masked (masked-op-p name)) (bits (member name *bit-ops* :test #'string=))
         (rows (if scalar-pos
                   (let ((s (nth scalar-pos (first rows))))
                     (mapcar (lambda (r) (let ((c (copy-list r))) (setf (nth scalar-pos c) s) c)) rows))
                   rows))
         (rows (remove-if-not (lambda (r) (row-fits-p r (if scalar-pos
                                                              (let ((c (copy-list ets))) (setf (nth scalar-pos c) :scalar) c)
                                                              ets)))
                              rows))
         (refs '()) (good '()))
    (dolist (r rows)
      (let ((o (call-op cl r masked)))
        (when (and (eq (car o) :values) (= (length (cdr o)) 1))
          (push r good) (push (second o) refs))))
    (setf good (nreverse good) refs (nreverse refs))
    (when (null good) (return-from check-array-rows nil))
    (let* ((n (length good))
           (args (loop for i from 0 for et in ets
                       collect (if (eql i scalar-pos)
                                   (nth i (first good))
                                   (make-arr et n (mapcar (lambda (r) (nth i r)) good)))))
           (result (if masked (with-masked (apply vm args)) (apply vm args)))
           (bad '()))
      (cond
        ((not (arrayp result)) (push (list :not-an-array result) bad))
        (t
         (let ((expected-et (if bits 'bit (tightest-et refs))))
           (unless (and (typep result 'simple-array) (equal (array-element-type result) (upgraded-array-element-type expected-et)))
             (push (list :element-type (array-element-type result) :expected expected-et) bad)))
         (unless (equal (array-dimensions result) (list n)) (push (list :dims (array-dimensions result)) bad))
         (loop for i below (min n (array-total-size result)) for ref in refs for r in good
               do (unless (same-bits-p (row-major-aref result i) (if bits (if ref 1 0) ref))
                    (push (list :element i r (row-major-aref result i) ref) bad)))))
      (when bad
        (list (list name ets scalar-pos (subseq (nreverse bad) 0 (min 4 (length bad)))))))))

(defun ets-product (ets-list arity)
  (if (zerop arity) (list nil)
      (loop for et in ets-list
            append (mapcar (lambda (rest) (cons et rest)) (ets-product ets-list (1- arity))))))

(defun check-all-ets (name ets-list rows arity &key scalar-positions)
  (let ((bad '()))
    (dolist (ets (ets-product ets-list arity))
      (setf bad (append bad (check-array-rows name ets rows)))
      (dolist (pos scalar-positions)
        (setf bad (append bad (check-array-rows name ets rows :scalar-pos pos)))))
    (subseq bad 0 (min 6 (length bad)))))

(deftest integer-unary-arrays (integer-tests)
  (dolist (name *unary-int*)
    (assert-equal nil (check-all-ets name *int-ets* (unary-rows (int-corpus)) 1))))

(deftest integer-nary-bitwise-arrays (integer-tests)
  (let ((rows (pair-rows (small-int-corpus) (small-int-corpus))))
    (dolist (name (append *nary-bitwise* *binary-bitwise* '("logtest")))
      (assert-equal nil (check-all-ets name '((signed-byte 64) (unsigned-byte 64) (signed-byte 8) t) rows 2
                                       :scalar-positions '(0 1))))))

(deftest integer-ash-arrays (integer-tests)
  (assert-equal nil (check-all-ets "ash" '((signed-byte 64) (unsigned-byte 64) (signed-byte 8) fixnum t)
                                   (pair-rows (small-int-corpus) (shift-corpus)) 2 :scalar-positions '(0 1)))
  ;; the typed path and the generic path (T arrays) agree, element type included
  (let* ((xs (list 1 -1 3 -3 (expt 2 61) (- (expt 2 62)) 0 7))
         (ns (list 0 1 2 3 62 63 64 -1))
         (typed (funcall (vm-op "ash") (make-arr '(signed-byte 64) 8 xs) (make-arr '(signed-byte 64) 8 ns)))
         (generic (funcall (vm-op "ash") (make-arr t 8 xs) (make-arr t 8 ns))))
    (assert-true (same-contents-p typed generic))
    (assert-equal t (array-element-type typed))))

(deftest integer-ash-result-types (integer-tests)
  (let ((ash (vm-op "ash")))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 2 3) 2) '(signed-byte 64) 3 '(4 8 12)))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 2 3) 62) t 3 (list (expt 2 62) (expt 2 63) (* 3 (expt 2 62)))))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 -1 3) 62) t 3 (list (expt 2 62) (- (expt 2 62)) (* 3 (expt 2 62)))))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 -1) 63) t 2 (list (expt 2 63) (- (expt 2 63)))))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 -1) 62) '(signed-byte 64) 2 (list (expt 2 62) (- (expt 2 62)))))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 1 2 3) 70) t 3 (list (expt 2 70) (expt 2 71) (* 3 (expt 2 70)))))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 0 0) 100000) '(signed-byte 64) 2 '(0 0)))
    (assert-true (array-is (funcall ash (iarr '(signed-byte 64) 5 -5 99) -1000) '(signed-byte 64) 3 '(0 -1 0)))
    (assert-true (array-is (funcall ash 1 (iarr '(signed-byte 64) 0 1 63 64)) t 4 (list 1 2 (expt 2 63) (expt 2 64))))
    (assert-true (array-is (funcall ash (iarr '(unsigned-byte 64) 1 18446744073709551615) -1) '(signed-byte 64) 2
                           (list 0 9223372036854775807)))
    ;; ub64 values above the signed range: not typed, T result when they do not fit
    (assert-true (array-is (funcall ash (iarr '(unsigned-byte 64) 18446744073709551615) 1) t 1 (list 36893488147419103230)))))

(deftest integer-lognot-types (integer-tests)
  (let ((lognot (vm-op "lognot")))
    (assert-true (array-is (funcall lognot (iarr '(signed-byte 64) 0 -1 9223372036854775807 -9223372036854775808))
                           '(signed-byte 64) 4 '(-1 0 -9223372036854775808 9223372036854775807)))
    (assert-true (array-is (funcall lognot (iarr '(unsigned-byte 64) 0 1 9223372036854775807))
                           '(signed-byte 64) 3 '(-1 -2 -9223372036854775808)))
    ;; ub64 values from 2^63 on: the result is below the signed range
    (assert-true (array-is (funcall lognot (iarr '(unsigned-byte 64) 0 9223372036854775808)) t 2 '(-1 -9223372036854775809)))
    (assert-true (array-is (funcall lognot (iarr '(unsigned-byte 8) 0 255)) '(signed-byte 64) 2 '(-1 -256)))
    (assert-true (array-is (funcall lognot (make-arr 'bit 3 '(0 1 0))) '(signed-byte 64) 3 '(-1 -2 -1)))
    (assert-true (array-is (funcall lognot (make-arr t 2 (list (expt 2 70) 5))) t 2 (list (- (1+ (expt 2 70))) -6)))
    (assert-true (array-is (funcall lognot (make-arr t 2 '(5 6))) '(signed-byte 64) 2 '(-6 -7)))))

(deftest integer-logbitp-logtest-arrays (integer-tests)
  (assert-equal nil (check-all-ets "logbitp" '((signed-byte 64) (unsigned-byte 64) (signed-byte 8) t)
                                   (pair-rows (index-corpus) (small-int-corpus)) 2 :scalar-positions '(0 1)))
  (assert-equal nil (check-all-ets "logtest" '((signed-byte 64) (unsigned-byte 8) t)
                                   (pair-rows (small-int-corpus) (small-int-corpus)) 2 :scalar-positions '(0 1)))
  ;; a negative index is CL's error, from the typed or the generic path
  (let ((expected (call-op #'cl:logbitp (list -1 5) nil)))
    (assert-equal :condition (car expected))
    (assert-true (same-outcome-p expected (outcome (lambda () (funcall (vm-op "logbitp") (iarr '(signed-byte 64) 0 -1) 5)))))
    (assert-true (same-outcome-p expected (outcome (lambda () (funcall (vm-op "logbitp") (varr 0 -1) 5)))))))

(deftest integer-conversion-arrays (integer-tests)
  (let ((float-ets '(double-float single-float t))
        (reals (real-corpus)))
    ;; one argument
    (dolist (name '("rational" "rationalize"))
      (assert-equal nil (check-all-ets name '(double-float single-float (signed-byte 64) t) (unary-rows reals) 1)))
    (assert-equal nil (check-all-ets "numerator" '((signed-byte 64) t) (unary-rows (append (int-corpus) *ratio-corpus*)) 1))
    (assert-equal nil (check-all-ets "denominator" '((signed-byte 64) t) (unary-rows (append (int-corpus) *ratio-corpus*)) 1))
    (assert-equal nil (check-all-ets "float" '((signed-byte 64) double-float single-float t) (unary-rows reals) 1))
    (assert-equal nil (check-all-ets "float-sign" float-ets (unary-rows (append (double-corpus) (single-corpus))) 1))
    ;; two arguments
    (assert-equal nil (check-all-ets "float" '((signed-byte 64) double-float single-float t)
                                     (pair-rows (append (small-int-corpus) (double-corpus) (single-corpus) *ratio-corpus*)
                                                (list 1f0 1d0 -2.5f0 0d0 3d0))
                                     2 :scalar-positions '(0 1)))
    (assert-equal nil (check-all-ets "scale-float" '(double-float single-float)
                                     (pair-rows (append (double-corpus) (single-corpus)) (scale-corpus))
                                     2 :scalar-positions '(0 1)))
    (assert-equal nil (check-all-ets "scale-float" '(t)
                                     (pair-rows (append (double-corpus) (single-corpus)) (scale-corpus))
                                     2))
    (assert-equal nil (check-all-ets "float-sign" float-ets
                                     (pair-rows (append (double-corpus) (single-corpus)) (list 1f0 -1d0 0d0 -0d0 2.5f0 -7.5d0))
                                     2 :scalar-positions '(0 1)))))

(deftest integer-conversion-specials (integer-tests)
  "NaN and infinities in float arrays: whatever CL's function does for the element (the arrays are
built with the traps masked)."
  (let ((specials (float-specials)))
    (dolist (name '("float-sign" "float"))
      (assert-equal nil (check-all-ets name '(double-float single-float t)
                                       (unary-rows (append (list 1d0 -2d0 0d0) specials)) 1)))
    (assert-equal nil (check-all-ets "float" '(double-float single-float t)
                                     (pair-rows (append (list 1d0 -2d0) specials) (list 1f0 1d0)) 2))
    (assert-equal nil (check-all-ets "scale-float" '(double-float single-float t)
                                     (pair-rows (append (list 1d0 -2d0) specials) (list 0 3 -3 2000 -2000)) 2))
    ;; rational of NaN / infinity: the element's own outcome (a number or an error) in the caller's trap state
    (dolist (name '("rational" "rationalize"))
      (dolist (x specials)
        (let* ((et (if (typep x 'double-float) 'double-float 'single-float))
               (ref (call-op (cl-op name) (list x) nil))
               (got (outcome (lambda () (funcall (vm-op name) (make-arr et 1 (list x)))))))
          (if (eq (car ref) :values)
              (assert-true (eql (second ref) (row-major-aref (second got) 0)))
              (assert-true (same-outcome-p ref got))))))))

(deftest integer-denormals-and-extremes (integer-tests)
  (let ((float (vm-op "float")))
    ;; denormal doubles to single: whatever CL does, masked
    (let* ((xs (list 5d-324 1d-320 1d-310 2.2250738585072014d-308 1d-40 1d-46 1d-45 1d300 -1d300 1.7976931348623157d308))
           (refs (mapcar (lambda (x) (with-masked (cl:float x 1f0))) xs)))
      (assert-true (every #'same-bits-p (contents (funcall float (make-arr 'double-float (length xs) xs) 1f0)) refs)))
    ;; integers to float: correct rounding for values near the edges of the format
    (let* ((xs (list 9007199254740993 9007199254740995 -9007199254740993 9223372036854775807 -9223372036854775808
                     4611686018427387903 16777217 16777219 -16777217 4294967297 1152921504606846977))
           (arr (make-arr '(signed-byte 64) (length xs) xs)))
      (assert-true (every #'same-bits-p (contents (funcall float arr 1d0)) (mapcar (lambda (x) (cl:float x 1d0)) xs)))
      (assert-true (every #'same-bits-p (contents (funcall float arr 1f0)) (mapcar (lambda (x) (cl:float x 1f0)) xs)))
      ;; and equal to the exact rounding of the rational (round half to even), independent of CL:FLOAT
      (assert-true (every (lambda (got x) (= (rational got) (nearest-double x))) (contents (funcall float arr 1d0)) xs)))))

(defun nearest-double (x)
  "The double-float value (as a rational) nearest the integer X, ties to even, by exact arithmetic."
  (if (zerop x) 0
      (let* ((s (if (minusp x) -1 1)) (a (abs x)) (len (integer-length a))
             (shift (max 0 (- len 53))))
        (multiple-value-bind (q r) (floor a (ash 1 shift))
          (let ((half (ash 1 (max 0 (1- shift)))))
            (when (and (plusp shift) (or (> r half) (and (= r half) (oddp q)))) (incf q))
            (* s q (ash 1 shift)))))))

;;; ---------------------------------------------------------------------------
;;; errors on arrays

(defun same-type-error-p (a b)
  "A and B are outcomes of type errors with the same datum and expected type (the class may be
TYPE-ERROR or SIMPLE-TYPE-ERROR: CL's own function signals the latter on SBCL)."
  (and (eq (car a) :condition) (eq (car b) :condition)
       (subtypep (second a) 'type-error) (subtypep (second b) 'type-error)
       (same-thing-p (cddr a) (cddr b))))

(defun array-outcome (name &rest args)
  (outcome (lambda () (apply (vm-op name) args))))

(deftest integer-array-type-errors (integer-tests)
  (let ((d (darr 1.5d0 2d0)) (s (sarr 1f0)) (cd (cdarr #c(1d0 2d0))))
    (dolist (name (append *nary-bitwise* *binary-bitwise* '("ash" "logtest")))
      ;; floats, complex floats, ratios and non-numbers: type-error, expected type INTEGER
      (dolist (bad (list d s cd (varr 1/2 3) (varr 1.5 2) (varr "a" 1) "a" 1.5 1/2 'x))
        (let ((o (array-outcome name (iarr '(signed-byte 64) 1 2) bad)))
          (assert-equal :condition (car o))
          (assert-true (subtypep (second o) 'type-error))
          (assert-equal 'integer (fourth o)))
        (let ((o (array-outcome name bad (iarr '(signed-byte 64) 1 2))))
          (assert-equal :condition (car o))
          (assert-true (subtypep (second o) 'type-error))
          (assert-equal 'integer (fourth o)))))
    (dolist (name *unary-int*)
      (dolist (bad (list d s cd (varr 1/2 3) (varr 1.5 2) (varr "a" 1)))
        (let ((o (array-outcome name bad)))
          (assert-equal :condition (car o))
          (assert-true (subtypep (second o) 'type-error))
          (assert-equal 'integer (fourth o)))))
    ;; the datum is the first offending element / the scalar, like CL's
    (assert-true (same-type-error-p (outcome (lambda () (cl:logand 1.5d0 1))) (array-outcome "logand" d 1)))
    (assert-true (same-type-error-p (outcome (lambda () (cl:logand 1 "a"))) (array-outcome "logand" (iarr '(signed-byte 64) 1) "a")))
    (assert-true (same-type-error-p (outcome (lambda () (cl:lognot 1.5d0))) (array-outcome "lognot" d)))
    ;; even for empty float arrays
    (assert-true (subtypep (second (array-outcome "logand" (make-arr 'double-float 0 nil) 1)) 'type-error))
    (assert-true (subtypep (second (array-outcome "lognot" (make-arr 'double-float 0 nil))) 'type-error))
    ;; the conversion operators
    (dolist (name '("numerator" "denominator"))
      (dolist (bad (list d s (varr 1.5 2)))
        (let ((o (array-outcome name bad)))
          (assert-equal :condition (car o))
          (assert-true (subtypep (second o) 'type-error))
          (assert-equal 'rational (fourth o)))))
    (dolist (name '("rational" "rationalize" "float"))
      (let ((o (array-outcome name (cdarr #c(1d0 2d0)))))
        (assert-equal :condition (car o))
        (assert-true (subtypep (second o) 'type-error))))
    (assert-equal 'float (fourth (array-outcome "float" (iarr '(signed-byte 64) 1 2) 1)))
    (assert-equal 'float (fourth (array-outcome "float" (iarr '(signed-byte 64) 1 2) (iarr '(signed-byte 64) 1 2))))
    (assert-equal 'float (fourth (array-outcome "scale-float" (iarr '(signed-byte 64) 1 2) 1)))
    ;; (the expected type of the scale is what CL's function says: INTEGER, or FIXNUM on CCL without Clozure/ccl#743)
    (assert-true (same-type-error-p (outcome (lambda () (cl:scale-float 1.5d0 1.5d0))) (array-outcome "scale-float" d 1.5d0)))
    (assert-true (same-type-error-p (outcome (lambda () (cl:scale-float 1.5d0 1.5d0))) (array-outcome "scale-float" d d)))
    (assert-equal 'float (fourth (array-outcome "float-sign" (iarr '(signed-byte 64) 1 2))))
    (assert-equal 'float (fourth (array-outcome "float-sign" d (iarr '(signed-byte 64) 1 2))))))

(deftest integer-array-wrong-arity (integer-tests)
  (dolist (spec '(("lognand" 1) ("lognand" 3) ("ash" 1) ("ash" 3) ("logbitp" 1) ("lognot" 2) ("float" 3) ("scale-float" 1)
                  ("float-sign" 3) ("rational" 2)))
    (destructuring-bind (name n) spec
      (let ((o (apply #'array-outcome name (loop repeat n collect (iarr '(signed-byte 64) 1 2)))))
        (assert-equal :condition (car o))
        (assert-true (subtypep (second o) 'program-error))))))

;;; ---------------------------------------------------------------------------
;;; shapes: R rules, column-major recycling

(defun ref-recycle (fn a b)
  "(values list-of-results dims) of FN applied to the elements of A and B (arrays or scalars) by the
R rules: result length of the longer vector; a matrix takes the vector in column-major order."
  (flet ((arr-p (x) (arrayp x)))
    (cond
      ((and (arr-p a) (arr-p b) (>= (array-rank a) 2) (>= (array-rank b) 2))
       (values (loop for i below (array-total-size a) collect (funcall fn (row-major-aref a i) (row-major-aref b i)))
               (array-dimensions a)))
      ((or (and (arr-p a) (>= (array-rank a) 2)) (and (arr-p b) (>= (array-rank b) 2)))
       (let* ((m (if (and (arr-p a) (>= (array-rank a) 2)) a b)) (dims (array-dimensions m)) (n (array-total-size m)))
         (values (loop for r below n
                       collect (let* ((c (colmajor-index dims r)))
                                 (flet ((elt-of (x) (cond ((not (arr-p x)) x)
                                                          ((eq x m) (row-major-aref x r))
                                                          (t (row-major-aref x (mod c (array-total-size x)))))))
                                   (funcall fn (elt-of a) (elt-of b)))))
                 dims)))
      (t
       (let* ((na (if (arr-p a) (array-total-size a) 1)) (nb (if (arr-p b) (array-total-size b) 1))
              (n (if (or (zerop na) (zerop nb)) 0 (max na nb))))
         (values (loop for k below n
                       collect (funcall fn (if (arr-p a) (row-major-aref a (mod k na)) a)
                                        (if (arr-p b) (row-major-aref b (mod k nb)) b)))
                 (list n)))))))

(defun colmajor-index (dims r)
  "The column-major index of the element with row-major index R in an array of dimensions DIMS."
  (let ((subs '()) (c 0) (stride 1))
    (dolist (d (reverse dims)) (multiple-value-bind (q rem) (floor r d) (push rem subs) (setf r q)))
    (loop for i in subs for d in dims do (incf c (* i stride)) (setf stride (* stride d)))
    c))

(defun shape-arrays ()
  "Operand shapes: (description array-factory) for integer data."
  (flet ((vec (et n) (make-arr et n (loop for i below n collect (mod (* 7 (1+ i)) 13))))
         (mat (et dims) (let ((n (reduce #'* dims))) (make-arr et dims (loop for i below n collect (mod (* 5 (1+ i)) 11))))))
    (list (list "3x4 and 3-vector" (mat '(signed-byte 64) '(3 4)) (vec '(signed-byte 64) 3))
          (list "3x4 and 4-vector" (mat '(signed-byte 64) '(3 4)) (vec '(signed-byte 64) 4))
          (list "3x4 and 6-vector" (mat '(signed-byte 64) '(3 4)) (vec '(signed-byte 64) 6))
          (list "3x4 and 12-vector" (mat '(signed-byte 64) '(3 4)) (vec '(signed-byte 64) 12))
          (list "3x4 and 1-vector" (mat '(signed-byte 64) '(3 4)) (vec '(signed-byte 64) 1))
          (list "2x3x2 and 4-vector" (mat '(signed-byte 64) '(2 3 2)) (vec '(signed-byte 64) 4))
          (list "2x3x2 and 2x3x2" (mat '(signed-byte 64) '(2 3 2)) (mat '(signed-byte 8) '(2 3 2)))
          (list "vector and matrix" (vec '(signed-byte 64) 3) (mat '(signed-byte 64) '(3 4)))
          (list "vectors 6 and 3" (vec '(signed-byte 64) 6) (vec '(signed-byte 64) 3))
          (list "vectors 3 and 6" (vec '(signed-byte 64) 3) (vec '(signed-byte 64) 6))
          (list "vectors 40 and 20" (vec '(signed-byte 64) 40) (vec '(signed-byte 64) 20))
          (list "vectors 40 and 3 (cyclic)" (vec 't 40) (vec '(signed-byte 64) 3))
          (list "vectors 5 and 5" (vec '(unsigned-byte 8) 5) (vec '(signed-byte 64) 5))
          (list "vector and scalar 3" (vec '(signed-byte 64) 5) 3)
          (list "scalar 3 and vector" 3 (vec '(signed-byte 64) 5))
          (list "matrix and scalar" (mat '(signed-byte 64) '(3 4)) 2)
          (list "rank 0 and vector" (make-array nil :element-type '(signed-byte 64) :initial-element 9) (vec '(signed-byte 64) 3)))))

(deftest integer-shapes-recycling (integer-tests)
  "Binary operators on every shape combination equal CL's function applied by the R rules."
  (dolist (name '("logand" "logxor" "lognand" "logorc2" "ash" "logtest" "logbitp"))
    (let ((vm (vm-op name)) (cl (cl-op name)) (bits (member name *bit-ops* :test #'string=)))
      (dolist (case (shape-arrays))
        (destructuring-bind (what a b) case
          (declare (ignore what))
          ;; keep ASH counts and LOGBITP indices small and non-negative
          (let* ((a (if (member name '("logbitp") :test #'string=) (if (arrayp a) (map-array a (lambda (x) (mod x 70))) (mod a 70)) a))
                 (b (if (member name '("ash") :test #'string=) (if (arrayp b) (map-array b (lambda (x) (- (mod x 9) 3))) (- (mod b 9) 3)) b)))
            (multiple-value-bind (vals dims) (ref-recycle cl a b)
              (multiple-value-bind (result ws) (collect-warnings (lambda () (funcall vm a b)))
                (declare (ignore ws))
                (assert-equal dims (array-dimensions result))
                (assert-true (every (lambda (r v) (same-bits-p r (if bits (if v 1 0) v))) (contents-row-major result) vals))))))))))

(defun map-array (a fn)
  (let ((c (make-array (array-dimensions a) :element-type (array-element-type a))))
    (dotimes (i (array-total-size a) c) (setf (row-major-aref c i) (funcall fn (row-major-aref a i))))))

(defun contents-row-major (a) (loop for i below (array-total-size a) collect (row-major-aref a i)))

(deftest integer-shapes-float-and-scale (integer-tests)
  "FLOAT with an array prototype, SCALE-FLOAT and FLOAT-SIGN on shapes."
  (let* ((mat (make-arr '(signed-byte 64) '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))
         (proto3 (make-arr 'double-float 3 '(1d0 2d0 3d0)))
         (dm (make-arr 'double-float '(3 4) '(1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0 9d0 10d0 11d0 12d0)))
         (n3 (make-arr '(signed-byte 64) 3 '(1 2 3))))
    (multiple-value-bind (vals dims) (ref-recycle #'cl:float mat proto3)
      (let ((r (funcall (vm-op "float") mat proto3)))
        (assert-equal dims (array-dimensions r))
        (assert-equal 'double-float (array-element-type r))
        (assert-true (every #'same-bits-p (contents-row-major r) vals))))
    (multiple-value-bind (vals dims) (ref-recycle #'cl:scale-float dm n3)
      (let ((r (funcall (vm-op "scale-float") dm n3)))
        (assert-equal dims (array-dimensions r))
        (assert-equal 'double-float (array-element-type r))
        (assert-true (every #'same-bits-p (contents-row-major r) vals))))
    (multiple-value-bind (vals dims) (ref-recycle #'cl:float-sign proto3 dm)
      (let ((r (funcall (vm-op "float-sign") proto3 dm)))
        (assert-equal dims (array-dimensions r))
        (assert-true (every #'same-bits-p (contents-row-major r) vals))))))

(deftest integer-shapes-errors-and-warnings (integer-tests)
  (let ((logand (vm-op "logand")) (ash (vm-op "ash")))
    ;; non-conformable matrices, a vector longer than the matrix
    (assert-true (typep (condition-of (funcall logand (make-arr '(signed-byte 64) '(2 3) '(1 2 3 4 5 6))
                                              (make-arr '(signed-byte 64) '(3 2) '(1 2 3 4 5 6))))
                        'vm:non-conformable-arrays))
    (assert-true (typep (condition-of (funcall ash (make-arr '(signed-byte 64) '(2 2) '(1 2 3 4)) (iarr '(signed-byte 64) 1 2 3 4 5)))
                        'vm:non-conformable-arrays))
    ;; recycling warning when the longer length is not a multiple
    (with-warnings (r ws) (funcall logand (iarr '(signed-byte 64) 1 2 3 4 5 6 7) (iarr '(signed-byte 64) 1 3 5))
      (assert-equal 1 (length (recycling-warnings ws)))
      (assert-equal '(1 2 1 0 1 4 1) (contents r)))
    (with-warnings (r ws) (funcall logand (iarr '(signed-byte 64) 1 2 3 4 5 6) (iarr '(signed-byte 64) 1 3 5))
      (assert-equal 0 (length (recycling-warnings ws))))
    ;; zero-length operands
    (let ((e (make-arr '(signed-byte 64) 0 nil)))
      (assert-true (array-is (funcall logand e (iarr '(signed-byte 64) 1 2)) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall logand (iarr '(signed-byte 64) 1 2) e) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall ash e 3) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall ash 3 e) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall (vm-op "logbitp") e 3) 'bit 0 nil))
      (assert-true (array-is (funcall (vm-op "lognot") e) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall (vm-op "logcount") (make-arr t 0 nil)) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall (vm-op "float") e) 'single-float 0 nil))
      (assert-true (array-is (funcall (vm-op "float") e 1d0) 'double-float 0 nil))
      (assert-true (array-is (funcall (vm-op "float") (make-arr 'double-float 0 nil)) 'double-float 0 nil))
      (assert-true (array-is (funcall (vm-op "scale-float") (make-arr 'double-float 0 nil) 3) 'double-float 0 nil))
      (assert-true (array-is (funcall (vm-op "float-sign") (make-arr 'single-float 0 nil)) 'single-float 0 nil))
      (assert-true (array-is (funcall (vm-op "rational") (make-arr 'double-float 0 nil)) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall (vm-op "numerator") e) '(signed-byte 64) 0 nil))
      (assert-true (array-is (funcall (vm-op "denominator") e) '(signed-byte 64) 0 nil)))
    ;; a zero-length matrix keeps its dimensions
    (assert-equal '(0 3) (array-dimensions (funcall logand (make-array '(0 3) :element-type '(signed-byte 64)) 5)))))

(deftest integer-nary-fold (integer-tests)
  "(logand a b c) is the left fold, with the shape rules at each step, scalars anywhere."
  (let* ((a (iarr '(signed-byte 64) 255 254 253 252)) (b (iarr '(signed-byte 64) 15 14)) (c 7)
         (r (funcall (vm-op "logand") a b c)))
    (assert-true (array-is r '(signed-byte 64) 4 (list (cl:logand 255 15 7) (cl:logand 254 14 7) (cl:logand 253 15 7) (cl:logand 252 14 7)))))
  (assert-true (array-is (funcall (vm-op "logior") 1 2 (iarr '(signed-byte 64) 4 8)) '(signed-byte 64) 2 '(7 11)))
  (assert-true (array-is (funcall (vm-op "logxor") (iarr '(signed-byte 64) 1 2) 3 (iarr '(signed-byte 64) 1 2 3 4))
                         '(signed-byte 64) 4 (list (cl:logxor 1 3 1) (cl:logxor 2 3 2) (cl:logxor 1 3 3) (cl:logxor 2 3 4))))
  (assert-true (array-is (funcall (vm-op "logeqv") (iarr '(signed-byte 64) 5 6) 3 -1) '(signed-byte 64) 2
                         (list (cl:logeqv 5 3 -1) (cl:logeqv 6 3 -1))))
  ;; one operand: a copy
  (let* ((a (iarr '(signed-byte 8) 1 2 3)) (r (funcall (vm-op "logand") a)))
    (assert-true (array-is r '(signed-byte 64) 3 '(1 2 3)))
    (assert-false (eq r a)))
  (assert-true (array-is (funcall (vm-op "logior") (iarr '(unsigned-byte 64) 18446744073709551615)) t 1 '(18446744073709551615)))
  ;; the first array comes last: scalars first
  (assert-true (array-is (funcall (vm-op "logand") -1 12 (iarr '(signed-byte 64) 10 6)) '(signed-byte 64) 2 '(8 4))))

;;; ---------------------------------------------------------------------------
;;; result element types

(deftest integer-result-element-types (integer-tests)
  (let ((logand (vm-op "logand")) (logior (vm-op "logior")))
    (assert-true (array-is (funcall logand (iarr '(unsigned-byte 8) 255 7) (iarr '(unsigned-byte 8) 15 15)) '(signed-byte 64) 2 '(15 7)))
    (assert-true (array-is (funcall logand (make-arr 'bit 2 '(1 1)) (make-arr 'bit 2 '(1 0))) '(signed-byte 64) 2 '(1 0)))
    (assert-true (array-is (funcall logior (iarr '(unsigned-byte 64) 18446744073709551615 1) (iarr '(unsigned-byte 64) 1 1))
                           t 2 '(18446744073709551615 1)))
    (assert-true (array-is (funcall logior (iarr '(unsigned-byte 64) 4 1) (iarr '(unsigned-byte 64) 1 1)) '(signed-byte 64) 2 '(5 1)))
    (assert-true (array-is (funcall logand (make-arr t 2 '(1 2)) (make-arr t 2 '(3 3))) '(signed-byte 64) 2 '(1 2)))
    (assert-true (array-is (funcall logand (make-arr t 2 (list (expt 2 70) 2)) (make-arr t 2 (list (expt 2 70) 3))) t 2 (list (expt 2 70) 2)))
    (assert-true (array-is (funcall (vm-op "logcount") (make-arr t 2 (list (expt 2 70) 3))) '(signed-byte 64) 2 '(1 2)))
    (assert-true (array-is (funcall (vm-op "integer-length") (make-arr t 2 (list (expt 2 70) 3))) '(signed-byte 64) 2 '(71 2)))
    (assert-true (array-is (funcall (vm-op "logbitp") 3 (iarr '(signed-byte 64) 8 7)) 'bit 2 '(1 0)))
    (assert-true (array-is (funcall (vm-op "logtest") (iarr '(signed-byte 64) 8 7) 8) 'bit 2 '(1 0)))
    (assert-true (array-is (funcall (vm-op "float") (iarr '(signed-byte 64) 1 2)) 'single-float 2 '(1f0 2f0)))
    (assert-true (array-is (funcall (vm-op "float") (iarr '(signed-byte 64) 1 2) 1d0) 'double-float 2 '(1d0 2d0)))
    (assert-true (array-is (funcall (vm-op "float") (darr 1d0 2d0)) 'double-float 2 '(1d0 2d0)))
    (assert-true (array-is (funcall (vm-op "float") (sarr 1f0 2f0)) 'single-float 2 '(1f0 2f0)))
    (assert-true (array-is (funcall (vm-op "float") (darr 1d0 2d0) 1f0) 'single-float 2 '(1f0 2f0)))
    (assert-true (array-is (funcall (vm-op "float") (varr 1 1/2 1d0)) t 3 '(1f0 0.5f0 1d0)))
    (assert-true (array-is (funcall (vm-op "float") (varr 1 1/2 2)) 'single-float 3 '(1f0 0.5f0 2f0)))
    (assert-true (array-is (funcall (vm-op "float") (varr 1 1/2) 1d0) 'double-float 2 '(1d0 0.5d0)))
    (assert-true (array-is (funcall (vm-op "float") 1 (varr 1d0 1d0)) 'double-float 2 '(1d0 1d0)))
    (assert-true (array-is (funcall (vm-op "float") 1/3 (darr 1d0 1d0)) 'double-float 2 (list (cl:float 1/3 1d0) (cl:float 1/3 1d0))))
    (assert-true (array-is (funcall (vm-op "float") (expt 10 20) (darr 1d0)) 'double-float 1 (list 1d20)))
    (assert-true (array-is (funcall (vm-op "rational") (darr 1d0 2d0)) '(signed-byte 64) 2 '(1 2)))
    (assert-true (array-is (funcall (vm-op "rational") (darr 1d0 0.5d0)) t 2 '(1 1/2)))
    (assert-true (array-is (funcall (vm-op "rational") (sarr 0.1f0)) t 1 (list (cl:rational 0.1f0))))
    (assert-true (array-is (funcall (vm-op "rationalize") (sarr 0.1f0 3f0)) t 2 '(1/10 3)))
    (assert-true (array-is (funcall (vm-op "rationalize") (darr 1d20)) t 1 (list (expt 10 20))))
    (assert-true (array-is (funcall (vm-op "rational") (darr 1d19)) t 1 (list 10000000000000000000)))
    (assert-true (array-is (funcall (vm-op "rational") (iarr '(signed-byte 8) 1 2)) '(signed-byte 64) 2 '(1 2)))
    (assert-true (array-is (funcall (vm-op "rational") (make-arr t 2 '(1 1/2))) t 2 '(1 1/2)))
    (assert-true (array-is (funcall (vm-op "numerator") (varr 1/2 3 4/6)) '(signed-byte 64) 3 '(1 3 2)))
    (assert-true (array-is (funcall (vm-op "denominator") (varr 1/2 3 4/6)) '(signed-byte 64) 3 '(2 1 3)))
    (assert-true (array-is (funcall (vm-op "denominator") (iarr '(unsigned-byte 8) 1 2 3)) '(signed-byte 64) 3 '(1 1 1)))
    (assert-true (array-is (funcall (vm-op "numerator") (iarr '(unsigned-byte 64) 18446744073709551615 1)) t 2 '(18446744073709551615 1)))
    (assert-true (array-is (funcall (vm-op "denominator") (iarr '(unsigned-byte 64) 18446744073709551615 1)) '(signed-byte 64) 2 '(1 1)))
    (assert-true (array-is (funcall (vm-op "scale-float") (darr 1d0 2d0) 2) 'double-float 2 '(4d0 8d0)))
    (assert-true (array-is (funcall (vm-op "scale-float") (sarr 1f0 2f0) #(1 2)) 'single-float 2 '(2f0 8f0)))
    (assert-true (array-is (funcall (vm-op "scale-float") 1d0 (iarr '(signed-byte 8) 1 2 3)) 'double-float 3 '(2d0 4d0 8d0)))
    (assert-true (array-is (funcall (vm-op "float-sign") (darr -1d0 2d0)) 'double-float 2 '(-1d0 1d0)))
    ;; formats differ: the result format is what CL's function returns (CCL: that of the second
    ;; argument, SBCL: the contagion)
    (let ((et (type-of (cl:float-sign -1d0 3f0))))
      (assert-true (array-is (funcall (vm-op "float-sign") (darr -1d0 2d0) 3f0) et 2 (list (cl:float-sign -1d0 3f0) (cl:float-sign 2d0 3f0)))))
    (let ((et (type-of (cl:float-sign -1f0 3d0))))
      (assert-true (array-is (funcall (vm-op "float-sign") -1f0 (darr 3d0 -4d0)) et 2 (list (cl:float-sign -1f0 3d0) (cl:float-sign -1f0 -4d0)))))
    (assert-true (array-is (funcall (vm-op "float-sign") (darr -1d0 2d0) (darr 3d0 -4d0)) 'double-float 2 '(-3d0 4d0)))
    (assert-true (array-is (funcall (vm-op "float-sign") (make-arr 'single-float 0 nil) (make-arr 'double-float 0 nil))
                           (type-of (cl:float-sign 1f0 1d0)) 0 nil))))

;;; ---------------------------------------------------------------------------
;;; scale-float: overflow and underflow under the trap mask

(deftest integer-scale-float-ieee (integer-tests)
  "Arrays: IEEE results under the mask (what CL's function gives with the traps masked); scalars: CL's
own signal with the traps enabled."
  (let ((sf (vm-op "scale-float")))
    ;; overflow gives infinity, underflow zero / denormal, on both implementations
    (let ((r (funcall sf (darr 1d300 -1d300 1d0 1d0 1d-300 5d-324) (iarr '(signed-byte 64) 100 100 1023 -1074 -100 -1))))
      (assert-true (same-bits-p (aref r 0) (inf+)))
      (assert-true (same-bits-p (aref r 1) (inf-)))
      (assert-equal 8.98846567431158d307 (aref r 2))
      (assert-equal 5d-324 (aref r 3))
      (assert-equal 0d0 (aref r 4))
      (assert-equal 0d0 (aref r 5)))
    (let ((r (funcall sf (sarr 1f0 -1f0 1f0 1f-30) (iarr '(signed-byte 64) 200 200 -149 -120))))
      (assert-true (same-bits-p (aref r 0) (single-inf+)))
      (assert-true (same-bits-p (aref r 1) (- (single-inf+))))
      (assert-equal 1.4012985f-45 (aref r 2))
      (assert-equal 0f0 (aref r 3)))
    ;; a scalar overflows as CL's function does with the traps enabled: an error
    (assert-equal :condition (car (outcome (lambda () (funcall sf 1d300 100)))))
    (assert-true (same-outcome-p (outcome (lambda () (cl:scale-float 1d300 100))) (outcome (lambda () (funcall sf 1d300 100)))))
    ;; ... and the array path does not signal even when traps are enabled around the call
    (assert-true (typep (funcall sf (darr 1d300) 100) 'array))))

;;; ---------------------------------------------------------------------------
;;; :into

(deftest integer-into (integer-tests)
  (let ((logand (vm-op "logand")) (ash (vm-op "ash")) (lognot (vm-op "lognot")) (logbitp (vm-op "logbitp"))
        (float (vm-op "float")) (a (iarr '(signed-byte 64) 12 10 6)) (b (iarr '(signed-byte 64) 10 10 3)))
    ;; integer out, T out, wider unsigned out
    (let ((out (make-array 3 :element-type '(signed-byte 64))))
      (assert-true (eq out (funcall logand a b :into out)))
      (assert-equal '(8 10 2) (contents out)))
    (let ((out (make-array 3)))
      (funcall logand a b :into out)
      (assert-equal '(8 10 2) (contents out)))
    (let ((out (make-array 3 :element-type '(unsigned-byte 8))))
      (funcall logand a b :into out)
      (assert-equal '(8 10 2) (contents out)))
    ;; a value that does not fit the out array: an output type error, nothing stored
    (let ((out (make-array 3 :element-type '(unsigned-byte 8) :initial-element 77)))
      (assert-true (typep (condition-of (funcall lognot a :into out)) 'vm:output-type-error))
      (assert-equal '(77 77 77) (contents out)))
    ;; recycling and truncation as for the other operators
    (let ((out (make-array 6 :element-type '(signed-byte 64))))
      (funcall ash a 1 :into out)
      (assert-equal '(24 20 12 24 20 12) (contents out)))
    (with-warnings (r ws) (let ((out (make-array 2 :element-type '(signed-byte 64)))) (funcall ash a 1 :into out) out)
      (assert-equal 1 (length (recycling-warnings ws)))
      (assert-equal '(24 20) (contents r)))
    ;; scalars into an array
    (let ((out (make-array 4 :element-type '(signed-byte 64))))
      (funcall logand 12 10 :into out)
      (assert-equal '(8 8 8 8) (contents out)))
    ;; in place
    (let ((x (iarr '(signed-byte 64) 1 2 3)))
      (funcall ash x 2 :into x)
      (assert-equal '(4 8 12) (contents x))
      (funcall logand x 10 :into x)
      (assert-equal '(0 8 8) (contents x)))
    ;; bit outputs
    (let ((out (make-array 3 :element-type 'bit)))
      (assert-true (eq out (funcall logbitp 0 (iarr '(signed-byte 64) 1 2 3) :into out)))
      (assert-equal '(1 0 1) (contents out)))
    (let ((out (make-array 3 :element-type '(signed-byte 64))))
      (funcall logbitp 0 (iarr '(signed-byte 64) 1 2 3) :into out)
      (assert-equal '(1 0 1) (contents out)))
    (let ((out (make-array 3 :element-type 'double-float)))
      (assert-true (typep (condition-of (funcall logbitp 0 (iarr '(signed-byte 64) 1 2 3) :into out)) 'vm:output-type-error)))
    (let ((out (make-array 1 :element-type 'bit)))
      (funcall (vm-op "logtest") 3 5 :into out)
      (assert-equal '(1) (contents out)))
    ;; float outputs
    (let ((out (make-array 3 :element-type 'double-float)))
      (assert-true (eq out (funcall float a 1d0 :into out)))
      (assert-equal '(12d0 10d0 6d0) (contents out)))
    (let ((out (make-array 3 :element-type 'double-float)))
      (funcall float (iarr '(signed-byte 64) 1 2 3) :into out)
      (assert-equal '(1d0 2d0 3d0) (contents out)))
    (let ((out (make-array 3 :element-type 'single-float)))
      (assert-true (typep (condition-of (funcall float a 1d0 :into out)) 'vm:output-type-error)))
    (let ((out (make-array 3 :element-type '(signed-byte 64))))
      (assert-true (typep (condition-of (funcall float (darr 1d0 2d0 3d0) :into out)) 'vm:output-type-error)))
    (let ((out (make-array 2 :element-type 'double-float)))
      (funcall (vm-op "scale-float") (darr 1d0 2d0) 3 :into out)
      (assert-equal '(8d0 16d0) (contents out)))
    ;; rational results
    (let ((out (make-array 2)))
      (funcall (vm-op "rational") (darr 0.5d0 2d0) :into out)
      (assert-equal '(1/2 2) (contents out)))
    (let ((out (make-array 2 :element-type '(signed-byte 64))))
      (funcall (vm-op "numerator") (varr 1/2 3) :into out)
      (assert-equal '(1 3) (contents out)))
    ;; not an array
    (assert-equal 'type-error (type-of (condition-of (funcall logand a b :into 5))))
    (assert-equal 'type-error (type-of (condition-of (funcall logbitp 1 a :into 5))))
    ;; numbers and :into: the value fills the array
    (let ((out (make-array 2 :element-type '(signed-byte 64))))
      (funcall ash 3 2 :into out)
      (assert-equal '(12 12) (contents out)))))

;;; ---------------------------------------------------------------------------
;;; no argument is mutated; other array kinds

(defun snapshot (x) (if (arrayp x) (list (array-dimensions x) (array-element-type x) (copy-list (contents-row-major x))) x))

(deftest integer-no-mutation (integer-tests)
  (let* ((a (iarr '(signed-byte 64) 5 -6 7)) (b (iarr '(signed-byte 64) 1 2 3)) (d (darr 1.5d0 -2.5d0 3d0)) (v (varr 1 2 3))
         (u (iarr '(unsigned-byte 64) 18446744073709551615 1 2))
         (args (list a b d v u)))
    (let ((before (mapcar #'snapshot args)))
      (dolist (name *nary-bitwise*) (funcall (vm-op name) a b) (funcall (vm-op name) a) (funcall (vm-op name) u b))
      (dolist (name *binary-bitwise*) (funcall (vm-op name) a b) (funcall (vm-op name) u 3))
      (dolist (name *unary-int*) (funcall (vm-op name) a) (funcall (vm-op name) u) (funcall (vm-op name) v))
      (funcall (vm-op "ash") a b) (funcall (vm-op "ash") u 1) (funcall (vm-op "ash") 1 b)
      (funcall (vm-op "logbitp") 1 a) (funcall (vm-op "logtest") a b)
      (funcall (vm-op "float") a) (funcall (vm-op "float") a d) (funcall (vm-op "float") d) (funcall (vm-op "float") d 1f0)
      (funcall (vm-op "float") v) (funcall (vm-op "float") u 1d0)
      (funcall (vm-op "rational") d) (funcall (vm-op "rationalize") d) (funcall (vm-op "numerator") v) (funcall (vm-op "denominator") a)
      (funcall (vm-op "rational") a) (funcall (vm-op "numerator") a)
      (funcall (vm-op "scale-float") d a) (funcall (vm-op "float-sign") d) (funcall (vm-op "float-sign") d d)
      (assert-equal before (mapcar #'snapshot args)))
    ;; the result of a one-argument bitwise operator or numerator is a fresh array, never the argument
    (assert-false (eq a (funcall (vm-op "logand") a)))
    (assert-false (eq a (funcall (vm-op "numerator") a)))
    (assert-false (eq a (funcall (vm-op "rational") a)))
    (assert-false (eq d (funcall (vm-op "float") d)))))

(deftest integer-non-simple-arrays (integer-tests)
  "Fill-pointer vectors, adjustable and displaced arrays are read through their active elements."
  (let* ((fp (make-array 5 :element-type '(signed-byte 64) :fill-pointer 3 :initial-contents '(1 2 3 4 5)))
         (adj (make-array 3 :element-type '(signed-byte 64) :adjustable t :initial-contents '(6 7 8)))
         (base (make-array 6 :element-type '(signed-byte 64) :initial-contents '(10 11 12 13 14 15)))
         (disp (make-array 3 :element-type '(signed-byte 64) :displaced-to base :displaced-index-offset 2)))
    (assert-true (array-is (funcall (vm-op "logand") fp 3) '(signed-byte 64) 3 '(1 2 3)))
    (assert-true (array-is (funcall (vm-op "ash") adj 1) '(signed-byte 64) 3 '(12 14 16)))
    (assert-true (array-is (funcall (vm-op "logior") disp 1) '(signed-byte 64) 3 '(13 13 15)))
    (assert-true (array-is (funcall (vm-op "float") disp) 'single-float 3 '(12f0 13f0 14f0)))
    (assert-true (array-is (funcall (vm-op "lognot") (make-array '(2 2) :element-type '(signed-byte 64) :initial-contents '((1 2) (3 4))))
                           '(signed-byte 64) '(2 2) '(-2 -3 -4 -5)))))

;;; ---------------------------------------------------------------------------
;;; compiler macros and compiled calls

(defparameter *int-operator-forms*
  '((vm:logand 12 10) (vm:logior 12 10) (vm:logxor 12 10) (vm:logeqv 12 10) (vm:lognand 12 10) (vm:lognor 12 10)
    (vm:logandc1 12 10) (vm:logandc2 12 10) (vm:logorc1 12 10) (vm:logorc2 12 10) (vm:lognot 12) (vm:logcount 12)
    (vm:integer-length 12) (vm:ash 12 2) (vm:logbitp 2 12) (vm:logtest 12 4) (vm:float 12) (vm:float 12 1d0)
    (vm:rational 12) (vm:rationalize 0.5) (vm:numerator 3/4) (vm:denominator 3/4) (vm:scale-float 1d0 3)
    (vm:float-sign -1d0) (vm:float-sign -1d0 3d0)))

(deftest integer-compiler-macro-shapes (integer-tests)
  (flet ((expand (form) (funcall (compiler-macro-function (car form)) form nil)))
    (dolist (form *int-operator-forms*)
      (let* ((name (car form)) (cl-name (find-symbol (symbol-name name) :cl)))
        ;; constant numbers: straight to the CL operator
        (assert-equal (cons cl-name (cdr form)) (expand form))
        ;; :into and wrong argument counts are left alone
        (let ((f (append form '(:into out)))) (assert-true (eq f (expand f))))
        (unless (member (symbol-name name) *nary-bitwise* :test #'string-equal)
          (let ((f (list* name (loop repeat 9 collect 1)))) (assert-true (eq f (expand f)))))))
    (assert-equal -1 (expand '(vm:logand)))
    (assert-equal -1 (expand '(vm:logeqv)))
    (assert-equal 0 (expand '(vm:logior)))
    (assert-equal 0 (expand '(vm:logxor)))
    (let ((f '(vm:lognand))) (assert-true (eq f (expand f))))
    (let ((f '(vm:lognot))) (assert-true (eq f (expand f))))
    (let ((f '(vm:ash 1))) (assert-true (eq f (expand f))))
    (assert-equal 'if (first (let ((e (expand '(vm:logand a b)))) (unwrap-locally (if (eq (car e) 'let*) (third e) e)))))))

(deftest integer-compiled-calls (integer-tests)
  (let ((f (compile nil '(lambda (x y) (vm:logand x y))))
        (g (compile nil '(lambda (x y) (declare (type (signed-byte 32) x y)) (vm:logxor x y))))
        (h (compile nil '(lambda (x n) (vm:ash x n))))
        (k (compile nil '(lambda (x) (vm:float x)))))
    (assert-equal 8 (funcall f 12 10))
    (assert-equal 6 (funcall g 12 10))
    (assert-equal 48 (funcall h 12 2))
    (assert-equal 1.0 (funcall k 1))
    (assert-true (array-is (funcall f (iarr '(signed-byte 64) 12 10) 6) '(signed-byte 64) 2 '(4 2)))
    (assert-true (array-is (funcall h (iarr '(signed-byte 64) 1 2) (iarr '(signed-byte 64) 1 2)) '(signed-byte 64) 2 '(2 8)))
    (assert-true (array-is (funcall k (iarr '(signed-byte 64) 1 2)) 'single-float 2 '(1f0 2f0)))
    (assert-equal 'type-error (type-of (condition-of (funcall f 1.5 2))))
    (assert-equal 'type-error (type-of (condition-of (funcall f "a" 2))))
    (assert-equal 'type-error (type-of (condition-of (funcall f (darr 1d0) 2))))))

;;; ---------------------------------------------------------------------------
;;; the operators in a package that shadows them

(deftest integer-operators-in-shadowing-package (integer-tests)
  (let ((p (make-user-package)))
    (assert-equal 8 (eval-in p "(logand 12 10)"))
    (assert-equal -1 (eval-in p "(logand)"))
    (assert-equal 8 (eval-in p "(ash 1 3)"))
    (assert-equal 1.0d0 (eval-in p "(float 1 1d0)"))
    (assert-equal 1/2 (eval-in p "(rational 0.5)"))
    (assert-equal '(1 2) (eval-in p "(list (numerator 1/2) (denominator 1/2))"))
    (assert-equal 2.0d0 (eval-in p "(scale-float 1d0 1)"))
    (assert-equal -1.0d0 (eval-in p "(float-sign -3d0)"))
    (assert-true (eval-in p "(and (logbitp 1 2) (logtest 6 3) (not (logtest 4 3)))"))
    (assert-equal '(5 7 2) (eval-in p "(list (logcount 31) (integer-length 100) (integer-length -3))"))
    (assert-true (eval-in p "(equalp (logand (make-array 2 :element-type '(signed-byte 64) :initial-contents '(12 10)) 6) #(4 2))"))
    (assert-true (eval-in p "(equalp (float (make-array 2 :element-type '(signed-byte 64) :initial-contents '(1 2)) 1d0) #(1d0 2d0))"))
    (assert-true (eval-in p "(let ((a (make-array 3 :element-type '(signed-byte 64) :initial-contents '(1 2 3)))) (equalp (ash a 1 :into a) #(2 4 6)))"))
    (assert-true (eval-in p "(typep (float (make-array 2 :element-type 'double-float :initial-element 1d0)) '(simple-array double-float (*)))"))))

(deftest integer-ash-array-by-scalar-edges (integer-tests)
  "ASH of a (signed-byte 64) array by every scalar count from -70 to 70, over values at the edges of what
fits after the shift (the array/scalar kernel computes the bounds once): equal to CL's ASH per element,
and the result array is (signed-byte 64) exactly when every element fits."
  (let* ((vals (remove-duplicates
                (loop for k from 0 to 63
                      append (list (expt 2 k) (- (expt 2 k)) (1- (expt 2 k)) (1+ (expt 2 k))
                                   (- 1 (expt 2 k)) (- -1 (expt 2 k))))))
         (vals (remove-if-not (lambda (v) (typep v '(signed-byte 64))) (append '(0 1 -1 2 -2 3 -3) vals)))
         (a (apply #'iarr '(signed-byte 64) vals)))
    (loop for n from -70 to 70
          do (let* ((r (vm:ash a n))
                    (e (mapcar (lambda (x) (cl:ash x n)) vals)))
               (assert-equal e (contents r) n)
               (assert-equal (if (every (lambda (x) (typep x '(signed-byte 64))) e) '(signed-byte 64) t)
                             (array-element-type r) n)))))
