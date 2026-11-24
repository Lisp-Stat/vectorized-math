;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vm-tests)

(defsuite contagion-tests (vm-tests))

;;; name family element-type sample-data
(defparameter *array-types*
  `((ub8 :int (unsigned-byte 8) (1 2 250))
    (sb32 :int (signed-byte 32) (-5 0 2000000000))
    (fixnum :int fixnum (-7 3 4000000000))
    (sb64 :int (signed-byte 64) (-9 2 5))
    (single :sf single-float (1.5f0 -2.25f0 3f0))
    (double :df double-float (0.5d0 1d10 -3d0))
    (csingle :csf (complex single-float) (#c(1f0 2f0) #c(0.5f0 -1f0) #c(0f0 3f0)))
    (cdouble :cdf (complex double-float) (#c(1d0 2d0) #c(-1d0 0.5d0) #c(0d0 3d0)))
    (t :gen t (1 1/2 2.5d0))))

(defparameter *scalar-types*
  `((fixnum-s :int 7) (bignum-s :int ,(expt 10 30)) (ratio-s :rat 1/3)
    (single-s :sf 2.5f0) (double-s :df 1.25d0)
    (csingle-s :csf #c(1f0 1f0)) (cdouble-s :cdf #c(1d0 2d0)) (crat-s :crat #c(1 2))))

(defun family-float (f)
  "Float precision (0 none, 1 single, 2 double) and complexness of a family."
  (ecase f ((:int :rat :gen) (values 0 nil)) (:crat (values 0 t))
    (:sf (values 1 nil)) (:df (values 2 nil)) (:csf (values 1 t)) (:cdf (values 2 t))))

(defun expected-et (fa fb values)
  "Model of the result element type for families FA, FB with the computed VALUES."
  (if (or (eq fa :gen) (eq fb :gen))
      t
      (multiple-value-bind (pa ca) (family-float fa)
        (multiple-value-bind (pb cb) (family-float fb)
          (let ((p (max pa pb)) (c (or ca cb)))
            (cond ((zerop p) (if (and (not c) (every (lambda (v) (typep v '(signed-byte 64))) values)
                                     (member fa '(:int)) (member fb '(:int)))
                                 '(signed-byte 64)
                                 t))
                  ((= p 1) (if c '(complex single-float) 'single-float))
                  (t (if c '(complex double-float) 'double-float))))))))

(defun make-sample (spec)
  (destructuring-bind (name family et data) spec
    (declare (ignore name family))
    (make-arr et 3 data)))

(defun check-pair (label fa fb a b expected-data)
  "A and B arrays or scalars; return NIL on success, else a mismatch description."
  (let* ((r (handler-case (+ a b) (error (e) e)))
         (want-et (expected-et fa fb expected-data)))
    (cond ((not (arrayp r)) (format nil "~A: ~S" label r))
          ((not (array-is r want-et 3 expected-data))
           (format nil "~A: got ~S ~S, wanted ~S ~S" label (array-element-type r) (contents r)
                   want-et expected-data)))))

(deftest contagion-array-array (contagion-tests)
  "Every pair of element types: element type and values (values by CL:+ per element)."
  (let ((bad '()))
    (dolist (x *array-types*)
      (dolist (y *array-types*)
        (let* ((a (make-sample x)) (b (make-sample y))
               (ca (copy-arr a)) (cb (copy-arr b))
               (want (mapcar #'cl:+ (contents a) (contents b)))
               (msg (check-pair (list (first x) (first y)) (second x) (second y) a b want)))
          (when msg (push msg bad))
          (unless (and (same-contents-p a ca) (same-contents-p b cb))
            (push (list :mutated (first x) (first y)) bad)))))
    (when bad (format t "~&~{~S~%~}" bad))
    (assert-equal nil bad)))

(deftest contagion-array-scalar (contagion-tests)
  "Every array type with every scalar type, scalar on either side."
  (let ((bad '()))
    (dolist (x *array-types*)
      (dolist (s *scalar-types*)
        (let* ((a (make-sample x)) (sv (third s))
               (fa (second x)) (fs (second s)))
          ;; array + scalar
          (let ((msg (check-pair (list (first x) (first s)) fa fs a sv
                                 (mapcar (lambda (e) (cl:+ e sv)) (contents a)))))
            (when msg (push msg bad)))
          ;; scalar + array
          (let ((msg (check-pair (list (first s) (first x)) fs fa sv a
                                 (mapcar (lambda (e) (cl:+ sv e)) (contents a)))))
            (when msg (push msg bad))))))
    (when bad (format t "~&~{~S~%~}" bad))
    (assert-equal nil bad)))

(deftest integer-results (contagion-tests)
  (let ((max64 (1- (expt 2 63))) (min64 (- (expt 2 63))))
    ;; overflow impossible by ranges: typed (signed-byte 64)
    (assert-true (array-is (+ (iarr '(unsigned-byte 8) 200 100) (iarr '(unsigned-byte 8) 100 200))
                           '(signed-byte 64) 2 '(300 300)))
    (assert-true (array-is (+ (iarr 'fixnum most-positive-fixnum 1) (iarr 'fixnum most-positive-fixnum 1))
                           '(signed-byte 64) 2 (list (* 2 most-positive-fixnum) 2)))
    ;; overflow possible by ranges but values fit: still (signed-byte 64)
    (assert-true (array-is (+ (iarr '(signed-byte 64) 5 6) (iarr '(signed-byte 64) 1 2))
                           '(signed-byte 64) 2 '(6 8)))
    ;; overflow really happens: T array with bignums
    (assert-true (array-is (+ (iarr '(signed-byte 64) max64 1) (iarr '(signed-byte 64) 1 1))
                           t 2 (list (expt 2 63) 2)))
    (assert-true (array-is (+ (iarr '(signed-byte 64) min64 1) -1) t 2 (list (1- min64) 0)))
    (assert-true (array-is (+ (iarr '(unsigned-byte 64) (1- (expt 2 64)) 1) 1) t 2 (list (expt 2 64) 2)))
    (assert-true (array-is (+ (iarr '(unsigned-byte 8) 1 2) (expt 10 30)) t 2
                           (list (cl:+ 1 (expt 10 30)) (cl:+ 2 (expt 10 30)))))
    ;; big scalar that nevertheless produces values that fit
    (assert-true (array-is (+ (iarr '(signed-byte 64) -10 -20) (expt 2 63))
                           '(signed-byte 64) 2 (list (cl:- (expt 2 63) 10) (cl:- (expt 2 63) 20))))
    ;; ratio with integer arrays: element type T
    (assert-true (array-is (+ (iarr '(unsigned-byte 8) 1 2) 1/2) t 2 '(3/2 5/2)))
    ;; fold: overflow in the second step
    (assert-true (array-is (+ (iarr '(signed-byte 64) 1 2) (iarr '(signed-byte 64) 1 1) max64)
                           t 2 (list (cl:+ 2 max64) (cl:+ 3 max64))))
    ;; bit arrays
    (assert-true (array-is (+ (make-array 3 :element-type 'bit :initial-contents '(1 0 1)) 1)
                           '(signed-byte 64) 3 '(2 1 2)))))

(deftest float-contagion-specifics (contagion-tests)
  (assert-true (array-is (+ (iarr '(unsigned-byte 8) 1 2) 1.5f0) 'single-float 2 '(2.5f0 3.5f0)))
  (assert-true (array-is (+ (sarr 1f0 2f0) 1d0) 'double-float 2 '(2d0 3d0)))
  (assert-true (array-is (+ (sarr 1f0 2f0) (darr 1d0 2d0)) 'double-float 2 '(2d0 4d0)))
  (assert-true (array-is (+ (darr 1d0 2d0) #c(1d0 1d0)) '(complex double-float) 2 '(#c(2d0 1d0) #c(3d0 1d0))))
  (assert-true (array-is (+ (sarr 1f0) #c(1d0 1d0)) '(complex double-float) 1 '(#c(2d0 1d0))))
  (assert-true (array-is (+ (iarr 'fixnum 1) #c(1f0 1f0)) '(complex single-float) 1 '(#c(2f0 1f0))))
  (assert-true (array-is (+ (csarr #c(1f0 1f0)) (darr 1d0)) '(complex double-float) 1 '(#c(2d0 1d0))))
  (assert-true (array-is (+ (sarr 1f0) #c(1 2)) '(complex single-float) 1 '(#c(2f0 2f0)))))

(deftest backend-types-preserved-in-folds (contagion-tests)
  (assert-true (array-is (+ (sarr 1f0 2f0) (sarr 1f0 2f0) (sarr 1f0 2f0)) 'single-float 2 '(3f0 6f0)))
  (assert-true (array-is (+ (csarr #c(1f0 1f0)) 1 2.5f0) '(complex single-float) 1 '(#c(4.5f0 1f0)))))
