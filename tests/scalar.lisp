;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vm-tests)

(defsuite scalar-tests (vm-tests))

(defparameter *corpus*
  (list 0 1 -1 42 most-positive-fixnum (1+ most-positive-fixnum) (- most-negative-fixnum 1)
        (expt 10 30) 1/2 -3/7 1/3
        1.5f0 -0.0f0 0f0 2d0 -0.0d0 1d300 1.25d-5
        #c(1 2) #c(1/2 3) #c(1.5f0 2.5f0) #c(1d0 -1d0) #c(0.5d0 0d0)))

(defun call-both (args)
  "Return (values vm-result cl-result) or both conditions."
  (flet ((try (fn) (handler-case (apply fn args) (error (c) c))))
    (values (try #'vm:+) (try #'cl:+))))

(defun scalar-identical (a b)
  (or (eql a b)
      (and (typep a 'error) (typep b 'error)
           (eq (type-of a) (type-of b))
           (or (not (typep a 'type-error))
               (and (eql (type-error-datum a) (type-error-datum b))
                    (equal (type-error-expected-type a) (type-error-expected-type b)))))))

(deftest scalar-zero-and-one-args (scalar-tests)
  "(+) and (+ x) agree with CL."
  (assert-true (scalar-identical (vm:+) (apply #'cl:+ nil)))
  (assert-true (eql (vm:+) 0))
  (dolist (x *corpus*)
    (multiple-value-bind (v c) (call-both (list x))
      (assert-true (scalar-identical v c)))))

(deftest scalar-two-args (scalar-tests)
  "Every pair from the corpus: same value and same type as CL:+."
  (let ((ok t))
    (dolist (x *corpus*)
      (dolist (y *corpus*)
        (multiple-value-bind (v c) (call-both (list x y))
          (unless (and (scalar-identical v c) (equal (type-of v) (type-of c)))
            (setf ok nil) (format t "~&MISMATCH ~S ~S: ~S vs ~S~%" x y v c)))))
    (assert-true ok)))

(deftest scalar-three-and-four-args (scalar-tests)
  "Triples and quadruples drawn from the corpus agree with CL:+ (left-fold association)."
  (let ((ok t) (n (length *corpus*)))
    (dotimes (i n)
      (dotimes (j n)
        (let ((x (nth i *corpus*)) (y (nth j *corpus*)))
          (dolist (z (list 0 -7 1/3 2.5f0 3d0 #c(1 1)))
            (multiple-value-bind (v c) (call-both (list x y z))
              (unless (and (scalar-identical v c) (equal (type-of v) (type-of c)))
                (setf ok nil) (format t "~&MISMATCH ~S ~S ~S~%" x y z)))
            (multiple-value-bind (v c) (call-both (list x y z 1/7))
              (unless (and (scalar-identical v c) (equal (type-of v) (type-of c)))
                (setf ok nil) (format t "~&MISMATCH4 ~S ~S ~S~%" x y z)))))))
    (assert-true ok)))

(deftest scalar-non-numbers (scalar-tests)
  "Non-numbers signal the same TYPE-ERROR (datum and expected type) as CL:+."
  (dolist (args (list '(a) '(1 a) '(a 1)  '(1 2 #\y) '(:into) '(nil) '((1 2)) '(1 (1 2)) '(1 2 3 #\x)
                      '(1.5 :foo)))
    (let ((v (condition-of (apply #'vm:+ args)))
          (c (condition-of (apply #'cl:+ args))))
      (assert-true (typep v 'type-error))
      (assert-true (scalar-identical v c)))))

(deftest scalar-fp-traps-unchanged (scalar-tests)
  "Scalar overflow still signals, as in CL, also after array operations ran."
  (let ((big (big-double)))
    (assert-true (typep (condition-of (opaque (vm:+ big big))) 'floating-point-overflow))
    (vm:+ (darr 1d0 2d0) (darr 3d0 4d0))
    (assert-true (typep (condition-of (opaque (vm:+ big big))) 'floating-point-overflow))
    (assert-true (typep (condition-of (opaque (cl:+ big big))) 'floating-point-overflow))))
