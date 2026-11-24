;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:vm-tests
  (:use #:cl #:clunit)
  ;; This package shadows the operators exactly as a user package would: + - * / 1+ 1- incf decf
  ;; below are the VM operators (see VM:SHADOWED-SYMBOLS).
  (:shadowing-import-from #:vm #:+ #:- #:* #:/ #:1+ #:1- #:incf #:decf)
  (:export #:vm-tests #:run)
  (:documentation "Tests for vectorized-math (VM:+ - * / 1+ 1- incf decf).  Remember that these are the VM operators in this package;
the CL ones are written explicitly where the reference behaviour is wanted."))

(in-package #:vm-tests)

(defsuite vm-tests ())

(defun run (&optional interactive?)
  "Run all the tests for vectorized-math."
  (let ((*test-output-stream* *standard-output*))
    (prog1 (run-suite 'vm-tests :use-debugger interactive?)
      (when (fboundp 'report-ulp-table) (funcall 'report-ulp-table)))))

;;; --- helpers ---

(defun make-arr (et dims data)
  "Array of element type ET and dimensions DIMS filled row-major from the list DATA."
  (let ((a (make-array dims :element-type et)))
    (loop for x in data for i from 0 do (setf (row-major-aref a i) x))
    a))

(defun darr (&rest xs) (make-arr 'double-float (length xs) xs))
(defun sarr (&rest xs) (make-arr 'single-float (length xs) xs))
(defun cdarr (&rest xs) (make-arr '(complex double-float) (length xs) xs))
(defun csarr (&rest xs) (make-arr '(complex single-float) (length xs) xs))
(defun iarr (et &rest xs) (make-arr et (length xs) xs))
(defun varr (&rest xs) (make-arr t (length xs) xs))

(defun contents (a) (loop for i below (array-total-size a) collect (row-major-aref a i)))

(defun array-is (a et dims data)
  "True when A is a simple array of exactly element type ET, dimensions DIMS and contents DATA (EQL)."
  (and (arrayp a)
       (typep a 'simple-array)
       (equal (array-element-type a) (upgraded-array-element-type et))
       (equal (array-dimensions a) (if (listp dims) dims (list dims)))
       (every #'eql (contents a) data)))

(defun copy-arr (a)
  (let ((c (make-array (array-dimensions a) :element-type (array-element-type a))))
    (dotimes (i (array-total-size a) c) (setf (row-major-aref c i) (row-major-aref a i)))))

(defun same-contents-p (a b)
  (and (equal (array-dimensions a) (array-dimensions b))
       (equal (array-element-type a) (array-element-type b))
       (every #'eql (contents a) (contents b))))

(defun collect-warnings (thunk)
  "Call THUNK; return (values result list-of-warning-conditions).  Warnings are muffled."
  (let ((ws '()))
    (let ((r (handler-bind ((warning (lambda (w) (push w ws) (muffle-warning w))))
               (funcall thunk))))
      (values r (nreverse ws)))))

(defmacro with-warnings ((result warnings) form &body body)
  `(multiple-value-bind (,result ,warnings) (collect-warnings (lambda () ,form))
     (declare (ignorable ,result ,warnings))
     ,@body))

(defun recycling-warnings (ws) (remove-if-not (lambda (w) (typep w 'vm:recycling-warning)) ws))

(defmacro condition-of (form)
  "The condition signalled by FORM (an ERROR), or NIL."
  `(handler-case (progn ,form nil) (error (c) c)))

(declaim (notinline opaque))
(defun opaque (x) x)

(defun big-double () (opaque most-positive-double-float))

(defun inf+ ()
  (vm.impl::with-masked-float-traps (let ((b (big-double))) (cl:+ b b))))
(defun inf- () (vm.impl::with-masked-float-traps (cl:- (inf+))))
(defun nan () (vm.impl::with-masked-float-traps (let ((i (inf+))) (cl:- i i))))
(defun single-big () (opaque most-positive-single-float))
(defun single-inf+ () (vm.impl::with-masked-float-traps (let ((b (single-big))) (cl:+ b b))))
(defun single-nan () (vm.impl::with-masked-float-traps (let ((i (single-inf+))) (cl:- i i))))

(defun nanp (x)
  (typecase x
    ;; independent of the code under test and of the implementation's comparisons: SBCL's predicate,
    ;; the IEEE bits on CCL (its own NAN-OR-INFINITY-P / INFINITY-P are not reliable)
    (float #+sbcl (sb-ext:float-nan-p x)
           #+ccl (etypecase x
                   (double-float (multiple-value-bind (hi lo) (ccl::double-float-bits x)
                                   (let ((h (logand hi #x7fffffff)))
                                     (or (> h #x7ff00000) (and (= h #x7ff00000) (/= lo 0))))))
                   (single-float (> (logand (ccl::single-float-bits x) #x7fffffff) #x7f800000)))
           #-(or sbcl ccl) (/= x (opaque x)))
    (complex (or (nanp (realpart x)) (nanp (imagpart x))))
    (t nil)))

(defun same-bits-p (a b)
  "EQL, except that any NaN equals any NaN (payloads are not compared)."
  (cond ((eql a b) t)
        ((and (complexp a) (complexp b))
         (and (same-bits-p (realpart a) (realpart b)) (same-bits-p (imagpart a) (imagpart b))))
        (t (and (nanp a) (nanp b)))))

(defun arrays-same-bits-p (a b)
  (and (equal (array-dimensions a) (array-dimensions b))
       (equal (array-element-type a) (array-element-type b))
       (every #'same-bits-p (contents a) (contents b))))
