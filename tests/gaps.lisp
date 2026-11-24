;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Non-numeric arrays, rank-0 arrays, threads.
(in-package #:vm-tests)

(defsuite gap-tests (vm-tests))

(defun assert-number-type-error (c datum)
  (assert-true (typep c 'type-error))
  (assert-true (eq datum (type-error-datum c)))
  (assert-equal 'number (type-error-expected-type c)))

(deftest non-numeric-arrays (gap-tests)
  "Arrays that cannot hold numbers: TYPE-ERROR, expected type NUMBER, datum the array."
  (let ((s (make-string 3 :initial-element #\a))
        (v (vector 1 2 'x))
        (b (make-array 2 :element-type 'character :initial-element #\z)))
    (dolist (x (list s v b))
      (assert-number-type-error (condition-of (+ x 1)) x)
      (assert-number-type-error (condition-of (+ 1 x)) x)
      (assert-number-type-error (condition-of (+ (darr 1d0) x)) x)
      (assert-number-type-error (condition-of (+ x)) x))
    (assert-number-type-error (condition-of (+ 1 2 s)) s)
    ;; same as CL for the datum of a plain non-number
    (assert-equal 'number (type-error-expected-type (condition-of (apply #'cl:+ (opaque (list s 1))))))))

(deftest t-array-non-number-with-empty-result (gap-tests)
  "A non-number inside a T array is detected even when the result has zero length."
  (let ((bad (vector 'a)))
    (assert-number-type-error (condition-of (+ (make-array 0) bad)) bad)
    (assert-number-type-error (condition-of (+ bad (make-array 0))) bad)
    (assert-number-type-error (condition-of (+ (darr) bad)) bad)
    (assert-number-type-error (condition-of (+ bad (make-array '(0 3) :element-type 'double-float))) bad)
    (let ((bad2 (make-array 3 :initial-contents '(1 #\x 3))))
      (assert-number-type-error (condition-of (+ (vector 1 2) bad2)) bad2))))

(deftest rank-0-arrays (gap-tests)
  "A rank-0 array behaves as a length-1 array whose result is rank-0 (when no operand has more elements)."
  (flet ((r0 (x &optional (et 'double-float)) (make-array '() :element-type et :initial-element x)))
    (let ((r (+ (r0 1d0) 1)))
      (assert-true (array-is r 'double-float '() '(2d0)))
      (assert-equal 0 (array-rank r)))
    (assert-true (array-is (+ 1 (r0 1d0)) 'double-float '() '(2d0)))
    (assert-true (array-is (+ (r0 1d0) (r0 2d0)) 'double-float '() '(3d0)))
    (assert-true (array-is (+ (r0 1d0) (r0 2 '(signed-byte 64))) 'double-float '() '(3d0)))
    (assert-true (array-is (+ (r0 1d0)) 'double-float '() '(1d0)))
    (assert-true (array-is (+ (r0 2 t) (r0 3 t)) t '() '(5)))
    (assert-true (array-is (+ (r0 #c(1d0 1d0) '(complex double-float)) 1) '(complex double-float) '() '(#c(2d0 1d0))))
    ;; with a vector: the rank-0 array is a length-1 operand, the result is the vector's shape
    (assert-true (array-is (+ (r0 1d0) (darr 1d0 2d0 3d0)) 'double-float 3 '(2d0 3d0 4d0)))
    (assert-true (array-is (+ (iarr '(signed-byte 64) 1 2) (r0 10 '(signed-byte 64))) '(signed-byte 64) 2 '(11 12)))
    ;; with a matrix
    (let ((r (+ (r0 1d0) (make-arr 'double-float '(2 2) '(1d0 2d0 3d0 4d0)))))
      (assert-true (array-is r 'double-float '(2 2) '(2d0 3d0 4d0 5d0))))
    ;; zero length
    (assert-true (array-is (+ (r0 1d0) (darr)) 'double-float 0 '()))
    ;; large enough for MKL
    (let ((big (make-array 5000 :element-type 'double-float :initial-element 1d0)))
      (assert-true (every (lambda (x) (= x 2d0)) (contents (+ (r0 1d0) big)))))
    ;; :into
    (let ((out (r0 0d0)))
      (+ (r0 1d0) 2d0 :into out)
      (assert-true (array-is out 'double-float '() '(3d0))))
    (let ((out (darr 0d0 0d0)))
      (+ (r0 1d0) 2d0 :into out)
      (assert-true (array-is out 'double-float 2 '(3d0 3d0))))
    ;; input unchanged
    (let ((a (r0 1d0))) (+ a 1d0) (assert-true (array-is a 'double-float '() '(1d0))))))

(deftest mkl-available-exported (gap-tests)
  (assert-true (eq :external (nth-value 1 (find-symbol "*MKL-AVAILABLE*" :vm))))
  (assert-true (or (eq vm:*mkl-available* t) (null vm:*mkl-available*))))

(defun thread-sum-job (seed)
  "Worker: sum double arrays above the MKL cutoff several times; return (values ok vml-mode)."
  (let* ((n 20000)
         (a (make-array n :element-type 'double-float))
         (b (make-array n :element-type 'double-float))
         (ok t))
    (dotimes (i n) (setf (aref a i) (cl:* 1d0 (cl:+ i seed)) (aref b i) (cl:* 0.5d0 i)))
    (dotimes (rep 50)
      (let ((r (+ a b)))
        (dotimes (i n)
          (unless (= (aref r i) (cl:+ (aref a i) (aref b i))) (setf ok nil)))))
    (list ok (vm.impl::%vml-get-mode))))

(deftest four-threads (gap-tests)
  "vm:+ on double arrays from 4 threads at once: correct results, VML mode set in each thread."
  (when vm:*mkl-available*
    (let* ((before (bt:join-thread (bt:make-thread (lambda () (vm.impl::%vml-get-mode)))))
           (threads (loop for k below 4
                          collect (let ((k k)) (bt:make-thread (lambda () (thread-sum-job (* 1000 k)))))))
           (results (mapcar #'bt:join-thread threads)))
      ;; a fresh thread starts in the MKL default mode, not ours
      (assert-true (/= before vm.impl::+vml-mode+))
      (dolist (r results)
        (assert-true (first r))
        (assert-equal vm.impl::+vml-mode+ (logand (second r) #xFFFFFFFF))))))

;;; Several operator families from several threads at once: the kernels share nothing mutable (the
;;; stack scratch block of the NaN test and of the division kernels is per call, the float trap state and
;;; the VML mode are per thread), so every thread must get exactly what CL gives per element.

(defun thread-families-job (seed)
  "Worker: sqrt, floor, round, mod, rem, ffloor, max, <, + and * on (signed-byte 64), float; returns true when every
element of every result equals CL's, on each of 20 repetitions over arrays of 3000 elements (above the cutoffs)."
  (let* ((n 3000) (ok t)
         (d (make-array n :element-type 'double-float))
         (e (make-array n :element-type 'double-float))
         (s (make-array n :element-type 'single-float))
         (f (make-array n :element-type 'single-float))
         (i1 (make-array n :element-type '(signed-byte 64)))
         (i2 (make-array n :element-type '(signed-byte 64))))
    (dotimes (i n)
      (let ((k (cl:+ i seed)))
        (setf (aref d i) (cl:* (cl:+ 1d0 (cl:mod k 977)) 0.37d0 (if (oddp k) 1 -1))
              (aref e i) (cl:+ 0.5d0 (cl:* 0.25d0 (cl:mod (cl:* k 7) 41)))
              (aref s i) (cl:* (cl:+ 1f0 (cl:mod k 311)) 0.5f0 (if (oddp (cl:* k 3)) 1 -1))
              (aref f i) (cl:+ 0.5f0 (cl:mod (cl:* k 5) 23))
              (aref i1 i) (cl:* (cl:- (cl:mod k 1001) 500) 1000003)
              (aref i2 i) (cl:- (cl:mod (cl:* k 13) 777) 388))))
    (flet ((masked (fn &rest args) (vm.impl::with-masked-float-traps (apply fn args)))
           (check (what result reference)
             (unless (and (equal (array-dimensions result) (list n))
                          (dotimes (i n t)
                            (unless (same-bits-p (aref result i) (funcall reference i)) (return nil))))
               (setf ok nil))
             what))
      (let ((pd (make-array n :element-type 'double-float)))
        (dotimes (i n) (setf (aref pd i) (cl:abs (aref d i))))
        (dotimes (rep 20)
          (check "sqrt" (vm:sqrt pd) (lambda (i) (masked #'cl:sqrt (aref pd i))))
          (check "+" (+ d e) (lambda (i) (cl:+ (aref d i) (aref e i))))
          (check "max" (vm:max d e) (lambda (i) (cl:max (aref d i) (aref e i))))
          (check "<" (vm:< d e) (lambda (i) (if (cl:< (aref d i) (aref e i)) 1 0)))
          (check "floor q" (values (vm:floor d e)) (lambda (i) (values (masked #'cl:floor (aref d i) (aref e i)))))
          (check "floor r" (nth-value 1 (vm:floor d e)) (lambda (i) (nth-value 1 (masked #'cl:floor (aref d i) (aref e i)))))
          (check "round q" (values (vm:round d e)) (lambda (i) (values (masked #'cl:round (aref d i) (aref e i)))))
          (check "mod" (vm:mod d e) (lambda (i) (masked #'cl:mod (aref d i) (aref e i))))
          (check "rem single" (vm:rem s f) (lambda (i) (masked #'cl:rem (aref s i) (aref f i))))
          (check "ffloor single" (values (vm:ffloor s f)) (lambda (i) (values (masked #'cl:ffloor (aref s i) (aref f i)))))
          (check "+ sb64" (+ i1 i2) (lambda (i) (cl:+ (aref i1 i) (aref i2 i))))
          (check "* sb64" (* i1 i2) (lambda (i) (cl:* (aref i1 i) (aref i2 i))))
          (check "float" (vm:float d 1f0) (lambda (i) (masked #'cl:float (aref d i) 1f0))))))
    ok))

(deftest threads-several-families (gap-tests)
  "Four threads run arithmetic, sqrt, comparison, max, floor, round, mod, ffloor, (signed-byte 64) arithmetic and float conversion concurrently, each repeatedly checking every element against CL."
  (let* ((threads (loop for k below 4
                        collect (let ((k k)) (bt:make-thread (lambda () (thread-families-job (cl:* 100 (cl:1+ k))))))))
         (results (mapcar #'bt:join-thread threads)))
    (dolist (r results) (assert-true r))))

;;; --- locating MKL ---

(defmacro with-muffled-vm-warning ((count) &body body)
  `(let ((,count 0))
     (handler-bind ((warning (lambda (w)
                               (when (search "VECTORIZED-MATH" (princ-to-string w))
                                 (incf ,count) (muffle-warning w)))))
       ,@body)))

(deftest load-mkl-exported (gap-tests)
  (assert-true (eq :external (nth-value 1 (find-symbol "LOAD-MKL" :vm))))
  (assert-true (eq :external (nth-value 1 (find-symbol "*MKL-LIBRARY*" :vm)))))

(deftest load-mkl-bogus-path (gap-tests)
  "A bad path: one warning, MKL unavailable, Lisp loops give the right answers; reloading the
original location restores MKL."
  (let ((saved-lib vm:*mkl-library*) (saved-avail vm:*mkl-available*)
        (a (darr 0.5d0 1d0 2d0)))
    (unwind-protect
         (progn
           (with-muffled-vm-warning (n)
             (assert-false (vm:load-mkl "/nonexistent/directory/libmkl_rt.so"))
             (assert-equal 1 n))
           (assert-false vm:*mkl-available*)
           (assert-equal "/nonexistent/directory/libmkl_rt.so" vm:*mkl-library*)
           (let ((r (vm:exp a)))
             (assert-true (every #'= (contents r) (mapcar #'cl:exp (contents a))))))
      (with-muffled-vm-warning (n) (vm:load-mkl saved-lib))
      (assert-equal (and saved-avail t) (and vm:*mkl-available* t)))))
