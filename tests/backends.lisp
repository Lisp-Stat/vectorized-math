;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vm-tests)

(defsuite backend-tests (vm-tests))

(defun lcg-stream (seed)
  (let ((s seed))
    (lambda ()
      (setf s (mod (cl:+ (* s 6364136223846793005) 1442695040888963407) (expt 2 64)))
      (ash s -33))))

(defun make-special-array (type n seed)
  "Array of TYPE (:single :double :csingle :cdouble) with zeros, denormals, Inf, NaN, near-overflow and ordinary values."
  (let* ((next (lcg-stream seed))
         (single (member type '(:single :csingle)))
         (cplx (member type '(:csingle :cdouble)))
         (proto (if single 1f0 1d0))
         (big (if single (single-big) (big-double)))
         (tiny (if single least-positive-single-float least-positive-double-float))
         (inf (if single (single-inf+) (inf+)))
         (nan (if single (single-nan) (nan)))
         (et (cond ((eq type :single) 'single-float) ((eq type :double) 'double-float)
                   ((eq type :csingle) '(complex single-float)) (t '(complex double-float))))
         (a (make-array n :element-type et)))
    (flet ((one ()
             (vm.impl::with-masked-float-traps
               (let* ((r (funcall next)) (sign (if (evenp (ash r -20)) 1 -1))
                      (v (case (mod r 10)
                           (0 (if (evenp (ash r -4)) (float 0 proto) (cl:- (float 0 proto))))
                           (1 (cl:* sign tiny (float (cl:+ 1 (mod (ash r -3) 3)) proto)))
                           (2 (cl:* sign inf))
                           (3 nan)
                           (4 (cl:* sign big (float (/ (cl:+ 1 (mod (ash r -3) 100)) 100) proto)))
                           (t (cl:* sign (float (/ (mod (ash r -3) 1000000) 997) proto)
                                    (float (expt 10 (- (mod (ash r -9) 9) 4)) proto))))))
                 v))))
      (dotimes (i n a)
        (setf (aref a i) (if cplx (complex (one) (one)) (one)))))))

(defun scalar-of (type)
  (ecase type
    (:single 1.5f0) (:double 0.1d0)
    (:csingle #c(0.5f0 -2f0)) (:cdouble #c(0.1d0 -2d0))))

(defmacro with-cutoff ((value &optional (op ''+)) &body body)
  "Run BODY with the size cutoff of OP (default +) set to VALUE."
  (let ((o (gensym "OP")) (old (gensym "OLD")))
    `(let* ((,o ,op) (,old (vm:cutoff ,o)))
       (setf (vm:cutoff ,o) ,value)
       (unwind-protect (progn ,@body) (setf (vm:cutoff ,o) ,old)))))

(defun run-masked-add (a b) (vm.impl::with-masked-float-traps (cl:+ a b)))

(deftest mkl-and-lisp-bitwise-identical (backend-tests)
  "All four float types, sizes around the cutoff and large; array-array, array-scalar, scalar-array."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (dolist (n '(1 2 255 256 257 1023 1024 1025 4096 100000))
        (let* ((a (make-special-array type n (cl:+ n 1)))
               (b (make-special-array type n (cl:+ n 2)))
               (s (scalar-of type)))
          (dolist (case (list (list "aa" (lambda () (+ a b)))
                              (list "as" (lambda () (+ a s)))
                              (list "sa" (lambda () (+ s b)))))
            (let ((mkl (with-cutoff (0) (funcall (second case))))
                  (lisp (with-cutoff (most-positive-fixnum) (funcall (second case)))))
              (unless (arrays-same-bits-p mkl lisp)
                (push (list type n (first case)) bad)))))))
    (assert-equal nil bad)))

(deftest lisp-path-without-mkl (backend-tests)
  "With *MKL-AVAILABLE* false everything runs in Lisp and gives the same results."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (let* ((n 3000) (a (make-special-array type n 7)) (b (make-special-array type n 8))
             (mkl (with-cutoff (0) (+ a b)))
             (lisp (let ((vm:*mkl-available* nil)) (with-cutoff (0) (+ a b)))))
        (unless (arrays-same-bits-p mkl lisp) (push type bad))))
    (assert-equal nil bad)))

(deftest results-match-scalar-cl-plus (backend-tests)
  "Elementwise agreement with scalar CL:+ evaluated with traps masked, both backends."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (let* ((n 1100) (a (make-special-array type n 21)) (b (make-special-array type n 22)))
        (dolist (cut (list 0 most-positive-fixnum))
          (let ((r (with-cutoff (cut) (+ a b))))
            (dotimes (i n)
              (unless (same-bits-p (aref r i) (run-masked-add (aref a i) (aref b i)))
                (push (list type cut i) bad)))))))
    (assert-equal nil bad)))

(deftest inf-nan-no-signal (backend-tests)
  "Overflow and Inf - Inf in arrays give IEEE results, never a floating point condition."
  (dolist (cut (list 0 most-positive-fixnum))
    (with-cutoff (cut)
      (let* ((n 2000)
             (big (make-array n :element-type 'double-float :initial-element (big-double)))
             (r (+ big big)))
        (assert-true (every (lambda (x) (eql x (inf+))) (contents r)))
        (let ((r2 (+ r (make-array n :element-type 'double-float :initial-element (inf-)))))
          (assert-true (every #'nanp (contents r2)))))
      (let* ((n 2000)
             (big (make-array n :element-type 'single-float :initial-element (single-big)))
             (r (+ big big)))
        (assert-true (every (lambda (x) (eql x (single-inf+))) (contents r))))
      (let* ((r (+ (darr (big-double)) (darr (big-double)))))
        (assert-true (eql (aref r 0) (inf+))))
      (let* ((r (+ (cdarr (complex (big-double) 0d0)) (complex (big-double) 0d0))))
        (assert-true (eql (realpart (aref r 0)) (inf+))))
      ;; scalars still behave as CL afterwards: the right condition, not a stale-flag one
      (assert-true (typep (condition-of (opaque (+ (big-double) (big-double)))) 'floating-point-overflow)))))

(deftest signed-zero-and-denormals (backend-tests)
  (dolist (cut (list 0 most-positive-fixnum))
    (with-cutoff (cut)
      (let* ((n 600)
             (z (make-array n :element-type 'double-float :initial-element -0d0))
             (r (+ z z))
             (d (make-array n :element-type 'double-float :initial-element least-positive-double-float))
             (r2 (+ d d)))
        (assert-true (every (lambda (x) (eql x -0d0)) (contents r)))
        (assert-true (every (lambda (x) (eql x (cl:+ least-positive-double-float least-positive-double-float)))
                            (contents r2)))))))

(deftest cutoff-accessor (backend-tests)
  (let ((old (vm:cutoff '+)))
    (assert-true (integerp old))
    (setf (vm:cutoff '+) 12345)
    (assert-equal 12345 (vm:cutoff '+))
    (assert-equal 12345 (vm:cutoff 'cl:+))
    (setf (vm:cutoff '+) old)
    (assert-equal old (vm:cutoff '+))))

(deftest kernel-registry (backend-tests)
  "Integer kernels can be replaced through the registry."
  (let* ((orig (vm:find-kernel '+ :signed-byte-64 :aa))
         (calls 0))
    (vm:register-kernel '+ :signed-byte-64 :aa
                        (lambda (&rest args) (incf calls) (apply orig args)))
    (unwind-protect
         (progn (assert-true (array-is (+ (iarr '(signed-byte 32) 1 2) (iarr '(signed-byte 32) 3 4))
                                       '(signed-byte 64) 2 '(4 6)))
                (assert-equal 1 calls))
      (vm:register-kernel '+ :signed-byte-64 :aa orig))))

(deftest shadowed-symbols-list (backend-tests)
  (assert-equal (list 'vm:+ 'vm:- 'vm:* 'vm:/ 'vm:1+ 'vm:1- 'vm:incf 'vm:decf
                      'vm:exp 'vm:expt 'vm:log 'vm:sqrt 'vm:isqrt 'vm:sin 'vm:cos 'vm:tan 'vm:asin
                      'vm:acos 'vm:atan 'vm:sinh 'vm:cosh 'vm:tanh 'vm:asinh 'vm:acosh 'vm:atanh
                      'vm:cis 'vm:abs 'vm:signum 'vm:conjugate 'vm:phase 'vm:realpart 'vm:imagpart
                      'vm:complex
                      'vm:= 'vm:/= 'vm:< 'vm:> 'vm:<= 'vm:>= 'vm:zerop 'vm:plusp 'vm:minusp 'vm:evenp 'vm:oddp
                      'vm:max 'vm:min
                      'vm:floor 'vm:ceiling 'vm:truncate 'vm:round 'vm:ffloor 'vm:fceiling 'vm:ftruncate
                      'vm:fround 'vm:mod 'vm:rem 'vm:gcd 'vm:lcm
                      'vm:logand 'vm:logior 'vm:logxor 'vm:logeqv 'vm:lognand 'vm:lognor 'vm:logandc1 'vm:logandc2
                      'vm:logorc1 'vm:logorc2 'vm:lognot 'vm:logcount 'vm:integer-length 'vm:ash 'vm:logbitp 'vm:logtest
                      'vm:float 'vm:rational 'vm:rationalize 'vm:numerator 'vm:denominator 'vm:scale-float 'vm:float-sign)
                (vm:shadowed-symbols))
  (assert-equal 81 (length (vm:shadowed-symbols)))
  (dolist (s (vm:shadowed-symbols))
    (assert-true (eq (symbol-package s) (find-package :vm)))
    (assert-false (eq s (find-symbol (symbol-name s) :cl)))))
