;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; Backend agreement for VM:- VM:* VM:/ .
;;; Bitwise identity of MKL and the Lisp loop is required (and checked) for real + - * /
;;; and complex + -, because only there may the size cutoff switch backends.  Complex *
;;; and / are not bit-identical: they use MKL at every size, so the result must not
;;; depend on the array length; they are checked against CL within a stated tolerance
;;; (ops.lisp, COMPLEX-ARRAYS-ALL-OPS: 32 ulp).
(in-package #:vm-tests)

(defsuite ops-backend-tests (vm-tests))

(defun op-symbol (op) (intern (op-name op) :vm.impl))

(defun bitwise-ops-for (type)
  "The operations whose MKL and Lisp results must be bit-identical for TYPE."
  (if (member type '(:csingle :cdouble))
      (remove-if-not (lambda (op) (string= (op-name op) "-")) *new-ops*)
      *new-ops*))

(deftest mkl-and-lisp-bitwise-identical-all-ops (ops-backend-tests)
  "Real - * / and complex -: MKL (cutoff 0) vs Lisp loop (cutoff huge); sizes around the
cutoffs; array-array, array-scalar, scalar-array; zeros, denormals, Inf, NaN, near-overflow."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (dolist (op (bitwise-ops-for type))
        (dolist (n '(1 2 255 256 257 1023 1024 1025 2047 2048 2049 4096 100000))
          (let* ((a (make-special-array type n (cl:+ n 1)))
                 (b (make-special-array type n (cl:+ n 2)))
                 (s (scalar-of type)))
            (dolist (case (list (list "aa" (lambda () (funcall (op-vm op) a b)))
                                (list "as" (lambda () (funcall (op-vm op) a s)))
                                (list "sa" (lambda () (funcall (op-vm op) s b)))
                                (list "recip" (lambda () (if (string= (op-name op) "/") (funcall (op-vm op) a) a)))))
              (let ((mkl (with-cutoff (0 (op-symbol op)) (funcall (second case))))
                    (lisp (with-cutoff (most-positive-fixnum (op-symbol op)) (funcall (second case)))))
                (unless (arrays-same-bits-p mkl lisp)
                  (push (list type (op-name op) n (first case)) bad))))))))
    (when bad (format t "~&~{~S~%~}" bad))
    (assert-equal nil bad)))

(deftest lisp-path-without-mkl-all-ops (ops-backend-tests)
  "With *MKL-AVAILABLE* false everything runs in Lisp: identical to MKL for real - * / and complex -,
close for complex * / ."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (dolist (op *new-ops*)
        (let* ((n 3000) (a (make-special-array type n 7)) (b (make-special-array type n 8))
               (mkl (with-cutoff (0 (op-symbol op)) (funcall (op-vm op) a b)))
               (lisp (let ((vm:*mkl-available* nil)) (with-cutoff (0 (op-symbol op)) (funcall (op-vm op) a b)))))
          (when (member op (bitwise-ops-for type))
            (unless (arrays-same-bits-p mkl lisp) (push (list type (op-name op)) bad))))))
    (assert-equal nil bad)))

(deftest results-match-scalar-cl-all-ops (ops-backend-tests)
  "Elementwise agreement with scalar CL ops evaluated with traps masked, both backends."
  (let ((bad '()))
    (dolist (type '(:single :double :csingle :cdouble))
      (dolist (op (bitwise-ops-for type))
        (let* ((n 1100) (a (make-special-array type n 21)) (b (make-special-array type n 22)))
          (dolist (cut (list 0 most-positive-fixnum))
            (let ((r (with-cutoff (cut (op-symbol op)) (funcall (op-vm op) a b))))
              (dotimes (i n)
                (unless (same-bits-p (aref r i) (masked-cl (op-cl op) (aref a i) (aref b i)))
                  (push (list type (op-name op) cut i) bad))))))))
    (assert-equal nil bad)))

(deftest complex-mul-div-independent-of-length (ops-backend-tests)
  "Complex * and /: with MKL available the result for an element does not depend on the
array length or on the cutoff (they use MKL at every size)."
  (when vm:*mkl-available*
    (let ((bad '()))
      (dolist (type '(:csingle :cdouble))
        (dolist (op (remove "-" *new-ops* :key #'op-name :test #'string=))
          (let* ((big (make-special-array type 5000 31)) (big2 (make-special-array type 5000 32))
                 (s (scalar-of type))
                 (full (funcall (op-vm op) big big2))
                 (full-s (funcall (op-vm op) big s)))
            (dolist (n '(1 2 3 7 17 255 256 1000))
              (let ((small (funcall (op-vm op) (subseq big 0 n) (subseq big2 0 n)))
                    (small-s (funcall (op-vm op) (subseq big 0 n) s)))
                (unless (and (arrays-same-bits-p small (subseq full 0 n))
                             (arrays-same-bits-p small-s (subseq full-s 0 n)))
                  (push (list type (op-name op) n) bad))))
            ;; the cutoff setting makes no difference either
            (let ((a (with-cutoff (0 (op-symbol op)) (funcall (op-vm op) big big2)))
                  (b (with-cutoff (most-positive-fixnum (op-symbol op)) (funcall (op-vm op) big big2))))
              (unless (arrays-same-bits-p a b) (push (list type (op-name op) :cutoff) bad))))))
      (when bad (format t "~&~{~S~%~}" bad))
      (assert-equal nil bad))))

(deftest inf-nan-no-signal-all-ops (ops-backend-tests)
  "Overflow, Inf - Inf and x/0 in arrays give IEEE results, never a floating point condition."
  (dolist (cut (list 0 most-positive-fixnum))
    (dolist (opname '("-" "*" "/"))
      (setf (vm:cutoff (intern opname :vm.impl)) cut))
    (unwind-protect
         (let* ((n 2000)
                (big (make-array n :element-type 'double-float :initial-element (big-double)))
                (inf (make-array n :element-type 'double-float :initial-element (inf+)))
                (zero (make-array n :element-type 'double-float :initial-element 0d0)))
           (assert-true (every (lambda (x) (eql x (inf+))) (contents (* big big))))
           (assert-true (every (lambda (x) (eql x (inf-))) (contents (- (- big) big big))))
           (assert-true (every #'nanp (contents (- inf inf))))
           (assert-true (every #'nanp (contents (* zero inf))))
           (assert-true (every #'nanp (contents (/ zero zero))))
           (assert-true (every #'nanp (contents (/ inf inf))))
           (assert-true (every (lambda (x) (eql x (inf+))) (contents (/ big 0d0))))
           (assert-true (every (lambda (x) (eql x 0d0)) (contents (/ big inf))))
           (let ((s (make-array n :element-type 'single-float :initial-element (single-big))))
             (assert-true (every (lambda (x) (eql x (single-inf+))) (contents (* s s)))))
           (assert-true (typep (condition-of (opaque (* (big-double) (big-double)))) 'floating-point-overflow))
           (assert-true (typep (condition-of (opaque (/ 1d0 (opaque 0d0)))) 'division-by-zero)))
      (dolist (opname '("-" "*" "/"))
        (setf (vm:cutoff (intern opname :vm.impl)) (vm.impl::default-cutoff opname))))))

(deftest signed-zero-and-denormals-all-ops (ops-backend-tests)
  (dolist (cut (list 0 most-positive-fixnum))
    (let* ((n 600)
           (nz (make-array n :element-type 'double-float :initial-element -0d0))
           (pz (make-array n :element-type 'double-float :initial-element 0d0))
           (d (make-array n :element-type 'double-float :initial-element least-positive-double-float))
           (two (make-array n :element-type 'double-float :initial-element 2d0)))
      (with-cutoff (cut '-)
        (assert-true (every (lambda (x) (eql x 0d0)) (contents (- pz pz))))
        (assert-true (every (lambda (x) (eql x -0d0)) (contents (- nz pz))))
        (assert-true (every (lambda (x) (eql x 0d0)) (contents (- nz nz))))
        (assert-true (every (lambda (x) (eql x least-positive-double-float)) (contents (- (+ d d) d)))))
      (with-cutoff (cut '*)
        (assert-true (every (lambda (x) (eql x -0d0)) (contents (* nz two))))
        (assert-true (every (lambda (x) (eql x (cl:* 2 least-positive-double-float))) (contents (* d two))))
        (assert-true (every (lambda (x) (eql x 0d0)) (contents (* d d)))))
      (with-cutoff (cut '/)
        (assert-true (every (lambda (x) (eql x least-positive-double-float)) (contents (/ (* d two) two))))
        (assert-true (every (lambda (x) (eql x -0d0)) (contents (/ nz two))))))))

(deftest cutoff-defaults-and-accessor (ops-backend-tests)
  ;; the defaults of phase 9 (bench/cutoff.lisp): see DEFAULT-CUTOFF in src/kernels.lisp
  (assert-equal #+ccl 192 #+sbcl 64 #-(or ccl sbcl) 256 (vm:cutoff '/))
  (dolist (op '(+ - *))
    (assert-equal #+ccl 128 #+sbcl 256 #-(or ccl sbcl) 256 (vm:cutoff op)))
  (dolist (op '(- * /))
    (let ((old (vm:cutoff op)))
      (setf (vm:cutoff op) 4321)
      (assert-equal 4321 (vm:cutoff op))
      (assert-equal 4321 (vm:cutoff (intern (symbol-name op) :vm.impl)))
      (setf (vm:cutoff op) old))))

(deftest kernel-registry-all-ops (ops-backend-tests)
  (dolist (op '(+ - *))
    (dolist (key '(:single-float :double-float :complex-single-float :complex-double-float :signed-byte-64 :t))
      (dolist (variant '(:aa :as :sa))
        (assert-true (functionp (vm:find-kernel op key variant))))))
  (dolist (key '(:single-float :double-float :complex-single-float :complex-double-float :t))
    (dolist (variant '(:aa :as :sa))
      (assert-true (functionp (vm:find-kernel '/ key variant)))))
  (dolist (key '(:single-float :double-float :complex-single-float :complex-double-float :signed-byte-64 :t))
    (assert-true (functionp (vm:find-kernel "neg" key :u))))
  ;; replacing a kernel takes effect
  (let* ((orig (vm:find-kernel '- :signed-byte-64 :sa)) (calls 0))
    (vm:register-kernel '- :signed-byte-64 :sa (lambda (&rest args) (incf calls) (apply orig args)))
    (unwind-protect
         (progn (assert-true (array-is (- 10 (iarr '(signed-byte 32) 1 2)) '(signed-byte 64) 2 '(9 8)))
                (assert-equal 1 calls))
      (vm:register-kernel '- :signed-byte-64 :sa orig))))
