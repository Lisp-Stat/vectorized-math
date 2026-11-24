;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; VM:- VM:* VM:/ VM:1+ VM:1- VM:INCF VM:DECF: scalar identity, shapes, contagion,
;;; integers, division, complex, :into, no mutation.  In this package + - * / 1+ 1-
;;; incf decf are the VM operators; the CL ones are written cl:op.
(in-package #:vm-tests)

(defsuite ops-tests (vm-tests))

(defparameter *binary-ops*
  (list (list "+" #'vm:+ #'cl:+) (list "-" #'vm:- #'cl:-)
        (list "*" #'vm:* #'cl:*) (list "/" #'vm:/ #'cl:/))
  "(name vm-function cl-function)")
(defparameter *new-ops* (rest *binary-ops*))

(defun op-name (op) (first op))
(defun op-vm (op) (second op))
(defun op-cl (op) (third op))

(defun masked-cl (cl-fn &rest args)
  "CL-FN applied to ARGS with float traps masked (the reference for array elements)."
  (vm.impl::with-masked-float-traps (apply cl-fn args)))

;;; ---------------------------------------------------------------------------
;;; scalar identity with CL

(defparameter *ops-corpus*
  (list 0 1 -1 7 -42 most-positive-fixnum (1+ most-positive-fixnum) (- most-negative-fixnum 1)
        (expt 10 30) 1/2 -3/7 1/3
        1.5f0 -0.0f0 0f0 2d0 -0.0d0 1d300 1.25d-5 1d-300
        #c(1 2) #c(1/2 3) #c(1.5f0 2.5f0) #c(1d0 -1d0) #c(0.5d0 0d0)))

(defun call-pair (vm-fn cl-fn args)
  (flet ((try (fn) (handler-case (apply fn args) (error (c) c))))
    (values (try vm-fn) (try cl-fn))))

(defun same-result-p (v c)
  "Same value and type, or the same kind of error."
  (or (and (eql v c) (equal (type-of v) (type-of c)))
      (and (typep v 'error) (typep c 'error)
           (eq (type-of v) (type-of c))
           (or (not (typep v 'type-error))
               (and (eql (type-error-datum v) (type-error-datum c))
                    (equal (type-error-expected-type v) (type-error-expected-type c)))))))

(deftest scalar-arities-and-errors (ops-tests)
  "No arguments, one argument and wrong argument counts behave as in CL, for every operator."
  (dolist (op *binary-ops*)
    (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) '())
      (assert-true (same-result-p v c))
      ;; (+) = 0 and (*) = 1; (-) and (/) are errors, the very error CL signals
      (assert-equal (if (member (op-name op) '("+" "*") :test #'string=) nil t) (typep v 'error)))
    (dolist (x *ops-corpus*)
      (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) (list x))
        (assert-true (same-result-p v c)))))
  (assert-equal 0 (vm:+))
  (assert-equal 1 (vm:*))
  (let ((v (condition-of (apply #'vm:- (opaque '()))))
        (c (condition-of (apply #'cl:- (opaque '())))))
    (assert-true (typep v 'error))
    (assert-equal (type-of c) (type-of v)))
  (let ((v (condition-of (apply #'vm:/ (opaque '()))))
        (c (condition-of (apply #'cl:/ (opaque '())))))
    (assert-true (typep v 'error))
    (assert-equal (type-of c) (type-of v)))
  (dolist (pair (list (list #'vm:1+ #'cl:1+) (list #'vm:1- #'cl:1-)))
    (dolist (args (list '() '(1 2) '(1 2 3)))
      (multiple-value-bind (v c) (call-pair (first pair) (second pair) args)
        (assert-true (typep v 'error))
        (assert-equal (type-of c) (type-of v))))))

(deftest scalar-two-args-all-ops (ops-tests)
  "Every pair from the corpus, every operator: same value, type or condition as CL."
  (dolist (op *new-ops*)
    (let ((bad '()))
      (dolist (x *ops-corpus*)
        (dolist (y *ops-corpus*)
          (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) (list x y))
            (unless (same-result-p v c) (push (list (op-name op) x y v c) bad)))))
      (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 5 (length bad)))))
      (assert-equal nil bad))))

(deftest scalar-n-args-all-ops (ops-tests)
  "Three and four arguments fold left as in CL."
  (dolist (op *new-ops*)
    (let ((bad '()) (n (length *ops-corpus*)))
      (dotimes (i n)
        (dotimes (j n)
          (let ((x (nth i *ops-corpus*)) (y (nth j *ops-corpus*)))
            (dolist (z (list 0 -7 1/3 2.5f0 3d0 #c(1 1)))
              (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) (list x y z))
                (unless (same-result-p v c) (push (list (op-name op) x y z) bad)))
              (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) (list x y z 1/7))
                (unless (same-result-p v c) (push (list (op-name op) x y z 1/7) bad)))))))
      (assert-equal nil bad))))

(deftest scalar-1+-1- (ops-tests)
  (dolist (pair (list (list #'vm:1+ #'cl:1+) (list #'vm:1- #'cl:1-)))
    (dolist (x (append *ops-corpus* (list 'a "s" nil :k '(1 2))))
      (multiple-value-bind (v c) (call-pair (first pair) (second pair) (list x))
        (assert-true (same-result-p v c))))))

(deftest scalar-non-numbers-all-ops (ops-tests)
  "Non-numbers signal the same TYPE-ERROR as CL."
  (dolist (op *new-ops*)
    (dolist (args (list '(a) '(1 a) '(a 1) '(1 2 #\y) '(nil) '((1 2)) '(1 (1 2)) '(1.5 :foo)))
      (multiple-value-bind (v c) (call-pair (op-vm op) (op-cl op) args)
        (assert-true (typep v 'type-error))
        (assert-true (same-result-p v c))))))

(deftest scalar-fp-traps-unchanged-all-ops (ops-tests)
  "Scalar overflow and division by zero still signal, also after array operations ran."
  (let ((big (big-double)) (zero (opaque 0d0)))
    (dolist (round '(:before :after))
      (when (eq round :after)
        (let ((a (darr 1d0 2d0)))
          (vm:- a a) (vm:* a a) (vm:/ a a) (vm:/ a (darr 0d0 0d0))))
      (assert-true (typep (condition-of (opaque (vm:* big big))) 'floating-point-overflow))
      (assert-true (typep (condition-of (opaque (vm:- big (cl:- big)))) 'floating-point-overflow))
      (assert-true (typep (condition-of (opaque (vm:/ 1d0 zero))) 'division-by-zero))
      (assert-true (typep (condition-of (opaque (vm:/ 1 (opaque 0)))) 'division-by-zero))
      (assert-true (typep (condition-of (opaque (vm:/ (opaque 0)))) 'division-by-zero)))))

;;; ---------------------------------------------------------------------------
;;; shapes and recycling: model-based, non-commutative order, column-major

(defun fvals (n &optional (scale 1.5d0) (offset 1))
  (loop for i from 0 below n collect (cl:* scale (float (cl:+ i offset) 1d0))))

(defun dvec (list) (make-arr 'double-float (length list) list))

(defun column-major-index (dims r)
  "Column-major index of the element with row-major index R in an array of dimensions DIMS."
  (let ((subs '()) (c 0) (stride 1))
    (dolist (d (reverse dims))
      (push (mod r d) subs) (setf r (floor r d)))
    (loop for s in subs for d in dims
          do (setf c (cl:+ c (cl:* s stride)) stride (cl:* stride d)))
    c))

(defun ref-vector-vector (fn a b)
  (let* ((na (length a)) (nb (length b)) (n (cl:max na nb)))
    (loop for i below n collect (funcall fn (nth (mod i na) a) (nth (mod i nb) b)))))

(defun ref-matrix-vector (fn dims mdata v matrix-first)
  "Row-major result list; the vector recycles over the matrix in column-major order."
  (loop for r below (length mdata)
        collect (let ((m (nth r mdata)) (x (nth (mod (column-major-index dims r) (length v)) v)))
                  (if matrix-first (funcall fn m x) (funcall fn x m)))))

(deftest vector-vector-model (ops-tests)
  "Vector and vector, either order, sizes around the block and cyclic paths, both backends."
  (dolist (op *binary-ops*)
    (let ((bad '()))
      (dolist (cut (list 0 most-positive-fixnum))
        (with-cutoff (cut (intern (op-name op) :vm.impl))
          (dolist (case '((40 10) (100 20) (50 20) (37 5) (64 16) (33 17) (7 3) (8 2) (5 5) (1 4) (3 1) (20 20)))
            (destructuring-bind (na nb) case
              (let* ((la (fvals na 1.5d0 1)) (lb (fvals nb 0.25d0 3))
                     (a (dvec la)) (b (dvec lb))
                     (want (ref-vector-vector (op-cl op) la lb)))
                (with-warnings (r ws) (funcall (op-vm op) a b)
                  (unless (array-is r 'double-float (cl:max na nb) want)
                    (push (list (op-name op) :ab na nb cut) bad)))
                (with-warnings (r ws) (funcall (op-vm op) b a)
                  (unless (array-is r 'double-float (cl:max na nb) (ref-vector-vector (op-cl op) lb la))
                    (push (list (op-name op) :ba na nb cut) bad))))))))
      (when bad (format t "~&~{~S~%~}" bad))
      (assert-equal nil bad))))

(deftest matrix-vector-model (ops-tests)
  "Matrix and vector, either order, column-major recycling, all the matrix paths
(rows, general, same size), both backends."
  (dolist (op *binary-ops*)
    (let ((bad '()))
      (dolist (cut (list 0 most-positive-fixnum))
        (with-cutoff (cut (intern (op-name op) :vm.impl))
          (dolist (case '(((2 3) 2) ((2 3) 3) ((2 3) 1) ((2 3) 6) ((4 20) 4) ((4 20) 8) ((3 4 5) 3)
                          ((3 4 5) 6) ((5 4) 7) ((2 2 2) 4) ((20 3) 20) ((3 20) 3) ((3 20) 60)))
            (destructuring-bind (dims nv) case
              (let* ((size (reduce #'cl:* dims))
                     (ld (fvals size 1.5d0 1)) (lv (fvals nv 0.25d0 3))
                     (m (make-arr 'double-float dims ld)) (v (dvec lv)))
                (with-warnings (r ws) (funcall (op-vm op) m v)
                  (unless (array-is r 'double-float dims (ref-matrix-vector (op-cl op) dims ld lv t))
                    (push (list (op-name op) :mv dims nv cut) bad)))
                (with-warnings (r ws) (funcall (op-vm op) v m)
                  (unless (array-is r 'double-float dims (ref-matrix-vector (op-cl op) dims ld lv nil))
                    (push (list (op-name op) :vm dims nv cut) bad)))))))
        (when bad (format t "~&~{~S~%~}" bad)))
      (assert-equal nil bad))))

(deftest non-commutative-by-hand (ops-tests)
  "(- v s) and (- s v), (/ v s) and (/ s v); a 2x3 matrix with a length-2 vector by hand."
  (let ((v (darr 8d0 4d0 2d0)))
    (assert-true (array-is (- v 1d0) 'double-float 3 '(7d0 3d0 1d0)))
    (assert-true (array-is (- 1d0 v) 'double-float 3 '(-7d0 -3d0 -1d0)))
    (assert-true (array-is (/ v 2d0) 'double-float 3 '(4d0 2d0 1d0)))
    (assert-true (array-is (/ 8d0 v) 'double-float 3 '(1d0 2d0 4d0)))
    (assert-true (array-is (- v (darr 1d0 2d0 3d0)) 'double-float 3 '(7d0 2d0 -1d0)))
    (assert-true (array-is (- (darr 1d0 2d0 3d0) v) 'double-float 3 '(-7d0 -2d0 1d0)))
    (assert-true (array-is (- v 1d0 1d0) 'double-float 3 '(6d0 2d0 0d0)))       ; (v-1)-1
    (assert-true (array-is (/ 64d0 v 2d0) 'double-float 3 '(4d0 8d0 16d0))))     ; (64/v)/2
  ;; [[1 2 3][4 5 6]] - #(10 20): column-major, v[i] applies to row i -> [[-9 -8 -7][-16 -15 -14]]
  (let ((m (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0))))
    (assert-true (array-is (- m (darr 10d0 20d0)) 'double-float '(2 3)
                           '(-9d0 -8d0 -7d0 -16d0 -15d0 -14d0)))
    (assert-true (array-is (- (darr 10d0 20d0) m) 'double-float '(2 3)
                           '(9d0 8d0 7d0 16d0 15d0 14d0)))
    (assert-true (array-is (/ m (darr 1d0 2d0)) 'double-float '(2 3)
                           '(1d0 2d0 3d0 2d0 2.5d0 3d0)))
    (assert-true (array-is (* m (darr 10d0 20d0)) 'double-float '(2 3)
                           '(10d0 20d0 30d0 80d0 100d0 120d0)))))

(deftest recycling-warnings-and-errors (ops-tests)
  (dolist (op *new-ops*)
    (with-warnings (r ws) (funcall (op-vm op) (darr 1d0 2d0 3d0) (darr 1d0 2d0))
      (assert-equal 1 (length (recycling-warnings ws))))
    (with-warnings (r ws) (funcall (op-vm op) (darr 1d0 2d0 3d0 4d0) (darr 1d0 2d0))
      (assert-equal 0 (length (recycling-warnings ws))))
    (assert-true (typep (condition-of (funcall (op-vm op) (make-arr 'double-float '(2 3) (fvals 6))
                                               (make-arr 'double-float '(3 2) (fvals 6))))
                        'vm:non-conformable-arrays))
    (assert-true (typep (condition-of (funcall (op-vm op) (make-arr 'double-float '(2 2) (fvals 4))
                                               (dvec (fvals 5))))
                        'vm:non-conformable-arrays))
    (assert-true (array-is (funcall (op-vm op) (darr) (darr 1d0 2d0)) 'double-float 0 '()))
    (assert-true (array-is (funcall (op-vm op) (darr) 1d0) 'double-float 0 '()))))

(deftest unary-and-fold-shapes (ops-tests)
  (let ((m (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0))))
    (assert-true (array-is (- m) 'double-float '(2 3) '(-1d0 -2d0 -3d0 -4d0 -5d0 -6d0)))
    (assert-true (array-is (/ m) 'double-float '(2 3) (mapcar (lambda (x) (cl:/ x)) (contents m))))
    (assert-true (array-is (* m) 'double-float '(2 3) (contents m)))
    (assert-false (eq (* m) m))
    (assert-true (array-is (- (darr)) 'double-float 0 '()))
    (assert-true (array-is (/ (darr)) 'double-float 0 '())))
  ;; fold with recycling at each step
  (assert-true (array-is (- (darr 10d0 20d0 30d0 40d0) (darr 1d0 2d0) 1d0) 'double-float 4
                         '(8d0 17d0 28d0 37d0)))
  (assert-true (array-is (* (darr 1d0 2d0) (darr 3d0 4d0 5d0 6d0) 2d0) 'double-float 4
                         '(6d0 16d0 10d0 24d0)))
  (assert-true (array-is (/ (darr 8d0 16d0) (darr 1d0 2d0 4d0 8d0) 2d0) 'double-float 4
                         '(4d0 4d0 1d0 1d0)))
  ;; dimensions come from the first array operand
  (let ((r (- 1d0 (make-arr 'double-float '(2 3) (fvals 6)))))
    (assert-equal '(2 3) (array-dimensions r))))

(deftest rank-0-and-1+-1- (ops-tests)
  (let ((r0 (make-array '() :element-type 'double-float :initial-element 3d0)))
    (assert-true (array-is (- r0 1) 'double-float '() '(2d0)))
    (assert-true (array-is (- r0) 'double-float '() '(-3d0)))
    (assert-true (array-is (/ r0) 'double-float '() (list (cl:/ 3d0))))
    (assert-true (array-is (* 2 r0) 'double-float '() '(6d0))))
  (assert-true (array-is (1+ (darr 1d0 2d0)) 'double-float 2 '(2d0 3d0)))
  (assert-true (array-is (1- (darr 1d0 2d0)) 'double-float 2 '(0d0 1d0)))
  (assert-true (array-is (1+ (iarr '(unsigned-byte 8) 255 0)) '(signed-byte 64) 2 '(256 1)))
  (assert-true (array-is (1- (iarr '(signed-byte 64) (- (expt 2 63)) 5)) t 2 (list (cl:- (- (expt 2 63)) 1) 4)))
  (assert-true (array-is (1+ (iarr '(signed-byte 64) (1- (expt 2 63)) 5)) t 2 (list (expt 2 63) 6)))
  (assert-true (array-is (1+ (varr 1 1/2 2.5d0)) t 3 '(2 3/2 3.5d0)))
  (assert-true (array-is (1- (csarr #c(1f0 1f0))) '(complex single-float) 1 '(#c(0f0 1f0))))
  (assert-true (array-is (1+ (make-array '(2 2) :element-type 'single-float :initial-element 1f0))
                         'single-float '(2 2) '(2f0 2f0 2f0 2f0)))
  (let ((bad (vector 1 'x)))
    (assert-number-type-error (condition-of (1+ bad)) bad)))

;;; ---------------------------------------------------------------------------
;;; contagion

(defun expected-et-op (opname fa fb values)
  (if (and (string= opname "/")
           (multiple-value-bind (pa ca) (family-float fa)
             (multiple-value-bind (pb cb) (family-float fb)
               (and (zerop pa) (zerop pb) (not ca) (not cb)))))
      t
      (expected-et fa fb values)))

(defun complex-float-et-p (et)
  (member et '((complex single-float) (complex double-float)) :test #'equal))

(defun non-finite-p (x)
  "True for a float or complex float with a NaN or infinite part."
  (typecase x
    (complex (or (non-finite-p (realpart x)) (non-finite-p (imagpart x))))
    (float (or (nanp x)
               (vm.impl::with-masked-float-traps
                 (> (abs x) (if (typep x 'single-float) most-positive-single-float most-positive-double-float)))))
    (t nil)))

(defun close-complex-p (got want scale)
  "GOT within 32 ulp (relative to SCALE) of WANT, componentwise as a complex difference.
When WANT is not finite (division by zero) any non-finite GOT is accepted: MKL and Lisp
differ in which of Inf and NaN they put in each component."
  (vm.impl::with-masked-float-traps
    (cond ((eql got want) t)
          ((non-finite-p want) (non-finite-p got))
          ((non-finite-p got) nil)
          (t (let* ((single (typep (realpart got) 'single-float))
                    (eps (if single single-float-epsilon double-float-epsilon))
                    (diff (abs (cl:- (coerce got '(complex double-float)) (coerce want '(complex double-float))))))
               (<= diff (cl:* 32 eps scale)))))))

(defun check-pair-op (op label fa fb a b)
  "NIL on success, else a description.  Expected values by the CL operator per element
(traps masked, as array operations run), complex * and / compared with a tolerance."
  (let* ((opname (op-name op))
         (ea (if (arrayp a) (contents a) (list a)))
         (eb (if (arrayp b) (contents b) (list b)))
         (n (cl:max (length ea) (length eb)))
         (want (handler-case
                   (loop for i below n
                         collect (masked-cl (op-cl op) (nth (mod i (length ea)) ea) (nth (mod i (length eb)) eb)))
                 (error (e) e)))
         (r (handler-case (funcall (op-vm op) a b) (error (e) e))))
    (cond
      ((typep want 'error)
       (if (and (typep r 'error) (eq (type-of r) (type-of want))) nil
           (format nil "~A: wanted ~S, got ~S" label (type-of want) (if (arrayp r) (contents r) r))))
      ((not (arrayp r)) (format nil "~A: ~S" label r))
      (t
       (let ((want-et (expected-et-op opname fa fb want)))
         (cond
           ((not (and (typep r 'simple-array) (equal (array-element-type r) (upgraded-array-element-type want-et))
                      (equal (array-dimensions r) (list n))))
            (format nil "~A: got ~S ~S, wanted ~S ~S" label (array-element-type r) (contents r) want-et want))
           ((and (complex-float-et-p want-et) (member opname '("*" "/") :test #'string=))
            (unless (every (lambda (g w x y)
                             (vm.impl::with-masked-float-traps
                               (close-complex-p g w (if (string= opname "*")
                                                        (cl:* (abs x) (abs y))
                                                        (abs w)))))
                           (contents r) want
                           (loop for i below n collect (nth (mod i (length ea)) ea))
                           (loop for i below n collect (nth (mod i (length eb)) eb)))
              (format nil "~A: complex values differ: ~S vs ~S" label (contents r) want)))
           ((not (every #'same-bits-p (contents r) want))
            (format nil "~A: got ~S, wanted ~S" label (contents r) want))))))))

(deftest contagion-array-array-all-ops (ops-tests)
  "Every pair of element types, every operator: element type and values; inputs not mutated."
  (dolist (op *new-ops*)
    (let ((bad '()))
      (dolist (x *array-types*)
        (dolist (y *array-types*)
          (let* ((a (make-sample x)) (b (make-sample y))
                 (ca (copy-arr a)) (cb (copy-arr b))
                 (msg (check-pair-op op (list (op-name op) (first x) (first y)) (second x) (second y) a b)))
            (when msg (push msg bad))
            (unless (and (same-contents-p a ca) (same-contents-p b cb))
              (push (list :mutated (op-name op) (first x) (first y)) bad)))))
      (when bad (format t "~&~{~S~%~}" bad))
      (assert-equal nil bad))))

(deftest contagion-array-scalar-all-ops (ops-tests)
  "Every array type with every scalar type, scalar on either side, every operator."
  (dolist (op *new-ops*)
    (let ((bad '()))
      (dolist (x *array-types*)
        (dolist (s *scalar-types*)
          (let* ((a (make-sample x)) (sv (third s)))
            (let ((msg (check-pair-op op (list (op-name op) (first x) (first s)) (second x) (second s) a sv)))
              (when msg (push msg bad)))
            (let ((msg (check-pair-op op (list (op-name op) (first s) (first x)) (second s) (second x) sv a)))
              (when msg (push msg bad))))))
      (when bad (format t "~&~{~S~%~}" bad))
      (assert-equal nil bad))))

(deftest rational-results-feed-float-contagion (ops-tests)
  "Inside one n-ary call an intermediate array of rationals (integer / ) is not a general T
array: combined with a float later in the fold it gives a float array, the same as
(op ints (op rational float)) would.  Across calls the T array is opaque (generic)."
  (assert-true (array-is (/ (iarr 'fixnum 1 2 3) 2 0.5d0) 'double-float 3 '(1d0 2d0 3d0)))
  (assert-true (array-is (+ (iarr 'fixnum 1 2 3) 1/2 0.5d0) 'double-float 3 '(2d0 3d0 4d0)))
  (assert-true (array-is (* (iarr 'fixnum 1 2 3) 1/2 2f0) 'single-float 3 '(1f0 2f0 3f0)))
  (assert-true (array-is (- (/ (iarr 'fixnum 1 2 3) 2) 1/2) t 3 '(0 1/2 1)))
  (assert-true (array-is (* (iarr 'fixnum 1 2) #c(1 1) #c(1d0 0d0)) '(complex double-float) 2
                         (list (cl:* #c(1 1) #c(1d0 0d0)) (cl:* #c(2 2) #c(1d0 0d0)))))
  (assert-true (array-is (* (/ (iarr 'fixnum 1 2) #c(1 1)) #c(1d0 0d0)) t 2
                         (list (cl:* #c(1/2 -1/2) #c(1d0 0d0)) (cl:* #c(1 -1) #c(1d0 0d0)))))
  ;; one n-ary call with a complex rational then a complex float
  (assert-true (array-is (/ (iarr 'fixnum 1 2) #c(1 1) #c(1d0 0d0)) '(complex double-float) 2
                         (list (cl:/ #c(1/2 -1/2) #c(1d0 0d0)) (cl:/ #c(1 -1) #c(1d0 0d0))))))

;;; ---------------------------------------------------------------------------
;;; integers

(defparameter +max64+ (1- (expt 2 63)))
(defparameter +min64+ (- (expt 2 63)))

(deftest integer-subtraction (ops-tests)
  (assert-true (array-is (- (iarr '(unsigned-byte 8) 1 2) (iarr '(unsigned-byte 8) 5 1))
                         '(signed-byte 64) 2 '(-4 1)))
  (assert-true (array-is (- (iarr '(signed-byte 64) +min64+ 1) 1) t 2 (list (cl:- +min64+ 1) 0)))
  (assert-true (array-is (- 0 (iarr '(signed-byte 64) +min64+ 1)) t 2 (list (expt 2 63) -1)))
  (assert-true (array-is (- (iarr '(signed-byte 64) +max64+ 1) (iarr '(signed-byte 64) -1 1))
                         t 2 (list (expt 2 63) 0)))
  ;; overflow possible by range, values fit: still (signed-byte 64)
  (assert-true (array-is (- (iarr '(signed-byte 64) 5 6) (iarr '(signed-byte 64) 1 2)) '(signed-byte 64) 2 '(4 4)))
  ;; operand range beyond (signed-byte 64): generic arithmetic, narrowed when the values fit
  (assert-true (array-is (- (iarr '(unsigned-byte 64) 5 6) 1) '(signed-byte 64) 2 '(4 5)))
  (assert-true (array-is (- (iarr '(unsigned-byte 64) (1- (expt 2 64)) 6) 1) t 2 (list (cl:- (expt 2 64) 2) 5)))
  (assert-true (array-is (- 1/2 (iarr '(unsigned-byte 8) 1 2)) t 2 '(-1/2 -3/2)))
  (assert-true (array-is (- (iarr 'fixnum 10 20 30) 1 2 3) '(signed-byte 64) 3 '(4 14 24))))

(deftest integer-negation (ops-tests)
  (assert-true (array-is (- (iarr '(unsigned-byte 8) 1 2 0)) '(signed-byte 64) 3 '(-1 -2 0)))
  (assert-true (array-is (- (iarr '(signed-byte 64) +min64+ 1)) t 2 (list (expt 2 63) -1)))
  (assert-true (array-is (- (iarr '(signed-byte 64) 5 -6)) '(signed-byte 64) 2 '(-5 6)))
  (assert-true (array-is (- (iarr '(unsigned-byte 64) 5 6)) '(signed-byte 64) 2 '(-5 -6)))
  (assert-true (array-is (- (iarr '(unsigned-byte 64) (1- (expt 2 64)) 6)) t 2 (list (cl:- 1 (expt 2 64)) -6)))
  (assert-true (array-is (- (varr 1 1/2 2.5d0)) t 3 '(-1 -1/2 -2.5d0)))
  (assert-true (array-is (- (make-array 3 :element-type 'bit :initial-contents '(1 0 1))) '(signed-byte 64) 3 '(-1 0 -1)))
  ;; negation of float zero flips the sign: (- x) is not (- 0 x)
  (assert-true (array-is (- (darr 0d0 -0d0 1d0)) 'double-float 3 '(-0d0 0d0 -1d0)))
  (assert-true (array-is (- (sarr 0f0 -0f0)) 'single-float 2 '(-0f0 0f0)))
  (assert-true (array-is (- (cdarr #c(0d0 -0d0) #c(1d0 2d0))) '(complex double-float) 2
                         (list (cl:- #c(0d0 -0d0)) #c(-1d0 -2d0)))))

(deftest integer-multiplication (ops-tests)
  ;; no overflow possible by ranges: typed (signed-byte 64)
  (assert-true (array-is (* (iarr '(unsigned-byte 8) 200 100) (iarr '(unsigned-byte 8) 200 100))
                         '(signed-byte 64) 2 '(40000 10000)))
  (assert-true (array-is (* (iarr '(signed-byte 32) (- (expt 2 31)) 5) (iarr '(signed-byte 32) (- (expt 2 31)) 3))
                         '(signed-byte 64) 2 (list (expt 2 62) 15)))
  (assert-true (array-is (* (iarr '(signed-byte 8) -128 3) -128) '(signed-byte 64) 2 '(16384 -384)))
  ;; overflow possible by range but values fit: narrowed to (signed-byte 64)
  (assert-true (array-is (* (iarr '(unsigned-byte 32) 1 2) (iarr '(unsigned-byte 32) 3 4)) '(signed-byte 64) 2 '(3 8)))
  (assert-true (array-is (* (iarr '(signed-byte 64) 5 6) (iarr '(signed-byte 64) 2 3)) '(signed-byte 64) 2 '(10 18)))
  ;; overflow really happens: T with bignums
  (assert-true (array-is (* (iarr '(signed-byte 64) (expt 2 62) 1) 4) t 2 (list (expt 2 64) 4)))
  (assert-true (array-is (* (iarr '(signed-byte 64) +max64+ 1) (iarr '(signed-byte 64) +max64+ 1))
                         t 2 (list (cl:* +max64+ +max64+) 1)))
  (assert-true (array-is (* (iarr '(signed-byte 64) +min64+ 1) -1) t 2 (list (expt 2 63) -1)))
  (assert-true (array-is (* (iarr '(unsigned-byte 8) 0 1) (expt 10 30)) t 2 (list 0 (expt 10 30))))
  (assert-true (array-is (* (iarr '(unsigned-byte 8) 2 3) 1/2) t 2 '(1 3/2)))
  (assert-true (array-is (* (iarr '(signed-byte 64) 3 4) 0) '(signed-byte 64) 2 '(0 0)))
  (assert-true (array-is (* (iarr 'fixnum 1 2 3) 2 3 4) '(signed-byte 64) 3 '(24 48 72)))
  ;; fold: overflow in the second step
  (assert-true (array-is (* (iarr '(signed-byte 64) 1 2) (iarr '(signed-byte 64) 1 1) +max64+)
                         t 2 (list +max64+ (cl:* 2 +max64+)))))

(deftest integer-division-and-rationals (ops-tests)
  ;; integer and rational operands give rationals in a T array, never narrowed
  (assert-true (array-is (/ (iarr 'fixnum 1 2 3) 2) t 3 '(1/2 1 3/2)))
  (assert-true (array-is (/ (iarr 'fixnum 4 6) (iarr 'fixnum 2 3)) t 2 '(2 2)))
  (assert-true (array-is (/ 12 (iarr 'fixnum 1 2 3 4)) t 4 '(12 6 4 3)))
  (assert-true (array-is (/ (iarr 'fixnum 1 2 4)) t 3 '(1 1/2 1/4)))
  (assert-true (array-is (/ (iarr '(unsigned-byte 8) 255 7) 5) t 2 '(51 7/5)))
  (assert-true (array-is (/ (varr 1/2 3 2.5d0) 2) t 3 '(1/4 3/2 1.25d0)))
  (assert-true (array-is (/ (iarr 'fixnum 1 2) 1/3) t 2 '(3 6)))
  (assert-true (array-is (/ (iarr 'fixnum 8 16) 2 2) t 2 '(2 4)))
  (assert-true (array-is (/ (iarr '(signed-byte 64) +min64+ 2) -1) t 2 (list (expt 2 63) -2)))
  (assert-true (array-is (/ (expt 10 30) (iarr 'fixnum 10 1000)) t 2 (list (expt 10 29) (expt 10 27))))
  (assert-true (array-is (/ (iarr 'fixnum 1 2) #c(1 1)) t 2 (list (cl:/ 1 #c(1 1)) (cl:/ 2 #c(1 1)))))
  ;; with floats: float arrays
  (assert-true (array-is (/ (iarr 'fixnum 1 2) 2d0) 'double-float 2 '(0.5d0 1d0)))
  (assert-true (array-is (/ 1 (darr 2d0 4d0)) 'double-float 2 '(0.5d0 0.25d0)))
  (assert-true (array-is (/ (darr 3d0 6d0) 3) 'double-float 2 '(1d0 2d0)))
  (assert-true (array-is (/ (sarr 3f0 6f0) 3 2) 'single-float 2 '(0.5f0 1f0))))

(deftest division-by-zero (ops-tests)
  "Rationals: DIVISION-BY-ZERO as in CL.  Float arrays: IEEE Inf and NaN, no signal."
  (let ((zero (opaque 0)))
    (assert-true (typep (condition-of (/ (iarr 'fixnum 1 2) zero)) 'division-by-zero))
    (assert-true (typep (condition-of (/ (iarr 'fixnum 1 2) (iarr 'fixnum 1 0))) 'division-by-zero))
    (assert-true (typep (condition-of (/ (iarr 'fixnum 1 0))) 'division-by-zero))
    (assert-true (typep (condition-of (/ 1 (iarr 'fixnum 1 0))) 'division-by-zero))
    (assert-true (typep (condition-of (/ (varr 1 2/3) zero)) 'division-by-zero))
    (assert-true (typep (condition-of (/ (iarr 'fixnum 0 0) 0)) 'division-by-zero))
    (assert-true (typep (condition-of (/ (iarr 'fixnum 1 2) 1/2 0)) 'division-by-zero))
    ;; the message is CL's: same condition type as scalars
    (assert-equal 'division-by-zero (type-of (condition-of (opaque (cl:/ 1 zero)))))
    (assert-equal 'division-by-zero (type-of (condition-of (opaque (/ (iarr 'fixnum 1) zero))))))
  (dolist (cut (list 0 most-positive-fixnum))
    (with-cutoff (cut '/)
      (let ((r (/ (darr 1d0 -1d0 0d0) 0d0)))
        (assert-true (eql (aref r 0) (inf+)))
        (assert-true (eql (aref r 1) (inf-)))
        (assert-true (nanp (aref r 2))))
      ;; an integer zero scalar with a float array: IEEE, not an error
      (let ((r (/ (darr 1d0 0d0) 0)))
        (assert-true (eql (aref r 0) (inf+)))
        (assert-true (nanp (aref r 1))))
      (let ((r (/ (sarr 1f0) (sarr 0f0))))
        (assert-true (eql (aref r 0) (single-inf+))))
      (let ((r (/ (darr 0d0 -0d0 2d0))))                 ; reciprocal
        (assert-true (eql (aref r 0) (inf+)))
        (assert-true (eql (aref r 1) (inf-)))
        (assert-true (eql (aref r 2) 0.5d0)))
      (assert-true (every #'nanp (contents (/ (make-array 600 :element-type 'double-float :initial-element 0d0)
                                              (make-array 600 :element-type 'double-float :initial-element 0d0)))))
      ;; no residual FP state: scalars signal as usual afterwards
      (assert-true (typep (condition-of (opaque (/ 1d0 (opaque 0d0)))) 'division-by-zero)))))

;;; ---------------------------------------------------------------------------
;;; complex arrays

(defun random-complex-array (et n seed)
  (let* ((next (lcg-stream seed)) (proto (if (equal et '(complex single-float)) 1f0 1d0))
         (a (make-array n :element-type et)))
    (dotimes (i n a)
      (flet ((x () (float (cl:- (cl:/ (mod (funcall next) 20001) 1000) 10) proto)))
        (setf (aref a i) (complex (x) (x)))))))

(deftest complex-arrays-all-ops (ops-tests)
  "Complex arrays: + - bit-identical to CL; * and / within 32 ulp (of |x||y|, of |x/y|)."
  (let ((bad '()))
    (dolist (et '((complex single-float) (complex double-float)))
      (dolist (n '(5 300 5000))
        (let ((a (random-complex-array et n 11)) (b (random-complex-array et n 12))
              (s (if (equal et '(complex single-float)) #c(0.5f0 -2f0) #c(0.5d0 -2d0))))
          (dolist (op *binary-ops*)
            (let* ((x (funcall (op-vm op) a b))
                   (xs (funcall (op-vm op) a s)) (sx (funcall (op-vm op) s b))
                   (exact (member (op-name op) '("+" "-") :test #'string=)))
              (dotimes (i n)
                (dolist (case (list (list x (aref a i) (aref b i)) (list xs (aref a i) s) (list sx s (aref b i))))
                  (destructuring-bind (r p q) case
                    (let ((want (masked-cl (op-cl op) p q)))
                      (unless (if exact
                                  (same-bits-p (aref r i) want)
                                  (vm.impl::with-masked-float-traps
                                    (close-complex-p (aref r i) want
                                                     (if (string= (op-name op) "*")
                                                         (cl:* (abs p) (abs q))
                                                         (abs want)))))
                        (push (list et n (op-name op) i p q (aref r i) want) bad)))))))))))
    (when bad (format t "~&~{~S~%~}" (subseq bad 0 (cl:min 5 (length bad)))))
    (assert-equal nil bad)))

(deftest complex-reciprocal-and-negation (ops-tests)
  (let* ((a (random-complex-array '(complex double-float) 700 5))
         (r (/ a)) (n (- a)) (bad '()))
    (dotimes (i 700)
      (unless (vm.impl::with-masked-float-traps
                (close-complex-p (aref r i) (cl:/ (aref a i)) (abs (cl:/ (aref a i)))))
        (push (list :recip i) bad))
      (unless (eql (aref n i) (cl:- (aref a i))) (push (list :neg i) bad)))
    (assert-equal nil bad)))

;;; ---------------------------------------------------------------------------
;;; :into

(defun fzeros (n) (make-array n :element-type 'single-float :initial-element 0f0))

(deftest into-all-ops (ops-tests)
  (let ((out (dzeros 3)))
    (with-warnings (r ws) (- (darr 5d0 6d0 7d0) (darr 1d0 2d0 3d0) :into out)
      (assert-true (eq r out))
      (assert-true (array-is out 'double-float 3 '(4d0 4d0 4d0)))
      (assert-equal nil ws)))
  (let ((out (dzeros 3)))
    (* (darr 1d0 2d0 3d0) 2d0 3d0 :into out)
    (assert-true (array-is out 'double-float 3 '(6d0 12d0 18d0))))
  (let ((out (dzeros 3)))
    (/ 6d0 (darr 1d0 2d0 3d0) :into out)
    (assert-true (array-is out 'double-float 3 '(6d0 3d0 2d0))))
  ;; rationals into a T array and into a float array
  (let ((out (make-array 3)))
    (/ (iarr 'fixnum 1 2 3) 2 :into out)
    (assert-equal '(1/2 1 3/2) (contents out)))
  (let ((out (dzeros 3)))
    (/ (iarr 'fixnum 1 2 3) 2 :into out)
    (assert-true (array-is out 'double-float 3 '(0.5d0 1d0 1.5d0))))
  ;; unary forms
  (let ((out (dzeros 2)))
    (- (darr 1d0 -2d0) :into out)
    (assert-true (array-is out 'double-float 2 '(-1d0 2d0))))
  (let ((out (dzeros 2)))
    (/ (darr 2d0 4d0) :into out)
    (assert-true (array-is out 'double-float 2 '(0.5d0 0.25d0))))
  (let ((out (make-array 2 :element-type '(signed-byte 64) :initial-element 0)))
    (- (iarr '(unsigned-byte 8) 1 2) :into out)
    (assert-true (array-is out '(signed-byte 64) 2 '(-1 -2))))
  ;; scalars only: the value fills OUT
  (let ((out (dzeros 3)))
    (- 5d0 3d0 :into out)
    (assert-true (array-is out 'double-float 3 '(2d0 2d0 2d0))))
  ;; recycling and truncation warn
  (let ((out (dzeros 4)))
    (with-warnings (r ws) (- (darr 1d0 2d0 3d0) 0d0 :into out)
      (assert-true (array-is out 'double-float 4 '(1d0 2d0 3d0 1d0)))
      (assert-equal 1 (length (recycling-warnings ws)))))
  ;; multidimensional destination: column-major placement of a vector value
  (let ((out (make-array '(2 3) :element-type 'double-float :initial-element 0d0)))
    (- (darr 1d0 2d0) 0d0 :into out)
    (assert-true (array-is out 'double-float '(2 3) '(1d0 1d0 1d0 2d0 2d0 2d0)))))

(deftest into-aliasing-all-ops (ops-tests)
  (let ((a (darr 1d0 2d0 3d0)))
    (* a 2d0 :into a)
    (assert-true (array-is a 'double-float 3 '(2d0 4d0 6d0))))
  (let ((a (darr 1d0 2d0 3d0)))
    (- 10d0 a :into a)
    (assert-true (array-is a 'double-float 3 '(9d0 8d0 7d0))))
  (let ((a (darr 1d0 2d0 4d0)))
    (/ 1d0 a :into a)
    (assert-true (array-is a 'double-float 3 '(1d0 0.5d0 0.25d0))))
  (let ((a (darr 1d0 2d0 4d0)) (b (darr 1d0 1d0 1d0)))
    (- a b :into b)
    (assert-true (array-is b 'double-float 3 '(0d0 1d0 3d0)))
    (assert-true (array-is a 'double-float 3 '(1d0 2d0 4d0)))))

(deftest into-errors-all-ops (ops-tests)
  (flet ((check (thunk out)
           (let* ((before (copy-arr out)) (c (condition-of (funcall thunk out))))
             (list (typep c 'vm:output-type-error) (same-contents-p out before)))))
    (assert-equal '(t t) (check (lambda (o) (- (darr 1d0 2d0) 0d0 :into o)) (fzeros 2)))
    (assert-equal '(t t) (check (lambda (o) (* (darr 1d0 2d0) 2 :into o)) (make-array 2 :element-type '(signed-byte 64))))
    (assert-equal '(t t) (check (lambda (o) (/ (iarr 'fixnum 1 2) 2 :into o)) (make-array 2 :element-type '(signed-byte 64))))
    (assert-equal '(t t) (check (lambda (o) (/ (cdarr #c(1d0 1d0)) 2 :into o)) (dzeros 1)))
    (assert-true (typep (condition-of (- (darr 1d0) 1 :into 5)) 'type-error))))

;;; ---------------------------------------------------------------------------
;;; no mutation, other array kinds

(deftest no-mutation-all-ops (ops-tests)
  (dolist (op *new-ops*)
    (dolist (x *array-types*)
      (let* ((a (make-sample x)) (ca (copy-arr a)))
        ;; (some of these signal division-by-zero for arrays containing 0: ignored)
        (condition-of (funcall (op-vm op) a 2))
        (condition-of (funcall (op-vm op) 2 a))
        (condition-of (funcall (op-vm op) a))
        (condition-of (funcall (op-vm op) a a))
        (assert-true (same-contents-p a ca))))))

(deftest non-simple-and-t-arrays (ops-tests)
  (let ((adj (make-array 3 :element-type 'double-float :adjustable t :initial-contents '(1d0 2d0 3d0)))
        (fp (make-array 5 :element-type 'double-float :fill-pointer 3 :initial-contents '(1d0 2d0 3d0 0d0 0d0))))
    (assert-true (array-is (- adj 1d0) 'double-float 3 '(0d0 1d0 2d0)))
    (assert-true (array-is (* fp 2d0) 'double-float 3 '(2d0 4d0 6d0)))
    (assert-true (array-is (/ 6d0 adj) 'double-float 3 '(6d0 3d0 2d0))))
  (assert-true (array-is (- (varr 1 2.5d0 1/2) (varr 1 1 1)) t 3 '(0 1.5d0 -1/2)))
  (let ((bad (vector 1 'x)))
    (assert-number-type-error (condition-of (- bad)) bad)
    (assert-number-type-error (condition-of (* bad 2)) bad)
    (assert-number-type-error (condition-of (/ 2 bad)) bad)))

(deftest large-arrays-all-ops (ops-tests)
  "Above every cutoff: MKL results equal the elementwise CL operator."
  (let* ((n 100000)
         (a (make-array n :element-type 'double-float))
         (b (make-array n :element-type 'double-float)))
    (dotimes (i n) (setf (aref a i) (float (cl:+ 1 i) 1d0) (aref b i) (cl:/ 3d0 (float (cl:+ 1 i) 1d0))))
    (dolist (op *new-ops*)
      (let ((r (funcall (op-vm op) a b)) (bad 0))
        (dotimes (i n) (unless (eql (aref r i) (funcall (op-cl op) (aref a i) (aref b i))) (incf bad)))
        (assert-equal 0 bad)))))

;;; ---------------------------------------------------------------------------
;;; incf / decf

(defmacro tracing ((trace) &body body)
  `(let ((,trace '()))
     (flet ((note (tag value) (push tag ,trace) value))
       (declare (ignorable #'note))
       ,@body)))

(deftest incf-decf-scalars (ops-tests)
  (let ((x 1)) (assert-equal 2 (incf x)) (assert-equal 2 x))
  (let ((x 1)) (assert-equal 11 (incf x 10)) (assert-equal 11 x))
  (let ((x 1)) (assert-equal 0 (decf x)) (assert-equal 0 x))
  (let ((x 1)) (assert-equal -9 (decf x 10)) (assert-equal -9 x))
  (let ((x 1.5d0)) (assert-equal 2.0d0 (incf x 0.5d0)))
  ;; the delta may itself change the variable: same result as CL:INCF in this implementation
  (let ((x 5) (y 5)) (assert-true (eql (cl:incf y (cl:incf y)) (incf x (incf x))))))

(deftest incf-decf-arrays (ops-tests)
  ;; the variable is rebound to a new array; the old one is not mutated
  (let* ((x (darr 1d0 2d0)) (old x))
    (incf x 1d0)
    (assert-true (array-is x 'double-float 2 '(2d0 3d0)))
    (assert-true (array-is old 'double-float 2 '(1d0 2d0)))
    (decf x (darr 1d0 1d0))
    (assert-true (array-is x 'double-float 2 '(1d0 2d0)))
    (incf x)
    (assert-true (array-is x 'double-float 2 '(2d0 3d0))))
  ;; array place holding a number
  (let ((m (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0))))
    (incf (aref m 0) 10d0) (decf (aref m 2) 1d0)
    (assert-true (array-is m 'double-float 3 '(11d0 2d0 2d0))))
  ;; array place holding an array, incremented by an array
  (let ((m (vector (darr 1d0 2d0) (darr 10d0 20d0))))
    (incf (aref m 0) (darr 1d0 1d0))
    (decf (aref m 1) 5d0)
    (assert-true (array-is (aref m 0) 'double-float 2 '(2d0 3d0)))
    (assert-true (array-is (aref m 1) 'double-float 2 '(5d0 15d0))))
  ;; multi-dimensional place
  (let ((m (make-array '(2 2) :initial-element 1)))
    (incf (aref m 1 1) 4)
    (assert-equal 5 (aref m 1 1)))
  ;; a bad operand signals the CL error
  (let ((x 'a)) (assert-true (typep (condition-of (incf x)) 'type-error))))

(deftest incf-decf-generalized-places (ops-tests)
  (let ((cell (list 1 2))) (incf (car cell) 5) (decf (cadr cell) 1) (assert-equal '(6 1) cell))
  (let ((h (make-hash-table))) (incf (gethash :k h 0) 3) (incf (gethash :k h 0)) (assert-equal 4 (values (gethash :k h))))
  (let ((pl (list :a 1))) (incf (getf pl :a) 2) (incf (getf pl :b 10)) (assert-equal 3 (getf pl :a)) (assert-equal 11 (getf pl :b)))
  (let ((x #b1010)) (incf (ldb (byte 4 0) x) 1) (assert-equal #b1011 x))
  (let ((s (list 1 2 3))) (incf (nth 2 s) 10) (assert-equal '(1 2 13) s))
  (let ((v (vector 1 2 3))) (incf (svref v 1) 5) (assert-true (equalp #(1 7 3) v)))
  (let ((m (vector 1 2 3)) (i 0))
    ;; place subforms evaluated once
    (incf (aref m (incf i)) 100)
    (assert-equal 1 i)
    (assert-true (equalp #(1 102 3) m)))
  (let ((m (vector 1 2 3)) (i 0))
    (decf (aref m (incf i)) 100)
    (assert-true (equalp #(1 -98 3) m))))

(defstruct incf-box (n 0) (arr nil))

(deftest incf-decf-evaluation-order (ops-tests)
  "Place subforms are evaluated once, left to right, then the delta: same trace and result as CL:INCF."
  (flet ((run (use-vm)
           (tracing (tr)
             (let* ((v (vector 1 2 3)) (box (make-incf-box :n 5))
                    (r1 (if use-vm
                            (incf (aref (note :v v) (note :i 1)) (note :d 10))
                            (cl:incf (aref (note :v v) (note :i 1)) (note :d 10))))
                    (r2 (if use-vm
                            (decf (incf-box-n (note :box box)) (note :e 2))
                            (cl:decf (incf-box-n (note :box box)) (note :e 2))))
                    (r3 (if use-vm
                            (incf (aref (note :v2 v) (note :j (note :k 0))))
                            (cl:incf (aref (note :v2 v) (note :j (note :k 0)))))))
               (list r1 r2 r3 v (incf-box-n box) (reverse tr))))))
    (assert-true (equalp (run nil) (run t)))
    (assert-true (equalp '(12 3 2 #(2 12 3) 3 (:v :i :d :box :e :v2 :k :j)) (run t))))
  ;; each subform exactly once, even with side effects in the array position
  (let ((count 0) (v (vector 0 0)))
    (incf (aref v (progn (cl:incf count) 1)) 5)
    (assert-equal 1 count)
    (assert-true (equalp #(0 5) v))))

(deftest incf-decf-compiled-with-types (ops-tests)
  "Compiled with declarations the expansion is the plain CL operation."
  (let ((f (compile nil '(lambda (x) (declare (fixnum x)) (incf x 2) (decf x) x))))
    (assert-equal 6 (funcall f 5)))
  (let ((g (compile nil '(lambda (v) (incf (aref v 0) 1d0) v))))
    (assert-true (array-is (funcall g (darr 1d0 2d0)) 'double-float 2 '(2d0 2d0)))
    (assert-equal 2d0 (aref (funcall g (vector 1 2)) 0))))

;;; ---------------------------------------------------------------------------
;;; (signed-byte 64) arrays: the range of the data decides between the typed kernel and the generic path

(deftest ops-sb64-arrays-data-ranges (ops-tests)
  "+ - * of two (signed-byte 64) arrays equal CL's per element, whichever way they are computed: the
typed kernel when the data cannot overflow (its range is looked up in the data), the generic path with
narrowing otherwise.  Magnitudes from 2^8 to the extremes of the type, and the result's element type is
(signed-byte 64) exactly when every result fits."
  (flet ((arr (bits n seed)
           (let ((a (make-array n :element-type '(signed-byte 64))) (s seed))
             (dotimes (i n a)
               (setf s (logand (cl:+ (cl:* s 6364136223846793005) 1442695040888963407) #xFFFFFFFFFFFFFFFF))
               (let ((v (ldb (byte bits 0) (ash s -4))))
                 (setf (aref a i) (if (logbitp (cl:- bits 1) v) (cl:- v (ash 1 bits)) v)))))))
    (dolist (bits '(8 31 32 40 62 63 64))
      (dolist (op '("+" "-" "*"))
        (let* ((a (arr bits 200 (cl:+ bits 1))) (b (arr bits 200 (cl:+ bits 77)))
               (r (funcall (symbol-function (find-symbol op :vm)) a b))
               (e (map 'list (lambda (x y) (funcall (symbol-function (find-symbol op :cl)) x y)) a b))
               (fits (every #'sb64-p e)))
          (assert-equal e (contents r) op bits)
          (assert-equal (if fits '(signed-byte 64) t) (array-element-type r) op bits))))
    ;; a single huge element among small ones: the generic path
    (let* ((a (iarr '(signed-byte 64) 1 2 (cl:- (expt 2 63) 1))) (b (iarr '(signed-byte 64) 1 1 1)))
      (assert-true (array-is (+ a b) t 3 (list 2 3 (expt 2 63))))
      (assert-true (array-is (- a b) '(signed-byte 64) 3 (list 0 1 (cl:- (expt 2 63) 2)))))
    ;; the inputs are not changed
    (let* ((a (iarr '(signed-byte 64) 1 2 3)) (b (iarr '(signed-byte 64) 4 5 6)))
      (+ a b)
      (assert-equal '(1 2 3) (contents a))
      (assert-equal '(4 5 6) (contents b)))))
