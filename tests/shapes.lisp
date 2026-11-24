;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vm-tests)

(defsuite shape-tests (vm-tests))

(defun mat23 ()
  (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0)))

(deftest scalars-broadcast (shape-tests)
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0) 10d0)
    (assert-true (array-is r 'double-float 3 '(11d0 12d0 13d0)))
    (assert-equal nil ws))
  (assert-true (array-is (+ 10d0 (darr 1d0 2d0 3d0)) 'double-float 3 '(11d0 12d0 13d0)))
  (assert-true (array-is (+ 1 (mat23)) 'double-float '(2 3) '(2d0 3d0 4d0 5d0 6d0 7d0)))
  (assert-true (array-is (+ 1 2 (darr 1d0)) 'double-float 1 '(4d0)))
  (assert-true (array-is (+ (darr 1d0) 1 2) 'double-float 1 '(4d0))))

(deftest equal-length-vectors (shape-tests)
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0) (darr 10d0 20d0 30d0))
    (assert-true (array-is r 'double-float 3 '(11d0 22d0 33d0)))
    (assert-equal nil ws)))

(deftest vector-recycling-exact (shape-tests)
  "Longer length a multiple of the shorter: no warning, either argument order."
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0 4d0) (darr 10d0 20d0))
    (assert-true (array-is r 'double-float 4 '(11d0 22d0 13d0 24d0)))
    (assert-equal nil ws))
  (with-warnings (r ws) (+ (darr 10d0 20d0) (darr 1d0 2d0 3d0 4d0))
    (assert-true (array-is r 'double-float 4 '(11d0 22d0 13d0 24d0)))
    (assert-equal nil ws)))

(deftest vector-recycling-fractional-warns (shape-tests)
  "Fractional recycling warns with VM:RECYCLING-WARNING (a WARNING subclass) and still computes."
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0) (darr 10d0 20d0))
    (assert-true (array-is r 'double-float 3 '(11d0 22d0 13d0)))
    (assert-equal 1 (length (recycling-warnings ws))))
  (with-warnings (r ws) (+ (darr 10d0 20d0) (darr 1d0 2d0 3d0 4d0 5d0))
    (assert-true (array-is r 'double-float 5 '(11d0 22d0 13d0 24d0 15d0)))
    (assert-equal 1 (length (recycling-warnings ws))))
  (assert-true (subtypep 'vm:recycling-warning 'warning))
  ;; muffling works with the standard restart
  (assert-true (array-is (handler-bind ((vm:recycling-warning #'muffle-warning))
                           (+ (darr 1d0 2d0 3d0) (darr 10d0 20d0)))
                         'double-float 3 '(11d0 22d0 13d0))))

(deftest vector-vector-recycling-paths (shape-tests)
  "Longer/shorter vectors, covering the expand-to-temporary (short) and block (>=16) paths."
  (dolist (case '((40 10) (100 20) (50 20) (37 5) (64 16) (33 17)))
    (destructuring-bind (n m) case
      (let* ((a (make-arr 'double-float n (loop for i below n collect (float i 1d0))))
             (b (make-arr 'double-float m (loop for i below m collect (float (* 1000 (1+ i)) 1d0)))))
        (dolist (order '(:long-first :short-first))
          (with-warnings (r ws) (if (eq order :long-first) (+ a b) (+ b a))
            (assert-true (array-is r 'double-float n
                                   (loop for i below n
                                         collect (cl:+ (float i 1d0) (float (* 1000 (1+ (mod i m))) 1d0)))))
            (assert-equal (if (zerop (mod n m)) 0 1) (length (recycling-warnings ws)))))))))

(deftest one-element-vector-is-scalar-like (shape-tests)
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0) (darr 10d0))
    (assert-true (array-is r 'double-float 3 '(11d0 12d0 13d0)))
    (assert-equal nil ws))
  (with-warnings (r ws) (+ (darr 10d0) (mat23))
    (assert-true (array-is r 'double-float '(2 3) '(11d0 12d0 13d0 14d0 15d0 16d0)))
    (assert-equal nil ws)))

(deftest matrix-matrix (shape-tests)
  (assert-true (array-is (+ (mat23) (mat23)) 'double-float '(2 3) '(2d0 4d0 6d0 8d0 10d0 12d0)))
  (let ((c (condition-of (+ (mat23) (make-arr 'double-float '(3 2) '(1d0 2d0 3d0 4d0 5d0 6d0))))))
    (assert-true (typep c 'vm:non-conformable-arrays))
    (assert-true (typep c 'error))
    (assert-true (search "Non-conformable" (princ-to-string c)))))

(deftest matrix-plus-vector-column-major-by-hand (shape-tests)
  "2x3 matrix [[1 2 3][4 5 6]] plus the length-2 vector #(10 20).  Column-major recycling:
element (i,j) has column-major index i + 2j and takes v[(i+2j) mod 2] = v[i]; so row 0 gets +10 and
row 1 gets +20: result [[11 12 13][24 25 26]].  Computed by hand."
  (with-warnings (r ws) (+ (mat23) (darr 10d0 20d0))
    (assert-true (array-is r 'double-float '(2 3) '(11d0 12d0 13d0 24d0 25d0 26d0)))
    (assert-equal nil ws))
  ;; vector first: dims still come from the matrix
  (with-warnings (r ws) (+ (darr 10d0 20d0) (mat23))
    (assert-true (array-is r 'double-float '(2 3) '(11d0 12d0 13d0 24d0 25d0 26d0)))
    (assert-equal nil ws)))

(deftest matrix-plus-vector-length-3-by-hand (shape-tests)
  "Same matrix plus #(10 20 30): column-major indices (i,j) -> i+2j are
(0,0)=0 (0,1)=2 (0,2)=4 (1,0)=1 (1,1)=3 (1,2)=5, taking v[c mod 3] = v0 v2 v1 / v1 v0 v2,
so the result is [[1+10 2+30 3+20][4+20 5+10 6+30]] = [[11 32 23][24 15 36]]."
  (with-warnings (r ws) (+ (mat23) (darr 10d0 20d0 30d0))
    (assert-true (array-is r 'double-float '(2 3) '(11d0 32d0 23d0 24d0 15d0 36d0)))
    (assert-equal 0 (length (recycling-warnings ws)))))

(deftest matrix-plus-vector-same-size (shape-tests)
  "Vector as long as the matrix (6): v[c] with c = i + 2j.  Result [[1+v0 2+v2 3+v4][4+v1 5+v3 6+v5]]."
  (with-warnings (r ws) (+ (mat23) (darr 100d0 200d0 300d0 400d0 500d0 600d0))
    (assert-true (array-is r 'double-float '(2 3) '(101d0 302d0 503d0 204d0 405d0 606d0)))
    (assert-equal nil ws)))

(deftest matrix-plus-vector-fractional-warns (shape-tests)
  "6 elements, vector of 4: warning; c mod 4 for c = 0 2 4 / 1 3 5 gives v0 v2 v0 / v1 v3 v1."
  (with-warnings (r ws) (+ (mat23) (darr 10d0 20d0 30d0 40d0))
    (assert-true (array-is r 'double-float '(2 3) '(11d0 32d0 13d0 24d0 45d0 26d0)))
    (assert-equal 1 (length (recycling-warnings ws)))))

(deftest matrix-vector-longer-errors (shape-tests)
  (let ((c (condition-of (+ (mat23) (make-arr 'double-float 7 (loop repeat 7 collect 1d0))))))
    (assert-true (typep c 'vm:non-conformable-arrays)))
  (let ((c (condition-of (+ (make-arr 'double-float 7 (loop repeat 7 collect 1d0)) (mat23)))))
    (assert-true (typep c 'vm:non-conformable-arrays))))

(deftest matrix-rows-fast-path (shape-tests)
  "3x20 matrix plus a length-3 vector (per-row scalar broadcast path), both argument orders."
  (let* ((m (make-arr 'double-float '(3 20) (loop for i below 60 collect (float i 1d0))))
         (v (darr 100d0 200d0 300d0))
         (expected (loop for i below 60 collect (cl:+ (float i 1d0) (* 100d0 (1+ (floor i 20)))))))
    (with-warnings (r ws) (+ m v)
      (assert-true (array-is r 'double-float '(3 20) expected))
      (assert-equal nil ws))
    (assert-true (array-is (+ v m) 'double-float '(3 20) expected))
    ;; with a cutoff of 0 the MKL scalar kernels run per row
    (let ((old (vm:cutoff '+)))
      (setf (vm:cutoff '+) 0)
      (unwind-protect (assert-true (array-is (+ m v) 'double-float '(3 20) expected))
        (setf (vm:cutoff '+) old)))))

(deftest three-d-by-hand (shape-tests)
  "2x2x2 zeros plus #(100 200 300).  Column-major index of (i,j,k) is i+2j+4k:
(0,0,0)=0 (1,0,0)=1 (0,1,0)=2 (1,1,0)=3 (0,0,1)=4 (1,0,1)=5 (0,1,1)=6 (1,1,1)=7; c mod 3 selects
v0 v1 v2 v0 v1 v2 v0 v1 for the column-major sequence.  Mapping back to row-major order
(i,j,k) = 000 001 010 011 100 101 110 111 gives 000:v0 001:v1(c=4) 010:v2(c=2) 011:v0(c=6)
100:v1(c=1) 101:v2(c=5) 110:v0(c=3) 111:v1(c=7), i.e. 100 200 300 100 200 300 100 200."
  (with-warnings (r ws) (+ (make-arr 'double-float '(2 2 2) (loop repeat 8 collect 0d0))
                           (darr 100d0 200d0 300d0))
    (assert-true (array-is r 'double-float '(2 2 2)
                           '(100d0 200d0 300d0 100d0 200d0 300d0 100d0 200d0)))
    (assert-equal 1 (length (recycling-warnings ws)))))

(deftest three-d-rows-and-general (shape-tests)
  "2x8x8 array plus a length-2 vector (v[i]); and plus a length-3 vector (v[(i+2j+16k...) mod 3])."
  (let* ((n 128)
         (a (make-arr 'double-float '(2 8 8) (loop for r below n collect (float r 1d0))))
         (v2 (darr 1000d0 2000d0)))
    (assert-true (array-is (+ a v2) 'double-float '(2 8 8)
                           (loop for r below n collect (cl:+ (float r 1d0) (if (< r 64) 1000d0 2000d0)))))
    (let ((v3 (darr 1d0 2d0 3d0)))
      (assert-true
       (array-is (handler-bind ((vm:recycling-warning #'muffle-warning)) (+ a v3))
                 'double-float '(2 8 8)
                 (loop for r below n
                       collect (let* ((k (mod r 8)) (j (mod (floor r 8) 8)) (i (floor r 64))
                                      (c (cl:+ i (* 2 (cl:+ j (* 8 k))))))
                                 (cl:+ (float r 1d0) (float (cl:+ 1 (mod c 3)) 1d0)))))))))

(deftest zero-length (shape-tests)
  (with-warnings (r ws) (+ (darr) (darr 1d0 2d0))
    (assert-true (array-is r 'double-float 0 '()))
    (assert-equal nil ws))
  (assert-true (array-is (+ (darr 1d0 2d0) (darr)) 'double-float 0 '()))
  (assert-true (array-is (+ (darr) 1d0) 'double-float 0 '()))
  (assert-true (array-is (+ 1d0 (darr)) 'double-float 0 '()))
  (assert-true (array-is (+ (darr) (darr)) 'double-float 0 '()))
  (assert-true (array-is (+ (iarr '(signed-byte 32)) 1) '(signed-byte 64) 0 '()))
  (assert-true (array-is (+ (mat23) (darr)) 'double-float 0 '()))
  (assert-true (array-is (+ (make-array '(0 3) :element-type 'double-float) 1d0) 'double-float '(0 3) '())))

(deftest dims-from-first-array (shape-tests)
  "Result dimensions come from the array of rank >= 2; vectors give 1-D results."
  (assert-equal '(2 3) (array-dimensions (+ (mat23) 1d0)))
  (assert-equal '(2 3) (array-dimensions (+ 1d0 (mat23))))
  (assert-equal '(2 3) (array-dimensions (+ (mat23) (mat23))))
  (assert-equal '(2 3) (array-dimensions (+ (darr 1d0 2d0) (mat23))))
  (assert-equal '(5) (array-dimensions (+ (darr 1d0 2d0 3d0 4d0 5d0) 1d0))))

(deftest n-ary-fold (shape-tests)
  "Left fold with R rules at each step."
  (with-warnings (r ws) (+ (darr 1d0 2d0 3d0 4d0) (darr 10d0 20d0) (darr 100d0) 1000d0)
    (assert-true (array-is r 'double-float 4 '(1111d0 1122d0 1113d0 1124d0)))
    (assert-equal nil ws))
  (assert-true (array-is (+ (mat23) (darr 10d0 20d0) 1) 'double-float '(2 3)
                         '(12d0 13d0 14d0 25d0 26d0 27d0)))
  (assert-true (array-is (+ 1 2 3 (darr 1d0)) 'double-float 1 '(7d0))))

(deftest single-array-operand-copies (shape-tests)
  (let* ((a (darr 1d0 2d0 3d0)) (copy (copy-arr a)) (r (+ a)))
    (assert-true (array-is r 'double-float 3 '(1d0 2d0 3d0)))
    (assert-false (eq r a))
    (setf (aref r 0) 99d0)
    (assert-true (same-contents-p a copy)))
  (assert-true (array-is (+ (iarr '(unsigned-byte 8) 1 2 3)) '(signed-byte 64) 3 '(1 2 3)))
  (assert-true (array-is (+ (mat23)) 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0))))

(deftest non-simple-arrays (shape-tests)
  "Fill-pointer, adjustable and displaced arrays behave like their active contents; results are simple."
  (let ((fp (make-array 5 :element-type 'double-float :fill-pointer 3 :initial-contents '(1d0 2d0 3d0 4d0 5d0))))
    (assert-true (array-is (+ fp 1d0) 'double-float 3 '(2d0 3d0 4d0)))
    (assert-true (array-is (+ fp fp) 'double-float 3 '(2d0 4d0 6d0))))
  (let ((adj (make-array '(2 2) :element-type 'double-float :adjustable t
                                :initial-contents '((1d0 2d0) (3d0 4d0)))))
    (assert-true (array-is (+ adj 1d0) 'double-float '(2 2) '(2d0 3d0 4d0 5d0))))
  (let* ((base (darr 1d0 2d0 3d0 4d0 5d0 6d0))
         (disp (make-array 3 :element-type 'double-float :displaced-to base :displaced-index-offset 2)))
    (assert-true (array-is (+ disp (darr 10d0)) 'double-float 3 '(13d0 14d0 15d0)))
    (assert-true (array-is (+ disp disp) 'double-float 3 '(6d0 8d0 10d0))))
  (let ((fpt (make-array 4 :fill-pointer 2 :initial-contents '(1 2 3 4))))
    (assert-true (array-is (+ fpt 1) 'T 2 '(2 3)))))

(deftest bad-operands (shape-tests)
  "Non-numbers alongside arrays signal TYPE-ERROR with the offending object as datum."
  (let ((c (condition-of (+ (darr 1d0) 'foo))))
    (assert-true (typep c 'type-error))
    (assert-eq 'foo (type-error-datum c))
    (assert-equal 'number (type-error-expected-type c)))
  (let ((c (condition-of (+ 'foo (darr 1d0)))))
    (assert-eq 'foo (type-error-datum c)))
  (let ((c (condition-of (+ (darr 1d0) '(1 2)))))
    (assert-equal '(1 2) (type-error-datum c)))
  ;; arrays that cannot hold numbers: the array is the datum
  (let* ((s (make-array 3 :element-type 'character :initial-element #\a))
         (c (condition-of (+ s 1))))
    (assert-true (typep c 'type-error))
    (assert-eq s (type-error-datum c)))
  ;; element type T with a non-number inside
  (let* ((v (varr 1 2 'x))
         (c (condition-of (+ v 1))))
    (assert-true (typep c 'type-error))
    (assert-eq v (type-error-datum c))))

(deftest no-mutation-of-inputs (shape-tests)
  (let* ((a (mat23)) (b (darr 10d0 20d0)) (i (iarr '(unsigned-byte 8) 1 2 3 4 5 6))
         (ca (copy-arr a)) (cb (copy-arr b)) (ci (copy-arr i)))
    (+ a b) (+ a 1d0) (+ b a) (+ a b 1d0 b) (+ i a) (+ i 1) (+ a i) (+ i i)
    (assert-true (same-contents-p a ca))
    (assert-true (same-contents-p b cb))
    (assert-true (same-contents-p i ci))))
