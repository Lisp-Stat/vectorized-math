;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; = /= < > <= >=  zerop plusp minusp evenp oddp  max min  (src/compare.lisp).
;;; In this package none of these is shadowed: the VM operators are written vm:op, CL's cl:op.
(in-package #:vm-tests)

(defsuite compare-tests (vm-tests))

(defparameter *rel-names* '("=" "/=" "<" ">" "<=" ">="))
(defun rel-vm (name)
  (ecase (intern name :keyword) (:= #'vm:=) (:/= #'vm:/=) (:< #'vm:<) (:> #'vm:>) (:<= #'vm:<=) (:>= #'vm:>=)))
(defun rel-cl (name)
  (ecase (intern name :keyword) (:= #'cl:=) (:/= #'cl:/=) (:< #'cl:<) (:> #'cl:>) (:<= #'cl:<=) (:>= #'cl:>=)))

(defun ref-rel (name x y)
  "1 or 0: CL's relation NAME of x and y, with IEEE rules for a NaN (the reference for array elements)."
  (cond ((or (nanp x) (nanp y)) (if (string= name "/=") 1 0))
        (t (if (masked-cl (rel-cl name) x y) 1 0))))

(defun bits (a) (contents a))

(defun bit-array-p (a) (typep a '(simple-array bit)))

(defun cm-index (dims r)
  "Column-major index of the element with row-major index R of an array of dimensions DIMS."
  (let ((subs (let ((rem r))
                (loop for d in (reverse dims)
                      collect (multiple-value-bind (q m) (floor rem d) (setf rem q) m))))
        (c 0) (stride 1))
    (loop for i in (reverse subs) for d in dims do (incf c (* i stride)) (setf stride (* stride d)))
    c))

(defun operand-elt (o dims r)
  "Element of operand O (a number or an array) seen at row-major position R of an array of DIMS."
  (if (not (arrayp o))
      o
      (let ((m (array-total-size o)))
        (cond ((equal (array-dimensions o) dims) (row-major-aref o r))
              ((<= (length dims) 1) (row-major-aref o (mod r m)))
              (t (row-major-aref o (mod (cm-index dims r) m)))))))

(defun ref-chain (name operands dims)
  "The reference result (list of 0/1) of the n-ary relation NAME over OPERANDS recycled to DIMS."
  (let* ((n (length operands))
         (pairs (if (string= name "/=")
                    (loop for i below n append (loop for j from (1+ i) below n collect (cons i j)))
                    (loop for i below (1- n) collect (cons i (1+ i))))))
    (loop for r below (reduce #'* dims)
          collect (if (every (lambda (p)
                               (= 1 (ref-rel name (operand-elt (nth (car p) operands) dims r)
                                             (operand-elt (nth (cdr p) operands) dims r))))
                             pairs)
                      1 0))))

;;; ---------------------------------------------------------------------------
;;; scalars: exactly CL

(defparameter *cmp-corpus*
  (append *ops-corpus*
          (list (expt 2 53) (1+ (expt 2 53)) 9007199254740992d0 0.1d0 1/10 16777217 16777216f0 (expt 2 64)
                -1/2 100 (- (expt 2 63)))))

(deftest rel-scalar-identity (compare-tests)
  "For numbers every relation is CL's: values, conditions, arities."
  (dolist (name *rel-names*)
    (let ((vm (rel-vm name)) (cl (rel-cl name)))
      ;; arities: none, one, many
      (multiple-value-bind (v c) (call-pair vm cl '()) (assert-true (same-result-p v c)) (assert-true (typep v 'error)))
      (dolist (x *cmp-corpus*)
        (multiple-value-bind (v c) (call-pair vm cl (list x)) (assert-true (same-result-p v c)))
        (dolist (y *cmp-corpus*)
          (multiple-value-bind (v c) (call-pair vm cl (list x y)) (assert-true (same-result-p v c))))
        (multiple-value-bind (v c) (call-pair vm cl (list x 1 x)) (assert-true (same-result-p v c)))
        (multiple-value-bind (v c) (call-pair vm cl (list 1 2 3 x)) (assert-true (same-result-p v c))))
      (dolist (args '((a) (1 a) (a 1) ("abc" 1) (1 2 a) (:foo 1) (nil 1) ((1 2) 3)))
        (multiple-value-bind (v c) (call-pair vm cl args)
          (assert-true (same-result-p v c))
          (when (null (cddr args)) (when (rest args) (assert-true (typep v 'error)))))))))

(deftest predicate-scalar-identity (compare-tests)
  (dolist (spec (list (list #'vm:zerop #'cl:zerop) (list #'vm:plusp #'cl:plusp) (list #'vm:minusp #'cl:minusp)
                      (list #'vm:evenp #'cl:evenp) (list #'vm:oddp #'cl:oddp)))
    (destructuring-bind (vm cl) spec
      (multiple-value-bind (v c) (call-pair vm cl '()) (assert-true (same-result-p v c)) (assert-true (typep v 'error)))
      (multiple-value-bind (v c) (call-pair vm cl '(1 2)) (assert-true (same-result-p v c)) (assert-true (typep v 'error)))
      (dolist (x (append *cmp-corpus* '(a "abc" nil (1))))
        (multiple-value-bind (v c) (call-pair vm cl (list x)) (assert-true (same-result-p v c)))))))

(deftest maxmin-scalar-identity (compare-tests)
  (dolist (spec (list (list #'vm:max #'cl:max) (list #'vm:min #'cl:min)))
    (destructuring-bind (vm cl) spec
      (multiple-value-bind (v c) (call-pair vm cl '()) (assert-true (same-result-p v c)) (assert-true (typep v 'error)))
      (dolist (x (append *cmp-corpus* '(a "abc" nil)))
        (multiple-value-bind (v c) (call-pair vm cl (list x)) (assert-true (same-result-p v c)))
        (dolist (y *cmp-corpus*)
          (multiple-value-bind (v c) (call-pair vm cl (list x y)) (assert-true (same-result-p v c))))
        (multiple-value-bind (v c) (call-pair vm cl (list x 1 2.5d0)) (assert-true (same-result-p v c)))))))

(deftest compiled-scalar-identity (compare-tests)
  "Compiled calls (the compiler macros) with declared and undeclared arguments agree with CL."
  (dolist (name *rel-names*)
    (let ((f-decl (compile nil `(lambda (a b) (declare (double-float a b)) (,(find-symbol name :vm) a b))))
          (f-gen (compile nil `(lambda (a b) (,(find-symbol name :vm) a b))))
          (f-fix (compile nil `(lambda (a b) (declare (fixnum a b)) (,(find-symbol name :vm) a b 1)))))
      (dolist (x '(1d0 2d0 -3.5d0))
        (dolist (y '(1d0 2d0 -3.5d0))
          (assert-equal (funcall (rel-cl name) x y) (funcall f-decl x y))
          (assert-equal (funcall (rel-cl name) x y) (funcall f-gen x y))))
      (dolist (x '(0 1 2 -5))
        (dolist (y '(0 1 2 -5))
          (assert-equal (funcall (rel-cl name) x y 1) (funcall f-fix x y))))))
  ;; one evaluation of each argument, left to right
  (let ((trace '()))
    (flet ((tr (x) (push x trace) x))
      (assert-equal t (vm:< (tr 1) (tr 2) (tr 3)))
      (assert-equal '(3 2 1) trace)))
  (assert-equal 3 (vm:max 1 3 2))
  (assert-equal 1 (vm:min 1 3 2))
  (let ((f (compile nil '(lambda (x) (vm:max x 2)))))
    (assert-equal 5 (funcall f 5))
    (assert-true (array-is (funcall f (iarr '(signed-byte 64) 1 5)) '(signed-byte 64) 2 '(2 5)))))

;;; ---------------------------------------------------------------------------
;;; exactness: every pair of corpus values against cl:op

(defparameter *x-doubles* (list 0d0 -0d0 1d0 -1d0 0.1d0 0.5d0 2d0 9007199254740992d0 9007199254740994d0
                                1d300 -1d300 1d-300 3.4028234663852886d38 16777217d0 1d23 123456789.123d0 0.3d0))
(defparameter *x-singles* (list 0f0 -0f0 1f0 -1f0 0.1f0 1.5f0 16777216f0 3.4028235f38 1f-30 0.3f0))
(defparameter *x-sb64* (list 0 1 -1 7 -42 16777217 9007199254740992 9007199254740993 -9007199254740993
                             4611686018427387903 -4611686018427387904 9223372036854775807 -9223372036854775808))
(defparameter *x-ub8* (list 0 1 7 100 255))
(defparameter *x-all* (remove-if-not #'realp (append *cmp-corpus* *x-doubles* *x-singles* *x-sb64*
                                                      (list 1/10 (/ 1 9007199254740993) (expt 10 30) (- (expt 2 70))))))

(defparameter *kinds*
  `((:double double-float ,*x-doubles*) (:single single-float ,*x-singles*)
    (:sb64 (signed-byte 64) ,*x-sb64*) (:ub8 (unsigned-byte 8) ,*x-ub8*) (:all t ,*x-all*)))

(defun cross (xs ys xet yet)
  "Arrays X and Y of all pairs (x y), x in XS, y in YS."
  (let ((nx (length xs)) (ny (length ys)))
    (values (make-arr xet (* nx ny) (loop for x in xs append (make-list ny :initial-element x)))
            (make-arr yet (* nx ny) (loop repeat nx append ys)))))

(deftest rel-exact-against-cl (compare-tests)
  "Array results equal CL's relation per element, for every pair of operand element types, array
against array and array against scalar, including float against rational (exact) and big integers."
  (dolist (name *rel-names*)
    (dolist (ka *kinds*)
      (dolist (kb *kinds*)
        (destructuring-bind (ta eta xs) ka
          (declare (ignore ta))
          (destructuring-bind (tb etb ys) kb
            (declare (ignore tb))
            (multiple-value-bind (x y) (cross xs ys eta etb)
              (let ((r (funcall (rel-vm name) x y))
                    (expected (loop for xv in xs append (loop for yv in ys collect (ref-rel name xv yv)))))
                (assert-true (bit-array-p r))
                (assert-equal expected (bits r))))
            ;; array against scalar, both orders
            (let ((xa (make-arr eta (length xs) xs)))
              (dolist (yv ys)
                (assert-equal (mapcar (lambda (xv) (ref-rel name xv yv)) xs) (bits (funcall (rel-vm name) xa yv)))
                (assert-equal (mapcar (lambda (xv) (ref-rel name yv xv)) xs) (bits (funcall (rel-vm name) yv xa)))))))))))

(deftest rel-big-and-infinite (compare-tests)
  "Infinities against bignums and huge rationals: hard-coded correct values (a regression test for the CCL fix of infinity comparison)."
  (let ((big (expt 10 400)) (inf (inf+)) (ninf (inf-)))
    (let ((a (darr inf ninf 1d300)))
      (assert-equal '(0 1 1) (bits (vm:< a big)))
      (assert-equal '(1 0 0) (bits (vm:> a big)))
      (assert-equal '(0 0 0) (bits (vm:= a big)))
      (assert-equal '(1 1 1) (bits (vm:/= a big)))
      (assert-equal '(1 0 1) (bits (vm:> a (- big))))
      (assert-equal '(0 1 0) (bits (vm:< a (- big))))
      (assert-equal '(1 0 1) (bits (vm:> (varr big (- big) 5) (darr 1d300 inf ninf))))
      (assert-equal '(0 1 0) (bits (vm:< a (varr big 1/3 (- big)))))
      (assert-equal '(0 0 0) (bits (vm:= a (varr big 1/3 (- big)))))
      (assert-equal '(1 0 1) (bits (vm:= (darr inf ninf 5d0) (darr inf 1d0 5d0)))))))

;;; ---------------------------------------------------------------------------
;;; NaN, infinities, -0.0 in arrays (IEEE rules)

(deftest rel-nan-ieee (compare-tests)
  "Every relation with a NaN is 0 except /=, which is 1: double, single, T and complex arrays,
array against array and against scalars, whatever the implementation's own comparison does."
  (let* ((n (nan)) (sn (single-nan)) (inf (inf+)) (ninf (inf-))
         (a (darr n 1d0 n inf ninf -0d0 2d0))
         (b (darr 1d0 n n inf inf 0d0 1d0)))
    (dolist (name *rel-names*)
      (let ((expected (mapcar (lambda (x y) (ref-rel name x y)) (contents a) (contents b))))
        (assert-equal expected (bits (funcall (rel-vm name) a b)))
        ;; same data, T arrays
        (assert-equal expected (bits (funcall (rel-vm name) (make-arr t 7 (contents a)) (make-arr t 7 (contents b)))))
        (assert-equal (mapcar (lambda (x) (ref-rel name x n)) (contents a)) (bits (funcall (rel-vm name) a n)))
        (assert-equal (mapcar (lambda (x) (ref-rel name n x)) (contents a)) (bits (funcall (rel-vm name) n a)))
        (assert-equal (mapcar (lambda (x) (ref-rel name x 1)) (contents a)) (bits (funcall (rel-vm name) a 1)))
        (assert-equal (mapcar (lambda (x) (ref-rel name x 1/2)) (contents a)) (bits (funcall (rel-vm name) a 1/2)))
        (assert-equal (mapcar (lambda (x) (ref-rel name x 1d0)) (contents a)) (bits (funcall (rel-vm name) a 1d0)))
        (assert-equal (mapcar (lambda (x) (ref-rel name x 1d0)) (contents a)) (bits (funcall (rel-vm name) a 1f0)))
        ;; single arrays and a double scalar
        (let ((sa (sarr sn 1f0 sn -0f0 0.5f0)))
          (assert-equal (mapcar (lambda (x) (ref-rel name x 0.5d0)) (contents sa)) (bits (funcall (rel-vm name) sa 0.5d0)))
          (assert-equal (mapcar (lambda (x) (ref-rel name x sn)) (contents sa)) (bits (funcall (rel-vm name) sa sn)))
          (assert-equal (mapcar (lambda (x) (ref-rel name x 0)) (contents sa)) (bits (funcall (rel-vm name) sa 0))))))
    ;; the statement of the rule itself
    (assert-equal '(0 0 0 0 0) (bits (vm:= (darr n n 1d0 n 0d0) (darr n 1d0 n n n))))
    (assert-equal '(1 1 0 1) (bits (vm:/= (darr n n 1d0 n) (darr n 1d0 1d0 n))))
    (dolist (name '("<" ">" "<=" ">=" "="))
      (assert-equal '(0 0 0) (bits (funcall (rel-vm name) (darr n n n) (darr 1d0 n inf)))))
    (assert-equal '(1 1 1) (bits (vm:/= (darr n n n) (darr 1d0 n inf))))
    ;; -0.0 = 0.0, and infinities
    (assert-equal '(1 0 1 0) (bits (vm:= (darr -0d0 0d0 inf ninf) (darr 0d0 1d0 inf inf))))
    (assert-equal '(0 1 0 0) (bits (vm:< (darr inf ninf inf 0d0) (darr ninf inf inf 0d0))))
    ;; n-ary: any NaN in a chain kills it, except for /= where only pairs with NaN are distinct
    (assert-equal '(0 1) (bits (vm:< (darr 1d0 1d0) (darr 2d0 2d0) (darr n 3d0))))
    (assert-equal '(1 1) (bits (vm:/= (darr n 1d0) (darr n 2d0) (darr 5d0 3d0))))
    (assert-equal '(1 0) (bits (vm:/= (darr n 1d0) (darr n 1d0) (darr 5d0 3d0))))))

(deftest rel-complex-equality (compare-tests)
  "= and /= accept complex operands (arrays and scalars); < > <= >= signal CL's type error."
  (let* ((n (nan))
         (z (cdarr #c(1d0 2d0) #c(0d0 0d0) #c(3d0 -1d0) (complex n 1d0)))
         (w (cdarr #c(1d0 2d0) #c(0d0 1d0) #c(3d0 1d0) (complex n 1d0))))
    (assert-equal '(1 0 0 0) (bits (vm:= z w)))
    (assert-equal '(0 1 1 1) (bits (vm:/= z w)))
    (assert-equal '(1 0 0 0) (bits (vm:= z #c(1d0 2d0))))
    (assert-equal '(0 1 0 0) (bits (vm:= z 0)))
    (assert-equal '(0 1 0 0) (bits (vm:= 0 z)))
    (assert-equal '(1 0 0 0) (bits (vm:= z #c(1 2))))
    (assert-equal '(1 0 0 0) (bits (vm:= z (varr #c(1 2) 5 6 7))))
    (assert-equal '(0 1 1 1) (bits (vm:/= z #c(1 2))))
    ;; complex single, real double array
    (assert-equal '(1 1 0) (bits (vm:= (csarr #c(1f0 0f0) #c(2f0 0f0) #c(2f0 1f0)) (darr 1d0 2d0 2d0))))
    (assert-equal '(1 0) (bits (vm:= (csarr #c(0.1f0 0f0) #c(0.5f0 1f0)) (sarr 0.1f0 0.5f0))))
    (assert-equal '(0 1) (bits (vm:= (csarr #c(0.1f0 0f0) #c(0.5f0 0f0)) (darr 0.1d0 0.5d0))))
    ;; chains
    (assert-equal '(1 0) (bits (vm:= (cdarr #c(1d0 1d0) #c(2d0 0d0)) #c(1d0 1d0) (cdarr #c(1d0 1d0) #c(1d0 1d0)))))
    ;; every element against cl:=
    (let ((zs (list #c(1d0 2d0) #c(0d0 0d0) 1d0 #c(1 2) #c(1/2 0) 0.5d0 1/2 3)))
      (multiple-value-bind (x y) (cross zs zs t t)
        (assert-equal (loop for a in zs append (loop for b in zs collect (if (cl:= a b) 1 0))) (bits (vm:= x y)))
        (assert-equal (loop for a in zs append (loop for b in zs collect (if (cl:/= a b) 1 0))) (bits (vm:/= x y)))))
    (dolist (name '("<" ">" "<=" ">="))
      (let ((f (rel-vm name)))
        (dolist (args (list (list z 1d0) (list 1d0 z) (list z w) (list #c(1 2) (darr 1d0)) (list (darr 1d0) #c(1d0 2d0))
                            (list (varr 1 #c(1 2)) 1) (list (csarr #c(1f0 1f0)) 1) (list (darr 1d0 2d0) 1 z)))
          (let ((c (condition-of (apply f args))))
            (assert-true (typep c 'type-error))
            (assert-equal 'real (type-error-expected-type c))
            (assert-true (complexp (type-error-datum c)))))))
    (let ((c (condition-of (vm:< (make-array 0 :element-type '(complex double-float)) 1))))
      (assert-true (typep c 'type-error)))))

;;; ---------------------------------------------------------------------------
;;; shapes

(deftest rel-shapes (compare-tests)
  "Recycling, column-major emulation, warnings, errors, zero length, rank 0."
  ;; vectors
  (with-warnings (r ws) (vm:< (darr 1d0 5d0 2d0 6d0 3d0 7d0) (darr 2d0 4d0))
    (assert-equal 0 (length ws))
    (assert-true (array-is r 'bit 6 '(1 0 0 0 0 0))))
  (with-warnings (r ws) (vm:< (darr 1d0 5d0 2d0 6d0 3d0) (darr 2d0 4d0))
    (assert-equal 1 (length (recycling-warnings ws)))
    (assert-true (array-is r 'bit 5 '(1 0 0 0 0))))
  (with-warnings (r ws) (vm:< (darr 2d0 4d0) (darr 1d0 5d0 2d0 6d0 3d0))
    (assert-equal 1 (length (recycling-warnings ws)))
    (assert-true (array-is r 'bit 5 '(0 1 0 1 1))))
  (assert-true (array-is (vm:= (iarr 'fixnum 1 2 3) 2) 'bit 3 '(0 1 0)))
  (assert-true (array-is (vm:= 2 (iarr 'fixnum 1 2 3)) 'bit 3 '(0 1 0)))
  (assert-true (array-is (vm:= (iarr 'fixnum 1 2 3) (iarr 'fixnum 2)) 'bit 3 '(0 1 0)))
  ;; zero length
  (assert-true (array-is (vm:< (darr) 1) 'bit 0 '()))
  (assert-true (array-is (vm:< (darr) (darr 1d0 2d0)) 'bit 0 '()))
  (assert-true (array-is (vm:< 1 (varr)) 'bit 0 '()))
  ;; matrices: column-major recycling of a vector
  (let* ((m (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0)))     ; rows (1 2 3) (4 5 6)
         (v (darr 2d0 5d0)))
    ;; v recycled column-major: element (i,j) compares with v[i]: row 0 vs 2, row 1 vs 5
    (let ((r (vm:< m v)))
      (assert-true (array-is r 'bit '(2 3) '(1 0 0 1 0 0)))
      (assert-true (array-is (vm:> v m) 'bit '(2 3) '(1 0 0 1 0 0))))
    (assert-true (array-is (vm:= m (darr 1d0 2d0 3d0 4d0 5d0 6d0)) 'bit '(2 3)
                           (loop for r below 6 collect (if (= (1+ r) (1+ (cm-index '(2 3) r))) 1 0))))
    (assert-true (array-is (vm:< m 3.5d0) 'bit '(2 3) '(1 1 1 0 0 0)))
    (assert-true (array-is (vm:< m m) 'bit '(2 3) '(0 0 0 0 0 0)))
    (assert-true (array-is (vm:<= m (make-arr t '(2 3) (contents m))) 'bit '(2 3) '(1 1 1 1 1 1)))
    (let ((c (condition-of (vm:< m (make-arr 'double-float '(3 2) (contents m))))))
      (assert-true (typep c 'vm:non-conformable-arrays)))
    (let ((c (condition-of (vm:< m (darr 1d0 2d0 3d0 4d0 5d0 6d0 7d0)))))
      (assert-true (typep c 'vm:non-conformable-arrays)))
    (with-warnings (r ws) (vm:< m (darr 1d0 2d0 3d0 4d0))
      (assert-equal 1 (length (recycling-warnings ws)))
      (assert-equal '(2 3) (array-dimensions r))))
  ;; a 2x3x2 array against every vector length that divides its size, against the oracle
  (let* ((a (make-arr 'fixnum '(2 3 2) '(5 1 4 2 3 3 9 0 2 7 6 8))))
    (dolist (m '(1 2 3 4 6 12))
      (let ((v (make-arr 'fixnum m (loop for i below m collect (* 2 i)))))
        (dolist (name *rel-names*)
          (assert-equal (ref-chain name (list a v) '(2 3 2)) (bits (funcall (rel-vm name) a v)))
          (assert-equal (ref-chain name (list v a) '(2 3 2)) (bits (funcall (rel-vm name) v a)))))))
  ;; rank 0
  (let ((z (make-array nil :element-type 'double-float :initial-element 3d0)))
    (let ((r (vm:< z 4d0))) (assert-equal nil (array-dimensions r)) (assert-equal 1 (aref r)))
    (let ((r (vm:< z z))) (assert-equal nil (array-dimensions r)) (assert-equal 0 (aref r)))
    (let ((r (vm:< z (darr 1d0 5d0)))) (assert-true (array-is r 'bit 2 '(0 1)))))
  ;; non-simple arrays (fill pointer, displaced)
  (let ((fp (make-array 5 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0 5d0) :fill-pointer 3))
        (disp (make-array 3 :element-type 'double-float :displaced-to (darr 9d0 1d0 2d0 3d0) :displaced-index-offset 1)))
    (assert-true (array-is (vm:< fp 2.5d0) 'bit 3 '(1 1 0)))
    (assert-true (array-is (vm:> disp 1.5d0) 'bit 3 '(0 1 1)))))

(deftest rel-one-argument-and-errors (compare-tests)
  (dolist (name *rel-names*)
    (let ((f (rel-vm name)))
      (assert-true (array-is (funcall f (darr 1d0 2d0)) 'bit 2 '(1 1)))
      (assert-true (array-is (funcall f (make-arr 'fixnum '(2 2) '(1 2 3 4))) 'bit '(2 2) '(1 1 1 1)))
      (assert-true (array-is (funcall f (darr)) 'bit 0 '()))
      ;; a non-number: CL's error (the argument is the datum)
      (let ((c (condition-of (funcall f "abc" 1))))
        (assert-true (typep c 'type-error)) (assert-equal "abc" (type-error-datum c)) (assert-equal (type-error-expected-type (condition-of (apply (rel-cl name) (opaque (list "abc" 1))))) (type-error-expected-type c)))
      (let ((c (condition-of (funcall f (darr 1d0) 'a))))
        (assert-true (typep c 'type-error)) (assert-equal 'a (type-error-datum c)))
      (let ((c (condition-of (funcall f (varr 1 'a) 1))))
        (assert-true (typep c 'type-error)))
      (let ((c (condition-of (funcall f '(1 2) (darr 1d0)))))
        (assert-true (typep c 'type-error)) (assert-equal '(1 2) (type-error-datum c)))))
  (assert-equal t (vm:= 1))
  (assert-equal t (vm:= #c(1 2)))
  (assert-true (array-is (vm:= (darr 1d0 2d0)) 'bit 2 '(1 1))))

(deftest rel-chains (compare-tests)
  "N-ary relations are elementwise over one common shape; every operand is recycled to it."
  (let* ((a (iarr 'fixnum 1 2 3 4 5 6))
         (b (iarr 'fixnum 2 3 4))
         (c (iarr 'fixnum 6 5 7 8 9 9 1 1 1 1 1 1)))
    (dolist (name *rel-names*)
      (assert-equal (ref-chain name (list a b c) '(12)) (bits (funcall (rel-vm name) a b c)))
      (assert-equal (ref-chain name (list 3 a 6) '(6)) (bits (funcall (rel-vm name) 3 a 6)))
      (assert-equal (ref-chain name (list c 1 b a) '(12)) (bits (funcall (rel-vm name) c 1 b a)))
      (assert-equal (ref-chain name (list a b 2 b a) '(6)) (bits (funcall (rel-vm name) a b 2 b a)))))
  ;; matrix with vectors
  (let* ((m (make-arr 'double-float '(2 3) '(1d0 2d0 3d0 4d0 5d0 6d0)))
         (v (darr 1d0 5d0)) (w (darr 3d0 4d0 9d0)))
    (dolist (name *rel-names*)
      (assert-equal (ref-chain name (list m v w) '(2 3)) (bits (funcall (rel-vm name) m v w)))
      (assert-equal (ref-chain name (list v m 7d0) '(2 3)) (bits (funcall (rel-vm name) v m 7d0)))
      (assert-equal (ref-chain name (list w v m) '(2 3)) (bits (funcall (rel-vm name) w v m))))
    (assert-equal '(2 3) (array-dimensions (vm:< v m 7d0))))
  ;; the statement of the semantics
  (assert-true (array-is (vm:< (iarr 'fixnum 1 5 3) 4 (iarr 'fixnum 9 9 2)) 'bit 3 '(1 0 0)))
  (assert-true (array-is (vm:/= (iarr 'fixnum 1 2 3) (iarr 'fixnum 1 3 3) 3) 'bit 3 '(0 0 0)))
  (assert-true (array-is (vm:/= (iarr 'fixnum 1 2 3) (iarr 'fixnum 4 3 3) 7) 'bit 3 '(1 1 0)))
  ;; scalars inside a chain: a false scalar pair gives zeros of the array's shape
  (assert-true (array-is (vm:< 2 1 (darr 5d0 6d0)) 'bit 2 '(0 0)))
  (assert-true (array-is (vm:< 1 2 (darr 5d0 6d0)) 'bit 2 '(1 1)))
  (assert-true (array-is (vm:< (darr 5d0 6d0) 7 8) 'bit 2 '(1 1)))
  (assert-true (array-is (vm:< (darr 5d0 6d0) 8 7) 'bit 2 '(0 0)))
  (assert-true (array-is (vm:/= 1 1 (darr 5d0 6d0)) 'bit 2 '(0 0)))
  (assert-true (array-is (vm:= 1 1 (make-arr 'fixnum '(2 2) '(1 1 1 1))) 'bit '(2 2) '(1 1 1 1)))
  (assert-true (array-is (vm:= 1 2 (make-arr 'fixnum '(2 2) '(1 1 1 1))) 'bit '(2 2) '(0 0 0 0)))
  ;; warnings: one per operand whose length does not divide the common length
  (with-warnings (r ws) (vm:< (darr 1d0 2d0 3d0 4d0 5d0) (darr 2d0 9d0) 9d0)
    (assert-equal 1 (length (recycling-warnings ws)))
    (assert-true (array-is r 'bit 5 '(1 0 0 0 0))))
  (with-warnings (r ws) (vm:< (darr 1d0 2d0 3d0 4d0) (darr 2d0 9d0) (darr 9d0))
    (assert-equal 0 (length ws)))
  ;; non-conformable matrices in a chain
  (let ((m (make-arr 'fixnum '(2 3) '(1 2 3 4 5 6))))
    (assert-true (typep (condition-of (vm:< m 7 (make-arr 'fixnum '(3 2) '(1 2 3 4 5 6)))) 'vm:non-conformable-arrays))))

(deftest rel-type-checks-all-operands (compare-tests)
  "Every operand of a chain is checked, an array operand that cannot hold numbers gives CL's error."
  (let ((c (condition-of (vm:< (darr 1d0) 2 "abc"))))
    (assert-true (typep c 'type-error)) (assert-equal "abc" (type-error-datum c)))
  (let ((c (condition-of (vm:< 1 'x (darr 1d0)))))
    (assert-true (typep c 'type-error)) (assert-equal 'x (type-error-datum c)))
  (let ((c (condition-of (vm:= (darr 1d0) (make-array 2 :initial-element 'x)))))
    (assert-true (typep c 'type-error))))

;;; ---------------------------------------------------------------------------
;;; predicates

(defun ref-pred (name x)
  (if (nanp x)
      0
      (if (funcall (ecase (intern (string-upcase name) :keyword) (:zerop #'cl:zerop) (:plusp #'cl:plusp) (:minusp #'cl:minusp)
                     (:evenp #'cl:evenp) (:oddp #'cl:oddp))
                   x)
          1 0)))

(defun pred-vm (name)
  (ecase (intern (string-upcase name) :keyword) (:zerop #'vm:zerop) (:plusp #'vm:plusp) (:minusp #'vm:minusp) (:evenp #'vm:evenp) (:oddp #'vm:oddp)))

(deftest predicates-real-arrays (compare-tests)
  (let ((n (nan)) (sn (single-nan)) (inf (inf+)) (ninf (inf-)))
    (dolist (name '("zerop" "plusp" "minusp"))
      (let ((f (pred-vm name)))
        (let ((d (darr 0d0 -0d0 1d0 -1d0 n inf ninf 1d-300 -1d-300))
              (s (sarr 0f0 -0f0 1f0 -1f0 sn 1f-30 -1f-30)))
          (assert-true (array-is (funcall f d) 'bit 9 (mapcar (lambda (x) (ref-pred name x)) (contents d))))
          (assert-true (array-is (funcall f s) 'bit 7 (mapcar (lambda (x) (ref-pred name x)) (contents s))))
          (assert-true (array-is (funcall f (make-arr t 9 (contents d))) 'bit 9 (mapcar (lambda (x) (ref-pred name x)) (contents d)))))
        (let ((v (list 0 1 -1 1/2 -1/2 (expt 10 30) (- (expt 10 30)) 0.0d0)))
          (assert-true (array-is (funcall f (make-arr t 8 v)) 'bit 8 (mapcar (lambda (x) (ref-pred name x)) v))))))
    ;; -0.0: zerop true, minusp false (as CL); NaN: nothing
    (assert-equal '(1 1 0 0) (bits (vm:zerop (darr 0d0 -0d0 1d0 n))))
    (assert-equal '(0 0 0 0 0) (bits (vm:minusp (darr 0d0 -0d0 1d0 n inf))))
    (assert-equal '(0 0 1 0 1) (bits (vm:plusp (darr 0d0 -0d0 1d0 n inf))))
    (assert-equal '(0 0 0 0 1) (bits (vm:minusp (darr 0d0 -0d0 1d0 n ninf))))
    ;; complex zerop
    (assert-equal '(1 0 0 0) (bits (vm:zerop (cdarr #c(0d0 0d0) #c(0d0 1d0) #c(1d0 0d0) (complex n 0d0)))))
    (assert-equal '(1 0 0) (bits (vm:zerop (csarr #c(0f0 -0f0) #c(0f0 1f0) (complex sn 0f0)))))
    (assert-equal '(1 0) (bits (vm:zerop (varr #c(0 0) #c(0d0 1d0)))))
    ;; integer arrays of every element type
    (dolist (et '(bit (unsigned-byte 2) (unsigned-byte 8) (unsigned-byte 16) (signed-byte 8) (signed-byte 16)
                  (signed-byte 32) (signed-byte 64) (unsigned-byte 32) (unsigned-byte 64) fixnum))
      (let* ((vals (remove-if-not (lambda (x) (typep x et)) '(0 1 2 3 -1 -2 -3 100 -100 255 65535 2147483647 -2147483648
                                                              4611686018427387903 -4611686018427387904 9223372036854775807
                                                              -9223372036854775808 18446744073709551615)))
             (a (make-arr et (length vals) vals)))
        (dolist (name '("zerop" "plusp" "minusp" "evenp" "oddp"))
          (let ((r (funcall (pred-vm name) a)))
            (assert-true (array-is r 'bit (length vals) (mapcar (lambda (x) (ref-pred name x)) vals)))))))
    ;; a (unsigned-byte 64) array beyond the signed range
    (assert-equal '(0 1 0 1) (bits (vm:evenp (iarr '(unsigned-byte 64) 18446744073709551615 18446744073709551614 1 0))))
    (assert-equal '(1 0 1 0) (bits (vm:oddp (iarr '(unsigned-byte 64) 18446744073709551615 18446744073709551614 1 0))))
    (assert-equal '(1 0 1 0) (bits (vm:evenp (varr (expt 10 30) (1+ (expt 10 30)) 2 -3))))
    ;; shapes: the dimensions of the argument; rank 0; empty; non-simple
    (assert-true (array-is (vm:zerop (make-arr 'double-float '(2 3) '(0d0 1d0 0d0 1d0 0d0 1d0))) 'bit '(2 3) '(1 0 1 0 1 0)))
    (assert-true (array-is (vm:evenp (make-arr 'fixnum '(2 2) '(1 2 3 4))) 'bit '(2 2) '(0 1 0 1)))
    (let ((r (vm:zerop (make-array nil :element-type 'double-float :initial-element 0d0))))
      (assert-equal nil (array-dimensions r)) (assert-equal 1 (aref r)))
    (assert-true (array-is (vm:zerop (darr)) 'bit 0 '()))
    (assert-true (array-is (vm:evenp (iarr 'fixnum)) 'bit 0 '()))
    (assert-true (array-is (vm:zerop (make-array 3 :element-type 'double-float :initial-contents '(0d0 1d0 0d0) :fill-pointer 2))
                           'bit 2 '(1 0)))))

(deftest predicate-errors (compare-tests)
  "plusp/minusp of complex and evenp/oddp of non-integers signal the type error CL signals."
  (dolist (name '("plusp" "minusp"))
    (let ((f (pred-vm name)))
      (dolist (a (list (cdarr #c(1d0 1d0)) (csarr #c(1f0 0f0)) (varr 1 #c(1 2)) (varr #c(1/2 3))))
        (let ((c (condition-of (funcall f a))))
          (assert-true (typep c 'type-error))
          (assert-equal 'real (type-error-expected-type c))))
      (assert-true (typep (condition-of (funcall f (cdarr))) 'type-error))
      (assert-true (typep (condition-of (funcall f "abc")) 'type-error))))
  (dolist (name '("evenp" "oddp"))
    (let ((f (pred-vm name)))
      (dolist (a (list (darr 1d0 2d0) (sarr 2f0) (varr 1 1.5d0) (varr 1/2) (cdarr #c(1d0 1d0)) (varr 1 #c(2 0.5))))
        (let ((c (condition-of (funcall f a))))
          (assert-true (typep c 'type-error))
          (assert-equal 'integer (type-error-expected-type c))))
      (assert-true (typep (condition-of (funcall f (darr))) 'type-error))
      (assert-true (typep (condition-of (funcall f "abc")) 'type-error))))
  (dolist (name '("zerop" "plusp" "minusp" "evenp" "oddp"))
    (let ((c (condition-of (funcall (pred-vm name) "abc"))))
      (assert-true (typep c 'type-error)) (assert-equal "abc" (type-error-datum c)))
    (let ((c (condition-of (funcall (pred-vm name) (make-array 2 :initial-element 'a)))))
      (assert-true (typep c 'type-error)))
    ;; arity
    (assert-true (typep (condition-of (funcall (pred-vm name) (darr 1d0) (darr 1d0))) 'program-error))))

;;; ---------------------------------------------------------------------------
;;; :into

(deftest compare-into (compare-tests)
  (let ((a (darr 1d0 5d0 3d0 2d0)) (b (darr 2d0 2d0 4d0 2d0)))
    ;; bit output
    (let ((out (make-array 4 :element-type 'bit :initial-element 1)))
      (assert-true (eq out (vm:< a b :into out)))
      (assert-equal '(1 0 1 0) (bits out)))
    ;; other integer outputs and T
    (dolist (et '((unsigned-byte 8) (signed-byte 8) (signed-byte 32) (signed-byte 64) fixnum (unsigned-byte 2) t))
      (let ((out (make-array 4 :element-type et :initial-element 1)))
        (assert-true (eq out (vm:= a b :into out)))
        (assert-equal '(0 0 0 1) (bits out))
        (vm:> a b :into out)
        (assert-equal '(0 1 0 0) (bits out))))
    ;; float outputs are refused, and left unchanged
    (dolist (et '(single-float double-float (complex double-float)))
      (let* ((out (make-array 4 :element-type et :initial-element (coerce 7 et))) (before (copy-arr out)))
        (assert-true (typep (condition-of (vm:< a b :into out)) 'vm:output-type-error))
        (assert-true (same-contents-p out before))
        (assert-true (typep (condition-of (vm:zerop a :into out)) 'vm:output-type-error))
        (assert-true (typep (condition-of (vm:< 1 2 :into out)) 'vm:output-type-error))))
    ;; recycled and truncated like the other operators
    (let ((out (make-array 8 :element-type 'bit :initial-element 0)))
      (vm:< a b :into out)
      (assert-equal '(1 0 1 0 1 0 1 0) (bits out)))
    (with-warnings (r ws) (let ((out (make-array 3 :element-type 'bit :initial-element 0))) (vm:< a b :into out))
      (assert-equal 1 (length (recycling-warnings ws)))
      (assert-equal '(1 0 1) (bits r)))
    (let ((out (make-array 4 :element-type 'bit :initial-element 0)))
      (vm:< 1 2 :into out) (assert-equal '(1 1 1 1) (bits out))
      (vm:> 1 2 :into out) (assert-equal '(0 0 0 0) (bits out))
      (vm:< 1 (darr 2d0) :into out) (assert-equal '(1 1 1 1) (bits out)))
    ;; into one of the operands
    (let ((x (make-arr t 4 (list 1 2 3 4))))
      (vm:< x 3 :into x)
      (assert-equal '(1 1 0 0) (bits x)))
    ;; matrix out
    (let ((out (make-array '(2 2) :element-type 'bit)))
      (vm:< (make-arr 'fixnum '(2 2) '(1 5 3 2)) 3 :into out)
      (assert-equal '(1 0 0 1) (bits out)))
    ;; chains and predicates
    (let ((out (make-array 4 :element-type 'bit)))
      (vm:< 0 a 4 :into out) (assert-equal '(1 0 1 1) (bits out))
      (vm:zerop (darr 0d0 1d0 0d0 2d0) :into out) (assert-equal '(1 0 1 0) (bits out))
      (vm:evenp (iarr 'fixnum 1 2 3 4) :into out) (assert-equal '(0 1 0 1) (bits out)))
    ;; zero-sized out is accepted
    (assert-true (arrayp (vm:< a b :into (make-array 0 :element-type 'bit))))
    ;; not an array
    (assert-true (typep (condition-of (vm:< a b :into 5)) 'type-error))))

(deftest compare-no-mutation (compare-tests)
  (let* ((a (darr 1d0 2d0 3d0 4d0)) (b (iarr 'fixnum 3 3 3 3)) (c (varr 1 2 3 4)) (m (make-arr 'fixnum '(2 2) '(1 2 3 4)))
         (ac (copy-arr a)) (bc (copy-arr b)) (cc (copy-arr c)) (mc (copy-arr m)))
    (dolist (name *rel-names*) (funcall (rel-vm name) a b c) (funcall (rel-vm name) m 2 b))
    (dolist (name '("zerop" "plusp" "minusp" "evenp" "oddp"))
      (unless (member name '("evenp" "oddp") :test #'string=) (funcall (pred-vm name) a))
      (funcall (pred-vm name) b) (funcall (pred-vm name) c) (funcall (pred-vm name) m))
    (vm:max a b c) (vm:min a b c) (vm:max m 2 b) (vm:min b 2.5d0)
    (assert-true (same-contents-p a ac)) (assert-true (same-contents-p b bc))
    (assert-true (same-contents-p c cc)) (assert-true (same-contents-p m mc))))

;;; ---------------------------------------------------------------------------
;;; max and min

(defun ref-maxmin (maxp x y)
  "CL's max or min of the reals X and Y (no NaN), as a value of the contagion type."
  (let ((m (if maxp (cl:max x y) (cl:min x y)))
        (proto (cond ((or (typep x 'double-float) (typep y 'double-float)) 1d0)
                     ((or (typep x 'single-float) (typep y 'single-float)) 1f0))))
    (if proto (float m proto) m)))

(defun num-same-p (a b)
  "Same type and numerically equal (the sign of a zero is not compared)."
  (and (eq (type-of a) (type-of b)) (= a b)))

(defun maxmin-vm (maxp) (if maxp #'vm:max #'vm:min))

(deftest maxmin-against-cl (compare-tests)
  "Elementwise results are CL's max/min of each pair, as a value of the contagion type: every pair of
operand element types, array against array and against scalars."
  (dolist (maxp '(t nil))
    (let ((f (maxmin-vm maxp)))
      (dolist (ka *kinds*)
        (dolist (kb *kinds*)
          (destructuring-bind (ta eta xs) ka
            (declare (ignore ta))
            (destructuring-bind (tb etb ys) kb
              (declare (ignore tb))
              (multiple-value-bind (x y) (cross xs ys eta etb)
                (let ((r (funcall f x y))
                      (expected (loop for xv in xs append (loop for yv in ys collect (ref-maxmin maxp xv yv)))))
                  (assert-equal (length expected) (length (contents r)))
                  (assert-true (every #'num-same-p (contents r) expected))))
              (let ((xa (make-arr eta (length xs) xs)))
                (dolist (yv ys)
                  (assert-true (every #'num-same-p (contents (funcall f xa yv))
                                      (mapcar (lambda (xv) (ref-maxmin maxp xv yv)) xs)))
                  (assert-true (every #'num-same-p (contents (funcall f yv xa))
                                      (mapcar (lambda (xv) (ref-maxmin maxp yv xv)) xs))))))))))))

(deftest maxmin-contagion-types (compare-tests)
  "The result element type is the contagion type of the operands."
  (let ((i (iarr 'fixnum 1 5)) (u (iarr '(unsigned-byte 8) 3 3)) (s (sarr 2f0 9f0)) (d (darr 2d0 9d0)))
    (assert-true (array-is (vm:max i u) '(signed-byte 64) 2 '(3 5)))
    (assert-true (array-is (vm:min i u) '(signed-byte 64) 2 '(1 3)))
    (assert-true (array-is (vm:max i 3) '(signed-byte 64) 2 '(3 5)))
    (assert-true (array-is (vm:max i 3.5f0) 'single-float 2 '(3.5f0 5f0)))
    (assert-true (array-is (vm:min i 3.5f0) 'single-float 2 '(1f0 3.5f0)))
    (assert-true (array-is (vm:max i 3.5d0) 'double-float 2 '(3.5d0 5d0)))
    (assert-true (array-is (vm:max 3.5d0 i) 'double-float 2 '(3.5d0 5d0)))
    (assert-true (array-is (vm:max s 1) 'single-float 2 '(2f0 9f0)))
    (assert-true (array-is (vm:max s 3d0) 'double-float 2 '(3d0 9d0)))
    (assert-true (array-is (vm:max s d) 'double-float 2 '(2d0 9d0)))
    (assert-true (array-is (vm:max u s) 'single-float 2 '(3f0 9f0)))
    (assert-true (array-is (vm:min (varr 1/2 3) 2d0) 'double-float 2 '(0.5d0 2d0)))
    (assert-true (array-is (vm:max (varr 1/2 3) 1f0) 'single-float 2 '(1f0 3f0)))
    ;; rationals: exact values in an array of element type T
    (assert-true (array-is (vm:max i 1/2) t 2 '(1 5)))
    (assert-true (array-is (vm:min i 3/2) t 2 '(1 3/2)))
    (assert-true (array-is (vm:max (varr 1/3 1/2) (varr 1/2 1/3)) t 2 '(1/2 1/2)))
    ;; an unsigned 64-bit array beyond the signed range
    (assert-true (array-is (vm:max (iarr '(unsigned-byte 64) 18446744073709551615 1) (iarr 'fixnum -1 2))
                           t 2 '(18446744073709551615 2)))
    (assert-true (array-is (vm:min (iarr '(unsigned-byte 64) 18446744073709551615 1) (iarr 'fixnum -1 2))
                           '(signed-byte 64) 2 '(-1 1)))
    ;; scalars folded first keep the contagion: (max 1.0 2 i) is a single-float array
    (assert-true (array-is (vm:max 1f0 2 (iarr 'fixnum 3 0)) 'single-float 2 '(3f0 2f0)))
    (assert-true (array-is (vm:max 1d0 2 (iarr 'fixnum 3 0)) 'double-float 2 '(3d0 2d0)))
    (assert-true (array-is (vm:min 1f0 2 (iarr 'fixnum 3 0) 0.5d0) 'double-float 2 '(0.5d0 0d0)))
    ;; n-ary folds left
    (assert-true (array-is (vm:max i u 4) '(signed-byte 64) 2 '(4 5)))
    (assert-true (array-is (vm:max (darr 1d0 5d0 2d0 6d0) (darr 3d0 3d0) 4) 'double-float 4 '(4d0 5d0 4d0 6d0)))
    ;; one array argument: a fresh copy (of the type (+ array) gives)
    (let* ((a (iarr 'fixnum 1 2)) (r (vm:max a)))
      (assert-true (array-is r '(signed-byte 64) 2 '(1 2)))
      (assert-false (eq r a)))
    (assert-true (array-is (vm:min d) 'double-float 2 '(2d0 9d0)))
    ;; empty
    (assert-true (array-is (vm:max (darr) 1) 'double-float 0 '()))
    (assert-true (array-is (vm:max (iarr 'fixnum) (darr 1d0)) 'double-float 0 '()))))

(deftest maxmin-nan-rule (compare-tests)
  "NaN propagates (as in R's pmax/pmin), whatever the operand order and element type."
  (let ((n (nan)) (sn (single-nan)))
    (dolist (maxp '(t nil))
      (let ((f (maxmin-vm maxp)))
        (let ((r (funcall f (darr n 1d0 2d0 n) (darr 1d0 n 3d0 n))))
          (assert-equal '(t t nil t) (mapcar #'nanp (contents r)))
          (assert-equal (if maxp 3d0 2d0) (third (contents r))))
        (assert-equal '(t nil) (mapcar #'nanp (contents (funcall f (darr n 1d0) 0d0))))
        (assert-equal '(t nil) (mapcar #'nanp (contents (funcall f 0d0 (darr n 1d0)))))
        (assert-equal '(t t) (mapcar #'nanp (contents (funcall f (darr 1d0 2d0) n))))
        (assert-equal '(t t) (mapcar #'nanp (contents (funcall f n (darr 1d0 2d0)))))
        (assert-equal '(t t) (mapcar #'nanp (contents (funcall f (iarr 'fixnum 1 2) n))))
        (let ((r (funcall f (iarr 'fixnum 1 2) sn)))
          (assert-true (array-is-type r 'single-float))
          (assert-equal '(t t) (mapcar #'nanp (contents r))))
        (let ((r (funcall f (sarr sn 1f0) 2d0)))
          (assert-true (array-is-type r 'double-float))
          (assert-equal '(t nil) (mapcar #'nanp (contents r))))
        (assert-equal '(t nil) (mapcar #'nanp (contents (funcall f (varr n 1) 0))))
        (assert-equal '(t nil t) (mapcar #'nanp (contents (funcall f (varr n 1 2) (darr 0d0 0d0 n)))))
        ;; in an n-ary call
        (assert-equal '(t nil) (mapcar #'nanp (contents (funcall f (darr 1d0 2d0) (darr n 0d0) 5d0))))))
    ;; the infinities are ordinary values
    (let ((inf (inf+)) (ninf (inf-)))
      (assert-equal (list inf 1d0) (contents (vm:max (darr inf ninf) 1d0)))
      (assert-equal (list 1d0 ninf) (contents (vm:min (darr inf ninf) 1d0)))
      (assert-equal (list inf 5d0) (contents (vm:max (darr inf ninf) (iarr 'fixnum 5 5)))))))

(defun array-is-type (a et) (equal (array-element-type a) (upgraded-array-element-type et)))

(deftest maxmin-zeros-and-ties (compare-tests)
  "A tie returns the first operand, as CL does for the two zeros."
  (assert-true (array-is (vm:max (darr -0d0 0d0) (darr 0d0 -0d0)) 'double-float 2 '(-0d0 0d0)))
  (assert-true (array-is (vm:min (darr -0d0 0d0) (darr 0d0 -0d0)) 'double-float 2 '(-0d0 0d0)))
  (assert-true (array-is (vm:max 0d0 (darr -0d0)) 'double-float 1 '(0d0)))
  (assert-true (array-is (vm:max (darr -0d0) 0d0) 'double-float 1 '(-0d0)))
  (assert-true (array-is (vm:min (sarr -0f0) 0f0) 'single-float 1 '(-0f0)))
  (assert-equal (cl:max -0d0 0d0) (aref (vm:max (darr -0d0) 0d0) 0))
  (assert-equal (cl:min 0d0 -0d0) (aref (vm:min (darr 0d0) -0d0) 0)))

(deftest maxmin-shapes (compare-tests)
  (let ((m (make-arr 'fixnum '(2 3) '(1 2 3 4 5 6))))
    (assert-true (array-is (vm:max m (iarr 'fixnum 3 4)) '(signed-byte 64) '(2 3) '(3 3 3 4 5 6)))
    (assert-true (array-is (vm:min m (darr 3d0 4d0)) 'double-float '(2 3) '(1d0 2d0 3d0 4d0 4d0 4d0)))
    (assert-true (array-is (vm:max (iarr 'fixnum 3 4) m) '(signed-byte 64) '(2 3) '(3 3 3 4 5 6)))
    (assert-true (typep (condition-of (vm:max m (make-arr 'fixnum '(3 2) '(1 2 3 4 5 6)))) 'vm:non-conformable-arrays))
    (assert-true (typep (condition-of (vm:max m (iarr 'fixnum 1 2 3 4 5 6 7))) 'vm:non-conformable-arrays))
    (assert-true (array-is (vm:max m 0 m) '(signed-byte 64) '(2 3) '(1 2 3 4 5 6))))
  (with-warnings (r ws) (vm:max (iarr 'fixnum 1 2 3 4 5) (iarr 'fixnum 2 3))
    (assert-equal 1 (length (recycling-warnings ws)))
    (assert-true (array-is r '(signed-byte 64) 5 '(2 3 3 4 5))))
  (assert-true (array-is (vm:max (iarr 'fixnum 1 2 3 4 5 6) (iarr 'fixnum 2 5)) '(signed-byte 64) 6 '(2 5 3 5 5 6)))
  (let ((z (make-array nil :element-type 'double-float :initial-element 3d0)))
    (let ((r (vm:max z 4d0))) (assert-equal nil (array-dimensions r)) (assert-equal 4d0 (aref r)))
    (assert-true (array-is (vm:max z (darr 1d0 5d0)) 'double-float 2 '(3d0 5d0)))))

(deftest maxmin-errors-and-into (compare-tests)
  (dolist (f (list #'vm:max #'vm:min))
    (dolist (args (list (list (cdarr #c(1d0 1d0)) 1) (list 1 (varr 1 #c(1 2))) (list (csarr #c(1f0 0f0))) (list #c(1 2) (darr 1d0))))
      (let ((c (condition-of (apply f args))))
        (assert-true (typep c 'type-error)) (assert-equal 'real (type-error-expected-type c))))
    (let ((c (condition-of (funcall f "abc" 1))))
      (assert-true (typep c 'type-error)) (assert-equal "abc" (type-error-datum c)))
    (assert-true (typep (condition-of (funcall f (darr 1d0) 'x)) 'type-error))
    (assert-true (typep (condition-of (funcall f (darr 1d0) :into 5)) 'type-error)))
  ;; :into, widening yes, narrowing no
  (let ((out (make-array 3 :element-type 'double-float :initial-element 0d0)))
    (assert-true (eq out (vm:max (darr 1d0 5d0 3d0) 2d0 :into out)))
    (assert-equal '(2d0 5d0 3d0) (contents out))
    (vm:min (iarr 'fixnum 1 5 3) 2 :into out)
    (assert-equal '(1d0 2d0 2d0) (contents out)))
  (let ((out (make-array 3 :element-type t :initial-element 0)))
    (vm:max (iarr 'fixnum 1 5 3) 2 :into out)
    (assert-equal '(2 5 3) (contents out)))
  (let* ((out (make-array 3 :element-type 'single-float :initial-element 0f0)) (before (copy-arr out)))
    (assert-true (typep (condition-of (vm:max (darr 1d0 5d0 3d0) 2d0 :into out)) 'vm:output-type-error))
    (assert-true (same-contents-p out before)))
  (let ((out (make-array 4 :element-type '(signed-byte 64) :initial-element 0)))
    (vm:max (iarr 'fixnum 1 5) 3 :into out)
    (assert-equal '(3 5 3 5) (contents out))
    (assert-true (typep (condition-of (vm:max (darr 1d0 5d0) 3 :into out)) 'vm:output-type-error)))
  ;; in place
  (let ((x (darr 1d0 7d0 3d0)))
    (vm:max x 2d0 :into x)
    (assert-equal '(2d0 7d0 3d0) (contents x))))

;;; ---------------------------------------------------------------------------
;;; method combinations

(deftest method-combination-max-min (compare-tests)
  "(:method-combination max) and min, with :most-specific-last, in a package shadowing the operators."
  (let ((p (make-user-package)))
    (eval-in p "(progn (defclass mm-a () ()) (defclass mm-b (mm-a) ())
                  (defgeneric mm-f (x) (:method-combination max))
                  (defmethod mm-f max ((x mm-a)) 1)
                  (defmethod mm-f max ((x mm-b)) 10)
                  (defgeneric mm-g (x) (:method-combination min :most-specific-last))
                  (defvar *mm-trace* nil)
                  (defmethod mm-g min ((x mm-a)) (push :a *mm-trace*) 5)
                  (defmethod mm-g min ((x mm-b)) (push :b *mm-trace*) 7)
                  (defgeneric mm-h (x) (:method-combination max))
                  (defmethod mm-h max ((x mm-a)) 7)
                  (defgeneric mm-i (x) (:method-combination max :most-specific-last))
                  (defmethod mm-i max ((x mm-a)) 3)
                  (defmethod mm-i max ((x mm-b)) 2)
                  (defgeneric mm-j (x) (:method-combination min))
                  (defmethod mm-j min ((x mm-a)) 9)
                  (defmethod mm-j min ((x mm-b)) 4))")
    (assert-equal 10 (eval-in p "(mm-f (make-instance 'mm-b))"))
    (assert-equal 1 (eval-in p "(mm-f (make-instance 'mm-a))"))
    (assert-equal 5 (eval-in p "(mm-g (make-instance 'mm-b))"))
    (assert-equal '(:b :a) (eval-in p "*mm-trace*"))      ; most specific last: :a ran first
    (assert-equal 7 (eval-in p "(mm-h (make-instance 'mm-b))"))   ; one applicable method
    (assert-equal 3 (eval-in p "(mm-i (make-instance 'mm-b))"))
    (assert-equal 4 (eval-in p "(mm-j (make-instance 'mm-b))"))
    (assert-true (eq 'vm:max (let ((*package* p)) (read-from-string "max"))))
    (assert-true (eq 'vm:min (let ((*package* p)) (read-from-string "min"))))
    ;; the operators work in that package too, as functions and as arguments
    (assert-equal 3 (eval-in p "(max 1 3 2)"))
    (assert-equal 3 (eval-in p "(reduce #'max '(1 3 2))"))
    (assert-equal 1 (eval-in p "(apply #'min '(3 1 2))"))
    (assert-equal 2 (eval-in p "(funcall 'max 1 2)"))
    (assert-equal '(1 0 0) (contents (eval-in p "(< (make-array 3 :element-type 'fixnum :initial-contents '(1 5 9)) 4 (make-array 3 :element-type 'fixnum :initial-contents '(9 9 2)))")))
    (assert-equal t (eval-in p "(let ((x 5)) (and (< 0 x 10) (zerop 0) (plusp 1) (evenp 2) (oddp 3) (<= 1 1) (>= 2 1) (/= 1 2) (= 3 3)))"))))

(deftest method-combination-names (compare-tests)
  "VM:+, VM:MAX and VM:MIN are the built-in method-combination names among the operators."
  (assert-true (method-combination-exists-p 'vm:max))
  (assert-true (method-combination-exists-p 'vm:min))
  (dolist (s '(vm:= vm:/= vm:< vm:> vm:<= vm:>= vm:zerop vm:plusp vm:minusp vm:evenp vm:oddp))
    (assert-false (method-combination-exists-p s))))

(deftest scalars-with-nan-stay-cl (compare-tests)
  "A scalar NaN goes to cl:op whatever the implementation answers; only arrays follow IEEE."
  (let ((n (nan)))
    (dolist (name *rel-names*)
      (dolist (y (list 1d0 n 0 1/2))
        (assert-equal (masked-cl (rel-cl name) n y) (masked-cl (rel-vm name) n y))
        (assert-equal (masked-cl (rel-cl name) y n) (masked-cl (rel-vm name) y n))))
    (dolist (name '("zerop" "plusp" "minusp"))
      (assert-equal (masked-cl (ecase (intern (string-upcase name) :keyword) (:zerop #'cl:zerop) (:plusp #'cl:plusp) (:minusp #'cl:minusp)) n)
                    (masked-cl (pred-vm name) n)))
    (assert-equal (masked-cl #'cl:max n 1d0) (masked-cl #'vm:max n 1d0))
    ;; the same NaN inside a one-element array is IEEE on every implementation
    (assert-equal '(0) (bits (vm:< (darr n) 1d0)))
    (assert-equal '(0) (bits (vm:= (darr n) (darr n))))
    (assert-equal '(1) (bits (vm:/= (darr n) (darr n))))))

(deftest non-simple-arrays-everywhere (compare-tests)
  "Fill-pointer and displaced arrays are accepted by every operator of this file."
  (let ((fp (make-array 5 :element-type 'double-float :initial-contents '(1d0 0d0 3d0 4d0 5d0) :fill-pointer 3))
        (disp (make-array 3 :element-type 'fixnum :displaced-to (iarr 'fixnum 9 1 2 3) :displaced-index-offset 1))
        (adj (make-array 3 :adjustable t :initial-contents '(1 2 3))))
    (assert-true (array-is (vm:zerop fp) 'bit 3 '(0 1 0)))
    (assert-true (array-is (vm:evenp disp) 'bit 3 '(0 1 0)))
    (assert-true (array-is (vm:oddp adj) 'bit 3 '(1 0 1)))
    (assert-true (array-is (vm:max fp 2d0) 'double-float 3 '(2d0 2d0 3d0)))
    (assert-true (array-is (vm:min disp 2) '(signed-byte 64) 3 '(1 2 2)))
    (assert-true (array-is (vm:max disp adj) t 3 '(1 2 3)))
    (assert-true (array-is (vm:= disp adj fp) 'bit 3 '(1 0 1)))
    (assert-true (array-is (vm:< fp adj) 'bit 3 '(0 1 0)))))

(deftest method-combination-long-form-star (compare-tests)
  "The * of a qualifier pattern in the long form of define-method-combination means any qualifiers in
a package that shadows * (the type-wildcard hook canonicalises it), as in plain CL."
  (flet ((run (p)
           (eval-in p "(progn
  (define-method-combination mcl-all () ((methods *))
    `(list ,@(mapcar (lambda (m) `(call-method ,m)) methods)))
  (defgeneric mcl-f (x) (:method-combination mcl-all))
  (defmethod mcl-f ((x integer)) :plain)
  (defmethod mcl-f :weird ((x integer)) :weird)
  (defmethod mcl-f ((x number)) :number)
  (define-method-combination mcl-groups () ((around (:around)) (foos (:foo)) (rest () :order :most-specific-last)
                                              (tail (:tail . *)) (anyq (:any *)))
    `(list ,@(mapcar (lambda (m) `(call-method ,m)) (append foos rest tail anyq))))
  (defgeneric mcl-g (x) (:method-combination mcl-groups))
  (defmethod mcl-g ((x integer)) :plain-int)
  (defmethod mcl-g :foo ((x integer)) :foo)
  (defmethod mcl-g :tail 1 ((x integer)) :tail)
  (defmethod mcl-g :any 5 ((x integer)) :any)
  (list (mcl-f 1) (mcl-g 1)))")))
    (let ((plain (make-package (format nil "VM-TESTS-PLAIN-MC-~D" (incf *user-package-counter*)) :use '(:cl)))
          (user (make-user-package)))
      (let ((a (run user)) (b (run plain)))
        (assert-equal b a)
        (assert-equal '(:number :plain :weird) (sort (copy-list (first a)) #'string<))   ; the order of equally specific methods is the implementation's
        (assert-equal '(:foo :plain-int :tail :any) (second a))))))

;;; ---------------------------------------------------------------------------
;;; the datum of a type error is the offending operand, whatever numeric arrays precede it

(deftest type-error-datum-is-the-offending-operand (compare-tests)
  "(op (double-array) \"abc\") and (op \"abc\" (double-array)) report \"abc\", with the expected type CL
reports for it, for every operator that takes numbers (CL itself is called with 2 for the arrays)."
  (let ((ops (list (cons #'vm:+ #'cl:+) (cons #'vm:- #'cl:-) (cons #'vm:* #'cl:*) (cons #'vm:/ #'cl:/)
                   (cons #'vm:expt #'cl:expt) (cons #'vm:atan #'cl:atan) (cons #'vm:log #'cl:log)
                   (cons #'vm:complex #'cl:complex) (cons #'vm:max #'cl:max) (cons #'vm:min #'cl:min)
                   (cons #'vm:= #'cl:=) (cons #'vm:/= #'cl:/=) (cons #'vm:< #'cl:<) (cons #'vm:> #'cl:>)
                   (cons #'vm:<= #'cl:<=) (cons #'vm:>= #'cl:>=))))
    (loop for (vm-fn . cl-fn) in ops
          do (dolist (shape '((:array :string) (:string :array) (:array :array :string) (:string :array :array)
                              (:array :scalar :string)))
               (let* ((s "abc")
                      (args (mapcar (lambda (k) (ecase k (:array (darr 1d0 2d0)) (:scalar 3) (:string s))) shape))
                      (cl-args (mapcar (lambda (k) (ecase k (:array 2) (:scalar 3) (:string s))) shape))
                      (cl-c (condition-of (apply cl-fn (opaque cl-args)))))
                 ;; a relation stops at the first pair that is false, so CL may not see the string
                 (when (typep cl-c 'type-error)
                   (let ((c (condition-of (apply vm-fn args))))
                     (assert-true (typep c 'type-error))
                     (assert-true (eq s (type-error-datum c)))
                     (assert-equal (type-error-expected-type cl-c) (type-error-expected-type c)))))))
    ;; the unary operators
    (dolist (pair (list (cons #'vm:zerop #'cl:zerop) (cons #'vm:plusp #'cl:plusp) (cons #'vm:minusp #'cl:minusp)
                        (cons #'vm:evenp #'cl:evenp) (cons #'vm:oddp #'cl:oddp) (cons #'vm:sin #'cl:sin)
                        (cons #'vm:sqrt #'cl:sqrt) (cons #'vm:abs #'cl:abs)))
      (let ((c (condition-of (funcall (car pair) "abc")))
            (cl-c (condition-of (funcall (cdr pair) (opaque "abc")))))
        (assert-true (typep c 'type-error))
        (assert-equal "abc" (type-error-datum c))
        (assert-equal (type-error-expected-type cl-c) (type-error-expected-type c))))
    ;; :into does not change it
    (let ((c (condition-of (vm:+ (darr 1d0) "abc" :into (darr 0d0)))))
      (assert-true (typep c 'type-error))
      (assert-equal "abc" (type-error-datum c)))))

;;; ---------------------------------------------------------------------------
;;; NaN detection is bit level (NAN-P in the kernels, FLOAT-NAN-P elsewhere)

(defun float-from-bits (bits width)
  "The float of WIDTH 32 or 64 bits with the IEEE bit pattern BITS."
  (ecase width
    (64 (let ((hi (ldb (byte 32 32) bits)) (lo (ldb (byte 32 0) bits)))
          #+ccl (ccl::double-float-from-bits hi lo)
          #+sbcl (sb-kernel:make-double-float (if (>= hi #x80000000) (- hi #x100000000) hi) lo)))
    (32 #+ccl (ccl::host-single-float-from-unsigned-byte-32 bits)
        #+sbcl (sb-kernel:make-single-float (if (>= bits #x80000000) (- bits #x100000000) bits)))))

(defun nan-bits-p (bits width)
  "Is the IEEE pattern BITS of WIDTH 32 or 64 bits a NaN?  (Exponent all ones, mantissa not zero.)"
  (let* ((mant-bits (if (= width 64) 52 23))
         (exp-all-ones (1- (ash 1 (- width 1 mant-bits))))
         (exponent (ldb (byte (- width 1 mant-bits) mant-bits) bits))
         (mantissa (ldb (byte mant-bits 0) bits)))
    (and (= exponent exp-all-ones) (/= mantissa 0))))

(defun test-nan-d (x) (declare (type double-float x)) (vm.impl::nan-p x double-float))
(defun test-nan-s (x) (declare (type single-float x)) (vm.impl::nan-p x single-float))
(defun test-nan-scratch-d (x)
  (declare (type double-float x))
  (vm.impl::with-nan-scratch (vm.impl::nan-p x double-float)))
(defun test-nan-scratch-s (x)
  (declare (type single-float x))
  (vm.impl::with-nan-scratch (vm.impl::nan-p x single-float)))

(deftest nan-detection-from-bits (compare-tests)
  "NAN-P (with and without the kernel scratch block) and FLOAT-NAN-P agree with the IEEE bit pattern
for NaNs of both signs, quiet and signalling, payloads, infinities, zeros, denormals and ordinary
numbers; no comparison is involved."
  (dolist (width '(64 32))
    (let* ((mant (if (= width 64) 52 23)) (eexp (if (= width 64) 11 8))
           (exp-ones (1- (ash 1 eexp)))
           (patterns '()))
      (dolist (sign '(0 1))
        (dolist (exponent (list 0 1 (ash exp-ones -1) (1- exp-ones) exp-ones))
          (dolist (mantissa (list 0 1 (ash 1 (1- mant)) (1- (ash 1 mant)) (ash 1 (- mant 2)) #x12345))
            (push (logior (ash sign (+ eexp mant)) (ash exponent mant) mantissa) patterns))))
      (dolist (bits patterns)
        (let ((x (float-from-bits bits width)) (expected (nan-bits-p bits width)))
          (assert-equal expected (and (vm.impl::float-nan-p x) t))
          (if (= width 64)
              (progn (assert-equal expected (and (test-nan-d x) t))
                     (assert-equal expected (and (test-nan-scratch-d x) t)))
              (progn (assert-equal expected (and (test-nan-s x) t))
                     (assert-equal expected (and (test-nan-scratch-s x) t))))))))
  (assert-false (vm.impl::float-nan-p 1))
  (assert-false (vm.impl::float-nan-p "a"))
  (assert-false (vm.impl::float-nan-p #c(1d0 2d0))))
