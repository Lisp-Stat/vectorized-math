;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Operand classification and result-type computation (strict CL contagion).

(in-package #:vm.impl)

;;; An operand is a number or a simple array together with its "kind":
;;;   BASE  :int  integers (LO/HI give the value range)
;;;         :rat  rationals that may be ratios (or complex rationals)
;;;         :s :d single / double floats
;;;         :gen  anything else (element type T)
;;;   CPLX  true when the values are complex

(defstruct (operand (:constructor make-operand (value base cplx &optional lo hi)))
  value base cplx lo hi)

(declaim (inline operand-array-p))
(defun operand-array-p (o) (arrayp (operand-value o)))

(defconstant +sb64-min+ (- (expt 2 63)))
(defconstant +sb64-max+ (1- (expt 2 63)))

(declaim (inline fits-sb64))
(defun fits-sb64 (x) (and (integerp x) (<= +sb64-min+ x +sb64-max+)))

(defun not-a-number (datum)
  (error 'type-error :datum datum :expected-type 'number))

(defun not-a-numeric-array (array)
  (error 'type-error :datum array :expected-type 'number))

(defun classify-scalar (x)
  (typecase x
    (integer (make-operand x :int nil x x))
    (ratio (make-operand x :rat nil))
    (single-float (make-operand x :s nil))
    (double-float (make-operand x :d nil))
    (complex (let ((p (realpart x)))
               (typecase p
                 (single-float (make-operand x :s t))
                 (double-float (make-operand x :d t))
                 (t (make-operand x :rat t)))))
    (t (not-a-number x))))

(defvar *integer-ranges* (make-hash-table :test 'equal))

(defun integer-element-range (et)
  "Smallest (lo . hi) range containing the integer element type ET."
  (or (gethash et *integer-ranges*)
      (setf (gethash et *integer-ranges*)
            (cond ((null et) (cons 0 0))
                  ((subtypep et '(unsigned-byte 64))
                   (let ((n (loop for n from 1 to 64
                                  when (subtypep et `(unsigned-byte ,n)) return n)))
                     (cons 0 (1- (expt 2 n)))))
                  ((subtypep et '(signed-byte 64))
                   (let ((n (loop for n from 1 to 64
                                  when (subtypep et `(signed-byte ,n)) return n)))
                     (cons (- (expt 2 (1- n))) (1- (expt 2 (1- n))))))
                  (t (cons +sb64-min+ (1- (expt 2 64))))))))

(defun classify-array (a)
  "Classify simple array A by its element type; signal a type-error when it cannot hold numbers."
  (let ((et (array-element-type a)))
    (cond ((eq et 'single-float) (make-operand a :s nil))
          ((eq et 'double-float) (make-operand a :d nil))
          ((equal et '(complex single-float)) (make-operand a :s t))
          ((equal et '(complex double-float)) (make-operand a :d t))
          ((subtypep et 'integer)
           (let ((r (integer-element-range et)))
             (make-operand a :int nil (car r) (cdr r))))
          ((eq et t)
           ;; element type T: every element must be a number, even when the result is empty
           (let ((data (flat-data a)))
             (dotimes (i (array-total-size a))
               (unless (numberp (svref data i)) (not-a-numeric-array a))))
           (make-operand a :gen nil))
          ((subtypep et 'number) (make-operand a :gen nil))
          (t (not-a-numeric-array a)))))

(defun simplify-array (a)
  "A simple array with the same contents as A (A itself when already simple).
Vectors with fill pointers contribute their active elements only."
  (cond ((typep a 'simple-array) a)
        ((and (vectorp a) (array-has-fill-pointer-p a))
         (subseq a 0 (fill-pointer a)))
        (t (let* ((r (make-array (array-dimensions a) :element-type (array-element-type a))))
             (dotimes (i (array-total-size r) r)
               (setf (row-major-aref r i) (row-major-aref a i)))))))

(defun classify (x)
  (if (arrayp x)
      (classify-array (simplify-array x))
      (classify-scalar x)))

;;; --- contagion ---

(defun combine-base (x y)
  (cond ((or (eq x :gen) (eq y :gen)) :gen)
        ((and (eq x :int) (eq y :int)) :int)
        ((member x '(:int :rat)) (if (member y '(:int :rat)) :rat y))
        ((member y '(:int :rat)) x)
        ((or (eq x :d) (eq y :d)) :d)
        (t :s)))

(defun base-cplx-key (base cplx)
  "Element-type key for float kinds."
  (ecase base
    (:s (if cplx :complex-single-float :single-float))
    (:d (if cplx :complex-double-float :double-float))))

(defun key-element-type (key)
  (ecase key
    (:single-float 'single-float)
    (:double-float 'double-float)
    (:complex-single-float '(complex single-float))
    (:complex-double-float '(complex double-float))
    (:signed-byte-64 '(signed-byte 64))
    (:bit 'bit)
    (:t t)))

(defun key-float-p (key)
  (member key '(:single-float :double-float :complex-single-float :complex-double-float)))

(defun data-key (d)
  "Element-type key of a flat data vector, or NIL."
  (typecase d
    ((simple-array double-float (*)) :double-float)
    ((simple-array single-float (*)) :single-float)
    ((simple-array (complex double-float) (*)) :complex-double-float)
    ((simple-array (complex single-float) (*)) :complex-single-float)
    ((simple-array (signed-byte 64) (*)) :signed-byte-64)
    (simple-bit-vector :bit)
    (simple-vector :t)
    (t nil)))

(defun range-result (opname alo ahi blo bhi)
  "The (values lo hi) range of A OPNAME B for integer ranges [ALO,AHI] and [BLO,BHI]."
  (cond ((string= opname "+") (values (+ alo blo) (+ ahi bhi)))
        ((string= opname "-") (values (- alo bhi) (- ahi blo)))
        ((string= opname "*")
         (let ((p (list (* alo blo) (* alo bhi) (* ahi blo) (* ahi bhi))))
           (values (reduce #'min p) (reduce #'max p))))
        (t (error "no integer range for ~S" opname))))

(defun sb64-array-range (data n)
  "(values min max) of the first N elements of the (signed-byte 64) vector DATA (0, 0 when N is 0)."
  (declare (type (simple-array (signed-byte 64) (*)) data) (type fixnum n)
           (optimize (speed 3) (safety 0) (debug 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (zerop n)
      (values 0 0)
      (let ((lo (aref data 0)) (hi (aref data 0)))
        (declare (type (signed-byte 64) lo hi))
        (dotimes (i n)
          (let ((x (aref data i)))
            (declare (type (signed-byte 64) x))
            (when (< x lo) (setq lo x))
            (when (> x hi) (setq hi x))))
        (values lo hi))))

(defun tighten-operand-range (o)
  "When O is an array operand whose range is only that of its element type, (signed-byte 64): replace
the range by that of the elements actually present (one scan of the data) and return true.  The sum,
difference or product of two such arrays then usually fits in (signed-byte 64) and takes the typed
kernel; without the scan, the range of an element type alone can never promise that (the generic
path with a narrowing pass costs 20 times as much).  A range is only a bound: no result depends on
whether it is tight."
  (and (operand-array-p o)
       (eq (operand-base o) :int)
       (eql (operand-lo o) +sb64-min+) (eql (operand-hi o) +sb64-max+)
       (let ((data (flat-data (operand-value o))))
         (and (typep data '(simple-array (signed-byte 64) (*)))
              (multiple-value-bind (lo hi) (sb64-array-range data (array-total-size (operand-value o)))
                (setf (operand-lo o) lo (operand-hi o) hi)
                t)))))

(defun result-plan (opname a b)
  "Return (values KEY LO HI NARROW BASE) for the result of A OPNAME B (OPNAME one of
\"+\" \"-\" \"*\" \"/\").  KEY is the result element-type key; for the integer case LO/HI is
the result range; NARROW is true when the integer result must be computed with generic
arithmetic (possible overflow) and narrowed afterwards; BASE is the result's operand base.
Integer and rational operands of / give rationals: key :T, base :RAT (never narrowed)."
  (let ((base (combine-base (operand-base a) (operand-base b)))
        (cplx (or (operand-cplx a) (operand-cplx b))))
    (case base
      ((:s :d) (values (base-cplx-key base cplx) nil nil nil base))
      (:int
       (if (string= opname "/")
           (values :t nil nil nil :rat)
           (flet ((plan ()
                    (multiple-value-bind (lo hi)
                        (range-result opname (operand-lo a) (operand-hi a) (operand-lo b) (operand-hi b))
                      (if (and (fits-sb64 lo) (fits-sb64 hi)
                               (<= +sb64-min+ (operand-lo a)) (<= (operand-hi a) +sb64-max+)
                               (<= +sb64-min+ (operand-lo b)) (<= (operand-hi b) +sb64-max+))
                          (values :signed-byte-64 lo hi nil :int)
                          (values :t nil nil t :int)))))
             (multiple-value-bind (key lo hi narrow base) (plan)
               ;; the ranges of the element types do not promise a fit: look at the data
               (if (and narrow (let ((ta (tighten-operand-range a)) (tb (tighten-operand-range b))) (or ta tb)))
                   (plan)
                   (values key lo hi narrow base))))))
      (:rat (values :t nil nil nil :rat))
      (t (values :t nil nil nil :gen)))))

;;; --- scalar conversion ---

(defun coerce-scalar (x key)
  (ecase key
    (:single-float (float x 1f0))
    (:double-float (float x 1d0))
    (:complex-single-float
     (complex (float (realpart x) 1f0) (float (imagpart x) 1f0)))
    (:complex-double-float
     (complex (float (realpart x) 1d0) (float (imagpart x) 1d0)))
    ((:signed-byte-64 :t) x)))

;;; --- array conversion ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +int-source-types+
    (remove-duplicates
     (mapcar #'upgraded-array-element-type
             '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 8) (unsigned-byte 16)
               (unsigned-byte 32) (unsigned-byte 62) (unsigned-byte 63) (unsigned-byte 64)
               (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64) fixnum))
     :test #'equal))
  (defparameter +real-source-types+
    (append +int-source-types+ '(single-float double-float)))
  (defparameter +complex-source-types+
    '((complex single-float) (complex double-float))))

(defmacro dispatch-source ((var src types) &body body)
  "Expand BODY once per source vector type in TYPES with VAR declared accordingly,
plus a generic fallback."
  `(typecase ,src
     ,@(loop for ty in (symbol-value types)
             collect `((simple-array ,ty (*))
                       (let ((,var ,src))
                         (declare (type (simple-array ,ty (*)) ,var))
                         ,@body)))
     (t (let ((,var ,src))
          (declare (type (simple-array * (*)) ,var))
          ,@body))))

(defmacro def-converter (name target-type element-form &optional (types '+real-source-types+))
  "Define (NAME src n) -> fresh vector of TARGET-TYPE; ELEMENT-FORM computes the
converted element from X; it may use REAL-P-SOURCE to know whether the source is real."
  `(defun ,name (src n)
     (declare (type fixnum n) (optimize (speed 1) (safety 0)))
     (let ((out (make-array n :element-type ',target-type)))
       (declare (type (simple-array ,target-type (*)) out))
       (dispatch-source (s src ,types)
         (dotimes (i n) (let ((x (aref s i))) (setf (aref out i) ,element-form))))
       out)))

(def-converter convert-real-to-single single-float (float x 1f0))
(def-converter convert-real-to-double double-float (float x 1d0))
(def-converter convert-real-to-csingle (complex single-float) (complex (float x 1f0) 0f0))
(def-converter convert-real-to-cdouble (complex double-float) (complex (float x 1d0) 0d0))
(def-converter convert-real-to-sb64 (signed-byte 64) x +int-source-types+)

(defmacro def-complex-converter (name target-type proto)
  `(defun ,name (src n)
     (declare (type fixnum n) (optimize (speed 1) (safety 0)))
     (let ((out (make-array n :element-type ',target-type)))
       (declare (type (simple-array ,target-type (*)) out))
       (dispatch-source (s src +complex-source-types+)
         (dotimes (i n)
           (let ((x (aref s i)))
             (setf (aref out i) (complex (float (realpart x) ,proto) (float (imagpart x) ,proto))))))
       out)))
(def-complex-converter convert-complex-to-csingle (complex single-float) 1f0)
(def-complex-converter convert-complex-to-cdouble (complex double-float) 1d0)

(defun convert-to-simple-vector (arr src n)
  "Box SRC (flat data of ARR) into a simple-vector, checking that every element is a number."
  (declare (type fixnum n))
  (let ((out (make-array n)))
    (dotimes (i n)
      (let ((x (aref (the (simple-array * (*)) src) i)))
        (unless (numberp x) (not-a-numeric-array arr))
        (setf (svref out i) x)))
    out))

(defun convert-data (arr src n key)
  "Flat vector of element-type KEY holding the contents of SRC (flat data of ARR)."
  ;; a T array (results of / on integers) may hold complex rationals: convert it like a complex array
  (let ((complex-src (typep src '(or (simple-array (complex single-float) (*))
                                     (simple-array (complex double-float) (*))
                                     simple-vector))))
    (ecase key
      (:single-float (convert-real-to-single src n))
      (:double-float (convert-real-to-double src n))
      (:complex-single-float
       (if complex-src (convert-complex-to-csingle src n) (convert-real-to-csingle src n)))
      (:complex-double-float
       (if complex-src (convert-complex-to-cdouble src n) (convert-real-to-cdouble src n)))
      (:signed-byte-64 (convert-real-to-sb64 src n))
      (:t (convert-to-simple-vector arr src n)))))

(defun prepare-data (arr key)
  "Flat data of simple array ARR in representation KEY (no copy when it already matches,
except for T arrays, which are scanned for non-numbers)."
  (let ((src (flat-data arr)))
    (cond ((and (eq (data-key src) key) (not (eq key :t))) src)
          ((and (eq key :t) (simple-vector-p src))
           (dotimes (i (array-total-size arr) src)
             (unless (numberp (svref src i)) (not-a-numeric-array arr))))
          (t (convert-data arr src (array-total-size arr) key)))))

(defun narrow-to-sb64 (arr)
  "ARR is a T array of integers.  Return (values array lo hi): an (signed-byte 64) array
with the same dimensions when every element fits, else ARR itself with NIL bounds."
  (let* ((data (flat-data arr)) (n (array-total-size arr)) (lo 0) (hi 0))
    (declare (type simple-vector data))
    (dotimes (i n)
      (let ((x (svref data i)))
        (unless (fits-sb64 x) (return-from narrow-to-sb64 (values arr nil nil)))
        (when (< x lo) (setf lo x))
        (when (> x hi) (setf hi x))))
    (let* ((r (make-array (array-dimensions arr) :element-type '(signed-byte 64)))
           (rd (flat-data r)))
      (declare (type (simple-array (signed-byte 64) (*)) rd))
      (dotimes (i n) (setf (aref rd i) (svref data i)))
      (values r lo hi))))
