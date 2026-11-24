;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Infrastructure shared by the exponential, trigonometric and complex-part operators
;;; (transcendental.lisp):
;;;   * float specials (Inf, NaN) that do not depend on the trap state
;;;   * unary kernels  (out ooff a aoff n)  generated per element type, MKL-backed with a
;;;     Lisp fallback (registered under variant :U, see kernels.lisp)
;;;   * the generic path: CL's own function applied per element, results normalised to the
;;;     tightest array that holds them (R14: real and complex results give a complex array)
;;;   * the pair indexer that maps a result element to the operand elements for the R
;;;     shape rules (used by the generic binary path and the out-of-domain scan of EXPT).

(in-package #:vm.impl)

;;; --- float specials ---

(declaim (notinline opaque-float))
(defun opaque-float (x) x)

(defvar *dinf* 0d0)
(defvar *dnan* 0d0)
(defvar *sinf* 0f0)
(defvar *snan* 0f0)
(declaim (type double-float *dinf* *dnan*) (type single-float *sinf* *snan*))

(defun init-float-specials ()
  (with-masked-float-traps
    (let ((dbig (opaque-float most-positive-double-float))
          (sbig (opaque-float most-positive-single-float)))
      (setf *dinf* (* dbig dbig)
            *dnan* (- *dinf* *dinf*)
            *sinf* (* sbig sbig)
            *snan* (- *sinf* *sinf*)))))
(init-float-specials)

(defmacro float-inf (type) (if (eq type 'double-float) '*dinf* '*sinf*))
(defmacro float-nan (type) (if (eq type 'double-float) '*dnan* '*snan*))
(defmacro float-zero (type) (if (eq type 'double-float) 0d0 0f0))
(defmacro float-minus-zero (type) (if (eq type 'double-float) -0d0 -0f0))
(defmacro float-one (type) (if (eq type 'double-float) 1d0 1f0))

(declaim (inline float-nan-p))
(defun float-nan-p (x)
  "True for a NaN (any float; false for everything else).  Bit level: see NAN-P (ffi.lisp)."
  #+sbcl (and (floatp x) (sb-ext:float-nan-p x))
  #+ccl (typecase x
          (double-float (multiple-value-bind (hi lo) (ccl::double-float-bits x)
                          (let ((h (logand hi #x7fffffff)))
                            (or (> h #x7ff00000) (and (= h #x7ff00000) (/= lo 0))))))
          (single-float (> (logand (ccl::single-float-bits x) #x7fffffff) #x7f800000))
          (t nil))
  #-(or sbcl ccl) (and (floatp x) (/= x (opaque-float x))))

;;; Correctly rounded square root of a float X known to be >= 0 (or a NaN).  SBCL compiles CL:SQRT
;;; of such a value to the hardware instruction.  CCL's SQRT is a boxed full call (about 65-85 ns per
;;; element), so on CCL the C library's sqrt/sqrtf is called instead: an unboxed foreign call (about
;;; 14 ns), and correctly rounded like the hardware instruction (IEEE 754: sqrt is exactly rounded;
;;; glibc and the BSD/macOS libm return the correctly rounded result).  The result is bit-identical
;;; to CL:SQRT, see tests/transcendental.lisp (tc-sqrt-correctly-rounded).
(defmacro hw-sqrt (type x)
  #+ccl (ecase type
          (double-float `(the double-float (cffi:foreign-funcall "sqrt" :double ,x :double)))
          (single-float `(the single-float (cffi:foreign-funcall "sqrtf" :float ,x :float))))
  #-ccl `(real-only ,type (sqrt (the (,type ,(float 0 (if (eq type 'double-float) 1d0 1f0))) ,x))))

(defun domain-bug (value)
  (error "VECTORIZED-MATH internal error: the real-domain scan let a complex value ~S through." value))

(defmacro real-only (type form)
  "FORM's value, which the domain scan guarantees to be a TYPE float."
  `(let ((v ,form))
     (if (typep v ',type) v (domain-bug v))))

;;; --- the four float element types ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (real-key real-type bytes letter complex-key complex-type complex-bytes complex-letter)
  (defparameter +float-types+
    '((:single-float single-float 4 "S" :complex-single-float (complex single-float) 8 "C")
      (:double-float double-float 8 "D" :complex-double-float (complex double-float) 16 "Z")))

  (defun vml-fn (letter mkl-op)
    (intern (format nil "%V~A-~A" letter (string-upcase mkl-op)) '#:vm.impl)))

;;; --- unary kernels ---

(defmacro def-unary-kernel (name key in-type out-type in-bytes out-bytes mkl-fn expr
                            &key (mkl-p t))
  "Define the Lisp loop and (when MKL-FN) the MKL kernel of the unary operation NAME (a
string) on arrays of element-type KEY, and register the MKL kernel (the Lisp loop when there
is none) under (NAME KEY :U).  EXPR computes the element result from X.  The MKL kernel is
used at every size when MKL is available, so a result never depends on the array length."
  (let* ((lisp (kname 'lisp name key 'u)) (mkl (kname 'mkl name key 'u))
         (in `(simple-array ,in-type (*))) (out `(simple-array ,out-type (*))))
    `(progn
       (defun ,lisp (out ooff a aoff n)
         (declare (type ,out out) (type ,in a) (type fixnum ooff aoff n)
                  (optimize (speed 3) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (dotimes (i n)
           (let ((x (aref a (+ aoff i))))
             (setf (aref out (+ ooff i)) ,expr)))
         out)
       ,(if (and mkl-fn mkl-p)
            `(progn
               (defun ,mkl (out ooff a aoff n)
                 (declare (type ,out out) (type ,in a) (type fixnum ooff aoff n))
                 (if *mkl-available*
                     (progn
                       (ensure-vml-mode)
                       (with-pinned-ptr (po out)
                         (with-pinned-ptr (pa a)
                           (loop with done fixnum = 0
                                 while (< done n)
                                 do (let ((m (min (- n done) +mkl-chunk+)))
                                      (,mkl-fn m
                                               (ptr+ pa (* ,in-bytes (+ aoff done)))
                                               (ptr+ po (* ,out-bytes (+ ooff done))))
                                      (incf done m)))))
                       out)
                     (,lisp out ooff a aoff n)))
               (register-kernel ,name ,key :u #',mkl))
            `(register-kernel ,name ,key :u #',lisp)))))

(defmacro def-transc-kernels (fn mkl)
  "Kernels of the real function FN: real and complex float arrays, single and double."
  (let ((name (string-downcase (symbol-name fn))))
    `(progn
       ,@(loop for (key type bytes letter ckey ctype cbytes cletter) in +float-types+
               collect `(def-unary-kernel ,name ,key ,type ,type ,bytes ,bytes
                          ;; real SQRT is always the Lisp loop: CL's SQRT is correctly rounded,
                          ;; MKL's v?Sqrt is not
                          ,(if (eq fn 'sqrt) nil (vml-fn letter mkl))
                          ,(if (eq fn 'sqrt)
                               ;; negative: the kernel result is not used (the complex kernel
                               ;; computes it), but a harmless value must come out
                               `(if (< x (float-zero ,type))
                                    (float-nan ,type)
                                    (hw-sqrt ,type x))
                               `(real-only ,type (,fn x))))
               collect `(def-unary-kernel ,name ,ckey ,ctype ,ctype ,cbytes ,cbytes
                          ,(vml-fn cletter mkl) (,fn x))))))

;;; --- generic path ---

(defun call-masked (fn &rest args)
  (declare (dynamic-extent args))
  (with-masked-float-traps (apply fn args)))

(defun make-t-array (dims)
  (make-array dims))

(defun normalize-results (tmp dims narrow-p empty-key)
  "TMP is a simple-vector of results (CL numbers), DIMS the result dimensions.  The tightest
array: all single-floats (some complex: complex single-float), likewise double; all
integers (a (signed-byte 64) array when NARROW-P and every one fits); otherwise element
type T.  Real and complex results of the same float format give a complex array (R14).
An empty result has element type EMPTY-KEY."
  (declare (type simple-vector tmp))
  (let ((n (length tmp)) (kinds 0))
    (declare (type fixnum n kinds))
    (dotimes (i n)
      (let ((v (svref tmp i)))
        (setf kinds (logior kinds
                            (typecase v
                              (single-float 1)
                              (double-float 2)
                              ((complex single-float) 4)
                              ((complex double-float) 8)
                              (integer 16)
                              (t 32))))))
    (flet ((fill-array (key)
             (let* ((result (make-result key dims)) (data (flat-data result)))
               (if (key-float-p key)
                   (if (member key '(:complex-single-float :complex-double-float))
                       (dotimes (i n)
                         (let ((v (svref tmp i)))
                           (setf (aref data i) (if (complexp v) v (complex v (float 0 v))))))
                       (dotimes (i n) (setf (aref data i) (svref tmp i))))
                   (dotimes (i n) (setf (aref data i) (svref tmp i))))
               result)))
      (cond
        ((zerop n) (make-result empty-key dims))
        ((= kinds 16)
         (if (and narrow-p (every #'fits-sb64 tmp))
             (fill-array :signed-byte-64)
             (fill-array :t)))
        ((zerop (logand kinds (lognot 5)))
         (fill-array (if (logtest kinds 4) :complex-single-float :single-float)))
        ((zerop (logand kinds (lognot 10)))
         (fill-array (if (logtest kinds 8) :complex-double-float :double-float)))
        (t (fill-array :t))))))

(defun operand-data (o)
  "The flat data vector of array operand O."
  (flat-data (operand-value o)))

(defun generic-unary (fn o &key narrow empty-key)
  "FN (a CL function of one argument) applied to every element of array operand O, with
float traps masked; the result is normalised (NORMALIZE-RESULTS).  NARROW: integer results
of an integer array may be narrowed to (signed-byte 64)."
  (let* ((arr (operand-value o)) (n (array-total-size arr)) (data (flat-data arr))
         (tmp (make-array n)))
    (with-masked-float-traps
      (dotimes (i n) (setf (svref tmp i) (funcall fn (aref data i)))))
    (normalize-results tmp (array-dimensions arr)
                       (and narrow (eq (operand-base o) :int))
                       (or empty-key :t))))

;;; --- pair indexing for the R shape rules ---

(defun make-pair-indexer (a b dims n path extra)
  "A function of the row-major result index R returning (values IA IB), the flat indices of
the elements of operands A and B that are combined into element R, following the paths of
ANALYZE-SHAPE (the same as BINARY-ARRAYS uses)."
  (let ((sa (if (operand-array-p a) (operand-size a) 1))
        (sb (if (operand-array-p b) (operand-size b) 1)))
    (ecase path
      (:empty (lambda (r) (declare (ignore r)) (values 0 0)))
      ((:scalar-a :vec-scalar-a) (lambda (r) (values 0 r)))
      ((:scalar-b :vec-scalar-b) (lambda (r) (values r 0)))
      (:flat (lambda (r) (values r r)))
      ((:blocks :cyclic)
       (let ((m (min sa sb)))
         (if extra
             (lambda (r) (values r (mod r m)))
             (lambda (r) (values (mod r m) r)))))
      (:rows
       (let* ((d0 (first dims)) (blk (floor n d0)))
         (if extra
             (lambda (r) (values r (floor r blk)))
             (lambda (r) (values (floor r blk) r)))))
      (:colmajor
       (let ((m (if extra sb sa)))
         (if extra
             (lambda (r) (values r (mod (colmajor-order-index dims r) m)))
             (lambda (r) (values (mod (colmajor-order-index dims r) m) r))))))))

(defun operand-accessor (o)
  "A function of a flat index returning the element of operand O (a scalar's value for any index)."
  (if (operand-array-p o)
      (let ((d (operand-data o))) (lambda (i) (aref d i)))
      (let ((v (operand-value o))) (lambda (i) (declare (ignore i)) v))))

(defun generic-binary (fn a b shape &key narrow empty-key)
  "FN applied to the pairs of elements of operands A and B combined according to SHAPE (the
values of ANALYZE-SHAPE as a list); normalised as GENERIC-UNARY."
  (destructuring-bind (dims n path extra) shape
    (let ((tmp (make-array n)) (ga (operand-accessor a)) (gb (operand-accessor b)))
      (unless (eq path :empty)
        (let ((idx (make-pair-indexer a b dims n path extra)))
          (with-masked-float-traps
            (dotimes (r n)
              (multiple-value-bind (ia ib) (funcall idx r)
                (setf (svref tmp r) (funcall fn (funcall ga ia) (funcall gb ib))))))))
      (normalize-results tmp dims
                         (and narrow (eq (operand-base a) :int) (eq (operand-base b) :int))
                         (or empty-key :t)))))
