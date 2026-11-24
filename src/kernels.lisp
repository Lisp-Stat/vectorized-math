;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Kernel registry and kernels.  A kernel is registered under
;;;   (operation  element-type-key  variant)
;;; where element-type-key is one of :single-float :double-float :complex-single-float
;;; :complex-double-float :signed-byte-64 :t and variant is
;;;   :aa  (out ooff a aoff b boff n)   array op array
;;;   :as  (out ooff a aoff s n)        array op scalar
;;;   :sa  (out ooff s b boff n)        scalar op array
;;; There are also unary kernels, variant :u  (out ooff a aoff n), registered under the
;;; operation name "neg" (negation; reciprocal is the :sa kernel of "/" with scalar 1).
;;; All arrays are flat simple vectors of the element type; OOFF/AOFF/BOFF are element
;;; offsets; N elements are processed.  Replace an entry with REGISTER-KERNEL to
;;; plug in a different implementation (e.g. custom integer kernels).

(in-package #:vm.impl)

(defvar *kernels* (make-hash-table :test 'equal)
  "Operation name -> list of ((key . variant) . function).  One hash lookup on the (short) name and a
scan of the dozen entries of the operation: the lookup runs for every array operation, and an EQUAL
table keyed by the list (name key variant) made it a third of the time of a small one.")

(defun op-name (op) (if (stringp op) op (symbol-name op)))

(defun register-kernel (op key variant function)
  "Register FUNCTION as the kernel for OP / element type KEY / VARIANT (:aa :as :sa)."
  (let* ((name (op-name op))
         (entry (find-if (lambda (e) (and (eq (caar e) key) (eq (cdar e) variant)))
                         (gethash name *kernels*))))
    (if entry
        (setf (cdr entry) function)
        (push (cons (cons key variant) function) (gethash name *kernels*)))
    function))

(defun find-kernel (op key variant)
  (dolist (e (gethash (op-name op) *kernels*) (error "No kernel registered for ~S ~S ~S." op key variant))
    (when (and (eq (caar e) key) (eq (cdar e) variant))
      (return (cdr e)))))

;;; --- cutoffs ---
;;; Below the cutoff element count the Lisp loop is used, from it on MKL.  This is only
;;; allowed where both give bit-identical results (real + - * /, complex + -); the complex
;;; * and / kernels use MKL at every size when MKL is available (see DEF-MKL-KERNELS).

(defvar *cutoffs* (make-hash-table :test 'equal))

(defun default-cutoff (name)
  "Measured crossover (bench/cutoff.lisp): element count at which MKL beats the Lisp loop for real
arrays, per operation and implementation (the larger of single and double, rounded up to the grid of the
sweep).  CCL: the Lisp loops are slow (boxed arithmetic), MKL wins from about 100 elements.  SBCL: its
loops are fast, MKL wins from 200-400 elements for + - *, from about 50 for /."
  (cond ((string= name "/") #+ccl 192 #+sbcl 64 #-(or ccl sbcl) 256)
        (t #+ccl 128 #+sbcl 256 #-(or ccl sbcl) 256)))

(defun cutoff-cell (op)
  (let ((name (op-name op)))
    (or (gethash name *cutoffs*)
        (setf (gethash name *cutoffs*) (list (default-cutoff name))))))

(defun cutoff (op)
  "Element count below which OP (+ - * /) uses the Lisp loop instead of MKL for real and
complex + -.  SETF-able.  (SBCL: complex + - use MKL from 128 times that many elements, 32768 by
default, because SBCL's Lisp loops for complex arrays stay faster than MKL's memory-bound complex add
until then; a cutoff of 0 is MKL for every size, as for real arrays.)  Complex * and / ignore it: they
use MKL at every size (when MKL is available) because MKL and Lisp results differ in the last bits,
and a result must not depend on the array length."
  (car (cutoff-cell op)))

(defun (setf cutoff) (value op)
  (check-type value (integer 0))
  (setf (car (cutoff-cell op)) value))

;;; --- Lisp loops ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kname (&rest parts)
    (intern (format nil "~{~A~^-~}" parts) '#:vm.impl))
  (defun mkl-fn-name (letter mkl-op inc)
    (intern (format nil "%V~A-~A~:[~;-I~]" letter (string-upcase mkl-op) inc) '#:vm.impl)))

(defmacro def-lisp-kernels (opname key vtype (x y) expr &key muffle-notes)
  "Define typed Lisp loops for OPNAME on arrays of VTYPE; EXPR computes the result from X and Y.
MUFFLE-NOTES: do not report SBCL compiler notes (for an EXPR that calls a CL function that cannot be
inlined, so that boxing floats for the call is inherent)."
  (let* ((arr (if (eq vtype :t) 'simple-vector `(simple-array ,vtype (*))))
         (sty (if (eq vtype :t) 't vtype))
         (aa (kname 'lisp opname key 'aa))
         (as (kname 'lisp opname key 'as))
         (sa (kname 'lisp opname key 'sa))
         ;; the generic (T) kernels cannot do better than generic arithmetic: no SBCL notes
         (opts (if (eq vtype :t)
                   '(optimize (speed 1) (safety 0) (debug 0))
                   '(optimize (speed 3) (safety 0) (debug 0))))
         (muffle (if muffle-notes '(#+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)) '())))
    `(progn
       (defun ,aa (out ooff a aoff b boff n)
         (declare (type ,arr out a b) (type fixnum ooff aoff boff n) ,opts ,@muffle)
         (with-nan-scratch
           (dotimes (i n)
             (let ((,x (aref a (+ aoff i))) (,y (aref b (+ boff i))))
               (setf (aref out (+ ooff i)) ,expr))))
         out)
       (defun ,as (out ooff a aoff s n)
         (declare (type ,arr out a) (type ,sty s) (type fixnum ooff aoff n) ,opts ,@muffle)
         (let ((,y s))
           (with-nan-scratch
             (dotimes (i n)
               (let ((,x (aref a (+ aoff i))))
                 (setf (aref out (+ ooff i)) ,expr)))))
         out)
       (defun ,sa (out ooff s b boff n)
         (declare (type ,arr out b) (type ,sty s) (type fixnum ooff boff n) ,opts ,@muffle)
         (let ((,x s))
           (with-nan-scratch
             (dotimes (i n)
               (let ((,y (aref b (+ boff i))))
                 (setf (aref out (+ ooff i)) ,expr)))))
         out))))

;;; --- MKL kernels ---

(defconstant +mkl-chunk+ (expt 2 30) "Largest element count handed to one VML call.")

(defmacro def-mkl-kernels (opname regname key vtype bytes mkl-op letter scalar-setter always
                           &optional (shift 0))
  "Kernels that call MKL for N >= cutoff * 2^SHIFT and otherwise the Lisp loop (ALWAYS true: MKL for
every N when MKL is available).  SCALAR-SETTER stores a scalar into a foreign cell."
  (let ((aa (kname 'mkl opname key 'aa)) (as (kname 'mkl opname key 'as)) (sa (kname 'mkl opname key 'sa))
        (laa (kname 'lisp opname key 'aa)) (las (kname 'lisp opname key 'as)) (lsa (kname 'lisp opname key 'sa))
        (cfun (mkl-fn-name letter mkl-op nil)) (cfun-i (mkl-fn-name letter mkl-op t))
        (arr `(simple-array ,vtype (*)))
        (use `(and *mkl-available* ,(if always t `(>= (ash n ,(- shift)) (the fixnum (car cell)))))))
    `(progn
       (let ((cell (cutoff-cell ,regname)))
         (declare (ignorable cell))
         (defun ,aa (out ooff a aoff b boff n)
           (declare (type ,arr out a b) (type fixnum ooff aoff boff n))
           (if ,use
               (progn
                 (ensure-vml-mode)
                 (with-pinned-ptr (po out)
                   (with-pinned-ptr (pa a)
                     (with-pinned-ptr (pb b)
                       (loop with done fixnum = 0
                             while (< done n)
                             do (let ((m (min (- n done) +mkl-chunk+)))
                                  (,cfun m
                                         (ptr+ pa (* ,bytes (+ aoff done)))
                                         (ptr+ pb (* ,bytes (+ boff done)))
                                         (ptr+ po (* ,bytes (+ ooff done))))
                                  (incf done m))))))
                 out)
               (,laa out ooff a aoff b boff n)))
         (defun ,as (out ooff a aoff s n)
           (declare (type ,arr out a) (type fixnum ooff aoff n))
           (if ,use
               (progn
                 (ensure-vml-mode)
                 (cffi:with-foreign-object (ps :double 2)
                   (,scalar-setter ps s)
                   (with-pinned-ptr (po out)
                     (with-pinned-ptr (pa a)
                       (loop with done fixnum = 0
                             while (< done n)
                             do (let ((m (min (- n done) +mkl-chunk+)))
                                  (,cfun-i m
                                           (ptr+ pa (* ,bytes (+ aoff done))) 1
                                           ps 0
                                           (ptr+ po (* ,bytes (+ ooff done))) 1)
                                  (incf done m))))))
                 out)
               (,las out ooff a aoff s n)))
         (defun ,sa (out ooff s b boff n)
           (declare (type ,arr out b) (type fixnum ooff boff n))
           (if ,use
               (progn
                 (ensure-vml-mode)
                 (cffi:with-foreign-object (ps :double 2)
                   (,scalar-setter ps s)
                   (with-pinned-ptr (po out)
                     (with-pinned-ptr (pb b)
                       (loop with done fixnum = 0
                             while (< done n)
                             do (let ((m (min (- n done) +mkl-chunk+)))
                                  (,cfun-i m
                                           ps 0
                                           (ptr+ pb (* ,bytes (+ boff done))) 1
                                           (ptr+ po (* ,bytes (+ ooff done))) 1)
                                  (incf done m))))))
                 out)
               (,lsa out ooff s b boff n))))
       (register-kernel ,regname ,key :aa #',aa)
       (register-kernel ,regname ,key :as #',as)
       (register-kernel ,regname ,key :sa #',sa))))

(defun set-single-cell (p v) (setf (cffi:mem-ref p :float) v))
(defun set-double-cell (p v) (setf (cffi:mem-ref p :double) v))
(defun set-csingle-cell (p v)
  (setf (cffi:mem-aref p :float 0) (realpart v) (cffi:mem-aref p :float 1) (imagpart v)))
(defun set-cdouble-cell (p v)
  (setf (cffi:mem-aref p :double 0) (realpart v) (cffi:mem-aref p :double 1) (imagpart v)))

(defmacro def-op-kernels (opname regname mkl-op (x y) expr &key sb64 complex-always-mkl)
  "All kernels of the binary operation OPNAME (registered as REGNAME): MKL-backed float and
complex-float kernels, a typed (signed-byte 64) loop when SB64, and a generic loop for T.
EXPR computes the element result from X and Y.  COMPLEX-ALWAYS-MKL: the complex kernels use
MKL for every size (results of MKL and Lisp differ, so they must not depend on the size)."
  `(progn
     (def-lisp-kernels ,opname :single-float single-float (,x ,y) ,expr)
     (def-lisp-kernels ,opname :double-float double-float (,x ,y) ,expr)
     (def-lisp-kernels ,opname :complex-single-float (complex single-float) (,x ,y) ,expr)
     (def-lisp-kernels ,opname :complex-double-float (complex double-float) (,x ,y) ,expr)
     ,@(when sb64
         `((def-lisp-kernels ,opname :signed-byte-64 (signed-byte 64) (,x ,y) (the (signed-byte 64) ,expr))))
     (def-lisp-kernels ,opname :t :t (,x ,y) ,expr)
     (def-mkl-kernels ,opname ,regname :single-float single-float 4 ,mkl-op "S" set-single-cell nil)
     (def-mkl-kernels ,opname ,regname :double-float double-float 8 ,mkl-op "D" set-double-cell nil)
     (def-mkl-kernels ,opname ,regname :complex-single-float (complex single-float) 8 ,mkl-op "C"
       set-csingle-cell ,complex-always-mkl #+sbcl 7 #-sbcl 0)
     (def-mkl-kernels ,opname ,regname :complex-double-float (complex double-float) 16 ,mkl-op "Z"
       set-cdouble-cell ,complex-always-mkl #+sbcl 7 #-sbcl 0)
     ;; Integer and generic kernels: plain Lisp loops (replaceable via REGISTER-KERNEL).
     (dolist (key ',(if sb64 '(:signed-byte-64 :t) '(:t)))
       (register-kernel ,regname key :aa (symbol-function (kname 'lisp ',opname key 'aa)))
       (register-kernel ,regname key :as (symbol-function (kname 'lisp ',opname key 'as)))
       (register-kernel ,regname key :sa (symbol-function (kname 'lisp ',opname key 'sa))))))

(def-op-kernels add "+" "Add" (x y) (+ x y) :sb64 t)
(def-op-kernels sub "-" "Sub" (x y) (- x y) :sb64 t)
(def-op-kernels mul "*" "Mul" (x y) (* x y) :sb64 t :complex-always-mkl t)
;; integer / rational operands give rationals (key :T), so there is no (signed-byte 64) kernel
(def-op-kernels div "/" "Div" (x y) (/ x y) :complex-always-mkl t)

;;; --- unary kernels (negation); variant :u  (out ooff a aoff n) ---
;;; Plain loops: negation is memory bound and must flip the sign of zeros, which a
;;; subtraction from 0 would not.

(defmacro def-neg-kernels (&rest keys)
  `(progn
     ,@(loop for (key vtype) in keys
             collect
             (let* ((arr (if (eq vtype :t) 'simple-vector `(simple-array ,vtype (*))))
                    (name (kname 'lisp 'neg key 'u))
                    (expr (if (equal vtype '(signed-byte 64)) '(the (signed-byte 64) (- x)) '(- x)))
                    (opts (if (eq vtype :t)
                              '(optimize (speed 1) (safety 0) (debug 0))
                              '(optimize (speed 3) (safety 0) (debug 0)))))
               `(progn
                  (defun ,name (out ooff a aoff n)
                    (declare (type ,arr out a) (type fixnum ooff aoff n) ,opts)
                    (dotimes (i n)
                      (let ((x (aref a (+ aoff i))))
                        (setf (aref out (+ ooff i)) ,expr)))
                    out)
                  (register-kernel "neg" ,key :u #',name))))))

(def-neg-kernels (:single-float single-float) (:double-float double-float)
  (:complex-single-float (complex single-float)) (:complex-double-float (complex double-float))
  (:signed-byte-64 (signed-byte 64)) (:t :t))
