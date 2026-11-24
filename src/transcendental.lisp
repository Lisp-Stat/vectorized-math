;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; VM:EXP VM:EXPT VM:LOG VM:SQRT VM:ISQRT VM:SIN VM:COS VM:TAN VM:ASIN VM:ACOS VM:ATAN
;;; VM:SINH VM:COSH VM:TANH VM:ASINH VM:ACOSH VM:ATANH VM:CIS VM:ABS VM:SIGNUM
;;; VM:CONJUGATE VM:PHASE VM:REALPART VM:IMAGPART VM:COMPLEX.
;;;
;;; Numbers: exactly the CL function.  Arrays: elementwise.
;;;
;;; Element types follow CL per element type:
;;;   float arrays            their float format (complex float arrays: complex of the format);
;;;                           MKL VML (every size) or a typed Lisp loop when MKL is absent
;;;   integer / rational / T  the CL function per element (generic path), results normalised:
;;;                           (irrational functions of rationals give single-floats, as in CL)
;;; Out-of-domain arguments: when CL returns a complex number for a real argument (SQRT of a
;;; negative, LOG of a negative, ASIN of 2, ...) and ANY element of a real float array does so,
;;; the whole result is a complex float array.  A pre-scan of the input decides; it agrees with CL's own
;;; behaviour on the implementation (see DEF-DOMAIN-OP).

(in-package #:vm.impl)

;;; ---------------------------------------------------------------------------
;;; domain support for the functions whose result for a real argument can be complex

(defvar *domain-info* (make-hash-table :test 'equal)
  "(name key) -> plist of the typed domain functions (see DEF-DOMAIN-SUPPORT).")

(defmacro def-domain-support (name fn key type classify-body negate-above-one)
  "Typed helpers of the real function FN on arrays of TYPE (KEY).  CLASSIFY-BODY computes,
from the float X, 0 when CL returns a real number for X, 1 when it certainly returns a
complex one (MKL's complex kernel computes it), 2 when the case is exotic (NaN, Inf, -0.0,
...: the value is taken from CL itself, element by element)."
  (let ((classify (kname 'classify name key)) (scan (kname 'scan name key))
        (zin (kname 'zin name key)) (fix (kname 'fix name key)) (lisp (kname 'lisp-promote name key))
        (arr `(simple-array ,type (*)))
        (carr `(simple-array (complex ,type) (*)))
        (most (if (eq type 'double-float) most-positive-double-float most-positive-single-float)))
    `(progn
       (declaim (inline ,classify))
       (defun ,classify (x)
         (declare (type ,type x) (optimize (speed 3) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (macrolet ((nanp (v) `(nan-p ,v ,',type))
                    (infp (v) `(> (abs ,v) ,',most))
                    (neg0p (v) `(minusp (float-sign ,v)))
                    ;; float constants: comparing a float with the integer 0 or 1 is a
                    ;; generic call in CCL
                    (zero () '(float-zero ,type)) (one () '(float-one ,type)))
           ,classify-body))
       (defun ,scan (a n)
         (declare (type ,arr a) (type fixnum n) (optimize (speed 3) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (let ((flags 0))
           (declare (type fixnum flags))
           (with-nan-scratch
             (dotimes (i n) (setf flags (logior flags (the fixnum (,classify (aref a i)))))))
           flags))
       (defun ,zin (x w n)
         (declare (type ,arr x) (type ,carr w) (type fixnum n)
                  (optimize (speed 3) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (dotimes (i n)
           (let ((v (aref x i)))
             (setf (aref w i)
                   (complex v ,(if negate-above-one
                                   `(if (> v (float-one ,type)) (float-minus-zero ,type) (float-zero ,type))
                                   `(float-zero ,type))))))
         w)
       (defun ,fix (x r w n)
         (declare (type ,arr x r) (type ,carr w) (type fixnum n)
                  (optimize (speed 3) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (with-nan-scratch
           (dotimes (i n)
             (when (zerop (the fixnum (,classify (aref x i))))
               (setf (aref w i) (complex (aref r i) (float-zero ,type))))))
         w)
       (defun ,lisp (x w n)
         (declare (type ,arr x) (type ,carr w) (type fixnum n)
                  (optimize (speed 1) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (dotimes (i n)
           (let ((v (,fn (aref x i))))
             (setf (aref w i) (if (complexp v) v (complex v (float-zero ,type))))))
         w)
       (setf (gethash (list ,name ,key) *domain-info*)
             (list :classify #',classify :scan #',scan :zin #',zin :fix #',fix :lisp #',lisp
                   :fn #',fn)))))

(defmacro def-domain-op (fn mkl classify-body &key negate-above-one)
  "The kernels of FN and its domain support for single and double floats.  NEGATE-ABOVE-ONE:
the complex kernel is given -0.0 as the imaginary part for X > 1 (CL's branch convention)."
  (let ((name (string-downcase (symbol-name fn))))
    `(progn
       (def-transc-kernels ,fn ,mkl)
       ,@(loop for (key type) in +float-types+
               collect `(def-domain-support ,name ,fn ,key ,type ,classify-body ,negate-above-one)))))

;;; Classification: 0 = CL returns a real, 1 = CL returns a complex, 2 = exotic.
;;; These were derived from CCL 1.14 (patched) and SBCL 2.6.2 behaviour; the test suite compares
;;; them with CL element by element on a boundary corpus.
(def-domain-op sqrt "Sqrt" (cond ((nanp x) 2) ((< x (zero)) 1) (t 0)))
(def-domain-op log "Ln"
  (cond ((nanp x) 2)                       ; CCL: complex, SBCL: real
        ((< x (zero)) 1)
        ((and (= x (zero)) (neg0p x)) 2)        ; CCL: complex, SBCL: real -Inf
        (t 0)))
;; (the order of the tests matters: comparisons with NaN must not decide; NaN is exotic because
;; SBCL returns a complex NaN for ASIN of NaN and CCL a real one)
(def-domain-op asin "Asin" (cond ((nanp x) 2) ((<= (abs x) (one)) 0) ((infp x) 2) (t 1))
  :negate-above-one t)
(def-domain-op acos "Acos" (cond ((nanp x) 2) ((<= (abs x) (one)) 0) ((infp x) 2) (t 1))
  :negate-above-one t)
(def-domain-op acosh "Acosh" (cond ((nanp x) 2) ((< x (one)) (if (infp x) 2 1)) (t 0)))
;; CCL's (atanh 2d0) has imaginary part -pi/2, SBCL's +pi/2
(def-domain-op atanh "Atanh" (cond ((nanp x) 2) ((<= (abs x) (one)) 0) ((infp x) 2) (t 1))
  :negate-above-one #+ccl t #-ccl nil)

;;; functions that are real for every real argument
(def-transc-kernels exp "Exp")
(def-transc-kernels sin "Sin")
(def-transc-kernels cos "Cos")
(def-transc-kernels tan "Tan")
(def-transc-kernels atan "Atan")
(def-transc-kernels sinh "Sinh")
(def-transc-kernels cosh "Cosh")
(def-transc-kernels tanh "Tanh")
(def-transc-kernels asinh "Asinh")

;;; --- real SQRT ---
;;; Real single and double SQRT is the Lisp loop on every implementation (see DEF-TRANSC-KERNELS):
;;; CL's SQRT is correctly rounded (hardware sqrt), MKL's v?Sqrt is not (1.3% of double inputs
;;; differ in the last bit).  Complex SQRT, and negative real elements, use MKL's complex kernel.

;;; --- exact operations: Lisp loops (MKL would give the same bits) ---

(defmacro def-exact-kernels (name &rest specs)
  "SPECS: (key in-type out-type expr).  Lisp loops registered under (NAME KEY :U)."
  `(progn
     ,@(loop for (key in-type out-type expr) in specs
             collect `(def-unary-kernel ,name ,key ,in-type ,out-type 0 0 nil ,expr))))

(def-exact-kernels "abs"
  (:single-float single-float single-float (abs x))
  (:double-float double-float double-float (abs x))
  (:signed-byte-64 (signed-byte 64) (signed-byte 64) (the (signed-byte 64) (abs x))))
(def-exact-kernels "signum"
  (:single-float single-float single-float (signum x))
  (:double-float double-float double-float (signum x))
  (:complex-single-float (complex single-float) (complex single-float) (signum x))
  (:complex-double-float (complex double-float) (complex double-float) (signum x))
  (:signed-byte-64 (signed-byte 64) (signed-byte 64) (the (signed-byte 64) (signum x))))
(def-exact-kernels "conjugate"
  (:complex-single-float (complex single-float) (complex single-float) (conjugate x))
  (:complex-double-float (complex double-float) (complex double-float) (conjugate x)))
(def-exact-kernels "realpart"
  (:complex-single-float (complex single-float) single-float (realpart x))
  (:complex-double-float (complex double-float) double-float (realpart x)))
(def-exact-kernels "imagpart"
  (:single-float single-float single-float (imagpart x))
  (:double-float double-float double-float (imagpart x))
  (:complex-single-float (complex single-float) single-float (imagpart x))
  (:complex-double-float (complex double-float) double-float (imagpart x)))
(def-exact-kernels "phase"
  (:single-float single-float single-float (phase x))
  (:double-float double-float double-float (phase x)))

;;; abs and phase of complex arrays, and cis of real arrays: MKL at every size (the Lisp loop
;;; is the fallback when MKL is absent)
(def-unary-kernel "abs" :complex-single-float (complex single-float) single-float 8 4 %vc-abs (abs x))
(def-unary-kernel "abs" :complex-double-float (complex double-float) double-float 16 8 %vz-abs (abs x))
(def-unary-kernel "phase" :complex-single-float (complex single-float) single-float 8 4 %vc-arg (phase x))
(def-unary-kernel "phase" :complex-double-float (complex double-float) double-float 16 8 %vz-arg (phase x))
(def-unary-kernel "cis" :single-float single-float (complex single-float) 4 8 %vc-cis (cis x))
(def-unary-kernel "cis" :double-float double-float (complex double-float) 8 16 %vz-cis (cis x))

;;; ---------------------------------------------------------------------------
;;; binary kernels (:aa :as :sa)

(declaim (inline lisp-pow-d lisp-pow-s))
(defmacro def-lisp-pow (name type)
  `(defun ,name (x y)
     (declare (type ,type x y))
     ;; Real kernel of EXPT for the arrays whose pairs CL returns a real for.  Negative and NaN
     ;; bases never get here in a result (EXPT-FLOAT-ARRAYS patches them from CL) but may be
     ;; computed, so give them a harmless value; zero to a negative power is the IEEE
     ;; infinity instead of CL's error.
     (cond ((or (nan-p x ,type) (< x (float-zero ,type))) (float-nan ,type))
           ((and (= x (float-zero ,type)) (< y (float-zero ,type))) (float-inf ,type))
           (t (let ((v (expt x y))) (if (typep v ',type) v (float-nan ,type)))))))
(def-lisp-pow lisp-pow-d double-float)
(def-lisp-pow lisp-pow-s single-float)

(defun lisp-cpow (x y)
  (handler-case (expt x y)
    (error () (complex (float-nan double-float) (float-nan double-float)))))
(defun lisp-cpow-s (x y)
  (handler-case (expt x y)
    (error () (complex (float-nan single-float) (float-nan single-float)))))

(def-lisp-kernels atan2 :single-float single-float (x y) (atan x y))
(def-lisp-kernels atan2 :double-float double-float (x y) (atan x y))
(def-mkl-kernels atan2 "atan2" :single-float single-float 4 "Atan2" "S" set-single-cell t)
(def-mkl-kernels atan2 "atan2" :double-float double-float 8 "Atan2" "D" set-double-cell t)

;; The Lisp kernels of POW (used when MKL is unavailable) call CL's EXPT per element.  EXPT is a full
;; call, so its float arguments and its result are boxed whatever the kernel does (SBCL reports "float
;; to pointer coercion" notes for that, measured: boxing two doubles is about half of the 22 ns per
;; element on SBCL, and EXPT itself the rest); the notes are muffled.
(def-lisp-kernels pow :single-float single-float (x y) (lisp-pow-s x y) :muffle-notes t)
(def-lisp-kernels pow :double-float double-float (x y) (lisp-pow-d x y) :muffle-notes t)
(def-lisp-kernels pow :complex-single-float (complex single-float) (x y) (lisp-cpow-s x y) :muffle-notes t)
(def-lisp-kernels pow :complex-double-float (complex double-float) (x y) (lisp-cpow x y) :muffle-notes t)
(def-mkl-kernels pow "pow" :single-float single-float 4 "Pow" "S" set-single-cell t)
(def-mkl-kernels pow "pow" :double-float double-float 8 "Pow" "D" set-double-cell t)
(def-mkl-kernels pow "pow" :complex-single-float (complex single-float) 8 "Pow" "C" set-csingle-cell t)
(def-mkl-kernels pow "pow" :complex-double-float (complex double-float) 16 "Pow" "Z" set-cdouble-cell t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bit-test-form (expr)
    "The generalised boolean form equivalent to EXPR, whose value is the bit 1 or 0."
    (if (and (consp expr) (eq (first expr) 'if) (equal (cddr expr) '(1 0)))
        (second expr)
        `(eql 1 ,expr))))

(defmacro fill-out ((i out ooff n out-type) &body body)
  "Store the value of BODY, with I bound to 0 .. N-1, into the elements OOFF + I of the simple
vector OUT of element type OUT-TYPE (a literal type).  When OUT-TYPE is BIT, BODY is a generalised
boolean (true: 1).  For a bit vector on CCL, whose element store is a slow read-modify-write,
whole bytes are assembled and stored when OOFF is a multiple of 8: an optimisation that stores
the same bits."
  (let ((plain (if (eq out-type 'bit)
                   `(setf (aref ,out (+ ,ooff ,i)) (if (progn ,@body) 1 0))
                   `(setf (aref ,out (+ ,ooff ,i)) (progn ,@body)))))
    #+ccl
    (if (eq out-type 'bit)
        `(let ((done 0) (nb (if (zerop (logand ,ooff 7)) (ash ,n -3) 0)))
           (declare (type fixnum done nb))
           (when (plusp nb)
             (ccl:with-pointer-to-ivector (p ,out)
               (let ((byte0 (ash ,ooff -3)))
                 (declare (type fixnum byte0))
                 (dotimes (j nb)
                   (let ((w 0) (base (* j 8)))
                     (declare (type (unsigned-byte 8) w) (type fixnum base))
                     ,@(loop for k below 8
                             collect `(let ((,i (+ base ,k)))
                                        (declare (type fixnum ,i))
                                        (when (progn ,@body) (setf w (logior w ,(ash 1 k))))))
                     (setf (ccl::%get-unsigned-byte p (+ byte0 j)) w)))))
             (setf done (* nb 8)))
           (loop for ,i of-type fixnum from done below ,n do ,plain))
        `(dotimes (,i ,n) ,plain))
    #-ccl `(dotimes (,i ,n) ,plain)))

(defmacro def-mixed-binary-kernels (name key a-type b-type out-type (x y) expr &key (speed 1))
  "Lisp loops (:aa :as :sa) for operands of different element types, registered under (NAME KEY)."
  (let* ((aa (kname 'lisp name key 'aa)) (as (kname 'lisp name key 'as)) (sa (kname 'lisp name key 'sa))
         (ta `(simple-array ,a-type (*))) (tb `(simple-array ,b-type (*))) (to `(simple-array ,out-type (*)))
         (opts `(optimize (speed ,speed) (safety 0) (debug 0)))
         (expr (if (eq out-type 'bit) (bit-test-form expr) expr)))
    `(progn
       (defun ,aa (out ooff a aoff b boff n)
         (declare (type ,to out) (type ,ta a) (type ,tb b) (type fixnum ooff aoff boff n) ,opts
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (with-nan-scratch
           (fill-out (i out ooff n ,out-type)
             (let ((,x (aref a (the fixnum (+ aoff i)))) (,y (aref b (the fixnum (+ boff i)))))
               ,expr)))
         out)
       (defun ,as (out ooff a aoff s n)
         (declare (type ,to out) (type ,ta a) (type ,b-type s) (type fixnum ooff aoff n) ,opts
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (let ((,y s))
           (with-nan-scratch
             (fill-out (i out ooff n ,out-type)
               (let ((,x (aref a (the fixnum (+ aoff i)))))
                 ,expr))))
         out)
       (defun ,sa (out ooff s b boff n)
         (declare (type ,to out) (type ,a-type s) (type ,tb b) (type fixnum ooff boff n) ,opts
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (let ((,x s))
           (with-nan-scratch
             (fill-out (i out ooff n ,out-type)
               (let ((,y (aref b (the fixnum (+ boff i)))))
                 ,expr))))
         out)
       (register-kernel ,name ,key :aa #',aa)
       (register-kernel ,name ,key :as #',as)
       (register-kernel ,name ,key :sa #',sa))))

;; (complex re im) from real float arrays
(def-mixed-binary-kernels "complex" :single-float single-float single-float (complex single-float) (x y)
  (complex x y))
(def-mixed-binary-kernels "complex" :double-float double-float double-float (complex double-float) (x y)
  (complex x y))
;; (expt float-or-complex integer): CL's own algorithm (repeated multiplication) per element
(def-mixed-binary-kernels "expt-int" :single-float single-float (signed-byte 64) single-float (x y)
  (expt x y))
(def-mixed-binary-kernels "expt-int" :double-float double-float (signed-byte 64) double-float (x y)
  (expt x y))
(def-mixed-binary-kernels "expt-int" :complex-single-float (complex single-float) (signed-byte 64)
  (complex single-float) (x y) (expt x y))
(def-mixed-binary-kernels "expt-int" :complex-double-float (complex double-float) (signed-byte 64)
  (complex double-float) (x y) (expt x y))

;;; ---------------------------------------------------------------------------
;;; operator entry point

(defun elementwise-call (cl-fn min max array-fn args &optional (store #'store-into))
  "The body of every operator of this file.  ARGS: numbers give CL-FN's own result and
conditions; with an array the operands (those before a trailing :INTO OUT) go to ARRAY-FN.  STORE
puts the value into an :INTO array (STORE-BITS for operators with bit results)."
  (declare (type function cl-fn array-fn store) (type fixnum min max))
  (if (notany #'arrayp args)
      (apply cl-fn args)
      (let* ((nops (into-position args))
             (ops (if nops (subseq args 0 nops) args))
             (count (length ops)))
        (when (or (< count min) (> count max))
          (return-from elementwise-call (apply cl-fn ops)))   ; CL's own arity error
        (let ((out (and nops (car (last args)))))
          (when (and nops (not (arrayp out))) (error 'type-error :datum out :expected-type 'array))
          (let ((value (if (some #'arrayp ops)
                           ;; an array that cannot hold numbers (a string): signal CL's own error
                           (with-cl-error-replay (cl-fn ops) (apply array-fn ops))
                           (apply cl-fn ops))))
            (if nops (funcall store value out) value))))))

(defmacro define-elementwise-operator (name cl-fn min max array-fn doc)
  `(defun ,name (&rest args)
     ,doc
     (declare (dynamic-extent args))
     (elementwise-call #',cl-fn ,min ,max #',array-fn args)))

;;; ---------------------------------------------------------------------------
;;; unary float arrays

(defun float-operand-key (o)
  (base-cplx-key (operand-base o) (operand-cplx o)))

(defun complex-key-of (key)
  (ecase key
    (:single-float :complex-single-float)
    (:double-float :complex-double-float)))

(defun map-float-array (name in-key out-key o)
  "A fresh array of element type OUT-KEY: kernel (NAME IN-KEY :U) over the data of array operand O."
  (let* ((arr (operand-value o)) (n (array-total-size arr))
         (result (make-result out-key (array-dimensions arr))))
    (when (plusp n)
      (let ((x (prepare-data arr in-key)))
        (with-masked-float-traps
          (funcall (the function (find-kernel name in-key :u)) (flat-data result) 0 x 0 n))))
    result))

(defun exotic-value (fn x)
  "(FN x) as CL computes it, or NIL when CL signals an error."
  (handler-case (funcall fn x) (error () nil)))

(defun domain-slow-path (name fn key dims n x flags)
  "Real array X (flat data of element type KEY) of which the scan found elements that are not
plainly in the real domain (FLAGS: bit 0 some CL returns a complex for, bit 1 exotic ones).
Returns the result: real, or complex when CL returns a complex for any element."
  (let* ((info (gethash (list name key) *domain-info*))
         (classify (getf info :classify))
         (one (if (eq key :single-float) 1f0 1d0))
         (zero (if (eq key :single-float) 0f0 0d0))
         (nan (if (eq key :single-float) *snan* *dnan*))
         (xx x) (exotics '()))
    (declare (type function classify))
    ;; exotic elements: the value is CL's own; the kernels see a harmless 1 instead
    (when (logtest flags 2)
      (setf xx (copy-seq x))
      (dotimes (i n)
        (when (= 2 (the fixnum (funcall classify (aref x i))))
          (push (cons i (exotic-value fn (aref x i))) exotics)
          (setf (aref xx i) one))))
    (let* ((promoted (or (logtest flags 1) (some (lambda (e) (complexp (cdr e))) exotics)))
           (r (make-result key dims)) (rd (flat-data r))
           (need-real (or (not promoted) *mkl-available*)))
      (when need-real
        (funcall (the function (find-kernel name key :u)) rd 0 xx 0 n))
      (if (not promoted)
          (progn (dolist (e exotics) (when (cdr e) (setf (aref rd (car e)) (cdr e))))
                 r)
          (let* ((ckey (complex-key-of key)) (w (make-result ckey dims)) (wd (flat-data w)))
            (if *mkl-available*
                (let ((zin (make-array n :element-type (key-element-type ckey))))
                  (funcall (the function (getf info :zin)) xx zin n)
                  (funcall (the function (find-kernel name ckey :u)) wd 0 zin 0 n)
                  (funcall (the function (getf info :fix)) xx rd wd n))
                (funcall (the function (getf info :lisp)) xx wd n))
            (dolist (e exotics)
              (let ((v (cdr e)) (i (car e)))
                (setf (aref wd i)
                      (cond ((complexp v) v)
                            (v (complex v zero))
                            (t (complex (if need-real (aref rd i) nan) zero))))))
            w)))))

(defun float-transc (name fn o domain-p)
  "NAME's elementwise function on float array operand O (real or complex)."
  (let* ((arr (operand-value o)) (key (float-operand-key o)) (n (array-total-size arr))
         (dims (array-dimensions arr)) (x (prepare-data arr key)))
    (with-masked-float-traps
      (flet ((plain ()
               (let ((r (make-result key dims)))
                 (when (plusp n)
                   (funcall (the function (find-kernel name key :u)) (flat-data r) 0 x 0 n))
                 r)))
        (if (and domain-p (not (operand-cplx o)) (plusp n))
            (let ((flags (funcall (the function
                                       (getf (gethash (list name key) *domain-info*) :scan))
                                  x n)))
              (if (zerop flags)
                  (plain)
                  (domain-slow-path name fn key dims n x flags)))
            (plain))))))

(defun transc-array (name fn domain-p a)
  (let ((o (classify a)))
    (if (member (operand-base o) '(:s :d))
        (float-transc name fn o domain-p)
        (generic-unary fn o :empty-key :single-float))))

(defmacro def-transc-operator (vm-name cl-fn array-name doc &optional domain-p)
  (let ((name (string-downcase (symbol-name cl-fn))))
    `(progn
       (defun ,array-name (a) (transc-array ,name #',cl-fn ,domain-p a))
       (define-elementwise-operator ,vm-name ,cl-fn 1 1 ,array-name ,doc))))

(def-transc-operator vm:exp cl:exp exp-array "e^x; elementwise for arrays.  :INTO OUT as for VM:+.")
(def-transc-operator vm:sin cl:sin sin-array "Sine; elementwise for arrays.")
(def-transc-operator vm:cos cl:cos cos-array "Cosine; elementwise for arrays.")
(def-transc-operator vm:tan cl:tan tan-array "Tangent; elementwise for arrays.")
(def-transc-operator vm:sinh cl:sinh sinh-array "Hyperbolic sine; elementwise for arrays.")
(def-transc-operator vm:cosh cl:cosh cosh-array "Hyperbolic cosine; elementwise for arrays.")
(def-transc-operator vm:tanh cl:tanh tanh-array "Hyperbolic tangent; elementwise for arrays.")
(def-transc-operator vm:asinh cl:asinh asinh-array "Inverse hyperbolic sine; elementwise for arrays.")
(def-transc-operator vm:sqrt cl:sqrt sqrt-array
  "Square root; elementwise for arrays.  When any element of a real float array is negative the
result is a complex array (as CL returns a complex for each such element)." t)
(def-transc-operator vm:asin cl:asin asin-array
  "Arc sine; a complex array when any element of a real float array is outside [-1, 1]." t)
(def-transc-operator vm:acos cl:acos acos-array
  "Arc cosine; a complex array when any element of a real float array is outside [-1, 1]." t)
(def-transc-operator vm:acosh cl:acosh acosh-array
  "Inverse hyperbolic cosine; a complex array when any element of a real float array is below 1." t)
(def-transc-operator vm:atanh cl:atanh atanh-array
  "Inverse hyperbolic tangent; a complex array when any element of a real float array is outside [-1, 1]." t)

;;; ---------------------------------------------------------------------------
;;; abs signum conjugate realpart imagpart phase cis isqrt, one-argument complex

(defun sb64-contains-min-p (data n)
  (declare (type (simple-array (signed-byte 64) (*)) data) (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (dotimes (i n nil)
    (when (= (aref data i) #.(- (expt 2 63))) (return t))))

(defun int-operand-in-sb64-p (o)
  (and (<= +sb64-min+ (operand-lo o)) (<= (operand-hi o) +sb64-max+)))

(defun abs-array (a)
  (let* ((o (classify a)) (base (operand-base o)))
    (case base
      ((:s :d)
       (if (operand-cplx o)
           (map-float-array "abs" (float-operand-key o) (base-cplx-key base nil) o)
           (map-float-array "abs" (float-operand-key o) (float-operand-key o) o)))
      (:int
       (if (and (int-operand-in-sb64-p o)
                (or (< +sb64-min+ (operand-lo o))
                    (let ((arr (operand-value o)))
                      (not (sb64-contains-min-p (prepare-data arr :signed-byte-64)
                                                (array-total-size arr))))))
           (map-float-array "abs" :signed-byte-64 :signed-byte-64 o)
           (generic-unary #'cl:abs o :narrow t)))
      (t (generic-unary #'cl:abs o :empty-key :t)))))

(defun signum-array (a)
  (let* ((o (classify a)) (base (operand-base o)))
    (case base
      ((:s :d) (map-float-array "signum" (float-operand-key o) (float-operand-key o) o))
      (:int (if (int-operand-in-sb64-p o)
                (map-float-array "signum" :signed-byte-64 :signed-byte-64 o)
                (generic-unary #'cl:signum o :narrow t)))
      (t (generic-unary #'cl:signum o :empty-key :t)))))

(defun real-copy-p (o)
  "Is array operand O made of real numbers known from its type (so conjugate and realpart copy)?"
  (and (not (operand-cplx o)) (member (operand-base o) '(:s :d :int :rat))))

(defun conjugate-array (a)
  (let ((o (classify a)))
    (cond ((real-copy-p o) (operand-value (copy-operand-array o)))
          ((and (member (operand-base o) '(:s :d)) (operand-cplx o))
           (map-float-array "conjugate" (float-operand-key o) (float-operand-key o) o))
          (t (generic-unary #'cl:conjugate o :empty-key :t)))))

(defun realpart-array (a)
  (let ((o (classify a)))
    (cond ((real-copy-p o) (operand-value (copy-operand-array o)))
          ((and (member (operand-base o) '(:s :d)) (operand-cplx o))
           (map-float-array "realpart" (float-operand-key o) (base-cplx-key (operand-base o) nil) o))
          (t (generic-unary #'cl:realpart o :empty-key :t)))))

(defun imagpart-array (a)
  (let ((o (classify a)))
    (cond ((member (operand-base o) '(:s :d))
           (map-float-array "imagpart" (float-operand-key o) (base-cplx-key (operand-base o) nil) o))
          ((and (member (operand-base o) '(:int :rat)) (not (operand-cplx o)))
           (make-array (array-dimensions (operand-value o)) :element-type '(signed-byte 64)
                                                            :initial-element 0))
          (t (generic-unary #'cl:imagpart o :empty-key :t)))))

(defun phase-array (a)
  (let* ((o (classify a)) (base (operand-base o)))
    (if (member base '(:s :d))
        (map-float-array "phase" (float-operand-key o) (base-cplx-key base nil) o)
        (generic-unary #'cl:phase o :empty-key :single-float))))

(defun cis-array (a)
  (let* ((o (classify a)) (base (operand-base o)))
    (if (and (member base '(:s :d)) (not (operand-cplx o)))
        (map-float-array "cis" (float-operand-key o) (complex-key-of (float-operand-key o)) o)
        (generic-unary #'cl:cis o :empty-key :complex-single-float))))

(defun isqrt-array (a)
  (generic-unary #'cl:isqrt (classify a) :narrow t :empty-key :signed-byte-64))

(define-elementwise-operator vm:abs cl:abs 1 1 abs-array
  "Absolute value; elementwise for arrays (complex arrays give real arrays of the float format).")
(define-elementwise-operator vm:signum cl:signum 1 1 signum-array "Signum; elementwise for arrays.")
(define-elementwise-operator vm:conjugate cl:conjugate 1 1 conjugate-array
  "Complex conjugate; elementwise for arrays (a real array is copied).")
(define-elementwise-operator vm:realpart cl:realpart 1 1 realpart-array
  "Real part; elementwise for arrays (a real array is copied).")
(define-elementwise-operator vm:imagpart cl:imagpart 1 1 imagpart-array
  "Imaginary part; elementwise for arrays (zeros of CL's type for real arrays).")
(define-elementwise-operator vm:phase cl:phase 1 1 phase-array "Phase angle; elementwise for arrays.")
(define-elementwise-operator vm:cis cl:cis 1 1 cis-array "e^(i x) for real x; elementwise for arrays.")
(define-elementwise-operator vm:isqrt cl:isqrt 1 1 isqrt-array
  "Integer square root; integer arrays only (CL's type error otherwise).")

;;; ---------------------------------------------------------------------------
;;; binary operators

(defun float-binary-key (a b)
  "The common real float key of the real operands A and B when at least one is a float
(rationals are converted, as CL contagion does); otherwise NIL."
  (let ((ba (operand-base a)) (bb (operand-base b)))
    (and (not (operand-cplx a)) (not (operand-cplx b))
         (member ba '(:s :d :int :rat)) (member bb '(:s :d :int :rat))
         (or (member ba '(:s :d)) (member bb '(:s :d)))
         (base-cplx-key (combine-base ba bb) nil))))

(defun shape-of (a b)
  (multiple-value-list (analyze-shape a b)))

(defun atan-array (y &optional (x nil x-p))
  (if (not x-p)
      (transc-array "atan" #'cl:atan nil y)
      (let* ((a (classify y)) (b (classify x)) (key (float-binary-key a b)))
        (if key
            (run-binary a b key key key "atan2" key (shape-of a b))
            (generic-binary #'cl:atan a b (shape-of a b) :empty-key :single-float)))))

(defun complex-array (re &optional (im nil im-p))
  (if (not im-p)
      (let* ((o (classify re)) (base (operand-base o)))
        (cond ((and (member base '(:s :d)) (not (operand-cplx o)))
               (let* ((key (float-operand-key o)) (ckey (complex-key-of key))
                      (arr (operand-value o))
                      (result (make-result ckey (array-dimensions arr))))
                 (replace (flat-data result) (prepare-data arr ckey))
                 result))
              ((real-copy-p o) (operand-value (copy-operand-array o)))
              (t (generic-unary #'cl:complex o :empty-key :t))))
      (let* ((a (classify re)) (b (classify im)) (key (float-binary-key a b)))
        (if key
            (run-binary a b key key (complex-key-of key) "complex" key (shape-of a b))
            (generic-binary #'cl:complex a b (shape-of a b) :empty-key :t)))))

;;; --- the out-of-domain scan of the real EXPT ---

(defun expt-base-scan-needed-p (a key)
  "Does base operand A hold an element for which CL's EXPT of floats may return a complex
(a negative base, or NaN: CCL returns a complex for it)?"
  (let ((v (operand-value a)))
    (flet ((suspicious (x) (or (float-nan-p x) (< x 0))))
      (if (arrayp v)
          (let ((d (prepare-data v key)))
            (with-masked-float-traps
              (dotimes (i (array-total-size v) nil)
                (when (suspicious (aref d i)) (return t)))))
          (with-masked-float-traps (suspicious (coerce-scalar v key)))))))

(defun expt-float-arrays (a b key shape)
  "EXPT of real float operands (data converted to KEY)."
  (let* ((r (run-binary a b key key key "pow" key shape))
         (va (operand-value a)) (vb (operand-value b)))
    (if (not (expt-base-scan-needed-p a key))
        r
        (destructuring-bind (dims n path extra) shape
          (let* ((da (if (arrayp va) (prepare-data va key) (coerce-scalar va key)))
                 (db (if (arrayp vb) (prepare-data vb key) (coerce-scalar vb key)))
                 (ga (if (arrayp va) (lambda (i) (aref da i)) (lambda (i) (declare (ignore i)) da)))
                 (gb (if (arrayp vb) (lambda (i) (aref db i)) (lambda (i) (declare (ignore i)) db)))
                 (idx (make-pair-indexer a b dims n path extra))
                 (rd (flat-data r)) (patches '()))
            (with-masked-float-traps
              (dotimes (i n)
                (multiple-value-bind (ia ib) (funcall idx i)
                  (let ((x (funcall ga ia)))
                    (when (or (float-nan-p x) (< x 0))
                      (let ((v (exotic-value (lambda (u) (expt u (funcall gb ib))) x)))
                        (when v (push (cons i v) patches))))))))
            (if (notany (lambda (p) (complexp (cdr p))) patches)
                (progn (dolist (p patches) (setf (aref rd (car p)) (cdr p))) r)
                (let* ((ckey (complex-key-of key)) (w (make-result ckey dims)) (wd (flat-data w))
                       (zero (if (eq key :single-float) 0f0 0d0)))
                  (dotimes (i n) (setf (aref wd i) (complex (aref rd i) zero)))
                  (dolist (p patches)
                    (let ((v (cdr p))) (setf (aref wd (car p)) (if (complexp v) v (complex v zero)))))
                  w)))))))

(defun expt-array (base power)
  (let* ((a (classify base)) (b (classify power))
         (ba (operand-base a)) (bb (operand-base b)))
    (cond
      ;; float or complex float base, integer exponent: CL's repeated multiplication per element
      ((and (member ba '(:s :d)) (eq bb :int) (int-operand-in-sb64-p b))
       (let ((key (float-operand-key a)))
         (run-binary a b key :signed-byte-64 key "expt-int" key (shape-of a b))))
      ;; float / complex float to a float / complex float power
      ((and (member ba '(:s :d)) (member bb '(:s :d)))
       (let ((fkey (base-cplx-key (combine-base ba bb) nil)))
         (if (or (operand-cplx a) (operand-cplx b))
             (let ((ckey (complex-key-of fkey)))
               (run-binary a b ckey ckey ckey "pow" ckey (shape-of a b)))
             (expt-float-arrays a b fkey (shape-of a b)))))
      (t (generic-binary #'cl:expt a b (shape-of a b) :narrow t :empty-key :single-float)))))

;;; --- two-argument LOG: CL's (/ (log x) (log base)) ---

(defun operand-has-zero-p (o)
  (let ((v (operand-value o)))
    (if (arrayp v)
        (let ((d (operand-data o)))
          (with-masked-float-traps
            (dotimes (i (array-total-size v) nil)
              (when (zerop (aref d i)) (return t)))))
        (zerop v))))

(defun log-operand (o)
  "The operand holding (log x) for the real float operand O."
  (let ((v (operand-value o)))
    (classify (if (arrayp v)
                  (transc-array "log" #'cl:log t v)
                  (call-masked #'cl:log v)))))

(defun log-array (number &optional (base nil base-p))
  (if (not base-p)
      (transc-array "log" #'cl:log t number)
      (let ((a (classify number)) (b (classify base)))
        (if (and (member (operand-base a) '(:s :d)) (member (operand-base b) '(:s :d))
                 (not (operand-cplx a)) (not (operand-cplx b))
                 (not (operand-has-zero-p b)))
            ;; both real floats, no zero base (CL special-cases that): (/ (log x) (log base)),
            ;; each log promoting to complex when out of domain, then the division with contagion
            (operand-value (arith-step "/" (log-operand a) (log-operand b)))
            (generic-binary #'cl:log a b (shape-of a b) :empty-key :single-float)))))

(define-elementwise-operator vm:atan cl:atan 1 2 atan-array
  "Arc tangent of one number, or (atan y x) of two reals; elementwise for arrays.")
(define-elementwise-operator vm:complex cl:complex 1 2 complex-array
  "(complex re) or (complex re im); elementwise for arrays.  Rationals give CL's results: (complex 1 0) is 1.")
(define-elementwise-operator vm:expt cl:expt 2 2 expt-array
  "Power; elementwise for arrays with R's recycling.  A negative real base with a non-integer power gives a complex array.")
(define-elementwise-operator vm:log cl:log 1 2 log-array
  "Natural logarithm, or (log x base); elementwise for arrays.  A complex array when any element of a real float array is negative.")

;;; (The type and class names COMPLEX, FLOAT, RATIONAL and MOD: see wildcard-hook.lisp.)
