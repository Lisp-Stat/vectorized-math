;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; Integer, bitwise and conversion operators.
;;;
;;;   VM:LOGAND VM:LOGIOR VM:LOGXOR VM:LOGEQV           n-ary, integer arrays
;;;   VM:LOGNAND VM:LOGNOR VM:LOGANDC1 VM:LOGANDC2 VM:LOGORC1 VM:LOGORC2     binary
;;;   VM:LOGNOT VM:LOGCOUNT VM:INTEGER-LENGTH           unary
;;;   VM:ASH                                            integer, shift count
;;;   VM:LOGBITP VM:LOGTEST                             bit-array results (R15)
;;;   VM:FLOAT VM:SCALE-FLOAT VM:FLOAT-SIGN             float arrays
;;;   VM:RATIONAL VM:RATIONALIZE VM:NUMERATOR VM:DENOMINATOR     (signed-byte 64) or T arrays
;;;
;;; Numbers: exactly CL (the CL function is applied).  Arrays: every element is whatever CL's own
;;; function returns for the element(s), shapes by the R rules (shape.lisp).  Two paths give it:
;;;   * typed kernels, used when every operand is an array (or a scalar) of integers within
;;;     (signed-byte 64) or of single/double floats as the operator needs, and the kernel can compute
;;;     the element exactly as CL does; a kernel that meets an element it does not handle (an ASH
;;;     that overflows, a negative LOGBITP index) reports so and the call is redone on
;;;   * the generic path: the CL function per element on the boxed elements.
;;; So the result never depends on the path or the array length.  There is no MKL path.
;;;
;;; Result element types are the tightest the values allow (NORMALIZE-RESULTS): integers give
;;; (signed-byte 64) when every value fits, else T; floats give single-float or double-float; a mixed
;;; or exotic result gives T.  An empty result is (signed-byte 64) for integer results, bit for
;;; predicates, and the float format of the (first) float operand, or single-float, for float results.
;;;
;;; Float traps: FLOAT, SCALE-FLOAT and FLOAT-SIGN on arrays run with the traps masked (R13), the
;;; others (no float arithmetic) in the caller's trap state, so CL's function sees them as CL does.

(in-package #:vm.impl)

;;; ---------------------------------------------------------------------------
;;; operand checks

(defun classify-int-ops (cl-fn ops)
  "Classify the operands OPS of CL-FN.  A scalar that is not a number gets CL's own error."
  (dolist (x ops)
    (unless (or (arrayp x) (numberp x))
      (replay-scalar-error cl-fn ops)
      (not-a-number x)))
  (mapcar #'classify ops))

(defun sb64-int-operand-p (o)
  "True when operand O is an integer scalar or array whose values are all within (signed-byte 64)."
  (and (eq (operand-base o) :int)
       (not (operand-cplx o))
       (or (and (fits-sb64 (operand-lo o)) (fits-sb64 (operand-hi o)))
           (and (operand-array-p o)
                (multiple-value-bind (lo hi) (scan-int-range (operand-value o))
                  (and (fits-sb64 lo) (fits-sb64 hi)))))))

(defun real-float-operand-p (o)
  "True when operand O is a single or double float scalar or array (not complex)."
  (and (member (operand-base o) '(:s :d)) (not (operand-cplx o))))

(defun operand-float-key (o) (base-cplx-key (operand-base o) nil))

(defun operand-int-key (o)
  "The data key for the integer or float operand O accepted by the typed kernels."
  (if (eq (operand-base o) :int) :signed-byte-64 (operand-float-key o)))

;;; ---------------------------------------------------------------------------
;;; the generic path: CL's function per element

(defun generic-map1 (fn o &key empty-key masked)
  "FN applied to every element of array operand O; the results normalised (integer results are
narrowed to (signed-byte 64) whenever they fit).  The float traps are masked during the calls when
MASKED."
  (let* ((arr (operand-value o)) (n (array-total-size arr)) (data (flat-data arr))
         (tmp (make-array n)))
    (flet ((run () (dotimes (i n) (setf (svref tmp i) (funcall fn (aref data i))))))
      (if masked (with-masked-float-traps (run)) (run)))
    (normalize-results tmp (array-dimensions arr) t (or empty-key :t))))

(defun generic-map2 (fn a b shape &key empty-key masked)
  "FN applied to the pairs of elements of operands A and B combined according to SHAPE; as GENERIC-MAP1."
  (destructuring-bind (dims n path extra) shape
    (let ((tmp (make-array n)) (ga (operand-accessor a)) (gb (operand-accessor b)))
      (unless (eq path :empty)
        (let ((idx (make-pair-indexer a b dims n path extra)))
          (flet ((run ()
                   (dotimes (r n)
                     (multiple-value-bind (ia ib) (funcall idx r)
                       (setf (svref tmp r) (funcall fn (funcall ga ia) (funcall gb ib)))))))
            (if masked (with-masked-float-traps (run)) (run)))))
      (normalize-results tmp dims t (or empty-key :t)))))

(defun generic-bits2 (fn a b shape)
  "A bit array: 1 where the generalised boolean (FN x y) of the paired elements is true."
  (destructuring-bind (dims n path extra) shape
    (let ((result (make-result :bit dims)))
      (unless (eq path :empty)
        (let ((idx (make-pair-indexer a b dims n path extra))
              (ga (operand-accessor a)) (gb (operand-accessor b))
              (rd (flat-data result)))
          (declare (type (simple-array bit (*)) rd))
          (dotimes (r n)
            (multiple-value-bind (ia ib) (funcall idx r)
              (setf (aref rd r) (if (funcall fn (funcall ga ia) (funcall gb ib)) 1 0))))))
      result)))

;;; ---------------------------------------------------------------------------
;;; kernels on (signed-byte 64): two operands
;;;
;;; Registered under (NAME :signed-byte-64 :aa/:as/:sa) with the signature of the two-output kernels of
;;; division.lisp, (qout rout ooff a aoff b boff n), QOUT unused (NIL); returning true when every
;;; element was processed, NIL as soon as one is not handled (the output is then garbage).

(defmacro def-sb64-pair-kernels (name out-type (x y) expr &key (check t))
  "Kernels of the operation NAME on (signed-byte 64) operands, element type OUT-TYPE
((signed-byte 64) or BIT).  EXPR computes the element from X and Y; a false CHECK ends the kernel
with NIL."
  (let* ((aa (kname 'int name 'aa)) (as (kname 'int name 'as)) (sa (kname 'int name 'sa))
         (body `((unless ,check (return-from kernel nil))
                 (setf (aref rout (the fixnum (+ ooff i)))
                       ,(if (eq out-type 'bit) `(if ,expr 1 0) `(the (signed-byte 64) ,expr)))))
         (decls `((type (simple-array ,out-type (*)) rout) (ignore qout) (type fixnum ooff n)
                  (optimize (speed 3) (safety 0) (debug 0))      ; speed 3: CCL open-codes more (7.4 -> 4.1 ns for logand)
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)))
         (arr '(simple-array (signed-byte 64) (*))))
    `(progn
       (defun ,aa (qout rout ooff a aoff b boff n)
         (declare ,@decls (type ,arr a b) (type fixnum aoff boff))
         (block kernel
           (dotimes (i n)
             (let ((,x (aref a (the fixnum (+ aoff i)))) (,y (aref b (the fixnum (+ boff i)))))
               ,@body))
           t))
       (defun ,as (qout rout ooff a aoff s n)
         (declare ,@decls (type ,arr a) (type (signed-byte 64) s) (type fixnum aoff))
         (block kernel
           (let ((,y s))
             (dotimes (i n)
               (let ((,x (aref a (the fixnum (+ aoff i)))))
                 ,@body)))
           t))
       (defun ,sa (qout rout ooff s b boff n)
         (declare ,@decls (type ,arr b) (type (signed-byte 64) s) (type fixnum boff))
         (block kernel
           (let ((,x s))
             (dotimes (i n)
               (let ((,y (aref b (the fixnum (+ boff i)))))
                 ,@body)))
           t))
       (register-kernel ,name :signed-byte-64 :aa #',aa)
       (register-kernel ,name :signed-byte-64 :as #',as)
       (register-kernel ,name :signed-byte-64 :sa #',sa))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bitwise-ops+
    '(("logand" cl:logand) ("logior" cl:logior) ("logxor" cl:logxor) ("logeqv" cl:logeqv)
      ("lognand" cl:lognand) ("lognor" cl:lognor) ("logandc1" cl:logandc1) ("logandc2" cl:logandc2)
      ("logorc1" cl:logorc1) ("logorc2" cl:logorc2))))

;; The bitwise operations are exactly defined on two's complement integers, and the result of two
;; (signed-byte 64) values is one again.
(macrolet ((def-all ()
             `(progn
                ,@(loop for (name fn) in +bitwise-ops+
                        collect `(def-sb64-pair-kernels ,name (signed-byte 64) (x y) (,fn x y))))))
  (def-all))

;; ASH: right shifts always fit; a shift count of -64 or less gives 0 or -1 as -64 does.  A left shift
;; is done only when the result fits in 64 bits (integer-length x + n <= 63, or x = 0).
(def-sb64-pair-kernels "ash" (signed-byte 64) (x y)
  (if (< y 0)
      (ash x (the (integer -64 -1) (max y -64)))
      (ash x (the (integer 0 63) (min y 63))))
  :check (or (< y 0) (= x 0) (and (< y 64) (<= (+ (integer-length x) y) 63))))

;; ASH of an array by one scalar (the usual call): the shift count is the same for every element, so the
;; per-element test of the general kernel (integer-length x + n <= 63) becomes two comparisons with bounds
;; computed once: for 0 <= n < 64 the shifted value fits exactly when -2^(63-n) <= x < 2^(63-n); for n >= 64
;; only x = 0 fits.  Same elements, same results as the general kernel (which it replaces for this variant).
(defun int-ash-as (qout rout ooff a aoff s n)
  (declare (ignore qout)
           (type (simple-array (signed-byte 64) (*)) rout a) (type (signed-byte 64) s)
           (type fixnum ooff aoff n)
           (optimize (speed 3) (safety 0) (debug 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (block kernel
    (if (< s 0)
        (let ((c (max s -64)))
          (declare (type (integer -64 -1) c))
          (dotimes (i n)
            (setf (aref rout (the fixnum (+ ooff i))) (ash (aref a (the fixnum (+ aoff i))) c))))
        (let* ((c (min s 63))
               (hi (if (< s 64) (1- (ash 1 (- 63 c))) 0))
               (lo (if (< s 64) (- (ash 1 (- 63 c))) 0)))
          (declare (type (integer 0 63) c) (type (signed-byte 64) lo hi))
          (dotimes (i n)
            (let ((x (aref a (the fixnum (+ aoff i)))))
              (unless (and (<= lo x) (<= x hi)) (return-from kernel nil))
              (setf (aref rout (the fixnum (+ ooff i))) (the (signed-byte 64) (ash x c)))))))
    t))
(register-kernel "ash" :signed-byte-64 :as #'int-ash-as)

;; LOGBITP index integer: a negative index is CL's error (generic path); an index of 64 or more
;; reads the sign bit of a 64-bit value.
(def-sb64-pair-kernels "logbitp" bit (idx x)
  (if (> idx 63) (< x 0) (logbitp (the (integer 0 63) idx) x))
  :check (>= idx 0))

(def-sb64-pair-kernels "logtest" bit (x y) (/= 0 (logand x y)))

;;; ---------------------------------------------------------------------------
;;; unary kernels on (signed-byte 64): (NAME :signed-byte-64 :u)  (out ooff a aoff n)

(def-unary-kernel "lognot" :signed-byte-64 (signed-byte 64) (signed-byte 64) 8 8 nil (lognot x))
(def-unary-kernel "logcount" :signed-byte-64 (signed-byte 64) (signed-byte 64) 8 8 nil (logcount x))
(def-unary-kernel "integer-length" :signed-byte-64 (signed-byte 64) (signed-byte 64) 8 8 nil
  (integer-length x))

;;; ---------------------------------------------------------------------------
;;; integer operators: n-ary bitwise, binary bitwise, ASH, bit-array predicates

(defun int-binary-step (name fn a b &key bits)
  "(FN A B) for operands A and B: a fresh operand (a bit array operand when BITS).  Two scalars
give CL's own result."
  (when (and (not (operand-array-p a)) (not (operand-array-p b)))
    (return-from int-binary-step (classify-scalar (funcall fn (operand-value a) (operand-value b)))))
  (let* ((shape (shape-of a b)) (dims (first shape)) (path (third shape)))
    (cond
      ((eq path :empty)
       (if bits
           (bit-operand (make-result :bit dims))
           (classify (make-result :signed-byte-64 dims))))
      (t
       (let ((typed (and (sb64-int-operand-p a) (sb64-int-operand-p b)
                         (run-binary2 a b :signed-byte-64 nil (if bits :bit :signed-byte-64) name shape))))
         (cond
           (bits (bit-operand (if typed (second typed) (generic-bits2 fn a b shape))))
           (typed (make-operand (second typed) :int nil +sb64-min+ +sb64-max+))
           (t (classify (generic-map2 fn a b shape :empty-key :signed-byte-64)))))))))

(defun integer-arrays (name fn args &key bits)
  "The operator NAME (CL function FN) of ARGS, at least one an array; the result array."
  (let ((ops (classify-int-ops fn args)))
    (check-integer-operands ops)
    (if (null (rest ops))
        (operand-value (copy-operand-array (first ops)))
        (let ((acc (first ops)))
          (if (and bits (null (cddr ops)))
              (setf acc (int-binary-step name fn acc (second ops) :bits t))
              (dolist (o (rest ops)) (setf acc (int-binary-step name fn acc o))))
          (operand-value acc)))))

(defun integer-unary-array (name fn a)
  "NAME (CL function FN) of the elements of array A: an integer array."
  (let ((o (first (classify-int-ops fn (list a)))))
    (check-integer-operands (list o))
    (if (sb64-int-operand-p o)
        (map-float-array name :signed-byte-64 :signed-byte-64 o)
        (generic-map1 fn o :empty-key :signed-byte-64))))

(defmacro define-integer-operator (name cl-fn min max (&rest array-args) array-call doc &key bits)
  "Define the function NAME (a VM symbol): CL-FN for numbers, ARRAY-CALL (a form over ARRAY-ARGS, the
operands) for arrays.  BITS: the result of arrays is a bit array (stored into :INTO with STORE-BITS)."
  `(defun ,name (&rest args)
     ,doc
     (declare (dynamic-extent args))
     (elementwise-call #',cl-fn ,min ,max
                       (lambda (&rest ops) (destructuring-bind ,array-args ops ,array-call))
                       args
                       ,@(when bits '(#'store-bits)))))

(defmacro define-bitwise-operators ()
  `(progn
     ,@(loop for (name fn) in +bitwise-ops+
             for vm = (intern (string-upcase name) '#:vm)
             for nary = (member name '("logand" "logior" "logxor" "logeqv") :test #'string=)
             collect
             (if nary
                 `(defun ,vm (&rest args)
                    ,(format nil "Like ~A for integers; elementwise with R recycling when any argument is an integer array.
The result is a (signed-byte 64) array when every value fits, else element type T.  :INTO OUT as for VM:+." (string-upcase name))
                    (declare (dynamic-extent args))
                    (elementwise-call #',fn 0 most-positive-fixnum
                                      (lambda (&rest ops) (integer-arrays ,name #',fn ops))
                                      args))
                 `(defun ,vm (&rest args)
                    ,(format nil "Like ~A for integers; elementwise for integer arrays (two arguments)." (string-upcase name))
                    (declare (dynamic-extent args))
                    (elementwise-call #',fn 2 2
                                      (lambda (&rest ops) (integer-arrays ,name #',fn ops))
                                      args))))))

(define-bitwise-operators)

(define-integer-operator vm:lognot cl:lognot 1 1 (a) (integer-unary-array "lognot" #'cl:lognot a)
  "Like LOGNOT for integers; elementwise for integer arrays: a (signed-byte 64) array when every
value fits, else element type T.")
(define-integer-operator vm:logcount cl:logcount 1 1 (a) (integer-unary-array "logcount" #'cl:logcount a)
  "Like LOGCOUNT for integers; elementwise for integer arrays ((signed-byte 64) results).")
(define-integer-operator vm:integer-length cl:integer-length 1 1 (a)
  (integer-unary-array "integer-length" #'cl:integer-length a)
  "Like INTEGER-LENGTH for integers; elementwise for integer arrays ((signed-byte 64) results).")
(define-integer-operator vm:ash cl:ash 2 2 (a b) (integer-arrays "ash" #'cl:ash (list a b))
  "Like ASH for integers; elementwise with R recycling when either argument is an integer array.  The
result is a (signed-byte 64) array when every value fits, else element type T.")
(define-integer-operator vm:logbitp cl:logbitp 2 2 (a b) (integer-arrays "logbitp" #'cl:logbitp (list a b) :bits t)
  "Like LOGBITP for numbers (index, integer); a bit array for arrays.  :INTO OUT takes the bits."
  :bits t)
(define-integer-operator vm:logtest cl:logtest 2 2 (a b) (integer-arrays "logtest" #'cl:logtest (list a b) :bits t)
  "Like LOGTEST for numbers; a bit array for arrays.  :INTO OUT takes the bits."
  :bits t)

;;; ---------------------------------------------------------------------------
;;; FLOAT, SCALE-FLOAT, FLOAT-SIGN
;;;
;;; Mixed kernels (x of one element type, a float prototype or an integer): registered under
;;; (NAME KEY :aa/:as/:sa) with KEY naming the element types; used with RUN-BINARY.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun float-pair-key (xkey ykey)
    "The kernel key for operand data keys XKEY and YKEY (:signed-byte-64 :single-float :double-float)."
    (flet ((short (k) (ecase k (:signed-byte-64 "SB64") (:single-float "SINGLE") (:double-float "DOUBLE"))))
      (intern (format nil "~A>~A" (short xkey) (short ykey)) '#:keyword)))
  (defun key-type (key)
    (ecase key (:signed-byte-64 '(signed-byte 64)) (:single-float 'single-float) (:double-float 'double-float))))

;; (float x proto): x converted to the float format of PROTO.  The conversion is the inline one
;; (correct rounding; checked against CL:FLOAT per element in the tests).  Only the format of PROTO
;; matters, so the kernels convert to a literal prototype: CCL open-codes (float x 1d0) but makes a full
;; call (about 13 ns instead of 2) for a prototype that is a variable.
(macrolet ((def-all ()
             `(progn
                ,@(loop for xkey in '(:signed-byte-64 :single-float :double-float)
                        append (loop for pkey in '(:single-float :double-float)
                                     collect `(def-mixed-binary-kernels "float" ,(float-pair-key xkey pkey)
                                                ,(key-type xkey) ,(key-type pkey) ,(key-type pkey) (x y)
                                                (progn y (float x ,(ecase pkey (:single-float 1f0) (:double-float 1d0))))))))))
  (def-all))

;; (scale-float x n) and (float-sign x y): CL's function itself, notinline.  FLOAT-SIGN of floats of
;; different formats is not typed: CL leaves the format of the result open (CCL gives the format of the
;; second argument, SBCL the contagion of the two).
(macrolet ((def-all ()
             `(progn
                ,@(loop for xkey in '(:single-float :double-float)
                        collect `(def-mixed-binary-kernels "scale-float" ,xkey ,(key-type xkey) (signed-byte 64)
                                   ,(key-type xkey) (x y)
                                   (locally (declare (notinline scale-float)) (scale-float x y)))
                        collect `(def-mixed-binary-kernels "float-sign" ,xkey ,(key-type xkey) ,(key-type xkey)
                                   ,(key-type xkey) (x y)
                                   (locally (declare (notinline float-sign)) (float-sign x y)))
                        collect `(def-unary-kernel "float-sign" ,xkey ,(key-type xkey) ,(key-type xkey) 8 8 nil
                                   (locally (declare (notinline float-sign)) (float-sign x)))))))
  (def-all))

(defun float-result-key (o)
  "The key of the float format of operand O, single-float when it is not a float."
  (if (real-float-operand-p o) (operand-float-key o) :single-float))

(defun float-sign-result-key (a b)
  "The key of the float format CL's FLOAT-SIGN returns for the formats of operands A and B (the
implementation's own choice for mixed formats), single-float when they are not floats."
  (flet ((proto (o) (case (and (real-float-operand-p o) (operand-float-key o))
                      (:single-float 1f0) (:double-float 1d0) (t nil))))
    (let ((pa (proto a)) (pb (proto b)))
      (if (and pa pb)
          (if (typep (float-sign pa pb) 'double-float) :double-float :single-float)
          (float-result-key b)))))

(defun float-array (x &optional (proto nil proto-p))
  "(float x [proto]) for operands of which at least one is an array."
  (let ((ops (if proto-p (list x proto) (list x))))
    (let ((cops (classify-int-ops #'cl:float ops)))
      (if (not proto-p)
          (let ((o (first cops)))
            (cond ((real-float-operand-p o) (operand-value (copy-operand-array o)))
                  ((sb64-int-operand-p o)
                   (float-pair o (classify-scalar 1f0)))
                  (t (generic-map1 #'cl:float o :empty-key :single-float :masked t))))
          (float-pair (first cops) (second cops))))))

(defun float-pair (a b)
  "(float x proto) for the classified operands A (of X) and B (of PROTO), at least one an array."
  (let ((shape (shape-of a b)))
    (if (and (real-float-operand-p b)
             (not (operand-cplx a))
             (or (real-float-operand-p a) (sb64-int-operand-p a)))
        (let* ((akey (operand-int-key a)) (pkey (operand-float-key b)))
          (run-binary a b akey pkey pkey "float" (float-pair-key akey pkey) shape))
        (generic-map2 #'cl:float a b shape :empty-key (float-result-key b) :masked t))))

(defun scale-float-array (x n)
  (let* ((cops (classify-int-ops #'cl:scale-float (list x n))) (a (first cops)) (b (second cops))
         (shape (shape-of a b)))
    (if (and (real-float-operand-p a) (sb64-int-operand-p b))
        (let ((key (operand-float-key a)))
          (run-binary a b key :signed-byte-64 key "scale-float" key shape))
        (generic-map2 #'cl:scale-float a b shape :empty-key (float-result-key a) :masked t))))

(defun float-sign-array (x &optional (y nil y-p))
  (let* ((cops (classify-int-ops #'cl:float-sign (if y-p (list x y) (list x)))) (a (first cops)))
    (if (not y-p)
        (if (real-float-operand-p a)
            (map-float-array "float-sign" (operand-float-key a) (operand-float-key a) a)
            (generic-map1 #'cl:float-sign a :empty-key :single-float :masked t))
        (let* ((b (second cops)) (shape (shape-of a b)))
          (if (and (real-float-operand-p a) (real-float-operand-p b)
                   (eq (operand-float-key a) (operand-float-key b)))
              (let ((key (operand-float-key a)))
                (run-binary a b key key key "float-sign" key shape))
              (generic-map2 #'cl:float-sign a b shape :empty-key (float-sign-result-key a b) :masked t))))))

(define-elementwise-operator vm:float cl:float 1 2 float-array
  "Like FLOAT for numbers.  For arrays: a float array of the format of the prototype (single-float
without one, unless the argument is a float array, which is copied); the prototype may itself be an
array, with R recycling.  Results of mixed formats (element type T prototypes) give element type T.
Float arrays are converted with the traps masked.")
(define-elementwise-operator vm:scale-float cl:scale-float 2 2 scale-float-array
  "Like SCALE-FLOAT for numbers; elementwise for a float array and an integer scale (R recycling),
the result in the format of the float.  Computed with the float traps masked: what CL's function
does on overflow and underflow in that state (see the README).")
(define-elementwise-operator vm:float-sign cl:float-sign 1 2 float-sign-array
  "Like FLOAT-SIGN for numbers; elementwise for float arrays (the result has the format of the
second argument, or of the first without one).")

;;; ---------------------------------------------------------------------------
;;; RATIONAL RATIONALIZE NUMERATOR DENOMINATOR
;;;
;;; Generic path (CL's function per element, in the caller's trap state: no float arithmetic is
;;; involved, and a NaN or an infinity gets whatever CL's function does with it).  Integer arrays are
;;; their own rational, numerator and have denominator 1.

(defun rational-array-1 (name fn a)
  (let ((o (first (classify-int-ops fn (list a)))))
    (cond ((and (eq (operand-base o) :int) (not (operand-cplx o)))
           (if (string= name "denominator")
               (make-array (array-dimensions (operand-value o)) :element-type '(signed-byte 64)
                                                                :initial-element 1)
               (operand-value (copy-operand-array o))))
          (t (generic-map1 fn o :empty-key :signed-byte-64)))))

(define-integer-operator vm:rational cl:rational 1 1 (a) (rational-array-1 "rational" #'cl:rational a)
  "Like RATIONAL for numbers; elementwise for arrays: a (signed-byte 64) array when every value is an
integer that fits (integral floats give integers), else element type T.  NaN and infinities: what
CL's function does for that element.")
(define-integer-operator vm:rationalize cl:rationalize 1 1 (a)
  (rational-array-1 "rationalize" #'cl:rationalize a)
  "Like RATIONALIZE for numbers; elementwise for arrays, with the result types of VM:RATIONAL.")
(define-integer-operator vm:numerator cl:numerator 1 1 (a)
  (rational-array-1 "numerator" #'cl:numerator a)
  "Like NUMERATOR for numbers; elementwise for rational arrays (integers are their own numerator).")
(define-integer-operator vm:denominator cl:denominator 1 1 (a)
  (rational-array-1 "denominator" #'cl:denominator a)
  "Like DENOMINATOR for numbers; elementwise for rational arrays (1 for integers).")
