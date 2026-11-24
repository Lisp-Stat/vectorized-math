;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; VM:=  VM:/=  VM:<  VM:>  VM:<=  VM:>=      comparisons
;;; VM:ZEROP VM:PLUSP VM:MINUSP VM:EVENP VM:ODDP predicates
;;; VM:MAX VM:MIN                                elementwise maximum and minimum
;;;
;;; Numbers: exactly CL.  When an operand is an array the comparisons and predicates return
;;; a (simple-array bit) of the R-rules result shape (1 where the relation holds), MAX and
;;; MIN an array of the contagion element type.
;;;
;;; NaN rule (arrays only, independent of the implementation and of the NaN comparison bug of
;;; released CCL): every relation with a NaN is 0 except /=, which is 1; MAX and MIN
;;; propagate a NaN.  The typed kernels test for a NaN explicitly (NAN-P, elementwise.lisp)
;;; and never trust a comparison with a NaN, so they are right whatever the implementation's
;;; comparison does.
;;;
;;; Exactness: a float and a rational are compared exactly (as CL does, via RATIONAL, never by
;;; converting the rational to a float).  The typed kernels run in the double-float domain only
;;; when every operand converts to it without error (integers within +-2^53, rational scalars
;;; that are exactly representable); everything else takes the generic path, which calls the CL
;;; relation per element.

(in-package #:vm.impl)

;;; ---------------------------------------------------------------------------
;;; kernels: the six relations -> bit arrays  (variants :aa :as :sa, out = simple-bit-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +compare-ops+
    '(("=" =) ("/=" /=) ("<" <) (">" >) ("<=" <=) (">=" >=))))

(defmacro either-nan-p (type x y)
  "True when the TYPE floats X and Y (variables) are not both numbers, i.e. one is a NaN.  A NaN
operand always makes X + Y a NaN, so one bit-level test of the sum screens both operands; the sum
is also a NaN for an infinity against its opposite, and then the operands are tested one by one."
  `(and (nan-p (+ ,x ,y) ,type) (or (nan-p ,x ,type) (nan-p ,y ,type))))

(defmacro ieee-compare (fn type x y)
  "1 or 0: (FN X Y) for X and Y of the float TYPE with IEEE semantics for a NaN: every relation
is 0, /= is 1.  The comparison goes first and the (bit level) NaN test only when it could be wrong
because of a NaN, so the result does not depend on how the implementation compares a NaN."
  (if (eq fn '/=)
      `(if (or (/= ,x ,y) (either-nan-p ,type ,x ,y)) 1 0)
      `(if (and (,fn ,x ,y) (not (either-nan-p ,type ,x ,y))) 1 0)))

(defmacro ieee-complex-compare (fn type x y)
  "As IEEE-COMPARE for = and /= of complex numbers with TYPE float parts."
  `(let ((xr (realpart ,x)) (xi (imagpart ,x)) (yr (realpart ,y)) (yi (imagpart ,y)))
     (declare (type ,type xr xi yr yi))
     ,(if (eq fn '=)
          `(if (and (= xr yr) (= xi yi)
                    (not (nan-p xr ,type)) (not (nan-p xi ,type))
                    (not (nan-p yr ,type)) (not (nan-p yi ,type)))
               1 0)
          `(if (or (/= xr yr) (/= xi yi)
                   (nan-p xr ,type) (nan-p xi ,type) (nan-p yr ,type) (nan-p yi ,type))
               1 0))))

;;; NaN screening.  Kernels for float arrays exist in two forms: a FAST one that compares with the
;;; implementation's own operators (right only when no operand is a NaN) and a CAREFUL one that tests
;;; every element for a NaN, bit level (NAN-P), and applies the IEEE rules.  The registered kernel
;;; screens its operands first (MAYBE-NAN-D / -S: the sum of the elements is a NaN when any element
;;; is, one bit-level test for a whole array) and runs the fast form when they cannot hold a NaN.  The
;;; sum is also a NaN for an infinity and its opposite: that sends the array to the careful form,
;;; which is merely slower.  Results are identical in both forms by construction.  The scan runs
;;; with the float traps masked (every caller does).

(defmacro def-maybe-nan-scans ()
  `(progn
     ,@(loop for (type fn zero) in '((double-float maybe-nan-d 0d0) (single-float maybe-nan-s 0f0))
             collect
             `(defun ,fn (a off n)
                "True when the elements OFF .. OFF+N-1 of A may include a NaN (false: certainly none)."
                (declare (type (simple-array ,type (*)) a) (type fixnum off n)
                         (optimize (speed 3) (safety 0) (debug 0))
                         #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                (let ((s0 ,zero) (s1 ,zero) (s2 ,zero) (s3 ,zero) (i off) (end (+ off n)))
                  (declare (type ,type s0 s1 s2 s3) (type fixnum i end))
                  (loop while (<= (+ i 4) end)
                        do (setf s0 (+ s0 (aref a i)) s1 (+ s1 (aref a (+ i 1)))
                                 s2 (+ s2 (aref a (+ i 2))) s3 (+ s3 (aref a (+ i 3))))
                           (setf i (+ i 4)))
                  (loop while (< i end) do (setf s0 (+ s0 (aref a i))) (setf i (+ i 1)))
                  (let ((total (+ (+ s0 s1) (+ s2 s3))))
                    (declare (type ,type total))
                    (with-nan-scratch (nan-p total ,type))))))))
(def-maybe-nan-scans)

(defmacro def-nan-binary-kernels (name key type out-type (x y) fast careful)
  "Register the binary kernels (NAME KEY) for float arrays of TYPE: FAST and CAREFUL expressions of
X and Y as described above."
  (let* ((fname (format nil "~A!fast" name)) (cname (format nil "~A!careful" name))
         (scan (if (eq type 'double-float) 'maybe-nan-d 'maybe-nan-s))
         (fa (kname 'lisp fname key 'aa)) (fs (kname 'lisp fname key 'as)) (fr (kname 'lisp fname key 'sa))
         (ca (kname 'lisp cname key 'aa)) (cs (kname 'lisp cname key 'as)) (cr (kname 'lisp cname key 'sa))
         (aa (kname 'screen name key 'aa)) (as (kname 'screen name key 'as)) (sa (kname 'screen name key 'sa))
         (arr `(simple-array ,type (*))) (opts '(optimize (speed 1) (safety 0) (debug 0))))
    #+sbcl
    ;; SBCL tests a NaN with one inline instruction: no screening, the careful form is as fast
    (progn fast fa fs fr aa as sa scan arr opts
           `(progn
              (def-mixed-binary-kernels ,cname ,key ,type ,type ,out-type (,x ,y) ,careful :speed 3)
              (register-kernel ,name ,key :aa #',ca)
              (register-kernel ,name ,key :as #',cs)
              (register-kernel ,name ,key :sa #',cr)))
    #-sbcl
    `(progn
       (def-mixed-binary-kernels ,fname ,key ,type ,type ,out-type (,x ,y) ,fast :speed 3)
       (def-mixed-binary-kernels ,cname ,key ,type ,type ,out-type (,x ,y) ,careful)
       (defun ,aa (out ooff a aoff b boff n)
         (declare (type ,arr a b) (type fixnum ooff aoff boff n) ,opts)
         (if (or (,scan a aoff n) (,scan b boff n))
             (,ca out ooff a aoff b boff n)
             (,fa out ooff a aoff b boff n)))
       (defun ,as (out ooff a aoff s n)
         (declare (type ,arr a) (type ,type s) (type fixnum ooff aoff n) ,opts)
         (if (or (nan-p s ,type) (,scan a aoff n))
             (,cs out ooff a aoff s n)
             (,fs out ooff a aoff s n)))
       (defun ,sa (out ooff s b boff n)
         (declare (type ,arr b) (type ,type s) (type fixnum ooff boff n) ,opts)
         (if (or (nan-p s ,type) (,scan b boff n))
             (,cr out ooff s b boff n)
             (,fr out ooff s b boff n)))
       (register-kernel ,name ,key :aa #',aa)
       (register-kernel ,name ,key :as #',as)
       (register-kernel ,name ,key :sa #',sa))))

(defmacro def-compare-kernel-set ()
  `(progn
     ,@(loop for (name fn) in +compare-ops+
             append (loop for (key type) in '((:single-float single-float) (:double-float double-float))
                          collect `(def-nan-binary-kernels ,name ,key ,type bit (x y)
                                     (if (,fn x y) 1 0)
                                     (ieee-compare ,fn ,type x y)))
             collect `(def-mixed-binary-kernels ,name :signed-byte-64 (signed-byte 64) (signed-byte 64) bit (x y)
                        (if (,fn x y) 1 0) :speed 3)
             when (member name '("=" "/=") :test #'string=)
               append (loop for (key type) in '((:complex-single-float single-float)
                                                (:complex-double-float double-float))
                            collect `(def-mixed-binary-kernels ,name ,key (complex ,type) (complex ,type) bit (x y)
                                       (ieee-complex-compare ,fn ,type x y))))))

(def-compare-kernel-set)

;;; AND of two bit arrays (chains: (< a b c) is (and (< a b) (< b c)))
(def-lisp-kernels bit-and :bit bit (x y) (logand x y))
(dolist (v '(:aa :as :sa))
  (register-kernel "bit-and" :bit v (symbol-function (kname 'lisp 'bit-and :bit v))))

;;; ---------------------------------------------------------------------------
;;; kernels: max and min  (NaN propagates; ties return the first operand, as CL does)

(defmacro def-maxmin-kernels (opname cmp)
  (let ((regname (string-downcase (symbol-name opname))))
    `(progn
       ,@(loop for (key type) in '((:single-float single-float) (:double-float double-float))
               collect `(def-nan-binary-kernels ,regname ,key ,type ,type (x y)
                          (if (,cmp x y) y x)
                          (if (either-nan-p ,type x y)
                              (if (nan-p x ,type) x y)
                              (if (,cmp x y) y x))))
       (def-lisp-kernels ,opname :signed-byte-64 (signed-byte 64) (x y)
         (if (,cmp x y) y x))
       (dolist (v '(:aa :as :sa))
         (register-kernel ,regname :signed-byte-64 v (symbol-function (kname 'lisp ',opname :signed-byte-64 v)))))))

(def-maxmin-kernels max <)
(def-maxmin-kernels min >)

;;; ---------------------------------------------------------------------------
;;; kernels: predicates  (variant :u  (out ooff a aoff n), out a simple-bit-vector)

(defmacro def-pred-kernel (opname key type expr &key (speed 1))
  "Kernel of the predicate OPNAME for arrays of element TYPE; EXPR computes the generalised
boolean from X."
  (let ((fn (kname 'lisp 'pred opname key 'u)))
    `(progn
       (defun ,fn (out ooff a aoff n)
         (declare (type (simple-array bit (*)) out) (type (simple-array ,type (*)) a)
                  (type fixnum ooff aoff n)
                  (optimize (speed ,speed) (safety 0) (debug 0))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (with-nan-scratch
           (dotimes (i n)
             (let ((x (aref a (+ aoff i))))
               (setf (aref out (+ ooff i)) (if ,expr 1 0)))))
         out)
       (register-kernel ,opname ,key :u #',fn))))

(defmacro def-nan-pred-kernel (opname key type fast careful)
  "The predicate kernel OPNAME for float arrays of TYPE: the FAST expression of X when the array
cannot hold a NaN (see the NaN screening above), else the CAREFUL one."
  (let* ((fname (format nil "~A!fast" opname)) (cname (format nil "~A!careful" opname))
         (scan (if (eq type 'double-float) 'maybe-nan-d 'maybe-nan-s))
         (ff (kname 'lisp 'pred fname key 'u)) (cf (kname 'lisp 'pred cname key 'u))
         (fn (kname 'screen 'pred opname key 'u)))
    #+sbcl
    (progn fast ff fn scan
           `(progn
              (def-pred-kernel ,cname ,key ,type ,careful :speed 3)
              (register-kernel ,opname ,key :u #',cf)))
    #-sbcl
    `(progn
       (def-pred-kernel ,fname ,key ,type ,fast :speed 3)
       (def-pred-kernel ,cname ,key ,type ,careful)
       (defun ,fn (out ooff a aoff n)
         (declare (type (simple-array ,type (*)) a) (type fixnum ooff aoff n)
                  (optimize (speed 1) (safety 0) (debug 0)))
         (if (,scan a aoff n)
             (,cf out ooff a aoff n)
             (,ff out ooff a aoff n)))
       (register-kernel ,opname ,key :u #',fn))))

(defmacro def-float-pred-kernels ()
  (flet ((float-zero (type) (if (eq type 'double-float) 0d0 0f0)))
  `(progn
     ,@(loop for (key type) in '((:single-float single-float) (:double-float double-float))
             append `((def-nan-pred-kernel "zerop" ,key ,type (= x ,(float-zero type))
                        (and (= x ,(float-zero type)) (not (nan-p x ,type))))
                      (def-nan-pred-kernel "plusp" ,key ,type (> x ,(float-zero type))
                        (and (> x ,(float-zero type)) (not (nan-p x ,type))))
                      (def-nan-pred-kernel "minusp" ,key ,type (< x ,(float-zero type))
                        (and (< x ,(float-zero type)) (not (nan-p x ,type))))))
     ,@(loop for (key type) in '((:complex-single-float single-float) (:complex-double-float double-float))
             collect `(def-pred-kernel "zerop" ,key (complex ,type)
                        (let ((xr (realpart x)) (xi (imagpart x)))
                          (declare (type ,type xr xi))
                          (and (= xr ,(float-zero type)) (= xi ,(float-zero type))
                               (not (nan-p xr ,type)) (not (nan-p xi ,type)))))))))
(def-float-pred-kernels)

(def-pred-kernel "zerop" :signed-byte-64 (signed-byte 64) (= x 0) :speed 3)
(def-pred-kernel "plusp" :signed-byte-64 (signed-byte 64) (> x 0) :speed 3)
(def-pred-kernel "minusp" :signed-byte-64 (signed-byte 64) (< x 0) :speed 3)
(def-pred-kernel "evenp" :signed-byte-64 (signed-byte 64) (not (logbitp 0 x)) :speed 3)
(def-pred-kernel "oddp" :signed-byte-64 (signed-byte 64) (logbitp 0 x) :speed 3)

;;; ---------------------------------------------------------------------------
;;; common pieces

(defun compare-function (opname)
  (cond ((string= opname "=") #'cl:=)
        ((string= opname "/=") #'cl:/=)
        ((string= opname "<") #'cl:<)
        ((string= opname ">") #'cl:>)
        ((string= opname "<=") #'cl:<=)
        ((string= opname ">=") #'cl:>=)
        (t (error "unknown comparison ~S" opname))))

(defun equality-op-p (opname) (or (string= opname "=") (string= opname "/=")))

(defun store-bits (value out)
  "Store the result VALUE of a comparison or predicate (T/NIL or a bit array) into OUT, which must
be able to hold 0 and 1; return OUT."
  (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
  (let ((et (array-element-type out)))
    (unless (and (typep 0 et) (typep 1 et))
      (output-error 1 et))
    (if (arrayp value)
        (store-into value out)
        (let ((bit (if value 1 0)))
          (dotimes (i (array-total-size out)) (setf (row-major-aref out i) bit))
          out))))

(defun elt-nan-p (x)
  "True when the number X is a NaN or a complex number with a NaN part."
  (typecase x
    (float (float-nan-p x))
    (complex (or (elt-nan-p (realpart x)) (elt-nan-p (imagpart x))))
    (t nil)))

(defun compare-elements (opname fn x y)
  "The relation OPNAME (function FN) between the numbers X and Y with the array rules for a NaN."
  (cond ((or (elt-nan-p x) (elt-nan-p y)) (string= opname "/="))
        ((or (complexp x) (complexp y))
         (if (equality-op-p opname)
             (let ((same (and (cl:= (realpart x) (realpart y))
                              (cl:= (imagpart x) (imagpart y)))))
               (if (string= opname "=") same (not same)))
             (error 'type-error :datum (if (complexp x) x y) :expected-type 'real)))
        (t (funcall fn x y))))

(defun shape-of-pair (a b)
  (multiple-value-list (analyze-shape a b)))

(defun complex-operand-error (o)
  (let ((v (operand-value o)))
    (error 'type-error
           :datum (if (and (arrayp v) (plusp (array-total-size v))) (row-major-aref v 0) v)
           :expected-type 'real)))

;;; --- integer ranges and exact conversions ---

(defconstant +double-exact-limit+ (expt 2 53))

(defun int-range-exact-in-double-p (lo hi)
  (and (<= (- +double-exact-limit+) lo) (<= hi +double-exact-limit+)))

(defun scan-int-range (arr)
  "(values lo hi) of the elements of integer array ARR (0 0 when empty)."
  (let ((data (flat-data arr)) (n (array-total-size arr)) (lo nil) (hi nil))
    (dispatch-source (s data +int-source-types+)
      (dotimes (i n)
        (let ((x (aref s i)))
          (when (or (null lo) (< x lo)) (setf lo x))
          (when (or (null hi) (> x hi)) (setf hi x)))))
    (values (or lo 0) (or hi 0))))

(defun operand-int-range (o)
  "The (values lo hi) range of integer operand O: its declared range, narrowed by a scan of the
data when the declared range is too wide to convert to a double exactly."
  (let ((lo (operand-lo o)) (hi (operand-hi o)))
    (if (or (not (operand-array-p o)) (int-range-exact-in-double-p lo hi))
        (values lo hi)
        (scan-int-range (operand-value o)))))

(defun exactify-scalar (o other)
  "When O is a scalar rational that has no exact double-float image problem (a ratio, or an
integer beyond 2^53) and OTHER is a float operand, return O as a double-float scalar operand if
that is exact, else O itself."
  (if (and (not (operand-array-p o)) (not (operand-cplx o))
           (member (operand-base other) '(:s :d))
           (or (eq (operand-base o) :rat)
               (and (eq (operand-base o) :int)
                    (not (int-range-exact-in-double-p (operand-lo o) (operand-hi o))))))
      (let* ((x (operand-value o))
             (d (handler-case (with-masked-float-traps (float x 1d0)) (error () nil))))
        (if (and d (<= (abs d) most-positive-double-float) (= (rational d) x))
            (make-operand d :d nil)
            o))
      o))

(defun compare-key (opname a b)
  "The element-type key of the typed kernel that compares operands A and B exactly, or NIL when
the generic path must be used."
  (let ((ba (operand-base a)) (bb (operand-base b))
        (cplx (or (operand-cplx a) (operand-cplx b))))
    (cond
      ((or (member ba '(:rat :gen)) (member bb '(:rat :gen))) nil)
      ((and (eq ba :int) (eq bb :int))
       (and (fits-sb64 (operand-lo a)) (fits-sb64 (operand-hi a))
            (fits-sb64 (operand-lo b)) (fits-sb64 (operand-hi b))
            :signed-byte-64))
      (t
       (and (flet ((ok (o) (or (not (eq (operand-base o) :int))
                               (multiple-value-bind (lo hi) (operand-int-range o)
                                 (int-range-exact-in-double-p lo hi)))))
              (and (ok a) (ok b)))
            (let ((single (and (eq ba :s) (eq bb :s))))
              (cond ((not cplx) (if single :single-float :double-float))
                    ((equality-op-p opname) (if single :complex-single-float :complex-double-float))
                    (t nil))))))))

(defun generic-compare (opname a b shape)
  "The relation OPNAME between the elements of operands A and B combined according to SHAPE,
by CL's own comparison per element.  A fresh simple-bit-vector."
  (destructuring-bind (dims n path extra) shape
    (let ((result (make-result :bit dims)))
      (unless (eq path :empty)
        (let ((idx (make-pair-indexer a b dims n path extra))
              (ga (operand-accessor a)) (gb (operand-accessor b))
              (rd (flat-data result)) (fn (compare-function opname)))
          (declare (type (simple-array bit (*)) rd))
          (with-masked-float-traps
            (dotimes (r n)
              (multiple-value-bind (ia ib) (funcall idx r)
                (setf (aref rd r)
                      (if (compare-elements opname fn (funcall ga ia) (funcall gb ib)) 1 0)))))))
      result)))

(defun bit-operand (array) (make-operand array :int nil 0 1))

(defun compare-pair (opname a b)
  "OPNAME between operands A and B: T or NIL when both are scalars (CL's own answer, errors
included), else a bit-array operand."
  (if (and (not (operand-array-p a)) (not (operand-array-p b)))
      (funcall (compare-function opname) (operand-value a) (operand-value b))
      (let* ((a (exactify-scalar a b)) (b (exactify-scalar b a))
             (shape (shape-of-pair a b))
             (key (compare-key opname a b)))
        (bit-operand
         (if key
             (run-binary a b key key :bit opname key shape)
             (generic-compare opname a b shape))))))

(defun zero-bits (dims) (make-array dims :element-type 'bit :initial-element 0))

(defun and-combine* (acc r)
  "ACC AND R for ACC and R each T, NIL or a bit-array operand: a boolean broadcasts over the shape
of an array operand, two arrays combine by the R shape rules."
  (let ((acc-array (operand-p acc)) (r-array (operand-p r)))
    (cond ((and acc-array r-array)
           (bit-operand (run-binary acc r :bit :bit :bit "bit-and" :bit (shape-of-pair acc r))))
          (acc-array (if r acc (bit-operand (zero-bits (array-dimensions (operand-value acc))))))
          (r-array (if acc r (bit-operand (zero-bits (array-dimensions (operand-value r))))))
          (t (and acc r)))))

(defun common-dims (ops)
  "The dimensions of the result of combining the array operands among OPS under the R rules (the
shape rules of two operands applied left to right; they signal the errors and warnings)."
  (let ((acc nil))
    (dolist (o ops (and acc (operand-dims acc)))
      (when (operand-array-p o)
        (setf acc (if acc
                      (let ((dims (analyze-shape acc o)))
                        (if (equal dims (operand-dims acc))
                            acc
                            (bit-operand (make-array dims :element-type 'bit))))
                      o))))))

(defun recycle-operand (o dims)
  "Array operand O recycled to dimensions DIMS (R rules: column-major for rank 2 and more):
O itself when it has those dimensions, else a fresh operand of the same kind."
  (let* ((arr (operand-value o)) (m (array-total-size arr)))
    (if (equal (array-dimensions arr) dims)
        o
        (let* ((n (reduce #'* dims))
               (out (make-array dims :element-type (array-element-type arr))))
          (when (plusp n)
            (replace (flat-data out)
                     (if (<= (length dims) 1)
                         (expand-cyclic (flat-data arr) m n)
                         (expand-colmajor (flat-data arr) m dims))))
          (make-operand out (operand-base o) (operand-cplx o) (operand-lo o) (operand-hi o))))))

(defun comparison-pairs (opname n)
  "The operand index pairs whose relations are ANDed: neighbours for a chain, all pairs for /=."
  (if (string= opname "/=")
      (loop for i below n append (loop for j from (1+ i) below n collect (cons i j)))
      (loop for i below (1- n) collect (cons i (1+ i)))))

(defun classify-real-args (args numbers-ok)
  "Classify ARGS in order.  A scalar that is not a number signals the type error CL's ordering
functions do (expected type REAL), or NUMBER when NUMBERS-OK (= and /=)."
  (mapcar (lambda (x)
            (if (and (not numbers-ok) (not (arrayp x)) (not (numberp x)))
                (error 'type-error :datum x :expected-type 'real)
                (classify x)))
          args))

(defun compare-arrays (opname args)
  "OPNAME of ARGS (at least one an array): a bit array operand value."
  (let* ((ops (classify-real-args args (equality-op-p opname)))      ; type-errors in argument order
         (n (length ops)))
    (unless (equality-op-p opname)
      (dolist (o ops) (when (operand-cplx o) (complex-operand-error o))))
    (if (= n 1)
        (let ((arr (operand-value (first ops))))
          (make-array (array-dimensions arr) :element-type 'bit :initial-element 1))
        (let ((vec (coerce (if (and (>= n 3) (> (count-if #'operand-array-p ops) 1))
                               ;; a chain over several arrays is elementwise over ONE common
                               ;; shape: every array operand is recycled to it first
                               (let ((dims (common-dims ops)))
                                 (mapcar (lambda (o) (if (operand-array-p o) (recycle-operand o dims) o)) ops))
                               ops)
                           'simple-vector))
              (acc t))
          (dolist (p (comparison-pairs opname n))
            (setf acc (and-combine* acc (compare-pair opname (svref vec (car p)) (svref vec (cdr p))))))
          (operand-value acc)))))

(defun compare-operator (opname scalar-function args)
  "The body of the six comparisons.  SCALAR-FUNCTION is CL's."
  (flet ((arrays (ops)
           ;; an array that cannot hold numbers (a string): CL's own error
           (with-cl-error-replay (scalar-function ops) (compare-arrays opname ops))))
    (if (notany #'arrayp args)
        (apply scalar-function args)
        (let ((nops (into-position args)))
          (if nops
              (let ((out (car (last args))) (ops (subseq args 0 nops)))
                (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
                (store-bits (if (some #'arrayp ops) (arrays ops) (apply scalar-function ops)) out))
              (arrays args))))))

(defmacro define-compare-operator (name opname scalar-function doc)
  `(defun ,name (&rest args)
     ,doc
     (declare (dynamic-extent args))
     (compare-operator ,opname #',scalar-function args)))

(define-compare-operator vm:= "=" =
  "True like = for numbers.  When any argument is an array: a (simple-array bit) with 1 where the
relation holds, with R's recycling rules; (= a b c) is (and (= a b) (= b c)) elementwise.  A NaN is
never equal to anything.  Complex numbers are allowed.  :INTO OUT stores the bits in OUT (an array
that can hold 0 and 1).")
(define-compare-operator vm:/= "/=" /=
  "Like /= for numbers.  For arrays 1 where all the elements are pairwise distinct; a NaN is distinct
from everything.  :INTO OUT as for VM:=.")
(define-compare-operator vm:< "<" <
  "Like < for numbers; (simple-array bit) results for arrays as for VM:=.  Complex numbers signal CL's type error.")
(define-compare-operator vm:> ">" >
  "Like > for numbers; bit array results for arrays.")
(define-compare-operator vm:<= "<=" <=
  "Like <= for numbers; bit array results for arrays.")
(define-compare-operator vm:>= ">=" >=
  "Like >= for numbers; bit array results for arrays.")

;;; ---------------------------------------------------------------------------
;;; predicates

(defun predicate-error (cl-fn arr expected)
  "Signal the type error CL-FN gives for the first element of ARR (EXPECTED when ARR is empty)."
  (when (plusp (array-total-size arr)) (funcall cl-fn (row-major-aref arr 0)))
  (error 'type-error :datum arr :expected-type expected))

(defun generic-predicate (name cl-fn o)
  (let* ((arr (operand-value o)) (n (array-total-size arr))
         (result (make-result :bit (array-dimensions arr)))
         (data (flat-data arr)) (rd (flat-data result))
         (nan-guard (cond ((string= name "zerop") #'elt-nan-p)
                          ((or (string= name "plusp") (string= name "minusp"))
                           (lambda (x) (and (floatp x) (float-nan-p x))))
                          (t nil))))
    (declare (type (simple-array bit (*)) rd))
    (with-masked-float-traps
      (dotimes (i n)
        (let ((x (aref data i)))
          (setf (aref rd i)
                (if (if (and nan-guard (funcall nan-guard x)) nil (funcall cl-fn x)) 1 0)))))
    result))

(defun predicate-array (name cl-fn a)
  "The predicate NAME (CL function CL-FN) of the elements of array A: a bit array."
  (let* ((o (classify a)) (arr (operand-value o)) (n (array-total-size arr))
         (base (operand-base o)) (cplx (operand-cplx o)))
    (flet ((run (key)
             (let ((result (make-result :bit (array-dimensions arr))))
               (when (plusp n)
                 (with-masked-float-traps
                   (funcall (the function (find-kernel name key :u))
                            (flat-data result) 0 (prepare-data arr key) 0 n)))
               result)))
      (cond
        ((member base '(:s :d))
         (cond ((and cplx (not (string= name "zerop"))) (predicate-error cl-fn arr 'real))
               ((or (string= name "evenp") (string= name "oddp")) (predicate-error cl-fn arr 'integer))
               (t (run (base-cplx-key base cplx)))))
        ((and (eq base :int) (fits-sb64 (operand-lo o)) (fits-sb64 (operand-hi o)))
         (run :signed-byte-64))
        (t (generic-predicate name cl-fn o))))))

(defun predicate-call (name cl-fn args)
  "The body of the five predicates: exactly one operand, and :INTO OUT."
  (if (notany #'arrayp args)
      (apply cl-fn args)
      (let* ((nops (into-position args))
             (ops (if nops (subseq args 0 nops) args)))
        (unless (= (length ops) 1)
          (return-from predicate-call (apply cl-fn ops)))     ; CL's own arity error
        (let ((out (and nops (car (last args)))))
          (when (and nops (not (arrayp out))) (error 'type-error :datum out :expected-type 'array))
          (let ((value (if (arrayp (first ops))
                           (with-cl-error-replay (cl-fn ops) (predicate-array name cl-fn (first ops)))
                           (apply cl-fn ops))))
            (if nops (store-bits value out) value))))))

(defmacro define-predicate-operator (name cl-fn doc)
  `(defun ,name (&rest args)
     ,doc
     (declare (dynamic-extent args))
     (predicate-call ,(string-downcase (symbol-name cl-fn)) #',cl-fn args)))

(define-predicate-operator vm:zerop cl:zerop
  "Like ZEROP for numbers; a bit array for arrays (1 where the element is zero; a NaN is not).")
(define-predicate-operator vm:plusp cl:plusp
  "Like PLUSP for numbers; a bit array for arrays.  Complex elements signal CL's type error.")
(define-predicate-operator vm:minusp cl:minusp
  "Like MINUSP for numbers; a bit array for arrays.  Complex elements signal CL's type error.")
(define-predicate-operator vm:evenp cl:evenp
  "Like EVENP for numbers; a bit array for integer arrays (any other element type signals CL's type error).")
(define-predicate-operator vm:oddp cl:oddp
  "Like ODDP for numbers; a bit array for integer arrays.")

;;; ---------------------------------------------------------------------------
;;; MAX and MIN
;;;
;;; The result element type is the contagion type of the operands: integer arrays give
;;; (signed-byte 64) arrays (T when a value does not fit), a float operand makes the result
;;; single or double float (the double if either is), rationals and element type T give what
;;; CL's MAX returns per element.  A NaN anywhere gives a NaN (like R's pmax/pmin).  Complex
;;; operands signal CL's type error.

(defun float-proto (x y)
  "1d0 when either of the numbers X, Y is a double-float, 1f0 when either is a single, else NIL."
  (cond ((or (typep x 'double-float) (typep y 'double-float)) 1d0)
        ((or (typep x 'single-float) (typep y 'single-float)) 1f0)
        (t nil)))

(defun maxmin-elements (maxp x y)
  "MAX (MAXP true) or MIN of the numbers X and Y with the array rules: a NaN propagates; the
winner has the contagion type."
  (let ((proto (float-proto x y)))
    (cond ((and (floatp x) (float-nan-p x)) (if proto (float x proto) x))
          ((and (floatp y) (float-nan-p y)) (if proto (float y proto) y))
          (t (let ((m (if maxp (cl:max x y) (cl:min x y))))
               (if proto (float m proto) m))))))

(defun maxmin-scalars (maxp x y)
  (with-masked-float-traps (maxmin-elements maxp x y)))

(defun maxmin-key (a b)
  "(values KEY BASE) of the typed kernel for operands A and B, or NIL for the generic path."
  (let ((ba (operand-base a)) (bb (operand-base b)))
    (cond ((or (member ba '(:gen)) (member bb '(:gen))) nil)
          ((and (eq ba :int) (eq bb :int))
           (and (fits-sb64 (operand-lo a)) (fits-sb64 (operand-hi a))
                (fits-sb64 (operand-lo b)) (fits-sb64 (operand-hi b))
                (values :signed-byte-64 :int)))
          ((or (eq ba :d) (eq bb :d)) (values :double-float :d))
          ((or (eq ba :s) (eq bb :s)) (values :single-float :s))
          (t nil))))

(defun maxmin-step (opname maxp a b)
  "(OPNAME A B) for operands A and B (at least one an array, or both scalars): an operand."
  (if (and (not (operand-array-p a)) (not (operand-array-p b)))
      (classify-scalar (maxmin-scalars maxp (operand-value a) (operand-value b)))
      (let ((shape (shape-of-pair a b)))
        (multiple-value-bind (key base) (maxmin-key a b)
          (if key
              (let ((result (run-binary a b key key key opname key shape)))
                (if (eq key :signed-byte-64)
                    (make-operand result :int nil
                                  (min (operand-lo a) (operand-lo b)) (max (operand-hi a) (operand-hi b)))
                    (make-operand result base nil)))
              (classify (generic-binary (lambda (x y) (maxmin-elements maxp x y)) a b shape
                                        :narrow t :empty-key :t)))))))

(defun maxmin-arrays (opname maxp args)
  (let ((ops (classify-real-args args nil)))
    (dolist (o ops) (when (operand-cplx o) (complex-operand-error o)))
    (if (null (rest ops))
        (operand-value (copy-operand-array (first ops)))
        (let ((acc (first ops)))
          (dolist (o (rest ops)) (setf acc (maxmin-step opname maxp acc o)))
          (operand-value acc)))))

(defun maxmin-operator (opname cl-fn args)
  (flet ((arrays (ops)
           (with-cl-error-replay (cl-fn ops) (maxmin-arrays opname (string= opname "max") ops))))
    (if (notany #'arrayp args)
        (apply cl-fn args)
        (let ((nops (into-position args)))
          (if nops
              (let ((out (car (last args))) (ops (subseq args 0 nops)))
                (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
                (store-into (if (some #'arrayp ops) (arrays ops) (apply cl-fn ops)) out))
              (arrays args))))))

(defun vm:max (&rest args)
  "Like MAX for numbers.  When any argument is an array: the elementwise maximum (R's pmax) with
R's recycling rules and the contagion element type.  A NaN gives a NaN.  :INTO OUT as for VM:+."
  (declare (dynamic-extent args))
  (maxmin-operator "max" #'cl:max args))

(defun vm:min (&rest args)
  "Like MIN for numbers; elementwise minimum for arrays; see VM:MAX."
  (declare (dynamic-extent args))
  (maxmin-operator "min" #'cl:min args))

;;; --- method combinations (built-in names in CL) ---
(define-method-combination vm:max :identity-with-one-argument t)
(define-method-combination vm:min :identity-with-one-argument t)
