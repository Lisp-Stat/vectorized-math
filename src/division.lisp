;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; VM:FLOOR VM:CEILING VM:TRUNCATE VM:ROUND          two values, quotient an integer
;;; VM:FFLOOR VM:FCEILING VM:FTRUNCATE VM:FROUND      two values, quotient a float
;;; VM:MOD VM:REM                                     one value (the remainder)
;;; VM:GCD VM:LCM                                     n-ary, integers only
;;;
;;; Numbers: exactly CL (the CL function is applied).  When an operand is an array the operator is
;;; applied per element with the R shape rules (shape.lisp) and the result is TWO arrays (quotient,
;;; remainder) of the result shape, or one for MOD, REM, GCD and LCM.
;;;
;;; The rule behind every array result: an element is whatever CL's own function returns for the
;;; pair of elements, in the caller's float trap state.  Two paths give it:
;;;   * typed kernels (integer arrays within (signed-byte 64), double and single float arrays) that
;;;     call the CL function itself, notinline, on unboxed elements, after checking that the element
;;;     is an ordinary one (below).  A kernel that meets an element it does not handle reports so and
;;;     the call is redone on the generic path, so the result never depends on the path.
;;;   * the generic path: the CL function per element on the boxed elements.
;;; Typed loops are used only where this is the same computation as CL's: there is no MKL path
;;; (v?Round rounds half away from zero, v?Floor and friends have no remainder and no integer
;;; quotient).
;;;
;;; Zero divisors and non-finite values.  FLOOR CEILING TRUNCATE ROUND need an integer quotient,
;;; which Inf and NaN cannot give, so they call CL's function in the caller's trap state and CL
;;; signals what it signals for that scalar (the same condition on both implementations for an
;;; unmasked trap; with the traps masked CL's own behaviour differs between implementations and is
;;; reproduced as it is).  Integer division by zero signals DIVISION-BY-ZERO as in CL.  The F family
;;; and MOD, REM on float elements give IEEE results and no signal, defined for the pairs (x, y)
;;; that CL's computation cannot handle (IEEE-DIVISION-SPECIAL below).  Everything else is CL's
;;; function.

(in-package #:vm.impl)

;;; ---------------------------------------------------------------------------
;;; the operator table: (name cl-function mode kind)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +division-ops+
    '(("floor" cl:floor :floor :int-q) ("ceiling" cl:ceiling :ceiling :int-q)
      ("truncate" cl:truncate :truncate :int-q) ("round" cl:round :round :int-q)
      ("ffloor" cl:ffloor :floor :float-q) ("fceiling" cl:fceiling :ceiling :float-q)
      ("ftruncate" cl:ftruncate :truncate :float-q) ("fround" cl:fround :round :float-q)
      ("mod" cl:mod :floor :rem) ("rem" cl:rem :truncate :rem))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun division-spec (name)
    (or (find name +division-ops+ :key #'first :test #'string=)
        (error "unknown division operator ~S" name))))

;;; ---------------------------------------------------------------------------
;;; typed kernels
;;;
;;; Kernel (NAME KEY :aa)  (qout rout ooff a aoff b boff n); :as (qout rout ooff a aoff s n);
;;; :sa (qout rout ooff s b boff n).  QOUT is NIL for MOD and REM.  The kernels return true when
;;; they processed every element, NIL as soon as one is not an ordinary element (the outputs are
;;; then garbage).  Elements are ordinary when
;;;   integers:       y /= 0 and the quotient fits (not most-negative / -1)
;;;   integer family: x/y is a number of magnitude below 4e18 (the integer quotient fits 64 bits)
;;;                   and y is finite
;;;   other floats:   x/y is finite and y is finite (hence x is finite and y /= 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun division-kernel-types (kind key)
    "(values element-type quotient-type) of the kernels of KIND for KEY."
    (let ((xtype (ecase key
                   (:signed-byte-64 '(signed-byte 64))
                   (:double-float 'double-float)
                   (:single-float 'single-float))))
      (values xtype
              (ecase kind
                (:int-q '(signed-byte 64))
                (:float-q (if (eq key :signed-byte-64) 'single-float xtype))
                ((:rem :gcd :lcm) nil)))))

  (defun division-check-condition (kind key)
    "A condition over the variables X and Y (and, for floats, the variable TQ, bound to X/Y of the
element type) that is true for an ordinary element."
    (if (eq key :signed-byte-64)
        (ecase kind
          ((:int-q :float-q :rem) `(not (or (= y 0) (and (= y -1) (= x ,(- (expt 2 63)))))))
          ((:gcd) `(and (/= x ,(- (expt 2 63))) (/= y ,(- (expt 2 63)))))
          ((:lcm) t))
        (let* ((double (eq key :double-float))
               (type (if double 'double-float 'single-float))
               (big (if double most-positive-double-float most-positive-single-float))
               (limit (ecase kind
                        (:int-q (if double 4d18 4f18))
                        ((:float-q :rem) big))))
          `(and (not (nan-p tq ,type))
                ,(if (eq kind :int-q) `(< (abs tq) ,limit) `(<= (abs tq) ,limit))
                (<= (abs y) ,big)))))

  (defun division-check-form (kind key)
    "A form over the variables X and Y that is true for an ordinary element."
    (if (eq key :signed-byte-64)
        (division-check-condition kind key)
        (let ((type (if (eq key :double-float) 'double-float 'single-float)))
          `(let ((tq (/ x y)))
             (declare (type ,type tq))
             ,(division-check-condition kind key))))))

;;; ---------------------------------------------------------------------------
;;; CCL: non-consing kernels for float elements
;;;
;;; CCL's TRUNCATE FLOOR CEILING ROUND MOD REM and the F functions are full calls that box their float
;;; arguments and cons their results (about 300 ns per element).  For an ordinary element whose
;;; quotient is below 2^52 in magnitude the kernels below repeat CCL's own computation inline, with
;;; the same operations in the same order (see CCL's TRUNCATE in level-0/l0-numbers.lisp):
;;;     tq  = x / y                         (float division)
;;;     q   = the integer truncation of tq, as a fixnum (zero for 0 < |tq| < 1: an integer 0)
;;;     rem = x - float(q) * y              (float multiplication, then float subtraction)
;;; and then the adjustment of FLOOR, CEILING or ROUND exactly as CCL's FLOOR, CEILING and ROUND do it
;;; (including the signs of zeros: float(0) is +0.0 whatever the sign of tq).  ROUND chooses the even
;;; neighbour for an exact tie.  Every other element (|tq| >= 2^52) goes to CL's function itself.
;;;
;;; The truncation is done without conversion instructions CCL does not expose: for |tq| < 2^52,
;;; |tq| + 2^52 is a float whose 52 low mantissa bits are the integer nearest to |tq|; one is subtracted
;;; when that integer is larger than |tq|.  The bits are read back through the stack scratch block.
;;;
;;; This is a re-implementation of CCL's algorithm, so it is verified rather than assumed: at load
;;; time FAST-DIVISION-SELFTEST (below) compares each fast kernel with the plain one on a corpus and
;;; registers it only when every result is identical (EQL: signed zeros included); the tests repeat
;;; this on a much larger corpus (tests/division.lisp).

#+(and ccl little-endian-target)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun division-fast-body (mode kind key cl-call)
    "The body of a kernel's element loop (over the variables X Y, writing element I) for the operation
of MODE (:truncate :floor :ceiling :round) of KIND (:int-q :float-q :rem), floats of KEY."
    (let* ((double (eq key :double-float))
           (ftype (if double 'double-float 'single-float))
           (zero (if double 0d0 0f0))
           (one (if double 1d0 1f0))
           (big (if double most-positive-double-float most-positive-single-float))
           (two52 4503599627370496d0)
           (check (division-check-condition kind key))
           (store (ecase kind
                    (:int-q `(setf (aref qout (the fixnum (+ ooff i))) q
                                   (aref rout (the fixnum (+ ooff i))) rem))
                    (:float-q `(setf (aref qout (the fixnum (+ ooff i))) (float q ,one)
                                     (aref rout (the fixnum (+ ooff i))) rem))
                    (:rem `(setf (aref rout (the fixnum (+ ooff i))) rem))))
           (slow (ecase kind
                   ((:int-q :float-q)
                    `(multiple-value-bind (q r) ,cl-call
                       (setf (aref qout (the fixnum (+ ooff i))) (the ,(if (eq kind :int-q) '(signed-byte 64) ftype) q)
                             (aref rout (the fixnum (+ ooff i))) (the ,ftype r))))
                   (:rem `(setf (aref rout (the fixnum (+ ooff i))) (the ,ftype ,cl-call)))))
           (adjust
             (ecase mode
               (:truncate nil)
               (:floor `((when (and (/= rem ,zero) (if (< y ,zero) (> x ,zero) (< x ,zero)))
                           (setq q (1- q) rem (+ rem y)))))
               (:ceiling `((when (and (/= rem ,zero) (if (< y ,zero) (< x ,zero) (> x ,zero)))
                            (setq q (1+ q) rem (- rem y)))))
               (:round `((when (/= rem ,zero)
                           (let* ((minusp-num (< x ,zero)) (minusp-div (< y ,zero))
                                  (2rem (if (eq minusp-num minusp-div) (* rem ,(if double 2d0 2f0)) (* rem ,(if double -2d0 -2f0)))))
                             (declare (type ,ftype 2rem))
                             (if minusp-div
                                 (when (or (< 2rem y) (and (= 2rem y) (logbitp 0 q)))
                                   (if minusp-num
                                       (setq q (1+ q) rem (- rem y))
                                       (setq q (1- q) rem (+ rem y))))
                                 (when (or (> 2rem y) (and (= 2rem y) (logbitp 0 q)))
                                   (if minusp-num
                                       (setq q (1- q) rem (+ rem y))
                                       (setq q (1+ q) rem (- rem y))))))))))))
      `((let* ((tq (/ x y))
               (atq (if (< tq ,zero) (- tq) tq))
               (ay (if (< y ,zero) (- y) y))
               (m (+ ,(if double 'atq '(float atq 1d0)) ,two52)))
          (declare (type ,ftype tq atq ay) (type double-float m))
          (setf (ccl::%get-double-float %nan-scratch 0) m)
          (let ((hi (ccl::%get-unsigned-long %nan-scratch 4))
                (lo (ccl::%get-unsigned-long %nan-scratch 0)))
            (declare (type (unsigned-byte 32) hi lo))
            ;; the exponent of M is that of 2^52 exactly when 0 <= |tq| < 2^52 - 0.5 (a NaN or an
            ;; infinity has exponent 2047); |y| finite excludes an infinite divisor (tq = 0 then)
            (if (and (= (ash hi -20) #x433) (<= ay ,big))
                (let* ((ri (the fixnum (+ (the fixnum (ash (the (unsigned-byte 20) (logand hi #xFFFFF)) 32)) lo)))
                       (q 0)
                       (rem ,zero))
                  (declare (type fixnum ri q) (type ,ftype rem))
                  (when (> (the double-float (float ri 1d0)) (the double-float ,(if double 'atq '(float atq 1d0))))
                    (setq ri (the fixnum (1- ri))))
                  (setq q (if (< tq ,zero) (the fixnum (- ri)) ri))
                  (setq rem (- x (* (the ,ftype (float q ,one)) y)))
                  ,@adjust
                  ,store)
                (progn
                  (unless (let ((tq (/ x y))) (declare (type ,ftype tq)) ,check) (return-from kernel nil))
                  ,slow)))))))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun division-fast-p (kind key)
    "True when there is a non-consing variant of the kernels of KIND for float arrays of KEY."
    (and (member key '(:double-float :single-float)) (member kind '(:int-q :float-q :rem)) t)))

(defmacro def-division-kernel (name cl-fn kind key &key mode fast)
  "The kernels (:aa :as :sa) of the operation NAME (CL function CL-FN) for arrays of KEY.  FAST (CCL, float
KEY): the non-consing variant of division-fast-body, named and registered separately (FAST-DIVISION-REGISTER)."
  (declare (ignorable mode))
  (multiple-value-bind (xtype qtype) (division-kernel-types kind key)
    (let* ((tag (if fast 'divf 'div))
           (aa (kname tag name key 'aa)) (as (kname tag name key 'as)) (sa (kname tag name key 'sa))
           (check (division-check-form kind key))
           ;; plain: CL's function itself, as a full call on unboxed elements.  Fast (SBCL): the same
           ;; call open-coded: the arguments are declared floats, so the compiler expands it with its
           ;; own transforms for the CL function (the code its full call runs), without boxing.
           (call (if (and fast #+sbcl t #-sbcl nil)
                     `(,cl-fn x y)
                     `(locally (declare (notinline ,cl-fn)) (,cl-fn x y))))
           (body (if (and fast #+(and ccl little-endian-target) t #-(and ccl little-endian-target) nil)
                     #+(and ccl little-endian-target) (division-fast-body mode kind key call)
                     #-(and ccl little-endian-target) nil
                   (ecase kind
                   ((:int-q :float-q)
                    `((unless ,check (return-from kernel nil))
                      (multiple-value-bind (q r) ,call
                        (setf (aref qout (the fixnum (+ ooff i))) (the ,qtype q)
                              (aref rout (the fixnum (+ ooff i))) (the ,xtype r)))))
                   (:rem
                    `((unless ,check (return-from kernel nil))
                      (setf (aref rout (the fixnum (+ ooff i))) (the ,xtype ,call))))
                   ((:gcd :lcm)
                    `((unless ,check (return-from kernel nil))
                      (let ((v ,call))
                        (unless (typep v '(signed-byte 64)) (return-from kernel nil))
                        (setf (aref rout (the fixnum (+ ooff i))) v)))))))
           (decls `((type (simple-array ,xtype (*)) rout)
                    ,@(if qtype `((type (simple-array ,qtype (*)) qout)) `((ignore qout)))
                    (type fixnum ooff n)
                    (optimize (speed ,(if fast 3 1)) (safety 0) (debug 0))
                    #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)))
           (arr `(simple-array ,xtype (*))))
      `(progn
         (defun ,aa (qout rout ooff a aoff b boff n)
           (declare ,@decls (type ,arr a b) (type fixnum aoff boff))
           (block kernel
             (with-nan-scratch
               (dotimes (i n)
                 (let ((x (aref a (the fixnum (+ aoff i)))) (y (aref b (the fixnum (+ boff i)))))
                   ,@body)))
             t))
         (defun ,as (qout rout ooff a aoff s n)
           (declare ,@decls (type ,arr a) (type ,xtype s) (type fixnum aoff))
           (block kernel
             (let ((y s))
               (with-nan-scratch
                 (dotimes (i n)
                   (let ((x (aref a (the fixnum (+ aoff i)))))
                     ,@body))))
             t))
         (defun ,sa (qout rout ooff s b boff n)
           (declare ,@decls (type ,arr b) (type ,xtype s) (type fixnum boff))
           (block kernel
             (let ((x s))
               (with-nan-scratch
                 (dotimes (i n)
                   (let ((y (aref b (the fixnum (+ boff i)))))
                     ,@body))))
             t))
         ,@(unless fast
             `((register-kernel ,name ,key :aa #',aa)
               (register-kernel ,name ,key :as #',as)
               (register-kernel ,name ,key :sa #',sa)))))))

(defmacro def-division-kernels ()
  `(progn
     ,@(loop for (name cl-fn nil kind) in +division-ops+
             append (loop for key in '(:signed-byte-64 :double-float :single-float)
                          collect `(def-division-kernel ,name ,cl-fn ,kind ,key)))
     #+(or sbcl (and ccl little-endian-target))
     ,@(loop for (name cl-fn mode kind) in +division-ops+
             append (loop for key in '(:double-float :single-float)
                          when (division-fast-p kind key)
                            collect `(def-division-kernel ,name ,cl-fn ,kind ,key :mode ,mode :fast t)))
     (def-division-kernel "gcd" cl:gcd :gcd :signed-byte-64)
     (def-division-kernel "lcm" cl:lcm :lcm :signed-byte-64)))

(def-division-kernels)

#+(or sbcl (and ccl little-endian-target))
(progn
  (defvar *fast-division-failures* '()
    "(name key) of the fast division kernels that FAST-DIVISION-SELFTEST rejected (normally empty).")

  (defun fast-division-corpus (key n)
    "Two simple arrays of N floats of KEY: pairs chosen to exercise ordinary and borderline elements
(exact ties for ROUND, exact multiples, signed zeros, tiny and large quotients), from a fixed
generator, so that every load tests the same elements."
    (let* ((double (eq key :double-float))
           (type (if double 'double-float 'single-float))
           (xs (make-array n :element-type type)) (ys (make-array n :element-type type))
           (state 12345))
      (flet ((rnd (k) (setf state (mod (+ (* state 25214903917) 11) (expt 2 48))) (mod (ash state -8) k))
             (fl (v) (float v (if double 1d0 1f0))))
        (dotimes (i n)
          (let* ((class (mod i 14))
                 (m (fl (1+ (rnd 100000)))) (k (fl (1+ (rnd 1000))))
                 (sx (if (zerop (rnd 2)) 1 -1)) (sy (if (zerop (rnd 2)) 1 -1))
                 (x 0) (y 1))
            (ecase class
              (0 (setf x (/ m 7) y (/ k 3)))                                   ; ordinary
              (1 (setf x (* m k) y k))                                          ; exact multiples
              (2 (setf x (* (+ (fl (rnd 50)) 0.5) k) y k))                      ; exact ties
              (3 (setf x (/ m 1000) y (* k 1000)))                              ; |q| < 1
              (4 (setf x (fl 0) y k))                                                ; zero x (signed below)
              (5 (setf x (/ m 3) y (fl (/ 1 (+ 1 (rnd 8)))) ))                  ; small divisors
              (6 (setf x (* m 1000000) y (/ k 1000)))                           ; large quotients (up to 1e14)
              (7 (setf x (* m m) y (/ k 100000)))                               ; quotients near and above 2^52 for doubles
              (8 (setf x (fl (rnd 1000)) y (fl (+ 1 (rnd 7)))))                 ; small integers
              (9 (setf x (fl (+ (rnd 1000) 0.5)) y 1))                          ; halves, divisor 1
              (10 (setf x (fl (+ (rnd 1000) 0.5)) y 2))
              (11 (setf x (fl (+ (rnd 20) 0.25)) y 0.5))
              (12 (setf x (/ m 3) y (* k (fl (expt 2 (- (rnd 30) 15))))))      ; wide range of divisors
              (13 (setf x (* m (fl (expt 2 (- (rnd 60) 30)))) y (/ k 7))))
            (setf (aref xs i) (fl (* sx x)) (aref ys i) (fl (* sy y))))))
      ;; a few fixed specials
      (let ((specials (list (list 0 1) (list -0f0 1) (list 1 -0.5) (list -1 0.5) (list 0.5 1) (list 1.5 1) (list 2.5 1)
                            (list -0.5 1) (list -1.5 1) (list -2.5 1) (list 5 -2) (list -5 2) (list 7 -3) (list -7 3)
                            (list (if double 1d-300 1f-30) (if double 1d300 1f30)) (list 1 3) (list -1 3) (list 1 -3) (list 1f30 3) (list 3 1f-30))))
        (loop for (a b) in specials for i from 0 below n
              do (setf (aref xs i) (float a (if double 1d0 1f0)) (aref ys i) (float b (if double 1d0 1f0)))))
      (values xs ys)))

  (defun fast-division-agrees-p (name key plain fast)
    "True when the kernels PLAIN and FAST (the :aa variants) give identical results, element by element,
on the corpus, and so do the :as and :sa variants."
    (let* ((n 4000) (type (if (eq key :double-float) 'double-float 'single-float))
           (kind (fourth (division-spec name))))
      (multiple-value-bind (xs ys) (fast-division-corpus key n)
        (let ((q1 (make-array 1 :element-type (if (eq kind :float-q) type '(signed-byte 64))))
              (q2 (make-array 1 :element-type (if (eq kind :float-q) type '(signed-byte 64))))
              (r1 (make-array 1 :element-type type)) (r2 (make-array 1 :element-type type))
              (as (find-kernel name key :as)) (sa (find-kernel name key :sa))
              (fas (symbol-function (kname 'divf name key 'as))) (fsa (symbol-function (kname 'divf name key 'sa))))
          (with-masked-float-traps
            (dotimes (i n t)
              (flet ((same-p (f g &rest args)
                       (setf (aref r1 0) (float 0 (aref xs 0)) (aref r2 0) (float 0 (aref xs 0)))
                       (let ((v1 (apply f q1 r1 0 args)) (v2 (apply g q2 r2 0 args)))
                         (and (eq (and v1 t) (and v2 t))
                              (or (not v1)
                                  (and (eql (aref r1 0) (aref r2 0))
                                       (or (not (member kind '(:int-q :float-q))) (eql (aref q1 0) (aref q2 0)))))))))
                (let ((x (aref xs i)) (y (aref ys i)))
                  (unless (and (same-p plain fast xs i ys i 1)
                               (same-p as fas xs i y 1)
                               (same-p sa fsa x ys i 1))
                    (return nil))))))))))

  (defun fast-division-selftest ()
    "Register the fast division kernels that agree with the plain ones (see the comment above
DIVISION-FAST-BODY); record the others in *FAST-DIVISION-FAILURES*.  Returns the failures."
    (setf *fast-division-failures* '())
    (loop for (name nil nil kind) in +division-ops+
          do (dolist (key '(:double-float :single-float))
               (when (division-fast-p kind key)
                 (let ((plain (find-kernel name key :aa))
                       (fast (symbol-function (kname 'divf name key 'aa))))
                   (if (fast-division-agrees-p name key plain fast)
                       (dolist (v '(:aa :as :sa))
                         (register-kernel name key v (symbol-function (kname 'divf name key (symbol-name v)))))
                       (push (list name key) *fast-division-failures*))))))
    (when *fast-division-failures*
      (warn "VECTORIZED-MATH: the non-consing division kernels ~S disagree with this implementation's floor/round/...; using the plain kernels for them."
            *fast-division-failures*))
    *fast-division-failures*)

  (fast-division-selftest))

;;; ---------------------------------------------------------------------------
;;; running a two-output kernel over the R shapes (the counterpart of RUN-BINARY)

(defun run-binary2 (a b key qkey rkey kernel-name shape)
  "Run the kernel (KERNEL-NAME KEY) over operands A and B (at least one an array) combined according
to SHAPE.  Returns (list Q R) of fresh arrays of element types QKEY (NIL: no quotient, Q is NIL) and
RKEY, or NIL when the kernel met an element it does not handle."
  (destructuring-bind (dims n path extra) shape
    (declare (type fixnum n))
    (let* ((q (and qkey (make-result qkey dims))) (r (make-result rkey dims)))
      (if (eq path :empty)
          (list q r)
          (let* ((kaa (find-kernel kernel-name key :aa))
                 (kas (find-kernel kernel-name key :as))
                 (ksa (find-kernel kernel-name key :sa))
                 (va (operand-value a)) (vb (operand-value b))
                 (da (if (arrayp va) (prepare-data va key) (coerce-scalar va key)))
                 (db (if (arrayp vb) (prepare-data vb key) (coerce-scalar vb key)))
                 (qd (and q (flat-data q))) (rd (flat-data r)))
            (flet ((run ()
                     (macrolet ((call (&rest args) `(unless (funcall ,@args) (return-from run nil))))
                       (ecase path
                         (:scalar-a (call ksa qd rd 0 da db 0 n))
                         (:scalar-b (call kas qd rd 0 da 0 db n))
                         (:flat (call kaa qd rd 0 da 0 db 0 n))
                         (:vec-scalar-a (call ksa qd rd 0 (aref da 0) db 0 n))
                         (:vec-scalar-b (call kas qd rd 0 da 0 (aref db 0) n))
                         (:blocks
                          (let* ((m (if extra (length (the vector db)) (length (the vector da)))))
                            (loop for k of-type fixnum from 0 below n by m
                                  do (let ((c (min m (- n k))))
                                       (if extra
                                           (call kaa qd rd k da k db 0 c)
                                           (call kaa qd rd k da 0 db k c))))))
                         (:cyclic
                          (if extra
                              (call kaa qd rd 0 da 0 (expand-cyclic db (length (the vector db)) n) 0 n)
                              (call kaa qd rd 0 (expand-cyclic da (length (the vector da)) n) 0 db 0 n)))
                         (:rows
                          (let* ((d0 (first dims)) (blk (floor n d0)))
                            (dotimes (i d0)
                              (if extra
                                  (call kas qd rd (* i blk) da (* i blk) (aref db i) blk)
                                  (call ksa qd rd (* i blk) (aref da i) db (* i blk) blk)))))
                         (:colmajor
                          (if extra
                              (call kaa qd rd 0 da 0 (expand-colmajor db (length (the vector db)) dims) 0 n)
                              (call kaa qd rd 0 (expand-colmajor da (length (the vector da)) dims) 0 db 0 n))))
                       t)))
              (and (if (eq key :signed-byte-64)
                       (run)
                       (with-masked-float-traps (run)))
                   (list q r))))))))

;;; ---------------------------------------------------------------------------
;;; which typed kernel (if any) handles the operands

(defconstant +single-exact-limit+ (expt 2 24))

(defun division-key (a b)
  "The element-type key of the typed kernel that computes exactly what CL computes for operands A
and B (integers within (signed-byte 64); floats with integers that convert exactly), else NIL."
  (let ((ba (operand-base a)) (bb (operand-base b)))
    (cond
      ((or (member ba '(:rat :gen)) (member bb '(:rat :gen))) nil)
      ((and (eq ba :int) (eq bb :int))
       (and (fits-sb64 (operand-lo a)) (fits-sb64 (operand-hi a))
            (fits-sb64 (operand-lo b)) (fits-sb64 (operand-hi b))
            :signed-byte-64))
      (t
       (let* ((double (or (eq ba :d) (eq bb :d)))
              (limit (if double +double-exact-limit+ +single-exact-limit+)))
         (and (flet ((ok (o) (or (not (eq (operand-base o) :int))
                                 (multiple-value-bind (lo hi) (operand-int-range o)
                                   (<= (- limit) lo hi limit)))))
                (and (ok a) (ok b)))
              (if double :double-float :single-float)))))))

(defun division-result-keys (kind a b)
  "(values quotient-key remainder-key) of the result arrays of the operation KIND for operands A and B."
  (let* ((ba (operand-base a)) (bb (operand-base b))
         (base (combine-base ba bb))
         (rkey (case base
                 ((:s :d) (base-cplx-key base nil))
                 (:int :signed-byte-64)      ; integers: (signed-byte 64) when they fit, as for + (an empty result does)
                 (t :t))))
    (values (ecase kind
              (:int-q :signed-byte-64)
              (:float-q (if (member base '(:s :d)) rkey :single-float))
              (:rem nil))
            rkey)))

;;; ---------------------------------------------------------------------------
;;; the generic path

(defun generic-binary2 (fn a b shape masked)
  "FN (returning two values) applied to the pairs of elements of operands A and B combined according
to SHAPE.  Returns two simple-vectors of the first and second values.  The float traps are masked
during the calls when MASKED."
  (destructuring-bind (dims n path extra) shape
    (let ((tq (make-array n)) (tr (make-array n)) (ga (operand-accessor a)) (gb (operand-accessor b)))
      (unless (eq path :empty)
        (let ((idx (make-pair-indexer a b dims n path extra)))
          (flet ((run ()
                   (dotimes (r n)
                     (multiple-value-bind (ia ib) (funcall idx r)
                       (multiple-value-bind (q rem) (funcall fn (funcall ga ia) (funcall gb ib))
                         (setf (svref tq r) q (svref tr r) rem))))))
            (if masked (with-masked-float-traps (run)) (run)))))
      (values tq tr))))

;;; --- float elements: CL's function for ordinary pairs, IEEE results for the others ---

(defun float-finite-p (x)
  (and (not (float-nan-p x))
       (<= (abs x) (if (typep x 'double-float) most-positive-double-float most-positive-single-float))))

(defun regular-float-pair-p (x y)
  "True when CL's own computation of X and Y (floats of one type) raises no exception: both finite,
Y not zero, X/Y finite."
  (and (float-finite-p x) (float-finite-p y) (not (zerop y)) (float-finite-p (/ x y))))

(defun ieee-division-special (mode x y)
  "(values quotient remainder) for the floats X and Y (one type) that REGULAR-FLOAT-PAIR-P rejects,
as IEEE arithmetic gives them, without a signal; MODE is :truncate, :floor, :ceiling or :round.
  NaN in X or Y                      NaN, NaN
  Y = 0                              X/Y (Inf with the sign of X*Y, NaN when X = 0), NaN
  X = Inf                            X/Y (Inf, NaN when Y is infinite too), NaN
  X finite, Y = Inf                  as the limit: remainder X; the quotient is the zero X/Y, except
                                     that floor (ceiling) of a quotient of the signs that would make
                                     it negative (positive) is -1 (+1) with remainder Y (-Y)
  X, Y finite, X/Y overflows         quotient X/Y (Inf), remainder exact (computed with rationals)"
  (let ((nan (if (typep x 'double-float) *dnan* *snan*))
        (one (float 1 x)))
    (cond ((or (float-nan-p x) (float-nan-p y)) (values nan nan))
          ((zerop y) (values (/ x y) nan))
          ((not (float-finite-p x)) (values (/ x y) nan))
          ((not (float-finite-p y))
           (if (zerop x)
               (values (/ x y) x)
               (let ((same (plusp (* (float-sign x) (float-sign y)))))
                 (ecase mode
                   ((:truncate :round) (values (/ x y) x))
                   (:floor (if same (values (/ x y) x) (values (- one) y)))
                   (:ceiling (if same (values one (- y)) (values (/ x y) x)))))))
          (t
           (let ((rx (rational x)) (ry (rational y)))
             (multiple-value-bind (q r)
                 (ecase mode
                   (:truncate (truncate rx ry))
                   (:floor (floor rx ry))
                   (:ceiling (ceiling rx ry))
                   (:round (round rx ry)))
               (declare (ignore q))
               (values (/ x y) (float r x))))))))

(defun to-float-or-nil (v proto)
  (handler-case (float v proto) (error () nil)))

(defun float-division-element (spec x y)
  "(values quotient remainder) of the operation SPEC (F family, MOD, REM) for the elements X and Y;
the quotient is NIL for MOD and REM."
  (destructuring-bind (name cl-fn mode kind) spec
    (declare (ignore name))
    (flet ((plain ()
             (if (eq kind :rem) (values nil (funcall cl-fn x y)) (funcall cl-fn x y))))
      (if (not (or (floatp x) (floatp y)))
          (plain)
          (let* ((proto (float-proto x y))
                 (xf (to-float-or-nil x proto)) (yf (to-float-or-nil y proto)))
            (cond ((not (and xf yf)) (plain))       ; CL signals its own error
                  ((regular-float-pair-p xf yf)
                   (if (eq kind :rem) (values nil (funcall cl-fn xf yf)) (funcall cl-fn xf yf)))
                  (t (multiple-value-bind (q r) (ieee-division-special mode xf yf)
                       (values (and (not (eq kind :rem)) q) r)))))))))

;;; --- the operators ---

(defun division-arrays (spec number &optional (divisor 1))
  "SPEC (one of +DIVISION-OPS+) applied to NUMBER and DIVISOR, at least one an array.  Returns
(values quotient remainder) as arrays or, for the kind :REM, (values NIL remainder)."
  (destructuring-bind (name cl-fn mode kind) spec
    (declare (ignore mode))
    (let ((ops (list number divisor)))
      (dolist (x ops)
        (unless (or (arrayp x) (numberp x))
          (replay-scalar-error cl-fn ops)
          (not-a-number x)))
      (let* ((a (classify number)) (b (classify divisor)))
        (dolist (o (list a b)) (when (operand-cplx o) (complex-operand-error o)))
        (let* ((shape (shape-of a b))
               (path (third shape))
               (key (and (not (eq path :empty)) (division-key a b))))
          (multiple-value-bind (qkey rkey) (division-result-keys kind a b)
            (let ((typed (and key
                              (run-binary2 a b key qkey rkey name shape))))
              (cond
                (typed (values (first typed) (second typed)))
                ((eq path :empty)
                 (values (and qkey (make-result qkey (first shape))) (make-result rkey (first shape))))
                (t
                 (multiple-value-bind (tq tr)
                     (generic-binary2 (if (eq kind :int-q)
                                          cl-fn
                                          (lambda (x y) (float-division-element spec x y)))
                                      a b shape (not (eq kind :int-q)))
                   (let ((dims (first shape))
                         (int-int (and (eq (operand-base a) :int) (eq (operand-base b) :int))))
                     (values (and qkey (normalize-results tq dims (eq kind :int-q) qkey))
                             (normalize-results tr dims int-int rkey)))))))))))))

(defun two-valued-operator (spec args)
  "The body of FLOOR CEILING TRUNCATE ROUND and the F family: ARGS are (number [divisor] [:into out]).
The :INTO array receives the quotient (first value); the remainder is always a fresh array."
  (let ((cl-fn (symbol-function (second spec))))
    (if (notany #'arrayp args)
        (apply cl-fn args)
        (let* ((nops (into-position args))
               (ops (if nops (subseq args 0 nops) args))
               (count (length ops)))
          (when (or (< count 1) (> count 2))
            (return-from two-valued-operator (apply cl-fn ops)))     ; CL's own arity error
          (let ((out (and nops (car (last args)))))
            (when (and nops (not (arrayp out))) (error 'type-error :datum out :expected-type 'array))
            (multiple-value-bind (q r)
                (if (some #'arrayp ops)
                    (with-cl-error-replay (cl-fn ops) (apply #'division-arrays spec ops))
                    (apply cl-fn ops))
              (if nops
                  (values (store-into q out) r)
                  (values q r))))))))

(defun one-valued-operator (spec args)
  "The body of MOD and REM: exactly two operands and [:into out]."
  (let ((cl-fn (symbol-function (second spec))))
    (if (notany #'arrayp args)
        (apply cl-fn args)
        (let* ((nops (into-position args))
               (ops (if nops (subseq args 0 nops) args)))
          (when (/= (length ops) 2)
            (return-from one-valued-operator (apply cl-fn ops)))
          (let ((out (and nops (car (last args)))))
            (when (and nops (not (arrayp out))) (error 'type-error :datum out :expected-type 'array))
            (let ((value (if (some #'arrayp ops)
                             (with-cl-error-replay (cl-fn ops)
                               (nth-value 1 (apply #'division-arrays spec ops)))
                             (apply cl-fn ops))))
              (if nops (store-into value out) value)))))))

(defmacro define-division-operator (name doc)
  "Define the function NAME (a VM symbol) as the operator of the table entry of the same name."
  (let ((spec (division-spec (string-downcase (symbol-name name)))))
    `(defun ,name (&rest args)
       ,doc
       (declare (dynamic-extent args))
       (,(if (eq (fourth spec) :rem) 'one-valued-operator 'two-valued-operator) ',spec args))))

(define-division-operator vm:floor
  "Like FLOOR for numbers.  When an argument is an array: two values, both arrays of the R-rules result
shape: the quotient (integers: (signed-byte 64) when every one fits, else element type T) and the
remainder (the contagion type of the operands).  A zero divisor or a non-finite float signals what CL
signals for that scalar.  :INTO OUT receives the quotient; the remainder is a fresh array.")
(define-division-operator vm:ceiling
  "Like CEILING for numbers; two arrays for arrays, as VM:FLOOR.")
(define-division-operator vm:truncate
  "Like TRUNCATE for numbers; two arrays for arrays, as VM:FLOOR.")
(define-division-operator vm:round
  "Like ROUND for numbers (ties to even); two arrays for arrays, as VM:FLOOR.")
(define-division-operator vm:ffloor
  "Like FFLOOR for numbers.  For arrays two arrays: the quotient as a float (CL's float type for the
operands: integers and rationals give single-float), the remainder.  Float elements give IEEE results and no signal: x/0,
Inf and NaN (see IEEE-DIVISION-SPECIAL).  :INTO OUT receives the quotient.")
(define-division-operator vm:fceiling "Like FCEILING for numbers; two arrays for arrays, as VM:FFLOOR.")
(define-division-operator vm:ftruncate "Like FTRUNCATE for numbers; two arrays for arrays, as VM:FFLOOR.")
(define-division-operator vm:fround "Like FROUND for numbers; two arrays for arrays, as VM:FFLOOR.")
(define-division-operator vm:mod
  "Like MOD for numbers; the remainder array (contagion type) for arrays, with the IEEE rules of
VM:FFLOOR for float elements.  :INTO OUT as for VM:+.")
(define-division-operator vm:rem
  "Like REM for numbers; the remainder array for arrays, as VM:MOD.")

;;; ---------------------------------------------------------------------------
;;; GCD and LCM

(defun check-integer-operands (ops)
  "Signal CL's type error (expected type INTEGER) for the first operand that cannot hold integers: a
non-integer scalar, a float or complex array."
  (dolist (o ops)
    (let ((v (operand-value o)))
      (cond ((not (arrayp v))
             (unless (integerp v) (error 'type-error :datum v :expected-type 'integer)))
            ((or (member (operand-base o) '(:s :d)))
             (error 'type-error
                    :datum (if (plusp (array-total-size v)) (row-major-aref v 0) v)
                    :expected-type 'integer))))))

(defun gcd-lcm-step (name fn a b)
  "(FN A B) for operands A and B, at least one an array (else both scalars): an operand."
  (if (and (not (operand-array-p a)) (not (operand-array-p b)))
      (classify-scalar (funcall fn (operand-value a) (operand-value b)))
      (let* ((shape (shape-of a b))
             (both-int (and (eq (operand-base a) :int) (eq (operand-base b) :int)))
             (typed (and both-int (not (eq (third shape) :empty))
                         (fits-sb64 (operand-lo a)) (fits-sb64 (operand-hi a))
                         (fits-sb64 (operand-lo b)) (fits-sb64 (operand-hi b))
                         (run-binary2 a b :signed-byte-64 nil :signed-byte-64 name shape))))
        (if typed
            (make-operand (second typed) :int nil +sb64-min+ +sb64-max+)
            (classify (generic-binary fn a b shape :narrow t
                                      :empty-key (if both-int :signed-byte-64 :t)))))))

(defun gcd-lcm-arrays (name cl-fn &rest args)
  "GCD or LCM of ARGS, at least one an array: the result array (or number when only scalars remain)."
  (let ((ops (mapcar #'classify args)))
    (check-integer-operands ops)
    (if (null (rest ops))
        (let ((o (first ops)))
          (generic-unary cl-fn o :narrow t :empty-key (if (eq (operand-base o) :int) :signed-byte-64 :t)))
        (let ((acc (first ops)))
          (dolist (o (rest ops)) (setf acc (gcd-lcm-step name cl-fn acc o)))
          (operand-value acc)))))

(defun gcd-lcm-operator (name cl-fn args)
  (if (notany #'arrayp args)
      (apply cl-fn args)
      (let ((nops (into-position args)))
        (if nops
            (let ((out (car (last args))) (ops (subseq args 0 nops)))
              (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
              (store-into (if (some #'arrayp ops)
                              (with-cl-error-replay (cl-fn ops) (apply #'gcd-lcm-arrays name cl-fn ops))
                              (apply cl-fn ops))
                          out))
            (with-cl-error-replay (cl-fn args) (apply #'gcd-lcm-arrays name cl-fn args))))))

(defun vm:gcd (&rest args)
  "Like GCD for numbers.  When any argument is an array: the elementwise greatest common divisor with
R's recycling rules; integer arrays only (anything else signals CL's type error).  The element type is
(signed-byte 64), or T when a result does not fit.  :INTO OUT as for VM:+."
  (declare (dynamic-extent args))
  (gcd-lcm-operator "gcd" #'cl:gcd args))

(defun vm:lcm (&rest args)
  "Like LCM for numbers; the elementwise least common multiple for arrays, as VM:GCD (overflowing
products give element type T)."
  (declare (dynamic-extent args))
  (gcd-lcm-operator "lcm" #'cl:lcm args))
