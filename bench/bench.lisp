;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM-BENCH -*-
;;; Benchmark suite for vectorized-math (not part of the test system).
;;;
;;;   sbcl --no-userinit --non-interactive --dynamic-space-size 8192 --load bench/bench.lisp
;;;   ccl --no-init --batch --load bench/bench.lisp --eval '(ccl:quit)' </dev/null
;;;
;;; Writes one tab-separated line per measurement to stdout (and to $BENCH_OUT when set):
;;;   impl  section  family  op  type  n  mode  ns
;;; ns is nanoseconds per element (array section) or per call (scalar section), the best of
;;; several repetitions.  Modes of the array section:
;;;   base     a typed CL loop that allocates its result(s), the baseline
;;;   vm       the operator as shipped (default MKL cutoffs)
;;;   vm-lisp  the operator with MKL switched off (vm.impl::*mkl-available* NIL): the Lisp kernel
;;;   vm-mkl   the operator with the cutoff of + - * / set to 0: MKL at every size
;;; Scalar section modes: cl-decl vm-decl cl-undecl vm-undecl (compiled calls with declared /
;;; undeclared argument types).
;;; Environment: BENCH_SIZES="1 16 ..." BENCH_FILTER=substring of family/op/type
;;;              BENCH_SECTIONS="array scalar" BENCH_OUT=file.   tools: bench/report.py renders tables.
(load "/home/vscode/quicklisp/setup.lisp")
(push (let ((dir (uiop:getenv "VM_SYSTEM_DIR")))     ; default: the directory above this one
        (if (and dir (plusp (length dir)))
            (uiop:ensure-directory-pathname dir)
            (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))))
      asdf:*central-registry*)
(asdf:load-system :vectorized-math)

(defpackage #:vm-bench (:use #:cl))
(in-package #:vm-bench)

(defparameter *impl* #+ccl "ccl" #+sbcl "sbcl")
(defparameter *out-stream* nil)

(defun env (name &optional default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))

(defun emit (&rest fields)
  (let ((line (format nil "~{~A~^~C~}" (loop for (f . more) on fields collect f when more collect #\Tab))))
    (write-line line) (finish-output)
    (when *out-stream* (write-line line *out-stream*) (finish-output *out-stream*))))

(defvar *timespec* (cffi:foreign-alloc :int64 :count 2))
(defun now ()
  "Monotonic clock in seconds (clock_gettime(CLOCK_MONOTONIC): get-internal-real-time is too coarse)."
  (cffi:foreign-funcall "clock_gettime" :int 1 :pointer *timespec* :int)
  (+ (cffi:mem-aref *timespec* :int64 0) (* 1d-9 (cffi:mem-aref *timespec* :int64 1))))

(defun load-average ()
  (with-open-file (s "/proc/loadavg" :if-does-not-exist nil)
    (and s (read-line s))))

;;; --- timing ---

(defun best-time (thunk &key (target 0.03d0) (budget 1.5d0))
  "Seconds per call of THUNK, best of several repetitions.  Each repetition runs the thunk
enough times to last about TARGET seconds."
  (funcall thunk)                       ; warm up (allocation, MKL load, page faults)
  (let* ((t0 (let ((s (now))) (funcall thunk) (- (now) s)))
         (inner (max 1 (min 100000 (floor target (max t0 1d-7)))))
         (outer (max 2 (min 7 (floor budget (max (* inner t0) 1d-6)))))
         (best nil))
    (dotimes (r outer)
      (let ((s (now)))
        (dotimes (i inner) (funcall thunk))
        (let ((dt (/ (- (now) s) inner)))
          (when (or (null best) (< dt best)) (setf best dt)))))
    (min best t0)))

;;; --- data ---

(defun rnd (lo hi) (+ lo (random (- hi lo))))

(defun etkey (et)
  (cond ((eq et 'double-float) :d) ((eq et 'single-float) :s)
        ((equal et '(complex double-float)) :cd) ((equal et '(complex single-float)) :cs)
        ((equal et '(signed-byte 64)) :i) (t :t)))

(defun gen-elt (et kind)
  (ecase kind
    (:any (ecase (etkey et)
            (:d (rnd -100d0 100d0)) (:s (rnd -100f0 100f0))
            (:cd (complex (rnd -10d0 10d0) (rnd -10d0 10d0)))
            (:cs (complex (rnd -10f0 10f0) (rnd -10f0 10f0)))
            (:i (- (random (expt 2 32)) (expt 2 31)))
            (:t (rnd -100d0 100d0))))
    (:nonzero (ecase (etkey et)
                (:d (let ((v (rnd 0.5d0 50d0))) (if (zerop (random 2)) v (- v))))
                (:s (let ((v (rnd 0.5f0 50f0))) (if (zerop (random 2)) v (- v))))
                (:cd (complex (rnd 1d0 10d0) (rnd -10d0 10d0)))
                (:cs (complex (rnd 1f0 10f0) (rnd -10f0 10f0)))
                (:i (let ((v (1+ (random 1000)))) (if (zerop (random 2)) v (- v))))
                (:t (rnd 0.5d0 50d0))))
    (:pos (ecase (etkey et)
            (:d (rnd 0.1d0 100d0)) (:s (rnd 0.1f0 100f0))
            (:cd (complex (rnd 0.1d0 100d0) (rnd -10d0 10d0)))
            (:cs (complex (rnd 0.1f0 100f0) (rnd -10f0 10f0)))
            (:i (1+ (random 1000)))
            (:t (rnd 0.1d0 100d0))))
    (:unit (ecase (etkey et)
             (:d (rnd -3d0 3d0)) (:s (rnd -3f0 3f0))
             (:cd (complex (rnd -3d0 3d0) (rnd -3d0 3d0)))
             (:cs (complex (rnd -3f0 3f0) (rnd -3f0 3f0)))
             (:i (- (random 7) 3)) (:t (rnd -3d0 3d0))))
    (:floor-x (ecase (etkey et)
                (:d (rnd -1d6 1d6)) (:s (rnd -1f5 1f5))
                (:i (- (random 2000001) 1000000)) (:t 0)))))

(defun gen-array (et kind n)
  (let ((a (make-array n :element-type et)))
    (dotimes (i n a) (setf (aref a i) (gen-elt et kind)))))

(defun array-type (et) (if (eq et t) 'simple-vector `(simple-array ,et (*))))

;;; --- array cases ---

(defstruct bcase family op type kinds ins vm base dual cutoff-op)
(defvar *cases* '())

(defmacro defcase ((family op type) &key (kinds '(:any :any)) ins outs vm base dual cutoff-op)
  "INS / OUTS: element types of the input / output arrays.  VM: a form over arrays A and B.
BASE: a form over the elements X and Y, returning the element (or, for two OUTS, two values)."
  (let ((o1 (first outs)) (o2 (second outs)))
    `(push (make-bcase
            :family ,family :op ,op :type ,type :kinds ',kinds :ins ',ins :dual ,dual :cutoff-op ,cutoff-op
            :vm (lambda (a b) (declare (ignorable a b)) ,vm)
            :base (lambda (n a b)
                    (declare (type fixnum n) (ignorable b)
                             (type ,(array-type (first ins)) a)
                             ,@(when (second ins) `((type ,(array-type (second ins)) b)))
                             (optimize (speed 3) (safety 0) (debug 0))
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                    (let ((o1 (make-array n :element-type ',o1))
                          ,@(when o2 `((o2 (make-array n :element-type ',o2)))))
                      (declare (type ,(array-type o1) o1) ,@(when o2 `((type ,(array-type o2) o2))))
                      (dotimes (i n)
                        (let ((x (aref a i)) ,@(when (second ins) '((y (aref b i)))))
                          (declare (ignorable x))
                          ,(if o2
                               `(multiple-value-bind (q r) ,base (setf (aref o1 i) q (aref o2 i) r))
                               `(setf (aref o1 i) ,base))))
                      o1)))
           *cases*)))

(defmacro vm-fn (name) `(function ,(find-symbol (string name) :vm)))
(defmacro vmc (name &rest args) `(,(find-symbol (string name) :vm) ,@args))

;; arithmetic
(macrolet ((arith (opname op types)
             `(progn
                ,@(loop for (tname et dual) in types
                        collect `(defcase ("arith" ,opname ,tname)
                                   :kinds ,(if (string= opname "/") '(:any :nonzero) '(:any :any))
                                   :ins (,et ,et) :outs (,et) :dual ,dual :cutoff-op ,dual
                                   :vm (vmc ,op a b) :base (,(find-symbol (string op) :cl) x y))))))
  (arith "+" + (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)
                ("sb64" (signed-byte 64) nil) ("T" t nil)))
  (arith "-" - (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)
                ("sb64" (signed-byte 64) nil) ("T" t nil)))
  (arith "*" * (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)
                ("sb64" (signed-byte 64) nil) ("T" t nil)))
  (arith "/" / (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)
                ("T" t nil))))

;; transcendental
(macrolet ((un (opname op kind types)
             `(progn
                ,@(loop for (tname et dual) in types
                        collect `(defcase ("transc" ,opname ,tname)
                                   :kinds (,kind) :ins (,et) :outs (,et) :dual ,dual
                                   :vm (vmc ,op a) :base (,(find-symbol (string op) :cl) x))))))
  (un "exp" exp :unit (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)))
  (un "log" log :pos (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)))
  (un "sin" sin :unit (("double" double-float t) ("single" single-float t) ("cdouble" (complex double-float) t)))
  (un "sqrt" sqrt :pos (("double" double-float nil) ("single" single-float nil) ("cdouble" (complex double-float) t))))
(defcase ("transc" "expt" "double") :kinds (:pos :unit) :ins (double-float double-float) :outs (double-float)
  :dual t :vm (vmc expt a b) :base (expt x y))

;; comparison, max / min
(macrolet ((cmp (opname op types)
             `(progn
                ,@(loop for (tname et) in types
                        collect `(defcase ("compare" ,opname ,tname)
                                   :ins (,et ,et) :outs (bit)
                                   :vm (vmc ,op a b)
                                   :base (if (,(find-symbol (string op) :cl) x y) 1 0))))))
  (cmp "<" < (("double" double-float) ("single" single-float) ("sb64" (signed-byte 64))))
  (cmp "=" = (("double" double-float) ("single" single-float) ("sb64" (signed-byte 64)))))
(macrolet ((mm (opname op types)
             `(progn
                ,@(loop for (tname et) in types
                        collect `(defcase ("compare" ,opname ,tname)
                                   :ins (,et ,et) :outs (,et)
                                   :vm (vmc ,op a b) :base (,(find-symbol (string op) :cl) x y))))))
  (mm "max" max (("double" double-float) ("single" single-float) ("sb64" (signed-byte 64))))
  (mm "min" min (("double" double-float) ("single" single-float) ("sb64" (signed-byte 64)))))

;; division family: integer quotient (sb64 array) and remainder
(macrolet ((div (opname op types)
             `(progn
                ,@(loop for (tname et) in types
                        collect `(defcase ("division" ,opname ,tname)
                                   :kinds (:floor-x :nonzero) :ins (,et ,et) :outs ((signed-byte 64) ,et)
                                   :vm (vmc ,op a b)
                                   :base (multiple-value-bind (q r) (,(find-symbol (string op) :cl) x y)
                                           (values (the (signed-byte 64) q) r)))))))
  (div "floor" floor (("double" double-float) ("single" single-float) ("sb64" (signed-byte 64))))
  (div "ceiling" ceiling (("double" double-float)))
  (div "truncate" truncate (("double" double-float)))
  (div "round" round (("double" double-float))))
(macrolet ((rem1 (opname op types)
             `(progn
                ,@(loop for (tname et) in types
                        collect `(defcase ("division" ,opname ,tname)
                                   :kinds (:floor-x :nonzero) :ins (,et ,et) :outs (,et)
                                   :vm (vmc ,op a b) :base (,(find-symbol (string op) :cl) x y))))))
  (rem1 "mod" mod (("double" double-float) ("sb64" (signed-byte 64))))
  (rem1 "rem" rem (("double" double-float)))
  (rem1 "ffloor" ffloor (("double" double-float))))
;; ffloor returns two values; its case above is registered as a one-output case for the
;; quotient only through the remainder: replace with the proper two-output form
(setf *cases* (remove-if (lambda (c) (string= (bcase-op c) "ffloor")) *cases*))
(defcase ("division" "ffloor" "double") :kinds (:floor-x :nonzero) :ins (double-float double-float)
  :outs (double-float double-float) :vm (vmc ffloor a b) :base (ffloor x y))

;; integer and conversion
(defcase ("integer" "logand" "sb64") :ins ((signed-byte 64) (signed-byte 64)) :outs ((signed-byte 64))
  :vm (vmc logand a b) :base (logand x y))
(defcase ("integer" "ash" "sb64") :kinds (:any) :ins ((signed-byte 64)) :outs ((signed-byte 64))
  :vm (vmc ash a 3) :base (ash x 3))
(defcase ("integer" "lognot" "sb64") :kinds (:any) :ins ((signed-byte 64)) :outs ((signed-byte 64))
  :vm (vmc lognot a) :base (lognot x))
(defcase ("convert" "float" "sb64->double") :kinds (:any) :ins ((signed-byte 64)) :outs (double-float)
  :vm (vmc float a 1d0) :base (float x 1d0))
(defcase ("convert" "float" "double->single") :kinds (:any) :ins (double-float) :outs (single-float)
  :vm (vmc float a 1f0) :base (float x 1f0))
(defcase ("convert" "rational" "double") :kinds (:any) :ins (double-float) :outs (t)
  :vm (vmc rational a) :base (rational x))

(setf *cases* (nreverse *cases*))

(defun run-case (c n)
  (let* ((ins (bcase-ins c)) (kinds (bcase-kinds c))
         (a (gen-array (first ins) (first kinds) n))
         (b (when (second ins) (gen-array (second ins) (or (second kinds) :any) n)))
         (id (list (bcase-family c) (bcase-op c) (bcase-type c) n))
         (vm (bcase-vm c)) (base (bcase-base c)))
    (flet ((row (mode secs) (apply #'emit *impl* "array" (append id (list mode (format nil "~,3F" (/ (* secs 1d9) n)))))))
      (row "base" (best-time (lambda () (funcall base n a b))))
      (row "vm" (best-time (lambda () (funcall vm a b))))
      (when (bcase-dual c)
        (let ((vm.impl::*mkl-available* nil))
          (row "vm-lisp" (best-time (lambda () (funcall vm a b)))))
        (when (bcase-cutoff-op c)
          (let* ((op (find-symbol (bcase-op c) :vm)) (old (vm:cutoff op)))
            (unwind-protect
                 (progn (setf (vm:cutoff op) 0)
                        (row "vm-mkl" (best-time (lambda () (funcall vm a b)))))
              (setf (vm:cutoff op) old))))))))

(defun run-array-section (sizes filter)
  (dolist (n sizes)
    (dolist (c *cases*)
      (when (or (null filter)
                (search filter (format nil "~A/~A/~A" (bcase-family c) (bcase-op c) (bcase-type c))))
        (when (> n 2000000) #+sbcl (sb-ext:gc :full t) #+ccl (ccl:gc))
        (handler-case (run-case c n)
          (error (e) (format *error-output* "FAILED ~A ~A ~A n=~D: ~A~%"
                             (bcase-family c) (bcase-op c) (bcase-type c) n e)))))))

;;; --- scalar overhead ---
;;; Each case is a loop that applies the operation N times with an accumulator; compiled with
;;; the argument types declared (decl) or not (undecl), once with the CL operators and once
;;; with the VM ones (the same source with every CL symbol replaced by the VM symbol).

(defun vmify (form)
  (cond ((and (symbolp form) form (eq (symbol-package form) (find-package :cl))
              (find-symbol (symbol-name form) :vm)
              (member (symbol-name form) (mapcar #'symbol-name (vm:shadowed-symbols)) :test #'string=))
         (find-symbol (symbol-name form) :vm))
        ((consp form) (cons (vmify (car form)) (vmify (cdr form))))
        (t form)))

(defvar *scalar-cases* '())
(defmacro defscalar (name (type-of-acc y-type y-value) form)
  "FORM computes the new accumulator from ACC and Y."
  `(push (list ,name ',type-of-acc ',y-type ,y-value ',form) *scalar-cases*))

(defun scalar-lambda (form acc-type y-type declared)
  `(lambda (n acc y)
     (declare (type fixnum n) (optimize (speed 1) (safety 1) (debug 0))
              ,@(when declared `((type ,acc-type acc) (type ,y-type y))))
     (dotimes (i n) (setf acc (the ,(if declared acc-type t) ,form)))
     acc))

(defscalar "+ double" (double-float double-float 1.0000001d0) (+ acc y))
(defscalar "* double" (double-float double-float 1.0000001d0) (* acc y))
(defscalar "- double" (double-float double-float 1.0000001d0) (- acc y))
(defscalar "+ fixnum" (fixnum fixnum 1) (+ acc y))
(defscalar "* fixnum" (fixnum fixnum 1) (* acc y))
(defscalar "sqrt double" (double-float double-float 2.5d0) (sqrt y))
(defscalar "exp double" (double-float double-float 0.5d0) (exp y))
(defscalar "max double" (double-float double-float 1.5d0) (max acc y))
(defscalar "floor double" (fixnum double-float 12345.6d0) (floor y 3d0))
(defscalar "mod fixnum" (fixnum fixnum 12345) (mod y 7))
(defscalar "logand fixnum" (fixnum fixnum 12345) (logand acc y))
(defscalar "ash fixnum" (fixnum fixnum 12345) (ash y -2))
(defscalar "float fixnum" (double-float fixnum 12345) (float y 1d0))
(defscalar "< double" (t double-float 1.5d0) (< y 1.25d0))

(defun run-scalar-section (filter)
  (dolist (sc (reverse *scalar-cases*))
    (destructuring-bind (name acc-type y-type y-value form) sc
      (when (or (null filter) (search filter name))
        (let ((n 1000000)
              (acc0 (ecase acc-type (double-float 1d0) (fixnum 0) ((t) nil))))
          (flet ((run (mode f)
                   (let ((fn (let ((*error-output* (make-broadcast-stream))) (compile nil f))))
                     (emit *impl* "scalar" "scalar" name "-" 1 mode
                           (format nil "~,3F" (/ (* (best-time (lambda () (funcall fn n acc0 y-value))) 1d9) n))))))
            (run "cl-decl" (scalar-lambda form acc-type y-type t))
            (run "vm-decl" (scalar-lambda (vmify form) acc-type y-type t))
            (run "cl-undecl" (scalar-lambda form acc-type y-type nil))
            (run "vm-undecl" (scalar-lambda (vmify form) acc-type y-type nil))))))))

(defun main ()
  (let* ((sizes (mapcar #'parse-integer
                        (uiop:split-string (env "BENCH_SIZES" "1 16 256 4096 65536 1000000 10000000") :separator " ")))
         (filter (env "BENCH_FILTER"))
         (sections (env "BENCH_SECTIONS" "array scalar")))
    (let ((out (env "BENCH_OUT")))
      (when out (setf *out-stream* (open out :direction :output :if-exists :append :if-does-not-exist :create))))
    (emit "#" *impl* (lisp-implementation-version) (format nil "mkl=~A" vm.impl::*mkl-available*)
          (format nil "load=~A" (load-average)))
    (when (search "scalar" sections) (run-scalar-section filter))
    (when (search "array" sections) (run-array-section sizes filter))
    (emit "#" "load-after" (load-average))
    (when *out-stream* (close *out-stream*))))

(unless (env "BENCH_NO_MAIN") (main))
