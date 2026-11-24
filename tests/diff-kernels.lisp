;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Differential test of the typed kernels against CL itself, on large corpora (not part of the
;;; clunit suite; the suite runs a small version of the same checks).
;;;
;;;   ccl --no-init --batch --load tests/diff-kernels.lisp --eval '(ccl:quit)' </dev/null
;;;   sbcl --no-userinit --non-interactive --load tests/diff-kernels.lisp
;;;
;;; Environment: DIFF_N (pairs per class and operation, default 200000), DIFF_SEED (default 1),
;;; DIFF_SECTIONS ("sqrt division", default both).
;;; For every element that the kernel handles (it reports "not ordinary" for the others, which take
;;; CL's own path) the kernel's result must be EQL to CL's: the quotient (integer or float), the
;;; remainder and the sign of a zero remainder included.  SQRT: the real kernel against CL:SQRT on
;;; all non-negative patterns tried (NaNs compare as equal to each other).  Exit status 1 on a mismatch.
(load "/home/vscode/quicklisp/setup.lisp")
(push (let ((dir (uiop:getenv "VM_SYSTEM_DIR")))     ; default: the directory above this one
        (if (and dir (plusp (length dir)))
            (uiop:ensure-directory-pathname dir)
            (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))))
      asdf:*central-registry*)
(asdf:load-system :vectorized-math)

(defpackage #:vm-diff (:use #:cl))
(in-package #:vm-diff)

(defun env (name default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))
(defparameter *n* (parse-integer (env "DIFF_N" "200000")))
(defparameter *state* (make-random-state t))

(defun dbits (x)
  #+ccl (multiple-value-bind (hi lo) (ccl::double-float-bits x) (logior (ash (ldb (byte 32 0) hi) 32) lo))
  #+sbcl (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32) (sb-kernel:double-float-low-bits x)))
(defun bits-double (bits)
  #+ccl (ccl::double-float-from-bits (ash bits -32) (ldb (byte 32 0) bits))
  #+sbcl (sb-kernel:make-double-float (let ((h (ldb (byte 32 32) bits))) (if (logbitp 31 h) (- h (ash 1 32)) h)) (ldb (byte 32 0) bits)))
(defun sbits (x)
  #+ccl (ldb (byte 32 0) (ccl::single-float-bits x))
  #+sbcl (ldb (byte 32 0) (sb-kernel:single-float-bits x)))
(defun bits-single (bits)
  #+ccl (ccl:%stack-block ((p 4)) (setf (ccl:%get-unsigned-long p 0) bits) (ccl:%get-single-float p 0))
  #+sbcl (sb-kernel:make-single-float (if (logbitp 31 bits) (- bits (ash 1 32)) bits)))
(defun nanp (x) (and (floatp x) (let ((b (if (typep x 'double-float) (dbits x) (sbits x))))
                                  (if (typep x 'double-float)
                                      (> (logand b #x7fffffffffffffff) #x7ff0000000000000)
                                      (> (logand b #x7fffffff) #x7f800000)))))
(defun finitep (x) (and (not (nanp x)) (< (abs x) (if (typep x 'double-float) most-positive-double-float most-positive-single-float))))

(defun rnd (k) (random k *state*))
(defun fl (v type) (float v (if (eq type 'double-float) 1d0 1f0)))
(defun random-float (type)
  "A random float of TYPE: a random bit pattern (any exponent, subnormals and specials included)."
  (if (eq type 'double-float) (bits-double (rnd (ash 1 64))) (bits-single (rnd (ash 1 32)))))
(defun random-normal (type lo-exp hi-exp)
  (let ((v (* (+ 1 (random 1d0 *state*)) (expt 2d0 (+ lo-exp (rnd (1+ (- hi-exp lo-exp)))))) ))
    (fl (if (zerop (rnd 2)) v (- v)) type)))
(defun nextafter (x dir type)
  (if (or (not (finitep x)) (zerop x)) x
      (if (eq type 'double-float)
          (let ((b (dbits x))) (bits-double (if (eq (plusp x) (plusp dir)) (1+ b) (1- b))))
          (let ((b (sbits x))) (bits-single (if (eq (plusp x) (plusp dir)) (1+ b) (1- b)))))))

(defun random-pair (type class)
  "Two floats (values X Y) of TYPE; CLASS 0-9 selects the kind of pair."
  (flet ((few-bits (&optional (e 5)) (fl (* (1+ (rnd 4000)) (expt 2d0 (- (rnd (* 2 e)) e))) type)))
    (let ((sx (if (zerop (rnd 2)) 1 -1)) (sy (if (zerop (rnd 2)) 1 -1)))
      (ecase class
        (0 (values (random-float type) (random-float type)))                                  ; any patterns
        (1 (values (random-normal type -40 40) (random-normal type -40 40)))                  ; ordinary, wide range
        (2 (values (random-normal type -5 20) (random-normal type -5 20)))                    ; ordinary, moderate
        (3 (let* ((y (few-bits)) (n (rnd (ash 1 (if (eq type 'double-float) 30 12)))))        ; exact multiples
             (values (* sx (fl n type) y) (* sy y))))
        (4 (let* ((y (few-bits)) (n (rnd (ash 1 (if (eq type 'double-float) 30 10)))))        ; exact ties (n + 1/2) y
             (values (* sx (+ (fl n type) (fl 1/2 type)) y) (* sy y))))
        (5 (let* ((y (few-bits)) (n (rnd (ash 1 20)))                                          ; neighbours of multiples and ties
                  (x (* (+ (fl n type) (if (zerop (rnd 2)) 0 (fl 1/2 type))) y)))
             (values (* sx (nextafter x (if (zerop (rnd 2)) 1 -1) type)) (* sy y))))
        (6 (values (fl (* sx (- (rnd 2001) 1000)) type) (fl (* sy (1+ (rnd 20))) type)))     ; small integers
        (7 (values (fl (* sx (/ (- (rnd 8001) 4000) 4)) type) (fl (* sy (/ (1+ (rnd 40)) 2)) type))) ; quarters / halves
        (8 (let ((specials (if (eq type 'double-float)
                               (list 0d0 -0d0 1d0 -1d0 0.5d0 1.5d0 2.5d0 least-positive-double-float
                                     least-positive-normalized-double-float most-positive-double-float
                                     1d-300 1d300 4d18 4.5d15 9d15 (expt 2d0 52) (expt 2d0 53) (expt 2d0 63))
                               (list 0f0 -0f0 1f0 -1f0 0.5f0 1.5f0 2.5f0 least-positive-single-float
                                     least-positive-normalized-single-float most-positive-single-float
                                     1f-30 1f30 4f18 4.5f15 9f15 (expt 2f0 23) (expt 2f0 24) (expt 2f0 52)))))
             (values (* sx (nth (rnd (length specials)) specials)) (* sy (nth (rnd (length specials)) specials)))))
        (9 (let ((y (few-bits)))                                                                ; quotients around 2^52 .. 2^63
             (values (* sx y (fl (expt 2 (+ 48 (rnd 18))) type) (fl (+ 1 (rnd 8)) type)) (* sy y))))))))

(defvar *mismatches* 0)
(defvar *checked* 0)
(defvar *skipped* 0)

(defun report (what x y expected got)
  (incf *mismatches*)
  (when (<= *mismatches* 20)
    (format t "MISMATCH ~A  x=~S y=~S~%    CL:     ~S~%    kernel: ~S~%" what x y expected got)))

(defun test-division (name cl-fn kind key)
  (let* ((type (if (eq key :double-float) 'double-float 'single-float))
         (kernel (vm.impl::find-kernel name key :aa))
         (qtype (if (eq kind :float-q) type '(signed-byte 64)))
         (q (make-array 1 :element-type qtype)) (r (make-array 1 :element-type type))
         (xa (make-array 1 :element-type type)) (ya (make-array 1 :element-type type)))
    (vm.impl::with-masked-float-traps
      (dotimes (class 10)
        (dotimes (k *n*)
          (multiple-value-bind (x y) (random-pair type class)
            (setf (aref xa 0) x (aref ya 0) y)
            (if (funcall kernel q r 0 xa 0 ya 0 1)
                (progn
                  (incf *checked*)
                  (if (eq kind :rem)
                      (let ((expected (funcall cl-fn x y)))
                        (unless (eql expected (aref r 0))
                          (report (list name key class) x y expected (aref r 0))))
                      (multiple-value-bind (eq er) (funcall cl-fn x y)
                        (unless (and (eql eq (aref q 0)) (eql er (aref r 0)))
                          (report (list name key class) x y (list eq er) (list (aref q 0) (aref r 0)))))))
                (incf *skipped*))))))))

(defun test-sqrt (key)
  (let* ((type (if (eq key :double-float) 'double-float 'single-float))
         (kernel (vm.impl::find-kernel "sqrt" key :u))
         (a (make-array 1 :element-type type)) (o (make-array 1 :element-type type)))
    (vm.impl::with-masked-float-traps
      (dotimes (class 5)
        (dotimes (k (* 4 *n*))
          (let ((x (ecase class
                     (0 (let ((v (random-float type))) (abs v)))                            ; any non-negative pattern
                     (1 (abs (random-normal type -300 300)))
                     (2 (abs (random-normal type -20 20)))
                     (3 (let ((v (abs (random-float type)))) (if (finitep v) (nextafter (* (fl (isqrt (rnd (ash 1 40))) type) (fl (isqrt (rnd (ash 1 40))) type)) 1 type) v)))
                     (4 (fl (rnd 1000000) type)))))
            (unless (or (and (floatp x) (not (nanp x)) (< x 0)))
              (setf (aref a 0) x)
              (funcall kernel o 0 a 0 1)
              (incf *checked*)
              (let ((ref (cl:sqrt x)) (got (aref o 0)))
                (unless (or (and (nanp ref) (nanp got)) (eql ref got))
                  (report (list "sqrt" key class) x nil ref got))))))))))

(defun main ()
  (let ((sections (env "DIFF_SECTIONS" "sqrt division")))
    (format t "~A ~A: ~D pairs per class~%" (lisp-implementation-type) (lisp-implementation-version) *n*)
    (when (search "sqrt" sections)
      (dolist (key '(:single-float :double-float)) (test-sqrt key))
      (format t "sqrt: ~D checked, ~D mismatches~%" *checked* *mismatches*))
    (when (search "division" sections)
      (setf *checked* 0 *skipped* 0)
      (dolist (spec vm.impl::+division-ops+)
        (destructuring-bind (name cl-fn mode kind) spec
          (declare (ignore mode))
          (dolist (key '(:double-float :single-float))
            (let ((before *mismatches*))
              (test-division name cl-fn kind key)
              (format t "~A ~A: ~D mismatches so far~%" name key (- *mismatches* before))))))
      (format t "division: ~D elements checked against CL (~D not ordinary, left to CL's own path), ~D mismatches in all~%"
              *checked* *skipped* *mismatches*))
    (format t "~A~%" (if (zerop *mismatches*) "ALL IDENTICAL" "MISMATCHES FOUND"))
    (uiop:quit (if (zerop *mismatches*) 0 1))))

(main)
