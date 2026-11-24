;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM-CUTOFF -*-
;;; Where does MKL beat the Lisp loop?  Times the two kernels of + - * / on float and complex float
;;; arrays directly (no operator overhead, no allocation), for sizes from 4 to 65536, and prints
;;; ns per element for each plus the smallest size from which MKL is faster for all larger sizes
;;; (per operation and element type).  Results are bit-identical on both sides of a cutoff, so the
;;; cutoff changes speed only (see src/kernels.lisp, CUTOFF).
;;;
;;;   sbcl --no-userinit --non-interactive --load bench/cutoff.lisp
;;;   ccl --no-init --batch --load bench/cutoff.lisp --eval '(ccl:quit)' </dev/null
;;; Environment: VM_SYSTEM_DIR (the system to load), CUTOFF_REPS (repetitions, default 9).
(load "/home/vscode/quicklisp/setup.lisp")
(push (let ((dir (uiop:getenv "VM_SYSTEM_DIR")))
        (if (and dir (plusp (length dir)))
            (uiop:ensure-directory-pathname dir)
            (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))))
      asdf:*central-registry*)
(asdf:load-system :vectorized-math)

(defpackage #:vm-cutoff (:use #:cl))
(in-package #:vm-cutoff)

(defvar *timespec* (cffi:foreign-alloc :int64 :count 2))
(defun now ()
  (cffi:foreign-funcall "clock_gettime" :int 1 :pointer *timespec* :int)
  (+ (cffi:mem-aref *timespec* :int64 0) (* 1d-9 (cffi:mem-aref *timespec* :int64 1))))

(defparameter *reps* (let ((v (uiop:getenv "CUTOFF_REPS"))) (if (and v (plusp (length v))) (parse-integer v) 9)))
(defparameter *sizes* '(4 8 16 32 48 64 96 128 192 256 384 512 768 1024 1536 2048 3072 4096 8192 12288 16384 24576 32768 49152 65536 131072))

(defun time-kernel (fn out a b n)
  "ns per element, best of *REPS* repetitions of about 5 ms each."
  (funcall fn out 0 a 0 b 0 n)
  (let* ((t0 (let ((s (now))) (funcall fn out 0 a 0 b 0 n) (- (now) s)))
         (inner (max 1 (min 200000 (floor 0.005d0 (max t0 1d-8))))) (best nil))
    (dotimes (r *reps*)
      (let ((s (now)))
        (dotimes (i inner) (funcall fn out 0 a 0 b 0 n))
        (let ((dt (/ (- (now) s) inner))) (when (or (null best) (< dt best)) (setf best dt)))))
    (/ (* best 1d9) n)))

(defun fill-array (a type)
  (dotimes (i (length a) a)
    (setf (aref a i) (ecase type
                       (:single-float (+ 1f0 (random 10f0))) (:double-float (+ 1d0 (random 10d0)))
                       (:complex-single-float (complex (+ 1f0 (random 10f0)) (+ 1f0 (random 10f0))))
                       (:complex-double-float (complex (+ 1d0 (random 10d0)) (+ 1d0 (random 10d0))))))))

(defun sweep (opname regname type)
  (let* ((lisp (symbol-function (vm.impl::kname 'lisp opname type 'aa)))
         (mkl (vm.impl::find-kernel regname type :aa))
         (et (vm.impl::key-element-type type))
         (cell (vm.impl::cutoff-cell regname))
         (old (car cell))
         (rows '()))
    (unwind-protect
         (progn
           (setf (car cell) 0)               ; MKL for every size
           (dolist (n *sizes*)
             (let ((a (fill-array (make-array n :element-type et) type)) (b (fill-array (make-array n :element-type et) type))
                   (o (make-array n :element-type et)))
               (vm.impl::with-masked-float-traps
                 (push (list n (time-kernel lisp o a b n) (time-kernel mkl o a b n)) rows)))))
      (setf (car cell) old))
    (nreverse rows)))

(defun crossover (rows)
  "Smallest N from which MKL is faster (by 3%) at every larger size of the sweep."
  (let ((best nil))
    (dolist (r (reverse rows) best)
      (destructuring-bind (n lisp mkl) r
        (if (< mkl (* 0.97 lisp)) (setf best n) (return best))))))

(defun main ()
  (format t "# ~A ~A  default cutoffs: ~{~A ~}~%" (lisp-implementation-type) (lisp-implementation-version)
          (loop for op in '("+" "-" "*" "/") collect (format nil "~A=~D" op (vm:cutoff op))))
  (dolist (spec '(("+" add) ("-" sub) ("*" mul) ("/" div)))
    (destructuring-bind (regname opname) spec
      (dolist (type (if (member regname '("+" "-") :test #'string=)
                        '(:single-float :double-float :complex-single-float :complex-double-float)
                        '(:single-float :double-float)))
        (let ((rows (sweep opname regname type)))
          (format t "~%~A ~A  (ns per element: Lisp loop / MKL)~%" regname type)
          (dolist (r rows) (format t "  ~6D  ~8,2F  ~8,2F~%" (first r) (second r) (third r)))
          (format t "  crossover: ~A~%" (or (crossover rows) "none (MKL not faster below 65536)")))))))

(main)
