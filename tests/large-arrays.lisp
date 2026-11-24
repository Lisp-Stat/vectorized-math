;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Optional test: arrays of more than 2^30 elements.  NOT part of the regular suite (it allocates
;;; about 13 GB of arrays, 21 GB resident at the peak, and takes one to three minutes).
;;;
;;;   sbcl --dynamic-space-size 32GB --no-userinit --non-interactive --load tests/large-arrays.lisp
;;;   ccl --no-init --batch --load tests/large-arrays.lisp --eval '(ccl:quit)' </dev/null
;;;
;;; MKL takes its length as a 32-bit int, so the MKL kernels hand it at most 2^30 elements per call
;;; and loop (+MKL-CHUNK+, kernels.lisp and elementwise.lisp).  This test uses single-float arrays of
;;; 2^30 + 1000 elements (three chunks' worth would need more memory; two chunks, the second one tiny,
;;; exercise the boundary) and checks, element by element at the chunk boundary and on a sample across
;;; the arrays, that + - * / exp and sqrt, a comparison, max, floor and :into give what a scalar
;;; computation gives.  Pass.  Prints FAIL lines and exits with status 1 otherwise.
(load "/home/vscode/quicklisp/setup.lisp")
(push (let ((dir (uiop:getenv "VM_SYSTEM_DIR")))
        (if (and dir (plusp (length dir)))
            (uiop:ensure-directory-pathname dir)
            (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))))
      asdf:*central-registry*)
(asdf:load-system :vectorized-math)

(defpackage #:vm-large (:use #:cl))
(in-package #:vm-large)

(defconstant +n+ (+ (expt 2 30) 1000))
(defvar *failures* 0)

(defun collect-garbage ()
  "A full collection between the steps: each result is 4 GB of garbage afterwards."
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc))

(defun check (what ok &rest info)
  (unless ok
    (incf *failures*)
    (format t "FAIL ~A ~{~S ~}~%" what info))
  (finish-output))

(defun sample-indices ()
  (append (list 0 1 2 (- (expt 2 30) 2) (- (expt 2 30) 1) (expt 2 30) (+ (expt 2 30) 1) (- +n+ 2) (- +n+ 1))
          (loop for i from 1 below 50 collect (floor (* i (/ +n+ 50))))))

(defun main ()
  (format t "~A ~A, n = ~D~%" (lisp-implementation-type) (lisp-implementation-version) +n+)
  (let ((a (make-array +n+ :element-type 'single-float))
        (b (make-array +n+ :element-type 'single-float)))
    (dotimes (i +n+)
      (setf (aref a i) (+ 1f0 (float (mod i 1000) 1f0))
            (aref b i) (+ 2f0 (float (mod i 7) 1f0))))
    (format t "arrays filled~%") (finish-output)
    (flet ((elementwise (name op ref)
             (let ((r (funcall op a b)))
               (check name (= (length r) +n+))
               (dolist (i (sample-indices))
                 (check name (eql (aref r i) (funcall ref (aref a i) (aref b i))) i (aref r i)))
               (format t "~A ok~%" name) (finish-output)
               (setf r nil)
               (collect-garbage))))
      (elementwise "+" #'vm:+ #'cl:+)
      (elementwise "-" #'vm:- #'cl:-)
      (elementwise "*" #'vm:* #'cl:*)
      (elementwise "/" #'vm:/ #'cl:/)
      (elementwise "max" #'vm:max #'cl:max)
      (elementwise "<" #'vm:< (lambda (x y) (if (cl:< x y) 1 0)))
      (elementwise "floor (quotient)" (lambda (x y) (values (vm:floor x y)))
                   (lambda (x y) (values (cl:floor x y)))))
    ;; unary: exp of a small value (MKL) and sqrt (the Lisp loop)
    (let ((r (vm:sqrt a)))
      (dolist (i (sample-indices))
        (check "sqrt" (eql (aref r i) (cl:sqrt (aref a i))) i))
      (format t "sqrt ok~%") (finish-output))
    (collect-garbage)
    (let ((r (vm:exp (vm:* 0.001f0 a))))
      (dolist (i (sample-indices))
        (check "exp" (<= (abs (- (aref r i) (cl:exp (* 0.001f0 (aref a i))))) (* 4 single-float-epsilon (aref r i))) i))
      (format t "exp ok~%") (finish-output))
    (collect-garbage)
    ;; :into the first operand (the sum is computed in a fresh 4 GB array and then stored)
    (let ((a0 (aref a (- (expt 2 30) 1))) (b0 (aref b (- (expt 2 30) 1))))
      (vm:+ a b :into a)
      (check ":into" (eql (aref a (- (expt 2 30) 1)) (cl:+ a0 b0)))
      (check ":into end" (eql (aref a (- +n+ 1)) (cl:+ (+ 1f0 (float (mod (- +n+ 1) 1000) 1f0))
                                                       (+ 2f0 (float (mod (- +n+ 1) 7) 1f0)))))
      (format t ":into ok~%")))
  (format t "~A~%" (if (zerop *failures*) "ALL OK" "FAILURES"))
  (uiop:quit (if (zerop *failures*) 0 1)))

(main)
