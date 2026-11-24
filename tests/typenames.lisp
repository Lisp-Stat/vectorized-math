;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; FLOAT, RATIONAL, COMPLEX and MOD are CL operators that VM shadows, and also CL type specifiers (and,
;;; except for MOD, class names).  In a package that shadows them the symbols read as VM:FLOAT etc.;
;;; the type hook (src/wildcard-hook.lisp) maps them to the CL symbols for type specifiers and the class
;;; aliases make FIND-CLASS and DEFMETHOD specialisers work.  Everything below must behave exactly as
;;; in a plain package.
(in-package #:vm-tests)

(defsuite typename-tests (vm-tests))

(defun eval-corpus-file (package file)
  "Evaluate each toplevel form of tests/FILE read in PACKAGE; the list of printed results (errors as
:ERROR and the condition type)."
  (with-open-file (s (test-file file))
    (let ((*package* package))
      (loop for form = (read s nil :eof)
            until (eq form :eof)
            collect (print-in package
                              (handler-case (let ((*error-output* (make-broadcast-stream)))
                                              (multiple-value-list (eval form)))
                                (error (e) (list :error (type-of e)))))))))

(deftest typename-corpus-same-as-plain (typename-tests)
  "Each form of tests/typenames-forms.lisp gives the same printed result in a package shadowing the
operators as in plain CL, and none is an error."
  (let* ((user (make-user-package)) (plain (make-plain-package))
         (a (eval-corpus-file user "typenames-forms.lisp"))
         (b (eval-corpus-file plain "typenames-forms.lisp")))
    (assert-true (> (length a) 120))
    (assert-equal (length a) (length b))
    (let ((diff (loop for x in a for y in b for i from 0
                      unless (string= (remove-prefix x) (remove-prefix y)) collect (list i x y))))
      (when diff (format t "~&~{~S~%~}" diff))
      (assert-equal nil diff))
    (assert-equal nil (remove-if-not (lambda (x) (search "(:ERROR " x)) a))   ; (a form that evaluates to :ERROR prints (:ERROR))
    (assert-true (eq (find-symbol "FLOAT" user) 'vm:float))
    (assert-true (eq (find-symbol "RATIONAL" user) 'vm:rational))
    (assert-true (eq (find-symbol "COMPLEX" user) 'vm:complex))
    (assert-true (eq (find-symbol "MOD" user) 'vm:mod))
    (assert-true (eq (find-symbol "FLOAT" plain) 'cl:float))))

(deftest typename-values-in-shadowing-package (typename-tests)
  "Spot checks of the corpus with the values written out."
  (let ((p (make-user-package)))
    (assert-true (eval-in p "(typep 1.5 'float)"))
    (assert-false (eval-in p "(typep 1 'float)"))
    (assert-true (eval-in p "(typep -1/2 '(rational * 0))"))
    (assert-false (eval-in p "(typep 1/2 '(rational * 0))"))
    (assert-true (eval-in p "(typep 1.5d0 '(float 0d0 2d0))"))
    (assert-true (eval-in p "(typep #c(1.5 2.5) '(complex float))"))
    (assert-true (eval-in p "(subtypep '(float 0.0 1.0) 'float)"))
    (assert-true (eval-in p "(subtypep 'integer 'rational)"))
    (assert-equal '(t t) (eval-in p "(multiple-value-list (subtypep '(and float rational) nil))"))
    (assert-equal "FLOAT" (eval-in p "(symbol-name (class-name (find-class 'float)))"))
    (assert-true (eval-in p "(eq (find-class 'rational) (find-class 'cl:rational))"))
    (assert-true (eval-in p "(eq (find-class 'complex) (find-class 'cl:complex))"))
    (assert-equal '(:float :rational :complex (:integer :rational))
                  (eval-in p "(progn (defmethod ty-m1 ((x float)) :float) (defmethod ty-m1 ((x rational)) :rational)
                                     (defmethod ty-m1 ((x complex)) :complex)
                                     (defmethod ty-m1 ((x integer)) (list :integer (call-next-method)))
                                     (list (ty-m1 1.5) (ty-m1 1/2) (ty-m1 #c(1 2)) (ty-m1 3)))"))
    (assert-equal '(:neg :nonneg :pos-float :neg-float)
                  (eval-in p "(mapcar (lambda (x) (typecase x ((rational * 0) :neg) ((rational 0) :nonneg)
                                                     ((float 0.0) :pos-float) (float :neg-float)))
                                      '(-1/2 1/2 1.5 -1.5))"))
    (assert-equal 1.0 (eval-in p "(coerce 1 'float)"))
    (assert-equal 1/2 (eval-in p "(let ((x 1/2)) (check-type x rational) x)"))
    (assert-equal :type-error
                  (eval-in p "(handler-case (funcall (compile nil '(lambda (x) (declare (type float x) (optimize (safety 3))) x)) 1)
                                (type-error () :type-error))"))
    (assert-equal :type-error
                  (eval-in p "(handler-case (funcall (compile nil '(lambda (x) (declare (float x) (optimize (safety 3))) x)) 1)
                                (type-error () :type-error))"))
    ;; the operators of the same names work in the same package
    (assert-equal 1.0d0 (eval-in p "(float 1 1d0)"))
    (assert-equal 1/2 (eval-in p "(rational 0.5)"))
    (assert-equal #c(1 2) (eval-in p "(complex 1 2)"))
    (assert-equal 3 (eval-in p "(mod 13 5)"))
    (assert-true (eval-in p "(equalp (float #(1 2) 1d0) #(1d0 2d0))"))))

(deftest typename-deftype-and-structs-in-compiled-file (typename-tests)
  "DEFTYPE bodies, DEFSTRUCT and DEFCLASS slot types and FTYPE declarations compiled with COMPILE-FILE in
a shadowing package and loaded behave as in a plain package."
  (flet ((run (package-name shadow)
           (let* ((pkg (make-package package-name :use '(:cl)))
                  (dir (uiop:ensure-directory-pathname
                        (merge-pathnames (format nil "vm-typenames-~D-~D/" (get-universal-time) (random 1000000))
                                         (uiop:temporary-directory))))
                  (src (merge-pathnames "typenames-file.lisp" dir)))
             (when shadow (shadowing-import (vm:shadowed-symbols) pkg))
             (ensure-directories-exist dir)
             (unwind-protect
                  (progn
                    (with-open-file (out src :direction :output :if-exists :supersede)
                      (format out "(in-package ~S)~%" package-name)
                      (write-string "
(deftype tn-real-pair () '(cons (or float rational) (or float rational)))
(deftype tn-small-float (&optional (hi 10.0)) `(float 0.0 ,hi))
(deftype tn-pos-rational () '(rational (0) *))
(defstruct tn-s (a 1.5 :type float) (b 1/2 :type rational) (c #c(1 2) :type complex) (d 3 :type (mod 10)))
(defclass tn-c () ((x :type float :initform 2.5 :initarg :x :reader tn-x) (y :type rational :initform 1 :initarg :y :reader tn-y)))
(declaim (ftype (function (float rational) (values float &optional)) tn-f))
(defun tn-f (a b) (declare (type float a) (type rational b)) (+ a b))
(defgeneric tn-g (x))
(defmethod tn-g ((x float)) :float)
(defmethod tn-g ((x ratio)) :ratio)
(defmethod tn-g ((x rational)) :rational)
(defmethod tn-g ((x complex)) :complex)
(defun tn-h (x) (typecase x (tn-small-float :small) (float :float) (tn-pos-rational :pos) (rational :rat) (complex :cx) (t :other)))
(defun tn-k (x) (declare (type (or float rational) x)) (etypecase x (float :float) (rational :rational)))
(defmacro chk (form) `(handler-case (format nil \"~S => ~S\" ',form (multiple-value-list ,form)) (error (e) (format nil \"~S => ERROR ~A\" ',form (type-of e)))))
(defun run-checks ()
  (list (chk (typep (cons 1.5 1/2) 'tn-real-pair)) (chk (typep (cons 1.5 'a) 'tn-real-pair))
        (chk (typep 5.0 'tn-small-float)) (chk (typep 5.0 '(tn-small-float 1.0))) (chk (typep 1/2 'tn-pos-rational))
        (chk (typep 0 'tn-pos-rational))
        (chk (let ((s (make-tn-s))) (list (tn-s-a s) (tn-s-b s) (tn-s-c s) (tn-s-d s))))
        (chk (make-tn-s :a 1)) (chk (make-tn-s :b 1.5)) (chk (make-tn-s :c 1)) (chk (make-tn-s :d 12))
        (chk (list (tn-x (make-instance 'tn-c)) (tn-y (make-instance 'tn-c))))
        (chk (tn-f 1.5 1/2)) (chk (tn-f 1 1/2))
        (chk (list (tn-g 1.5) (tn-g 1/2) (tn-g 3) (tn-g #c(1 2)) (tn-g 1d0)))
        (chk (mapcar #'tn-h '(1.5 50.0 1/2 -1/2 #c(1 2) a)))
        (chk (list (tn-k 1.5) (tn-k 1/2) (tn-k 3)))))
" out))
                    (multiple-value-bind (fasl warnings-p failure-p)
                        (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
                          (compile-file src))
                      (declare (ignore warnings-p))
                      (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
                        (load fasl))
                      (values (let ((*package* pkg)) (funcall (find-symbol "RUN-CHECKS" pkg))) failure-p)))
               (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
    (let ((n (incf *plain-package-counter*)))
      (multiple-value-bind (a fa) (run (format nil "VM-TYPENAMES-USER-~D" n) t)
        (multiple-value-bind (b fb) (run (format nil "VM-TYPENAMES-PLAIN-~D" n) nil)
          (assert-true (> (length a) 15))
          (assert-equal (length a) (length b))
          (let ((diff (loop for x in a for y in b for i from 0
                            unless (string= (remove-prefix x) (remove-prefix y)) collect (list i x y))))
            (when diff (format t "~&~{~S~%~}" diff))
            (assert-equal nil diff))
          (assert-equal fb fa))))))

(deftest typename-aliases-idempotent (typename-tests)
  "Installing the class aliases again changes nothing."
  (let ((f (find-class 'cl:float)))
    (vm.impl::install-type-name-aliases)
    (vm.impl::install-type-name-aliases)
    (assert-true (eq f (find-class 'vm:float)))
    (assert-true (eq (find-class 'cl:float) (find-class 'vm:float)))
    (assert-equal 'cl:float (class-name (find-class 'vm:float)))
    (assert-equal nil (vm.impl::wildcard-hook-failures))))
