;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Backward compatibility of shadowing: REPL variable, method combination.
(in-package #:vm-tests)

(defsuite compat-tests (vm-tests))

(defvar *user-package-counter* 0)

(defun make-user-package ()
  "A fresh package that uses CL and shadowing-imports (vm:shadowed-symbols), like a user package."
  (let ((p (make-package (format nil "VM-TESTS-USER-~D" (incf *user-package-counter*)) :use '(:cl))))
    (shadowing-import (vm:shadowed-symbols) p)
    p))

(defun eval-in (package string)
  "Read and evaluate the form in STRING with *PACKAGE* PACKAGE."
  (let ((*package* package))
    (eval (read-from-string string))))

(deftest shadowed-symbols-shadow (compat-tests)
  (let ((p (make-user-package)))
    (assert-true (eq (find-symbol "+" p) 'vm:+))
    (assert-false (eq (find-symbol "+" p) 'cl:+))))

(deftest repl-variable-symbol-macro (compat-tests)
  "In a package shadowing VM:+, the symbol + is still the REPL variable CL:+."
  (let ((p (make-user-package)))
    (let ((cl:+ '(some form)))
      (assert-equal '(some form) (eval-in p "+"))
      (assert-equal 11 (eval-in p "(let ((+ 5)) (cl:+ + 6))"))
      (assert-equal 5 (eval-in p "(let ((+ 5)) +)"))
      (assert-equal 7 (eval-in p "(let ((+ 5)) (+ + 2))"))
      (assert-equal 3 (eval-in p "(setq + 3)"))
      (assert-equal 3 cl:+)
      (assert-equal 4 (eval-in p "(+ + 1)"))
      (assert-equal '(3 4 5) (eval-in p "(list + (+ + 1) (+ + 2))")))))

(deftest method-combination-plus (compat-tests)
  "(:method-combination +) in a package shadowing VM:+, with :most-specific-last."
  (let ((p (make-user-package)))
    (eval-in p "(progn (defclass mc-a () ()) (defclass mc-b (mc-a) ())
                  (defgeneric mc-f (x) (:method-combination +))
                  (defmethod mc-f + ((x mc-a)) 1)
                  (defmethod mc-f + ((x mc-b)) 10)
                  (defgeneric mc-g (x) (:method-combination + :most-specific-last))
                  (defvar *mc-trace* nil)
                  (defmethod mc-g + ((x mc-a)) (push :a *mc-trace*) 1)
                  (defmethod mc-g + ((x mc-b)) (push :b *mc-trace*) 10)
                  (defgeneric mc-h (x) (:method-combination +))
                  (defmethod mc-h + ((x mc-a)) 7))")
    (assert-equal 11 (eval-in p "(mc-f (make-instance 'mc-b))"))
    (assert-equal 1 (eval-in p "(mc-f (make-instance 'mc-a))"))
    (assert-equal 11 (eval-in p "(mc-g (make-instance 'mc-b))"))
    (assert-equal '(:b :a) (eval-in p "*mc-trace*"))   ; most specific last: :a ran first
    ;; one applicable method with :identity-with-one-argument
    (assert-equal 7 (eval-in p "(mc-h (make-instance 'mc-b))"))
    ;; the method combination is the same object whether named by VM:+ or by a shadowed + symbol
    (assert-true (eq 'vm:+ (let ((*package* p)) (read-from-string "+"))))))

(deftest repl-variables-all-operators (compat-tests)
  "- * / are symbol macros for the CL variables, like +; - * / 1+ 1- are functions too."
  (let ((p (make-user-package)))
    (dolist (name '("+" "-" "*" "/"))
      (assert-true (eq (find-symbol name p) (find-symbol name :vm)))
      (assert-false (eq (find-symbol name p) (find-symbol name :cl))))
    (let ((cl:- '(some form)) (cl:* 17) (cl:/ '(4 5)))
      (assert-equal '(some form) (eval-in p "-"))
      (assert-equal 17 (eval-in p "*"))
      (assert-equal '(4 5) (eval-in p "/"))
      (assert-equal '((some form) 17 (4 5)) (eval-in p "(list - * /)"))
      (assert-equal 34 (eval-in p "(* * 2)"))
      (assert-equal 15 (eval-in p "(- * 2)"))
      (assert-equal 17/2 (eval-in p "(/ * 2)"))
      (assert-equal 5 (eval-in p "(let ((- 5)) -)"))
      (assert-equal 3 (eval-in p "(let ((* 5)) (- * 2))"))
      (assert-equal 6 (eval-in p "(let ((/ 3)) (* / 2))")))
    (assert-equal 3 (eval-in p "(funcall '- 5 2)"))
    (assert-equal 3 (eval-in p "(funcall #'- 5 2)"))
    (assert-equal 6 (eval-in p "(apply #'* '(1 2 3))"))
    (assert-equal 1/2 (eval-in p "(/ 2)"))
    (assert-equal 2 (eval-in p "(1+ 1)"))
    (assert-equal 0 (eval-in p "(1- 1)"))
    (assert-equal 7 (eval-in p "(let ((x 5)) (incf x 2) x)"))
    (assert-equal 3 (eval-in p "(let ((x 5)) (decf x 2) x)"))
    ;; setq through the symbol macro sets the CL variable
    (let ((cl:- nil))
      (assert-equal 5 (eval-in p "(setq - 5)"))
      (assert-equal 5 cl:-))))

(defun method-combination-exists-p (name)
  "True when (defgeneric g (x) (:method-combination NAME)) is accepted."
  (handler-case (progn (eval `(defgeneric ,(gensym "MC-PROBE") (x) (:method-combination ,name))) t)
    (error () nil)))

(deftest only-plus-is-a-method-combination (compat-tests)
  "VM:+ (and VM:MAX, VM:MIN: see compare.lisp) are the built-in method-combination names among these operators."
  (assert-true (method-combination-exists-p 'vm:+))
  (dolist (s '(vm:- vm:* vm:/))
    (assert-false (method-combination-exists-p s))))
