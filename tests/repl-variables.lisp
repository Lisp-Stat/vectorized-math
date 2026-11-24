;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; The REPL variables in a package that shadows + - * / (src/repl-variables.lisp).  VM:+ VM:- VM:* VM:/
;;; are true aliases of CL:+ CL:- CL:* CL:/ when VM:*REPL-VARIABLE-MODE* is :NATIVE or :DOWNSTREAM;
;;; with :SYMBOL-MACRO (SBCL, other implementations, a failed self-test, or VM_REPL_VARIABLES=
;;; symbol-macro) they are global symbol macros: evaluating the names works, BOUNDP and
;;; SYMBOL-VALUE do not.  The tests assert whichever behaviour the mode promises.
;;;
;;; Forms that only make sense for a special variable (a LET of the alias with a SPECIAL
;;; declaration, which is an error for a symbol macro) are built at run time and COMPILEd or EVALed,
;;; so that this file compiles in every mode.  In this package + - * / are the VM symbols;
;;; the CL ones are written cl:.
(in-package #:vm-tests)

(defsuite repl-variable-tests (vm-tests))

(defparameter *repl-pairs* '((vm:+ . cl:+) (vm:- . cl:-) (vm:* . cl:*) (vm:/ . cl:/)))

(defun alias-mode-p ()
  (and (member vm:*repl-variable-mode* '(:native :downstream)) t))

(defun in-thread (fn)
  "Run FN in a new thread with no bindings of its own; its value."
  (bt:join-thread (bt:make-thread fn)))

(defun run-form (form)
  "COMPILE a function of no arguments that evaluates FORM, and call it."
  (funcall (compile nil (list 'lambda '() form))))

(defmacro with-saved-globals (&body body)
  "BODY, then the global values of the four CL REPL variables restored (from a thread with no bindings)."
  `(let ((saved (in-thread (lambda () (mapcar (lambda (p) (symbol-value (cdr p))) *repl-pairs*)))))
     (unwind-protect (progn ,@body)
       (in-thread (lambda () (loop for p in *repl-pairs* for v in saved
                                   do (setf (symbol-value (cdr p)) v)))))))

;;; --- the mode ---

(deftest repl-mode-is-one-of-three (repl-variable-tests)
  (assert-true (member vm:*repl-variable-mode* '(:native :downstream :symbol-macro)))
  (assert-eq vm:*repl-variable-mode* vm.impl::*repl-variable-mode*)
  #+sbcl (assert-eq :symbol-macro vm:*repl-variable-mode*)
  (when (equal (uiop:getenv "VM_REPL_VARIABLES") "symbol-macro")
    (assert-eq :symbol-macro vm:*repl-variable-mode*))
  ;; the paths exclude each other: a symbol is a global symbol macro or special, not both
  (dolist (pair *repl-pairs*)
    (assert-equal (not (alias-mode-p))
                  (and (nth-value 1 (macroexpand-1 (car pair))) t))))

;;; --- what works in every mode: evaluating the names ---

(deftest repl-names-evaluate-to-the-variables (repl-variable-tests)
  ;; (the type of CL:/ is a list in SBCL)
  (let ((cl:+ 1) (cl:- 2) (cl:* 3) (cl:/ '(4)))
    (assert-equal '(1 2 3 (4)) (list + - * /))
    (assert-equal '(1 2 3 (4)) (eval '(list vm:+ vm:- vm:* vm:/)))
    (assert-equal '(1 2 3 (4)) (run-form '(list vm:+ vm:- vm:* vm:/))))
  (let ((cl:* 3))
    (setq * 5)
    (assert-equal '(5 5) (list cl:* *))
    (eval '(setq vm:* 6))
    (assert-equal '(6 6) (list cl:* *))
    (setf * 7)
    (assert-equal 7 cl:*)))

(deftest repl-names-and-function-namespace (repl-variable-tests)
  (assert-equal 3 (+ 1 2))
  (assert-equal 3 (funcall '+ 1 2))
  (assert-equal 3 (funcall #'+ 1 2))
  (assert-equal 12 (* 3 4))
  (assert-equal 4 (let ((cl:* 2)) (* * *)))
  (assert-equal 3 (let ((cl:+ 1)) (+ + 2))))

;;; --- compile-file of a file that uses the names as variables, then load it ---

(defun repl-variable-file-source ()
  (format nil "~
(in-package :vm-tests)
(defun repl-file-read () (list vm:+ vm:- vm:* vm:/))
(defun repl-file-bind ()
  (let ((cl:* 5)) (list vm:* (funcall (lambda () vm:*)))))
(defun repl-file-setq ()
  (let ((cl:/ '(0))) (setq vm:/ '(8)) (list cl:/ vm:/)))
(defun repl-file-global-set (v) (setq vm:+ v))
(defun repl-file-global-get () vm:+)
"))

(deftest repl-names-in-compiled-file (repl-variable-tests)
  (uiop:with-temporary-file (:stream s :pathname source :type "lisp" :keep nil)
    (write-string (repl-variable-file-source) s)
    :close-stream
    (let* ((fasl (compile-file source :output-file (make-pathname :type "fasl" :defaults source)))
           (cl:* 1))
      (assert-true fasl)
      (unwind-protect
           (progn
             (load fasl)
             (assert-equal 4 (length (funcall 'repl-file-read)))
             (assert-equal '(5 5) (funcall 'repl-file-bind))
             (assert-equal '((8) (8)) (funcall 'repl-file-setq))
             (with-saved-globals
               (in-thread (lambda () (funcall 'repl-file-global-set 41)))
               (assert-equal 41 (in-thread (lambda () cl:+)))
               (assert-equal 41 (in-thread (lambda () (funcall 'repl-file-global-get))))
               (setf (symbol-value 'cl:+) 42)
               (assert-equal 42 (in-thread (lambda () (funcall 'repl-file-global-get))))))
        (uiop:delete-file-if-exists fasl)))))

;;; --- boundp, symbol-value and the rest: the functions of symbols ---

(deftest repl-symbol-functions-in-alias-mode (repl-variable-tests)
  (if (alias-mode-p)
      (dolist (pair *repl-pairs*)
        (let ((a (car pair)) (v (cdr pair)))
          (assert-true (boundp a))
          (assert-true (boundp v))
          (assert-eq (symbol-value v) (symbol-value a))
          ;; the same symbol-value cell, seen from either name, in the current thread's binding
          (let ((val (list a)))
            (progv (list v) (list val)
              (assert-eq val (symbol-value a))
              (assert-eq val (symbol-value v)))
            (progv (list a) (list val)
              (assert-eq val (symbol-value a))
              (assert-eq val (symbol-value v))))
          (progv (list v) (list (list :one))
            (set a (list :two))
            (assert-equal '(:two) (symbol-value v))
            (setf (symbol-value v) (list :three))
            (assert-equal '(:three) (symbol-value a))
            (assert-true (member (makunbound a) (list a v))) ; the symbol (CCL's own change returns the target)
            (assert-false (boundp a))
            (assert-false (boundp v))
            (set v (list :four))
            (assert-true (boundp a))
            (assert-equal '(:four) (symbol-value a))
            (makunbound v)
            (assert-false (boundp a))
            (assert-false (boundp v)))
          (assert-true (boundp a))
          (assert-eq (symbol-value v) (symbol-value a))))
      ;; :symbol-macro: today's behaviour
      (dolist (pair *repl-pairs*)
        (assert-false (boundp (car pair)))
        (assert-true (boundp (cdr pair)))
        (assert-true (condition-of (symbol-value (car pair)))))))

(deftest repl-special-declarations-and-eval (repl-variable-tests)
  (if (alias-mode-p)
      (progn
        (assert-equal '(11 11 11 11)
                      (run-form '(let ((vm:* 11)) (declare (special vm:*))
                                  (list cl:* (symbol-value 'cl:*) vm:* (funcall (lambda () vm:*))))))
        (assert-equal '(12 12 12)
                      (run-form '(let ((cl:* 12)) (list vm:* (symbol-value 'vm:*) (eval 'vm:*)))))
        (assert-equal '(13 13)
                      (eval '(let ((vm:+ 13)) (declare (special vm:+)) (list cl:+ vm:+))))
        ;; a variable bound to its own value, set through the other name
        (assert-equal '(6 6)
                      (run-form '(let ((cl:* 5))
                                  (let ((vm:* vm:*)) (declare (special vm:*))
                                    (setq cl:* 6)
                                    (list cl:* vm:*)))))
        (assert-equal 5 (run-form '(let ((cl:* 5)) (let ((vm:* vm:*)) (declare (special vm:*)) (setq vm:* 6))
                                    cl:*)))
        ;; defvar through the alias does not change a bound variable
        (assert-equal '(21 21)
                      (run-form '(let ((cl:/ 21)) (eval '(defvar vm:/ 99))
                                  (list cl:/ vm:/)))))
      (progn
        (assert-equal 11 (eval '(let ((cl:* 11)) vm:*)))
        (assert-equal 11 (eval '(let ((cl:* 11)) (eval 'vm:*)))))))

(defvar *repl-test-special* 1)

(deftest repl-unrelated-symbols-unaffected (repl-variable-tests)
  (assert-true (boundp '*repl-test-special*))
  (assert-equal 1 (symbol-value '*repl-test-special*))
  (assert-equal 2 (set '*repl-test-special* 2))
  (assert-equal 2 *repl-test-special*)
  (assert-equal 3 (progv '(*repl-test-special*) '(3) (symbol-value '*repl-test-special*)))
  (assert-eq '*repl-test-special* (makunbound '*repl-test-special*))
  (assert-false (boundp '*repl-test-special*))
  (setq *repl-test-special* 1)
  (assert-true (condition-of (symbol-value (make-symbol "UNBOUND"))))
  (assert-true (condition-of (makunbound (opaque 3))))
  (assert-true (condition-of (boundp (opaque 3))))
  (assert-true (boundp 'nil))
  (assert-true (boundp 't)))

;;; --- the global value, set in one thread and read in another (Swank, Jupyter) ---

(deftest repl-global-setf-in-another-thread (repl-variable-tests)
  (with-saved-globals
    (dolist (pair *repl-pairs*)
      (let ((a (car pair)) (v (cdr pair)) (x (list :x)) (y (list :y)))
        ;; set CL's variable in a thread, read through VM's name (compiled code), and the reverse
        (let ((set-v (compile nil `(lambda (val) (setq ,v val))))
              (set-a (compile nil `(lambda (val) (setq ,a val))))
              (get-a (compile nil `(lambda () ,a)))
              (get-v (compile nil `(lambda () ,v))))
          (in-thread (lambda () (funcall set-v x)))
          (assert-eq x (in-thread get-a))
          (assert-eq x (in-thread get-v))
          (in-thread (lambda () (funcall set-a y)))
          (assert-eq y (in-thread get-v))
          (assert-eq y (in-thread get-a)))
        ;; and the functions of symbols, which need the alias
        (if (alias-mode-p)
            (progn
              (in-thread (lambda () (setf (symbol-value v) x)))
              (assert-eq x (in-thread (lambda () (symbol-value a))))
              (assert-true (in-thread (lambda () (boundp a))))
              (in-thread (lambda () (set a y)))
              (assert-eq y (in-thread (lambda () (symbol-value v)))))
            (progn
              (assert-false (in-thread (lambda () (boundp a))))))))))

;;; --- installation (only the downstream path has something to uninstall) ---

(deftest repl-downstream-uninstall-and-reinstall (repl-variable-tests)
  (when (eq vm:*repl-variable-mode* :downstream)
    #+(and ccl x8664-target)
    (let ((names (mapcar #'car vm.impl::*downstream-replacements*)))
      (assert-true (every (lambda (name) (assoc name vm.impl::*original-definitions*)) names))
      ;; each CCL function is vm's replacement while installed ...
      (assert-true (every (lambda (r) (eq (fdefinition (car r)) (fdefinition (cdr r))))
                          vm.impl::*downstream-replacements*))
      (let ((originals (copy-alist vm.impl::*original-definitions*)))
        (unwind-protect
             (progn
               (vm.impl::uninstall-repl-variables)
               ;; ... and CCL's own again afterwards
               (assert-eq nil vm.impl::*repl-variable-mode*)
               (assert-true (every (lambda (o) (eq (fdefinition (car o)) (cdr o))) originals))
               (assert-false (logbitp ccl::$sym_vbit_indirect (ccl::%symbol-bits 'vm:*))))
          (assert-eq :downstream (vm.impl::establish-repl-variables))))
      ;; reinstalled, and the same originals are the ones that will be restored
      (assert-true (every (lambda (r) (eq (fdefinition (car r)) (fdefinition (cdr r))))
                          vm.impl::*downstream-replacements*))
      (assert-true (every (lambda (name) (assoc name vm.impl::*original-definitions*)) names))
      (assert-eq :downstream vm:*repl-variable-mode*)
      (assert-true (boundp 'vm:*))
      (assert-eq (symbol-value 'vm:*) (symbol-value 'cl:*))
      (assert-equal 7 (run-form '(let ((vm:* 7)) (declare (special vm:*)) cl:*))))))

(deftest repl-alias-errors-in-alias-mode (repl-variable-tests)
  (when (eq vm:*repl-variable-mode* :downstream)
    #+(and ccl x8664-target)
    (progn
      ;; the same errors as CCL::%ALIAS-SPECIAL
      (assert-true (condition-of (vm.impl::alias-special 'cl:* 'vm:*)))      ; alias of itself
      (assert-true (condition-of (vm.impl::alias-special 'vm:/ 'cl:*)))      ; already an alias of /
      (assert-true (condition-of (vm.impl::alias-special (make-symbol "X") 'cl:pi)))
      (assert-true (eq 'vm:/ (vm.impl::alias-special 'vm:/ 'cl:/)))         ; same again: no-op
      (assert-true (condition-of (run-form '(let ((vm:/ 1)) (declare (special vm:/))
                                             (vm.impl::alias-special 'vm:/ 'cl:*))))))))
