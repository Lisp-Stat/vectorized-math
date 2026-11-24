;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VM-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-Identifier: MS-PL
;;;
;;; The * type-wildcard hook (src/wildcard-hook.lisp): in a package that shadows VM:*, the
;;; wildcard in type specifiers must behave exactly as in plain CL.
;;; This package shadows * as VM:*, so the literal specifiers below read as VM:* .
(in-package #:vm-tests)

(defsuite wildcard-tests (vm-tests))

(defvar *plain-package-counter* 0)

(defun make-plain-package ()
  (make-package (format nil "VM-TESTS-PLAIN-~D" (incf *plain-package-counter*)) :use '(:cl)))

(defun test-file (name)
  (asdf:system-relative-pathname :vectorized-math (concatenate 'string "tests/" name)))

(deftest wildcard-symbol-is-vm-star-here (wildcard-tests)
  (assert-true (eq '* 'vm:*))
  (assert-false (eq '* 'cl:*)))

(deftest wildcard-literal-specifiers (wildcard-tests)
  (assert-true (typep (darr 1d0 2d0) '(simple-array double-float (*))))
  (assert-true (subtypep '(simple-array double-float (*)) '(simple-array double-float (cl:*))))
  (assert-true (subtypep '(simple-array double-float (cl:*)) '(simple-array double-float (*))))
  (assert-true (subtypep '(simple-array double-float (*)) 'array))
  (assert-true (typep (make-array '(2 2)) '(array * (* *))))
  (assert-true (typep 5 '(integer 0 *)))
  (assert-false (typep -5 '(integer 0 *)))
  (assert-true (subtypep '(function (fixnum) *) 'function))
  (assert-true (typep (make-array 3 :element-type 'bit) '(simple-bit-vector *)))
  (assert-true (funcall (compile nil '(lambda (x) (typep x '(simple-array double-float (*))))) (darr 1d0)))
  (assert-false (funcall (compile nil '(lambda (x) (typep x '(simple-array double-float (*))))) (sarr 1f0)))
  (assert-true (array-is (funcall (compile nil '(lambda (x) (declare (type (simple-array double-float (*)) x)) x))
                                  (darr 1d0))
                         'double-float 1 '(1d0)))
  (assert-true (typep (condition-of (funcall (compile nil '(lambda (x) (declare (type (simple-array double-float (*)) x)
                                                                                (optimize (safety 3)))
                                                            x))
                                             (sarr 1f0)))
                      'type-error)))

(deftype wildcard-test-vec (&optional (n '*)) `(simple-array double-float (,n)))
(deftype wildcard-test-tt (&optional (n '*)) (if (eq n '*) 'fixnum 'string))

(deftest wildcard-deftype (wildcard-tests)
  "deftype with &optional (n '*): the default and an explicit * both work, and a user deftype
that tests its argument with (eq n '*) sees its own *, not CL's."
  (let ((v (darr 1d0 2d0 3d0)))
    (assert-true (typep v 'wildcard-test-vec))
    (assert-true (typep v '(wildcard-test-vec)))
    (assert-true (typep v '(wildcard-test-vec *)))
    (assert-true (typep v '(wildcard-test-vec 3)))
    (assert-false (typep v '(wildcard-test-vec 4))))
  (assert-true (typep 1 '(wildcard-test-tt *)))
  (assert-true (typep 1 '(wildcard-test-tt)))
  (assert-true (typep "a" '(wildcard-test-tt 3)))
  (assert-true (typep 1 '(or (wildcard-test-tt *) null))))

(defstruct wildcard-test-struct
  (d (make-array 2 :element-type 'double-float) :type (simple-array double-float (*))))

(deftest wildcard-defstruct-and-typecase (wildcard-tests)
  (assert-true (typep (wildcard-test-struct-d (make-wildcard-test-struct)) '(simple-array double-float (2))))
  (assert-true (typep (condition-of (make-wildcard-test-struct :d 5)) 'type-error))
  (assert-equal :d (typecase (darr 1d0) ((simple-array double-float (*)) :d) (t :other)))
  (assert-equal :other (typecase (sarr 1f0) ((simple-array double-float (*)) :d) (t :other)))
  (let ((x (darr 1d0)))
    (check-type x (simple-array double-float (*)))
    (assert-true (typep (condition-of (let ((y (opaque 5))) (check-type y (simple-array double-float (*))))) 'type-error))))

(defun print-in (package thing)
  (let ((*package* package)) (prin1-to-string thing)))

(defun eval-corpus (package)
  "Evaluate each toplevel form of tests/wildcard-forms.lisp read in PACKAGE; the list of printed
results (errors as ERROR and the condition type)."
  (with-open-file (s (test-file "wildcard-forms.lisp"))
    (let ((*package* package))
      (loop for form = (read s nil :eof)
            until (eq form :eof)
            collect (print-in package
                              (handler-case (let ((*error-output* (make-broadcast-stream)))
                                              (multiple-value-list (eval form)))
                                (error (e) (list :error (type-of e)))))))))

(deftest wildcard-corpus-same-as-plain (wildcard-tests)
  "Each form of the corpus gives the same printed result in a package shadowing VM:* as in plain CL;
none of them is an error."
  (let* ((user (make-user-package)) (plain (make-plain-package))
         (a (eval-corpus user)) (b (eval-corpus plain)))
    (assert-true (> (length a) 60))
    (assert-equal (length a) (length b))
    (let ((diff (loop for x in a for y in b for i from 0 unless (string= x y) collect (list i x y))))
      (when diff (format t "~&~{~S~%~}" diff))
      (assert-equal nil diff))
    (assert-equal nil (remove-if-not (lambda (x) (search ":ERROR" x)) a))
    (assert-true (eq (find-symbol "*" user) 'vm:*))
    (assert-true (eq (find-symbol "*" plain) 'cl:*))))

(defun remove-prefix (string)
  "STRING without COMMON-LISP: prefixes and with runs of whitespace collapsed (pretty-printer line breaks)."
  (let ((s (uiop:frob-substrings string '("COMMON-LISP:") (lambda (m emit) (declare (ignore m)) (funcall emit "")))))
    (with-output-to-string (out)
      (let ((space nil))
        (loop for c across s
              do (if (member c '(#\Space #\Newline #\Tab))
                     (setf space t)
                     (progn (when space (write-char #\Space out) (setf space nil))
                            (write-char c out))))))))

(defun compile-and-run-template (package-name shadow)
  "Instantiate tests/wildcard-file-template.lisp for a fresh package, COMPILE-FILE and LOAD it,
and return the list of RUN-CHECKS results and the compile-file failure flag."
  (let* ((pkg (make-package package-name :use '(:cl)))
         (dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "vm-wildcard-~D-~D/" (get-universal-time) (random 1000000))
                                (uiop:temporary-directory))))
         (src (merge-pathnames "wildcard-file.lisp" dir)))
    (when shadow (shadowing-import (vm:shadowed-symbols) pkg))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (out src :direction :output :if-exists :supersede)
             (write-string (uiop:frob-substrings (uiop:read-file-string (test-file "wildcard-file-template.lisp"))
                                                 '("@PKG@") (lambda (m emit) (declare (ignore m)) (funcall emit package-name)))
                           out))
           (multiple-value-bind (fasl warnings-p failure-p)
               (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
                 (compile-file src))
             (declare (ignore warnings-p))
             (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
               (load fasl))
             (let ((results (let ((*package* pkg)) (funcall (find-symbol "RUN-CHECKS" pkg)))))
               (values results failure-p))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest wildcard-compile-file-same-as-plain (wildcard-tests)
  "A file with * in declarations, ftype, deftype, defstruct, defclass, typecase, check-type,
compiled with COMPILE-FILE and loaded: same results as the plain-CL version."
  (let ((n (incf *plain-package-counter*)))
    (multiple-value-bind (a fa) (compile-and-run-template (format nil "VM-WILDCARD-FILE-USER-~D" n) t)
      (multiple-value-bind (b fb) (compile-and-run-template (format nil "VM-WILDCARD-FILE-PLAIN-~D" n) nil)
        (assert-true (> (length a) 25))
        (assert-equal (length a) (length b))
        ;; (error messages print the canonical wildcard with a package prefix in the shadowing
        ;; package: (COMMON-LISP:*) for (*), cosmetic only, so compare without the prefix)
        (let ((diff (loop for x in a for y in b for i from 0
                          unless (string= (remove-prefix x) (remove-prefix y)) collect (list i x y))))
          (when diff (format t "~&~{~S~%~}" diff))
          (assert-equal nil diff))
        ;; compile-file does not fail more often than in plain CL (same warnings either way)
        (assert-equal fb fa)))))

(deftest wildcard-hook-self-test-and-idempotence (wildcard-tests)
  "The self-test passes; installing again does not stack; the self-test detects a missing hook."
  (assert-equal nil (vm.impl::wildcard-hook-failures))
  (assert-true (vm.impl::wildcard-hook-self-test))
  (unwind-protect
       (progn
         ;; install three more times, then remove ONCE: if installation stacked, a hook would remain
         (dotimes (i 3) (vm.impl::install-wildcard-hook))
         (assert-equal nil (vm.impl::wildcard-hook-failures))
         (vm.impl::uninstall-wildcard-hook)
         (let ((failures (vm.impl::wildcard-hook-failures)))
           (assert-true (consp failures))
           (let ((c (condition-of (vm.impl::wildcard-hook-self-test))))
             (assert-true (typep c 'error))
             (assert-true (search "type-wildcard hook" (princ-to-string c)))
             (assert-true (search (lisp-implementation-type) (princ-to-string c))))))
    (vm.impl::install-wildcard-hook))
  (assert-equal nil (vm.impl::wildcard-hook-failures))
  (assert-true (subtypep '(simple-array double-float (*)) '(simple-array double-float (cl:*)))))
