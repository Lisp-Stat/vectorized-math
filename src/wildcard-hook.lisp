;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; The type-specifier wildcard (and the type names COMPLEX and MOD, which VM shadows as operators).
;;;
;;; In a package that shadows * (as every user of VM does), the wildcard in type
;;; specifiers -- (simple-array double-float (*)), (function (fixnum) *), deftype's
;;; &optional (n '*) -- reads as VM:*, which the implementation's type system does
;;; not know.  The hook below canonicalises VM:* to CL:* inside each implementation's
;;; type-specifier parser.  It patches implementation internals (approved by the user,
;;; 2026-11-23; spike: .agent-work/spikes/vm-shadowing/REPORT.md):
;;;
;;;   CCL    ccl:advise on ccl::type-expand and ccl::%deftype
;;;   SBCL   sb-int:encapsulate on sb-kernel::%parse-type and sb-impl::%deftype
;;;
;;; The same symbol is the qualifier pattern of the long form of DEFINE-METHOD-COMBINATION:
;;; ((methods *)) must mean "any qualifiers", which the implementations test with EQ against
;;; CL:*.  The hook therefore also canonicalises the qualifier patterns of the method group
;;; specifiers (CCL: advice on ccl::parse-method-group-specifiers; SBCL: encapsulation of
;;; sb-pcl::expand-long-defcombin).
;;;
;;; COMPLEX, FLOAT and RATIONAL are also CLASS names (and MOD is not): a method specialised on
;;; one, (defmethod f ((x float))), or (find-class 'float) in a package that shadows the operator
;;; looks the class up by the VM symbol, so each is also registered as a name of the CL class
;;; (INSTALL-TYPE-NAME-ALIASES; idempotent, global, and the same class object: class-name stays
;;; CL's symbol).
;;;
;;; The substitution shares unchanged structure, does not descend into
;;; (member eql satisfies quote ...) and does not descend into a form headed by a
;;; user DEFTYPE defined after the hook (so a user's (eq n '*) in a shadowing package
;;; still sees its own *); the output of such an expander is canonicalised instead.
;;;
;;; Installation is idempotent (advice and encapsulations are named, so reloading
;;; replaces rather than stacks them) and is followed by a self-test that signals an
;;; error if the hook is ineffective.  The hook must be installed before user code
;;; that uses the wildcard is read, so this file loads right after the package
;;; declarations.

(in-package #:vm.impl)

(defvar *user-expanders*
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized t)
  #+ccl (make-hash-table :test 'eq :weak :key :shared t)
  #-(or sbcl ccl) (make-hash-table :test 'eq)
  "Deftype expanders defined after the hook was installed (wrapped by it).")

(defun user-deftype-p (name)
  (let ((e #+sbcl (sb-int:info :type :expander name)
           #+ccl (gethash name ccl::%deftype-expanders%)
           #-(or sbcl ccl) nil))
    (and (functionp e) (gethash e *user-expanders*) t)))

(defun canon-type (x)
  "Replace VM:* by CL:* in the type specifier X; shares unchanged structure."
  (cond ((eq x 'vm:*) 'cl:*)
        ((eq x 'vm:complex) 'cl:complex)   ; COMPLEX is also an operator, hence shadowed
        ((eq x 'vm:mod) 'cl:mod)           ; likewise MOD: the type (mod n)
        ((eq x 'vm:float) 'cl:float)       ; FLOAT and RATIONAL are types and classes too
        ((eq x 'vm:rational) 'cl:rational)
        ((atom x) x)
        ((member (car x) '(member eql satisfies quote)) x)
        ((and (symbolp (car x)) (user-deftype-p (car x))) x)
        (t (let ((a (canon-type (car x)))
                 (d (canon-type-tail (cdr x))))
             (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))))

(defun canon-type-tail (x)
  (cond ((null x) nil)
        ((atom x) (canon-type x))
        (t (let ((a (canon-type (car x)))
                 (d (canon-type-tail (cdr x))))
             (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))))

;;; method group specifiers: (name {qualifier-pattern+ | predicate} [option value]...)
(defun canon-qualifier-pattern (x)
  "Replace VM:* by CL:* anywhere in the qualifier pattern X (a symbol, or a possibly dotted list)."
  (cond ((eq x 'vm:*) 'cl:*)
        ((atom x) x)
        (t (let ((a (canon-qualifier-pattern (car x))) (d (canon-qualifier-pattern (cdr x))))
             (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))))

(defun canon-method-group-specifier (spec)
  (if (and (consp spec) (listp (cdr spec)))
      (cons (car spec)
            (loop for tail on (cdr spec)
                  for x = (car tail)
                  until (keywordp x)
                  collect (canon-qualifier-pattern x) into patterns
                  finally (return (append patterns tail))))
      spec))

(defun canon-method-groups (specs)
  (if (listp specs) (mapcar #'canon-method-group-specifier specs) specs))

(defun canon-define-method-combination-form (form)
  "FORM, a (define-method-combination name lambda-list method-groups . body) long form, with VM:*
replaced by CL:* in the qualifier patterns of the method groups; other forms unchanged."
  (if (and (consp form) (consp (cdr form)) (consp (cddr form)) (listp (caddr form))
           (consp (cdddr form)))
      (list* (first form) (second form) (third form) (canon-method-groups (fourth form))
             (cddddr form))
      form))

(defun wrap-expander (fn)
  "FN, a deftype expander, wrapped so that its output is canonicalised."
  (let ((w (lambda (&rest args) (canon-type (apply fn args)))))
    (setf (gethash w *user-expanders*) t)
    w))

(defun install-wildcard-hook ()
  "Install (or re-install: it replaces, never stacks) the VM:* -> CL:* hook.
Returns true when a hook exists for this implementation."
  #+ccl
  (progn
    (ccl:advise ccl::type-expand
                (progn (setf (first ccl::arglist) (canon-type (first ccl::arglist)))
                       (canon-type (:do-it)))
                :when :around :name vm-wildcard-canon)
    (ccl:advise ccl::%deftype
                (let ((fn (second ccl::arglist)))
                  (when (functionp fn)
                    (setf (second ccl::arglist) (wrap-expander fn)))
                  (:do-it))
                :when :around :name vm-wildcard-mark)
    (ccl:advise ccl::parse-method-group-specifiers
                (progn (setf (first ccl::arglist) (canon-method-groups (first ccl::arglist)))
                       (:do-it))
                :when :around :name vm-wildcard-groups)
    t)
  #+sbcl
  (progn
    (sb-int:encapsulate 'sb-kernel::%parse-type 'vm-wildcard-canon
                        (lambda (f spec context) (funcall f (canon-type spec) context)))
    ;; %PARSE-TYPE re-parses deftype expansions by a local self-call that encapsulation
    ;; cannot intercept, so canonicalise the output of newly defined expanders.
    (sb-int:encapsulate 'sb-impl::%deftype 'vm-wildcard-canon
                        (lambda (f name expander source-location &optional doc)
                          (funcall f name
                                   (if (functionp expander) (wrap-expander expander) expander)
                                   source-location doc)))
    (sb-int:encapsulate 'sb-pcl::expand-long-defcombin 'vm-wildcard-canon
                        (lambda (f form) (funcall f (canon-define-method-combination-form form))))
    t)
  #-(or ccl sbcl) nil)

(defun install-type-name-aliases ()
  "Make VM:COMPLEX, VM:FLOAT and VM:RATIONAL (shadowed operators that are also CL class names) names
of the CL classes, for FIND-CLASS, DEFMETHOD specialisers and the like."
  (flet ((alias (vm-symbol cl-symbol)
           ;; (setting it again would make SBCL warn that it redefines a type to be a class)
           (let ((class (find-class cl-symbol)))
             (unless (eq (find-class vm-symbol nil) class)
               (setf (find-class vm-symbol) class)))))
    (alias 'vm:complex 'cl:complex)
    (alias 'vm:float 'cl:float)
    (alias 'vm:rational 'cl:rational))
  t)

(defun uninstall-wildcard-hook ()
  "Remove the hook (used by the tests to check that installation does not stack)."
  #+ccl (progn (ccl:unadvise ccl::type-expand :name vm-wildcard-canon)
               (ccl:unadvise ccl::%deftype :name vm-wildcard-mark)
               (ccl:unadvise ccl::parse-method-group-specifiers :name vm-wildcard-groups))
  #+sbcl (progn (sb-int:unencapsulate 'sb-kernel::%parse-type 'vm-wildcard-canon)
                (sb-int:unencapsulate 'sb-impl::%deftype 'vm-wildcard-canon)
                (sb-int:unencapsulate 'sb-pcl::expand-long-defcombin 'vm-wildcard-canon))
  nil)

;;; --- self-test ---
;;; The forms are built at run time and EVALed: a literal (subtypep '(... (vm:*)) ...)
;;; in this file could be folded by the compiler before the hook exists.

(declaim (notinline runtime-typep))
(defun runtime-typep (object spec) (typep object spec))

(defun wildcard-hook-failures ()
  "A list of descriptions of the wildcard checks that fail (empty when the hook works)."
  (let ((failures '()))
    (flet ((check (what thunk)
             (let ((ok (handler-case (funcall thunk)
                         (error (c) (push (format nil "~A signalled ~A" what c) failures)
                           :errored))))
               (unless (or ok (eq ok :errored))
                 (push (format nil "~A is false" what) failures)))))
      (check "(subtypep '(simple-array double-float (vm:*)) '(simple-array double-float (cl:*)))"
             (lambda ()
               (and (subtypep (list 'simple-array 'double-float (list 'vm:*))
                              (list 'simple-array 'double-float (list 'cl:*)))
                    (subtypep (list 'simple-array 'double-float (list 'cl:*))
                              (list 'simple-array 'double-float (list 'vm:*))))))
      (check "(typep 5 '(integer 0 vm:*))"
             (lambda () (runtime-typep 5 (list 'integer 0 'vm:*))))
      (check "(typep x '(array * (vm:* vm:*)))"
             (lambda () (runtime-typep (make-array '(2 2)) (list 'array 'vm:* (list 'vm:* 'vm:*)))))
      (check "(subtypep '(function (fixnum) vm:*) 'function)"
             (lambda () (subtypep (list 'function (list 'fixnum) 'vm:*) 'function)))
      (check "(deftype with &optional (n 'vm:*))"
             (lambda ()
               (eval (list 'deftype 'vm-wildcard-selftest '(&optional (n 'vm:*))
                           (list 'list ''simple-array ''double-float '(list n))))
               (unwind-protect
                    (let ((v (make-array 3 :element-type 'double-float)))
                      (and (runtime-typep v 'vm-wildcard-selftest)
                           (runtime-typep v (list 'vm-wildcard-selftest))
                           (runtime-typep v (list 'vm-wildcard-selftest 3))
                           (not (runtime-typep v (list 'vm-wildcard-selftest 4)))
                           (runtime-typep v (list 'vm-wildcard-selftest 'vm:*))))
                 #+sbcl (sb-int:clear-info :type :expander 'vm-wildcard-selftest)
                 #+ccl (remhash 'vm-wildcard-selftest ccl::%deftype-expanders%))))
      (check "(find-class 'vm:float) is the class FLOAT, likewise RATIONAL and COMPLEX"
             (lambda ()
               (and (eq (find-class 'vm:float) (find-class 'cl:float))
                    (eq (find-class 'vm:rational) (find-class 'cl:rational))
                    (eq (find-class 'vm:complex) (find-class 'cl:complex)))))
      (check "(typep x 'vm:float), (typep x '(vm:rational 0 *)), (typep x 'vm:complex)"
             (lambda ()
               (and (runtime-typep 1.5 'vm:float) (not (runtime-typep 1 'vm:float))
                    (runtime-typep 1/2 (list 'vm:rational 0 'vm:*))
                    (not (runtime-typep -1 (list 'vm:rational 0 'vm:*)))
                    (runtime-typep #c(1 2) 'vm:complex)
                    (runtime-typep 1.0 (list 'vm:float 0.0 'vm:*)))))
      (check "(define-method-combination name () ((methods vm:*)) ...) matches any qualifiers"
             (lambda ()
               (eval (list 'define-method-combination 'vm-wildcard-selftest-mc nil
                           (list (list 'methods 'vm:*))
                           '(list 'call-method (first methods))))
               (eval '(defgeneric vm-wildcard-selftest-gf (x) (:method-combination vm-wildcard-selftest-mc)))
               (unwind-protect
                    (progn
                      ;; the qualifier :WEIRD can only be matched by the * pattern
                      (eval '(defmethod vm-wildcard-selftest-gf :weird ((x integer)) :matched))
                      (eq (funcall (fdefinition 'vm-wildcard-selftest-gf) 0) :matched))
                 (fmakunbound 'vm-wildcard-selftest-gf)))))
    (nreverse failures)))

(defun wildcard-hook-self-test ()
  (let ((failures (wildcard-hook-failures)))
    (when failures
      (error "VECTORIZED-MATH: the type-wildcard hook (VM:* -> CL:*) is ineffective on ~A ~A:~%~{  ~A~%~}~
              Package-shadowing of * will not work.  The hook patches implementation internals and ~
              needs porting to this version."
             (lisp-implementation-type) (lisp-implementation-version) failures))
    t))

(install-type-name-aliases)

(locally
    ;; SBCL knows INSTALL-WILDCARD-HOOK returns T there, so the WARN arm is dead code
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if (install-wildcard-hook)
        (wildcard-hook-self-test)
        (warn "VECTORIZED-MATH: no type-wildcard hook for ~A; (shadowing-import 'vm:*) will break ~
               type specifiers such as (simple-array double-float (*))."
              (lisp-implementation-type))))
