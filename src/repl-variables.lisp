;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; The REPL variables + ++ +++ - * ** *** / // /// in a package that shadows + - * /.
;;;
;;; The REPL keeps its history in the CL variables +, ++, +++, -, *, **, ***, /, // and ///.  In
;;; a package that shadows + - * / (VM:SHADOWED-SYMBOLS), the reader gives VM:+ and so on, so a
;;; user typing * at the REPL would get an unrelated, unbound variable.  VM:+ VM:- VM:* VM:/ must
;;; therefore denote the same variables as CL:+ CL:- CL:* CL:/.  Three ways of doing that, chosen
;;; at load time and recorded in *REPL-VARIABLE-MODE* (also VM:*REPL-VARIABLE-MODE*):
;;;
;;;   :NATIVE        The Lisp provides CCL::%ALIAS-SPECIAL (a CCL with the special-variable alias
;;;                  support).  VM calls it for the four symbols.
;;;   :DOWNSTREAM    CCL on x86-64 without it.  VM installs replacements for the CCL functions
;;;                  and compiler hooks that the alias support changes (below), then calls its
;;;                  own %ALIAS-SPECIAL equivalent.  The two names then share one global value
;;;                  and one set of thread-local bindings: BOUNDP, SYMBOL-VALUE, SET, MAKUNBOUND,
;;;                  PROGV, SPECIAL declarations, EVAL, and a SETF of the global value in another
;;;                  thread (Swank, Jupyter) all see the same variable under either name.
;;;   :SYMBOL-MACRO  SBCL, other implementations and other CCL platforms, or when the self-test of
;;;                  the other two fails: (define-symbol-macro vm:+ cl:+) and so on.  Evaluating
;;;                  the names gives the history, but (boundp 'vm:+), (symbol-value 'vm:+),
;;;                  PROGV and the other functions of symbols are about a symbol that is not the
;;;                  variable.
;;;
;;; The paths exclude each other: a symbol cannot be both a global symbol macro and special.
;;;
;;; Environment variable VM_REPL_VARIABLES, read at load time:
;;;   symbol-macro      use the fallback even where an alias is possible (for testing, and for
;;;                     anyone who does not want vm to patch the Lisp)
;;;   fail-self-test    make the self-test fail, to exercise the rollback to the fallback (tests)
;;;   anything else     (or unset) choose the best path.
;;;
;;; The downstream installation (CCL, x86-64 only; other CCL platforms get :SYMBOL-MACRO because
;;; the compiler hook below is the x86 back end's)
;;; -------------------------------------------------------------------------------------------
;;; It reproduces the CCL change "special variable aliases" (the two-line compiler hooks, the
;;; resolution of an alias in six runtime functions, and the alias table):
;;;
;;;   * An alias is a symbol with the (otherwise unused) flag $sym_vbit_indirect set, a weak
;;;     table entry alias -> target, the SPECIAL bit, and the BINDING INDEX of the target.
;;;     Thread-local bindings live in the per-thread binding vector at the symbol's binding
;;;     index, so a binding made through either name is the same binding; compiled code never
;;;     needs to know.  Global values live in the symbol's value cell, so the functions that read
;;;     or write a symbol's value (%SYM-VALUE, %SET-SYM-VALUE, %SYM-GLOBAL-VALUE,
;;;     %SET-SYM-GLOBAL-VALUE, %SYMBOL-BINDING-ADDRESS, MAKUNBOUND) are replaced by versions that
;;;     use the target when the flag is set, and the compiler (X862-SYMBOL-VALUE-CELL on x86 and
;;;     NX1-NOTE-VCELL-REF) compiles a global reference to an alias as a reference to the target.
;;;   * Code compiled BEFORE the alias exists keeps a reference to the alias's own, now dead,
;;;     value cell: global reads signal unbound-variable and global SETQs are lost (bindings
;;;     stay shared).  vm creates the aliases when it loads, before any of its own files that
;;;     could refer to them as variables, and before user code that uses them.  User code
;;;     compiled in an image where vm was not loaded (or loaded with the fallback) refers to
;;;     the CL variables, because a symbol macro expands to them, so a fasl compiled with
;;;     either path works with the other.
;;;
;;; Install mechanism: the CCL functions are REDEFINED, by (setf fdefinition) with
;;; CCL::*WARN-IF-REDEFINE-KERNEL* bound to NIL, and the originals are kept for uninstallation.
;;; The alternative, CCL:ADVISE, wraps the function in a closure that takes &rest arguments and
;;; calls through the advice machinery.  Measured on CCL 1.14 (x86-64) for out-of-line SYMBOL-VALUE,
;;; BOUNDP and SET of a symbol that is not an alias, median of 7, three runs each (ns per call):
;;;                       symbol-value    boundp     set
;;;       no vm                6.5-7.0     6.0       8.9-9.0
;;;       redefinition         7.8         7.1      10.0      (+1 ns)
;;;       advise (a bare pass-through, a lower bound for any real advice)
;;;                           16.5-17.0   15.0      26.7-27.3 (+9 to +18 ns)
;;; Redefinition is the cheaper one and is safe here: nothing calls these functions other than
;;; through their function cells (the compiler does not inline them; the one use of
;;; %SYM-GLOBAL-VALUE in compiler source folds the value of a constant).  The test of the flag is
;;; open-coded in each replacement (macro RESOLVE-ALIAS): through a function call it cost +5 ns
;;; against +1 ns.
;;;
;;; Things vm cannot reach, because CCL keeps them in closure variables of l0-symbol.lisp:
;;;   * the binding-index lock.  Alias creation here is not serialised with CCL's own
;;;     ENSURE-BINDING-INDEX, only with vm's lock.  Alias creation happens once, at load, for
;;;     four symbols nobody else is proclaiming at that moment, so the unlocked window is
;;;     theoretical.
;;;   * the binding-index reverse map (index -> symbol).  The alias's old index stays mapped to
;;;     the alias, and the shared index stays mapped to the target.  The only consumers are
;;;     debugging aids (BINDING-INDEX-SYMBOL, the names in a backtrace of saved specials), and
;;;     the old index is no longer used by any binding.
;;;   * ENSURE-BINDING-INDEX does not know aliases.  It does nothing for a symbol that already has
;;;     an index, and an alias has the target's, so (defvar vm:+), (proclaim '(special vm:+)) and
;;;     the compiler leave it alone.
;;;
;;; Self-test: after the aliases exist (:NATIVE and :DOWNSTREAM), REPL-VARIABLE-FAILURES checks
;;; boundp/symbol-value, the global value set in one thread and read in another (both
;;; directions, compiled code), a dynamic binding through each name, PROGV, and a freshly
;;; compiled function that refers to the aliases as variables.  If anything fails, everything
;;; installed is removed, one warning is signalled and the symbol macros are defined.
;;;
;;; Everything is in EVAL-WHEN because the aliases must exist while vm's own files and user code
;;; are compiled as well as when they are loaded; establishing them is idempotent.

(in-package #:vm.impl)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *repl-variable-mode* nil
  "How VM:+ VM:- VM:* VM:/ relate to the REPL variables CL:+ CL:- CL:* CL:/: :NATIVE (the Lisp has
CCL::%ALIAS-SPECIAL), :DOWNSTREAM (vm installed the same change into CCL), or :SYMBOL-MACRO (global
symbol macros: evaluating the names works, BOUNDP and SYMBOL-VALUE do not).  NIL before vm is
loaded.  Set at load time; do not set it.")

(defparameter *repl-variable-pairs*
  '((vm:+ . cl:+) (vm:- . cl:-) (vm:* . cl:*) (vm:/ . cl:/))
  "(alias . variable): the VM symbol and the CL REPL variable it must denote.")

;;; ------------------------------------------------------------------------------------------
;;; Fallback

(defun define-repl-symbol-macros ()
  (dolist (pair *repl-variable-pairs*)
    (eval (list 'define-symbol-macro (car pair) (cdr pair)))))

;;; ------------------------------------------------------------------------------------------
;;; CCL, x86-64: aliases

#+(and ccl x8664-target)
(progn

(defvar *alias-lock* (ccl:make-lock "vectorized-math REPL variable aliases"))

(defvar *alias-targets* (make-hash-table :test #'eq :weak :key)
  "Alias symbol -> target symbol.  Written before the flag of the alias is set.")

(defun alias-target (alias)
  (gethash alias *alias-targets*))

(defmacro resolve-alias (form)
  "The value of FORM (a symbol), or its target when it is an alias.  The flag test is open-coded:
a function call for it costs +5 ns in SYMBOL-VALUE, BOUNDP and SET against +1 ns."
  (let ((s (gensym "S")))
    `(let ((,s ,form))
       (if (logbitp ccl::$sym_vbit_indirect
                    (the fixnum (ccl::%svref (ccl::symptr->symvector (ccl::%symbol->symptr ,s))
                                             x8664::symbol.flags-cell)))
           (alias-target ,s)
           ,s))))

;;; --- replacements of CCL's functions (same as CCL with the alias support) ---

(defun alias-sym-value (name)
  (ccl::%symptr-value (ccl::%symbol->symptr (resolve-alias name))))

(defun alias-set-sym-value (name val)
  (ccl::%set-symptr-value (ccl::%symbol->symptr (resolve-alias name)) val))

(defun alias-sym-global-value (name)
  (ccl::%svref (ccl::symptr->symvector (ccl::%symbol->symptr (resolve-alias name)))
               x8664::symbol.vcell-cell))

(defun alias-set-sym-global-value (name val)
  (setf (ccl::%svref (ccl::symptr->symvector (ccl::%symbol->symptr (resolve-alias name)))
                     x8664::symbol.vcell-cell)
        val))

(defun alias-symbol-binding-address (sym)
  (ccl::%symptr-binding-address (ccl::%symbol->symptr (resolve-alias sym))))

(defvar *original-definitions* nil
  "While the downstream hooks are installed: alist function name -> the CCL function they replace.")

(defun original-definition (name)
  (or (cdr (assoc name *original-definitions*))
      (error "VECTORIZED-MATH: no original definition of ~S saved." name)))

(defun alias-makunbound (sym)
  ;; Resolved here because the original would clear the flag bits of an alias, among them the
  ;; alias flag.  Returns its argument (CCL's change returns the target).
  (funcall (the function (original-definition 'cl:makunbound))
           (if (symbolp sym) (resolve-alias sym) sym))
  sym)

(defun alias-x862-symbol-value-cell (sym)
  (funcall (the function (original-definition 'ccl::x862-symbol-value-cell))
           (if (symbolp sym) (resolve-alias sym) sym)))

(defun alias-nx1-note-vcell-ref (sym)
  (funcall (the function (original-definition 'ccl::nx1-note-vcell-ref))
           (if (symbolp sym) (resolve-alias sym) sym))
  ;; the original returns its argument
  sym)

(defparameter *downstream-replacements*
  '((ccl::%sym-value . alias-sym-value)
    (ccl::%set-sym-value . alias-set-sym-value)
    (ccl::%sym-global-value . alias-sym-global-value)
    (ccl::%set-sym-global-value . alias-set-sym-global-value)
    (ccl::%symbol-binding-address . alias-symbol-binding-address)
    (cl:makunbound . alias-makunbound)
    (ccl::x862-symbol-value-cell . alias-x862-symbol-value-cell)
    (ccl::nx1-note-vcell-ref . alias-nx1-note-vcell-ref))
  "(CCL function name . vm.impl function that replaces it).")

(defun downstream-hooks-possible-p ()
  (every (lambda (r) (fboundp (car r))) *downstream-replacements*))

(defun install-downstream-hooks ()
  "Redefine the CCL functions of *DOWNSTREAM-REPLACEMENTS*, saving the originals the first time.
Idempotent; calling it again points the CCL names at the current definitions of the replacements
(they are redefined when this file is loaded after having been evaluated at compile time)."
  (ccl:with-lock-grabbed (*alias-lock*)
    (let ((ccl::*warn-if-redefine-kernel* nil)
          (ccl::*warn-if-redefine* nil))
      (unless *original-definitions*
        (setf *original-definitions*
              (mapcar (lambda (r) (cons (car r) (fdefinition (car r))))
                      *downstream-replacements*)))
      (dolist (r *downstream-replacements*)
        (unless (eq (fdefinition (car r)) (fdefinition (cdr r)))
          (setf (fdefinition (car r)) (fdefinition (cdr r)))))))
  t)

(defun uninstall-downstream-hooks ()
  "Restore the CCL functions that INSTALL-DOWNSTREAM-HOOKS replaced."
  (ccl:with-lock-grabbed (*alias-lock*)
    (let ((ccl::*warn-if-redefine-kernel* nil)
          (ccl::*warn-if-redefine* nil))
      (dolist (saved *original-definitions*)
        (setf (fdefinition (car saved)) (cdr saved)))
      (setf *original-definitions* nil)))
  t)

;;; --- creating and removing an alias ---

(defun special-thread-bound-p (sym)
  "True if any thread has a thread-local binding of SYM.  (CCL has the same function with the
alias support, as ccl::%special-thread-bound-p; vm never relies on it being there.)"
  (ccl:with-lock-grabbed (ccl::*kernel-tcr-area-lock*)
    (dolist (p (ccl::all-processes) nil)
      (let ((tcr (ccl::process-tcr p)))
        (when tcr
          (if (eq tcr (ccl::%current-tcr))
              (when (ccl::%tcr-binding-location tcr sym)
                (return t))
              (unwind-protect
                   (progn (ccl::%suspend-tcr tcr)
                          (when (ccl::%tcr-binding-location tcr sym)
                            (return t)))
                (ccl::%resume-tcr tcr))))))))

(defun alias-special (alias target)
  "Make ALIAS another name for the special variable TARGET (as CCL::%ALIAS-SPECIAL does): both
then share the global value and the thread-local bindings.  The global value of TARGET wins; ALIAS's
own is used only if TARGET is unbound."
  (check-type alias symbol)
  (check-type target symbol)
  (flet ((bits (s) (the fixnum (ccl::%symbol-bits s))))
    (let ((root (or (alias-target target) target)))
      (when (eq root alias)
        (error "Can't make ~S an alias of itself." alias))
      (dolist (s (list alias root))
        (when (or (null s) (eq s t) (keywordp s)
                  (logbitp ccl::$sym_vbit_const (bits s))
                  (logbitp ccl::$sym_vbit_global (bits s)))
          (error "~S can't take part in a special variable alias." s)))
      (ccl:with-lock-grabbed (*alias-lock*)
        (let ((old (alias-target alias)))
          (when old
            (if (eq old root)
                (return-from alias-special alias)
                (error "~S is already an alias of ~S." alias old))))
        (ccl::%proclaim-special alias)
        (ccl::%proclaim-special root)
        (ccl::ensure-binding-index root)
        (when (special-thread-bound-p alias)
          (error "~S has thread-local bindings; can't make it an alias." alias))
        ;; CCL's own lock for binding indices is not reachable from here (see above).
        (ccl:without-interrupts
          (let* ((aliasvec (ccl::symptr->symvector (ccl::%symbol->symptr alias)))
                 (rootvec (ccl::symptr->symvector (ccl::%symbol->symptr root)))
                 (index (ccl::%svref rootvec x8664::symbol.binding-index-cell)))
            (when (eq (ccl::%svref rootvec x8664::symbol.vcell-cell) (ccl::%unbound-marker))
              (setf (ccl::%svref rootvec x8664::symbol.vcell-cell)
                    (ccl::%svref aliasvec x8664::symbol.vcell-cell)))
            (setf (ccl::%svref aliasvec x8664::symbol.vcell-cell) (ccl::%unbound-marker))
            (setf (ccl::%svref aliasvec x8664::symbol.binding-index-cell) index)
            (setf (gethash alias *alias-targets*) root)
            ;; the flag last: readers test it before consulting the table
            (ccl::%symbol-bits alias (ccl::bitset ccl::$sym_vbit_indirect (bits alias))))))
      alias)))

(defun unalias-special (alias)
  "Undo ALIAS-SPECIAL (or CCL::%ALIAS-SPECIAL, whose table entry stays but is never read once the flag is
clear): ALIAS is an ordinary, non-special, unbound symbol again.  Used to roll back a failed self-test."
  (ccl:with-lock-grabbed (*alias-lock*)
    (ccl:without-interrupts
      (let ((bits (the fixnum (ccl::%symbol-bits alias))))
        (setf bits (logandc2 bits (ash 1 ccl::$sym_vbit_indirect)))
        (setf bits (logandc2 bits (ash 1 ccl::$sym_vbit_special)))
        (ccl::%symbol-bits alias bits))
      (remhash alias *alias-targets*)
      (let ((aliasvec (ccl::symptr->symvector (ccl::%symbol->symptr alias))))
        (setf (ccl::%svref aliasvec x8664::symbol.binding-index-cell) 0)
        (setf (ccl::%svref aliasvec x8664::symbol.vcell-cell) (ccl::%unbound-marker)))))
  alias)

) ; #+(and ccl x8664-target)

;;; ------------------------------------------------------------------------------------------
;;; Choosing the path

#+ccl
(defun native-alias-function ()
  "CCL::%ALIAS-SPECIAL if this CCL has it, else NIL.  (FIND-SYMBOL: vm never creates the name.)"
  (let ((s (find-symbol "%ALIAS-SPECIAL" "CCL")))
    (and s (fboundp s) (fdefinition s))))

(defun alias-all-repl-variables (mode)
  (dolist (pair *repl-variable-pairs*)
    (ecase mode
      #+ccl (:native (funcall (the function (native-alias-function)) (car pair) (cdr pair)))
      #+(and ccl x8664-target) (:downstream (alias-special (car pair) (cdr pair))))))

(defun remove-repl-variable-aliases (mode)
  (declare (ignorable mode))
  #+(and ccl x8664-target)
  (dolist (pair *repl-variable-pairs*)
    (when (logbitp ccl::$sym_vbit_indirect (the fixnum (ccl::%symbol-bits (car pair))))
      (unalias-special (car pair))))
  #+(and ccl x8664-target)
  (when (eq mode :downstream) (uninstall-downstream-hooks))
  nil)

(defun repl-variable-environment-request ()
  (string-downcase (or (uiop:getenv "VM_REPL_VARIABLES") "")))

;;; --- self-test ---
;;; The forms are built at run time and COMPILEd: literal references to VM:* in this file would
;;; be compiled before the aliases exist.

#+ccl
(defun call-in-new-thread (fn)
  (ccl::join-process (ccl:process-run-function "vectorized-math self-test" fn)))

#+ccl
(defun repl-variable-failures ()
  "A list of descriptions of the REPL-variable checks that fail (empty when the aliases work)."
  (let ((failures '()))
    (flet ((check (what thunk)
             (let ((ok (handler-case (funcall thunk)
                         (error (c) (push (format nil "~A signalled ~A" what c) failures)
                           :errored))))
               (unless (or ok (eq ok :errored))
                 (push (format nil "~A is false" what) failures))))
           (compile-quietly (form)
             ;; also a check that the compiler does not warn about the free variables
             (let ((ws '()))
               (let ((fn (handler-bind ((warning (lambda (c) (push c ws) (muffle-warning c))))
                           (compile nil form))))
                 (when ws (error "compiling ~S warned: ~{~A~^; ~}" form ws))
                 fn))))
      (when (equal (repl-variable-environment-request) "fail-self-test")
        (push "forced failure (VM_REPL_VARIABLES=fail-self-test)" failures))
      (dolist (pair *repl-variable-pairs*)
        (let* ((vm (car pair)) (cl (cdr pair))
               (tag (format nil "~S/~S" vm cl))
               (v1 (list :self-test-1)) (v2 (list :self-test-2))
               (set-vm (compile-quietly `(lambda (v) (setq ,vm v))))
               (set-cl (compile-quietly `(lambda (v) (setq ,cl v))))
               (get-vm (compile-quietly `(lambda () ,vm)))
               (get-cl (compile-quietly `(lambda () ,cl)))
               (old (call-in-new-thread get-cl)))
          (check (format nil "~A: BOUNDP and SYMBOL-VALUE agree" tag)
                 (lambda () (and (eq (boundp vm) (boundp cl))
                                 (or (not (boundp cl))
                                     (eq (symbol-value vm) (symbol-value cl))))))
          (check (format nil "~A: the global value of the one set in a thread is seen as the other, in another thread" tag)
                 (lambda ()
                   (unwind-protect
                        (and (progn (call-in-new-thread (lambda () (funcall set-cl v1))) t)
                             (eq (call-in-new-thread get-vm) v1)
                             (eq (call-in-new-thread (lambda () (symbol-value vm))) v1)
                             (progn (call-in-new-thread (lambda () (funcall set-vm v2))) t)
                             (eq (call-in-new-thread get-cl) v2)
                             (eq (call-in-new-thread (lambda () (symbol-value cl))) v2))
                     (call-in-new-thread (lambda () (funcall set-cl old))))))
          (check (format nil "~A: a binding through either name is one binding" tag)
                 (lambda ()
                   (let ((bind-vm (compile-quietly
                                   `(lambda () (let ((,vm 5)) (declare (special ,vm))
                                                 (list ,cl (symbol-value ',cl) ,vm (boundp ',cl))))))
                         (bind-cl (compile-quietly
                                   `(lambda () (let ((,cl 6)) (declare (special ,cl))
                                                 (list ,vm (symbol-value ',vm) ,cl (boundp ',vm))))))
                         (progv-vm (compile-quietly
                                    `(lambda (syms) (progv syms (list 7) (list ,cl (symbol-value ',cl)))))))
                     (and (equal (funcall bind-vm) '(5 5 5 t))
                          (equal (funcall bind-cl) '(6 6 6 t))
                          (equal (funcall progv-vm (list vm)) '(7 7))
                          (equal (funcall progv-vm (list cl)) '(7 7)))))))))
    (nreverse failures)))

(defun report-repl-variable-fallback (mode failures)
  (warn "VECTORIZED-MATH: the REPL variables VM:+ VM:- VM:* VM:/ could not be made aliases of ~
         the CL ones on ~A ~A (path ~S):~%~{  ~A~%~}Using global symbol macros instead: ~
         evaluating the names works, but BOUNDP and SYMBOL-VALUE of them do not."
        (lisp-implementation-type) (lisp-implementation-version) mode failures))

#+ccl
(defun try-alias-mode ()
  "Try :NATIVE or :DOWNSTREAM.  Returns the mode that works, or NIL (after rolling everything back)."
  (let ((mode (cond ((native-alias-function) :native)
                    #+x8664-target ((downstream-hooks-possible-p) :downstream)
                    (t nil))))
    (when mode
      (let ((failures
              (handler-case
                  (progn
                    #+x8664-target (when (eq mode :downstream) (install-downstream-hooks))
                    (alias-all-repl-variables mode)
                    (repl-variable-failures))
                (error (c) (list (format nil "installation signalled: ~A" c))))))
        (cond ((null failures) mode)
              (t (ignore-errors (remove-repl-variable-aliases mode))
                 (report-repl-variable-fallback mode failures)
                 nil))))))

(defun choose-repl-variable-mode ()
  (let ((mode (and (not (equal (repl-variable-environment-request) "symbol-macro"))
                   #+ccl (try-alias-mode)
                   #-ccl nil)))
    (or mode
        (progn (define-repl-symbol-macros) :symbol-macro))))

(defun establish-repl-variables ()
  "Choose and install the path for the REPL variables, once; returns the mode."
  (if *repl-variable-mode*
      (progn
        #+(and ccl x8664-target)
        (when (eq *repl-variable-mode* :downstream) (install-downstream-hooks))
        *repl-variable-mode*)
      (setf *repl-variable-mode* (choose-repl-variable-mode))))

(defun uninstall-repl-variables ()
  "Remove the aliases and hooks (for tests; the symbol macros of :SYMBOL-MACRO are not removed) and
forget the mode, so that ESTABLISH-REPL-VARIABLES chooses again."
  (when (member *repl-variable-mode* '(:native :downstream))
    (remove-repl-variable-aliases *repl-variable-mode*))
  (setf *repl-variable-mode* nil))

(establish-repl-variables)

) ; eval-when
