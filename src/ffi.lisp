;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; Direct CFFI bindings to the MKL Vector Math Library, array pinning, FP trap
;;; masking and per-thread VML mode.  Based on the phase 1 spike
;;; (.agent-work/spikes/vm-ffi/REPORT.md).

(in-package #:vm.impl)

;;; --- locating and loading MKL ---
;;; vectorized-math depends only on CFFI and MKL itself.  The single dynamic
;;; library libmkl_rt is loaded here; its location can be set with the
;;; environment variable VM_MKL_LIBRARY (read at load time), VM:*MKL-LIBRARY*
;;; and VM:LOAD-MKL.  Nothing else is needed (no setvars.sh).

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *mkl-available* nil
  "True when the VML symbols could be found at load time.  When NIL every
operation runs in Lisp loops.")

(defvar *mkl-library*
  (let ((env (uiop:getenv "VM_MKL_LIBRARY")))
    (and env (plusp (length env)) env))
  "Full path of the MKL single dynamic library (libmkl_rt), or NIL to use the
system's default dynamic library search.  Initialised from the environment
variable VM_MKL_LIBRARY when the system is loaded.")

(cffi:define-foreign-library libmkl-rt
  (:darwin (:or "libmkl_rt.2.dylib" "libmkl_rt.dylib"))
  (:unix (:or "libmkl_rt.so.3" "libmkl_rt.so.2" "libmkl_rt.so"))
  (:windows (:or "mkl_rt.3.dll" "mkl_rt.2.dll" "mkl_rt.dll"))
  (t (:default "libmkl_rt")))

(defun mkl-symbols-present-p ()
  (and (ignore-errors (cffi:foreign-symbol-pointer "vdAdd"))
       (ignore-errors (cffi:foreign-symbol-pointer "vmlSetMode"))
       t))

(defparameter *mkl-companion-libraries*
  '("libiomp5" "libmkl_core" "libmkl_intel_lp64" "libmkl_intel_thread"
    "libmkl_def" "libmkl_mc" "libmkl_mc3" "libmkl_avx" "libmkl_avx2" "libmkl_avx512" "libmkl_avx10"
    "libmkl_vml_def" "libmkl_vml_cmpt" "libmkl_vml_mc" "libmkl_vml_mc2" "libmkl_vml_mc3"
    "libmkl_vml_avx" "libmkl_vml_avx2" "libmkl_vml_avx512" "libmkl_vml_avx10")
  "Base names of the libraries libmkl_rt itself loads (by bare name, so through the system's
search path) when it is first used.  When MKL is loaded from an explicit path, those found in
the same directory are loaded first, from there, so that the dynamic loader matches them by
soname instead of searching, and MKL can be moved anywhere.  See README, Locating MKL.")

(defun preload-mkl-companions (directory)
  "Load the files of *MKL-COMPANION-LIBRARIES* found in DIRECTORY, in that order."
  (let ((found '()))
    (dolist (file (uiop:directory-files directory))
      (let* ((fname (file-namestring file))
             (dot (search ".so" fname))
             (pos (and dot (position (subseq fname 0 dot) *mkl-companion-libraries* :test #'string=))))
        (when pos (push (cons pos file) found))))
    (dolist (entry (sort found #'< :key #'car))
      (ignore-errors (cffi:load-foreign-library (uiop:native-namestring (cdr entry)))))))

(defun load-mkl-library (path)
  "Load the MKL library from PATH (NIL: default search).  Returns true on success.
Signals no error."
  (handler-case
      (progn
        (cond (path
               (preload-mkl-companions (uiop:pathname-directory-pathname path))
               (cffi:load-foreign-library (uiop:native-namestring path)))
              (t (cffi:load-foreign-library 'libmkl-rt)))
        t)
    (error () nil)))

(defun check-mkl (&optional (loaded t))
  "Check the VML symbols; set *MKL-AVAILABLE*; warn once if absent.  LOADED false means that
an explicitly requested library could not be loaded: MKL is then unavailable, whatever other
copy may already be in the process."
  (setf *mkl-available* (and loaded (mkl-symbols-present-p)))
  (unless *mkl-available*
    (warn "VECTORIZED-MATH: MKL Vector Math symbols not found~@[ (library ~A)~]; using Lisp loops for all array operations."
          *mkl-library*))
  *mkl-available*)

(defun load-mkl (&optional (path *mkl-library*))
  "(Re)load MKL from PATH, the full path of libmkl_rt (default: *MKL-LIBRARY*; when that is
NIL too, the system's default search).  Checks that the required symbols are present, sets
*MKL-AVAILABLE* and returns it.  Signals one warning and returns NIL when MKL cannot be
used; the Lisp loops are then used.  Call it before using any array operation: it does not
change the library of calls already made."
  (setf *mkl-library* path)
  (check-mkl (or (load-mkl-library path) (null path))))

;;; The library must be loaded before the bindings below are compiled and loaded.
(defvar *mkl-loaded* (or (load-mkl-library *mkl-library*) (null *mkl-library*)))
) ; eval-when

;;; --- bindings (LP64: MKL_INT is a C int) ---

(defmacro def-vml-binary (opname &optional (letters '("s" "d" "c" "z")))
  "Bind v{s,d,c,z}OPNAME and v{s,d,c,z}OPNAMEI (for the LETTERS that exist) as
%V{S,D,C,Z}-OPNAME and ...-I."
  `(progn
     ,@(loop for letter in letters
             append
             (let* ((lname (string-upcase letter))
                    (plain (intern (format nil "%V~A-~A" lname (string-upcase opname)) '#:vm.impl))
                    (inc (intern (format nil "%V~A-~A-I" lname (string-upcase opname)) '#:vm.impl)))
               `((cffi:defcfun (,(format nil "v~A~A" letter opname) ,plain) :void
                   (n :int) (a :pointer) (b :pointer) (r :pointer))
                 (cffi:defcfun (,(format nil "v~A~AI" letter opname) ,inc) :void
                   (n :int) (a :pointer) (ia :int) (b :pointer) (ib :int) (r :pointer) (ir :int)))))))

(defmacro def-vml-unary (opname &optional (letters '("s" "d" "c" "z")))
  "Bind v{s,d,c,z}OPNAME (n a r) for the LETTERS that exist as %V{S,D,C,Z}-OPNAME.  The
pointer types differ between operations (complex -> real for Abs and Arg, real -> complex
for CIS) but all are pointers."
  `(progn
     ,@(loop for letter in letters
             collect
             (let ((plain (intern (format nil "%V~A-~A" (string-upcase letter) (string-upcase opname))
                                  '#:vm.impl)))
               `(cffi:defcfun (,(format nil "v~A~A" letter opname) ,plain) :void
                  (n :int) (a :pointer) (r :pointer))))))

(def-vml-binary "Add")
(def-vml-binary "Sub")
(def-vml-binary "Mul")
(def-vml-binary "Div")
(def-vml-binary "Pow")
(def-vml-binary "Atan2" ("s" "d"))
(def-vml-unary "Exp") (def-vml-unary "Ln") (def-vml-unary "Sqrt")
(def-vml-unary "Sin") (def-vml-unary "Cos") (def-vml-unary "Tan")
(def-vml-unary "Asin") (def-vml-unary "Acos") (def-vml-unary "Atan")
(def-vml-unary "Sinh") (def-vml-unary "Cosh") (def-vml-unary "Tanh")
(def-vml-unary "Asinh") (def-vml-unary "Acosh") (def-vml-unary "Atanh")
(def-vml-unary "Abs")
(def-vml-unary "Arg" ("c" "z"))
(def-vml-unary "CIS" ("c" "z"))
(cffi:defcfun ("vmlSetMode" %vml-set-mode) :unsigned-int (mode :unsigned-int))
(cffi:defcfun ("vmlGetMode" %vml-get-mode) :unsigned-int)

(check-mkl *mkl-loaded*)

;;; --- VML mode: HA | ERRMODE_IGNORE | FTZDAZ_OFF.  Per thread. ---
;;; Choice (see the report): the mode is cached per thread.  *VML-MODE-THREAD*
;;; holds the last thread that set the mode; a thread skips the FFI call when it
;;; finds itself there.  If threads interleave they merely call vmlSetMode more
;;; often; a thread never skips the call unless it set the mode itself.

(defconstant +vml-mode+ #x140102)
(defvar *vml-mode-thread* nil)

(declaim (inline current-thread-object))
(defun current-thread-object ()
  #+sbcl sb-thread:*current-thread*
  #+ccl ccl:*current-process*
  #-(or sbcl ccl) nil)

(defmacro ensure-vml-mode ()
  `(let ((th (current-thread-object)))
     (unless (eq *vml-mode-thread* th)
       (%vml-set-mode +vml-mode+)
       (setf *vml-mode-thread* th))))

;;; --- array access ---

(defun flat-data (array)
  "The 1-D storage vector of a simple ARRAY of any rank (the array itself when rank 1)."
  (declare (type simple-array array))
  (if (= (array-rank array) 1)
      array
      #+sbcl (sb-ext:array-storage-vector array)
      #+ccl (values (ccl::array-data-and-offset array))
      #-(or sbcl ccl) (error "flat-data: unsupported implementation")))

;;; WITH-PINNED-PTR takes a simple 1-D specialised vector (as returned by FLAT-DATA).
#+sbcl
(defmacro with-pinned-ptr ((ptr vector) &body body)
  (let ((v (gensym "V")))
    `(let ((,v ,vector))
       (sb-sys:with-pinned-objects (,v)
         (let ((,ptr (sb-sys:vector-sap ,v)))
           ,@body)))))

#+ccl
(defmacro with-pinned-ptr ((ptr vector) &body body)
  (let ((v (gensym "V")))
    `(let ((,v ,vector))
       ;; No adjustment for (complex double-float) vectors: CCL 1.14 and
       ;; earlier return the address of a 64-bit pad before their data, but
       ;; per project policy CCL bugs are fixed in CCL (Clozure/ccl#737), not
       ;; worked around here.  See README, Known Issues.
       (ccl:with-pointer-to-ivector (,ptr ,v)
         ,@body))))

(declaim (inline ptr+))
(defun ptr+ (ptr bytes)
  #+sbcl (sb-sys:sap+ ptr bytes)
  #+ccl (ccl:%inc-ptr ptr bytes))

;;; --- trap masking ---

(defmacro with-masked-float-traps (&body body)
  "Run BODY with all FP traps masked; sticky status flags are cleared afterwards."
  #+sbcl `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
            ,@body)
  #+ccl `(let ((saved (ccl::%get-mxcsr)))
           (declare (fixnum saved))
           (unwind-protect
                (progn (ccl::%set-mxcsr (logior saved #x1F80)) ,@body)
             (ccl::%set-mxcsr (logandc2 saved #x3F))))
  #-(or sbcl ccl) `(progn ,@body))

;;; --- NaN test (bit level) ---
;;;
;;; A NaN is recognised from its IEEE bits, never from a comparison: comparisons of a NaN are
;;; not trustworthy on every implementation (CCL's treat a NaN as equal to everything, and its
;;; compiler folds (/= x x) to false), and a comparison-based test would silently break when
;;; the implementation's comparison changes.  SBCL: SB-EXT:FLOAT-NAN-P (inline).  CCL: the float
;;; is stored to a small stack scratch block and its 32-bit halves are read back (inline, no
;;; consing; CCL::DOUBLE-FLOAT-BITS would box the float).  Kernels bind the scratch block once
;;; per call with WITH-NAN-SCRATCH; NAN-P uses it when it is lexically visible and allocates a
;;; block of its own otherwise (slower, correct).

#+ccl
(defmacro with-nan-scratch (&body body)
  `(ccl:%stack-block ((%nan-scratch 8))
     (declare (ignorable %nan-scratch))
     ,@body))
#-ccl
(defmacro with-nan-scratch (&body body)
  `(progn ,@body))

(defmacro nan-p (x type &environment env)
  "Inline NaN test of the TYPE float X (TYPE a literal symbol, SINGLE-FLOAT or DOUBLE-FLOAT)."
  #+sbcl (progn env `(sb-ext:float-nan-p (the ,type ,x)))
  #+ccl
  (let* ((double (eq type 'double-float))
         (v (gensym "V")) (hi (gensym "HI")) (lo (gensym "LO")) (h (gensym "H"))
         (bound (eq (ccl:variable-information '%nan-scratch env) :lexical))
         (p (if bound '%nan-scratch (gensym "P")))
         (body (if double
                   `(progn
                      (setf (ccl::%get-double-float ,p 0) ,v)
                      (let* ((,hi (ccl::%get-unsigned-long ,p 4))
                             (,lo (ccl::%get-unsigned-long ,p 0))
                             (,h (logand ,hi #x7fffffff)))
                        (declare (type (unsigned-byte 32) ,hi ,lo ,h))
                        (or (> ,h #x7ff00000) (and (= ,h #x7ff00000) (/= ,lo 0)))))
                   `(progn
                      (setf (ccl::%get-single-float ,p 0) ,v)
                      (> (logand (the (unsigned-byte 32) (ccl::%get-unsigned-long ,p 0)) #x7fffffff)
                         #x7f800000)))))
    #-little-endian-target (error "NAN-P assumes a little-endian target")
    `(let ((,v ,x))
       (declare (type ,type ,v))
       ,(if bound body `(ccl:%stack-block ((,p 8)) ,body))))
  #-(or sbcl ccl) `(let ((v ,x)) (/= v (opaque-float v))))
