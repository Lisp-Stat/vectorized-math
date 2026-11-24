;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; The R recycling engine with column-major emulation.

(in-package #:vm.impl)

(defconstant +min-block+ 16
  "Smallest block (shorter vector length, or row length) for which repeated kernel calls
on sub-ranges are used; below it the vector is expanded into a temporary instead.")

(defun recycling-warn (longer shorter message)
  (warn 'recycling-warning :message message :longer longer :shorter shorter))

(defun non-conformable (message &rest dims)
  (error 'non-conformable-arrays :message message :dimensions dims))

(defun operand-dims (o) (array-dimensions (operand-value o)))
(defun operand-size (o) (array-total-size (operand-value o)))
(defun operand-rank (o) (array-rank (operand-value o)))

(defun analyze-shape (a b)
  "Decide how to combine operands A and B (at least one an array).
Returns (values DIMS N PATH EXTRA) where PATH is one of
  :empty             zero-length result
  :scalar-a :scalar-b  one operand is a scalar
  :flat              same layout, one kernel call
  :vec-scalar-a/b    the vector operand has one element: broadcast it (EXTRA unused)
  :blocks            vector-vector, EXTRA = A-is-longer-p
  :cyclic            vector-vector expanded to a temporary, EXTRA = A-is-longer-p
  :rows              matrix + vector of length = first dimension, EXTRA = A-is-matrix-p
  :colmajor          matrix + vector expanded in column-major order, EXTRA = A-is-matrix-p"
  (let ((aa (operand-array-p a)) (ba (operand-array-p b)))
    (cond
      ((not aa) (values (operand-dims b) (operand-size b)
                        (if (zerop (operand-size b)) :empty :scalar-a) nil))
      ((not ba) (values (operand-dims a) (operand-size a)
                        (if (zerop (operand-size a)) :empty :scalar-b) nil))
      (t
       (let* ((da (operand-dims a)) (db (operand-dims b))
              (ra (length da)) (rb (length db))
              (sa (operand-size a)) (sb (operand-size b)))
         (cond
           ;; two arrays of rank >= 2
           ((and (>= ra 2) (>= rb 2))
            (unless (equal da db)
              (non-conformable (format nil "dimensions ~S and ~S differ" da db) da db))
            (values da sa (if (zerop sa) :empty :flat) nil))
           ;; matrix and vector
           ((or (>= ra 2) (>= rb 2))
            (let* ((a-matrix (>= ra 2))
                   (dm (if a-matrix da db)) (sm (if a-matrix sa sb))
                   (sv (if a-matrix sb sa)))
              (when (> sv sm)
                (non-conformable
                 (format nil "vector of length ~D is longer than the array of dimensions ~S (~D elements)"
                         sv dm sm)
                 da db))
              (cond
                ((zerop sv) (values (if (zerop sm) dm (list 0)) 0 :empty nil))
                ((zerop sm) (values dm 0 :empty nil))
                (t
                 (unless (zerop (mod sm sv))
                   (recycling-warn sm sv
                                   (format nil "array of ~D elements is not a multiple of the vector length ~D"
                                           sm sv)))
                 (let ((d0 (first dm)))
                   (cond
                     ((= sv 1) (values dm sm (if a-matrix :vec-scalar-b :vec-scalar-a) nil))
                     ((and (= sv sm) (every (lambda (d) (= d 1)) (rest dm)))
                      (values dm sm :flat nil))
                     ((and (= sv d0) (>= (floor sm d0) +min-block+))
                      (values dm sm :rows a-matrix))
                     (t (values dm sm :colmajor a-matrix))))))))
           ;; two vectors
           (t
            (let ((n (max sa sb)) (m (min sa sb)))
              (cond
                ((zerop m) (values (list 0) 0 :empty nil))
                ;; two rank-0 arrays: a length-1 operation with a rank-0 result
                ((and (= ra 0) (= rb 0)) (values nil 1 :flat nil))
                ((= sa sb) (values (list n) n :flat nil))
                (t
                 (unless (zerop (mod n m))
                   (recycling-warn n m
                                   (format nil "longer vector length ~D is not a multiple of the shorter length ~D"
                                           n m)))
                 (let ((a-longer (> sa sb)))
                   (cond ((= m 1) (values (list n) n (if a-longer :vec-scalar-b :vec-scalar-a) nil))
                         ((>= m +min-block+) (values (list n) n :blocks a-longer))
                         (t (values (list n) n :cyclic a-longer))))))))))))))

(defun expand-cyclic (v m n)
  "Fresh vector of N elements, element r = V[r mod m]."
  (declare (type fixnum m n))
  (let ((out (make-array n :element-type (array-element-type v))))
    (dotimes (r n out)
      (setf (aref out r) (aref (the (simple-array * (*)) v) (mod r m))))))

(defun expand-colmajor (v m dims)
  "Fresh vector with the array size of DIMS laid out row-major, where the element with
subscripts (i0 i1 ...) is V[c mod m], c being its column-major index."
  (declare (type fixnum m))
  (let* ((rank (length dims))
         (size (reduce #'* dims))
         (out (make-array size :element-type (array-element-type v)))
         (dv (coerce dims 'simple-vector))
         ;; column-major strides
         (cs (let ((s (make-array rank)) (acc 1))
               (dotimes (k rank s) (setf (svref s k) acc) (setf acc (* acc (svref dv k))))))
         (idx (make-array rank :initial-element 0))
         (c 0)
         (src (the (simple-array * (*)) v)))
    (declare (type fixnum rank size c))
    (dotimes (r size out)
      (setf (aref out r) (aref src (mod c m)))
      ;; advance the row-major odometer, updating c incrementally
      (loop for k from (1- rank) downto 0
            do (incf (the fixnum (svref idx k)))
               (incf c (the fixnum (svref cs k)))
               (if (< (the fixnum (svref idx k)) (the fixnum (svref dv k)))
                   (return)
                   (progn (decf c (* (the fixnum (svref idx k)) (the fixnum (svref cs k))))
                          (setf (svref idx k) 0)))))))

(defun make-result (key dims)
  (make-array dims :element-type (key-element-type key)))

(defun run-binary (a b key-a key-b out-key kernel-name kernel-key shape)
  "Run the binary kernel (KERNEL-NAME KERNEL-KEY) over operands A and B (at least one an
array) combined according to SHAPE, the list of the values of ANALYZE-SHAPE.  Operand data
is converted to KEY-A / KEY-B; the result is a fresh array of element type OUT-KEY."
  (destructuring-bind (dims n path extra) shape
    (declare (type fixnum n))
    (let ((result (make-result out-key dims)))
      (unless (eq path :empty)
        (let* ((va (operand-value a)) (vb (operand-value b))
               (da (if (arrayp va) (prepare-data va key-a) (coerce-scalar va key-a)))
               (db (if (arrayp vb) (prepare-data vb key-b) (coerce-scalar vb key-b)))
               (out (flat-data result)))
          ;; only the kernel variants this path uses are looked up
          (flet ((run ()
                   (ecase path
                     (:scalar-a (funcall (the function (find-kernel kernel-name kernel-key :sa)) out 0 da db 0 n))
                     (:scalar-b (funcall (the function (find-kernel kernel-name kernel-key :as)) out 0 da 0 db n))
                     (:flat (funcall (the function (find-kernel kernel-name kernel-key :aa)) out 0 da 0 db 0 n))
                     (:vec-scalar-a (funcall (the function (find-kernel kernel-name kernel-key :sa)) out 0 (aref da 0) db 0 n))
                     (:vec-scalar-b (funcall (the function (find-kernel kernel-name kernel-key :as)) out 0 da 0 (aref db 0) n))
                     (:blocks
                      (let* ((kaa (find-kernel kernel-name kernel-key :aa))
                             (m (if extra (length (the vector db)) (length (the vector da)))))
                        (declare (type function kaa))
                        (loop for k of-type fixnum from 0 below n by m
                              do (let ((c (min m (- n k))))
                                   (if extra
                                       (funcall kaa out k da k db 0 c)
                                       (funcall kaa out k da 0 db k c))))))
                     (:cyclic
                      (let ((kaa (find-kernel kernel-name kernel-key :aa)))
                        (declare (type function kaa))
                        (if extra
                            (funcall kaa out 0 da 0
                                     (expand-cyclic db (length (the vector db)) n) 0 n)
                            (funcall kaa out 0
                                     (expand-cyclic da (length (the vector da)) n) 0 db 0 n))))
                     (:rows
                      (let* ((kas (find-kernel kernel-name kernel-key :as))
                             (ksa (find-kernel kernel-name kernel-key :sa))
                             (d0 (first dims)) (blk (floor n d0)))
                        (declare (type function kas ksa))
                        (dotimes (i d0)
                          (if extra
                              (funcall kas out (* i blk) da (* i blk) (aref db i) blk)
                              (funcall ksa out (* i blk) (aref da i) db (* i blk) blk)))))
                     (:colmajor
                      (let ((kaa (find-kernel kernel-name kernel-key :aa)))
                        (declare (type function kaa))
                        (if extra
                            (funcall kaa out 0 da 0
                                     (expand-colmajor db (length (the vector db)) dims) 0 n)
                            (funcall kaa out 0
                                     (expand-colmajor da (length (the vector da)) dims) 0 db 0 n)))))))
            (if (eq out-key :signed-byte-64)
                (run)
                (with-masked-float-traps (run))))))
      result)))

(defun binary-arrays (opname a b)
  "Pairwise elementwise OPNAME of operands A and B (at least one an array).
Returns a fresh operand."
  (multiple-value-bind (key lo hi narrow base) (result-plan opname a b)
    (multiple-value-bind (dims n path extra) (analyze-shape a b)
      (let ((result (run-binary a b key key key opname key (list dims n path extra))))
        (finish-result result key lo hi narrow base (or (operand-cplx a) (operand-cplx b)))))))

(defun finish-result (result key lo hi narrow base cplx)
  "Wrap RESULT in an operand; narrow generic integer results to (signed-byte 64) if they fit.
Results of element type T are tracked as :RAT (rationals, possibly complex) when the
operands were integers or rationals, so that later float contagion produces float arrays."
  (cond ((key-float-p key)
         (ecase key
           (:single-float (make-operand result :s nil))
           (:double-float (make-operand result :d nil))
           (:complex-single-float (make-operand result :s t))
           (:complex-double-float (make-operand result :d t))))
        ((eq key :signed-byte-64) (make-operand result :int nil lo hi))
        (narrow
         (multiple-value-bind (arr nlo nhi) (narrow-to-sb64 result)
           (if nlo
               (make-operand arr :int nil nlo nhi)
               (make-operand arr :rat nil))))
        ((member base '(:int :rat)) (make-operand result :rat (and (eq base :rat) cplx)))
        (t (make-operand result :gen nil))))

(defun copy-operand-array (o)
  "Fresh array holding the elements of array operand O, in the representation (+ O) yields."
  (let* ((arr (operand-value o))
         (key (ecase (operand-base o)
                ((:s :d) (base-cplx-key (operand-base o) (operand-cplx o)))
                (:int (if (and (<= +sb64-min+ (operand-lo o)) (<= (operand-hi o) +sb64-max+))
                          :signed-byte-64 :t))
                ((:gen :rat) :t)))
         (result (make-result key (array-dimensions arr)))
         (src (prepare-data arr key)))
    (if (eq src (flat-data arr))
        (replace (flat-data result) src :end2 (array-total-size arr))
        (replace (flat-data result) src))
    (case key
      (:signed-byte-64 (make-operand result :int nil (operand-lo o) (operand-hi o)))
      (:t (case (operand-base o)
            (:int (finish-result result :t nil nil t :int nil))
            (:rat (make-operand result :rat (operand-cplx o)))
            (t (make-operand result :gen nil))))
      (t (make-operand result (operand-base o) (operand-cplx o))))))

(defun negate-array (o)
  "Elementwise negation of array operand O: a fresh operand."
  (let* ((arr (operand-value o)) (base (operand-base o)) (cplx (operand-cplx o)))
    (multiple-value-bind (key lo hi narrow)
        (ecase base
          ((:s :d) (values (base-cplx-key base cplx) nil nil nil))
          (:int (let ((lo (- (operand-hi o))) (hi (- (operand-lo o))))
                  (if (and (<= +sb64-min+ (operand-lo o)) (<= (operand-hi o) +sb64-max+)
                           (fits-sb64 lo) (fits-sb64 hi))
                      (values :signed-byte-64 lo hi nil)
                      (values :t nil nil t))))
          ((:rat :gen) (values :t nil nil nil)))
      (let ((n (array-total-size arr))
            (result (make-result key (array-dimensions arr))))
        (when (plusp n)
          (funcall (find-kernel "neg" key :u) (flat-data result) 0 (prepare-data arr key) 0 n))
        (finish-result result key lo hi narrow base cplx)))))
