;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
;;;
;;; The :into output argument: out[] <- value with R recycling rules.

(in-package #:vm.impl)

(defun output-error (value out-et)
  (error 'output-type-error :datum value :expected-type out-et :value value))

(defun float-et-info (et)
  "For a float or complex-float element type return (values precision complex-p) with
precision 1 for single and 2 for double; NIL otherwise."
  (cond ((eq et 'single-float) (values 1 nil))
        ((eq et 'double-float) (values 2 nil))
        ((equal et '(complex single-float)) (values 1 t))
        ((equal et '(complex double-float)) (values 2 t))
        (t nil)))

(defun float-widening-ok-p (v-et out-et)
  "May values of float element type V-ET be stored in OUT-ET without narrowing?"
  (or (eq out-et t)
      (multiple-value-bind (vp vc) (float-et-info v-et)
        (multiple-value-bind (op oc) (float-et-info out-et)
          (and op (<= vp op) (or oc (not vc)))))))

(defun convert-for-output (v out-et)
  "V converted to something storable in an array of element type OUT-ET, or an
OUTPUT-TYPE-ERROR.  Rationals are converted to float outputs as CL contagion would."
  (cond ((eq out-et t) v)
        ((typep v out-et) v)
        (t
         (multiple-value-bind (op oc) (float-et-info out-et)
           (cond
             ((null op) (output-error v out-et))
             ((typep v 'rational)
              (if oc
                  (complex (float v (if (= op 1) 1f0 1d0)) (float 0 (if (= op 1) 1f0 1d0)))
                  (float v (if (= op 1) 1f0 1d0))))
             ((typep v 'float)
              (if (and (typep v 'single-float) (<= 1 op))
                  (if oc
                      (complex (float v (if (= op 1) 1f0 1d0)) (float 0 (if (= op 1) 1f0 1d0)))
                      (if (= op 1) v (float v 1d0)))
                  (if (and (typep v 'double-float) (= op 2))
                      (if oc (complex v 0d0) v)
                      (output-error v out-et))))
             ((typep v 'complex)
              (let ((p (realpart v)) (proto (if (= op 1) 1f0 1d0)))
                (cond ((not oc) (output-error v out-et))
                      ((typep p 'rational)
                       (complex (float p proto) (float (imagpart v) proto)))
                      ((and (typep p 'single-float) (<= 1 op))
                       (complex (float p proto) (float (imagpart v) proto)))
                      ((and (typep p 'double-float) (= op 2)) v)
                      (t (output-error v out-et)))))
             (t (output-error v out-et)))))))

(defun colmajor-order-index (dims r)
  "Column-major index of the element with row-major index R in an array of dimensions DIMS."
  (let ((c 0) (stride 1) (subs '()))
    ;; subscripts from the last dimension backwards
    (dolist (d (reverse dims))
      (multiple-value-bind (q rem) (floor r d)
        (push rem subs) (setf r q)))
    (loop for i in subs for d in dims
          do (setf c (+ c (* i stride)) stride (* stride d)))
    c))

(defun rowmajor-from-colmajor (dims c)
  "Row-major index of the element with column-major index C in an array of dimensions DIMS."
  (let ((subs '()))
    (dolist (d dims)
      (multiple-value-bind (q rem) (floor c d)
        (push rem subs) (setf c q)))
    ;; SUBS is reversed (last dimension first): row-major Horner from the first dimension
    (let ((r 0))
      (loop for i in (reverse subs) for d in dims do (setf r (+ (* r d) i)))
      r)))

(defun store-into (value out)
  "Store VALUE (a number or a fresh simple array) into array OUT with R recycling
rules; return OUT."
  (unless (arrayp out) (error 'type-error :datum out :expected-type 'array))
  (let* ((out-et (array-element-type out))
         (nout (array-total-size out)))
    (when (zerop nout) (return-from store-into out))
    (cond
      ((not (arrayp value))
       (let ((v (convert-for-output value out-et)))
         (dotimes (i nout) (setf (row-major-aref out i) v))))
      (t
       (let* ((nv (array-total-size value))
              (v-et (array-element-type value))
              (vdims (array-dimensions value)) (odims (array-dimensions out))
              (same-layout (or (equal vdims odims)
                               (and (<= (length vdims) 1) (<= (length odims) 1)))))
         (when (zerop nv)
           (error 'non-conformable-arrays
                  :message "a zero-length value cannot fill a non-empty :into array"
                  :dimensions (list vdims odims)))
         ;; type-level check for float results
         (when (and (float-et-info v-et) (not (float-widening-ok-p v-et out-et)))
           (output-error (row-major-aref value 0) out-et))
         (cond ((> nv nout)
                (recycling-warn nv nout
                                (format nil "value of length ~D truncated to the ~D elements of the :into array"
                                        nv nout)))
               ((/= 0 (mod nout nv))
                (recycling-warn nout nv
                                (format nil "the ~D elements of the :into array are not a multiple of the value length ~D"
                                        nout nv))))
         (let ((fast (and (typep out 'simple-array) (typep value 'simple-array)
                          (equal v-et out-et) same-layout)))
           (cond
             ;; same element type and layout: straight copies of the flat data
             ((and fast (>= nv nout))
              (replace (flat-data out) (flat-data value) :end2 nout))
             (fast
              (let ((od (flat-data out)) (vd (flat-data value)))
                (loop for k from 0 below nout by nv
                      do (replace od vd :start1 k :end1 (min nout (+ k nv))))))
             (t
              ;; general path: validate every element first, then fill
              (flet ((vref (j)
                       (row-major-aref value
                                       (if (<= (length vdims) 1) j (rowmajor-from-colmajor vdims j))))
                     (oidx (r) (if (<= (length odims) 1) r (colmajor-order-index odims r))))
                (dotimes (r nout)
                  (convert-for-output (vref (mod (oidx r) nv)) out-et))
                (dotimes (r nout)
                  (setf (row-major-aref out r)
                        (convert-for-output (vref (mod (oidx r) nv)) out-et))))))))))
    out))
