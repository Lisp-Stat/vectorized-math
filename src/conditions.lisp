;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VM.IMPL -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:vm.impl)

(define-condition non-conformable-arrays (error)
  ((message :initarg :message :reader non-conformable-message)
   (dimensions :initarg :dimensions :initform nil :reader non-conformable-dimensions))
  (:report (lambda (c s)
             (format s "Non-conformable arrays: ~A" (non-conformable-message c))))
  (:documentation "Signalled when operand shapes cannot be combined under the R recycling rules."))

(define-condition recycling-warning (warning)
  ((message :initarg :message :reader recycling-message)
   (longer :initarg :longer :initform nil :reader recycling-longer)
   (shorter :initarg :shorter :initform nil :reader recycling-shorter))
  (:report (lambda (c s)
             (format s "~A" (recycling-message c))))
  (:documentation "Signalled when a shorter operand does not recycle a whole number of times,
or when a value is truncated to fit an :into array."))

(define-condition output-type-error (type-error)
  ((value :initarg :value :initform nil :reader output-type-value))
  (:report (lambda (c s)
             (format s "Cannot store ~S into an output array of element type ~S."
                     (output-type-value c) (type-error-expected-type c))))
  (:documentation "Signalled when the :into array cannot hold the result (narrowing)."))
