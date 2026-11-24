;;; Driven by relocate-mkl.sh.  Loads vectorized-math (with VM_MKL_LIBRARY set to a bogus
;;; path by the script), then moves to the relocated MKL with VM:LOAD-MKL.
(load "/home/vscode/quicklisp/setup.lisp")
(push (uiop:pathname-directory-pathname (uiop:getenv "VM_SYSTEM_DIR")) asdf:*central-registry*)
(defvar *warnings* 0)
(handler-bind ((warning (lambda (w) (when (search "VECTORIZED-MATH" (princ-to-string w))
                                      (incf *warnings*) (muffle-warning w)))))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload :cffi :silent t)
    ;; ASDF, not ql:quickload: Quicklisp muffles load-time warnings
    (asdf:load-system :vectorized-math)))
;; separate toplevel forms: the VM package must exist when they are read
(format t "RESULT after load: available=~S library=~S vm-warnings=~D~%" vm:*mkl-available* vm:*mkl-library* *warnings*)
(defvar *a* (make-array 1000 :element-type 'double-float :initial-element 0.5d0))
(defvar *lisp* (let ((vm:*mkl-available* nil)) (vm:exp *a*)))
(format t "RESULT lisp loop exp(0.5)=~S~%" (aref *lisp* 0))
(format t "RESULT load-mkl ~A => ~S~%" (uiop:getenv "RELOCATED_MKL") (vm:load-mkl (uiop:getenv "RELOCATED_MKL")))
(let ((mkl (vm:exp *a*)))
  (format t "RESULT mkl exp(0.5)=~S equal-to-lisp=~S~%" (aref mkl 0) (every #'= mkl *lisp*)))
