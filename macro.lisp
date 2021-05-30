(in-package :cl-opencl-utils)

(defparameter *clc-macros*
  (make-hash-table :test 'eq))

(defmacro defclcmacro (name (&rest args) &body body)
  "Defines a Lispified OpenCL C macro."
  `(setf (gethash ',name *clc-macros*)
         (lambda (,@args) ,@body)))

(defun expand-clc-macros (expr)
  "Finds & repeatedly expands any res-macros present in expr until
none are present."
  (let ((macs *clc-macros*))
    (labels ((rec (f &optional no-op)
               ;; non-nil no-op means that the form being passed
               ;; should not be treated for operator content.
               (let ((result
                      (cond
                        ((atom f)
                         f)
                        ((and (not no-op)
                              (gethash (first f) macs))
                         (let ((newf (apply (gethash (first f) macs)
                                            (rest f))))
                           (rec newf)))
                        (t
                         (cons (rec (car f))
                               (rec (cdr f) t))))))
                 result)))
      (rec expr))))
