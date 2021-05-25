(in-package :cl-opencl-utils)

;;;; Utilities for managing OpenCL and foreign memory allocation

(defmacro with-opencl-cleanup ((var return-cleanup) &body body)
  "Many functions in cl-opencl-utils return a list (object cleanup)
where cleanup is called once object is no longer needed.  This macro
sets var to the value of object and automatically calls the cleanup
function at the end of the body."
  (let* ((rc (gensym "RC"))
         (cleanup (gensym "CLEANUP"))
         (retval (gensym "RETVAL")))
    `(let* ((,rc ,return-cleanup)
            (,var (first ,rc))
            (,cleanup (second ,rc))
            (,retval (progn ,@body)))
       (funcall ,cleanup)
       ,retval)))
