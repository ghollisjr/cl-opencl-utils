(defpackage #:cl-opencl-utils
  (:use :cl
        :cffi
        :cl-opencl)
  (:export
   ;; utils
   :c-type-name
   ;; reductions
   :+OPENCL-ADD-REXPR+
   :make-opencl-reducer
   ;; maps
   :make-opencl-mapper))
