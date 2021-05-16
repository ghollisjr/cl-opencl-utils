(defpackage #:cl-opencl-utils
  (:use :cl
        :cffi
        :cl-opencl)
  (:export
   ;; utils
   :c-type-name
   :opencl-function-expr
   ;; reductions
   :+OPENCL-ADD-REXPR+
   :make-opencl-reducer
   ;; maps
   :make-opencl-mapper
   ;; convolution
   :make-opencl-convolutor
   ;; Sampling functions
   :make-opencl-function-sampler
   ;; Midpoint rule integration
   :make-opencl-integrator
   ;;; OpenCL Lispified C DSL
   ;; clc
   :clc
   ;; clc-utils
   :copy-array
   ;; function
   :set-explicit-clc-functions
   :defclcfun
   :undefclcfun
   :defclckernel
   :undefclckernel
   ;; syntax
   :call
   :struct
   :sizeof
   :str
   :pmember
   :typecast
   :concat
   :kernel
   :+CLK-LOCAL-MEM-FENCE+
   :+CLK-GLOBAL-MEM-FENCE+
   ;; headers
   :set-explicit-headers
   :explicit-headers
   :defheader-fn
   :defheader
   :with-defheader
   :required-headers ; required headers for program
   :compile-flags ; compile flags for program
   ;; var
   :var
   :vararray
   :global
   :constant
   :local
   :private
   :const
   :pointer
   :address
   :value
   ;; control
   :for
   :while
   ;; math
   :int-/
   :long-/
   :pow
   :<<
   :>>
   ;;; math constants
   :+maxfloat+
   :+huge-valf+
   :+infinity+
   :+nan+
   ;; double
   :+e+
   :+log2e+
   :log10e+
   :ln2+
   :+ln10+
   :+pi+
   :+pi/2+
   :+pi/4+
   :+1/pi+
   :+2/pi+
   :+2/sqrtpi+
   :+sqrt2+
   :+1/sqrt2+
   ;; single
   :+e-f+
   :+log2e-f+
   :log10e-f+
   :ln2-f+
   :+ln10-f+
   :+pi-f+
   :+pi/2-f+
   :+pi/4-f+
   :+1/pi-f+
   :+2/pi-f+
   :+2/sqrtpi-f+
   :+sqrt2-f+
   :+1/sqrt2-f+
   ;; half
   :+e-h+
   :+log2e-h+
   :log10e-h+
   :ln2-h+
   :+ln10-h+
   :+pi-h+
   :+pi/2-h+
   :+pi/4-h+
   :+1/pi-h+
   :+2/pi-h+
   :+2/sqrtpi-h+
   :+sqrt2-h+
   :+1/sqrt2-h+
   ;; raw-functions
   :defrawclcfun
   :deffileclcfun
   ;; compile
   :program-source-from-forms-fn
   :program-source-from-forms
   :program-source-from-kernels-fn
   :program-source-from-kernels
   ))
