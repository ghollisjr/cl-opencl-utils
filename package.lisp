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
   :+OPENCL-COMPLEX-ADD-REXPR+
   :make-opencl-reducer
   ;; maps
   :make-opencl-mapper
   ;; convolution
   :make-opencl-convolutor
   ;; sampling functions
   :make-opencl-function-sampler
   ;; integration
   :make-opencl-integrator
   :make-opencl-complex-integrator
   ;; splines
   :eval_cubic_spline
   :eval_cubic_spline_f
   :serialize-spline-coef-array
   :make-opencl-spline-form
   ;; RNG
   :pcg32
   :pcg32_init
   :uniform_random
   :uniform_randomf
   ;; BLAS
   :make-opencl-axpy
   ;; RK4
   :make-opencl-rk4
   ;; cleanup
   :with-opencl-cleanup
   ;;; OpenCL Lispified C DSL
   ;; clc
   :clc
   ;; clc-utils
   :copy-array
   ;; macro
   :defclcmacro
   ;; function
   :set-explicit-clc-functions
   :defclcfun
   :undefclcfun
   :defclckernel
   :undefclckernel
   ;; struct
   :defclcstruct
   :defclcstruct-no-cffi
   :undefclcstruct
   :pmember
   ;; syntax
   :call
   :struct
   :sizeof
   :str
   :pmember
   :typecast
   :concat
   :statement
   :statements
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
   :defclcglobalvar
   :var
   :vararray
   :varfpointer
   :global
   :constant
   :local
   :private
   :const
   :pointer
   :fpointer
   :address
   :value
   ;; control
   :for
   :while
   :do-while
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
   :program-source-from-functions-fn
   :program-source-from-functions
   :program-source-from-kernels-fn
   :program-source-from-kernels
   ;;; OpenCL C functions
   ;; General
   ;; Distributions
   :gaussian
   :skewed-gaussian
   :lorentz
   :voigt
   :skewed-voigt
   ;; Complex numbers
   :cl_complex
   :complex+
   :complex-
   :complex*
   :complex/
   :complexr* ; scalar multiply
   :complex_conj
   :cabs
   :cabs2
   :complex_polar ; make complex number from (r,theta)
   :cexp
   ;; libcerf
   :cerfcx
   :cerfi
   :erfi
   :dawson
   :faddeeva_re_w_of_z
   :faddeeva_im_w_of_z
   :voigt
   :cerf
   :cerfc
   :cdawson
   :erfcx
   :faddeeva_im_w_of_x
   :faddeeva_w_of_z))
