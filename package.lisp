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
   :make-opencl-mapper
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
   ;; raw-functions
   :defrawclcfun
   :deffileclcfun
   ;; compile
   :program-source-from-forms-fn
   :program-source-from-forms
   :program-source-from-kernels-fn
   :program-source-from-kernels
   ))
