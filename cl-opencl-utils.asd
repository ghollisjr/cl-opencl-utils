(defsystem cl-opencl-utils
    :serial t
    :description "OpenCL utility library built on cl-opencl"
    :license "GPLv3"
    :author "Gary Hollis"
    :depends-on (:cl-opencl)
    :components
    ((:file "package")
     (:file "utils")
     ;; OpenCL Lispified C system
     (:file "macro")
     (:file "clc")
     (:file "clc-utils")
     (:file "function")
     (:file "struct")
     (:file "syntax")
     (:file "headers")
     (:file "var")
     (:file "control")
     (:file "bool")
     (:file "math")
     (:file "raw-functions")
     (:file "compile")
     ;; OpenCL C math utilities
     (:file "complex")
     (:file "math-functions")
     (:file "libcerf")
     ;; Cleanup
     (:file "cleanup")
     ;; Map-reduce
     (:file "map-reduce")
     ;; Convolutions
     (:file "convolution")
     ;; Function sampling
     (:file "function-sampling")
     ;; Integration
     (:file "integration")
     ;; Splines
     (:file "spline")
     ))
