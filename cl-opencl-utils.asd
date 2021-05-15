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
     (:file "clc")
     (:file "clc-utils")
     (:file "function")
     (:file "syntax")
     (:file "headers")
     (:file "var")
     (:file "control")
     (:file "bool")
     (:file "math")
     (:file "raw-functions")
     (:file "compile")
     ;; Map-reduce
     (:file "map-reduce")
     ;; Convolutions
     (:file "convolution")
     ))
