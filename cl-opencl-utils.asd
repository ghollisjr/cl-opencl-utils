(defsystem cl-opencl-utils
    :serial t
    :description "OpenCL utility library built on cl-opencl"
    :license "Public Domain"
    :author "Gary Hollis"
    :depends-on (:cl-opencl)
    :components
    ((:file "package")
     (:file "cl-opencl-utils")))
