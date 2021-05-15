(in-package :cl-opencl-utils)

(defmacro defrawclcfun (type name clc-args body-string
                        &key
                          headers
                          cheaders
                          functions)
  "Defines a OpenCL C function from its type, name, argument list, string
for body, and allows specification of explicit header, cheader and
function dependencies of the function.  No arguments are evaluated.
Make sure that the string contains only the body with enclosing braces
{}, and does not include the prototype, name, or type information.
Can include these in a comment."
  `(progn
     (defclcfun ,type ,name ,clc-args ,body-string)
     ,@(when headers
         `((set-explicit-headers ',name ',headers)))
     ,@(when cheaders
         `((set-explicit-cheaders ',name ',cheaders)))
     ,@(when functions
         `((set-explicit-clc-functions ',name ',functions)))))

(defmacro deffileclcfun (type name clc-args body-path
                         &key
                           headers
                           cheaders
                           functions)
  "Calls defrawclcfun using the contents of a source file located at
body-path.  body-path is evaluated, no other arguments are evaluated.
Make sure that the file contains only the body with enclosing braces
{}, and does not include the prototype, name, or type information.
Can include these in a comment, and this is recommended for good
style."
  `(let* ((lines (read-lines-from-pathname ,body-path))
          (src (apply #'string-append
                      (intersperse (format nil "~%")
                                   lines))))
     (eval `(defrawclcfun ,',type ,',name ,',clc-args
                          ,src
                          :headers ,',headers
                          :cheaders ,',cheaders
                          :functions ,',functions))))
