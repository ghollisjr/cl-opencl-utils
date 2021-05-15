(in-package :cl-opencl-utils)

(defun program-source (&rest top-level-forms)
  "Returns full program source code (minus headers) given top-level
forms"
  (let* (;; many functions require proper code
         (raw-progned-forms
          (cons 'progn top-level-forms))
         (required-functions
          (required-functions raw-progned-forms))

         ;; Prototyping required:
         (explicit-function-forms
          (remove-if-not (lambda (form)
                           (eq (first form)
                               'function))
                         top-level-forms))
         (implicit-function-forms
          (mapcar #'definition
                  ;; (lambda (fsym)
                  ;;   (destructuring-bind (&key type clc-args body)
                  ;;       (gethash fsym *clc-funs*)
                  ;;     `(function ,type ,fsym ,clc-args ,@body)))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (altdefinitions rf)))
         (function-forms (append implicit-function-forms
                                 alt-function-forms
                                 explicit-function-forms))
         (prototypes
          (mapcar #'prototype function-forms))
         (non-function-forms
          (remove-if (lambda (form)
                       (eq (first form)
                           'function))
                     top-level-forms)))
    (append prototypes
            non-function-forms
            function-forms)))

(defun program-source-from-forms-fn (&rest top-level-forms)
  "Returns string for the entire OpenCL C program consisting of the
top-level forms preceded by any required headers"
  (let* (;; many functions require proper code
         (raw-progned-forms
          (cons 'progn top-level-forms))
         (required-functions
          (required-functions raw-progned-forms))

         ;; Prototyping required:
         (explicit-function-forms
          (remove-if-not (lambda (form)
                           (eq (first form)
                               'function))
                         top-level-forms))
         (implicit-function-forms
          (mapcar (lambda (fsym)
                    (destructuring-bind (&key type clc-args body)
                        (gethash fsym *clc-funs*)
                      `(function ,type ,fsym ,clc-args ,@body)))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (altdefinitions rf)))
         (function-forms (append implicit-function-forms
                                 alt-function-forms
                                 explicit-function-forms))
         (prototypes
          (mapcar #'prototype function-forms))
         (non-function-forms
          (remove-if (lambda (form)
                       (eq (first form)
                           'function))
                     top-level-forms))
         (progned-forms
          (cons 'progn
                (append function-forms
                        non-function-forms)))
         (headers
          (required-headers progned-forms)))
    (with-output-to-string (out)
      (loop
         for hs in headers
         do (if (atom hs)
                (format out "#include<~a>~%" hs)
                (loop
                   for h in hs
                   do (format out "#include<~a>~%" hs))))
      (loop
         for expr in prototypes
         do (format out "~a;~%" expr))
      (loop
         for expr in non-function-forms
         do (format out "~a;~%" (clc expr)))
      (loop
         for expr in function-forms
         do (format out "~a~%" (clc expr))))))

(defmacro program-source-from-forms (&body top-level-forms)
  "Macro version of program-fn where top-level-forms are not
evaluated."
  `(program-source-from-forms-fn
    ,@(loop
         for expr in top-level-forms
         collecting `',expr)))

(defun program-source-from-kernels-fn (&rest kernels)
  "Returns string source code of program containing all code needed
for the supplied kernel names, including other functions and header
includes."
  (let* (;; many functions require proper code
         (required-functions
          (required-functions kernels))
         
         ;; Prototyping required:
         (implicit-function-forms
          (mapcar (lambda (fsym)
                    (destructuring-bind (&key type clc-args body)
                        (gethash fsym *clc-funs*)
                      `(function ,type ,fsym ,clc-args ,@body)))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (altdefinitions rf)))
         (function-forms (append implicit-function-forms
                                 alt-function-forms))
         (prototypes
          (mapcar #'prototype function-forms))
         (progned-forms
          (cons 'progn
                function-forms))
         (headers
          (required-headers progned-forms)))
    (with-output-to-string (out)
      (loop
         for hs in headers
         do (if (atom hs)
                (format out "#include<~a>~%" hs)
                (loop
                   for h in hs
                   do (format out "#include<~a>~%" hs))))
      (loop
         for expr in prototypes
         do (format out "~a;~%" expr))
      (loop
         for expr in function-forms
         do (format out "~a~%" (clc expr))))))

(defmacro program-source-from-kernels (&rest kernels)
  "Returns all source code needed to create program using the kernels
supplied."
  `(program-source-from-kernels-fn
    ,@(loop
         for k in kernels
         collecting `',k)))
