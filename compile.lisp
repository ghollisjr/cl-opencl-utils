(in-package :cl-opencl-utils)

(defun program-source (&rest top-level-forms)
  "Returns full program source code (minus headers) given top-level
forms"
  (let* ((top-level-forms
          (expand-clc-macros top-level-forms))
         ;; many functions require proper code
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
                  ;;     `(function ,fsym ,type ,clc-args ,@body)))
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
         ;; structs depend on non-functions and prototypes
         (structs
          (required-structs
           (append prototypes
                   non-function-forms
                   function-forms)))
         (struct-definitions
          (mapcar #'struct-definition
                  structs))
         (globals
          (required-gvars
           (append prototypes
                   non-function-forms
                   function-forms)))
         (global-definitions
          (loop
             for g in globals
             collecting (gethash g *clc-gvars*))))
    (append struct-definitions
            global-definitions
            prototypes
            non-function-forms
            function-forms)))

(defun program-source-from-forms-fn (&rest top-level-forms)
  "Returns string for the entire OpenCL C program consisting of the
top-level forms preceded by any required headers"
  (let* ((top-level-forms
          (expand-clc-macros top-level-forms))
         ;; many functions require proper code
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
                      (expand-clc-macros
                       `(function ,fsym ,type ,clc-args ,@body))))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (expand-clc-macros
                (altdefinitions rf))))
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
         (structs
          (required-structs (append prototypes
                                    function-forms
                                    non-function-forms)))
         (struct-definitions
          (mapcar #'struct-definition structs))
         (globals
          (required-gvars (append prototypes
                                  function-forms
                                  non-function-forms)))
         (global-definitions
          (loop
             for g in globals
             collecting (gethash g *clc-gvars*)))
         (progned-forms
          (cons 'progn
                (append struct-definitions
                        global-definitions
                        function-forms
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
         for expr in struct-definitions
         do (format out "~a;~%" (clc expr)))
      (loop
         for expr in global-definitions
         do (format out "~a;~%" (clc expr)))
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
  (let* ((top-level-forms
          (expand-clc-macros top-level-forms)))
    `(program-source-from-forms-fn
      ,@(loop
           for expr in top-level-forms
           collecting `',expr))))

(defun program-source-from-functions-fn (&rest functions)
  "Returns string source code of program containing all code needed
for the supplied function names, including other functions and header
includes."
  (let* (;; many functions require proper code
         (required-functions
          (required-functions functions))
         
         ;; Prototyping required:
         (implicit-function-forms
          (mapcar (lambda (fsym)
                    (destructuring-bind (&key type clc-args body)
                        (gethash fsym *clc-funs*)
                      (expand-clc-macros
                       `(function ,fsym ,type ,clc-args ,@body))))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (expand-clc-macros
                (altdefinitions rf))))
         (function-forms (append implicit-function-forms
                                 alt-function-forms))
         (structs (required-structs function-forms))
         (struct-definitions (mapcar #'struct-definition structs))
         (globals (required-gvars function-forms))
         (global-definitions
          (loop
             for g in globals
             collecting (gethash g *clc-gvars*)))
         (prototypes
          (mapcar #'prototype function-forms))
         (progned-forms
          (cons 'progn
                (append struct-definitions
                        global-definitions
                        function-forms)))
         
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
         for expr in struct-definitions
         do (format out "~a;~%" (clc expr)))
      (loop
         for expr in global-definitions
         do (format out "~a;~%" (clc expr)))
      (loop
         for expr in prototypes
         do (format out "~a;~%" expr))
      (loop
         for expr in function-forms
         do (format out "~a~%" (clc expr))))))

(defmacro program-source-from-functions (&rest functions)
  "Returns all source code needed to create program using the functions
supplied."
  `(program-source-from-functions-fn
    ,@(loop
         for f in functions
         collecting `',f)))

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
                      (expand-clc-macros
                       `(function ,fsym ,type ,clc-args ,@body))))
                  required-functions))
         (alt-function-forms
          (loop
             for rf in required-functions
             appending
               (expand-clc-macros
                (altdefinitions rf))))
         (function-forms (append implicit-function-forms
                                 alt-function-forms))
         (structs (required-structs function-forms))
         (struct-definitions (mapcar #'struct-definition structs))
         (globals (required-gvars function-forms))
         (global-definitions
          (loop
             for g in globals
             collecting (gethash g *clc-gvars*)))
         (prototypes
          (mapcar #'prototype function-forms))
         (progned-forms
          (cons 'progn
                (append struct-definitions
                        global-definitions
                        function-forms)))
         
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
         for expr in struct-definitions
         do (format out "~a;~%" (clc expr)))
      (loop
         for expr in global-definitions
         do (format out "~a;~%" (clc expr)))
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
