(in-package :cl-opencl-utils)

;; Simple declaration and copy construction
(defclc var (name type &optional value)
  (with-output-to-string (out)
    (format out "~a ~a" (clc type) (clc name))
    (when value
      (format out " = ~a" (clc value)))))

;; Array declaration
(defclc vararray (name type dimensions &rest inits)
  (with-output-to-string (s)
    (format s "~a ~a~{[~a]~}" (clc type) (clc name)
            (mapcar #'clc dimensions))
    (when inits
      (format s " = {~{~a~^,~}}" (mapcar #'clc inits)))
    (format s ";")))

;; Returns a type which requires many type-tokens
(defclc type (&rest type-tokens)
  (format nil "~{~a~^ ~}"
          (mapcar #'clc type-tokens)))

;; OpenCL __global, __constant, __local, and __private
(defclc global (type)
  (format nil "__global ~a" (clc type)))
(defclc constant (type)
  (format nil "__constant ~a" (clc type)))
(defclc local (type)
  (format nil "__local ~a" (clc type)))
(defclc private (type)
  (format nil "__private ~a" (clc type)))

;; Returns the const type for a given type
(defclc const (type)
  (format nil "~a const"
          (clc type)))

;; Returns the pointer type for a given type
(defclc pointer (type)
  (format nil "~a*" (clc type)))

;; Returns address of variable
(defclc address (var)
  (format nil "&(~a)" (clc var)))

;; Returns value pointed to by pointer
(defclc value (pointer)
  (format nil "*(~a)" (clc pointer)))
