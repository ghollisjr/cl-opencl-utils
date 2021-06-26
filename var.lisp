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

;; Function pointer declaration
(defclc varfpointer (name return-type argument-types
                          &optional value)
  (with-output-to-string (s)
    (format s "~a (*~a)(~{~a~^,~})"
            (clc return-type)
            (clc name)
            (mapcar #'clc argument-types))
    (when value
      (format s " = (~a)" (clc value)))
    (format s ";")))

;; Returns a type which requires many type-tokens
(defclc type (&rest type-tokens)
  (format nil "~{~a~^ ~}"
          (mapcar #'clc type-tokens)))

(defclc fpointer (return-type
                  argument-types)
  (with-output-to-string (s)
    (format s "~a (*)(~{~a~^,~})"
            (clc return-type)
            (mapcar #'clc argument-types))))

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

;; Returns

;; Returns address of variable
(defclc address (var)
  (format nil "&(~a)" (clc var)))

;; Returns value pointed to by pointer
(defclc value (pointer)
  (format nil "*(~a)" (clc pointer)))

;;;; Global variables
;;;;
;;;; Global variables are known by their symbol alone, so be careful
;;;; not to clobber another variable/function/macro.

(defparameter *clc-gvars*
  (make-hash-table :test 'equal))

(defmacro defclcglobalvar (form)
  "Defines a global variable using form.  form should be one of (var
...) or (vararray ...).  The name of the global variable is taken from
the var form."
  (let* ((name (second form)))
    `(setf (gethash ',name
                    *clc-gvars*)
           ',form)))

(defun required-gvars (code)
  "Returns list of required global variables of code in dependence
ordering."
  (let* ((found (make-hash-table :test 'eq))
         (result NIL))
    (labels ((rec (code)
               (cond
                 ((NULL code) nil)
                 ((atom code)
                  (when (and (gethash code *clc-gvars*)
                             (not (gethash code found)))
                    (setf (gethash code found) T)
                    (rec (subseq (gethash code *clc-gvars*) 3))
                    (push code result)))
                 ((consp code)
                  (rec (car code))
                  (rec (cdr code))))))
      (rec code)
      (reverse result))))
