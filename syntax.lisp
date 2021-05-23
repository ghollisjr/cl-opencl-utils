(in-package :cl-opencl-utils)

;;;; Call any function:
(defclc call (fname &rest args)
  (format nil "~a(~{~a~^,~})"
          (clc fname)
          (mapcar #'clc args)))

;;;; Define a structure
(defclc struct (name &rest body)
  (format nil "struct ~a {~%~{~a;~%};~};"
          (clc name)
          (mapcar #'clc body)))

;;;; sizeof operator
(defclc sizeof (type)
  (format nil "sizeof(~a)"
          (clc type)))

;;;; String constants

;; Need this due to Lisp strings needing literal double-quotes inside
;; the string in order to be properly output to the C++ file.
(defclc str (lisp-string)
  (format nil "\"~a\""
          (clc lisp-string)))

;;;; Function definition

(defclc function (fname type arg-list &body body)
  (with-output-to-string (out)
    (format out "~a ~a(~{~a~^,~}) {~%"
            (clc type)
            (clc fname)
            (mapcar #'clc arg-list))
    (loop
       for expr in body
       do (format out "~a;~%" (clc expr)))
    (format out "}~%")))

;;; Kernels
;;;
;;; Note that OpenCL C kernels always have void return type, so
;;; there's no need to supply return type.
(defclc kernel (fname arg-list &body body)
  (let ((rettype :void))
    (with-output-to-string (out)
      (format out "__kernel ~a ~a(~{~a~^,~}) {~%"
              (clc rettype)
              (clc fname)
              (mapcar #'clc arg-list))
      (loop
         for expr in body
         do (format out "~a;~%" (clc expr)))
      (format out "}~%"))))

;;;; Struct members

;; access member of struct
(defclc member (object member)
  (with-output-to-string (out)
    (format out "(~a).~a"
            (clc object)
            (clc member))))

;; access member on object pointed to
(defclc pmember (pointer member)
  (with-output-to-string (out)
    (format out "(~a)->~a"
            (clc pointer)
            (clc member))))

;; Assignment (didn't know exactly where to place this)
(defclc setf (place value)
  (with-output-to-string (out)
    (format out "~a = ~a"
            (clc place)
            (clc value))))

;; type casting
(defclc typecast (val type)
  (with-output-to-string (out)
    (format out "((~a) ~a)"
            (clc type)
            (clc val))))

;; Conveniencen alias
(defclc coerce (val type)
  (clc `(typecast ,val ,type)))

;; Array referencing (operator[])
(defclc aref (array &rest indices)
  (with-output-to-string (out)
    (format out "(~a)~{[~a]~}"
            (clc array)
            (mapcar #'clc indices))))

;; Function returns:
(defclc return (&optional val)
  (with-output-to-string (out)
    (format out "return")
    (when val
      (format out " (~a)" (clc val)))))

;; Break statement:
(defclc break ()
  "break")

;; Continue statement:
(defclc continue ()
  "continue")

;; NULL
(defclc NULL () "NULL")

;; Evaluating a Lisp form:
(defclc eval (form)
  (clc (eval form)))

;; Concatenating resulting code strings:
(defclc concat (&rest forms)
  (apply #'concatenate 'string
         (mapcar #'clc forms)))

;; Create a statement from some other form (statements end with
;; semicolons).
(defclc statement (form)
  (format nil "~a;"
          (clc form)))

;; Concatenating a list of statements generated from forms.
(defclc statements (&rest forms)
  (clc `(concat ,@(map 'list
                       (lambda (f)
                         `(statement ,f))
                       forms))))

;; OpenCL built-ins
(defclc get-work-dim (&rest indices)
  (clc `(get_work_dim ,@indices)))
(defclc get-global-size (&rest indices)
  (clc `(get_global_size ,@indices)))
(defclc get-global-id (&rest indices)
  (clc `(get_global_id ,@indices)))
(defclc get-local-size (&rest indices)
  (clc `(get_local_size ,@indices)))
(defclc get-local-id (&rest indices)
  (clc `(get_local_id ,@indices)))
(defclc get-num-groups (&rest indices)
  (clc `(get_num_groups ,@indices)))
(defclc get-group-id(&rest indices)
  (clc `(get_group_id ,@indices)))

(defclc +CLK-LOCAL-MEM-FENCE+ ()
  "CLK_LOCAL_MEM_FENCE")

(defclc +CLK-GLOBAL-MEM-FENCE+ ()
  "CLK_GLOBAL_MEM_FENCE")
