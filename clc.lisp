(in-package :cl-opencl-utils)

;;;; OpenCL Lispified C System
;;;;
;;;; This system provides the ability to use a Lisp-like syntax for
;;;; OpenCL C along with automatic header and library management for
;;;; use in creating and building OpenCL programs.

(defparameter *opsymbol->opfunction*
  (make-hash-table :test 'eq)
  "Map from operation symbol to C-generating function.")

;;; utilities
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; from let-over-lambda
(defun mkstr (&rest args)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (dolist (a args)
        (princ a s)))))

;; list utility
(defun single (list)
  "Checks to see if list is a list with exactly one element."
  (and (consp list)
       (null (rest list))))

;; Provides the equivalent of package-free symbols by interning them
;; all into cl-opencl-utils before use with the clc system.
(defun depackage (symbol)
  (if (symbolp symbol)
      (intern (string symbol) :cl-opencl-utils)
      symbol))

(defmacro defclc (opname lambda-list &body body)
  "Defines an operator which takes arguments as specified in
lambda-list and executes the body.  Body must return the resulting
string for the generated OpenCL C code.

&body will be replaced with &rest in the lambda-list since it will be
supplied to the lambda operator."
  (alexandria:with-gensyms (ops)
    `(setf (gethash (depackage ',opname)
                    *opsymbol->opfunction*)
           (lambda ,(mapcar (lambda (token)
                              (if (eq token '&body)
                                  '&rest
                                  token))
                            lambda-list)
             ,@body))))

;; Rules for clc function:
;;
;; If form is an atom, then if there is a defined cpp generation
;; function for that atom it is called.  Else, the atom is passed to
;; mkstr and the resulting string is downcased and returned.
;;
;; If form is a list, then the first element in the list is searched
;; in the map, and the function found in the map is called on the
;; remaining elements in the form.  If the first element is not found
;; in the map, then an error is thrown.

(defun clc (form)
  "Looks up either 1. Form if form is an atom, or 2. First element of
form if form is a list in the operator table *opsymbol->opfunction*
and calls the function mapped there with no arguments for atoms or the
remaining arguments in the form for lists.  If there is no defined
operator corresponding to the first element of the list, standard
function call notation is assumed and the symbol is downcased as a
string for the function name."
  (let ((*print-pretty* nil))
    (handler-case
        (when form
          (let ((sym->fn *opsymbol->opfunction*))
            (if (atom form)
                (progn
                  (aif (gethash (depackage form) sym->fn)
                       (funcall it)
                       (if (stringp form)
                           form
                           (string-downcase
                            (lisp->clc form)))))
                (let ((first
                       (first form)))
                  (aif (gethash (depackage first) sym->fn)
                       (apply it
                              (rest form))
                       (format nil "~a(~{~a~^,~})"
                               (string-downcase (string first))
                               (mapcar #'clc (rest form))))))))
      (error (err) (error "~a" err))
      )))

(defgeneric lisp->clc (lisp-object &rest args)
  (:documentation "Generates an OpenCL string for the Lisp object.
  args can be used along with side effects to ensure that the OpenCL C
  string is sensible.")
  (:method ((x float) &rest args)
    (let* ((rawstr (mkstr x))
           (rawlen (length rawstr))
           (str
            ;; Remove unnecessary exponents
            (if (string= (subseq rawstr (- rawlen 2))
                         "d0")
                (subseq rawstr 0 (- rawlen 2))
                rawstr)))
      (map 'string
           (lambda (char)
             (if (or (char= char #\d)
                     (char= char #\f))
                 #\e
                 char))
           str)))
  (:method ((x integer) &rest args)
    (format nil "~d" x))
  (:method ((x string) &rest args)
    (with-output-to-string (out)
      (format out "\"")
      (loop
         for char across x
         do (if (char= char #\Newline)
                (format out "\\n")
                (format out "~a" char)))
      (format out "\"")))
  (:method (x &rest args)
    (format nil "~a" x)))
