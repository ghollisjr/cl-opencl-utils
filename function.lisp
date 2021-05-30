(in-package :cl-opencl-utils)

(defparameter *clc-funs*
  (make-hash-table :test 'equal)
  "Map from OpenCL C function symbol or string to function code
  description")

(defparameter *clc-alt-funs*
  (make-hash-table :test 'equal)
  "Map from OpenCL C function symbol to alternative definitions")

(defparameter *explicit-funs*
  (make-hash-table :test 'equal)
  "Map from OpenCL C function symbol to explicit function symbol
dependencies.")

(defun list->set (list &optional (test #'eql))
  "Returns list with duplicates removed"
  (reduce (lambda (res el)
            (adjoin el res :test test))
          list
          :initial-value nil))

(defun set-explicit-clc-functions (symbol functions &optional (op :set))
  "Defines explicit function dependencies.  op can be :set, :add,
or :reset."
  (symbol-macrolet ((fns (gethash symbol *explicit-funs*)))
    (case op
      (:set (setf fns functions))
      (:add (setf fns (append functions fns)))
      (:reset (setf fns nil)))
    symbol))

(defun explicit-clc-functions (symbol)
  "Returns explicit OpenCL C function dependencies of that symbol."
  (values (gethash symbol *explicit-funs*)))

(defmacro defclcfun (fname return-type clc-args &body body)
  "Defines a OpenCL C function which is loaded automatically into a
program's code."
  `(setf (gethash ',fname
                  *clc-funs*)
         (list :type ',return-type
               :clc-args ',clc-args
               :body ',body)))

(defmacro defclckernel (fname clc-args &body body)
  "Defines a OpenCL C kernel which is loaded automatically into a
program's code."
  `(setf (gethash ',fname
                  *clc-funs*)
         (list :type `(type __kernel :void)
               :clc-args ',clc-args
               :body ',body)))

(defmacro undefclcfun (fname)
  `(remhash ',fname *clc-funs*))

(defmacro undefclckernel (fname)
  `(remhash ',fname *clc-funs*))

(defmacro defclcaltfun (fname return-type clc-args &body body)
  (alexandria:with-gensyms (fnam typ cas bod altfuns)
    `(let ((,typ ',return-type)
           (,fnam ',fname)
           (,cas ',clc-args)
           (,bod ',body))
       (symbol-macrolet ((,altfuns (gethash ,fnam
                                            *clc-alt-funs*)))
         (setf ,altfuns
               (adjoin 
                (list :type ,typ
                      :clc-args ,cas
                      :body ,bod)
                ,altfuns
                :test #'equal))))))

(defun undefclcaltfun (fname)
  (setf (gethash fname *clc-alt-funs*) nil))

(defun required-functions (code)
  "Returns list of required functions for code"
  (let ((code (expand-clc-macros code))
        (traversed (make-hash-table :test 'eq)))
    (labels ((rec (code)
               ;; recurses through code
               (list->set
                (cond
                  ((null code) nil)
                  ((listp code)
                   (cond
                     ((eq (first code)
                          'eval)
                      nil)
                     ((eq (first code)
                          :struct)
                      nil)
                     ((gethash (first code) traversed)
                      nil)
                     (t
                      (setf (gethash (first code) traversed)
                            t)
                      (apply #'append
                             (required-functions (first code))
                             (mapcar #'required-functions (rest code)))))
                   ;; (apply #'append
                   ;;        (required-functions (first code))
                   ;;        (mapcar #'required-functions (rest code)))
                   )
                  ((atom code)
                   (cond
                     ((gethash code traversed)
                      nil)
                     ((gethash code *clc-funs*)
                      (append
                       ;; Automatic:
                       (list* code
                              (rec `(progn ,@(getf (gethash code *clc-funs*)
                                                   :body))))
                       ;; Explicit function dependencies:
                       (explicit-clc-functions code)))
                     (t nil))))
                #'eq)))
      (rec code))))

(defun definition (fsym)
  "Returns OpenCL C code for the definition of a function."
  (destructuring-bind (&key type clc-args body)
      (gethash fsym *clc-funs*)
    `(function ,fsym ,type ,clc-args ,@body)))

(defun prototype (function-code)
  "Returns OpenCL C code for the prototype of a function definition"
  (with-output-to-string (out)
    (destructuring-bind (fname type clc-args &rest body)
        (rest function-code)
      (format out "~a ~a(~{~a~^,~})"
              (clc type)
              (clc fname)
              (mapcar #'clc clc-args)))))

(defun altdefinitions (fsym)
  (loop
     for def in (gethash fsym *clc-alt-funs*)
     collecting
       (destructuring-bind (&key type clc-args body)
           def
         `(function ,fsym ,type ,clc-args ,@body))))

(defun altprototypes (altdefs)
  (loop
     for function-code in altdefs
     collecting
       (with-output-to-string (out)
         (destructuring-bind (type fname clc-args &rest body)
             (rest function-code)
           (format out "~a ~a(~{~a~^,~})"
                   (clc type)
                   (clc fname)
                   (mapcar #'clc clc-args))))))
