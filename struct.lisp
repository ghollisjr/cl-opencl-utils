(in-package :cl-opencl-utils)

;;;; Provides OpenCL structs.  Some notes:
;;;;
;;;; Always refer to structs via (:struct structname) when referring
;;;; to types in Lispified OpenCL C code.  Not doing so is deprecated
;;;; and leads to bugs in CFFI code, and is mandatory for this library
;;;; as only (:struct ...) forms signal required structs in Lispified
;;;; OpenCL C code.
;;;;
;;;; The syntax of the slots is slightly different from CFFI:
;;;;
;;;; (name type &rest inits)
;;;;
;;;; type can be a simple type, a struct like (:struct &rest slots),
;;;; or an array like (:array element-type &rest dimensions).

(defparameter *clc-structs*
  (make-hash-table :test 'equal))

(defmacro defclcstruct (sname &body slots)
  "Defines both a CFFI struct and an analogous struct available to
OpenCL C."
  `(progn
     (defcstruct ,sname ,@slots)
     (setf (gethash ',sname *clc-structs*)
           ',(loop
                for s in slots
                collecting s))))

(defmacro defclcstruct-no-cffi (sname &body slots)
  "Defines struct available to OpenCL C, but no CFFI type."
  `(progn
     ;; (defcstruct ,sname ,@slots)
     (setf (gethash ',sname *clc-structs*)
           ',(loop
                for s in slots
                collecting s))))

(defmacro undefclcstruct (sname)
  `(remhash ',sname *clc-structs*))

(defun required-structs-worker (code)
  "Returns list of required structs for code in reverse order of dependence."
  (let ((code (expand-clc-macros code))
        (traversed (make-hash-table :test 'eq)))
    (labels ((rec (code)
               (list->set
                (cond
                  ((null code) nil)
                  ((listp code)
                   (cond
                     ((eq (first code)
                          'eval)
                      nil)
                     ((eq (first code) :struct)
                      (let* ((s (second code)))
                        (when (not (gethash s traversed))
                          (setf (gethash s traversed)
                                t)
                          (required-structs-worker s))))
                     (t
                      (apply #'append
                             (mapcar #'rec code)))))
                  ((atom code)
                   (cond
                     ((gethash code traversed)
                      nil)
                     ((gethash code *clc-structs*)
                      (append
                       (list* code
                              (rec (gethash code *clc-structs*)))))
                     (t nil))))
                #'eq)))
      (rec code))))

(defun required-structs (code)
  "Returns list of required structs for code in order of dependence."
  (reverse (required-structs-worker code)))

(defun struct-definition (structsym)
  "Returns OpenCL C code for the definition of a struct."
  (let* ((slotdefs (gethash structsym *clc-structs*)))
    `(defstruct ,structsym
       ,@(loop
            for (name type &rest inits) in slotdefs
            collecting
              (cond
                ((atom type)
                 `(var ,name ,type ,@inits))
                ((listp type)
                 (cond
                   ;; handle arrays
                   ((eq (first type)
                        :array)
                    (destructuring-bind (element-type &rest dimensions)
                        (rest type)
                      `(vararray ,element-type ,dimensions ,@inits)))
                   (t
                    `(var ,name ,type ,@inits)))))))))
