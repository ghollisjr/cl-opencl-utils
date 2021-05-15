(in-package :cl-opencl-utils)

;; utilities
(defun compress (list &key
                        (key #'identity)
                        (test 'eql)
                        sort-by
                        singleton-pairs)
  "Compress list such that duplicate elements are combined into a
single pair entry which has as the car the element value and as the
cdr the count.

The singleton-pairs option controls whether single elements are still
placed inside a pair.  This is useful when the elements of the list
you wish to compress are cons cells, as this would otherwise lead to
ambiguity in the result."
  (let* ((result-table (make-hash-table :test test)))
    (dolist (x list)
      (if (gethash x result-table)
          (incf (gethash x result-table))
          (setf (gethash x result-table) 1)))
    (let* ((map-fn (if singleton-pairs
                       (lambda (x c)
                         (cons x c))
                       (lambda (x c)
                         (if (= 1 c)
                             x
                             (cons x c)))))
           (raw-alist
            (loop
               for k being the hash-keys of result-table
               for v being the hash-values of result-table
               collect (funcall map-fn k v))))
      (if sort-by
          (sort raw-alist sort-by
                :key (if singleton-pairs
                         (lambda (x)
                           (funcall key (car x)))
                         (lambda (x)
                           (if (consp x)
                               (funcall key (car x))
                               (funcall key x)))))
          raw-alist))))

(defparameter *explicit-headers*
  (make-hash-table :test 'equal)
  "Map from symbol to explicit list of headers required by that
symbol.")

(defun set-explicit-headers (symbol headers &optional (op :set))
  "Defines explicit headers for symbol.  op can be :set, :add,
or :reset to either set the header list, add new headers to the list,
or clear all headers for that symbol.

Do not use keyword symbols.  Symbols are treated as package-less."
  (symbol-macrolet ((hs (gethash symbol
                                 *explicit-headers*)))
    (case op
      (:set
       (setf hs
             headers))
      (:add
       (setf hs
             (append headers hs)))
      (:reset
       (setf hs nil)))
    symbol))

(defun explicit-headers (symbol)
  "Returns list of explicit headers for that symbol"
  (when (symbolp symbol)
    (values (gethash symbol
                     *explicit-headers*))))

(defparameter *headers*
  (make-hash-table :test 'equal))

(defun defheader-fn (fnames provided
                     &key
                       flags)
  "Defines a OpenCL C header or set of headers by its filename(s) fnames,
the OpenCL C objects it provides, and the flags necessary for compiling and
linking.

flags must be a string of compile & link flags for your OpenCL C compiler."
  (setf (gethash fnames *headers*)
        (list :provided provided
              :flags (if flags
                         flags
                         ""))))

(defmacro defheader (fnames provided
                     &key
                       flags)
  "Macro version of defheader-fn"
  `(defheader-fn ',fnames ',provided
     ,@(when flags `(:flags ,flags))))

(defmacro with-defheader (fnames defclcs
                          &key
                            additional
                            flags)
  "Defines a C header as well as executing the defclc forms"
  (let ((provided
         (append additional
                 (mapcar #'second defclcs))))
    `(progn
       (defheader ,fnames ,provided
         ,@(when flags
             (list :flags flags)))
       ,@defclcs)))

(defun generate-clc->header ()
  "Returns map from clc to header for that clc symbol"
  (let ((result (make-hash-table :test 'eq)))
    (loop
       for header being the hash-keys in *headers*
       for val being the hash-values in *headers*
       do (destructuring-bind (&key provided flags) val
            (loop
               for p in provided
               do (setf (gethash p result)
                        header))))
    result))

(defun required-headers (form)
  "Returns the list of required headers for a form"
  (let ((clc->header (generate-clc->header)))
    (labels ((rec (f)
               (let ((result nil))
                 (cond
                   ((null f)
                    nil)
                   ((listp f)
                    (let ((sub-results (mapcar #'rec f)))
                      (loop
                         for i in sub-results
                         when i
                         do (setf result
                                  (mapcar #'car
                                          (compress
                                           (append result
                                                   i)
                                           :test #'equal
                                           :singleton-pairs t))))))
                   ((atom f)
                    (progn
                      ;; Automatic dependencies:
                      (setf result
                            (append
                             (when (gethash f clc->header)
                               (list (gethash f clc->header)))
                             ;; Explicit dependencies:
                             (explicit-headers f))))))
                 result)))
      (rec form))))

(defparameter *clc-compile-flags*
  ""
  "Default compilation flags for all OpenCL C programs")

(defun compile-flags (clc)
  "Returns all compilation flags required by code form clc"
  (let ((headers (required-headers clc)))
    (format
     nil "~a ~{~a~^ ~}"
     *clc-compile-flags*
     (remove ""
             (mapcar (lambda (h)
                       (destructuring-bind (&key flags provided)
                           (gethash h *headers*)
                         flags))
                     headers)
             :test #'equal))))
