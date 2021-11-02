(in-package :cl-opencl-utils)

;;; utility functions

;; Modified version of c-type-name from CFFI's groveler.  As per their
;; request, here's their message:
#|
Copyright (C) 2005-2007, James Bielman  <jamesjb@jamesjb.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
|#
;; I've modified their code, and I don't offer you any warranties
;; either.
(defun c-type-name (type-keyword)
  (case type-keyword
    ((:uchar :unsigned-char) "unsigned char")
    ((:unsigned-short :ushort) "unsigned short")
    ((:unsigned-int :uint) "unsigned int")
    ((:unsigned-long :ulong) "unsigned long")
    ((:long-long :llong) "long long")
    ((:unsigned-long-long :ullong) "unsigned long long")
    (:pointer "void*")
    (:string "char*")
    (t (cffi::foreign-name type-keyword nil))))

;;; Expression generator
(defun opencl-function-expr (fname)
  "Returns an expression function generating the Lispified OpenCL code
to call that function when supplied with a list of arguments.  Useful
for multiple utilities, e.g. make-opencl-reducer, make-opencl-mapper,
make-opencl-convolutor."
  (lambda (&rest xs)
    `(,fname ,@xs)))

;;; OpenCL handler system:
;;;
;;; I've run into the same situation over and over again: I need to
;;; write a Lisp function that manages a bunch of OpenCL resources
;;; (buffers, kernels, some kind of make-opencl-* (function cleanup)
;;; system) and will eventually need to cleanup after the the
;;; resources are no longer needed.  As usual, this is accomplished by
;;; pairing the function with a cleanup function that shares access to
;;; the same resources.  Creating this pair is automatically handled
;;; by the with-opencl-cleanup macro.
;;;
;;; Another common feature is the need for sticky arguments to the
;;; handler.  Data can be retained in the managed buffers, so new data
;;; only needs to be sent to the buffers when different values are
;;; required.  To support this in a uniform but general way, buffers
;;; that are automatically synchronized with input parameters should
;;; are automated with the make-opencl-handler system.

(defun ensure-sequence (x)
  (if (typep x 'sequence)
      x
      (list x)))

(defmacro make-opencl-handler (queue args buffers event-binding
                               (&body init-body) (&body cleanup-body)
                               &body body)
  "Macro that ultimately returns a list (handler cleanup) where
handler and cleanup have been generated according to the input
specifications.

* args are non-buffer arguments to the handler function.

* buffers are buffer arguments to the handler function.  Each entry in
  buffers should have the form (namesymbol type &key count flags init)
  where the buffers are initialized with init if supplied and then set
  to new values whenever new values are supplied.  Note that
  namesymbol is a symbol for the parameter argument, not the buffer
  itself.  The corresponding buffer is only updated when a non-NIL
  value for the parameter is supplied.

* event-binding should be a symbol for the event list that will be
  used by the handler.  It will also be used by the parameter update
  code executed before the supplied code body is executed, so if
  out-of-order execution is a possibility then the event list will
  need to be treated properly by the code body.

* init-body will be called before returning the (handler cleanup)
  result list.  This allows e.g. setting up kernel arguments or
  further initialization prior to return.

* cleanup-body will be placed at the top of the cleanup function body
  to e.g. cleanup after programs and kernels created for the handler.

Parameters can be non-sequence atoms or sequences.  Non-sequence atoms
will be converted into a sequence prior to writing to a buffer.  No
nested sequences are supported, as there is no way to know how to
unflatten the data in general, and so there is no good reason to send
unflattened data as an OpenCL parameter.

There is a macro (buffer (param)...) defined in the handler body that
allows you to acquire the parameter buffer given the parameter
argument symbol as the argument to the macro.  This is useful for
e.g. copying buffers."
  (let* ((handler (gensym))
         (cleanup (gensym))
         (bufmap (gensym))
         (qsym (gensym))
         (csym (gensym))
         (keyp (member '&key args))
         (otherkeyp (member '&allow-other-keys args))
         (parameter-arguments (mapcar #'first buffers))
         (parameter-types (mapcar #'second buffers))
         (parameter-buffer-gsyms
          (loop
             for pa in parameter-arguments
             collecting (gensym)))
         (parameter-bindings
          (loop
             for buffer in buffers
             for parg in parameter-arguments
             for buf in parameter-buffer-gsyms
             collecting
               (destructuring-bind (namesymbol type &key count flags init)
                   buffer
                 `(,buf
                   (cl-create-buffer ,csym
                                     :type ,type
                                     ,@(when flags
                                         `(:flags ,flags))
                                     ,@(when count
                                         `(:count ,count))
                                     ,@(when init
                                         `(:data (init->data ,init))))))))
         (parameter-inits
          (loop
             for buffer in buffers
             for parg in parameter-arguments
             for buf in parameter-buffer-gsyms
             collecting
               (destructuring-bind (namesymbol type &key count flags init)
                   buffer
                 `(when ,parg
                    (push (cl-enqueue-write-buffer
                           ,qsym ,buf ,type (ensure-sequence ,parg))
                          ,event-binding)))))
         (lambda-list (if keyp
                          (append (remove '&allow-other-keys args)
                                  parameter-arguments)
                          (append args
                                  (list '&key)
                                  parameter-arguments))))
    `(let* ((,qsym ,queue)
            (,bufmap (make-hash-table :test 'eq))
            (,csym (cl-get-command-queue-info
                    ,qsym
                    +CL-QUEUE-CONTEXT+))
            (,event-binding nil)
            ,@parameter-bindings
            (,handler
             (lambda ,lambda-list
               (macrolet ((buffer (param)
                            (list 'gethash param ',bufmap)))
                 ,@parameter-inits
                 ,@body)))
            (,cleanup
             (lambda ()
               ,@cleanup-body
               ,@(loop
                    for pb in parameter-buffer-gsyms
                    collecting `(cl-release-mem-object ,@pb)))))
       ,@(loop
            for pa in parameter-arguments
            for bufsym in parameter-buffer-gsyms
            collecting `(setf (gethash ',pa ,bufmap)
                              ,bufsym))
       ,@init-body
       (list ,handler ,cleanup))))
