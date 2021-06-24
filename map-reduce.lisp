(in-package :cl-opencl-utils)

;;; Reductions
(defparameter +OPENCL-ADD-REXPR+
  (opencl-function-expr '+))

(defparameter +OPENCL-COMPLEX-ADD-REXPR+
  (opencl-function-expr 'complex+))

(defun make-opencl-reducer
    (queue type rexpr
     &key
       (preamble "")
       headers
       options)
  "Returns a list (reducer cleanup) where reducer is of the form

(lambda (buffer &key start end event-wait-list) ...)

that will perform a reduction operation according to the input
parameters.  buffer must an OpenCL memory object, and start and end
are optional indices into the buffer.  cleanup should be called once
the reducer is done being used so that OpenCL memory can be cleaned
up.

queue must be an OpenCL queue handle.

type should be a CFFI type designator.

rexpr should be a Lisp function accepting two strings and returning
either Lispified OpenCL C code or a string for an OpenCL C expression
of a binary operation.  The expression can refer to code defined in
the preamble string argument or included in one of the headers.

memobj is the source memory buffer.

preamble can be a string of OpenCL C code to be placed below headers
but above the reduction kernel.  Useful for defining utility functions
to call in the expression.

headers can be a list of device header file names to include in the
kernel OpenCL C code.

options can be compiler options for the OpenCL kernel.

start can be an initial index offset to start reading from the buffer.

end can be a final index offset to stop reading from the buffer.  Uses
subseq index convention, i.e. the element at index=end is not
included.

kernel and program will need to be released at some point once you're
done using the reducer."
  (let* ((kernel-source
          (concatenate
           'string
           (format nil "~{#include \"~a\"~^~%~}"
                   headers)
           (program-source-from-forms-fn
            `(concat
              ,preamble
              (kernel reduce
                      ((var input (global (pointer ,type)))
                       (var startend (global (pointer :ulong)))
                       (var output (global (pointer ,type)))
                       (var acc (local (pointer ,type))))
                      (var start (const :ulong)
                           (aref startend 0))
                      (var end (const :ulong)
                           (aref startend 1))
                      (var n (const :ulong)
                           (- end start))
                      (var gid (const :int)
                           (get-global-id 0))
                      (var wid (const :int)
                           (get-local-id 0))
                      (var localsize (const :int)
                           (get-local-size 0))
                      (var groupid (const :int)
                           (get-group-id 0))
                      (var lastgroupid (const :int)
                           (1- (ceil (/ (coerce n :float)
                                        localsize))))
                      (var nwork (const :int)
                           (? (= groupid lastgroupid)
                              (? (= n localsize)
                                 localsize
                                 (mod n localsize))
                              localsize))
                      (when (< (+ start gid)
                               end)
                        (setf (aref acc wid)
                              (aref input
                                    (+ start gid)))
                        (barrier +CLK-LOCAL-MEM-FENCE+)
                        (var niter :ulong
                             ;; I have no idea why this doesn't work
                             ;; on POCL 1.7, but it doesn't.  It works
                             ;; on the Linux NVIDIA 465.31 OpenCL
                             ;; implementation.
                             ;;
                             ;; (ceil (log2 (coerce nwork :float)))
                             ;;
                             ;; However, this works.  I can contact
                             ;; POCL upstream about this issue.  My
                             ;; guess is that loop limits need to be
                             ;; some quasi-constant values, and that
                             ;; the values returned by OpenCL job ID
                             ;; functions like get-local-id are
                             ;; treated as quasi-constant values.
                             (ceil (log2 (coerce localsize :float))))
                        (for (var i :ulong 0) (< i niter) (incf i)
                             (var stride (const :int)
                                  (<< 1
                                      (+ i 1)))
                             (when (zerop (mod wid stride))
                               (var next (const :ulong)
                                    (+ wid (<< 1 i)))
                               (when (< next nwork)
                                 (setf (aref acc wid)
                                       ,(funcall rexpr
                                                 `(aref acc wid)
                                                 `(aref acc next)))))
                             (barrier +CLK-LOCAL-MEM-FENCE+))
                        (when (zerop wid)
                          (setf (aref output
                                      (get-group-id 0))
                                (aref acc 0)))))))))
         (context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program-with-log program (list dev)
                               :options options)
    (let* ((kernel
            (cl-create-kernel program "reduce"))
           (reducefn
            (lambda (buffer &key event-wait-list start end)
              (let* ((start (if start
                                start
                                0))
                     (bufsize (cl-get-mem-object-info
                               buffer
                               +CL-MEM-SIZE+))
                     (end (if end
                              end
                              (/ bufsize
                                 (foreign-type-size type))))
                     (events NIL) ; accumulated events and cleanup functions
                     (n (- end start))
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups)
                       ;; (min n
                       ;;      (* wgsize ngroups))
                       )
                     (startendbuf
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :data (list start end)))
                     (outbuf1
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-WRITE+
                                        :type type
                                        :count ngroups))
                     (outbuf2
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-WRITE+
                                        :type type
                                        :count ngroups)))
                ;; initial execution
                (cl-set-kernel-arg kernel 0 :value buffer)
                (cl-set-kernel-arg kernel 1 :value startendbuf)
                (cl-set-kernel-arg kernel 2 :value outbuf1)
                ;; local memory buffer
                (cl-set-kernel-arg kernel 3
                                   :type type
                                   :count wgsize)
                (push (cl-enqueue-kernel queue kernel globalworksize
                                         :event-wait-list event-wait-list)
                      events)
                ;; loop
                (let* ((niter (ceiling (log ngroups
                                            wgsize)))
                       (buf1 NIL)
                       (buf2 outbuf1)
                       (n NIL)
                       (localworksize NIL))
                  (when (not (zerop niter))
                    (loop
                       for i below niter
                       do
                         (setf n ngroups)
                         (setf ngroups (ceiling n wgsize))
                         (setf localworksize (min wgsize n))
                         (setf globalworksize (* ngroups
                                                 localworksize))
                         (if (evenp i)
                             (progn
                               (setf buf1 outbuf1)
                               (setf buf2 outbuf2))
                             (progn
                               (setf buf1 outbuf2)
                               (setf buf2 outbuf1)))
                         (push
                          (cl-enqueue-write-buffer queue
                                                   startendbuf
                                                   :ulong
                                                   (list 0 n)
                                                   :event-wait-list
                                                   (append events event-wait-list))
                          events)
                         (cl-set-kernel-arg kernel 0 :value buf1)
                       ;; might not actually need this, will test
                         (cl-set-kernel-arg kernel 1 :value startendbuf)
                         (cl-set-kernel-arg kernel 2 :value buf2)
                         (push
                          (cl-enqueue-kernel queue
                                             kernel
                                             globalworksize
                                             :event-wait-list
                                             (append events event-wait-list))
                          events)))
                  (destructuring-bind (finalevent returner)
                      (cl-enqueue-read-buffer
                       queue
                       buf2
                       type 1
                       :event-wait-list (append events event-wait-list))
                    (setf events (nreverse events))
                    (let* ((cleanup
                            (lambda ()
                              (loop
                                 for ev in events
                                 do
                                   (release-opencl-event ev)
                                 ;; (if (listp ev)
                                 ;;     (progn
                                 ;;       (funcall (second ev))
                                 ;;       (cl-release-event (first ev)))
                                 ;;     (cl-release-event ev))
                                   )
                              (let* ((result (elt (funcall returner) 0)))
                                (cl-release-mem-object outbuf2)
                                (cl-release-mem-object outbuf1)
                                (cl-release-mem-object startendbuf)
                                result))))
                      (list finalevent cleanup)))))))
           (cleanup
            (lambda ()
              (cl-release-kernel kernel)
              (cl-release-program program))))
      (list reducefn cleanup))))

;;; Maps
(defun make-opencl-mapper (queue input-types mexpr
                           &key
                             (nparams 0)
                             output-type
                             (preamble "")
                             headers
                             options)
  "Returns a list (mapper cleanup) where mapper is of the form

(lambda (in-buffers out-buffer &key params event-wait-list) ...)

that will perform a map operation according to the input parameters.
in-buffers and out-buffer must be a list of OpenCL memory objects and
a single object respectively.  The results will be written to
out-buffer starting at 0.  If only a subset of a buffer should be read
from or written to, use OpenCL sub buffers via cl-create-sub-buffer.
The return value of the generated function is an event that will need
to be waited on and released at some point.

cleanup should be called once the mapper is done being used so that
OpenCL memory can be cleaned up.  The with-opencl-cleanup macro can be
useful.

queue must be an OpenCL queue handle.

input-types should be a list of CFFI type designators or a single type
designator.  In the case of a single type designator, in-starts and
in-ends are assumed to be indices rather than lists of indices.

If output-type is not specified, it is assumed to be the first
input-type.

mexpr should be a Lisp function accepting one string and returning
OpenCL C code for an expression of a unary operation to apply to the
variable referenced in the string argument.  The expression can refer
to code defined in the preamble string argument or included in one of
the headers.

params can be a list of parameters to supply to the mexpr form in
addition to the primary argument.  They will be handed to OpenCL
through a buffer.

memobj is the source memory buffer.

preamble can be a string of OpenCL C code to be placed below headers
but above the reduction kernel.  Useful for defining utility functions
to call in the expression.

headers can be a list of device header file names to include in the
kernel OpenCL C code.

options can be compiler options for the OpenCL kernel.

kernel and program will need to be released at some point once you're
done using the reducer."
  (let* ((input-list-p (and (listp input-types)
                            (not (eq (first input-types) :struct))))
         (input-type-list (if input-list-p
                              input-types
                              (list input-types)))
         (ninputs (length input-type-list))
         (output-type (if output-type
                          output-type
                          (first input-type-list)))
         (kernel-source
          (concatenate
           'string
           (format nil "~{#include \"~a\"~^~%~}"
                   headers)
           (program-source-from-forms-fn
            `(concat
              ,preamble
              (kernel map
                      ((var n (global (pointer :ulong)))
                       (var params (global (pointer ,output-type)))
                       (var output (global (pointer ,output-type)))
                       ,@(loop
                            for i below ninputs
                            for type in input-type-list
                            collecting
                              `(var ,(format nil "input~a" i)
                                    (global (pointer ,type)))))
                      (var gid (const :int)
                           (get-global-id 0))
                      (var end (const :ulong)
                           (aref n 0))
                      (when (< gid
                               end)
                        (setf (aref output gid)
                              ,(apply mexpr
                                      (append
                                       (loop
                                          for i below ninputs
                                          collecting
                                            `(aref ,(format nil
                                                            "input~a"
                                                            i)
                                                   gid))
                                       (loop
                                          for i below nparams
                                          collecting `(aref params ,i)))))))))))
         (context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program-with-log program (list dev)
                               :options options)
    (let* ((kernel
            (cl-create-kernel program "map"))
           (lastparams NIL)
           (lastn NIL)
           (nbuf
            (cl-create-buffer context
                              :flags +CL-MEM-READ-ONLY+
                              :type :ulong
                              :count 1))
           (paramsbuf (when (not (zerop nparams))
                        (cl-create-buffer context
                                          :flags +CL-MEM-READ-ONLY+
                                          :type output-type
                                          :count nparams)))
           (mapfn
            (lambda (in-buffers out-buffer &key event-wait-list params)
              (let* ((events nil)
                     (in-buffer-list
                      (if (listp in-buffers)
                          in-buffers
                          (list in-buffers)))
                     (params (when (and (not (zerop nparams))
                                        params
                                        (not (equal params lastparams)))
                               (setf lastparams params)
                               params))
                     (n
                      (reduce #'min
                              (loop
                                 for b in in-buffer-list
                                 for type in input-type-list
                                 collecting
                                   (floor (cl-get-mem-object-info
                                           b
                                           +CL-MEM-SIZE+)
                                          (foreign-type-size type)))))
                     (event NIL)
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups)))
                (when (or (not lastn)
                          (not (equal n lastn)))
                  (setf lastn n)
                  (push
                   (cl-enqueue-write-buffer queue
                                            nbuf
                                            :ulong
                                            (list lastn)
                                            :event-wait-list
                                            event-wait-list)
                   events))
                (when params
                  (push
                   (cl-enqueue-write-buffer queue paramsbuf
                                            output-type
                                            params
                                            :event-wait-list
                                            (append events event-wait-list))
                   events))
                ;; set kernel arguments
                (cl-set-kernel-arg kernel 2 :value out-buffer)
                (loop
                   for i from 3
                   for inbuf in in-buffer-list
                   do
                     (cl-set-kernel-arg kernel i :value inbuf))
                (setf event
                      (list
                       (cl-enqueue-ndrange-kernel queue kernel
                                                  (list globalworksize)
                                                  (list wgsize)
                                                  :event-wait-list
                                                  (append events event-wait-list))
                       (lambda ()
                         (loop for ev in events
                            do (release-opencl-event ev)))))
                event)))
           (cleanup
            (lambda ()
              (cl-release-kernel kernel)
              (cl-release-program program)
              (cl-release-mem-object nbuf)
              (when (not (zerop nparams))
                (cl-release-mem-object paramsbuf)))))
      ;; set constant arguments
      (cl-set-kernel-arg kernel 0 :value nbuf)
      (cl-set-kernel-arg kernel 1 :value paramsbuf)
      (list mapfn cleanup)
      )))
