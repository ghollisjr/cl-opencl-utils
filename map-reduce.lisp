(in-package :cl-opencl-utils)

;;; Reductions
(defparameter +OPENCL-ADD-REXPR+
  (opencl-function-expr '+))

(defun make-opencl-reducer (queue type rexpr
                            &key
                              (preamble "")
                              headers
                              options)
  "Returns a list (reducer cleanup) where reducer is of the form

(lambda (buffer &key start end) ...)

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
           preamble
           (program-source-from-forms-fn
            `(kernel reduce
                     ((var input (global (pointer ,type)))
                      (var startend (global (pointer :ulong)))
                      (var output (global (pointer ,type)))
                      (var acc (local (pointer ,type))))
                     (var nwork (const :int)
                          (get-local-size 0))
                     (var gid (const :int)
                          (get-global-id 0))
                     (var wid (const :int)
                          (get-local-id 0))
                     (var start (const :ulong)
                          (aref startend 0))
                     (var end (const :ulong)
                          (aref startend 1))
                     (setf (aref acc wid) 0)
                     (barrier +CLK-LOCAL-MEM-FENCE+)
                     (when (< (+ start gid)
                              end)
                       (setf (aref acc wid)
                             (aref input
                                   (+ start gid)))
                       (barrier +CLK-LOCAL-MEM-FENCE+)
                       (var niter :ulong
                            (ceil (log2 (coerce nwork :float))))
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
                               (aref acc 0))))))))
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
            (lambda (buffer &key start end)
              (let* ((start (if start
                                start
                                0))
                     (bufsize (cl-get-mem-object-info buffer
                                                      +CL-MEM-SIZE+))
                     (end (if end
                              end
                              (/ bufsize
                                 (foreign-type-size type))))
                     (events NIL) ; accumulated events and cleanup functions
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (n (- end start))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups))
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
                (push (cl-enqueue-ndrange-kernel queue kernel
                                                 (list globalworksize)
                                                 (list wgsize))
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
                                                   (list 0 n))
                          events)
                         (cl-set-kernel-arg kernel 0 :value buf1)
                       ;; might not actually need this, will test
                         (cl-set-kernel-arg kernel 1 :value startendbuf)
                         (cl-set-kernel-arg kernel 2 :value buf2)
                         (push
                          (cl-enqueue-ndrange-kernel queue
                                                     kernel
                                                     (list globalworksize)
                                                     (list localworksize))
                          events)))
                  (destructuring-bind (finalevent returner)
                      (cl-enqueue-read-buffer
                       queue buf2
                       type 1)
                    (setf events (nreverse events))
                    (let* ((cleanup
                            (lambda ()
                              (loop
                                 for ev in events
                                 do
                                   (if (listp ev)
                                       (progn
                                         (funcall (second ev))
                                         (cl-release-event (first ev)))
                                       (cl-release-event ev)))
                              (cl-release-mem-object outbuf2)
                              (cl-release-mem-object outbuf1)
                              (cl-release-mem-object startendbuf)
                              (elt (funcall returner) 0))))
                      (list finalevent cleanup)))))))
           (cleanup
            (lambda ()
              (cl-release-kernel kernel)
              (cl-release-program program))))
      (list reducefn cleanup)))) 

;;; Maps
(defun make-opencl-mapper (queue input-type mexpr
                           &key
                             (nparams 0)
                             output-type
                             (preamble "")
                             headers
                             options)
  "Returns a list (mapper cleanup) where mapper is of the form

(lambda (in-buffer out-buffer &key params in-start in-end out-start) ...)

that will perform a map operation according to the input parameters.
in-buffer and out-buffer must be OpenCL memory objects, and in-start
and in-end are optional indices into the in-buffer.  The results will
be written to out-buffer starting at out-start.  in-start defaults to
0, in-end defaults to the end of the input buffer, and out-start
defaults to 0.  The return value of the generated function is an event
that will need to be waited on and released at some point.

cleanup should be called once the mapper is done being used so that
OpenCL memory can be cleaned up.

queue must be an OpenCL queue handle.

input-type should be a CFFI type designator.

If output-type is not specified, it is assumed to be input-type.

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

start can be an initial index offset to start reading from the buffer.

end can be a final index offset to stop reading from the buffer.  Uses
subseq index convention, i.e. the element at index=end is not
included.

kernel and program will need to be released at some point once you're
done using the reducer."
  (let* ((output-type (if output-type output-type input-type))
         (kernel-source
          (concatenate
           'string
           (format nil "~{#include \"~a\"~^~%~}"
                   headers)
           preamble
           (program-source-from-forms-fn
            `(kernel map
                     ((var input (global (pointer ,input-type)))
                      (var input_startend (global (pointer :ulong)))
                      (var params (global (pointer ,output-type)))
                      (var output_start (global (pointer :ulong)))
                      (var output (global (pointer ,output-type))))
                     (var gid (const :int)
                          (get-global-id 0))
                     (var start (const :ulong)
                          (aref input_startend 0))
                     (var end (const :ulong)
                          (aref input_startend 1))
                     (var outstart (const :ulong)
                          (aref output_start 0))
                     (when (< (+ start gid)
                              end)
                       (setf (aref output (+ outstart gid))
                             ,(apply mexpr
                                     `(aref input (+ start gid))
                                     (loop
                                        for i below nparams
                                        collecting `(aref params ,i)))))))))
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
           (lastin-start 0)
           (lastin-end NIL)
           (lastout-start 0)
           (in-startendbuf
            (cl-create-buffer context
                              :flags +CL-MEM-READ-ONLY+
                              :type :ulong
                              :count 2))
           (out-startbuf
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
            (lambda (in-buffer out-buffer &key params in-start in-end out-start)
              (let* ((params (when (and (not (zerop nparams))
                                        params
                                        (not (equal params lastparams)))
                               (setf lastparams params)
                               params))
                     (in-start (when (and in-start
                                          (not (equal in-start lastin-start)))
                                 (setf lastin-start in-start)
                                 in-start))
                     (in-bufsize (cl-get-mem-object-info in-buffer
                                                         +CL-MEM-SIZE+))
                     (in-end (cond
                               ((and in-end
                                     (not (equal in-end lastin-end)))
                                (setf lastin-end in-end)
                                in-end)
                               ((not lastin-end)
                                (setf lastin-end
                                      (/ in-bufsize
                                         (foreign-type-size input-type)))
                                lastin-end)
                               (t NIL)))
                     (out-start (when (and out-start
                                           (not (equal out-start lastout-start)))
                                  (setf lastout-start
                                        out-start)
                                  out-start))
                     (event NIL)
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (n (- lastin-end lastin-start))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups)))
                (when (or in-start in-end)
                  (cl-enqueue-write-buffer queue in-startendbuf
                                           :ulong
                                           (list lastin-start
                                                 lastin-end)
                                           :blocking-p t))
                (when out-start
                  (cl-enqueue-write-buffer queue out-startbuf
                                           :ulong
                                           (list out-start)
                                           :blocking-p t))
                (when params
                  (cl-enqueue-write-buffer queue paramsbuf
                                           :output-type
                                           params
                                           :blocking-p t))
                ;; set kernel arguments
                (cl-set-kernel-arg kernel 0 :value in-buffer)
                (cl-set-kernel-arg kernel 4 :value out-buffer)
                (setf event
                      (cl-enqueue-ndrange-kernel queue kernel
                                                 (list globalworksize)
                                                 (list wgsize)))
                
                event)))
           (cleanup
            (lambda ()
              (cl-release-kernel kernel)
              (cl-release-program program)
              (cl-release-mem-object in-startendbuf)
              (cl-release-mem-object out-startbuf)
              (when (not (zerop nparams))
                (cl-release-mem-object paramsbuf)))))
      ;; set constant arguments
      (cl-set-kernel-arg kernel 1 :value in-startendbuf)
      (cl-set-kernel-arg kernel 2 :value paramsbuf)
      (cl-set-kernel-arg kernel 3 :value out-startbuf)
      (list mapfn cleanup)
      )))
