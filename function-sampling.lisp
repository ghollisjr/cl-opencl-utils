(in-package :cl-opencl-utils)

(defun make-opencl-function-sampler-old
    (queue expr
     &key
       (nparams 0)
       (output-type :float)
       (preamble "")
       headers
       options)
  "Returns a list (function cleanup) where function is of the form

(lambda (out-buffer &key low high nsamples out-start) ...)

that will perform a function sampling operation according to the input
parameters.  out-buffer must be an OpenCL memory object with write
ability, and low, high, and nsamples are parameters for function
sampling that must be set at least once.  The optional parameters are
sticky, so that once set, they maintain their value until changed.
The results will be written to out-buffer starting at out-start.
out-start defaults to 0.  The return value of the generated function
is an event that will need to be waited on and released at some point.

queue must be an OpenCL queue handle.

input-type should be a CFFI type designator.

If output-type is not specified, it is assumed to be input-type.

expr should be a Lisp function accepting one string and returning
OpenCL C code for an expression of a unary operation to apply to the
variable referenced in the string argument.  The expression can refer
to code defined in the preamble string argument or included in one of
the headers.  opencl-function-expr can be useful to generate
expressions from already-defined OpenCL C functions.

memobj is the source memory buffer.

preamble can be Lispified OpenCL C code or a string of OpenCL C code
to be placed below headers but above the reduction kernel.  Useful for
defining utility functions to call in the expression.

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
              (kernel sampler
                      ((var output_start (global (pointer :ulong)))
                       (var lowhigh (global (pointer ,output-type)))
                       (var nsamples (global (pointer :ulong)))
                       (var params (global (pointer ,output-type)))
                       (var output (global (pointer ,output-type))))
                      (var n (const :ulong) (aref nsamples 0))
                      (var gid (const :int)
                           (get-global-id 0))
                      (when (< gid
                               n)
                        (var outstart (const :ulong)
                             (aref output_start 0))
                        (var low (const ,output-type)
                             (aref lowhigh 0))
                        (var high (const ,output-type)
                             (aref lowhigh 1))

                        (var step (const ,output-type)
                             (/ (- high low)
                                (coerce (- n 1)
                                        ,output-type)))
                        (var x (const ,output-type)
                             (+ low (* step gid)))
                        (setf (aref output
                                    ;; 0
                                    (+ outstart gid)
                                    )
                              ;; 1d0
                              ,(apply expr
                                      'x
                                      (loop
                                         for i below nparams
                                         collecting `(aref params ,i)))

                              )))))))
         (context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (lowhighbuf (cl-create-buffer context
                                       :flags +CL-MEM-READ-ONLY+
                                       :type output-type
                                       :count 2))
         (nsamplesbuf (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :count 1))
         (paramsbuf (when (not (zerop nparams))
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type output-type
                                        :count nparams)))
         (outstartbuf (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :count 1))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program-with-log program (list dev)
                               :options options)
    (let* ((kernel
            (cl-create-kernel program "sampler"))
           (lastlow (if (eq output-type :double)
                        0d0
                        0f0))
           (lasthigh (if (eq output-type :double)
                         1d0
                         1f0))
           (lastnsamples 100)
           (lastout-start 0)
           (lastparams NIL)
           (sampler
            (lambda (out-buffer &key low high nsamples params out-start)
              (let* ((params (when (and params
                                        (not (equal params lastparams)))
                               (setf lastparams params)
                               params))
                     (out-start (when (and out-start
                                           (not (equal out-start lastout-start)))
                                  (setf lastout-start out-start)
                                  out-start))
                     (low (when
                              (and low
                                   (not (equal low lastlow)))
                            (setf lastlow low)
                            low))
                     (high (when (and high
                                      (not (equal high lasthigh)))
                             (setf lasthigh high)
                             high))
                     (nsamples (when (and nsamples
                                          (not (equal nsamples lastnsamples)))
                                 (setf lastnsamples nsamples)
                                 nsamples))

                     (event NIL))
                (destructuring-bind (globalworksize wgsize)
                    (get-opencl-kernel-work-size kernel dev lastnsamples)
                  ;; Setup potentially new out-buffer argument
                  (cl-set-kernel-arg kernel 4 :value out-buffer)
                  ;; Optional update before sampling
                  (when (or low high)
                    (cl-enqueue-write-buffer queue lowhighbuf
                                             output-type
                                             (list lastlow lasthigh)
                                             :blocking-p t))
                  (when nsamples
                    (cl-enqueue-write-buffer queue nsamplesbuf
                                             :ulong
                                             (list lastnsamples)
                                             :blocking-p t))
                  (when out-start
                    (cl-enqueue-write-buffer queue outstartbuf
                                             :ulong
                                             (list lastout-start)
                                             :blocking-p t))
                  (when params
                    (cl-enqueue-write-buffer queue paramsbuf
                                             output-type
                                             lastparams
                                             :blocking-p t))
                  (setf event
                        (cl-enqueue-ndrange-kernel queue kernel
                                                   (list globalworksize)
                                                   (list wgsize)))
                  event))))
           (cleanup (lambda ()
                      (cl-release-mem-object lowhighbuf)
                      (cl-release-mem-object nsamplesbuf)
                      (cl-release-mem-object outstartbuf)
                      (when (not (zerop nparams))
                        (cl-release-mem-object paramsbuf))
                      (cl-release-kernel kernel)
                      (cl-release-program program))))
      ;; initialize kernel with fixed buffer arguments
      (cl-enqueue-write-buffer queue outstartbuf :ulong
                               (list lastout-start)
                               :blocking-p t)
      (cl-enqueue-write-buffer queue lowhighbuf output-type
                               (list lastlow lasthigh)
                               :blocking-p t)
      (cl-enqueue-write-buffer queue nsamplesbuf :ulong
                               (list lastnsamples)
                               :blocking-p t)
      (cl-set-kernel-arg kernel 0 :value outstartbuf)
      (cl-set-kernel-arg kernel 1 :value lowhighbuf)
      (cl-set-kernel-arg kernel 2 :value nsamplesbuf)
      (cl-set-kernel-arg kernel 3 :value paramsbuf)
      (list sampler cleanup))))

(defun make-opencl-function-sampler
    (queue expr
     &key
       (domain-type :float)
       (range-type domain-type)
       (nparams 0)
       (preamble "")
       headers
       options)
  "Returns a list (function cleanup) where function is of the form

(lambda (out-buffer &key low high nsamples out-start) ...)

that will perform a function sampling operation according to the input
parameters.  out-buffer must be an OpenCL memory object with write
ability, and low, high, and nsamples are parameters for function
sampling that must be set at least once.  The optional parameters are
sticky, so that once set, they maintain their value until changed.
The results will be written to out-buffer starting at out-start.
out-start defaults to 0.  The return value of the generated function
is an event that will need to be waited on and released at some point.

queue must be an OpenCL queue handle.

input-type should be a CFFI type designator.

If output-type is not specified, it is assumed to be input-type.

expr should be a Lisp function accepting one string and returning
OpenCL C code for an expression of a unary operation to apply to the
variable referenced in the string argument.  The expression can refer
to code defined in the preamble string argument or included in one of
the headers.  opencl-function-expr can be useful to generate
expressions from already-defined OpenCL C functions.

memobj is the source memory buffer.

preamble can be Lispified OpenCL C code or a string of OpenCL C code
to be placed below headers but above the reduction kernel.  Useful for
defining utility functions to call in the expression.

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
              (kernel sampler
                      ((var output_start (global (pointer :ulong)))
                       (var lowhigh (global (pointer ,domain-type)))
                       (var nsamples (global (pointer :ulong)))
                       (var params (global (pointer ,domain-type)))
                       (var output (global (pointer ,range-type))))
                      (var n (const :ulong) (aref nsamples 0))
                      (var gid (const :int)
                           (get-global-id 0))
                      (when (< gid
                               n)
                        (var outstart (const :ulong)
                             (aref output_start 0))
                        (var low (const ,domain-type)
                             (aref lowhigh 0))
                        (var high (const ,domain-type)
                             (aref lowhigh 1))

                        (var step (const ,domain-type)
                             (/ (- high low)
                                (coerce (- n 1)
                                        ,domain-type)))
                        (var x (const ,domain-type)
                             (+ low (* step gid)))
                        (setf (aref output
                                    ;; 0
                                    (+ outstart gid)
                                    )
                              ;; 1d0
                              ,(apply expr
                                      'x
                                      (loop
                                         for i below nparams
                                         collecting `(aref params ,i)))

                              )))))))
         (context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (lowhighbuf (cl-create-buffer context
                                       :flags +CL-MEM-READ-ONLY+
                                       :type domain-type
                                       :count 2))
         (nsamplesbuf (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :count 1))
         (paramsbuf (when (not (zerop nparams))
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type domain-type
                                        :count nparams)))
         (outstartbuf (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :count 1))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program-with-log program (list dev)
                               :options options)
    (let* ((kernel
            (cl-create-kernel program "sampler"))
           (lastlow (if (eq domain-type :double)
                        0d0
                        0f0))
           (lasthigh (if (eq domain-type :double)
                         1d0
                         1f0))
           (lastnsamples 100)
           (lastout-start 0)
           (lastparams NIL)
           (sampler
            (lambda (out-buffer &key low high nsamples params out-start)
              (let* ((params (when (and params
                                        (not (equal params lastparams)))
                               (setf lastparams params)
                               params))
                     (out-start (when (and out-start
                                           (not (equal out-start lastout-start)))
                                  (setf lastout-start out-start)
                                  out-start))
                     (low (when
                              (and low
                                   (not (equal low lastlow)))
                            (setf lastlow low)
                            low))
                     (high (when (and high
                                      (not (equal high lasthigh)))
                             (setf lasthigh high)
                             high))
                     (nsamples (when (and nsamples
                                          (not (equal nsamples lastnsamples)))
                                 (setf lastnsamples nsamples)
                                 nsamples))

                     (event NIL))
                (destructuring-bind (globalworksize wgsize)
                    (get-opencl-kernel-work-size kernel dev lastnsamples)
                  ;; Setup potentially new out-buffer argument
                  (cl-set-kernel-arg kernel 4 :value out-buffer)
                  ;; Optional update before sampling
                  (when (or low high)
                    (cl-enqueue-write-buffer queue lowhighbuf
                                             domain-type
                                             (list lastlow lasthigh)
                                             :blocking-p t))
                  (when nsamples
                    (cl-enqueue-write-buffer queue nsamplesbuf
                                             :ulong
                                             (list lastnsamples)
                                             :blocking-p t))
                  (when out-start
                    (cl-enqueue-write-buffer queue outstartbuf
                                             :ulong
                                             (list lastout-start)
                                             :blocking-p t))
                  (when params
                    (cl-enqueue-write-buffer queue paramsbuf
                                             domain-type
                                             lastparams
                                             :blocking-p t))
                  (setf event
                        (cl-enqueue-ndrange-kernel queue kernel
                                                   (list globalworksize)
                                                   (list wgsize)))
                  event))))
           (cleanup (lambda ()
                      (cl-release-mem-object lowhighbuf)
                      (cl-release-mem-object nsamplesbuf)
                      (cl-release-mem-object outstartbuf)
                      (when (not (zerop nparams))
                        (cl-release-mem-object paramsbuf))
                      (cl-release-kernel kernel)
                      (cl-release-program program))))
      ;; initialize kernel with fixed buffer arguments
      (cl-enqueue-write-buffer queue outstartbuf :ulong
                               (list lastout-start)
                               :blocking-p t)
      (cl-enqueue-write-buffer queue lowhighbuf domain-type
                               (list lastlow lasthigh)
                               :blocking-p t)
      (cl-enqueue-write-buffer queue nsamplesbuf :ulong
                               (list lastnsamples)
                               :blocking-p t)
      (cl-set-kernel-arg kernel 0 :value outstartbuf)
      (cl-set-kernel-arg kernel 1 :value lowhighbuf)
      (cl-set-kernel-arg kernel 2 :value nsamplesbuf)
      (cl-set-kernel-arg kernel 3 :value paramsbuf)
      (list sampler cleanup))))
