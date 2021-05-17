(in-package :cl-opencl-utils)

;;;; Convolute two OpenCL C functions
  
(defun make-opencl-convolutor (queue expr-A expr-B domain
                               &key
                                 (nparams-A 0)
                                 (nparams-B 0)
                                 (type :float)
                                 (ndomain (expt 10 6)))
  "Returns a list (convolutor cleanup), where convolutor accepts an
input value and returns the convolution of functions A and B at that
point, and cleanup is a function to call to cleanup foreign and OpenCL
memory allocations.

expr-A and expr-B should be functions that accept a list of strings
and return an OpenCL C expression for the A or B function value
respectively.  The first string will be the primary argument, and the
rest of the the strings will be parameter values.

ndomain is the number of samples to take from domain in order to
perform the numerical integration.

convolutor will be a function (lambda (x &key params-A params-B)...)
with sticky values for params-A and params-B so that they only need
supplying when they need to be changed.  The number of parameters are
set by the nparams-A and nparams-B for A and B respectively."
  (let* ((context (cl-get-command-queue-info
                   queue
                   +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (nbuf (cl-create-buffer context
                                 :flags +CL-MEM-READ-ONLY+
                                 :type :ulong
                                 :data (list ndomain)))
         (lohibuf (cl-create-buffer
                   context
                   :flags +CL-MEM-READ-ONLY+
                   :type type
                   :data (list (car domain)
                               (cdr domain))))
         (xbuf (cl-create-buffer context
                                 :flags +CL-MEM-READ-ONLY+
                                 :type type
                                 :count 1))
         (Bbuf (cl-create-buffer context
                                 :flags +CL-MEM-READ-WRITE+
                                 :type type
                                 :count ndomain))
         (Rbuf (cl-create-buffer context
                                 :flags +CL-MEM-READ-WRITE+
                                 :type type
                                 :count ndomain))
         ;; Convolution kernel
         (convolution-source
          (program-source-from-forms-fn
           `(kernel convolution
                    ((var nn (global (pointer :ulong)))
                     (var lohi (global (pointer ,type)))
                     (var xx (global (pointer ,type)))
                     (var Aparams (global (pointer ,type)))
                     (var B (global (pointer ,type)))
                     (var R (global (pointer ,type))))
                    (var n (const :ulong)
                         (value nn))
                    (var gid (const :int) (get-global-id 0))
                    (when (< gid n)
                      (var lo (const ,type) (aref lohi 0))
                      (var hi (const ,type) (aref lohi 1))
                      (var x (const ,type)
                           (value xx))
                      (var step (const ,type) (/ (- hi lo) n))
                      (var t (const ,type)
                           (+ lo
                              (* gid step)
                              (* 0.5 step)))
                      (setf (aref R gid)
                            (* ,(apply expr-A
                                       '(- x t)
                                       (loop
                                          for i below nparams-A
                                          collecting `(aref Aparams ,i)))
                               (aref B gid)
                               step)))))
           ;; (format nil
           ;;                   "__kernel
           ;; void convolution(__global long unsigned int* N,
           ;;                  __global ~a *lohi,
           ;;                  __global ~a *X,
           ;;                  __global ~a *Aparams,
           ;;                  __global ~a *B,
           ;;                  __global ~a *R)
           ;; {
           ;;   const long unsigned int n = *N;
           ;;   const int gid = get_global_id(0);
           ;;   if(gid < n) {
           ;;     const ~a lo = lohi[0];
           ;;     const ~a hi = lohi[1];
           ;;     const ~a x = *X;
           ;;     const ~a step = (hi - lo)/n;
           ;;     const ~a t = lo + gid*step + 0.5*step;
           ;;     R[gid] = (~a) * B[gid] * step;
           ;;   }
           ;; }"
           ;;                   typename typename typename typename typename ; args
           ;;                   typename typename typename ; lo hi x
           ;;                   typename typename ; step t
           ;;                   (apply expr-A "(x - t)"
           ;;                          (loop
           ;;                             for i below nparams-A
           ;;                             collecting (format nil "Aparams[~a]" i))))
           )
         (convolution-program
          (let* ((program
                   (cl-create-program-with-source
                    context convolution-source)))
            (cl-build-program-with-log program (list dev))
            program))
         (convolution-kernel (cl-create-kernel
                              convolution-program "convolution"))
         ;; Sum kernel
         (reducer-results
          (make-opencl-reducer
           queue type +OPENCL-ADD-REXPR+))
         (reducer (first reducer-results))
         (reducer-cleanup (second reducer-results))
         (Apars NIL)
         (Aparambuf (when (not (zerop nparams-A))
                      (cl-create-buffer context
                                        :count nparams-A
                                        :type type)))
         (Bpars NIL)
         (Bparambuf (when (not (zerop nparams-B))
                      (cl-create-buffer context
                                        :count nparams-B
                                        :type type)))
         ;; B buffer kernel
         (b-source
          (program-source-from-forms-fn
           `(kernel setb
                    ((var lohi (global (pointer ,type)))
                     (var Bparams (global (pointer ,type)))
                     (var nn (global (pointer :ulong)))
                     (var B (global (pointer ,type))))
                    (var n (const :ulong)
                         (value nn))
                    (var gid (const :int)
                         (get-global-id 0))
                    (when (< gid n)
                      (var lo (const ,type) (aref lohi 0))
                      (var hi (const ,type) (aref lohi 1))
                      (var step (const ,type) (/ (- hi lo) n))
                      (var x (const ,type)
                           (+ lo
                              (* gid step)
                              (* 0.5 step)))
                      (setf (aref B gid)
                            ,(apply expr-B
                                    'x
                                    (loop
                                       for i below nparams-B
                                       collecting `(aref Bparams ,i))))))))
         (b-program
          (let* ((program
                   (cl-create-program-with-source context b-source)))
            (cl-build-program-with-log program (list dev))
            program))
         (b-kernel
          (cl-create-kernel b-program "setb"))
         (convolutor
          (lambda (x &key params-A params-B)
            (let* ((events NIL))
              (when (and (not (zerop nparams-A))
                         params-A
                         (not (equal params-A Apars)))
                (setf Apars params-A)
                (push (cl-enqueue-write-buffer
                       queue Aparambuf
                       type
                       params-A)
                      events))
              (when (and (not (zerop nparams-B))
                         params-B
                         (not (equal params-B Bpars)))
                (setf Bpars params-B)
                (push (cl-enqueue-write-buffer
                       queue Bparambuf
                       type
                       params-B)
                      events)
                (push (cl-enqueue-kernel
                       queue b-kernel
                       ndomain)
                      events))
              (push (cl-enqueue-write-buffer queue xbuf type (list x)
                                             :blocking-p nil)
                    events)
              (push (cl-enqueue-kernel
                     queue convolution-kernel
                     ndomain)
                    events)
              (push (funcall reducer Rbuf)
                    events)
              (first (cl-wait-and-release-events events)))))
         (cleanup
          (lambda ()
            (cl-release-kernel convolution-kernel)
            (cl-release-program convolution-program)
            (funcall reducer-cleanup)
            (cl-release-kernel b-kernel)
            (cl-release-program b-program)
            (cl-release-mem-object nbuf)
            (cl-release-mem-object lohibuf)
            (when (not (zerop nparams-A))
              (cl-release-mem-object Aparambuf))
            (when (not (zerop nparams-B))
              (cl-release-mem-object Bparambuf))
            (cl-release-mem-object xbuf)
            (cl-release-mem-object Bbuf)
            (cl-release-mem-object Rbuf))))
    ;;; Initial setup of buffers:
    ;; N buffer already initialized above
    ;; B buffer
    (cl-set-kernel-arg b-kernel 0
                       :value lohibuf)
    (cl-set-kernel-arg b-kernel 1
                       :value
                       Bparambuf)
    (cl-set-kernel-arg b-kernel 2
                       :value nbuf)
    (cl-set-kernel-arg b-kernel 3
                       :value Bbuf)
    ;; Setup convolution kernel
    (cl-set-kernel-arg convolution-kernel 0 :value nbuf)
    (cl-set-kernel-arg convolution-kernel 1 :value lohibuf)
    (cl-set-kernel-arg convolution-kernel 2 :value xbuf)
    (cl-set-kernel-arg convolution-kernel 3 :value Aparambuf)
    (cl-set-kernel-arg convolution-kernel 4 :value Bbuf)
    (cl-set-kernel-arg convolution-kernel 5 :value Rbuf)
    (list convolutor cleanup)))
