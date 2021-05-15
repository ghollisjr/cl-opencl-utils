(in-package :cl-opencl-utils)

;;;; Convolute two OpenCL C functions
(defun make-opencl-convolutor (queue fnA-expr fnB-expr domain
                               &key
                                 (nparams-A 0)
                                 (nparams-B 0)
                                 (type :float)
                                 (ndomain (expt 10 6)))
  "Returns a list (convolutor cleanup), where convolutor accepts an
input value and returns the convolution of fnA and fnB at that point,
and cleanup is a function to call to cleanup foreign and OpenCL memory
allocations.

fnA-expr and fnB-expr should be functions that accept a list of
strings and return an OpenCL C expression for the fnA or fnB function
value respectively.  The first string will be the primary argument,
and the rest of the the strings will be parameter values.

ndomain is the number of samples to take from domain in order to
perform the numerical integration.

convolutor will be a function (lambda (x &key Aparams Bparams)...)
with sticky values for Aparams and Bparams so that they only need
supplying when they need to be changed.  The number of parameters are
set by the nparams-A and nparams-B for fnA and fnB respectively."
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
           `(kernel :void convolution
                    ((var (global (pointer :ulong)) nn)
                     (var (global (pointer ,type)) lohi)
                     (var (global (pointer ,type)) xx)
                     (var (global (pointer ,type)) Aparams)
                     (var (global (pointer ,type)) B)
                     (var (global (pointer ,type)) R))
                    (var (const :ulong) n
                         (value nn))
                    (var (const :int) gid (get-global-id 0))
                    (when (< gid n)
                      (var (const ,type) lo (aref lohi 0))
                      (var (const ,type) hi (aref lohi 1))
                      (var (const ,type) x
                           (value xx))
                      (var (const ,type) step (/ (- hi lo) n))
                      (var (const ,type) t
                           (+ lo
                              (* gid step)
                              (* 0.5 step)))
                      (setf (aref R gid)
                            (* ,(apply fnA-expr
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
           ;;                   (apply fnA-expr "(x - t)"
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
         (reducer-kernel (second reducer-results))
         (reducer-program (third reducer-results))
         (Apars NIL)
         (Aparambuf (cl-create-buffer context
                                      :count nparams-A
                                      :type type))
         (Bpars NIL)
         (Bparambuf (cl-create-buffer context
                                      :count nparams-B
                                      :type type))
         ;; B buffer kernel
         (b-source
          (program-source-from-forms-fn
           `(kernel :void setb
                    ((var (global (pointer ,type)) lohi)
                     (var (global (pointer ,type)) Bparams)
                     (var (global (pointer :ulong)) nn)
                     (var (global (pointer ,type)) B))
                    (var (const :ulong) n
                         (value nn))
                    (var (const :int) gid
                         (get-global-id 0))
                    (when (< gid n)
                      (var (const ,type) lo (aref lohi 0))
                      (var (const ,type) hi (aref lohi 1))
                      (var (const ,type) step (/ (- hi lo) n))
                      (var (const ,type) x
                           (+ lo
                              (* gid step)
                              (* 0.5 step)))
                      (setf (aref B gid)
                            ,(apply fnB-expr
                                    'x
                                    (loop
                                       for i below nparams-B
                                       collecting `(aref Bparams ,i)))))))

           ;; (format nil "__kernel
           ;; void setb (__global ~a *lohi,
           ;;            __global ~a *Bparams,
           ;;            __global long unsigned int* N,
           ;;            __global ~a *B)
           ;; {
           ;;   const long unsigned int n = *N;
           ;;   const int gid = get_global_id(0);
           ;;   if(gid < n) {
           ;;     const ~a lo = lohi[0];
           ;;     const ~a hi = lohi[1];
           ;;     const ~a step = (hi - lo)/n;
           ;;     const ~a x = lo + gid*step + 0.5*step;
           ;;     B[gid] = ~a;
           ;;   }
           ;; }"
           ;;                   typename typename typename ; arguments
           ;;                   typename typename ; lo hi
           ;;                   typename typename ; step x
           ;;                   (apply fnB-expr "x"
           ;;                          (loop
           ;;                             for i below nparams-B
           ;;                             collecting (format nil "Bparams[~a]" i))))
           )
         (b-program
          (let* ((program
                   (cl-create-program-with-source context b-source)))
            (cl-build-program-with-log program (list dev))
            program))
         (b-kernel
          (cl-create-kernel b-program "setb"))
         (convolutor
          (lambda (x &key Aparams Bparams)
            (let* ((events NIL))
              (when (and Aparams
                         (not (equal Aparams Apars)))
                (setf Apars Aparams)
                (push (cl-enqueue-write-buffer
                       queue Aparambuf
                       type
                       Aparams)
                      events))
              (when (and Bparams
                         (not (equal Bparams Bpars)))
                (setf Bpars Bparams)
                (push (cl-enqueue-write-buffer
                       queue Bparambuf
                       type
                       Bparams)
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
              ;; debug
              ;; (cl-wait-and-release-events events)
              ;; (cl-enqueue-read-buffer queue Bbuf :float ndomain
              ;;                                   :blocking-p t)
              ;; end debug

              (push (funcall reducer Rbuf)
                    events)

              (first (cl-wait-and-release-events events))
              )))
         (cleanup
          (lambda ()
            (cl-release-kernel convolution-kernel)
            (cl-release-program convolution-program)
            (cl-release-kernel reducer-kernel)
            (cl-release-program reducer-program)
            (cl-release-kernel b-kernel)
            (cl-release-program b-program)
            (cl-release-mem-object nbuf)
            (cl-release-mem-object lohibuf)
            (cl-release-mem-object Aparambuf)
            (cl-release-mem-object Bparambuf)
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
