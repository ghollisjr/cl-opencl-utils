(in-package :cl-opencl-utils)

;;;; Sampling integration
(defun make-opencl-integrator (queue expr
                               &key
                                 (mode :simpson)
                                 (nparams 0)
                                 (type :float))
  "Returns a list (integrator cleanup), where integrator is of the form

(lambda (xlow xhigh &key params (ndomain (expt 10 3))...)

which returns the value of the integral of a function between xlow and
xhigh, where the function is supplied as a Lispified OpenCL expression
generated by the expr function.

expr should accept a string representing the input x value and
returning Lispified OpenCL C code for the value of the function at
that x value, optionally accepting additional parameter arguments up
to nparams.

ndomain controls the number of points sampled from the domain
[xlow,xhigh] and is a sticky parameter in that once it's set, it
maintains its value until changed.

params are a sticky argument supplying optional parameters to the
integrated function.

mode can be one of :simpson, :midpoint, :trapezoid, :left, or :right
to use the corresponding sampling method."
  (let* ((context (cl-get-command-queue-info
                   queue
                   +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (nbuf (cl-create-buffer context
                                 :flags +CL-MEM-READ-ONLY+
                                 :type :ulong
                                 :count 1))
         (lohibuf (cl-create-buffer
                   context
                   :flags +CL-MEM-READ-ONLY+
                   :type type
                   :count 2))
         (samplebuf NIL)
         ;; Sampling kernel
         (sampler-source
          (program-source-from-forms-fn
           `(kernel sampler
                    ((var nsamples (global (pointer :ulong)))
                     (var lowhigh (global (pointer ,type)))
                     (var params (global (pointer ,type)))
                     (var output (global (pointer ,type))))
                    (var n (const :ulong) (aref nsamples 0))
                    (var gid (const :int)
                         (get-global-id 0))
                    (when (< gid
                             n)
                      (var low (const ,type)
                           (aref lowhigh 0))
                      (var high (const ,type)
                           (aref lowhigh 1))

                      (var step (const ,type)
                           (/ (- high low)
                              (coerce n
                                      ,type)))
                      (var x (const ,type)
                           (+ low
                              (* 0.5 step)
                              (* step gid)))
                      (setf (aref output
                                  gid)
                            (* step
                               ,(cond
                                  ((eq mode :simpson)
                                   `(+ (* (/ 2d0 3d0)
                                          ,(apply expr
                                                  'x
                                                  (loop
                                                     for i below nparams
                                                     collecting `(aref params ,i))))
                                       (* (/ 1d0 6d0)
                                          (+ ,(apply expr
                                                     '(- x (* 0.5 step))
                                                     (loop
                                                        for i below nparams
                                                        collecting `(aref params ,i)))
                                             ,(apply expr
                                                     '(+ x (* 0.5 step))
                                                     (loop
                                                        for i below nparams
                                                        collecting `(aref params ,i)))))))
                                  ((eq mode :midpoint)
                                   (apply expr
                                          'x
                                          (loop
                                             for i below nparams
                                             collecting `(aref params ,i))))
                                  ((eq mode :trapezoid)
                                   `(* 0.5
                                       (+ ,(apply expr
                                                  '(- x (* 0.5 step))
                                                  (loop
                                                     for i below nparams
                                                     collecting `(aref params ,i)))
                                          ,(apply expr
                                                  '(+ x (* 0.5 step))
                                                  (loop
                                                     for i below nparams
                                                     collecting `(aref params ,i))))))
                                  ((eq mode :left)
                                   (apply expr
                                          '(- x (* 0.5 step))
                                          (loop
                                             for i below nparams
                                             collecting `(aref params ,i))))
                                  ((eq mode :right)
                                   (apply expr
                                          '(+ x (* 0.5 step))
                                          (loop
                                             for i below nparams
                                             collecting `(aref params ,i)))))))))))
         (sampler-program
          (let* ((program
                  (cl-create-program-with-source
                   context sampler-source)))
            (cl-build-program-with-log program (list dev))
            program))
         (sampler-kernel (cl-create-kernel
                          sampler-program "sampler"))
         ;; Sum kernel
         (reducer-results
          (make-opencl-reducer
           queue type +OPENCL-ADD-REXPR+))
         (reducer (first reducer-results))
         (reducer-cleanup (second reducer-results))
         (pars NIL)
         (parambuf (when (not (zerop nparams))
                     (cl-create-buffer context
                                       :count nparams
                                       :type type)))
         (ndom NIL)
         (integrator
          (lambda (xlow xhigh &key params (ndomain 1000))
            (let* ((events NIL))
              ;; On ndomain change, resampling needs new space.
              ;;
              ;; This could potentially be left alone unless more
              ;; space is needed, but I have a bad feeling about
              ;; never-shrinking memory demands.
              (when (and ndomain
                         (not (equal ndomain ndom)))
                (setf ndom ndomain)
                (push (cl-enqueue-write-buffer
                       queue nbuf
                       :ulong
                       (list ndom))
                      events)
                (when samplebuf
                  (cl-release-mem-object samplebuf)
                  (setf samplebuf NIL)))
              (when (not samplebuf)
                (setf samplebuf
                      (cl-create-buffer context
                                        :type type
                                        :count ndom))
                (cl-set-kernel-arg sampler-kernel 3 :value samplebuf))
              ;; sticky params
              (when (and (not (zerop nparams))
                         params
                         (not (equal params pars)))
                (setf pars params)
                (push (cl-enqueue-write-buffer
                       queue parambuf
                       type
                       params)
                      events))
              (push (cl-enqueue-write-buffer queue lohibuf
                                             type
                                             (list xlow xhigh)
                                             :blocking-p nil)
                    events)
              (push (cl-enqueue-kernel
                     queue sampler-kernel
                     ndomain)
                    events)
              (push (funcall reducer samplebuf)
                    events)
              (first (cl-wait-and-release-events events)))))
         (cleanup
          (lambda ()
            (cl-release-kernel sampler-kernel)
            (cl-release-program sampler-program)
            (funcall reducer-cleanup)
            (cl-release-mem-object nbuf)
            (cl-release-mem-object lohibuf)
            (when (not (zerop nparams))
              (cl-release-mem-object parambuf))
            (when samplebuf
              (cl-release-mem-object samplebuf)))))
    ;; setup fixed kernel arguments
    (cl-set-kernel-arg sampler-kernel 0
                       :value nbuf)
    (cl-set-kernel-arg sampler-kernel 1
                       :value lohibuf)
    (cl-set-kernel-arg sampler-kernel 2
                       :value parambuf)
    (list integrator cleanup)))
