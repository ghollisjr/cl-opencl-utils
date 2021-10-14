(in-package :cl-opencl-utils)

(defun make-opencl-rk4 (queue dy/dx-kernels y-count
                        &key
                          x0
                          (nparams 0)
                          (type :float))
  "Returns (stepper cleanup) where stepper is of the form (lambda (buf
&key x-delta event-wait-list params x0) ...) and has the effect of stepping the differential
equation solution for y(x) forward by x-delta using the RK4 algorithm.
buf will be modified by this, as it will contain the estimated value
of y(x + x-delta).

params is an optional argument supplying values to be placed in the
last argument to the kernel, either as a list of data to transfer to
the device or a buffer ID for data already on the device.  If
parameters will be supplied, set nparams to the total number of
parameters required.  It is up to the user to interpret the parameters
and make use of them in the various kernels supplied as arguments.

x-delta is a sticky parameter that retains its value between calls
until changed.

x0 when set to non-NIL will manually set the x0 value before starting
another iteration of the RK4 algorithm.  This is useful for reusing an
existing RK4 stepper starting at a different place on the x-axis but
for the same differential equation.  Make sure to set the values of
the supplied buffer to the proper initial conditions prior to calling
the stepper function.

The x0 argument supplied to make-opencl-rk4 is automatically set to
either 0f0 or 0d0 depending on the value of type.

Each element of dy/dx-kernels should the symbol name of a kernel or a
kernel of the form

(kernel dy/dx-kernel
  ((var x (global (pointer type)))
   (var y (global (pointer type)))
   (var dy (global (pointer type)))
   (var params (global (pointer type))))
 ...)

which has the effect of setting dy to the value of dy/dx given x and
y.  x will always be a single value, but there are y-count elements
available in y.

Note that each dy/dx-kernel does not need to check (get-global-id 0)
to see if it is below y-count, as this is automatically checked before
the kernel is called."
  (let* ((x0 (cond
               (x0 x0)
               ((eq type :float) 0f0)
               (t 0d0)))
         (context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (tmpbuf (cl-create-buffer context
                                   :type type
                                   :count y-count))
         (kbufs (loop
                   for i below 4
                   collecting
                     (cl-create-buffer context
                                       :type type
                                       :count y-count)))
         (pbuf (when (plusp nparams)
                 (cl-create-buffer context
                                   :type type
                                   :count nparams)))
         (bufsize (cl-get-mem-object-info (first kbufs)
                                          +CL-MEM-SIZE+))
         (x x0)
         (xbuf (cl-create-buffer context
                                 :type type
                                 :data (list x0)))
         (callernames
          (loop
             for k in dy/dx-kernels
             collecting (string (gensym "RK4CALLER"))))
         (dy/dx-kernel-names
          (loop
             for dy/dx-kernel in dy/dx-kernels
             collecting (if (listp dy/dx-kernel)
                            (second dy/dx-kernel)
                            dy/dx-kernel)))
         (caller-sources
          (loop
             for dy/dx-kernel-name in dy/dx-kernel-names
             for callername in callernames
             collecting
               `(kernel ,callername
                        ((var x (global (pointer ,type)))
                         (var y (global (pointer ,type)))
                         (var dy (global (pointer ,type)))
                         (var params (global (pointer ,type))))
                        (var gid (const :ulong)
                             (get-global-id 0))
                        (var ycount (const :ulong)
                             ,y-count)
                        (when (< gid ycount)
                          (,dy/dx-kernel-name x y dy params)))))
         (kernel-source
          `(concat
            ,@(loop
                 for dy/dx-kernel in dy/dx-kernels
                 for caller-source in caller-sources
                 collecting
                   (if (listp dy/dx-kernel)
                       `(concat
                         ,dy/dx-kernel
                         ,caller-source)
                       caller-source))))
         (program
          (let* ((p (cl-create-program-with-source
                     context
                     (program-source-from-forms-fn kernel-source))))
            (cl-build-program-with-log p (list dev))
            p))
         ;; kernel to compute dy/dx(x,y)
         (kernels
          (loop
             for callername in callernames
             collecting (cl-create-kernel program callername)))
         (events NIL)
         (lastparams NIL))
    (destructuring-bind (axpy axpy-cleanup)
        (make-opencl-axpy queue :type type)
      (labels ((eventcleanup ()
                 (mapcar #'release-opencl-event events)
                 (setf events NIL))
               (stepper (buf &key x-delta event-wait-list params x0)
                 ;; possibly adjust x
                 (when x0
                   (push (cl-enqueue-write-buffer
                          queue
                          xbuf
                          type
                          (list x0)
                          :event-wait-list event-wait-list)
                         events))
                 (let* ((h x-delta)
                        (h2 (if (eq type :float)
                                (/ x-delta 2f0)
                                (/ x-delta 2d0)))
                        (h6 (if (eq type :float)
                                (/ x-delta 6f0)
                                (/ x-delta 6d0))))
                   ;; optionally adjust parameters
                   (when (and params
                              (not (equal params lastparams)))
                     (if (atom params)
                         ;; buffer supplied
                         (loop
                            for kernel in kernels
                            do
                              (cl-set-kernel-arg kernel 3 :value params))
                         ;; data list supplied
                         (progn
                           (loop
                              for kernel in kernels
                              do
                                (cl-set-kernel-arg kernel 3 :value pbuf))
                           (push (cl-enqueue-write-buffer
                                  queue pbuf type
                                  (setf lastparams params)
                                  :event-wait-list event-wait-list)
                                 events))))
                   ;; copy y into kbufs
                   (loop
                      for kbuf in kbufs
                      do (push (cl-enqueue-copy-buffer
                                queue buf kbuf bufsize
                                :event-wait-list event-wait-list)
                               events))
                   ;; k1 = dy/dx(x,y)
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list x)
                          :event-wait-list event-wait-list)
                         events)
                   (let* ((evs (copy-list events)))
                     (loop
                        for kernel in kernels
                        do 
                          (cl-set-kernel-arg kernel 1 :value buf)
                          (cl-set-kernel-arg kernel 2 :value (first kbufs))
                          (push (cl-enqueue-kernel queue kernel y-count
                                                   :event-wait-list
                                                   (append evs event-wait-list))
                                events)))
                   ;; k2 = dy/dx(x + h/2, y + k1*h/2)
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list (+ x h2))
                          :event-wait-list (append events event-wait-list))
                         events)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize
                          :event-wait-list (append (subseq events 1)
                                                   event-wait-list))
                         events)
                   (push (funcall axpy
                                  h2
                                  (first kbufs)
                                  tmpbuf
                                  :event-wait-list
                                  (append events event-wait-list))
                         events)
                   (let* ((evs (copy-list events)))
                     (loop
                        for kernel in kernels
                        do 
                          (cl-set-kernel-arg kernel 1 :value tmpbuf)
                          (cl-set-kernel-arg kernel 2 :value (second kbufs))
                          (push (cl-enqueue-kernel queue kernel y-count
                                                   :event-wait-list
                                                   (append evs
                                                           event-wait-list))
                                events)))
                   ;; k3 = dy/dx(x + h/2, y + k2*h/2)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize
                          :event-wait-list
                          (append events event-wait-list))
                         events)
                   (push (funcall axpy
                                  h2
                                  (second kbufs)
                                  tmpbuf
                                  :event-wait-list
                                  (append events event-wait-list))
                         events)
                   (let* ((evs (copy-list events)))
                     (loop
                        for kernel in kernels
                        do 
                          (cl-set-kernel-arg kernel 2 :value (third kbufs))
                          (push (cl-enqueue-kernel queue kernel y-count
                                                   :event-wait-list
                                                   (append evs
                                                           event-wait-list))
                                events)))
                   ;; k4
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list (+ x h))
                          :event-wait-list
                          (append events event-wait-list))
                         events)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize
                          :event-wait-list
                          (append events event-wait-list))
                         events)
                   (push (funcall axpy
                                  h
                                  (third kbufs)
                                  tmpbuf
                                  :event-wait-list
                                  (append events
                                          event-wait-list))
                         events)
                   (let* ((evs (copy-list events)))
                     (loop
                        for kernel in kernels
                        do 
                          (cl-set-kernel-arg kernel 2 :value (fourth kbufs))
                          (push (cl-enqueue-kernel queue kernel y-count
                                                   :event-wait-list
                                                   (append evs event-wait-list))
                                events)))
                   ;; update x
                   (incf x h)
                   ;; y + h6*(k1 + 2*k2 + 2*k3 + k4)
                   (push
                    (funcall axpy
                             h6
                             (first kbufs)
                             buf
                             :event-wait-list
                             (append events event-wait-list))
                    events)
                   (push
                    (funcall axpy
                             (* (if (eq type :float)
                                    2f0
                                    2d0)
                                h6)
                             (second kbufs)
                             buf
                             :event-wait-list
                             (append events event-wait-list))
                    events)
                   (push
                    (funcall axpy
                             (* (if (eq type :float)
                                    2f0
                                    2d0)
                                h6)
                             (third kbufs)
                             buf
                             :event-wait-list
                             (append events event-wait-list))
                    events)
                   (destructuring-bind (axpyevent axpycleanup)
                       (funcall axpy
                                h6
                                (fourth kbufs)
                                buf
                                :event-wait-list
                                (append events event-wait-list))
                     (list axpyevent
                           (lambda ()
                             (funcall axpycleanup)
                             (eventcleanup))))))
               (cleanup ()
                 (funcall axpy-cleanup)
                 (when events
                   (eventcleanup))
                 (mapcar #'cl-release-mem-object
                         (list* xbuf tmpbuf kbufs))
                 (when pbuf
                   (cl-release-mem-object pbuf))
                 (loop
                    for kernel in kernels
                    do 
                      (cl-release-kernel kernel))
                 (cl-release-program program)))
        (loop
           for kernel in kernels
           do
             (cl-set-kernel-arg kernel 0 :value xbuf)
             (cl-set-kernel-arg kernel 3 :value pbuf))
        (list #'stepper #'cleanup)))))
