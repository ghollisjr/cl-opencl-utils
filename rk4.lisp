(in-package :cl-opencl-utils)

(defun make-opencl-rk4 (queue dy/dx-kernel y-count x0
                        &key
                          (type :float))
  "Returns (stepper cleanup) where stepper is of the form (lambda (buf
&key x-delta) ...) and has the effect of stepping the differential
equation solution for y(x) forward by x-delta using the RK4 algorithm.
buf will be modified by this, as it will contain the estimated value
of y(x + x-delta).

x-delta is a sticky parameter that retains its value between calls
until changed.

fxy-kernel should be a kernel of the form

(kernel dy/dx-kernel
  ((var x (global (pointer type)))
   (var y (global (pointer type)))
   (var dy (global (pointer type))))
 ...)

which has the effect of setting dy to the value of dy/dx given x and
y.  x will always be a single value, but there are y-count elements
available in y.

It is also possible to pass the symbol name of a kernel defined with
defclckernel as the argument to dy/dx-kernel.

Note that dy/dx-kernel does not need to check (get-global-id 0) to see
if it is below y-count, as this is automatically checked before the
kernel is called."
  (let* ((context (cl-get-command-queue-info
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
         (bufsize (cl-get-mem-object-info (first kbufs)
                                          +CL-MEM-SIZE+))
         (x x0)
         (xbuf (cl-create-buffer context
                                 :type type
                                 :data (list x0)))
         (callername (string (gensym "RK4CALLER")))
         (dx/dy-kernel-name (if (listp dy/dx-kernel)
                                (second dy/dx-kernel)
                                dy/dx-kernel))
         (caller-source
          `(kernel ,callername
                   ((var x (global (pointer ,type)))
                    (var y (global (pointer ,type)))
                    (var dy (global (pointer ,type))))
                   (var gid (const :ulong)
                        (get-global-id 0))
                   (var ycount (const :ulong)
                        ,y-count)
                   (when (< gid ycount)
                     (,dx/dy-kernel-name x y dy))))
         (kernel-source
          (if (listp dy/dx-kernel)
              `(concat
                ,dy/dx-kernel
                ,caller-source)
              caller-source))
         (program
          (let* ((p (cl-create-program-with-source
                     context
                     (program-source-from-forms-fn kernel-source))))
            (cl-build-program-with-log p (list dev))
            p))
         ;; kernel to compute dy/dx(x,y)
         (kernel (cl-create-kernel program callername))
         (events NIL))
    (destructuring-bind (axpy axpy-cleanup)
        (make-opencl-axpy queue :type type)
      (labels ((eventcleanup ()
                 (mapcar #'release-opencl-event events)
                 (setf events NIL))
               (stepper (buf &key x-delta)
                 (let* ((h x-delta)
                        (h2 (/ x-delta 2d0))
                        (h6 (/ x-delta 6d0)))
                   ;; copy y into kbufs
                   (loop
                      for kbuf in kbufs
                      do (push (cl-enqueue-copy-buffer
                                queue buf kbuf bufsize)
                               events))
                   ;; k1 = dy/dx(x,y)
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list x))
                         events)
                   (cl-set-kernel-arg kernel 1 :value buf)
                   (cl-set-kernel-arg kernel 2 :value (first kbufs))
                   (push (cl-enqueue-kernel queue kernel y-count)
                         events)
                   ;; k2 = dy/dx(x + h/2, y + k1*h/2)
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list (+ x h2)))
                         events)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize)
                         events)
                   (push (funcall axpy
                                  h2
                                  (first kbufs)
                                  tmpbuf)
                         events)
                   (cl-set-kernel-arg kernel 1 :value tmpbuf)
                   (cl-set-kernel-arg kernel 2 :value (second kbufs))
                   (push (cl-enqueue-kernel queue kernel y-count)
                         events)
                   ;; k3 = dy/dx(x + h/2, y + k2*h/2)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize)
                         events)
                   (push (funcall axpy
                                  h2
                                  (second kbufs)
                                  tmpbuf)
                         events)
                   (cl-set-kernel-arg kernel 2 :value (third kbufs))
                   (push (cl-enqueue-kernel queue kernel y-count)
                         events)
                   ;; k4
                   (push (cl-enqueue-write-buffer
                          queue xbuf type (list (+ x h)))
                         events)
                   (push (cl-enqueue-copy-buffer
                          queue buf tmpbuf bufsize)
                         events)
                   (push (funcall axpy
                                  h
                                  (third kbufs)
                                  tmpbuf)
                         events)
                   (cl-set-kernel-arg kernel 2 :value (fourth kbufs))
                   (push (cl-enqueue-kernel queue kernel y-count)
                         events)
                   ;; update x
                   (incf x h)
                   ;; y + h6*(k1 + 2*k2 + 2*k3 + k4)
                   (setf events
                         (list* (funcall axpy
                                         h6
                                         (first kbufs)
                                         buf)
                                (funcall axpy
                                         (* 2d0 h6)
                                         (second kbufs)
                                         buf)
                                (funcall axpy
                                         (* 2d0 h6)
                                         (third kbufs)
                                         buf)
                                events))
                   (list (funcall axpy
                                  h6
                                  (fourth kbufs)
                                  buf)
                         #'eventcleanup)))
               (cleanup ()
                 (funcall axpy-cleanup)
                 (mapcar #'release-opencl-event
                         events)
                 (setf events NIL)
                 (mapcar #'cl-release-mem-object
                         (list* xbuf tmpbuf kbufs))
                 (cl-release-kernel kernel)
                 (cl-release-program program)))
        (cl-set-kernel-arg kernel 0 :value xbuf)
        (list #'stepper #'cleanup)))))
