(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;;; RK4 algorithm example
;;;;
;;;; This example uses a vector-valued y(x) to solve two ODEs
;;;; simultaneously:
;;;;
;;;; 1. dy/dx = y (solution: y(x) = exp(x) + constant)
;;;; 2. dy/dx = x (solution: y(x) = x^2/2 + constant)
;;;;
;;;; You can use this RK4 solver to solve higher order systems of many
;;;; equations as well, and for efficiency it is recommended to define
;;;; and use kernels in such a way as to minimize the amount of
;;;; branching.  This example is not scalable due to the branch
;;;; statement, but is simple.  A better version of this example would
;;;; have two kernels, one for equation 1 and one for equation 2, with
;;;; no branching but rather a partitioned global ID space.  It is
;;;; with this in mind that the RK4 utility function was built to
;;;; support a list of kernels as input rather than a single kernel.
;;;; This makes it possible to easily batch-submit OpenCL jobs that
;;;; e.g. run simulations of heterogenous physical systems.

(defclckernel rk4kernel
    ((var x (global (pointer :double)))
     (var y (global (pointer :double)))
     (var dy (global (pointer :double)))
     (var params (global (pointer :double))))
  (var gid (const :ulong)
       (get-global-id 0))
  (case gid
    ;; 1. dy/dx = y
    (0 (setf (aref dy gid)
             (aref y gid))
       (break))
    ;; 2. dy/dx = x
    (1 (setf (aref dy gid)
             (value x))
       (break))))

(defun rk4-example (&optional (nsteps 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+)))
         (x0 0d0)
         (h (/ 5d0 nsteps))
         (y0 (list 1d0 0d0))
         (y1 (list (cons x0 (first y0))))
         (y2 (list (cons x0 (second y0)))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (with-opencl-cleanup
            (stepper
             (make-opencl-rk4 queue '(rk4kernel) 2 x0
                              :type :double))
          (let* ((ybuf (cl-create-buffer context
                                         :type :double
                                         :data y0)))
            (loop
               for i to nsteps
               for x = (+ x0 h) then (+ x h)
               do (let* ((nextys
                          (first
                           (last
                            (cl-wait-and-release-events
                             (list (funcall stepper
                                            ybuf
                                            :x-delta h)
                                   (cl-enqueue-read-buffer
                                    queue ybuf :double 2)))))))
                    (push (cons x (aref nextys 0))
                          y1)
                    (push (cons x (aref nextys 1))
                          y2)))
            (cl-release-mem-object ybuf)
            (list (nreverse y1)
                  (nreverse y2))))))))

;;;; A more involved example where coupled pendulums
;; (defclckernel coupled_pendulums_dydx
;;     ((var n 

;; (defun coupled-pendulums-example (&optional (npendulums 10))
  
