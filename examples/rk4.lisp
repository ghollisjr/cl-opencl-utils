(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;;; RK4 algorithm example

(defclckernel rk4kernel
    ((var x (global (pointer :double)))
     (var y (global (pointer :double)))
     (var dy (global (pointer :double)))
     (var params (global (pointer :double))))
  (var gid (const :ulong)
       (get-global-id 0))
  (case gid
    ;; dy/dy = y
    (0 (setf (aref dy gid)
             (aref y gid))
       (break))
    ;; dy/dx = x
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
  
