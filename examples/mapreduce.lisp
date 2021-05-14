(in-package :cl-opencl-utils)

(defun reduce-example ()
  (let* ((plat
          (first (cl-get-platform-ids)))
         (dev
          (first (cl-get-device-ids plat +CL-DEVICE-TYPE-GPU+)))
         (context
          (cl-create-context plat (list dev)))
         (queue
          (cl-create-command-queue context dev)))
    (destructuring-bind (reducefn reduce-kernel reduce-program)
        (make-opencl-reducer queue :float +OPENCL-ADD-REXPR+)
      (let* ((data (loop
                      for i from 1 to 10000
                      collecting (float i)))
             (buf (cl-create-buffer context
                                    :type :float
                                    :data data))
             (result
              (first
               (cl-wait-and-release-events
                (list (funcall reducefn buf))))))
        (cl-release-kernel reduce-kernel)
        (cl-release-program reduce-program)
        (cl-release-mem-object buf)
        result))))

(defun map-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-GPU+)))
         (context
          (cl-create-context plat (list dev)))
         (queue
          (cl-create-command-queue context dev)))
    (destructuring-bind (logmap map-kernel map-program)
        (make-opencl-mapper queue :float
                            (lambda (x)
                              (format nil "log(~a)" x)))
      (let* ((data
              (loop
                 for i from 1 to 10000
                 collecting (float i)))
             (inbuf
              (cl-create-buffer context
                                :type :float
                                :data data))
             (outbuf
              (cl-create-buffer context
                                :type :float
                                :count (length data)))
             (result
              (first
               (last
                (cl-wait-and-release-events
                 (list (funcall logmap inbuf outbuf)
                       (cl-enqueue-read-buffer
                        queue outbuf
                        (list :array :float (length data)))))))))
        (cl-release-kernel map-kernel)
        (cl-release-program map-program)
        (mapcar #'cl-release-mem-object
                (list inbuf outbuf))
        (cl-release-command-queue queue)
        (cl-release-context context)
        result))))

(defun mapreduce-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-GPU+)))
         (context
          (cl-create-context plat (list dev)))
         (queue
          (cl-create-command-queue context dev)))
    (destructuring-bind (logmap logmap-kernel logmap-program)
        (make-opencl-mapper queue :float
                            (lambda (x)
                              (format nil "log(~a)" x)))
      (destructuring-bind (reducefn reduce-kernel reduce-program)
          (make-opencl-reducer queue :float +OPENCL-ADD-REXPR+)
        (let* ((data
                (loop
                   for i from 1 to 10000
                   collecting (float i)))
               (inbuf
                (cl-create-buffer context
                                  :type :float
                                  :data data))
               (outbuf
                (cl-create-buffer context
                                  :type :float
                                  :count (length data)))
               (result
                (first
                 (last
                  (cl-wait-and-release-events
                   (list (funcall logmap inbuf outbuf)
                         (funcall reducefn outbuf)))))))
          (cl-release-kernel reduce-kernel)
          (cl-release-program reduce-program)
          (cl-release-kernel logmap-kernel)
          (cl-release-program logmap-program)
          (mapcar #'cl-release-mem-object
                  (list inbuf outbuf))
          (cl-release-command-queue queue)
          (cl-release-context context)
          result)))))
