(require 'cl-opencl-utils)
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
    (destructuring-bind (reducefn reducecleanup)
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
        (funcall reducecleanup)
        (cl-release-mem-object buf)
        (cl-release-command-queue queue)
        (cl-release-context context)
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
    (destructuring-bind (logmap logmapcleanup)
        (make-opencl-mapper queue :float
                            (lambda (x)
                              `(log ,x)))
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
                        :float
                        (length data))))))
               ))
        (funcall logmapcleanup)
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
    (destructuring-bind (logmap logmapcleanup)
        (make-opencl-mapper queue :float
                            (lambda (x)
                              `(log ,x)))
      (destructuring-bind (reducefn reducecleanup)
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
          (funcall reducecleanup)
          (funcall logmapcleanup)
          (mapcar #'cl-release-mem-object
                  (list inbuf outbuf))
          (cl-release-command-queue queue)
          (cl-release-context context)
          result)))))
