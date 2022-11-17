(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

(defun reduce-example ()
  (let* ((plat
           (first (cl-get-platform-ids)))
         (dev
           (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue
          ;; This example uses out-of-order execution as a
          ;; demonstration
          (queue context dev
           :properties
           (list +CL-QUEUE-OUT-OF-ORDER-EXEC-MODE-ENABLE+))
        (with-opencl-cleanup
            (reducefn
             (make-opencl-reducer queue
                                  :float +OPENCL-ADD-REXPR+))
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
            (cl-release-mem-object buf)
            result))))))

(defun map-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context
        (context plat (list dev))
      (with-opencl-command-queue
          (queue context dev
           :properties (list +CL-QUEUE-OUT-OF-ORDER-EXEC-MODE-ENABLE+))
        (with-opencl-cleanup
            (logmap
             (make-opencl-mapper queue :float
                                 (lambda (x)
                                   `(log ,x))))
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
                             (length data))))))))
            (mapcar #'cl-release-mem-object
                    (list inbuf outbuf))
            result))))))

(defun mapreduce-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+)))
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

(defun multimap-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((xs
                 (loop
                   for i below 100
                   collecting (float i 1d0)))
               (ys
                 (loop
                   for i below 100
                   collecting (float (* i 2) 1d0)))
               (ndata (min (length xs)
                           (length ys)))
               (xbuf
                 (cl-create-buffer context
                                   :type :double
                                   :data xs))
               (ybuf
                 (cl-create-buffer context
                                   :type :double
                                   :data ys))
               (rbuf
                 (cl-create-buffer context
                                   :type :double
                                   :count ndata)))
          (with-opencl-cleanup
              (mapper
               (make-opencl-mapper queue
                                   (list :double :double)
                                   (lambda (x y)
                                     `(+ ,x ,y))))
            (let* ((result
                     (alexandria:last-elt
                      (cl-wait-and-release-events
                       (list (funcall mapper
                                      (list xbuf ybuf)
                                      rbuf)
                             (cl-enqueue-read-buffer
                              queue rbuf
                              :double ndata))))))
              (mapcar #'cl-release-mem-object
                      (list rbuf xbuf ybuf))
              result)))))))
