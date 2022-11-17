(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

(defun floatsum (start end)
  (let* ((plat
           (first (cl-get-platform-ids)))
         (dev
           (first (cl-get-device-ids plat
                                     +CL-DEVICE-TYPE-GPU+)))
         (context
           (cl-create-context plat (list dev)))
         (queue
           (cl-create-command-queue context dev)))
    (destructuring-bind (clsum clsumcleanup)
        (make-opencl-reducer queue :double
                             +OPENCL-ADD-REXPR+)
      (let* ((databuf
               (cl-create-buffer context
                                 :flags +CL-MEM-WRITE-ONLY+
                                 :type :double
                                 :data
                                 (loop
                                   for i from start below end
                                   collecting (float i 1d0)))))
        (let* ((result
                 (first
                  (cl-wait-and-release-events
                   (list
                    (funcall clsum databuf))))))
          (funcall clsumcleanup)
          (cl-release-mem-object databuf)
          (cl-release-command-queue queue)
          (cl-release-context context)
          result)))))
