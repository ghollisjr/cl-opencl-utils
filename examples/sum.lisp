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
    (destructuring-bind (clsum clsumk clsump)
        (make-opencl-reducer queue :float
                             +OPENCL-ADD-REXPR+)
      (let* ((databuf
              (cl-create-buffer context
                                +CL-MEM-WRITE-ONLY+
                                :type :float
                                :data
                                (loop
                                   for i from start below end
                                   collecting (float i)))))
        (let* ((result
                (first
                 (cl-wait-and-release-events
                  (list 
                   (funcall clsum databuf))))))
          (cl-release-kernel clsumk)
          (cl-release-program clsump)
          (cl-release-mem-object databuf)
          result)))))
