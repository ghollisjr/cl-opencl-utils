(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

(defun complex-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((data
                ;; complex data can be sent directly to OpenCL
                (loop
                   for i below 1000
                   collecting (complex (float i 1d0)
                                       (float (* i 2) 1d0))))
               (ndata (length data))
               ;; just make sure to use '(:struct cl_complex) as the
               ;; CFFI type
               (inbuf
                (cl-create-buffer context
                                  :type '(:struct cl_complex)
                                  :data data))
               (outbuf
                (cl-create-buffer context
                                  :type :double
                                  :count ndata)))
          (with-opencl-cleanup (mapper
                                (make-opencl-mapper
                                 queue
                                 ;; here again make sure to use
                                 ;; '(:struct cl_complex) as the CFFI
                                 ;; type
                                 '(:struct cl_complex)
                                 (lambda (x)
                                   `(realpart
                                     (complex* ,x
                                               (complex/ ,x
                                                         (complex 1d0 1d0)))))
                                 :output-type :double))
            (let* ((result
                    (first
                     (last
                      (cl-wait-and-release-events
                       (list (funcall mapper
                                      inbuf
                                      outbuf)
                             (cl-enqueue-read-buffer queue outbuf
                                                     :double ndata)))))))
              (mapcar #'cl-release-mem-object
                      (list inbuf outbuf))
              (list (map 'vector (lambda (c)
                                   (realpart
                                    (* c
                                       (/ c #c(1d0 1d0)))))
                         data)
                    result))))))))
