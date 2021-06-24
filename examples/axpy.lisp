(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;;; Example for the axpy operation from BLAS
;;;;
;;;; To turn this into a test:
;;;;
;;;; (every #'identity (apply #'map 'list #'equal (axpy-example)))
(defun axpy-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+)))
         ;; manual context and queue management for demonstration
         (context
          (cl-create-context plat (list dev)))
         (queue
          (cl-create-command-queue context dev))
         (data (loop
                  for i from 1 to 100
                  collecting (float i 1d0)))
         (ndata (length data))
         (a 2d0)
         (xbuf (cl-create-buffer context
                                 :type :double
                                 :data data))
         (ybuf (cl-create-buffer context
                                 :type :double
                                 :data data)))
    (with-opencl-cleanup (axpy
                          (make-opencl-axpy queue :type :double))
      (let* ((result
              (first
               (last
                (cl-wait-and-release-events
                 (list (funcall axpy a xbuf ybuf)
                       (cl-enqueue-read-buffer
                        queue ybuf :double ndata)))))))
        (mapcar #'cl-release-mem-object
                (list xbuf ybuf))
        (cl-release-command-queue queue)
        (cl-release-context context)
        (list
         ;; Lisp results
         (map 'vector
              (lambda (x y)
                (+ (* a x)
                   y))
              data data)
         ;; OpenCL results
         result)))))
