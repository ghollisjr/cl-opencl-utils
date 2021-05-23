(require 'cl-ana)
(require 'cl-opencl)
(require 'cl-opencl-utils)
(defpackage #:spline-example
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils:use-package-group :cl-ana :spline-example)
(in-package :spline-example)

;;;; Example of computing splines in OpenCL C
(defun spline-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev))
         (points (list (cons 1d0 1d0)
                       (cons 2d0 1.5d0)
                       (cons 3d0 2d0)
                       (cons 4d0 -1d0)
                       (cons 5d0 2d0)))
         (spline (second
                  (multiple-value-list
                   (polynomial-spline
                    points))))
         (nsamples 1000)
         (type :double))
    (destructuring-bind (sampler cleanup)
        (make-opencl-function-sampler
         queue
         (lambda (x)
           `(testspline ,x))
         :preamble
         (make-opencl-spline-form
          'testspline
          (polynomial-spline-xs spline)
          (polynomial-spline-coefs spline)
          :type type)
         :output-type type)
      (let* ((outbuf (cl-create-buffer context
                                       :type type
                                       :count nsamples))
             (samples
              (first
               (last
                (cl-wait-and-release-events
                 (list (funcall sampler outbuf
                                :low 1d0
                                :high 5d0
                                :nsamples nsamples)
                       (cl-enqueue-read-buffer queue
                                               outbuf
                                               type
                                               nsamples)))))))
        (setf samples (coerce samples 'list))
        (funcall cleanup)
        (cl-release-mem-object outbuf)
        (draw
         (page (list
                (plot2d (list
                         (line points
                               :title "data"
                               :style "points"
                               :color "red"
                               :point-size 2)
                         (line (zip (range 1d0 5d0
                                           (/ (- 5d0 1d0) (1- nsamples)))
                                    samples)
                               :color "black"
                               :title "OpenCL spline"
                               :style "lines"))))))))))
