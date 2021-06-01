(require 'cl-opencl-utils)
(require 'cl-ana)
(defpackage #:cerf-example
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils::use-package-group :cl-ana :cerf-example)
(in-package :cerf-example)

(defun cerf-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (with-opencl-cleanup (sampler
                              (make-opencl-function-sampler
                               queue
                               (lambda (x)
                                 `(cerf (complex ,x ,x)))
                               :domain-type :double
                               :range-type '(:struct cl_complex)))
          (let* ((nsamples 1000)
                 (samplebuf
                  (cl-create-buffer context
                                    :type '(:struct cl_complex)
                                    :count nsamples))
                 (samples
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (funcall sampler
                                    :nsamples nsamples
                                    :low 0d0
                                    :high 5d0)
                           (cl-enqueue-read-buffer queue
                                                   samplebuf
                                                   '(:struct cl_complex)
                                                   nsamples)))))))
            (cl-ana::draw
             (cl-ana::page (list
                            (cl-ana::plot2d (list
                                             (cl-ana::line samples
                                                           :style "lines"))))))
            (cl-release-mem-object samplebuf)))))))
