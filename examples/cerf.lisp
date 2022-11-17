(require 'cl-opencl-utils)
(require 'cl-cerf)
(require 'cl-ana)
(defpackage #:cerf-example
  (:use :cl
   :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils::use-package-group :cl-ana :cerf-example)
(in-package :cerf-example)

(defun cerf-example (&key
                       (nsamples 1000)
                       (xmin -3d0)
                       (xmax 3d0))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (with-opencl-cleanup (sampler
                              (make-opencl-function-sampler
                               queue
                               (lambda (x)
                                 `(cdawson (complex ,x ,x)))
                               :domain-type :double
                               :range-type '(:struct cl_complex)))
          (let* ((samplebuf
                   (cl-create-buffer context
                                     :type '(:struct cl_complex)
                                     :count nsamples))
                 (samples
                   (map
                    'list #'identity
                    (first
                     (last
                      (cl-wait-and-release-events
                       (list (funcall sampler
                                      samplebuf
                                      :nsamples nsamples
                                      :low xmin
                                      :high xmax)
                             (cl-enqueue-read-buffer queue
                                                     samplebuf
                                                     '(:struct cl_complex)
                                                     nsamples)))))))
                 (reals (mapcar #'realpart samples))
                 (imags (mapcar #'imagpart samples))
                 (lisp-samples
                   (cdrs (sample-function
                          (lambda (x)
                            (cl-cerf:dawson (complex x x)))
                          xmin xmax nsamples)))
                 (lisp-reals (mapcar #'realpart lisp-samples))
                 (lisp-imags (mapcar #'imagpart lisp-samples)))
            (cl-release-mem-object samplebuf)
            (draw
             (page (list
                    (plot2d (list
                             ;; (line (zip xs reals)
                             ;;       :title "real"
                             ;;       :style "lines")
                             ;; (line (zip xs imags)
                             ;;       :title "imag"
                             ;;       :style "lines")
                             (line (zip reals imags)
                                   :title "joined"
                                   :style "lines")
                             (line (zip lisp-reals lisp-imags)
                                   :title "lisp joined"
                                   :style "lines"))))))))))))

(defun cerf-example-2 (&key
                         (nsamples 1000)
                         (xmin -3d0)
                         (xmax 3d0))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (with-opencl-cleanup (sampler
                              (make-opencl-function-sampler
                               queue
                               (lambda (x)
                                 `(erfcx ,x))
                               :domain-type :double
                               :range-type :double))
          (let* ((samplebuf
                   (cl-create-buffer context
                                     :type :double
                                     :count nsamples))
                 (xs (cdrs
                      (sample-function #'identity
                                       xmin
                                       xmax
                                       nsamples)))
                 (samples
                   (map
                    'list #'identity
                    (first
                     (last
                      (cl-wait-and-release-events
                       (list (funcall sampler
                                      samplebuf
                                      :nsamples nsamples
                                      :low xmin
                                      :high xmax)
                             (cl-enqueue-read-buffer queue
                                                     samplebuf
                                                     :double
                                                     nsamples)))))))
                 (reals samples)
                 (imags xs)
                 (lisp-samples
                   (cdrs (sample-function
                          (lambda (x)
                            (cl-cerf:erfcx x))
                          xmin xmax nsamples)))
                 (lisp-reals lisp-samples)
                 (lisp-imags xs))
            (cl-release-mem-object samplebuf)
            (draw
             (page (list
                    (plot2d (list
                             ;; (line (zip xs reals)
                             ;;       :title "real"
                             ;;       :style "lines")
                             ;; (line (zip xs imags)
                             ;;       :title "imag"
                             ;;       :style "lines")
                             (line (zip reals imags)
                                   :title "joined"
                                   :style "lines")
                             (line (zip lisp-reals lisp-imags)
                                   :title "lisp joined"
                                   :style "lines"))))))))))))
