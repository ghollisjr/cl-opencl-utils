(require 'cl-opencl)
(require 'cl-opencl-utils)
(require 'cl-ana)

(defpackage #:integration-example
  (:use
   :cl
   :cl-opencl
   :cl-opencl-utils))
(cl-ana.package-utils:use-package-group :cl-ana :integration-example)

(in-package :integration-example)

(defun integration-example (&optional (ndomain 1000) (mode :simpson))
  "Plot ln(x) in multiple ways"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev
                                         :properties
                                         (list +CL-QUEUE-OUT-OF-ORDER-EXEC-MODE-ENABLE+))))
    (destructuring-bind (integrator cleanup)
        (make-opencl-integrator queue
                                (lambda (x)
                                  `(/ 1d0 ,x))
                                :type :double
                                :mode mode)
      (let* ((ln (lambda (x)
                   (funcall integrator 1d0 x :ndomain ndomain))))
        (draw
         (page (list
                (plot2d (list
                         (line ln
                               :sampling (list :low 1d0
                                               :high 10d0
                                               :nsamples 1000)
                               :title "OpenCL integrated log")
                         (line (lambda (x)
                                 (gsll:integration-qag (lambda (y)
                                                         (/ 1d0 y))
                                                       1d0
                                                       x
                                                       :gauss15))
                               :sampling (list :low 1d0
                                               :high 10d0
                                               :nsamples 1000)
                               :title "GSL integrated ln(x)")
                         (line #'log
                               :sampling (list :low 1d0
                                               :high 10d0
                                               :nsamples 1000)
                               :title "Lisp ln(x)")))))))
      (funcall cleanup)
      (cl-release-command-queue queue)
      (cl-release-context context))))

(defun integration-example-no-draw (&optional (ndomain 1000) (mode :simpson))
  "Plot ln(x) in multiple ways"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev)))
    (destructuring-bind (integrator cleanup)
        (make-opencl-integrator queue
                                (lambda (x)
                                  `(/ 1d0 ,x))
                                :type :double
                                :mode mode
                                )
      (let* ((gslln (lambda (x)
                      (gsll:integration-qag (lambda (y)
                                              (/ 1d0 y))
                                            1d0
                                            x
                                            :gauss15)))
             (ln (lambda (x)
                   (funcall integrator 1d0 x :ndomain ndomain))))
        (time (sample-function ln 1d0 10d0 1000))
        (time (sample-function gslln
                               1d0 10d0 1000))
        (print (funcall ln 2d0))
        (print (funcall gslln 2d0)))
      (funcall cleanup)
      (cl-release-command-queue queue)
      (cl-release-context context))))

;;;; Example with complex numbers:
(defun integration-complex-example (&optional (ndomain 1000) (mode :simpson))
  "Plot ln(x) in multiple ways"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev
                                         :properties
                                         (list +CL-QUEUE-OUT-OF-ORDER-EXEC-MODE-ENABLE+))))
    (destructuring-bind (integrator cleanup)
        (make-opencl-complex-integrator
         queue
         (lambda (x)
           `(complex ,x 1d0))
         :mode mode)
      (let* ((rfn (lambda (x)
                    (realpart (funcall integrator 1d0 x :ndomain ndomain))))
             (ifn (lambda (x)
                    (imagpart (funcall integrator 1d0 x :ndomain ndomain)))))
        (draw
         (page (list
                (plot2d (list
                         (line rfn
                               :sampling (list :low 1d0
                                               :high 10d0
                                               :nsamples 1000)
                               :title "real part")
                         (line ifn
                               :sampling (list :low 1d0
                                               :high 10d0
                                               :nsamples 1000)
                               :title "imaginary part")))))))
      (funcall cleanup)
      (cl-release-command-queue queue)
      (cl-release-context context))))
