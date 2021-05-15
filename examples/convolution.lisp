;; This example makes use of cl-opencl, cl-opencl-utils, and cl-ana to
;; draw the convolution of 2 gaussians along with the mathematically
;; correct result for comparison.
;;
;; cl-ana is found here: https://www.github.com/ghollisjr/cl-ana

(require 'cl-ana)
(require 'cl-opencl-utils)
(defpackage #:convolution-example
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils:use-package-group :cl-ana :convolution-example)
(in-package :convolution-example)

(defclcfun :double gaussian
    ((var :double x)
     (var :double amp)
     (var :double mu)
     (var :double sigma))
  (return
    (* (/ amp sigma)
       0.5 +1/sqrt2+ +2/sqrtpi+ ; 1/sqrt(2 pi)
       (exp (* -0.5 (expt (/ (- x mu) sigma) 2))))))

(defun convolution-example (&optional (ndomain 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev))
         (gauss-expr
          (lambda (x amp mu sigma)
            `(gaussian ,x ,amp ,mu ,sigma))))
    (destructuring-bind (convolutor cleanup)
        (make-opencl-convolutor queue
                                gauss-expr
                                gauss-expr
                                (cons -3d0 3d0)
                                :nparams-A 3
                                :nparams-B 3
                                :type :double
                                :ndomain ndomain)
      (draw
       (page (list
              (plot2d (list
                       (line (lambda (x)
                               (funcall convolutor
                                        x
                                        :Aparams (list 1d0 0d0 1d0)
                                        :Bparams (list 1d0 0d0 1d0)))
                             :sampling (list :low -3d0
                                             :high 3d0
                                             :nsamples 1000)
                             :title "OpenCL-Convoluted Gaussians")
                       (line (alexandria:curry
                              #'gaussian
                              (list 1d0 0d0 (sqrt 2d0)))
                             :sampling (list :low -3d0
                                             :high 3d0
                                             :nsamples 1000)
                             :title "Mathematically-Convoluted Gaussians"))))))
      (funcall cleanup))))

;; For comparison: CPU convolution
;; Convolution function
(defun convolution (fnA fnB domain
                    &key
                      (step 1d-5))
  "Computes the convolution of fnA and fnB over the supplied domain,
which should be a cons pair.  Both fnA and fnB should be functions
mapping from numbers to numbers."
  (declare (cons domain))
  (destructuring-bind (low . high) domain
    (let* ((npoints (floor (/ (- high low) step))))
      (lambda (x)
        (loop
           for i below npoints
           for y = (+ (* 0.5d0 step) low) then (+ y step)
           summing (* step
                      (funcall fnA (- x y))
                      (funcall fnB y)))))))
(defun slow-convolution-example (&optional (ndomain 1000))
  (let* ((domain (cons -3.0 3.0))
         (step (/ (- (cdr domain) (car domain))
                  ndomain))
         (convolutor
          (convolution (alexandria:curry #'gaussian
                                         (list 1d0 0d0 1d0))
                       (alexandria:curry #'gaussian
                                         (list 1d0 0d0 1d0))
                       domain
                       :step step)))
    (draw
     (page (list
            (plot2d (list
                     (line convolutor
                           :sampling (list :low -3d0
                                           :high 3d0
                                           :nsamples 1000)
                           :title "CPU-Convoluted Gaussians")
                     (line (alexandria:curry
                            #'gaussian
                            (list 1d0 0d0 (sqrt 2d0)))
                           :sampling (list :low -3d0
                                           :high 3d0
                                           :nsamples 1000)
                           :title "Mathematically-Convoluted Gaussians"))))))))

;; Running
;;
;; (time (convolution-example)) (time (slow-convolution-example))
;;
;; on my system reveals:
;; 
;; 4.9x speedup for ndomain=1000,
;; 44.3x speedup for ndomain=10000
;;
;; with the OpenCL overhead included.
