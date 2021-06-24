;;;; These examples makes use of cl-opencl, cl-opencl-utils, cl-cerf,
;;;; and cl-ana to numerically convolute functions and compare them
;;;; with mathematically known results.
;;;;
;;;; cl-ana is found here: https://www.github.com/ghollisjr/cl-ana

(require 'cl-ana)
(require 'cl-opencl-utils)
(require 'cl-cerf)
(defpackage #:convolution-example
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils:use-package-group :cl-ana :convolution-example)
(in-package :convolution-example)

;;;; Example 1: Two Gaussians.

;; This is already defined in math.lisp
;; (defclcfun gaussian :double
;;     ((var x :double)
;;      (var amp :double)
;;      (var mu :double)
;;      (var sigma :double))
;;   (return
;;     (* (/ amp sigma)
;;        0.5 +1/sqrt2+ +2/sqrtpi+ ; 1/sqrt(2 pi)
;;        (exp (* -0.5 (expt (/ (- x mu) sigma) 2))))))

(defun convolution-example-1 (&optional (ndomain 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+)))
         (gauss-expr
          ;; note that gaussian has been shadowed by cl-ana
          (opencl-function-expr 'cl-opencl-utils:gaussian)))
    (with-opencl-context
        (context plat (list dev))
      (with-opencl-command-queue
          (queue context dev
                 :properties
                 (list +CL-QUEUE-OUT-OF-ORDER-EXEC-MODE-ENABLE+))
        (with-opencl-cleanup
            (convolutor
             (make-opencl-convolutor queue
                                     gauss-expr
                                     gauss-expr
                                     (cons -3d0 3d0)
                                     :nparams-A 3
                                     :nparams-B 3
                                     :type :double
                                     :ndomain ndomain))
          (draw
           (page (list
                  (plot2d (list
                           (line (lambda (x)
                                   (funcall convolutor
                                            x
                                            :params-A (list 1d0 0d0 1d0)
                                            :params-B (list 1d0 0d0 1d0)))
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
                                 :title "Mathematically-Convoluted Gaussians")))))))))))

(defun convolution-example-1-no-draw (&optional
                                        (ndomain 1000)
                                        (nsamples 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+)))
         (gauss-expr
          (opencl-function-expr 'cl-opencl-utils:gaussian)))

    (with-opencl-context
        (context plat (list dev))
      (with-opencl-command-queue
          (queue context dev)

        (with-opencl-cleanup
            (convolutor
             (make-opencl-convolutor queue
                                     gauss-expr
                                     gauss-expr
                                     (cons -3d0 3d0)
                                     :nparams-A 3
                                     :nparams-B 3
                                     :type :double
                                     :ndomain ndomain))
          (let* ((result
                  (sample-function (lambda (x)
                                     (funcall convolutor
                                              x
                                              :params-A (list 1d0 0d0 1d0)
                                              :params-B (list 1d0 0d0 1d0)))
                                   -3d0 3d0 nsamples)))
            result))))))

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
(defun slow-convolution-example-1 (&optional (ndomain 1000))
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
;; (time (convolution-example-1)) (time (slow-convolution-example-1))
;;
;; on my system reveals:
;;
;; 4.9x average speedup for ndomain=1000,
;; 44.3x average speedup for ndomain=10000
;;
;; with the OpenCL overhead included.

;;;; Example 2: Voigt profile (convolution of Lorentz and Gaussian)
;; Lorentz distribution:
(defun lorentz (x mu gamma)
  (/ (* pi gamma
        (+ 1d0
           (/ (expt (- x mu) 2)
              gamma)))))

;; Defined in math.lisp
;; (defclcfun lorentz :double
;;     ((var x :double)
;;      (var mu :double)
;;      (var gamma :double))
;;   (return
;;     ;; note that due to typing issues, single-argument division
;;     ;; doesn't work in Lispified OpenCL C.
;;     (/ 1d0
;;        (* pi gamma
;;           (+ 1d0
;;              (/ (expt (- x mu) 2)
;;                 gamma))))))

(defun convolution-example-2 (&optional (ndomain 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context
        (context plat (list dev))
      (with-opencl-command-queue
          (queue context dev)
        (with-opencl-cleanup
            (convolutor
             (make-opencl-convolutor queue
                                     (opencl-function-expr 'cl-opencl-utils:gaussian)
                                     (opencl-function-expr 'cl-opencl-utils:lorentz)
                                     (cons -7d0 7d0)
                                     :type :double
                                     :nparams-A 3
                                     :nparams-B 2
                                     :ndomain ndomain))
          (draw
           (page (list
                  (plot2d (list
                           (line (lambda (x)
                                   (funcall convolutor
                                            x
                                            :params-A (list 1d0 0d0 1d0)
                                            :params-B (list 0d0 1d0)))
                                 :title "OpenCL-Convoluted Voigt"
                                 :sampling (list :low -5d0
                                                 :high 5d0
                                                 :nsamples 1000))
                           (line (lambda (x)
                                   (cl-cerf:voigt x 1d0 1d0))
                                 :title "Mathematically-Convoluted Voigt"
                                 :sampling (list :low -5d0
                                                 :high 5d0
                                                 :nsamples 1000))))))))))))

(defun convolution-example-2-no-draw (&optional (ndomain 1000))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context
        (context plat (list dev))
      (with-opencl-command-queue
          (queue context dev)
        (with-opencl-cleanup
            (convolutor
             (make-opencl-convolutor queue
                                     (opencl-function-expr 'cl-opencl-utils:gaussian)
                                     (opencl-function-expr 'cl-opencl-utils:lorentz)
                                     (cons -7d0 7d0)
                                     :type :double
                                     :nparams-A 3
                                     :nparams-B 2
                                     :ndomain ndomain))
          (let* ((result
                  (sample-function
                   (lambda (x)
                     (funcall convolutor
                              x
                              :params-A (list 1d0 0d0 1d0)
                              :params-B (list 0d0 1d0)))
                   -3d0 3d0 1000)))
            result))))))
