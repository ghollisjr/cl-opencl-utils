(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

(defclcfun gaussian :double
    ((var x :double)
     (var amp :double)
     (var mu :double)
     (var sigma :double))
  (return
    (* (/ amp sigma)
       0.5 +1/sqrt2+ +2/sqrtpi+ ; 1/sqrt(2 pi)
       (exp (* -0.5 (expt (/ (- x mu) sigma) 2))))))

(defun function-sampling-example (&optional (nsamples 10))
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context (cl-create-context plat (list dev)))
         (queue (cl-create-command-queue context dev))
         (buf (cl-create-buffer context
                                :type :double
                                :count nsamples)))
    (destructuring-bind (sampler cleanup)
        (make-opencl-function-sampler
         queue
         (opencl-function-expr 'gaussian)
         :nparams 3
         :domain-type :double)
      (cl-wait-and-release-events
       (list (funcall sampler buf
                      :params (list 1d0 0d0 1d0)
                      :nsamples nsamples
                      :low 0d0
                      :high 3d0)))
      (funcall cleanup)
      (let* ((result
               (cl-enqueue-read-buffer queue buf
                                       :double nsamples
                                       :blocking-p t)))
        (cl-release-mem-object buf)
        (cl-release-command-queue queue)
        (cl-release-context context)
        result))))
