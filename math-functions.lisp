(in-package :cl-opencl-utils)

;;;; OpenCL C math

;;; Probability distributions
(defclcfun gaussian :double
    ((var x :double)
     (var amp :double)
     (var mu :double)
     (var sigma :double))
  (return
    (* (/ amp sigma)
       0.5 +1/sqrt2+ +2/sqrtpi+ ; 1/sqrt(2 pi)
       (exp (* -0.5 (expt (/ (- x mu) sigma) 2))))))

(defclcfun lorentz :double
    ((var x :double)
     (var mu :double)
     (var gamma :double))
  (return
    ;; note that due to typing issues, single-argument division
    ;; doesn't work in Lispified OpenCL C.
    (/ 1d0
       (* pi gamma
          (+ 1d0
             (/ (expt (- x mu) 2)
                gamma))))))
