(in-package :cl-opencl-utils)

(defclc + (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^+~})" (mapcar #'clc forms))))

(defclc - (&rest forms)
  (with-output-to-string (out)
    (if (single forms)
        (format out "(-~a)" (clc (first forms)))
        (format out "(~{(~a)~^-~})" (mapcar #'clc forms)))))

(defclc * (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^*~})" (mapcar #'clc forms))))

;; Note: Not valid for single argument, only works with two or more arguments
(defclc / (&rest forms)
  (with-output-to-string (out)
    (format out "(~{~a~^/~})" (mapcar #'clc forms))))

(defclc incf (var &optional amount)
  (format nil "(++~a)" (clc var)))

(defclc decf (var)
  (format nil "(--~a)" (clc var)))

;;; modular arithmetic
(defclc mod (x divisor)
  (format nil "((~a) % (~a))"
          (clc x)
          (clc divisor)))

(defclc int-/ (num den)
  (clc `(/ (typecast int ,num)
           (typecast int ,den))))

(defclc long-/ (num den)
  (clc `(/ (typecast long ,num)
           (typecast long ,den))))

;; Bit operators
(defclc logior (&rest args)
  (with-output-to-string (out)
    (format out "(~{(~a)~^|~})" (mapcar #'clc args))))

(defclc logand (&rest args)
  (with-output-to-string (out)
    (format out "(~{(~a)~^&~})" (mapcar #'clc args))))

(defclc logxor (&rest args)
  (with-output-to-string (out)
    (format out "(~{(~a)~^^~})" (mapcar #'clc args))))

(defclc lognot (arg)
  (with-output-to-string (out)
    (format out "(~~~a)" (clc arg))))

(defclc << (x y)
  (format nil "((~a) << (~a))"
          (clc x) (clc y)))

(defclc >> (x y)
  (format nil "((~a) >> (~a))"
          (clc x) (clc y)))

;; Inline conditional
(defclc ? (condition if else)
  (format nil "((~a) ? (~a) : (~a))"
          (clc condition)
          (clc if)
          (clc else)))

;; convenience
(defclc 1+ (x)
  (clc `(+ ,x 1)))
(defclc 1- (x)
  (clc `(- ,x 1)))

;;;; Comparison functions:

(macrolet ((defboolop (sym &optional str)
             `(defclc ,sym (&rest forms)
                (with-output-to-string (out)
                  (format out "(")
                  (loop
                     for fcons on forms
                     when (second fcons)
                     do (let ((left (first fcons))
                              (right (second fcons)))
                          (format out "((~a) ~a (~a))"
                                  (clc left)
                                  ,(if str
                                       str
                                       (string-downcase (string sym)))
                                  (clc right))
                          (when (third fcons)
                            (format out " && "))))
                  (format out ")")))))
  (defboolop <)
  (defboolop <=)
  (defboolop >)
  (defboolop >=)
  (defboolop = "=="))

;;; Alias for the pow function:
(defclc expt (x y)
  (clc `(pow ,x ,y)))

;;; Mathematical constants:
;;;
;;; Many constants are available in the M_* style that aren't part of
;;; Lisp, but pi is available in Lisp, so it gets two cases for
;;; convenience.

(defclc pi ()
  "M_PI")

(defclc +maxfloat+ () "MAXFLOAT")
(defclc +huge-valf+ () "HUGE_VALF")
(defclc +infinity+ () "INFINITY")
(defclc +nan+ () "NAN")

;; double
(defclc +e+ ()
  "M_E")
(defclc +log2e+ ()
  "M_LOG2E")
(defclc +log10e+ ()
  "M_LOG10E")
(defclc +ln2+ ()
  "M_LN2")
(defclc +ln10+ ()
  "M_LN10")
(defclc +pi+ ()
  "M_PI")
(defclc +pi/2+ ()
  "M_PI_2")
(defclc +pi/4+ ()
  "M_PI_4")
(defclc +1/pi+ ()
  "M_1_PI")
(defclc +2/pi+ ()
  "M_2_PI")
(defclc +2/sqrtpi+ ()
  "M_2_SQRTPI")
(defclc +sqrt2+ ()
  "M_SQRT2")
(defclc +1/sqrt2+ ()
  "M_SQRT1_2")
;; single
(defclc +e-f+ ()
  "M_E_F")
(defclc +log2e-f+ ()
  "M_LOG2E_F")
(defclc +log10e-f+ ()
  "M_LOG10E_F")
(defclc +ln2-f+ ()
  "M_LN2_F")
(defclc +ln10-f+ ()
  "M_LN10_F")
(defclc +pi-f+ ()
  "M_PI_F")
(defclc +pi/2-f+ ()
  "M_PI_2_F")
(defclc +pi/4-f+ ()
  "M_PI_4_F")
(defclc +1/pi-f+ ()
  "M_1_PI_F")
(defclc +2/pi-f+ ()
  "M_2_PI_F")
(defclc +2/sqrtpi-f+ ()
  "M_2_SQRTPI_F")
(defclc +sqrt2-f+ ()
  "M_SQRT2_F")
(defclc +1/sqrt2-f+ ()
  "M_SQRT1_2_F")
;; half
(defclc +e-h+ ()
  "M_E_H")
(defclc +log2e-h+ ()
  "M_LOG2E_H")
(defclc +log10e-h+ ()
  "M_LOG10E_H")
(defclc +ln2-h+ ()
  "M_LN2_H")
(defclc +ln10-h+ ()
  "M_LN10_H")
(defclc +pi-h+ ()
  "M_PI_H")
(defclc +pi/2-h+ ()
  "M_PI_2_H")
(defclc +pi/4-h+ ()
  "M_PI_4_H")
(defclc +1/pi-h+ ()
  "M_1_PI_H")
(defclc +2/pi-h+ ()
  "M_2_PI_H")
(defclc +2/sqrtpi-h+ ()
  "M_2_SQRTPI_H")
(defclc +sqrt2-h+ ()
  "M_SQRT2_H")
(defclc +1/sqrt2-h+ ()
  "M_SQRT1_2_H")

;;;; OpenCL C math

;;; Complex numbers:
;;;
;;; Lisp side uses native complex types, OpenCL C side uses complex
;;; number library that unfortunately isn't directly defined by OpenCL
;;; C yet.  If that changes, this can change.

(defclcstruct-no-cffi cl_complex
  (:real :double)
  (:imag :double))

(defcstruct (cl_complex :class cl_complex)
  (:real :double)
  (:imag :double))

;; Allow CFFI to translate native complex types:
(defmethod translate-into-foreign-memory (z
                                          (type cl_complex)
                                          pointer)
  (setf (foreign-slot-value pointer '(:struct cl_complex) :real)
        (realpart z))
  (setf (foreign-slot-value pointer '(:struct cl_complex) :imag)
        (imagpart z)))

(defmethod translate-from-foreign (pointer (type cl_complex))
  (complex (foreign-slot-value pointer '(:struct cl_complex) :real)
           (foreign-slot-value pointer '(:struct cl_complex) :imag)))

;; Complex arithmetic
(defclcfun complex (:struct cl_complex)
    ((var real :double)
     (var imag :double))
  (var result (:struct cl_complex))
  (setf (member result :real) real)
  (setf (member result :imag) imag)
  (return result))

(defclcfun realpart :double
    ((var z (:struct cl_complex)))
  (return (member z :real)))

(defclcfun imagpart :double
    ((var z (:struct cl_complex)))
  (return (member z :imag)))

(defclcfun cabs :double
    ((var z (:struct cl_complex)))
  (var rp (const :double)
       (member z :real))
  (var ip (const :double)
       (member z :imag))
  (return (sqrt (+ (* rp rp)
                   (* ip ip)))))

(defclcfun cabs2 :double
    ((var z (:struct cl_complex)))
  (var rp (const :double)
       (member z :real))
  (var ip (const :double)
       (member z :imag))
  (return (+ (* rp rp)
             (* ip ip))))

(defclcmacro complex+ (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_add ,(rec (butlast xs))
                               ,(first (last xs)))
                 (first xs))))
    (rec args)))

(defclcmacro complex- (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_sub ,(rec (butlast xs))
                               ,(first (last xs)))
                 (first xs))))
    (rec args)))

(defclcfun complex_add (:struct cl_complex)
    ((var x (:struct cl_complex))
     (var y (:struct cl_complex)))
  (var result (:struct cl_complex))
  (setf (member result :real)
        (+ (member x :real)
           (member y :real)))
  (setf (member result :imag)
        (+ (member x :imag)
           (member y :imag)))
  (return result))

(defclcfun complex_sub (:struct cl_complex)
    ((var x (:struct cl_complex))
     (var y (:struct cl_complex)))
  (var result (:struct cl_complex))
  (setf (member result :real)
        (- (member x :real)
           (member y :real)))
  (setf (member result :imag)
        (- (member x :imag)
           (member y :imag)))
  (return result))

(defclcfun complex_mult (:struct cl_complex)
    ((var x (:struct cl_complex))
     (var y (:struct cl_complex)))
  (var result (:struct cl_complex))
  (setf (member result :real)
        (- (* (member x :real)
              (member y :real))
           (* (member x :imag)
              (member y :imag))))
  (setf (member result :imag)
        (+ (* (member x :real)
              (member y :imag))
           (* (member x :imag)
              (member y :real))))
  (return result))

(defclcmacro complex* (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_mult ,(rec (butlast xs))
                                ,(first (last xs)))
                 (first xs))))
    (rec args)))

(defclcfun complex_conj (:struct cl_complex)
    ((var z (:struct cl_complex)))
  (var result (:struct cl_complex))
  (setf (member result :real)
        (member z :real))
  (setf (member result :imag)
        (- (member z :imag)))
  (return result))

(defclcfun complex_div (:struct cl_complex)
    ((var x (:struct cl_complex))
     (var y (:struct cl_complex)))
  (var yabs2 :double
       (cabs2 y))
  (var result (:struct cl_complex)
       (complex* x
                 (complex_conj y)))
  (setf (member result :real)
        (/ (member result :real)
           yabs2))
  (setf (member result :imag)
        (/ (member result :imag)
           yabs2))
  (return result))

(defclcmacro complex/ (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_div ,(rec (butlast xs))
                               ,(first (last xs)))
                 (first xs))))
    (rec args)))

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

;; Modified from libcerf (https://sourceforge.net/projects/libcerf/)
;;
;; This is under the MIT license unless it can be promoted to GPL, and
;; I'm not sure whether that's the case.
;; (defclcfun voigt :double
;;     ((var x :double)
;;      (var sigma :double)
;;      (var gamma :double))
;;   ;; Joachim Wuttke, January 2013.
;;   ;;
;;   ;; Compute Voigt's convolution of a Gaussian
;;   ;;    G(x,sigma) = 1/sqrt(2*pi)/|sigma| * exp(-x^2/2/sigma^2)
;;   ;; and a Lorentzian
;;   ;;    L(x,gamma) = |gamma| / pi / ( x^2 + gamma^2 ),
;;   ;; namely
;;   ;;    voigt(x,sigma,gamma) =
;;   ;;          \int_{-infty}^{infty} dx' G(x',sigma) L(x-x',gamma)
;;   ;; using the relation
;;   ;;    voigt(x,sigma,gamma) = Re{ w(z) } / sqrt(2*pi) / |sigma|
;;   ;; with
;;   ;;    z = (x+i*|gamma|) / sqrt(2) / |sigma|.
;;   ;;
;;   ;; Reference: Abramowitz&Stegun (1964), formula (7.4.13).
;;   (var gam :double
;;        (? (< gamma 0)
;;           (- gamma)
;;           gamma))
;;   (var sig :double
;;        (? (< sigma 0)
;;           (- sigma)
;;           sigma))
;;   (if (zerop gam)
;;       (if (zerop sig)
;;           ;; It's kind of a delta function
;;           (return (? x 0 +infinity+))
;;           ;; It's a pure Gaussian
;;           (return
;;             (* (/ (* 0.5 +1/sqrt2+ +2/sqrtpi+) ; 1/sqrt(2 pi)
;;                   sigma)
;;                (exp (* -0.5 (expt (/ x sigma) 2))))))
;;       (if (zerop sig)
;;           ;; It's a pure Lorentzian
;;           (return (/ gam
;;                      (* pi
;;                         (+ (* x x)
;;                            (* gam gam)))))
