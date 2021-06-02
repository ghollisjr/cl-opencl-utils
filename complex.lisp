(in-package :cl-opencl-utils)

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

(defclcfun complex_polar (:struct cl_complex)
    ((var r :double)
     (var t :double))
  (if (and (zerop r)
           (not (isnan t)))
      (return (complex 0d0 0d0))
      (return (complex (* r (cos t))
                       (* r (sin t))))))

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

(defclcfun carg :double
    ((var z (:struct cl_complex)))
  (atan2 (member z :imag)
         (member z :real)))

(defclcmacro complex+ (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_add ,(rec (butlast xs))
                               ,(first (last xs)))
                 (first xs))))
    (rec args)))

(defclcmacro complex- (&rest args)
  (if (equal (length args) 1)
      `(complex_unary_sub ,(first args))
      (labels ((rec (xs)
                 (if (cdr xs)
                     `(complex_sub ,(rec (butlast xs))
                                   ,(first (last xs)))
                     (first xs))))
        (rec args))))

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

(defclcfun complex_unary_sub (:struct cl_complex)
    ((var z (:struct cl_complex)))
  (var result
       (:struct cl_complex))
  (setf (member result :real)
        (- (member z :real)))
  (setf (member result :imag)
        (- (member z :imag)))
  (return result))

(defclcmacro complex* (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_mult ,(rec (butlast xs))
                                ,(first (last xs)))
                 (first xs))))
    (rec args)))

;; scalar multiplication
(defclcfun complex_rmult (:struct cl_complex)
    ((var x (:struct cl_complex))
     (var y :double))
  (var result (:struct cl_complex))
  (setf (member result :real)
        (* (member x :real) y))
  (setf (member result :imag)
        (* (member x :imag)
           y))
  (return result))

(defclcmacro complexr* (&rest args)
  (labels ((rec (xs)
             (if (cdr xs)
                 `(complex_rmult ,(rec (butlast xs))
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

;;; basic math
;; complex exponential
(defclcfun cexp (:struct cl_complex)
    ((var z (:struct cl_complex)))
  (return
    (complexr*
     (complex (cos (member z :imag))
              (sin (member z :imag)))
     (exp (member z :real)))))
