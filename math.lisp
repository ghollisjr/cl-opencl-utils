(in-package :cl-opencl-utils)

(defclc + (&rest forms)
  (with-output-to-string (out)
    (format out "(~{(~a)~^+~})" (mapcar #'clc forms))))

(defclc - (&rest forms)
  (with-output-to-string (out)
    (if (single forms)
        (format out "(-~a)" (clc (first forms)))
        (format out "(~{(~a)~^-~})" (mapcar #'clc forms)))))

(defclc * (&rest forms)
  (with-output-to-string (out)
    (format out "(~{(~a)~^*~})" (mapcar #'clc forms))))

;; Note: Not valid for single argument, only works with two or more arguments
(defclc / (&rest forms)
  (with-output-to-string (out)
    (format out "(~{(~a)~^/~})" (mapcar #'clc forms))))

(defclc incf (var &optional amount)
  (if amount
      (format nil "((~a) += (~a))"
              (clc var)
              (clc amount))
      (format nil "(++(~a))" (clc var))))

(defclc decf (var &optional amount)
  (if amount
      (format nil "((~a) -= (~a))"
              (clc var)
              (clc amount))
      (format nil "(--(~a))" (clc var))))

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
