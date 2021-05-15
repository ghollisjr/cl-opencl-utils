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
                          (format out "~a ~a ~a"
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
