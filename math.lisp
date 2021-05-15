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
