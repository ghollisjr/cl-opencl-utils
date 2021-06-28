(in-package :cl-opencl-utils)

(defclc and (&rest expressions)
  (with-output-to-string (out)
    (format out "(~{(~a)~^~( && ~)~})"
            (mapcar #'clc expressions))))

(defclc or (&rest expressions)
  (with-output-to-string (out)
    (format out "(~{(~a)~^~( || ~)~})"
            (mapcar #'clc expressions))))

(defclc not (expression)
  (with-output-to-string (out)
    (format out "(! (~a))"
            (clc expression))))

(defclc zerop (expression)
  (format nil "((~a) == 0)"
          (clc expression)))

(defclc plusp (expression)
  (format nil "((~a) > 0)"
          (clc expression)))

(defclc minusp (expression)
  (format nil "((~a) < 0)"
          (clc expression)))
