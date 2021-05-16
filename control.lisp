(in-package :cl-opencl-utils)

;; guaranteed context
(defclc progn (&body body)
  (with-output-to-string (out)
    (format out "{~%")
    (loop
       for expr in body
       do (format out "~a;~%" (clc expr)))
    (format out "}")))

(defclc let (bindings &body body)
  (with-output-to-string (out)
    (format out "{~%")
    (loop
       for binding in bindings
       do (destructuring-bind (var type &optional init) binding
            (format out "~a ~a" (clc type) (clc var))
            (when init
              (format out " = ~a" (clc init)))
            (format out ";~%")))
    (loop
       for expr in body
       do (format out "~a;~%"
                  (clc expr)))
    (format out "}~%")))

(defclc cond (&rest test-bodies)
  (with-output-to-string (out)
    (loop
       for test-body in test-bodies
       for entry from 0
       do (let ((opstr (if (zerop entry)
                           "if"
                           "else if")))
            (destructuring-bind (test &rest body) test-body
              (if (not (eq test t))
                  (format out "~a(~a) {~%"
                          opstr (clc test))
                  (format out "else {~%"))
              (loop
                 for expr in body
                 do (format out "~a;~%" (clc expr)))
              (format out "}~%")
              (if (eq test t)
                  (return t)))))))

(defclc if (test expr &optional else)
  (with-output-to-string (out)
    (format out "if(~a) {~%" (clc test))
    (format out "~a;~%" (clc expr))
    (format out "}~%")
    (when else
      (format out "else {~%")
      (format out "~a;~%" (clc else))
      (format out "}~%"))))

(defclc when (test &rest body)
  (clc `(if ,test (progn ,@body))))

;; Transliteration of OpenCL C for
(defclc for (init test incr &body body)
  (with-output-to-string (out)
    (format out "for(~a; ~a; ~a) {~%"
            (clc init) (clc test) (clc incr))
    (loop
       for expr in body
       do (format out "~a;~%" (clc expr)))
    (format out "}~%")))

;; Same for while
(defclc while (test &body body)
  (with-output-to-string (out)
    (format out "while(~a) {~%"
            (clc test))
    (loop
       for expr in body
       do (format out "~a;~%" (clc expr)))
    (format out "}~%")))
