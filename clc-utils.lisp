(in-package :cl-opencl-utils)

(defclc copy-array (from to length)
  (let ((i (gsym))
        (len (gsym)))
    (clc
     `(progn
        (var int ,len ,length)
        (for (var int ,i 0) (< ,i ,len) (incf ,i)
             (setf (aref ,to ,i)
                   (aref ,from ,i)))))))
