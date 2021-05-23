(in-package :cl-opencl-utils)

;;;; Use cubic splines in OpenCL C

;; Evaluates a cubic spline
(defclcfun eval_cubic_spline :double
    ((var x :double)
     (var nxs :ulong)
     (var xs (pointer :double))
     (var coefs (pointer :double)))
  (var ncoefs (const :ulong)
       (* 4 nxs))
  (when (< x (aref xs 0))
    (return 0d0))
  (for (var i :ulong 0) (< i (1- nxs)) (incf i)
       (when (<= (aref xs i)
                 x
                 (aref xs (1+ i)))
         (var delta (const :double) (- (aref xs (1+ i))
                                      (aref xs i)))
         (var xx :double
              (/ (- x (aref xs i))
                 delta))
         (var result :double 0d0)
         (var tmp :double 1d0)
         (for (var j :ulong 0) (< j 4) (incf j)
              (setf result (+ result
                              (* tmp (aref coefs
                                           (+ j
                                              (* 4 i))))))
              (setf tmp (* tmp xx)))
         (return result)))
  (return 0d0))


;; Same for single-float
(defclcfun eval_cubic_spline_f :float
    ((var x :float)
     (var nxs :ulong)
     (var xs (pointer :float))
     (var coefs (pointer :float)))
  (var ncoefs (const :ulong)
       (* 4 nxs))
  (when (< x (aref xs 0))
    (return 0f0))
  (for (var i :ulong 0) (< i (1- nxs)) (incf i)
       (when (<= (aref xs i)
                 x
                 (aref xs (1+ i)))
         (var delta (const :float) (- (aref xs (1+ i))
                                      (aref xs i)))
         (var xx :float
              (/ (- x (aref xs i))
                 delta))
         (var result :float 0f0)
         (var tmp :float 1f0)
         (for (var j :ulong 0) (< j 4) (incf j)
              (setf result (+ result
                              (* tmp (aref coefs
                                           (+ j
                                              (* 4 i))))))
              (setf tmp (* tmp xx)))
         (return result)))
  (return 0f0))

(defun serialize-spline-coef-array (array)
  "Creates list of coefficients from 2-D coefficient array as stored
in a cl-ana polynomial-spline."
  (let* ((dims (array-dimensions array))
         (nrows (first dims))
         (ncols (second dims)))
    (loop
       for row below nrows
       appending
         (loop
            for col below ncols
            collecting (aref array row col)))))

(defun make-opencl-spline-form (name xs coefs
                                &key (type :float))
  "Returns Lispified OpenCL code for a spline function called name.
Can be supplied to the preamble of other utilities like
make-opencl-integrator or make-opencl-function-sampler."
  (let* ((xs (coerce xs 'list))
         (coefs (if (listp coefs)
                    coefs
                    (serialize-spline-coef-array coefs))))
    `(function ,name ,type
               ((var x ,type))
               (var nxs (const :ulong) ,(length xs))
               (vararray xs ,type
                         (,(length xs))
                         ,@xs)
               (vararray coefs ,type
                         (,(length coefs))
                         ,@coefs)
               (return (,(if (eq type :double)
                             'eval_cubic_spline
                             'eval_cubic_spline_f)
                         x nxs xs coefs)))))
