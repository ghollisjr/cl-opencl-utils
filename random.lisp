(in-package :cl-opencl-utils)

;;;; Psuedo-Random Number Generation

;;; PCG-XSH-RR (32 bit)
;;;
;;; Quick, cheap, and effective generator from 2014:
;;;
;;; http://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf
;;;
;;; Note that this defines a private-memory RNG, so seeds need to be
;;; supplied to each job and there is no need to worry about race
;;; conditions or mutexes.  A quick and dirty way is to create one
;;; unique seed for a batch of jobs and then add (get-global-id 0) to
;;; that seed.  Obviously you can be more careful if needed.
;;;
;;; See examples/rng.lisp for the quick and dirty seed method.

(defclcglobalvar
    (var GV_PCG32_INIT_STATE
         :ulong
         "0x4d595df4d0f33173"))

(defclcglobalvar
    (var GV_PCG32_MULTIPLIER
         (constant :ulong)
         "6364136223846793005u"))

(defclcglobalvar
    (var GV_PCG32_INCREMENT
         (constant :ulong)
         "1442695040888963407u"))

(defclcfun pcg32_rotr32 :uint
    ((var x :uint)
     (var r :uint))
  (return (logior (>> x r)
                  (<< x (logand (- r)
                                31)))))

(defclcfun pcg32 :uint
    ((var state (pointer :ulong)))
  (var x :ulong (value state))
  (var count :uint
       (coerce (>> x 59)
               :uint))
  (setf (value state)
        (+ (* x GV_PCG32_MULTIPLIER)
           GV_PCG32_INCREMENT))
  (setf x
        (logxor x
                (>> x 18)))
  (return (pcg32_rotr32 (coerce (>> x 27)
                                :uint)
                        count)))
    
(defclcfun pcg32_init :ulong
    ((var seed :ulong))
  (var state :ulong
       (+ seed GV_PCG32_INCREMENT))
  (pcg32)
  (return state))

;; These use pcg32, so seed with pcg32_init before use.
(defclcfun uniform_randomf :float
    ((var state (:pointer :ulong)))
  (return (/ (coerce (pcg32 state) :float)
             4294967295.0))) ; 2^32-1

(defclcfun uniform_random :double
    ((var state (:pointer :ulong)))
  (return (/ (coerce (pcg32 state) :double)
             4294967295.0))) ; 2^32-1
