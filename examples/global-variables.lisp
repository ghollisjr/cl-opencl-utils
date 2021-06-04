(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;;; Example demonstrating global variables.  A few things:
;;;;
;;;; * Notice I'm using GV_ as a prefix.  As implemented, Lispified
;;;;   OpenCL C downcases all symbols when converting to C code
;;;;   strings, which means the case here is just for ease of reading
;;;;   for the programmer.
;;;;
;;;; * If you want to use upper case names, then just like the rest of
;;;;   the Lispified OpenCL C system, you can use strings instead of
;;;;   symbols to denote terms.  E.g.,
;;;;
;;;;   (defclcglobalvar (var "SOME_GLOBAL" ...))
;;;;
;;;;   would define SOME_GLOBAL in C code, whereas
;;;;
;;;;   (defclcglobalvar (var SOME_GLOBAL ...))
;;;;
;;;;   would define some_global in C code.
;;;; 
;;;; * The usual Lisp convention of +SOME-VARIABLE+ doesn't work as
;;;;   the + symbol is not removed.  This might be changed at some
;;;;   point, but it seems like something that would add unnecessary
;;;;   confusion to an otherwise simple system.

;; Note: To be used in definitions of other global variables, types
;; must be const.
(defclcglobalvar
    (var GV_A (const :double) 5d0))

(defclcglobalvar
    ;; If GV_A weren't const, this would cause error.
    (var GV_B :double (+ GV_A 2d0)))

(defclcfun testfn :double
    ((var x :double))
  (return (+ x GV_B)))

(defclckernel gvartest
    ((var n (global (pointer :ulong)))
     (var xs (global (pointer :double)))
     (var result (global (pointer :double))))
  (var gid (const :int) (get-global-id 0))
  (var nn (const :ulong) (value n))
  (when (< gid nn)
    (setf (aref result gid)
          (testfn (aref xs gid)))))

(defun global-variable-example ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((data (list 1d0 2d0 3d0 4d0 5d0))
               (ndata (length data))
               (nbuf (cl-create-buffer context
                                       :type :ulong
                                       :data (list ndata)))
               (dbuf (cl-create-buffer context
                                       :type :double
                                       :data data))
               (rbuf (cl-create-buffer context
                                       :type :double
                                       :count ndata))
               (program
                (let* ((p (cl-create-program-with-source
                           context
                           (program-source-from-kernels gvartest))))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel (cl-create-kernel program "gvartest")))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value dbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list
                      (cl-enqueue-kernel queue kernel ndata)
                      (cl-enqueue-read-buffer queue rbuf :double ndata)))))))
            (cl-release-kernel kernel)
            (cl-release-program program)
            (mapcar #'cl-release-mem-object
                    (list nbuf dbuf rbuf))
            result))))))
