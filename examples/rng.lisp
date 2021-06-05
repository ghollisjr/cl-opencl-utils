(require 'cl-opencl-utils)
(defpackage #:rng-example
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(in-package :rng-example)

(defclckernel rngtest
    ((var n (global (pointer :ulong)))
     (var seed (global (pointer :ulong)))
     (var results (global (pointer :uint))))
  (var gid (const :int)
       (get-global-id 0))
  (var nn (const :ulong) (value n))
  (when (< gid nn)
    (pcg32_init (+ (value seed)
                   gid ))
    (setf (aref results gid)
          (pcg32))))

(defun rng-example-1 ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((n 10)
               (nbuf (cl-create-buffer context
                                       :type :ulong
                                       :data (list n)))
               (sbuf (cl-create-buffer context
                                       :type :ulong
                                       :data
                                       (list
                                        (mod (* (get-internal-real-time)
                                                (get-internal-real-time))
                                             (expt 2 64)))))
               (rbuf (cl-create-buffer context
                                       :type :uint
                                       :count n))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context
                         (program-source-from-kernels rngtest))))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel (cl-create-kernel program "rngtest")))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value sbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (cl-enqueue-kernel queue kernel n)
                           (cl-enqueue-read-buffer
                            queue rbuf :uint n)))))))
            (cl-release-kernel kernel)
            (cl-release-program program)
            (mapcar #'cl-release-mem-object
                    (list rbuf sbuf nbuf))
            result))))))

(defclckernel uniformtest
    ((var n (global (pointer :ulong)))
     (var seed (global (pointer :ulong)))
     (var results (global (pointer :double))))
  (var gid (const :int)
       (get-global-id 0))
  (var nn (const :ulong) (value n))
  (when (< gid nn)
    (pcg32_init (+ (value seed)
                   gid ))
    (setf (aref results gid)
          (uniform_random))))

(defun rng-example-2 ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat
                                        +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((n 10)
               (nbuf (cl-create-buffer context
                                       :type :ulong
                                       :data (list n)))
               (sbuf (cl-create-buffer context
                                       :type :ulong
                                       :data
                                       (list
                                        (mod (* (get-internal-real-time)
                                                (get-internal-real-time))
                                             (expt 2 64)))))
               (rbuf (cl-create-buffer context
                                       :type :double
                                       :count n))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context
                         (program-source-from-kernels uniformtest))))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel (cl-create-kernel program "uniformtest")))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value sbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (cl-enqueue-kernel queue kernel n)
                           (cl-enqueue-read-buffer
                            queue rbuf :double n)))))))
            (cl-release-kernel kernel)
            (cl-release-program program)
            (mapcar #'cl-release-mem-object
                    (list rbuf sbuf nbuf))
            result))))))
