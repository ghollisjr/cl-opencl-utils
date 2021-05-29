(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;; Nested structs example.  Nested structs must be declared in order
;;; of dependence, as in CFFI.
(defclcstruct teststruct_a
  (:x :double)
  (:y :double)
  (:z :double))

(defclcstruct teststruct_b
  (:s (:struct teststruct_a))
  (:w :double))

(defclcstruct teststruct_c
  (:s (:struct teststruct_b))
  (:w :double))

(defclckernel testkernel
    ((var x (global (pointer :double)))
     (var y (global (pointer :double))))
  (var s (:struct teststruct_a))
  (setf (member s :x) 0d0))

;; Compile test
(defun test ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-GPU+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((source
                (program-source-from-kernels 'testkernel))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context source)))
                  (cl-build-program-with-log p (list dev))
                  p)))
          (cl-release-program program))))))

;;; Struct I/O Example
(defclcstruct triple_xyz
  (:x :double)
  (:y :double)
  (:z :double))

;; Note that this could be easily accomplished with make-opencl-mapper
;; and a utility function, but this example illustrates how to use
;; structs in Lispified OpenCL C with direct kernel definition.
(defclckernel structshiftdot
    ((var n (global (pointer :ulong)))
     (var s (global (pointer (:struct triple_xyz))))
     (var result (global (pointer :double))))
  (var gid (const :int) (get-global-id 0))
  (var nn (const :ulong) (value n))
  (when (< gid nn)
    (var ss (:pointer (:struct triple_xyz))
         (+ s gid))
    (setf (aref result gid)
          (+ (* (pmember ss :x) ; pmember accesses member from pointer
                (pmember ss :y))
             (pmember ss :z)))))

(defun tripletest ()
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-GPU+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((source
                (program-source-from-kernels 'structshiftdot))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context source)))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel (cl-create-kernel program "structshiftdot"))
               (data
                (loop
                   for i below 100
                   collecting (list :x (float i 1d0)
                                    :y (float i 1d0)
                                    :z (float i 1d0))))
               (ndata (length data))
               (nbuf (cl-create-buffer context
                                       :type :ulong
                                       :data (list ndata)))
               ;; Data will be automatically converted from p-lists
               ;; into an array of CFFI structs and sent to OpenCL by
               ;; cl-create-buffer
               (sbuf
                (cl-create-buffer context
                                  :type '(:struct triple_xyz)
                                  :data data))
               (rbuf
                (cl-create-buffer context
                                  :type :double
                                  :count ndata)))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value sbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (cl-enqueue-kernel queue
                                              kernel
                                              ndata)
                           (cl-enqueue-read-buffer queue
                                                   rbuf
                                                   :double
                                                   ndata)))))))
            (mapcar #'cl-release-mem-object
                    (list rbuf sbuf nbuf))
            (cl-release-kernel kernel)
            (cl-release-program program)
            ;; OpenCL and Lisp computation comparison
            (list result
                  (map 'vector
                       (lambda (x)
                         (+ x (* x x)))
                       (loop
                          for i below 100
                          collecting (float i 1d0))))))))))
