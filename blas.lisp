(in-package :cl-opencl-utils)

;;;; A very much in-progress system for BLAS operations

;;;; AXPY
(defun make-opencl-axpy (queue
                         &key
                           (type :float))
  "Returns (axpy cleanup) where axpy is of the form (lambda (a xbuf
ybuf &key count)...) which sets ybuf to the value of a*x + y where a is a scalar
x and y are OpenCL buffers treated as vectors.  The return value of
axpy is an event to be waited on for the completion of the OpenCL
operation.

cleanup should be called once the axpy function is no longer needed.
with-opencl-cleanup is useful here.

count is the number of elements to use in the x and y buffers.  If not
supplied, then the minimum number of elements that would fit in xbuf
or ybuf of the given type will be used.

type is the OpenCL type to use for a, xbuf, and ybuf."
  (let* ((context (cl-get-command-queue-info
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info
               queue +CL-QUEUE-DEVICE+))
         (source
          `(kernel axpy
                   ((var count (global (pointer :ulong)))
                    (var a (global (pointer ,type)))
                    (var x (global (pointer ,type)))
                    (var y (global (pointer ,type))))
                   (var gid (const :ulong)
                        (get-global-id 0))
                   (when (< gid (value count))
                     (setf (aref y gid)
                           (+ (aref y gid)
                              (* (value a)
                                 (aref x gid)))))))
         (program
          (let* ((p (cl-create-program-with-source
                     context
                     (program-source-from-forms-fn source))))
            (cl-build-program-with-log p (list dev))
            p))
         (kernel (cl-create-kernel program "axpy"))
         (countbuf (cl-create-buffer context
                                     :count 1
                                     :type :ulong))
         (abuf (cl-create-buffer context
                                 :count 1
                                 :type type))
         (last-a NIL)
         (last-count NIL))
    (labels ((axpy (a xbuf ybuf &key count)
               (when (and a
                          (not (equal a last-a)))
                 (setf last-a a)
                 (cl-enqueue-write-buffer
                  queue abuf type (list last-a)
                  :blocking-p t))
               (when (and count
                          (not (equal count last-count)))
                 (setf last-count count)
                 (cl-enqueue-write-buffer
                  queue countbuf :ulong (list last-count)))
               (when (not last-count)
                 (setf last-count
                       (min (floor (cl-get-mem-object-info xbuf
                                                           +CL-MEM-SIZE+)
                                   (foreign-type-size type))
                            (floor (cl-get-mem-object-info ybuf
                                                           +CL-MEM-SIZE+)
                                   (foreign-type-size type))))
                 (cl-enqueue-write-buffer
                  queue countbuf :ulong (list last-count)
                  :blocking-p t))
               (cl-set-kernel-arg kernel 2 :value xbuf)
               (cl-set-kernel-arg kernel 3 :value ybuf)
               (cl-enqueue-kernel queue kernel last-count))
             (cleanup ()
               (mapcar #'cl-release-mem-object
                       (list abuf countbuf))
               (cl-release-kernel kernel)
               (cl-release-program program)))
      (cl-set-kernel-arg kernel 0 :value countbuf)
      (cl-set-kernel-arg kernel 1 :value abuf)
      (list #'axpy #'cleanup))))
