(require 'cl-opencl-utils)
(in-package :cl-opencl-utils)

;;;; A simple hello world type of program
;;;;
;;;; This example shows an alternative model for writing OpenCL
;;;; libraries using cl-opencl.
;;;;
;;;; Kernels and other functions are defined in Lisp code like so:
(defclckernel :void hello
    ((var (global (pointer :uint)) n)
     (var (global (pointer :uint)) buf))
  (var int gid (get-global-id 0))
  (when (< gid (value n))
    (setf (aref buf gid)
          gid)))

;;;; Then a Lisp function can create source code from either a set of
;;;; top-level-forms comprising a Lispified OpenCL program with
;;;; implicit function and kernel dependencies, or the source code can
;;;; be generated from a list of required kernels.
(defun hello-opencl ()
  "Demonstrate OpenCL API"
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids plat +CL-DEVICE-TYPE-ALL+)))
         (context
          (cl-create-context plat (list dev)))
         (source
          ;; Source generated from a list of supplied kernels:
          (program-source-from-kernels hello)
           ;; Or alternatively the source could be generated from
           ;; Lispified OpenCL C code with implicit function and
           ;; kernel dependencies:
           ;;
           ;; NOTE: Duplicate definitions like would occur if this
           ;; were uncommented are bad, as there is no mechanism for
           ;; detecting duplicate definitions at the moment.  So
           ;; either remove the defclckernel above and use this, or
           ;; keep the defclckernel and use the
           ;; program-source-from-kernels tool above.
           ;; 
           ;; (program-source-from-forms
           ;;   (kernel :void hello
           ;;           ((var (global (pointer :uint)) n)
           ;;            (var (global (pointer :uint)) buf))
           ;; 
           ;;           (var int gid (get-global-id 0))
           ;;           (when (< gid (value n))
           ;;             (setf (aref buf gid)
           ;;                   gid))))
           )
         (program
           (cl-create-program-with-source
            context
            source)))
    (cl-build-program-with-log program (list dev)
                               :options "-cl-kernel-arg-info")
    (let* ((njobs 100)
           (kernel
            (cl-create-kernel program "hello"))
           (nbuf
            (cl-create-buffer context
                              :flags
                              ;; technically +CL-MEM-COPY-HOST-PTR+ is
                              ;; automatically included whenever data
                              ;; is supplied, but you can still supply
                              ;; it
                              (list +CL-MEM-READ-WRITE+
                                    +CL-MEM-COPY-HOST-PTR+)
                              :type :uint
                              :data (list njobs)))
           (outbuf
            ;; Manual size calculation:
            ;; 
            ;; (cl-create-buffer context
            ;;                   :flags 
            ;;                   +CL-MEM-READ-WRITE+
            ;;                   :size
            ;;                   (* njobs
            ;;                      (foreign-type-size
            ;;                       :uint)))

            ;; More convenient automatic size calculation:
            (cl-create-buffer context
                              :flags +CL-MEM-READ-WRITE+ ; default value
                              :count njobs
                              :type :uint))
           (queue
            (cl-create-command-queue context dev))
           (nwork
            (cl-get-kernel-work-group-info
             kernel dev +CL-KERNEL-WORK-GROUP-SIZE+))
           (nglobal (* nwork
                       (ceiling njobs nwork))))
      (cl-set-kernel-arg kernel 0 :value nbuf)
      (cl-set-kernel-arg kernel 1 :value outbuf)
      (cl-wait-and-release-events
       (list
        (cl-enqueue-ndrange-kernel queue kernel
                                   (list nglobal)
                                   (list nwork))))
      (let* ((result
              (cl-enqueue-read-buffer queue outbuf
                                      :uint njobs
                                      :blocking-p t)))
        (cl-release-kernel kernel)
        (cl-release-program program)
        (cl-release-mem-object outbuf)
        (cl-release-mem-object nbuf)
        (cl-release-context context)
        result))))
