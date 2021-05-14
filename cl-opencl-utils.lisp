(in-package :cl-opencl-utils)

;;; utility functions

;; Modified version of c-type-name from CFFI's groveler.  As per their
;; request, here's their message:
#|
Copyright (C) 2005-2007, James Bielman  <jamesjb@jamesjb.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
|#
;; I've modified their code, and I don't offer you any warranties
;; either.
(defun c-type-name (type-keyword)
  (case type-keyword
    ((:uchar :unsigned-char) "unsigned char")
    ((:unsigned-short :ushort) "unsigned short")
    ((:unsigned-int :uint) "unsigned int")
    ((:unsigned-long :ulong) "unsigned long")
    ((:long-long :llong) "long long")
    ((:unsigned-long-long :ullong) "unsigned long long")
    (:pointer "void*")
    (:string "char*")
    (t (cffi::foreign-name type-keyword nil))))

;;; Reductions
(defparameter +OPENCL-ADD-REXPR+
  (lambda (x y)
    (format nil "((~a) + (~a))" x y)))

(defun make-opencl-reducer (queue type rexpr
                            &key
                              (preamble "")
                              headers
                              options)
  "Returns a list (function kernel program) where function is of the form

(lambda (buffer &key start end) ...)

that will perform a reduction operation according to the input
parameters.  buffer must an OpenCL memory object, and start and end
are optional indices into the buffer.

queue must be an OpenCL queue handle.

type should be a CFFI type designator.

rexpr should be a Lisp function accepting two strings and returning
OpenCL C code for an expression of a binary operation.  The expression
can refer to code defined in the preamble string argument or included
in one of the headers.

memobj is the source memory buffer.

preamble can be a string of OpenCL C code to be placed below headers
but above the reduction kernel.  Useful for defining utility functions
to call in the expression.

headers can be a list of device header file names to include in the
kernel OpenCL C code.

options can be compiler options for the OpenCL kernel.

start can be an initial index offset to start reading from the buffer.

end can be a final index offset to stop reading from the buffer.  Uses
subseq index convention, i.e. the element at index=end is not
included.

kernel and program will need to be released at some point once you're
done using the reducer."
  (let* ((typename (c-type-name type))
         
         (kernel-source
          (concatenate
           'string
           (format nil "~{#include \"~a\"~^~%~}"
                   headers)
           preamble
           (format nil "
__kernel
void reduce(__global ~a *input,
            __global long unsigned int *startend,
            __global ~a *output,
            __local ~a *acc)
{
  const int nwork = get_local_size(0);
  const int gid = get_global_id(0);
  const int wid = get_local_id(0);
  const long unsigned int start = startend[0];
  const long unsigned int end = startend[1];
  acc[wid] = 0;
  barrier(CLK_LOCAL_MEM_FENCE);
  if((start + gid) < end) {
    acc[wid] = input[start + gid];
    barrier(CLK_LOCAL_MEM_FENCE);
    int niter = ceil(log2((float) nwork));
    for(int i = 0; i < niter; ++i) {
      const int stride = (1<<(i+1));
      if((wid%stride) == 0) {
        const int next = wid + (1<<i);
        if(next < nwork) {
          acc[wid] = ~a;
        }
      }
      barrier(CLK_LOCAL_MEM_FENCE);
    }
    if(wid == 0) {
      output[get_group_id(0)] = acc[0];
    }
  }
}
"
                   typename typename typename
                   (funcall rexpr "acc[wid]" "acc[next]"))))
         (context (cl-get-command-queue-info 
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info 
               queue +CL-QUEUE-DEVICE+))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program program (list dev)
                      :options options)
    (let* ((kernel
            (cl-create-kernel program "reduce"))
           (reducefn
            (lambda (buffer &key start end)
              (let* ((start (if start
                                start
                                0))
                     (bufsize (cl-get-mem-object-info buffer
                                                      +CL-MEM-SIZE+))
                     (end (if end
                              end
                              (/ bufsize
                                 (foreign-type-size type))))
                     (events NIL) ; accumulated events and cleanup functions
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (n (- end start))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups))
                     (startendbuf
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :data (list start end)))
                     (outbuf1
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-WRITE+
                                        :type type
                                        :count ngroups))
                     (outbuf2
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-WRITE+
                                        :type type
                                        :count ngroups)))
                ;; initial execution
                (cl-set-kernel-arg kernel 0 :value buffer)
                (cl-set-kernel-arg kernel 1 :value startendbuf)
                (cl-set-kernel-arg kernel 2 :value outbuf1)
                ;; local memory buffer
                (cl-set-kernel-arg kernel 3
                                   :type type
                                   :count wgsize)
                (cl-enqueue-ndrange-kernel queue kernel
                                           (list globalworksize)
                                           (list wgsize))
                ;; loop
                (let* ((niter (ceiling (log ngroups
                                            wgsize)))
                       (buf1 NIL)
                       (buf2 outbuf1)
                       (n NIL)
                       (localworksize NIL))
                  (when (not (zerop niter))
                    (loop
                       for i below niter
                       do
                         (setf n ngroups)
                         (setf ngroups (ceiling n wgsize))
                         (setf localworksize (min wgsize n))
                         (setf globalworksize (* ngroups
                                                 localworksize))
                         (if (evenp i)
                             (progn
                               (setf buf1 outbuf1)
                               (setf buf2 outbuf2))
                             (progn
                               (setf buf1 outbuf2)
                               (setf buf2 outbuf1)))
                         (push 
                          (cl-enqueue-write-buffer queue
                                                   startendbuf
                                                   :ulong
                                                   (list 0 n))
                          events)
                         (cl-set-kernel-arg kernel 0 :value buf1)
                       ;; might not actually need this, will test
                         (cl-set-kernel-arg kernel 1 :value startendbuf)
                         (cl-set-kernel-arg kernel 2 :value buf2)
                         (push
                          (cl-enqueue-ndrange-kernel queue
                                                     kernel
                                                     (list globalworksize)
                                                     (list localworksize))
                          events)))
                  (destructuring-bind (finalevent returner)
                      (cl-enqueue-read-buffer
                       queue buf2
                       type 1)
                    (setf events (nreverse events))
                    (let* ((cleanup
                            (lambda ()
                              (loop
                                 for ev in events
                                 do
                                   (if (listp ev)
                                       (progn
                                         (funcall (second ev))
                                         (cl-release-event (first ev)))
                                       (cl-release-event ev)))
                              (elt (funcall returner) 0))))
                      (list finalevent cleanup))))))))
      (list reducefn kernel program))))

;;; Maps
(defun make-opencl-mapper (queue input-type mexpr
                           &key
                             output-type
                             (preamble "")
                             headers
                             options)
  "Returns a list (function kernel program) where function is of the form

(lambda (in-buffer out-buffer &key in-start in-end out-start) ...)

that will perform a map operation according to the input parameters.
in-buffer and out-buffer must be OpenCL memory objects, and in-start
and in-end are optional indices into the in-buffer.  The results will
be written to out-buffer starting at out-start.  in-start defaults to
0, in-end defaults to the end of the input buffer, and out-start
defaults to 0.  The return value of the generated function is an event
that will need to be waited on and released at some point.

queue must be an OpenCL queue handle.

input-type should be a CFFI type designator.

If output-type is not specified, it is assumed to be input-type.

mexpr should be a Lisp function accepting one string and returning
OpenCL C code for an expression of a unary operation to apply to the
variable referenced in the string argument.  The expression can refer
to code defined in the preamble string argument or included in one of
the headers.

memobj is the source memory buffer.

preamble can be a string of OpenCL C code to be placed below headers
but above the reduction kernel.  Useful for defining utility functions
to call in the expression.

headers can be a list of device header file names to include in the
kernel OpenCL C code.

options can be compiler options for the OpenCL kernel.

start can be an initial index offset to start reading from the buffer.

end can be a final index offset to stop reading from the buffer.  Uses
subseq index convention, i.e. the element at index=end is not
included.

kernel and program will need to be released at some point once you're
done using the reducer."
  (let* ((input-typename (c-type-name input-type))
         (output-type (if output-type output-type input-type))
         (output-typename (c-type-name output-type))
         (kernel-source
          (concatenate
           'string
           (format nil "~{#include \"~a\"~^~%~}"
                   headers)
           preamble
           (format nil "
__kernel
void map(__global ~a *input,
         __global long unsigned int *input_startend,
         __global long unsigned int *output_start,
         __global ~a *output)
{
  const int nwork = get_local_size(0);
  const int gid = get_global_id(0);
  const int wid = get_local_id(0);
  const long unsigned int start = input_startend[0];
  const long unsigned int end = input_startend[1];
  const long unsigned int outstart = output_start[0];
  if((start + gid) < end) {
    output[outstart + gid] = ~a;
  }
}
"
                   input-typename output-typename
                   (funcall mexpr "input[start + gid]"))))
         (context (cl-get-command-queue-info 
                   queue +CL-QUEUE-CONTEXT+))
         (dev (cl-get-command-queue-info 
               queue +CL-QUEUE-DEVICE+))
         (program
          (cl-create-program-with-source context kernel-source)))
    (cl-build-program program (list dev)
                      :options options)
    (let* ((kernel
            (cl-create-kernel program "map"))
           (mapfn
            (lambda (in-buffer out-buffer &key in-start in-end out-start)
              (let* ((in-start (if in-start
                                   in-start
                                   0))
                     (in-bufsize (cl-get-mem-object-info in-buffer
                                                         +CL-MEM-SIZE+))
                     (in-end (if in-end
                                 in-end
                                 (/ in-bufsize
                                    (foreign-type-size input-type))))
                     (out-start (if out-start
                                    out-start
                                    0))
                     (event NIL)
                     (wgsize
                      (cl-get-kernel-work-group-info
                       kernel
                       dev
                       +CL-KERNEL-WORK-GROUP-SIZE+))
                     (n (- in-end in-start))
                     (ngroups
                      (ceiling n
                               wgsize))
                     (globalworksize
                      (* wgsize ngroups))
                     (in-startendbuf
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :data (list in-start in-end)))
                     (out-startbuf
                      (cl-create-buffer context
                                        :flags +CL-MEM-READ-ONLY+
                                        :type :ulong
                                        :data (list out-start))))
                ;; initial execution
                (cl-set-kernel-arg kernel 0 :value in-buffer)
                (cl-set-kernel-arg kernel 1 :value in-startendbuf)
                (cl-set-kernel-arg kernel 2 :value out-startbuf)
                (cl-set-kernel-arg kernel 3 :value out-buffer)
                (setf event
                      (cl-enqueue-ndrange-kernel queue kernel
                                                 (list globalworksize)
                                                 (list wgsize)))
                (cl-release-mem-object in-startendbuf)
                (cl-release-mem-object out-startbuf)
                event))))
      (list mapfn kernel program))))
