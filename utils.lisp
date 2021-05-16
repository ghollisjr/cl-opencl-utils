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

;;; Expression generator
(defun opencl-function-expr (fname)
  "Returns an expression function generating the Lispified OpenCL code
to call that function when supplied with a list of arguments.  Useful
for multiple utilities, e.g. make-opencl-reducer, make-opencl-mapper,
make-opencl-convolutor."
  (lambda (&rest xs)
    `(,fname ,@xs)))
