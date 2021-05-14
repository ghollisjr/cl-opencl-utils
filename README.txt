cl-opencl-utils is a library of basic utilities for working with
OpenCL.  It is built on top of the cl-opencl library
(https://www.github.com/ghollisjr/cl-opencl).

The examples/ directory has a few examples showing how to use some of
the included utilities.

make-opencl-reducer and make-opencl-mapper are particularly useful as
they provide basic map and reduce functionality that are
OpenCL-accelerated but relatively convenient to use from a Lisp
perspective.

cl-opencl-utils is in the public domain with the exception of the
c-type-name function which was modified from someone whose license
requires giving them credit.  I don't mind getting credit, but charity
doesn't depend on credit.
