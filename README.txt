cl-opencl-utils is a library of utilities for working with OpenCL,
especially including a Lispified version of OpenCL C.  It is built on
top of the cl-opencl library
(https://www.github.com/ghollisjr/cl-opencl).

The examples/ directory has a few examples showing how to use some of
the included utilities.

make-opencl-reducer and make-opencl-mapper are particularly useful as
they provide basic map and reduce functionality that are
OpenCL-accelerated but convenient to use from a Lisp perspective.

cl-opencl-utils is GPL3 with the exception of the c-type-name
function, which is possible to use but I'm not sure if it can be
promoted to GPL3 completely.  If so, then cl-opencl-utils is GPL3.  If
not, then just that part isn't.

The Lispified OpenCL C language follows a few rules:

1. If a symbol has not been given special meaning, it will be
   translated into an all-lowercase string, non-alphanumeric symbols
   preserved.

2. If the first element of a list does not contain a symbol with a
   special meaning, it is translated into a C-style function call.
   E.g., (f x y) ==> f(x,y).

3. Specialized meanings for symbols are defined with defclc, which
   causes the #'clc function to generate a different C source code
   string result.  E.g., (clc `(zerop x)) ==> ((x) == 0) since zerop
   has a special meaning already defined.

4. Functions in C that have a Lisp equivalent have their Lisp aliases
   defined.  E.g., (expt x y) ==> pow(x,y), but (pow x y) ==> pow(x,y)
   as well.

5. Variables are declared via the var and vararray operators:

   (var x :int 0) ==> int x = 0.
   (var f (const :double) 5d0) ==> const double f = 5.0;
   (vararray x :double (2 3)) ==> double x[2][3].
   (vararray y :double (5) 1 2 3 4 5) ==> double x[5] = {1,2,3,4,5}

   I tend to use the CFFI-style keyword symbols for types as it
   improves readability, but it's not technically necessary.

6. Blocks are created with progn:

   (progn
    (var i :int 0)
    (var j :int i))
   ==>
   {int i = 0; int j = i;}

7. for, while, and do-while provide for-loops, while-loops and
   do-while loops:

   (for (var i :int 0) (< i 10) (incf i)
        (setf sum (+ sum i)))
   ==>
   for(int i = 0; i < 10; ++i) {
     sum = sum + i;
   }

   (var x :double 1d0)
   (var i :int 0)
   (while (< i 5)
     (incf i)
     (setf x (* x 2d0)))
   ==>
   double x = 1.0;
   int i = 0;
   while(i < 5) {
     ++i;
     x = x*2.0;
   }

   (do-while (< i 5)
     (incf i)
     (setf x (* x 2d0)))
   ==>
   do {
     ++i;
     x = x*2.0;
   } while(i < 5);

8. Non-lower case terms can be supplied with Lisp strings, whereas
   symbols are converted to lowercase.

   (function "Hello" :int () (return 1)) ==> int Hello () {return 1;}

   Whereas

   (function Hello :int () (return 1)) ==> int hello () {return 1;}

   This also works with the defclcfun and defclckernel macros:

   (defclcfun "AddSomeNumbers" :int
       ((var x :int)
        (var y:int))
     (return (+ x y)))

   ("AddSomeNumbers" 1 2) ==> 3

   Whereas

   (AddSomeNumbers 1 2) ==> compilation error

9. Typecasting works using either the typecast or coerce operators:

   (typecast x :int) ==> ((int) x)
   (coerce x :int) ==> ((int) x)

10. Pointers have a type operator, pointer.  The address and value
    operators return the address or value of their argument:

    (var x :int 0)
    ;; create pointer variable to address of x
    (var px (pointer :int)
         (address x))
    ;; get value of x through the pointer px
    (value px)

11. Arrays are accessed with aref:

    (aref xs 5) ==> x[5]
    (aref ys 1 2 3) ==> y[1][2][3]

12. Structs are defined with defstruct:

    (defstruct mystruct
     (var x :double)
     (var y :int))
     ==>
     struct mystruct {
       double x;
       int y;
     };

    Members are accessed by member and pmember, member for struct
    objects and pmember for pointers to struct objects:

    (member s x) ==> s.x
    (pmember p x) ==> p->x

    Structs can also be defined in a similar way to functions via
    defclcstruct, which both defines a CFFI type accessible to Lisp
    and an OpenCL C struct of the same size and structure available on
    the OpenCL device.  The above example could be defined in Lisp via

    (defclcstruct mystruct
     (:x :double)
     (:y :int))

    and later referred to as a type in Lispified OpenCL C code, e.g.

    (var s (:struct mystruct))
    (setf (member s :x) 1d0) ; setf works with members

    with the definition for "mystruct" automatically included in the
    OpenCL C source string produced by the various compiler functions
    listed below.  Note that the (:struct structname) form must be
    used to refer to structs in Lispified OpenCL C, while support for
    bare structs in CFFI is deprecated but still possible.  Good
    practice is to use (:struct structname) for both.  See
    examples/struct.lisp.

    Also note that as stated below, #'clc de-packages symbols (except
    functions, kernels, and structs defined with defclcfun,
    defclckernel, and defclcstruct), so members can be defined and
    referred to using any package.  To match CFFI style, keyword
    symbols are recommended for defclcstruct, but in the above
    examples you can change the package of any of the slot names and
    the same OpenCL C code will result.

13. Macros can be defined with defclcmacro:

    (defclcmacro square (x)
      `(* ,x ,x))
    (square 2) ==> (* 2 2) ==> 2*2

    Macros have already been used to define complex+ and complex- in
    math.lisp, for example.

14. Complex numbers are supported by CFFI type (:struct cl_complex).
    Note that native Lisp complex numbers are automatically converted
    to and from CFFI, so this type can be used with the OpenCL
    functions and native complex data can be sent and received.  See
    examples/complex.lisp.

When in doubt, test a form with the #'clc function to see what OpenCL
C code it produces.

Note that the OpenCL C code generated has many technically unnecessary
parentheses and code blocks from the perspective of well-written C
code, but this is to ensure that all possible use cases from the Lisp
perspective lead to reasonable code.  You can have overly specific C
code generated from Lisp and be safe, but ambiguous code can lead to
strange problems, so I went with the safe route.

Note that symbols are essentially de-packaged from the perspective of
defclc and clc, as they are interned into the cl-opencl-utils package
before processing.  This allows the use of symbols from any package to
denote Lispified OpenCL C code, which is surprisingly inconvenient to
use when symbols are treated as if they belong to a single package and
you don't want to import all of cl-opencl-utils into the package
you're using.  E.g.,

(clc `(cl:+ 2 2)) ==> (2+2)
(clc `(some-package:+ 2 2)) ==> (2+2)

However, there are mechanisms for defining OpenCL C functions and
struct types, and these are package-dependent so as to allow different
utility libraries to define functions without worrying about clashes.
See notes on defclcfun, defclckernel, and defclcstruct.

OpenCL source code is still converted into strings and supplied to the
cl-opencl Lisp API functions like cl-create-program-with-source, but
there are a few options for how to generate the OpenCL C source code
from the Lispified code.  The following functions and accompanying
macros aid in this:

* program-source-from-forms-fn: Function to create source code from
  top-level forms of Lispified OpenCL C code.  You provide the
  Lispified code as arguments to this function directly, and any
  defined kernels or programs with the defclcfun and defclckernel
  macros are automatically added to the source code when they're
  referred to by the supplied code.

* program-source-from-forms: Macro version of the same where arguments
  aren't evaluated.

* program-source-from-kernels-fn: Creates source code for a program
  from just the kernel symbols supplied.  The definitions and
  necessary previously defined functions are found and included in the
  OpenCL C program generated.

* program-source-from-kernels: Macro version where the arguments
  aren't evaluated.

For example, a basic hello-world kernel might be produced via:

;; Kernel definition
(defclckernel hello
    ((var n (global (pointer :uint)))
     (var buf (global (pointer :uint))))
  (var gid :int (get-global-id 0))
  (when (< gid (value n))
    (setf (aref buf gid)
          gid)))

;; Make kernel
(let* ((platform (first (cl-get-platform-ids)))
       (device (first (cl-get-device-ids platform
                                         +CL-DEVICE-TYPE-ALL+)))
       (context (cl-create-context platform
                                   (list device)))
       (source
        (program-source-from-kernels hello))
       (program
        (cl-create-program-with-source context source)))
  (cl-build-program-with-log program
                             (list device))
  (cl-create-kernel program "hello"))

I recommend reading the examples at least once to get an idea of how
to use the utilities in a complementary way.
