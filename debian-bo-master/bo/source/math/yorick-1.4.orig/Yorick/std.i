/*
    STD.I
    Declarations of standard Yorick functions.

    $Id: std.i,v 1.1 1993/08/27 18:32:09 munro Exp munro $

    The Codger automatic code generator program uses this file to
    generate appropriate C code to initialize the various built-in
    functions declared here.
    This file is also used as online documentation for these functions
    by Yorick's help mechanism.

    The "extern" declaration of each function or variable is a no-op,
    but causes Yorick to place the variable in the sourceList for this
    include file, making it available for online help.  The DOCUMENT
    comment is provided in a standard format to simplify manipulation
    of such comments by programs other than Yorick;  it should immediately
    follow the corresponding "extern" so that it will be visible when
    the page containing the "extern" is displayed.

    The Codger code generator finds each "extern" line and creates
    initialization code binding the associated Yorick variable to
    either a BuiltIn function (see ydata.h) Y_variable, or, if a
    "reshape, variable, ..." declaration is found, to a global
    compiled variable y_variable with the compiled data type
    corresponding to the Yorick data type mentioned in the "reshape"
    command.  Codger can generate certain simple Y_variable wrapper
    routines if further information is provided in a PROTOTYPE comment.
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

extern help;
/* DOCUMENT help, topic
         or help
     Prints DOCUMENT comment from include file in which the variable
     TOPIC was defined, followed by the line number and filename.
     By opening the file with a text editor, you may be able to find
     out more, especially if no DOCUMENT comment was found.
     Examples:
       help, set_path
     prints the documentation for the set_path function.
       help
     prints the DOCUMENT comment you are reading.

     This copy of Yorick was launched from the directory:
     **** Y_LAUNCH (computed at runtime) ****
     Yorick's "site directory" at this site is:
     **** Y_SITE (computed at runtime) ****
     You can find out a great deal more about Yorick by browsing
     through these directories.  Begin with the site directory,
     and pay careful attention to the subdirectories doc/ (which
     contains documentation relating to Yorick), and include/ and
     contrib/ (which contain many examples of Yorick programs).
     Look for files called README (or something similar) in any
     of these directories -- they are intended to assist browsers.
     The site directory itself contains std.i and graph.i, which
     are worth reading.

     Type:
       help, dbexit
     for help on debug mode.  If your prompt is "dbug>" instead of
     ">", dbexit will return you to normal mode.

     Type:
       quit
     to quit Yorick.

   SEE ALSO: quit, info, print, copyright, warranty, legal
 */

local copyright, warranty;
/* DOCUMENT copyright, (no) warranty

     Copyright (c) 1996.  The Regents of the University of California.
                   All rights reserved.

     Yorick is provided "as is" without any warranty, either expressed or
     implied.  For a complete statement, type:

        legal

     at the Yorick prompt.

   SEE ALSO: legal
 */

func legal(void)
/* DOCUMENT legal
     Prints the legal details of Yorick's copyright, licensing,
     and lack of warranty.
   SEE ALSO: copyright, warranty
 */
{
  require, "legal.i";
  raw_legal;
}

func help_worker
/* xxDOCUMENT help_worker (Not for interactive use -- called by help.)
 */
{
  /* help_worker task is pushed by help function -- topic and file
     arguments are left in help_topic and help_file variables */
  topic= help_topic;   help_topic= [];
  file= help_file;     help_file= [];

  if (file) {
    mark= bookmark(file);
    line= rdline(file);

    if (typeof(topic)!="struct_definition") {
      /* non-struct looks for DOCUMENT comment before any blank lines */
      n= 10;   /* read at most 10 lines looking for DOCUMENT comment */
      while (strtok(line)(1) && n--) {
	if (strmatch(line, "/* DOCUMENT")) break;
	line= rdline(file);
      }
      if (strmatch(line, "/* DOCUMENT")) {
	do {
	  if (strmatch(line, "**** Y_LAUNCH (computed at runtime) ****"))
	    write, "      "+Y_LAUNCH;
	  else if (strmatch(line, "**** Y_SITE (computed at runtime) ****"))
	    write, "      "+Y_SITE;
	  else
	    write, line;
	  line= rdline(file);
	  if (!line) break;
	} while (!strmatch(line, "*/"));
	write, line;
      } else {
	write, "<DOCUMENT comment not found>";
      }

    } else {
      /* struct just prints definition */
      gotopen= 0;
      do {
	if (!gotopen) gotopen= strmatch(line, "{");
	write, line;
	if (gotopen && strmatch(line, "}")) break;
      } while (line= rdline(file));
    }

    write, "defined at:"+print(mark)(2);

  } else {
    write, "<not defined in an include file, running info function>";
    info, topic;
  }
}

func info(topic)
/* DOCUMENT info, expr
     prints the data type and array dimensions of EXPR.
   SEE ALSO: help, print
 */
{
  if (is_array(topic)) {
    void= use_origins(1);  /* assure NON-forced origin */
    line= "array(" + nameof(structof(topic));
    dims= dimsof(topic);
    orgs= orgsof(topic);
    ndims= dims(1)+1;
    for (i=2 ; i<=ndims ; i++) {
      line+= ",";
      if (orgs(i)!=1)
	line+= print(orgs(i))(1)+":"+print(orgs(i)+dims(i)-1)(1);
      else
	line+= print(dims(i))(1);
    }
    line+= ")";
    write, line;
  } else {
    print, topic;
  }
}

/*--------------------------------------------------------------------------*/

extern quit;
/* DOCUMENT quit
     Exit YMainLoop when current task finishes.
     Normally this terminates the program.
 */

extern system;
/* DOCUMENT system, "shell command line"
     Passes the command line string to a shell for execution.
     If the string is constant, you may use the special syntax:
         $shell command line
     (A long command line may be continued by ending the line with \
     as usual.)  The system function syntax allows Yorick to compute
     parts of the command line string, while the simple $ escape
     syntax does not.  In either case, the only way to get output
     back from such a command is to redirect it to a file, then
     read the file.  Note that Yorick does not regain control
     until the subordinate shell finishes.  (Yorick will get control
     back if the command line backgrounds the job.)
     WARNING: If Yorick has grown to a large size, this may crash
     your operating system, since the underlying POSIX fork function
     first copies all of the running Yorick process before the exec
     function can start the shell.  See Y_SITE/sysafe.i for a fix.
   SEE ALSO: popen
 */

extern yorick_init;
/* xxDOCUMENT yorick_init
     Re-initializes all of the built-in functions for this version
     of Yorick.  To be used in desperation if you overwrite some
     critical built-in function by mistake.  Of course, if you
     redefine yorick_init, you won't be able to recover anything.
 */

extern set_path;
/* DOCUMENT set_path, "dir1:dir2:dir3:..."
         or set_path
     sets the include file search path to the specified list of
     directories.  The specified directories are searched left to
     right for include files specified as relative file names in
     #include directives, or to the include or require functions.
     If the argument is omitted, restores the default search path,
     ".:~/Yorick:Y_LAUNCH/include:Y_SITE/include:Y_SITE/contrib",
     where y_site is the main Yorick directory for this site.
     The Y_LAUNCH directory is the directory which contains the
     executable; this directory is omitted if it is the same as
     Y_SITE.

     Only the "end user" should ever call set_path, and then only in
     his or her custom.i file, for the purpose of placing a more
     elaborate set of personal directories containing Yorick procedures.
     For example, if someone else maintains Yorick code you use, you
     might put their ~/Yorick on your include path.

   SEE ALSO: Y_LAUNCH, Y_SITE, include, require
 */

extern set_site;
/* xxDOCUMENT set_site, site_directory
     sets Y_LAUNCH, Y_SITE as a side effect.  Should only be called from
     paths.i, and is never called by default.  See paths.i.  */

extern yorick_stats;
/* DOCUMENT yorick_stats
     returns an array of longs describing Yorick memory usage.
     For debugging.  See ydata.c source code.
 */

extern disassemble;
/* DOCUMENT disassemble(function)
         or disassemble, function
     Disassembles the specified function.  If called as a function, the
     result is returned as a vector of strings; if called as a subroutine,
     the disassembly is printed at the terminal.  If the function is nil,
     the current *main* program is disassembled -- you must include the
     call to disassemble in the main program, of course, NOT on its own
     line as a separate main program.
 */

extern reshape;
/* DOCUMENT reshape, reference, address, type, dimension_list
         or reshape, reference, type, dimension_list
         or reshape, reference
     The REFERENCE must be an unadorned variable, not an expression;
     reshape sets this variable to an LValue at the specified ADDRESS
     with the specified TYPE and DIMENSION_LIST.  (See the array
     function documentation for acceptable DIMENSION_LIST formats.)
     If ADDRESS is an integer (e.g.- a long), the programmer is
     responsible for assuring that the data at ADDRESS is valid.
     If ADDRESS is a (Yorick) pointer, Yorick will assure that the
     data pointed to will not be discarded, and the reshape will
     fail if TYPE and DIMENSION_LIST extend beyond the pointee
     bounds.  In the second form, ADDRESS is taken to be &REFERENCE;
     that is, the TYPE and DIMENSION_LIST of the variable are changed
     without doing any type conversion.  In the third form, REFERENCE
     is set to nil ([]).  (Simple redefinition will not work on a
     variable defined using reshape.)
     WARNING: There are almost no situations for which reshape is
       the correct operation.  See reform in Y_SITE/include/string.i.
  SEE ALSO: array, dimsof, numberof, is_array, eq_nocopy
 */

extern eq_nocopy;
/* DOCUMENT eq_nocopy, y, x
     is the same as
            y= x
     except that if x is an array, it is not copied, even if it is
     not a temporary (i.e.- an expression).  Having multiple variables
     reference the same data can be confusing, which is why the default
     = operation copies the array.  The most important use of eq_nocopy
     involves pointers or lists:
            y= *py
	    z= _car(list)
     always causes the data pointed to by py to be copied, while
            eq_nocopy, y, *py
	    eq_nocopy, z, _car(list)
     does not copy the data - often more nearly what you wanted.
     Note that scalar int, long, and double variables are always copied,
     so you cannot count on eq_nocopy setting up an "equivalence"
     between variables.
 */

/*--------------------------------------------------------------------------*/

extern array;
/* DOCUMENT array(value, dimension_list)
         or array(type, dimension_list)
     returns an object of the same type as VALUE, consisting of copies
     of VALUE, with the given DIMENSION_LIST appended to the dimensions
     of VALUE.  Hence, array(1.5, 3, 1) is the same as [[1.5, 1.5, 1.5]].
     In the second form, the VALUE is taken as scalar zero of the TYPE.
     Hence, array(short, 2, 3) is the same as [[0s,0s],[0s,0s],[0s,0s]].
     A DIMENSION_LIST is a list of arguments, each of which may be
     any of the following:
        (1) A positive scalar integer expression,
	(2) An index range with no step field (e.g.-  1:10), or
	(3) A vector of integers [number of dims, length1, length2, ...]
	    (that is, the format returned by the dimsof function).
  SEE ALSO: reshape, is_array, dimsof, numberof, grow, span, use_origins,
            _lst
 */

/*--------------------------------------------------------------------------*/

extern structof;
/* DOCUMENT structof(object)
     returns the data type of OBJECT, or nil for non-array OBJECTs.
     Use typeof(object) to get the ASCII name of a the data type.
  SEE ALSO: typeof, dimsof, numberof, sizeof, nameof
 */

extern dimsof;
/* DOCUMENT dimsof(object)
         or dimsof(object1, object2, ...)
     returns a vector of integers describing the dimensions of OBJECT.
     The format of the vector is [number of dims, length1, length2, ...].
     The orgsof function returns the origin of each dimension (normally 1).
     If more than one argument is given, dimsof returns the dimension
     list of the result of binary operations between all the objects,
     or nil if the objects are not conformable.
  SEE ALSO: typeof, structof, numberof, sizeof, orgsof
 */

extern orgsof;
/* DOCUMENT orgsof(object)
     returns a vector of integers describing the dimensions of OBJECT.
     The format of the vector is [number of dims, origin1, origin2, ...].
     By default, dimension origins are ignored, but use_origins changes
     this.  The dimsof function returns the length of each dimension.
     *** NOTE NOTE NOTE ***
     Unless use_origins(1) is in effect, orgsof will always return
     1 for all of the originI in the list.  Thus, whether use_origins(1)
     is in effect or not, you are guaranteed that x(orgsof(x)(2)) is the
     first element of x.
  SEE ALSO: dimsof, typeof, structof, numberof, sizeof, use_origins
 */

extern use_origins;
/* DOCUMENT dummy= use_origins(dont_force)
     Yorick array dimensions have an origin as well as a length.
     By default, this origin is 1 (like FORTRAN arrays, unlike C
     arrays).  However, the array function and the pseudo-index (-)
     can be used to produce arrays with other origins.

     Initially, the origin of an array index is ignored by Yorick; the
     first element of any array has index 1.  You can change this
     default behavior by calling use_origins with non-zero DONT_FORCE,
     and restore the default behavior by calling use_origins(0).

     When the returned object DUMMY is destroyed, either by return from
     the function in which it is a local variable, or by explicit
     redefintion of the last reference to it, the treatment of array
     index origins reverts to the behavior prior to the call to
     use_origins.  Thus, you can call use_origins at the top of a
     function and not worry about restoring the external behavior
     before every possible return (including errors).

  SEE ALSO: array, dimsof, orgsof
 */

extern sizeof;
/* DOCUMENT sizeof(object)
     returns the size of the object in bytes, or 0 for non-array objects.
     sizeof(structure_definition) returns the number of bytes per instance.
     sizeof(binary_file) returns the file size in bytes.
  SEE ALSO: dimsof, typeof, structof, numberof
 */

extern numberof;
/* DOCUMENT numberof(object)
     returns the number of elements if object is an array, or 0 if not.
  SEE ALSO: sizeof, dimsof, typeof, structof
 */

extern typeof;
/* DOCUMENT typeof(object)
     returns a string describing the type of object.  For the basic
     data types, these are "char", "short", "int", "long", "float",
     "double", "complex", "string", "pointer", "struct_instance",
     "void", "range", "struct_definition", "function", "builtin",
     "stream" (for a binary stream), and "text_stream".
  SEE ALSO: structof, dimsof, sizeof, numberof, nameof
 */

extern nameof;
/* DOCUMENT nameof(object)
     If OBJECT is a function or a structure definition, returns the
     name of the func or struct as it was defined (not necessarily
     the name of the variable passed to the nameof function).
  SEE ALSO: typeof
 */

/*--------------------------------------------------------------------------*/

extern print;
/* DOCUMENT print, object1, object2, object3, ...
         or print(object1, object2, object3, ...)
     prints an ASCII representation of the OBJECTs, in roughly the format
     they could appear in Yorick source code.  When invoked as a subroutine
     (in the first form), output is to the terminal.  When invoked as a
     function (int the second form), the output is stored as a vector of
     strings, one string per line that would have been output.
     Printing a structure definition prints the structure definition;
     printing a function prints its "func" definition; printing files,
     bookmarks, and other objects generally provides some sort of
     useful description of the object.
  SEE ALSO: pr1, print_format, write, exit, error, nameof, typeof
 */

func pr1(x)
/* DOCUMENT pr1(x)
     returns text representing expression X, equivalent to print(X)(1).
   SEE ALSO: print, swrite
 */
{ return print(x)(1); }

extern print_format;
/* DOCUMENT print_format, line_length, char=, short=, int=, float=,
                          double=, complex=, pointer=
     sets the format string the print function will use for each of
     the basic data types.  Yorick format strings are the same as the
     format strings for the printf function defined in the ANSI C standard.
     The default strings may be restored individually by setting the
     associated format string to ""; all defaults are restored if
     print_format is invoked with no arguments.  The default format strings
     are:  "0x%02x", "%d", "%d", "%ld", "%g", "%g", and "%g%+gi".
     Note that char and short values are converted to int before being
     passed to printf, and that float is converted to double.
     If present, an integer positional argument is taken as the line
     length; <=0 restores the default line length of 80 characters.
  SEE ALSO: print, write, nameof, typeof
 */

/*--------------------------------------------------------------------------*/

extern is_array;
/* DOCUMENT is_array(object)
     returns 1 if OBJECT is an array data type (as opposed to a function,
     structure definition, index range, I/O stream, etc.), else 0.
     An array OBJECT can be written to or read from a binary file;
     non-array Yorick data types cannot.
  SEE ALSO: is_func, is_void, is_range, is_struct, is_stream
 */

extern is_func;
/* DOCUMENT is_func(object)
     returns 1 if OBJECT is a Yorick interpreted function, 2 if OBJECT
     is a built-in (that is, compiled) function, else 0.
  SEE ALSO: is_array, is_void, is_range, is_struct, is_stream
 */

extern is_void;
/* DOCUMENT is_void(object)
     returns 1 if OBJECT is nil (the one instance of the void data type),
     else 0.
  SEE ALSO: is_array, is_func, is_range, is_struct, is_stream
 */

extern is_range;
/* DOCUMENT is_range(object)
     returns 1 if OBJECT is an index range (e.g.-  3:5 or 11:31:2),
     else 0.
  SEE ALSO: is_array, is_func, is_void, is_struct, is_stream
 */

extern is_struct;
/* DOCUMENT is_struct(object)
     returns 1 if OBJECT is the definition of a Yorick struct, else 0.
     Thus, is_struct(double) returns 1, but is_struct(1.0) returns 0.
  SEE ALSO: is_array, is_func, is_void, is_range, is_stream
 */

extern is_stream;
/* DOCUMENT is_stream(object)
     returns 1 if OBJECT is a binary I/O stream (usually a file), else 0.
     The _read and _write functions work on object if and only if
     is_stream returns non-zero.  Note that is_stream returns 0 for a
     text stream -- you need the typeof function to test for those.
  SEE ALSO: is_array, is_func, is_void, is_range, is_struct
 */

/*--------------------------------------------------------------------------*/

extern am_subroutine;
/* DOCUMENT am_subroutine()
     returns 1 if the current Yorick function was invoked as a subroutine,
     else 0.  If am_subroutine() returns true, the result of the current
     function will not be used, and need not be computed (the function
     has been called for its side effects only).
 */

/*--------------------------------------------------------------------------*/

extern sin;
extern cos;
extern tan;
/* DOCUMENT sin(x)
            cos(x)
            tan(x)
     returns the sine, cosine, or tangent of its argument,
     which is in radians.
  SEE ALSO: asin, acos, atan
 */

extern asin;
/* DOCUMENT asin(x)
     returns the inverse sine of its argument, range [-pi/2, pi/2].
  SEE ALSO: sin, cos, tan, asin, acos, atan
 */

extern acos;
/* DOCUMENT acos(x)
     returns the inverse cosine of its argument, range [0, pi].
  SEE ALSO: sin, cos, tan, asin, acos, atan
 */

extern atan;
/* DOCUMENT atan(x)
         or atan(y, x)
     returns the inverse tangent of its argument, range [-pi/2, pi/2].
     In the two argument form, returns the angle from (1, 0) to (x, y),
     in the range (-pi, pi], with atan(1, 0)==pi/2.  (If x>=0, this is
     the same as atan(y/x).)
  SEE ALSO: sin, cos, tan, asin, acos, atan
 */

local pi;
/* DOCUMENT pi
     roughly 3.14159265358979323846264338327950288
 */
pi= 4.0*atan(1.0);    /* to double precision on this machine */

extern sinh;
extern cosh;
extern tanh;
/* DOCUMENT sinh(x)
            cosh(x)
            tanh(x)
     returns the hyperbolic sine, cosine, or tangent of its argument.
  SEE ALSO: sech, csch, asinh, acosh, atanh
 */

func sech(x) { y= tanh(x);  return sqrt(1.0-y*y); }
func csch(x) { y= 1.0/tanh(x);  return sqrt(y*y-1.0); }
/* DOCUMENT sech(x)
            csch(x)
     returns the hyperbolic secant (1/cosh) or cosecant (1/sinh) of
     its argument, without overflowing for large x.
  SEE ALSO: sinh, cosh, tanh, asinh, acosh, atanh
 */

func asinh(x) { return log(x+sqrt(x*x+1.0)); }
func acosh(x) { return log(x+sqrt(x*x-1.0)); }
func atanh(x) { return 0.5*log((1.0+x)/(1.0-x)); }
/* DOCUMENT asinh(x)
            acosh(x)
            atanh(x)
     returns the inverse hyperbolic sine, cosine, or tangent of
     its argument.  The range of acosh is >=0.0.
  SEE ALSO: sinh, cosh, tanh, sech, csch
 */

extern exp;
/* DOCUMENT exp(x)
     returns the exponential function of its argument (inverse of log).
  SEE ALSO: log, log10, sinh, cosh, tanh, sech, csch
 */

extern log;
/* DOCUMENT log(x)
     returns the natural logarithm of its argument (inverse of exp).
  SEE ALSO: log10, exp, asinh, acosh, atanh
 */

extern log10;
/* DOCUMENT log10(x)
     returns the base 10 logarithm of its argument (inverse of 10^x).
  SEE ALSO: log, exp, asinh, acosh, atanh
 */

extern sqrt;
/* DOCUMENT sqrt(x)
     returns the square root of its argument.
  SEE ALSO: abs, also note the rms range function
 */

extern poly;
/* DOCUMENT poly(x, a0, a1, a2, ..., aN)
     returns the polynomial  A0 + A1*x + A2*x^2 + ... + AN*X^N
     The data type and dimensions of the result, and conformability rules
     for the inputs are identical to those for the expression.
 */

extern ceil;
/* DOCUMENT ceil(x)
     returns the smallest integer not less than x (no-op on integers).
  SEE ALSO: floor
 */

extern floor;
/* DOCUMENT floor(x)
     returns the largest integer not greater than x (no-op on integers).
  SEE ALSO: ceil
 */

extern abs;
/* DOCUMENT abs(x)
         or abs(x, y, z, ...)
     returns the absolute value of its argument.
     In the multi-argument form, returns sqrt(x^2+y^2+z^2+...).
  SEE ALSO: sign, sqrt, also note the rms range function
 */

extern sign;
/* DOCUMENT sign(x)
     returns algebraic sign of it argument, or closest point on the
     unit circle for complex x.  Guaranteed that x==sign(x)*abs(x).
     sign(0)==+1.
  SEE ALSO: abs
 */

extern conj;
/* DOCUMENT conj(z)
     returns the complex conjugate of its argument.
 */

local re_part;
/* DOCUMENT re_part(z)
     returns the real part of its argument.  (Same as double(z).)
     Unlike z.re, works if z is not complex.
 */
re_part= double;

func im_part
/* DOCUMENT im_part(z)
     returns the imaginary part of its argument.
     Unlike z.im, works if z is not complex (returns zero).
 */
{
  return (structof(z)==complex)? z.im : array(0.0, dimsof(z));
}

extern random;
extern random_seed;
/* DOCUMENT random(dimension_list)
            random_seed, seed
     returns an array of random double values with the given
     DIMENSION_LIST (nil for a scalar result), uniformly distributed
     on the interval from 0.0 to 1.0.
     The algorithm is from Press and Teukolsky, Computers in Physics,
     vol. 6, no. 5, Sep/Oct 1992 (ran2).  They offer a reward of $1000
     to anyone who can exhibit a statistical test that this random
     number generator fails in a "non-trivial" way.
     The random_seed call reinitializes the random number sequence;
     SEED should be between 0.0 and 1.0 non-inclusive; if SEED is
     omitted, nil, or out of range, the sequence is reinitialized as
     when Yorick starts.
     The numbers are actually at the centers of 2147483562 equal width
     bins on the interval [0,1].  Although only these 2 billion numbers
     are possible, the period of the generator is roughly 2.3e18.

   SEE ALSO: randomize
 */

func randomize(void)
/* DOCUMENT randomize
            randomize()
     set the seed for random "randomly" (based on the timer clock
     and the current state of random).  As a function, returns the
     value of the seed passed to random_seed.

   SEE ALSO: random, random_seed
 */
{
  seed= array(0., 3);
  timer, seed;
  seed= pi*sum(abs(seed));
  while (seed > 0.9) seed*= 0.1;
  seed+= 0.05;
  random_seed, seed;
  return seed;
}

/*--------------------------------------------------------------------------*/

extern min;
/* DOCUMENT min(x)
         or min(x, y, z, ...)
     returns the scalar minimum value of its array argument, or, if
     more than one argument is supplied, returns an array of the
     minimum value for each array element among the several arguments.
     In the multi-argument case, the arguments must be conformable.
  SEE ALSO: max, sum, avg
 */

extern max;
/* DOCUMENT max(x)
         or max(x, y, z, ...)
     returns the scalar maximum value of its array argument, or, if
     more than one argument is supplied, returns an array of the
     maximum value for each array element among the several arguments.
     In the multi-argument case, the arguments must be conformable.
  SEE ALSO: min, sum, avg
 */

extern sum;
/* DOCUMENT sum(x)
     returns the scalar sum of all elements of its array argument.
  SEE ALSO: avg, min, max
 */

extern avg;
/* DOCUMENT avg(x)
     returns the scalar average of all elements of its array argument.
  SEE ALSO: sum, min, max
 */

extern allof;
extern anyof;
extern noneof;
extern nallof;
/* DOCUMENT allof(x)
            anyof(x)
            nallof(x)
            noneof(x)
     Respectively:
      returns 1 if every element of the array x is non-zero, else 0.
      returns 1 if at least one element of the array x is non-zero, else 0.
      returns 1 if at least one element of the array x is zero, else 0.
      returns 1 if every element of the array x is zero, else 0.
  SEE ALSO: allof, anyof, noneof, nallof, where, where2
 */

extern where;
/* DOCUMENT where(x)
     returns the vector of longs which is the index list of non-zero
     values in the array x.  Thus, where([[0,1,3],[2,0,4]]) would
     return [2,3,4,6].  If noneof(x), where(x) is a special range
     function which will return a nil value if used to index an array;
     hence, if noneof(x), then x(where(x)) is nil.
     If x is a non-zero scalar, then where(x) returns a scalar value.
     The rather recondite behavior for scalars and noneof(x) provides
     maximum performance when the merge function to be used with the
     where function.
  SEE ALSO: where2, merge, merge2 allof, anyof, noneof, nallof, sort
 */

func where2(x)
/* DOCUMENT where2(x)
     like where(x), but the returned list is decomposed into indices
     according to the dimensions of x.  The returned list is always
     2 dimensional, with the second dimension the same as the dimension
     of where(x).  The first dimension has length corresponding to the
     number of dimensions of x.  Thus, where2([[0,1,3],[2,0,4]]) would
     return [[2,1],[3,1],[1,2],[3,2]].
     If noneof(x), where2 returns [] (i.e.- nil).
  SEE ALSO: where, merge, merge2, allof, anyof, noneof, nallof, sort
 */
{
  w= where(x);
  /* Since the result of where2 cannot be used as an index list, the
     case noneof(x) can be disposed of more easily than with where.  */
  if (!is_array(w)) return [];
  d= dimsof(x);
  n= d(1);
  if (!n) return w;  /* catcall for passing a scalar */
  d= d(2:);
  o= orgsof(x)(2:);
  w2= w(-:1:n,);
  w-= o(1);
  for (i=1 ; i<=n ; i++) {
    w2(i,)= w%d(i) + o(i);
    w/= d(i);
  }
  return w2;
}

extern merge;
/* DOCUMENT merge(true_expr, false_expr, condition)
     returns the values TRUE_EXPR or FALSE_EXPR where CONDITION is
     non-zero or zero, respectively.  The result has the data type of
     TRUE_EXPR or FALSE_EXPR, promoted to the higher arithmetic type
     if necessary.  The result has the dimensions of CONDITION.
     The number of elements in TRUE_EXPR must match the number of
     non-zero elements of CONDITION, and the number of elements in
     FALSE_EXPR must match the number of zero elements of CONDITION.
     (TRUE_EXPR or FALSE_EXPR should be nil if there are no such
     elements of CONDITION.  Normally, TRUE_EXPR and FALSE_EXPR should
     be 1-D arrays if they are not nil.)
     This function is intended for vectorizing a function whose
     domain is divided into two or more parts, as in:
        func f(x) {
	  big= (x>=threshhold);
	  wb= where(big);
	  ws= where(!big);
	  if (is_array(wb)) {
	    xx= x(wb);
	    fb= <function of xx>
	  }
	  if (is_array(ws)) {
	    xx= x(ws);
	    fs= <function of xx>
	  }
	  return merge(fb, fs, big);
	}
   SEE ALSO: merge2, where
 */

func merge2(t, f, c)
/* DOCUMENT merge2(true_expr, false_expr, condition)
     returns the values TRUE_EXPR or FALSE_EXPR where CONDITION is
     non-zero or zero, respectively.  The result has the data type of
     TRUE_EXPR or FALSE_EXPR, promoted to the higher arithmetic type
     if necessary.  Unlike the merge function, TRUE_EXPR and FALSE_EXPR
     must be conformable with each other, and with the CONDITION.
   SEE ALSO: merge2, where
 */
{
  dims= dimsof(t, f, c);
  if (dims(1)) {
    c+= array(structof(c), dims);
    tt= array(structof(t), dims);  tt(..)= t;
    ff= array(structof(f), dims);  ff(..)= f;
  } else {
    tt= t;
    ff= f;
  }
  return merge(tt(where(c)), ff(where(!c)), c);
}

/*--------------------------------------------------------------------------*/

extern grow;
extern _;
/* DOCUMENT grow, x, xnext1, xnext2, ...
         or grow(x, xnext1, xnext2, ...)
	 or    _(x, xnext1, xnext2, ...)
     lengthens the array X by appending XNEXT1, XNEXT2, etc. to its
     final dimension.  If X is nil, X is first redefined to the first
     non-nil XNEXT, and the remainder of the XNEXT list is processed
     normally.  Each XNEXT is considered to have the same number of
     dimensions as X, by appending unit-length dimensions if necessary.
     All but this final dimension of each XNEXT must be right-conformable
     (that is, conformable in the sense of the right hand side of an
     assignment statement) with all but the final dimension of X.
     The result has a final dimension which is the sum of the final
     dimension of X and all the final dimensions of the XNEXT.  Nil
     XNEXT are ignored.  The value of the result is obtained by
     concatenating all the XNEXT to X, after any required broadcasts.

     If invoked as a function, grow returns the new value of X; in
     this case, X may be an expression.  X must be a simple variable
     reference for the subroutine form of grow; otherwise there is
     nowhere to return the result.  The subroutine form is slightly
     more efficient than the function form for the common usage:
          x= grow(x, xnext1, xnext2)           is the same as
	  grow, x, xnext1, xnext2              the preferred form

     The _ function is a synonym for grow, for people who want this
     operator to look like punctuation in their source code, on analogy
     with the array building operator [a, b, c, ...].

     The _cat function is sometimes more appropriate than grow.

   SEE ALSO: _cat, array, compare with the array building operator [...]
 */

extern indgen;
/* DOCUMENT indgen(n)
         or indgen(start:stop)
         or indgen(start:stop:step)
     returns "index generator" list -- an array of longs running from
     1 to N, inclusive.  In the second and third forms, the index
     values specified by the index range are returned.
   SEE ALSO: span, spanl, array
 */

extern span;
/* DOCUMENT span(start, stop, n)
         or span(start, stop, n, which)
     returns array of N doubles equally spaced from START to STOP.
     The START and STOP arguments may themselves be arrays, as long as
     they are conformable.  In this case, the result will have one
     dimension of length N in addition to dimsof(START, STOP).
     By default, the result will be N-by-dimsof(START, STOP), but
     if WHICH is specified, the new one of length N will be the
     WHICHth.  WHICH may be non-positive to position the new
     dimension relative to the end of dimsof(START, STOP); in
     particular WHICH of 0 produces a result with dimensions
     dimsof(START, STOP)-by-N.
   SEE ALSO: spanl, indgen, array
 */

func spanl(start, stop, n, which)
/* DOCUMENT spanl(start, stop, n)
         or spanl(start, stop, n, which)
     similar to the span function, but the result array have N points
     spaced at equal ratios from START to STOP (that is, equally
     spaced logarithmically).  See span for discussion of WHICH argument.
     START and STOP must have the same algebraic sign for this to make
     any sense.
   SEE ALSO: span, indgen, array
 */
{
  return exp(span(log(abs(start)), log(abs(stop)), n,
		  (is_void(which)? 1 : which)))*sign(start);
}

extern digitize;
/* DOCUMENT digitize(x, bins)
     returns an array of longs with dimsof(X), and values i such that
     BINS(i-1) <= X < BINS(i) if BINS is monotonically increasing, or
     BINS(i-1) > X >= BINS(i) if BINS is monotonically decreasing.
     Beyond the bounds of BINS, returns either i=1 or i=numberof(BINS)+1
     as appropriate.
   SEE ALSO: histogram, interp, integ, sort, where, where2
 */

extern histogram;
/* DOCUMENT histogram(list)
         or histogram(list, weight)
     returns an array hist which counts the number of occurrences of each
     element of the input index LIST, which must consist of positive
     integers (1-origin index values into the result array):
          histogram(list)(i) = number of occurrences of i in LIST

     A second argument WEIGHT must have the same shape as LIST; the result
     will be the sum of WEIGHT:
          histogram(list)(i) = sum of all WEIGHT(j) where LIST(j)==i

     The result of the single argument call will be of type long; the
     result of the two argument call will be of type double (WEIGHT is
     promoted to that type).  The input argument(s) may have any number
     of dimensions; the result is always 1-D.

   KEYWORD: top=max_list_value
     By default, the length of the result is max(LIST).  You may
     specify that the result have a larger length by means of the TOP
     keyword.  (Elements beyond max(LIST) will be 0, of course.)

   SEE ALSO: digitize, sort
 */

extern interp;
/* DOCUMENT interp(y, x, xp)
         or interp(y, x, xp, which)
     returns yp such that (XP, yp) lies on the piecewise linear curve
     (X(i), Y(i)) (i=1, ..., numberof(X)).  Points beyond X(1) are set
     to Y(1); points beyond X(0) are set to Y(0).  The array X must be
     one dimensional, have numberof(X)>=2, and be either monotonically
     increasing or monotonically decreasing.  The array Y may have more
     than one dimension, but dimension WHICH must be the same length as
     X.  WHICH defaults to 1, the first dimension of Y.  WHICH may be
     non-positive to count dimensions from the end of Y; a WHICH of 0
     means the final dimension of Y.  The result yp has dimsof(XP)
     in place of the WHICH dimension of Y (if XP is scalar, the WHICH
     dimension is not present).  (The dimensions of the result are the
     same as if an index list with dimsof(XP) were placed in slot
     WHICH of Y.)
   SEE ALSO: integ, digitize, span
 */

extern integ;
/* DOCUMENT integ(y, x, xp)
         or integ(y, x, xp, which)
     See the interp function for an explanation of the meanings of the
     arguments.  The integ function returns ypi which is the integral
     of the piecewise linear curve (X(i), Y(i)) (i=1, ..., numberof(X))
     from X(1) to XP.  The curve (X, Y) is regarded as constant outside
     the bounds of X.  Note that X must be monotonically increasing or
   SEE ALSO: interp, digitize, span
 */

extern sort;
/* DOCUMENT sort(x)
         or sort(x, which)
     returns an array of longs with dimsof(X) containing index values
     such that X(sort(X)) is a monotonically increasing array.  X can
     contain integer, real, or string values.  If X has more than one
     dimension, WHICH determines the dimension to be sorted.  The
     default WHICH is 1, corresponding to the first dimension of X.
     WHICH can be non-positive to count dimensions from the end of X;
     in particular a WHICH of 0 will sort the final dimension of X.

     WARNING: The sort function is non-deterministic if some of the
              values of X are equal, because the Quick Sort algorithm
	      involves a random selection of a partition element.

     For information on sorting with multiple keys (and on making
     sort deterministic), type the following:
        #include "msort.i"
        help, msort

   SEE ALSO: median, digitize, interp, integ, histogram
 */

func median(x, which)
/* DOCUMENT median(x)
         or median(x, which)
     returns the median of the array X.  The search for the median takes
     place along the dimension of X specified by WHICH.  WHICH defaults
     to 1, meaning the first index of X.  The median function returns an
     array with one fewer dimension than its argument X (the WHICH
     dimension of X is missing in the result), in exact analogy with
     rank reducing index range functions.  If dimsof(X)(WHICH) is
     odd, the result will have the same data type as X; if even, the
     result will be a float or a double, since the median is defined
     as the arithmetic mean between the two central values in that
     case.
   SEE ALSO: sort, built-in index range functions such as avg, rms
 */
{
  if (is_void(which)) which= 1;
  list= sort(x, which);
  dims= dimsof(x);
  if (which<1) which= dims(1)-which;
  n= dims(1+which);
  odd= n%2;
  n/= 2;         /* index with half above, half below... */
  n+= 1;         /* ...corrected for 1-origin */
  stride= 1;
  for (i=1 ; i<which ; i++) stride*= dims(1+i);
  ldims= dims(1)-which+1;
  /**/ local l;
  reshape, l, &list, long, stride, grow(ldims, dims(1+which:));
  lm= l(,n,..);
  if (which<dims(1)) dims(1+which:-1)= dims(2+which:0);
  --dims(1);
  reshape, lm, long, dims;
  xm= x(lm);
  if (!odd) {     /* even length dimensions have more complicated median */
    reshape, lm;  /* undefine the LValue lm so following define works */
    lm= l(,n-1,..);
    reshape, lm, long, dims;
    xm= 0.5f*(xm+x(lm));
  }
  return xm;
}

extern transpose;
/* DOCUMENT transpose(x)
         or transpose(x, permutation1, permutation2, ...)
     transpose the first and last dimensions of array X.  In the second
     form, each PERMUTATION specifies a simple permutation of the
     dimensions of X.  These permutations are compounded left to right
     to determine the final permutation to be applied to the dimensions
     of X.  Each PERMUTATION is either an integer or a 1D array of
     integers.  A 1D array specifies a cyclic permutation of the
     dimensions as follows: [3, 5, 2] moves the 3rd dimension to the
     5th dimension, the 5th dimension to the 2nd dimension, and the 2nd
     dimension to the 3rd dimension.  Non-positive numbers count from the
     end of the dimension list of X, so that 0 is the final dimension,
     -1 in the next to last, etc.  A scalar PERMUTATION is a shorthand
     for a cyclic permutation of all of the dimensions of X.  The value
     of the scalar is the dimension to which the 1st dimension will move.

     Examples:  Let x have dimsof(x) equal [6, 1,2,3,4,5,6] in order
        to be able to easily identify a dimension by its length. Then:
	dimsof(x)                          == [6, 1,2,3,4,5,6]
	dimsof(transpose(x))               == [6, 6,2,3,4,5,1]
        dimsof(transpose(x,[1,2]))         == [6, 2,1,3,4,5,6]
	dimsof(transpose(x,[1,0]))         == [6, 6,2,3,4,5,1]
	dimsof(transpose(x,2))             == [6, 6,1,2,3,4,5]
	dimsof(transpose(x,0))             == [6, 2,3,4,5,6,1]
	dimsof(transpose(x,3))             == [6, 5,6,1,2,3,4]
	dimsof(transpose(x,[4,6,3],[2,5])) == [6, 1,5,6,3,2,4]
 */

/*--------------------------------------------------------------------------*/

extern strlen;
/* DOCUMENT strlen(string_array)
     returns an long array with dimsof(STRING_ARRAY) containing the
     lengths of the strings.  The null string (0) is considered to
     have length 0, just like "".
   SEE ALSO: strmatch, strpart, strtok
 */

extern strtok;
/* DOCUMENT strtok(string_array, delimiters)
         or strtok(string_array)
     strips the first token off of each string in STRING_ARRAY.
     A token is delimited by any of the characters in the string
     DELIMITERS.  If DELIMITERS is blank, nil, or not given, the
     default DELIMITERS is " \t\n" (blanks, tabs, or newlines).
     The result is a string array ts with dimensions
     2-by-dimsof(STRING_ARRAY); ts(1,) is the first token, and
     ts(2,) is the remainder of the string (the character which
     terminated the first token will be in neither of these parts).
     The ts(2,) part will be 0 (i.e.- the null string) if no more
     characters remain after ts(1,); the ts(1,) part will be 0 if
     no token was present.  A STRING_ARRAY element may be 0, in
     which case (0, 0) is returned for that element.
   SEE ALSO: strmatch, strpart, strlen
 */

extern strmatch;
/* DOCUMENT strmatch(string_array, pattern)
         or strmatch(string_array, pattern, case_fold)
     returns an int array with dimsof(STRING_ARRAY) with 0 where
     PATTERN was not found in STRING_ARRAY and 1 where it was found.
     If CASE_FOLD is specified and non-0, the pattern match is
     insensitive to case, that is, an upper case letter will match
     the same lower case letter and vice-versa.
   SEE ALSO: strtok, strpart, strlen
 */

extern strpart;
/* DOCUMENT strpart(string_array, m:n)
      returns another string array with the same dimensions as
      STRING_ARRAY which consists of characters M through N of
      the original strings.  M and N are 1-origin indices; if
      M is omitted, the default is 1; if N is omitted, the default
      is the end of the string.  If M or N is non-positive, it is
      interpreted as an index relative to the end of the string,
      with 0 being the last character, -1 next to last, etc.
      Finally, the returned string will be shorter than N-M+1
      characters if the original doesn't have an Mth or Nth
      character, with "" (note that this is otherwise impossible)
      if neither an Mth nor an Nth character exists.  A 0
      is returned for any string which was 0 on input.
   SEE ALSO: strmatch, strtok, strlen
 */

/*--------------------------------------------------------------------------*/

extern open;
/* DOCUMENT f= open(filename)
         or f= open(filename, filemode)
         or f= open(filename, filemode, errmode)
     opens the file FILENAME according to FILEMODE (both are strings).
     If ERRMODE is non-nil and non-zero, fail by returning nil F,
     otherwise failure to open or create the file is a runtime error.

     To use ERRMODE to check for the existence of a file:
        if (open(filename,,1)) file_exists;
	else file_does_not_exist;

     The return value F is an IOStream (or just stream for short).  When
     the last reference to this return value is discarded, the file will
     be closed.  The file can also be explicitly closed with the close
     function.  The FILEMODE determines whether the file is to be
     opened in read, write, or update mode, and whether writes are
     restricted to the end-of-file (append mode).  FILEMODE also
     determines whether the file is opened as a text file or as a
     binary file.  FILEMODE can have the following values, which are
     the same as for the ANSI standard fopen function:
        "r"     - read only
        "w"     - write only, random access, existing file overwritten
	"a"     - write only, forced to end-of-file,
	        existing file preserved
	"r+"    - read/write, random access, existing file preserved
	"w+"    - read/write, random access, existing file overwritten
	"a+"    - read/write, reads random access,
	        writes forced to end-of-file, existing file preserved
	"rb"  "wb"  "ab"  "r+b"  "rb+"  "w+b"  "wb+"  "a+b"  "ab+"
	        without b means text file, with b means binary file
     The default FILEMODE is "r" -- open an existing text file for
     reading.

     The read and write functions perform I/O on text files.
     I/O to binary files may be performed explicitly using the save
     and restore functions, or implicitly by using the stream variable
     F as if it were a data structure instance (e.g.- f.x refers to
     variable x in the binary file f).
  SEE ALSO: create, close, read, write, rdline, bookmark, backup, popen
            rename, remove, save, restore
 */

extern popen;
/* DOCUMENT f= popen(command, mode)
     opens a pipe to COMMAND, which is executed as with the system
     function.  If MODE is 0, the returned file handle is open for
     reading, and you are reading the stdout produced by COMMAND.
     If MODE is 1, f is opened for writing and you are writing to
     the stdin read by COMMAND.
  SEE ALSO: open, system
 */

extern fflush;
/* DOCUMENT fflush, file
     flush the I/O buffers for the text file FILE.  (Binary files are
     flushed at the proper times automatically.)  You should only need
     this after a write, especially to a pipe.
   SEE ALSO: write, popen
 */

func create(filename)
/* DOCUMENT f= create(filename)
     is a synonym for       f= open(filename, "w")
     Creates a new text file FILENAME, destroying any existing file of
     that name.  Use the write function to write into the file F.
   SEE ALSO: write, close, open
 */
{ return open(filename, "w"); }

extern close;
/* DOCUMENT close, f
     closes the I/O stream F (returned earlier by the open function).
     If F is a simple variable reference (as opposed to an expression),
     the close function will set F to nil.  If F is the only reference
     to the I/O stream, then "close, f" is equivalent to "f= []".
     Otherwise, "close, f" will close the file (so that subsequent
     I/O operations will fail) and print a warning message about the
     outstanding ("stale") references.
  SEE ALSO: open, read, write, rdline, bookmark, backup, save, restore,
            rename, remove
 */

extern rename;
extern remove;
/* DOCUMENT rename, old_filename, new_filename
            remove filename
     rename or remove a file.
   SEE ALSO: open, close, openb, closeb
 */

extern read;
extern sread;
/* DOCUMENT n= read(f, format=fstring, obj1, obj2, ...)
         or n= read(prompt= pstring, format=fstring, obj1, obj2, ...)
	 or n= sread(source, format=fstring, obj1, obj2, ...)
     reads text from I/O stream F (1st form), or from the keyboard (2nd
     form), or from the string or string array SOURCE (3rd form),
     interprets it according to the optional FSTRING, and uses that
     interpretation to assign values to OBJ1, OBJ2, ...  If the input
     is taken from the keyboard, the optional prompt PSTRING (default
     "read> ") is printed before each line is read.  The Yorick write
     function does not interact with the read function -- writes are
     always to end-of-file, and do not affect the sequence of lines
     returned by read.  The backup (and bookmark) function is the
     only way to change the sequence of lines returned by read.

     There must be one non-supressed conversion specifier (see below)
     in FSTRING for each OBJ to be read; the type of the conversion
     specifier must generally match the type of the OBJ.  That is,
     an integer OBJ requires an integer specifier (d, i, o, u, or x)
     in FSTRING, a real OBJ requires a real specifier (e, f, or g),
     and a string OBJ requires a string specifier (s or []).  An OBJ
     may not be complex, a pointer, a structure instance, or any non-
     array Yorick object.  If FSTRING is not supplied, or if it has
     fewer conversion specifiers than the number of OBJ arguments,
     then Yorick supplies default specifiers ("%ld" for integers,
     "%lg" for reals, and "%s" for strings).  If FSTRING contains more
     specifiers than there are OBJ arguments, the part of FSTRING
     beginning with the first specifier with no OBJ is ignored.

     The OBJ may be scalar or arrays, but the dimensions of every OBJ
     must be identical.  If the OBJ are arrays, Yorick behaves as
     if the read were called in a loop numberof(OBJ1) times, filling
     one array element of each of the OBJ according to FSTRING on
     each pass through the loop.  (Note that this behavior includes
     the case of reading columns of numbers by a single call to read.)

     The return value N is the total number of scalar assignments
     which were made as a result of this call.  (If there were 4
     OBJ arguments, and each was an array with 17 elements, a return
     value of N==35 would mean the following:  The first 8 elements
     of OBJ1, OBJ2, OBJ3, and OBJ4 were read, and the 9th element of
     OBJ1, OBJ2, and OBJ3 was read.)  The read function sets any
     elements of the OBJ which were not read to zero -- hence,
     independent of the returned N, the all of the old data in the
     OBJ arguments is overwritten.

     The read or sread functions continue reading until either:
     (1) all elements of all OBJ have been filled, or (2) end-of-file
     (or end of SOURCE for sread) is reached ("input failure"), or
     (3) part of FSTRING or a conversion specifier supplied by
     default fails to match the source text ("matching failure").

     The FSTRING is composed of a series of "directives" which are
     (1) whitespace -- means to skip any amount of whitespace in the
         source text
     (2) characters other than whitespace and % -- must match the
         characters in the source text exactly, or matching failure
	 occurs and the read operation stops
     (3) conversion specifiers beginning with % and ending with a
         character specifying the type of conversion -- optionally
	 skip whitespace, then convert as many characters as
	 continue to "look like" the conversion type, possibly
	 producing a matching failure
     The conversion specifier is of the form %*WSC, where:
     * is either the character '*' or not present
       A specifier beginning with %* does not correspond to any of
       the OBJ; the converted value will be discarded.
     W is either a positive decimal integer specifying the maximum
       field width (not including any skipped leading whitespace),
       or not present if any number of characters up to end-of-line
       is acceptable.
     S is either one of the characters 'h', 'l', or 'L', or not
       present.  Yorick allows this for compatibility with the C
       library functions, but ignores it.
     C is a character specifying the type of conversion:
       d   - decimal integer
       i   - decimal, octal (leading 0), or hex (leading 0x) integer
       o   - octal integer
       u   - unsigned decimal integer (same as d for Yorick)
       x, X            - hex integer
       e, f, g, E, G   - floating point real
       s   - string of non-whitespace characters
       [xxx]   - (xxx is any sequence of characters) longest string
                 of characters matching those in the list
       [^xxx]  - longest string of characters NOT matching those in
                 the list (this is how you can extend %s to be
		 delimited by something other than whitespace)
       %   - the ordinary % character; complete conversion
             specification must be "%%"

     The read function is modeled on the ANSI standard C library
     fscanf and sscanf functions, but differs in several respects:
       (1) Yorick's read cannot handle the %c, %p, or %n conversion
           specifiers in FSTRING.
       (2) Yorick's read never results in a portion of a line
           being read -- any unused part of a line is simply discarded
	   (end FSTRING with "%[^\n]" if you want to save the trailing
	   part of an input line).
       (3) As a side effect of (2), there are some differences between
           fscanf and Yorick's read in how whitespace extending across
	   newlines is handled.
   SEE ALSO: rdline, write, open, close, bookmark, backup, save, restore,
             read_n
 */

extern rdline;
/* DOCUMENT rdline(f)
         or rdline(f, n, prompt= pstring)
     returns next line from stream F (stdin if F nil).  If N is non-nil,
     returns a string array containing the next N lines of F.  If
     end-of-file occurs, rdline returns nil strings.  If F is nil,
     uses the PSTRING to prompt for input (default "read> ").
   SEE ALSO: read, open, close, bookmark, backup, read_n
 */

func read_n(f, &n0, &n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9)
/* DOCUMENT read_n, f, n0, n1, n2, ...
     grabs the next numbers N0, N1, N2, ... from file F, skipping over
     any whitespace, comma, semicolon, or colon delimited tokens which
     are not numbers.  (Actually, only the first and last characters of
     the token have to look like a number -- 4xxx3 would be read as 4.)
     ***WARNING*** at most ten Ns are allowed
     The Ns can be arrays, provided all have the same dimensions.
   SEE ALSO: read, rdline
 */
{
  require, "readn.i";
  return raw_read_n(f, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9);
}

extern write;
extern swrite;
/* DOCUMENT n= write(f, format=fstring, linesize=l, obj1, obj2, ...)
            n= write(format=fstring, linesize=l, obj1, obj2, ...)
	 or strings= swrite(format=fstring, linesize=l, obj1, obj2, ...)
     writes text to I/O stream F (1st form), or to the terminal (2nd
     form), or to the STRINGS string array (3rd form), representing
     arrays OBJ1, OBJ2, ..., according to the optional FSTRING.  The
     optional linesize L defaults to 80 characters, and helps restrict
     line lengths when FSTRING is not given, or does not contain
     newline directives.  The write function always appends to the
     end of a text file; the position for a sequence of reads is
     not affected by intervening writes.

     There must be one conversion specifier (see below) in FSTRING for
     each OBJ to be written; the type of the conversion specifier must
     generally match the type of the OBJ.  That is, an integer OBJ
     requires an integer specifier (d, i, o, u, x, or c) in FSTRING,
     a real OBJ requires a real specifier (e, f, or g), a string OBJ
     requires the string specifier (s), and a pointer OBJ requires a
     the pointer specifier (p).  An OBJ may not be complex, a structure
     instance, or any non-array Yorick object.  If FSTRING is not
     supplied, or if it has fewer conversion specifiers than the
     number of OBJ arguments, then Yorick supplies default specifiers
     (" %8ld" for integers, " %14.6lg" for reals, " %s" for strings, and
     " %8p" for pointers).  If FSTRING contains more specifiers than
     there are OBJ arguments, the part of FSTRING beginning with the
     first specifier with no OBJ is ignored.

     The OBJ may be scalar or arrays, but the dimensions of the OBJ
     must be conformable.  If the OBJ are arrays, Yorick behaves as
     if he write were called in a loop dimsof(OBJ1, OBJ2, ...) times,
     writing one array element of each of the OBJ according to FSTRING
     on each pass through the loop.  The swrite function returns a
     string array with dimensions dimsof(OBJ1, OBJ2, ...).  The write
     function inserts a newline between passes through the array if
     the line produced by the previous pass did not end with a
     newline, and if the total number of characters output since the
     previous inserted newline, plus the number of characters about
     to be written on the current pass, would exceed L characters
     (L defaults to 80).  The write function returns the total
     number of characters output.

     The FSTRING is composed of a series of "directives" which are
     (1) characters other than % -- copied directly to output
     (2) conversion specifiers beginning with % and ending with a
         character specifying the type of conversion -- specify
	 how to convert an OBJ into characters for output
     The conversion specifier is of the form %FW.PSC, where:
     F is zero or more optional flags:
       - left justify in field width
       + signed conversion will begin with either + or -
         (space) signed conversion  will begin with either space or -
       # alternate form (see description of each type below)
       0 pad field width with leading 0s instead of leading spaces
     W is either a decimal integer specifying the minimum field width
       (padded as specified by flags), or not present to use the
       minimum number of characters required.
     .P is either a decimal integer specifying the precision of the
       result, or not present to get the default.  For integers, this
       is the number of digits to be printed (possibly forcing leading
       zeroes), and defaults to 1.  For reals, this is the number of
       digits after the decimal point, and defaults to 6.  For strings,
       this is the maximum number of characters to print, and defaults
       to infinity.
     S is either one of the characters 'h', 'l', or 'L', or not
       present.  Yorick allows this for compatibility with the C
       library functions, but ignores it.
     C is a character specifying the type of conversion:
       d, i  - decimal integer
       o     - octal integer (# forces leading 0)
       u     - unsigned decimal integer (same as d for Yorick)
       x, X            - hex integer (# forces leading 0x)
       f     - floating point real in fixed point notation
               (# forces decimal)
       e, E  - floating point real in scientific notation
       g, G  - floating point real in fixed or scientific notation
               depending on the value converted (# forces decimal)
       s   - string of ASCII characters
       c   - integer printed as corresponding ASCII character
       p   - pointer
       %   - the ordinary % character; complete conversion
             specification must be "%%"

     The write function is modeled on the ANSI standard C library
     fprintf and sprintf functions, but differs in several respects:
       (1) Yorick's write cannot handle the %n conversion specifier
           in FSTRING.
       (2) Yorick's write may insert additional newlines if the OBJ
           are arrays, to avoid extremely long output lines.
   SEE ALSO: print, exit, error, read, rdline, open, close, save, restore
 */

extern bookmark;
extern backup;
/* DOCUMENT backup, f
         or bmark= bookmark(f)
            ...
            backup, f, bmark
     back up the text stream F, so that the next call to the read
     function returns the same line as the previous call to read
     (note that you can only back up one line).  If the optional
     second argument BMARK is supplied, restores the state of the
     file F to its state at the time the bookmark function was
     called.
     After a matching failure in read, use the single argument form
     of backup to reread the line containing the matching failure.
   SEE ALSO: read, rdline, open, close
 */

extern include;
extern require;
/* DOCUMENT #include "yorick_source.i"
            require, filename
            include, filename
         or include, filename, now

     #include is a parser directive, not a Yorick statement.  Use it
     to read Yorick source code which you have saved in a file; the
     file yorick_source.i will be read one line at a time, exactly as
     if you had typed those lines at the keyboard.  The following
     directories are searched (in this order) to find yorick_source.i:

        .               (current working directory)
	~/Yorick        (your personal directory of Yorick functions)
	Y_SITE/include  (Yorick distribution library)
	Y_SITE/contrib  (contributed source at your site)

     To find out what is available in the Yorick/include directory,
     type:
         library
     You can also type
         Y_SITE
     to find the name of the site directory at your site, go to the
     include or contrib subdirectory, and browse through the *.i files.
     This is a good way to learn how to write a Yorick program.  Be
     alert for files like README as well.

     The require function checks to see whether FILENAME has already
     been included (actually whether any file with the same final
     path component has been included).  If so, require is a no-op,
     otherwise, the action is the same as the include function with
     NOW == 1.

     The include function causes Yorick to parse and execute FILENAME
     immediately.  The effect is similar to the #include parser
     directive, except the finding, parsing, and execution of FILENAME
     occurs at runtime.  If the NOW argument is given and positive,
     the include occurs immediately, if nil or 0, it occurs just before
     the next line would have been parsed.  If NOW is negative, the
     include file is pushed onto a stack, and will be popped off and
     parsed when all pending input has been processed.

     Unless you are writing a startup file, or have some truly bizarre
     technical reason for using the include function, use #include
     instead.  The functional form of include may involve recursive
     parsing, which you will not be able to understand without deep
     study.  Stick with #include.

   SEE ALSO: set_path, Y_SITE
 */

func library(void)
/* DOCUMENT library
     print the Y_SITE/include/README file at the terminal.
 */
{
  f= open(Y_SITE+"include/README");
  while ((line= rdline(f))) write, line;
}

/*--------------------------------------------------------------------------*/

extern cd;
/* DOCUMENT cd, directory_name
         or cd(directory_name)
     change current working directory to DIRECTORY_NAME, returning
     the expanded path name (i.e.- with leading environment variables,
     ., .., or ~ replaced by the actual pathname).  If called as a
     function, returns nil to indicate failure, otherwise failure
     causes a Yorick error.
   SEE ALSO: get_cwd, get_home, get_env, get_argv
 */

extern get_cwd;
extern get_home;
/* DOCUMENT get_cwd()
         or get_home()
     returns the pathname of the current working directory or of your
     home directory.
   SEE ALSO: cd, get_env, get_argv
 */

extern get_env;
/* DOCUMENT get_env(environment_variable_name)
     returns the environment variable (a string) associated with
     ENVIRONMENT_VARIABLE_NAME (calls ANSI getenv routine).
   SEE ALSO: cd, get_cwd, get_home, get_env, get_argv
 */

extern get_argv;
/* DOCUMENT get_argv()
     returns string array containing the argv from the command line.
     The -batch and batch_include.i arguments are removed (not returned).
   SEE ALSO: process_argv, cd, get_cwd, get_home, get_env, batch
 */

func process_argv(msg)
/* DOCUMENT remaining= process_argv()
       -or- remaining= process_argv("your startup message")
     Performs standard command line processing.  This function is
     invoked by the default custom.i file (in $Y_HOME/include); you
     can also invoke it from your personal ~/Yorick/custom.i file.
     The process_argv calls get_argv, removes any arguments of
     the form "-ifilename" or "-i filename" (the latter is a pair of
     arguments.  It returns any arguments not of this form as its
     result, after including any filenames it found in the order
     they appeared on the command line.
     The optional string argument may be an array of strings to print
     a multi-line message.

     A Yorick package may define the function get_command_line in
     order to feed process_argv something other than get_argv.

   SEE ALSO: batch
 */
{
  if (is_void(get_command_line)) command_line= get_argv();
  else command_line= get_command_line();
  if (numberof(command_line)>=2) {
    command_line= command_line(2:);
    mask= strmatch(strpart(command_line, 1:2), "-i");
    list= where(mask);
    n= numberof(list);
    for (i=1 ; i<=n ; i++) {
      file= strpart(command_line(list(i)), 3:);
      if (file=="") {
	if (list(i)==numberof(command_line)) break;  /* ignore trailing -i */
	file= command_line(list(i)+1);
	mask(list(i)+1)= 1;
      }
      include, file;
    }
    command_line= command_line(where(!mask));
  } else {
    command_line= [];
  }
  if (numberof(command_line)<1 || noneof(command_line=="-q")) {
    if (is_void(msg))
      v= Y_VERSION;
      msg= [
" Copyright (c) 1996.  The Regents of the University of California.",
" All rights reserved.  Yorick "+v+" ready.  For help type 'help'"];
    write, msg, format="%s\n";
  } else {
    command_line= command_line(where(command_line!="-q"));
  }
  return command_line;
}

/*--------------------------------------------------------------------------*/

func openb(filename, clogfile, update, open102=)
/* DOCUMENT file= openb(filename)
         or file= openb(filename, clogfile)
     open the existing file FILENAME for read-only binary I/O.
     (Use updateb or createb, respectively, to open an existing file
      with read-write access or to create a new file.)
     If the CLOGFILE argument is supplied, it represents the structure
     of FILENAME in the Clog binary data description language.
     After an openb, the file variable may be used to extract variables
     from the file as if it were a structure instance.  That is, the
     expression "file.var" refers to the variable "var" in file "file".
     A complete list of the variable names present in the file may
     be obtained using the get_vars function.  If the file contains
     history records, the jt and jc functions may be used to set the
     current record -- initially, the first record is current.
     The restore function may be used to make memory copies of data
     in the file; this will be faster than a large number of
     references to "file.var".
   SEE ALSO: updateb, createb, open, cd
             show, jt, jc, restore
             get_vars, get_times, get_ncycs, get_member, has_records
             set_blocksize, dump_clog, read_clog, recover_file
	     openb_hooks, open102, close102, get_addrs
 */
{
  f= open(filename, (update? "r+b" : "rb"));
  if (!is_void(clogfile)) return read_clog(f, clogfile);
  if (!is_void(open102)) yPDBopen= ((open102&3)|(at_pdb_open&~3));
  else yPDBopen= at_pdb_open;
  for (hooks=openb_hooks ; hooks ; hooks=_cdr(hooks)) {
    if (_car(hooks)(f)) continue;
    if (has_records(f)) edit_times, f;  /* force increasing times */
    return f;
  }
  return [];
}

func show(f, pat)
/* DOCUMENT show, f
         or show, f, pat
         or show, f, 1
     prints a summary of the variables contained in binary file F.
     If there are too many variables, use the second form to select
     only those variables whose first few characters match PAT.
     In the third form, continues the previous show command where it
     left off -- this may be necessary for files with large numbers of
     variables.
     The variables are printed in alphabetical order down the columns.
     The print function can be used to obtain other information about F.
   SEE ALSO: openb, jt, jc
 */
{
  require, "show.i";
  raw_show, f, pat;
}

func collect(f, name)
/* DOCUMENT result= collect(f, name_string)
     scans through all records of the history file F accumulating the
     variable NAME_STRING into a single array with one additional
     index varying from 1 to the number of records.

     NAME_STRING can be either a simple variable name, or a name
     followed by up to four simple indices which are either nil, an
     integer, or an index range with constant limits.  (Note that
     0 or negative indices count from the end of a dimension.)

     Examples:
        collect(f, "xle")        -- collects the variable f.xle
	collect(f, "tr(2,2:)")   -- collects f.tr(2,2:)
	collect(f, "akap(2,-1:0,)") -- collects f.akap(2,-1:0,)
	             (i.e.- akap in the last two values of its
		            second index)

   SEE ALSO: get_times
 */
{
  require, "collec.i";
  return raw_collect(f, name);
}

extern get_member;
/* DOCUMENT get_member(f_or_s, member_name)
     returns F_OR_S member MEMBER_NAME, like F_OR_S.MEMBER_NAME syntax,
     but MEMBER_NAME can be a computed string.  The F_OR_S may be a
     binary file or a structure instance.
   SEE ALSO: openb
 */

extern read_clog;
/* DOCUMENT file= read_clog(file, clog_name)
     raw routine to set the binary data structure of FILE according
     to the text description in the Contents Log file CLOG_NAME.
 */

func recover_file(filename, clogfile)
/* DOCUMENT recover_file, filename
         or recover_file, filename, clogfile
     writes the descriptive information at the end of a corrupted
     binary file FILENAME from its Contents Log file CLOGFILE, which
     is FILENAME+"L" by default.
 */
{
  if (is_void(clogfile)) clogfile= filename+"L";
  if (clogfile==filename+"L") {  /* open clobbers this one */
    changed= 1;
    rename, clogfile, filename+"M";
    clogfile= filename+"M";
  } else {
    changed= 0;
  }
  f= open(filename, "r+b");
  i= array(char, 12);
  _read, f, 0, i;
  read_clog, f, clogfile;
  if (string(&i)=="!<<PDB:II>>!") _set_pdb, f, at_pdb_close;
  else _init_clog, f;
  close, f;
  if (changed) remove, clogfile;
}

extern _not_pdb;
/* DOCUMENT _not_pdb(file, familyOK)
     returns 1 if FILE is not a PDB file, otherwise returns 0 after
     setting the structure and data tables, and cataloguing any
     history records.  Used to open an existing file.  Also detects
     a file with an appended Clog description.
     Before calling _not_pdb, set the variable yPDBopen to the value
     of at_pdb_open you want to be in force.  (For historical reasons
     -- in order to allow for the open102 keyword to openb -- _not_pdb
     looks at the value of the variable yPDBopen, rather than at_pdb_open
     directly.)
 */

local close102, open102, close102_default;
/* DOCUMENT close102  is a keyword for createb or updateb,
            open102   is a keyword for openb or updateb
	    close102_default   is a global variable (initially 0)
	      ***Do not use close102_default -- use at_pdb_close
	         -- this is for backward compatibility only***

	    close102=1  means to close the PDB file "Major-Order:102"
	    close102=0  means close it "Major-Order:101"
	       if not specified, uses 1 if close102_default non-zero,
	       otherwise the value specified in at_pdb_close

	    open102=1   means to ignore what the PDB file says internally,
	                and open it as if it were "Major-Order:102"
	    open102=0   (the default) means to assume the PDB file is
                        correctly writen
	    open102=2   means to assume that the file is incorrectly
	                written, whichever way it is marked
	    open102=3   means to ignore what the PDB file says internally,
	                and open it as if it were "Major-Order:101"

     The PDB file format comes in two styles, "Major-Order:101", and
     "Major-Order:102".  Yorick interprets these correctly by default,
     but other codes may ignore them, or write them incorrectly.

     Unlike Yorick, not all codes are able to correctly read both
     styles.  If you are writing a file which needs to be read by
     a "102 style" code, create it with the close102=1 keyword.

     If you notice that a file you though was a history file isn't, or
     that the dimensions of multi-dimensional variables are transposed
     from the order you expected, the code which wrote the file probably
     blew it.  Try openb("filename", open102=2).  The choices 1 and 3
     are for cases in which you know the writing code was supposed to
     write the file one way or the other, and you don't want to be
     bothered.

     The open102 and close102 keywords, if present, override the
     defaults in the variables at_pdb_open and at_pdb_close.

   SEE ALSO: at_pdb_open, at_pdb_close
 */
close102_default= [];

local at_pdb_open, at_pdb_close;
/* DOCUMENT at_pdb_open
            at_pdb_close
     bits for optional behavior when a PDB file is opened or closed:

     at_pdb_open:
     000  Major-Order:  value specified in file is correct
     001  Major-Order:102 always 
     002  Major-Order:  opposite from what file says
     003  Major-Order:101 always

     004  Strip Basis @... suffices from variable names (when possible)
          Danger!  If you do this and open a file for update, the variable
	  names will be stripped when you close the file!
     010  Use Basis @history convention on input

     The 001 and 002 bits may be overridden by the open102 keyword.
     The default value of at_pdb_open is 010.

     at_pdb_close (the value at the time the file is opened or created
                   is remembered):
     001  Write Major-Order 102 PDB file
     002  Write PDB style history data
        The following are no-ops unless bit 002 is set:
     004  Use Basis @history convention on output
     010  Do NOT pack all history record variables into
          a single structure instance.

     The 001 bit may be overridden by the close102 keyword or if
     close102_default is non-zero.
     The default value of at_pdb_close is 007.

   SEE ALSO: close102_default
 */
at_pdb_open= 010;
at_pdb_close= 007;

func _not_pdbf(f) { return _not_pdb(f, 1); }

extern _init_pdb;
extern _set_pdb;
/* DOCUMENT _init_pdb, file, at_pdb_close
            _set_pdb, file, at_pdb_close
     initializes a PDB binary file.  Used after creating a new file --
     must be called AFTER the primitive data formats have been set.
     The _set_pdb call only sets the CloseHook, on the assumption that
     the file header has already been written (as in recover_file).
   SEE ALSO: createb, recover_file, at_pdb_close
 */

extern _init_clog;
/* DOCUMENT _init_clog, file
     initializes a Clog binary file.  Used after creating a new file --
     must be called AFTER the primitive data formats have been set.
 */

extern dump_clog;
/* DOCUMENT dump_clog, file, clog_name
     dumps a Contents Log of the binary file FILE into the text file
     CLOG_NAME.  Any previous file named CLOG_NAME is overwritten.
  SEE ALSO: openb
 */

func _not_cdf(file)
/* DOCUMENT _not_cdf(file)
     is like _not_pdb, but for netCDF files.
 */
{
  i= array(char, 4);
  _read, f, 0, i;
  if (string(&i)!="CDF\001") return 1;  /* test magic number */
  require, "netcdf.i";
  return raw_not_cdf(file);
}

local openb_hooks;
/* DOCUMENT openb_hooks
     list of functions to be tried by openb if the file to be
     opened is not a PDB file.  By default,
       openb_hooks= _lst(_not_pdbf, _not_cdf).
     The hook functions will be called with the file as argument
     (e.g.- _not_cdf(file)), beginning with _car(openb_hooks), until
     one of them returns 0.  Note that a hook should return 0 if it
     "recognizes" the file as one that it should be able to open, but
     finds that the file is misformatted (alternatively, it could call
     error to abort the whole process).
 */
openb_hooks= _lst(_not_pdbf, _not_cdf);

func createb(filename, primitives, close102=)
/* DOCUMENT file= createb(filename)
         or file= createb(filename, primitives)
     creates FILENAME as a PDB file in "w+b" mode, destroying any
     existing file by that name.  If the PRIMITIVES argument is
     supplied, it must be the name of a procedure that sets the
     primitive data types for the file.  The default is to create
     a file with the native primitive types of the machine on which
     Yorick is running.  The following PRIMITIVES functions are
     predefined:
        sun_primitives    -- appropriate for Sun, HP, IBM, and
	                     most other workstations
        sun3_primitives   -- appropriate for old Sun-2 or Sun-3
        dec_primitives    -- appropriate for DEC (MIPS) workstations
        alpha_primitives  -- appropriate for DEC alpha workstations
        cray_primitives   -- appropriate for Cray 1, XMP, and YMP
	mac_primitives    -- appropriate for MacIntosh
	macl_primitives   -- appropriate for MacIntosh, 12-byte double
	pc_primitives     -- appropriate for IBM PC
        vax_primitives    -- appropriate for VAXen only (H doubles)
        vaxg_primitives   -- appropriate for VAXen only (G doubles)
        xdr_primitives    -- appropriate for XDR files
  SEE ALSO: openb, updateb, cd
            save, add_record, set_filesize, set_blocksize
	    close102, close102_default, at_pdb_open, at_pdb_close
 */
{
  file= open(filename, "w+b");
  if (!is_void(primitives)) primitives, file;
  if (!is_void(close102)) yPDBclose= ((close102&1)|(at_pdb_close&~1));
  else if (is_void(close102_default)) yPDBclose= at_pdb_close;
  else yPDBclose= ((close102_default&1)|(at_pdb_close&~1));
  _init_pdb, file, yPDBclose;
  return file;
}

func sun_primitives(file)
/* DOCUMENT sun_primitives, file
     sets FILE primitive data types to be native to Sun, HP, IBM, etc.
 */
{
  require, "prmtyp.i";
  raw_sun_p, file;
}

func sun3_primitives(file)
/* DOCUMENT sun3_primitives, file
     sets FILE primitive data types to be native to Sun-2 or Sun-3.
 */
{
  require, "prmtyp.i";
  raw_sun3_p, file;
}

func dec_primitives(file)
/* DOCUMENT dec_primitives, file
     sets FILE primitive data types to be native to DEC (MIPS) workstations.
 */
{
  require, "prmtyp.i";
  raw_dec_p, file;
}

func alpha_primitives(file)
/* DOCUMENT alpha_primitives, file
     sets FILE primitive data types to be native to DEC alpha workstations.
 */
{
  require, "prmtyp.i";
  raw_alpha_p, file;
}

func cray_primitives(file)
/* DOCUMENT cray_primitives, file
     sets FILE primitive data types to be native to Cray 1, XMP, and YMP.
 */
{
  require, "prmtyp.i";
  raw_cray_p, file;
}

func mac_primitives(file)
/* DOCUMENT mac_primitives, file
     sets FILE primitive data types to be native to MacIntosh, 8 byte double.
 */
{
  require, "prmtyp.i";
  raw_mac_p, file;
}

func macl_primitives(file)
/* DOCUMENT macl_primitives, file
     sets FILE primitive data types to be native to MacIntosh, long double.
 */
{
  require, "prmtyp.i";
  raw_macl_p, file;
}

func pc_primitives(file)
/* DOCUMENT pc_primitives, file
     sets FILE primitive data types to be native to IBM PC.
 */
{
  require, "prmtyp.i";
  raw_pc_p, file;
}

func vax_primitives(file)
/* DOCUMENT vax_primitives, file
     sets FILE primitive data types to be native to VAXen, H-double, only.
 */
{
  require, "prmtyp.i";
  raw_vax_p, file;
}

func vaxg_primitives(file)
/* DOCUMENT vaxg_primitives, file
     sets FILE primitive data types to be native to VAXen, G-double, only.
 */
{
  require, "prmtyp.i";
  raw_vaxg_p, file;
}

func xdr_primitives(file)
/* DOCUMENT xdr_primitives, file
     sets FILE primitive data types to be XDR (external data representation).
 */
{
  require, "prmtyp.i";
  raw_xdr_p, file;
}

func updateb(filename, primitives, close102=, open102=)
/* DOCUMENT file= updateb(filename)
         or file= updateb(filename, primitives)
     open a binary date file FILENAME for update (mode "r+b").
     The optional PRIMITIVES argument is as for the createb function.
     If the file exists, it is opened as if by openb(filename),
     otherwise a new PDB file is created as if by createb(filename).
   SEE ALSO: openb, createb, cd, save, restore, get_vars, get_addrs
             close102, close102_default, open102, at_pdb_open, at_pdb_close
 */
{
  if (is_void(open(filename, "r", 1)))   /* "rb" does much more work */
    return createb(filename, primitives, close102=close102);
  else
    return openb(filename,,1, open102=open102);
}

extern save;
extern restore;
/* DOCUMENT save, file, var1, var2, ...
            restore, file, var1, var2, ...
     saves the variables VAR1, VAR2, etc. in the binary file FILE,
     or restores them from that file.
     The VARi may be either non-record or record data in the case that
     FILE contains records.

     If one of the VARi does not already exist in FILE, it is created
     by the save command; after add_record, save adds or stores VARi to
     the current record.  See add_record for more.  The VARi may be
     structure definitions (for the save command) to declare data
     structures for the file.  This is necessary only in the case that
     a record variable is a pointer -- all of the potential data types
     of pointees must be known.  No data structures may be declared
     using the save command after the first record has been added.

     If no VARi are present, save saves all array variables, and
     restore restores every non-record variable in the file if there
     is no current record, and every variable in the current record if
     there is one.
   SEE ALSO: openb, createb, updateb, get_vars, add_record, get_addrs
             jt, jc, _read, _write, data_align
 */

func jt(file, time)
/* DOCUMENT jt, time
         or jt, file, time
         or jt, file
         or jt, file, -
     jump to the record nearest the specified TIME.  If no FILE is
     specified, the current record of all open binary files containing
     records is shifted.
     If both FILE and TIME are specified and jt is called as a function,
     it returns the actual time of the new current record.

   N.B.: "jt, file" and "jt, file, -" are obsolete.  Use the jr function to
     step through a file one record at a time.

     If only the FILE is specified, increment the current record of that
     FILE by one.  If the TIME argument is - (the pseudo-index range
     function), decrement the current record of FILE by one.
     If the current record is the last, "jt, file" unsets the current record
     so that record variables will be inaccessible until another jt or jc.
     The same thing happens with "jt, file, -" if the current record was the
     first.
     If only FILE is specified, jt returns 1 if there is a new current
     record, 0 if the call resulted in no current record.  Thus "jt(file)"
     and "jt(file,-)" may be used as the condition in a while loop to step
     through every record in a file:
        file= openb("example.pdb");
        do {
	  restore, file, interesting_record_variables;
	  ...calculations...
	} while (jt(file));

   SEE ALSO: jc, _jt, edit_times, show, jr
 */
{
  return is_void(time)? _jt(file) : _jt(file, time);
}

func jc(file, ncyc)
/* DOCUMENT jc, file, ncyc
     jump to the record of FILE nearest the specified NCYC.
   SEE ALSO: jt, _jc, edit_times, show, jr
 */
{
  return _jc(file, ncyc);
}

extern _jr;
extern _jt;
extern _jc;
/* DOCUMENT _jt, file, time
            _jc, file, ncyc
	    _jr, file
     are raw versions of jt and jc provided to simplify redefining
     the default jt and jc functions to add additional features.
     For example, you could redefine jt to jump to a time, then
     plot something.  The new jt can pass its arguments along to
     _jt, then call the appropriate plotting functions.
     There is a raw version of jr as well.
 */

func jr(file, i)
/* DOCUMENT jr, file, i
         or _jr(file, i)
     Jump to a particular record number I (from 1 to n_records) in a
     binary file FILE.  The function returns 1 if such a record exists,
     0 if there is no such record.  In the latter case, no action is
     taken; the program halts with an error only if jr was invoked
     as a subroutine.  Record numbering wraps like array indices; use
     jr, file, 0  to jump to the last record, -1 to next to last, etc.
   SEE ALSO: jt, jc, edit_times, show
 */
{
  return _jr(file, i);
}

extern add_record;
/* DOCUMENT add_record, file, time, ncyc
         or add_record, file, time, ncyc, address
         or add_record, file
     adds a new record to FILE corresponding to the specified TIME and
     NCYC (respectively a double and a long).  Either or both TIME
     and NCYC may be nil or omitted, but the existence of TIME and
     NCYC must be the same for every record added to one FILE.
     If present, ADDRESS specifies the disk address of the new record,
     which is assumed to be in the current file.  Without ADDRESS, or
     if ADDRESS<0, the next available address is used; this may create
     a new file in the family (see the set_filesize function).
     The add_record function leaves the new record current
     for subsequent save commands to actually write the data.

     The TIME, NCYC, and ADDRESS arguments may be equal length vectors
     to add several records at once; in this case, the first of the
     newly added records is the current one.  If all three of TIME,
     NCYC, and ADDRESS are nil or omitted, no new records are added,
     but the file becomes a record file if it was not already, and in
     any case, no record will be the current record after such an
     add_record call.

     After the first add_record call (even if no records were added),
     subsequent add_variable commands will create record variables.
     After the first record has been added, subsequent save commands
     will create any new variables as record variables.
     After a second record has been added using add_record, neither
     save commands nor add_variable commands may be used to introduce
     any new record variables.
   SEE ALSO: save, createb, updateb, openb, set_filesize, set_blocksize
             add_variable
 */

extern add_variable;
/* DOCUMENT add_variable, file, address, name, type, dimlist
     adds a variable NAME to FILE at the specified ADDRESS, with the
     specified TYPE and dimensions given by DIMLIST.  The DIMLIST may
     be zero or more arguments, as for the "array" function.  If the
     ADDRESS is <0, the next available address is used. Note that,
     unlike the save command, add_variable does not actually write any
     data -- it merely changes Yorick's description of the contents of
     FILE.
     After the first add_record call, add_variable adds a variable to
     the record instead of a non-record variable.  See add_record.
   SEE ALSO: save, openb, createb, updateb, add_record,
             add_member, install_struct, data_align
 */

extern set_blocksize;
/* DOCUMENT set_blocksize, file, blocksize
     sets smallest cache block size for FILE to BLOCKSIZE.  BLOCKSIZE
     is rounded to the next larger number of the form 4096*2^n if
     necessary; cache blocks for this file will be multiples of
     BLOCKSIZE bytes long.  The default BLOCKSIZE is 0x4000 (16 KB).
   SEE ALSO: openb, updateb, createb, save, restore, _read, _write
 */

extern set_filesize;
/* DOCUMENT set_filesize, file, filesize
     sets the new family member threshhold for FILE to FILESIZE.
     Whenever a new record is added (see add_record), if the current file
     in the FILE family has at least one record and the new record would
     cause the current file to exceed FILESIZE bytes, a new family
     member will be created to hold the new record.
     The default FILESIZE is 0x400000 (4 MB).
   SEE ALSO: openb, updateb, createb, add_record
 */

extern get_vars;
/* DOCUMENT name_lists= get_vars(file)
     returns the lists of non-record and record variable names in the
     binary FILE.  The return value is an array of two pointers to
     arrays of type string; *name_lists(1) is the array of non-record
     variable names (or nil if there are none), *name_lists(2) is the
     array of record variable names.
     The get_addrs function returns corresponding lists of disk
     addresses; the get_member function can be used in conjunction
     with the dimsof, structof, and typeof functions to determine
     the other properties of a variable.
   SEE ALSO: openb, updateb, restore, jt, jc, has_records, get_addrs
 */

extern get_addrs;
/* DOCUMENT addr_lists= get_addrs(file)
     returns the byte addresses of the non-record and record variables
     in the binary file FILE, and lists of the record addresses, file
     indices, and filenames for file families with history records.
          *addr_lists(1)   absolute addresses of non-record variables
	  *addr_lists(2)   relative addresses of record variables
	                   (add record address to get absolute address)
	     The order of these two address lists matches the
	     corresponding lists of names returned by get_vars.
	  *addr_lists(3)   absolute addresses of records
	  *addr_lists(4)   list of file indices corresponding to
	                   addr_lists(3); indices are into addr_lists(5)
	  *addr_lists(5)   list of filenames in the family
   SEE ALSO: openb, updateb, restore, jt, jc, has_records, get_vars
 */

func has_records(file)
/* DOCUMENT has_records(file)
     returns 1 if FILE has history records, 0 if it does not.
 */
{
  return get_vars(file)(2)? 1n : 0n;
}

extern get_times;
extern get_ncycs;
/* DOCUMENT times= get_times(file)
            ncycs= get_ncycs(file)
     returns the list of time or ncyc values associated with the records
     if FILE, or nil if there are none.  The time values are not guaranteed
     to be precise (but they should be good to at least 6 digits or so);
     the precise time associated with each record may be stored as a record
     variable.
   SEE ALSO: collect, openb, updateb, restore, jt, jc, edit_times
 */

extern edit_times;
/* DOCUMENT edit_times, file
         or edit_times, file, keep_list
         or edit_times, file, keep_list, new_times, new_ncycs
     edits the records for FILE.  The KEEP_LIST is a 0-origin index list
     of records to be kept, or nil to keep all records.  The NEW_TIMES
     array is the list of new time values for the (kept) records, and
     the NEW_NCYCS array is the list of new cycle number values for the
     (kept) records.  Either NEW_TIMES, or NEW_NCYCS, or both, may be
     nil to leave the corresponding values unchanged.  If non-nil,
     NEW_TIMES and NEW_NCYCS must have the same length as KEEP_LIST,
     or, if KEEP_LIST is nil, as the original number of records in
     the file.  If KEEP_LIST, NEW_TIME, and NEW_NCYCS are all omitted
     or nil, then edit_times removes records as necessary to ensure
     that the remaining records have monotonically increasing times,
     or, if no times are present, monotonically increasing ncycs.
     (The latest record at any given time/ncyc is retained, and earlier
     records are removed.)
     In no case does edit_times change the FILE itself; only Yorick's
     in-memory model of the file is altered.
   SEE ALSO: get_times, get_ncycs, jt, jc
 */

extern _read;
extern _write;
/* DOCUMENT _write, file, address, expression
            _read, file, address, variable
	 or nbytes= _read(file, address, variable);
     are low level read and write functions which do not "see" the
     symbol table for the binary FILE.  The ADDRESS is the byte address
     at which to begin the write or read operation.  The type and number
     of objects of the EXPRESSION or VARIABLE determines how much data
     to read, and what format conversion operations to apply.  In the
     case of type char, no conversion operations are ever applied, and
     _read will return the actual number of bytes read, which may be
     fewer than the number implied by VARIABLE in this one case.
     (In all other cases, _read returns numberof(VARIABLE).)
     If the FILE has records, the ADDRESS is understood to be in the
     file family member in which the current record resides.
   SEE ALSO: openb, createb, updateb, save, restore, sizeof
 */

extern add_member;
/* DOCUMENT add_member, file, struct_name, offset, name, type, dimlist
     adds a member to a data type in the file FILE.  The data type name
     (struct name) is STRUCT_NAME, which will be created if it does
     not already exist.  The new member will be at OFFSET (in bytes)
     from the beginning of an instance of this structure, and will
     have the specified NAME, TYPE, and DIMLIST.  Use OFFSET -1 to
     have add_member compute the next available offset in the structure.
     The TYPE can be either a structure definition, or a string naming
     a previously defined data type in FILE.  The optional DIMLIST is
     as for the "array" function.
     The STRUCT_NAME built from a series of add_member calls cannot be
     used until it is installed with install_struct.
     This function should be used very sparingly, mostly in code which
     is building the structure of a foreign-format binary file.
   SEE ALSO: add_variable, install_struct, struct_align
 */

extern install_struct;
/* DOCUMENT install_struct, file, struct_name
         or install_struct, file, struct_name, size, align, order
         or install_struct, file, struct_name, size, align, order, layout
     installs the data type named STRUCT_NAME in the binary FILE.  In
     the two argument form, STRUCT_NAME must have been built by one or
     more calls to the add_member function.  In the 5 and 6 argument calls,
     STRUCT_NAME is a primitive data type -- an integer type for the 5
     argument call, and a floating point type for the 6 argument call.
     The 5 argument form may also be used to declare opaque data types.
     SIZE is the size of an instance in bytes, ALIGN is its alignment
     boundary (also in bytes), and ORDER is the byte order.  ORDER is
     1 for most significant byte first, -1 for least significant byte
     first, and 0 for opaque (unconverted) data.  Other ORDER values
     represent more complex byte permutations (2 is the byte order for
     VAX floating point numbers).  If ORDER equals SIZE, then the data
     type is not only opaque, but also must be read sequentially.
     LAYOUT is an array of 7 long values parameterizing the floating
     point format, [sign_address, exponent_address, exponent_size,
     mantissa_address, mantissa_size, mantissa_normalized, exponent_bias]
     (the addresses and sizes are in bits, reduced to MSB first order).
     Use, e.g., nameof(float) for STRUCT_NAME to redefine the meaning
     of the float data type for FILE.
   SEE ALSO: add_variable, add_member
 */

extern data_align;
/* DOCUMENT data_align, file, alignment
     in binary file FILE, align new variables to begin at a byte address
     which is a multiple of ALIGNMENT.  (This affects placement of data
     declared using save and add_variable.  For add_variable, data_align
     has an effect only if the address is not specified.)  If ALIGNMENT
     is <=0, new variables will be aligned as they would be if they were
     data structure members.  The default value is 0.
   SEE ALSO: save, add_variable
 */

extern struct_align;
/* DOCUMENT struct_align, file, alignment
     in binary file FILE, align new struct members which are themselves
     struct instances to begin at a byte address which is a multiple of
     ALIGNMENT.  (This affects members declared explicitly by add_member,
     as well as implicitly by save or add_variable.)  If ALIGNMENT is <=0,
     returns to the default for this machine.  The struct alignment is in
     addition to the alignment implied by the most restrictively aligned
     member of the struct.  Most machines want ALIGNMENT of 1.
   SEE ALSO: add_member
 */

extern add_next_file;
/* DOCUMENT failure= add_next_file(file, filename, create_flag)
     adds the next file to the FILE, which must contain history records.
     If FILENAME is non-nil, the new file will be called that, otherwise
     the next sequential filename is used.  If CREATE_FLAG is present
     and non-zero, the new file will be created if it does not already
     exist.  If omitted or nil, CREATE_FLAG defaults to 1 if the file has
     write permission and 0 if it does not.
     Returns 0 on success.
   SEE ALSO: openb, updateb, createb, add_record
 */

/*--------------------------------------------------------------------------*/

extern error;
extern exit;
/* DOCUMENT exit, msg
            error, msg
     Exits the current interpreted *main* program, printing the MSG.
     (MSG can be omitted to print a default.)
     In the case of exit, the result is equivalent to an immediate
     return from every function in the current calling chain.
     In the case of error, the result is the same as if an error had
     occurred in a compiled routine.
   SEE ALSO: print, write, batch, catch
 */

extern catch;
/* DOCUMENT catch(category)
     Catch errors of the specified category.  Category may be -1 to
     catch all errors, or a bitwise or of the following bits:

        0x01 math errors (SIGFPE, math library)
	0x02 I/O errors
	0x04 keyboard interrupts (e.g.- control C interrupt)
	0x08 other compiled errors (YError)
	0x10 interpreted errors (error)

     Use catch by placing it in a function before the section of code
     in which you are trying to catch errors.  When catch is called,
     it always returns 0, but it records the virtual machine program
     counter where it was called, and longjumps there if an error is
     detected.  The most recent matching call to catch will catch the
     error.  Returning from the function in which catch was called
     pops that call off the list of catches the interpreter checks.

     To use catch, place the call near the top of a function:

        if (catch(category)) {
	  ...<code to execute if error is caught>...
	}
        ...<code "protected" by the catch>...

     If an error with the specified category occurs in the "protected"
     code, the program jumps back to the point of the catch and acts
     as if the catch function had returned 1 (remember that when catch
     is actually called it always returns 0).

     In order to lessen the chances of infinite loops, the catch is
     popped off the active list if it is actually used, so that a
     second error will *not* be caught.  Often, this is only desirable
     for the error handling code itself -- if you want to re-execute
     the "protected" code, do this, and take care of the possibility
     of infinite loops in your interpreted code:

        while (catch(category)) {
	  ...<code to execute if error is caught>...
	}
        ...<code "protected" by the catch>...

     After an error has been caught, the associated error message
     (what would have been printed had it not been caught) is left
     in the variable catch_message.

   SEE ALSO: error
 */

extern batch;
/* DOCUMENT batch, 1
            batch, 0
	    batch()
     turns on, turns off, or tests for batch mode, respectively.
     If yorick is started with the command line:
        yorick -batch batch_include.i ...
     then batch mode is turned on, the usual custom.i startup file is
     skipped, and the file batch_include.i is parsed and executed.  The
     -batch and batch_include.i command line arguments are removed from
     the list returned by get_argv().  These must be the first two
     arguments on the command line.

     In batch mode, any error will terminate Yorick (as by the quit
     function) rather than entering debug mode.  Also, any attempt to
     read from the keyboard is an error.

   SEE ALSO: process_argv, get_argv, set_idler
 */

extern set_idler;
/* DOCUMENT set_idler, idler_function
     sets the idler function to IDLER_FUNCTION.  Instead of waiting
     for keyboard input when all its tasks are finished, the interpreter
     will invoke IDLER_FUNCTION with no arguments.  The idler function
     is normally invoked only once, so input from the keyboard resumes
     after one call to the idler.  Of course, an idler is free to call
     set_idler again before it returns, which will have the effect of
     calling that function in a loop.
   SEE ALSO: batch
 */

/*--------------------------------------------------------------------------*/

extern timestamp;
/* DOCUMENT timestamp()
     returns string of the form "Sun Jan  3 15:14:13 1988" -- always
     has 24 characters.
   SEE ALSO: timer
 */

extern timer;
/* DOCUMENT timer, elapsed
         or timer, elapsed, split
     updates the ELAPSED and optionally SPLIT timing arrays.  These
     arrays must each be of type array(double,3); the layout is
     [cpu, system, wall], with all three times measured in seconds.
     ELAPSED is updated to the total times elapsed since this copy
     of Yorick started.  SPLIT is incremented by the difference between
     the new values of ELAPSED and the values of ELAPSED on entry.
     This feature allows for primitive code profiling by keeping
     separate accounting of time usage in several categories, e.g.--
        elapsed= total= cat1= cat2= cat3= array(double, 3);
        timer, elapsed0;
	elasped= elapsed0;
        ... category 1 code ...
	timer, elapsed, cat1;
        ... category 2 code ...
	timer, elapsed, cat2;
        ... category 3 code ...
	timer, elapsed, cat3;
        ... more category 2 code ...
	timer, elapsed, cat2;
        timer, elapsed0, total;
     The wall time is not absolutely reliable, owning to possible
     rollover at midnight.
   SEE ALSO: timestamp, timer_print
 */

func timer_print(label, split, ..)
/* DOCUMENT timer_print, label1, split1, label2, split2, ...
         or timer_print
         or timer_print, label_total
     prints out a timing summary for splits accumulated by timer.
        timer_print, "category 1", cat1, "category 2", cat2,
                     "category 3", cat3, "total", total;
   SEE ALSO: timer
 */
{
  elapsed= s= array(double, 1:3);
  timer, elapsed;
  write,format="%30s     CPU sec  System sec    Wall sec\n","Timing Category";
  if (!is_void(label) && !is_void(split)) {
    s(1:3)= split;
    write,format="%30s %11.3f %11.3f %11.3f\n", label, s(1), s(2), s(3);
  }
  while (more_args()>1) {
    labl= next_arg();
    s(1:3)= next_arg();
    write,format="%30s %11.3f %11.3f %11.3f\n", labl, s(1), s(2), s(3);
  }
  if (is_void(label) || is_void(split)) {
    if (is_void(label)) labl= "-----Total Elapsed Times-----";
    else labl= label;
    s(1:3)= elapsed;
    write,format="%30s %11.3f %11.3f %11.3f\n", labl, s(1), s(2), s(3);
  }
}

_timer_elapsed= [0.,0.,0.];
timer, _timer_elapsed;

/*--------------------------------------------------------------------------*/

func area(y, x)
/* DOCUMENT area(y, x)
     returns the zonal areas of the 2-D mesh (X, Y).  If Y and X are
     imax-by-jmax, the result is (imax-1)-by-(jmax-1).  The area is
     positive when, say, X increases with i and Y increases with j.
     For example, area([[0,0],[1,1]],[[0,1],[0,1]]) is +1.
   SEE ALSO: volume
 */
{ return x(dif,zcen)*y(zcen,dif) - x(zcen,dif)*y(dif,zcen); }

func volume(r, z)
/* DOCUMENT volume(r, z)
     returns the zonal volumes of the 2-D cylindrical mesh (R, Z).
     If R and Z are imax-by-jmax, the result is (imax-1)-by-(jmax-1).
     The volume is positive when, say, Z increases with i and R increases
     with j.  For example, volume([[0,0],[1,1]],[[0,1],[0,1]]) is +pi.
   SEE ALSO: area
 */
{
  s= r*r;
  v= z(dif,zcen)*s(zcen,dif) - z(zcen,dif)*s(dif,zcen);
  s= z*r;
  return (2.0*pi/3.0)*(v+s(dif,zcen)*r(zcen,dif)-s(zcen,dif)*r(dif,zcen));
}

func ptcen(zncen, ireg)
/* DOCUMENT ptcen(zncen)
         or ptcen(zncen, ireg)
     returns point centered version of the 2-D zone centered array ZNCEN.
     The result is imax-by-jmax if ZNCEN is (imax-1)-by-(jmax-1).
     If the region number array IREG is specified, zones with region
     number 0 are not included in the point centering operation.
     Note that IREG should have dimensions imax-by-jmax; the first
     row and column of IREG are ignored.
     Without IREG, ptcen(zncen) is equivalent to zncen(pcen,pcen).
  SEE ALSO: zncen, uncen
 */
{
  if (is_void(ireg)) return zncen(pcen, pcen, ..);
  void= use_origins(0);
  exist= (ireg(2:,2:)!=0);
  return (exist*zncen)(pcen,pcen,..)/(exist(pcen,pcen)+1.e-35);
}

func zncen(ptcen, ireg)
/* DOCUMENT zncen(ptcen)
         or zncen(ptcen, ireg)
     returns zone centered version of the 2-D point centered array PTCEN.
     The result is (imax-1)-by-(jmax-1) if PTCEN is imax-by-jmax.
     If the region number array IREG is specified, zones with region
     number 0 are not included in the point centering operation.
     Note that IREG should have dimensions imax-by-jmax, like
     the input PTCEN array; the first row and column of IREG are ignored.
     Without IREG, zncen(ptcen) is equivalent to ptcen(zcen,zcen).
  SEE ALSO: ptcen, uncen
 */
{
  if (is_void(ireg)) return ptcen(zcen, zcen, ..);
  void= use_origins(0);
  exist= (ireg(2:,2:)!=0);
  return exist*ptcen(zcen, zcen, ..);
}

func uncen(ptcen, ireg)
/* DOCUMENT uncen(ptcen)
         or uncen(ptcen, ireg)
     returns zone centered version of the 2-D zone centered array PTCEN.
     The result is (imax-1)-by-(jmax-1) if PTCEN is imax-by-jmax.
     If the region number array IREG is specified, zones with region
     number 0 are not included in the point centering operation.
     Note that IREG should have dimensions imax-by-jmax, like
     the input PTCEN array; the first row and column of IREG are ignored.
     Without IREG, uncen(ptcen) is equivalent to ptcen(uncp,uncp).

     Do not use uncen to zone center data which is naturally point
     centered -- use the zncen function for that purpose.  The uncen
     function is the (nearly) exact inverse of the ptcen function,
     so that uncen(ptcen(zncen, ireg), ireg) will return the original
     zncen array.  The uncen reconstruction is as exact as possible,
     given the finite precision of floating point operations.
  SEE ALSO: ptcen, zncen
 */
{
  if (is_void(ireg)) return ptcen(uncp, uncp, ..);
  void= use_origins(0);
  exist= (ireg(2:,2:)!=0);
  return (exist(pcen,pcen)*ptcen)(uncp, uncp, ..);
}

/*--------------------------------------------------------------------------*/

func call(void)
/* DOCUMENT call, subroutine(arg1, arg2, arg3, arg4, arg5
                             arg6, arg7, arg8);
     allows a SUBROUTINE to be called with a very long argument list
     as an alternative to:
          subroutine, arg1, arg2, arg3, arg4, arg5,
            arg6, arg7, arg8;
     Note that the statement
          subroutine(arg1, arg2, arg3, arg4, arg5,
                     arg6, arg7, arg8);
     will print the return value of subroutine, even if it is nil.
     If invoked as a function, call simply returns its argument.
 */
{ return void; }

extern symbol_def;
/* DOCUMENT symbol_def(func_name)(arglist)
         or symbol_def(var_name)
     invokes the function FUNC_NAME with the specified ARGLIST,
     returning the return value.  ARGLIST may be zero or more arguments.
     In fact, symbol_def("fname")(arg1, arg2, arg3) is equivalent to
     fname(arg1, arg2, arg3), so that "fname" can be the name of any
     variable for which the latter syntax is meaningful -- interpreted
     function, built-in function, or array.

     Without an argument list, symbol_def("varname") is equivalent to
     varname, which allows you to get the value of a variable whose name
     you must compute.

     DO NOT OVERUSE THIS FUNCTION.  It works around a specific deficiency
     of the Yorick language -- the lack of pointers to functions -- and
     should be used for such purposes as hook lists (see openb).

   SEE ALSO: symbol_set
 */

extern symbol_set;
/* DOCUMENT symbol_set, var_name, value
     is equivalent to the redefinition
          varname= value
     except that var_name="varname" is a string which must be computed.

     DO NOT OVERUSE THIS FUNCTION.  It works around a specific deficiency
     of the Yorick language -- the lack of pointers to functions, streams,
     bookmarks, and other special non-array data types.

   SEE ALSO: symbol_def
 */

/*--------------------------------------------------------------------------*/

extern dbexit;
extern dbcont;
extern dbret;
extern dbskip;
extern dbup;
extern dbinfo;
extern dbdis;
extern dbauto;
/* DOCUMENT Debug mode.

   Yorick errors fall into two general categories: Syntax errors discovered
   during parsing, and runtime errors discovered when a Yorick program is
   actually running.  When a runtime error occurs, Yorick offers the
   choice of entering "debug mode", which you can do by typing the <RETURN>
   key immediately after the error occurs.  Typing a non-blank line exits
   debug mode automatically by default.  In debug mode, the Yorick prompt
   becomes "dbug>" instead of the usual ">".  When you see this prompt,
   Yorick has halted "in the middle of" the function in which the error
   occurred, and you can print, plot, modify, or save the local variables
   in that function by means of ordinary Yorick commands.  Debug mode is
   recursive; that is, you can debug an error which occurred during
   debugging to any number of levels.

   You can exit from debug mode in several ways:

      dbexit            -- exit current debug level, discarding all
                           active functions and their local variables
      dbexit, 0         -- exit all debug levels
      dbexit, n         -- exit (at most) N debug levels

      dbcont            -- continue execution of the current function
         Continuing is useful if you have managed to repair the
	 problem which caused the error.  The expression in which the
	 error occurred will be evaluated a second time, so beware of
	 side effects.

      dbret, value      -- continue execution by returning VALUE (which
                           may be nil or omitted) to the caller of the
			   function in which the error occurred.
	 This is useful if the function in which the error occurred is
	 hopelessly confounded, but you know the value it should return.

   Yorick does not allow "single stepping" directly, although you can
   execute the statements in a function by copying them, then tell
   Yorick to skip those statements you have executed "by hand".  There
   are two functions for skipping execution:

      dbskip            -- skip the next logical line (This will be only
                           a portion of a source line if several statements
			   are stacked on the source line.)
      dbskip, n         -- skip next N (positive or negative) logical lines

      dbup              -- discard the current function, so that you are
                           debugging its caller -- there is no way to go
			   back "down", so be careful

   There are two functions which print information (like other print
   functions, if called as functions instead of subroutines, their
   result is returned as a string array with one line per string):

      dbinfo            -- returns current function and source line

      dbdis             -- returns disassembled virtual machine code
                           for the next line (use the disassemble function
			   to get the entire function)
	 This allows you to see exactly where in a line the error occurred.

   Finally,

      dbauto            -- toggles whether debug mode will be entered
                           automatically when a runtime error occurs
      dbauto, 1         -- enter debug mode automatically after an error
      dbauto, 0         -- type <RETURN> after error to enter debug mode
 */

/*--------------------------------------------------------------------------*/

extern _lst;
extern _cat;
extern _car;
extern _cdr;
extern _cpy;
extern _len;
/* DOCUMENT list= _lst(item1, item2, item3, ...)
            list= _cat(item_or_list1, item_or_list2, item_or_list3, ...)
	    list= _cpy(list)
	      list= _cpy(list, i)
	    length= _len(list)
            item= _car(list)
              item_i= _car(list, i)
              _car, list, i, new_item_i
	    list= _cdr(list)
	      list= _cdr(list, i)
              _cdr, list, i, new_list_i

     implement rudimentary Lisp-like list handling in Yorick.
     However, in Yorick, a list must have a simple tree structure
     - no loops or rings are allowed (loops break Yorick's memory
     manager - beware).  You need to be careful not to do this as
     the error will not be detected.

     Lists are required in Yorick whenever you need to hold an
     indeterminate amount of non-array data, such as file handles,
     bookmarks, functions, index ranges, etc.  Note that Yorick
     pointers cannot point to these objects.  For array data, you have
     a choice between a list and a struct or an array of pointers.
     Note that a list cannot be written into a file with the save
     function, since it may contain unsaveable items.

     The _lst (list), _cat (catenate), and _cpy (copy) functions
     are the principal means for creating and maintaining lists.
     _lst makes a list out of its arguments, so that each argument
     becomes one item of the new list.  Unlike Yorick array data
     types, a statement like x=list does not make a copy of the
     list, it merely makes an additional reference to the list.
     You must explicitly use the _cpy function to copy a list.  Note
     that _cpy only copies the outermost list itself, not the items
     in the list (even if those items are lists).  With the second
     argument i, _cpy copies only the first i items in the list.
     The _cat function concatentates several lists together,
     "promoting" any arguments which are not lists.  This operation
     changes the values of list arguments to _cat, except for the
     final argument, since after _cat(list, item), the variable list
     will point to the new longer list returned by _cat.

     Nil, or [], functions as an empty list.  This leads to ambiguity
     in the argument list for _cat, since _cat "promotes" non-list
     arguments to lists; _cat treats [] as an empty list, not as a
     non-list item.  Also, _lst() or _lst([]) returns a single item list,
     not [] itself.

     The _len function returns the number of items in a list, or 0
     for [].

     The _car and _cdr functions (the names are taken from Lisp,
     where they originally stood for something like "address register"
     and "data register" of some long forgotten machine) provide
     access to the items stored in a list.  _car(list,i) returns the
     i-th item of the list, and i defaults to 1, so _car(list) is the
     first item.  Also, _car,list,i,new_item_i sets the i-th item
     of the list.  Finally, _cdr(list,i) returns a list of all the
     items beyond the i-th, where i again defaults to 1.  The form
     _cdr,list,i,new_list_i can be used to reset all list items
     beyond the i-th to new values.  In the _cdr function, i=0 is
     allowed.  When used to set values, both _car and _cdr can also
     be called as functions, in which case they return the item or
     list which has been replaced.  The _cdr(list) function returns
     nil if and only if LIST contains only a single item; this is
     the usual means of halting a loop over items in a list.

   SEE ALSO: array, grow, _prt, _map, _rev, _nxt
 */

func _prt(x, indent)
/* DOCUMENT _prt, list
     print every item in a list, recursing if some item is itself a list.
   SEE ALSO: _lst
 */
{
  if (is_void(indent)) indent= "";
  if (typeof(x)!="list") {
    write,format="%s\n",indent+print(x);
    return;                       /* exit recursion */
  }
  write,format="%s\n",indent+"list items:";
  do {
    _prt, _car(x), indent+"  ";   /* recurse */
    x= _cdr(x);
  } while (!is_void(x));
}

func _map(f__map, list__map)
/* DOCUMENT _map(f, list)
     return a list of the results of applying function F to each
     element of the input LIST in turn, as if by
       _lst(f(_car(list,1)),f(_car(list,2)),...)
   SEE ALSO: _lst
 */
{
  /* all locals here must have weird names, since the function f will
   * very often rely on external variables for arguments not varying
   * in the input list, or for accumulated outputs */
  if (is_void(list__map)) return [];
  result__map= tail__map= _lst(f__map(_car(list__map)));
  for (list__map=_cdr(list__map) ;
       !is_void(list__map) ; list__map=_cdr(list__map)) {
    _cat, tail__map, _lst(f__map(_car(list__map)));
    tail__map= _cdr(tail__map);
  }
  return result__map;
}

func _rev(list)
/* DOCUMENT _rev(list)
     returns the input list in reverse order
   SEE ALSO: _lst
 */
{
  if (is_void(list)) return;
  prev= [];
  for (;;) {
    tail= _cdr(list, 1, prev);
    if (is_void(tail)) return list;
    prev= list;
    list= tail;
  }
}

func _nxt(&list)
/* DOCUMENT item= _nxt(list)
     return first item in LIST, and set LIST to list of remaining
     items.  If you are iterating through a list, this is the way
     to do it, since a loop on _car(list,i) with i varying from 1
     to _len(list) scales quadratically with the length of the list,
     while a loop on _nxt(list) scales linearly.
   SEE ALSO: _car, _lst
 */
{
  item= _car(list);
  list= _cdr(list);
  return item;
}

/*--------------------------------------------------------------------------*/
