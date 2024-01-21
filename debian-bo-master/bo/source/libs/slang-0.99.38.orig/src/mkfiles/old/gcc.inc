/*-------------------------------*- C -*------------------------------- *\
 * gcc.inc
 * 
 * a cpp macro file for gcc to be included on the gcc command line
 * with -imacros.  Below is a description from the gcc manual:
 * 
 * `-include FILE'
 *	Process FILE as input before processing the regular input file.
 *	In effect, the contents of FILE are compiled first.  Any `-D' and
 *	`-U' options on the command line are always processed before
 *	`-include FILE', regardless of the order in which they are
 *	written.  All the `-include' and `-imacros' options are processed
 *	in the order in which they are written.
 * 
 * `-imacros FILE'
 *	Process FILE as input, discarding the resulting output, before
 *	processing the regular input file.  Because the output generated
 *	from FILE is discarded, the only effect of `-imacros FILE' is to
 *	make the macros defined in FILE available for use in the main
 *	input.
 *
 *	Any `-D' and `-U' options on the command line are always processed
 *	before `-imacros FILE', regardless of the order in which they are
 *	written.  All the `-include' and `-imacros' options are processed
 *	in the order in which they are written.
\*----------------------------------------------------------------------*/

#if defined (__EMX__) || defined (__GO32__)
# ifndef JED_ROOT
#   define JED_ROOT	"c:/usr/local/jed"
# endif

# ifndef HAS_MEMSET
#   define HAS_MEMSET
# endif
# ifndef HAS_MEMCPY
#   define HAS_MEMCPY
# endif
# ifndef HAS_MEMCMP
#   define HAS_MEMCMP
# endif

#endif

#if defined (__EMX__)
#define HAS_DIRENT
#endif

#if defined (__GO32__)
# undef MSDOS 
# undef __MSDOS
# undef __MSDOS__
# undef unix
# undef __unix
# undef __unix__
#endif
