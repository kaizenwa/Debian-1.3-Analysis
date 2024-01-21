define(diversion_number, divnum)dnl
divert(-1)


# Handle the existence or non-existence of a C++ compiler.
#
AC_DEFUN([UL_PROG_CXX], [dnl
    UC_PROG_CXX dnl
    case "${CXX-}" in
	'')
	    CXX_TARGET_ALL=
	    CXX_TARGET_TEST=
	    CXX_TARGET_INSTALL=
	    ;;
	*)
	    CXX_TARGET_ALL=cxx/all
	    CXX_TARGET_TEST=cxx/test
	    CXX_TARGET_INSTALL=cxx/install
	    ;;
    esac
    AC_SUBST(CXX_TARGET_ALL) dnl
    AC_SUBST(CXX_TARGET_TEST) dnl
    AC_SUBST(CXX_TARGET_INSTALL) dnl
    UC_DEFAULT(CXXCPPFLAGS, )
    UC_DEFAULT(CXXFLAGS, )
])


# Handle the existence or non-existence of a FORTRAN compiler.
#
AC_DEFUN([UL_PROG_FC], [dnl
    UC_PROG_FC dnl
    case "${FC-}" in
	'')
	    FORTRAN_TARGET_ALL=
	    FORTRAN_TARGET_TEST=
	    FORTRAN_TARGET_INSTALL=
	    FORTRAN_MANUAL=
	    ;;
	*)
	    FORTRAN_TARGET_ALL=fortran/all
	    FORTRAN_TARGET_TEST=fortran/test
	    FORTRAN_TARGET_INSTALL=fortran/install
	    FORTRAN_MANUAL=netcdf.3f
	    ;;
    esac
    AC_SUBST(FORTRAN_TARGET_ALL) dnl
    AC_SUBST(FORTRAN_TARGET_TEST) dnl
    AC_SUBST(FORTRAN_TARGET_INSTALL) dnl
    AC_SUBST(FORTRAN_MANUAL) dnl
])


# Check for fill value usage.
#
AC_DEFUN([UL_FILLVALUES], [dnl
AC_MSG_CHECKING(for fill-value usage)
if test "${OLD_FILLVALUES-}" = yes; then
    AC_DEFINE(NC_OLD_FILLVALUES, 1)
    AC_MSG_RESULT(old usage)
else
    AC_DEFINE(NC_OLD_FILLVALUES, 0)
    AC_MSG_RESULT(new usage)
fi
])


# Check endianess.
#
AC_DEFUN([UL_SWAP],
[dnl
    AC_MSG_CHECKING(endianess)
    SWAP=0
    AC_TRY_RUN(
	[
	    main()
	    {
		long i = 0;
		char *ip = (char*)&i;
		*ip	= 'a';
		/* false if little endian or sizeof(long) == sizeof(char): */
		exit(i != 'a');
	    }
	],
	[SWAP=1]
	AC_MSG_RESULT(little endian),
	AC_MSG_RESULT(big endian),
	false
    )
    AC_DEFINE_UNQUOTED(SWAP, $SWAP)
])


# Check type of 32-bit network long integer and internal type 
# corresponding to network long.
#
AC_DEFUN([UL_NETLONG],
[dnl
    AC_REQUIRE([UC_OS])dnl
    AC_MSG_CHECKING(type of netlong)
    case "$OS" in
	unicos*)
	    NETLONG=long
	    SIZEOF_NETLONG=8
	    AC_MSG_RESULT(no 32-bit type: using long)
	    ;;
	*)
	    AC_TRY_RUN(
		[main() {exit(sizeof(long) == 4);}],
		[dnl
		    NETLONG=int
		    AC_MSG_RESULT(int)
		],
		[dnl
		    NETLONG=long
		    AC_MSG_RESULT(long)
		],
		false)
		SIZEOF_NETLONG=4
	    ;;
    esac
    AC_MSG_CHECKING(type of internal netlong)
    case "$OS" in
	irix64*)
	    INTERNAL_NETLONG=long
	    SIZEOF_INTERNAL_NETLONG=8;;
	*)  INTERNAL_NETLONG=$NETLONG
	    SIZEOF_INTERNAL_NETLONG=$SIZEOF_NETLONG
	    ;;
    esac
    AC_MSG_RESULT($INTERNAL_NETLONG)
    AC_DEFINE_UNQUOTED(NETLONG, $NETLONG)dnl
    AC_DEFINE_UNQUOTED(SIZEOF_NETLONG, $SIZEOF_NETLONG)dnl
    AC_DEFINE_UNQUOTED(INTERNAL_NETLONG, $INTERNAL_NETLONG)dnl
    AC_DEFINE_UNQUOTED(SIZEOF_INTERNAL_NETLONG, $SIZEOF_INTERNAL_NETLONG)dnl
])


# Determine the size of a C int.
#
AC_DEFUN([UL_SIZEOF_INT], [
    AC_CHECK_SIZEOF(int)
])


# Determine the size of a C long.
#
AC_DEFUN([UL_SIZEOF_LONG], [
    AC_CHECK_SIZEOF(long)
])


# Check the type of an nclong netCDF integer: the C type that
# corresponds to an argument of type NC_LONG.
#
AC_DEFUN([UL_NCLONG], [dnl
    AC_REQUIRE([UL_SIZEOF_INT])
    AC_REQUIRE([UL_SIZEOF_LONG])
    AC_MSG_CHECKING(type of nclong)
    case "$ac_cv_sizeof_long" in
	4)
	    AC_DEFINE(NC_NCLONG_T, long)
	    AC_DEFINE(SIZEOF_NCLONG, SIZEOF_LONG)
	    AC_MSG_RESULT(long)
	    ;;
	*)
	    AC_DEFINE(NC_NCLONG_T, int)
	    AC_DEFINE(SIZEOF_NCLONG, SIZEOF_INT)
	    AC_MSG_RESULT(int)
	    ;;
    esac
])


# Check for XDR header-file directory.
#
AC_DEFUN([UL_CPP_XDR], [dnl
AC_MSG_CHECKING(for XDR header-file)
AC_REQUIRE([UC_PREFIX])dnl
UC_TEST_DIR(CPP_XDR, /usr/[[include]]/rpc /usr/local/[[include]]/rpc \
    $prefix/[[include]] UC_ABSPATH(xdr), xdr.h,
    XDR [[[include]]]-directory, -I/usr/[[[include]]]/rpc)dnl
if test "${CPP_XDR-}"; then
  CPP_XDR=-I${CPP_XDR}
  AC_MSG_RESULT($CPP_XDR)
fi
])


# Ensure a valid XDR library.
#
AC_DEFUN([UL_LIB_XDR], [dnl
UC_CHECK_LIB(LD_XDR, xdr_long, , rpc nsl sun, XDR, -lrpc)dnl
case "$CPP_XDR" in
    -I/usr/include*)
	;;
    *)
	UC_ENSURE(CPPFLAGS, ${CPP_XDR})dnl
	;;
esac
UC_ENSURE(LIBS, ${LD_XDR-})dnl
AC_MSG_CHECKING(XDR implementation)
AC_TRY_RUN(dnl
[
#undef NDEBUG

#ifdef _AIX
#   ifndef _ALL_SOURCE
#   	define _ALL_SOURCE
#   endif
#endif

#include <assert.h>
#include <stdio.h>
#include <rpc/types.h>
#include <rpc/xdr.h>

#define TESTFILE	"conftest.xdr"
#ifndef FLT_MIN
#   define	FLT_MIN	1e-37		/* Standard C maximum */
#endif
#ifndef FLT_EPSILON
#   define FLT_EPSILON	1e-5		/* Standard C maximum */
#endif

main(ac,av)
    int			ac ;
    char		*av[] ;
{
    int			ii, jj	= 0 ;
    char		*fname	= ac > 1 ? av[1] : TESTFILE;
    FILE		*F	= fopen(fname,"w");
    XDR			xdrs[1] ;
    unsigned int	count ;
    unsigned int	szof ;

    /*
     * The last value below should be a subnormal number on a DECstation,
     * RS6000, and SPARCstation.  Hopefully, it will catch bad xdr(3)
     * implementations (such as a VAXstation's) which don't correctly handle
     * such values.  Such a value should be the last one in the list.
     *
     * The double test was put in for BSD/OS (it passes the float test).
     */
    static float	floats[]	= { 100.125, 100.25, 100.375, 
					    100.5, 100.625, FLT_MIN/32 } ;
    float *fp , got_af[sizeof(floats)/sizeof(floats[0])] ;

    static double       doubles[]       = { 100.125, 100.25, 100.375,
                                            100.5, 100.625, FLT_MIN/32 } ;
    double *dp , got_ad[sizeof(doubles)/sizeof(doubles[0])] ;


/* create */

    if( F == NULL)
    {
	    perror("fopen failed") ;
	    exit(-1) ;
    }
    xdrstdio_create(xdrs, F, XDR_ENCODE) ;

/* populate */

    szof = sizeof(float) ;
    count = sizeof(floats)/sizeof(float) ;
    assert( xdr_vector(xdrs, (char *)floats, count, szof, xdr_float)) ;

    szof = sizeof(double) ;
    count = sizeof(doubles)/sizeof(double) ;
    assert( xdr_vector(xdrs, (char *)doubles, count, szof, xdr_double)) ;

/* flush, rewind  and reopen */

    assert(fflush((FILE *)xdrs->x_private) != EOF) ; /* xdr_destroy(xdrs) */

    assert(fclose(F) != EOF) ;
    F = fopen(fname,"r") ;
    if( F == NULL)
    {
	    perror("fopen failed") ;
	    exit(-1) ;
    }
    xdrstdio_create(xdrs, F, XDR_DECODE) ;

/* check */

    szof = sizeof(float) ;
    count = sizeof(floats)/sizeof(float) ;
    assert( xdr_vector(xdrs, (char *)got_af, count, szof, xdr_float)) ;
    for(ii = 0, fp = got_af;
	ii < count - 1;
	ii++, fp++) 
    {
	assert(*fp <= floats[ii]*(1+FLT_EPSILON) && *fp >= floats[ii]*(1-FLT_EPSILON)) ;
    }
    assert(*fp == 0 || (*fp <= floats[ii]*(1+FLT_EPSILON) && *fp >= floats[ii]*(1-FLT_EPSILON))) ;

    szof = sizeof(double) ;
    count = sizeof(doubles)/sizeof(double) ;
    assert( xdr_vector(xdrs, (char *)got_ad, count, szof, xdr_double)) ;
    for(ii = 0, dp = got_ad;
        ii < count - 1;
        ii++, dp++)
    {
        assert(*dp <= doubles[ii]*(1+FLT_EPSILON) && *dp >= doubles[ii]*(1-FLT_EPSILON)) ;
    }
    assert(*dp == 0 || (*dp <= doubles[ii]*(1+FLT_EPSILON) && *dp >= doubles[ii]*(1-FLT_EPSILON))) ;

    exit(0) ;
}
],dnl
[
    AC_MSG_RESULT(ok)
    XDR_LIBOBJS=
    XDR_INSTALL_DEPS=
],
[
    AC_MSG_WARN(problem with system-supplied XDR: using own)
    XDR_LIBOBJS="xdr.o xdrfloat.o xdrstdio.o xdrarray.o"
    XDR_INSTALL_DEPS=installed_headers
    LD_XDR=
],
false
)
AC_SUBST(XDR_LIBOBJS)dnl
AC_SUBST(XDR_INSTALL_DEPS)dnl
AC_SUBST(LD_XDR)dnl
])


# Check which XDR module to use.
#
AC_DEFUN([UL_XDRFILE], [dnl
    AC_REQUIRE([UC_OS])dnl
    AC_MSG_CHECKING(which XDR module to use)
    case "$XDRFILE" in
	'')
	    case "$OS" in
		unicos*)
		    XDRFILE=xdrffio
		    ;;
		*)
		    XDRFILE=xdrposix
		    ;;
	    esac
	    ;;
    esac
    AC_MSG_RESULT($XDRFILE)
    AC_SUBST(XDRFILE) dnl
])dnl


# Check the type pointed to by the XDR `inline' function.
#
AC_DEFUN([UL_XDR_INLINE], [dnl
    AC_MSG_CHECKING(type pointed to by inline XDR function)
    AC_REQUIRE([UC_OS])dnl
    case "$OS" in
	unicos*)
	    INLINE=inline_t;;
	irix64*)
	    INLINE=long;;
	*)
	    INLINE=netlong;;
    esac
    AC_MSG_RESULT($INLINE)
    AC_DEFINE_UNQUOTED(INLINE, $INLINE)
])


# Check whether or not the `xdr_ops' structure has an `x_getint' member.
#
AC_DEFUN([UL_XDR_GETINT], [dnl
    AC_MSG_CHECKING(XDR structure xdr_ops for x_getint member)
    AC_TRY_COMPILE(
	[
#include <rpc/types.h>
#include <rpc/xdr.h>,
	],
	[
struct xdr_ops ops;
ops.x_getint = (int(*)())0;
	],
	[dnl
	    AC_MSG_RESULT(yes)
	    HAS_XDR_GETINT=1
	],
	[dnl
	    AC_MSG_RESULT(no)
	    HAS_XDR_GETINT=0
	])dnl
    AC_DEFINE_UNQUOTED(HAS_XDR_GETINT, $HAS_XDR_GETINT)dnl
])


# Check for XDR implementation (header file and library).
#
AC_DEFUN([UL_XDR], [dnl
UL_XDRFILE dnl
AC_REQUIRE([UL_CPP_XDR])dnl
AC_REQUIRE([UL_LIB_XDR])dnl
AC_REQUIRE([UL_XDR_INLINE])dnl
AC_REQUIRE([UL_XDR_GETINT])dnl
])


# Check the size of a block I/O buffer.
#
AC_DEFUN([UL_BLKIO_BUFSIZE], [dnl
    AC_REQUIRE([UC_OS])
    AC_MSG_CHECKING(for size of block buffer)
    case "$OS" in
	unicos*)
	    BIOBUFSIZ=196608;;
	*)  BIOBUFSIZ=8192;;
    esac
    AC_MSG_RESULT($BIOBUFSIZ)
    AC_DEFINE_UNQUOTED(BIOBUFSIZ, $BIOBUFSIZ)
])


# Check for IEEE floating-point arithmetic.
#
AC_DEFUN([UL_IEEE], [dnl
AC_MSG_CHECKING(compiler options for IEEE arithmetic)
AC_REQUIRE([UC_OS])dnl
case "$OS" in
osf*)
    case `uname -m` in
    alpha)
	UC_ENSURE(CFLAGS, -ieee_with_inexact)dnl
	AC_MSG_RESULT(-ieee_with_inexact)
	;;
    esac
    ;;
*)  AC_MSG_RESULT()
esac
])


# Define C macros for function prototyping
#
AC_DEFUN([UL_PROTOTYPES],
[dnl
    AC_MSG_CHECKING(for function prototype)
    AC_TRY_COMPILE(,
	[extern int foo(int bar);],
	AC_MSG_RESULT(yes),
	AC_MSG_RESULT(no)
	[UC_ENSURE(CPPFLAGS, -DNO_HAVE_PROTOTYPES)])dnl
])


# Check for functioning `const' keyword
#
AC_DEFUN([UL_CONST],
[
    AC_MSG_CHECKING(for working const)
    AC_TRY_COMPILE(,
	[
	    /* Ultrix mips cc rejects this.  */
	    typedef int charset[2]; const charset x;
	],
	[AC_MSG_RESULT(yes)],
	[dnl
	    UC_REPLACE(UD_NO_CONST,1)dnl
	    UC_ENSURE(CPPFLAGS, -Dconst=)dnl
	])dnl
])


# Define macros for variadic function support
#
AC_DEFUN([UL_VARIADIC_FUNCTIONS],
[dnl
    AC_REQUIRE([UC_PROG_CC])dnl
    AC_MSG_CHECKING(for variadic function support)
    AC_TRY_COMPILE(
	[#include <stdarg.h>],
	[
	    }
	    int foo(int bar, ...) {
		va_list     alist;
		va_start(alist, bar);
		bar = (int)va_arg(alist, int);
		va_end(alist);
	],
	[AC_MSG_RESULT(yes)],
	[dnl
	    AC_MSG_RESULT(no)
	    UC_ENSURE(CPPFLAGS, -DNO_STDARG)dnl
	]dnl
    )dnl
])


# Check for strerror().
#
AC_DEFUN([UL_STRERROR], [dnl
AC_FUNC_CHECK(strerror, , UC_ENSURE(CPPFLAGS, -DNO_STRERROR))dnl
])


# Check for <float.h>.
#
AC_DEFUN([UL_CHECK_FLOAT_H], [dnl
    AC_CHECK_HEADER(float.h,
	[dnl
	    CPP_NO_FLOAT_H=
	],dnl
	[dnl
	    CPP_NO_FLOAT_H=-DNO_FLOAT_H
	]dnl
    )dnl
    AC_SUBST(CPP_NO_FLOAT_H)dnl
])


divert(diversion_number)dnl
