/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Global run time system definitions.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*
 * Author: Jukka Virtanen
 *	   jtv@hut.fi
 *
 *	   Computing Centre
 *	   Helsinki University of Technology
 *	   Finland
 */

/* edit history:
    15 6 85: First version for Pax compiler at HUT.FI.

    Later (89?) I converted this for Gnu Pascal compiler (GPC).

    The run time system is rewritten for GPC and it should support
    all features of the Extended Pascal Standard. If you find deviances,
    please report them to <jtv@hut.fi>

    July '96 : adapted for GNU autoconf <j.j.vanderheijde@student.utwente.nl>
 */

#ifndef __RTS_H
#define __RTS_H

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#ifndef errno
extern int errno;
#endif

#include "limits.h"

#include "config.h" /* GCC xm-*.h configuration file for system type */

/* Created by autoconf */
#include "rts-config.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef MACH
void *malloc();
#endif

#ifdef STDC_HEADERS
# include <string.h>
#else
# ifndef HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
char *strchr(), *strrchr();
# ifndef HAVE_MEMCPY
#  define memcpy(d, s, n) bcopy ((s), (d), (n))
#  define memmove(d, s, n) bcopy ((s), (d), (n))
# endif
#endif

#ifdef BSD_RTS
# undef USG_RTS
# include <sys/file.h>
#else
# define USG_RTS
# include <fcntl.h>
#endif

/* If you don't have this file, you can generate it with
   the `enquire -f > float.h' command. Enquire comes
   with GCC distribution, and should have been made when
   you compiled GCC */
#include "float.h"	/* ANSI C float.h file */

/* It has a null device, but confuses it with stderr. (?!) */
#ifdef __DJGPP__
# define NO_DEVNULL
#endif

/* Features present */
#ifndef NO_DEVNULL
# define HAVE_DEVNULL		/* /dev/null in system */
#endif

/* Unset features */
/* # define HAVE_TIME		*/
/* # define WARN_READ_ONLY	*/ /*Warn if file opened in read only mode */
/* # define GPC_PACKED_STRUCTURES */	/* gettimestamp aborts() is this is set */

/* Un*x path components separated by '/' */
#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR '/'
#endif

# ifdef DEBUG
# define D(level, x)	do if (Gpc_debug >= level) x; while (0)
# else
# define D(level, x)
# endif

#ifdef HAVE_STRDUP
# define _p_strdup strdup
#endif

/* Compiler and run time system must agree on the representation
 * of COMPLEX data type for run time system complex functions
 * in rts-zmath.c
 */
#ifdef __GNUC__

typedef __complex__ double COMPLEX;

#define CPART_RE(z) (__real__ (z))
#define CPART_IM(z) (__imag__ (z))

#else

/* I used to use this always, but it does not work on machines that
 * use special techniques to pass structure arguments,
 * like the SPARC.
 */

typedef struct {
  double x;
  double y;
} COMPLEX;

#define CPART_RE(z) (z.x)
#define CPART_IM(z) (z.y)

#endif /* __GNUC__ */

/* Calculate LENGTH of complex number in polar coordinates */
#define z_LENGTH(z) sqrt(CPART_RE(z)*CPART_RE(z) + CPART_IM(z)*CPART_IM(z))

/* calculate ANGLE of complex number in polar coordinates == _p_arg (z) */
#define z_ANGLE(z)  atan2(CPART_IM(z), CPART_RE(z))

/* This is a RECORD_TYPE in the compiler, so it can be a struct.
 *
 * It works only if you pass a reference to this, because the
 * string field is variable length.
 */
typedef struct {
  	  int  Capacity;
	  int  length;
	  char string [ 1 ];
} STRING;

/* This type must match the compiler definition of BindingType
 * if gpc-util.c (Initialize_world())
 *
 * The standard fields are: BOUND and NAME
 *
 * The name is copied to heap, so the length does not matter for the
 * run time system.
 */
typedef struct {
    char Bound;
    char Extensions_valid;
    char Readable;
    char Writable;
    char Existing;
    int  Error;
    int  Size;
    STRING Name;
} GPC_BINDING;

/*
 * BIND (f, b);
 *
 * Do an implementation dependent binding of a variable to an external extity.
 *
 * f is a BINDABLE variable_access, and b is an EXPRESSION of type BindingType
 * PACKED RECORD
 *   bound : Boolean;
 *   name  : string
 * END;
 * with required field identifiers NAME and BOUND
 *
 * NAME is a variable-string-type
 * BOUND is of type Boolean
 */

typedef struct BindList {
    struct BindList *next;	/* list pointer 	   */
    void 	    *addr;	/* address of bound object */
    int		     type;	/* type of bound variable  */
    GPC_BINDING	    *to;	/* It's bound to this      */
    char	    *name;	/* But the name is here    */
    int		     status;    /* Additional status flags */
} BINDING;

/* A list of collected constructor addresses is built with these
 * Used in module initialization.
 */
typedef struct contype {
  void (*fun)();
  int  run_id;
  struct contype *next;
} CONSTRUCTOR;

#ifndef PROTO
# if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#  define PROTO(ARGS) ARGS
# else
#  define PROTO(ARGS) ()
# endif
#endif

#include "rts-types.h"
#include "rts-hdr.h"

/* More stuff */

# define NEWPAGE '\f'		/* PAGE(File) string */
# define NEWLINE '\n'		/* WRITELN writes this */
# define FALSE_str "False"	/* For Write Boolean */
# define TRUE_str  "True"
# define BOOL_OUT_WIDTH  6
# define REAL_OUT_WIDTH 14      /* digits in floating point output */
# define REAL_MIN_WIDTH  8	/* min # of chars outputting floats */
# define INT_OUT_WIDTH  10	/* digits in integer output */
# define TRUE	1
# define FALSE	0

# define EOT '\004'		/* File name queries abort if first char is EOT */

#ifndef NULL_DEVICE_NAME
# define NULL_DEVICE_NAME "/dev/null"
#endif

/* Signal handling types */
# define NONE   (0)
# define UND    (-1)
# define FAST   (-2)

/* error message handling codes */
# define ABORT  (-1)
# define REPORT (-2)
# define IGNORE (-3)

/* Access the _p_GLOBAL[] array as a pointer and integer */
/* the run time system stores HEAP limits there for error checking. */
# define GLOBAL_P(x) (*((char **)(&_p_GLOBAL[x])))
# define GLOBAL_I(x) (*((int *)(&_p_GLOBAL[x])))

/* heap limits, in _p_GLOBAL */
# define G_NP	(0)
# define G_NPE	(4)
# define G_NPB	(8)
# define GLOBAL_SIZE (G_NPB + sizeof (char *))

/* Gpc uses this value to detect uninitialized integer variables.
   Should be the most negative integer you can use with your hardware.

   @@ Not used yet.
 */
#define ILLINT	 INT_MIN

/* When reading integers, values greater than this before last digit
 * are trapped.
 */
#define READ_INT_TRAP ((int)(INT_MAX / 10))

/* General purpose macros */
#define isdigit(ch) ((ch) >= '0' && (ch) <= '9')
#define isspace(ch) ((ch) == ' ' || (ch) == '\t')
#define isspace_nl(ch) (isspace(ch) || (ch) == '\n')
#define islower(ch) ((ch) >= 'a' && (ch) <= 'z')
#define isupper(ch) ((ch) >= 'A' && (ch) <= 'Z')
#define tolower(ch) ((ch) + ' ')
#define toupper(ch) ((ch) - ' ')

/* association tabel for internal and external file names
   set with the "-a Intname:extname" option (Intname is the
   file name in your program, first letter capitalized)
 */
typedef struct assoc
{
	char	*int_name;
	char	*ext_name;
} assoc;

extern char     *gpc_rts_version;
extern int	Gpc_signal;
extern int	Gpc_sigcause;
extern int	Gpc_debug;
extern int	Gpc_warn;
extern assoc	*Gpc_assoc;
extern int        Gpc_argc;
extern char     **Gpc_argv;
extern char	**Gpc_envp;

extern int	Gpc_DEVNULL;
extern char 	*_p_GLOBAL[];

extern struct Fdr _p_stdin;
extern struct Fdr _p_stdout;

extern FILE       *current_stdin;

/* A list of saved constructors to be run */
extern CONSTRUCTOR *Gpc_c_list;

/* This is set nonzero when the constructors are collected */
extern int _p_collect_flag;

extern int _p_eoln_reset_hack;

/* 1 if direct access routines should work only for direct
 * access files.
 */
extern int _p_force_direct_files;

#endif /* __RTS_H */
