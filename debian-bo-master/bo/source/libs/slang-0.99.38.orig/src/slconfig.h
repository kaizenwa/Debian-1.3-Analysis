/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

/* This is for everything but unix */

#ifdef VMS
# ifdef __DECC
#  define HAVE_STDLIB_H
# endif
#endif

#if defined(__WATCOMC__) && !defined(__QNX__)
#  ifndef msdos
#    define msdos
#  endif
#  ifndef  DOS386
#    define  DOS386
#  endif
#  ifndef FLOAT_TYPE
#    define FLOAT_TYPE
#  endif
#  ifndef pc_system
#    define pc_system
#  endif
#endif /* __watcomc__ */

/* Set of the various defines for pc systems.  This includes OS/2 */

/* DJGPP has a split personality.  It tries implement a unix like environment
 * under MSDOS.  Unfortunately, the personalities clash.
 */
#ifdef __GO32__
# ifndef pc_system
#   define pc_system
# endif
# ifdef REAL_UNIX_SYSTEM
#   undef REAL_UNIX_SYSTEM
# endif
# undef msdos
# undef MSDOS
# undef __MSDOS
# undef __MSDOS__
# ifndef __DJGPP__
#  define __DJGPP__ 1
# endif
#endif

#ifdef __MSDOS__
# ifndef msdos
#   define msdos
# endif
# ifndef pc_system
#   define pc_system
# endif
#endif

#if defined(__EMX__) && defined(OS2)
# ifndef pc_system
#   define pc_system
# endif
# ifndef __os2__
#   define __os2__
# endif
#endif


#ifdef pc_system
# define HAVE_STDLIB_H
# define HAVE_PUTENV
# ifdef __GO32__
#  define HAVE_UNISTD_H
# endif
#endif

#if !defined(VMS) && !defined(__WATCOMC__)
# define HAVE_MEMORY_H
#endif

#define HAVE_MEMCPY
#define HAVE_MEMSET
#ifndef VMS
# define HAVE_MEMCMP
# define HAVE_MEMCHR
#endif

#define HAVE_GETCWD 1

#ifndef VMS
# define HAVE_VFSCANF 1
#endif
