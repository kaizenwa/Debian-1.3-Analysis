/* Configuration file for Checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written August 1993 by Tristan Gingold.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

#ifndef _DEFINE_H_
#define _DEFINE_H_

/** WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING **
 ** Don't change the content of this file.  Most of the combinaison of      **
 ** defines are not tested.                                                 **
 ****************************************************************************/
 
/* Secure test */
#ifdef __CHECKER__
#error "Checker must *not* be compiled with itself."
#endif

#ifdef __GNUC__
#define ATTRIBUTE(x) __attribute__(x)
#define INLINE __inline__
#define PTR void *
#else
#define ATTRIBUTE(x)
#define INLINE
#define PTR char *
#define abs(x) ((x) > 0 ? (x) : -(x))
#endif

/* Configuration file *******************************************************
 * Comment the '#define' line, if you don't want to use a feature           *
 ****************************************************************************/


/**************************************************************************** 
 * These features are independant from Checker. They come directly from     *
 * the standard GNU Malloc package.                                         *
 ****************************************************************************/
/* if CHKR_MSTAT is defined, malloc updates the memory statistics.
   mstats() can be used.
   If it is not defined, malloc and free are slightly faster. */
#define CHKR_MSTAT	1

/* if CHKR_HOOK is defined, Checker supports hooks. It is necessary to use
   mcheck() and mtrace().
   If it is not defined, Checker is slightly faster */
#define CHKR_HOOK	1

/* if CHECKER is not defined, Checker will be a simple malloc package. */
#define CHECKER		1

/* if CHKR_DEBUG is defined, some functions, that display internal table or
   that do internal check will be enable. Define it only when you debug
   checker, or when you works on the sources */
#define CHKR_DEBUG	1

/* If defined, the output file is check before each write. */
#undef CHECK_OUTPUT_FILE

/**************************************************************************
 * CHECKER must be defined to use the following features                  *
 **************************************************************************/
/* if CHKR_GARBAGE is defined, the garbage detector will be enable */
#define CHKR_GARBAGE	1

/* if CHKR_SAVESTACK is defined, Checker will access to the symbol table */
#define CHKR_SAVESTACK	1


/**************************************************************************
 * These are for detecting access error                                   *
 **************************************************************************/
#if !defined(PLCHECKER) && !defined(MDCHECKER)

/* Which sections have a protection bitmap */
#define CHKR_DATABITMAP	1
#define CHKR_HEAPBITMAP	1
/* CHKR_STACKBITMAP can be defined only if CHKR_HEAPBITMAP is defined */
#define CHKR_STACKBITMAP 1

#endif
/**************************************************************************
 *  stuff                                                                 *
 **************************************************************************/
/* 1 if there are no bugs... If not defined, this will add code */
/*#define CHKR_IS_SAFE */

/* define, if you want to be compatible with previous versions. Unuseful */ 
/*#define OLD_CHKR */

/* define, if you want to allow profile. Little slower. */
#define CHKR_PROFILE

/* skeleton of the tempory file */
#define TMP_FILE "/tmp/Chkr.XXXXXX"

/**************************************************************************
 * Do not change define.h below this. This is to prevent anormal value    *
 **************************************************************************/
/* CHKR_GARBAGE, CHKR_SAVESTACK, CHKR_HEAPBITMAP, CHKR_DATABITMAP 
    cannot be defined if CHECKER is not */
#ifndef CHECKER
# ifdef CHKR_GARBAGE
#  undef CHKR_GARBAGE
# endif
# ifdef CHKR_SAVESTACK
#  undef CHKR_SAVESTACK
# endif
# ifdef CHKR_DATABITMAP
#  undef CHKR_DATABITMAP
# endif
# ifdef CHKR_HEAPBITMAP
#  undef CHKR_HEAPBITMAP
# endif
#endif

/* CHKR_STACKBM can't be defined if CHKR_HEAPBITMAP is not defined */
#ifndef CHKR_HEAPBITMAP
# ifdef CHKR_STACKBM
#  undef CHKR_STACKBM
# endif
#endif

/* Can't define CHKR_PROFILE if bitmaps are not enable */
#if 0
#ifdef MDCHECKER
#undef CHKR_PROFILE
#endif
#endif

#endif /* _DEFINE_H_ */
