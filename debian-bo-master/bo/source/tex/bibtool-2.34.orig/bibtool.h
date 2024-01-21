/******************************************************************************
** $Id: bibtool.h,v 2.14 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#ifndef BIBTOOL_H
#define BIBTOOL_H

#include "config.h"

#include <stdio.h>

#ifdef __STDC__
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef MAYBE_NOT
 int fputs(char*,FILE*);
 int fputc(char,FILE*);
 int fclose(FILE*);
 int fflush(FILE*);
 int fprintf(FILE*,char*, ...);
 int printf(char*, ...);
#endif


#else

#ifdef OLD_STYLE_STRINGS
#include <strings.h>
#define strchr(A,B)  index(A,B)
#define strrchr(A,B) rindex(A,B)
#else
#include <string.h>
#endif

#ifdef HAS_stdlib
#include <stdlib.h>

#else
 extern void    exit();
 extern VoidPTR malloc();
 extern VoidPTR realloc();
 extern char    *getenv();
#ifdef SIZE_T
#define size_t		SIZE_T
#else
#define size_t		int
#endif
#endif
#endif


/*---------------------------------------------------------------------------*/
/* Misc definitions                                                          */
/*---------------------------------------------------------------------------*/

#ifndef TRUE
#define TRUE  (-1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#define FOREVER for (;;)

#define uchar UNSIGNED_CHAR
 typedef unsigned char uchar;

#endif
