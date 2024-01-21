/******************************************************************************
** $Id: sbuffer.h,v 2.12 1996/02/22 21:43:51 gerd Exp gerd $
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

#ifndef  sbputchar

 typedef struct 
 { char *sb__string;
   int  sb__size;
   int  sb__ptr;
 } StringBuffer;

#define StringBufferIncrement 256


 extern StringBuffer* sbopen(
#ifdef __STDC__
		       void
#endif
		      );
 extern int     sbclose(
#ifdef __STDC__
		       StringBuffer*
#endif
		      );
 extern int      sbputs(
#ifdef __STDC__
		       char *,
		       StringBuffer*
#endif
		      );
 extern int      sbputc(
#ifdef __STDC__
		       int,
		       StringBuffer*
#endif
		      );
 extern char*   sbflush(
#ifdef __STDC__
		       StringBuffer*
#endif
		      );
 extern int      sbtell(
#ifdef __STDC__
		       StringBuffer*
#endif
		      );
 extern void   sbrewind(
#ifdef __STDC__
		       StringBuffer*
#endif
		      );
 extern int      sbseek(
#ifdef __STDC__
		       StringBuffer*,
		       int
#endif
		      );

#define sbputchar(C,SB) ((SB)->sb__ptr < (SB)->sb__size		\
			? (SB)->sb__string[(SB)->sb__ptr++] = C	\
			: sbputc(C,SB))

#endif
