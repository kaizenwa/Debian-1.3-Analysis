/******************************************************************************
** $Id: parse.h,v 2.12 1996/02/22 21:43:51 gerd Exp gerd $
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

 extern char * filename;

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int parse _ARG((void));			   /* parse.c                */
 int read_rsc _ARG((char *name));		   /* parse.c                */
 int see _ARG((char * fname));			   /* parse.c                */
 int see_bib _ARG((char * fname));		   /* parse.c                */
 int see_bib_msg _ARG((char *s));		   /* parse.c                */
 int see_rsc _ARG((char * fname));		   /* parse.c                */
 int seen _ARG((void));				   /* parse.c                */
 void init_read _ARG((void));			   /* parse.c                */
 void normalize_symbol _ARG((char * s));	   /* parse.c                */
 void set_rsc_path _ARG((char * val));		   /* parse.c                */
 void set_symbol_type _ARG((char * s));		   /* parse.c                */
