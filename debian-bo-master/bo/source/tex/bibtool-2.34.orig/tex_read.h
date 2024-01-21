/******************************************************************************
** $Id: tex_read.h,v 2.10 1996/02/22 21:43:51 gerd Exp gerd $
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

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int TeX_read _ARG((char * cp,char **sp));	   /* tex-read.c             */
 void TeX_active _ARG((int c,int arity,char * s)); /* tex-read.c             */
 void TeX_close _ARG((void));			   /* tex-read.c             */
 void TeX_def _ARG((char *s));			   /* tex-read.c             */
 void TeX_define _ARG((char *name,int arity,char *body));/* tex-read.c       */
 void TeX_open_file _ARG((FILE * file));	   /* tex-read.c             */
 void TeX_open_string _ARG((char * s));		   /* tex-read.c             */
