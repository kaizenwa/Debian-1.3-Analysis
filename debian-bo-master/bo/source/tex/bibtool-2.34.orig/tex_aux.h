/******************************************************************************
** $Id: tex_aux.h,v 1.7 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "record.h"

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int lookup_item _ARG((Record rec));		   /* tex_aux.c              */
 void read_aux _ARG((char *fname,int verbose));	   /* tex_aux.c              */
 void save_ref _ARG((char *s));			   /* tex_aux.c              */
 void save_regex _ARG((char *s));		   /* tex_aux.c              */
