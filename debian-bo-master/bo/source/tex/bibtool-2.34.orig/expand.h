/******************************************************************************
** $Id: expand.h,v 1.6 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "sbuffer.h"


#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 char * expand_rhs _ARG((char *s,int braces));	   /* expand.c               */
