/******************************************************************************
** $Id: stack.h,v 2.10 1996/02/22 21:43:51 gerd Exp gerd $
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
 char * pop_string _ARG((void));		   /* stack.c                */
 void push_string _ARG((char * s));		   /* stack.c                */
