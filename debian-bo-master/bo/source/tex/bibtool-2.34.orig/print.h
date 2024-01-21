/******************************************************************************
** $Id: print.h,v 2.13 1996/02/22 21:43:51 gerd Exp gerd $
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

#define tell TELL /* Just because tell() is already used */

#include "record.h"

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int tell _ARG((char * fname));			   /* print.c                */
 int told _ARG((void));				   /* print.c                */
 void print_commented _ARG((Record rec));	   /* print.c                */
 void print_record _ARG((Record rec));		   /* print.c                */
 void putc_in _ARG((int c));			   /* print.c                */
 void puts_in _ARG((char *s, int in));	   	   /* print.c                */
 void set_symbol_type _ARG((char * s));		   /* print.c                */
