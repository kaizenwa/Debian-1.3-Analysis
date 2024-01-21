/******************************************************************************
** $Id: rewrite.h,v 2.12 1996/02/22 21:43:51 gerd Exp gerd $
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
 int is_selected _ARG((Record rec));		   /* rewrite.c              */
 void add_check_rule _ARG((char *s));		   /* rewrite.c              */
 void add_extract _ARG((char *s));		   /* rewrite.c              */
 void add_field _ARG((char *spec));		   /* rewrite.c              */
 void add_rewrite_rule _ARG((char *s));		   /* rewrite.c              */
 void delete_field _ARG((char *token));		   /* rewrite.c              */
 void remove_field _ARG((char *field,Record rec)); /* rewrite.c              */
 void rewrite_record _ARG((Record rec));	   /* rewrite.c              */
 void save_regex _ARG((char *s));		   /* rewrite.c              */
