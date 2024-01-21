/******************************************************************************
** $Id: key.h,v 2.11 1996/02/22 21:43:51 gerd Exp gerd $
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
 char *get_field _ARG((Record rec,char *name));	   /* key.c                  */
 void add_format _ARG((char *s));		   /* key.c                  */
 void add_ignored_word _ARG((char *s));		   /* key.c                  */
 void add_sort_format _ARG((char *s));		   /* key.c                  */
 void make_key _ARG((Record rec));		   /* key.c                  */
 void make_sort_key _ARG((Record rec));		   /* key.c                  */
 void mark_key _ARG((Record rec));		   /* key.c                  */
 void set_base _ARG((char *s));			   /* key.c                  */
 void set_separator _ARG((int n,char *s));	   /* key.c                  */
