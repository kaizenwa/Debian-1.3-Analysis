/******************************************************************************
** $Id: entry.h,v 2.11 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "symbols.h"


 extern StringTab *entry_type;

#define EntryName(I)  entry_type[I]->st_name
#define EntryCount(I) entry_type[I]->st_count
#define EntryUsed(I)  entry_type[I]->st_flags

#define EndOfFile -2
#define NOOP	  -1
#define STRING	   0
#define PREAMBLE   1
#define COMMENT	   2

#define IsSpecialRecord(T) ( T <= 2 )
#define IsNormalRecord(T)  ( T > 2 )



#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int find_entry_type _ARG((char *s));		   /* entry.c                */
 void def_entry_type _ARG((char * s));		   /* entry.c                */
 void entry_statistics _ARG((int all));		   /* entry.c                */
 void init_entries _ARG((void));		   /* entry.c                */
