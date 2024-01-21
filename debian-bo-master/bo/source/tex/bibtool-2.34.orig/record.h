/******************************************************************************
** $Id: record.h,v 2.11 1996/02/22 21:43:51 gerd Exp gerd $
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

#ifndef RecordNULL

typedef struct rECORD
  { char	  * rc_key;
    char	  * rc_old_key;
    int		    rc_token,
		    rc_free;
    char	  ** rc_heap;
    struct rECORD * rc_next;
    struct rECORD * rc_prev;
  } SRecord, *Record;

#define RecordNULL		(Record)0
#define RecNULL			(Record)0


#define RecordTokenMask		0x7fff
#define RecordTokenXREF		0x8000
#define SetRecordXREF(R)	(((R)->rc_token)|=RecordTokenXREF)
#define IsRecordXREF(R)		(((R)->rc_token)&RecordTokenXREF)
#define RecordToken(R)		((R)->rc_token & RecordTokenMask)
#define Record_Full_Token(R)	((R)->rc_token)

#define RecordOldKey(R)		((R)->rc_old_key)
#define RecordKey(R)		((R)->rc_key)
#define RecordFree(R)		((R)->rc_free)
#define RecordHeap(R)		((R)->rc_heap)
#define NextRecord(R)		((R)->rc_next)
#define PrevRecord(R)		((R)->rc_prev)


#include "wordlist.h"

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 Record copy_record _ARG((Record rec));		   /* record.c               */
 Record get_master_record _ARG((void));		   /* record.c               */
 WordList new_wordlist _ARG((char * s));	   /* record.c               */
 void add_sort_order _ARG((char *val));		   /* record.c               */
 void clear_master_record _ARG((int token));	   /* record.c               */
 void push_to_master_record _ARG((char *s,char *t));/* record.c              */
 void sort_record _ARG((Record rec));		   /* record.c               */

#endif
