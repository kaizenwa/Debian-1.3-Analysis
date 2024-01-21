/******************************************************************************
** $Id: record.c,v 2.14 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "bibtool.h"
#include "error.h"
#include "type.h"
#include "record.h"
#include "symbols.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
** Function:	copy_record()
** Purpose:	Copy a record.
** Arguments:
**	rec
** Returns:	
**___________________________________________________			     */
Record copy_record(rec)				   /*			     */
  register Record rec;				   /*			     */
{ register Record new;				   /*			     */
  register char	 **new_heap,			   /*			     */
		 **old_heap;			   /*			     */
  register int	 i;				   /*			     */
						   /*			     */
  if (	(new=(Record)malloc(sizeof(SRecord))) == 0L/*			     */
     || (new_heap=(char**)malloc(sizeof(char*)*(size_t)RecordFree(rec)))/*   */
	== 0L )					   /*			     */
  { OUT_OF_MEMORY("Record"); }      		   /*                        */
  RecordKey(new)	  = "";			   /*			     */
  RecordOldKey(new)	  = RecordOldKey(rec);	   /*			     */
  NextRecord(new)	  = RecNULL;		   /*			     */
  PrevRecord(new)	  = RecNULL;		   /*			     */
  Record_Full_Token(new)  = Record_Full_Token(rec);/*			     */
  RecordFree(new)	  = RecordFree(rec);	   /*			     */
  RecordHeap(new)	  = new_heap;		   /*			     */
  for ( i=0, old_heap=RecordHeap(rec);		   /*			     */
	i < RecordFree(new);			   /*			     */
	++i )					   /*			     */
  { *(new_heap++) = *(old_heap++); }		   /*			     */
  return (new);					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			                         			   ***/
/*****************************************************************************/

#define HEAP_INIT 16
#define HEAP_INC  8

 static Record	master_record = RecordNULL;
 static size_t	master_size   = 0;
 char		*sym_crossref = (char*)0;

/*-----------------------------------------------------------------------------
** Function:	clear_master_record()
** Purpose:	This function initializes this master record properly.
**		The master record is a static Record structure used by
**		the parser to deposit its intermediate results.
** Arguments:
**	token
** Returns:	nothing
**___________________________________________________			     */
void clear_master_record(token)			   /*			     */
  register int token;				   /*			     */
{						   /*			     */
  if ( master_record == RecordNULL )		   /*			     */
  { master_size = HEAP_INIT;			   /*			     */
    if (   RecordNULL == (master_record		   /*			     */
			  =(Record)malloc(sizeof(SRecord)))/*		     */
	|| (char**)0  == (RecordHeap(master_record)/*			     */
			  =(char**)malloc(master_size*sizeof(char*))) )/*    */
    { OUT_OF_MEMORY("master record"); }		   /*                        */
    sym_crossref = sym_add("crossref",-1);	   /*			     */
  }						   /*			     */
  RecordOldKey(master_record)	   = NULL;	   /*			     */
  Record_Full_Token(master_record) = token;	   /*			     */
  RecordFree(master_record)	   = 0;		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	push_to_master_record()
** Purpose:	Put an equation s=t onto the master record.
** Arguments:
**	s
**	t
** Returns:	nothing
**___________________________________________________			     */
void push_to_master_record(s,t)			   /*			     */
  register char *s;				   /*			     */
  register char *t;				   /*			     */
{ register int i;				   /*			     */
						   /*			     */
  if ( (i=RecordFree(master_record)) >= master_size-2 )/*		     */
  { master_size += HEAP_INC;			   /*			     */
    if ( (char**)0 == (RecordHeap(master_record)   /*			     */
		       =(char**)realloc(RecordHeap(master_record),/*	     */
					master_size*sizeof(char*))) )/*	     */
    { OUT_OF_MEMORY("heap"); }      		   /*                        */
  }						   /*			     */
						   /*			     */
  RecordHeap(master_record)[i++] = s;		   /*			     */
  RecordHeap(master_record)[i]	 = t;		   /*			     */
  RecordFree(master_record)	+= 2;		   /*			     */
  if ( s == sym_crossref )			   /*			     */
  { SetRecordXREF(master_record); }		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	get_master_record()
** Purpose:	Return the address of the master record structure.
** Arguments:	none
** Returns:	the address
**___________________________________________________			     */
Record get_master_record()			   /*			     */
{ return master_record;				   /*			     */
}						   /*------------------------*/



#include "wordlist.h"
#include "s_parse.h"
#include "entry.h"
 

/*-----------------------------------------------------------------------------
** Function:	new_wordlist()
** Purpose:	Allocate a WordList and fill its slots.
** Arguments:
**	s	Initial string to fill in the WordList structure
** Returns:	
**___________________________________________________			     */
WordList new_wordlist(s)			   /*                        */
  register char * s;				   /*                        */
{ register WordList wl;				   /*                        */
  if ( (wl=(WordList)malloc(sizeof(SWordList))) == WordNULL )/*              */
  { OUT_OF_MEMORY("WordList"); }		   /*                        */
  ThisWord(wl) = s;				   /*                        */
  NextWord(wl) = WordNULL;			   /*                        */
  return wl;					   /*                        */
}						   /*------------------------*/


 typedef struct oRDERLIST			   /*                        */
 { int		    ol_type;			   /*                        */
   WordList	    ol_val;			   /*                        */
   struct oRDERLIST *ol_next;			   /*                        */
 } SOrderList, *OrderList;			   /*                        */

#define OrderType(OL) ((OL)->ol_type)
#define OrderVal(OL)  ((OL)->ol_val)
#define NextOrder(OL) ((OL)->ol_next)
#define OrderNULL     ((OrderList)0)

 static OrderList order = OrderNULL;		   /*                        */

/*-----------------------------------------------------------------------------
** Function:	add_sort_order()
** Purpose:	Insert the sort order into the order list.
**		
**
** Arguments:
**	val	string resource of the order.
** Returns:	nothing
**___________________________________________________			     */
void add_sort_order(val)			   /*                        */
  char      *val;				   /*                        */
{ char      *s;					   /*                        */
  int       type;				   /*                        */
  OrderList ol;					   /*                        */
  WordList  *wlp, wl, wl_next;			   /*                        */
 						   /*                        */
  SParseSkip(&val);				   /*                        */
  if ( *val == '*' )				   /*                        */
  { s    = NULL;				   /*                        */
    type = NOOP;				   /*                        */
    val++;					   /*                        */
  }						   /*                        */
  else if ( (s=SParseSymbol(&val)) == NULL )	   /*                        */
  { return; }					   /*                        */
  else if ( (type=find_entry_type(s)) == NOOP )	   /*                        */
  { Err("Undefined entry type for sort order");	   /*                        */
    return;					   /*                        */
  }						   /*                        */
 						   /*                        */
  for ( ol=order;				   /*                        */
       ol != OrderNULL && OrderType(ol) != type;   /*                        */
       ol=NextOrder(ol) ) {}			   /*                        */
 						   /*                        */
  if ( ol )					   /*                        */
  { wlp = &OrderVal(ol);			   /*                        */
  } 						   /*                        */
  else if ( (ol=(OrderList)malloc(sizeof(SOrderList))) == OrderNULL )/*      */
  { OUT_OF_MEMORY("OrderList"); }		   /*                        */
  else						   /*                        */
  { OrderType(ol) = type;			   /*                        */
    OrderVal(ol)  = WordNULL;			   /*                        */
    NextOrder(ol) = order;			   /*                        */
    order         = ol;				   /*                        */
    wlp           = &OrderVal(ol);		   /*                        */
  }						   /*                        */
   						   /*                        */
  SParseSkip(&val);				   /*                        */
  						   /*                        */
  while ( *val )				   /*                        */
  { if ( (s=SParseSymbol(&val)) != NULL )	   /*                        */
    { if ( *wlp ) { ThisWord(*wlp) = s;     }	   /*                        */
      else        { *wlp = new_wordlist(s); }	   /*                        */
      wlp = &NextWord(*wlp);			   /*                        */
    }						   /*                        */
    else break;					   /*                        */
    SParseSkip(&val);				   /*                        */
  }						   /*                        */
  						   /*                        */
  wl   = *wlp;					   /*                        */
  *wlp = WordNULL;				   /*                        */
  while ( wl )					   /* Free remaining word    */
  { wl_next = NextWord(wl);			   /*  nodes at the end.     */
    free((char*)wl);				   /*                        */
    wl = wl_next;				   /*                        */
  }						   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sort_record()
** Purpose:	The heap is reordered according to the sorting order
**		determined by the record type.
**		For this purpose a copy of the original record is made and the
**		original record is overwritten. The copy is released at the
**		end.
**		Memory management is easy since all strings are in fact
**		symbols, i.e. they must not be freed and comparison is done
**		by pointer comparison.
** Arguments:
**	rec     Record to sort
** Returns:	nothing
**___________________________________________________			     */
void sort_record(rec)				   /*			     */
  register Record rec;				   /*			     */
{ Record          r;				   /* Temp. record           */
  OrderList       ol;				   /*                        */
  int             i, ptr;			   /*                        */
  int             type = RecordToken(rec);	   /*                        */
  WordList        wl;				   /*                        */
 						   /*                        */
  for ( ol=order;				   /* Search for an order    */
       ol && OrderType(ol) != type;		   /*  for this type of      */
       ol = NextOrder(ol) ) {}			   /*  record first.         */
  if ( ol == OrderNULL )			   /* If none was found then */
  { for ( ol=order;				   /* Search for an order    */
	 ol && OrderType(ol) != NOOP;		   /*  applicable for all    */
	 ol = NextOrder(ol) ) {}		   /*  record types.         */
    if ( ol == OrderNULL ) return;		   /* No order then return.  */
  }						   /*                        */
  						   /*                        */
  if ( (wl=OrderVal(ol))==WordNULL ) return;	   /* Empty order found. Done*/
 						   /*                        */
  r   = copy_record(rec);			   /*                        */
  ptr = 2;					   /* Heap pointer for rec   */
 						   /*                        */
  while ( wl )					   /* For all words          */
  { for ( i = 2; i < RecordFree(r); i += 2 )	   /* Find the word on the   */
    { if ( RecordHeap(r)[i] == ThisWord(wl) )	   /*  heap and              */
      { RecordHeap(rec)[ptr++] = RecordHeap(r)[i]; /*  transfer it into the  */
	RecordHeap(rec)[ptr++] = RecordHeap(r)[i+1];/* original record.      */
	RecordHeap(r)[i]       = NULL;		   /* Delete the copy.       */
	i = RecordFree(r);			   /*                        */
      }						   /*                        */
    }   					   /*                        */
    wl = NextWord(wl);				   /* Look at next word.     */
  }						   /*                        */
  for ( i = 2; i < RecordFree(r); i += 2 )	   /* Transfer all remaining */
  { if ( RecordHeap(r)[i] )			   /*  elements of the heap  */
    { RecordHeap(rec)[ptr++] = RecordHeap(r)[i];   /*  which are not         */
      RecordHeap(rec)[ptr++] = RecordHeap(r)[i+1]; /*  deleted.              */
    }						   /*                        */
  }   						   /*                        */
  while ( ptr < RecordFree(r) )			   /* Clear the remaining    */
  { RecordHeap(rec)[ptr++] = NULL;		   /*  elements of the heap. */
  }						   /*                        */
 						   /*                        */
  free((char*)RecordHeap(r));			   /* Free the temp. record. */
  free((char*)r);				   /*                        */
}						   /*------------------------*/

