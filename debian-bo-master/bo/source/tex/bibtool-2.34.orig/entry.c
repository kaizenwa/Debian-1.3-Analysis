/******************************************************************************
** $Id: entry.c,v 2.16 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "entry.h"
#include "error.h"
#include "type.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int find_entry_type _ARG((char *s));		   /* entry.c                */
 static int match _ARG((char *s,char *t));	   /* entry.c                */
 void def_entry_type _ARG((char * s));		   /* entry.c                */
 void entry_statistics _ARG((int all));		   /* entry.c                */
 void init_entries _ARG((void));		   /* entry.c                */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

#define EntrySizeIncrement 8

 StringTab *entry_type;
 int entry_ptr	= 0;
 int entry_size = 0;

/*-----------------------------------------------------------------------------
** Function:	init_entries()
** Purpose:	Predefine some entry types which are stored at compile time
**		in an array.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
void init_entries()				   /*			     */
{						   /*			     */
#ifdef INITIALIZE_BIBTEX_ENTRIES
  static char *word_list[] =			   /* default entry types.   */
  { INITIALIZE_BIBTEX_ENTRIES, NULL };		   /* Mark the end with NULL */
  register char**wp;				   /*			     */
#endif
						   /*			     */
  def_entry_type("STRING"	);		   /*			     */
  def_entry_type("PREAMBLE"	);		   /*			     */
  def_entry_type("COMMENT"	);		   /*			     */
#ifdef INITIALIZE_BIBTEX_ENTRIES
  for ( wp=word_list; *wp!=NULL; ++wp )		   /* add ignored words.     */
  { def_entry_type(*wp); }			   /*			     */
#endif
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	def_entry_type()
** Purpose:	Dynamically define an entry type.
** Arguments:
**	s	STring containg the name of the entry.
** Returns:	nothing
**___________________________________________________			     */
void def_entry_type(s)				   /*			     */
  register char * s;				   /*			     */
{ register int i;				   /*                        */
 						   /*                        */
  for (i=0; i<entry_ptr; ++i)			   /*			     */
  { 						   /*                        */
    if ( case_cmp(s,EntryName(i))!=0 )		   /*			     */
    { EntryName(i) = s;				   /*			     */
      return;				   	   /*			     */
    }						   /*			     */
  }						   /*			     */
 						   /*                        */
  if ( entry_ptr <= entry_size )		   /*			     */
  { entry_size += EntrySizeIncrement;		   /*			     */
    entry_type = ( entry_ptr == 0		   /*			     */
		  ? (StringTab*)malloc((size_t)(entry_size*sizeof(StringTab)))
		  : (StringTab*)realloc((char*)entry_type,
					(size_t)(entry_size*sizeof(StringTab)))
		  );				   /*			     */
    if ( entry_type == NULL )			   /*			     */
    { OUT_OF_MEMORY("entry type"); }		   /*                        */
  }						   /*			     */
  entry_type[entry_ptr++] = new_string_tab(s,0,0); /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	match()
** Purpose:	Compare two strings ignoring case.
**		Return TRUE iff they are identical or the second string is a
**		substring not followed by a letter or digit.
** Arguments:
**	s	First string
**	t	Second string
** Returns:	
**___________________________________________________			     */
static int match(s,t)				   /*			     */
  register char *s;				   /*			     */
  register char *t;				   /*			     */
{						   /*			     */
  while( *t )					   /*			     */
  { if ( ToLower(*s) != ToLower(*t) ) return(FALSE);/*			     */
    s++; t++;					   /*			     */
  }						   /*			     */
  return( is_alpha(*s) || is_digit(*s) ? FALSE : TRUE );/*		     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	find_entry_type()
** Purpose:	Look up an entry type in the array of defined entries.
** Arguments:
**	s	
** Returns:	The index in the array or NOOP
**___________________________________________________			     */
int find_entry_type(s)				   /*			     */
  register char *s;				   /*			     */
{ register int i;				   /*			     */
						   /*			     */
  for (i=0; i<entry_ptr; ++i)			   /*			     */
  { if ( match(s,EntryName(i)) )		   /*			     */
    { EntryCount(i)++;				   /*			     */
      return(i);				   /*			     */
    }						   /*			     */
  }						   /*			     */
  return NOOP;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	entry_statistics()
** Purpose:	Print a statistics on used entry types.
** Arguments:
**	all	boolean. If all==0 only the used entry types are listed.
** Returns:	nothing
**___________________________________________________			     */
void entry_statistics(all)			   /*			     */
  register int all;				   /*			     */
{ register int i;				   /*			     */
						   /*			     */
  (void)fputc('\n',err_file);		   	   /*			     */
  for (i=0; i<entry_ptr; ++i)			   /*			     */
  { if ( all || EntryCount(i) > 0 )		   /*			     */
    { (void)fprintf(err_file,			   /*			     */
		    "---  %-15s %5d read  %5d written\n",/*		     */
		    EntryName(i),		   /*			     */
		    EntryCount(i),		   /*			     */
		    EntryUsed(i));		   /*			     */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

