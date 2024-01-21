/******************************************************************************
** $Id: wordlist.c,v 2.16 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "wordlist.h"
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
 int find_word _ARG((char *s,WordList wl));	   /* wordlist.c	     */
 void list_words _ARG((WordList wl));		   /* wordlist.c	     */
 void save_word _ARG((char *s,WordList *wlp));	   /* wordlist.c	     */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
** Function:	save_word()
** Purpose:	Put a word into a word list.
** Arguments:
**	s
**	wlp
** Returns:	nothing
**___________________________________________________			     */
void save_word(s,wlp)				   /*			     */
  register char	    *s;				   /*			     */
  register WordList *wlp;			   /*			     */
{ register WordList wl;				   /*			     */
  register int	    cmp = 1;			   /*			     */
						   /*			     */
  while ( *wlp != WordNULL			   /*			     */
	 && (cmp=strcmp(ThisWord(*wlp),s)) < 0 )   /*			     */
  { wlp = & NextWord(*wlp); }			   /*			     */
						   /*			     */
  if ( cmp == 0 ) return;			   /*			     */
						   /*			     */
  if ( (wl=(WordList)malloc(sizeof(SWordList))) == WordNULL )/*		     */
  { OUT_OF_MEMORY("WordList"); }   		   /*                        */
						   /*			     */
  ThisWord(wl) = s;				   /*			     */
  NextWord(wl) = *wlp;				   /*			     */
  *wlp	       = wl;				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	find_word()
** Purpose:	Look up a word in a word list.
**
** Arguments:
**	s
**	wl
** Returns:	
**___________________________________________________			     */
int find_word(s,wl)				   /*			     */
  register char	    *s;				   /*			     */
  register WordList wl;				   /*			     */
{ register int	    cmp = 0;			   /*			     */
						   /*			     */
  while ( wl != WordNULL			   /*			     */
	 && (cmp=case_cmp(ThisWord(wl),s)) == 0 )  /*			     */
  { wl = NextWord(wl); }			   /*			     */
					   	   /*			     */
  return ( cmp != 0 );				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	list_words()
** Purpose:	List all words in the word list.
** Arguments:
**	wl
** Returns:	nothing
**___________________________________________________			     */
void list_words(wl)				   /*			     */
  WordList wl;					   /*			     */
{						   /*			     */
  for ( ; wl!=WordNULL; wl=NextWord(wl) )	   /*			     */
  { ErrPrintF("%s\n",ThisWord(wl)); }		   /*			     */
}						   /*------------------------*/
