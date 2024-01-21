/******************************************************************************
** $Id: type.c,v 2.13 1996/02/22 21:43:51 gerd Exp gerd $
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

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#define INIT_TYPE
#include "type.h"

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

#define to_lower(C)           ((C) + 'a'-'A')
#define to_upper(C)           ((C) + 'A'-'a')

/*-----------------------------------------------------------------------------
** Function:	init_type()
** Purpose:	I roll my own types.
**		For efficiency I initialize an appropriate array.
** Arguments:
**	
** Returns:	nothing
**___________________________________________________			     */
void init_type()				   /*                        */
{ register int i;				   /*                        */
 						   /*                        */
  for ( i=0; i<256; ++i )			   /*                        */
  { trans_lower[i] = is_upper(i)?to_lower(i):i;	   /*                        */
    trans_upper[i] = is_lower(i)?to_upper(i):i;	   /*                        */
    trans_id[i] = i;				   /*                        */
  }						   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	case_cmp()
** Purpose:	Compare two strings ignoring cases.
** Arguments:
**	s
**	t
** Returns:	0 iff the strings differ
**___________________________________________________			     */
int case_cmp(s,t)				   /*                        */
  register char * s;				   /*                        */
  register char * t;				   /*                        */
{						   /*                        */
  while ( *s )					   /*                        */
  { 						   /*                        */
    if ( ToLower(*(s++)) != ToLower(*(t++)) )	   /*                        */
      return 0;				   	   /*                        */
  }						   /*                        */
  return(*t=='\0'?1:0);				   /*                        */
}						   /*------------------------*/
