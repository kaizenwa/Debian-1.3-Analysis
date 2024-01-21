/******************************************************************************
** $Id: sbuffer.c,v 2.15 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "config.h"

#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#else
 extern VoidPTR malloc();
 extern VoidPTR realloc();

#define NULL 0L
#endif
/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#include "sbuffer.h"

/*-----------------------------------------------------------------------------
** Function:	sbopen()
** Purpose:	Allocate a new string buffer.
**		Return a pointer to the new string buffer or NULL if none was
**		available.
** Arguments:	none.
** Returns:	pointer to new string buffer or NULL
**___________________________________________________			     */
StringBuffer* sbopen()				   /*                        */
{ register StringBuffer* sb;			   /*                        */
 						   /*                        */
  if ( (sb=(StringBuffer*)malloc(sizeof(StringBuffer))) == NULL )/*          */
  { return NULL; }				   /*                        */
  sb->sb__string = NULL;			   /*                        */
  sb->sb__ptr = sb->sb__size = 0;		   /*                        */
  return sb;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbclose()
** Purpose:	Free an old string buffer.
** Arguments:
**	sb	Pointer to string buffer which should be sclosed
** Returns:	Return 0 upon failure.
**___________________________________________________			     */
int sbclose(sb)					   /*                        */
  StringBuffer* sb;				   /*                        */
{						   /*                        */
  if ( sb == NULL ) return 1;			   /*                        */
  if ( sb->sb__string != NULL ) free(sb->sb__string);/*                      */
  free((char*)sb);				   /*                        */
  return 0;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbputs()
** Purpose:	Push a whole string onto a string buffer.
** Arguments:
**	s
**	sb
** Returns:	
**___________________________________________________			     */
int sbputs(s,sb)				   /*                        */
  char         *s;				   /*                        */
  StringBuffer* sb;				   /*                        */
{						   /*                        */
  if ( sb == NULL ) return 1;			   /*                        */
 						   /*                        */
  while ( *s )					   /*                        */
  { (void)sbputchar(*s,sb); ++s;		   /*                        */
  }						   /*                        */
  return 0;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbputc()
** Purpose:	Push a single character onto a string buffer.
** Arguments:
**	c
**	sb
** Returns:	
**___________________________________________________			     */
int sbputc(c,sb)				   /*                        */
  register int           c;			   /*                        */
  register StringBuffer* sb;			   /*                        */
{ register char         *cp;			   /*                        */
 						   /*                        */
  if ( sb->sb__ptr >= sb->sb__size )		   /*                        */
  { sb->sb__size += StringBufferIncrement;	   /*                        */
    if ( (cp=( sb->sb__ptr == 0		   	   /*                        */
	      ? malloc(sb->sb__size)		   /*                        */
	      : realloc(sb->sb__string,sb->sb__size))/*                      */
	 ) == NULL )				   /*                        */
    { sb->sb__size -= StringBufferIncrement;	   /*                        */
      return 1;					   /*                        */
    }						   /*                        */
    sb->sb__string = cp;			   /*                        */
  }						   /*                        */
 						   /*                        */
  sb->sb__string[sb->sb__ptr++] = c;		   /*                        */
  return 0;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbflush()
** Purpose:	Close a string buffer with a trailing '\0' and reset the
**		current pointer to the beginning.
**		The next write operation starts.
** Arguments:
**	sb
** Returns:	
**___________________________________________________			     */
char* sbflush(sb)				   /*                        */
  StringBuffer* sb;				   /*                        */
{ 						   /*                        */
  (void)sbputchar('\0',sb);			   /*                        */
  return sb->sb__string;			   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbtell()
** Purpose:	Return the current pointer to the string buffer position.
**		This can be used with sbseek() to reset it.
** Arguments:
**	sb
** Returns:	
**___________________________________________________			     */
int sbtell(sb)					   /*                        */
  StringBuffer* sb;				   /*                        */
{ return sb->sb__ptr;				   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbrewind()
** Purpose:	Reset the string buffer pointer to the beginning.
**		The next write or read will operate there.
** Arguments:
**	sb
** Returns:	nothing
**___________________________________________________			     */
void sbrewind(sb)				   /*                        */
  StringBuffer* sb;				   /*                        */
{ sb->sb__ptr = 0;				   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sbseek()
** Purpose:	Reset the current pointer to the position given.
** Arguments:
**	sb
**	pos
** Returns:	
**___________________________________________________			     */
int sbseek(sb,pos)				   /*                        */
  StringBuffer* sb;				   /*                        */
  int           pos;				   /*                        */
{ 						   /*                        */
  if ( pos < 0 || pos > sb->sb__ptr ) return 1;	   /*                        */
  sb->sb__ptr = pos;				   /*                        */
  return 0;					   /*                        */
}						   /*------------------------*/
