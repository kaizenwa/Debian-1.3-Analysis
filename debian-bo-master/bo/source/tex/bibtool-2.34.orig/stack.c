/******************************************************************************
** $Id: stack.c,v 2.14 1996/02/22 21:43:51 gerd Exp gerd $
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

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 char * pop_string _ARG((void));		   /* stack.c                */
 void push_string _ARG((char * s));		   /* stack.c                */

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

 static char **stack;
 static size_t stack_size = 0;
 static size_t stack_ptr  = 0;

/*-----------------------------------------------------------------------------
** Function:	push_string()
** Purpose:	Push a string onto the stack.
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void push_string(s)				   /*                        */
  register char * s;				   /*                        */
{ if ( stack_ptr >= stack_size )		   /*                        */
  { stack_size += 16;		   		   /*                        */
    stack = (stack_ptr == 0			   /*                        */
	     ?(char**)malloc((size_t)(stack_size*sizeof(char*)))/*           */
	     :(char**)realloc((char*)stack,	   /*                        */
			      (size_t)(stack_size*sizeof(char*))));/*        */
    if ( stack == NULL )			   /*                        */
    { OUT_OF_MEMORY("stack"); }	   		   /*                        */
  }		   				   /*                        */
  stack[stack_ptr++] = s;   			   /*                        */
  DebugPrint2("pushing ",s);		   	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	pop_string()
** Purpose:	Pop a string from the stack.
**		No error checking is performed.
** Arguments:	none
** Returns:	
**___________________________________________________			     */
char * pop_string()				   /*                        */
{ if ( stack_ptr <= 0 ) return(NULL);		   /*                        */
 						   /*                        */
  DebugPrint2("poping ",stack[stack_ptr-1]);	   /*                        */
  return(stack[--stack_ptr]);			   /*                        */
}						   /*------------------------*/
