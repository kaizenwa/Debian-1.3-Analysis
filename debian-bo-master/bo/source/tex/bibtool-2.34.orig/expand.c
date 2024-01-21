/******************************************************************************
** $Id: expand.c,v 1.10 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "expand.h"
#include "symbols.h"
#include "macros.h"
#include "type.h"

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 char * expand_rhs _ARG((char *s,int braces));	   /* expand.c               */
 static int expand _ARG((char *s,StringBuffer *sb,int brace,int first,char *q_open,char *q_close));/* expand.c*/
 static void expand__ _ARG((char *s,StringBuffer *sb,char *q_open,char *q_close));/* expand.c*/

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
** Function:	expand_rhs()
** Purpose:	Expand the right hand side of an item.
** Arguments:
**	s	string to expand
**	braces	boolean. if TRUE braces {} are used for the result.
**		Otherwise double quotes "".
** Returns:	
**___________________________________________________			     */
char * expand_rhs(s,braces)			   /*                        */
  char *s;					   /*                        */
  int  braces;					   /*                        */
{ static StringBuffer *sb = NULL;		   /*                        */
 						   /*                        */
  if ( sb == NULL && (sb = sbopen()) == NULL )	   /*                        */
  { OUT_OF_MEMORY("string expansion");}		   /*                        */
 						   /*                        */
  DebugPrint2("Expanding ",s);			   /*                        */
 						   /*                        */
  sbrewind(sb);					   /*                        */
  expand__(s,					   /*                        */
	   sb,					   /*                        */
	   (braces?"{":"\""),			   /*                        */
	   (braces?"}":"\"") );			   /*                        */
  return sbflush(sb);				   /*                        */
}						   /*------------------------*/

#define PUTS(S,SB) (void)sbputs(S,SB)
#define PUTC(C,SB) (void)sbputchar(C,SB)

/*-----------------------------------------------------------------------------
** Function:	expand__()
** Purpose:	Wrapper for expand().
**		Things have to be cleaned up afterwards.
** Arguments:
**	s
**	sb
**	q_open
**	q_close
** Returns:	nothing
**___________________________________________________			     */
static void expand__(s,sb,q_open,q_close)	   /*                        */
  register char *s;				   /*                        */
  StringBuffer  *sb;				   /*                        */
  char          *q_open;			   /*                        */
  char          *q_close;			   /*                        */
{						   /*                        */
  if ( ! expand(s,sb,TRUE,TRUE,q_open,q_close) )   /*                        */
  { PUTS(q_close,sb); }				   /*                        */
  else if ( sbtell(sb) == 0 )			   /*                        */
  { PUTS(q_open,sb);			   	   /*                        */
    PUTS(q_close,sb);			   	   /*                        */
  }						   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	expand()
** Purpose:	
**		
**
** Arguments:
**	s	String to apply expansion to.
**	sb	String buffer which will receive the output.
**	brace	Boolean. Determine if braces should be used.
**		The strings of the braces are in q_open and q_close.
**	first	In multi parts determine if this is the first part.
**	q_open	Open delimiter. This is an open brace or a double quote.
**	q_close	Close delimiter. This is a close brace or a double quote.
** Returns:	
**___________________________________________________			     */
static int expand(s,sb,brace,first,q_open,q_close) /*                        */
  register char *s;				   /* specification          */
  StringBuffer  *sb;				   /* output device          */
  int           brace;				   /* is a brace needed?     */
  int           first;				   /* is this the first part?*/
  char          *q_open;			   /* open delimiter         */
  char          *q_close;			   /* close delimiter        */
{						   /*                        */
  while ( *s )					   /*                        */
  { 						   /*                        */
    switch ( *s )				   /*                        */
    { case '\0': break;				   /* Just in case.          */
      case '"':					   /* Start a string part.   */
        DebugPrint2("Start String:",s);		   /*                        */
        if ( *++s == '"' ) { ++s; break; }	   /* Ignore the empty string*/
        if ( brace ) 				   /*                        */
	{ if ( !first ) { PUTS(" # ",sb); }	   /*                        */
	  PUTS(q_open,sb); brace = FALSE;  	   /*                        */
        }					   /*                        */
        first = FALSE;				   /*                        */
 						   /*                        */
        for ( ; *s && *s!='"'; ++s )		   /* Until the end is found */
	{ PUTC(*s,sb);		   		   /*  transfer character.   */
	  if ( *s == '\\' && *(s+1) ) 		   /*  \ is for quoting.     */
	  { ++s; PUTC(*s,sb); }	   		   /*                        */
	}					   /*                        */
        if ( *s ) ++s;				   /* Advance if not end     */
        break;					   /*                        */
      case '{': 				   /* Start a block.         */
        DebugPrint2("Start block:",s);		   /*                        */
        if ( *++s == '}' ) { ++s; break; }	   /* Ignore empty block.    */
        if ( brace ) 				   /*                        */
	{ if ( !first ) { PUTS(" # ",sb); }	   /*                        */
	  PUTS(q_open,sb); brace = FALSE;  	   /*                        */
        }					   /*                        */
        first = FALSE;				   /*                        */
 						   /*                        */
        { register int level = 1;		   /* Initialize brace count */
	  for ( ; *s && level>0; ++s )		   /* Until level 0 or end   */
	  { switch ( *s )			   /*                        */
	    { case '\\':			   /* \ is for quoting.      */
		PUTC(*s,sb);		   	   /*                        */
		if ( *++s ) { PUTC(*s,sb); }	   /*                        */
		break;				   /*                        */
	      case '{':				   /*                        */
		++level;			   /*                        */
		PUTC(*s,sb);			   /*                        */
		break;		   		   /*                        */
	      case '}': 			   /*                        */
		if ( --level > 0 ) PUTC(*s,sb);	   /*                        */
		break;		   		   /*                        */
	      default:				   /*                        */
		PUTC(*s,sb);			   /*                        */
	    }					   /*                        */
	  }					   /*                        */
	}					   /*                        */
        break;					   /*                        */
      case '#': ++s;  break;			   /* Silently assume #.     */
      case '0': case '1': case '2': case '3': case '4':/* Start a number     */
      case '5': case '6': case '7': case '8': case '9':/*                    */
        DebugPrint2("Start number:",s);		   /*                        */
        					   /*                        */
        if ( brace ) 				   /*                        */
	{ if ( !first ) { PUTS(" # ",sb); }	   /*                        */
	  PUTS(q_open,sb); brace = FALSE;  	   /*                        */
        }					   /*                        */
        first = FALSE;				   /*                        */
 						   /*                        */
        do					   /*                        */
	{ PUTC(*s,sb);		   		   /*                        */
	  ++s;					   /*                        */
	} while ( is_digit(*s) );		   /*                        */
        break;        				   /*                        */
      default:					   /*                        */
        if ( is_space(*s) ) ++s;		   /* Ignore spaces.         */
        else 					   /* Only macros are left.  */
	{ register char *sym = s;		   /*                        */
	  char          *val;			   /*                        */
	  char          c;			   /*                        */
 						   /*                        */
          DebugPrint2("Start symbol:",s);	   /*                        */
	  while ( is_allowed(*s) ) ++s;		   /*                        */
	  c = *s; *s = '\0';			   /*                        */
	  { register char *sym_copy;		   /* Local copy of sym      */
	    if ( (sym_copy = (char*)malloc(strlen(sym)+1))/* Allocate memory */
		== NULL )			   /* Upon failure           */
	    { OUT_OF_MEMORY("expand string"); }	   /*  exit.                 */
	    (void)strcpy(sym_copy,sym);		   /* Save sym               */
	    *s = c;				   /* Restore end mark       */
	    sym = symbol(sym_copy);		   /* Intern the symbol.     */
	    free(sym_copy);			   /* Free the temp. memory  */
	  }					   /*                        */
	  val = look_macro(sym,0);		   /*                        */
	  if ( val != sym )			   /*                        */
	  { brace = expand(val,sb,brace,first,q_open,q_close); }/*           */
	  else					   /*                        */
	  { if ( !brace ) PUTS(q_close,sb);	   /*                        */
	    if ( !first ) PUTS(" # ",sb);  	   /*                        */
	    PUTS(sym,sb);		   	   /*                        */
	    brace = TRUE;			   /*                        */
	    first = FALSE;			   /*                        */
	  }					   /*                        */
	}	   				   /*                        */
    }    					   /*                        */
  }						   /*                        */
  return brace;					   /*                        */
}						   /*------------------------*/
