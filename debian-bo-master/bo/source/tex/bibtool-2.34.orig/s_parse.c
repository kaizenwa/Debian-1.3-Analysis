/******************************************************************************
** $Id: s_parse.c,v 2.13 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "type.h"
#include "symbols.h"
#include "error.h"
#include "parse.h"
#include "s_parse.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 char * s_parse _ARG((int type,char **sp,int errp));/* s_parse.c             */
 int sp_open _ARG((char * s));			   /* s_parse.c              */
 void sp_close _ARG((void));			   /* s_parse.c              */
 void sp_error _ARG((char *s,char *a,char *b));	   /* s_parse.c              */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

#define Error(E,S,A,B)	\
  if(E) error(ERR_ERROR|ERR_POINT,A,B,(char*)0,sp_line,(uchar*)S,0,(char*)0)

 static uchar *sp_line = NULL;

/*-----------------------------------------------------------------------------
** Function:	sp_open()
** Purpose:	Open a string for parsing.
** Arguments:
**	s
** Returns:	TRUE
**___________________________________________________			     */
int sp_open(s)					   /*                        */
  register char * s;				   /*                        */
{ sp_line = (uchar*)s;				   /*                        */
  return TRUE;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	s_parse()
** Purpose:	Parse a string.
** Arguments:
**	type	is the type of construct to parse. it is defined in s_parse.h
**	sp	is a pointer to the string which is parsed. The value is
**		changed to hold the remaining characters at the end.
**	errp	this boolean indicated whether or not a verbose error
**		message should be created in case of an error.
** Returns:	
**___________________________________________________			     */
char * s_parse(type,sp,errp)			   /*                        */
  int 		type;				   /*                        */
  char		**sp;				   /*                        */
  int		errp;				   /*                        */
{ register char *s = *sp;			   /*                        */
  char          c,				   /*                        */
  		*cp;				   /*                        */
  static char   *unexpected = "Unexpected ";	   /*                        */
  static char   *expected   = " expected.";	   /*                        */
 						   /*                        */
  while( is_space(*s) ) s++;			   /*                        */
  *sp = s;					   /*                        */
 						   /*                        */
  switch ( type )				   /*                        */
  { case StringParseSymbol:			   /*                        */
      if ( is_allowed(*s) )			   /*                        */
      { do { s++; } while ( is_allowed(*s) );	   /*                        */
      }						   /*                        */
      else					   /*                        */
      { Error(errp,s,"Symbol",expected);	   /*                        */
	return NULL;				   /*                        */
      }						   /*                        */
      break;					   /*                        */
 						   /*                        */
    case StringParseNumber:			   /*                        */
      if ( is_digit(*s) )			   /*                        */
      { do { s++; } while ( is_digit(*s) );	   /*                        */
      }						   /*                        */
      else					   /*                        */
      { Error(errp,s,"Number",expected);	   /*                        */
	return NULL;				   /*                        */
      }						   /*                        */
      break;					   /*                        */
 						   /*                        */
    case StringParseBraces:			   /*                        */
    case StringParseUnquotedBraces:		   /*                        */
      if ( *s != '{' )				   /*                        */
      { Error(errp,s,"Brace",expected);	   	   /*                        */
	return NULL;				   /*                        */
      }						   /*                        */
      s++;					   /*                        */
      if ( type == StringParseUnquotedBraces )	   /*                        */
      { *sp = s; }				   /*                        */
      { int level = 1;				   /*                        */
      						   /*                        */
	while ( *s && level > 0 )		   /*                        */
	{ switch ( *s )				   /*                        */
	  { case '{': level++; break;		   /*                        */
	    case '}': --level; break;		   /*                        */
	  }					   /*                        */
	  s++;					   /*                        */
	}					   /*                        */
	if ( level > 0 )			   /*                        */
	{ Error(errp,s,unexpected,"end of braces.");/*                       */
	}					   /*                        */
	else if ( type==StringParseUnquotedBraces )/*                        */
	{ s--; }				   /*                        */
      }						   /*                        */
      break;					   /*                        */
 						   /*                        */
    case StringParseString:			   /*                        */
    case StringParseUnquotedString:		   /*                        */
      if ( *s != '"' )				   /*                        */
      { Error(errp,s,"String",expected);	   /*                        */
	return NULL;				   /*                        */
      }						   /*                        */
      s++;					   /*                        */
      if ( type == StringParseUnquotedString )	   /*                        */
      { *sp = s; }				   /*                        */
      						   /*                        */
      while ( *s && *s != '"' )			   /*                        */
      { if ( *s == '\\' && *(s+1) != '\0' ) s += 2;/*                        */
      else ++s;					   /*                        */
      }						   /*                        */
  						   /*                        */
      if ( *s == '"' ) 				   /*                        */
      { if ( type != StringParseUnquotedString ) s++;/*                      */
      }			   			   /*                        */
      else					   /*                        */
      { Error(errp,s,unexpected,"end of string."); /*                        */
      }						   /*                        */
      break;					   /*                        */
 						   /*                        */
    case StringParseSkip:			   /*                        */
      while(   is_space(*s) 			   /*                        */
	    || *s == '='			   /*                        */
	    || *s == '#'    ) s++;		   /*                        */
      *sp = s;					   /*                        */
      return s;					   /*                        */
 						   /*                        */
    case StringParseValue:			   /*                        */
      switch ( *s )				   /*                        */
      { case '"':				   /*                        */
	  return s_parse(StringParseUnquotedString,sp,errp);/*               */
	case '{':				   /*                        */
	  return s_parse(StringParseUnquotedBraces,sp,errp);/*               */
	case '0': case '1': case '2': case '3':    /*                        */
	case '4': case '5': case '6': case '7':	   /*                        */
	case '8': case '9':			   /*			     */
	  return s_parse(StringParseNumber,sp,errp);/*                       */
	default:				   /*                        */
	  return s_parse(StringParseSymbol,sp,errp);/*                       */
      }						   /*                        */
      break;					   /*                        */
 						   /*                        */
    case StringParseEOS:			   /*                        */
      if ( *s )					   /*                        */
      { Error(errp,s,unexpected,"characters at end of string.");/*           */
      }						   /*                        */
      						   /*                        */
      sp_line = NULL;				   /*                        */
      return (*s?s:NULL);			   /*                        */
 						   /*                        */
    default:					   /*                        */
      return NULL;				   /*                        */
  }						   /*                        */
 						   /*                        */
  c   = *s;					   /*                        */
  *s  = '\0';					   /*                        */
  cp  = new_string(*sp);			   /*                        */
  *s  = c;					   /*                        */
  if (   type == StringParseUnquotedString	   /*                        */
      || type == StringParseUnquotedBraces ) s++;  /*                        */
  *sp = s;					   /*                        */
  if (   type == StringParseSymbol )		   /*                        */
  { normalize_symbol(cp); }			   /*                        */
  s   = symbol(cp);				   /*                        */
  free(cp);					   /*                        */
  return s;					   /*                        */
}						   /*------------------------*/
