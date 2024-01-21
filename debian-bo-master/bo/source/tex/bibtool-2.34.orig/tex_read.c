/******************************************************************************
** $Id: tex_read.c,v 2.17 1996/02/22 21:43:51 gerd Exp gerd $
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
** 
** Purpose: Simulate the TeX reading mechanism.
**
** Compilation: cc tex_read.c -o tex_read -DSTANDALONE
**	    
******************************************************************************/

#include "config.h"
#include <stdio.h>
#include <ctype.h>

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#else
 extern VoidPTR malloc();
#endif


#define CARET_CARET	    /* enable ^^A type specification of unprintables */
#undef	TRANSLATE_NEWLINE   /* Translate nl to space or \par */

 typedef struct tOKEN				   /*			     */
 { short int	to_char;			   /*			     */
   char		*to_s;				   /*			     */
   struct tOKEN *to_next;			   /*			     */
 } *Token, SToken;				   /*			     */

#define TokenChar(X) ((X)->to_char)
#define TokenSeq(X)  ((X)->to_s)
#define NextToken(X) ((X)->to_next)

#define TokenNULL (Token)0

 typedef struct mACdEF				   /*			     */
 { short int	 md_arity;			   /*			     */
   char		 *md_name;			   /*			     */
   Token	 md_token;			   /*			     */
   struct mACdEF *md_next;			   /*			     */
 } *MacDef, SMacDef;				   /*			     */

#define MacroArity(X)  ((X)->md_arity)
#define MacroName(X)   ((X)->md_name)
#define MacroToken(X)  ((X)->md_token)
#define NextMacro(X)   ((X)->md_next)
#define MacDefNULL     (MacDef)0

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int TeX_read _ARG((char * cp,char **sp));	   /* tex_read.c             */
 static MacDef find_macro _ARG((char *name,MacDef md));/* tex_read.c         */
 static MacDef new_macdef _ARG((char *name,int arity,Token tokens));/* tex_read.c*/
 static Token TeX_get_token _ARG((int (*get_fct)_ARG((void))));/* tex_read.c */
 static Token new_token _ARG((int type,char *string)); /* tex_read.c         */
 static Token token_list_copy _ARG((Token t,Token nt,Token *argp));/* tex_read.c*/
 static Token tokenize _ARG((char *s,int arity));  /* tex_read.c             */
 static char * tokens_to_string _ARG((Token t));   /* tex_read.c             */
 static int TeX_fill_line _ARG((int (*get_fct)_ARG((void))));/* tex_read.c   */
 static int do_get _ARG((void));		   /* tex_read.c             */
 static int fill_token _ARG((Token*tp));	   /* tex_read.c             */
 static int get_EOF _ARG((void));		   /* tex_read.c             */
 static int get_file _ARG((void));		   /* tex_read.c             */
 static int get_string _ARG((void));		   /* tex_read.c             */
 static void free_1_token _ARG((Token t));	   /* tex_read.c             */
 static void free_tokens _ARG((Token t));	   /* tex_read.c             */
 static void init_TeX _ARG((void));		   /* tex_read.c             */
 static void init_get _ARG((char * s));		   /* tex_read.c             */
 void TeX_active _ARG((int c,int arity,char * s)); /* tex_read.c             */
 void TeX_close _ARG((void));			   /* tex_read.c             */
 void TeX_def _ARG((char *s));			   /* tex_read.c             */
 void TeX_define _ARG((char *name,int arity,char *body));/* tex_read.c       */
 void TeX_open_file _ARG((FILE * file));	   /* tex_read.c             */
 void TeX_open_string _ARG((char * s));		   /* tex_read.c             */

#ifdef STANDALONE
 int main _ARG((int argc,char *argv[]));	   /* tex_read.c             */
 static char * new_string _ARG((char * s));	   /* tex-read.c	     */
 static void show_tokens _ARG((Token t));	   /* tex-read.c	     */
#else
 extern char * new_string _ARG((char * s));
#endif

/*---------------------------------------------------------------------------*/

#define TRUE  (0)
#define FALSE (-1)

#ifdef STANDALONE
#define Err(X) (void)fputs(X,stderr)
#define ERROR_EXIT(X) Err(X);exit(1)
#define OUT_OF_MEMORY(X) Err("Out of memory for ");Err(X);exit(1)
#else
#include "error.h"
#endif

/*****************************************************************************/
/*** catcode Management.						   ***/
/*****************************************************************************/

#define CHAR_ESCAPE		'\\'
#define CHAR_BEG_GROUP		'{'
#define CHAR_END_GROUP		'}'
#define CHAR_MATH		'$'
#define CHAR_ALIGN		'&'
#define CHAR_EOL		'\n'
#define CHAR_PARAMETER		'#'
#define CHAR_SUPERSCRIPT	'^'
#define CHAR_SUBSCRIPT		'_'
#define CHAR_IGNORED		'\0'
#define CHAR_SPACE		' '
#define CHAR_COMMENT		'%'

#define CATCODE_ESCAPE		0x0000
#define CATCODE_BEG_GROUP	0x0100
#define CATCODE_END_GROUP	0x0200
#define CATCODE_MATH		0x0300
#define CATCODE_ALIGN		0x0400
#define CATCODE_EOL		0x0500
#define CATCODE_PARAMETER	0x0600
#define CATCODE_SUPERSCRIPT	0x0700
#define CATCODE_SUBSCRIPT	0x0800
#define CATCODE_IGNORED		0x0900
#define CATCODE_SPACE		0x0A00
#define CATCODE_LETTER		0x0B00
#define CATCODE_OTHER		0x0C00
#define CATCODE_ACTIVE		0x0D00
#define CATCODE_COMMENT		0x0E00
#define CATCODE_INVALID		0x0F00

#define CATCODE_MASK 0x0F00

 static short int catcode[256];

 static MacDef macro	   = MacDefNULL;
 static MacDef active[256];

#define EnsureInit init_TeX()

/*-----------------------------------------------------------------------------
** Function:	init_TeX()
** Purpose:	Initialize the TeX reading apparatus.
**		Mainly the catcodes are assigned.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
static void init_TeX()				   /*			     */
{ register int i;				   /*			     */
  static int   first = TRUE;			   /*			     */
						   /*			     */
  if ( !first ) return;				   /*			     */
  first = FALSE;				   /*			     */
						   /*			     */
  for ( i=0; i<256; ++i )			   /*			     */
  { active[i]  = MacDefNULL;			   /*			     */
    catcode[i] = (  isalpha(i)			   /*			     */
		  ? CATCODE_LETTER		   /*			     */
		  : CATCODE_OTHER  );		   /*			     */
  }						   /*			     */
  catcode['\\'] = CATCODE_ESCAPE;		   /*			     */
  catcode['{']	= CATCODE_BEG_GROUP;		   /*			     */
  catcode['}']	= CATCODE_END_GROUP;		   /*			     */
  catcode['$']	= CATCODE_MATH;			   /*			     */
  catcode['&']	= CATCODE_ALIGN;		   /*			     */
  catcode['\n'] = CATCODE_EOL;			   /*			     */
  catcode['#']	= CATCODE_PARAMETER;		   /*			     */
  catcode['^']	= CATCODE_SUPERSCRIPT;		   /*			     */
  catcode['_']	= CATCODE_SUBSCRIPT;		   /*			     */
  catcode['\0'] = CATCODE_IGNORED;		   /*			     */
  catcode[' ']	= CATCODE_SPACE;		   /*			     */
  catcode['~']	= CATCODE_ACTIVE;		   /*			     */
  catcode['%']	= CATCODE_COMMENT;		   /*			     */
}						   /*------------------------*/
 
/*****************************************************************************/
/*** Token Management.							   ***/
/*****************************************************************************/

 static Token token_free_list = TokenNULL;

/*-----------------------------------------------------------------------------
** Function:	new_token()
** Purpose:	Allocate a new token cell and fill it with values.
** Arguments:
**	type
**	string
** Returns:	
**___________________________________________________			     */
static Token new_token(type,string)		   /*			     */
  int	   type;				   /*			     */
  char	   *string;				   /*			     */
{ Token	   new;					   /*			     */
						   /*			     */
  if ( token_free_list != TokenNULL )		   /*			     */
  { new = token_free_list;			   /*			     */
    token_free_list = NextToken(token_free_list);  /*			     */
  }						   /*			     */
  else if ( (new=(Token)malloc(sizeof(SToken))) == TokenNULL )/*	     */
  { OUT_OF_MEMORY("TeX token."); }  		   /*			     */
 						   /*                        */
  TokenChar(new) = type;			   /*			     */
  TokenSeq(new)	 = string;			   /*			     */
  NextToken(new) = TokenNULL;			   /*			     */
  return new;					   /*			     */
}						   /*------------------------*/

#define CopyToken(t) \
  new_token(TokenChar(t),TokenSeq(t)?new_string(TokenSeq(t)):NULL)

#define NewToken(C) new_token(C,(char*)0)

/*-----------------------------------------------------------------------------
** Function:	token_list_copy()
** Purpose:	Copy a list of tokens.
** Arguments:
**	t
**	nt
**	argp
** Returns:	
**___________________________________________________			     */
static Token token_list_copy(t,nt,argp)		   /*			     */
  register Token t;				   /*			     */
  Token		 nt;				   /*			     */
  Token		 *argp;				   /*			     */
{ register Token a,p;				   /*			     */
  int		 i;				   /*			     */
						   /*			     */
  if ( t == TokenNULL ) return TokenNULL;	   /*			     */
  if ( (i=TokenChar(t)) > 0xff )		   /*			     */
  { a = p = token_list_copy(argp[i>>8],TokenNULL,argp);/*		    */
    while ( NextToken(p) != TokenNULL )		   /*			     */
    { p = NextToken(p); }			   /*			     */
  }						   /*			     */
  else { a = p = CopyToken(t);	}		   /*			     */
						   /*			     */
  while ( (t=NextToken(t)) != TokenNULL )	   /*			     */
  {						   /*			     */
    if ( (i=TokenChar(t)) > 0xff )		   /*			     */
    { NextToken(p) = token_list_copy(argp[i>>8],TokenNULL,argp);/*	    */
      while ( NextToken(p) != TokenNULL )	   /*			     */
      { p = NextToken(p); }			   /*			     */
    }						   /*			     */
    else					   /*			     */
    { NextToken(p) = CopyToken(t);		   /*			     */
      p = NextToken(p);				   /*			     */
    }						   /*			     */
  }						   /*			     */
  NextToken(p) = nt;				   /*			     */
  return a;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	free_1_token()
** Purpose:	Free a single Token.
**		Free the string and place it in the free list.
** Arguments:
**	t
** Returns:	nothing
**___________________________________________________			     */
static void free_1_token(t)			   /*			     */
  register Token t;				   /*			     */
{ NextToken(t) = token_free_list;		   /*			     */
  token_free_list = t;				   /*			     */
  if ( TokenSeq(t) ) free(TokenSeq(t));		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	free_tokens()
** Purpose:	Free a list of Tokens.
**		Walk along to the end and free the strings on the way.
**		Link it into the free list.
** Arguments:
**	t
** Returns:	nothing
**___________________________________________________			     */
static void free_tokens(t)			   /*			     */
  register Token t;				   /*			     */
{ register Token t0;				   /*			     */
						   /*			     */
  for ( t0 = t;					   /*			     */
	NextToken(t0) != TokenNULL;		   /*			     */
	t0 = NextToken(t0) )			   /*			     */
  { if ( TokenSeq(t0) ) free(TokenSeq(t0)); }	   /*			     */
						   /*			     */
  if ( TokenSeq(t0) ) free(TokenSeq(t0));	   /*			     */
  NextToken(t0) = token_free_list;		   /*			     */
  token_free_list = t;				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	tokens_to_string()
** Purpose:	Make a string out of a list of tokens.
** Arguments:
**	t
** Returns:	
**___________________________________________________			     */
static char * tokens_to_string(t)		   /*			     */
  Token		 t;				   /*			     */
{ register Token t1;				   /*			     */
  register char	 *s,				   /*			     */
		 *sp;				   /*			     */
  register int	 len;				   /*			     */
						   /*			     */
  for ( len=1,t1=t;				   /* Count the elements of  */
	t1!=TokenNULL;				   /*  the token list.	     */
	t1=NextToken(t1) )			   /*  (+1 for '\0')	     */
  { ++len; }					   /*			     */
						   /*			     */
  if ( (s=malloc(len*sizeof(char))) == NULL )	   /* Try to get             */
  { OUT_OF_MEMORY("TeX string."); } 		   /*     new memory.	     */
						   /*			     */
  for ( sp=s,t1=t;				   /* Transfer the characters*/
	t1!=TokenNULL;				   /*  from the token list   */
	t1=NextToken(t1) )			   /*  to the string.	     */
  { *(sp++) = TokenChar(t1); }			   /*			     */
  *sp = '\0';					   /*			     */
  return s;					   /* Return the string.     */
}						   /*------------------------*/

/*****************************************************************************/
/*** Reading Apparatus.							   ***/
/*****************************************************************************/

#define StateN 0
#define StateM 1
#define StateS 2

 static int   tex_state = StateN;		   /*			     */
 static Token tex_line	= TokenNULL;		   /*			     */

#define tex_clear tex_state = StateN; tex_line = TokenNULL

/*-----------------------------------------------------------------------------
** Function:	TeX_fill_line()
** Purpose:	
**		
**
** Arguments:
**	get_fct
** Returns:	
**___________________________________________________			     */
static int TeX_fill_line(get_fct)		   /*			     */
  int		(*get_fct)_ARG((void));		   /* function * to get char */
{ register int	 c;				   /*			     */
  register Token t;				   /*			     */
  register int	 spaces = 0;			   /*			     */
						   /*			     */
  if ( (c=(*get_fct)()) == EOF ) return 1;	   /*			     */
  tex_line = t = NewToken(c);			   /*			     */
						   /*			     */
  while ( (c=(*get_fct)()) != EOF && c != '\n' )   /*			     */
  { if ( isspace(c) ) { ++spaces; }		   /* Spaces are delayed.    */
    else					   /*			     */
    { while ( spaces > 0 )			   /*			     */
      { NextToken(t) = NewToken(CHAR_SPACE);	   /*			     */
	t = NextToken(t);			   /*			     */
	spaces--;				   /*			     */
      }						   /*			     */
      NextToken(t) = NewToken(c);		   /*			     */
      t = NextToken(t);				   /*			     */
    }						   /*			     */
  }						   /*			     */
  if ( TokenChar(t) == CHAR_SPACE ) TokenChar(t) = CHAR_EOL;/*		     */
  else NextToken(t) = NewToken(CHAR_EOL);	   /*			     */
						   /*			     */
#ifdef CARET_CARET
  { register Token *tp;				   /*			     */
    for ( tp= &tex_line;			   /* Loop over the read     */
	 *tp!=TokenNULL;			   /*  line.		     */
	 tp= &NextToken(*tp) )			   /*			     */
    { t = NextToken(*tp);			   /*			     */
      if ( TokenChar(*tp) == '^'		   /* Translate ^^.	     */
	  && TokenChar(t=NextToken(*tp)) == '^'	   /*  to single characters  */
	  && (c=TokenChar(NextToken(t))) != '\n' ) /*			     */
      { NextToken(*tp) = NextToken(NextToken(t));  /*			     */
	free_1_token(NextToken(t));		   /*			     */
	free_1_token(t);			   /*			     */
	TokenChar(*tp) = (c>=64?c-64:c+64);	   /*			     */
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
#endif
						   /*			     */
  return 0;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_get_token()
** Purpose:	Get characters and pack them into a Token structure.
**		The argument is a function called to get the next character.
**		It returns the next character of EOF if no more can be read.
** Arguments:
**	get_fct
** Returns:	
**___________________________________________________			     */
static Token TeX_get_token(get_fct)		   /*			     */
  int		 (*get_fct)_ARG((void));	   /* function * to get char */
{ register Token t, t1, t2;			   /*			     */
						   /*			     */
  for (;;)					   /*			     */
  {						   /*			     */
    if (   tex_line == TokenNULL		   /*			     */
	&& TeX_fill_line(get_fct) )		   /*			     */
    { return TokenNULL; }			   /*			     */
						   /*			     */
    t		 = tex_line;			   /*			     */
    tex_line	 = NextToken(t);		   /*			     */
    NextToken(t) = TokenNULL;			   /*			     */
						   /*			     */
    switch ( TokenChar(t) )			   /*			     */
    { case CHAR_IGNORED:			   /*			     */
	break;					   /*			     */
#ifdef TRANSLATE_NEWLINE
      case CHAR_EOL:				   /*			     */
	switch (tex_state)			   /*			     */
	{ case StateN:				   /*			     */
	    TokenChar(t) = CHAR_ESCAPE;		   /*			     */
	    TokenSeq(t)	 = new_string("par");	   /*			     */
	    return t;				   /*			     */
	  case StateM:				   /*			     */
	    TokenChar(t) = CHAR_SPACE;		   /*			     */
	    return t;				   /*			     */
	}					   /*			     */
#endif
      case CHAR_COMMENT:			   /*			     */
	free_tokens(tex_line);			   /*			     */
	tex_line = TokenNULL;			   /*			     */
	break;					   /*			     */
      case CHAR_SPACE:				   /*			     */
	if ( tex_state == StateM )		   /*			     */
	{ tex_state = StateS;			   /*			     */
	  return t;				   /*			     */
	}					   /*			     */
	break;					   /*			     */
      case CHAR_ESCAPE:				   /*			     */
	t1 = tex_line;				   /*			     */
	if ( isalpha(TokenChar(tex_line)) )	   /*			     */
	{ while ( isalpha(TokenChar(NextToken(tex_line))) )/*		     */
	  { tex_line = NextToken(tex_line); }	   /*			     */
	}					   /*			     */
	else if ( TokenChar(tex_line) == CHAR_EOL )/*			     */
	{ tex_state   = StateM;			   /*			     */
	  TokenSeq(t) = new_string("");		   /*			     */
	  return t;				   /*			     */
	}					   /*			     */
	t2	      = tex_line;		   /*			     */
	tex_line      = NextToken(tex_line);	   /*			     */
	NextToken(t2) = TokenNULL;		   /*			     */
	TokenSeq(t)   = tokens_to_string(t1);	   /*			     */
	free_tokens(t1);			   /*			     */
	tex_state     = StateM;			   /*			     */
	return t;				   /*			     */
						   /*			     */
      default:					   /*			     */
	tex_state = StateM;			   /*			     */
	return t;				   /*			     */
    }						   /*			     */
    free_1_token(t);				   /*			     */
  }						   /*			     */
}						   /*------------------------*/




 static char *g_s;
 static char *g_p;

/*-----------------------------------------------------------------------------
** Function:	init_get()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
static void init_get(s)				   /*			     */
  register char * s;				   /*			     */
{ g_p = g_s = s;				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	do_get()
** Purpose:	
**		
**
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
static int do_get()				   /*			     */
{ return ( *g_p == '\0' ? EOF : *(g_p++));	   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	tokenize()
** Purpose:	
**		
**
** Arguments:
**	s
**	arity
** Returns:	
**___________________________________________________			     */
static Token tokenize(s,arity)			   /*			     */
  char		*s;				   /*			     */
  int		arity;				   /*			     */
{ Token		t ,				   /*			     */
		t_ret = TokenNULL,		   /*			     */
		nt, t0;				   /*			     */
  int		a;				   /*			     */
						   /*			     */
  EnsureInit;					   /*			     */
  init_get(s);					   /*			     */
						   /*			     */
  while ( (nt=TeX_get_token(do_get)) != TokenNULL )/*			     */
  { if ( TokenChar(nt) =='\n' )			   /*			     */
    { free_1_token(nt); }			   /*			     */
    else					   /*			     */
    { if ( TokenChar(nt) == '#'			   /*			     */
	  && (t0=TeX_get_token(do_get)) != TokenNULL )/*		     */
      { a = TokenChar(t0) - '0';		   /*			     */
	if ( 0<a && a<=arity ) TokenChar(nt) |= a<<8;/*			     */
	else { Err("*** Argument error.\n"); }	   /*			     */
	free_1_token(t0);			   /*			     */
      }						   /*			     */
      if ( t_ret == TokenNULL ) { t_ret = t = nt; }/*			     */
      else { NextToken(t) = nt; t = nt; }	   /*			     */
    }						   /*			     */
  }						   /*			     */
  return t_ret;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	new_macdef()
** Purpose:	
**		
**
** Arguments:
**	name
**	arity
**	tokens
** Returns:	
**___________________________________________________			     */
static MacDef new_macdef(name,arity,tokens)	   /*			     */
  char	    *name;				   /*			     */
  int	    arity;				   /*			     */
  Token	    tokens;				   /*			     */
{ MacDef    new;				   /*			     */
						   /*			     */
  if ( (new=(MacDef)malloc(sizeof(SMacDef))) == NULL )/*		     */
  { OUT_OF_MEMORY("TeX macro."); }  		   /*			     */
						   /*			     */
  MacroName(new)  = name;			   /*			     */
  MacroArity(new) = arity;			   /*			     */
  MacroToken(new) = tokens;			   /*			     */
  NextMacro(new)  = MacDefNULL;			   /*			     */
  return new;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	find_macro()
** Purpose:	
**		
**
** Arguments:
**	name
**	md
** Returns:	
**___________________________________________________			     */
static MacDef find_macro(name,md)		   /*			     */
  register char	  *name;			   /*			     */
  register MacDef md;				   /*			     */
{						   /*			     */
  for ( ; md != MacDefNULL; md=NextMacro(md) )	   /*			     */
  { if ( strcmp(MacroName(md),name) == 0 )	   /*			     */
      return md;				   /*			     */
  }						   /*			     */
  return MacDefNULL;				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_define()
** Purpose:	
**		
**
** Arguments:
**	name
**	arity
**	body
** Returns:	nothing
**___________________________________________________			     */
void TeX_define(name,arity,body)		   /*			     */
  char		  *name;			   /*			     */
  int		  arity;			   /*			     */
  char		  *body;			   /*			     */
{ register MacDef md;				   /*			     */
						   /*			     */
  if ( 0 > arity || arity > 9 ) return;		   /*			     */
						   /*			     */
  md		= new_macdef(name,arity,tokenize(body,arity));/*	     */
  NextMacro(md) = macro;			   /*			     */
  macro		= md;				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_def()
** Purpose:	Define a macro.
**		The argument is a string specification of the following form:
**		    \name[arity]=replacement text
**		    \name=replacement text
**		0 <= arity <= 9
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void TeX_def(s)					   /*			     */
  register char *s;				   /*			     */
{ char		*name,				   /*			     */
		*ep   = NULL;			   /*			     */
  char		c;				   /*			     */
  int		arity = 0;			   /*			     */
						   /*			     */
  while( isspace(*s) ) s++;			   /*                        */
  name = s;					   /*			     */
  while ( *s && *s != '=' )			   /*			     */
  { if ( *s == '[' )				   /*			     */
    { arity = *(s+1) -'0'; ep = s; }		   /*			     */
    ++s;					   /*			     */
  }						   /*			     */
						   /*			     */
  if ( ep == NULL ) ep = s;			   /*			     */
						   /*			     */
  if ( *name == '\\' )				   /*                        */
  {  						   /*                        */
    c = *ep; *ep = '\0';			   /*			     */
    TeX_define(new_string(name+1),		   /*			     */
	       arity,				   /*			     */
	       new_string(s+1));		   /*			     */
    *ep = c;					   /*			     */
  }
  else
  { TeX_active(*name,arity,new_string(s+1));
  }
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_active()
** Purpose:	
**		
**
** Arguments:
**	c
**	arity
**	s
** Returns:	nothing
**___________________________________________________			     */
void TeX_active(c,arity,s)			   /*			     */
  int  c;					   /*			     */
  int  arity;					   /*			     */
  char * s;					   /*			     */
{						   /*			     */
  EnsureInit;					   /*			     */
  active[c] = new_macdef((char*)NULL,arity,tokenize(s,arity));/*	     */
}						   /*------------------------*/



/*-----------------------------------------------------------------------------
** Function:	get_EOF()
** Purpose:	
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
static int get_EOF()				   /*			     */
{ return EOF;					   /*			     */
}						   /*------------------------*/

 static FILE *src_file;				   /*			     */
 static char *src_string;			   /*			     */
 static char *src_ptr;				   /*			     */
 static int  (*src_get)() = get_EOF;		   /*			     */

/*-----------------------------------------------------------------------------
** Function:	get_string()
** Purpose:	
**		
**
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
static int get_string()				   /*			     */
{ return ( *src_ptr ? *(src_ptr++) : EOF );	   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	get_file()
** Purpose:	
**		
**
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
static int get_file()				   /*			     */
{ return getc(src_file);			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_open_file()
** Purpose:	Prepare things to parse from a file.
** Arguments:
**	file
** Returns:	nothing
**___________________________________________________			     */
void TeX_open_file(file)			   /*			     */
  FILE * file;					   /*			     */
{ src_file = file;				   /*			     */
  src_get  = get_file;				   /*			     */
  tex_clear;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_open_string()
** Purpose:	Prepare things to parse from a string.
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void TeX_open_string(s)				   /*			     */
  char * s;					   /*			     */
{ src_ptr = src_string = s;			   /*			     */
  src_get = get_string;				   /*			     */
  tex_clear;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	TeX_close()
** Purpose:	
** Arguments:
**	
** Returns:	nothing
**___________________________________________________			     */
void TeX_close()				   /*			     */
{ char c, *s;					   /*			     */
  src_get = get_EOF;				   /*			     */
  while ( TeX_read(&c,&s) ) ;			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fill_token()
** Purpose:	Check if *tp contains a token.
**		Otherwise try to get one and store it there.
** Arguments:
**	tp
** Returns:	
**___________________________________________________			     */
static int fill_token(tp)			   /*			     */
  register Token*tp;				   /*			     */
{ if (	 *tp == TokenNULL			   /*			     */
      && (*tp=TeX_get_token(src_get)) == TokenNULL )/*			     */
  { return TRUE; }				   /*			     */
  return FALSE;					   /*			     */
}						   /*------------------------*/

#define UnlinkToken(T,T2)		T2 = T; T = NextToken(T)
#define UnlinkAndFreeToken(T,T2)	UnlinkToken(T,T2); free_1_token(T2)

/*-----------------------------------------------------------------------------
** Function:	TeX_read()
** Purpose:	
** Arguments:
**	cp
**	sp
** Returns:	
**___________________________________________________			     */
int TeX_read(cp,sp)				   /*			     */
  char		* cp;				   /*			     */
  char		**sp;				   /*			     */
{ static Token	t     = TokenNULL;		   /*			     */
  static Token	old_t = TokenNULL;		   /*			     */
  static Token	arg[10];			   /*			     */
  Token		t2,tp;				   /*			     */
  MacDef	mac;				   /*			     */
  int		i,d;				   /*			     */
						   /*			     */
  EnsureInit;					   /*			     */
  if ( old_t != TokenNULL ) free_1_token(old_t);   /*			     */
						   /*			     */
  while ( fill_token(&t) )			   /* One token left or	     */
  {						   /* I got a new one	     */
    if (   (mac=active[TokenChar(t)]) == MacDefNULL/*			     */
	&& ( TokenChar(t) != CHAR_ESCAPE	   /* Is it a character?     */
	   || (mac=find_macro(TokenSeq(t),macro))  /*  or an undefined	     */
	      == MacDefNULL )  )		   /*  macro?		     */
    { *cp = TokenChar(t);			   /*			     */
      *sp = TokenSeq(t);			   /*			     */
      UnlinkToken(t,old_t);			   /*			     */
      return 1;					   /*			     */
    }						   /*			     */
    else					   /*			     */
    { UnlinkAndFreeToken(t,t2);			   /* delete macro token     */
						   /*			     */
      while ( fill_token(&t)			   /* While there are more   */
	     && TokenChar(t) == ' ')		   /*  tokens and == ' '     */
      { UnlinkAndFreeToken(t,t2); }		   /* unlink space	     */
						   /*			     */
      for ( i=1; i<=MacroArity(mac); ++i )	   /* Fill the argument	     */
      {						   /*  vector		     */
	if ( !fill_token(&t) )			   /*			     */
	{ arg[i] = TokenNULL;			   /* Not enough tokens	     */
	  Err("*** Unexpected EOF\n");		   /*			     */
	}					   /*			     */
	else if ( TokenChar(t) == CHAR_BEG_GROUP ) /* If there is a group    */
	{ tp = t;				   /*			     */
	  d  = 0;				   /*			     */
	  while ( fill_token(&NextToken(tp))  &&   /* While there are more   */
		 (TokenChar(NextToken(tp)) != '}'  /*  tokens and no	     */
		 || d != 0 ) )			   /*  matching '}'	     */
	  { tp = NextToken(tp);			   /*  walk ahead and	     */
	    switch ( TokenChar(tp) )		   /*  count braces	     */
	    { case CHAR_BEG_GROUP: ++d; break;	   /*			     */
	      case '}': d--; break;		   /*			     */
	    }					   /*			     */
	  }					   /*			     */
	  t2 = NextToken(NextToken(tp));	   /*			     */
	  free_1_token(NextToken(tp));		   /* remove final '}'	     */
	  NextToken(tp) = TokenNULL;		   /*			     */
	  tp = t; t = t2;			   /*			     */
	  UnlinkAndFreeToken(tp,t2);		   /* remove initial '{'     */
	  arg[i] = tp;				   /*			     */
	}					   /*			     */
	else					   /*			     */
	{ arg[i] = t;				   /* Move one token	     */
	  t = NextToken(t);			   /*  to argument vector    */
	  NextToken(arg[i]) = TokenNULL;	   /*			     */
	}					   /*			     */
      }						   /*			     */
						   /*			     */
      t = token_list_copy(MacroToken(mac),t,arg);  /*			     */
						   /*			     */
      for ( i=1; i<MacroArity(mac); ++i )	   /* free the arg vector    */
      {	if ( arg[i] != TokenNULL )		   /*			     */
	{ free_tokens(arg[i]);			   /*			     */
	  arg[i] = TokenNULL;			   /*			     */
	}					   /*			     */
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
  old_t = TokenNULL;				   /*			     */
  return 0;					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			 Stand alone version     			   ***/
/*****************************************************************************/

#ifdef STANDALONE

/*-----------------------------------------------------------------------------
** Function:	new_string()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	
**___________________________________________________			     */
static char * new_string(s)			   /*			     */
  register char * s;				   /*			     */
{ register char * t;				   /*			     */
  if ( (t=malloc(strlen(s)+1)) == NULL )	   /*			     */
  { OUT_OF_MEMORY("string"); }	   		   /*			     */
  (void)strcpy(t,s);  return(t);		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:    main()
** Purpose:     Main rountine for the stand alone version.
** Arguments:
**	argc	Number of arguments (+1)
**	argv	Array of command line arguments (and program name)
** Returns:     
**___________________________________________________                        */
int main(argc,argv)				   /*			     */
  int  argc;					   /*			     */
  char *argv[];					   /*			     */
{ FILE *file;					   /*			     */
  int  kept  = 0;				   /*			     */
  char buffer[1024];				   /*			     */
  char c;					   /*			     */
  char *s;					   /*			     */
						   /*			     */
						   /*			     */
  if ( argc > 3 )				   /*			     */
  { Err("\nTeX macro expander. gene 1/95\n\n");
    Err("Usage: tex_read [macro_file [input_file]]\n\n");
    Err("\tRead the macros and expand them in the input file\n");
    Err("\tafterwards. Comments are also eliminated.\n\n");
    Err("\tThe macros are made up of lines of the following form:\n");
    Err("\t\tmacname[args]=replacement text\n");
    Err("\twhere 0<=args<=9. If args=0 then [0] can be omitted.\n");
    Err("\treplacement text may also contain macros which are expanded.\n\n");
    Err("\tThe program mimics the reading mechanism of TeX.\n");
    Err("\t\n");
    return;					   /*                        */
  }						   /*                        */
  if ( argc > 1 )				   /*			     */
  { if ( (file = fopen(argv[1],"r")) == NULL )	   /*			     */
    { ERROR_EXIT("File open error"); }		   /*			     */
    while ( fgets(buffer,1024,file) )		   /*			     */
    { for(s=buffer;*s;++s) if(*s=='\n') *s='\0';   /*			     */
      for(s=buffer;*s&&isspace(*s);++s) ;	   /*			     */
      if ( *s == '\\' ) TeX_def(s);		   /*			     */
    }						   /*			     */
    TeX_close();				   /*			     */
    (void)fclose(file);				   /*			     */
  }						   /*			     */
  file = stdin;					   /*			     */
  if ( argc > 2 &&				   /*			     */
      (file = fopen(argv[2],"r")) == NULL )	   /*			     */
  { ERROR_EXIT("File open error"); }		   /*			     */
						   /*			     */
  TeX_open_file(file);				   /*			     */
						   /*			     */
  while ( TeX_read(&c,&s) )			   /*			     */
  { if ( kept )					   /*			     */
    { if ( isalpha(c) ) (void)putchar(' ');	   /*			     */
      kept = 0;					   /*			     */
    }						   /*			     */
    (void)putchar(c);				   /*			     */
    if ( s )					   /*			     */
    { (void)fputs(s,stdout);			   /*			     */
      kept = isalpha(*s);			   /*			     */
    }						   /*			     */
  }						   /*			     */
						   /*			     */
  TeX_close();					   /*			     */
						   /*			     */
  if ( file != stdin ) (void)fclose(file);	   /*			     */
  return 0;					   /*			     */
}						   /*------------------------*/

#endif /*STANDALONE*/

#ifdef DEBUG
/*-----------------------------------------------------------------------------
** Function:	show_tokens()
** Purpose:	
**		
**
** Arguments:
**	t
** Returns:	nothing
**___________________________________________________			     */
static void show_tokens(t)			   /*			     */
  register Token t;				   /*			     */
{						   /*			     */
  while ( t != TokenNULL )			   /*			     */
  { if ( TokenChar(t) == CHAR_ESCAPE )		   /*			     */
    { (void)printf("_\\%s",TokenSeq(t)); }	   /*			     */
    else if ( TokenChar(t) > 0xff )		   /*			     */
    { (void)printf("_#%d",TokenChar(t)>>8);	   /*			     */
    }						   /*			     */
    else					   /*			     */
    { (void)printf("_%c",TokenChar(t)); }	   /*			     */
    t = NextToken(t);				   /*			     */
  }						   /*			     */
}						   /*------------------------*/
#endif /*DEBUG*/
