/******************************************************************************
** $Id: print.c,v 2.23 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "print.h"
#include "entry.h"
#include "rsc.h"
#include "macros.h"
#include "type.h"
#include "sbuffer.h"
#include "expand.h"
#include "error.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int tell _ARG((char * fname));			   /* print.c                */
 int told _ARG((void));				   /* print.c                */
 static void indent _ARG((int col));		   /* print.c                */
 static void line_breaking _ARG((char *t,int align));/* print.c              */
 static void pretty_print _ARG((Record rec,char *at));/* print.c             */
 static void print_equation _ARG((char *s,char *t,int align));/* print.c     */
 void puts_in _ARG((char *s, int in));	   	   /* print.c                */
 void print_commented _ARG((Record rec));	   /* print.c                */
 void print_record _ARG((Record rec));		   /* print.c                */
 void putc_in _ARG((int c));			   /* print.c                */
 void set_symbol_type _ARG((char * s));		   /* print.c                */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

 extern int rsc_braces;

/*---------------------------------------------------------------------------*/

#define TAB_WIDTH 8

 static FILE * ofile = stdout;


 static int symbol_type = SYMBOL_TYPE_LOWER;

 static char * upper = "upper";
 static char * lower = "lower";
 static char * cased = "cased";

/*-----------------------------------------------------------------------------
** Function:	set_symbol_type()
** Purpose:	Wrapper function to set the static variable symbol_type.
**		This function is called from rsc.c
** Arguments:
**	s	String description of the value.
** Returns:	nothing
**___________________________________________________			     */
void set_symbol_type(s)				   /*			     */
  register char * s;				   /*			     */
{ if ( case_cmp(s,upper) )			   /*			     */
  { symbol_type = SYMBOL_TYPE_UPPER; }		   /*			     */
  else if ( case_cmp(s,cased) )		   	   /*			     */
  { symbol_type = SYMBOL_TYPE_CASED; }		   /*			     */
  else if ( case_cmp(s,lower) )		   	   /*			     */
  { symbol_type = SYMBOL_TYPE_LOWER; }		   /*			     */
  else						   /*			     */
  { Err("Unknown symbol type ignored.\n"); }	   /*			     */
}						   /*------------------------*/


#ifdef MAYBE_IN_THE_NEXT_RELEASE

 static int key_type = SYMBOL_TYPE_LOWER;

/*-----------------------------------------------------------------------------
** Function:	set_key_type()
** Purpose:	Wrapper function to set the static variable key_type.
**		This function is called from rsc.c
** Arguments:
**	s	String description of the value.
** Returns:	nothing
**___________________________________________________			     */
void set_key_type(s)				   /*			     */
  register char * s;				   /*			     */
{ if ( case_cmp(s,upper) )			   /*			     */
  { key_type = SYMBOL_TYPE_UPPER; }		   /*			     */
  else if ( case_cmp(s,cased) )		   	   /*			     */
  { key_type = SYMBOL_TYPE_CASED; }		   /*			     */
  else if ( case_cmp(s,lower) )		   	   /*			     */
  { key_type = SYMBOL_TYPE_LOWER; }		   /*			     */
  else						   /*			     */
  { Err("Unknown key type ignored.\n"); }	   /*			     */
}						   /*------------------------*/

#endif


/*-----------------------------------------------------------------------------
** Function:	tell()
** Purpose:	Redirect further output to the file given.
**		To avoid problems with a library function of this name,
**		the name is remapped in a header file.
**		The tell()/told() is inpired by the Prolog predicates of
**		the same names.
** Arguments:
**	fname	File name of the new output stream.
** Returns:	
**___________________________________________________			     */
int tell(fname)					   /*			     */
  register char * fname;			   /*			     */
{						   /*			     */
  if ( ofile != stdout ) (void)fclose(ofile);	   /*			     */
  if ( fname == NULL ||				   /*			     */
       (ofile = fopen(fname,"w"))==NULL )	   /*			     */
  { ofile = stdout; return FALSE; }		   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	told()
** Purpose:	Close formerly opened output file.
**		Reset the output file to stdout.
** Arguments:	none
** Returns:	TRUE
**___________________________________________________			     */
int told()					   /*			     */
{						   /*			     */
  if ( ofile != stdout )			   /*			     */
  { (void)fclose(ofile);			   /*			     */
    ofile = stdout;				   /*			     */
  }						   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/


						   /*------------------------*/
 static int column = 0;				   /* The current column of  */
						   /*  the output stream is  */
						   /*  kept in this variable.*/

#define NL	(void)fputc('\n',ofile),column=0
#define PUTC(C) (void)(fputc(C,ofile),++column)
#define PUTS(S) puts_in(S,0)

/*-----------------------------------------------------------------------------
** Function:	puts_in()
** Purpose:	Print a string and update current column.
** Arguments:
**	s	string to be printed.
**	in	indentation. Alignment column.
** Returns:	nothing
**___________________________________________________			     */
void puts_in(s,in)			   	   /*			     */
  register char *s;				   /*			     */
  register int  in;				   /*                        */
{						   /*			     */
  while ( *s )					   /*			     */
  { (void)putc(*s,ofile);			   /*                        */
    switch ( *(s++) )				   /*			     */
    { case '\t':				   /*                        */
	column += TAB_WIDTH - (column%TAB_WIDTH);  /*                        */
	break;					   /*	                     */
      case '\n':				   /*                        */
	column = 0;			   	   /*                        */
        if ( in > 0 ) indent(in);		   /*                        */
        break;  				   /*			     */
      default:	 ++column;			   /*			     */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	putc_in()
** Purpose:	Print a character and update the current column.
** Arguments:
**	c	character to be printed.
** Returns:	nothing
**___________________________________________________			     */
void putc_in(c)					   /*			     */
  register int c;				   /*			     */
{						   /*			     */
  (void)putc(c,ofile);				   /*			     */
 						   /*                        */
  switch ( c )					   /*			     */
  { case '\t':					   /*                        */
      column += TAB_WIDTH - (column%TAB_WIDTH);	   /*                        */
      break;					   /*	                     */
    case '\n':					   /*                        */
      column = 0;				   /*                        */
      break;	   				   /*			     */
    default:					   /*                        */
      ++column;			   		   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	indent()
** Purpose:	Add spaces or tabs to indent to the given column.
**		the current column is beyond col nothing happens.
**		The resource use.tabs can be used to disable the use of TAB.
** Arguments:
**	col	Target column
** Returns:	nothing
**___________________________________________________			     */
static void indent(col)				   /*			     */
  register int col;				   /*			     */
{						   /*			     */
  while ( column < col )			   /*			     */
  { if (   rsc_use_tabs				   /*	TAB is allowed and   */
	&& column+TAB_WIDTH-(column%TAB_WIDTH) <= col )/* enough space left  */
    { (void)putc('\t',ofile);			   /*	then put a TAB and   */
      column += TAB_WIDTH - (column%TAB_WIDTH);	   /*	update column.	     */
    }						   /*			     */
    else					   /* otherwise		     */
    { (void)putc(' ',ofile);			   /*  write a singe space   */
      ++column;					   /*  and advance column.   */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	line_breaking()
** Purpose:	Write out a right hand side of an equation.
**		If it doesn't fit break the line into several parts and
**		print them on successive lines.
**		Temporarily end marks are placed inside the string,
**		but the old contents has been restored at the end.
** Arguments:
**	t	string to print.
**	align	starting column for continuation lines.
** Returns:	nothing
**___________________________________________________			     */
static void line_breaking(t,align)		   /*			     */
  register char *t;				   /* string to print.	     */
  int		align;				   /* alignment column	     */
{ register char *s;				   /* intermediate pointer   */
  char		end_c;				   /* temp. character.	     */
  int		brace,				   /* brace counter	     */
		len,				   /* length of rem. output  */
		first = TRUE;			   /* indicator for #	     */
						   /*			     */
  while ( is_space(*t) ) ++t;			   /* skip leading spaces    */
						   /*			     */
  indent(align);				   /* goto alignment column  */
						   /*			     */
  while ( *t )					   /* as long as sth to print*/
  { s = t;					   /*			     */
						   /*			     */
    switch( *t )				   /*			     */
    { case '"':					   /* QUOTED PART	     */
	for ( len=2,++t;			   /*			     */
	     *t && *t != '\"';			   /* Search terminating "   */
	     ++t,++len )			   /*			     */
	{ if ( *t == '\\' && *(t+1) != '\0' )	   /* skip over quoted and   */
	  { ++t; ++len; }			   /*  similar constructs.   */
	}					   /*			     */
	if ( *t ) ++t;				   /* skip after end, if poss*/
	if ( *t ) { end_c = *++t; *t = '\0'; }	   /* save char and mark end.*/
	else	  { end_c = *t; }		   /*			     */
	break;					   /*			     */
      case '{':					   /* BRACED PART	     */
	brace = 1;				   /*			     */
	for ( len=2,++t;			   /* find matching brace.   */
	     *t && brace>0;			   /*			     */
	     ++t,++len )			   /*			     */
	{ switch ( *t )				   /*			     */
	  { case '\\': if ( *(t+1) ) ++t; break;   /* ignore \{ \} etc	     */
	    case '{': ++brace; break;		   /*			     */
	    case '}': brace--; break;		   /*			     */
	  }					   /*			     */
	}					   /*			     */
	if ( *t ) { end_c = *++t; *t = '\0'; }	   /* save char and mark end.*/
	else	  { end_c = *t; }		   /*			     */
	break;					   /*			     */
      default:					   /* Now we should have a   */
	while ( is_allowed(*t) ) ++t;		   /*	SYMBOL		     */
	end_c = *t; *t = '\0';			   /*			     */
	s = get_item(symbol(s),symbol_type);	   /*			     */
	len = strlen(s);			   /*			     */
    }						   /*			     */
						   /* Now s is a single	     */
						   /*  string to print.	     */
						   /* t points to the closing*/
						   /*  '\0' of s	     */
						   /* end_c is the old *t    */
    while ( *s )				   /*			     */
    { if ( len + (first?0:3) <= rsc_linelen - column)/* Is there enough space*/
      { if ( !first ) PUTS(" # ");		   /* Maybe add separator    */
	puts_in(s,align);			   /* write it out	     */
	s = t;					   /* and we are done	     */
      }						   /*			     */
      else if ( !first )			   /* If sth has been before */
      { puts_in("\n# ",align-2);	   	   /*  start a new line	     */
	first = TRUE;				   /*			     */
      }						   /* Now we have to break   */
      else					   /*  a single entry	     */
      { char save_c,				   /*                        */
	     *save_ptr,				   /*                        */
	     *ptr;			   	   /*			     */
						   /*			     */
	save_ptr = s + rsc_linelen - column;	   /* Potential end	     */
	for(ptr=s;				   /* Search next newline    */
	    ptr <= save_ptr && *ptr != '\n';	   /*  or end of region      */
	    ptr++) {}				   /*                        */
 						   /*                        */
	if ( *ptr == '\n' )			   /*                        */
	{ save_ptr = ptr;			   /*                        */
	  *save_ptr = '\0';	   		   /* Save and mark end.     */
	  puts_in(s,align); NL; indent(align);	   /*			     */
	  *save_ptr = '\n';			   /* Restore end	     */
	  len += s - save_ptr - 1;		   /* Update the length	     */
	  s = save_ptr+1;			   /*			     */
	}					   /*                        */
	else					   /*                        */
	{					   /*                        */
	  while ( save_ptr != s && *save_ptr != ' ' )/*                      */
	  { save_ptr--; }			   /* Find a  SPC  backward  */
	  					   /*			     */
	  if ( save_ptr == s  )			   /* If no SPC found then   */
	  { while ( *save_ptr && *save_ptr != ' ' )/*  search one forward.   */
	    { ++save_ptr; }	   		   /*                        */
	  }					   /*                        */
	  len += s - save_ptr;			   /* Update the length	     */
	  save_c = *save_ptr; *save_ptr = '\0';	   /* Save and mark end.     */
	  puts_in(s,align); NL; indent(align);	   /*			     */
	  *save_ptr = save_c;			   /* Restore end	     */
	  s = save_ptr;	   			   /*			     */
	  while ( is_space(*s) ) { ++s; len--; }   /* Skip spaces	     */
	}					   /*                        */
      }						   /*			     */
    }						   /*			     */
    *t = end_c;					   /* Restore the end	     */
						   /*			     */
    while ( *t && *t != '#' ) ++t;		   /* Search next #	     */
    if ( *t ) ++t;				   /* Skip beyond the #	     */
    while ( is_space(*t) ) ++t;			   /* Ignore following spaces*/
    first = FALSE;				   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	print_equation()
** Purpose:	Print something of the form  s = t 
**		If desired it can be indented. t is broken if it doesn't fit
**		in one line.
** Arguments:
**	s	left hand side
**	t	right hand side
**	align	target column. If negative no indentation is performed.
** Returns:	nothing
**___________________________________________________			     */
static void print_equation(s,t,align)		   /*			     */
  register char *s;				   /*			     */
  register char *t;				   /*			     */
  int		align;				   /*			     */
{						   /*			     */
  if ( align < 0 )				   /*			     */
  { PUTS(get_item(s,symbol_type));		   /*			     */
    PUTC('=');					   /*			     */
    PUTS(t);					   /*			     */
  }						   /*			     */
  else						   /*			     */
  { indent(rsc_indent);				   /*			     */
    PUTS(get_item(s,symbol_type));		   /*			     */
    indent(align-2);				   /*			     */
    PUTC('=');					   /*			     */
    line_breaking(t,align);			   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	print_record()
** Purpose:	Print a record to the assigned output file
** Arguments:
**	rec	record to print
** Returns:	nothing
**___________________________________________________			     */
void print_record(rec)				   /*			     */
  register Record rec;				   /* record to print	     */
{ pretty_print(rec,"@");			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	print_commented()
** Purpose:	Print a record to the assigned output file, but print ### 
**		instead of the eading @.
** Arguments:
**	rec	record to print
** Returns:	nothing
**___________________________________________________			     */
void print_commented(rec)			   /*			     */
  register Record rec;				   /* record to print	     */
{ pretty_print(rec,"###");			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	pretty_print()
** Purpose:	Format and print a complete record.
**		The record type and several resources are taken into acc.
** Arguments:
**	rec	record to print
**	at	initial string used before the type. Should be "@" normally.
** Returns:	nothing
**___________________________________________________			     */
static void pretty_print(rec,at)		   /*			     */
  register Record	rec;			   /* record to print	     */
  char			*at;			   /* initial string = "@"   */
{ register char		**hp;			   /* heap pointer	     */
  register unsigned int i;			   /*			     */
  char open_brace, close_brace;			   /*			     */
 						   /*                        */
  sort_record(rec);				   /*                        */
 						   /*                        */
  hp = RecordHeap(rec);				   /*			     */
  if ( IsNormalRecord(RecordToken(rec)) ) { NL; }  /*			     */
  PUTS(at);					   /*			     */
  PUTS(EntryName(RecordToken(rec)));		   /*			     */
 						   /*                        */
  if ( rsc_parentheses )			   /*                        */
  { open_brace  = '(';			   	   /*                        */
    close_brace = ')';			   	   /*                        */
  }						   /*                        */
  else						   /*                        */
  { open_brace  = '{';			   	   /*                        */
    close_brace = '}';			   	   /*                        */
  }						   /*                        */
 						   /*                        */
  switch ( RecordToken(rec) )			   /*			     */
  { case COMMENT:				   /*			     */
      indent(rsc_col_c);			   /*			     */
      PUTS(*hp);				   /*			     */
      PUTC(' ');				   /*                        */
      NL;				   	   /*			     */
      break;					   /*			     */
    case PREAMBLE:				   /*			     */
      PUTC(open_brace);				   /*			     */
      indent(rsc_col_p);			   /*			     */
      line_breaking(*hp,rsc_col_p);		   /*			     */
      PUTC(' ');				   /*                        */
      PUTC(close_brace);			   /*                        */
      NL;				   	   /*			     */
      break;					   /*			     */
    case STRING:				   /*			     */
      PUTC(open_brace);				   /*			     */
      print_equation(*hp,*(hp+1),rsc_col_s);	   /*			     */
      PUTC(' ');				   /*                        */
      PUTC(close_brace);			   /*                        */
      NL;				   	   /*			     */
      break;					   /*			     */
    default:					   /*			     */
      PUTC(open_brace);				   /*			     */
      for ( i=RecordFree(rec); i>0; i-=2 )	   /*			     */
      { if ( *hp )				   /* No deleted entry	     */
	{ if ( *(hp+1) )			   /* If equation	     */
	  { at = ( rsc_expand_macros		   /*			     */
		  ? expand_rhs(*(hp+1),rsc_braces) /*			     */
		  : *(hp+1) );			   /*			     */
	    print_equation(*hp,			   /*			     */
			   at,			   /*			     */
			   rsc_col);		   /*			     */
	  }					   /*			     */
	  else					   /* Otherwise print a key  */
	  { indent(rsc_col_key);		   /*			     */
	    PUTS(get_key_name(*hp));      	   /*			     */
	  }					   /*			     */
	  if ( i > 2 ) { PUTC(','); NL; }	   /* write sep if required  */
	}					   /*			     */
	hp+=2;					   /* Goto next pair.	     */
      }						   /*			     */
      NL; PUTC(close_brace);	   	   	   /*                        */
      for ( i=rsc_newlines; i>0; --i ) { NL; }	   /*                        */
  }						   /*			     */
}						   /*------------------------*/
