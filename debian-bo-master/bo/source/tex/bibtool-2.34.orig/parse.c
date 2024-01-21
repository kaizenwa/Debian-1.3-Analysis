/******************************************************************************
** $Id: parse.c,v 2.26 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "entry.h"
#include "error.h"
#include "pxfile.h"
#include "rsc.h"
#include "stack.h"
#include "sbuffer.h"
#include "macros.h"
#include "print.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int parse _ARG((void));			   /* parse.c                */
 int read_rsc _ARG((char *name));		   /* parse.c                */
 int see_bib _ARG((char * fname));		   /* parse.c                */
 int see_bib_msg _ARG((char *s));		   /* parse.c                */
 int see_rsc _ARG((char * fname));		   /* parse.c                */
 int seen _ARG((void));				   /* parse.c                */
 static int fill_line _ARG((void));		   /* parse.c                */
 static int parse_block _ARG((int quotep));	   /* parse.c                */
 static int parse_equation _ARG((int macp));	   /* parse.c                */
 static int parse_key _ARG((int alpha));	   /* parse.c                */
 static int parse_rhs _ARG((void));		   /* parse.c                */
 static int parse_string _ARG((int quotep));	   /* parse.c                */
 static int parse_symbol _ARG((int alpha));	   /* parse.c                */
 static int parse_value _ARG((void));		   /* parse.c                */
 static int skip _ARG((int inc));		   /* parse.c                */
 static int skip_c _ARG((void));		   /* parse.c                */
 static int skip_nl _ARG((void));		   /* parse.c                */
 static void init___ _ARG((char ***pathp,char **pattern,char **envvp,char *env));/* parse.c*/
 static void init_parse _ARG((void));		   /* parse.c                */
 static void parse_number _ARG((void));		   /* parse.c                */
 void init_read _ARG((void));			   /* parse.c                */
 void normalize_symbol _ARG((char * s));	   /* parse.c                */
 void set_rsc_path _ARG((char * val));		   /* parse.c                */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

#define Error(X)	    error(ERR_ERROR|ERR_POINT|ERR_FILE,X,empty,empty,\
				  file_line_buffer,flp,flno,filename)
#define Warning(X)	    error(ERR_WARN|ERR_POINT|ERR_FILE,X,empty,empty, \
				  file_line_buffer,flp,flno,filename)


 static StringBuffer *parse_sb = (StringBuffer*)NULL;
 
#define FLBLEN	80		/* initial size and increment of line buffer */

 char		*filename;
 static FILE	*file;
 static uchar	*file_line_buffer;
 static size_t	fl_size	 = 0;
 static int	flno	 = 0;
 static uchar	*flp;


#define EmptyC		(*flp=='\0')
#define CurrentC	*flp
#define FutureC		*(flp+1)
#define ClearLine	(*flp='\0')
#define GetC		skip(TRUE)
#define NextC		*(flp++)
#define SkipC		++flp
#define TestC		skip(FALSE)
#define UnGetC		flp--

#define InitLine	*file_line_buffer = '\0';	\
			flp  = file_line_buffer;	\
			flno = 0;

 static char *str_syntax = "Syntax Error";
 static char *str_eof	 = "EOF unexpected";
 static char *str_stdin	 = "<stdin>";

#define SyntaxError	Error(str_syntax)
#define EofError	Error(str_eof)

/*-----------------------------------------------------------------------------
** Function:	init___()
** Purpose:	Initialize the reading apparatus.
** Arguments:
**	pathp
**	pattern
**	envvp
**	env
** Returns:	nothing
**___________________________________________________			     */
static void init___(pathp,pattern,envvp,env)	   /*			     */
  char		***pathp;			   /*			     */
  char		**pattern;			   /*			     */
  char		**envvp;			   /*			     */
  char		*env;				   /*			     */
{ register char **cpp,				   /*			     */
		*cp;				   /*			     */
						   /*			     */
  if ( *pathp != (char**)0 )			   /*			     */
  { free((char*)*pathp); *pathp = (char**)0; }	   /*			     */
						   /*			     */
  if ( (cp = getenv(env)) != NULL )		   /*			     */
  { *envvp = cp; }				   /*			     */
						   /*			     */
  if ( *envvp )					   /*			     */
  { *pathp = px_s2p(*envvp,*rsc_env_sep);	   /*			     */
    if ( *pathp == (char**)0 )			   /*			     */
    { WARNING2(env,"search path extension failed.");/*			     */
    }						   /*			     */
    DebugPrint2("Path extension ",env);		   /*                        */
  }						   /*			     */
						   /*			     */
  if ( *rsc_dir_file_sep != '/' )		   /*			     */
  { for ( cpp=pattern; *cpp; ++cpp )		   /*			     */
    { *cpp = new_string(*cpp);			   /*                        */
      for (cp= *cpp; *cp; ++cp )		   /*			     */
      { if ( *cp == '/' ) *cp = *rsc_dir_file_sep; /*			     */
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

 static char **f_path = (char**)0;
 static char *f_pattern[] =
 { "%s/%s", "%s/%s.bib", NULL };

/*-----------------------------------------------------------------------------
** Function:	init_read()
** Purpose:	Initialize the reading apparatus.
**		Primarily try to figure out the file search path.
** Arguments:
**	
** Returns:	nothing
**___________________________________________________			     */
void init_read()				   /*			     */
{						   /*			     */
  init___(&f_path,				   /*			     */
	  f_pattern,				   /*			     */
	  &rsc_v_bibtex,			   /*			     */
	  rsc_e_bibtex	);			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	see_bib_msg()
** Purpose:	Message function for use with  px_fopen().
** Arguments:
**	s
** Returns:	
**___________________________________________________			     */
int see_bib_msg(s)				   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  if ( rsc_verbose ) VerbosePrint2("Trying ",s);   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	see_bib()
** Purpose:	Open a bib file to read from.
**		If the argument is NULL then stdin is used as input stream.
** Arguments:
**	fname
** Returns:	
**___________________________________________________			     */
int see_bib(fname)				   /*			     */
  register char * fname;			   /*			     */
{						   /*			     */
  init_parse();					   /*			     */
  InitLine;					   /*			     */
  if ( fname )					   /*			     */
  { file = px_fopen(fname,			   /*			     */
		    "r",			   /*			     */
		    f_pattern,			   /*			     */
		    f_path,			   /*			     */
		    see_bib_msg);		   /*			     */
    filename = px_filename;			   /*			     */
    return( file != NULL );			   /*			     */
  }						   /*			     */
  filename = str_stdin;				   /*			     */
  file	   = stdin;				   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	seen()
** Purpose:	Close input file.
** Arguments:	none
** Returns:	Failure status
**___________________________________________________			     */
int seen()					   /*			     */
{ if ( file && file != stdin )			   /*			     */
  { (void)fclose(file);				   /*			     */
    file = 0L;					   /*			     */
    return TRUE;				   /*			     */
  }						   /*			     */
  file = 0L;					   /*			     */
  return FALSE;					   /*			     */
}						   /*------------------------*/


#define Expect(C,N)	  if ( GetC != C )	    { SyntaxError; return(N); }
#define ExpectSymbol(C,N) if (!parse_symbol(C))		return(N)
#define ExpectKey(C,N)    if (!parse_key(C))		return(N)
#define ExpectRhs(N)	  if (!parse_rhs())		return(N)
#define ExpectEq(N)	  if (!parse_equation(FALSE))	return(N)
#define ExpectEqMac(N)	  if (!parse_equation(TRUE))	return(N)

/*-----------------------------------------------------------------------------
** Function:	init_parse()
** Purpose:	Initialize the parser.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
static void init_parse()			   /*			     */
{						   /*			     */
  if ( fl_size != 0 ) return;			   /* Already initialzed?    */
						   /*			     */
  fl_size = FLBLEN;				   /*			     */
  if ( (file_line_buffer=(uchar*)malloc(fl_size)) == NULL )/* Allocate the   */
  { OUT_OF_MEMORY("line buffer"); }  		   /*  line buffer	     */
				   		   /*  or message and exit.  */
  if ( parse_sb		   == (StringBuffer*)0 &&  /* Try to initialize the  */
       (parse_sb=sbopen()) == (StringBuffer*)0 )   /*  string buffer for the */
  { OUT_OF_MEMORY("parser");	   		   /*  parser or exit	     */
  }			   			   /*  with error message.   */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fill_line()
** Purpose:	Filling the line buffer until end-of-line or end-of-file
**		encountered.
**		
**		Since I don't want to use a fixed line length the algorithm
**		is a little bit more complicated.
**		First I try to read into the file_line_buffer as it is.
**		If this succeeds I check to see what caused fgets to terminate.
**		Either a end-of-line is in the buffer or the buffer is not
**		filled, i.e. end-of-file has been encountered,
**		then 0 is returned.
**		Otherwise the buffer has been filled and I try to enlarge the
**		buffer and read another chunk of bytes.
** Arguments:	none
** Returns:	Returns 0 iff a character has been read.
**___________________________________________________			     */
static int fill_line()				   /*			     */
{ register int	len;				   /*			     */
						   /*			     */
  flp = file_line_buffer;			   /* Reset line pointer     */
  ++flno;					   /* Increase line number   */
						   /*			     */
  if ( fgets((char*)file_line_buffer,fl_size,file) == NULL )/*Get first chunk*/
  { ClearLine; 		   			   /*	or report EOF	     */
    DebugPrint1("Reading failed for first line."); /*                        */
    return 1;					   /*                        */
  }						   /*                        */
						   /*			     */
  FOREVER					   /*			     */
  { for (len=0;					   /* Find the end	     */
	 file_line_buffer[len] != '\0';		   /*  of the buffer and     */
	 ++len ) ;				   /*  count the length.     */
						   /*			     */
#ifdef DEBUG
    ErrPrintF2("+++ BibTool: line buffer: used %d of %d\n",len+1,fl_size);/* */
#endif
						   /*			     */
    if ( file_line_buffer[len-1] == '\n'	   /*			     */
	|| len < fl_size-1)			   /*			     */
    { return 0; }				   /*			     */
						   /*			     */
    if ( (file_line_buffer = (uchar*)		   /* Try to enlarge	     */
	  realloc((char*)file_line_buffer,	   /*  the line buffer	     */
		  fl_size+=FLBLEN)) == NULL )	   /*			     */
    { OUT_OF_MEMORY("line buffer"); }		   /*			     */
    flp = file_line_buffer;			   /* Reset line pointer     */
						   /*			     */
    if ( fgets((char*)file_line_buffer+len,FLBLEN+1,file)/*		     */
	== NULL )				   /*			     */
    { return 0; }				   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	skip()
** Purpose:	Skip over spaces. Return the next nonspace character or EOF.
**		If inc is TRUE point to the first character after the one
**		returned.
** Arguments:
**	inc
** Returns:	The next character
**___________________________________________________			     */
static int skip(inc)				   /*			     */
  register int inc;				   /*			     */
{						   /*			     */
  FOREVER					   /*			     */
  { if ( EmptyC && fill_line() )   return EOF;	   /*			     */
    else if ( is_space(CurrentC) ) SkipC;	   /*			     */
    else return ( inc ? NextC : CurrentC );	   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	skip_c()
** Purpose:	Return the next character or EOF.
** Arguments:	none
** Returns:	
**___________________________________________________			     */
static int skip_c()				   /*			     */
{						   /*			     */
  FOREVER					   /*			     */
  { if ( EmptyC && fill_line() )   return EOF;	   /*			     */
    else { return ( NextC ); }			   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	skip_nl()
** Purpose:	Return the next character or EOF.
**		Any number of spaces is returned as a single space.
**		Doubled newlines are preserved.
** Arguments:	none
** Returns:	
**___________________________________________________			     */
static int skip_nl()				   /*			     */
{ static int state = FALSE;			   /*			     */
  int c;					   /*                        */
 						   /*                        */
  if ( state ) { state = FALSE; return '\n'; }	   /*                        */
 						   /*                        */
  FOREVER					   /*			     */
  { if ( EmptyC && fill_line() )   return EOF;	   /*			     */
    else if ( is_space(CurrentC) )		   /*			     */
    {						   /*                        */
      for ( c=skip_c();				   /*                        */
	    c != EOF && is_space(c) && c != '\n';  /*                        */
	    c=skip_c() ) {}			   /*                        */
      if ( c == EOF  ) return EOF;		   /*                        */
      if ( c != '\n' ) { UnGetC; return ' '; }	   /*                        */
 						   /*                        */
      for ( c=skip_c();				   /*                        */
	    c != EOF && is_space(c) && c != '\n';  /*                        */
	    c=skip_c() ) {}			   /*                        */
      if ( c == EOF  ) return EOF;		   /*                        */
      if ( c != '\n' ) { UnGetC; return ' '; }	   /*                        */
 						   /*                        */
      for ( c=skip_c();				   /*                        */
	    c != EOF && is_space(c);		   /*                        */
	    c=skip_c() ) {}			   /*                        */
      if ( c == EOF  ) return EOF;		   /*                        */
      UnGetC;					   /*                        */
      state = TRUE;				   /*                        */
      return '\n';				   /*                        */
    }						   /*			     */
    else { return ( NextC ); }		   	   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	normalize_symbol()
** Purpose:	Function to translate a symbol into a normal form.
**		This will translate the symbol to lower case.
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void normalize_symbol(s)			   /*                        */
  register char * s;				   /*                        */
{ 						   /*                        */
  while ( *s ) { *s = ToLower(*s); ++s; }	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_symbol()
** Purpose:	Parse a symbol and push it to the stack.
**		Upon failure issue an appropriate message.
** Arguments:
**	alpha
** Returns:	Success status
**___________________________________________________			     */
static int parse_symbol(alpha)			   /*			     */
  register int alpha;				   /*			     */
{ register int c;				   /*			     */
  register char * cp;				   /*			     */
						   /*			     */
  c = GetC;  cp = (char*)flp-1;			   /*			     */
  if ( alpha && (! is_alpha(c)) )		   /*			     */
  { SyntaxError; return(FALSE); }		   /*			     */
  while ( is_allowed(CurrentC) ) { SkipC; }	   /*			     */
  c = CurrentC; CurrentC = '\0';		   /*			     */
  normalize_symbol(cp);				   /*			     */
  push_string(symbol(cp));			   /*			     */
  CurrentC = c;					   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_key()
** Purpose:	Parse a symbol and push it to the stack.
**		Upon failure issue an appropriate message.
** Arguments:
**	alpha
** Returns:	Success status
**___________________________________________________			     */
static int parse_key(alpha)			   /*			     */
  register int alpha;				   /*			     */
{ register int c;				   /*			     */
  register char * cp, *s;			   /*			     */
						   /*			     */
  c = GetC;  cp = (char*)flp-1;			   /*			     */
  if ( alpha && (! is_alpha(c)) )		   /*			     */
  { SyntaxError; return(FALSE); }		   /*			     */
  while ( is_allowed(CurrentC) ) { SkipC; }	   /*			     */
  c = CurrentC; CurrentC = '\0';		   /*			     */
  if ( rsc_key_case )				   /*                        */
  { s = symbol(cp);				   /*                        */
    normalize_symbol(cp);			   /*			     */
    cp = symbol(cp);				   /*                        */
    save_key(cp,s);				   /*                        */
  }						   /*                        */
  else						   /*                        */
  { normalize_symbol(cp);			   /*			     */
    cp = symbol(cp);				   /*                        */
  }						   /*                        */
  push_string(cp);			   	   /*			     */
  CurrentC = c;					   /*			     */
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_number()
** Purpose:	Parse a number and push it to the stack.
**		This function is called when at least one digit has been seen.
**		Thus no error can occur in this function.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
static void parse_number()			   /*			     */
{ register int c;				   /*			     */
  register char * cp;				   /*			     */
						   /*			     */
  cp = (char*)flp;				   /*                        */
  while ( is_digit(*flp) ) flp++;	   	   /*			     */
  c = *flp; *flp = '\0';			   /*			     */
  push_string(symbol(cp));			   /*                        */
  *flp = c;		   			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_string()
** Purpose:	Parse a string and push it to the stack.
**		A string is something enclosed in ""
**		Consider the brace level to determine the end of the string.
** Arguments:
**	quotep	Boolean. TRUE iff the leading " has already been stripped off
**		the input stream.
** Returns:	Success status
**___________________________________________________			     */
static int parse_string(quotep)			   /*			     */
  int          quotep;				   /*			     */
{ register int c,				   /*                        */
    	       left;				   /*			     */
						   /*			     */
  left = 0;					   /*			     */
  if ( quotep ) (void)sbputchar('"',parse_sb);	   /*			     */
  do						   /*			     */
  { switch ( c=skip_nl() )			   /*			     */
    { case EOF:	 EofError; return(FALSE);	   /*			     */
      case '{':	 left++; (void)sbputchar(c,parse_sb); break;/*		     */
      case '}':	 if ( left--<0 )		   /*			     */
		 { Warning("Expecting \" here"); } /*			     */
		 (void)sbputchar(c,parse_sb); break;/*			     */
      case '\\': (void)sbputchar(c,parse_sb); c = NextC;/*		     */
		 (void)sbputchar(c,parse_sb); c = ' ';/*		     */
		 break;				   /*			     */
      case '"':	 if ( !quotep ) break;		   /*			     */
      default:	 (void)sbputchar(c,parse_sb);	   /*			     */
    }						   /*			     */
  } while ( c != '"' );				   /*			     */
						   /*			     */
  if ( left ) Warning("Unbalenced parenthesis");   /*			     */
  return(TRUE);					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_block()
** Purpose:	Parse a block and push it to the stack.
**		A block is something enclosed in {}
**		Consider the brace level to determine the end of the string.
** Arguments:
**	quotep	Boolean. TRUE iff the leading { has already been stripped off
**		the input stream.
** Returns:	Success status
**___________________________________________________			     */
static int parse_block(quotep)			   /*			     */
  int quotep;					   /*			     */
{ register int c,left;				   /*			     */
						   /*			     */
  left = 1;					   /*			     */
  if ( quotep ) (void)sbputchar('{',parse_sb);	   /*			     */
 						   /*                        */
  FOREVER					   /*			     */
  { switch ( c=skip_nl() )			   /*			     */
    { case EOF: EofError; return FALSE;		   /*			     */
      case '{': left++; break;			   /*			     */
      case '}':					   /*			     */
	if ( --left < 1 )			   /*			     */
	{ if ( quotep ) (void)sbputchar('}',parse_sb);/*		     */
	  return TRUE;				   /*			     */
	}					   /*			     */
    }						   /*			     */
    (void)sbputchar(c,parse_sb);		   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_rhs()
** Purpose:	Parse the right hand side of an item.
**		This can be composed of strings, blocks, numbers, and symbols
**		separated by #
** Arguments:	none
** Returns:	Success status
**___________________________________________________			     */
static int parse_rhs()				   /*			     */
{						   /*			     */
  sbrewind(parse_sb);				   /*			     */
  do						   /*			     */
  { if ( sbtell(parse_sb) != 0 )		   /*			     */
    { (void)sbputs(" # ",parse_sb); }		   /*			     */
						   /*			     */
    switch ( GetC )				   /*			     */
    { case EOF: EofError;	   return(FALSE);  /*			     */
						   /*			     */
      case '"': if ( !parse_string(TRUE) ) return(FALSE);/*		     */
	break;					   /*			     */
						   /*			     */
      case '{': if ( !parse_block(TRUE) ) return(FALSE);/*		     */
	break;					   /*			     */
						   /*			     */
      case '0': case '1': case '2': case '3': case '4':/*		     */
      case '5': case '6': case '7': case '8': case '9':/*		     */
	UnGetC; parse_number();			   /*			     */
	(void)sbputs(pop_string(),parse_sb);	   /*			     */
	break;					   /*			     */
						   /*			     */
      default:					   /*			     */
	UnGetC; ExpectSymbol(TRUE,FALSE);	   /*			     */
	{ register char * mac;			   /*			     */
	  mac = pop_string();			   /*			     */
	  (void)look_macro(mac,1);		   /*			     */
	  (void)sbputs(mac,parse_sb);		   /*			     */
	}					   /*			     */
    }						   /*			     */
  } while ( GetC == '#' );			   /*			     */
						   /*			     */
  push_string(symbol(sbflush(parse_sb)));	   /*			     */
  sbrewind(parse_sb);				   /*			     */
  UnGetC; return(TRUE);				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_equation()
** Purpose:	
**		
**
** Arguments:
**	macp
** Returns:	Success status
**___________________________________________________			     */
static int parse_equation(macp)			   /*			     */
  int macp;					   /*			     */
{ register char *s, *t;				   /*			     */
						   /*			     */
  ExpectSymbol(TRUE,FALSE);			   /*			     */
  Expect('=',FALSE);				   /*			     */
  ExpectRhs(FALSE);				   /*			     */
						   /*			     */
  t = pop_string();				   /*			     */
  s = pop_string();				   /*			     */
  push_to_master_record(s,t);			   /*			     */
  if ( macp ) { def_macro(s,t,0); }		   /*			     */
  return(TRUE);					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse()
** Purpose:	Read one entry and fill the internal record structure.
**		return the type of the entry.
** Arguments:	none
** Returns:	
**___________________________________________________			     */
int parse()					   /*			     */
{ register int type,				   /*			     */
	       n,				   /*			     */
	       c,				   /*			     */
	       om;				   /*			     */
  long	       ignored = 0L;			   /*			     */
 						   /*                        */
  do						   /*                        */
  { init_parse();				   /*			     */
						   /*			     */
    while ( (c=skip_c()) != '@' )		   /* Skip to next @	     */
    { if ( c==EOF ) return(EndOfFile);		   /*			     */
      if ( !is_space(c) ) ++ignored;		   /*			     */
      if ( ignored > 0 && rsc_pass_comment )	   /*			     */
      { putc_in(c); }				   /*			     */
    }						   /*			     */
    						   /*			     */
    if ( ignored != 0L )			   /*			     */
    { if ( rsc_pass_comment ) putc_in('\n');	   /*			     */
      else					   /*			     */
      { static char buffer[32];			   /*			     */
      						   /*			     */
	(void)sprintf(buffer,"%ld",ignored);	   /*			     */
	error(ERR_WARN|ERR_FILE,buffer,		   /*			     */
	      " non-space characters ignored.",	   /*			     */
	      (char*)0,(uchar*)0,(uchar*)0,	   /*			     */
	      flno,filename);			   /*			     */
      }						   /*			     */
    }						   /*			     */
						   /*			     */
    if ( (type=find_entry_type((char*)flp)) == NOOP )/*			     */
    { Error("Unknown entry type"); return(NOOP); } /*			     */
    flp += strlen(EntryName(type));		   /*			     */
 						   /*                        */
    if ( type == COMMENT && rsc_pass_comment )
    { putc_in('@');
      puts_in(EntryName(type),0);
    }
  } while (type == COMMENT);			   /*                        */
					   	   /*			     */
  c = GetC;					   /*			     */
  if ( c != '{' && c != '(' )			   /*			     */
  { Error("Expected '{' or '(' missing");	   /*			     */
    return(NOOP);				   /*			     */
  }						   /*			     */
						   /*			     */
  clear_master_record(type);			   /*			     */
						   /*			     */
  switch ( type )				   /*			     */
  { case COMMENT:				   /* This code is not used  */
      UnGetC; 					   /*  any more.             */
      (void)parse_rhs();			   /*                        */
      push_to_master_record(pop_string(),(char*)0);/*			     */
      return type;				   /*                        */
 						   /*                        */
    case PREAMBLE:				   /*			     */
      ExpectRhs(NOOP);				   /*			     */
      push_to_master_record(pop_string(),(char*)0);/*			     */
      break;					   /*			     */
						   /*			     */
    case STRING:				   /*			     */
      ExpectEqMac(NOOP);			   /*			     */
      break;					   /*			     */
						   /*			     */
    default:					   /*			     */
      if ( TestC == ',' )			   /*			     */
      { Warning("Missing reference key");	   /*			     */
	push_to_master_record("",(char*)0);	   /*			     */
	(void)GetC;				   /*			     */
      }						   /*			     */
      else					   /*			     */
      { ExpectKey(FALSE,NOOP);		   	   /*			     */
	Expect(',',NOOP);			   /*			     */
	push_to_master_record(pop_string(),(char*)0);/*			     */
      }						   /*			     */
						   /*			     */
      do					   /*			     */
      { ExpectEq(NOOP);				   /*			     */
	for( n=0; GetC==','; n++)		   /*			     */
	{ if ( n > 0 )				   /*			     */
	  { Warning("Multiple ',' ignored."); }	   /*			     */
	}					   /*			     */
	UnGetC;					   /*			     */
	om = ( (TestC != '}') && (TestC != ')') ); /*			     */
	if ( om && n==0 )			   /*			     */
	{ Warning("Missing ',' assumed."); }	   /*			     */
						   /*			     */
      } while ( om );				   /*			     */
  }						   /*			     */
						   /*			     */
  switch ( GetC )				   /*			     */
  { case '}':					   /*			     */
      if( c != '{' )				   /*			     */
      { Warning("Parenthesis '(' closed by '}'"); }/*			     */
      break;					   /*			     */
    case ')':					   /*			     */
      if( c != '(' )				   /*			     */
      { Warning("Parenthesis '{' closed by ')'"); }/*			     */
      break;					   /*			     */
    default:					   /*			     */
      Error("Expected '}' or ')' missing");	   /*			     */
      return(NOOP);				   /*			     */
  }						   /*			     */
  return(type);					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/*									     */
/*****************************************************************************/

 static char **r_path	  = (char**)0;
 static char *r_pattern[] =
 { "%s/%s", "%s/%s.rsc", NULL };

/*-----------------------------------------------------------------------------
** Function:	set_rsc_path()
** Purpose:	Initialize the resource file reading apparatus.
**		Primarily try to figure out the file search path.
** Arguments:
**	val
** Returns:	nothing
**___________________________________________________			     */
void set_rsc_path(val)				   /*			     */
  register char * val;				   /*			     */
{						   /*			     */
  rsc_v_rsc = val;				   /*			     */
  init___(&r_path,				   /*			     */
	  r_pattern,				   /*			     */
	  &rsc_v_rsc,				   /*			     */
	  rsc_e_rsc  );				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	see_rsc()
** Purpose:	Open a rsc file to read from.
** Arguments:
**	fname
** Returns:	
**___________________________________________________			     */
int see_rsc(fname)				   /*			     */
  register char * fname;			   /*			     */
{						   /*			     */
  if ( fname )					   /*			     */
  { init_parse();				   /*			     */
    InitLine;					   /*			     */
    file = px_fopen(fname,			   /*			     */
		    "r",			   /*			     */
		    r_pattern,			   /*			     */
		    r_path,			   /*			     */
		    see_bib_msg);		   /*			     */
    filename = px_filename;			   /*			     */
    return( file != NULL );			   /*			     */
  }						   /*			     */
  return FALSE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	parse_value()
** Purpose:	
**		
**
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
static int parse_value()			   /*			     */
{						   /*			     */
  sbrewind(parse_sb);				   /*			     */
  switch ( GetC )				   /*			     */
  { case EOF: EofError;		 return(FALSE);	   /*			     */
						   /*			     */
    case '"':					   /*			     */
      if ( !parse_string(FALSE) ) return(FALSE);   /*			     */
      push_string(symbol(sbflush(parse_sb)));	   /*			     */
      sbrewind(parse_sb);			   /*			     */
      break;					   /*			     */
						   /*			     */
    case '0': case '1': case '2': case '3': case '4':/*			     */
    case '5': case '6': case '7': case '8': case '9':/*			     */
      UnGetC; parse_number();			   /*			     */
      break;					   /*			     */
						   /*			     */
    case '{':					   /*			     */
      if ( !parse_block(FALSE) ) return(FALSE);	   /*			     */
      push_string(symbol(sbflush(parse_sb)));	   /*			     */
      sbrewind(parse_sb);			   /*			     */
      break;					   /*			     */
						   /*			     */
    default:					   /*			     */
      UnGetC; ExpectSymbol(TRUE,FALSE);		   /*			     */
  }						   /*			     */
						   /*			     */
  return(TRUE);					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	read_rsc()
** Purpose:	
**		
**
** Arguments:
**	name
** Returns:	
**___________________________________________________			     */
int read_rsc(name)				   /*			     */
  char	        *name;				   /*			     */
{ int	        c;				   /*			     */
  char	        *token;				   /*			     */
 						   /*                        */
  char		*s_filename;			   /*			     */
  FILE		*s_file;			   /*                        */
  uchar		*s_file_line_buffer;		   /*                        */
  size_t	s_fl_size;			   /*                        */
  int		s_flno;				   /*                        */
  uchar		*s_flp;				   /*                        */
 						   /*                        */
  s_filename	     = filename;		   /*			     */
  s_file	     = file;			   /*                        */
  s_file_line_buffer = file_line_buffer;	   /*                        */
  s_fl_size	     = fl_size;			   /*                        */
  s_flno	     = flno;			   /*                        */
  s_flp		     = flp;			   /*                        */
 						   /*                        */
  fl_size	     = 0;			   /*                        */
						   /*			     */
  init_parse();					   /*			     */
						   /*			     */
  if ( see_rsc(name) )				   /*                        */
  {						   /*                        */
    while ( (c=TestC) != EOF )			   /*			     */
    { switch (c)				   /*			     */
      { case '#': case '%': case ';':		   /*			     */
	  ClearLine; break;			   /*			     */
	default:				   /*			     */
	  if ( !parse_symbol(c) )		   /*			     */
	  { (void)seen(); return(1); }		   /*			     */
	  token = pop_string();			   /*			     */
	  if ( TestC == '=' ) (void)GetC;	   /* = is optional	     */
	  if ( !parse_value() )			   /*			     */
	  { SyntaxError; (void)seen(); return(-1); }/*			     */
	  (void)set_rsc(token,pop_string());	   /*			     */
	}					   /*			     */
    }						   /*			     */
    (void)seen();				   /*			     */
    c = 0;					   /*			     */
  }						   /*                        */
  else						   /*                        */
  { c = 1;		   			   /*			     */
  }						   /*                        */
  if ( fl_size > 0 ) 				   /*                        */
  { free((char*)file_line_buffer);		   /*                        */
  }						   /*                        */
  filename	   = s_filename;		   /*			     */
  file	     	   = s_file;			   /*                        */
  file_line_buffer = s_file_line_buffer;	   /*                        */
  fl_size	   = s_fl_size;			   /*                        */
  flno		   = s_flno;			   /*                        */
  flp		   = s_flp;			   /*                        */
 						   /*                        */
  return c;					   /*                        */
}						   /*------------------------*/
