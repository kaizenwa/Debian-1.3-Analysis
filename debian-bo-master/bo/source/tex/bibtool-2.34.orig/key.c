/******************************************************************************
** $Id: key.c,v 2.24 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "key.h"
#include "keynode.h"
#include "rsc.h"
#include "names.h"
#include "sbuffer.h"
#include "tex_read.h"
#include "wordlist.h"
#include "expand.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 KeyNode new_key_node _ARG((int type,char *string));/* key.c                 */
 char *get_field _ARG((Record rec,char *name));	   /* key.c                  */
 static char * itostr _ARG((int i,char *digits));  /* key.c                  */
 static int add_fmt_tree _ARG((char *s,KeyNode *treep));/* key.c             */
 static int deTeX _ARG((char *line,void (*save_fct)_ARG((char*)),int commap));/* key.c*/
 static int eval__fmt _ARG((KeyNode kn,Record rec));/* key.c                 */
 static int eval_fmt _ARG((KeyNode kn,Record rec));/* key.c                  */
 static int fmt__parse _ARG((char **sp,KeyNode *knp));/* key.c               */
 static int fmt_c_names _ARG((char *line,int min,int max,int not));/* key.c  */
 static int fmt_c_string _ARG((char * s,int min,int max,int not));/* key.c   */
 static int fmt_c_words _ARG((char *line,int min,int max,int not,int ignore));/* key.c*/
 static int fmt_parse _ARG((char **sp,KeyNode *knp));/* key.c                */
 static void eval__special _ARG((KeyNode kn,Record rec));/* key.c            */
 static void fmt_digits _ARG((char *s,int n));	   /* key.c                  */
 static void fmt_names _ARG((char *line,int maxname,int post,char *trans));/* key.c*/
 static void fmt_string _ARG((char * s,int n,char *trans));/* key.c          */
 static void fmt_title _ARG((char *line,int len,int in,char *trans,int ignore,char *sep));/* key.c*/
 static void key_init _ARG((void));		   /* key.c                  */
 static void push_S _ARG((unsigned char *s,unsigned char *trans));/* key.c   */
 static void push_s _ARG((unsigned char *s,int max,unsigned char *trans));/* key.c*/
 void Push_Word _ARG((char *s));		   /* key.c                  */
 void add_format _ARG((char *s));		   /* key.c                  */
 void add_ignored_word _ARG((char *s));		   /* key.c                  */
 void add_sort_format _ARG((char *s));		   /* key.c                  */
 void def_format_type _ARG((char *s));		   /* key.c                  */
 void free_key_node _ARG((KeyNode kn));		   /* key.c                  */
 void init_key _ARG((int state));		   /* key.c                  */
 void make_key _ARG((Record rec));		   /* key.c                  */
 void make_sort_key _ARG((Record rec));		   /* key.c                  */
 void mark_key _ARG((Record rec));		   /* key.c                  */
 void push_word _ARG((char * s));		   /* key.c                  */
 void set_base _ARG((char *s));			   /* key.c                  */
 void set_separator _ARG((int n,char *s));	   /* key.c                  */

#ifdef DEBUG
 static void show_fmt _ARG((KeyNode kn,int in));   /* key.c                  */
#endif

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

#define KEYSTYLE_EMPTY		0x0000
#define KEYSTYLE_SHORT		0x0010
#define KEYSTYLE_LONG		0x0020
#define KEYSTYLE_SHORT_NEED	0x0011
#define KEYSTYLE_LONG_NEED	0x0021
#define KEYSTYLE_EXTENDED	0x0030

#define KEYSTYLE_NEED_MASK	0x0001

#define DetexNone  0
#define DetexLower 1
#define DetexUpper 2

 static StringBuffer *key_sb = (StringBuffer*)NULL;
 static StringBuffer *tmp_sb = (StringBuffer*)NULL;

/*---------------------------------------------------------------------------*/

 static KeyNode key_tree      = (KeyNode)0;
 static KeyNode sort_key_tree = (KeyNode)0;


 static NameNode format[NUMBER_OF_FORMATS];


#define SkipSpaces(CP)	  while ( is_space(*CP) ) ++(CP)
#define SkipAllowed(CP)	  while ( is_allowed(*CP) ) ++(CP)
#define ParseNumber(CP,N) N = 0;	\
			  while(is_digit(*CP) ){ N = N*10 + (*CP)-'0'; ++CP; }
#define Expect(CP,C,RET)  SkipSpaces(CP);				\
			  if ( *CP == C ) { ++(CP); }			\
			  else { ErrPrintF("*** BibTool: Missing %c",C); \
				   return RET; }
#define MakeNode(KNP,TYPE,SP,CP) c = *CP; *CP = '\0';			\
			  *KNP = new_key_node(TYPE,symbol(*SP));	\
			  *CP  = c;
#define ParseOrReturn(CP,NODEP)						\
	if ( (ret=fmt_parse(CP,NODEP)) != 0  ) return ret


/*****************************************************************************/
/***				Private word stack			   ***/
/*****************************************************************************/

 char	**words	   = (char**)0;
 size_t words_len  = 0;
 size_t words_used = 0;
#define WordLenInc 16

#define PushWord(S)	if(words_len>words_used) words[words_used++]=S;	\
			else			 Push_Word(S)
#define ResetWords	words_used = 0

/*-----------------------------------------------------------------------------
** Function:	push_word()
** Purpose:	Push a word to the stack. Wrapper function.
** Arguments:
**	s	word to push
** Returns:	nothing
**___________________________________________________			     */
void push_word(s)				   /*			     */
  register char * s;				   /*			     */
{ PushWord(s);					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	Push_Word()
** Purpose:	Push a word to the stack when no space is left in the
**		allocated array. 
** Arguments:
**	sword to push
** Returns:	nothing
**___________________________________________________			     */
void Push_Word(s)				   /*			     */
  register char *s;				   /*			     */
{ register char **wp;				   /*			     */
						   /*			     */
  words_len += WordLenInc;			   /*			     */
  if ( words_len == WordLenInc )		   /*			     */
  { wp = (char**)malloc(words_len*sizeof(char*)); }/*			     */
  else						   /*			     */
  { wp = (char**)realloc(words,words_len*sizeof(char*)); }/*		     */
  if ( wp == (char**)0 )			   /*			     */
  { words_len -= WordLenInc;			   /*			     */
    ERROR("Push_Word() failed: Word not pushed."); /*		             */
    return;					   /*			     */
  }						   /*			     */
						   /*			     */
  words = wp;					   /*			     */
  PushWord(s);					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			Key Separator Section				   ***/
/*****************************************************************************/

#define NoSeps 8

 static char *key_seps[NoSeps] =
 { /* 0 */ "**key*",
   /* 1 */ "-",
   /* 2 */ ".",
   /* 3 */ ".",
   /* 4 */ ":",
   /* 5 */ "-",
   /* 6 */ "*",
   /* 7 */ ".ea"
 };

#define DefaultKey    key_seps[0]
#define InterNameSep  key_seps[1]
#define NamePreSep    key_seps[2]
#define NameNameSep   key_seps[3]
#define NameTitleSep  key_seps[4]
#define TitleTitleSep key_seps[5]
#define KeyNumberSep  key_seps[6]
#define EtAl	      key_seps[7]

/*-----------------------------------------------------------------------------
** Function:	set_separator()
** Purpose:	Modify the key_seps array.
** Arguments:
**	n	Array index
**	s	New value
** Returns:	nothing
**___________________________________________________			     */
void set_separator(n,s)				   /*			     */
  register int	n;				   /*			     */
  register char *s;				   /*			     */
{ register char *t, *tp;			   /*			     */
  						   /*                        */
  if ( n < 0 || n >= NoSeps )			   /*			     */
  { ERROR("Invalid separator specification.");     /*			     */
    return;					   /*			     */
  }						   /*			     */
 						   /*                        */
  t  = new_string(s);				   /*			     */
  for ( tp=t; *s ; s++ )		   	   /*			     */
  { if ( is_allowed(*s) ) *tp++ = *s; }	   	   /*			     */
  *tp = '\0';					   /*			     */
 						   /*                        */
  key_seps[n] = symbol(t);			   /*			     */
  free(t);					   /*                        */
}						   /*------------------------*/


/*****************************************************************************/
/***			   Key Base Section				   ***/
/*****************************************************************************/

 static char * key__base[] = 
 { "0123456789",
   "abcdefghijklmnopqrstuvwxyz",
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 };
#define KEY_BASE_DIGIT 0
#define KEY_BASE_LOWER 1
#define KEY_BASE_UPPER 2
 static int key_base = KEY_BASE_DIGIT;

/*-----------------------------------------------------------------------------
** Function:	set_base()
** Purpose:	Define the key base.
** Arguments:
**	s	String representation of the new value.
** Returns:	nothing
**___________________________________________________			     */
void set_base(s)				   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  if	  ( case_cmp(s,"upper") ) key_base = KEY_BASE_UPPER;/*		     */
  else if ( case_cmp(s,"lower") ) key_base = KEY_BASE_LOWER;/*		     */
  else if ( case_cmp(s,"digit") ) key_base = KEY_BASE_DIGIT;/*		     */
  else if ( is_upper(*s) )	  key_base = KEY_BASE_UPPER;/*		     */
  else if ( is_lower(*s) )	  key_base = KEY_BASE_LOWER;/*		     */
  else if ( is_digit(*s) )	  key_base = KEY_BASE_DIGIT;/*		     */
  else { ERROR("Unknown base ignored."); }	   /*			     */
}						   /*------------------------*/


#define ITOA_LEN 32
/*-----------------------------------------------------------------------------
** Function:	itostr()
** Purpose:	Translate number using the "digits" given.
**		A static string is returned containing the result.
**		This is a routine generalizing itoa().
** Examples:
**	itostr(X,"0123456789ABCDEF")	 returns the hexadecimal representation
**	itostr(X,"0123456789")		 returns the decimal representation
**	itostr(X,"01234567")		 returns the octal representation
**	itostr(X,"01")			 returns the binary representation
**
** Arguments:
**	i	Integer to translate
**	digits	String of digits to use
** Returns:	
**___________________________________________________			     */
static char * itostr(i,digits)			   /*			     */
  register int	 i;				   /*			     */
  register char	 *digits;			   /*			     */
{ static char	 buffer[ITOA_LEN];		   /* buffer to store result */
  register char	 *bp;				   /* buffer pointer	     */
  register int	 sign,				   /*			     */
		 base;				   /*			     */
						   /*			     */
  base	  = strlen(digits);			   /* how many digits?	     */
  bp	  = buffer+ITOA_LEN-1;			   /* set pointer to the end.*/
  *(bp--) = '\0';				   /* mark the end.	     */
  if ( i<0 ) { sign = -1; i = -i; }		   /*			     */
  else { sign = 0; if ( i == 0 ) *(bp--) = '0'; }  /*			     */
  while ( i > 0 )				   /*			     */
  { *(bp--) = digits[i%base];			   /*			     */
    i	    = i/base;				   /*			     */
  }						   /*			     */
  if ( sign ) *bp = '-'; else ++bp;		   /*			     */
  return(bp);					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			       Key Init Section				   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	init_key()
** Purpose:	Global initializations for the key module.
**		
**
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
void init_key(state)				   /*                        */
  int  state;
{ int  i;					   /*			     */
  char *s;
  						   /*                        */
  switch (state)
  { case 0:
      for (i=0;i<NUMBER_OF_FORMATS;i++)		   /*                        */
      { format[i] = NameNULL;			   /*                        */
      }						   /*                        */
      break;
    case 1:
      if ( format[0] == NameNULL )		   /*                        */
      { s = new_string("%l");
	format[0] = name_format(s);
	free(s);
	NameMid(format[0]) = InterNameSep;
      }
      
      if ( format[1] == NameNULL )		   /*                        */
      { s = new_string("%l%1f");
	format[1] = name_format(s);
	free(s);
	NameMid(format[1]) = InterNameSep;
      }
      if (NextName(format[1]))
      { NamePre(NextName(format[1])) = NamePreSep;		
      }
      break;
  }
}						   /*------------------------*/


/*-----------------------------------------------------------------------------
** Function:	key_init()
** Purpose:	Perform initializations for key generation.
**		The string buffer is opened.
**		Ignored words are initialized if necessary.
** Arguments:   none
** Returns:	nothing
**___________________________________________________			     */
static void key_init()				   /*			     */
{ char * s;					   /* Intermediate string ptr*/
#ifdef INITIALIZE_IGNORED_WORDS
  static char *word_list[] =			   /* default ignored words. */
  { INITIALIZE_IGNORED_WORDS, NULL };		   /* Mark the end with NULL */
  register char**wp;				   /*			     */
#endif
						   /*			     */
  if ( key_sb == (StringBuffer*)0 )		   /* Is it the first time?  */
  {						   /*                        */
    if ( (key_sb=sbopen()) == (StringBuffer*)0 )   /* open string buffer     */
    { OUT_OF_MEMORY("key generation."); }	   /*			     */
#ifdef INITIALIZE_IGNORED_WORDS
    for ( wp=word_list; *wp!=NULL; ++wp )	   /* add ignored words.     */
    { add_ignored_word(*wp); }			   /*			     */
#endif
 						   /*                        */
  }						   /*			     */
}						   /*------------------------*/



/*****************************************************************************/
/***			      Key DeTeX Section				   ***/
/*****************************************************************************/

#define DeTeX_BUFF_LEN 256


/*-----------------------------------------------------------------------------
** Function:	deTeX()
** Purpose:	Expand TeX seqeuences or eliminate them.
**
** Arguments:
**	line
**	save_fct
**	commap
** Returns:	
**___________________________________________________			     */
static int deTeX(line,save_fct,commap)		   /*			     */
  char		*line;				   /*			     */
  int		commap;				   /*			     */
  void		(*save_fct)_ARG((char*));	   /*			     */
{ static char	*buffer;			   /*			     */
  static size_t len   = 0;			   /*			     */
  char		c, *s, *bp;			   /*			     */
  char		last  = ' ';			   /*			     */
  int		wp    = 0;			   /*			     */
  int		brace;				   /*			     */
						   /*			     */
#define SaveWord     wp++; (*save_fct)(bp)
#define SaveChar(C)  *(bp++) = last = C
#define StoreChar(C) *(bp++) = C
						   /*			     */
  brace = 2*strlen(line);			   /*			     */
  if ( brace > len )				   /*			     */
  { if ( len == 0 )				   /*			     */
    { len    = ( brace < DeTeX_BUFF_LEN		   /*			     */
		? DeTeX_BUFF_LEN : brace );	   /*			     */
      buffer = malloc(len);			   /*			     */
    }						   /*			     */
    else					   /*			     */
    { len    = brace;				   /*			     */
      buffer = realloc(buffer,len);		   /*			     */
    }						   /*			     */
    if ( buffer == NULL ) 			   /*                        */
    { OUT_OF_MEMORY("deTeX()"); }   		   /*			     */
  }						   /*			     */
						   /*			     */
  TeX_open_string(line);			   /*			     */
  bp	= buffer;				   /*			     */
  brace = 0;					   /*			     */
  wp    = 0;					   /*                        */
  SaveWord;					   /*			     */
						   /*			     */
  while ( TeX_read(&c,&s) )			   /*			     */
  {						   /*			     */
    if (   is_alpha(c)				   /* Letters and	     */
	|| is_digit(c)				   /* digits and	     */
	|| is_extended(c)			   /* extended chars and     */
	|| ( c == '-' && last != c ) )		   /* nonrepeated hyphens    */
    { SaveChar(c); }				   /*			     */
    else if ( is_space(c) && !is_space(last) )	   /* Don't repeat spaces.   */
    { if ( brace == 0 )				   /* Remember the beginning */
      { StoreChar('\0'); SaveWord; last = ' '; }   /*  of the next word.     */
      else					   /*                        */
      { char *t;				   /*                        */
	for (t=InterNameSep; *t; ++t )		   /*                        */
	{ SaveChar(*t);	}			   /*                        */
      }		   				   /*			     */
    }						   /*			     */
    else if ( c == '{'	) { ++brace; }		   /* Count braces.	     */
    else if ( c == '}'	)			   /*                        */
    { if ( --brace<0 ) brace = 0;		   /*                        */
    }						   /*			     */
    else if ( c == ',' && commap )		   /* Commas are counted as  */
    { if ( bp != buffer && *(bp-1) != '\0' )	   /*                        */
      { StoreChar('\0');			   /*  single words upon     */
	SaveWord;				   /*  request, or ignored.  */
      }						   /*                        */
      StoreChar(',');				   /*			     */
      StoreChar('\0');				   /*			     */
      SaveWord;					   /*			     */
      last = ' ';				   /*			     */
    }						   /*			     */
  }						   /*			     */
 						   /*                        */
  if ( bp != buffer && *(bp-1) != '\0' )	   /*                        */
  { StoreChar('\0');				   /*                        */
    SaveWord;					   /*                        */
  }						   /*                        */
      						   /*                        */
  StoreChar('\0');				   /* Mark the end.	     */
						   /*			     */
  if ( bp-buffer >= len )			   /*			     */
  { ERROR_EXIT("deTeX buffer overflow."); }	   /*			     */
						   /*			     */
  TeX_close();					   /*			     */
  return wp-1;					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***		       Title Formatting Section				   ***/
/*****************************************************************************/

#define PushS(S)	push_s(S,0,trans)
#define PushStr(S,M)	push_s(S,M,trans)
#define PushC(C)	(void)sbputchar(trans[C],key_sb);

/*-----------------------------------------------------------------------------
** Function:	push_s()
** Purpose:	Write a translated string to the key string buffer.
** Arguments:
**	s	String to translate
**	trans	Translation table
** Returns:	nothing
**___________________________________________________			     */
static void push_S(s,trans)			   /*			     */
  register unsigned char *s;			   /*			     */
  register unsigned char *trans;		   /*			     */
{ while ( *s )					   /*			     */
  { (void)sbputchar(trans[*s],key_sb);		   /*			     */
    ++s;					   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	push_s()
** Purpose:	Write a translated string to the key string buffer.
** Arguments:
**	s	String to translate
**	max	Maximum number of characters
**	trans	Translation table
** Returns:	nothing
**___________________________________________________			     */
static void push_s(s,max,trans)			   /*			     */
  register unsigned char *s;			   /*			     */
  register int max;				   /*                        */
  register unsigned char *trans;		   /*			     */
{						   /*                        */
  if ( max <= 0 ) 				   /*                        */
  { while ( *s )				   /*			     */
    { (void)sbputchar(trans[*s],key_sb);	   /*			     */
      ++s;					   /*			     */
    }						   /*                        */
  }						   /*			     */
  else						   /*                        */
  { while ( *s && max-->0 )			   /*			     */
    { (void)sbputchar(trans[*s],key_sb);	   /*			     */
      ++s;					   /*			     */
    }						   /*                        */
  }						   /*                        */
}						   /*------------------------*/

 static WordList ignored_words[32] = 
 { WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL
 };

/*-----------------------------------------------------------------------------
** Function:	add_ignored_word()
** Purpose:	Add a new word to the list of ignored words for title
**		key generation.
**		The argument has to be saved by the caller!
** Arguments:
**	s	Word to add.
** Returns:	nothing
**___________________________________________________			     */
void add_ignored_word(s)			   /*			     */
  register char *s;				   /*			     */
{ save_word(s,&ignored_words[(*s)&31]);		   /*			     */
  DebugPrint2("Adding ignored word ",s);	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_title()
** Purpose:	Format a string according to rules for titles.
**		TeX sequences are expanded and some words may be ignored.
**		The result is pushed to the local string buffer.
** Arguments:
**	line	String to format.
**	len	Number of words to use
**	in	Number of characters per word to use
**	trans	Translation table
**	ignore	Boolean. Certain words are ignored if it is TRUE
**	sep	String separating the words.
** Returns:	nothing
**___________________________________________________			     */
static void fmt_title(line,len,in,trans,ignore,sep)/*			     */
  char	 *line;					   /*			     */
  int	 len;					   /*			     */
  int	 in;					   /*			     */
  char	 *trans;				   /* Translation table	     */
  int    ignore;				   /*                        */
  char   *sep;					   /*                        */
{ int	 nw, i, j;				   /*			     */
  char	 *s;					   /*			     */
  int	 first = TRUE;				   /*			     */
						   /*			     */
  if (	 tmp_sb == (StringBuffer*)0		   /*			     */
      && (tmp_sb=sbopen()) == (StringBuffer*)0 )   /*			     */
  { OUT_OF_MEMORY("fmt_title()"); } 		   /*			     */
						   /*			     */
  ResetWords;					   /*                        */
  nw = deTeX(*line=='{'?line+1:line,push_word,FALSE);/*			     */
 						   /*                        */
  for ( i=0; i < nw; ++i )		   	   /*			     */
  {						   /*			     */
    sbrewind(tmp_sb);				   /* Reset the string buffer*/
    for ( s=words[i]; *s; ++s )		   	   /* Translate the current  */
    { (void)sbputchar(trans[*s],tmp_sb); }	   /*  word into the sbuffer */
    s = sbflush(tmp_sb);			   /* Get the translated word*/
						   /*			     */
    if ( ! ignore || 				   /*                        */
	 ! find_word(s,ignored_words[(*s)&31] ) )  /*			     */
    { if ( first ) { first = FALSE; }		   /*			     */
      else { PushS(sep); }		   	   /*			     */
      if ( in <= 0 )				   /*                        */
      { PushS(words[i]); }		   	   /* Push the current word  */
      else					   /*                        */
      { for ( s=words[i], j=in; *s && j-->0; ++s ) /* Push the initial part  */
	{ PushC(*s); }	   			   /*  of the current word.  */
      }						   /*                        */
      if ( len == 1 ) return;	   		   /*                        */
      if ( len > 0  ) len--;			   /*                        */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_c_words()
** Purpose:	Count a list of words.
**		
**
** Arguments:
**	line	string to analyze
**	min	minimum
**	max	maximum or 0
**	not	negation flag
**	ignore	flag to indicate if certain words should be ignored.
** Returns:	
**___________________________________________________			     */
static int fmt_c_words(line,min,max,not,ignore)	   /*			     */
  char	 *line;					   /*			     */
  int	 min;					   /*			     */
  int	 max;					   /*			     */
  int	 not;					   /*			     */
  int	 ignore;				   /*			     */
{ int	 n, i, nw;				   /*			     */
						   /*			     */
  ResetWords;					   /*                        */
  nw = deTeX(*line=='{'?line+1:line,push_word,FALSE);/*			     */
  n = 0;					   /*                        */
 						   /*                        */
  for ( i=0; i < nw; ++i )		   	   /*			     */
  {						   /*			     */
    if ( !ignore || 				   /*                        */
	 !find_word(words[i],ignored_words[(*words[i])&31]) )/*		     */
    { n++; }					   /*			     */
  }						   /*			     */
 						   /*                        */
  if ( n < min || (max>0 && n> max))		   /*                        */
  { return (not?FALSE:TRUE); }			   /*                        */
 						   /*                        */
  return (not?TRUE:FALSE);			   /*                        */
}						   /*------------------------*/


/*****************************************************************************/
/***		         Name Formatting Section			   ***/
/*****************************************************************************/


/*-----------------------------------------------------------------------------
** Function:	def_format_type()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void def_format_type(s)				   /*                        */
  char *s;					   /*                        */
{ int  n;					   /*                        */
  char *cp;					   /*                        */
  char c;					   /*                        */
 						   /*                        */
  SkipSpaces(s);				   /*                        */
  n = 0;					   /*                        */
  while ( is_digit(*s) ) { n = n*10 + (*s++) - '0'; }/*                      */
  if ( n >= NUMBER_OF_FORMATS )			   /*                        */
  { WARNING("Format type number is out of range.");/*                        */
    return;					   /*                        */
  }						   /*                        */
  if ( n == 0 )			   		   /*                        */
  { VerbosePrint1("Name format specifier 0 has been changed.\n");/*          */
  }						   /*                        */
  else if ( n == 1 )				   /*                        */
  { VerbosePrint1("Name format specifier 1 has been changed.\n");/*          */
  }						   /*                        */
 						   /*                        */
  SkipSpaces(s);				   /*                        */
  if ( *s == '=' ) s++;				   /*                        */
  Expect(s,'"',);				   /*                        */
  cp = s;					   /*                        */
  while ( *cp && *cp != '"' ) cp++;		   /*                        */
  c   = *cp;					   /*                        */
  *cp = '\0';					   /*                        */
  format[n] = name_format(s);			   /*                        */
  *cp = c;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_names()
** Purpose:	Format a list of names separated by 'and'.
**		'and others' is handled properly.
** Arguments:
**	line	String to format.
**	maxname	Number of names to consider
**	maxlen  Number of characters per name to consider
**	trans	Translation table
** Returns:	nothing
**___________________________________________________			     */
static void fmt_names(line,maxname,post,trans)	   /*		             */
  char	      *line;				   /* Name list string	     */
  int	      maxname;				   /* number of names b4 etal*/
  int         post;				   /* number of relevant char*/
  char	      *trans;				   /* Translation table	     */
{ int	      wp,				   /*			     */
	      i;				   /*			     */
  char	      *s;				   /*                        */
  static char *and   = "&";			   /*                        */
  static char *comma = ",";			   /*                        */
  static int  undef_warning = FALSE;		   /*                        */
  						   /*                        */
  if (   post < 0				   /*                        */
      || post >= NUMBER_OF_FORMATS		   /*                        */
      || format[post] == NameNULL )		   /*                        */
  { if (undef_warning)				   /*                        */
    { ErrPrintF("*** BibTool: Format %d is not defined. Ignored.\n",/*       */
		post);				   /*                        */
      undef_warning = TRUE;			   /*                        */
    }						   /*                        */
    return;					   /*                        */
  }						   /*                        */
 						   /*                        */
  ResetWords;					   /*                        */
  wp = deTeX(*line=='{'?line+1:line,push_word,TRUE);/*			     */
  words[wp] = NULL;				   /*                        */
 						   /*                        */
  for ( i=0; i<wp; i++)			   	   /*			     */
  { if ( strcmp(words[i],"and") == 0 )		   /*                        */
    { words[i] = and; }				   /*                        */
    else if ( strcmp(words[i],",") == 0 )	   /*                        */
    { words[i] = comma; }			   /*                        */
  }						   /*                        */
 						   /*                        */
  PushS(pp_list_of_names(words,		   	   /*                        */
			 format[post],		   /*                        */
			 trans,			   /*                        */
			 maxname,		   /*                        */
			 comma,		   	   /*                        */
			 and,		   	   /*                        */
			 NameNameSep,EtAl));	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_c_names()
** Purpose:	Count a list of names
**		
**
** Arguments:
**	line	string to analyze
**	min	minimum
**	max	maximum or 0
**	not	negation flag
** Returns:	nothing
**___________________________________________________			     */
static int fmt_c_names(line,min,max,not)	   /*		             */
  char	      *line;				   /* Name list string	     */
  int         min;				   /* number of relevant char*/
  int	      max;				   /* number of names b4 etal*/
  int         not;				   /* negation flag          */
{ int	      wp,				   /*			     */
	      i,				   /*                        */
	      n;				   /*			     */
  						   /*                        */
  ResetWords;					   /*                        */
  wp = deTeX(*line=='{'?line+1:line,push_word,TRUE);/*			     */
  words[wp] = NULL;				   /*                        */
 						   /*                        */
  for ( i=0, n=1; i<wp; i++)			   /*			     */
  { if ( strcmp(words[i],"and") == 0 ) { n++; }	   /*                        */
  }						   /*                        */
 						   /*                        */
  if ( n < min || (max>0 && n> max))		   /*                        */
  { return (not?FALSE:TRUE); }			   /*                        */
 						   /*                        */
  return (not?TRUE:FALSE);			   /*                        */
}						   /*------------------------*/


/*****************************************************************************/
/***		         Number Formatting Section			   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	fmt_digits()
** Purpose:	Search a sequence of digits and push at most n of them 
**		counting from right to the string buffer.
** Example:	fmt_digits("jun 1958",2) pushes "58" to the string buffer.
** Arguments:
**	s	String to format
**	n	Length
** Returns:	nothing
**___________________________________________________			     */
static void fmt_digits(s,n)			   /*			     */
  register char *s;				   /*			     */
  register int	n;				   /*			     */
{ register char *cp;				   /*			     */
						   /*			     */
  while( *s && !is_digit(*s) )	{ ++s; }	   /* search first digit     */
  cp = s;					   /*			     */
  while( *cp && is_digit(*cp) )	{ ++cp; }	   /* skip over digits	     */
  if ( cp-s > n ) s = cp-n;			   /*			     */
  while ( s != cp )				   /*			     */
  { (void)sbputchar(*s,key_sb); ++s; }		   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***		         String Formatting Section			   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	fmt_string()
** Purpose:	Push characters from s onto the string buffer.
**		At most n characters are transfered.
**		Letters are translated accoring to mode.
** Arguments:
**	s	String to format
**	n	Length
**	trans	Translation table
** Returns:	nothing
**___________________________________________________			     */
static void fmt_string(s,n,trans)		   /*			     */
  register char * s;				   /*			     */
  register int	n;				   /*			     */
  register char *trans;				   /*			     */
{						   /*			     */
  while ( *s && n>0 )				   /*			     */
  { if ( is_allowed(*s) )			   /*			     */
    { (void)sbputchar(trans[*s],key_sb); n--; }	   /*			     */
    else if ( is_space(*s) )			   /*			     */
    { (void)sbputs(TitleTitleSep,key_sb); n--;	   /*                        */
      while ( is_space(*s) ) s++;		   /* skip over multiple SPC */
    }   					   /*			     */
    ++s;					   /*			     */
  }						   /*			     */
						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_c_string()
** Purpose:	Count the number of characters in a string and compare it
**		against an interval.
**
** Arguments:
**	s	string to analyze
**	min	minimum
**	max	maximum or 0
**	not	negation flag
** Returns:	
**___________________________________________________			     */
static int fmt_c_string(s,min,max,not)		   /*			     */
  register char * s;				   /*			     */
  register int  min;				   /*			     */
  register int  max;				   /*			     */
  register int  not;				   /*			     */
{ int           n = 0;				   /*			     */
 						   /*                        */
  while ( *s )				   	   /*			     */
  { if ( is_allowed(*s) ) { n++; }	   	   /*			     */
    else if ( is_space(*s) )			   /*			     */
    { n += strlen(TitleTitleSep);	   	   /*                        */
      while ( is_space(*s) ) s++;		   /* skip over multiple SPC */
    }   					   /*			     */
    ++s;					   /*			     */
  }						   /*			     */
 						   /*                        */
  if ( n < min || (max>0 && n> max))		   /*                        */
  { return (not?FALSE:TRUE); }			   /*                        */
 						   /*                        */
  return (not?TRUE:FALSE);			   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	get_field()
** Purpose:	Evaluate the record rec.
**		If name starts with '@' then check the record name.
**		If name starts with '$' then return the special info. 
**		Else search in Record rec for the field name and return it's
**		value. NULL is returned to indicate failure.
** Arguments:
**	rec	Record to analyze
**	name	Field name to search for
** Returns:	The address of the value or NULL
**___________________________________________________			     */
char *get_field(rec,name)		   	   /*			     */
  register Record rec;				   /*			     */
  register char	  *name;			   /*			     */
{ static char     *crossref = NULL;		   /*			     */
 						   /*                        */
  if ( *name == '@' )				   /*			     */
  { if ( case_cmp(name+1,EntryName(RecordToken(rec))) )/*		     */
      return EntryName(RecordToken(rec));	   /*		             */
  }						   /*			     */
  else if ( *name == '$' )			   /*			     */
  { ++name;					   /*			     */
    if ( case_cmp(name,"key") )			   /*			     */
    { return (**RecordHeap(rec)?*RecordHeap(rec):NULL); }/*		     */
    else if ( case_cmp(name,"type") )		   /*			     */
    { return EntryName(RecordToken(rec)); }	   /*			     */
    else if ( case_cmp(name,"default.key") )	   /*			     */
    { return DefaultKey; }			   /*			     */
    else if ( case_cmp(name,"fmt.et.al") )	   /*			     */
    { return DefaultKey; }			   /*			     */
    else if ( case_cmp(name,"fmt.name.pre") )	   /*			     */
    { return NamePreSep; }			   /*			     */
    else if ( case_cmp(name,"fmt.inter.name") )	   /*			     */
    { return InterNameSep; }			   /*			     */
    else if ( case_cmp(name,"fmt.name.name") )	   /*			     */
    { return NameNameSep; }			   /*			     */
    else if ( case_cmp(name,"fmt.name.title") )	   /*			     */
    { return NameTitleSep; }			   /*			     */
    else if ( case_cmp(name,"fmt.title.title") )   /*			     */
    { return TitleTitleSep; }			   /*			     */
  }						   /*			     */
  else						   /*			     */
  { register char **cpp;			   /*			     */
    register int  n;				   /*			     */
						   /*			     */
    for ( cpp=RecordHeap(rec), n=RecordFree(rec);  /*			     */
	  n > 0;				   /*			     */
	  n -= 2 )				   /*			     */
    { if ( name == *cpp )			   /*                        */
      { return ( rsc_key_expand_macros 		   /*                        */
		? expand_rhs(*(cpp+1),0)	   /*                        */
		: *(cpp+1) );	   		   /*			     */
      }						   /*                        */
      else { cpp++; cpp++; }			   /*			     */
    }						   /*			     */
 
    if ( crossref==NULL ) crossref = symbol("crossref");
 
    for ( cpp=RecordHeap(rec), n=RecordFree(rec);  /*			     */
	  n > 0 && crossref != *cpp;		   /*			     */
	  n -= 2 )				   /*			     */
    { cpp++; cpp++; }			   /*			     */

    if ( n > 0 && crossref == *cpp )
    { cpp++;
      printf("I should try to consult entry %s\nBut this isn't implemented yet",*cpp);
    }
  }						   /*			     */
 						   /*                        */
  return (char*)0;				   /*			     */
}						   /*------------------------*/

#define IfGetField(S,NAME) if((S=get_field(rec,NAME))!=NULL)

#define GetEntryOrReturn(S,NAME)					\
	if((S=get_field(rec,NAME))==NULL) return(FALSE)


/*-----------------------------------------------------------------------------
** Function:	make_key()
** Purpose:	Generate a key for a given record.
** Arguments:
**	rec	Record to consider
** Returns:	nothing
**___________________________________________________			     */
void make_key(rec)				   /*			     */
  register Record rec;				   /*			     */
{ register char	  *kp,				   /*			     */
		  **cpp;			   /*			     */
  int		  n,				   /*			     */
		  pos;				   /*			     */
						   /*			     */
  if ( IsSpecialRecord(RecordToken(rec)) ) return; /*			     */
						   /*			     */
  if ( key_tree == (KeyNode)0 )			   /*			     */
  { *RecordHeap(rec) = "";			   /* store an empty key     */
    return;					   /*			     */
  }						   /*			     */
						   /*			     */
  if (	 NodeType(key_tree) == NodeSPECIAL	   /*			     */
      && (NodePre(key_tree)&KEYSTYLE_NEED_MASK)	   /*			     */
      && **RecordHeap(rec) != '\0'	)	   /*			     */
  { return; }					   /*			     */
						   /*			     */
  key_init();					   /*			     */
  sbrewind(key_sb);				   /* clear key		     */
						   /*			     */
  (void)eval_fmt(key_tree,rec);			   /*			     */
						   /*			     */
  pos		    = sbtell(key_sb);		   /*			     */
  kp		    = sbflush(key_sb);		   /* get collected key	     */
  cpp		    = RecordHeap(rec);		   /* get destination for key*/
  RecordOldKey(rec) = *cpp;			   /* save old key	     */
  *cpp		    = symbol(kp);		   /* store new key	     */
						   /*			     */
  if ( sym_flag() != 0 )			   /* is key already used?   */
  {						   /* Then disambiguate:     */
    (void)sbseek(key_sb,pos);			   /*			     */
    (void)sbputs(KeyNumberSep,key_sb);		   /* put separator at end   */
    n	= 1;					   /* start with no 1	     */
    pos = sbtell(key_sb);			   /*			     */
    do						   /* last symbol was present*/
    { (void)sbseek(key_sb,pos);			   /*			     */
      (void)sbputs(itostr(n++,key__base[key_base]), /*			     */
		   key_sb);			   /*			     */
      *cpp = symbol(sbflush(key_sb));		   /*	store new key	     */
    } while ( sym_flag() != 0 );		   /*			     */
  }						   /*			     */
  sym_set_flag(1);				   /* Mark the symbol as key */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	mark_key()
** Purpose:	Set the key mark for the key symbol of a record.
** Arguments:
**	rec
** Returns:	nothing
**___________________________________________________			     */
void mark_key(rec)				   /*			     */
  register Record rec;				   /*			     */
{						   /*			     */
  if ( IsSpecialRecord(RecordToken(rec)) ) return; /*			     */
 						   /*                        */
  if ( *RecordHeap(rec) == NULL ) { return; }	   /*			     */
  (void)sym_add(*RecordHeap(rec),0);		   /*			     */
  sym_set_flag(1);				   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	make_sort_key()
** Purpose:	
**
** Arguments:
**	rec
** Returns:	nothing
**___________________________________________________			     */
void make_sort_key(rec)				   /*			     */
  register Record rec;				   /*			     */
{ register char	  *kp;				   /*			     */
						   /*			     */
  if ( IsSpecialRecord(RecordToken(rec)) ) return; /*			     */
						   /*			     */
  key_init();					   /*			     */
  sbrewind(key_sb);				   /* clear key		     */
						   /*			     */
  if (	 sort_key_tree != (KeyNode)0		   /*			     */
      && eval_fmt(sort_key_tree,rec) == 0 )	   /*			     */
  { kp		   = sbflush(key_sb);		   /* get collected key	     */
    RecordKey(rec) = symbol(kp);		   /* store new key	     */
  }						   /*			     */
  else						   /* If everything fails    */
  { RecordKey(rec) = RecordHeap(rec)[0];	   /* use the reference key  */
  }						   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***		      Key Format Parsing and Evaluating			   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	new_key_node()
** Purpose:	Allocate a new key node
** Arguments:
**	type	
**	string	
** Returns:	The address of the new node.
**		Upon failure exit() is called.
**___________________________________________________			     */
KeyNode new_key_node(type,string)		   /*			     */
  int	  type;					   /*			     */
  char	  *string;				   /*			     */
{ KeyNode new;					   /*			     */
  if( (new=(KeyNode)malloc(sizeof(SKeyNode))) == (KeyNode)0 )/*		     */
  { OUT_OF_MEMORY(" new_key_node()"); } 	   /*			     */
						   /*			     */
  NodeType(new)	  = type;			   /*			     */
  NodeSymbol(new) = string;			   /*			     */
  NodeNext(new)	  = (KeyNode)0;			   /*			     */
  NodeThen(new)	  = (KeyNode)0;			   /*			     */
  NodeElse(new)	  = (KeyNode)0;			   /*			     */
  return new;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	free_key_node()
** Purpose:	Here KeyNodes should be freed.
**		Well, in a future release ...
**
** Arguments:
**	kn	KeyNode to be freed.
** Returns:	nothing
**___________________________________________________			     */
void free_key_node(kn)				   /*			     */
  KeyNode kn;					   /*			     */
{						   /*			     */
#ifdef FUTURE_RELEASE
  if ( kn != (KeyNode)0 )			   /*                        */
  {						   /*                        */
    free(kn);					   /*                        */
  }						   /*                        */
#endif
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	add_fmt_tree()
** Purpose:	Extend the format tree
**
** Arguments:
**	s
**	treep
** Returns:	
**___________________________________________________			     */
static int add_fmt_tree(s,treep)		   /*			     */
  char	  *s;					   /*			     */
  KeyNode *treep;				   /*			     */
{ KeyNode kn, kn_or;				   /*			     */
  int	  special   = 0;			   /*			     */
  unsigned char *s0 = (unsigned char*)s;	   /*			     */
						   /*			     */
  if ( s == (char*)NULL ) return FALSE;		   /*			     */
						   /*			     */
  if ( strcmp(s,"empty") == 0 )			   /*			     */
  { free_key_node(*treep);			   /*			     */
    *treep  = (KeyNode)0;			   /*			     */
  }						   /*			     */
  if	  ( strcmp(s,"short"	) == 0 ) { special = KEYSTYLE_SHORT;	  }/**/
  else if ( strcmp(s,"long"	) == 0 ) { special = KEYSTYLE_LONG;	  }/**/
  else if ( strcmp(s,"new.short") == 0 ) { special = KEYSTYLE_SHORT_NEED; }/**/
  else if ( strcmp(s,"new.long" ) == 0 ) { special = KEYSTYLE_LONG_NEED;  }/**/
						   /*			     */
  if ( special )				   /*			     */
  { free_key_node(*treep);			   /*			     */
    *treep = new_key_node(NodeSPECIAL,(char*)NULL);/*			     */
    NodePre(*treep) = special;			   /*			     */
    return TRUE;				   /*			     */
  }						   /*			     */
						   /*			     */
  if ( fmt_parse(&s,&kn) > 0 )			   /*			     */
  { error(ERR_POINT|ERR_WARN,			   /*			     */
	  "Format Error. Format ignored.",	   /*			     */
	  (char*)0,(char*)0,s0,(unsigned char*)s,0,empty);/*		     */
    return FALSE;				   /*			     */
  }						   /*			     */
  if ( *treep == (KeyNode)0 ) { *treep	= kn; }	   /*			     */
  else if ( NodeType(*treep) == NodeSPECIAL )	   /*			     */
  { free_key_node(*treep);			   /*			     */
    *treep  = kn;				   /*			     */
  }						   /*			     */
  else						   /*			     */
  { kn_or = new_key_node(NodeOR,(char*)NULL);	   /*			     */
    NodeThen(kn_or) = *treep;			   /*			     */
    NodeElse(kn_or) = kn;			   /*			     */
    *treep	    = kn_or;			   /*			     */
  }						   /*			     */
						   /*			     */
#ifdef DEBUG
  show_fmt(*treep,0);			   	   /*			     */
#endif
  return TRUE;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	add_format()
** Purpose:	Add a key format specification to the current specification.
**
** Arguments:
**	s	Specification string
** Returns:	nothing
**___________________________________________________			     */
void add_format(s)				   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  if ( s == NULL )				   /*			     */
  { WARNING("Missing key format.");		   /*			     */
    return;					   /*			     */
  }						   /*			     */
  rsc_make_key = TRUE;				   /*			     */
  (void)add_fmt_tree(s,&key_tree);		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	add_sort_format()
** Purpose:	Add a sort format specification to the current specification.
**
** Arguments:
**	s	Specification string
** Returns:	nothing
**___________________________________________________			     */
void add_sort_format(s)				   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  (void)add_fmt_tree(s,&sort_key_tree);		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt_parse()
** Purpose:	Parse the format specification given in the string *sp.
**		Store the resulting KeyNode tree in *knp.
**		*sp is modified to point to the first character not used.
**		The return value is 0 iff no problem has been detected.
** Arguments:
**	sp
**	knp
** Returns:	Error code. 0 = success
**___________________________________________________			     */
static int fmt_parse(sp,knp)			   /*			     */
  char		**sp;				   /*			     */
  KeyNode	*knp;				   /*			     */
{ int		ret;				   /*			     */
  KeyNode	new;				   /*			     */
						   /*			     */
  while ( (ret=fmt__parse(sp,knp)) == 0 )	   /*			     */
  { SkipSpaces(*sp);				   /*			     */
    if ( **sp == '#' )				   /*			     */
    { new  = new_key_node(NodeOR,(char*)NULL);	   /*			     */
      NodeThen(new) = *knp;			   /*			     */
      *knp = new;				   /*			     */
      knp  = &NodeElse(new);			   /*			     */
      (*sp)++;					   /*			     */
    }						   /*			     */
    else return 0;				   /*			     */
  }						   /*			     */
  return ret;					   /* return the error code. */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	fmt__parse()
** Purpose:	
**
** Arguments:
**	sp
**	knp
** Returns:	Error code or 0 upon success
**___________________________________________________			     */
static int fmt__parse(sp,knp)			   /*			     */
  char		**sp;				   /*			     */
  KeyNode	*knp;				   /*			     */
{ register char *cp;				   /*			     */
  char		c;				   /*			     */
  int		ret;				   /*			     */
  static char	*percent_chars = "NntTdsSwWp";     /*                        */
						   /*			     */
  cp = *sp;					   /*			     */
						   /*			     */
  while ( *cp != '\0' )				   /*			     */
  {						   /*			     */
    switch ( *cp )				   /*			     */
    { case ' ' : case '\t': case '\r':		   /* Skip over spaces.	     */
      case '\n': case '\f': case '\b':		   /*			     */
	cp++; break;				   /*			     */
      case '#':					   /* Two cases evaluated    */
      case '}': return 0;			   /*  somewhere else.	     */
      case '%':					   /* Format specification:  */
	{ int  pre  = -1,			   /*			     */
	       post = -1,			   /* %[-][0-9]*[.[0-9]*[a-Z]*/
	       type = 0;			   /*	([a-Z]*)	     */
	  switch ( *++cp )			   /*			     */
	  { case '+': type = NodePlusMask;  ++cp; break;/*		     */
	    case '-': type = NodeMinusMask; ++cp; break;/*		     */
	  }					   /*			     */
	  ParseNumber(cp,pre);			   /*			     */
	  if ( *cp == '.' || *cp == ',' || *cp == '-' )/*		     */
	  { cp++;				   /*                        */
	    ParseNumber(cp,post);		   /*                        */
	  }	   				   /*			     */
	  if ( *cp == '#' )			   /*                        */
	  { cp++;				   /*                        */
	    type |= NodeCountMask;		   /*                        */
	  }					   /*                        */
	  if ( !is_alpha(*cp) )			   /*			     */
	  { ErrPrintF("*** BibTool: Missing format type before: %s",cp);/*   */
	    *sp = cp; return(1);		   /*			     */
	  }					   /*			     */
	  if ( strchr(percent_chars,*cp) == (char*)0 )/*		     */
	  { ErrPrintF("*** BibTool: Illegal format character: %c",*cp);/*    */
	    *sp = cp; return 1;			   /*			     */
	  }					   /*			     */
	  type |= *(cp++);			   /*			     */
	  Expect(cp,'(',1);			   /*			     */
	  SkipSpaces(cp);			   /*			     */
	  *sp = cp;				   /*			     */
	  SkipAllowed(cp);			   /*			     */
	  MakeNode(knp,type,sp,cp)		   /*			     */
	  NodePre(*knp)	 = pre;			   /*			     */
	  NodePost(*knp) = post;		   /*			     */
	  SkipSpaces(cp);			   /*			     */
	  Expect(cp,')',1);			   /*			     */
	  *sp = cp;				   /*			     */
	  knp = & NodeNext(*knp);		   /*			     */
	}					   /*			     */
	break;					   /*			     */
      case '(':					   /*			     */
	cp = (*sp)+1;				   /*			     */
	SkipSpaces(cp);				   /*			     */
	*sp = cp;				   /*			     */
	SkipAllowed(cp);			   /*			     */
	MakeNode(knp,NodeTEST,sp,cp)		   /*			     */
	Expect(cp,')',1);			   /*			     */
	Expect(cp,'{',1);			   /*			     */
	*sp = cp;				   /*			     */
	ParseOrReturn(sp,&NodeThen(*knp));	   /*			     */
	Expect(*sp,'}',1);			   /*			     */
	Expect(*sp,'{',1);			   /*			     */
	ParseOrReturn(sp,&NodeElse(*knp));	   /*			     */
	Expect(*sp,'}',1);			   /*			     */
	knp = & NodeNext(*knp);			   /*			     */
	cp = *sp;				   /*			     */
	break;					   /*			     */
						   /*			     */
      case '{':					   /*			     */
	(*sp)++;				   /*			     */
	ParseOrReturn(sp,knp);			   /*			     */
	Expect(*sp,'}',1);			   /*			     */
	knp = & NodeNext(*knp);			   /*			     */
	cp = *sp;				   /*			     */
	break;					   /*			     */
      default:					   /*			     */
	*sp = cp;				   /*			     */
	if ( is_allowed(*cp) )			   /*			     */
	{					   /*			     */
	  while ( is_allowed(*cp) ) { ++cp; }	   /*			     */
	  MakeNode(knp,NodeSTRING,sp,cp);	   /*			     */
	  *sp = cp;				   /*			     */
	  knp = & NodeNext(*knp);		   /*			     */
	}					   /*			     */
	else					   /*			     */
	{ ERROR2("Parse format. Illegal character ignored: ",cp);/*          */
	  ++cp;					   /*			     */
	}					   /*			     */
    }						   /*			     */
  }						   /*			     */
  return -1;					   /* Signal end-of-string.  */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	eval_fmt()
** Purpose:	Evaluate the given KeyNode tree w.r.t. the given Record
**
** Arguments:
**	kn
**	rec
** Returns:	
**___________________________________________________			     */
static int eval_fmt(kn,rec)			   /*			     */
  register KeyNode kn;				   /*			     */
  register Record  rec;				   /*			     */
{ int		   pos = sbtell(key_sb);	   /*			     */
						   /*			     */
  if (	 kn != (KeyNode)0			   /*			     */
      && eval__fmt(kn,rec) != 0 )		   /*			     */
  { (void)sbseek(key_sb,pos); return 1; }	   /*			     */
						   /*			     */
  return 0;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	eval__fmt()
** Purpose:	Evaluate the given KeyNode tree w.r.t. the given Record
**		Internal version. trash of conjunctions is not removed.
** Arguments:
**	kn
**	rec
** Returns:	
**___________________________________________________			     */
static int eval__fmt(kn,rec)			   /*			     */
  register KeyNode kn;				   /*			     */
  register Record rec;				   /*			     */
{ char	   *s;					   /*			     */
  int	   pos;					   /*			     */
  char     *trans;				   /*                        */
						   /*			     */
  while ( kn != (KeyNode)0 )			   /*			     */
  { switch ( NodeType(kn) )			   /*			     */
    { case NodeSTRING:				   /*			     */
	DebugPrint3("STRING \"",NodeSymbol(kn),"\"");/*		             */
	(void)sbputs(NodeSymbol(kn),key_sb);	   /*			     */
	break;					   /*			     */
      case NodeTEST:				   /*			     */
#ifdef DEBUG
	DebugPrint3("IF_THEN_ELSE (",NodeSymbol(kn),")");/*		     */
	IfGetField(s,NodeSymbol(kn))		   /*			     */
	  DebugPrint1("Field found. Continuing with THEN part");
	else DebugPrint1("Field NOT found. Continuing with ELSE part");
#endif
	IfGetField(s,NodeSymbol(kn))		   /*			     */
	{ (void)eval__fmt(NodeThen(kn),rec); }	   /*			     */
	else					   /*			     */
	{ (void)eval__fmt(NodeElse(kn),rec); }	   /*			     */
	break;					   /*			     */
      case NodeOR:				   /*			     */
	DebugPrint1("OR Trying THEN part");   	   /*			     */
	pos = sbtell(key_sb);			   /*			     */
	if ( eval__fmt(NodeThen(kn),rec) != 0 )	   /*			     */
	{ (void)sbseek(key_sb,pos);		   /*			     */
	  DebugPrint1("OR THEN part failed. Trying ELSE part.");/*	     */
	  if ( eval__fmt(NodeElse(kn),rec) != 0 )  /*			     */
	  { (void)sbseek(key_sb,pos);		   /*			     */
	    DebugPrint1("OR ELSE part failed");    /*			     */
	    return 1;				   /*			     */
	  }					   /*			     */
	  DebugPrint1("OR ELSE part succeeded");   /*			     */
	}					   /*			     */
	break;					   /*			     */
      case NodeSPECIAL:				   /*			     */
	eval__special(kn,rec);			   /*			     */
	break;					   /*			     */
      default:					   /*			     */
#ifdef DEBUG
	fprintf(err_file,"+++ BibTool: FORMAT %d.%d %c (%s)",/*	             */
		NodePre(kn),			   /*			     */
		NodePost(kn),			   /*			     */
		NodeType(kn)&0xff,		   /*			     */
		NodeSymbol(kn));		   /*			     */
#endif
	IfGetField(s,NodeSymbol(kn))		   /*			     */
	{					   /*			     */
	  DebugPrint1("Field found");	   	   /*			     */
 						   /*                        */
	  if (NodeType(kn)&NodeMinusMask) 	   /*                        */
	  { trans = trans_lower; }		   /*                        */
	  else if (NodeType(kn)&NodePlusMask) 	   /*                        */
	  { trans = trans_upper; }		   /*                        */
	  else					   /*                        */
	  { trans = trans_id; }			   /*                        */
#define UsePostOr(X) (NodePost(kn) > 0 ? NodePost(kn) : (X))
#define UsePreOr(X)  (NodePre(kn)  > 0 ? NodePre(kn)  : (X))
#define HasMinus     (NodeType(kn)&NodeMinusMask)
						   /*			     */
	  switch ( NodeType(kn)	& 0x1ff )	   /*			     */
	  {					   /*                        */
	    case 'p':				   /*			     */
	      fmt_names(s,UsePreOr(2),UsePostOr(0),trans);/*                 */
	      break;				   /*			     */
	    case 'n':				   /*			     */
	      NameStrip(format[0]) = UsePostOr(-1);/*                        */
	      fmt_names(s,UsePreOr(2),0,trans);    /*                        */
	      break;				   /*			     */
	    case 'N':				   /*			     */
	      NameStrip(format[1]) = UsePostOr(-1);/*                        */
	      if (NextName(format[1]))		   /*                        */
	      { NamePre(NextName(format[1])) = NamePreSep;/*                 */
	      }					   /*                        */
	      fmt_names(s,UsePreOr(2),1,trans);    /*                        */
	      break;				   /*			     */
	    case 'T':				   /*			     */
	      fmt_title(s,UsePreOr(1),UsePostOr(0),trans,TRUE,TitleTitleSep);
	      break;				   /*			     */
	    case 't':				   /*			     */
	      fmt_title(s,UsePreOr(1),UsePostOr(0),trans,FALSE,TitleTitleSep);
	      break;				   /*			     */
	    case 'd':				   /*			     */
	      fmt_digits(s,UsePreOr(0xffff));	   /*			     */
	      break;				   /*			     */
	    case 's':				   /*			     */
	      fmt_string(s,UsePreOr(0xffff),trans);/*			     */
	      break;				   /*			     */
	    case 'W':				   /*			     */
	      fmt_title(s,UsePreOr(1),UsePostOr(0),trans,TRUE,"");/*         */
	      break;				   /*			     */
	    case 'w':				   /*			     */
	      fmt_title(s,UsePreOr(1),UsePostOr(0),trans,FALSE,"");/*        */
	      break;				   /*			     */
	    default: return 1;			   /*			     */
	    case 'p' | NodeCountMask:		   /*			     */
	    case 'n' | NodeCountMask:		   /*			     */
	    case 'N' | NodeCountMask:		   /*			     */
	      if (fmt_c_names(s,UsePreOr(0),UsePostOr(0),HasMinus))/*        */
	      { return 1; }			   /*	                     */
	      break;				   /*			     */
	    case 'd' | NodeCountMask:		   /*			     */
	    case 's' | NodeCountMask:		   /*			     */
	      if (fmt_c_string(s,UsePreOr(0),UsePostOr(0),HasMinus))/*       */
	      { return 1; }			   /*	                     */
	      break;				   /*			     */
	    case 'T' | NodeCountMask:		   /*			     */
	    case 'W' | NodeCountMask:		   /*			     */
	      if (fmt_c_words(s,UsePreOr(0),UsePostOr(0),HasMinus,TRUE))/*   */
	      { return 1; }			   /*	                     */
	      break;				   /*			     */
	    case 't' | NodeCountMask:		   /*			     */
	    case 'w' | NodeCountMask:		   /*			     */
	      if (fmt_c_words(s,UsePreOr(0),UsePostOr(0),HasMinus,FALSE))/*  */
	      { return 1; }			   /*	                     */
	      break;				   /*			     */
	  }					   /*			     */
	}					   /*			     */
	else { return 1; }			   /*			     */
    }						   /*			     */
    kn = NodeNext(kn);				   /*			     */
  }						   /*			     */
  return 0;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	eval__special()
** Purpose:	
**
** Arguments:
**	kn
**	rec
** Returns:	nothing
**___________________________________________________			     */
static void eval__special(kn,rec)		   /*			     */
  register KeyNode kn;				   /*			     */
  register Record  rec;				   /*			     */
{ register char	   *s;				   /*			     */
  int		   missing	= TRUE,		   /*			     */
		   fmt;			   	   /*			     */
  static char	   *s_author	= NULL;		   /*			     */
  static char	   *s_editor	= NULL;		   /*			     */
  static char	   *s_title	= NULL;		   /*			     */
  static char	   *s_booktitle = NULL;		   /*			     */
  static char	   *s_key	= NULL;		   /*			     */
						   /*			     */
  if ( s_author == NULL )			   /*			     */
  { s_author	= sym_add("author",-1);		   /*			     */
    s_editor	= sym_add("editor",-1);		   /*			     */
    s_title	= sym_add("title",-1);		   /*			     */
    s_booktitle = sym_add("booktitle",-1);	   /*			     */
    s_key	= sym_add("key",-1);		   /*			     */
  }						   /*			     */
						   /*			     */
  fmt = (   NodePre(kn)==KEYSTYLE_LONG	   	   /*			     */
	 || NodePre(kn)==KEYSTYLE_LONG_NEED ? 1 : 0 );/*                     */
  NameStrip(format[fmt]) = UsePostOr(-1);	   /*                        */
    					   	   /*			     */
  IfGetField(s,s_author)			   /*			     */
  { fmt_names(s,2,fmt,trans_lower);	   	   /*                        */
    missing = FALSE;				   /*			     */
  }						   /*			     */
  else IfGetField(s,s_editor)			   /*			     */
  { fmt_names(s,2,fmt,trans_lower);	   	   /*                        */
    missing = FALSE;				   /*			     */
  }						   /*			     */
						   /*			     */
  IfGetField(s,s_title)				   /*			     */
  { (void)sbputs(NameTitleSep,key_sb);		   /*			     */
    fmt_title(s,1,0,trans_lower,TRUE,TitleTitleSep);/*			     */
    missing = FALSE;				   /*			     */
  }						   /*			     */
  else IfGetField(s,s_booktitle)		   /*			     */
  { (void)sbputs(NameTitleSep,key_sb);		   /*			     */
    fmt_title(s,1,0,trans_lower,TRUE,TitleTitleSep);/*			     */
    missing = FALSE;				   /*			     */
  }						   /*			     */
						   /*			     */
  if ( missing )				   /*			     */
  { sbrewind(key_sb);				   /*			     */
    IfGetField(s,s_key)				   /*			     */
    { fmt_title(s,1,0,trans_lower,TRUE,TitleTitleSep); }/*		     */
    else { (void)sbputs(DefaultKey,key_sb); }	   /*			     */
  }						   /*			     */
}						   /*------------------------*/


#ifdef DEBUG
/*-----------------------------------------------------------------------------
** Function:	show_fmt()
** Purpose:	Print a format tree onto the error stream
** Arguments:
**	kn
**	in
** Returns:	nothing
**___________________________________________________			     */
static void show_fmt(kn,in)			   /*			     */
  register KeyNode kn;				   /*			     */
  register int     in;				   /*                        */
{ register int     i;				   /*			     */
#define NLin ErrC('\n');for(i=in;i>0;i--) ErrC(' ')
#define ErrS(S) (void)fputs(S,err_file)
						   /*			     */
  while ( kn != (KeyNode)0 )			   /*			     */
  { switch ( NodeType(kn) )			   /*			     */
    { case NodeSTRING:				   /*			     */
	ErrPrintF(" \"%s\"",NodeSymbol(kn));	   /*			     */
	NLin;					   /*			     */
	break;					   /*			     */
      case NodeTEST:				   /*			     */
	ErrPrintF(" (%s)",NodeSymbol(kn));	   /*			     */
	NLin; ErrS("{ ");			   /*			     */
	show_fmt(NodeThen(kn),in+2);		   /*			     */
	NLin; ErrS("}");			   /*			     */
	NLin; ErrS("{ ");			   /*			     */
	show_fmt(NodeElse(kn),in+2);		   /*			     */
	NLin;					   /*			     */
	ErrS("}");				   /*			     */
	break;					   /*			     */
      case NodeOR:				   /*			     */
	ErrS("{ ");				   /*			     */
	show_fmt(NodeThen(kn),in+2);		   /*			     */
	NLin; ErrS("}");			   /*			     */
	NLin; ErrS("#");			   /*			     */
	NLin; ErrS("{ ");			   /*			     */
	show_fmt(NodeElse(kn),in+2);		   /*			     */
	NLin;					   /*			     */
	ErrS("}");				   /*			     */
	break;					   /*			     */
      default:					   /*			     */
	NLin;					   /*			     */
	fprintf(err_file,"%% %d.%d %c (%s)",	   /*			     */
	       NodePre(kn),			   /*			     */
	       NodePost(kn),			   /*			     */
	       NodeType(kn)&0xff,		   /*			     */
	       NodeSymbol(kn));			   /*			     */
    }						   /*			     */
    kn = NodeNext(kn);				   /*			     */
  }						   /*			     */
}						   /*------------------------*/

#endif
