/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
#include <string.h>
#include "buffer.h"
#include "screen.h"
#include "file.h"
#include "misc.h"
#include "ledit.h"

static unsigned short *Char_Syntax;
static char **Keywords = NULL;	       /* array of keywords */
static int Keyword_Not_Case_Sensitive;

#if JED_HAS_COLOR_COLUMNS
static void color_columns (register unsigned short *p, register unsigned short *pmax) /*{{{*/
{
   register unsigned char *color = CBuf->column_colors;
   if (color == NULL) return;
   while (p < pmax) 
     {
	*p |= ((unsigned short) *color) << 8;
	p++; color++;
     }
}

/*}}}*/
#endif   

static int try_keyword (register unsigned short *q, int n, register char *t, unsigned short color) /*{{{*/
{   
   unsigned short *p;
   
   while (*t)
     {
	p = q - n;
	if (Keyword_Not_Case_Sensitive == 0)
	  {
	     while ((p < q) && (*t == (char) *p))
	       {
		  p++; t++;
	       }
	  }
	else while ((p < q) && (*t == (char) LOWER_CASE (*p)))
	  {
	     p++; t++;
	  }
	
	if (p == q)
	  {
	     p = q - n;
	     while (p < q) *p++ |= color;
	     return 0;
	  }
	
	/* alphabetical */
	if (*t > ((char) *p | Keyword_Not_Case_Sensitive))
	  break;
	
	t += (int) (q - p);
     }
   return -1;
}

/*}}}*/

static unsigned short *highlight_word (unsigned short *p, unsigned short *pmax) /*{{{*/
{
   char **kwds;
   register unsigned short *q;
   int n;
   int i;
   unsigned short color;
   
   q = p;
   while ((q < pmax) && (Char_Syntax[*q] & WORD_SYNTAX)) q++;
   
   n = (int) (q - p);
   
   kwds = Keywords;
   if ((kwds == NULL) || (n > MAX_KEYWORD_LEN)) return q;
   
   for (i = 0; i < MAX_KEYWORD_TABLES; i++)
     {
	char *t;
	t = kwds[n - 1];
	if (t != NULL)
	  {
	     color = (JKEY_COLOR + i) << 8;
	     if (0 == try_keyword (q, n, t, color)) return q;
	  }
	kwds += MAX_KEYWORD_LEN;
     }
   return q;
}

/*}}}*/

static unsigned short *highlight_string (unsigned short *p, unsigned short *pmax, /*{{{*/
					 unsigned char quote)
{
   unsigned char ch;
   unsigned char q = (unsigned char) *p;

   *p++ |= JSTR_COLOR << 8;
   
   while (p < pmax)
     {
	ch = (unsigned char) *p;
	*p++ |= JSTR_COLOR << 8;
	if (ch == q) break;
	if ((ch == quote) && (p < pmax))
	  {
	     *p++ |= JSTR_COLOR << 8;
	  }
     }
   return p;
}

/*}}}*/

static unsigned short *highlight_number (unsigned short *p, unsigned short *pmax) /*{{{*/
{
   unsigned short *p1;
   unsigned char ch;
   
   
   
   ch = (unsigned char) *p;
   if (ch == '-')
     {
	p1 = p + 1;
	if ((p1 < p) && (Char_Syntax[*p1] & NUMBER_SYNTAX))
	  {
	     *p |= JNUM_COLOR << 8;
	     *p1 |= JNUM_COLOR << 8;
	     p += 2;
	  }
	else 
	  {
	     *p |= JOP_COLOR << 8;
	     return p + 1;
	  }
     }
   
   while ((p < pmax) && (Char_Syntax[*p] & NUMBER_SYNTAX))
     {
	*p++ |= JNUM_COLOR << 8;
     }
	     
   return p;
}

/*}}}*/

static unsigned short *highlight_comment (unsigned short *p, unsigned short *pmax, /*{{{*/
					  Syntax_Table_Type *st)
{
   int comment_type = st->flags;
   unsigned short *p1;
   unsigned char comment_end = st->comment_end;
   
   *p++ |= JCOM_COLOR << 8;
   if ((p < pmax) && (comment_type & MULTICHAR_TYPE)) *p++ |= JCOM_COLOR << 8;
   
   while (p < pmax)
     {
	if (*p == comment_end)
	  {
	     if ((comment_type & MULTICHAR_TYPE) == 0) 
	       {
		  *p++ |= JCOM_COLOR << 8;
		  break;
	       }
	     
	     p1 = p + 1;
	     if ((p1 < pmax) && (*p1 == st->comment_end2))
	       {
		  *p |= JCOM_COLOR << 8;
		  *p1 |= JCOM_COLOR << 8;
		  p += 2;
		  break;
	       }
	  }
	*p++ |= JCOM_COLOR << 8;
     }
   return p;
}

/*}}}*/

#if JED_HAS_DFA_SYNTAX
# include "dfasyntx.c"
#endif

void syntax_highlight (register unsigned short *p, /*{{{*/
		       register unsigned short *pmax)
{
   Syntax_Table_Type *st = CBuf->syntax_table;
   unsigned char ch;
   unsigned int flags;
   unsigned short *pmin, syntax, *p1;
   
#if JED_HAS_COLOR_COLUMNS
   if (CBuf->coloring_style)
     {
	color_columns (p, pmax);
	return;
     }
#endif
   
#if JED_HAS_DFA_SYNTAX
   if ((st->hilite != NULL) && (st->hilite->dfa != NULL))
     {
	dfa_syntax_highlight (p, pmax, st);
	return;
     }
#endif
   
   /* Check for preprocessor character */
   if (*p == st->preprocess)
     {
	/* I should only scan to a comment. But.... */
	while (p < pmax) *p++ |= (JPREPROC_COLOR << 8);
	return;
     }
       
   flags = st->flags;

   /* Look for Fortran like comments.  These are allowed to have any character
    * except a numeral at the beginning of a line.
    */
   if ((flags & FORTRAN_TYPE)
       && ((*p != ' ') && ((*p > '9') || (*p < '0'))))
     {
	while (p < pmax) *p++ |= (JCOM_COLOR << 8);
	return;
     }
 
   /* Skip whitespace */
   while ((p < pmax) && (*p == ' ')) p++;
   if (p == pmax) return;
   
   /* If we are looking at a '* ' combination, then this is considered to 
    * be a C comment (like this line).
    */
   if ((flags & C_COMMENT_TYPE) && (*p == '*'))
     {
	unsigned short *p11 = p + 1;
	while ((p11 < pmax) && (*p11 == '*')) p11++;
	if ((p11 == pmax) || (*p11 == ' '))
	  p = highlight_comment (p, pmax, st);
     }
   
   pmin = p;
   
   /* Now the preliminary stuff is done so do the hard part */
   while (p < pmax)
     {
	syntax = Char_Syntax[*p];
	
	if (syntax & WORD_SYNTAX) 
	  {
	     if ((*p > '9') || (0 == (syntax & NUMBER_SYNTAX)))
	       {
		  p = highlight_word (p, pmax);
		  continue;
	       }
	  }
	if (syntax == 0) 
	  {
	     p++;
	     continue;
	  }
	
	if (syntax & DELIM_SYNTAX)
	  {
	     *p++ |= JDELIM_COLOR << 8;
	     continue;
	  }
	
	if (syntax & STRING_SYNTAX)
	  {
	     p = highlight_string (p, pmax, st->quote_char);
	     continue;
	  }
	
	if (syntax & COMMENT_SYNTAX)
	  {
	     p1 = p + 1;
	     ch = *p;
	     
	     if (flags & MULTICHAR_TYPE)
	       {
		  if (p1 < pmax)
		    {
		       if ((ch == st->comment_beg) && (*p1 == st->comment_beg2))
			 {
			    p = highlight_comment (p, pmax, st);
			    continue;
			 }
		       if ((ch == st->comment_end) && (*p1 == st->comment_end2))
			 {
			    /* Missed the beginning of it.  So, start from the
			      * beginning and comment
			      the whole line, like this one */
			    p += 2;
			    while (pmin < p)
			      {
				 *pmin = (JCOM_COLOR << 8) | (*pmin & 0xFF);
				 pmin++;
			      }
			    continue;
			 }
		       /* Check for C++ */
		       if ((flags & C_COMMENT_TYPE)
			   && (ch == '/') && (*p1 == ch))
			 {
			    while (p < pmax) *p++ |= (JCOM_COLOR << 8);
			    return;
			 }
		    }
	       }
	     else
	       {
		  if (flags & EOL_COMMENT_TYPE)
		    {
		       while (p < pmax) *p++ |= (JCOM_COLOR << 8);
		       return;
		    }
		  if (ch == st->comment_beg) 
		    {
		       p = highlight_comment (p, pmax, st);
		       continue;
		    }
		  else if (ch == st->comment_end)
		    {
		       p++;
		       while (pmin < p)
			 {
			    *pmin++ |= (JCOM_COLOR << 8);
			 }
		       continue;
		    }
	       }
	  }			       /* comment syntax */
	
	if (syntax & OP_SYNTAX)
	  {
	     *p++ |= JOP_COLOR << 8;
	     continue;
	  }
	
	if (syntax & NUMBER_SYNTAX)
	  {
	     p = highlight_number (p, pmax);
	     continue;
	  }
	
	if (syntax & HTML_START_SYNTAX)
	  {
	     *p++ |= JKEY_COLOR << 8;
	     while (p < pmax)
	       {
		  if (Char_Syntax[*p] & HTML_END_SYNTAX)
		    {
		       *p++ |= JKEY_COLOR << 8;
		       break;
		    }
		  *p++ |= JKEY_COLOR << 8;
	       }
	     continue;
	  }
	
	if (syntax & HTML_END_SYNTAX)  /* missed start from previous line */
	  {
	     while (pmin < p)
	       {
		  *pmin = (JKEY_COLOR << 8) | (*pmin & 0xFF);
		  pmin++;
	       }
	     *p++ |= JKEY_COLOR << 8;
	     continue;
	  }
	
	
	if ((syntax & OPEN_DELIM_SYNTAX) || (syntax & CLOSE_DELIM_SYNTAX))
	  {
	     *p++ |= JDELIM_COLOR << 8;
	     continue;
	  }
	
	if ((syntax & QUOTE_SYNTAX) && (flags & TEX_LIKE_KEYWORDS))
	  {
	     *p++ |= JKEY_COLOR << 8;
	     if (p < pmax)
	       {
		  if (Char_Syntax[*p] & WORD_SYNTAX)
		    {
		       do
			 {
			    *p++ |= JKEY_COLOR << 8;
			 }
		       while ((p < pmax) && (Char_Syntax[*p] & WORD_SYNTAX));
		    }
		  else *p++ |= JKEY_COLOR << 8;
	       }
	     continue;
	  }
	/* Undefined. */
	p++;
     }
}

/*}}}*/

void init_syntax_highlight (void) /*{{{*/
{
    Syntax_Table_Type *st = CBuf->syntax_table;

    Mode_Has_Syntax_Highlight = 1;
    if (CBuf->coloring_style) return;

   if ((st == NULL) || (st == Default_Syntax_Table)) 
     {
        Mode_Has_Syntax_Highlight = 0;
        return;
    }

#if JED_HAS_DFA_SYNTAX
   if ((NULL != st->hilite) && (NULL != st->hilite->dfa))
     return;
#endif
   
   Char_Syntax = st->char_syntax;
   
   if (st->flags & SYNTAX_NOT_CASE_SENSITIVE) 
     Keyword_Not_Case_Sensitive = 0x20;
   else
     Keyword_Not_Case_Sensitive = 0;
   
   Keywords = (char **) st->keywords;   
}

/*}}}*/

