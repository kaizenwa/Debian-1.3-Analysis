/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <string.h>
#include <ctype.h>
#include "buffer.h"
#include "ins.h"
#include "ledit.h"
#include "text.h"
#include "screen.h"
#include "cmds.h"
#include "paste.h"
#include "misc.h"

/*}}}*/

/* This routine deletes multiple spaces except those following a period, '?'
 * or a '!'.
   Returns the address of beginning of non whitespace */
unsigned char *text_format_line(void) /*{{{*/
{
    unsigned char *p, *p1;
    int min;

    p = CLine->data;
    Point = 0;

    while(((*p == '\t') || (*p == ' ')) && (Point < CLine->len)) p++, Point++;
    min = Point;

    Point = CLine->len - 1;
    if (Point < 0) Point = 0;
    p = CLine->data + Point;

    while (Point > min)
      {
          if ((*p == ' ') || (*p == '\t'))
            {
	       Point--; p--;
	       p1 = p - 1;
	       if (((*p == ' ') || (*p == '\t')) && (Point > min)
		   && (*p1 != '.') && (*p1 != '?') && (*p1 != '!'))
		 {
		    del();
		    if (*p == '\t') *p = ' ';
		 }
	    }
          else
            {
                Point--; p--;
            }
      }
   return(CLine->data + min);
}

/*}}}*/

static int wrap_line1(int format) /*{{{*/
{
   unsigned char *p, *pmin;
   int col;

   if (format) pmin = text_format_line(); else pmin = CLine->data;
   eol();
   col = calculate_column();
   if (col < Jed_Wrap_Column)
     {
	return(0);
     }

   point_column(Jed_Wrap_Column - 1);
   p = CLine->data + Point;

   while(p > pmin)
     {
	if ((*p == ' ') || (*p == '\t')) break;
	p--;
     }

   if (p == pmin)
     {
	/* that failed, so go the other way */
	p = CLine->data + CLine->len;
	while(pmin < p)
	  {
	     if ((*pmin == ' ') || (*pmin == '\t')) break;
	     pmin++;
	  }
	if (p == pmin) return(0);
	p = pmin;
     }

   Point = (int) (p - CLine->data);
   trim_whitespace();
   newline();
   CLine = CLine->prev; LineNum--;
   return(1);
}

/*}}}*/

void wrap_line(int format) /*{{{*/
{
    push_spot();
    wrap_line1(format);
    pop_spot();
}

/*}}}*/

/* Here a paragraph follows either an indentation, a '\\' char or two
   '\n' chars.  or a '%' char since tex uses this */
static int is_paragraph_sep(void) /*{{{*/
{
   unsigned char *p;
   int ret;
   p = CLine->data;
   
   if (CBuf->par_sep != NULL)
     {
	SLexecute_function(CBuf->par_sep);
	(void) SLang_pop_integer(&ret);
	if (SLang_Error) 
	  {
	     CBuf->par_sep = NULL;
	     ret = 1;
	  }
	return ret;
     }
   if ((*p == '\n') || (*p == '\\') || (*p == '%')) return(1);
   return(0);
}

/*}}}*/

int backward_paragraph(void) /*{{{*/
{
   Line *prev = CLine->prev;
   int ro = CBuf->flags & READ_ONLY;
   
   if (NULL == CBuf->par_sep)
     {
	CBuf->par_sep = SLang_get_function("is_paragraph_separator");
     }

   Point = 0;
   if (prev == NULL) return(0);
   CLine = prev; LineNum--;
   
   while(1)
     {
	eol();
	if (!ro) trim_whitespace();
	if (is_paragraph_sep() || (CLine->prev == NULL)) break;
	CLine = CLine->prev; LineNum--;
     }
   Point = 0;
   return(1);
}

/*}}}*/

int forward_paragraph(void) /*{{{*/
{
   int ro = CBuf->flags & READ_ONLY;   
   
   if (NULL == CBuf->par_sep)
     {
	CBuf->par_sep = SLang_get_function("is_paragraph_separator");
     }

   while(1)
     {
	if (CLine->next == NULL) break;
	CLine = CLine->next;  LineNum++;
	eol();
	if (!ro) trim_whitespace();
	if (is_paragraph_sep()) break;
     }

   eol();
   return(1);
}

/*}}}*/

/* format paragraph and if Prefix argument justify_hook is called. */
int text_format_paragraph () /*{{{*/
{
   unsigned char *p;
   int n, col;
   Line *end, *beg, *next;
   
   CHECK_READ_ONLY
   push_spot();
   if (is_paragraph_sep())
     {
	pop_spot();
	return(0);
     }
   
   /* if (CBuf->modes != WRAP_MODE) return(0); */

   get_current_indent(&n);

   /* find end */
   forward_paragraph();
   if (CLine->next == NULL) end = NULL; 
   else
     {
	end = CLine;
     }
      
   /* find paragraph start */
   backward_paragraph();
   if (is_paragraph_sep() && (CLine->next != NULL))
     {
	CLine = CLine->next; LineNum++;
     }
   beg = CLine;
   Point = 0;
   
   /* Now loop formatting as we go until the end is reached */
   while(CLine != end)
     {
	eol();
	if (CLine != beg) indent_to(n);
	if (wrap_line1(1))
	  {
	     CLine = CLine->next;
	     LineNum++;
	     indent_to(n);
	     continue;
	  }
	else if (CLine->next == end) break;

	next = CLine->next;
	if (next != end)
	  {
	     /* Now count the length of the word on the next line. */
	     CLine = next;  LineNum++;
	     Point = 0;
	     trim_whitespace();
	     p = CLine->data;
	     while((*p > ' ') && (p - CLine->data < CLine->len)) p++;

	     CLine = CLine->prev; LineNum--;
	     eol();

	     col = calculate_column();
	     if ((p - next->data) + col < Jed_Wrap_Column - 1)
	       {
		  del();
		  ins(' ');
	       }
	     else 
	       {
		  CLine = CLine->next;
		  LineNum++;
	       }
	  }
     }
   if (Repeat_Factor != NULL) 
     {
	SLang_run_hooks("format_paragraph_hook", NULL, NULL);
	Repeat_Factor = NULL;
     }
   pop_spot();
   return(1);
}

/*}}}*/

int narrow_paragraph(void) /*{{{*/
{
   int wrap, n;
   
   CHECK_READ_ONLY
   /* if (CBuf->modes != WRAP_MODE) return(0); */
   get_current_indent(&n);
   wrap = Jed_Wrap_Column;
   if (wrap - n <= wrap/2) return(0);
   Jed_Wrap_Column -= n;
   text_format_paragraph();
   Jed_Wrap_Column = wrap;
   return(1);
}

/*}}}*/

int center_line(void) /*{{{*/
{
   unsigned char *p, *pmax;
   int len;

   CHECK_READ_ONLY
   push_spot();
   (void) eol_cmd();
   p = CLine->data;
   pmax = p + CLine->len;

   while(p < pmax)
     {
	if (*p > ' ') break;
	p++;
     }
   if ((len = (int)(pmax - p)) < 0) len = 0;
   if ((len = (Jed_Wrap_Column - len) / 2) < 0) len = 0;
   indent_to(len);
   pop_spot();
   return(1);
}

/*}}}*/

int text_smart_quote(void) /*{{{*/
{
   char c, last;
   int upd;

   /* Force a screen update.  This help syntax highlighting */
   JWindow->trashed = 1;
   
   if (Point) c = (char ) *(CLine->data + (Point - 1)); else c = 0;
   if (!(CBuf->modes & WRAP_MODE) || (c == '\\')) return ins_char_cmd();

   last = SLang_Last_Key_Char;
   if ((c == '(') || (c == '[') || (c == '{') || (c <= ' ') || !Point)
     SLang_Last_Key_Char = '`';
   else
     SLang_Last_Key_Char = '\'';

   upd = ins_char_cmd();
   if (last == '"') upd = ins_char_cmd();
   SLang_Last_Key_Char = last;
   return upd;
}

/*}}}*/

char Jed_Word_Range[256];
void define_word(char *w) /*{{{*/
{
  strcpy(Jed_Word_Range, w);
}

/*}}}*/

/* capitalize region does not really work since it involves words, etc... */
void transform_region(int *what) /*{{{*/
{
   int pnt, n;
   Line *line;
   unsigned char *p;

   CHECK_READ_ONLY_VOID
   if (!check_region(&Number_One)) return;    /* spot pushed */

   pnt = Point;
   line = CLine;
   pop_mark(&Number_One);

   p = CLine->data + Point; 
   while (1)
     {
	if (line == CLine) n = pnt; else n = CLine->len;
	
	switch (*what)
	  {
	   case 'u': 
	     while (Point < n)
	       {
		  p = CLine->data + Point;
		  replace_char(UPPER_CASE(*p));
		  Point++;
	       }
	     break;
	     
	   case 'c':
	     if (Point >= n)
	       break;
	     p = CLine->data + Point;
	     replace_char(UPPER_CASE(*p));
	     Point++;
	     p = CLine->data + Point;
	     /* drop through */
	     
	   case 'd': 
	     while (Point < n) 
	       {
		  p = CLine->data + Point;
		  replace_char(LOWER_CASE(*p));
		  Point++;
	       }
	     break;
	     
	   default: 
	     while (Point < n) 
	       {
		  p = CLine->data + Point;
		  replace_char(CHANGE_CASE(*p));
		  Point++;
	       }
	     break;
	  }
	
	
	if (line == CLine) break;
	CLine = CLine->next;
	LineNum++;
	Point = 0;
	p = CLine->data;
     }
   pop_spot();
   /* mark_buffer_modified(&Number_One); */
}

/*}}}*/

void skip_word_chars(void) /*{{{*/
{
   skip_chars1(Jed_Word_Range, 0);
}

/*}}}*/

void skip_non_word_chars(void) /*{{{*/
{
   skip_chars1(Jed_Word_Range, 1);
}

/*}}}*/

void bskip_word_chars(void) /*{{{*/
{
   bskip_chars1(Jed_Word_Range, 0);
}

/*}}}*/

void bskip_non_word_chars(void) /*{{{*/
{
   bskip_chars1(Jed_Word_Range, 1);
}

/*}}}*/

