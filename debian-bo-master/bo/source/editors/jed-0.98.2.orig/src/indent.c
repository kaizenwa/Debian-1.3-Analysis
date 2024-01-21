/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <string.h>
#include "buffer.h"
#include "window.h"
#include "misc.h"
#include "cmds.h"
#include "display.h"
#include "ledit.h"
#include "screen.h"
#include "sysdep.h"

/*}}}*/

static Syntax_Table_Type *Syntax_Tables;

static void goto_effective_eol (Syntax_Table_Type *table) /*{{{*/
{
   unsigned char *p, *pmax;
   unsigned short *syntax;
   register unsigned char ch;
   unsigned char in_char = 0;
   int cb0, cb1, quote, sc, cc;
   
   cb0 = table->comment_beg;
   quote = table->quote_char;
   sc = table->string_char;
   cc = table->char_char;
   syntax = table->char_syntax;
   
   if (table->flags & MULTICHAR_TYPE) cb1 = table->comment_beg2;
   else cb1 = -1;
   
   p = CLine->data;
   pmax = CLine->data + CLine->len;
   
   while (p < pmax)
     {
	ch = *p++;
	if ((syntax[ch] & SYNTAX_MASK) == 0) continue;
	
	if ((in_char == 0) && (ch == cb0))
	  {
	     if (cb1 != -1)
	       {
		  if ((p < pmax) && (*p != cb1)) continue;
		  p--;
	       }
	     p -= 2;
	     Point = (int) (p - CLine->data);
	     /* Note: it is ok if Point is negative here. */
	     return;
	  }
	
	if (ch == quote) 
	  {
	     p++;
	     continue;
	  }
	
	if (ch == in_char) in_char = 0;
	else if (in_char == 0)
	  {
	     if (ch == sc) in_char = sc;
	     if (ch == cc) in_char = cc;
	  }
     }
   
   Point = CLine->len - 1;
}

/*}}}*/

Syntax_Table_Type *Default_Syntax_Table;


/* Go backward looking for the matching ch--- not the char that matches ch.
 * Rather, ch is the matching character.
 * This routine returns:
 *   1 if found and leaves the point on the match
 *  -2 if not found but we appear to be in a comment.  In this case, the point
 *     if left at the beginning of the comment
 *  -1 Not found but we appear to be in a string.  This leaves the point at the 
 *     beginning of the string.
 *   0 if not found.  The point is left where we gave up
 *   2 if went back too far
 * count is the number of lines to go back
 */

static int backward_goto_match (int count, unsigned char ch) /*{{{*/
{
   unsigned char *p, *pmin, *save_pos = NULL, want_ch;
   unsigned short *syntax;
   int in_string, in_comment, level, is_quoted;
   int cb, ce, ce0, cb0, quote, flags;
   Syntax_Table_Type *table;
   unsigned int this_syntax;
   
   p = CLine->data + Point;
   
   table = CBuf->syntax_table;
   if (table == NULL) return 0;
   syntax = table->char_syntax;
   quote = table->quote_char;
   
   flags = table->flags;
   if (flags & MULTICHAR_TYPE) 
     {
	cb = table->comment_beg2;
	ce = table->comment_end2;
	cb0 = table->comment_beg;
	ce0 = table->comment_end;
     }
   else
     {
	cb = table->comment_beg;
	ce = table->comment_end;
	cb0 = -1;
	ce0 = -1;
     }
   
   /* Here we go */
   
   if (ch == 0) ch = *p;
   want_ch = table->matching_delim [ch];
   if (want_ch == 0) return 0;
   
   level = 1;
   in_string = 0; in_comment = 0;
   
   Point--; 
   while (count)
     {
	p = CLine->data + Point;
	pmin = CLine->data;
	
	/* This loop here is where it all happens. */
	while (p >= pmin)
	  {
	     ch = *p--;
	     
	     if ((syntax[ch] & SYNTAX_MASK) == 0) continue;
	     
	     
	     /* Check to see if it is quoted. */
	     if ((p >= pmin) && (*p == quote))
	       {
		  unsigned char *psave;
		  is_quoted = 0;
		  psave = p;
		  while ((p >= pmin) && (*p == quote))
		    {
		       p--;
		       is_quoted = !is_quoted;
		    }
		  if (is_quoted) continue;
		  p = psave;
	       }
	     
	     this_syntax = syntax[ch];
	     
	     if (this_syntax & COMMENT_SYNTAX)
	       {
		  if (in_string && !in_comment) continue;
		  if (in_comment == 0)
		    {
		       if (ch == ce)
			 {
			    if (ce0 != -1)
			      {
				 if ((p >= pmin) && (*p == ce0))
				   {
				      in_comment = 1;
				      p--;
				   }
			      }
			    else in_comment = 1;
			 }
		       if (in_comment) continue;
		    }
		  
		  if (ch == cb)
		    {
		       /* if we hit a comment start, we are out of 
			* here since there is no point in parsing beyond 
			* because there are no syntax contraints within  a
			* comment.
			*/
		       if (cb0 != -1)
			 {
			    if ((p >= pmin) && (*p == cb0))
			      {
				 in_comment--;
				 p--;
			      }
			    else goto comment_endif;
			 }
		       else in_comment--;
		       
		       if (in_comment || in_string)
			 {
			    p++;
			    Point = (int) (p - pmin);
			    return -2;
			 }
		       continue;
		    }
	       }
	     
	     comment_endif:
	     
	     if (this_syntax & STRING_SYNTAX)
	       {
		  /* string/char */
		  if (in_comment) continue;
		  if (in_string == 0) in_string = ch;
		  else if (in_string == ch) in_string = 0;
		  /* Save this in case we are really in a comment
		   * because if we do not find the other match, we 
		   * must come back again.  Sigh.
		   */
		  if (in_string) save_pos = p;
		  continue;
	       }

	     if (this_syntax & OPEN_DELIM_SYNTAX)
	       {
		  /* opening delimiter */
		  if (in_string || in_comment) continue;
		  if (level == 1)
		    {
		       Point = (int) (p - pmin) + 1;
		       if (ch == want_ch) return 1;
		       return 0;
		    }
		  level--;
	       }

	     if (this_syntax & CLOSE_DELIM_SYNTAX)
	       {
		  if (!in_string && !in_comment) level++;
	       }
	  }
   
	/* END OF MAIN LOOP: while (p >= pmin) */
	
	if (in_string && !in_comment)
	  {
	     /* Ok so we have several choices.  Here I am going to assume 
	      * that we are in a comment unless there is a quote char at the
	      * end of the previous line to indicate a continuation.
	      */
	     if ((CLine->prev == NULL)
		 || (CLine->prev->len <= 1)
		 || (quote != *(CLine->prev->data + (CLine->prev->len - 2))))
	       {
		  /*
		   * Do the same for the current line since it is possible
		   * for the quote at the end of the line to indicate that 
		   * the string char is really the beginning.
		   */		  
		  if ((CLine->len <= 1) 
		      || (quote != *(CLine->data + (CLine->len - 2))))
		    {
		       in_comment = 1;	     
		    }
		  /* So we have a quote at the end of this line.  So, 
		   * instead of assuming a comment, let's assume that we
		   * are really back in code again.
		   */
		  else in_string = 0;
		  
		  /* go back and try again under new assumption */
		  Point = (int) (save_pos - CLine->data);
		  continue;
	       }
	  }
	
	/* Move to the previous line. */
	if (CLine->prev == NULL) 
	  {
	     Point = 0;
	     break;
	  }
	CLine = CLine->prev; LineNum--; Point = CLine->len - 1;
	count--;
	
	/* Compute the effective end of line since we do not want to 
	 * start out in a comment.  This only happens for eol type comments.
	 * This means the the Point is now in a position to continue parsing
	 * from.  It is possible that Point is negative after this call meaning
	 * that the comment started at the beginning of the line.
	 */
	if (flags & EOL_COMMENT_TYPE) 
	  goto_effective_eol (table);
	
     }
   
   /* What have we learned? */
   
   if (Point < 0) Point = 0;
   if (count == 0)
     {
	/* In this case, we went back as far as permitted.  Nothing much can be
	 * said.
	 */
	Point = 0;
	return 2;
     }
   
   if (in_string) return -1;
   if (in_comment) return -2;
   
   /* If we are here, then we have a mismatch */
   return 0;
}

/*}}}*/

static int forward_goto_match (unsigned char ch) /*{{{*/
{
   unsigned char *p, *pmax, want_ch;
   unsigned short *syntax;
   int in_string, in_comment, level;
   int cb, ce, ce1, cb1, flags;
   unsigned int this_syntax;
   Syntax_Table_Type *table;
   
   p = CLine->data + Point;
   
   table = CBuf->syntax_table;
   if (table == NULL) return 0;
   syntax = table->char_syntax;
   
   flags = table->flags;
   if (flags & MULTICHAR_TYPE) 
     {
	cb1 = table->comment_beg2;
	ce1 = table->comment_end2;
	cb = table->comment_beg;
	ce = table->comment_end;
     }
   else
     {
	cb = table->comment_beg;
	ce = table->comment_end;
	cb1 = -1;
	ce1 = -1;
     }
   
   /* Here we go */
   
   if (ch == 0) ch = *p;
   want_ch = table->matching_delim [ch];
   if (want_ch == 0) return 0;
   
   level = 1;
   in_string = 0; in_comment = 0;
   
   Point++;
   while (1)			       /* The only way to get out of
					* this loop is to find a match.
					*/
     {
	p = CLine->data + Point;
	pmax = CLine->data + CLine->len;
	
	/* This loop here is where it all happens. */
	while (p < pmax)
	  {
	     ch = *p++;
	     
	     if ((syntax[ch] & SYNTAX_MASK) == 0) continue;
	     
	     this_syntax = syntax[ch];
		  
	     if (this_syntax & COMMENT_SYNTAX)
	       {
		  if (in_string) continue;
		  if (in_comment)
		    {
		       if (ch == ce)
			 {
			    if (ce1 != -1)
			      {
				 if ((p < pmax) && (*p == ce1))
				   {
				      in_comment = 0;
				      p++;
				   }
			      }
			    else in_comment = 0;
			 }
		       if (in_comment == 0) continue;
		    }
		  else if (ch == cb)   /* not in comment */
		    {
		       if (cb1 != -1)
			 {
			    if ((p < pmax) && (*p == cb1))
			      {
				 in_comment = 1;
				 p++;
			      }
			 }
		       else in_comment = 1;

		       if (in_comment && (flags & EOL_COMMENT_TYPE))
			 {
			    /* This will need modified once I support two
			     * types of comments, e.g., C++ (yuk)
			     */
			    p = pmax;
			    in_comment = 0;
			 }
		       if (in_comment) continue;
		    }
	       }
	     
	     if (this_syntax & STRING_SYNTAX)
	       {
		  /* string/char */
		  if (in_comment) continue;
		  if (in_string == 0) in_string = ch;
		  else if (in_string == ch) in_string = 0;
		  continue;
	       }
	     
	     if (this_syntax & OPEN_DELIM_SYNTAX)
	       {
		  if (!in_string && !in_comment) level++;
		  continue;
	       }
	     
	     if (this_syntax & CLOSE_DELIM_SYNTAX)
	       {
		  if (in_string || in_comment) continue;
		  if (level == 1)
		    {
		       Point = (int) (p - CLine->data) - 1;
		       if (ch == want_ch) return 1;
		       return 0;
		    }
		  level--;
		  continue;
	       }
	     
	     if (this_syntax & QUOTE_SYNTAX) p++; /* skip next char */
	  }
	/* END OF MAIN LOOP: while (p < pmax) */
	
	/* Move to the next line. */
	if (CLine->next == NULL) break;
	CLine = CLine->next; LineNum++; Point = 0;
     }
   
   eol ();
   if (in_string) return -1;
   if (in_comment) return -2;
   
   /* If we are here, then we have a mismatch */
   return 0;
}

/*}}}*/

int find_matching_delimiter (int *ch) /*{{{*/
{
   unsigned char ch1 = (unsigned char) *ch;
   Syntax_Table_Type *table = CBuf->syntax_table;
   
   if (ch1 == 0) ch1 = *(CLine->data + Point);
   if (table->char_syntax[ch1] & OPEN_DELIM_SYNTAX)
     return forward_goto_match (ch1);
   else return backward_goto_match (5000, ch1);
}

/*}}}*/

int goto_match (void) /*{{{*/
{
   unsigned char ch;
   int ret;
   Syntax_Table_Type *table = CBuf->syntax_table;
   
   ch = *(CLine->data + Point);
   
   if (table->char_syntax[ch] & OPEN_DELIM_SYNTAX)
     ret = forward_goto_match (ch);
   else ret = backward_goto_match (LineNum, ch);
   
   if (ret != 1)
     {
	if (!IS_MINIBUFFER) msg_error("Mismatch!!");
	return (0);
     }
   return (1);
}

/*}}}*/

static int parse_to_point1 (Syntax_Table_Type *table) /*{{{*/
{
   unsigned char *p, *pmax, ch;
   unsigned char in_char = 0, in_comm = 0;
   int cb0,  cb1,  ce0,  ce1, quote,  sc,  cc,  flags;
   
   sc = table->string_char;
   cc = table->char_char;
   flags = table->flags;
   if (flags & MULTICHAR_TYPE)
     {
	cb0 = table->comment_beg;
	ce0 = table->comment_end;
	cb1 = table->comment_beg2;
	ce1 = table->comment_end2;
     }
   else
     {
	cb0 = table->comment_beg;
	ce0 = table->comment_end;
	cb1 = ce1 = -1;
     }
   quote = table->quote_char;
   
   p = CLine->data;
   pmax = p + Point;
   
   while (p < pmax)
     {
	ch = *p++;
	if (in_comm)
	  {
	     if (ch == ce0)
	       {
		  if (ce1 == -1) in_comm = 0;
		  else if ((p < pmax) && (*p == ce1))
		    {
		       in_comm = 0;
		       p++;
		    }
	       }
	     if (in_comm) continue;
	  }
	
	if (in_char)
	  {
	     if (ch == in_char) in_char = 0;
	     else if (ch == quote) p++;
	     continue;
	  }
	
	if (ch == cb0)
	  {
	     if (cb1 == -1) in_comm = 1;
	     else if ((p < pmax) && (*p == cb1))
	       {
		  in_comm = 1;
		  p++;
	       }
	     if (in_comm) continue;
	  }
	
	if ((ch == cc) || (ch == sc)) in_char = ch;
     }
   if (in_char) return -1;
   if (in_comm) return -2;
   return 0;
}

/*}}}*/

int parse_to_point (void) /*{{{*/
{
   Syntax_Table_Type *table = CBuf->syntax_table;
   if (table == NULL) return 0;
   return  parse_to_point1 (table);
}

/*}}}*/

/* blink the matching fence.  This assumes that the window is ok */
void blink_match (void) /*{{{*/
{
   Line *save;
   int pnt, code, matchp;
   unsigned int l;
   char buf[600], strbuf[256];
   
   if (!Blink_Flag || (Repeat_Factor != NULL) || Batch) return;
   if (JWindow->trashed) update((Line *) NULL, 0, 0);
   if (JWindow->trashed) return;
   pnt = Point;
   save = CLine;
   l = LineNum;
   
   if (Point) Point--;
   code = backward_goto_match (1000, 0);
   
   if (code == 0)
     {
	if ((! (CBuf->modes == WRAP_MODE)) && (!IS_MINIBUFFER)) 
	  message("Mismatch??");
     }
   else if ((code == 1) && is_line_visible (LineNum))
     {
	point_cursor(0);
	input_pending(&Number_Ten);
	Point = pnt;
	CLine = save;
	LineNum = l;
	point_cursor(0);
	return;
     }
   else if (code == 1)
     {
	matchp = Point;
	Point = 0;
	strcpy(buf, "Matches ");
	skip_whitespace();
	if ((matchp == Point) && prevline(&Number_One))
	  {
	     Point = 0;
	     strcat(buf, make_line_string(strbuf));
	     nextline(&Number_One);
	     Point = 0;
	  }
	strcat(buf, make_line_string(strbuf));
	message(buf);
     }
   Point = pnt;
   CLine = save;
   LineNum = l;
}

/*}}}*/

Syntax_Table_Type *find_syntax_table (char *name, int err) /*{{{*/
{
   Syntax_Table_Type *table = Syntax_Tables;
   while (table != NULL)
     {
	if (!strncmp (table->name, name, 15)) return table;
	table = table->next;
     }
   if (err) msg_error ("Syntax table undefined.");
   return table;
}

/*}}}*/

void set_syntax_flags (char *name, int *flags) /*{{{*/
{
   Syntax_Table_Type *table;
   
   table = find_syntax_table (name, 1);
   if (table == NULL) return;
   
   table->flags |= *flags & 0xFF;
}

/*}}}*/

void define_syntax (int *what, char *name) /*{{{*/
{
   Syntax_Table_Type *table;
   int c2;
   unsigned int i;
   char *s1 = NULL, *s2 = NULL;
   unsigned char lut[256], *s;
   
   table = find_syntax_table (name, 1);
   if (table == NULL) return;
   
   switch (*what)
     {
      case '%':
	if (SLpop_string (&s2)) break;
	if (SLpop_string (&s1)) break;
	
	table->char_syntax[(unsigned char) *s2] |= COMMENT_SYNTAX;
	if (0 != (table->comment_end = (unsigned char) *s2))
	  {
	     table->comment_end2 = (unsigned char) *(s2 + 1);
	     table->char_syntax[(unsigned char) *(s2 + 1)] |= COMMENT_SYNTAX;
	  }
	
	table->char_syntax[(unsigned char) *s1] |= COMMENT_SYNTAX;
	if (0 != (table->comment_beg = (unsigned char) *s1))
	  {
	     table->comment_beg2 = (unsigned char) *(s1 + 1);
	     table->char_syntax[(unsigned char) *(s1 + 1)] |= COMMENT_SYNTAX;
	  }
	
	if (table->comment_beg2) table->flags |= MULTICHAR_TYPE;
	
	if ((table->comment_end == 0)
	    || (table->comment_end == '\n'))
	  table->flags |= EOL_COMMENT_TYPE;
	
	break;
	
      case '\\':
	if (SLang_pop_integer (&c2)) break;
	table->char_syntax[(unsigned char) c2] |= QUOTE_SYNTAX;
	table->quote_char = (unsigned char) c2;
	break;
	
      case '#':
	if (SLang_pop_integer (&c2)) break;
	table->preprocess = (unsigned char) c2;
	break;
      
      case '\'':
	if (SLang_pop_integer (&c2)) break;
	table->char_syntax[(unsigned char) c2] |= STRING_SYNTAX;
	table->char_char = (unsigned char) c2;
	break;
      case '"':
	if (SLang_pop_integer (&c2)) break;
	table->char_syntax[(unsigned char) c2] |= STRING_SYNTAX;
	table->string_char = (unsigned char) c2;
	break;
	
      case '<':
      case '>':
	if (SLpop_string (&s1)) break;
	s2 = s1;
	while (*s2 != 0)
	  {
	     if (*(s2 + 1) == 0) break;
	     table->char_syntax[(unsigned char) *s2] |= HTML_START_SYNTAX;
	     table->char_syntax[(unsigned char) *(s2 + 1)] |= HTML_END_SYNTAX;
	     s2 += 2;
	  }
	s2 = NULL;
	break;
	
      case '(':
      case ')':
	if (SLpop_string (&s2)) break;
	if (SLpop_string (&s1)) break;
	
	i = strlen (s1);
	if (i != strlen (s2))
	  {
	     msg_error ("Delimiter set does not match.");
	  }
	while (i > 0)
	  {
	     unsigned char ch1, ch2;
	     i--;
	     ch1 = (unsigned char) s1[i]; ch2 = (unsigned char) s2[i];
	     table->char_syntax[ch1] |= OPEN_DELIM_SYNTAX;
	     table->char_syntax[ch2] |= CLOSE_DELIM_SYNTAX;
	     table->matching_delim[ch2] = ch1;
	     table->matching_delim[ch1] = ch2;
	  }
	break;
	
      case '+':
	if (SLpop_string (&s1)) break;
	for (i = 0; i < 256; i++) table->char_syntax[i] &= ~OP_SYNTAX;

	s = (unsigned char *) s1;
	while (*s)
	  {
	     table->char_syntax[*s] |= OP_SYNTAX;
	     s++;
	  }
	break;
	
      case '0':
	if (SLpop_string (&s1)) break;
	SLmake_lut (lut, (unsigned char *) s1, 0);

	for (i = 0; i < 256; i++)
	  {
	     if (lut[i]) table->char_syntax[i] |= NUMBER_SYNTAX;
	     else table->char_syntax[i] &= ~NUMBER_SYNTAX;
	  }
	break;
	
      case ',':
	if (SLpop_string (&s1)) break;
	s = (unsigned char *) s1;
	for (i = 0; i < 256; i++) table->char_syntax[i] &= ~DELIM_SYNTAX;
	while (*s)
	  {
	     table->char_syntax[*s] |= DELIM_SYNTAX;
	     s++;
	  }
	break;
	
      case 'w':
	if (SLpop_string (&s1)) break;
	SLmake_lut (lut, (unsigned char *) s1, 0);

	for (i = 0; i < 256; i++)
	  {
	     if (lut[i]) table->char_syntax[i] |= WORD_SYNTAX;
	     else table->char_syntax[i] &= ~WORD_SYNTAX;
	  }
	break;

	
      default:
	msg_error ("Bad parameter to define_syntax");
     }
   
   if (s1 != NULL) SLFREE (s1);
   if (s2 != NULL) SLFREE (s2);
}

/*}}}*/

void use_syntax_table (char *s) /*{{{*/
{
   Syntax_Table_Type *table = find_syntax_table (s, 1);
   if (table == NULL) return;
   CBuf->syntax_table = table;
}

/*}}}*/

void create_syntax_table (char *name) /*{{{*/
{
   Syntax_Table_Type *table;
   
   if (NULL != find_syntax_table (name, 0)) return;
   
   if (NULL == (table = (Syntax_Table_Type *) SLMALLOC(sizeof (Syntax_Table_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   
   SLMEMSET ((char *) table, 0, sizeof (Syntax_Table_Type));
   table->next = Syntax_Tables;
   Syntax_Tables = table;
   strncpy (table->name, name, 15);
   table->name[15] = 0;
}

/*}}}*/

void init_syntax_tables (void) /*{{{*/
{
   unsigned short *a;
   unsigned char *m;
   
   Default_Syntax_Table = (Syntax_Table_Type *) SLMALLOC (sizeof (Syntax_Table_Type));
   if (Default_Syntax_Table == NULL) return;
   SLMEMSET ((char *) Default_Syntax_Table, 0, sizeof (Syntax_Table_Type));
   a = Default_Syntax_Table->char_syntax;
   m = Default_Syntax_Table->matching_delim;
   
   a [(unsigned char) '['] = OPEN_DELIM_SYNTAX; m[(unsigned char) '['] = ']';
   a [(unsigned char) ']'] = CLOSE_DELIM_SYNTAX; m[(unsigned char) ']'] = '[';
   a [(unsigned char) '('] = OPEN_DELIM_SYNTAX; m[(unsigned char) '('] = ')';
   a [(unsigned char) ')'] = CLOSE_DELIM_SYNTAX; m[(unsigned char) ')'] = '(';
   a [(unsigned char) '{'] = OPEN_DELIM_SYNTAX; m[(unsigned char) '{'] = '}';
   a [(unsigned char) '}'] = CLOSE_DELIM_SYNTAX; m[(unsigned char) '}'] = '{';
}

/*}}}*/

void define_keywords (char *name, char *kwords, int *lenp, int *tbl_nump) /*{{{*/
{
   char *kw;
   int len;
   int kwlen;
   unsigned int table_number = (unsigned int) *tbl_nump;
   Syntax_Table_Type *table = find_syntax_table (name, 1);
   
   if (table == NULL) return;
   
   if (table_number >= MAX_KEYWORD_TABLES)
     {
	msg_error ("Table number too high.");
	return;
     }
   
   len = *lenp;
   
   if ((len < 1) || (len > MAX_KEYWORD_LEN))
     {
	msg_error ("Keyword length not supported.");
	return;
     }
   
   kwlen = strlen (kwords);
   if (kwlen % len)
     {
	msg_error ("Keyword list is improperly formed.");
	return;
     }
   
   len--;
   kw = table->keywords[table_number][len];
   if (kw == NULL) SLang_push_string ("");
   else SLang_push_malloced_string (kw);
   
   kw = (char *) SLMALLOC (kwlen + 1);
   if (kw == NULL) 
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   
   strcpy (kw, kwords);
   
   table->keywords[table_number][len] = kw;
}

/*}}}*/

   
   
