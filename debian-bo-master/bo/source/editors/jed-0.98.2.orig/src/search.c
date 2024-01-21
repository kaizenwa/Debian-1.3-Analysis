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
#include "buffer.h"
#include "vfile.h"
#include "search.h"
#include "misc.h"
#include "ins.h"
#include "paste.h"
#include "ledit.h"

/*}}}*/

int Case_Sensitive = 0;
static SLsearch_Type Search_Struct;

int search(char *str, int dir, int n) /*{{{*/
{
   unsigned char *beg, *end, *p;
   Line *line;
   int key_len;
   unsigned int num = 0;

   if ((key_len = SLsearch_init (str, dir, Case_Sensitive, &Search_Struct)) <= 0)
     return 0;
   
   line = CLine;

   if (dir == 1)
     {
	beg = line->data + Point;
	end = line->data + line->len;
	do
	  {
	     if (NULL != (p = SLsearch (beg, end, &Search_Struct)))
	       {
		  CLine = line;
		  LineNum += num;
		  Point = (int) (p - line->data);
		  return key_len;
	       }
	     line = line->next; num++;
	     if (line == NULL) return 0;
	     beg = line->data;
	     end = line->data + line->len;
	  }
	while (--n);
     }
   else if (dir == -1)
     {
	end = beg = line->data;
	if (Point)
	  {
	     int tmp = Point - 1 + key_len;
	     if (tmp > line->len) tmp = line->len;
	     end += tmp;
	  }
	
	do
	  {
	     if (NULL != (p = SLsearch (beg, end, &Search_Struct)))
	       {
		  CLine = line;
		  LineNum -= num;

		  Point = (int) (p - line->data);
		  return (key_len);
	       }
	     line = line->prev;
	     num++;
	     if (line == NULL) return(0);
	     beg = line->data;
	     end = line->data + line->len;
	  }
	while (--n);
     }
   return(0);
}

/*}}}*/

int search_forward(char *s) /*{{{*/
{
   return search(s, 1, 0);
}

/*}}}*/

int search_backward(char *s) /*{{{*/
{
   return search(s, -1, 0);
}

/*}}}*/

int forward_search_line(char *s) /*{{{*/
{
   return search(s, 1, 1);
}

/*}}}*/

int backward_search_line(char *s) /*{{{*/
{
   return( search(s, -1, 1));
}

/*}}}*/

#ifdef upcase
#undef upcase
#endif
#define upcase(ch) (cs ? ch : UPPER_CASE(ch))

static int bol_search(char *str, int dir) /*{{{*/
{
   Line *tthis;
   int max, n, cs = Case_Sensitive;
   unsigned int num;
   unsigned char *s, ch1, ch2;
   
   max = strlen(str);
   if ((dir > 0) && Point)
     {
	num = 1;
	tthis = CLine->next;
     }
   else if ((dir < 0) && !Point)
     {
	tthis = CLine->prev;
	num = 1;
     }
   else
     {
	tthis = CLine;
	num = 0;
     }   
	
   while (tthis != NULL)
     {
	if (max <= tthis->len)
	  {
	     s = tthis->data;
	     for (n = 0; n < max; n++)
	       {
		  ch1 = upcase(s[n]);
		  ch2 = upcase(str[n]);
		  if (ch1 != ch2) break;
	       }
	     if (n == max)
	       {
		  Point = 0;
		  CLine = tthis;
		  if (dir > 0) LineNum += num; else LineNum -= num;
		  return max;
	       }
	  }
	num++;
	if (dir > 0) tthis = tthis->next; else tthis = tthis->prev;
     }
   return(0);
}

/*}}}*/

int bol_fsearch(char *s) /*{{{*/
{
   return bol_search(s, 1);
}

/*}}}*/

int bol_bsearch(char *s) /*{{{*/
{
   return bol_search(s, -1);
}

/*}}}*/

static SLRegexp_Type reg;

static int re_search_dir(unsigned char *pat, int dir) /*{{{*/
{
   int psave, skip, n, epos;
   unsigned char rbuf[512], *match;
   Line *l;
   
   reg.case_sensitive = Case_Sensitive;
   reg.buf = rbuf;
   reg.pat = pat;
   reg.buf_len = 512;
   
   if (SLang_regexp_compile(&reg)) 
     {
	msg_error("Unable to compile pattern.");
	return(0);
     }
   
   if (reg.osearch)
     {
	if (reg.must_match_bol)
	  {
	     if (!bol_search((char *) pat, dir)) return (0);
	  }
	else if (!search((char *) pat, dir, 0)) return (0);
	
	reg.beg_matches[0] = Point;
	n = strlen((char *) pat);
	reg.end_matches[0] = n;
	return n + 1;
     }

   if (reg.must_match_bol)
     {
	if (dir < 0) 
	  {
	     if (Point == 0)
	       {
		  if (!backwchars(&Number_One)) return 0;
	       }
	  }
	else if (Point)
	  {
	     if (CLine->next == NULL) return (0);
	     CLine = CLine->next; LineNum++; Point = 0;
	  }
     }
   
   
   if (reg.must_match && (0 != reg.must_match_str[1])) skip = 0; else skip = 1;
   while (1)
     {
	psave = Point; 
	if (!skip)
	  {
	     l = CLine;
	     if (!search((char *) reg.must_match_str, dir, 0)) return (0);

	     if (l != CLine)
	       {
		  if (dir < 0) eol(); else Point = 0;
		  /* if ((dir == -1) && (!reg.must_match_bol)) eol(); else Point = 0; */
		  psave = Point;
	       }
	  }
	
	Point = psave;
	if (dir == 1)
	  {
	     match = SLang_regexp_match(CLine->data + Point, CLine->len - Point, &reg);
	     if (match != NULL)
	       {
		  /* adjust offsets */
		  reg.offset = Point;
	       }
	  }
	
	else if (NULL != (match = SLang_regexp_match(CLine->data,
						     Point, /* was CLine->len */
						     &reg)))
	  {
	     if (Point && (reg.beg_matches[0] >= Point)) match = NULL;
	     else if (reg.must_match_bol == 0)
	       {
		  epos = Point - 1;
		  /* found a match on line now find one closest to current point */
		  while (epos >= 0)
		    {
		       match = SLang_regexp_match(CLine->data + epos, 
						  Point - epos, /* was: CLine->len - epos, */
						  &reg);
		       if (match != NULL) break;
		       epos--;
		    }
	       }
	  }
	if (match != NULL)
	  {
	     Point = (int) (match - CLine->data);
	     n = reg.end_matches[0];
	     return (n + 1);
	  }
	if (dir > 0)
	  {
	     if (CLine->next == NULL) break;
	     CLine = CLine->next; LineNum++; Point = 0;
	  }
	else 
	  {
	     if (CLine->prev == NULL) break;
	     CLine = CLine->prev; LineNum--;
	     eol ();
	  }
     }
   return (0);
}

/*}}}*/

int re_search_forward(char *pat) /*{{{*/
{
   int n, p, len;
   Line *l;
   
   p = Point; n = LineNum; l = CLine;
   if (0 != (len = re_search_dir((unsigned char *) pat, 1))) return (len);
   Point = p; LineNum = n; CLine = l;
   return (0);
}

/*}}}*/

int re_search_backward(char *pat) /*{{{*/
{
   int n, p, len;
   Line *l;
   
   p = Point; n = LineNum; l = CLine;
   if (0 != (len = re_search_dir((unsigned char *) pat, -1))) return (len);
   Point = p; LineNum = n; CLine = l;
   return (0);
}

/*}}}*/

int replace_match(char *s, int *literal) /*{{{*/
{
   int n, nmax;
   char ch;
   unsigned char *p;
   
   if ((reg.pat == NULL) || (reg.beg_matches[0] == -1) 
       || (reg.beg_matches[0] + reg.offset + reg.end_matches[0] >= (unsigned int) CLine->len)) 
     return 0;
   
   if (*literal)
     {
	Point = reg.beg_matches[0] + reg.offset;
	n = reg.end_matches[0];
	generic_deln(&n);
	insert_string(s);
	return (1);
     }
   /* This is painful --- \& means whole expression, \x x = 1..9 means a 
      sub expression */
   
   /* must work with relative numbers since ins/del may do a realloc */
   Point = reg.end_matches[0] + reg.offset + reg.beg_matches[0];
   while ((ch = *s++) != 0)
     {
	if ((ch != '\\') || ((ch = *s++) == '\\'))
	  {
	     if (ch != '\n') ins(ch);
	     /* Note that I should do a 'newline' call.  However, as soon as
	      * I do this, I lose the nth_match strings.  Clearly, I need to
	      * re-think this!  */
	     continue;
	  }
	if (ch == 0) break;
	if (ch == '&') ch = '0';
	
	if ((ch >= '0') && (ch <= '9'))
	  {
	     nmax = ch - '0';
	     if ((n = reg.beg_matches[nmax]) == -1) continue;
	     nmax = reg.end_matches[nmax] + reg.beg_matches[nmax];
	  }
	else continue;
	
	while (n < nmax)
	  {
	     p = CLine->data + reg.offset;
	     ins((char) *(p + n));
	     n++;
	  }
     }
   push_spot();
   Point = reg.beg_matches[0] + reg.offset;
   n = reg.end_matches[0];
   generic_deln(&n);
   pop_spot();
   return (1);
}

/*}}}*/

static int push_string(char *b, int n) /*{{{*/
{
   char *s;
   s = (char *) SLMALLOC(n + 1);
   if (s == NULL)
     {
	SLang_Error = SL_MALLOC_ERROR;
	return (0);
     }
   if (n) SLMEMCPY(s, b, n);
   *(s + n) = 0;
   
   SLang_push_malloced_string (s);
   return (SLang_Error == 0);
}

/*}}}*/

void regexp_nth_match (int *np) /*{{{*/
{
   int b = 0, n = *np;
   
   if ((reg.pat == NULL) || (reg.beg_matches[0] == -1)
       || (reg.beg_matches[0] + reg.offset + reg.end_matches[0] > (unsigned int) CLine->len))
     {
	SLang_Error = UNKNOWN_ERROR;
	return;
     }

   if ((n <= 0) || (n > 9)) n = 0;
   else
     {
	if ((b = reg.beg_matches[n]) == -1) n = 0;
	else
	  {
	     n = reg.end_matches[n];
	  }
     }
   b += reg.offset;
   push_string((char *) CLine->data + b, n);
}

/*}}}*/

int search_file(char *file, char *pat, int *np) /*{{{*/
{
   unsigned char rbuf[512], *buf;
   unsigned int n;
   VFILE *vp;
   int n_matches = 0, n_max = *np, key_len = 1;
   
   reg.case_sensitive = Case_Sensitive;
   reg.buf = rbuf;
   reg.pat = (unsigned char *) pat;
   reg.buf_len = 512;
   
   if (SLang_regexp_compile(&reg)) 
     {
	msg_error("Unable to compile pattern.");
	return(0);
     }
   
   if (reg.osearch)
     {
        key_len = SLsearch_init ((char *) reg.pat, 1, reg.case_sensitive, &Search_Struct);
     }
   else if (reg.must_match)
     {
	key_len = SLsearch_init ((char *) reg.must_match_str, 1, reg.case_sensitive, &Search_Struct);
     }

   if (key_len <= 0) return 0;
   if (NULL == (vp = vopen(file, 0, VFILE_TEXT))) return(0);
   while (NULL != (buf = (unsigned char *) vgets(vp, &n)))
     {
	if (reg.must_match)
	  {
	     if ((unsigned int) key_len > n) continue;
	     if (NULL == SLsearch (buf, buf + n, &Search_Struct))
	       {
		  continue;
	       }
	     if (reg.osearch) 
	       {
		  goto match_found;
	       }
	  }
	
	if (!SLang_regexp_match(buf, (int) n, &reg)) continue;
	
	match_found:
	n_matches++;
	
	n_max--;
	if (!push_string((char *) buf, n) || (n_max <= 0)) break;
     }
   vclose(vp);
   return(n_matches);
}

/*}}}*/

int insert_file_region (char *file, char *rbeg, char *rend) /*{{{*/
{
   VFILE *vp;
   unsigned int n;
   unsigned int len = (unsigned int) strlen (rbeg);
   int num = 0;
   unsigned char *buf;
   
   if (NULL == (vp = vopen(file, 0, VFILE_TEXT))) return (-1);
   
   while (NULL != (buf = (unsigned char *) vgets(vp, &n)))
     {
	if ((len == 0) || 
	    ((n >= len) && !strncmp ((char *) buf, rbeg, len)))
	  {
	     Suspend_Screen_Update = 1;
	     quick_insert(buf, (int) n);
	     num++;
	     
	     len = (unsigned int) strlen (rend);
	     
	     while (NULL != (buf = (unsigned char *) vgets(vp, &n)))
	       {
		  if (len &&
		      ((n >= len) && !strncmp ((char *) buf, rend, len)))
		    break;

		  quick_insert(buf, (int) n);
		  if (SLang_Error) break;
		  num++;
	       }
	     break;
	  }
     }
   vclose (vp);
   return num;
}

/*}}}*/

