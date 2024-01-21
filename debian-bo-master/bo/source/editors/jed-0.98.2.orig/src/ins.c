/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1996 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <string.h>

#include "buffer.h"
#include "ins.h"
#include "line.h"
#include "screen.h"
#include "window.h"
#include "misc.h"
#include "paste.h"
#include "vterm.h"
#include "ledit.h"
#include "undo.h"

/*}}}*/

int Suspend_Screen_Update = 0;
int No_Screen_Update;

static void update_generic_marks(register Mark *m, register int type, int n) /*{{{*/
{
   int tmp;
   if (m == NULL) return;
   
   if (type == CINSERT) while(m != NULL)
     {
	if ((m->line == CLine) && (m->point > Point))
	  {
	     m->point += n;
	  }
	m = m->next;
     }
   else if (type == CDELETE) while(m != NULL)
     {
	if ((m->line == CLine) && (m->point > Point))
	  {
	     /* BCC generates wrong code here with optimization */
	     tmp = m->point;
	     tmp -= n;
	     if (tmp < Point) tmp = Point;
	     m->point = tmp;
	     
	     /*
	      BAD CODE:
	     m->point -= n;
	     if (m->point < Point) m->point = Point; */
	  }
	m = m->next;
     }
   
   
   /* called by line deletion routines */
   else if (type == LDELETE) while(m != NULL)
     {
	if (CLine == m->line)
	  {
	     if (CLine->prev != NULL)
	       {
		  m->line = CLine->prev;
	       }
	     else m->line = CBuf->beg;
	     m->point = 0;
	  }
	if (LineNum + CBuf->nup <= m->n) m->n -= 1;
	m = m->next;
     }
   
   /* newline added-- affects only marks onward from insertion point */
    else if (type == NLINSERT) while(m != NULL)
      {
	 /* This is a bit controversial if the mark corresponds to JWindow->beg.
	    In this case, JWindow beg gets shifted if Point = 0.  */
	 
	 if ((LineNum + CBuf->nup < m->n) 
	     || ((LineNum + CBuf->nup == m->n) && (m->point > Point))) m->n += 1;
	    
	 if ((m->line == CLine) && (m->point > Point))
            {
	       m->line = CLine->next;
	       m->point -= Point;
	       if (m->point > m->line->len) m->point = m->line->len;
            }
	 m = m->next;
      }
   
   /* deletion performed at end of a line (CLine->prev)  */
    else if (type == NLDELETE) while(m != NULL)
      {
	 if (m->line == CLine)
	   {
	      m->line = CLine->prev;
	      m->point += Point;
	   }
	 if (LineNum + CBuf->nup <= m->n) m->n -= 1;
	 m = m->next;
      }
}

/*}}}*/

void update_marks(int type, int n) /*{{{*/
{
   register Window_Type *w;
   register Buffer *b = CBuf;
   Mark *m;
#if JED_HAS_SAVE_NARROW
   Jed_Save_Narrow_Type *save_narrow;
#endif
   
   if (!n) return;
   Cursor_Motion = 0;
   if (b->flags & UNDO_ENABLED)
     {
	if (b->undo == NULL) create_undo_ring();
	Undo_Buf_Unch_Flag = !(b->flags & BUFFER_TRASHED);
     }
   
   mark_buffer_modified(&Number_One);
   if ((m = b->spots) != NULL) update_generic_marks(m, type, n);
   if ((m = b->marks) != NULL) update_generic_marks(m, type, n);
   if ((m = b->user_marks) != NULL) update_generic_marks(m, type, n);

#if JED_HAS_SAVE_NARROW
   save_narrow = b->save_narrow;
   while (save_narrow != NULL)
     {
	update_generic_marks (save_narrow->beg, type, n);
	update_generic_marks (save_narrow->end, type, n);
	save_narrow = save_narrow->next;
     }
#endif
   
   w = JWindow;
   do
     {
	if (w->buffer == b)
	  {
	     update_generic_marks(&w->mark, type, n);
	     update_generic_marks(&w->beg, type, n);
	  }

	w = w->next;
     }
   while (w != JWindow);

   if (!Suspend_Screen_Update) register_change(type);
}

/*}}}*/

void ins (char c) /*{{{*/
{
   register unsigned char *p, *p1, *p2;

    if (CLine == NULL)
      {
          exit_error("ins: CLine is NULL", 1);
      }

#ifdef KEEP_SPACE_INFO
   if (CLine->space <= CLine->len + 1)
     remake_line(CLine->space + 15);
#else
   remake_line (CLine->len + 1);
#endif

    p = CLine->data + Point;
    if (Point < CLine->len)
      {
          p1 = CLine->data + (CLine->len - 1);
	 p2 = p1 + 1;
          while(p1 >= p)
            {
	       *p2 = *p1;
	       p2 = p1;
	       p1--;
	       /* *(p1 + 1) = *p1; p1--; */
            }
      }
    *p = c;
    CLine->len += 1;
    update_marks(CINSERT,1);
   if ((c != '\n') || (CBuf == MiniBuffer)) record_insertion(1);
    Point++;
}

/*}}}*/

void del_newline(void) /*{{{*/
{
   CHECK_READ_ONLY_VOID
#if JED_HAS_LINE_ATTRIBUTES
   if ((CLine->next != NULL) && (CLine->next->flags & JED_LINE_READONLY))
     {
	msg_error (Line_Read_Only_Error);
	return;
     }
#endif

   if (!eol() || eobp()) return;
#ifdef DEBUG_JED
   if (CLine->len == 0)
     {
	exit_error("del(): empty line", 1);
     }
#endif
   
   CLine->len -= 1;
   update_marks(CDELETE,1);
   record_deletion((unsigned char *) "\n", 1);
   splice_line();
}

/*}}}*/

/* del *np chars up until newline.  Return actual number deleted */
int deln(int *np) /*{{{*/
{
   int n = *np;
   register int nn;
   register unsigned char *p, *pmax;
   
   if ((n == 0) || !CLine->len) return(0);
   
   nn = CLine->len - 1;
   p = CLine->data + nn;
   if ((*p == '\n') && (CBuf != MiniBuffer)) nn = nn - Point; else nn = nn - Point + 1;
   
   p = CLine->data + Point;

   nn = nn > n ? n : nn;
   if (!nn) return (0);
   update_marks(CDELETE, nn);
   record_deletion(p, nn);
   CLine->len -= nn;
   pmax = CLine->data + CLine->len;
   while (p < pmax)
     {
	*p = *(p + nn);
	p++;
     }
   return(nn);
}

/*}}}*/

/* delete n characters, crossing nl if necessary */
void generic_deln(int *np) /*{{{*/
{
   int n = *np;

   CHECK_READ_ONLY_VOID
   
   while ((n > 0) && (SLang_Error == 0))
     {
	if (eobp())
	  {
	     msg_error("End of Buffer.");
	     return;
	  }
	n -= deln(&n);
	if (n) del_newline();
	n--;
     }
}

/*}}}*/

void del() /*{{{*/
{
   generic_deln(&Number_One);
}

/*}}}*/

void quick_insert(register unsigned char *s, int n) /*{{{*/
{
   register unsigned char *p, *p1;
   int nl = 0;
   
   if (n == 0) return;
   if ((*(s + (n - 1)) == '\n') && (CBuf != MiniBuffer))
     {
	n--;
	nl = 1;
     }
   
#ifdef KEEP_SPACE_INFO
   if (CLine->space <= CLine->len + n + 1) remake_line(CLine->space + n + 8);
#else
   if (n) remake_line (CLine->len + n);
#endif
    
   
   /* shove n chars over to make space */
   p = CLine->data + Point;
   if (Point < CLine->len)   /* could be equal for last line of buffer */
     {
	p1 = CLine->data + CLine->len - 1;
	while(p1 >= p)
	  {
	     *(p1 + n) = *p1;
	     p1--;
	  }
     }
   CLine->len += n;
   SLMEMCPY((char *) p, (char *) s, n);
   
   update_marks(CINSERT, n);
   record_insertion(n);
   Point += n;

   if (nl)
     {
	split_line();
	ins('\n');
	CLine = CLine->next;
	LineNum++;
	Point = 0;
     }
}

/*}}}*/

void ins_chars(unsigned char *ss, int n) /*{{{*/
{
   register unsigned char nl, *pmax;
   register unsigned char *p, *p1, *s = ss;
   int n1;
   
   if (CBuf == MiniBuffer) nl = 0; else nl = '\n';
   
   p1 = s;
   while (1)
     {
	p = p1;
	/* count the number until a new line is reached */
	pmax = p1 + n;
	while((p1 < pmax) && (*p1 != nl)) p1++;
	
	n1 = (int) (p1 - p);
	if (p1 != pmax) n1++;
	quick_insert(p, n1);
	if (p1++ == pmax) return;
	n -= n1;
     }
}

/*}}}*/

/* This needs fixed to handle large n */
void ins_char_n_times(char c, int n) /*{{{*/
{
    char b[256], *p;
    int n1;

    if (n == 0) return;
    if (n > 255) n = 255;
    p = b;
    n1 = n;
    while(n1--) *p++ = c;
    ins_chars((unsigned char *) b, n);
}

/*}}}*/

void insert_buffer(Buffer *b) /*{{{*/
{
   Buffer *cb;

   if ((cb = CBuf) == b) return;

   switch_to_buffer(b);
   push_spot();
   bob(); push_mark();
   eob();
   copy_region_to_buffer(cb);
   pop_spot();
   switch_to_buffer(cb);

   touch_window();
}

/*}}}*/

void fast_ins(char ch) /*{{{*/
{
    if (JWindow == JWindow->next) No_Screen_Update = 1;
    vins(ch);
    ins(ch);
}

/*}}}*/

void fast_del() /*{{{*/
{
    if (JWindow == JWindow->next) No_Screen_Update = 1;
    vdel();
    del();
}

/*}}}*/

void replace_char(unsigned char ch) /*{{{*/
{
   if (ch == '\n') return;
   if (*(CLine->data + Point) == ch) return;
   ins(ch); del(); Point--;
}

/*}}}*/
