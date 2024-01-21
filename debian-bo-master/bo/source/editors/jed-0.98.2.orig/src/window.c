/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <string.h> 

#include "buffer.h"
#include "window.h"
#include "screen.h"
#include "misc.h"
#include "ledit.h"
#include "sysdep.h"
#include "display.h"
#include "paste.h"

/*}}}*/

extern Window_Type *The_MiniWindow;
Window_Type *JWindow;
int Top_Window_Row = 2;

Window_Type *create_window(int top, int rows, int col, int width) /*{{{*/
{
    Window_Type *w;

    if (NULL == (w = (Window_Type *) SLMALLOC(sizeof(Window_Type))))
      {
	  exit_error("create_window: malloc error.", 0);
      }

   SLMEMSET ((char *) w, 0, sizeof (Window_Type));
   if (top < 1) top = 1;
   w->top = top;
   if (rows < 1) rows = 1;
   w->rows = rows;
   if (width < 1) width = 1;
   w->width = width;
   w->column = col;

   return(w);
}

/*}}}*/
void window_buffer(Buffer *b) /*{{{*/
{
   if (JWindow == NULL)
     {
	JWindow = create_window(Top_Window_Row, 
				*tt_Screen_Rows - 2 - (Top_Window_Row - 1),
				1, *tt_Screen_Cols);
	JWindow->next = JWindow;
     }

   touch_window();
   
   JWindow->beg.line = JWindow->mark.line = b->line;
   JWindow->beg.point = JWindow->mark.point = b->point;
   JWindow->beg.n = JWindow->mark.n = b->linenum + b->nup;
   
   JWindow->buffer = b;
   JWindow->trashed = 1;
}

/*}}}*/

#if JED_HAS_SUBPROCESSES
void move_window_marks (int all) /*{{{*/
{
   Window_Type *w = JWindow;
   if (w == NULL) return;
   do 
     {
	if (w->buffer == CBuf)
	  {
	     w->mark.point = Point;
	     w->mark.line = CLine;
	     w->mark.n = LineNum + CBuf->nup;
	     if (all == 0) break;
	  }
	w = w->next;
     }
   while (w != JWindow);
}

/*}}}*/
#endif

int other_window() /*{{{*/
{
   switch_to_buffer(JWindow->buffer);
   /* CBuf->line = CLine;
   CBuf->point = Point; */
   JWindow->mark.point = Point;
   JWindow->mark.line = CLine;
   JWindow->mark.n = LineNum + CBuf->nup;
   /* JWindow->buffer = CBuf; */

   JWindow = JWindow->next;
   switch_to_buffer(JWindow->buffer);
   /* CBuf = JWindow->buffer; */
   Point = JWindow->mark.point;
   CLine = JWindow->mark.line;
   LineNum = JWindow->mark.n - CBuf->nup;
   return(1);
}

/*}}}*/
static int find_screen_line(void) /*{{{*/
{
   int i, imax;
   imax = *tt_Screen_Rows - 2;
   for (i = 0; i < imax; i++)
     {
	if (JScreen[i].line == CLine) return i + 1;
     }
   return 0;
}

/*}}}*/
int split_window (void) /*{{{*/
{
   int n, top, width, row;
   Window_Type *w, *neew;
   Line *l, *cline;

    if (JWindow->rows < 5)
      {
	  msg_error("Window too small.");
	  return(0);
      }

   switch_to_buffer(JWindow->buffer);
   n = JWindow->rows / 2;
   top = JWindow->top + n + 1;
   width = JWindow->width;
   n = JWindow->rows - n - 1;
   JWindow->rows = JWindow->rows / 2;

   if (NULL == (JWindow->beg.line = find_top()))
     JWindow->beg.line = CLine;
   
   w = JWindow->next;
   JWindow->next = neew = create_window(top, n, JWindow->column, width);
   
   neew->next = w;
   neew->buffer = CBuf;
   neew->mark.point = Point;
   neew->mark.line = CLine;
   neew->mark.n = LineNum + CBuf->nup;

   other_window();
   touch_window();
   
     
#if JED_HAS_LINE_ATTRIBUTES
   if (NULL == (cline = jed_find_non_hidden_line (CLine)))
     cline = CLine;
#else
   cline = CLine;
#endif
   
   l = find_top ();
   if (l == NULL)
     l = cline;
   
   JWindow->beg.line = cline;
		
   n = 0;
   while (l != cline)
     {
	l = l->next;
	n++;
     }
   JWindow->beg.n = LineNum + CBuf->nup - n;
   
   /* Try to leave Point on same line of display if possible */
   if ((row = find_screen_line ()) > 0)
     {
	w = JWindow;
	do
	  {
	     if ((JWindow->buffer == CBuf) && (JWindow->top <= row)
		 && (JWindow->top + JWindow->rows > row)) break;
	     other_window();
	  }
	while (w != JWindow);
     }

    return(1);
}

/*}}}*/
int one_window (void) /*{{{*/
{
   Window_Type *w, *next, *mini;
   Buffer *b;
   mini = NULL;
   if (JWindow->top == *tt_Screen_Rows) return(0);  /* mini-buffer */
   w = JWindow->next;
   b = JWindow->buffer;
   while(w != JWindow)
     {
	next = w->next;
	if (w != The_MiniWindow) 
	  {
	     if (w->buffer != b) touch_window_hard (w, 0);
	     SLFREE(w);
	  }
	else mini = w;
	w = next;
     }
   if (mini == NULL) mini = JWindow;
   JWindow->next = mini;
   mini->next = JWindow;
   JWindow->top = Top_Window_Row;
   JWindow->rows = *tt_Screen_Rows - 2 - (Top_Window_Row - 1);
   touch_window();
   return(1);
}

/*}}}*/
int enlarge_window() /*{{{*/
{
   Window_Type *w, *w1;
   int min = 2;

   if (JWindow == The_MiniWindow) return(0);
   /* if (IS_MINIBUFFER) return(0); */
   if (JWindow == JWindow->next) return(0);
   w = JWindow->next;
   while(w->rows <= min) w = w->next;
   if (w == JWindow) return(0);

   if (w->top < JWindow->top)
     {
	w->rows -= 1;
	JWindow->rows += 1;
	do
	  {
	     w = w->next;
	     w->top -= 1;
	  }
	while (w != JWindow);
     }
   else
     {
	JWindow->rows += 1;
	w1 = JWindow;
	while(w1 != w)
	  {
	     w1 = w1->next;
	     w1->top += 1;
	  }
	w->rows -= 1;
     }
   w = JWindow;
   do
     {
	touch_window();
	JWindow = JWindow->next;
     }
   while (w != JWindow);
   return(1);
}

/*}}}*/
static void adjust_windows(int height) /*{{{*/
{
   Window_Type *w = JWindow;
   int rows;

   do
     {
	if (w->rows + w->top + 1 == *tt_Screen_Rows)
	  {
	     rows = height - 1 - w->top;
	     if (rows > 1)
	       {
		  w->rows = rows;
		  return;
	       }
	     while(JWindow->top != Top_Window_Row) other_window();
	     one_window();
	     JWindow->rows = height - 2 - (Top_Window_Row - 1);
	     if (JWindow->rows < 1) JWindow->rows = 1;
	     return;
	  }
	w = w->next;
     }
   while (w != JWindow);
   /* not reached! */
}

/*}}}*/

#if JED_HAS_COLOR_COLUMNS
static void adjust_buffer_column_attributes (int width) /*{{{*/
{
   Buffer *b = CBuf;
   unsigned char *p, ch, *pmax;
   int old_width = *tt_Screen_Cols;
   if (old_width <= 0) return;
   do
     {
	if (NULL != (p = b->column_colors))
	  {
	     p = b->column_colors = (unsigned char *) SLREALLOC (p, width);
	     if (p != NULL)
	       {
		  pmax = p + width;
		  p += old_width;
		  ch = *(p - 1);
		  while (p < pmax) *p++ = ch;
	       }
	  }
	b = b->next;
     }
   while (b != CBuf);
}

/*}}}*/
#endif

void change_screen_size(int width, int height) /*{{{*/
{
   Window_Type *w;

   if (JWindow == NULL) return;

   if (height < 5) height = 5;
   if (width < 5) width = 5;
   
   if (height != *tt_Screen_Rows)
     {
	adjust_windows(height);
     }
#if JED_HAS_COLOR_COLUMNS
   adjust_buffer_column_attributes (width);
#endif
   reset_display();
   *tt_Screen_Cols = width;
   *tt_Screen_Rows = height;
   init_display(0);
   w = JWindow;
   do
     {
	JWindow->width = width;

	/* touch_window(); */
	JWindow = JWindow->next;
     }
   while(w != JWindow);
   if (The_MiniWindow != NULL)
     {
	The_MiniWindow->top = height;
	The_MiniWindow->width = width;
     }
   /* cls();
   update((Line*) NULL); */
   redraw_screen (1);
}

/*}}}*/
int buffer_visible(Buffer *b) /*{{{*/
{
   Window_Type *w = JWindow;
   int n = 0;
   if (b == NULL) return 0;
   
   do
     {
	if (w->buffer == b) n++;
	w = w->next;
     }
   while (w != JWindow);
   return n;
}

/*}}}*/
int delete_window() /*{{{*/
{
   Window_Type *tthis, *prev, *next;
   int nr1;
   
   tthis = JWindow;
   next = tthis->next;
   if ((MiniBuffer_Active && ((tthis == The_MiniWindow) || (tthis == next->next)))
       || (tthis == next)) return(0);
   
   
   
   nr1 = tthis->top + tthis->rows + 1;
   if (nr1 != *tt_Screen_Rows)
     {
	while (JWindow->top != nr1) other_window();
	JWindow->top = tthis->top;
     }
   else
     {
	while(JWindow->top + JWindow->rows + 1 != tthis->top) other_window();
     }
   
   JWindow->rows += tthis->rows + 1;
   touch_window();
   
   prev = next;
   while(prev->next != tthis) prev = prev->next;
   prev->next = next;

   SLFREE(tthis);
   return(1);
}

/*}}}*/
void touch_window_hard(Window_Type *w, int all) /*{{{*/
{
   int i;
   Window_Type *wsave = w;
   
   do 
     {
	for (i = 0; i < w->rows; i++)
	  {
	     JScreen[i + w->top - 1].flags = 1;
	     JScreen[i + w->top - 1].line = NULL;
	  }
	w->trashed = 1;
	w = w->next;
     }
   while (all && (w != wsave));
}

/*}}}*/
void touch_screen_for_buffer(Buffer *b) /*{{{*/
{
   Window_Type *w;
   
   w = JWindow;
   do
     {
	if (w->buffer == b)
	  {
	     touch_window_hard (w, 0);
	  }
	w = w->next;
     }
   while(w != JWindow);
}

/*}}}*/
int is_line_visible (int lnum) /*{{{*/
{
   int n = JWindow->rows;
   Line *l, *beg = JWindow->beg.line;
   
   push_spot ();
   goto_line (&lnum);
   l = CLine;
   pop_spot ();
   
#if JED_HAS_LINE_ATTRIBUTES
   if (l->flags & JED_LINE_HIDDEN) return 0;
#endif
   
   while (n && (beg != NULL))
     {
	if (l == beg) return 1;
	
#if JED_HAS_LINE_ATTRIBUTES
	if (0 == (beg->flags & JED_LINE_HIDDEN))
#endif
	  n--;
	
	beg = beg->next;
     }
   return 0;
}

/*}}}*/
