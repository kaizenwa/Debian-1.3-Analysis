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
#include "ins.h"
#include "line.h"
#include "paste.h"
#include "screen.h"
#include "misc.h"
#include "cmds.h"

/*}}}*/

/* SLang user object be larger than 128 */
#define JED_MARK_TYPE 128

Buffer *Paste_Buffer;
Buffer *Rectangle_Buffer;

/* This is used by the narrow command so that multiple windows are 
 * handled properly. The assumption is that we are dealing with a canonical
 * region */
      
static void touch_windows(void) /*{{{*/
{
   Window_Type *w;
   Line *l;
   unsigned int n;
   
   w = JWindow;
   JWindow = JWindow->next;
   while (JWindow != w)
     {
	if (CBuf == JWindow->buffer)
	  {
	     /* The mark is set with line at top of buffer */
	     l = CBuf->marks->line;
	     n = CBuf->marks->n;
	     while ((l != NULL) && (l != JWindow->mark.line)) l = l->next, n++;
	     if (l == NULL) 
	       {
		  JWindow->mark.line = CLine;
		  JWindow->mark.point = Point;
		  JWindow->mark.n = n - 1;
	       }
	     touch_window();
	  }
	JWindow = JWindow->next;
     }
   pop_mark(&Number_Zero);
}

/*}}}*/

#if 0
static int line_in_buffer(Line *line) /*{{{*/
{
   Line *l;
   
   l = CBuf->beg;
   while (l != NULL) if (l == line) return(1); else l = l->next;
   return(0);
}

/*}}}*/
#endif

/*{{{ Spot/Mark Functions */

static void init_mark (Mark *m, unsigned int flags) /*{{{*/
{
   m->line = CLine;
   m->point = Point;
   m->n = LineNum + CBuf->nup;
   m->flags = flags;
}

/*}}}*/

#if JED_HAS_SAVE_NARROW
static Mark *create_mark (unsigned int flags) /*{{{*/
{
   Mark *m;
   
   if (NULL == (m = (Mark *) SLMALLOC(sizeof(Mark))))
      {
	  exit_error("create-mark: malloc error", 0);
      }
   init_mark (m, flags);
   
   return m;
}

/*}}}*/
#endif   
/* with prefix argument, pop marks */
int set_mark_cmd (void) /*{{{*/
{
   Mark *m;
   if (Repeat_Factor != NULL)
     {
	while (CBuf->marks != NULL) pop_mark(&Number_Zero);
	Repeat_Factor = NULL;
	return(1);
     }
   
   if (CBuf->marks == NULL)
     {
	push_mark ();
     }

   m = CBuf->marks;
   
   init_mark (m, m->flags);
   
   if ((m->flags & VISIBLE_MARK) == 0) 
     {
	m->flags |= VISIBLE_MARK;
	CBuf->vis_marks++;
     }
   
   /* if (m != CBuf->marks) m->next = CBuf->marks;
    CBuf->marks = m; */
   if (Last_Key_Function == (FVOID_STAR) set_mark_cmd) message("Mark Set.");
   return(1);
}

/*}}}*/

static Mark *allocate_mark (Jed_Mark_Array_Type **ap, Mark **mp, unsigned int flags) /*{{{*/
{
   Jed_Mark_Array_Type *a, *b;
   Mark *m;
   
   a = *ap;
   if ((a == NULL)
       || (a->num_marks == JED_MAX_MARK_ARRAY_SIZE))
     {
	if (NULL == (b = (Jed_Mark_Array_Type *) SLMALLOC(sizeof(Jed_Mark_Array_Type))))
	  {
	     exit_error("allocate_mark: malloc error", 0);
	  }
	memset ((char *) b, 0, sizeof (Jed_Mark_Array_Type));
	b->next = a;
	a = b;
	*ap = a;
     }
   
   m = &a->marks[a->num_marks];
   a->num_marks += 1;
   
   m->next = *mp;
   *mp = m;
   
   init_mark (m, flags);
   
   return m;
}

/*}}}*/

static void deallocate_mark (Jed_Mark_Array_Type **ap, Mark **mp) /*{{{*/
{
   Jed_Mark_Array_Type *a;
   Mark *m;
   
   m = *mp;
   *mp = m->next;
   
   a = *ap;
   a->num_marks -= 1;
   
   if (a->num_marks == 0)
     {
	*ap = a->next;
	SLFREE (a);
     }
}

/*}}}*/

int push_spot() /*{{{*/
{
   (void) allocate_mark (&CBuf->spot_array, &CBuf->spots, 0);
   return 1;
}

/*}}}*/

int push_mark() /*{{{*/
{
   (void) allocate_mark (&CBuf->mark_array, &CBuf->marks, 0);
   return 1;
}

/*}}}*/

int goto_mark(Mark *m) /*{{{*/
{
   Line *l;
   int ret = -1;
   
   l = m->line;
   LineNum = m->n;
	
   if (LineNum <= CBuf->nup) bob();
   else if (LineNum > CBuf->nup + Max_LineNum) eob();
   else
     {
	CLine = l;
	Point = m->point;
	LineNum -= CBuf->nup;
	ret = 0;
     }
   return ret;
}

/*}}}*/

int pop_mark (int *go) /*{{{*/
{
   Mark *m;
   
   m = CBuf->marks;
   if (m == NULL) return(0);

   if (*go) goto_mark(m);
   if (m->flags & VISIBLE_MARK)
     {
	CBuf->vis_marks--;
	/* touch screen since region may be highlighted */
	if (CBuf->vis_marks == 0) touch_screen();
     }

   deallocate_mark (&CBuf->mark_array, &CBuf->marks);
   
   return 1;
}

/*}}}*/

int mark_spot() /*{{{*/
{
    push_spot();
    message("Spot Marked.");
    return(1);
}

/*}}}*/

static int pop_spot_go (int go) /*{{{*/
{
   Mark *m;
   
   m = CBuf->spots;
   if (m == NULL) return(0);
   
   if (go) goto_mark (m);

   deallocate_mark (&CBuf->spot_array, &CBuf->spots);
   return 1;
}

/*}}}*/

int pop_spot () /*{{{*/
{
   return pop_spot_go (1);
}

/*}}}*/

int exchange_point_mark(void) /*{{{*/
{
   Line *save_line;
   int save_point;
   unsigned int save_n;
   Mark *m;
   
   if ((m = CBuf->marks) == NULL) return(0);
   
   save_point = Point;
   save_line = CLine;
   save_n = LineNum + CBuf->nup;
   
   goto_mark (m);
   
   m->point = save_point; m->line = save_line; m->n = save_n;
   return(1);
}

/*}}}*/

/*}}}*/

/*{{{ Narrow/Widen/Region Functions */

 /*returns 0 if the mark is not set and gives error.  Exchanges point and mark
  * to produce valid region.  A valid region is one with mark
  * earlier in the buffer than point.  Always call this if using a region
  * which reqires point > mark.  Also, push spot first then pop at end. 
  */
int check_region(int *push) /*{{{*/
{
   register Line *beg, *tthis = CLine;
   int pbeg;

   if (CBuf->marks == NULL)
     {
	msg_error("Set mark first.");
	return(0);
     }

   if (*push) push_spot();
   beg = CBuf->marks->line;
   pbeg = CBuf->marks->point;

   if (beg == CLine)
     {
	if (pbeg <= Point) return(1);
     }

   else
     {
	while((beg != NULL) && (beg != tthis)) beg = beg->next;
	if (beg == tthis) return(1);
     }

   exchange_point_mark();
   return(1);
}

/*}}}*/

static int widen_buffer_lines (Buffer *b) /*{{{*/
{
   Narrow_Type *n;
   Buffer *save = CBuf;
   
   if (NULL == (n = b->narrow)) return(0);
   
   /* make sure buffer ends in final newline */
   
   switch_to_buffer(b);
   push_spot();
   eob();
   if ((n->end != NULL)
       && (!CLine->len || ('\n' != *(CLine->data + (CLine->len - 1)))))
     {
	unsigned int flags = CBuf->flags;
	ins('\n');
	CBuf->flags = flags;
     }
   
   pop_spot();
   
   if (n->end != NULL) n->end->prev = b->end;
   if (n->beg != NULL) n->beg->next = b->beg;
   b->end->next = n->end;
   b->beg->prev = n->beg;
   b->beg = n->beg1;
   if (n->end != NULL) b->end = n->end1;
   
   Max_LineNum += n->ndown + n->nup;
   LineNum += n->nup;
   
   /* adjust absolute offsets */
   b->nup -= n->nup;
   b->ndown -= n->ndown;
   b->narrow = n->next;
   
   SLFREE(n);
   switch_to_buffer(save);
   return(1);
}

/*}}}*/

int widen_buffer (Buffer *b) /*{{{*/
{
   unsigned int flags;
#if JED_HAS_LINE_ATTRIBUTES
   unsigned int line_flags;
#endif
   Buffer *save;
   
   if (b->narrow == NULL) return 0;

   if (b->narrow->is_region == 0)
     return widen_buffer_lines (b);
   
   flags = b->flags;
   b->flags &= ~READ_ONLY;
   
   save = CBuf;
   if (b != CBuf) switch_to_buffer (b);
   push_spot ();
   bob ();
   push_spot ();
   eob ();
   widen_buffer_lines (CBuf);
   
#if JED_HAS_LINE_ATTRIBUTES
   line_flags = CLine->flags;
   CLine->flags &= ~JED_LINE_READONLY;
#endif
   del ();
#if JED_HAS_LINE_ATTRIBUTES
   CLine->flags = line_flags;
#endif
   
   pop_spot ();
   prevline (&Number_One);
#if JED_HAS_LINE_ATTRIBUTES
   line_flags = CLine->flags;
   CLine->flags &= ~JED_LINE_READONLY;
#endif
   del ();
#if JED_HAS_LINE_ATTRIBUTES
   CLine->flags = line_flags;
#endif
   pop_spot ();
   if (save != CBuf) switch_to_buffer (save);
   
   b->flags = flags;
   return 1;
}

/*}}}*/

int narrow_to_region (void) /*{{{*/
{
   int pnt;
   Line *line;
   unsigned int flags;
#if JED_HAS_LINE_ATTRIBUTES
   unsigned int line_flags;
#endif
     
   if (0 == check_region (&Number_One))/* spot pushed */
     return 0;
   
   flags = CBuf->flags;
   CBuf->flags &= ~READ_ONLY;
   
   push_spot ();
   
   line = CLine;
   pnt = Point;
   
   pop_mark (&Number_One);
   /* Special case if region is empty */
   if ((CLine == line) && (pnt == Point))
     {
	pop_spot ();
	pop_spot ();

#if JED_HAS_LINE_ATTRIBUTES
	line_flags = CLine->flags;
	CLine->flags &= ~JED_LINE_READONLY;
#endif
	
	newline ();
	
#if JED_HAS_LINE_ATTRIBUTES
	CLine->prev->flags = line_flags;
#endif
	
	push_mark ();

	newline ();
	
#if JED_HAS_LINE_ATTRIBUTES
	CLine->flags = line_flags;
#endif
	prevline (&Number_One);

#if JED_HAS_LINE_ATTRIBUTES
	CLine->flags = line_flags;
#endif

	if (narrow_to_lines ())
	  CBuf->narrow->is_region = 1;
     }
   else
     {
#if JED_HAS_LINE_ATTRIBUTES
	line_flags = CLine->flags;
	CLine->flags &= ~JED_LINE_READONLY;
#endif
	newline ();
#if JED_HAS_LINE_ATTRIBUTES
	CLine->flags = line_flags;
	CLine->prev->flags = line_flags;
#endif

	push_mark ();
	pop_spot ();
	
#if JED_HAS_LINE_ATTRIBUTES
	line_flags = CLine->flags;
	CLine->flags &= ~JED_LINE_READONLY;
#endif
	newline ();
#if JED_HAS_LINE_ATTRIBUTES
	CLine->flags = line_flags;
#endif
	prevline (&Number_One);

#if JED_HAS_LINE_ATTRIBUTES
	CLine->flags = line_flags;
#endif
	if (narrow_to_lines ())
	  CBuf->narrow->is_region = 1;
	
	pop_spot ();
     }
   
   CBuf->flags = flags;
   return 1;
}

/*}}}*/

int widen () /*{{{*/
{
   return widen_buffer_lines (CBuf);
}

/*}}}*/

int widen_region (void) /*{{{*/
{
   return widen_buffer (CBuf);
}

/*}}}*/

/* not really a region of points but a region of lines. */
int narrow_to_lines () /*{{{*/
{
   Line *beg;
   Narrow_Type *nt;
   
   if (NULL == (nt = (Narrow_Type *) SLMALLOC(sizeof(Narrow_Type))))
     {
	msg_error("Malloc Error during narrow.");
	return(0);
     }
   
   if (!check_region(&Number_One)) return(0);       /* spot pushed */

   push_spot();
   pop_mark(&Number_One);
   push_mark();			       /* popped and used in touch_windows! */
   beg = CLine;
   nt->nup = LineNum - 1;
   
   pop_spot();  /* eor now */
   
   nt->ndown = Max_LineNum - LineNum;

   Max_LineNum = LineNum = LineNum - nt->nup;
   CBuf->nup += nt->nup;
   CBuf->ndown += nt->ndown;
   
   
   nt->next = CBuf->narrow;
   CBuf->narrow = nt;
   nt->beg = beg->prev;
   nt->end = CLine->next;
   nt->beg1 = CBuf->beg;
   nt->end1 = CBuf->end;
   
   nt->is_region = 0;
   CBuf->beg = beg;
   CBuf->end = CLine;
   beg->prev = NULL;
   CLine->next = NULL;
   
   if (CLine->len && (CLine->data[CLine->len - 1] == '\n'))
     {
	/* I do not think that this will affect undo. */
	CLine->len--;
     }
   
   pop_spot();
   touch_windows();
   return(1);
}

/*}}}*/

/*}}}*/

/*{{{ Pastebuffer, Delete Region functions */

int yank() /*{{{*/
{
   CHECK_READ_ONLY
    if (Paste_Buffer == NULL) return(0);
    insert_buffer(Paste_Buffer);
    return(1);
}

/*}}}*/

int copy_region_to_buffer(Buffer *b) /*{{{*/
{
   int first_point, last_point, n, tmpm;
   Line *first, *last;
   Buffer *save_buffer;

   if (b->flags & READ_ONLY) 
     {
	msg_error(Read_Only_Error);
	return (0);
     }
   
   if (!check_region(&Number_One)) return(0);  /* spot pushed */
   last = CLine;
   last_point = Point;

   tmpm = 1; pop_mark(&tmpm);
   if (b == CBuf)
     {
	msg_error("A buffer cannot be inserted upon itself.");
	pop_spot();
	return(0);
     }

   first = CLine;
   first_point = Point;

   save_buffer = CBuf;
   switch_to_buffer(b);

   /* go through standard routines for undo comapatability */
   Suspend_Screen_Update = 1;
   if (first == last)
     {
	n = last_point - first_point;
	if (save_buffer == MiniBuffer)
	  {
	     ins_chars(first->data + first_point, n);
	  }
	else quick_insert(first->data + first_point, n);
     }
   else 
     {
	n = first->len - first_point;
	quick_insert(first->data + first_point, n);
	while (first = first->next, first != last)
	  {
	     quick_insert(first->data, first->len);
	  }
	quick_insert(first->data, last_point);
     }
   switch_to_buffer(save_buffer);
   pop_spot();
   return(1);
}

/*}}}*/

int copy_to_pastebuffer() /*{{{*/
{
   /* delete paste buffer */
   if (Paste_Buffer != NULL) delete_buffer(Paste_Buffer);
   Paste_Buffer = make_buffer();
   strcpy(Paste_Buffer->name, " <paste>");

   copy_region_to_buffer(Paste_Buffer);
   return(0);
}

/*}}}*/

int delete_region (void) /*{{{*/
{
   int beg_point, tmpm, end_point, n;
   Line *beg, *end;
   
   CHECK_READ_ONLY
    if (!check_region(&Number_Zero)) return(0);

   /* make this go through standard ins/del routines to ensure undo */
   
   end = CLine; end_point = Point;
   push_spot();
   tmpm = 1; pop_mark(&tmpm);
   beg = CLine; beg_point = Point;
   pop_spot();
   
   Point = 0;
   
   if (end != beg) 
     {
	deln(&end_point);
	
	/* go back because we do not want to mess with Line structures
	   changing on us --- shouldn't happen anyway */
	
	while (CLine = CLine->prev, LineNum--, CLine != beg)
	  {
	     eol(); 
	     n = Point + 1;
	     Point = 0;
	     generic_deln(&n);
	  }
	eol();
	n = Point - beg_point + 1;  /* the \n char */
	Point = beg_point;
     }
   else 
     {
	Point = beg_point;
	n = end_point - Point;
     }
   
   generic_deln(&n);
   return(1);
}

/*}}}*/

int kill_region() /*{{{*/
{
   int tmpm = 1;
   
   CHECK_READ_ONLY

   /* need two marks for this one */
   push_spot();
   if (!pop_mark(&tmpm))
     {
	check_region(&Number_Zero);
	pop_spot();
	return(0);
     }
   push_mark();
   push_mark();
   pop_spot();

   copy_to_pastebuffer();
   delete_region();
   return(1);
}

/*}}}*/

/*}}}*/

/*{{{ Rectangle Functions */

static char *Rect_Error = "Rectangle has 0 width.";
int insert_rectangle() /*{{{*/
{
   int c1;
   Line *rline;

   CHECK_READ_ONLY
   if (Rectangle_Buffer == NULL) return(0);

   Suspend_Screen_Update = 1;
   c1 = calculate_column();
   rline = Rectangle_Buffer->beg;
   if (rline != NULL) while (1)
     {
	goto_column(&c1);
	quick_insert(rline->data, rline->len);
	rline = rline->next;
	if (rline == NULL) break;
	if (CLine->next == NULL)
	  {
	     eol();
	     newline();
	  }
	else 
	  {
	     CLine = CLine->next;
	     LineNum++;
	  }
     }
   return(1);
}

/*}}}*/

int open_rectangle() /*{{{*/
{
   int c1, n, c2, tmpm;
   Line *save_line;
   
   CHECK_READ_ONLY
   if (!check_region(&Number_One)) return(0); /* push_spot(); performed */

   c1 = calculate_column();
   save_line = CLine;
   tmpm = 1; pop_mark(&tmpm);
   c2 = calculate_column();
   n = c2 - c1;
   if (n < 0)
     {
	n = -n;
	c1 = c2;
     }
   
   Suspend_Screen_Update = 1;
   while(1)
     {
	goto_column(&c1);
	ins_char_n_times(' ', n);
	if (CLine == save_line) break;
	CLine = CLine->next;
	LineNum++;
     }
   pop_spot();

   return(1);
}

/*}}}*/

/* rectangle commands */
int copy_rectangle() /*{{{*/
{
    Line *save_line, *line, *beg;
    int c1, c2, dc, tmp, tmpm, dc_malloc;
    unsigned char *p1, *p2, *data;
   
    if (!check_region(&Number_One)) return(0);       /* spot pushed */
    /* delete Rectangle buffer */
    if (Rectangle_Buffer != NULL) delete_buffer(Rectangle_Buffer);

    Rectangle_Buffer = make_buffer();
    strcpy(Rectangle_Buffer->name, " <rect>");
    c2 = calculate_column();
    save_line = CLine;

    tmpm = 1; pop_mark(&tmpm);
    c1 = calculate_column();
    if (c1 == c2)
      {
	 msg_error(Rect_Error);
	 pop_spot();
	 return(0);
      }
    if (c1 > c2)
      {
	 tmp = c1;
	 c1 = c2;
	 c2 = tmp;
	 goto_column(&c1);
      }

    /* go through the region copying rectanglar blocks to Rectanglebuffer */
    dc = c2 - c1;
   if (dc == 1) dc_malloc = 2; else dc_malloc = dc;
    line = beg = make_line1(dc_malloc);
   
    beg->prev = NULL;
    while (1)
      {
	 data = line->data;
	 /* p1 = data;
	 p2 = data + dc;
	 while (p1 < p2) *p1++ = ' '; */
	 SLMEMSET ((char *) data, ' ', dc);
	 
	 line->len = dc;

	 if (c1 == goto_column1(&c1))
	   {
	      p1 = CLine->data + Point;
	      (void) goto_column1(&c2);
	      p2 = CLine->data + Point;

	      /* while(p1 < p2) *data++ = *p1++; */
	      SLMEMCPY((char *) data, (char *) p1, (int) (p2 - p1));
	   }
	 if (CLine == save_line) break;
	 CLine = CLine->next;
	 LineNum++;

	 line->next = make_line1(dc_malloc);
	 line->next->prev = line;
	 line = line->next;
      }

    line->next = NULL;

    Rectangle_Buffer->line = Rectangle_Buffer->beg = beg;
    Rectangle_Buffer->end = line;
    Rectangle_Buffer->point = 0;

    pop_spot();
    return(0);
}

/*}}}*/

int kill_rectangle() /*{{{*/
{
    Line *save_line, *line, *beg;
   int c1, c2, dc, tmp, n, tmpm, dc_malloc;
   unsigned char *p1, *p2, *data;

   CHECK_READ_ONLY
   if (!check_region(&Number_One)) return(0);

   /* delete Rectangle buffer */
   if (Rectangle_Buffer != NULL) delete_buffer(Rectangle_Buffer);

   Rectangle_Buffer = make_buffer();
   strcpy(Rectangle_Buffer->name, " <rect>");
   c2 = calculate_column();
   save_line = CLine;

   tmpm = 1; pop_mark(&tmpm);
    c1 = calculate_column();
    if (c1 == c2)
      {
	 msg_error(Rect_Error);
	 pop_spot();
	 return(0);
      }
    if (c1 > c2)
      {
	 tmp = c1;
	 c1 = c2;
	 c2 = tmp;
	 goto_column(&c1);
      }

   Suspend_Screen_Update = 1;
    /* go through the region copying rectanglar blocks to Rectanglebuffer */
    dc = c2 - c1;
   if (dc == 1) dc_malloc = 2; else dc_malloc = dc;
   		       /* length of 1 not usable here.
			* See makeline1 to see why 
			*/
    line = beg = make_line1(dc_malloc);
    beg->prev = NULL;
    while (1)
      {
	 data = line->data;
	 
	 /* p2 = data + dc; p1 = data;
	 while (p1 < p2) *p1++ = ' '; */
	 SLMEMSET((char *) data, ' ', dc);
	 line->len = dc;

	 if (c1 == goto_column1(&c1))
	   {
	      p1 = CLine->data + Point;
	      (void) goto_column1(&c2);
	      p2 = CLine->data + Point;
	      Point = (int) (p1 - CLine->data);
	      n = (int) (p2 - p1);
	      SLMEMCPY((char *) data, (char *) p1, n);
	      deln (&n);
	      /* while(n-- > 0)
	        {
		   *data++ = *p1;
		   del();
		} */
	   }
	 if (CLine == save_line) break;
	 CLine = CLine->next;  LineNum++;

	 line->next = make_line1(dc_malloc);
	 line->next->prev = line;
	 line = line->next;
      }

    line->next = NULL;

    Rectangle_Buffer->line = Rectangle_Buffer->beg = beg;
    Rectangle_Buffer->end = line;
    Rectangle_Buffer->point = 0;

    pop_spot();
    return(0);
}

/*}}}*/

int blank_rectangle() /*{{{*/
{
   int c1, n, c2, pnt, tmpm;
   Line *save_line;
   int nn;
   

   CHECK_READ_ONLY
   if (!check_region(&Number_One)) return(0); /* push_spot(); performed */

   c1 = calculate_column();
   save_line = CLine;
   tmpm = 1; pop_mark(&tmpm);
   c2 = calculate_column();
   n = c2 - c1;
   if (n < 0)
     {
	n = -n;
	c1 = c2;
     }
   
   Suspend_Screen_Update = 1;
   while(1)
     {
	goto_column(&c1);
	pnt = Point;
	eol();
	nn = Point - pnt;
	if (nn > n) nn = n;
	Point = pnt;
	
	deln(&nn);
	ins_char_n_times( ' ', nn);
	
	if (CLine == save_line) break;
	CLine = CLine->next;
	LineNum++;
     }
   pop_spot();
   /* mark_buffer_modified(&Number_One); */
   return(1);
}

/*}}}*/

/*}}}*/

/*{{{ User Mark Functions */

typedef struct /*{{{*/
{
   Mark m;			       /* MUST be the first */
   Buffer *b;
}

/*}}}*/
User_Mark_Type;

static void free_user_mark (User_Mark_Type *um) /*{{{*/
{
   Mark *m, *m1;
   Buffer *b;

   m1 = &um->m;

   /* The mark is only valid if the buffer that it was created for still
    * exists.
    */
   if ((m1->flags & MARK_INVALID) == 0)
     {
	/* Unlink the mark from the chain. */
	b = um->b;
	m = b->user_marks;
#if JED_HAS_LINE_MARKS
	if (m1->flags & JED_LINE_MARK) 
	  touch_screen ();
#endif
	if (m == m1)	b->user_marks = m1->next;
	else 
	  {
	     while (m->next != m1) m = m->next;
	     m->next = m1->next;
	  }
     }
   
   SLFREE (um);
}

/*}}}*/

void free_user_marks (Buffer *b) /*{{{*/
{
   Mark *m = b->user_marks;
   
   while (m != NULL)
     {
	m->flags |= MARK_INVALID;
	m = m->next;
     }
}

/*}}}*/

static int mark_valid (Mark *m) /*{{{*/
{
   if (m->flags & MARK_INVALID)
     {
	msg_error ("Mark is invalid.");
	return 0;
     }
   return 1;
}

/*}}}*/

int jed_move_user_object_mark (SLuser_Object_Type *uo) /*{{{*/
{
   User_Mark_Type *um;
   Mark *m;
   
   um = (User_Mark_Type *) uo->obj;
   m = &um->m;
   
   if (!mark_valid (m)) return 0;
   
   if (CBuf != um->b)
     {	
	msg_error ("Mark not in buffer.");
	return 0;
     }
   
   
   m->line = CLine;
   m->point = Point;
   m->n = LineNum + CBuf->nup;
   return 1;
}

/*}}}*/

void move_user_mark (void) /*{{{*/
{
   SLuser_Object_Type *uo;
   
   if ((uo = SLang_pop_user_object (JED_MARK_TYPE)) == NULL) return;
   (void) jed_move_user_object_mark (uo);
   SLang_free_user_object (uo);
}

/*}}}*/

static int x_user_mark_fun (int (*xfun)(Mark *)) /*{{{*/
{
   SLuser_Object_Type *uo;
   User_Mark_Type *um;
   int ret = -1;
   
   if ((uo = SLang_pop_user_object (JED_MARK_TYPE)) == NULL) return -1;
   um = (User_Mark_Type *) uo->obj;
   
   if (mark_valid (&um->m))
     {
	if (CBuf != um->b) msg_error ("Mark not in buffer.");
	else
	  ret = (*xfun) (&um->m);
     }
   SLang_free_user_object (uo);
   return ret;
}

/*}}}*/

/* It is up to calling routine to ensure that mark is in buffer. */
static int is_mark_in_narrow (Mark *m)
{
   return ((m->n > CBuf->nup)
	   && (m->n <= CBuf->nup + Max_LineNum));
}

int jed_is_user_mark_in_narrow (void)
{
   return x_user_mark_fun (is_mark_in_narrow);
}

void goto_user_mark (void)
{
   (void) x_user_mark_fun (goto_mark);
}

SLuser_Object_Type *jed_make_user_object_mark (int main_type) /*{{{*/
{
   User_Mark_Type *um;
   SLuser_Object_Type *uo;
   Mark *m;
   
   uo = SLang_create_user_object (JED_MARK_TYPE);
   if (uo == NULL) return NULL;
   uo->main_type = main_type;
   
   if (NULL == (um = (User_Mark_Type *) SLMALLOC (sizeof(User_Mark_Type))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	SLFREE (uo);
	return NULL;
     }
   m = &um->m;
   
   init_mark (m, 0);
   
   m->next = CBuf->user_marks;
   
   CBuf->user_marks = m;
   
   um->b = CBuf;   
   uo->obj = (long *) um;
   return uo;
}

/*}}}*/

void jed_free_user_object_mark (SLuser_Object_Type *uo) /*{{{*/
{
   /* let slang handle this. */
   uo->main_type = SLANG_DATA;
   SLang_free_user_object (uo);
}

/*}}}*/

void create_user_mark (void) /*{{{*/
{
   SLuser_Object_Type *uo = jed_make_user_object_mark (SLANG_DATA);
   if (uo != NULL) SLang_push_user_object (uo);
}

/*}}}*/

char *user_mark_buffer (void) /*{{{*/
{
   SLuser_Object_Type *uo;
   User_Mark_Type *um;
   char *s = "";
   
   if (NULL == (uo = SLang_pop_user_object (JED_MARK_TYPE))) return s;
   um = (User_Mark_Type *) uo->obj;
   
   if (mark_valid (&um->m))
     {
	s = um->b->name;
     }
   
   SLang_free_user_object (uo);
   return s;
}

/*}}}*/


/*}}}*/

#if JED_HAS_LINE_MARKS
void jed_create_line_mark (int *color) /*{{{*/
{
   SLuser_Object_Type *uo = jed_make_user_object_mark (SLANG_DATA);
   User_Mark_Type *um;
   
   if (uo != NULL) 
     {
	um = (User_Mark_Type *) uo->obj;
	um->m.flags |= JED_LINE_MARK | (*color & MARK_COLOR_MASK);
	SLang_push_user_object (uo);
     }
}

/*}}}*/
#endif
   

int register_jed_classes (void) /*{{{*/
{
   return SLang_register_class (JED_MARK_TYPE, (FVOID_STAR) free_user_mark, NULL);
}

/*}}}*/

void jed_widen_whole_buffer (Buffer *b) /*{{{*/
{
   while (b->narrow != NULL) widen_buffer (b);
}

/*}}}*/

#if JED_HAS_SAVE_NARROW
static void restore_saved_narrow (void) /*{{{*/
{
   Mark *beg, *end;
   
   Jed_Save_Narrow_Type *save_narrow;
   
   if (NULL == (save_narrow = CBuf->save_narrow))
     return;
   
   push_spot ();
   /* remove current restriction */
   jed_widen_whole_buffer (CBuf);
   
   beg = save_narrow->beg;
   end = save_narrow->end;
   
   while (beg != NULL)
     {
	goto_mark (beg);
	push_mark ();
	goto_mark (end);
	if (end->flags & NARROW_REGION_MARK)
	  narrow_to_region ();
	else narrow_to_lines ();
	
	beg = beg->next;
	end = end->next;
     }
   pop_spot ();
}

/*}}}*/
static void free_mark_chain (Mark *m) /*{{{*/
{
   Mark *next;
	
   while (m != NULL)
     {
	next = m->next;
	SLFREE (m);
	m = next;
     }
}

/*}}}*/
void jed_free_saved_narrow (void) /*{{{*/
{
   Jed_Save_Narrow_Type *save_narrow;
   
   save_narrow = CBuf->save_narrow;
   if (save_narrow == NULL) return;
   
   CBuf->save_narrow = save_narrow->next;
   
   free_mark_chain (save_narrow->beg);
   free_mark_chain (save_narrow->end);
   SLFREE (save_narrow);
}

/*}}}*/
void jed_push_narrow (void) /*{{{*/
{
   Jed_Save_Narrow_Type *save_narrow;
   
   if (NULL == (save_narrow = (Jed_Save_Narrow_Type *) SLMALLOC (sizeof (Jed_Save_Narrow_Type))))
     {
	exit_error ("push_narrow: malloc error.", 0);
     }
   save_narrow->beg = save_narrow->end = NULL;
   save_narrow->next = CBuf->save_narrow;
   CBuf->save_narrow = save_narrow;
   
   push_spot ();
   while (CBuf->narrow != NULL)
     {
	Mark *m;
	
	bob ();
	m = create_mark (0);
	m->next = save_narrow->beg;
	save_narrow->beg = m;
	
	eob ();
	m = create_mark (CBuf->narrow->is_region ? NARROW_REGION_MARK : 0);
	m->next = save_narrow->end;
	save_narrow->end = m;
	
	widen_buffer (CBuf);
     }
   
   restore_saved_narrow ();
   
   pop_spot ();
}

/*}}}*/
void jed_pop_narrow (void) /*{{{*/
{
   restore_saved_narrow ();
   jed_free_saved_narrow ();
}

/*}}}*/
#endif

int jed_count_narrows (void) /*{{{*/
{
   int n = 0;
   Narrow_Type *nt = CBuf->narrow;
   
   while (nt != NULL)
     {
	n++;
	nt = nt->next;
     }
   return n;
}

/*}}}*/
