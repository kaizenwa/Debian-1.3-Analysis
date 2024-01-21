/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "vterm.h"

int Scroll_By_Copying;

static void sbc_do_copy(register unsigned short *dst, register unsigned short *src, int n) /*{{{*/
{
   register unsigned short *smax = src + n;
   while (src < smax) *dst++ = *src++;
}

/*}}}*/

void execute_vscroll_down(int r1, int r2, int num) /*{{{*/
{
   unsigned short *old;
   int r, rp1;
   int sbc = Scroll_By_Copying, n = JWindow->width;

   r1--; r2--;
   for (r = r2; r >= r1 + num; r--)
     {
	rp1 = r - num;
	JScreen[r].n = JScreen[rp1].n;
	JScreen[r].hi0 = JScreen[rp1].hi0;
	JScreen[r].hi1 = JScreen[rp1].hi1;
	old = JScreen[r].old;
	JScreen[r].old = JScreen[rp1].old;
	JScreen[rp1].old = old;
	
	if (sbc)
	  {
	     sbc_do_copy(old, JScreen[r].old, n);
	  }
     }
   
   for (r = r1; r < r1 + num; r++)
     {
	JScreen[r].line = NULL;
	JScreen[r].flags = sbc;
	
	if (sbc == 0)
	  {
	     JScreen[r].n = 0;
	     JScreen[r].hi0 = NULL;
	     JScreen[r].hi1 = NULL;
	     blank_line(r);
	  }
     }
}

/*}}}*/

void vscroll_down(int r1, int r2, int num) /*{{{*/
{
   int r, rp1;

   r1--; r2--;
   for (r = r2; r >= r1 + num; r--)
     {
	rp1 = r - num;
	JScreen[r].line = JScreen[rp1].line;
	JScreen[r].flags = JScreen[rp1].flags;
     }
}

/*}}}*/

void execute_vscroll_up (int r1, int r2, int num) /*{{{*/
{
   unsigned short *old;
   register int r, rp1;

   int sbc = Scroll_By_Copying, n = JWindow->width;
   r1--; r2--;
   
   for (r = r1; r <= r2 - num; r++)
     {
	rp1 = r + num;
	old = JScreen[r].old;
	JScreen[r].old = JScreen[rp1].old;
	JScreen[rp1].old = old;
	JScreen[r].hi0 = JScreen[rp1].hi0;
	JScreen[r].hi1 = JScreen[rp1].hi1;
	JScreen[r].n = JScreen[rp1].n;
	
	if (sbc)
	  {
	     sbc_do_copy(old, JScreen[r].old, n);
	  }
     }

   for (r = r2 - num + 1; r <= r2; r++)
     {
	JScreen[r].line = NULL;
	JScreen[r].flags = sbc;
	if (sbc == 0)
	  {
	     JScreen[r].n = 0;
	     JScreen[r].hi0 = 0;
	     JScreen[r].hi1 = 0;
	     blank_line(r);
	  }
     }
}

/*}}}*/

void vscroll_up(int r1, int r2, int num) /*{{{*/
{
   register int r, rp1;
   
   r1--; r2--;
   
   for (r = r1; r <= r2 - num; r++)
     {
	rp1 = r + num;
	JScreen[r].line = JScreen[rp1].line;
	JScreen[r].flags = JScreen[rp1].flags;
     }
}

/*}}}*/

void vins(char ch) /*{{{*/
{
   int r;
   unsigned short *p, *p1;
   unsigned short *pins;

    r = Screen_Row - 1;
    if (ch > ' ') JScreen[r].n += 1;
    pins = JScreen[r].old + Screen_Col - 1;
    p = JScreen[r].old + JWindow->width - 1;
    while(p > pins)
      {
	 p1 = p - 1;
	 *p = *p1;
         p = p1;
      }
    *p = ch;
}

/*}}}*/

void vdel() /*{{{*/
{
   int r;
   register unsigned short *p, *p1, *pmax;

    r = Screen_Row - 1;
    p = JScreen[r].old + Screen_Col - 1;
    pmax = JScreen[r].old + JWindow->width - 1;
    while(p < pmax)
      {
	 p1 = p + 1;
	 *p = *p1;
	 p = p1;
      }
    *p = ' ';
}

/*}}}*/

void vdel_eol() /*{{{*/
{
   int r;
   register unsigned short *p, *pmax;
   
   r = Screen_Row - 1;
   p = JScreen[r].old + Screen_Col - 1;
   pmax = JScreen[r].old + JWindow->width;
   /* MEMSET(p, ' ', JWindow->width - Screen_Col + 1); */
   
    while(p < pmax)
      {
          *p = ' ';
          p++;
      }
    *p = ' '; 
}

/*}}}*/
