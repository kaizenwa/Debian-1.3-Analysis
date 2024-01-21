/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * a copy of which is included here in file "GNU_GENERAL"
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering
 * teams as part of the course "Introduction to Software Engineering"
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *              soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations or by individuals for personal HOME use.
 * This software, or any of its parts, may not be used by for-profit
 * organization, regardless of application or intended product or
 * customer, without the permission of the Board of Regents of the
 * University  of Wisconsin.
 *
 * Contact:     soft-eng@cs.uwm.edu
 *                      or
 *
 *              Software Engineering Coordinator
 *              Computer Science
 *              Department of EECS
 *              University of Wisconsin - Milwaukee
 *              Milwaukee, WI  53201
 *              414-229-4677
 *
 *              HISTORY,CLAIMS and CONTRIBUTIONS
 */
/*--------------author RAMA DEVI PUVVADA ------------------------------------*/
#include <config.h>

#include <curses.h>

#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "scXstuff.h"
#endif /* HAVE_X11_X_H */

#include "sc.h"
#include <signal.h>
#include <errno.h>

#define CLIM 3
#define RLIM 4

int sort_order = 1, sort_type = 1;
int sort_row, sort_col;

static void	get_col PROTO((void));
static void	get_row PROTO((void));
static void	sort_reset PROTO((void));
static void	sort_id PROTO((void));
static void	sort_rc PROTO((void));
static void	sort_col_label PROTO((void));
static void	sort_col_value PROTO((void));
static void	sort_row_label PROTO((void));
static void	sort_row_value PROTO((void));
static void	sort_vl PROTO((void));
static void	swap_col PROTO((int, int));
static void	swap_row PROTO((int, int));


void
Sort_Menu()
{
static char *mainmenu[] = {
                      "Column/Row",
                      "Dec/Inc",
                      "Label/Value",
                      "Reset"
                      };

static char *help[] = {
                   "Sort the table byrows/bycolumns",
                   "Set the Increasing/Decreasing order option",
                   "Set the value/label type option",
                   "Reset the sort options to Increasing & Value"
                             };
/* auto. */  unsigned int  choice=0;

sort_reset();
    do{ /* do until ESC is selected */
      choice = menu(4, mainmenu, help);
      switch(choice) {
                case 0: Main_Menu();
                        break;
                case 1: sort_rc();
                        break;
                case 2: sort_id();
                        break;
                case 3: sort_vl();
                        break;
                case 4: sort_reset();
                        break;
                     }
      if (choice == 1) break;
    } while (choice != 0);  

}

/*--------*/

static void
sort_reset()
{
sort_order = 1; /* increasing order */
sort_type = 1; /* by value */
}

/*------*/
static void
sort_id()
{
static char *item[] = {
                      "Decreasing",
                      "Increasing"
                      };

static char *help_prompt[] = {
                           "Sort in decreasing order",
                           "Sort in increasing order"
                             };

switch (menu(2, item, help_prompt)) {

        case 1: /* Decreasing */
                sort_order = 0;
                break;
        case 2: /* Increasing */
                sort_order = 1;
                break;
        }

}
/*--------*/

static void
sort_vl()
{
static char *item[] = {
                      "Label",
                      "Value"
                      };

static char *help_prompt[] = {
                           "Sort using label",
                           "Sort using value"
                             };

switch (menu(2, item, help_prompt)) {

        case 1: /* label */
                sort_type = 0;
                break;

        case 2: /* value */
                sort_type = 1;
                break;

        }

}

/*--------*/

static void
sort_rc()
{
int temprow,tempcol;

static char *item[] = {
                      "Row",
                      "Column"
                      };

static char *help_prompt[] = {
                           "Sort the table by rows",
                           "Sort the table by column"
                             };

temprow = currow;
tempcol = curcol;
switch (menu(2, item, help_prompt)) {

      case 1:
             get_row();
             switch (sort_type) {
                    case 0: sort_row_label();
                            break;
                    case 1: sort_row_value();
                            break;
                    }
             break;
      case 2:
             get_col();
             switch (sort_type) {
                    case 0: sort_col_label();
                            break;
                    case 1: sort_col_value();
                            break;
                    }
             break;
      }
currow = temprow;
curcol = tempcol;
}

/*----------------------*/
static void
sort_row_label()
{
int i,j;
int mm_index;
char* minmax;

for (i = 0; i <= maxcol; i++)
  {
  register struct ent *p = *ATBL(tbl,sort_row,i);
  if (!p || p->flags&is_valid || !(p->flags&is_label)) continue;

  minmax = p->label;
  mm_index = i;
  
  for (j = i; j <= maxcol; j++)
      {
      register struct ent *q = *ATBL(tbl,sort_row,j);
      if (!q || q->flags&is_valid || !(q->flags&is_label)) continue;
      switch (sort_order) {
          case 0: /* decreasing */
                  if (strcmp(q->label, minmax) > 0) 
                      {
                      mm_index = j;
                      minmax = q->label;
                      }
                  break;
          case 1: /* increasing */
                  if (strcmp(q->label, minmax) < 0)
                      {
                      mm_index = j;
                      minmax = q->label;
                      }

                  break;
          }
      }
      if (i != mm_index)
	swap_col(i, mm_index);
   }
FullUpdate++;
modflg++;
}

/*----------------------*/
static void
sort_row_value()
{
int i,j;
int mm_index;
double minmax;

for (i = 0; i <= maxcol; i++)
  {
  register struct ent *p = *ATBL(tbl,sort_row,i);
  if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;

  minmax = p->v;
  mm_index = i;
  
  for (j = i; j <= maxcol; j++)
      {
      register struct ent *q = *ATBL(tbl,sort_row,j);
      if (!q || !(q->flags&is_valid) || q->flags&is_label) continue;
      switch (sort_order) {
          case 0: /* decreasing */
                  if (q->v > minmax) 
                      {
                      mm_index = j;
                      minmax = q->v;
                      }
                  break;
          case 1: /* increasing */
                  if (q->v < minmax)
                      {
                      mm_index = j;
                      minmax = q->v;
                      }
                  break;
          }
      }
   if (i != mm_index) swap_col(i, mm_index);
   }
FullUpdate++;
modflg++;
}
/*----------------------*/
static void
sort_col_label()
{
int i,j;
int mm_index;
char* minmax;

for (i = 0; i <= maxrow; i++)
  {
  register struct ent *p = *ATBL(tbl,i,sort_col);
  if (!p || p->flags&is_valid || !(p->flags&is_label)) continue;

  minmax = p->label;
  mm_index = i;
  
  for (j = i; j <= maxrow; j++)
      {
      register struct ent *q = *ATBL(tbl,j,sort_col);
      if (!q || q->flags&is_valid || !(q->flags&is_label)) continue;
      switch (sort_order) {
          case 0: /* decreasing */
                  if (strcmp(q->label, minmax) > 0) 
                      {
                      mm_index = j;
                      minmax = q->label;
                      }
                  break;
          case 1: /* increasing */
                  if (strcmp(q->label, minmax) < 0)
                      {
                      mm_index = j;
                      minmax = q->label;
                      }

                  break;
          }
      }
   if (i != mm_index) swap_row(i, mm_index);
   }
FullUpdate++;
modflg++;
}
/*----------------------*/
static void
sort_col_value()
{
int i,j;
int mm_index;
double minmax;

for (i = 0; i <= maxrow; i++)
  {
  register struct ent *p = *ATBL(tbl,i,sort_col);
  if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;

  minmax = p->v;
  mm_index = i;
  
  for (j = i; j <= maxrow; j++)
      {
      register struct ent *q = *ATBL(tbl,j,sort_col);
      if (!q || !(q->flags&is_valid) || q->flags&is_label) continue;
      switch (sort_order) {
          case 0: /* decreasing */
                  if (q->v > minmax) 
                      {
                      mm_index = j;
                      minmax = q->v;
                      }
                  break;
          case 1: /* increasing */
                  if (q->v < minmax)
                      {
                      mm_index = j;
                      minmax = q->v;
                      }

                  break;
          }
      }
   if (i != mm_index) swap_row(i, mm_index);
   }
FullUpdate++;
modflg++;

}
/*----------------------*/

static void
swap_col(a,b)
int a, b;
{
int c;

curcol = a;
dupcol();
erase_area(0,a,maxrow,a);
for (c = 0; c <= maxrow; c++)
    {
    register struct ent *p = *ATBL(tbl, c, b+1);
    if (p) {
           register struct ent *n;
           n = lookat (c, a);
           (void) clearent(n);
           (void) copyent (n, p, 1, 0);
           }
    }

for (c = 0; c <= maxrow; c++)
    {
    register struct ent *p = *ATBL(tbl, c, a+1);
    if (p) {
           register struct ent *n;
           n = lookat (c, b+1);
           (void) clearent(n);
           (void) copyent (n, p, 1, 0);
           }
    }
curcol = a+1;
closecol(curcol,1);
}

/*----------------------*/
static void
swap_row(a,b)
int a, b;
{
int c;

currow = a;
duprow();
erase_area(a,0,a,maxcol);
for (c = 0; c <= maxcol; c++)
    {
    register struct ent *p = *ATBL(tbl, b+1, c);
    if (p) {
           register struct ent *n;
           n = lookat (a, c);
           (void) clearent(n);
           (void) copyent (n, p, 1, 0);
           }
    }

for (c = 0; c <= maxcol; c++)
    {
    register struct ent *p = *ATBL(tbl, a+1, c);
    if (p) {
           register struct ent *n;
           n = lookat (b+1, c);
           (void) clearent(n);
           (void) copyent (n, p, 1, 0);
           }
    }
currow = a+1;
deleterow(1);
}

/*----------------------*/
static void
get_col()
{
  char s[100];   /* stored number to be converted */
  char msg[100]; /* message for display */
  int  ierr;     /* 1 = No error on input */
  int  colm;     /* column number from label entered */
  int  i;        /* miscellaneous index */

do {
   ierr = 1;
   sprintf(s, "Input column label-- 2 character maximum:");
   get_str(s, CLIM);

  /* convert label in "base 26" to column number */
          /* set ierr=0 if an error occurs */
          for(i=colm=0; (i<CLIM) && (s[i] != '\0') && ierr ; i++){
            if ( !isalpha(s[i]) && !isspace(s[i])) {
              ierr = 0;
              continue;
            }
            if (isspace(s[i])) continue;
            if ( islower(s[i]))
              s[i] = toupper(s[i]);
            colm = (colm * 26) + (s[i] - 'A' + 1);
          }
          if (colm == 0)
            ierr = 0;  /* input was all spaces */
          else
            colm--;    /* adjust to zero-based system */

          if (ierr == 0 )
            {
             sprintf(msg,"Input must be character(s)");
             (void) message(msg);
            }
         } while ( ierr == 0 );

sort_col = colm;

}

/*----------------------*/
static void
get_row()
{
  char s[100];   /* stored number to be converted */
  char msg[100]; /* message for display */
  int  ierr;     /* 1 = No error on input */

      do {  
         clearlines(0,0);  /* clear from row 0 to row 0 */
         ierr = 0;
         sprintf(s,"Input row number:");
         sort_row = atoi( get_str(s,RLIM) );
         if ( !isdigit(s[0]) || (sort_row > 200) )
           {
            sprintf(msg,"Input error !");
            (void) message(msg);
            ierr = 1;
           } /* end if */
         } while ( ierr  == 1 );  /* end do */
}

