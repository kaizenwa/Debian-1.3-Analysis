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
/* -------------- author: RAMA DEVI PUVVADA -------------------------------*/
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

int search_rc = 1, search_type = 1, search_area = 1;
int search_row, search_col;
int search_start_row, search_start_col, search_end_row, search_end_col;
int continue_row, continue_col;
int found = 0;

double search_value;
char* search_label = 0;
char search_l = 0;

static void	nsearch_the_label_by_col PROTO((void));
static void	nsearch_the_label_by_row PROTO((void));
static void	nsearch_the_value_by_col PROTO((void));
static void	nsearch_the_value_by_row PROTO((void));
static void	psearch_the_label_by_col PROTO((void));
static void	psearch_the_label_by_row PROTO((void));
static void	psearch_the_value_by_col PROTO((void));
static void	psearch_the_value_by_row PROTO((void));
static void	search_lv PROTO((void));
static void	search_next PROTO((void));
static void	search_previous PROTO((void));
static void	search_rcp PROTO((void));
static void	search_reset PROTO((void));
static void	search_tr PROTO((void));

void
Search_Menu()
{
static char *mainmenu[] = {
                      "Next",
                      "Previous",
                      "Label/Value",
                      "Column/Row",
                      "EntireTable/Range",
                      "Reset"
                      };

static char *help[] = {
                   "Search next occurrence",
                   "Search the previous occurrence",
                   "Set the value/label and read it",
                   "Set the byrows/bycolumns option",
                   "Set the entire table/range option and read the range",
                   "Reset the search options to entire table & of type Value"
                             };
/* auto. */  unsigned int  choice=0;

search_reset();
    do{ /* do until ESC is selected */
      choice = menu(6, mainmenu, help);
      switch(choice) {
                case 0: Main_Menu();
                        break;
                case 1: search_next();
                        break;
                case 2: search_previous();
                        break;
                case 3: search_lv();
                        break;
                case 4: search_rcp();
                        break;
                case 5: search_tr();
                        break;
                case 6: search_reset();
                        break;
                     }
       } while (choice != 0);  

}

/*--------*/

static void
search_reset()
{
search_area = 1; /* entire table*/
search_start_row = 0;
search_start_col = 0;
search_end_row = maxrow;
search_end_col = maxcol;
continue_row = 0;
continue_col = 0;
search_type = 1; /* by value */
search_label = 0;
search_value = 0;
found = 0;
}

/*------*/
static void
search_tr()
{
struct m_range *search_range;
char s[100];
static char *item[] = {
                      "Range",
                      "EntireTable"
                      };

static char *help_prompt[] = {
                           "Search the Range",
                           "Search the entire table"
                             };

switch (menu(2, item, help_prompt)) {

        case 1: /* Range */
                search_area = 0;
                clearlines(0,0);
                sprintf(s,"Range: ");
                get_str(s,TSSIZE);
                search_range = findrge(s);
                if (search_range == NULL) 
                     {
                     message("Input syntax message");
                     return;
                     }
                search_start_row = search_range->sr;
                search_start_col = search_range->sc;
                continue_row = search_start_row;
                continue_col = search_start_col;
                search_end_row = search_range->er;
                search_end_col = search_range->ec;
                found = 0;
                break;
        case 2: /* Entire Table*/
                search_area= 1;
                search_start_row = 0;
                search_start_col = 0;
                continue_row = search_start_row;
                continue_col = search_start_col;
                search_end_row = maxrow;
                search_end_col = maxcol;
                found = 0;
                break;
        }

}
/*--------*/

static void
search_rcp()
{
static char *item[] = {
                      "Row",
                      "Column"
                      };

static char *help_prompt[] = {
                           "Search the table by rows",
                           "Search the table by column"
                             };

switch (menu(2, item, help_prompt)) {

      case 1:
             search_rc = 0;
             found = 0;
             break;
      case 2:
             search_rc = 1;
             found = 0;
             break;
      }
}

/*--------*/

static void
search_lv()
{
char s[1024];
static char *item[] = {
                      "Label",
                      "Value"
                      };

static char *help_prompt[] = {
                           "Search using label",
                           "Search using value"
                             };

switch (menu(2, item, help_prompt)) {

        case 1: /* label */
                search_type = 0;
                sprintf(s,"enter the label");
                get_str(s,TSSIZE);
                search_label = s;
                found = 0; 
                break;
        case 2:
               search_type = 1;
               sprintf(s, "enter the value");
               get_str(s, TSSIZE);
               mystrtof(s, &search_value);
               found = 0;
               break;
   }

}
/*-----------*/
static void
search_next()
{
switch(search_type) {
    case 0: 
                switch(search_rc)
                   {
                   case 0:
                      nsearch_the_label_by_row();
                      break;
                   case 1:
                      nsearch_the_label_by_col();
                      break;
                   }
                break;

    case 1:
               switch(search_rc)
                   {
                   case 0:
                      nsearch_the_value_by_row();
                      break;
                   case 1:
                      nsearch_the_value_by_col();
                      break;
                   }
               break;
                    }
          FullUpdate++;
          modflg++;

update(FALSE);
}
/*-----------*/
static void
search_previous()
{
switch(search_type) {
    case 0: 
                switch(search_rc)
                   {
                   case 0:
                      psearch_the_label_by_row();
                      break;
                   case 1:
                      psearch_the_label_by_col();
                      break;
                   }
                break;

    case 1:
               switch(search_rc)
                   {
                   case 0:
                      psearch_the_value_by_row();
                      break;
                   case 1:
                      psearch_the_value_by_col();
                      break;
                   }
               break;
                    }
          FullUpdate++;
          modflg++;

update(FALSE);
}
/*----------------------*/
static void
nsearch_the_label_by_row()
{
int i,j;
register struct ent *p;

for (i = search_start_row; i <= search_end_row; i++)
  {
  for (j = search_start_col; j <= search_end_col; j++)
    {
    if ((i < continue_row) 
    || ((i == continue_row) && (j < continue_col))) continue;
    p = *ATBL(tbl,i,j);
    if (!p || (p->flags&is_valid) || !(p->flags&is_label)) continue;
    if (strcmp(p->label , search_label) == 0) 
          {
          found = 1;
          currow = i;
          curcol = j;
          continue_row = i;
          continue_col = j;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_start_row;
    continue_col = search_start_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    nsearch_the_label_by_row(); 
    }
else 
    message("pattern not found");
}

/*----------------------*/
static void
nsearch_the_label_by_col()
{
int i,j;
register struct ent *p;

for (i = search_start_col; i <= search_end_col; i++)
  {
  for (j = search_start_row; j <= search_end_row; j++)
    {
    if ((i < continue_col) 
    || ((i == continue_col) && (j <= continue_row))) continue;
    p = *ATBL(tbl,j,i);
    if (!p || (p->flags&is_valid) || !(p->flags&is_label)) continue;
    if (strcmp(p->label , search_label) == 0) 
          {
          found = 1;
          currow = j;
          curcol = i;
          continue_row = j;
          continue_col = i;
          return;
          }
    }
  }

if (found) 
    {
    continue_row = search_start_row;
    continue_col = search_start_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    nsearch_the_label_by_col(); 
    }
else 
    message("pattern not found");
}

/*----------------------*/
static void
nsearch_the_value_by_row()
{
int i,j;
register struct ent *p;

for (i = search_start_row; i <= search_end_row; i++)
  {
  for (j = search_start_col; j <= search_end_col; j++)
    {
    if  ((i < continue_row) 
     || ((i == continue_row) && (j <= continue_col)) ) continue;
    p = *ATBL(tbl,i,j);
    if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;
    if (p->v == search_value)
          {
          found = 1;
          currow = i;
          curcol = j;
          continue_row = i;
          continue_col = j;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_start_row;
    continue_col = search_start_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    nsearch_the_value_by_row(); 
    }
else 
    message ("pattern not found");
}
/*----------------------*/
static void
nsearch_the_value_by_col()
{
int i,j;
register struct ent *p;

for (i = search_start_col; i <= search_end_col; i++)
  {
  for (j = search_start_row; j <= search_end_row; j++)
    {
    if  ((i < continue_col) 
     || ((i == continue_col) && (j <= continue_row)) ) continue;
    p = *ATBL(tbl,j,i);
    if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;
    if (p->v == search_value)
          {
          found = 1;
          currow = j;
          curcol = i;
          continue_row = j;
          continue_col = i;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_start_row;
    continue_col = search_start_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    nsearch_the_value_by_col(); 
    }
else 
    message ("pattern not found");
}
/*----------------------*/
static void
psearch_the_label_by_row()
{
int i,j;
register struct ent *p;

for (i = search_end_row; i >= search_start_row; i--)
  {
  for (j = search_end_col; j >= search_start_col; j--)
    {
    if ((i > continue_row) 
    || ((i == continue_row) && (j > continue_col))) continue;
    p = *ATBL(tbl,i,j);
    if (!p || (p->flags&is_valid) || !(p->flags&is_label)) continue;
    if (strcmp(p->label , search_label) == 0) 
          {
          found = 1;
          currow = i;
          curcol = j;
          continue_row = i;
          continue_col = j;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_end_row;
    continue_col = search_end_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    psearch_the_label_by_row(); 
    }
else 
    message("pattern not found");
}

/*----------------------*/
static void
psearch_the_label_by_col()
{
int i,j;
register struct ent *p;

for (i = search_end_col; i >= search_start_col; i--)
  {
  for (j = search_end_row; j >= search_start_row; j--)
    {
    if ((i > continue_col) 
    || ((i == continue_col) && (j >= continue_row))) continue;
    p = *ATBL(tbl,j,i);
    if (!p || (p->flags&is_valid) || !(p->flags&is_label)) continue;
    if (strcmp(p->label , search_label) == 0) 
          {
          found = 1;
          currow = j;
          curcol = i;
          continue_row = j;
          continue_col = i;
          return;
          }
    }
  }

if (found) 
    {
    continue_row = search_end_row;
    continue_col = search_end_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    psearch_the_label_by_col(); 
    }
else 
    message("pattern not found");
}

/*----------------------*/
static void
psearch_the_value_by_row()
{
int i,j;
register struct ent *p;

for (i = search_end_row; i >= search_start_row; i--)
  {
  for (j = search_end_col; j >= search_start_col; j--)
    {
    if  ((i > continue_row) 
     || ((i == continue_row) && (j >= continue_col)) ) continue;
    p = *ATBL(tbl,i,j);
    if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;
    if (p->v == search_value)
          {
          found = 1;
          currow = i;
          curcol = j;
          continue_row = i;
          continue_col = j;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_end_row;
    continue_col = search_end_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    psearch_the_value_by_row(); 
    }
else 
    message ("pattern not found");
}
/*----------------------*/
static void
psearch_the_value_by_col()
{
int i,j;
register struct ent *p;

for (i = search_end_col; i >= search_start_col; i--)
  {
  for (j = search_end_row; j >= search_start_row; j--)
    {
    if  ((i > continue_col) 
     || ((i == continue_col) && (j >= continue_row)) ) continue;
    p = *ATBL(tbl,j,i);
    if (!p || !(p->flags&is_valid) || p->flags&is_label) continue;
    if (p->v == search_value)
          {
          found = 1;
          currow = j;
          curcol = i;
          continue_row = j;
          continue_col = i;
          return;
          }
    }
  }
if (found) 
    {
    continue_row = search_end_row;
    continue_col = search_end_col;
    found++;
    if (found > 2) {
                   found = 0;
                   return;
                   }
    psearch_the_value_by_col(); 
    }
else 
    message ("pattern not found");
}
/*----------------------*/

