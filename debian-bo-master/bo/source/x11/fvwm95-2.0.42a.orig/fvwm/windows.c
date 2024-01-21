/****************************************************************************
 * This module is all new
 * by Rob Nation 
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/
/***********************************************************************
 *
 * fvwm window-list popup code
 *
 ***********************************************************************/

#include <FVWMconfig.h>

#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "fvwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"

/* I tried to include "limits.h" to get these values, but it
 * didn't work for some reason */
/* Minimum and maximum values a `signed int' can hold.  */
#define MY_INT_MIN (- MY_INT_MAX - 1)
#define MY_INT_MAX 2147483647

/*
 * Change by PRB (pete@tecc.co.uk), 31/10/93.  Prepend a hot key
 * specifier to each item in the list.  This means allocating the
 * memory for each item (& freeing it) rather than just using the window
 * title directly.  */
void do_windowList(XEvent *eventp,Window w,FvwmWindow *tmp_win,
		unsigned long context, char *action,int *Module)
{
  MenuRoot *mr;
  FvwmWindow *t;
  char *tmp;
  char loc[40], *name = NULL;
  int dwidth, dheight;
  char tlabel[50];
  int last_desk_done = MY_INT_MIN;
  int next_desk;
  char *t_hot;			/* Menu label with hotkey added */
  char scut = '0';		/* Current short cut key */

  int show_geometry = False;
  int desk_mode = 0;
  long desk_num = 0;
  int use_icon_name = False;

  action = GetNextToken(action, &tmp);
  while (tmp != NULL)
    {
    if (!*tmp) { free(tmp); break; }
    else if (mystrcasecmp(tmp, "ShowGeometry") == 0) show_geometry = True;
    else if (mystrcasecmp(tmp, "UseIconNames") == 0) use_icon_name = True;
    else if (mystrcasecmp(tmp, "UseWindowNames") == 0) use_icon_name = False;
    else if (mystrcasecmp(tmp, "ShowAllDesks") == 0) desk_mode = 0;
    else if (mystrcasecmp(tmp, "ShowCurrentDesk") == 0) desk_mode = 2;
    else if (mystrcasecmp(tmp, "ShowDesk") == 0)
      {
      int n = 0, junk;

      desk_mode = 4;
      free(tmp);
      action = GetNextToken(action, &tmp);
      if (tmp != NULL) n = GetOneArgument(tmp, &desk_num, &junk);
      if (n != 1) desk_mode = 0;
      }
    if (tmp != NULL) free(tmp);
    action = GetNextToken(action, &tmp);
    }

  sprintf(tlabel,"CurrentDesk: %d%s",Scr.CurrentDesk,
          show_geometry ? "\tGeometry" : "");

  mr = NewMenuRoot(tlabel,0);
  AddToMenu(mr, tlabel, "TITLE");      

  next_desk = 0;
  while(next_desk != MY_INT_MAX)
    {
    /* Sort window list by desktop number */
    if (desk_mode == 0)
      {
      next_desk = MY_INT_MAX;
      for (t = Scr.FvwmRoot.next; t != NULL; t = t->next)
	{
	if((t->Desk >last_desk_done)&&(t->Desk < next_desk))
	  next_desk = t->Desk;
	}
      }
    else if (desk_mode == 2)
      {
      if (last_desk_done  == MY_INT_MIN)
	next_desk = Scr.CurrentDesk;
      else
	next_desk = MY_INT_MAX;
      }
    else 
      {
      if (last_desk_done  == MY_INT_MIN)
        next_desk = desk_num;
      else
	next_desk = MY_INT_MAX;
      }
    last_desk_done = next_desk;

    for (t = Scr.FvwmRoot.next; t != NULL; t = t->next)
      {
      if ((t->Desk == next_desk) && (!(t->flags & WINDOWLISTSKIP)))
        {
	if (++scut == ('9' + 1)) scut = 'A';	/* Next shortcut key */

	if(use_icon_name)
	  name = t->icon_name;
	else
	  name = t->name;

	t_hot = safemalloc(strlen(name) + 48);
	sprintf(t_hot, "&%c.  %s", scut, name); /* Generate label */

        /* this seems to slow down the menu creation
         * in some machines, we will add the pixmap
         * directly after the call to AddToMenu() 
         *
         * if (t->title_pixmap_file != NULL) {
         *   sprintf(t_hot + strlen(t_hot), "%%%s%%", 
         *           t->title_pixmap_file);
         * }
         */

        if (show_geometry)
          {
          char *tname;

	  tname = safemalloc(40);
	  tname[0] = 0;
	  if (t->flags & ICONIFIED) strcpy(tname, "(");
	  sprintf(loc, "%d:", t->Desk);
	  strcat(tname, loc);
	  if (t->frame_x >= 0)
	    sprintf(loc, "+%d", t->frame_x);
	  else
	    sprintf(loc, "%d", t->frame_x);
	  strcat(tname, loc);
	  if(t->frame_y >= 0)
	    sprintf(loc, "+%d", t->frame_y);
	  else
	    sprintf(loc, "%d", t->frame_y);
	  strcat(tname, loc);
	  dheight = t->frame_height - t->title_height - 2*t->boundary_width;
	  dwidth = t->frame_width - 2*t->boundary_width;
	      
	  dwidth -= t->hints.base_width;
	  dheight -= t->hints.base_height;
	      
	  dwidth /= t->hints.width_inc;
	  dheight /= t->hints.height_inc;

	  sprintf(loc, "x%d", dwidth);
	  strcat(tname, loc);
	  sprintf(loc, "x%d", dheight);
	  strcat(tname, loc);
	  if (t->flags & ICONIFIED) strcat(tname, ")");

          strcat(t_hot, "\t");
	  strcat(t_hot, tname);
          free(tname);
          }

        sprintf(tlabel, "RAISE_IT %ld %ld", t, t->w);
	AddToMenu(mr, t_hot, tlabel);

        /* Add the title pixmap */
        mr->last->lpicture = t->title_icon;
        if (t->title_icon)
          t->title_icon->count++; /* increase the cache count!!
                                     otherwise the pixmap will be
                                     eventually removed from the 
                                     cache by DestroyMenu */

	free(t_hot);
	}
      }
    }

  mr->has_coords = False;
  MakeMenu(mr);

  do_menu(mr, 1);
  DestroyMenu(mr);
}

