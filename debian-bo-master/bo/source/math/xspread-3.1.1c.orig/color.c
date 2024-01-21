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

#ifdef HAVE_X11_X_H	/* this code for now is X specific */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "scXstuff.h"

#include "sc.h"

#define CLIM 3
#define RLIM 4

int copyc = 0;
char **copyv = 0;
unsigned long bg, fg;
Colormap cmap;
XColor exactBG, BG, exactFG, FG;
char foreg[30];
char backg[30];

static void	set_back PROTO((void));
static void	set_fore PROTO((void));

void
Color_Menu()
{
static char *mainmenu[] = {
                          "Foreground",
                          "Background"
                          };

static char *help[] = {
                       "Set the Foreground color",
                       "Set the Background color"
                      };
/* auto. */  unsigned int  choice=0;

    do{ /* do until ESC is selected */
      choice = menu(2, mainmenu, help);
      switch(choice) {
                case 1: set_fore();
                        break;
                case 2: set_back();
                        break;
                     }
       } while (choice != 0);  

XUnmapWindow(dpy, mainwin);
XCloseDisplay(dpy);

sc_Xinit(copyc, copyv);
update(TRUE);
}

/*--------*/

static void
set_fore()
{
char tempforeg[30];
XColor exacttempforeg, TempForeg;

sprintf(tempforeg, "Foreground: ");
get_str(tempforeg, 30);

cmap = DefaultColormap (dpy, DefaultScreen (dpy));
if (tempforeg && (XAllocNamedColor (dpy, cmap, tempforeg, &exacttempforeg, &TempForeg)) != 0)
   {
   strcpy(foreg, tempforeg);
   }

if (foreg && (XAllocNamedColor (dpy, cmap, foreg, &exactFG, &FG) != 0))
    {
    fg = FG.pixel;
    }
   else
    {
    fg = BlackPixel(dpy, DefaultScreen(dpy)); /* foreground */
    }
if (backg && (XAllocNamedColor (dpy, cmap, backg, &exactBG, &BG) != 0))
    {
    bg = BG.pixel;
    }
   else
    {
    bg = WhitePixel(dpy, DefaultScreen(dpy)); /* background */
    }

  XSetForeground(dpy, maingc, fg);
  XSetBackground(dpy, maingc, bg);
  XSetForeground(dpy, maingcreversed, bg);
  XSetBackground(dpy, maingcreversed, fg);

FullUpdate++;
modflg++;

}
/*--------*/

static void
set_back()
{
char tempbackg[30];
XColor exacttempbackg, TempBackg;

sprintf(tempbackg, "Background: ");
get_str(tempbackg, 30);

cmap = DefaultColormap (dpy, DefaultScreen (dpy));
if (tempbackg && (XAllocNamedColor (dpy, cmap, tempbackg, &exacttempbackg, &TempBackg)) != 0)
   {
   strcpy(backg, tempbackg);
   }

if (foreg && (XAllocNamedColor (dpy, cmap, foreg, &exactFG, &FG) != 0))
    {
    fg = FG.pixel;
    }
   else
    {
    fg = BlackPixel(dpy, DefaultScreen(dpy)); /* foreground */
    }
if (backg && (XAllocNamedColor (dpy, cmap, backg, &exactBG, &BG) != 0))
    {
    bg = BG.pixel;
    }
   else
    {
    bg = WhitePixel(dpy, DefaultScreen(dpy)); /* background */
    }

  XSetForeground(dpy, maingc, fg);
  XSetBackground(dpy, maingc, bg);
  XSetForeground(dpy, maingcreversed, bg);
  XSetBackground(dpy, maingcreversed, fg);

FullUpdate++;
modflg++;
}

/*------*/

#endif /* HAVE_X11_X_H	this code for now is X specific */
