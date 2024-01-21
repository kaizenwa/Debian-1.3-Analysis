/* ########################################################################

				scroll.h

   File: scroll.h
   Path: /home/fournigault/c/X11/xcoral-2.31/scroll.h
   Description: 
   Created: Fri Jan 27 11:28:25 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:28:27 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#ifndef _SCROLL_H_
#define _SCROLL_H_

#include "proto_decl.h"

#define SCROLL_WIDTH 16
#define DEFAULT_SIZE 10

#define UP 0
#define DOWN 1
#define TOP 2
#define BOTTOM 3
#define OTHER 4
#define FIRST 5
#define LAST 6
#define NEXT 7
#define PREVIOUS 8
#define CURRENT 9
#define CURSOR 10

typedef struct _Swin {
	Window	frame;		/* Main window */
	Window	scroll;		/* Le curseur */
	unsigned long fg, bg;	/* Les couleurs */
	int width;		/* Largeur totale */
	int f_height;		/* geometrie frame et scroll */
	int s_height;
	int fx, fy, sx, sy;	
	XFontStruct *font;	/* La fonte utilisee pour le texte a scroller */
	int line;		/* Infos sur le nb de lignes a scroller */
	int linepage;  		
	int line_to_scroll;
	double	delta;		/* Pour les calculs */
	int y_min, y_max;    
	int last_dir;		/* Derniere direction pour la scrollbar */
	int last_y;     		  	
	double	rest;		/* La fraction de ligne restant a scroller */
	char	*text;		/* Vers les infos sur le texte */
} SWin;

FCT (int, ButtonPressInScroll, (SWin *swin, Window w, int y, int *result) );
FCT (void, DeleteScroll, (Display *display, SWin *swin) );
FCT (int, ExposeInScroll, (Display *display, Window w, SWin *swin) );
FCT (void, HandleScrollBar, (Display *display, SWin *swin, void (*f)()) );
FCT (void, InitScroll, (Display *display) );
FCT (SWin *, MakeScroll, (Display *display, Window parent, int x, int y) );
FCT (int, MoveScrollBar, (Display *display, SWin *swin, int flag, int dy) );
FCT (void, RefreshScroll, (Display *display, SWin *swin, int width, int height, int n) );
FCT (void, RefreshScrollBar, (Display *display, SWin *s) );
FCT (void, RefreshScrollFrame, (Display *display, SWin *s) );
FCT (void, SetScrollBarSize, (Display *display, SWin *swin) );
FCT (void, SetScrollLine, (SWin *swin, int n) );
FCT (void, ShowScrollFrame, (Display *display, SWin *s) );

#define SetScrollFont(s, f)	(s -> font = f)
#define SetScrollLinePage(s, n)	(s -> linepage = n)
#define GetScrollWidth()	(SCROLL_WIDTH)

#endif /* _SCROLL_H_ */

