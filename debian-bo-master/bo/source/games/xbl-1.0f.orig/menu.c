/*
    XBlockOut a 3D Tetris

    Copyright (C) 1992,1993,1994  Thierry EXCOFFIER

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact: Thierry.EXCOFFIER@ligia.univ-lyon1.fr
*/

#include "bl.h"
#include "buttons.h"
#include <X11/Xutil.h>

void displaymenu(m,x,y,dx,dy)
struct menu *m ;
int x,y,dx,dy ;
{
void displayhelp() ;

displaystairs( m ) ,
display_rowcol( m->all,x,y,dx,dy ) ;
displaynextpiece( m->bl ) ;
x = m->key->height ;
m->key->height = 0 ;
displayhelp( m->key ) ;
m->key->height = x ;
}	

void menuevent(bl,event)
struct bl *bl ;
XEvent *event ;
{
int b ;

switch( event->type )
   {
   case Expose :
	displaymenu(&bl->menu,((XExposeEvent*)event)->x,
			      ((XExposeEvent*)event)->y,
			      ((XExposeEvent*)event)->width,
			      ((XExposeEvent*)event)->height);
	break ;
   case ButtonRelease :
	if ( bl->opt.state==DEMO )
		{
		endgame(bl,0) ;
		}
	switch( ((XButtonEvent*)event)->button )
		{
		case Button1 : b = -1 ; break ;
		case Button2 : b =  0 ; break ;
		case Button3 : b =  1 ; break ;
		default	     : b =  0 ; break ;
		}
	push_rowcol(bl->menu.all,
		    ((XButtonEvent*)event)->x,
		    ((XButtonEvent*)event)->y,
		    b);
	break ;
   case ConfigureNotify :
        {
        XSizeHints sh ;
        sh.flags = PPosition ;
        sh.x = ((XConfigureEvent*)event)->x ;
        sh.y = ((XConfigureEvent*)event)->y ;
        XSetWMNormalHints( bl->x.display,bl->x.window,&sh ) ;
        }

   case UnmapNotify :
   case MapNotify :
   case ReparentNotify :
/* martin@oc2.oc.chemie.th-darmstadt.de (Martin Kroeker)

Thierry,
Just wanted to let you know that I had to comment out the call to setargs
in menu.c (case ReparentNotify) to get it to work with Linux0.99p14,Xfree2.1
and the twm and fvwm window managers. Otherwise the program would crash with
bad window parameters in XTranslateCoordinates, taking the window manager
with it . ;-(

The program apparently crashes at the call to
XTranslateCoordinates in the *second* 'if' block, i.e. bl->menu.window,
at the second or third invocation of setargs. This appears to be immediately
after the display of the score list at the beginning of the game. Unfortunately
I have very little experience with X programming, so I see no immediate
solution. I get a BadWindow error from XtranslateCoordinates although the
parameters passed to it appear to be correct, and the window manager dies
with a segmentation fault. All shells etc. are still running, and if I
restart the game now, it appears to work flawlessly.
*/
#ifndef linux
#ifndef LINUX
	setargs(bl) ;
#endif
#endif
   case ButtonPress :
   case NoExpose :	/* Due to CopyArea */
	break ;
   default :
	fprintf(stderr,"Unknow event type in menu window\n") ;
	fprintf(stderr,"event.type = %d\n",event->type) ;
   }
}
