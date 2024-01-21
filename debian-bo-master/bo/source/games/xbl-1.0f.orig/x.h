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

#include <X11/Xlib.h>
#include "ansi.h"

#define NBKEYCODE 14

struct x
	{
	Display *display ;
	int      screen ;
	Window   window ;
	Window	 root ;
	Window	 wscore ;
	Colormap colormap,color1,color2 ;
	Visual	 *visual ;
	int	 depth ;
	
	unsigned long white_pixel ;
	unsigned long black_pixel ;
	unsigned long back_pixel ;
	unsigned long map_entries ;

	GC face[6] ;
	GC white ;
	GC black ;
	GC transp ;
	GC movingwhite,movingwhite2 ;
	GC movingblack,movingblack2 ;
	GC movingtransp,movingtransp2 ;
	GC copybuffer ;
	GC grid ;
	GC clearline ;	/* For erase line bloc */

	XFontStruct *xfont ;	/* Copy from struct menu */
	GC text ;		/* Copy from struct menu */

	Pixmap back ;
	Pixmap work ;
	Pixmap icone ;
	Pixmap tile ;

	int dimx,dimy,posx,posy ;
	int kpcode[NBKEYCODE] ; /* Keycode du clavier numerique + curseur */
	char kpstring[NBKEYCODE] ;
	} ;

extern void MyXParseGeometry(R7(Display *d,int screen,char* c,  int *x,int *y,
			unsigned int *dx,unsigned int *dy)) ;

