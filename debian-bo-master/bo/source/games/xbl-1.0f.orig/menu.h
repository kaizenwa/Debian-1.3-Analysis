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

#include "ansi.h"
#include <X11/Xlib.h>
#include "movingbutton.h"
#include "keyid.h"

#define TEXT 0
#define INTEGER 1
#define FUNCTION 2

#define ZOO_NBX_PIECES 7	/* Number of piece in zoo width */
#define ZOO_NBY_PIECES 6	/* Number of piece in zoo height */
#define ZOO_WIDTH	512

struct menu
	{
	Display *display ;
	Window window ;		/* The menu window */
	Window zoo ;		/* The zoo window */
	int showscore ;		/* Boolean */
	int showzoo ;		/* Boolean */
	unsigned int xzoo,yzoo ;/* Zoo window size */
	Font font,font2 ;	/* Normal and big font */
	XFontStruct *xfont,*xfont2 ;
	int char_height ;	/* Maximum char height for normal char */
	GC white,black,text,text1,text2 ;
	GC flat,upleft,downright ;
	int layersize ;
	Pixmap helpkey ;	/* The bitmap with the help cube */
	int widthhelpkey,heighthelpkey ;
	int tx[KEY_ROT_Z_NEG+1] ;
	int ty[KEY_ROT_Z_NEG+1] ;
	struct bl *bl ;
	struct row_column *all ;
	struct moving_button
		*score,
		*hiscore,
		*cube,
		*bloc,
		*destroylevel,
		*level,
		*nextpiece,

		*land,
		*typepiece,
		*width,*height,*depth,
		*startlevel,
		*training,
		*volume,
		*viewzoo,*viewscore,
		*frame,

		*smooth,
		*draw,
		*key,
		*help,
		*quit,
		*state,
		*counting,
		*copyright ;
	 } ;

extern void initmenu(R1(struct bl *bl)) ;
extern void createhelp(R2(struct moving_button *b,char *key)) ;
extern void displaymenu(R5(struct menu *b,int x,int y,int dx,int dy)) ;
extern void menuevent(R2(struct bl *bl,XEvent *event)) ;
