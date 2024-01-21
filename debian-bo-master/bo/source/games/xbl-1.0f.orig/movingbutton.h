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

#define FLAT_TEXT	0
#define RELIEF_TEXT	1
#define PUSHING_TEXT	0
#define PULLING_TEXT	1
#define STOP_TEXT	2
#define A_BUTTON	0
#define A_LINE		1
#define A_COLUMN	2
#define INIT_MINIMUM	0
#define COMPUTE_MINIMUM 1
#define GIVE_MINIMUM	2

struct text_line
	{
	char *current_text ;	/* The text to display on the line */
	char *maximum_text ;	/* The widest text to display	*/
	XFontStruct *xfont ;	/* The font to display text */
	GC gc ;			/* To draw text */
	int x,y,height ;	/* Position of text in window */
	struct text_line *next ;
	} ;

struct movinggc
	{
	GC	rectangle ;	/* Draw rectangle for flat buttons */
	GC	upleft,downright ; /* For relief */
	GC	back ;		/* inside the button */
	Pixmap	tile[17] ;	/* For text vanishement */
	} ;

struct moving_button
	{
	int type ;		/* Must be A_BUTTON */
	int dx,dy ;		/* Must be here */
	struct text_line *text ;
	Display	*display ;
	Window window ;
	int typet ;		/* FLAT_TEXT , RELIEF_TEXT */
	int direction ;		/* PUSHING_TEXT, PULLING_TEXT, STOP_TEXT */
	int height ;		/* height of the button, dz is the maximum */
	int x,y,dz ;		/* External size of button */
	int margin ;		/* Size of margin in pixel */
	int linestretching ;	/* */
	struct movinggc *gc ;
	void (*fct)() ;		/* fct to call if press */
	int but ;		/* Argument for fct */
	void (*prp)() ;		/* fct to call before display */
	struct menu *menu ;
	} ;

struct row_column
	{
	int type ; /* A_LINE A_COLUMN */
	int dx,dy ; /* Must be here */
	struct row_column *in ;
	struct row_column *next ;
	} ;

extern struct text_line* create_line(R3(char *maximum,
					XFontStruct *xfont, GC gc)) ;
extern struct moving_button* create_button() ;
/*
		R7(Display *d,Window w,int height,int type,struct movinggc *gc,
		  void (*fct)(),void (*prp)(),...)) ;
*/
extern void init_movinggc(R6(Display *d,Window w,struct movinggc *mgc,
			unsigned long background,unsigned long foreground,
			unsigned long shadow)) ;
extern struct row_column* scotch() ;
extern void walkrowcol(R6(struct row_column *b,
			void (*fct)(),int x,int y,int but,int dy)) ;
extern void button_extent(R1(struct moving_button *b)) ;
extern void display_button(R5(struct moving_button *b,int x,int y, int dx,int dy)) ;
extern void next_button(R1(struct moving_button *b)) ;
extern void sizerowcol(R1(struct row_column *b)) ;
extern void push_button(R4(struct moving_button *b,int x,int y,int but)) ;
extern int minimumheight(R2(struct moving_button *b,int x)) ;
extern void windowset(R2(struct row_column *b,Window x)) ;
extern void stretch(R2(struct moving_button *b,int x)) ;
extern void posit_button(R3(struct row_column *b,int x,int y)) ;
extern void resizex(R2(struct row_column *r,int x)) ;
extern int compute_height(R1(struct row_column *r)) ;
extern void compute_stretch(R2(struct row_column *r,int x)) ;
extern void compute_rowcol(R1(struct row_column *r)) ;
extern void compute_posit(R2(struct row_column *r,int x)) ;
extern void display_rowcol(R5(struct row_column *r,int x,int y,int dx,int dy)) ;
extern void push_rowcol(R4(struct row_column *r,int x,int y,int b)) ;

