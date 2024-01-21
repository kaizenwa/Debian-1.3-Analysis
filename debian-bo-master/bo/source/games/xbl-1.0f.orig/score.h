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

Window scorewin(R11( Display *d, int s, Window r, int depth, Visual *v,
                        unsigned long b, GC gc, 
			XFontStruct *xfont, Pixmap icone,
                        Colormap colormap,char *geom ));
int displayscore(R5(int x, int y, int z, int piece,int display)) ;
void scoreevent(R2(struct bl *bl,XEvent *event)) ;
void drawscores(R2(struct bl *bl,int draw)) ;
int addscore(R8(int x,int y,int z,int p,int nc,int nb,int nl,int score)) ;

