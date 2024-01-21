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

#include <stdio.h>

#include "view.h"

struct draw	{
		struct point min,max ; /* Changing area */
		struct point oldmin,oldmax ; /* Last Changing area (buffer=3)*/
		struct viewtransfo view ;
		} ;

extern void drawback(R5(struct opt *opt,struct x *x,struct draw *draw,
                        struct bloc *world,struct transfo *tworld));
extern void drawrealback(R6(struct x *x,
                        struct bloc *world,struct transfo *tworld,
                        struct viewtransfo *view,Drawable d,struct opt *opt));
extern void updatescreen(R4(struct opt *opt,struct x *x,struct point *min,
				struct point *max)) ;

