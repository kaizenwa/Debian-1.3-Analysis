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
#ifndef FACECOLOR

#define FACECOLOR 6 /* Do Not Change ! */

#define FLAT	0
#define SIMPLE	1
#define COMPLEX	2

#define SHOW	0
#define STOP	1
#define SUSPEND 2
#define RUN	3
#define DEMO	4
#define SPEEDTEST 5

#define USER	0
#define SMALL	1
#define BIG	2

#define PLAY	0
#define TRAINING 1

#define PROX(A) ( (A)<0 ? (int)((A)-.5) : (int)((A)+.5) )

#define NB_KEYBOARD 6 /* Number of different keyboard */

#define LINE_LENGTH 256 /* For some buffer */

#endif
