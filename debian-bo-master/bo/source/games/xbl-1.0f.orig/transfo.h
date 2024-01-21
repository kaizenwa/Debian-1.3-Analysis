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

#ifndef ROTX

#define ROTX 0
#define ROTY 1
#define ROTZ 2
#define DILX 3
#define DILY 4
#define DILZ 5
#define HOMO 6
#define UNIT 7

struct transfo
	{
	float mat[3][3] ;
	float vec[3] ;
	} ;

#include "ansi.h"

void createtransfo(R6(int typ,struct transfo *t,float val,
			float tx,float ty,float tz)) ;
void traXtra(R3(struct transfo *tg,struct transfo *td,struct transfo *tr)) ;
void printtra(R1(struct transfo *t)) ;
void traXvec(R3(struct transfo *t,float v[3],float r[3])) ;
void matXmat(R3(float a[3][3],float b[3][3],float c[3][3])) ;
void matXvec(R3(float a[3][3],float b[3],float c[3])) ;
void trapXtrap(R3(struct transfo *tg,struct transfo *td,struct transfo *tr)) ;


#endif
