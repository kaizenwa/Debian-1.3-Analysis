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

#include "view.h"
#include <X11/Xlib.h>

struct point { int coord[3] ;		/* 3D position */
	       int x,y ;		/* Screen Position */
	       int in_a_bloc ;		/* True if the point is set */
	       struct point *next ;	/* Next point */
	       struct edge *edge ;
	     } ;
struct edge  { struct point *start,*end ; /* Start and end edge point */
	       int nb_acces ;		  /* Number of faces pointing this */
	       struct edge *next ;	  /* Next edge */
	     } ;
struct face  { struct point *p[4] ;	/* The for points of face */
	       int level ;
	     } ;

struct bloc
	{
	int dx,dy,dz ;			/* Bloc sizes */
	char ***t ;			/* Data 0:empty */
	int nbcubes ;
	struct edge *edge ;
	int nbedges ;
	struct point *point,*firstpoint ;
	int nbpoints ;
	struct face *face ;
	int nbfaces ;
	char *name ;
	int nbsegments ;	/* Number of segment in next table */
	struct edge *segments ;
	int world ;		/* True if this bloc is the world */
	} ;

extern struct bloc *createbloc(R1(char *def)) ;
extern void notecube(R4(struct bloc *b,int x,int y,int z)) ;
extern void createfaces(R1(struct bloc *b)) ;
extern void createsegments(R1(struct bloc *b)) ;
extern void createedge(R7(struct bloc *b,int z1,int y1,int x1,
			  int z2,int y2,int x2)) ;
extern struct point *createpoint(R4(struct bloc *b,int x,int y,int z)) ;
extern struct bloc *allocbloc(R3(int dx,int dy,int dz)) ;
extern void freebloc(R1(struct bloc *b)) ;
extern void drawlinebloc(R8(Display *disp,Drawable dr,GC gc,struct bloc *b,
			struct transfo *t,struct viewtransfo *v,
			struct point *min,struct point *max )) ;
extern int clearlinebloc(R3(Display *disp,Drawable b,GC gc)) ;
extern void clearlastline(R3(Display *disp,Drawable b,GC gc)) ;
extern void forgetlinebloc() ;
extern void drawfacebloc(R9(Display *disp,Drawable dr,GC *gc,GC gcl,struct bloc *b,
			struct transfo *t,struct viewtransfo *v,
			struct point *min,struct point *max )) ;
extern void drawtranspbloc(R9(Display *disp,Drawable dr,GC gc,GC gcl,struct bloc *b,
			struct transfo *t,struct viewtransfo *v,
			struct point *min,struct point *max )) ;
extern void transfopoint(R5(struct bloc *b,struct transfo *t,
			struct viewtransfo *v,struct point *min,struct point *max)) ;

extern struct bloc *findbloc(R4(int x,int y,int z,struct bloc **b)) ;
extern struct bloc *copy_bloc(R2(struct bloc *b,struct bloc *bb)) ;
extern int depthbloc(R1(struct bloc *b)) ;
