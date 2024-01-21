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

#ifndef NBTR

#include "ansi.h"

#include "transfo.h"
#include "bloc.h"

#define NBTR 40

#define TXP 0			/* Don't change the order */
#define TXN 1			/* from TXP to TZN */
#define TYP 2			/* Don't add others value between */
#define TYN 3
#define TZP 4
#define TZN 5
#define RZP 6
#define RZN 7
#define RYP 8
#define RYN 10
#define RXP 11
#define RXN 12
#define FALL 13
#define UNKNOWN 14

#define PERSP 4.			/* Tuning of perspective */

struct time_transfo
	{
	int type ;			/* UNKNOWN, TX TY */
	float v,dv ;			/* Current and increment value for TR */
	float t[4],dt[4],vt[2],dvt[2] ;	/* Idem for rotations */
	struct transfo final ;		/* final transformation */
	int timelife ;			/* Decrement to 0 */
	} ;

struct movingbloc
	{
	struct bloc *b ; 	/* Original bloc */
	struct bloc *work ; 	/* Pointe on current bloc */
	struct transfo real ;	/* Real object position */
	struct transfo visual ;	/* Current visual transformation */
	struct transfo starting ; /* Initial Transformation */
	int nb ;		/* Number of concurent transformation */
	struct time_transfo t[NBTR] ;
	float  sdepth ;		/* Start depth movment */
	float  depth ;		/* current depth */
	float  fdepth ;		/* Final depth */
	float  ddepth ;		/* Speed of fall */

	int tx,ty,tz ; /* Difference between the world and this piece */
	float upleft[3] ;
 
	struct bloc *world ;
	struct transfo tworld ;

        int nextpiece,thisone ;
	int typepiece ;
        struct bloc *piece[40] ;
        int statpiece[40] ;
        int flat,simple,complex ;
	} ;

extern int intersection(R2(struct movingbloc *m,struct bloc *b)) ;
extern int orbloc(R2(struct movingbloc *m,struct bloc *b)) ;
extern int delete_level(R2(struct movingbloc *m,struct bloc *b)) ;
extern void initmovingbloc(R2(struct movingbloc *b,struct transfo *t)) ;
extern int stupidaddtransfo(R4(int type, int between,struct movingbloc *b,
				int l)) ;
extern int addtransfo(R4(int type, int between,struct movingbloc *b, int l)) ;
extern void dontaccept(R8(struct movingbloc *b,struct transfo *save,int snb,
				int stx, int sty, int stz,float sf,float sd)) ;
extern int nextime(R1(struct movingbloc *b)) ;
extern void createmove(R4(int type,struct time_transfo *t,int between,
			struct movingbloc *b)) ;
extern void newfallingbloc( R2(struct movingbloc *b,int num)) ;

extern void initbloc( R1(struct movingbloc *b)) ;
extern void push_moving_bloc( R1(struct movingbloc *b) ) ;
extern void pull_moving_bloc( R1(struct movingbloc *b) ) ;
extern void validate_moving_bloc() ;


#endif
