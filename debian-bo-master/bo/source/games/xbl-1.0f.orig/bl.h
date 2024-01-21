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

#include "define.h"
#include "menu.h"
#include "realtime.h"
#include "movingbloc.h"
#include "opt.h"
#include "x.h"
#include "draw.h"


struct bl
	{
	struct x x ;
	struct opt opt ;

	struct draw draw ;
	struct movingbloc bloc ;
	struct menu menu ;
	struct realtime realtime ;

	int nbbloc,nbcube,nblevel,score ;
	int hiscore ;
	int currentlevel ;
	int endplay ;
	char *boardkey[NB_KEYBOARD] ;
	char *key ;
	char *progname ;
	} ;

extern void initdisp(R2(struct opt* opt,struct x* x)) ;
extern void initdisp2(R2(struct opt* opt,struct x* x)) ;
extern void displaymoving(R4(struct opt *opt, struct x *x,
			     struct movingbloc *bloc,struct draw *draw)) ;
extern void initwin(R2(struct opt *opt,struct x *x)) ;
extern void setargs(R1(struct bl *bl)) ;
extern void initgc(R2(struct opt *opt,struct x *x)) ;
extern void initbuffer(R2(struct opt *opt,struct x *x)) ;
extern void zooevent(R2(struct bl *bl,XEvent *e)) ;
extern void gameevent(R2(struct bl *bl,XEvent *e)) ;


/* loop.c */
extern void loop(R1(struct bl *bl)) ;
extern void newworld(R1(struct bl *bl)) ;
extern void updateworld(R1(struct bl *bl)) ;
extern int splash(R2(struct bl *bl,struct realtime *r)) ;
extern void unsuspend(R2(struct bl *bl,struct realtime *r)) ;
extern void startgame(R1(struct bl *bl)) ;
extern void endgame(R2(struct bl *bl,int music)) ;
extern void updatework(R3(struct opt *opt,struct x *x,struct draw *draw)) ;


/* sound.c */
extern void savebell(R1(Display *d)) ;
extern void restorebell(R1(Display *d)) ;
extern void setbell(R3(Display *d,int p,int dur)) ;
extern void sound(R4(Display *d,int p,int dur,int vol)) ;
extern void playsound(R3(Display *d,int type,int vol)) ;
