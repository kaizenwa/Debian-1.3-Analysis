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
#include "bl.h"
#include "transfo.h"
#include "keyid.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <time.h>
#include <X11/Xutil.h>

void gameevent(bl,e)
struct bl *bl ;
XEvent *e ;
{	
char buf[LINE_LENGTH] ; /* keyboard texte */
int nb ;
int between ;
int i,j ;
struct point min,max ;


switch( e->type )
   {
   case ConfigureNotify :
	if ( bl->opt.verbose ) fprintf(stderr,"Configure notify!\n") ;
	if ( bl->x.dimx == ((XConfigureRequestEvent*)e)->width &&
	     bl->x.dimy == ((XConfigureRequestEvent*)e)->height )
		{
		break ;
		}
	if ( bl->opt.verbose ) fprintf(stderr,"New size!\n") ;
	bl->x.dimx = ((XConfigureRequestEvent*)e)->width ;
	bl->x.dimy = ((XConfigureRequestEvent*)e)->height ;
	initbuffer(&bl->opt,&bl->x) ;
	if ( bl->bloc.world )
		bl->bloc.world->world = 2 ; /* Must recompute points */
	updateworld( bl ) ;
	if ( bl->opt.state!=STOP && bl->opt.state!=SUSPEND )
		{
		XSync(bl->x.display,False) ;
		inittime(&bl->realtime,1) ;
		}
        {
        XSizeHints sh ;
        sh.flags = PPosition ;
        sh.x = ((XConfigureEvent*)e)->x ;
        sh.y = ((XConfigureEvent*)e)->y ;
        XSetWMNormalHints( bl->x.display,bl->x.window,&sh ) ;
        }

   	setargs(bl) ;
	break ;

   case NoExpose :
	if ( bl->opt.verbose ) fprintf(stderr,"NoExpose event\n") ;
	break ;

   case KeyPress :
	if ( bl->opt.verbose )
		{
		 fprintf(stderr,"KeyPress event\n") ;
		 fprintf(stderr,"Keycode %ud ou %x\n",
				((XKeyEvent*)e)->keycode,
				((XKeyEvent*)e)->keycode);
		 fprintf(stderr,"state %ud ou %x\n",((XKeyEvent*)e)->state,
				((XKeyEvent*)e)->state) ;
		 fprintf(stderr,"XKeysymToKeycode(1)=%d\n",
				XKeysymToKeycode(bl->x.display,0xFFB1)) ;
		}

	if ( bl->opt.state==DEMO ) endgame(bl,0) ;

	/* Not optimum, but table is small..... */
	/* Translate KeyPad code to the number 0 1 2 .... */
	nb = 1 ;
	for(i=0;i<NBKEYCODE;i++)
	   {
	   if ( ((XKeyEvent*)e)->keycode==bl->x.kpcode[i] )
		{
		buf[0] = bl->x.kpstring[i] ;
		break ;
		}
	   }

	if (i==NBKEYCODE) nb = XLookupString((XKeyEvent*)e,buf,256,0,0);
	if ( nb!=0 && bl->opt.verbose )
		fprintf(stderr,"String : %s\n",buf) ;
	if ( nb==0 ) break ;
	if (buf[0]==bl->key[KEY_QUIT] )
		{
		endgame(bl,0) ;
		bl->endplay = 1 ;
		break ;
		}
	if (buf[0]==bl->key[KEY_CANCEL] )
		{
		endgame(bl,1) ;
		break ;
		}
	if ( bl->opt.state==STOP )
		{
		startgame(bl) ;
		break ;
		}
	if ( bl->opt.state==SUSPEND )
		{
		unsuspend(bl,&bl->realtime) ;
		break ;
		}
	for(i=0;i<nb;i++)
	   {

	   if ( bl->opt.smooth ) between = BETWEEN ;
			else between = 1 ;

	   if ( buf[i]==bl->key[KEY_LEFT] )
		addtransfo(TXN,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_RIGHT] )
		addtransfo(TXP,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_UP] )
		addtransfo(TYN,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_DOWN] )
		addtransfo(TYP,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_DOWN_LEFT] )
		{
		buf[nb++] = bl->key[KEY_LEFT] ;
		buf[nb++] = bl->key[KEY_DOWN] ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_UP_LEFT] )
		{
		buf[nb++] = bl->key[KEY_LEFT] ;
		buf[nb++] = bl->key[KEY_UP] ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_UP_RIGHT] )
		{
		buf[nb++] = bl->key[KEY_UP] ;
		buf[nb++] = bl->key[KEY_RIGHT] ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_DOWN_RIGHT] )
		{
		buf[nb++] = bl->key[KEY_DOWN] ;
		buf[nb++] = bl->key[KEY_RIGHT] ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_ROT_X_POS] )
		addtransfo(RXP,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_ROT_X_NEG] )
		addtransfo(RXN,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_ROT_Y_POS] )
		addtransfo(RYP,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_ROT_Y_NEG] )
		addtransfo(RYN,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_ROT_Z_POS] )
		addtransfo(RZP,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_ROT_Z_NEG] )
		addtransfo(RZN,between,&bl->bloc,between) ;
	   else
	   if ( buf[i]==bl->key[KEY_DROP] )
		{
		j = 0 ;
		while( addtransfo(FALL,10,&bl->bloc,10) ) j++ ;
		if ( j!=0 ||
		     bl->realtime.lastfall - bl->realtime.lrtime >
			14*OPTIMUM_DISPLAY_TIME) /* If yet fallen */
		     /* Give him a last chance to move the bloc he just drop */
		     bl->realtime.lastfall = bl->realtime.lrtime +
				        14*OPTIMUM_DISPLAY_TIME ;
		if ( bl->opt.mode==TRAINING ) (void)splash(bl,&bl->realtime) ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_CANCEL] )
		{
		endgame(bl,1) ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_PAUSE] && bl->opt.mode!=TRAINING )
		{
		bl->opt.state = SUSPEND ;
	display_button( bl->menu.state,bl->menu.state->x,bl->menu.state->y,
                                bl->menu.state->dx,bl->menu.state->dy ) ;
		drawback(&bl->opt,&bl->x,&bl->draw,
			bl->bloc.world,&bl->bloc.tworld) ;
		}
	   else
	   if ( buf[i]==bl->key[KEY_QUIT] )
		{
		endgame(bl,0) ; return ;
		}
	   }
	break ;
   case KeyRelease :
	if ( bl->opt.verbose ) fprintf(stderr,"KeyRelease event\n") ;
	break ;
   case Expose :
	if ( bl->opt.verbose ) fprintf(stderr,"Expose event\n") ;
	switch( bl->opt.buffering )
	  {
	  case 2 :
		min.x = ((XExposeEvent*)e)->x ;
		min.y = ((XExposeEvent*)e)->y ;
		max.x = ((XExposeEvent*)e)->x+((XExposeEvent*)e)->width ;
		max.y = ((XExposeEvent*)e)->y+((XExposeEvent*)e)->height ;
		updatescreen(&bl->opt,&bl->x,&min,&max) ;
		break ;
	  case 1 :
	  case 3 :
	  case 4 :
	  case 5 :
		if ( ((XExposeEvent*)e)->count!=0 ) break ;
		drawback(&bl->opt,&bl->x,&bl->draw,
			bl->bloc.world,&bl->bloc.tworld) ;
		break ;
	  case 0 :
		if ( bl->opt.state==SUSPEND || bl->opt.state==STOP )
			drawback(&bl->opt,&bl->x,&bl->draw,
				bl->bloc.world,&bl->bloc.tworld) ;
		break ;
	  }
	if ( bl->opt.state!=STOP && bl->opt.state!=SUSPEND )
		{
		XSync(bl->x.display,False) ;
		inittime(&bl->realtime,1) ;
		}
	break ;
/*
	case FocusIn :
		inittime(&bl->realtime,1) ;
*/
   case MapNotify :
	if ( bl->opt.verbose ) fprintf(stderr,"Map notify!\n") ;
	unsuspend(bl,&bl->realtime) ; ;
	display_button( bl->menu.state,bl->menu.state->x,bl->menu.state->y,
                                bl->menu.state->dx,bl->menu.state->dy ) ;
   	setargs(bl) ;
	break ; 
   case FocusOut :
	if ( bl->opt.verbose ) fprintf(stderr,"Focus out!\n") ;
	if ( bl->opt.state!=SUSPEND && bl->opt.state!=STOP
		&& bl->opt.mode!=TRAINING )
		{
		bl->opt.state = SUSPEND ;
		drawback(&bl->opt,&bl->x,&bl->draw,
			bl->bloc.world,&bl->bloc.tworld) ;
		}
   case UnmapNotify :
	if ( bl->opt.verbose ) fprintf(stderr,"Unmap notify!\n") ;
	if ( bl->opt.state==RUN ) bl->opt.state = SUSPEND ;
	if ( bl->opt.state==DEMO )
		{
		endgame(bl,0) ;
		}
	display_button( bl->menu.state,bl->menu.state->x,bl->menu.state->y,
                                bl->menu.state->dx,bl->menu.state->dy ) ;
   	setargs(bl) ;
	break ;
   default :
	if ( bl->opt.verbose ) fprintf(stderr,"Event : %d\n",e->type) ;
	break ;
	}
}
