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
#include "score.h"
#include "buttons.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <time.h>

void loop(bl)
struct bl *bl ;
{
XEvent event ;
time_t last_event,tmp ;
void demo() ;

/*
while(1)
   {
   XNextEvent(bl->x.display,&event) ;
   printf("event %d\n",event.type) ;
   }
*/
/******************************************************************************/
/******************************************************************************/
/* Compute microsleep minimum delay time */
/* I subtract this value of the microsleep arg */
/* But it's not so perfect, the ``select'' have a ``random'' minimum time */
/* So I remove the minimum time from this program..... */
#ifdef NEVER_DEFINED

t = time(0L) ;
while( time(0L)==t ) ;		/* Wait for a ``second'' change */
t++;
for(i=0;time(0L)==t;i++) microsleep(1) ; /* How many time can I call in 1sec? */
bl->realtime.microsleepdelay = 1000000/i/ 2 ;
if ( bl->opt.verbose )
  fprintf(stderr,"minimum select time=%d micro sec.\n",1000000/i) ;

#else

bl->realtime.microsleepdelay = 0 ;

#endif
/******************************************************************************/
/******************************************************************************/

time(&last_event) ;
bl->opt.Time_to_demo = bl->opt.time_to_demo ;

do
   {
   if ( bl->opt.verbose )
	fprintf(stderr,"State = %d\n",bl->opt.state) ;

   /* Wait an event */

   while( XPending(bl->x.display)==0 ) /* If no event, animate the buttons */
	{
	walkrowcol(bl->menu.all,next_button,0,0,0,0) ; /* Display buttons */
   	if ( bl->opt.state!=STOP && bl->opt.state!=SUSPEND )
		{
			/* Here : Its RUNNING or DEMO */
		if ( bl->opt.state==DEMO ) demo(bl) ;
		nextframe(bl,&bl->realtime) ; /* The game run */
		time(&last_event) ;
		}
	else
		{
		/* SUSPEND or STOPPED */
		if ( bl->opt.state!=SUSPEND )
		   {
		   tmp = bl->opt.time_to_demo - ( time(0L) - last_event ) ;
		   if ( bl->opt.Time_to_demo != tmp )
			{
			if ( tmp<=-1 )
				{
				startgame(bl) ;
				bl->opt.state = DEMO ;
				display_button(bl->menu.state,
				       bl->menu.state->x,bl->menu.state->y,
				       1,1) ;
				demo((struct bl*)0) ; /* Indicate new demo */
				}
			else
				{
				bl->opt.Time_to_demo = tmp ;
				sprintf(bl->menu.frame->text->current_text,
					"%ds. to demo",tmp) ;
				display_button(bl->menu.frame,
				       bl->menu.frame->x,bl->menu.frame->y,
				       1,1) ;
				}
			}
		   }
		microsleep(100000-bl->realtime.microsleepdelay ) ;
		}
	XSync(bl->x.display,False) ;
	if ( bl->endplay ) break ;
	}
   if ( bl->endplay ) break ;

   XNextEvent(bl->x.display,&event) ;

   if ( event.type == ButtonPress ||
	     event.type == ButtonRelease ) time(&last_event) ;

   if ( event.type == DestroyNotify )
	{
	printf("Somebody destroy me\n");
	return ;
	}
   if ( ((XAnyEvent*)&event)->window==bl->menu.window )
	{
	menuevent(bl,&event) ;
	if ( bl->endplay ) return ;
	continue ;
	}
   if ( ((XAnyEvent*)&event)->window==bl->x.wscore )
	{
	scoreevent(bl,&event) ;
	continue ;
	}
   if ( ((XAnyEvent*)&event)->window==bl->menu.zoo )
	{
	zooevent(bl,&event) ;
	continue ;
	}
   gameevent(bl,&event) ;
   }
while(!bl->endplay) ;

}	

/*****************************************************************************/
/*****************************************************************************/

void newworld( bl )
struct bl *bl ;
{

bl->nbbloc = 0 ;
bl->nbcube = 0 ;
bl->nblevel= 0 ;
bl->score  = 0 ;
bl->currentlevel = bl->opt.level ;
bl->endplay = 0 ;

if ( bl->bloc.world ) freebloc(bl->bloc.world) ;
bl->bloc.world = allocbloc(bl->opt.wx,bl->opt.wy,bl->opt.wz) ;
bl->bloc.world->world = 2 ; /* Must all recalculate */

createtransfo( HOMO,&bl->bloc.tworld,1.,
		bl->bloc.world->dx/-2. ,
		bl->bloc.world->dy/-2. , PERSP ) ;

if ( bl->bloc.thisone == -1 )
  {
    newfallingbloc(&bl->bloc,-1) ;
    displaystat(bl,bl->bloc.thisone) ;
    displaynextpiece( bl ) ;
  }
updateworld( bl ) ;
drawscores(bl,bl->menu.showscore) ;
}

/*****************************************************************************/
/*****************************************************************************/

void updateworld( bl )
struct bl *bl ;
{
struct bloc *b ;
int i ;

b = bl->bloc.world ;
b->nbedges = 0 ;
for(i=0;i<b->nbpoints;i++) b->point[i].edge = 0 ;

for(i=0;i<=b->dx;i++)
        {
        createedge(b,0,0,i,b->dz,0,i) ;
        createedge(b,0,b->dy,i,b->dz,b->dy,i) ;
        createedge(b,b->dz,0,i,b->dz,b->dy,i) ;
        }
for(i=0;i<=b->dy;i++)
        {
       	createedge(b,b->dz,i,0,b->dz,i,b->dx) ;
	if ( i!=0 && i!=b->dy )
		{
        	createedge(b,0,i,b->dx,b->dz,i,b->dx) ;
        	createedge(b,0,i,0,b->dz,i,0) ;
		}
        }
for(i=0;i<b->dz;i++)
        {
        createedge(b,i,0,0,i,b->dy,0) ;
        createedge(b,i,b->dy,0,i,b->dy,b->dx) ;
        createedge(b,i,b->dy,b->dx,i,0,b->dx) ;
        createedge(b,i,0,b->dx,i,0,0) ;
        }
createfaces(bl->bloc.world) ;
createsegments(bl->bloc.world) ;
drawback(&bl->opt,&bl->x,&bl->draw,
	bl->bloc.world,&bl->bloc.tworld) ;
}

/*****************************************************************************/
/* Return 0 if no level destroy because sound take time */
/* It's for realitme */
/*****************************************************************************/

int splash(bl,r)
struct bl *bl ;
struct realtime *r ;
{
int i,j ;
time_t t ;
char buf[80] ;
static int levelscore[] = { 1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,
				2584,4181,6765,10946,17711 } ;


i = orbloc( &bl->bloc , bl->bloc.world ) ;
bl->nblevel += i ;
bl->nbbloc++ ;
bl->nbcube += bl->bloc.b->nbcubes ;
j = levelscore[bl->currentlevel]*i ;
if ( bl->opt.mode!=TRAINING || bl->opt.state==DEMO )
	if ( bl->bloc.nextpiece )
	bl->score += bl->bloc.b->nbcubes + j*7 ;
	else
	bl->score += bl->bloc.b->nbcubes + j*10 ;
j = bl->currentlevel ;
bl->currentlevel = bl->opt.level+bl->nblevel/bl->opt.wz ;

if ( bl->score>bl->hiscore ) bl->hiscore = bl->score ;

display_button( bl->menu.score,bl->menu.score->x,bl->menu.score->y,1,1 ) ;
display_button( bl->menu.hiscore,bl->menu.hiscore->x,bl->menu.hiscore->y,1,1 ) ;
sprintf(buf,"XBlockOut %s   score=%d/%d",
	XBLVERSION,bl->score,bl->hiscore) ;
XStoreName(bl->x.display,bl->x.window,buf) ;
display_button( bl->menu.cube,bl->menu.cube->x,bl->menu.cube->y,1,1 ) ;
display_button( bl->menu.bloc,bl->menu.bloc->x,bl->menu.bloc->y,1,1 ) ;
display_button( bl->menu.level,bl->menu.level->x,bl->menu.level->y,1,1 ) ;
display_button( bl->menu.destroylevel,bl->menu.destroylevel->x,bl->menu.destroylevel->y,1,1 ) ;
displaystairs( &bl->menu ) ;

updateworld( bl ) ; /*  replaced by the 2 next lines (more fast) but don't work */
/*
createfaces(bl->bloc.world) ;
drawback(&bl->opt,&bl->x,&bl->draw,bl->bloc.world,&bl->bloc.tworld) ;
*/


displaystat(bl,bl->bloc.thisone) ;
newfallingbloc( &bl->bloc,-1 ) ;
displaystat(bl,bl->bloc.thisone) ;
displaynextpiece(bl) ;
displaymoving(&bl->opt,&bl->x,&bl->bloc,&bl->draw) ;
if ( intersection( &bl->bloc,bl->bloc.world ) )
	{
	endgame(bl,1) ;
	return(1) ;
	}

if ( j!=bl->currentlevel )
	{
	/* The change level sound */
	playsound(bl->x.display,3,bl->opt.volume) ;
	i = 1 ;
	}
   else {
	if ( i!=0 )
		{
		/* The delete level sound */
		playsound(bl->x.display,2,bl->opt.volume) ;
		i = 1 ;
		}
	   else {
		/* The PLOP sound */
		playsound(bl->x.display,1,bl->opt.volume) ;
		i = 0 ;
		}
	}
if ( i )
	{
	XSync(bl->x.display,False) ;
	if ( bl->opt.volume == 0 ) microsleep(500000) ;
	inittime(&bl->realtime,1) ;
	return(1) ;
	}
   else {
	XSync(bl->x.display,False) ;
	r->nocalcdisplaytime=1 ;
	time( &t ) ;
	r->starttime += t - r->lastcurrenttime ;
	r->lastcurrenttime = t ;
	r->lastfall = r->lrtime - 1.01*r->falltime ;
	return(0) ;
	}

}

/*****************************************************************************/
/*****************************************************************************/

void startgame(bl)
struct bl *bl ;
{
XEvent e ;

bl->menu.width->typet =
bl->menu.height->typet =
bl->menu.depth->typet =
bl->menu.land->typet =
bl->menu.typepiece->typet =
bl->menu.training->typet =
bl->menu.startlevel->typet = FLAT_TEXT ;
bl->opt.state = RUN ;
bl->currentlevel = bl->opt.level ;

newworld(bl) ;

displaymenu( &bl->menu,0,0,
		       bl->menu.all->dx+bl->menu.layersize,bl->menu.all->dy) ;
newfallingbloc(&bl->bloc,-1) ;
displaystat(bl,bl->bloc.thisone);
displaynextpiece(bl) ;
displaymoving(&bl->opt,&bl->x,&bl->bloc,&bl->draw) ;

/* Remove all unecessary event */
while( True==XCheckWindowEvent(bl->x.display,bl->x.window,KeyPressMask,&e) ) ;

XSync(bl->x.display,False) ;
inittime(&bl->realtime,1) ;
}

/*****************************************************************************/
/*****************************************************************************/

void endgame(bl,music)
struct bl *bl ;
int music ;
{
if ( bl->opt.state==STOP ) return ; /* Yet stopped */

bl->menu.width->typet =
bl->menu.height->typet =
bl->menu.depth->typet =
bl->menu.land->typet =
bl->menu.typepiece->typet =
bl->menu.training->typet =
bl->menu.startlevel->typet = RELIEF_TEXT ;

bl->menu.width->direction =
bl->menu.height->direction =
bl->menu.depth->direction =
bl->menu.land->direction =
bl->menu.typepiece->direction =
bl->menu.training->direction =
bl->menu.startlevel->direction = PULLING_TEXT ;

if ( bl->opt.state!=DEMO )
   {
   if ( addscore(bl->opt.wx,bl->opt.wy,bl->opt.wz,bl->bloc.typepiece,
		bl->nbcube,bl->nbbloc,bl->nblevel, bl->score) )
	{
	if ( bl->menu.showscore==0 )
        	{
		XMapWindow(bl->x.display,bl->x.wscore) ;
        	}
	else
	  {
	    drawscores(bl,bl->menu.showscore) ;
	  }

	XRaiseWindow(bl->x.display,bl->x.wscore ) ;
	/*XSetInputFocus(bl->x.display,bl->x.wscore,RevertToNone,CurrentTime);*/
	}
    if ( bl->score!=0 && music ) playsound(bl->x.display,4,bl->opt.volume) ;
    }
bl->opt.state = STOP ;
display_button(bl->menu.state, bl->menu.state->x,bl->menu.state->y, 1,1) ;

XFlush(bl->x.display) ;
}

/*****************************************************************************/
/*****************************************************************************/

void unsuspend(bl,r)
struct bl * bl ;
struct realtime *r ;
{
time_t t ;

if ( bl->opt.state!=SUSPEND ) return ;
bl->opt.state = RUN ;
/* I redraw to clear the message : press a key */
drawback(&bl->opt,&bl->x,&bl->draw,
        bl->bloc.world,&bl->bloc.tworld) ;
display_button( bl->menu.state,bl->menu.state->x,bl->menu.state->y,
				bl->menu.state->dx,bl->menu.state->dy ) ;
displaymoving(&bl->opt,&bl->x,&bl->bloc,&bl->draw) ;
time( &t ) ;
r->starttime += t - r->lastcurrenttime ;
r->nocalcdisplaytime=1 ;
r->lastcurrenttime = t ;
}

/*****************************************************************************/
/*****************************************************************************/

void drawscores(bl,dodraw)
struct bl *bl ;
int dodraw ;
{
bl->hiscore = displayscore(bl->opt.wx,bl->opt.wy,bl->opt.wz,
			   bl->bloc.typepiece,
			   dodraw);
if ( bl->menu.window )
display_button( bl->menu.hiscore,bl->menu.hiscore->x,bl->menu.hiscore->y,1,1) ;
} 
