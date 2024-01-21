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
#include <X11/Xutil.h>


#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif


/********************************************************************/
/* Display all the zoo (no clipping) */
/********************************************************************/
void displayzoo(bl,doclear,dodraw)
struct bl *bl ;
int doclear ;
int dodraw ;
{
int i ;
int dx,dy ;
int lastpiece ;
struct transfo id ;
struct point min,max ;
struct viewtransfo v ;
int xmin,ymin,xmax,ymax ;
char buf[LINE_LENGTH] ;

if ( !dodraw ) return ;

dx = bl->menu.xzoo/ZOO_NBX_PIECES ;
dy = bl->menu.yzoo/ZOO_NBY_PIECES ;

switch(bl->bloc.typepiece)
	{
	case FLAT    : lastpiece = bl->bloc.flat    ; break ;
        case SIMPLE  : lastpiece = bl->bloc.simple  ; break ;
        case COMPLEX : lastpiece = bl->bloc.complex ; break ;
	default	     : lastpiece = 0 ; /* Only to remove a GCC warning */
        }

if ( doclear ) XClearWindow(bl->x.display , bl->menu.zoo) ;

createtransfo(UNIT,&id,0.,-2.5,-2.5,5.) ;

for(i=0;i<lastpiece;i++)
	{
	xmin = (i%ZOO_NBX_PIECES)*dx ;
	ymin = (i/ZOO_NBX_PIECES)*dy+4+bl->menu.char_height ;
	xmax = xmin + dx - 2 ;
	ymax = ymin + dy - 2 ;

	v.xcenter = (xmin+xmax)/2+2 ;
	v.ycenter = (ymin+ymax)/2+2 ;
	v.xprod   = (xmax-xmin) ;
	v.yprod   = (ymax-ymin) ;


	if ( bl->opt.drawmode==2 )
		{
		createfaces( bl->bloc.piece[i] ) ;
		drawtranspbloc( bl->x.display , bl->menu.zoo ,
				bl->x.transp ,
				bl->x.white , bl->bloc.piece[i] , &id ,
				&v , &min,&max ) ;
		}
	else
		{
		drawlinebloc( bl->x.display , bl->menu.zoo ,
				bl->x.white , bl->bloc.piece[i] , &id ,
				&v , &min,&max ) ;
		}

	sprintf(buf,"%-3d %s",bl->bloc.statpiece[i],bl->bloc.piece[i]->name) ;	
	XDrawString(bl->x.display,bl->menu.zoo,bl->menu.text,
		  xmin + 2 , ymin - 4 , buf , (int)strlen(buf)
		) ;
	}
}

/********************************************************************/
/* Display the name and the statistics on the piece */
/********************************************************************/
void displaystat(bl,i)
struct bl * bl ;
int i ;
{
int dx,dy,xmin,ymin ;
char buf[LINE_LENGTH] ;

if ( bl->menu.zoo==0 ) return ; /* No open window (speed test) */
dx = bl->menu.xzoo/ZOO_NBX_PIECES ;
dy = bl->menu.yzoo/ZOO_NBY_PIECES ;
xmin = (i%ZOO_NBX_PIECES)*dx ;
ymin = (i/ZOO_NBX_PIECES)*dy+bl->menu.char_height+4 ;
sprintf(buf,"%-3d",bl->bloc.statpiece[i]) ;	
XDrawImageString(bl->x.display,bl->menu.zoo,bl->menu.text,
		  xmin + 2 , ymin - 4 , buf , strlen(buf) ) ;
}

/********************************************************************/
/* The zoo window event */
/********************************************************************/
void zooevent(bl,event)
struct bl *bl ;
XEvent *event ;
{
int i ;

switch( event->type )
   {
   case ConfigureNotify :
     if ( bl->opt.verbose )
       {
	 fprintf(stderr,"Zoo:Configure notify\n") ;
	 fprintf(stderr,"xzoo = %d yzoo = %d e->width %d e->height %d\n",
		 bl->menu.xzoo,bl->menu.yzoo,
		 ((XConfigureEvent*)event)->width,
		 ((XConfigureEvent*)event)->height);
       }
	if ( bl->menu.xzoo != ((XConfigureEvent*)event)->width ||
	     bl->menu.yzoo != ((XConfigureEvent*)event)->height )
		{
		/* We redraw on new size */
		bl->menu.xzoo = ((XConfigureEvent*)event)->width ;
		bl->menu.yzoo = ((XConfigureEvent*)event)->height ;
		/* Now I muste redraw zoo */
		XClearWindow(bl->x.display,bl->menu.zoo) ;
		}
        {
        XSizeHints sh ;
        sh.flags = PPosition ;
        sh.x = ((XConfigureEvent*)event)->x ;
        sh.y = ((XConfigureEvent*)event)->y ;
        XSetWMNormalHints( bl->x.display,bl->menu.zoo,&sh ) ;
        }

     setargs(bl) ;
	break ;
   case Expose :
     if ( bl->opt.verbose ) fprintf(stderr,"Zoo:Expose\n") ;
	do	{
		i = XCheckWindowEvent(bl->x.display,bl->menu.zoo,
					ExposureMask,event) ;
		}
	while(i==True) ;
	displayzoo(bl,0,1) ;
	break ;
   case MapNotify :
   case UnmapNotify :
     if ( bl->opt.verbose ) fprintf(stderr,"Zoo:map/unmap notify\n") ;
	if ( (bl->menu.showzoo    && event->type==UnmapNotify) ||
	     (bl->menu.showzoo==0 && event->type==MapNotify) )
	push_button(bl->menu.viewzoo,
		    bl->menu.viewzoo->x+bl->menu.viewzoo->dx/2,
		    bl->menu.viewzoo->y+bl->menu.viewzoo->dy/2,
		    1) ;
     setargs(bl) ;
	break ;
   case ReparentNotify :
	break ;
   default :
	fprintf(stderr,"Unknown event type in zoo window\n") ;
	fprintf(stderr,"event.type = %d\n",event->type) ;
   }
}
