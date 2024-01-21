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
#include "movingbloc.h"
#include "opt.h"
#include "x.h"
#include "draw.h"
#include "define.h"

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

/*#define XDEBUG*/


#ifdef XDEBUG
#define XPR(X,S) (XFillRectangle(X->display,X->window,X->black,0,0,200,40),XDrawString(X->display,X->window,X->white,3,20,S,strlen(S)))
#define SLP XFlush(x->display) ; sleep(1) 
#else
#define XPR(X,S) 
#define SLP
#endif

/*ARGSUSED*/
void tstgc(x,d)
struct x *x ;
Drawable d ;
{
XFillRectangle(x->display,x->work,x->black	,0,0,10,10) ;
XFillRectangle(x->display,x->work,x->movingwhite,1,1,8,11) ;
XFillRectangle(x->display,x->work,x->white	,2,2,6,12) ;

XFillRectangle(x->display,x->work,x->black	,11,11,10,10) ;
XFillRectangle(x->display,x->work,x->white	,12,12,8,10) ;
XFillRectangle(x->display,x->work,x->movingwhite,13,13,6,10) ;

XFillRectangle(x->display,x->work,x->white	,21,21,10,10) ;
XFillRectangle(x->display,x->work,x->black	,22,22,8,10) ;
XFillRectangle(x->display,x->work,x->movingwhite,23,23,6,10) ;

XFillRectangle(x->display,x->work,x->white	,31,31,10,10) ;
XFillRectangle(x->display,x->work,x->movingwhite,32,32,8,10) ;
XFillRectangle(x->display,x->work,x->black	,33,33,6,10) ;

XFlush(x->display) ;
}

/* Display image on the screen */

void updatescreen(opt,x,min,max)
struct opt *opt ;
struct x *x ;
struct point *min,*max ;
{
XPR(x,"(updatescreen") ;

if ( opt->verbose )
	 fprintf(stderr,"start updatescreen\n") ;

switch( opt->buffering )
  {
  case 1 :
  case 2 :
	if ( opt->verbose )
	 fprintf(stderr,"Start of copy work->screen %dx%d %d %d\n",
			min->x,min->y,max->x-min->x,max->y-min->y) ;
SLP;
	XCopyArea(x->display,x->work,x->window,x->copybuffer,
		min->x,min->y,max->x-min->x,max->y-min->y,
		min->x,min->y) ;
	if ( opt->verbose ) fprintf(stderr,"end of copy work->screen\n") ;
	break ;
  case 0 :
	break ;
  case 3 : /* Echange */
	{
	GC tmp ;
	
	tmp = x->movingwhite ;
	x->movingwhite = x->movingwhite2 ;
	x->movingwhite2 = tmp ;
	tmp = x->movingblack ;
	x->movingblack = x->movingblack2 ;
	x->movingblack2 = tmp ;
	tmp = x->movingtransp ;
	x->movingtransp = x->movingtransp2 ;
	x->movingtransp2 = tmp ;

	x->colormap = x->color2 ;
	x->color2 = x->color1 ;
	x->color1 = x->colormap ;

/* Why these tow lines, avoid flicking screen ????? */
/*
XSync(x->display,False) ;
microsleep(400000);
*/
	XInstallColormap(x->display,x->color1) ;
	break ;
	}
   case 4 :
   case 5 :
	break;
   }

if ( opt->verbose )
	 fprintf(stderr,"end updatescreen\n") ;

XPR(x,"updatescreen)") ;
}



void updatework(opt,x,draw )
struct opt *opt ;
struct x *x ;
struct draw *draw ;
{
int i ;

XPR(x,"(updatework  ") ;
if ( opt->verbose )
	 fprintf(stderr,"start updatework\n") ;
SLP;

switch( opt->buffering )
  {
  case 2 :
	i = 0 ;
	if ( opt->drawmode==0 && opt->clearline )
		i = clearlinebloc( x->display,x->work,x->clearline ) ;
	if ( i==0 )
	        XCopyArea(x->display,x->back,x->work,x->white,
			  draw->min.x,draw->min.y,
			  draw->max.x-draw->min.x,draw->max.y-draw->min.y,
			  draw->min.x,draw->min.y) ;
	break ;
  case 1 :
	if ( opt->drawmode==0 && opt->clearline )
		(void)clearlinebloc( x->display,x->work,x->black ) ;
	else
	XFillRectangle(x->display,x->work,x->black,draw->min.x,draw->min.y,
		       draw->max.x-draw->min.x,draw->max.y-draw->min.y ) ;
	break ;
  case 0 :
	break ;
  case 3 :
	if ( opt->drawmode==0 && opt->clearline )
		(void)clearlastline( x->display,x->window,x->movingblack2 ) ;
	else
	XFillRectangle(x->display,x->window,x->movingblack2,
		       draw->oldmin.x,draw->oldmin.y,
		       draw->oldmax.x-draw->oldmin.x,
		       draw->oldmax.y-draw->oldmin.y ) ;
	break ;
  case 4 :
	if ( opt->drawmode==0 && opt->clearline )
		(void)clearlinebloc( x->display,x->window,x->movingblack ) ;
	else
	XFillRectangle(x->display,x->window,x->movingblack,
		       draw->min.x,draw->min.y,
		       draw->max.x-draw->min.x,draw->max.y-draw->min.y ) ;
	break ;
  case 5 :
	if ( opt->drawmode==0 && opt->clearline )
		(void)clearlinebloc( x->display,x->window,x->clearline ) ;
	else
	XCopyArea(x->display,x->work,x->window,x->white,draw->min.x,draw->min.y,
		  draw->max.x-draw->min.x,draw->max.y-draw->min.y,
		  draw->min.x,draw->min.y) ;
	break ;
   }
if ( opt->verbose )
	 fprintf(stderr,"end updatework\n") ;
XPR(x,"updatework)  ") ;
}



void displaymoving(opt,x,bloc,draw)
struct opt *opt ;
struct x *x ;
struct movingbloc *bloc ;
struct draw *draw ;
{
struct point min,max,newmin,newmax ;
Drawable d ;

XPR(x,"(displaymovin") ;
if ( opt->verbose )
	 fprintf(stderr,"start displaymoving\n") ;
SLP;

d  = x->window ;

switch( opt->buffering )
   {
   case 1 :
   case 2 : d = x->work ;
	    updatework(opt,x,draw) ;
	    break ;
   case 0 : 
	    drawback(opt,x,draw,bloc->world,&bloc->tworld) ;
	    break ;
   case 3 :
   case 4 :
   case 5 :
	    updatework(opt,x,draw) ;
	    break ;
   }

XPR(x,"(draw moving ") ;
if ( opt->state!=STOP && opt->state!=SUSPEND )
switch( opt->drawmode )
   {
   case 0 :
	   drawlinebloc( x->display,d,
			 opt->buffering==3?x->movingwhite2:x->movingwhite,
			 bloc->b, &bloc->visual ,&draw->view,
			 &newmin,&newmax);
	   break ;
   case 2 :
	   drawtranspbloc( x->display,d,
			   opt->buffering==3?x->movingtransp2:x->movingtransp,
			   opt->buffering==3?x->movingwhite2:x->movingwhite,
			   bloc->b, &bloc->visual ,&draw->view,
			   &newmin,&newmax);
	   break ;
  }
newmin.x -= opt->linewidth/2 ;
newmin.y -= opt->linewidth/2 ;
newmax.x += opt->linewidth/2 ;
newmax.y += opt->linewidth/2 ;

XPR(x,"draw moving) ") ;
SLP ;

switch( opt->buffering )
   {
   case 2 :
   case 1 :
	if ( newmin.x<draw->min.x ) min.x = newmin.x ;
		     	       else min.x = draw->min.x ;
	if ( newmin.y<draw->min.y ) min.y = newmin.y ;
		     	       else min.y = draw->min.y ;
	if ( newmax.x>draw->max.x ) max.x = newmax.x ;
			       else max.x = draw->max.x ;
	if ( newmax.y>draw->max.y ) max.y = newmax.y ;
			       else max.y = draw->max.y ;
   case 3 :
   case 4 :
   case 5 :
	if ( opt->buffering!=2 && opt->buffering!=1 )
		{
		min = newmin ;
		max = newmax ;
		}
	updatescreen(opt,x,&min,&max) ;
	draw->oldmin = draw->min ;
	draw->oldmax = draw->max ;
	draw->min = newmin ;
	draw->max = newmax ;
	break ;
   case 0 : 
	    break ;
   }
if ( opt->verbose )
	 fprintf(stderr,"end displaymoving\n") ;
XPR(x,"displaymovin)") ;
SLP ;
}




void drawback(opt,x,draw,world,tworld)
struct opt *opt ;
struct x *x ;
struct draw *draw ;
struct bloc *world ;
struct transfo *tworld ;
{
XPR(x,"(drawback    ") ;
if ( opt->verbose )
	 fprintf(stderr,"start drawback\n") ;

draw->min.x = draw->min.y = 0 ;
draw->max.x = x->dimx+1 ;
draw->max.y = x->dimy+1 ;

draw->view.xcenter = x->dimx/2 ;
draw->view.ycenter = x->dimy/2 ;

if ( x->dimy/(float)x->dimx > world->dy/(float)world->dx )
        {
        draw->view.xprod   = PERSP*(x->dimx-1)/world->dx ;
        draw->view.yprod   = draw->view.xprod ;
        }
else    {
        draw->view.yprod   = PERSP*(x->dimy-1)/world->dy ;
        draw->view.xprod   = draw->view.yprod ;
        }

draw->oldmin.x = draw->oldmin.y = draw->oldmax.x = draw->oldmax.y = 0 ;

switch( opt->buffering )
   {
   case 2 :
	    if ( opt->verbose ) fprintf(stderr,"Create static image\n") ;
	    XFillRectangle(x->display,x->back,x->black,
			  0,0,x->dimx,x->dimy ) ;
	    drawrealback(x,world,tworld,&draw->view,x->back,opt) ;
	    updatework(opt,x,draw) ;
	    updatescreen(opt,x,&draw->min,&draw->max) ;
	    break ;
   case 1 : 
   case 5 :
	    XFillRectangle(x->display,x->work,x->black,
			  0,0,x->dimx,x->dimy ) ;
	    drawrealback(x,world,tworld,&draw->view,x->work,opt) ;
	    XCopyArea(x->display,x->work,x->window,x->white,
			  0,0,x->dimx,x->dimy,0,0 ) ;
	    if ( opt->buffering==1 )
	    	XFillRectangle(x->display,x->work,x->black,
		 	0,0,x->dimx,x->dimy ) ;
	    break ;
   case 0 :
   case 3 :
   case 4 :
	    XClearWindow( x->display,x->window ) ;
	    drawrealback(x,world,tworld,&draw->view,x->window,opt) ;
	    break ;
   }
if ( opt->buffering==2 ) XSetTile( x->display, x->clearline , x->back ) ;
if ( opt->buffering==5 ) XSetTile( x->display, x->clearline , x->work ) ;

if ( opt->verbose )
	 fprintf(stderr,"end drawback\n") ;
XPR(x,"drawback)    ") ;
}


void ShadowText(x,d,b,w,t,dh)
struct x *x ;
Drawable d ;
GC b,w ;
char *t ;
int dh ;
{
int ascent,descent,dir ;
XCharStruct o ;
int h,l ;
int px,py ;

XTextExtents(x->xfont,t,(int)strlen(t),&dir,&ascent,&descent,&o) ;
h = ascent+descent ;
l = strlen(t) ;
px = (x->dimx-o.width)/2 ;
py = (x->dimy+h*dh)/2 ;

XDrawString(x->display,d,b,px+1,py,   t,l) ;
XDrawString(x->display,d,b,px+2,py,   t,l) ;
XDrawString(x->display,d,b,px+1,py+1, t,l) ;
XDrawString(x->display,d,b,px+2,py+1, t,l) ;
XDrawString(x->display,d,b,px+1,py+2, t,l) ;
XDrawString(x->display,d,b,px+2,py+2, t,l) ;
XDrawString(x->display,d,w,px  ,py,   t,l) ;
}


void drawrealback(x,world,tworld,view,d,opt)
struct x *x ;
struct bloc *world ;
struct transfo *tworld ;
struct viewtransfo *view ;
Drawable d ;
struct opt *opt ;
{
struct point min,max ;

XPR(x,"(drawrealback") ;

drawlinebloc( x->display,d,x->grid,
              world, tworld ,view,&min,&max);

forgetlinebloc() ;

drawfacebloc( x->display,d,x->face,x->black,
              world, tworld ,view,&min,&max);

if ( opt->presskey==0 && (opt->state==STOP || opt->state==SUSPEND) )
	{
	static GC blac=0,whit=0 ;

	if ( blac==0 )
		{
		XGCValues xgc ;
		blac = XCreateGC(x->display,x->window,(unsigned long)0,&xgc) ;
		whit = XCreateGC(x->display,x->window,(unsigned long)0,&xgc) ;
		XCopyGC(x->display,x->text,(1<<(GCLastBit+1))-1,blac) ;
		XCopyGC(x->display,x->text,(1<<(GCLastBit+1))-1,whit) ;
		xgc.foreground = x->black_pixel ;
		XChangeGC(x->display,blac,GCForeground,&xgc) ;
		xgc.foreground = x->white_pixel ;
		XChangeGC(x->display,whit,GCForeground,&xgc) ;
		}
	ShadowText(x,d,blac,whit,"To start or resume game",-2) ;
	ShadowText(x,d,blac,whit,"Activate this window",1) ;
	ShadowText(x,d,blac,whit,"and press a key",4) ;
	}

XPR(x,"drawrealback)") ;
}
