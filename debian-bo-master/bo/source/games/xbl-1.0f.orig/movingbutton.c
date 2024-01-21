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

#include "menu.h"
#include <stdio.h>
#include <varargs.h>

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif

#include "til0.bm"
#include "til1.bm"
#include "til2.bm"
#include "til3.bm"
#include "til4.bm"
#include "til5.bm"
#include "til6.bm"
#include "til7.bm"
#include "til8.bm"
#include "til9.bm"
#include "til10.bm"
#include "til11.bm"
#include "til12.bm"
#include "til13.bm"
#include "til14.bm"
#include "til15.bm"
#include "til16.bm"

/************************************************************************/
/* Utility to create lines of text for a button				*/
/* xfont must be the same used in gc					*/
/************************************************************************/
struct text_line* create_line(maximum,xfont,gc)
char *maximum ;
XFontStruct *xfont ;
GC gc ;
{
struct text_line *newone ;
newone = (struct text_line *) malloc( sizeof(*newone) ) ;
newone->current_text = (char*)malloc(strlen(maximum)+1+10 /*Not necessary*/) ;
strcpy( newone->current_text,maximum ) ;
newone->maximum_text = maximum ;
newone->xfont	     = xfont ;
newone->gc	     = gc ;
newone->x	     = -1 ;
return(newone) ;
}
/************************************************************************/
/* Utility to compute maximum extent of a button			*/
/************************************************************************/
void button_extent(b)
struct moving_button *b ;
{
struct text_line *l ;
int ascent,descent,dir ;
XCharStruct overall_return ;
int dx,dy,dz ;

if ( b->typet == FLAT_TEXT ) dz = 0 ;
			else dz = b->dz ;
l = b->text ;
dx = 0 ;
dy = b->margin ;
while( l!=0 )
	{
	XTextExtents(l->xfont,l->maximum_text,(int)strlen(l->maximum_text),
			&dir,&ascent,&descent,&overall_return) ;
	dy += ascent+descent+b->margin ;
	if ( overall_return.width > dx ) dx = overall_return.width ;
	l = l->next ;
	}
b->dx = dx + 2*b->margin + 2*(dz+1) ;
b->dy = dy + 2*(dz+1) ;
if ( b->dx & 1 ) b->dx++ ;
if ( b->dy & 1 ) b->dy++ ;
}
/************************************************************************/
/* Utility to create a button                  				*/
/************************************************************************/
struct moving_button* create_button(d,w,h,type,gc,fct,prp,va_alist)
Display *d ;
int h ;
Window w ;
int type ;
struct movinggc *gc ;
void (*fct)() ;
void (*prp)() ;
va_dcl
{
struct text_line *cur,*last ;
struct moving_button *newone ;
va_list lines ;

newone = (struct moving_button *) malloc( sizeof(*newone) ) ;
newone->type	= A_BUTTON ;
newone->display	= d ;
newone->window	= w ;
newone->typet	= type ;
newone->direction = PULLING_TEXT ;
newone->height	= 0 ;		/* Button initial state is PUSH */
newone->x	= 0 ;
newone->y	= 0 ;
newone->dz	= h ;		/* By default height is 6 */
if ( type==FLAT_TEXT ) newone->dz = 0 ;
newone->margin  = 1 ;		/* 1 pixel around text */
newone->linestretching = 0 ;
newone->gc	= gc ;
newone->fct	= fct ;
newone->prp	= prp ;
newone->text	= 0 ;
last = 0 ;
va_start(lines) ;
do
	{
	cur = va_arg(lines,struct text_line*) ;
	if ( last ) 
		{
		last->next = cur ;
		last = cur ;
		}
	else
		{
		newone->text = last = cur ;
		}
	}
while(cur) ;
va_end(lines) ;
button_extent(newone) ;
return(newone) ;
}
/************************************************************************/
/* Display a button                           				*/
/************************************************************************/
void display_button(b,xx,yy,dx,dy)
struct moving_button *b ;
int xx,yy,dx,dy ; /* Position to clip */
{
XPoint pts[5] ;
XPoint ul,ul2,ur,ur2,dl,dl2,dr,dr2 ;
int x,y ;
struct text_line* text ;
XGCValues xgc ;
int ascent,descent,dir ;
XCharStruct overall_return ;
int width ;
int dz ;
static GC lastgc = (GC)-1 ;
static Pixmap laststipple = -1 ;

if ( b->type!=A_BUTTON ) return ;

/* Clipping */
if ( b->x>xx+dx || b->y>yy+dy || b->x+b->dx<xx || b->y+b->dy<yy ) return ;

dz = b->dz ;

if ( b->height==0 )
  {
    /* The button is flat */
    XDrawRectangle(b->display,b->window,b->gc->rectangle,
		   b->x,b->y,b->dx-1,b->dy-1) ;
  }
else
  {
    /* The button is in relief */
    ul.x = b->x                  	; ul.y = b->y ;
    ur.x = ul.x+b->dx-1			; ur.y = ul.y ;
    ul2.x = ul.x+b->height		; ul2.y = ul.y+b->height ;
    ur2.x = ur.x-b->height		; ur2.y = ul2.y ;
    dl.x = ul.x        			; dl.y = ul.y+b->dy-1 ;
    dr.x = ur.x        			; dr.y = dl.y ;
    dl2.x = ul2.x        		; dl2.y = dl.y-b->height ;
    dr2.x = ur2.x        		; dr2.y = dl2.y ;
    
    pts[0]=ul ; pts[1]=ur ; pts[2]=ur2 ; pts[3]=ul2 ; pts[4]=pts[0] ;
    if ( b->height>1 )
      XFillPolygon(b->display,b->window,b->gc->upleft,
		   pts,4,Convex,CoordModeOrigin) ;
    XDrawLines(b->display,b->window,b->gc->rectangle,pts,5,CoordModeOrigin);
    pts[0]=ur ; pts[1]=dr ; pts[2]=dr2 ; pts[3]=ur2 ; pts[4]=pts[0] ;
    if ( b->height>1 )
      XFillPolygon(b->display,b->window,b->gc->downright,
		   pts,4,Convex,CoordModeOrigin) ;
    XDrawLines(b->display,b->window,b->gc->rectangle,pts,5,CoordModeOrigin);
    pts[0]=ul ; pts[1]=ul2; pts[2]=dl2 ; pts[3]=dl  ; pts[4]=pts[0] ;
    if ( b->height>1 )
      XFillPolygon(b->display,b->window,b->gc->upleft,
		   pts,4,Convex,CoordModeOrigin) ;
    XDrawLines(b->display,b->window,b->gc->rectangle,pts,5,CoordModeOrigin);
    pts[0]=dl2; pts[1]=dr2; pts[2]=dr  ; pts[3]=dl  ; pts[4]=pts[0] ;
    if ( b->height>1 )
      XFillPolygon(b->display,b->window,b->gc->downright,
		   pts,4,Convex,CoordModeOrigin) ;
    XDrawLines(b->display,b->window,b->gc->rectangle,pts,5,CoordModeOrigin);
  }


width = b->dx - 2*(dz+1) ;
/* Erase center */
/* or a rectangle of pixel if pushing button or nothing if pulling button */
if ( b->height==0 )
	{
	XFillRectangle(b->display,b->window,b->gc->back,
		b->x+b->height+1,b->y+b->height+1,
		b->dx - 2*(b->height+1),b->dy-2*(b->height+1)) ;
	}
else	{
	if ( b->direction==PUSHING_TEXT )
		{
		XDrawRectangle(b->display,b->window,b->gc->back,
			b->x+b->height+1,b->y+b->height+1,
			b->dx - 2*(b->height+1)-1,b->dy-2*(b->height+1)-1) ;
		}
	}	


x = b->x+dz+1+b->margin ; /* first pixel free up left */
y = b->y+dz+1+b->margin+b->linestretching ;

text = b->text ;

if ( b->height!=0 || b->typet==FLAT_TEXT )
   while( text )
	{
	if ( b->typet==FLAT_TEXT ) xgc.stipple = b->gc->tile[16] ;
	else if ( b->height==1 ) xgc.stipple = b->gc->tile[0] ;
		          else xgc.stipple = b->gc->tile[(b->height*16)/dz] ;
	if ( !(text->gc==lastgc && xgc.stipple==laststipple) )
		{
		xgc.fill_style  = FillOpaqueStippled ;
		if ( b->height==dz ) xgc.fill_style  = FillSolid ;
		XChangeGC(b->display,text->gc,GCStipple|GCFillStyle,&xgc) ;
		lastgc = text->gc ;
		laststipple = xgc.stipple ;
		}
	if ( b->text==text && b->prp )
		(*(b->prp))(b) ; /* Prepare the text to draw */
	if ( text->x==-1 )
		{
		XTextExtents(text->xfont,
		     text->current_text,(int)strlen(text->current_text),
		     &dir,&ascent,&descent,&overall_return) ;
		text->x = x+ (width-overall_return.width)/2
		           - overall_return.lbearing ;
		text->y = y + ascent ;
		text->height = ascent+descent+b->margin+b->linestretching ;
		}
        XDrawString(b->display,b->window,text->gc,text->x,text->y,
                   text->current_text,(int)strlen(text->current_text) ) ;
	y += text->height ;
	text = text->next ;
	}
  else
	if ( b->prp ) (*(b->prp))(b) ; /* Prepare the text to draw */
}
/************************************************************************/
/* Next frame                                 				*/
/************************************************************************/
void next_button(b)
struct moving_button *b ;
{
int yetdisplay ;

if ( b->type!=A_BUTTON ) return ;
if ( b->typet==FLAT_TEXT )
	if ( b->height==0 ) return ;
		else b->direction = PUSHING_TEXT ;
if ( b->typet==RELIEF_TEXT && b->height!=b->dz && b->direction==STOP_TEXT )
	b->direction = PULLING_TEXT ;

yetdisplay = 0 ;
switch( b->direction )
	{
	case STOP_TEXT :  return ;
	case PUSHING_TEXT : if ( b->height!=0 ) b->height-- ;
			    if ( b->height==0 )
			      if ( b->typet==FLAT_TEXT )
				{
				b->direction = STOP_TEXT ;
				}
			      else
				{
				if ( b->fct )
					{
					(*b->fct)(b,b->but) ;
					display_button(b,b->x,b->y,0,0) ;
					yetdisplay = 1 ;
					}
				b->direction = PULLING_TEXT ;
				}
			    break ;
	case PULLING_TEXT : if ( b->height!=b->dz ) b->height++ ;
			    if ( b->height==b->dz )
					b->direction = STOP_TEXT ;
			    break ;
	}
if ( !yetdisplay ) display_button(b,b->x,b->y,0,0) ;
}
/************************************************************************/
/* Initialisation of initgc                                             */
/************************************************************************/
void init_movinggc(d,w,mgc,background,foreground,shadow)
Display *d ;
Window w ;
struct movinggc *mgc ;
unsigned long background,foreground,shadow ;
{
XGCValues xgc ;

mgc->tile[0]=XCreateBitmapFromData(d,w,til0_bits,til0_width,til0_height ) ;
mgc->tile[1]=XCreateBitmapFromData(d,w,til1_bits,til1_width,til1_height ) ;
mgc->tile[2]=XCreateBitmapFromData(d,w,til2_bits,til2_width,til2_height ) ;
mgc->tile[3]=XCreateBitmapFromData(d,w,til3_bits,til3_width,til3_height ) ;
mgc->tile[4]=XCreateBitmapFromData(d,w,til4_bits,til4_width,til4_height ) ;
mgc->tile[5]=XCreateBitmapFromData(d,w,til5_bits,til5_width,til5_height ) ;
mgc->tile[6]=XCreateBitmapFromData(d,w,til6_bits,til6_width,til6_height ) ;
mgc->tile[7]=XCreateBitmapFromData(d,w,til7_bits,til7_width,til7_height ) ;
mgc->tile[8]=XCreateBitmapFromData(d,w,til8_bits,til8_width,til8_height ) ;
mgc->tile[9]=XCreateBitmapFromData(d,w,til9_bits,til9_width,til9_height ) ;
mgc->tile[10]=XCreateBitmapFromData(d,w,til10_bits,til10_width,til10_height ) ;
mgc->tile[11]=XCreateBitmapFromData(d,w,til11_bits,til11_width,til11_height ) ;
mgc->tile[12]=XCreateBitmapFromData(d,w,til12_bits,til12_width,til12_height ) ;
mgc->tile[13]=XCreateBitmapFromData(d,w,til13_bits,til13_width,til13_height ) ;
mgc->tile[14]=XCreateBitmapFromData(d,w,til14_bits,til14_width,til14_height ) ;
mgc->tile[15]=XCreateBitmapFromData(d,w,til15_bits,til15_width,til15_height ) ;
mgc->tile[16]=XCreateBitmapFromData(d,w,til16_bits,til16_width,til16_height ) ;

xgc.foreground  = background ;
mgc->back = XCreateGC(d,w,(unsigned long)GCForeground,&xgc) ;

xgc.foreground  = foreground ;
xgc.background  = background ;
xgc.fill_style  = FillOpaqueStippled ;
xgc.stipple     = mgc->tile[8] ;
mgc->upleft = XCreateGC(d,w,(unsigned long)(GCForeground|GCBackground|
					GCFillStyle|GCStipple),
			&xgc) ;

xgc.foreground  = background ;
xgc.background  = shadow ;
if ( background==shadow ) xgc.fill_style  = FillSolid ;
		     else xgc.fill_style  = FillOpaqueStippled ;
xgc.stipple     = mgc->tile[8] ;
mgc->downright = XCreateGC(d,w,(unsigned long)(GCForeground|GCBackground|
				GCFillStyle|GCStipple),
			&xgc) ;

xgc.foreground  = foreground ;
mgc->rectangle = XCreateGC(d,w,(unsigned long)GCForeground,&xgc) ;
}
/************************************************************************/
/* Push button                                                          */
/************************************************************************/
void push_button(b,x,y,but)
struct moving_button *b ;
int x,y,but ;
{
if ( b->type!=A_BUTTON ) return ;
if ( b->typet==RELIEF_TEXT &&
     x>b->x+b->height && y>b->y+b->height &&
     x<b->x+b->dx-b->height && y<b->y+b->dy-b->height )
	{
	/* If I push a button who is being pushed, then
	   I don't wait to call the function */
	if ( b->direction == PUSHING_TEXT )
		{
		if ( b->fct )
			{
			(*b->fct)(b,b->but) ;
			}
		}
	else
		{
		b->direction = PUSHING_TEXT ;
		}
	b->but = but ;
	}
}
/************************************************************************/
/* Scotch a set of button or lines or columns                           */
/* By default a button is consider as a LINE				*/
/************************************************************************/
struct row_column* scotch(va_alist)
va_dcl
{
va_list b ;
struct row_column *newone,*last,*cur,*first ;
int type,newtype ;

va_start(b) ;

cur = va_arg(b,struct row_column*) ;
if ( cur==0 )
	{
	fprintf(stderr,"You must have something to scotch %s:%d\n",
			__FILE__,__LINE__) ;
	exit(1) ;
	}
type = cur->type ;
if ( type==A_COLUMN ) newtype = A_LINE ;
		else  newtype = A_COLUMN ;
last = 0 ;
first = 0 ; /* Only to remove a GCC warning */
do
	{
	newone = (struct row_column *) malloc( sizeof(*newone) ) ;
	newone->type = newtype ;
	newone->in = cur ;
	newone->next = 0 ;
	if ( last ) last->next = newone ;
	       else first = newone ;
	last = newone ;
	cur = va_arg(b,struct row_column*) ;
	}
while(cur) ;
va_end(b) ;
return(first) ;
}
/************************************************************************/
/* Recursive call of a fonction                                         */
/************************************************************************/
void walkrowcol(r,fct,x,y,b,b2)
struct row_column *r ;
void (*fct)() ;
int x,y,b,b2 ;
{
if ( r==0 ) return ;
if ( r->type==A_BUTTON )
	{
	(*fct)(r,x,y,b,b2) ;
	}
	else
		{
		walkrowcol(r->in,fct,x,y,b,b2) ;
		walkrowcol(r->next,fct,x,y,b,b2) ;
		(*fct)(r,x,y,b,b2) ;
		}
}
/************************************************************************/
/* Calculate the size of a structure		                        */
/************************************************************************/
void sizerowcol(r)
struct row_column *r ;
{
if ( r==0 ) return ;
switch( r->type )
	{
	case A_BUTTON : return ;
	case A_LINE   :
		r->dx = r->in->dx ;
		r->dy = r->in->dy ;
		if ( r->next )
			{
			if ( r->next->dy>r->in->dy ) r->dy=r->next->dy;
			r->dx += r->next->dx ;
			}
		break ;
	case A_COLUMN   :
		r->dx = r->in->dx ;
		r->dy = r->in->dy ;
		if ( r->next )
			{
			if ( r->next->dx>r->in->dx ) r->dx=r->next->dx;
			r->dy += r->next->dy ;
			}
		break ;
	}
}
/************************************************************************/
/* Calculate the size of a structure		                        */
/************************************************************************/
void resizex(b,x)
struct row_column *b ;
int x ;
{
int dx ;
struct row_column *c ;

if ( b==0 ) return ;
b->dx = x ;
switch( b->type )
	{
	case A_BUTTON : b->dx = x ;
		break ;
	case A_LINE   :
		dx = x ;
		c = b ;
		while( c )
			{
			resizex( c->in , c->in->dx ) ;
			dx -= c->in->dx ;
			c = c->next ;
			}
		if ( dx>0 ) /* Stretch left button of line */
			{
			resizex( b->in , b->in->dx+dx ) ;
			}
		break ;
	case A_COLUMN   :
		resizex( b->next,x ) ;
		resizex( b->in , x ) ;
		break ;
	}
}
/************************************************************************/
/* Find minimun height of a button  		                        */
/************************************************************************/
int minimumheight(b,x)
struct moving_button *b ;
int x ;
{
static int mini ;

switch(x)
	{
	case INIT_MINIMUM : mini = 100000 ;
			    break ;
	case GIVE_MINIMUM : break ;
	case COMPUTE_MINIMUM :
		if ( b->type == A_BUTTON )
			if ( b->dy<mini ) mini = b->dy ;
	}
return(mini) ;
}
/************************************************************************/
/* Stretch button to integer size * minimum size               		*/
/************************************************************************/
void stretch(b,x)
struct moving_button *b ;
int x ;
{
int dy ;
int nbline ;
struct text_line *t ;

if ( b->type!=A_BUTTON ) return ;
b->dx = (((b->dx-1)/x)+1)*x ;
dy = (((b->dy-1)/x)+1)*x ;

/* Increase line spacing */
t = b->text ;
nbline = 0 ;
while( t )
	{
	nbline++ ;
	t = t->next ;
	}
b->linestretching += (dy-b->dy)/(nbline+1) ;
b->dy = dy ;
}
/************************************************************************/
/* Compute position of all buttons                                      */
/* x,y is the current position						*/
/************************************************************************/
void posit_button(r,x,y)
struct row_column *r ;
int x,y ;
{
if ( r==0 ) return ;
switch( r->type )
	{
	case A_BUTTON :
		((struct moving_button*)r)->x = x ;
		((struct moving_button*)r)->y = y ;
		break ;
	case A_LINE :
		posit_button(r->in,x,y) ;
		posit_button(r->next,x+r->in->dx,y) ;
		break ;
	case A_COLUMN :
		posit_button(r->in,x,y) ;
		posit_button(r->next,x,y+r->in->dy) ;
		break ;
	}
}
/************************************************************************/
/* Some function calling walkrowcol	                                */
/************************************************************************/
/* Compute the minimum height */
int compute_height(r)
struct row_column *r ;
{
(void)minimumheight((struct moving_button*)0,INIT_MINIMUM) ;
walkrowcol(r, (void (*)())minimumheight,COMPUTE_MINIMUM,0,0,0) ;
return( minimumheight((struct moving_button*)0,GIVE_MINIMUM) ) ;
}
/* Stretch to minimum height or 2* 3* ... minimum height */
void compute_stretch(r,minimum)
struct row_column *r ;
int minimum ;
{
walkrowcol(r,stretch,minimum,0,0,0) ;
}
/* Compute the size of all columns and rows */
void compute_rowcol(r)
struct row_column *r ;
{
walkrowcol(r,sizerowcol,0,0,0,0) ;
}
/* Compute the position of all columns and rows */
void compute_posit(r,xdecal)
struct row_column *r ;
int xdecal ;
{
walkrowcol(r,posit_button,xdecal,0,0,0) ;
}
/* Display all the buttons */
void display_rowcol(r,x,y,dx,dy)
struct row_column *r ;
int x,y,dx,dy ;
{
walkrowcol(r,display_button,x,y,dx,dy) ;
}
/* Push buttons... */
void push_rowcol(r,x,y,b)
struct row_column *r ;
int x,y,b ;
{
walkrowcol(r,push_button,x,y,b,0) ;
}
/* Set window value */
static void setwindowvalue(b,x)
struct moving_button *b ;
int x ;
{
if ( b->type==A_BUTTON ) b->window = x ;
}
void windowset(b,x)
struct row_column *b ;
Window x ;
{
/* Sorry for the warning    v         */
/* May be this thing doesn't work with all compiler */
walkrowcol(b,setwindowvalue,x,0,0,0) ;
}

