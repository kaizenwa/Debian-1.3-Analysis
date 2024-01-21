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
#include "icone.h"
#include <stdio.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

void restorecolor(d,n,c1,c2)
Display *d ;
int n ;
Colormap c1,c2 ;
{
XColor *c ;
unsigned int i ;
unsigned long plane_mask ;
unsigned long *pixel ;

c = (XColor *) malloc( n*sizeof(XColor) ) ;
pixel = (unsigned long*) malloc( n*sizeof(unsigned long) ) ;

XAllocColorCells(d,c1,False,&plane_mask,0,pixel,n) ;

for(i=0;i<n;i++)
  if ( pixel[i] != (unsigned long)i )
    {
    fprintf(stderr,"Future Problems... pixel[%d]=%d\n", pixel[i],i);
    break ;
    }

for(i=0;i<n;i++) c[i].pixel = i ;

XQueryColors(d,c2,c,n) ;

for(i=0;i<n;i++) c[i].flags = DoRed|DoGreen|DoBlue ;

XStoreColors(d,c1,c,n) ;

free( (void*) c) ;
free( (void*) pixel) ;
}



void initwin(opt,x)
struct opt *opt ;
struct x *x ;
{
XSetWindowAttributes wa ;
XSizeHints sh ;
XTextProperty tp ;
char buf[LINE_LENGTH] ;

x->icone = XCreateBitmapFromData(x->display,x->root,
           icone_bits,icone_width,icone_height) ;

if ( opt->buffering==1 || opt->buffering==3 || opt->buffering==4 )
        {
        if ( x->map_entries >= (opt->buffering!=3?16:32) &&
             ( x->visual->class==PseudoColor || x->visual->class==GrayScale )
           )
                {
                opt->newmap = 1 ;
                }
           else {
		if (opt->buffering!=3)
		   {
                   fprintf(stderr,"Can't make single buffering because :\n");
                   fprintf(stderr,"   Your display hasn't 16 colors\n");
		   }
		else
		   {
                   fprintf(stderr,"Can't make best buffering because :\n");
                   fprintf(stderr,"   Your display hasn't 32 colors\n");
		   }
	
                fprintf(stderr,"   OR Colors cannot be changed\n");
		exit(1) ;
                }
        }

if ( opt->newmap )
        {
	if ( opt->verbose ) fprintf(stderr,"START newcolormap\n") ;
	x->color1 = XCopyColormapAndFree(x->display,x->colormap) ;
	if ( opt->verbose ) fprintf(stderr,"      Create OK\n") ;
	restorecolor(x->display,(int)x->map_entries,
			x->color1,x->colormap) ;
	if ( opt->verbose ) fprintf(stderr,"      Colors OK\n") ;
	if (opt->buffering==3)
		{
		x->color2 = XCopyColormapAndFree(x->display,x->colormap) ;
		restorecolor(x->display,(int)x->map_entries,
				x->color2,x->colormap) ;
		x->colormap = x->color1 ;
		x->black_pixel = x->map_entries - 4*(opt->use_bw?2:8) ;
		x->white_pixel = x->black_pixel + (opt->use_bw?2:8)-1 ;
		}
	else { x->colormap = x->color1 ;
	       x->black_pixel = x->map_entries - 2*(opt->use_bw?2:8) ;
	       x->white_pixel = x->black_pixel + (opt->use_bw?2:8)-1 ;
	     }
	if ( opt->verbose ) fprintf(stderr,"END   newcolormap\n") ;
        }

if ( opt->verbose )
        {
        fprintf(stderr,"display=%d\n",(int)x->display);
        fprintf(stderr,"screen=%d\n",x->screen);
        fprintf(stderr,"root=%lu\n",x->root);
        fprintf(stderr,"depth=%d\n",x->depth);
        fprintf(stderr,"colormap=%lu\n",x->colormap);
        fprintf(stderr,"DISPLAY open OK\n");
        }

x->posx = 0 ;
x->posy = 0 ;

if ( opt->geometry[0]!='\0' )
    {
    MyXParseGeometry(x->display,x->screen,opt->geometry,&x->posx,&x->posy,
		(unsigned int*)&x->dimx,(unsigned int*)&x->dimy) ;
    }

wa.event_mask = KeyPressMask|KeyReleaseMask|ExposureMask|StructureNotifyMask
		|FocusChangeMask ;
wa.background_pixel = x->black_pixel ;

x->window = XCreateWindow(
                        x->display,
                        x->root,
                        x->posx,x->posy,
			(unsigned int)x->dimx,(unsigned int)x->dimy,
                        0,
                        x->depth,
                        InputOutput,
                        x->visual,
                        CWEventMask|CWBackPixel,
                        &wa);

XSetWindowColormap(x->display,x->window,x->colormap) ;

gethostname(buf,LINE_LENGTH) ;
tp.value = (unsigned char*)buf ;
tp.encoding = XA_STRING ;
tp.format = 8 ;
tp.nitems = strlen((char*)tp.value) ;
XSetWMClientMachine(x->display,x->window,&tp) ;

sh.flags = PResizeInc|PBaseSize ;
if ( opt->geometry[0]!='\0' ) sh.flags |= PPosition ;

sh.width_inc = 2 ;      /* Size must always be ODD in X and Y */
sh.height_inc = 2 ;
sh.base_width = 3 ;
sh.base_height = 3 ;
sh.x = x->posx ;
sh.y = x->posy ;
XSetWMNormalHints( x->display,x->window,&sh ) ;


x->back = 0 ;
x->work = 0 ;
XMapWindow( x->display,x->window) ;
}

/* Create the argument string, to set window properties */
/* This function is call when a change arise */

#define ADDARG(X) { sprintf(pc,"%s",X) ; argv[i++] = pc ; pc += strlen(pc)+1 ; }

void setargs(bl)
struct bl *bl ;
{
char *argv[LINE_LENGTH] ;
char tt[10*LINE_LENGTH] ;   /* Text of the parameters */
char tmp[LINE_LENGTH] ;
char *pc ;
int i ;
Window gar ;
int x,y ;
unsigned int nbc ;
unsigned int dx,dy,gar2,gar3 ;
XTextProperty tp,tp2 ;
XWMHints wh ;
Window w,root,father,*childrens[1] ;

i = 0 ;
pc = tt ;

ADDARG(bl->progname) ;

if ( bl->x.window )
   {
   XQueryTree(bl->x.display,bl->x.window,&root,&father,childrens,&nbc) ;
   XGetGeometry(bl->x.display,bl->x.window,&gar,&x,&y,&dx,&dy,&gar2,&gar3) ;
   XTranslateCoordinates(bl->x.display,father,bl->x.root,0,0,&x,&y,&w) ;
   ADDARG("-geometry") ;
   sprintf(tmp,"%dx%d+%d+%d",dx,dy,x,y) ;
   ADDARG(tmp) ;
   }

if ( bl->menu.window )
   {
   XQueryTree(bl->x.display,bl->menu.window,&root,&father,childrens,&nbc) ;
   XGetGeometry(bl->x.display,bl->menu.window,&gar,&x,&y,&dx,&dy,&gar2,&gar3) ;
   XTranslateCoordinates(bl->x.display,father,bl->x.root,0,0,&x,&y,&w) ;
   ADDARG("-menugeometry") ;
   sprintf(tmp,"%dx%d+%d+%d",dx,dy,x,y) ;
   ADDARG(tmp) ;
   }

if ( bl->x.wscore )
   {
   XQueryTree(bl->x.display,bl->x.wscore,&root,&father,childrens,&nbc) ;
   XGetGeometry(bl->x.display,bl->x.wscore,&gar,&x,&y,&dx,&dy,&gar2,&gar3) ;
   XTranslateCoordinates(bl->x.display,father,bl->x.root,0,0,&x,&y,&w) ;
   ADDARG("-scoregeometry") ;
   sprintf(tmp,"%dx%d+%d+%d",dx,dy,x,y) ;
   ADDARG(tmp) ;
   }

if ( bl->menu.zoo )
   {
   XQueryTree(bl->x.display,bl->menu.zoo,&root,&father,childrens,&nbc) ;
   XGetGeometry(bl->x.display,bl->menu.zoo,&gar,&x,&y,&dx,&dy,&gar2,&gar3) ;
   XTranslateCoordinates(bl->x.display,father,bl->x.root,0,0,&x,&y,&w) ;
   ADDARG("-zoogeometry") ;
   sprintf(tmp,"%dx%d+%d+%d",dx,dy,x,y) ;
   ADDARG(tmp) ;
   }

if ( bl->opt.smooth )		ADDARG("-smooth") ;
if ( bl->menu.showzoo )		ADDARG("-zoo") ;
if ( bl->menu.showscore )	ADDARG("-score") ;
if ( bl->opt.newmap )		ADDARG("-colormap") ;
if ( bl->opt.presskey )		ADDARG("-presskey") ;
if ( bl->opt.use_bw )		ADDARG("-bw") ;
if ( bl->opt.clearline )	ADDARG("-clearline") ;
if ( bl->opt.mode==TRAINING )	ADDARG("-training") ;
if ( bl->bloc.nextpiece )	ADDARG("-shownext") ;

ADDARG("-draw") ;	sprintf(tmp,"%d",bl->opt.drawmode) ;	ADDARG(tmp) ;
ADDARG("-linewidth") ;	sprintf(tmp,"%d",bl->opt.linewidth) ;	ADDARG(tmp) ;
ADDARG("-buffer") ;	sprintf(tmp,"%d",bl->opt.buffering) ;	ADDARG(tmp) ;
ADDARG("-volume") ;	sprintf(tmp,"%d",bl->opt.volume) ;	ADDARG(tmp) ;
ADDARG("-land") ;	sprintf(tmp,"%d",bl->opt.land) ;	ADDARG(tmp) ;
ADDARG("-x") ;		sprintf(tmp,"%d",bl->opt.wx) ;		ADDARG(tmp) ;
ADDARG("-y") ;		sprintf(tmp,"%d",bl->opt.wy) ;		ADDARG(tmp) ;
ADDARG("-z") ;		sprintf(tmp,"%d",bl->opt.wz) ;		ADDARG(tmp) ;
ADDARG("-level") ;	sprintf(tmp,"%d",bl->opt.level) ;	ADDARG(tmp) ;
ADDARG("-buttonheight");sprintf(tmp,"%d",bl->opt.button_height);ADDARG(tmp) ;
ADDARG("-keyboard") ;	sprintf(tmp,"%d",bl->opt.keyboard) ;	ADDARG(tmp) ;
ADDARG("-color") ;	sprintf(tmp,"%d",bl->opt.backcolor) ;	ADDARG(tmp) ;
ADDARG("-bloctype") ;	sprintf(tmp,"%d",bl->bloc.typepiece) ;	ADDARG(tmp) ;
ADDARG("-time_to_demo");sprintf(tmp,"%d",bl->opt.time_to_demo);	ADDARG(tmp) ;
if ( bl->opt.keyboard==0 )
	{
	ADDARG("-keytable") ;
	sprintf(tmp,"%s",bl->opt.userkey) ;
	ADDARG(tmp) ;
	}
ADDARG("-font") ;	ADDARG(bl->opt.thefont) ;
ADDARG("-bigfont") ;	ADDARG(bl->opt.thefont2) ;

sprintf(tmp,"XBlockOut %s",XBLVERSION) ;
tp.value = (unsigned char*)tmp ;
tp.encoding = XA_STRING ;
tp.format = 8 ;
tp.nitems = strlen((char*)tp.value) ;

tp2.value = (unsigned char*)"XBlockOut" ;
tp2.encoding = XA_STRING ;
tp2.format = 8 ;
tp2.nitems = strlen((char*)tp2.value) ;

wh.flags = IconPixmapHint ;
wh.icon_pixmap = bl->x.icone ;

XSetWMProperties(bl->x.display,bl->x.window,&tp,&tp2,
                                       argv,i,
                                       NULL,&wh,NULL ) ;
/*
{ int j ; for(j=0;j<i;j++) printf("%s ",argv[j]) ; printf("\n") ; }
*/
}

/* Compute window position, WHY THIS FUNCTION IS NOT IN XLIB? */
/*			 I have found it (too late) thanks to Brian V. SMITH */

void MyXParseGeometry(d,s,c,x,y,dx,dy)
Display *d ;
int s ;
char *c ;
int *x,*y ;
unsigned int *dx,*dy ;
{
*x = 0 ;
*y = 0 ;
*dy = 0 ;

*dx = atoi(c) ;
while( *c && *c!='x' ) c++ ;
if ( *c==0 ) return ;
c++ ;
*dy = atoi(c) ;
while( *c && *c!='+' && *c!='-' ) c++ ;
if ( *c==0 ) return ;
if ( *c=='+' ) *x = atoi(c+1) ;
	else *x = DisplayWidth(d,s) - atoi(c+1) - *dx ;
c++ ;
if ( *c==0 ) return ;
if ( *c=='-' || *c=='+' ) c++ ;
while( *c && *c!='+' && *c!='-' ) c++ ;
if ( *c==0 ) return ;
if ( *c=='+' ) *y = atoi(c+1) ;
	else *y = DisplayHeight(d,s) - atoi(c+1) - *dy ;
}
