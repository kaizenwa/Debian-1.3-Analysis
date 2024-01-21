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
#include "opt.h"
#include "x.h"
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <stdio.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

/* This function is best than XKeysymToKeycode
   because it take the KeyCode with the minimum modifier.
   If you have :
	keycode :      :      :      :      :
	 ...
	  80       x       y      z
	 ...
	  100	   x       y
   If you ask 'x' it will give you : 100 and not 80
   Idem if the line was :
	  100	   y       x
   or
	  100      x
*/
KeyCode MyXKeysymToKeycode(d,key)
Display *d ;
int key ;
{
int i,j,k,min,max ;
int bestcode,numbermod ;
KeySym *map ;
int nbmod,nb ;

XDisplayKeycodes(d,&min,&max) ;
nb = max-min+1 ;
map = XGetKeyboardMapping(d,min,nb,&nbmod) ;
bestcode = 0 ;
numbermod = nbmod+1 ;
for(i=0;i<nb;i++)
	{
	k = 0 ;
	for(j=0;j<nbmod;j++)
		if ( map[i*nbmod+j]!=0 ) k++ ;
	for(j=0;j<nbmod;j++)
		{
		if ( map[i*nbmod+j]==(KeySym)key && k<=numbermod )
			{
			bestcode = i+min ;
			numbermod = k ;
			break ;
			}
		}
	}
return( bestcode ) ;
}

void initdisp(opt,x)
struct opt *opt ;
struct x *x ;
{
int i ;
char *name ;

if ( opt->displayname[0]!='\0' )
	x->display = XOpenDisplay(opt->displayname) ;
if ( x->display==0 )
	{
	name = getenv("DISPLAY") ;
	if ( name==0 )
		{
		fprintf(stderr,"Xbl is an X11 game\n") ;
		fprintf(stderr,"And your DISPLAY variable doesn't exist\n") ;
		fprintf(stderr,"Goodbye\n") ;
		exit(1) ;
		}
	strcpy(opt->displayname,name) ;
	x->display = XOpenDisplay(opt->displayname) ;
	}
if ( x->display==0 )
	{
        fprintf(stderr,"Cannot open display\n");
	exit(1) ;
	}
x->screen = DefaultScreen( x->display ) ;
i = strlen(opt->displayname) ;
if ( opt->displayname[i-2]=='.' )
	{
	x->screen = opt->displayname[i-1] - '0' ;
	}
if ( x->screen>=ScreenCount(x->display) )
	{
	/* This case is impossible XOpenDisplay change the string */
	fprintf(stderr,"You give an invalide screen number\n") ;
	exit(1) ;
	}
x->root        = RootWindow( x->display,x->screen ) ;
x->depth       = DefaultDepth(x->display,x->screen);
}

void initdisp2(opt,x)
struct opt *opt ;
struct x *x ;
{
XVisualInfo viproto;                /* fill in for getting info */
XVisualInfo *vip;                   /* retured info */	
int nbv ;		
int i ;

x->visual      = DefaultVisual(x->display,x->screen) ;
x->colormap    = DefaultColormap( x->display, x->screen) ;

if ( opt->verbose )
	{
	fprintf(stderr,"DEFAULT Colormap %d\n",x->colormap) ;
	}
/*-----------------------------------------*/
/* Find the PseudoColor with maximum depth */
/* It's obviously not a good method        */
/*-----------------------------------------*/
if ( opt->visual )
{
int new_visual ;

viproto.screen = x->screen ;
viproto.class  = PseudoColor ;
vip = XGetVisualInfo (x->display, VisualScreenMask|VisualClassMask, &viproto, &nbv);


if ( opt->verbose )
		fprintf(stderr,"Number of Visual --------------> %d\n",nbv) ;

new_visual = 0 ;

for(i=0;i<nbv;i++)
  {
	if ( opt->verbose )
			fprintf(stderr,"PSEUDOCOLOR %d entries\n",
			vip[i].visual->map_entries) ;
	if ( x->visual->class != PseudoColor ||
	    vip[i].visual->map_entries > x->visual->map_entries
	    )
	  {
	        if ( opt->verbose )
			{
			fprintf(stderr,"I USE IT VisualID %d!!!!!\n",
				vip[i].visual->visualid) ;
			}
		x->visual = vip[i].visual ;
		new_visual = 1 ;
		if ( x->visual->map_entries>=128 ) break ; /* No more useful */
	  }
  }

if ( new_visual )
   {
   x->colormap = XCreateColormap( x->display, x->root, x->visual, AllocNone) ;
   if ( opt->verbose )
	{
	fprintf(stderr,"Colormap %d created\n",x->colormap) ;
	}
   }

}


/*-----------------------------------------*/
/*-----------------------------------------*/

x->map_entries = x->visual->map_entries ;
if ( x->map_entries&1 ) x->map_entries = (x->map_entries+1)/2 ;

x->white_pixel = XWhitePixel(x->display,x->screen) ;
x->black_pixel = XBlackPixel(x->display,x->screen) ;

if ( opt->buffering==-1)
	{
	if (x->visual->class==PseudoColor || x->visual->class==GrayScale)
		{
		if ( opt->use_bw )
			{
			if ( x->map_entries>=4 )
				if ( x->map_entries>=8 )
					     opt->buffering=3 ;
					else opt->buffering=1 ;
				else opt->buffering=2 ;
			}
		   else {
			if ( x->map_entries>=16 )
				if ( x->map_entries>=32 )
					     opt->buffering=3 ;
					else opt->buffering=1 ;
				else opt->buffering=2 ;
			}
		}
	   else opt->buffering = 2 ;
	if ( opt->verbose )
		fprintf(stderr,"I choose buffering=%d\n",opt->buffering) ;
	}

i = 0 ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_0) ; x->kpstring[i++] = '0' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_1) ; x->kpstring[i++] = '1' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_2) ; x->kpstring[i++] = '2' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_3) ; x->kpstring[i++] = '3' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_4) ; x->kpstring[i++] = '4' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_5) ; x->kpstring[i++] = '5' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_6) ; x->kpstring[i++] = '6' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_7) ; x->kpstring[i++] = '7' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_8) ; x->kpstring[i++] = '8' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_KP_9) ; x->kpstring[i++] = '9' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_Left) ; x->kpstring[i++] = '4' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_Right); x->kpstring[i++] = '6' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_Up)   ; x->kpstring[i++] = '8' ;
x->kpcode[i] = MyXKeysymToKeycode(x->display,XK_Down) ; x->kpstring[i++] = '2' ;
if ( i>NBKEYCODE )
	{
	fprintf(stderr,"You must increase NBKEYCODE.....\n") ;
	exit(1) ;
	}
}
