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
#include <stdio.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif


void initbuffer(opt,x)
struct opt *opt ;
struct x * x ;
{
if ( opt->verbose ) fprintf(stderr,"Start of buffer initialisation\n") ;

if ( x->back )
	{
	XFreePixmap( x->display,x->back ) ;
	x->back = 0 ;
	XFreePixmap( x->display,x->work ) ;
	x->work = 0 ;
	}



switch( opt->buffering )
   {
   case 2 :
   x->back=XCreatePixmap( x->display,x->root,x->dimx,x->dimy,x->depth ) ;
   x->work=XCreatePixmap( x->display,x->root,x->dimx,x->dimy,x->depth ) ;

   if ( x->back==0 || x->work==0 )
	{
	fprintf(stderr,"Not enough memory for double buffering\n") ;
	fprintf(stderr,"Try to use single buffering\n") ;
	fprintf(stderr,"Use option buffering=1 (except for black&white)\n") ;
	exit(1) ;
	}
   break ;
   
   case 1 :
   case 5 :

   if (  opt->buffering==1 && opt->verbose )
   {
    fprintf(stderr,"Create buffer windows for debugging\n") ;
    x->work = XCreateSimpleWindow(x->display,x->root,0,0,x->dimx,x->dimy,0,0,0);
    XMapWindow(x->display,x->work) ;
   }
   else {
        x->work = XCreatePixmap( x->display,x->root,x->dimx,x->dimy,x->depth ) ;
	}
   if ( x->work==0 )
	{
	fprintf(stderr,"Not enough memory for buffering\n") ;
	fprintf(stderr,"Try to play without buffering\n") ;
	fprintf(stderr,"Use option buffering=0\n") ;
	exit(1) ;
	}
   break ;
   }

if ( opt->verbose ) fprintf(stderr,"End of buffer initialisation\n") ;
}
