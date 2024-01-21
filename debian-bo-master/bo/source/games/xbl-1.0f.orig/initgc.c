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
#include "x.h"
#include "opt.h"
#include "define.h"
#include <stdio.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif


#include "p0.h"
#include "p1.h"
#include "p2.h"
#include "p3.h"
#include "p4.h"
#include "p5.h"
#include "transp.h"

#define MAX_COLOR_MAP 1024

static unsigned int gray[] = { 0,7,4,6,3,5,2,8 } ;

void dumpgc(d,gc,u)
Display *d ;
GC gc ;
char *u ;
{
XGCValues xgc ;
unsigned long value_mask ;
char *t ;

#ifdef HAVE_XGETGCVALUES

value_mask = GCTile|GCBackground|GCForeground|GCFillStyle|GCPlaneMask|
		GCStipple|GCLineStyle ;
XGetGCValues(d,gc,value_mask,&xgc) ;

fprintf(stderr,"---- Dump GC -------------- %s \n",u) ;
fprintf(stderr,"Background %d\n",xgc.background) ;
fprintf(stderr,"Foreground %d\n",xgc.foreground) ;
switch(xgc.fill_style)
	{
	case FillStippled : t = "FillStippled" ; break ;
	case FillSolid    : t = "FillSolid   " ; break ;
	case FillTiled    : t = "FillTiled   " ; break ;
	default		  : t = "unknown" ;
	}
fprintf(stderr,"FillStyle  %s\n",t) ;
fprintf(stderr,"Tile       %d\n",xgc.tile) ;
fprintf(stderr,"Stipple    %d\n",xgc.stipple) ;
fprintf(stderr,"Planemask  %d\n",xgc.plane_mask) ;
fprintf(stderr,"Linestyle  %d\n",xgc.line_style) ;

#else

printf("Your system haven't XGetGCValues function : sorry\n") ;

#endif
}

   
   

void makexcolor(opt,x,xcolor,pixel_return)
struct x *x ;
struct opt *opt ;
XColor *xcolor ;
unsigned long *pixel_return ;
{
int i ;

if ( x->visual->class==GrayScale )
        {
        for(i=0;i<8;i++)
                {
                xcolor[i].pixel = pixel_return[i] ;
                xcolor[i].flags = DoRed|DoGreen|DoBlue ;
                xcolor[i].red = xcolor[i].green = xcolor[i].blue =
                 (gray[i]<<13) - (i==0?0:1) ;
                }
        }
   else {
        for(i=0;i<8;i++)
                {
                xcolor[i].pixel = pixel_return[i] ;
                xcolor[i].red   = (i&1) ? (1<<16)-1 : 0 ;
                xcolor[i].green = (i&2) ? (1<<16)-1 : 0 ;
                xcolor[i].blue  = (i&4) ? (1<<16)-1 : 0 ;
                xcolor[i].flags = DoRed|DoGreen|DoBlue ;
                if ( opt->verbose )
                        fprintf(stderr,"%lu %d %d %d\n",
                                (unsigned long)xcolor[i].pixel,
				xcolor[i].red,
                                xcolor[i].green,xcolor[i].blue);
                }
        }
if ( opt->use_bw )
		{
		xcolor[1] = xcolor[7] ;
		xcolor[1].pixel = pixel_return[1] ;
		}
}




void initgc(opt,x)
struct opt *opt ;
struct x *x ;
{
unsigned long plane_mask[6] ;
unsigned long pixel_return[32] ;
unsigned long value_mask ;
Pixmap p[FACECOLOR] ;
XColor xcolor[MAX_COLOR_MAP] ; 
unsigned long close_pixel[8] ;
XGCValues xgc ;
int ret ;
int i,j ;
int nbc,nbc2 ;


if ( opt->use_bw ) { nbc=2 ; nbc2 = 1 ; }
	else { nbc=8 ; nbc2 = 3 ; }

switch( x->visual->class )
   {
   case StaticGray :
	if ( opt->verbose ) fprintf(stderr,"static gray\n") ;
	if ( opt->buffering!=2 && opt->buffering!=0 && opt->buffering!=5 )
		{
		opt->buffering = 2 ;
		fprintf(stderr,"With StaticGray use buffer=2\n") ;
		}
	if ( x->map_entries<8 )
		{
		opt->use_bw = 1 ;
		break ;
		}
	j = x->map_entries/8 ;
	for(i=0;i<8;i++) xcolor[i].pixel = gray[i]*j-(i==0?0:1) ;
	break ;
   
   case StaticColor :
	if ( opt->verbose ) fprintf(stderr,"static Color\n") ;
	if ( opt->buffering!=2 && opt->buffering!=0 && opt->buffering!=5 )
		{
		opt->buffering = 2 ;
		fprintf(stderr,"With StaticColor use buffer=2\n") ;
		}
	if ( x->map_entries<8 )
		{
		opt->use_bw = 1 ;
		break ;
		}
	if ( x->map_entries>MAX_COLOR_MAP )
		{
		fprintf(stderr,"A static color map greater than %d (%d)!\n",
			MAX_COLOR_MAP,x->map_entries) ;
		fprintf(stderr,"I will test only the first one...\n") ;
		fprintf(stderr,"If you are not agree : modify initgc.c''\n") ;
		x->map_entries = MAX_COLOR_MAP ;
		}

	/* Search closest colors */
	
	for(i=0;i<x->map_entries;i++) xcolor[i].pixel = i ;
	XQueryColors(x->display,x->colormap,xcolor,(int)x->map_entries);

/* The =1994 remove a compiler warning */
#define VMAX(F) j = -100000 ; ret=1994 ; \
		    for(i=0;i<x->map_entries;i++) \
			if ( F>j ) { j = F ; ret = i ; }

	VMAX(-(int)xcolor[i].red-(int)xcolor[i].green-(int)xcolor[i].blue) ;
	close_pixel[0] = ret ;
	VMAX((int)xcolor[i].red-(int)xcolor[i].green-(int)xcolor[i].blue) ;
	close_pixel[1] = ret ;
	VMAX(-(int)xcolor[i].red+(int)xcolor[i].green-(int)xcolor[i].blue) ;
	close_pixel[2] = ret ;
	VMAX((int)xcolor[i].red+(int)xcolor[i].green-(int)xcolor[i].blue) ;
	close_pixel[3] = ret ;
	VMAX(-(int)xcolor[i].red-(int)xcolor[i].green+(int)xcolor[i].blue) ;
	close_pixel[4] = ret ;
	VMAX((int)xcolor[i].red-(int)xcolor[i].green+(int)xcolor[i].blue) ;
	close_pixel[5] = ret ;
	VMAX(-(int)xcolor[i].red+(int)xcolor[i].green+(int)xcolor[i].blue) ;
	close_pixel[6] = ret ;
	VMAX((int)xcolor[i].red+(int)xcolor[i].green+(int)xcolor[i].blue) ;
	close_pixel[7] = ret ;
	for(i=0;i<8;i++) xcolor[i].pixel = close_pixel[i] ;
	break ;
	
   case DirectColor   :
   case TrueColor   :
	if ( opt->buffering!=2 )
		{
		opt->buffering = 2 ;
		fprintf(stderr,"With DirectColor/TrueColor use buffer=2\n") ;
		}
	xcolor[0].pixel = 0 ;
	xcolor[1].pixel = x->visual->red_mask ;
	xcolor[2].pixel = x->visual->green_mask ;
	xcolor[3].pixel = x->visual->red_mask+x->visual->green_mask ;
	xcolor[4].pixel = x->visual->blue_mask ;
	xcolor[5].pixel = x->visual->red_mask+x->visual->blue_mask ;
	xcolor[6].pixel = x->visual->green_mask+x->visual->blue_mask ;
	xcolor[7].pixel = x->visual->red_mask+x->visual->green_mask+
			  x->visual->blue_mask ;
	if ( x->visual->class==DirectColor )
		{
		if ( opt->use_bw==0 )
			{
			opt->use_bw = 1 ;
			fprintf(stderr,"With DirectColor\n") ;
			fprintf(stderr,"Only B&W display\n") ;
			}
		}
	break ;

   case GrayScale   :
   case PseudoColor   :
	if ( opt->verbose ) fprintf(stderr,"Modifiable colormap\n") ;

	if ( opt->buffering==1 || opt->buffering==3 || opt->buffering==4 )
		{
		/* Take last color possible may be no use (for flicking)*/

		for(i=0;i<32;i++)
		   pixel_return[i] = i+x->map_entries-(opt->buffering!=3?2*nbc:4*nbc) ;
		makexcolor( opt,x , xcolor , pixel_return ) ;
		for(j=0;j<nbc;j++)
			{
			xcolor[nbc+j]       = xcolor[nbc-1] ;
			xcolor[nbc+j].pixel = nbc+xcolor[j].pixel ;
			xcolor[2*nbc+j]      = xcolor[j] ;
			xcolor[2*nbc+j].pixel = 2*nbc+xcolor[j].pixel ;
			/* Use next if buffering=3 */
			xcolor[3*nbc+j]      = xcolor[nbc-1] ;
			xcolor[3*nbc+j].pixel = 3*nbc+xcolor[j].pixel ;
			}
		if ( opt->verbose )
			fprintf(stderr,"START CREATE COLORMAP 1\n") ;
		XStoreColors(x->display,x->color1,xcolor,
					opt->buffering!=3?2*nbc:4*nbc) ;
		if ( opt->verbose )
			{
			fprintf(stderr,"COLORMAP 1\n") ;
			for(j=0;j<nbc*4;j++)
				{
				fprintf(stderr,"[%2d] pixel %d ",j,xcolor[j].pixel) ;
				fprintf(stderr,"(%d,%d,%d)\n",xcolor[j].red,
						xcolor[j].green,
						xcolor[j].blue) ;
				}
			}

		if ( opt->buffering==3 )
			{
			for(j=0;j<nbc;j++)
				{
				xcolor[nbc+j]       = xcolor[j] ;
				xcolor[nbc+j].pixel = nbc+xcolor[j].pixel ;
				xcolor[2*nbc+j]      = xcolor[nbc-1] ;
				xcolor[2*nbc+j].pixel = 2*nbc+xcolor[j].pixel ;
				xcolor[3*nbc+j]      = xcolor[nbc-1] ;
				xcolor[3*nbc+j].pixel = 3*nbc+xcolor[j].pixel ;
				}
			XStoreColors(x->display,x->color2,xcolor,4*nbc) ;
			if ( opt->verbose )
				{
				fprintf(stderr,"COLORMAP 2\n") ;
				for(j=0;j<nbc*4;j++)
					{
					fprintf(stderr,"[%2d] pixel %d ",j,xcolor[j].pixel) ;
					fprintf(stderr,"(%d,%d,%d)\n",xcolor[j].red,
							xcolor[j].green,
							xcolor[j].blue) ;
					}
				}
			}

		x->black_pixel = xcolor[0].pixel ;
		x->white_pixel = xcolor[nbc-1].pixel ;
		if ( opt->verbose )
			{
			fprintf(stderr,"black_pixel %d\n",x->black_pixel) ;
			fprintf(stderr,"nbc = %d nbc2 = %d\n",nbc,nbc2) ;
			fprintf(stderr,"\n") ;
			fprintf(stderr,"XStoreColor success\n") ;
			}
		break ;
		}
        if ( opt->newmap )
		{
		for(i=0;i<nbc;i++)
			pixel_return[i] = x->map_entries - nbc  + i ;
		}
	else
	   {
	   ret = XAllocColorCells(x->display,x->colormap,
			       False, /* Not contig */
			       plane_mask,0,
			       pixel_return,nbc) ;
	   if ( opt->verbose )
		{
		fprintf(stderr,"pixels return (ret=%d) :",ret) ;
		for(j=0;j<nbc;j++) fprintf(stderr," %d",pixel_return[j]) ;
		fprintf(stderr,"\n") ;
		}
	   if ( ret==BadColor || ret==BadAlloc || ret==BadValue || ret==0 )
		{
		fprintf(stderr,"XAllocColorCells=%d\n",ret) ;
		fprintf(stderr,"Use your own color map with: -colormap\n") ;
		fprintf(stderr,"Find another visual with: -visual\n") ;
		fprintf(stderr,"Use another display\n") ;
		opt->use_bw = 1 ;
		break ;
		}
	   }
	makexcolor( opt,x , xcolor , pixel_return ) ;
	if ( opt->verbose )
		{
		fprintf(stderr,"START CREATE COLORMAP\n") ;
		fprintf(stderr,"colormap %d nbc %d\n",x->colormap,nbc) ;
		}
	XStoreColors(x->display,x->colormap,xcolor,nbc) ;
	x->black_pixel = xcolor[0].pixel ;
	x->white_pixel = xcolor[nbc-1].pixel ;
	if ( opt->verbose )
		{
		fprintf(stderr,"black_pixel %d\n",x->black_pixel) ;
		fprintf(stderr,"nbc = %d nbc2 = %d\n",nbc,nbc2) ;
		fprintf(stderr,"\n") ;
		fprintf(stderr,"XStoreColor success\n") ;
		}
	break ;
   }

x->back_pixel = xcolor[opt->backcolor].pixel ;
xgc.background = x->black_pixel ;
xgc.foreground = x->white_pixel ;
xgc.stipple    = XCreateBitmapFromData(x->display,x->root,transp_bits,
				       transp_width,transp_height) ;

x->tile = XCreatePixmapFromBitmapData(x->display,x->root,
		p0_bits,p0_width,p0_height,
		x->white_pixel,x->black_pixel,x->depth);

xgc.line_width  = opt->linewidth ;
xgc.cap_style  = CapRound ;

if ( opt->use_bw )
	{
	if ( opt->verbose ) fprintf(stderr,"Use Black & White\n") ;

	x->back_pixel = x->black_pixel ;

	p[0] = XCreatePixmapFromBitmapData(x->display,x->root,
		p0_bits,p0_width,p0_height,
		x->white_pixel,x->black_pixel,x->depth);
	p[1] = XCreatePixmapFromBitmapData(x->display,x->root,
		p1_bits,p1_width,p1_height,
		x->white_pixel,x->black_pixel,x->depth);
	p[2] = XCreatePixmapFromBitmapData(x->display,x->root,
		p2_bits,p2_width,p2_height,
		x->white_pixel,x->black_pixel,x->depth);
	p[3] = XCreatePixmapFromBitmapData(x->display,x->root,
		p3_bits,p3_width,p3_height,
		x->white_pixel,x->black_pixel,x->depth);
	p[4] = XCreatePixmapFromBitmapData(x->display,x->root,
		p4_bits,p4_width,p4_height,
		x->black_pixel,x->white_pixel,x->depth);
	p[5] = XCreatePixmapFromBitmapData(x->display,x->root,
		p5_bits,p5_width,p5_height,
		x->white_pixel,x->black_pixel,x->depth);

	value_mask = GCTile|GCBackground|GCForeground|GCFillStyle ;
	xgc.fill_style = FillTiled ;
	for(i=0;i<FACECOLOR;i++)
		{
		xgc.tile = p[i] ;
		if ( opt->verbose ) fprintf(stderr,"bitmap%d=%lu\n",i,p[i]) ;
		x->face[i] = XCreateGC(x->display,x->root,value_mask,&xgc) ;
		}

	value_mask = GCBackground|GCForeground|GCLineStyle ;
	xgc.line_style = LineOnOffDash ;
	x->grid = XCreateGC(x->display,x->root,value_mask,&xgc) ;

	if ( opt->verbose ) fprintf(stderr,"End of bitmap creation\n") ;
	}
   else {
	if ( opt->verbose ) fprintf(stderr,"Use Color map\n") ;
	value_mask = GCForeground|GCBackground|GCFillStyle ;
	xgc.fill_style = FillSolid ;
	for(i=0;i<FACECOLOR;i++)
	  {
	    if ( opt->verbose )
	      fprintf(stderr,"Color %d = %d\n",i, xcolor[i+1].pixel ) ;
	    xgc.foreground = xcolor[i+1].pixel ;
	    x->face[i] = XCreateGC(x->display,x->root,value_mask,&xgc) ;
	  }
	x->grid = x->face[1] ;
	}

value_mask      = GCForeground|GCFillStyle ;
xgc.fill_style  = FillSolid ;
xgc.foreground  = x->black_pixel ;
x->black        = XCreateGC(x->display,x->root,value_mask,&xgc) ;
xgc.foreground  = x->white_pixel ;
xgc.background  = x->back_pixel ;
x->white        = XCreateGC(x->display,x->root,value_mask|GCBackground,&xgc) ;
xgc.background  = x->black_pixel ;

xgc.fill_style  = FillStippled ;
xgc.foreground  = x->white_pixel ;
x->transp       = XCreateGC(x->display,x->root,value_mask|GCStipple,&xgc) ;


if ( opt->buffering==2 || opt->buffering==5 )
	{
        value_mask      = GCFillStyle|GCLineWidth|GCCapStyle ;
        xgc.fill_style  = FillTiled ;
        }
else    {
        value_mask      = GCFillStyle|GCLineWidth|GCForeground|GCCapStyle ;
	xgc.fill_style  = FillSolid ;
	xgc.foreground  = x->black_pixel ;
	}
x->clearline    = XCreateGC(x->display,x->root,value_mask,&xgc) ;


if ( opt->buffering==1 || opt->buffering==3 || opt->buffering==4 )
   {
   value_mask      = GCForeground|GCFillStyle|GCStipple
			|GCBackground|GCPlaneMask ;
   xgc.fill_style  = FillStippled ;
   xgc.plane_mask  = 1<<nbc2 ;
   xgc.foreground  = 1<<nbc2 ;
   x->movingtransp = XCreateGC(x->display,x->root,value_mask,&xgc) ;
   if ( opt->buffering==3 )
   	{
   	xgc.plane_mask  = 2<<nbc2 ; /* It's a power of 2 */
   	xgc.foreground  = 2<<nbc2 ;
   	x->movingtransp2= XCreateGC(x->display,x->root,value_mask,&xgc) ;
   	}

   value_mask      = GCForeground|GCFillStyle|GCPlaneMask|GCLineWidth|GCCapStyle ;
   xgc.foreground  = 1<<nbc2 ;
   xgc.plane_mask  = 1<<nbc2 ;
   xgc.fill_style  = FillSolid ;
   x->movingwhite = XCreateGC(x->display,x->root,value_mask,&xgc) ;
   if ( opt->buffering==3 || opt->buffering==4)
   	{
   	xgc.foreground  = 2<<nbc2 ;
   	xgc.plane_mask  = 2<<nbc2 ;
   	x->movingwhite2 = XCreateGC(x->display,x->root,value_mask,&xgc) ;
   	xgc.foreground  = 0 ;
   	xgc.plane_mask  = 1<<nbc2 ;
   	x->movingblack  = XCreateGC(x->display,x->root,value_mask,&xgc) ;
   	xgc.plane_mask  = 2<<nbc2 ;
   	x->movingblack2 = XCreateGC(x->display,x->root,value_mask,&xgc) ;
   	}
   
   value_mask      = GCPlaneMask ;
   xgc.plane_mask  = 1<<nbc2 ;
   x->copybuffer   = XCreateGC(x->display,x->root,value_mask,&xgc) ; 
   }
else
   {
   value_mask      = GCForeground|GCFillStyle|GCStipple|GCBackground ;
   xgc.fill_style  = FillStippled ;
   xgc.foreground  = x->white_pixel ;
   x->movingtransp = XCreateGC(x->display,x->root,value_mask,&xgc) ;

   value_mask      = GCForeground|GCFillStyle|GCLineWidth|GCCapStyle ;
   xgc.fill_style  = FillSolid ;
   xgc.foreground  = x->white_pixel ;
   x->movingwhite  = XCreateGC(x->display,x->root,value_mask,&xgc) ;

   x->copybuffer   = x->white ;
   }
   
if ( opt->verbose )
	{
	dumpgc(x->display,x->movingtransp,"moving transp") ;
	dumpgc(x->display,x->movingwhite,"moving white") ;
	if ( opt->buffering==3 )
		{
		dumpgc(x->display,x->movingtransp2,"moving transp2") ;
		dumpgc(x->display,x->movingwhite2,"moving white2") ;
		}
	dumpgc(x->display,x->copybuffer,"copybuffer") ;
	dumpgc(x->display,x->black,"black") ;
	dumpgc(x->display,x->white,"white") ;
	dumpgc(x->display,x->transp,"transp") ;
	fprintf(stderr,"END OF INITGC\n") ;
	}
}
   
   
