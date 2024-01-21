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

#include <math.h>
#include <stdlib.h>

#include "keyid.h"

#ifndef abs
#define abs(Y) ( (Y)>0 ? (Y) : -(Y) )
#endif
#define Px(x,y,z) ((int)(xx/2. + x - z/1.5))
#define Py(x,y,z) ((int)(yy/2. + y + z/2.6))

static unsigned char *e ;
static int l,xx,yy ;

/***************************************** 2D pixel plot */

void d2_pl(x,y)
int x,y ;
{
if ( x<0 || y<0 || x>=xx || y>=yy ) return ;
e[y*l+x/8] |= 1<<(x%8) ;
}

/***************************************** 3D pixel plot */

void pl(x,y,z)
float x,y,z ;
{
d2_pl( Px(x,y,z) , Py(x,y,z) ) ;
}

/***************************************** 3D big pixel */

void pl4(x,y,z)
float x,y,z ;
{
int i,j,k ;
for(i=0;i<2;i++)
for(j=0;j<2;j++)
for(k=0;k<2;k++)
	pl(x+i,y+j,z+k) ;
}

/***************************************** 2D pixel line */

void d2_line(xx1,yy1,x2,y2)
int xx1,yy1,x2,y2 ;
{
float dx,dy,x,y ;
int ll ;

dx = x2-xx1 ;
dy = y2-yy1 ;
ll = abs(dx)>abs(dy) ? abs(dx) : abs(dy) ;
dx /= ll ;
dy /= ll ;
x = xx1 ;
y = yy1 ;
while(ll>0)
	{
	d2_pl( (int)x,(int)y ) ;
	x += dx ;
	y += dy ;
	ll-- ;
	}
d2_pl(x2,y2) ;
}

/***************************************** 3D pixel line */

void d3_line(xx1,yy1,zz1,x2,y2,z2)
float xx1,yy1,zz1,x2,y2,z2 ;
{
d2_line( Px(xx1,yy1,zz1),Py(xx1,yy1,zz1),Px(x2,y2,z2),Py(x2,y2,z2)) ;
}

/***************************************** 3D pixel arrow */

void arrow(xx1,yy1,zz1,x2,y2,z2,d)
float xx1,yy1,zz1,x2,y2,z2 ;
int d ;
{
d3_line( xx1,yy1,zz1,x2+(d==0),y2+(d==1),z2+(d==2)) ; 
d3_line( xx1,yy1,zz1,x2+2*(d==0),y2+2*(d==1),z2+2*(d==2)) ; 
d3_line( xx1,yy1,zz1,x2+3*(d==0),y2+3*(d==1),z2+3*(d==2)) ; 
d3_line( xx1,yy1,zz1,x2+4*(d==0),y2+4*(d==1),z2+4*(d==2)) ; 
d3_line( xx1,yy1,zz1,x2+5*(d==0),y2+5*(d==1),z2+5*(d==2)) ; 
}

/***************************************** Create the help */

void cube(dx,dy,tx,ty)
int dx,dy ;
int tx[KEY_ROT_Z_NEG+1],ty[KEY_ROT_Z_NEG+1] ;
{
#define teta_arrow (M_PI/16)
#define teta_hole M_PI/30
float R,half ;
float dteta,teta,co,si,nco,nsi,plc ;
int df ;

half = dy/4. ;
R = half-3 ;

/* The cube */

d3_line(-half,-half,half,half,-half,half) ;
d3_line(-half,-half,half,-half,half,half) ;
d3_line(-half,-half,half,-half,-half,-half) ;
d3_line(-half,half,half,half,half,half) ;
d3_line(half,-half,half,half,half,half) ;
d3_line(-half,-half,-half,half,-half,-half) ;
d3_line(half,-half,half,half,-half,-half) ;
d3_line(half,half,-half,half,-half,-half) ;
d3_line(half,half,half,half,half,-half) ;

plc = R/3 ;
tx[KEY_ROT_Z_POS] = Px(0,plc,half)  ; ty[KEY_ROT_Z_POS] = Py(0,plc,half) ;
tx[KEY_ROT_Z_NEG] = Px(0,-plc,half) ; ty[KEY_ROT_Z_NEG] = Py(0,-plc,half) ;
tx[KEY_ROT_X_POS] = Px(half,plc,0)  ; ty[KEY_ROT_X_POS] = Py(half,plc,0) ;
tx[KEY_ROT_X_NEG] = Px(half,-plc,0) ; ty[KEY_ROT_X_NEG] = Py(half,-plc,0) ;
tx[KEY_ROT_Y_POS] = Px(0,-half,plc) ; ty[KEY_ROT_Y_POS] = Py(0,-half,plc) ;
tx[KEY_ROT_Y_NEG] = Px(0,-half,-plc); ty[KEY_ROT_Y_NEG] = Py(0,-half,-plc) ;

/* The circles */

dteta = M_PI/R/4 ;

for(teta=teta_hole;teta<=M_PI+1e-4-teta_hole;teta+=dteta)
	{
	co = R*cos(teta) ;
	si = R*sin(teta) ;
	pl( co,si,half ) ;
	pl( co,-si,half ) ;
	pl( half,si,co ) ;
	pl( half,-si,co ) ;
	pl( co,-half,si ) ;
	pl( co,-half,-si ) ;
	}


/* The circle arrows */

nco = R*cos(teta-teta_arrow) ;
nsi = R*sin(teta-teta_arrow) ;

arrow( co,si,half,  nco,nsi,half,  0 ) ; 
arrow( co,-si,half, nco,-nsi,half, 0 ) ; 
arrow( half,-si,co, half,-nsi,nco, 2 ) ; 
arrow( half,si,co,  half,nsi,nco,  2 ) ; 
arrow( co,-half,-si,nco,-half,-nsi, 0 ) ; 
arrow( co,-half,si, nco,-half,nsi,  0 ) ; 

/* The translate arrows */

df = dx/30 ;
d2_line(0,dy/2,df,dy/2+df) ;
d2_line(0,dy/2,df,dy/2-df) ;
d2_line(df,dy/2+df,df,dy/2-df) ;

d2_line(dx-1,dy/2,dx-df-1,dy/2+df) ;
d2_line(dx-1,dy/2,dx-df-1,dy/2-df) ;
d2_line(dx-df-1,dy/2+df,dx-df-1,dy/2-df) ;

d2_line(dx/2,0,dx/2-df,df) ;
d2_line(dx/2,0,dx/2+df,df) ;
d2_line(dx/2-df,df,dx/2+df,df) ;

d2_line(dx/2,dy-1,dx/2-df,dy-1-df) ;
d2_line(dx/2,dy-1,dx/2+df,dy-1-df) ;
d2_line(dx/2-df,dy-1-df,dx/2+df,dy-1-df) ;

df += 2 ;

d2_line(0,0,df,0) ;
d2_line(0,0,0,df) ;
d2_line(df,0,0,df) ;

d2_line(dx-1,0,dx-1-df,0) ;
d2_line(dx-1,0,dx-1,df) ;
d2_line(dx-1-df,0,dx-1,df) ;

d2_line(0,dy-1,df,dy-1) ;
d2_line(0,dy-1,0,dy-1-df) ;
d2_line(df,dy-1,0,dy-1-df) ;

d2_line(dx-1,dy-1,dx-1-df,dy-1) ;
d2_line(dx-1,dy-1,dx-1,dy-1-df) ;
d2_line(dx-1-df,dy-1,dx-1,dy-1-df) ;

df -= 2 ;
plc = ( Py(0,-half,-half) - df ) / 2 + df ;
tx[KEY_UP_LEFT]    = plc     ; ty[KEY_UP_LEFT]    = plc ;
tx[KEY_UP]         = dx/2    ; ty[KEY_UP]         = plc ;
tx[KEY_UP_RIGHT]   = dx-plc  ; ty[KEY_UP_RIGHT]   = plc ;
tx[KEY_DOWN_LEFT]  = plc     ; ty[KEY_DOWN_LEFT]  = dy-plc ;
tx[KEY_DOWN]       = dx/2    ; ty[KEY_DOWN]       = dy-plc ;
tx[KEY_DOWN_RIGHT] = dx-plc  ; ty[KEY_DOWN_RIGHT] = dy-plc ;
tx[KEY_LEFT]       = plc     ; ty[KEY_LEFT]       = dy/2 ;
tx[KEY_RIGHT]      = dx-plc  ; ty[KEY_RIGHT]      = dy/2 ;

}

char *helpbitmap(dx,dy,tx,ty)
int dx,dy ;
int tx[KEY_ROT_Z_NEG+1],ty[KEY_ROT_Z_NEG+1] ;
{
int i ;
int m ;

m = ((dx+7)*dy)/8 ;
e = (unsigned char*)malloc( m ) ;
l = (dx+7)/8 ;
xx = dx ;
yy = dy ;
for(i=0;i<m;i++) e[i] = 0 ;
cube(dx,dy,tx,ty) ;
return((char*)e) ;
}
