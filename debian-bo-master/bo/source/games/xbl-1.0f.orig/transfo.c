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
#include "transfo.h"
#include <math.h>
#include <stdio.h>

#ifdef HAVE_PROTOTYPES
void createtransfo( int typ, struct transfo *t, float val, float tx,
			float ty, float tz)
#else
void createtransfo( typ,t,val,tx,ty,tz )
int typ ;
struct transfo *t ;
float val ;
float tx,ty,tz ;
#endif
{
float (*m)[3] ;
int i ;

m = t->mat ;

switch( typ )
   {
   case ROTX :
	      m[0][0] = 1. ;
	      m[1][0] = m[2][0] = m[0][1] = m[0][2] = 0. ;
	      m[1][1] = m[2][2] = cos( (double)val ) ;
	      m[1][2] = -( m[2][1] = sin( (double)val ) ) ;
	      break ;
   case ROTY :
	      m[1][1] = 1. ;
	      m[1][0] = m[2][1] = m[0][1] = m[1][2] = 0. ;
	      m[0][0] = m[2][2] = cos( (double)val ) ;
	      m[0][2] = -( m[2][0] = -sin( (double)val ) ) ;
	      break ;
   case ROTZ :
	      m[2][2] = 1. ;
	      m[1][2] = m[2][0] = m[2][1] = m[0][2] = 0. ;
	      m[1][1] = m[0][0] = cos( (double)val ) ;
	      m[0][1] = -( m[1][0] = sin( (double)val ) ) ;
	      break ;
   case DILX :
	      for(i=0;i<9;i++) m[0][i] = 0.;
	      m[0][0] = val ;
	      m[1][1] = m[2][2] = 1. ;
	      break ;
   case DILY :
	      for(i=0;i<9;i++) m[0][i] = 0.;
	      m[1][1] = val ;
	      m[0][0] = m[2][2] = 1. ;
	      break ;
   case DILZ :
	      for(i=0;i<9;i++) m[0][i] = 0.;
	      m[2][2] = val ;
	      m[0][0] = m[1][1] = 1. ;
	      break ;
   case HOMO :
	      for(i=0;i<9;i++) m[0][i] = 0.;
	      m[2][2] = m[1][1] = m[0][0] = val ;
	      break ;
   case UNIT :
	      for(i=0;i<9;i++) m[0][i] = 0.;
	      m[2][2] = m[1][1] = m[0][0] = 1. ;
	      break ;
   }

t->vec[0] = tx ;
t->vec[1] = ty ;
t->vec[2] = tz ;
}


void traXtra( tg,td,tr )
struct transfo *tg,*td,*tr ;
{
float m[3][3] ;
int i ;

matXmat ( tg->mat,td->mat,m ) ;
traXvec ( tg,td->vec,tr->vec ) ;
for ( i=0;i<9;i++ ) tr->mat[0][i] = m[0][i] ;
}

void traXvec(t,b,c)
struct transfo *t ;
float b[3],c[3] ;
{
int i ;
float a[3] ;

matXvec( t->mat , b , a ) ;
for ( i=0;i<3;i++ ) c[i] = a[i] + t->vec[i] ;
}

void matXmat(a,b,c)
float a[3][3],b[3][3],c[3][3] ;
{
float r[3][3],l ;
int i,j,k ;

for(i=0;i<3;i++)
   for(j=0;j<3;j++)
      {
      l = 0. ;
      for(k=0;k<3;k++)
         l += a[i][k]*b[k][j] ;
      r[i][j] = l ;
      }
for (i=0;i<9;i++)  c[0][i] = r[0][i] ;
}

void matXvec(a,b,c)
float a[3][3],b[3],c[3] ;
{
int i,k ;
float t[3],l ;

for(i=0;i<3;i++)
      {
      l = 0. ;
      for(k=0;k<3;k++)
         l += a[i][k]*b[k] ;
      t[i] = l ;
      }
for (i=0;i<3;i++)  c[i] = t[i] ;
}

void trapXtrap(a,b,c)
struct transfo *a,*b,*c ;
{
int i ;
matXmat( a->mat , b->mat , c->mat ) ;
for (i=0;i<3;i++)  c->vec[i] = b->vec[i]+a->vec[i] ;
}

void printtra(t)
struct transfo *t ;
{
int i ;
for(i=0;i<3;i++)
printf("(%g %g %g) (%g)\n",t->mat[i][0],t->mat[i][1],t->mat[i][2],t->vec[i]) ;
}
