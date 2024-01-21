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
#include <stdio.h>
#include <X11/Xlib.h>
#include "transfo.h"
#include "view.h"
#include "bloc.h"
#include "define.h"
#if HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <memory.h>

#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif

#ifndef abs
#define abs(X) ( X>0?X:-(X) )
#endif

/* This structure is redondant, the problem is that in many cases
   the XDrawLines is slower than XDrawSegments */
#define USESEGMENTS

#define MEMO_SEG (64*12)

struct polypolyline
	{
	int	maxnbpoints ;		/* Max size of "points" table */
#ifdef USESEGMENTS
	int	nbpoints ;
	XSegment *segment ;		/* The segments */
#else
	int	*nbpoints ;		/* Table of points number by polyline */
	XPoint  *points ;		/* All the points */
#endif
	} ;

/* Contain last draw line bloc */
static struct polypolyline lastlinebloc = {0,0,0} ;

/* Definition of a bloc : a string

       y
     ^
     |
     |
    /------> x
   /
  V
z
   l : x--
   r : x++
   d : y--
   u : y++
   f : z++
   b : z--
*/
struct bloc *createbloc( def )
char *def ;
{
int xmin,xmax,ymin,ymax,zmin,zmax ;
int x,y,z ;
char *pc ;
struct bloc *b ;

xmin = ymin = zmin = xmax = ymax = zmax = x = y = z =0 ;

pc = def ;
while( *pc )
	switch( *pc++ )
		{
		case 'l' : x-- ; if ( x<xmin ) xmin=x ;
			   break ;
		case 'r' : x++ ; if ( x>xmax ) xmax=x ;
			   break ;
		case 'd' : y-- ; if ( y<ymin ) ymin=y ;
			   break ;
		case 'u' : y++ ; if ( y>ymax ) ymax=y ;
			   break ;
		case 'b' : z-- ; if ( z<zmin ) zmin=z ;
			   break ;
		case 'f' : z++ ; if ( z>zmax ) zmax=z ;
			   break ;
		default  : fprintf(stderr,"Bad Description %s\n",def) ;
			   exit(1) ;
		}

b = allocbloc(xmax-xmin+1,ymax-ymin+1,zmax-zmin+1) ;


pc = def ;
x = y = z = 0 ;
do
	{
	notecube(b,z-zmin,y-ymin,x-xmin) ;
	switch( *pc )
		{
		case 'l' : x-- ; break ;
		case 'r' : x++ ; break ;
		case 'd' : y-- ; break ;
		case 'u' : y++ ; break ;
		case 'b' : z-- ; break ;
		case 'f' : z++ ; break ;
		}
	}
while( *pc++ ) ;

b->name = def ;
b->world = 0 ;

createsegments(b) ;

return(b) ;
}

void createsegments(b)
struct bloc *b ;
{
int i,j ;
int nbtmp ;
struct edge *tmp,swap ;
struct point *start,*end,*sw ;

nbtmp = 0 ;
tmp = (struct edge*) malloc( b->nbedges*sizeof(*tmp) ) ;

start = end = 0 ; /* Only to remove a GCC warning */

/* First stage : concatenate lines */
for(i=0;i<b->nbedges;i++)
	{
	if ( b->edge[i].nb_acces!=2 && b->edge[i].nb_acces!=4 )
		{
		for ( j=0;j<nbtmp;j++ )
			{
			if ( tmp[j].start==b->edge[i].start )
				{
				start = tmp[j].end ;
				end   = b->edge[i].end ;
				}
			else
			if ( tmp[j].start==b->edge[i].end )
				{
				start = tmp[j].end ;
				end   = b->edge[i].start ;
				}
			else
			if ( tmp[j].end==b->edge[i].end )
				{
				start = tmp[j].start ;
				end   = b->edge[i].start ;
				}
			else
			if ( tmp[j].end==b->edge[i].start )
				{
				start = tmp[j].start ;
				end   = b->edge[i].end ;
				}
			else continue ;

			/* Two points in a row */
			if ( start->coord[0]==end->coord[0] )
				{
				if ( start->coord[1]==end->coord[1] ) break ;
				if ( start->coord[2]==end->coord[2] ) break ;
				}
			if ( start->coord[1]==end->coord[1] )
				{
				if ( start->coord[2]==end->coord[2] ) break ;
				}
			}
		if ( j==nbtmp )
			{
			/* Not stretch an existing line */
			tmp[j].start = b->edge[i].start ;
			tmp[j].end   = b->edge[i].end ;
			nbtmp++ ;
			}
		else    {
			/* Stretch an existing line */
			tmp[j].start = start ;
			tmp[j].end   = end ;
			}
		}
	}
/* Second stage : change order, for create polyline */
for(i=0;i<nbtmp;i++)
   for(j=nbtmp-1;j>i;j--)
	{
	if ( tmp[i].end==tmp[j].start )
		{
		swap = tmp[j] ;
		tmp[j] = tmp[i+1] ;
		tmp[i+1] = swap ;
		break ;
		}
	if ( tmp[i].end==tmp[j].end )
		{
		swap = tmp[j] ;
		sw = swap.start ;
		swap.start = swap.end ;
		swap.end = sw ;
		tmp[j] = tmp[i+1] ;
		tmp[i+1] = swap ;
		break ;
		}
	}

if ( b->segments ) free( (void*)b->segments ) ;
b->nbsegments = nbtmp ;
b->segments = tmp ;
}



void notecube(b,z,y,x)
struct bloc *b ;
int x,y,z ;
{
int i,j,k ;

if ( b->t[z][y][x]!=0 ) return ;
b->t[z][y][x] = 1 ;
b->nbcubes++ ;

for(i=0;i<8;i++)
for(j=i+1;j<8;j++)
	{
	k = i^j ;
	if ( k==1 || k==2 || k==4 )
		createedge(b,z+(i&1),y+((i/2)&1),x+((i/4)&1),
			     z+(j&1),y+((j/2)&1),x+((j/4)&1)  ) ;
	}
}

void noteface(b,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4 ,l)
struct bloc *b ;
int x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4 ;
int l ;
{
b->face[b->nbfaces].p[0] = createpoint(b,z1,y1,x1) ;
b->face[b->nbfaces].p[1] = createpoint(b,z2,y2,x2) ;
b->face[b->nbfaces].p[2] = createpoint(b,z3,y3,x3) ;
b->face[b->nbfaces].p[3] = createpoint(b,z4,y4,x4) ;
b->face[b->nbfaces].level = b->dz-l-1 ;
b->nbfaces++ ;
}


void createfaces(b)
struct bloc *b ;
{
int i,j,k,l ;
if ( b->face==0 )
	 b->face = (struct face*)
			malloc( 6*b->dx*b->dy*b->dz*sizeof(struct face) ) ;
b->nbfaces = 0 ;
for(k=b->dz-1;k>=0;k--)
for(l=0;l<3;l++)
for(i=0;i<b->dx;i++)
for(j=0;j<b->dy;j++)
   if ( b->t[k][j][i] )
	{
	switch(l)
	   {
	   case 2 :
		if ( k==0 || b->t[k-1][j][i]==0 )
			noteface(b,i,j,k,i+1,j,k,i+1,j+1,k,i,j+1,k ,k) ;
		break ;
	   case 1 :
		if ( i==0 || b->t[k][j][i-1]==0 )
			noteface(b,i,j,k,i,j+1,k,i,j+1,k+1,i,j,k+1 ,k) ;
		if ( j==0 || b->t[k][j-1][i]==0 )
			noteface(b,i,j,k,i,j,k+1,i+1,j,k+1,i+1,j,k ,k) ;
		if ( i==b->dx-1 || b->t[k][j][i+1]==0 )
			noteface(b,i+1,j,k,i+1,j,k+1,i+1,j+1,k+1,i+1,j+1,k ,k) ;
		if ( j==b->dy-1 || b->t[k][j+1][i]==0 )
			noteface(b,i,j+1,k,i+1,j+1,k,i+1,j+1,k+1,i,j+1,k+1 ,k) ;
		break ;
	   case 0 :
		if ( k==b->dz-1 || b->t[k+1][j][i]==0 )
			noteface(b,i,j,k+1,i,j+1,k+1,i+1,j+1,k+1,i+1,j,k+1 ,k) ;
		break ;
	    }
	}
}


void createedge(b,z1,y1,x1,z2,y2,x2)
struct bloc *b ;
int z1,y1,x1,z2,y2,x2 ;
{
struct point *start,*end ;
struct edge *e ;

start = createpoint(b,z1,y1,x1) ;
end   = createpoint(b,z2,y2,x2) ;

e = start->edge ;
while( e )
	{
	if ( e->end==end ) break ;
	e = e->next ;
	}

if ( e==0 )
	{
	b->edge[b->nbedges].start = start ;
	b->edge[b->nbedges].end   = end ;
	b->edge[b->nbedges].nb_acces = 1 ;
	b->edge[b->nbedges].next  = start->edge ;
	start->edge = &b->edge[b->nbedges] ;
	b->nbedges++ ;
	}
	else e->nb_acces++ ;
}

struct point *createpoint(b,z,y,x)
struct bloc *b ;
int x,y,z ;
{
int i ;

i = ((z*(b->dy+1))+y)*(b->dx+1) + x ;

if ( i>=b->nbpoints ) fprintf(stderr,"BUG : Impossible case\n") ;

if ( ! b->point[i].in_a_bloc )
	{
	b->point[i].coord[0] = x ;
	b->point[i].coord[1] = y ;
	b->point[i].coord[2] = z ;
	b->point[i].next = b->firstpoint ;
	b->point[i].edge = 0 ;
	b->point[i].in_a_bloc = 1 ;
	b->firstpoint = &b->point[i] ;
	}
return( &b->point[i] ) ;
}


struct bloc *allocbloc(dx,dy,dz)
int dx,dy,dz ;
{
struct bloc *b ;
int i,j ;
char *pc ;

b = (struct bloc*) malloc( sizeof(struct bloc) ) ;
b->dx = dx ;
b->dy = dy ;
b->dz = dz ;
b->nbedges = 0 ;
b->nbfaces = 0 ;
b->nbcubes = 0 ;
b->edge = (struct edge*)
	 malloc( (b->dz+1)*(b->dy+1)*(b->dx+1)*3*sizeof(struct edge) ) ;
b->nbpoints = (b->dz+1)*(b->dy+1)*(b->dx+1) ;
b->point = (struct point*)
	 malloc( b->nbpoints*sizeof(struct point) ) ;
b->face = 0 ;
b->segments = 0 ;
for(i=0;i<b->nbpoints;i++)
	{
	b->point[i].next = 0 ;
	b->point[i].edge = 0 ;
	b->point[i].in_a_bloc = 0 ;
	}
b->firstpoint = 0 ;

b->t = (char***) malloc( b->dz*sizeof(char **) ) ;
pc   = (char*) malloc( (unsigned)(b->dz*b->dy*b->dx) ) ;
memset( (void*)pc , '\0' , b->dz*b->dy*b->dx ) ;
for(i=0;i<b->dz;i++)
	{
	b->t[i] = (char**) malloc( b->dy*sizeof(char*) ) ;
	for(j=0;j<b->dy;j++)
		{
		b->t[i][j] = pc ;
		pc += b->dx ;
		}
	}
return(b) ;
}


void freebloc(b)
struct bloc *b ;
{
int i ;

free( (void*)b->t[0][0] ) ;
for(i=0;i<b->dz;i++) free( (void*)b->t[i] ) ;
free( (void*)b->t ) ;
free( (void*)b->edge ) ;
free( (void*)b->point ) ;
if ( b->face ) free( (void*)b->face ) ;
if ( b->segments ) free( (void*)b->segments ) ;
free( (void*)b ) ;
}



/*--------------------------*/
/* Utility for polypolyline */
/*--------------------------*/
void newpolypolyline(oldone,nbpoints)
struct polypolyline *oldone ;
int nbpoints ;
{
if ( nbpoints<=oldone->maxnbpoints ) return ; /* Nothing to do */
if ( oldone->maxnbpoints!=0 )
	{
#ifndef USESEGMENTS
	free( (void*)oldone->nbpoints ) ;
	free( (void*)oldone->points ) ;
#else
	free( (void*)oldone->segment ) ;
#endif
	}
#ifndef USESEGMENTS
oldone->nbpoints = (int *) malloc(2*nbpoints*sizeof( *(oldone->nbpoints) ) ) ;
oldone->points   = (XPoint *) malloc(2*nbpoints*sizeof( *(oldone->points) ) ) ;
oldone->nbpoints[0] = 0 ;
#else
oldone->segment  = (XSegment *) malloc(nbpoints*sizeof( *(oldone->segment) ) ) ;
oldone->nbpoints = 0 ;
#endif
oldone->maxnbpoints = nbpoints ;
}
void drawpolypolyline(disp,d,gc,ppl)
Display *disp ;
Drawable d ;
GC gc ;
struct polypolyline *ppl ;
{
#ifndef USESEGMENTS
int i,j ;
#endif

if ( ppl->maxnbpoints==0 ) return ;
#ifndef USESEGMENTS
i = 0 ;
j = 0 ;
while( ppl->nbpoints[i] )
	{
	XDrawLines(disp,d,gc,&ppl->points[j],ppl->nbpoints[i],CoordModeOrigin) ;
	j += ppl->nbpoints[i] ;
	i++ ;
	}
#else
XDrawSegments(disp,d,gc,ppl->segment,ppl->nbpoints) ;
#endif
}
/* Copy second into first */
void copypolypolyline(ppl1,ppl2)
struct polypolyline *ppl1 ;
struct polypolyline *ppl2 ;
{
int i ;
#ifndef USESEGMENTS
int j,k ;
#endif

newpolypolyline(ppl1,ppl2->maxnbpoints) ;
#ifndef USESEGMENTS
i = 0 ;
k = 0 ;
while( ppl2->nbpoints[i] )
	{
	ppl1->nbpoints[i] = ppl2->nbpoints[i] ;
	for(j=0;j<ppl1->nbpoints[i];j++)
		{
		ppl1->points[k] =  ppl2->points[k] ;
		k++ ;
		}
	i++ ;
	}
ppl1->nbpoints[i] = 0 ;
#else
for(i=0;i<ppl2->nbpoints;i++) ppl1->segment[i] = ppl2->segment[i] ;
ppl1->nbpoints = ppl2->nbpoints ;
#endif
}
/*--------------------------*/

void drawlinebloc( disp , d , gc , b , t , view ,min,max)
Display *disp ;
Drawable d ;
GC gc ;
struct bloc *b ;
struct transfo *t ;
struct viewtransfo *view ;
struct point *min,*max ;
{
int i ;
#ifndef USESEGMENTS
int l,k ;
#endif

newpolypolyline( &lastlinebloc,b->nbsegments*2 ) ;
transfopoint(b,t,view,min,max) ;
#ifndef USESEGMENTS
k = 0 ;
l = 0 ;
for(i=0;i<b->nbsegments;i++)
	{
	j = l ;
	lastlinebloc.points[l].x = b->segments[i].start->x ;
	lastlinebloc.points[l].y = b->segments[i].start->y ;
	while( b->segments[i].end == b->segments[i+1].start &&
	       i+1<b->nbsegments)
		{
		i++ ; l++ ;
		lastlinebloc.points[l].x = b->segments[i].start->x ;
		lastlinebloc.points[l].y = b->segments[i].start->y ;
		}
	l++ ;
	lastlinebloc.points[l].x = b->segments[i].end->x ;
	lastlinebloc.points[l].y = b->segments[i].end->y ;
	l++ ;
	lastlinebloc.nbpoints[k++] = l-j ;
	}
lastlinebloc.nbpoints[k] = 0 ;
#else
for(i=0;i<b->nbsegments;i++)
	{
	lastlinebloc.segment[i].x1 = b->segments[i].start->x ;
	lastlinebloc.segment[i].y1 = b->segments[i].start->y ;
	lastlinebloc.segment[i].x2 = b->segments[i].end->x ;
	lastlinebloc.segment[i].y2 = b->segments[i].end->y ;
	}
lastlinebloc.nbpoints = b->nbsegments ;
#endif
drawpolypolyline(disp,d,gc,&lastlinebloc) ;
}

int clearlinebloc( disp,d,gc )
Display *disp ;
Drawable d ;
GC gc ;
{
#ifndef USESEGMENTS
if ( lastlinebloc.nbpoints[0]!=0 )
#else
if ( lastlinebloc.nbpoints!=0 )
#endif
	{
	drawpolypolyline(disp,d,gc,&lastlinebloc) ;
#ifndef USESEGMENTS
	lastlinebloc.nbpoints[0] = 0 ;
#else
	lastlinebloc.nbpoints = 0 ;
#endif
	return(1) ;
	}
return(0) ;
}

void clearlastline(  disp , d , gc )
Display *disp ;
Drawable d ;
GC gc ;
{
static struct polypolyline memo = {0,0,0} ;
drawpolypolyline(disp,d,gc,&memo) ;
copypolypolyline(&memo,&lastlinebloc) ; 
}

void forgetlinebloc()
{
#ifndef USESEGMENTS
	lastlinebloc.nbpoints[0] = 0 ;
#else
	lastlinebloc.nbpoints = 0 ;
#endif
}


void drawfacebloc( disp , d , gc,gcl , b , t , view ,min,max)
Display *disp ;
Drawable d ;
GC *gc ;
GC gcl ;
struct bloc *b ;
struct transfo *t ;
struct viewtransfo *view ;
struct point *min,*max ;
{
int i,j,vx1,vy1,vx2,vy2,pv ;
XPoint p[5] ;

transfopoint(b,t,view,min,max) ;
for(i=0;i<b->nbfaces;i++)
	{
	for(j=0;j<4;j++)
		{
		p[j].x = b->face[i].p[j]->x ;
		p[j].y = b->face[i].p[j]->y ;
		}
	vx1 = p[1].x - p[0].x ;
	vy1 = p[1].y - p[0].y ;
	vx2 = p[2].x - p[1].x ;
	vy2 = p[2].y - p[1].y ;
	pv = vx1*vy2-vx2*vy1 ;
	if ( pv>0 )
		{
		XFillPolygon( disp,d,gc[b->face[i].level%FACECOLOR],
				p,4,Convex,CoordModeOrigin ) ;
		p[4] = p[0] ;
		XDrawLines( disp,d,gcl,p,5,CoordModeOrigin ) ;
		}
	}
}

void drawtranspbloc( disp , d , gc,gcl , b , t , view ,min,max)
Display *disp ;
Drawable d ;
GC gc ;
GC gcl ;
struct bloc *b ;
struct transfo *t ;
struct viewtransfo *view ;
struct point *min,*max ;
{
int i,j,k,vx1,vy1,vx2,vy2,pv ;
XSegment s[MEMO_SEG] ;
XPoint p[5] ;

transfopoint(b,t,view,min,max) ;

for(i=0;i<b->nbfaces;i++)
	{
	for(j=0;j<4;j++)
		{
		p[j].x = b->face[i].p[j]->x ;
		p[j].y = b->face[i].p[j]->y ;
		}
	vx1 = p[1].x - p[0].x ;
	vy1 = p[1].y - p[0].y ;
	vx2 = p[2].x - p[1].x ;
	vy2 = p[2].y - p[1].y ;
	pv = vx1*vy2-vx2*vy1 ;
	if ( pv>0 )
		XFillPolygon( disp,d,gc,
				p,4,Convex,CoordModeOrigin ) ;
	}

k = 0 ;
for(i=0;i<b->nbfaces;i++)
	{
	for(j=0;j<4;j++)
		{
		s[k].x1 = b->face[i].p[j]->x ;
		s[k].y1 = b->face[i].p[j]->y ;
		s[k].x2 = b->face[i].p[(j+1)%4]->x ;
		s[k].y2 = b->face[i].p[(j+1)%4]->y ;
		if ( s[k].x1 > s[k].x2 ||
		     ( s[k].x1 == s[k].x2 && s[k].y1 > s[k].y2 )
		   )
			if ( k<MEMO_SEG ) k++ ;
				else fprintf(stderr,"Too many seg %s:%d\n",
						__FILE__,__LINE__) ;
		}
	}
XDrawSegments(disp,d,gcl,s,k) ;

}


/* Compute all point position of the bloc, even if point are not used */
void transfopoint(b,t,view,min,max)
struct bloc *b ;
struct transfo *t ;
struct viewtransfo *view ;
struct point *min,*max ;
{
float curx[3],cury[3],curz[3],dirx[3],diry[3],dirz[3],v[3] ;
struct point *p ;
int i,j,k,l,m ;

if ( b->world==1 ) return ; /* The world doesn't move */
p = b->point ;
m = 0 ;

for(i=0;i<3;i++)
	{
	curz[i] = 0. ;
	dirx[i] = t->mat[i][0] ;
	diry[i] = t->mat[i][1] ;
	dirz[i] = t->mat[i][2] ;
	}

for(k=0;k<=b->dz;k++)
   {
   cury[0] = curz[0] ;
   cury[1] = curz[1] ;
   cury[2] = curz[2] ;
   for(j=0;j<=b->dy;j++)
      {
      curx[0] = cury[0] ;
      curx[1] = cury[1] ;
      curx[2] = cury[2] ;
	
      l = b->dx ;
      if ( !b->world ) while( l>=0 && !p[l].in_a_bloc ) l-- ;

      for(i=0;i<=l;i++)
	{
	if ( p->in_a_bloc || b->world ) /* All point in world */
	   {
	   v[0] = curx[0] + t->vec[0] ;
	   v[1] = curx[1] + t->vec[1] ;
	   v[2] = curx[2] + t->vec[2] ;
	   p->x = (int)(v[0]/v[2]*view->xprod)+view->xcenter ;
	   p->y = (int)(v[1]/v[2]*view->yprod)+view->ycenter ;
	   if ( m==0 )
		{
		m = 1 ;
		min->x = max->x = p->x ;
		min->y = max->y = p->y ;
		}
	   else
		{
		if ( p->x<min->x ) min->x = p->x ;
			else
			if ( p->x>max->x ) max->x = p->x ;
		if ( p->y<min->y ) min->y = p->y ;
			else
			if ( p->y>max->y ) max->y = p->y ;
		}
	   }
	curx[0] += dirx[0] ;
	curx[1] += dirx[1] ;
	curx[2] += dirx[2] ;

	p++ ;
	}
      p += b->dx - l ;
      cury[0] += diry[0] ;
      cury[1] += diry[1] ;
      cury[2] += diry[2] ;
      }
   curz[0] += dirz[0] ;
   curz[1] += dirz[1] ;
   curz[2] += dirz[2] ;
   }

max->x++ ;
max->y++ ;

if ( min->x<0 ) min->x = 0 ;
if ( min->y<0 ) min->y = 0 ;
if ( b->world ) b->world = 1 ; /* No more compute for world points */
}
