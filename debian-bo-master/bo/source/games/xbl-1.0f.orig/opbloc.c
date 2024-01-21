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
#include "define.h"

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif


/* Return 0 if no intersection, 1 if intersection
 * or 10+ TXN TYN TZN TXP TYP TZP if outside bloc
 */
int intersection(m,b)
struct movingbloc *m ;
struct bloc *b ;
{
int x,y,z ;
int i,j,k ;

x = m->tx ;
y = m->ty ;
z = m->tz ;

if ( x<0 ) return(10+TXP) ;
if ( x+m->work->dx>b->dx ) return(10+TXN) ;
if ( y<0 ) return(10+TYP) ;
if ( y+m->work->dy>b->dy ) return(10+TYN) ;
if ( z<0 ) return(10+TZP) ;
if ( z+m->work->dz>b->dz ) return(10+TZN) ;

for(i=m->work->dx-1;i>=0;i--)
for(j=m->work->dy-1;j>=0;j--)
for(k=m->work->dz-1;k>=0;k--)
	if ( m->work->t[k][j][i] &&
	     b->t[k+z][j+y][i+x] )      return(1) ;

return(0) ;
}

int delete_level(m,b)
struct movingbloc *m ;
struct bloc *b ;
{
int x,y,z ;
int i,j,k,kw ;
int nb ;
char *c,*e ;

x = m->tx ;
y = m->ty ;
z = m->tz ;

for(i=m->work->dx-1;i>=0;i--)
for(j=m->work->dy-1;j>=0;j--)
for(k=m->work->dz-1;k>=0;k--)
	b->t[k+z][j+y][i+x] |= m->work->t[k][j][i] ;

nb = b->dx*b->dy ;
kw = b->dz-1 ;
for(k=b->dz-1;k>=0;k--)
	{
	c = &b->t[k][0][0] ;
	e = c + nb ;
	while( *c && c<e ) c++ ;
	
	if (c!=e) /* Level is not destroy */
		{
		/* Copy level */
		memcpy(&b->t[kw][0][0],&b->t[k][0][0],nb*sizeof(b->t[k][0][0])) ;
		kw-- ;
		}
	}


if ( kw>=0 )
	{
	memset(&b->t[0][0][0],'\0',(kw+1)*nb*sizeof(b->t[k][0][0])) ;
	}

return(kw) ;
}


int orbloc(m,b)
struct movingbloc *m ;
struct bloc *b ;
{
int nb ;
struct point *p,*tmp ;

nb = delete_level(m,b) ;

p = b->firstpoint ;
do {
	p->in_a_bloc = 0 ;
	tmp = p->next ;
	p->next = 0 ;
	p->edge = 0 ;
	p = tmp ;
   } while(p) ;
b->firstpoint = 0 ;

return( nb+1 ) ; /* Number of destroy level */
}

int depthbloc(b)
struct bloc *b ;
{
int i,j,k ;
for(k=b->dz-1;k>=0;k--)
	{
	for(i=b->dx-1;i>=0;i--)
		{
		for(j=b->dy-1;j>=0;j--)
			if ( b->t[k][j][i]!=0 ) break ;
		if ( j!=-1 ) break ;
		}
	if (i==-1) break ;
	}
return( b->dz-1-k ) ;
}

struct bloc *findbloc(x,y,z,b)
int x,y,z ;
struct bloc **b ;
{
while( b[0]->dx!=x || b[0]->dy!=y || b[0]->dz!=z ) b++ ;
return(b[0]) ;
}

struct bloc *copy_bloc(n,o)
struct bloc *n,*o ;
{
int i,j,k ;

for(i=n->dx-1;i>=0;i--)
for(j=n->dy-1;j>=0;j--)
for(k=n->dz-1;k>=0;k--)
	n->t[k][j][i] = o->t[k][j][i] ;

n->nbcubes = o->nbcubes ;
n->name    = o->name ;

return(n) ;
}

