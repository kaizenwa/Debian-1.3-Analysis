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
#include <math.h>
#include <stdio.h>
#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

void printmoving() ;

#define MAX_TRY 3

static struct stack_try {	
	struct bloc *work ;	/* Work bloc save */
	struct transfo real ;
	int tx,ty,tz ;
	float fdepth,ddepth ;
	float upleft[3] ;
	int nb ;
       } table[MAX_TRY] ;
static int nb_try=0 ;


void push_moving_bloc(b)
struct movingbloc *b ;
{
struct stack_try *s ;

if ( nb_try==MAX_TRY )
	{
	fprintf(stderr,"Too many try in %s:%d\n",__FILE__,__LINE__) ;
	exit(1) ;
	}
s = &table[nb_try] ;

s->work = b->work ;
b->work = copy_bloc( allocbloc(s->work->dx,s->work->dy,s->work->dz),s->work) ;

s->real = b->real ;
s->nb   = b->nb ;
s->tx   = b->tx ;
s->ty   = b->ty ;
s->tz   = b->tz ;
s->fdepth = b->fdepth ;
s->ddepth = b->ddepth ;
s->upleft[0] = b->upleft[0] ;
s->upleft[1] = b->upleft[1] ;
s->upleft[2] = b->upleft[2] ;
nb_try++ ;
}

void pull_moving_bloc(b)
struct movingbloc *b ;
{
struct stack_try *s ;

if ( nb_try==0 )
	{
	fprintf(stderr,"Too many pull %s:%d\n",__FILE__,__LINE__) ;
		exit(1) ;
	}

nb_try-- ;
s = &table[nb_try] ;

freebloc(b->work) ;
b->work = s->work ;	/* Retrieve old pointer */
b->real = s->real ;
b->nb   = s->nb ;
b->tx   = s->tx ;
b->ty   = s->ty ;
b->tz   = s->tz ;
b->fdepth = s->fdepth ;
b->ddepth = s->ddepth ;
b->upleft[0] = s->upleft[0] ;
b->upleft[1] = s->upleft[1] ;
b->upleft[2] = s->upleft[2] ;
}

void validate_moving_bloc()
{
if ( nb_try==0 )
	{
	fprintf(stderr,"Too many validate %s:%d\n",__FILE__,__LINE__) ;
	exit(1) ;
	}
nb_try-- ;
freebloc( table[nb_try].work ) ;
}


void initmovingbloc(b,t)
struct movingbloc *b ;
struct transfo *t ;
{
b->nb = 0 ;
b->visual = *t ;
b->real   = *t ;
b->starting= *t ;
b->visual.vec[2] = PERSP ;
b->real.vec[2] = 0. ;
b->starting.vec[2] = 0. ;
b->tx = 0 ;
b->ty = 0 ;
b->tz = 0 ;
b->sdepth = PERSP ;
b->depth = b->sdepth ;
b->fdepth = b->sdepth ;
b->ddepth = 0. ;
b->upleft[0] = b->visual.vec[0] ;
b->upleft[1] = b->visual.vec[1] ;
b->upleft[2] = 0. ;
if ( b->work ) freebloc(b->work) ; /* Free previous block */
b->work = copy_bloc( allocbloc(b->b->dx,b->b->dy,b->b->dz),b->b ) ;
}


/* Add a trasnformation to the list without looking if
 * the movment is OK (don't touch something)
 * Return an error if too many movments
 */

int stupidaddtransfo(type,between,b,l)
int type ;
int between ;
struct movingbloc *b ;
int l ;
{
/* The fall is a special case, it fall always down even after rotation ! */
if ( type==FALL || type==TZP )
   {
   /* if ( b->work==0 ) b->work = copybloc(b->actual,b->tb[1]) ; */
   b->tz++ ;
   if ( between==0 ) return(0) ;
   b->fdepth += ((float)l)/between ;
   b->ddepth = (b->fdepth-b->sdepth)/between ;
   return(0) ;
   }

/* Does it possible to add a new transformation ? */
if ( b->nb>=NBTR-1 )
	{
	fprintf(stderr,"Too many movements\n") ;
	return(1) ;
	}

/* Add the transformation */

b->t[b->nb].timelife  = l ;		/* Time life of this transformation */
createmove(type,&b->t[b->nb],between,b) ;
traXtra( &b->t[b->nb].final,&b->real,&b->real ) ;/* Compute new real position */
b->nb++ ;				/* One more trasnformation */

return(0) ;
}

int addtransfo(type,between,b,l)
struct movingbloc *b ;
int type ;
int between ;
int l ;
{
int where ;
int i ;
int ok ;

if ( b->nb==0 ) b->starting = b->real ;

push_moving_bloc(b) ;

if ( stupidaddtransfo(type,between,b,l) )
	{
	pull_moving_bloc(b) ;
	return(0) ;
	}

ok = 1 ;

where = intersection( b , b->world ) ; /* Found where is the intersection */
if ( where )
  {
    if ( type>=RZP && type<=RXN  )
      {
	/* This intersection wasn't due to an translation */
	/* Look if a translation correct this intersection */
	where -= 10 ;		/* Intersection return +10 value avoiding 0 */
	/* In the next test it's <=TZP and not TZN else pieces can go up! */
	if ( where>=TXP && where<=TZP )	/* Here is the problem */
	  {
	    for(i=0;i<4;i++)	/* Try some time TX 1 then 2 3 */
	      {
		if ( stupidaddtransfo(where,between,b,l) ) /* too many transfo */
		  {
		    pull_moving_bloc(b) ;
		    return(0) ;
		  }
		if ( intersection( b , b->world )==0 )
		  break ;	/* The modification is OK */
	      }
	    if (i==4)
	      ok=0 ;		/* Can't correct, this is FALSE if */
				/* you use piece lentgh > 5 */
	    else
	      ok=2 ;
	  }
	else ok = 0  ;		/* piece go up!!! */
      }
    else
      ok = 0 ;			/* The translation wasn't good */
    
    if ( ok==0 )
      {
	pull_moving_bloc(b) ;
	return(0);
      }
  }

/* Validate all the work */

validate_moving_bloc() ;

return(1) ;
}


/* Return 0 if no change, 1 if change */

int nextime(b)
struct movingbloc *b ;
{
int i,j ;
struct time_transfo *tt ;
float t1,t2,t3,t4,n[9],*t,*vec,v[3] ;

if ( b->nb==0 ) b->starting = b->real ;

b->visual = b->starting ;
vec       = b->visual.vec ;
t         = &b->visual.mat[0][0] ;	

for(i=0;i<b->nb;i++)
   {
   tt = &b->t[i] ;
   if ( tt->type>=RZP && tt->type<=RXN )
	{
/*
printf("mat t %f %f %f %f\n",tt->t[0],tt->t[1],tt->t[2],tt->t[3]) ;
printf("mat dt %f %f %f %f\n",tt->dt[0],tt->dt[1],tt->dt[2],tt->dt[3]) ;
*/
	t1 = tt->t[0]*tt->dvt[0] + tt->t[1]*tt->dvt[1] + tt->vt[0] ;
	t2 = tt->t[2]*tt->dvt[0] + tt->t[3]*tt->dvt[1] + tt->vt[1] ;
	tt->vt[0] = t1 ;
	tt->vt[1] = t2 ;

	t1 = tt->t[0]*tt->dt[0] + tt->t[1]*tt->dt[2] ;
	t2 = tt->t[0]*tt->dt[1] + tt->t[1]*tt->dt[3] ;
	t3 = tt->t[2]*tt->dt[0] + tt->t[3]*tt->dt[2] ;
	t4 = tt->t[2]*tt->dt[1] + tt->t[3]*tt->dt[3] ;
	tt->t[0] = t1 ;
	tt->t[1] = t2 ;
	tt->t[2] = t3 ;
	tt->t[3] = t4 ;

	switch(tt->type)
	   {
	   case RXP :
	   case RXN :
		n[0] =                           t[0] ;
		n[3] = t1*t[3] + t2*t[6] ;
		n[6] = t3*t[3] + t4*t[6] ;
		n[1] =                           t[1] ;
		n[4] = t1*t[4] + t2*t[7] ;
		n[7] = t3*t[4] + t4*t[7] ;
		n[2] =                           t[2] ;
		n[5] = t1*t[5] + t2*t[8] ;
		n[8] = t3*t[5] + t4*t[8] ;
		v[0] =                         vec[0] ;
		v[1] = t1*vec[1] + t2*vec[2] + tt->vt[0] ;
		v[2] = t3*vec[1] + t4*vec[2] + tt->vt[1] ;
		break ;
		
	   case RYP :
	   case RYN :
		n[0] = t1*t[0] + t2*t[6] ;
		n[3] =                           t[3] ;
		n[6] = t3*t[0] + t4*t[6] ;
		n[1] = t1*t[1] + t2*t[7] ;
		n[4] =                           t[4] ;
		n[7] = t3*t[1] + t4*t[7] ;
		n[2] = t1*t[2] + t2*t[8] ;
		n[5] =                           t[5] ;
		n[8] = t3*t[2] + t4*t[8] ;
		v[0] = t1*vec[0] + t2*vec[2] + tt->vt[0] ;
		v[1] =                         vec[1] ;
		v[2] = t3*vec[0] + t4*vec[2] + tt->vt[1] ;
		break ;
		
	   case RZP :
	   case RZN :
		/* Last line unchanged */
		n[0] = t1*t[0] + t2*t[3] ;
		n[3] = t3*t[0] + t4*t[3] ;
		n[6] =                           t[6] ;
		n[1] = t1*t[1] + t2*t[4] ;
		n[4] = t3*t[1] + t4*t[4] ;
		n[7] =                           t[7] ;
		n[2] = t1*t[2] + t2*t[5] ;
		n[5] = t3*t[2] + t4*t[5] ;
		n[8] =                           t[8] ;
		v[0] = t1*vec[0] + t2*vec[1] + tt->vt[0] ;
		v[1] = t3*vec[0] + t4*vec[1] + tt->vt[1] ;
		v[2] =                         vec[2] ;
		break ;
	   }
	for(j=0;j<9;j++) t[j] = n[j] ;
	for(j=0;j<3;j++) vec[j] = v[j] ;
	}
    else
	{
	tt->v += tt->dv ;
	switch(tt->type)
	   {
	   case TXP : vec[0] += tt->v ; break ;
	   case TXN : vec[0] -= tt->v ; break ;
	   case TYP : vec[1] += tt->v ; break ;
	   case TYN : vec[1] -= tt->v ; break ;
	   case TZP : vec[2] += tt->v ; break ;
	   case TZN : vec[2] -= tt->v ; break ;
	   }
	}
   tt->timelife-- ;
   }

b->depth += b->ddepth ;
if ( b->depth>b->fdepth )
	{
	b->depth = b->fdepth ;
	b->sdepth = b->depth ;
	b->ddepth = 0. ;
	}

j = 0 ;
for(i=0;i<b->nb;i++)
   {
   if ( b->t[i].timelife==0 )
	{
	if ( j==0 )
		traXtra( &b->t[i].final , &b->starting , &b->starting ) ;
	else
		{
		printf("Bug1 never here\n") ;
		}
	}
   else
   	{
   	if ( i!=j ) b->t[j] = b->t[i] ;
   	j++ ;
   	}
   }
b->nb = j ;
b->visual.vec[2] += b->depth ;

return(1) ;
}



void create_rotate(type,t,r,between,cx,cy,cz,dir)
int type ;
struct transfo *t ;
struct transfo *r ;
int between ;
float cx,cy,cz ;
int dir ;
{
struct transfo tmp ;


createtransfo(UNIT,&tmp, 0. ,-cx,-cy,-cz ) ; 

switch(type)
   {
   case ROTX :
              r->mat[0][0] = 1. ;
              r->mat[1][0] = r->mat[2][0] = r->mat[0][1] = r->mat[0][2] = 0. ;
              r->mat[1][1] = r->mat[2][2] = 0 ;
              r->mat[1][2] = -( r->mat[2][1] = dir ) ;
              break ;
   case ROTY :
              r->mat[1][1] = 1. ;
              r->mat[1][0] = r->mat[2][1] = r->mat[0][1] = r->mat[1][2] = 0. ;
              r->mat[0][0] = r->mat[2][2] = 0 ;
              r->mat[0][2] = -( r->mat[2][0] = -dir ) ;
              break ;
   case ROTZ :
              r->mat[2][2] = 1. ;
              r->mat[1][2] = r->mat[2][0] = r->mat[2][1] = r->mat[0][2] = 0. ;
              r->mat[1][1] = r->mat[0][0] = 0 ;
              r->mat[0][1] = -( r->mat[1][0] = dir ) ;
              break ;
   }
r->vec[0] = cx ;
r->vec[1] = cy ;
r->vec[2] = cz ;

createtransfo(type,t, M_PI/2./dir ,cx,cy,cz ) ;
traXtra(r,&tmp,r) ;

if ( between!=0 )
	{
	createtransfo(type,t, M_PI/2./between ,cx,cy,cz ) ; 
	traXtra(t,&tmp,t) ;
	}
else
	*t = *r ;

}


/* Createmove, create a movment, this one must be validate */
/* Be careful this function is very complex ..... */

void createmove(type,tt,between,b)
int type ;
struct time_transfo *tt ;
int between ;
struct movingbloc *b ;
{
int i,j,k ;
struct bloc *n,*o ;
float center[3] ;		/* Center in world coordinates */
float tmp[3] ;
struct transfo t ;
struct transfo *r ;

r = &tt->final ;

/* Find rotate center in starting piece in piece/world coordinates */

center[0] = b->b->dx/2. ;
center[1] = b->b->dy/2. ;
center[2] = b->b->dz/2. ;

/* Find current center in world coordinates */

traXvec( &b->real,center,center) ;

o = b->work ;

switch(type)
  {
  case RZP :
  case RZN :
    if ( o->dx%2 != o->dy%2 )
      {
	center[0] += (1-(o->dx%2))*(b->real.vec[0]>center[0]?0.5:-0.5) ;
	center[1] += (1-(o->dy%2))*(b->real.vec[1]>center[1]?0.5:-0.5) ;
      }
    n = allocbloc( o->dy, o->dx, o->dz ) ;
    break ;
  case RYP :
  case RYN :
    if ( o->dx%2 != o->dz%2 )
      {
	center[0] += (1-(o->dx%2))*(b->real.vec[0]>center[0]?0.5:-0.5) ;
	center[2] += (1-(o->dz%2))*(b->real.vec[2]>center[2]?0.5:-0.5) ;
      }
    n = allocbloc( o->dz, o->dy, o->dx ) ;
    break ;
  case RXP :
  case RXN :
    if ( o->dy%2 != o->dz%2 )
      {
	center[1] += (1-(o->dy%2))*(b->real.vec[1]>center[1]?0.5:-0.5) ;
	center[2] += (1-(o->dz%2))*(b->real.vec[2]>center[2]?0.5:-0.5) ;
      }
    n = allocbloc( o->dx, o->dz, o->dy ) ;
    break ;

  default : n=(struct bloc*)1994 ; /* Only to remove a compiler warning */
  }



tmp[0] = b->upleft[0] = b->tx - b->world->dx/2. ;
tmp[1] = b->upleft[1] = b->ty - b->world->dy/2. ;
tmp[2] = b->upleft[2] ;

/* Create transformation */

switch(type)
  {
  case RZP :
    create_rotate(ROTZ,&t,r,between,center[0],center[1],0.,1) ;
    tt->dt[0] = t.mat[0][0] ; tt->dt[1] = t.mat[0][1] ;
    tt->dt[2] = t.mat[1][0] ; tt->dt[3] = t.mat[1][1] ;
    tt->dvt[0] = t.vec[0] ; tt->dvt[1] = t.vec[1] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
	for(i=o->dx-1;i>=0;i--)
	  n->t[k][i][o->dy-1-j] = o->t[k][j][i] ;
    tmp[1] += o->dy ;
    break ;

  case RZN :
    create_rotate(ROTZ,&t,r,-between,center[0],center[1],0.,-1) ;
    tt->dt[0] = t.mat[0][0] ; tt->dt[1] = t.mat[0][1] ;
    tt->dt[2] = t.mat[1][0] ; tt->dt[3] = t.mat[1][1] ;
    tt->dvt[0] = t.vec[0] ; tt->dvt[1] = t.vec[1] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
	for(i=o->dx-1;i>=0;i--)
	  n->t[k][o->dx-1-i][j] = o->t[k][j][i] ;
    tmp[0] += o->dx ;
    break ;

  case RYN :
    create_rotate(ROTY,&t,r,-between,center[0],0.,center[2],-1) ;
    tt->dt[0] = t.mat[0][0] ; tt->dt[1] = t.mat[0][2] ;
    tt->dt[2] = t.mat[2][0] ; tt->dt[3] = t.mat[2][2] ;
    tt->dvt[0] = t.vec[0] ; tt->dvt[1] = t.vec[2] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
        for(i=o->dx-1;i>=0;i--)
	  n->t[i][j][o->dz-1-k] = o->t[k][j][i] ;
    tmp[2] += o->dz ;
    break ;

  case RYP : 
    create_rotate(ROTY,&t,r,between,center[0],0.,center[2],1) ;
    tt->dt[0] = t.mat[0][0] ; tt->dt[1] = t.mat[0][2] ;
    tt->dt[2] = t.mat[2][0] ; tt->dt[3] = t.mat[2][2] ;
    tt->dvt[0] = t.vec[0] ; tt->dvt[1] = t.vec[2] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
        for(i=o->dx-1;i>=0;i--)
          n->t[o->dx-1-i][j][k] = o->t[k][j][i] ;
    tmp[0] += o->dx ;
    break ;

  case RXN :
    create_rotate(ROTX,&t,r,-between,0.,center[1],center[2],-1) ;
    tt->dt[0] = t.mat[1][1] ; tt->dt[1] = t.mat[1][2] ;
    tt->dt[2] = t.mat[2][1] ; tt->dt[3] = t.mat[2][2] ;
    tt->dvt[0] = t.vec[1] ; tt->dvt[1] = t.vec[2] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
        for(i=o->dx-1;i>=0;i--)
	  n->t[o->dy-1-j][k][i] = o->t[k][j][i] ;
    tmp[1] += o->dy ;
    break ;

  case RXP : 
    create_rotate(ROTX,&t,r,between,0.,center[1],center[2],1) ;
    tt->dt[0] = t.mat[1][1] ; tt->dt[1] = t.mat[1][2] ;
    tt->dt[2] = t.mat[2][1] ; tt->dt[3] = t.mat[2][2] ;
    tt->dvt[0] = t.vec[1] ; tt->dvt[1] = t.vec[2] ;
    for(k=o->dz-1;k>=0;k--)
      for(j=o->dy-1;j>=0;j--)
        for(i=o->dx-1;i>=0;i--)
	  n->t[j][o->dz-1-k][i] = o->t[k][j][i] ;
    tmp[2] += o->dz ;
    break ;

  case TXP : createtransfo(UNIT,r,0. , 1., 0., 0. ) ; b->tx++ ; break ;
  case TXN : createtransfo(UNIT,r,0. ,-1., 0., 0. ) ; b->tx-- ; break ;
  case TYP : createtransfo(UNIT,r,0. , 0., 1., 0. ) ; b->ty++ ; break ;
  case TYN : createtransfo(UNIT,r,0. , 0.,-1., 0. ) ; b->ty-- ; break ;
  case TZP : createtransfo(UNIT,r,0. , 0., 0., 1. ) ; b->tz++ ; break ;
  case TZN : createtransfo(UNIT,r,0. , 0., 0.,-1. ) ; b->tz-- ; break ;
  }

if ( type>=RZP && type<=RXN )
  {
  traXvec( r,tmp,tmp) ;
  b->tx += PROX(tmp[0]-b->upleft[0]) ;
  b->ty += PROX(tmp[1]-b->upleft[1]) ;
  b->tz += PROX(tmp[2]-b->upleft[2]) ;
  b->upleft[0] = tmp[0] ;
  b->upleft[1] = tmp[1] ;
  b->upleft[2] = tmp[2] ;
  tt->t[0] = tt->t[3] = 1. ; tt->t[1] = tt->t[2] = 0. ;
  tt->vt[0] = tt->vt[1] = 0. ;
  freebloc(o) ;
  b->work = n ;
  }
else
  {
  tt->v    = 0. ;		/* Start value : no translation */
  if ( between ) tt->dv   = 1./between ;	/* Delta v */
  }
tt->type = type ;

}



void newfallingbloc( b,bn )
struct movingbloc *b ;
int bn ;
{
struct transfo t1 ;
int min ;
int next ;
struct bloc *nb ;

min = b->world->dx<b->world->dy ? b->world->dx : b->world->dy ;
do
	{
	if ( bn>=0 ) next = bn ;
	    else
		switch( b->typepiece )
			{
			case FLAT    : next = rand()%b->flat    ; break ;
			case SIMPLE  : next = rand()%b->simple  ; break ;
			case COMPLEX : next = rand()%b->complex ; break ;
			default      : next = 1994 ;
					/* Only to remove a compiler warning */
				       fprintf(stderr,"Problem with newfall\n");
			}
	}
while( b->piece[next]->dx>min || b->piece[next]->dy>min ) ;

if ( b->thisone==-1 ) /* First bloc */
	{
	b->thisone = next ;
	b->statpiece[b->thisone]++ ;
	return ;
	}

nb = b->piece[b->thisone] ;
b->thisone = next ;
b->statpiece[b->thisone]++ ;

b->b = nb ;

t1 = b->tworld ;
initmovingbloc(b,&t1) ;

createfaces(nb) ;
}



void printmoving(b)
struct movingbloc *b ;
{
int i ;

printf("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n") ;
printf("tx(%d,%d,%d) world(%d,%d,%d) bloc(%d,%d,%d) currentbloc(%d,%d,%d)\n",
b->tx,b->ty,b->tz,b->world->dx,b->world->dy,b->world->dz,
b->b->dx,b->b->dy,b->b->dz,
b->work->dx,b->work->dy,b->work->dz) ;
printf("Real position :\n");
printtra(&b->real) ;
printf("Accumulate trasnformation :\n");
printtra(&b->starting) ;
printf("visual trasnformation :\n");
printtra(&b->visual) ;
printf("nb=%d\n",b->nb) ;
for(i=0;i<b->nb;i++)
	{
	printf("   type %d timelife %d\n",b->t[i].type,b->t[i].timelife) ;
	}
printf("sdepth %g depth %g fdepth %g    ddepth %g\n",
	b->sdepth,b->depth,b->fdepth,b->ddepth) ;
printf("upleft %g %g %g\n",b->upleft[0],b->upleft[1],b->upleft[2]) ;
printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n") ;
}

