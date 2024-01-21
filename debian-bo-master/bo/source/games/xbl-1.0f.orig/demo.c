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

/**********************************************/
/* To change demo : change function deep_fall */
/**********************************************/

#include "bl.h"

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#ifndef abs
#define abs(X) ( (X)>0 ? (X) : -(X) )
#endif

#define KABOOM -1000000000

/***********************************************************************/

static struct bloc *w=0 ;

void save_world(b)
struct bloc *b ;
{
if ( w==0 ) w = allocbloc(b->dx,b->dy,b->dz+1) ;
if ( w->dx != b->dx ||
     w->dy != b->dy ||
     w->dz != b->dz+1 )
	{
	freebloc(w) ;
	w = 0 ;
	save_world(b) ;
	}

memcpy( &w->t[0][0][0] , &b->t[0][0][0] ,
	 b->dx*b->dy*b->dz*sizeof(b->t[0][0][0]) ) ;
}

void restore_world(b)
struct bloc *b ;
{
memcpy( &b->t[0][0][0] , &w->t[0][0][0] ,
	 b->dx*b->dy*b->dz*sizeof(b->t[0][0][0]) ) ;
}

/*----------------------------------------------------------------------*/
/* Compute the value of the world          				*/
/* If you want to change heuristic, change this function		*/
/*----------------------------------------------------------------------*/

int deep_fall(b,gk)
struct movingbloc *b ;
int *gk ;
{
int i,j,k,l,m,nb ;
int stz ;
int score ; 
char *t ;
int dx,dy,dz ;
char *height,**theight,*tend ;
static int powtwo[20] = { 0 } ;

if ( powtwo[0]==0 )
	{
	powtwo[0] = 10 ;
	for( i=1 ; i<sizeof(powtwo)/sizeof(powtwo[0]) ; i++ )
		powtwo[i] = powtwo[i-1]*1.5 ;
	}

save_world( b->world ) ;
stz = b->tz ;

while( intersection(b,b->world)==0 ) b->tz++ ; /* Fall */
if ( b->tz==stz ) return(KABOOM) ;
b->tz-- ;
*gk = b->tz ;
delete_level(b,b->world) ;

dx = b->world->dx ;
dy = b->world->dy ;
dz = b->world->dz ;
nb = dx*dy ;

score = 0 ;

/* Compute weight and height of the world */
/* Also take into account the hole */

height = &w->t[dz][0][0] ;
for(i=0;i<nb;i++) *height++ = 0 ;

for(k=1;k<dz;k++)
   {
   /*------------------------------------*/
   /* Next value is the weight of a bloc */
   /* A high bloc is a bad thing         */
   /*------------------------------------*/
   l = powtwo[dz - k + 1] ;
   /*------------------------------------*/
   /* Next value is the score of a hole  */
   /* a hole is under some fill bloc     */
   /* More bloc above : a bad thing      */
   /* Linear function the number of above*/
   /* blocs                              */
   /*------------------------------------*/
   m = l*nb ;

   tend = &b->world->t[k][0][0]+nb ;
   height = &w->t[dz][0][0] ;
   for( t = &b->world->t[k][0][0] ; t<tend ; t++ )
	{
	if ( *t )
		{
		score -= l ;
		if ( *height==0 )
			*height = k ;
		}
	else	if ( *(t-nb) ) 
			score -= m*(k-*height) ;
	height++ ;
	}
   }

/* Set height of empty columns */

height = &w->t[dz][0][0] ;
for(i=0;i<nb;i++) 
	{
	if ( *height==0 ) *height = dz ;
	height++ ;
	}

/*-------------------------------------*/
/* Compute smoothness of the top world */
/* It's the absolute sum of weight of  */
/* top cubes. So deep hole are avoided */
/*-------------------------------------*/

l = 0 ;

theight = w->t[dz] ;
for(i=0;i<dx;i++)
   for(j=1;j<dy;j++)
	{
	l += powtwo[ abs( theight[j][i]-theight[j-1][i] ) ] ;
	}

for(i=1;i<dx;i++)
   for(j=0;j<dy;j++)
	{
	l += powtwo[ abs( theight[j][i]-theight[j][i-1] ) ] ;
	}
score -= l/4 ;

/*----------------------------------*/
/* Push the piece down right        */
/* And try to avoid very top pieces */
/*----------------------------------*/

m = powtwo[dz]*nb*nb ;
l = 0 ;

for(i=0;i<dx;i++)
   for(j=0;j<dy;j++)
	{
	switch( theight[j][i] )
		{
		case 0 : l -= 8*m ;
			 break ;
		case 1 : l -= 6*m ;
			 break ;
		case 2 : l -= 2*m ;
			 break ;
		default : l += (dz-theight[j][i])*i*i*j*j ;
		}
	}
score += (16*l)/nb/nb ;

b->tz = stz ;
restore_world( b->world ) ;
return(score) ;
}

/*----------------------------------------------------------------------*/
/* Try to drop the bloc at various position				*/
/*----------------------------------------------------------------------*/

#define DO_TEST \
		{ \
		k = deep_fall(b,&ggk) ; \
		if ( k==KABOOM ) break ; \
		if ( k>gdeep ) \
			{ \
			gdeep = k ; \
			*gi = b->tx-stx ; \
			*gj = b->ty-sty ; \
			*gk = ggk ; \
			} \
		}


int trans_deep(b,gi,gj,gk)
struct movingbloc *b ;
int *gi,*gj,*gk ;
{
int k,gdeep,ggk,stx,sty ;

stx = b->tx ;
sty = b->ty ;
gdeep = deep_fall(b,&ggk) ;
*gi = 0 ;
*gj = 0 ;
*gk = ggk ;

for(;b->tx<=b->world->dx-b->work->dx;b->tx++) /* Go to the right */
	{
	/* Go down (jump to the case below central one because
	   it is yet tested */
	for(b->ty=sty+(b->tx==stx);b->ty<=b->world->dy-b->work->dy;b->ty++)
		DO_TEST ;
	/* Test up */
	for(b->ty=sty-1;b->ty>=0;b->ty--)
		DO_TEST ;
	}
for(b->tx=stx-1;b->tx>=0;b->tx--) /* Go to the left */
	{
	/* Go down */
	for(b->ty=sty;b->ty<=b->world->dy-b->work->dy;b->ty++)
		DO_TEST ;
	/* Test up */
	for(b->ty=sty-1;b->ty>=0;b->ty--)
		DO_TEST ;
	}

b->tx = stx ;
b->ty = sty ;
return(gdeep) ;
}


void demo(bl)
struct bl *bl ;
{
int i,j,k ;
int gi,gj,gk,gdeep ;
int m ;
int sr ;
static int t,r ;
static int mgi,mgj,mgk,mgdeep ;
static struct movingbloc *b ;
static int last_score ;
#define TRY 200
#define PULL 201
#define STOPTRY 202
static int try[] = { RZP,TRY,RZP,TRY,PULL,RZN,TRY,
			PULL,RXP,TRY,RZP,TRY,RZP,TRY,RZP,TRY,	/* 7 */
			PULL,RXN,TRY,RZP,TRY,RZP,TRY,RZP,TRY,	/* 16 */
			PULL,RYP,TRY,RZP,TRY,RZP,TRY,RZP,TRY,	/* 25 */
			PULL,RYN,TRY,RZP,TRY,RZP,TRY,RZP,TRY,	/* 34 */		
			PULL,RYP,RYP,TRY,RZP,TRY,RZP,TRY,RZP,TRY, /* 43 */
			STOPTRY } ;

if ( bl == 0 )
    	{
	last_score = -1 ;
	return ;
	}

if ( last_score!=bl->score )
	{
	last_score = bl->score ;
	t = -1 ;
	}

m = 3*BETWEEN ;

if ( t==-2 )
	{
	static int tt[] = {TXP,TXN,TYP,TYN,RXP,RZP,RYP,RXN,RYN,RZN} ;

	/* Piece is down, now try to translate or rotate it to remove hole */
	/* Or to best place it (end of game) */

	/* I recompute best deep
	   I do so because the piece fall and then, it possible I can't
	   go to the best solution I found.
	   and for the nest stage I need to know the correct value
	*/ 
	mgdeep = deep_fall(&bl->bloc,&i) ; 

	r = -1 ;
	for( i=0 ; i<sizeof(tt)/sizeof(tt[0]) ; i++ )	/* For last sec. transformations */
	   {
	   push_moving_bloc(b) ;			/* Save moving */
	   if ( addtransfo(tt[i],0,&bl->bloc,1) )	/* Try transformation */
		{
		while( addtransfo(TZP,0,&bl->bloc,1) ) ;
		gdeep = deep_fall(&bl->bloc,&gk) ;		/* OK, look value */
		if ( gdeep>mgdeep )			/* Best than previous */
			{
			mgdeep = gdeep ;
			r = i ;
			}
		}
	   pull_moving_bloc(b) ;
	   }
	if ( r!=-1 )
		{
		/* Add this one */
		addtransfo(tt[r],m,&bl->bloc,m) ;/* Add and quit */
		while( addtransfo(TZP,0,&bl->bloc,1) ) ;
		return ;
		}

	bl->realtime.lastfall = bl->realtime.lrtime+m/BETWEEN ;
	t = -3 ; /* Do not try any more thing before fall */
	}

if ( t==-3 ) return ;

if ( t == -1 )
	{
	b = &bl->bloc ;
	mgdeep = trans_deep(b,&mgi,&mgj,&mgk) ;
	r = 0 ;
	}

push_moving_bloc(b) ;

while( try[++t]!=STOPTRY )
   switch(try[t])
   	{
   	case PULL : 
   		pull_moving_bloc(b) ;
		return ;
   	case TRY :
   		gdeep = trans_deep(b,&gi,&gj,&gk) ;
   		if ( gdeep>mgdeep )
   			{
   			mgdeep=gdeep ;
   			mgi=gi ;
   			mgj=gj ;
   			mgk=gk ;
   			r = t ;
   			}
   		break ;
   	default :
   		if ( !addtransfo(try[t],0,b,1) )
			{
			/* No more try */
			while( try[t]!=PULL && try[t]!=STOPTRY ) t++ ;
			t-- ;
			}
		break ;
   	}

pull_moving_bloc(b) ;


sr = r ;
if ( r!=0 )
	{
	while( r>=0 && try[r]!=PULL ) r-- ;
	r++ ;
	do
		{
		if ( try[r]!=TRY )
			addtransfo(try[r],m,&bl->bloc,m) ;
		}
	while( r++<sr ) ;
	}

for(i=0;i<mgi;i++)
	addtransfo(TXP,m,&bl->bloc,m) ;
for(i=0;i>mgi;i--)
	addtransfo(TXN,m,&bl->bloc,m) ;
for(j=0;j<mgj;j++)
	addtransfo(TYP,m,&bl->bloc,m) ;
for(j=0;j>mgj;j--)
	addtransfo(TYN,m,&bl->bloc,m) ;


for(k=0;k<mgk;k++)
	addtransfo(FALL,m,&bl->bloc,m) ;


t = -2 ;

}


