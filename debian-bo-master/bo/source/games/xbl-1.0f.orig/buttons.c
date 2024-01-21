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
#include "buttons.h"
#include "keyid.h"

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "score.h"

#define RET(X) if ( strcmp(b->text->current_text,X)!=0 ) \
			{ b->text->x = -1 ; \
	       		  strcpy(b->text->current_text,X) ; \
			} \
		return

/**************************************************************************/
/**************************************************************************/
void displaystairs( m )
struct menu *m ;
{
int i ;
int nb ;

nb = depthbloc( m->bl->bloc.world ) ;

for(i=0;i<m->bl->opt.wz;i++)
	{
	XDrawRectangle(m->display,m->window,m->white,
				   0,i*m->layersize,
				m->layersize-2,m->layersize-2) ;
	XFillRectangle(m->display,m->window,i<nb?m->bl->x.face[i%6]:m->black,
				   1,i*m->layersize+1,
				m->layersize-4,m->layersize-4) ;
	}
XFillRectangle(m->display,m->window,m->flat,
				   0,i*m->layersize,
				   m->layersize,(18-i)*m->layersize) ;
/* 18 is max depth */
}
/**************************************************************************/
/**************************************************************************/
void fctland(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.land = (b->menu->bl->opt.land+but+3)%3 ;
switch( b->menu->bl->opt.land )
	{
	case USER : 
		    b->menu->width->typet = RELIEF_TEXT ;
		    b->menu->height->typet = RELIEF_TEXT ;
		    b->menu->depth->typet = RELIEF_TEXT ;
		    break ;
	case SMALL:
		    b->menu->bl->opt.wx = 3 ; 
		    b->menu->bl->opt.wy = 3 ; 
		    b->menu->bl->opt.wz = 15 ; 
		    break ;
	case BIG  : 
		    b->menu->bl->opt.wx = 5 ; 
		    b->menu->bl->opt.wy = 5 ; 
		    b->menu->bl->opt.wz = 10 ; 
		    break ;
	}
if ( b->menu->bl->opt.land!=USER )
  {
    b->menu->width->typet = FLAT_TEXT ;
    b->menu->height->typet = FLAT_TEXT ;
    b->menu->depth->typet = FLAT_TEXT ;
    b->menu->width->height = 0 ;
    b->menu->height->height = 0 ;
    b->menu->depth->height = 0 ;
    newworld(b->menu->bl) ;
  }

displaystairs( b->menu ) ;

display_button(b->menu->width,
	       b->menu->width->x,
	       b->menu->width->y,
	       1,1) ;
display_button(b->menu->height,
	       b->menu->height->x,
	       b->menu->height->y,
	       1,1) ;
display_button(b->menu->depth,
	       b->menu->depth->x,
	       b->menu->depth->y,
	       1,1) ;
setargs(b->menu->bl) ;
}
void textland(b) 
struct moving_button *b ;
{
switch( b->menu->bl->opt.land )
	{
	case USER : RET(USER_LAND) ;
	case SMALL: RET(SMALL_LAND) ;
	case BIG  : RET(BIG_LAND) ;
	default   : RET("Bug land") ;
	}
}
/**************************************************************************/
/**************************************************************************/
void fcttype(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->bloc.typepiece = (b->menu->bl->bloc.typepiece+but+3)%3 ;
/* display_button(b) ; */
b->menu->bl->bloc.thisone = -1 ;
newfallingbloc(&b->menu->bl->bloc,-1);
displaynextpiece( b->menu->bl ) ;
drawscores( b->menu->bl,b->menu->showscore ) ;
displayzoo( b->menu->bl,1,b->menu->showzoo ) ;
setargs(b->menu->bl) ;
}
void texttype(b) 
struct moving_button *b ;
{
switch( b->menu->bl->bloc.typepiece )
	{
	case FLAT   : RET(FLAT_PIECE) ;
	case SIMPLE : RET(SIMPLE_PIECE) ;
	case COMPLEX: RET(COMPLEX_PIECE) ;
	default     : RET("Bug Pieces") ;
	}
/* setargs(b->menu->bl) ; */
}
/**************************************************************************/
/**************************************************************************/
void textstate(b) 
struct moving_button *b ;
{
switch( b->menu->bl->opt.state )
	{
	case STOP   : RET(STOPPED_GAME) ;
	case RUN    : RET(RUNNING_GAME) ;
	case SUSPEND: RET(SUSPENDED_GAME) ;
	case SHOW   : RET(SHOW_GAME) ;
	case DEMO   : RET(DEMO_GAME) ;
	default     : RET("Bug state") ;
	}
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fcttraining(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.mode = 1 - b->menu->bl->opt.mode ;
setargs(b->menu->bl) ;
}
void texttraining(b) 
struct moving_button *b ;
{
switch( b->menu->bl->opt.mode )
	{
	case 0 : RET(NORMAL_PLAY) ;
	case 1 : RET(TRAINING_PLAY) ;
	default: RET("Bug training") ;
	}
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctdraw(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.drawmode = 2 - b->menu->bl->opt.drawmode ;
displaynextpiece( b->menu->bl ) ;
displayzoo( b->menu->bl,1,b->menu->showzoo ) ;
setargs(b->menu->bl) ;
}
void textdraw(b) 
struct moving_button *b ;
{
switch( b->menu->bl->opt.drawmode )
	{
	case 2 : RET(WIRE_DRAW) ;
	case 0 : RET(TRANSP_DRAW) ;
	default: RET("Bug training") ;
	}
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctsmooth(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.smooth = 1 - b->menu->bl->opt.smooth ;
if ( b->menu->bl->opt.state==RUN )
	{
	displaymoving(&b->menu->bl->opt,&b->menu->bl->x,
			&b->menu->bl->bloc,&b->menu->bl->draw) ;
	}
setargs(b->menu->bl) ;
}
void textsmooth(b) 
struct moving_button *b ;
{
switch( b->menu->bl->opt.smooth )
	{
	case 1 : RET(HARD_DRAW) ;
	case 0 : RET(SMOOTH_DRAW) ;
	default: RET("Bug smooth") ;
	}
}
/**************************************************************************/
/**************************************************************************/
void fctwidth(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.wx = ( b->menu->bl->opt.wx-2*3+but+9)%(9-3) +3 ;
newworld(b->menu->bl) ;
setargs(b->menu->bl) ;
}
void textwidth(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"Width %d",b->menu->bl->opt.wx) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void fctheight(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.wy = ( b->menu->bl->opt.wy-2*3+but+9)%(9-3) +3 ;
newworld(b->menu->bl) ;
setargs(b->menu->bl) ;
}
void textheight(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"Height %d",b->menu->bl->opt.wy) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void fctdepth(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.wz = ( b->menu->bl->opt.wz-2*6+but+19)%(19-6) +6 ;
displaystairs( b->menu ) ;
newworld(b->menu->bl) ;
setargs(b->menu->bl) ;
}
void textdepth(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"Depth %d",b->menu->bl->opt.wz) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void fctlevel(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.level = ( b->menu->bl->opt.level+but+11)%11 ;
setargs(b->menu->bl) ;
}
void textlevel(b) 
struct moving_button *b ;
{
sprintf(b->text->next->current_text,"%d",b->menu->bl->currentlevel) ;
b->text->next->x = -1 ;
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctquit(b,but)
struct moving_button *b ;
int but ;
{
b->menu->bl->endplay = 1 ;
setargs(b->menu->bl) ;
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctcopyright(b,but)
struct moving_button *b ;
int but ;
{
char buf[LINE_LENGTH] ;
FILE *f ;

XFlush(b->menu->bl->x.display) ;
sprintf(buf,"%s/COPYING",SCOREDIR) ;
f = fopen( buf,"r" ) ;

if ( f==0 )
	{
	fprintf(stderr,"Please install copyright file %s\n",buf) ;
	return ;
	}

do
	{
	fgets(buf,LINE_LENGTH-1,f) ;
	if ( feof(f) ) break ;
	printf("%s",buf) ;
	}
while(!feof(f)) ;
fclose(f) ;
}
/**************************************************************************/
/**************************************************************************/
void fctvolume(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.volume = (b->menu->bl->opt.volume+but+11)%11 ;
setargs(b->menu->bl) ;
}
void textvolume(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"Sound volume %d",b->menu->bl->opt.volume) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctnextpiece(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->bloc.nextpiece = 1 - b->menu->bl->bloc.nextpiece ;
/* displaynextpiece( b->menu->bl ) ; */
setargs(b->menu->bl) ;
}
void textnextpiece(b) 
struct moving_button *b ;
{

switch( b->menu->bl->bloc.nextpiece )
	{
	case 0 :
		b->text->x = -1 ;
		strcpy(b->text->current_text,"Click me") ;
		b->text->next->x = -1 ;
		strcpy(b->text->next->current_text,"to see") ;
		b->text->next->next->x = -1 ;
		strcpy(b->text->next->next->current_text,"next piece") ;
		return ;
	case 1 : 
		b->text->current_text[0] = 0 ;
		b->text->next->current_text[0] = 0 ;
		b->text->next->next->current_text[0] = 0 ;
		if ( b->height==0 )
			displaynextpiece( b->menu->bl ) ;
		return ;
	default   : return ;
	}
}

void displaynextpiece(bl)
struct bl *bl ;
{
int xmin,ymin,xmax,ymax ;
struct transfo id ;
struct point min,max ;
struct viewtransfo v ;

if ( bl->bloc.nextpiece && bl->bloc.thisone!=-1 )
	{
	/* display_button( bl->menu.nextpiece ) ; */
	xmin = bl->menu.nextpiece->x+bl->menu.nextpiece->dz+1 ;
	ymin = bl->menu.nextpiece->y+bl->menu.nextpiece->dz+1 ;
	xmax = xmin+bl->menu.nextpiece->dx-2*(bl->menu.nextpiece->dz+1) ;
	ymax = ymin+bl->menu.nextpiece->dy-2*(bl->menu.nextpiece->dz+1) ;

	XFillRectangle(bl->x.display,bl->menu.window,
			bl->menu.nextpiece->gc->back,
			xmin,ymin,xmax-xmin-1,ymax-ymin-1) ;

	v.xcenter = (xmin+xmax)/2 ;
	v.ycenter = (ymin+ymax)/2 ;
	v.xprod   = (xmax-xmin)-4 ;
	v.yprod   = (ymax-ymin)-4 ;

	createtransfo(UNIT,&id,0.,-2.5,-2.5,5.) ;

	if ( bl->opt.drawmode==2 )
		{
		createfaces( bl->bloc.piece[bl->bloc.thisone] ) ;
		drawtranspbloc( bl->x.display , bl->menu.window ,
				bl->x.transp ,
				bl->x.white, bl->bloc.piece[bl->bloc.thisone],
				&id , &v , &min,&max ) ;
		}
	else
		{
		drawlinebloc( bl->x.display , bl->menu.window ,
		              bl->x.white , bl->bloc.piece[bl->bloc.thisone] ,
			      &id , &v , &min,&max ) ;
		}
	}
}
/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctzoo(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->showzoo = 1 - b->menu->showzoo ;
if ( b->menu->showzoo==0 )
	XUnmapWindow( b->menu->display , b->menu->zoo ) ;
	else
	XMapWindow( b->menu->display , b->menu->zoo ) ;
setargs(b->menu->bl) ;
}
void textzoo(b) 
struct moving_button *b ;
{
switch( b->menu->showzoo )
	{
	case 0 : RET(SHOW_ZOO) ;
	case 1 : RET(HIDE_ZOO) ;
	default: RET("Bug zoo") ;
	}
}

/**************************************************************************/
/**************************************************************************/
/*ARGSUSED*/
void fctscore(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->showscore = 1 - b->menu->showscore ;
if ( b->menu->showscore==0 )
	XUnmapWindow( b->menu->display , b->menu->bl->x.wscore ) ;
	else
	XMapWindow( b->menu->display , b->menu->bl->x.wscore ) ;
setargs(b->menu->bl) ;
}
void textscore(b) 
struct moving_button *b ;
{
switch( b->menu->showscore )
	{
	case 0 : RET(SHOW_SCORE) ;
	case 1 : RET(HIDE_SCORE) ;
	default: RET("Bug score") ;
	}
}
/**************************************************************************/
/**************************************************************************/
void textframe(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;

if ( b->menu->bl->opt.state != STOP )
    sprintf(buf,"Frame/Sec %d",(int)(1./b->menu->bl->realtime.displaytime)) ;
else
    sprintf(buf,"%ds. before demo",b->menu->bl->opt.Time_to_demo) ;

RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void fctkey(b,but) 
struct moving_button *b ;
int but ;
{
b->menu->bl->opt.keyboard = (b->menu->bl->opt.keyboard+but+NB_KEYBOARD)
				%NB_KEYBOARD ;

b->menu->bl->key = b->menu->bl->boardkey[b->menu->bl->opt.keyboard] ;
createhelp(b,b->menu->bl->key) ;
setargs(b->menu->bl) ;
}
void textkey(b) 
struct moving_button *b ;
{
void displayhelp() ;

displayhelp(b) ;
switch( b->menu->bl->opt.keyboard )
	{
	case 0 : RET("User defined key") ;
	case 1 : RET("AZERTY |") ;
	case 2 : RET("QWERTY |") ;
	case 3 : RET("AZERTY -") ;
	case 4 : RET("QWERTY -") ;
	case 5 : RET("SPATIAL ") ;
	default: RET("Bug key") ;
	}
}

void displayhelp(b)
struct moving_button *b ;
{
struct menu *m ;
int i ;
char buf[2] ;
int ascent,descent,dir ;
XCharStruct overall_return ;
int x,y ;			/* Position of help bitmap */

m = b->menu ;
x = m->key->x+m->key->dz+m->key->margin+1 ;
y = m->key->y+m->key->dz+3*(m->char_height+1)+2 ;

if ( m->key->height == 0 && m->window!=0 && m->helpkey!=0 )
     {
     XCopyPlane( m->bl->x.display,
                m->helpkey,     m->window,
                m->white,
                0,0,m->widthhelpkey,m->heighthelpkey,
		x,y,1 ) ;
     }


buf[1] = 0 ;
for(i=0;i<=KEY_ROT_Z_NEG;i++)
   {
   buf[0] = m->bl->key[i] ;
 
   XTextExtents(b->text->xfont,buf,(int)strlen(buf),
        &dir,&ascent,&descent,&overall_return) ;

   XDrawString(b->display,b->window,b->text->gc,
	       m->tx[i]+x- overall_return.width/2 ,
	       m->ty[i]+y+ (overall_return.ascent-overall_return.descent)/2 ,
	       buf,(int)strlen(buf)) ;

   }

}
/**************************************************************************/
/**************************************************************************/
void texthiscore(b) 
struct moving_button *b ;
{
sprintf(b->text->next->current_text,"%d",b->menu->bl->hiscore) ;
b->text->next->x = -1 ;
}
/**************************************************************************/
/**************************************************************************/
void textcube(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"#cubes %d",b->menu->bl->nbcube) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void textbloc(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"#blocs %d",b->menu->bl->nbbloc) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void textdlevel(b) 
struct moving_button *b ;
{
sprintf(b->text->next->current_text,"%d",b->menu->bl->nblevel) ;
b->text->next->x = -1 ;
}
/**************************************************************************/
/**************************************************************************/
void textstartlevel(b) 
struct moving_button *b ;
{
static char buf[LINE_LENGTH] ;
sprintf(buf,"Start level %d",b->menu->bl->opt.level) ;
RET(buf) ;
}
/**************************************************************************/
/**************************************************************************/
void textthescore(b) 
struct moving_button *b ;
{
sprintf(b->text->next->current_text,"%d",b->menu->bl->score) ;
b->text->next->x = -1 ;
}
/**************************************************************************/
/**************************************************************************/
