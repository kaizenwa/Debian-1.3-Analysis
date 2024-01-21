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
#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include <time.h>

#include "define.h"

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "bl.h"
#include "score.h"

extern int errno ;

#define MAXSCORE 25	/* Number of saved score */
#define MAXNAME  30	/* Len a a user name */
#define WIDTH 80
#define MAXFILENAME 256	/* Maximum len of the score file name */
#define LINESPACE 1
#define MAXLINE		"123456789#cubes9#blocs9#level99999score9Tue9Jan995914:58:3891993000000"


struct score
	{
	char name[MAXNAME+1] ;
	int nbcubes,nbblocs,score,nblevel ;
	time_t date ;
	} ;

struct scores
	{
	int nb ;
	time_t modify_time ;
	char last_file_name[MAXFILENAME] ;
	struct score score[MAXSCORE+1] ;
	} ;

/* Next variables are initialised in scorewin */
static Display *disp ;
static Window   wind ;
static int	heig ;
static GC	scgc ;
static XFontStruct *xfon ;

/**********************************************************************/
/* Find the file name of score file */
/**********************************************************************/
void scorename(x,y,z,p,buf)
int x,y,z,p ;
char *buf ;
{
sprintf(buf,"%s/%d-%d-%d-%d",SCOREDIR,x,y,z,p) ;
}

/**********************************************************************/
/* Read the score file */
/**********************************************************************/
int readscore(x,y,z,p,s)
int x,y,z,p ;
struct scores *s ;
{
FILE *f ;
char buf[MAXFILENAME] ;
struct score *cs ;
struct stat st ;

scorename(x,y,z,p,buf) ;

if( stat(buf,&st) )
   switch( errno )
	{
	default :
		perror("readscore") ;
	case ENOENT :
		s->nb = 0 ;
		strcpy(s->last_file_name,buf) ;
		return(0) ;
	}

if ( strcmp(buf,s->last_file_name) == 0 )
	{
	/* Same file !!! */
	if ( st.st_mtime==s->modify_time ) return(s->score[0].score) ;
	}
else
	{
	strcpy(s->last_file_name,buf) ;
	s->nb = 0 ;
        }

s->modify_time = st.st_mtime ;

f = fopen( buf,"r" ) ;
if ( f==0 )
	{
	if ( errno==EAGAIN )
		{
		microsleep(100000) ;
		return(readscore(x,y,z,p,s)) ;
		}
	return(0) ;
	}

cs = s->score ;
s->nb = 0 ;
while( fscanf(f,"%s%d%d%d%d%ld",cs->name,&cs->nbcubes,&cs->nbblocs,
				&cs->nblevel,&cs->score,&cs->date)==6 )
	{
	cs++ ;
	s->nb++ ;
	if ( s->nb==MAXSCORE ) break ;
	}

fclose(f) ;

return( s->score[0].score ) ;
}

/**********************************************************************/
/* Display the scores (read them) */
/**********************************************************************/
int displayscore(x,y,z,p,draw)
int x,y,z,p ;
int draw ;
{
static struct scores s = {-1} ;
char buf[LINE_LENGTH] ;
int i,yy ;
int hiscore ;
int ilast ;
time_t last ;
int ch ;
int ascent,descent,dir ;
XCharStruct overall_return ;

if ( s.nb==-1 ) s.last_file_name[0] = '\0' ;

hiscore = readscore(x,y,z,p,&s) ;

if ( draw==0 ) return(hiscore) ;

XClearWindow(disp,wind) ;

ch = heig+LINESPACE ;

yy = ch ;
sprintf(buf,"Width=%d Height=%d Depth=%d Pieces=%s",
		x,y,z,p==FLAT?"FLAT":p==SIMPLE?"SIMPLE":"COMPLEX") ;
XDrawString(disp,wind,scgc,3,yy,buf,(int)strlen(buf)) ; yy+=ch ;

yy += ch ;

sprintf(buf,"  Name   #cubes #blocs #level     score") ;
XDrawString(disp,wind,scgc,3,yy,buf,(int)strlen(buf)) ; yy+=ch ;

last = 0 ;
ilast = -1 ;
for(i=0;i<s.nb;i++) if ( s.score[i].date>last )
			{
			last = s.score[i].date ;
			ilast = i ;
			}

for(i=0;i<s.nb;i++)
	{
	if ( ilast==i ) yy++ ;
	sprintf(buf,"%8.8s %6d %6d %6d %9d %s",
			s.score[i].name,s.score[i].nbcubes,s.score[i].nbblocs,
			s.score[i].nblevel,s.score[i].score,
			ctime(&s.score[i].date) ) ;
	XDrawString(disp,wind,scgc,3,yy,buf,(int)(strlen(buf)-1)) ; yy+=ch ;
	if ( ilast==i )
		{
		XTextExtents(xfon,buf,strlen(buf),
                        &dir,&ascent,&descent,&overall_return) ;
		XDrawRectangle(disp,wind,scgc,1,yy-2*ch+LINESPACE+1,
				overall_return.width+4,ch+1) ;
		yy += 2 ;
		}
	}

return(hiscore) ;
}

/**********************************************************************/
/* Look if a name is good */
/* Eliminate NOBODY, UNKNOWN */
/**********************************************************************/
int tryname(buffer,name)
char *buffer ;		/* Where to stock the name */
char *name ;		/* Possible name */
{
char *tmp ;

if ( name==0 ) return(1) ;
if ( name[0]==0 ) return(1) ;

tmp = buffer ;
do {
	if ( *name>='A' && *name<='Z' ) *tmp++ = *name++ - 'A' + 'a' ;
				else	*tmp++ = *name++ ;
   }
while( *name!=0 || tmp-buffer == MAXNAME-1 ) ;
*tmp = '\0' ;

if ( strcmp(buffer,"nobody")==0 ) return(1) ;
if ( strcmp(buffer,"unknown")==0 ) return(1) ;
return(0) ;
}


/**********************************************************************/
/* Add a new score !!!! */
/**********************************************************************/
int addscore(x,y,z,p,nbcubes,nbblocs,nblevel,score)
int x,y,z,p ;
int nbcubes,nbblocs,nblevel,score ;
{
struct scores s ;
FILE *f ;
int fd ;
int i,j ;
char name[LINE_LENGTH] ;
char buf[MAXFILENAME] ;
int inscore ;

if ( score==0 ) return(0) ;
s.last_file_name[0] = '\0' ;

umask(0111) ;
(void)readscore(x,y,z,p,&s) ;
scorename(x,y,z,p,buf) ;
if ( s.nb==0 )
#ifdef USE_SETGID
	{
	 fd = creat(buf,(mode_t)0664) ;
	 chown( buf,0,GROUP_GID ) ;
	}
#else
	{
	 fd = creat(buf,(mode_t)0666) ;
	}
#endif
          else fd = open(buf,O_WRONLY) ;

if ( fd<0 )
	{
	fprintf(stderr,"PLEASE Install score directory\n") ;
	perror(buf) ;
	return(0) ; /* no score file */
	}
	
#ifdef F_LOCK
if ( lockf( fd , F_LOCK , 0 ) != 0 ) /* Sleep if necessary */
	{
	perror("score.c:lockf") ;
	}
#endif

f = fdopen(fd,"w") ;
if ( f==0 )
	{
	perror(buf) ;
	return(0) ; /* no high score, error writing */
	}


for(i=s.nb-1;i>=0;i--) if ( s.score[i].score>=score ) break ;

inscore = 0 ;

if ( i!=MAXSCORE-1 )
	{
	if ( tryname( name , getlogin() ) )
	  if ( tryname( name , cuserid(0L) ) )
	    if ( tryname( name , getenv("LOGNAME") ) )
	      if ( tryname( name , getenv("USER") ) )
		{
		fprintf(stderr,"I can't find your name...\n") ;
		fprintf(stderr,"file=%s line=%d\n",__FILE__,__LINE__) ;
		fprintf(stderr,"Please give me a patch\n") ;
		strcpy(name,"NONAME") ;
		}
	
	j = i ;
	while( i>=0 ) if ( strcmp( s.score[i].name,name )==0 ) break ;
			else i-- ;
	if ( i==-1 )
		{
		for(i=s.nb;i>j+1;i--) s.score[i] = s.score[i-1] ;
		s.score[j+1].nbblocs = nbblocs ;
		s.score[j+1].nbcubes = nbcubes ;
		s.score[j+1].nblevel = nblevel ;
		s.score[j+1].score = score ;
		time( &s.score[j+1].date ) ;
		strncpy( s.score[j+1].name,name,MAXNAME ) ;
		if ( s.nb!=MAXSCORE ) s.nb++ ;
		inscore = 1 ;
		}
	}
for(i=0;i<s.nb;i++)
	fprintf(f,"%s %d %d %d %d %ld\n",s.score[i].name,s.score[i].nbcubes,
		s.score[i].nbblocs,s.score[i].nblevel,s.score[i].score,
		s.score[i].date ) ;

#ifdef F_LOCK
lockf( fd , F_ULOCK , 0 ) ;
#endif

fclose(f) ;
close(fd) ;

return(inscore) ;
}

/**********************************************************************/
/* Open the score window !!!! */
/**********************************************************************/
Window scorewin(d,s,r,depth,v,b,gc,xfont,icone,colormap,geom)
Display *d ;
int s ;
Window r ;
int depth ;
Visual *v ;
unsigned long b ;
GC gc ;
XFontStruct *xfont ;
Pixmap icone ;
Colormap colormap ;
char *geom ;
{
XSetWindowAttributes wa ;
Window scores ;
char buf[LINE_LENGTH] ;
int ascent,descent,dir ;
XCharStruct overall_return ;
int x,y ;
unsigned int dx,dy,odx,ody ;
XSizeHints sh ;

wa.event_mask = ExposureMask|StructureNotifyMask ;
wa.background_pixel = b ;

XTextExtents(xfont,MAXLINE,(int)strlen(MAXLINE),
                        &dir,&ascent,&descent,&overall_return) ;
heig = ascent+descent ;

x = 0 ;
y = 0 ;
odx = dx = overall_return.width ;
ody = dy = (4+MAXSCORE)*(heig+LINESPACE) ;

if ( geom[0]!='\0' )
    MyXParseGeometry(d,s,geom,&x,&y,&dx,&dy) ;

scores = XCreateWindow(
                        d,
                        r,
                        x,y,dx,dy,
                        0,
                        depth,
                        InputOutput,
                        v,
                        CWEventMask|CWBackPixel,
                        &wa);
disp = d ;
wind = scores ;
scgc = gc ;
xfon = xfont ;

sprintf(buf,"XBlockOut %s scores",XBLVERSION) ;
XSetStandardProperties(d,scores,buf,
                                       "Scores",
                                       icone,
                                       (char**)0,0,
                                       NULL ) ;
XSetWindowColormap(d,scores,colormap) ;

/* Not interesting to increase score menu size */
sh.flags = PMaxSize ;
if ( geom[0]!='\0' ) sh.flags |= PPosition ;
sh.max_width = odx ;
sh.max_height = ody ;
sh.x = x ;
sh.y = y ;
XSetWMNormalHints( d,scores,&sh ) ;

return(scores) ;
}

/**********************************************************************/
/* Analyse score event */
/**********************************************************************/
void scoreevent(bl,event)
struct bl *bl ;
XEvent *event ;
{
int i ;

switch( event->type )
   {
   case Expose :
	do	{
		i = XCheckWindowEvent(bl->x.display,bl->x.wscore,
					ExposureMask,event) ;
		}
	while(i==True) ;
	drawscores(bl,1) ;
	break ;
   case UnmapNotify :
   case MapNotify :
	if ( (bl->menu.showscore    && event->type==UnmapNotify) ||
	     (bl->menu.showscore==0 && event->type==MapNotify) )
	push_button(bl->menu.viewscore,
		    bl->menu.viewscore->x+bl->menu.viewscore->dx/2,
		    bl->menu.viewscore->y+bl->menu.viewscore->dy/2,
		    1) ;
	setargs(bl) ;
	break ;
   case ConfigureNotify :

	{
	XSizeHints sh ;
	sh.flags = PPosition ;
	sh.x = ((XConfigureEvent*)event)->x ;
	sh.y = ((XConfigureEvent*)event)->y ;
	XSetWMNormalHints( bl->x.display,bl->x.wscore,&sh ) ;
	}

   case ReparentNotify :
	setargs(bl) ;
	break ;
   default :
	fprintf(stderr,"Unknow event type in score window\n") ;
	fprintf(stderr,"event.type = %d\n",event->type) ;
   }
}
