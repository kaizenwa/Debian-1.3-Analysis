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

#ifndef lint
static char sccsid[] = "@(#) Xbl 1.0f 3D tetris under X11, (C) 1992,1993,1994 Thierry EXCOFFIER" ;
#endif

#include "define.h"
#include "bl.h"
#include "transfo.h"
#include "options.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <signal.h>
#include <time.h>

#include "icone.h"
#include "score.h"
#include "buttons.h"

#include <X11/Xresource.h>

static struct bl bl ;		/* Global, because used by "signals" */

void quitprog() ;
void speedtest(R1(struct bl *bl)) ;
void savebell( R1(Display *d) ) ;
void restorebell( R1(Display *d) ) ;
int myerror() ;

void getres(db,allargs)
XrmDatabase db ;
char *allargs ;
{
XrmValue xrmvalue ;
char *value ;

if ( db!=0 ) 
	{
	if ( XrmGetResource(db,"*font","*Font",&value,&xrmvalue) )
		{
		strcat( allargs , " -font " ) ;
		strcat( allargs , xrmvalue.addr ) ;
		}
	if ( XrmGetResource(db,"xbl.font","Xbl.Font",&value,&xrmvalue) )
		{
		strcat( allargs , " -font " ) ;
		strcat( allargs , xrmvalue.addr ) ;
		}
	if ( XrmGetResource(db,"xbl.args","Xbl.Args",&value,&xrmvalue) )
		{
		strcat( allargs , xrmvalue.addr ) ;
		}
	}
}


int main(argc,argv)
int argc ;
char **argv ;
{
static int help=0,syncr=0 ;
/* Be carefull, comments define possible value */
static struct options optionbl[] =
       { { "h"       , 'b' , (void*)&help,
           "Print info on the game" },
         { "v"       , 'b' , (void*)&bl.opt.verbose,
           "Verbose mode" },
         { "colormap", 'b' , (void*)&bl.opt.newmap,
           "Create it's own colormap" },
         { "visual"  , 'b' , (void*)&bl.opt.visual,
           "Search a best visual (not the default one)." },
         { "bw", 'b' , (void*)&bl.opt.use_bw,
           "Work with black and white (use 2 less planes)" },
         { "buffer", 'i' , (void*)&bl.opt.buffering,
           "\
You indicate a buffering method, 6 different methods exist.\n\
#plane    number of need color plane\n\
          Substract 2 planes if you work in black&white\n\
Col       mean that program change the colormap\n\
Swap      mean that program work by swapping 2 colormap\n\
Flick     mean that moving piece ``flick'' on the window\n\
AllFlick  mean that all the window flick\n\
Redraw    mean that you see back redrawing when it changing\n\
Speed     Slow, ,Fast drawing\n\
#buf      Number of (huge) buffer to draw animation\n\
--+------+---+----+-----+--------+------+-----+----+\n\
B |#plane|Col|Swap|Flick|AllFlick|Redraw|Speed|#buf|\n\
--+------+---+----+-----+--------+------+-----+----+\n\
0:|    3 |   |    | Yes |   Yes  |  Yes |Slow |  0 |\n\
1:|    4 |Yes|    |     |        |      |Fast |  1 |\n\
2:|    3 |   |    |     |        |      |     |  2 |\n\
3:|    5 |Yes|Yes |     |        |  Yes |     |  0 |\n\
4:|    4 |Yes|    | Yes |        |  Yes |Fast |  0 |\n\
5:|    3 |   |    | Yes |        |      |Fast |  1 |\n\
--+------+---+----+-----+--------+------+-----+----+\n\
-1: choose an display adapted method " },
         { "keytable", 's' , (void*)bl.opt.userkey,
           "The characters table for\n\
		Translations  DL,L,UL,U,UR,R,DR,D\n\
		Rotations     RX+,RX-,RY+,RY-,RZ+,RZ-\n\
	        and           LaunchBloc CancelGame SuspendGame QuitGame" },
         { "font", 's' , (void*)bl.opt.thefont,
           "The font for menu, scores,..." },
         { "bigfont", 's' , (void*)bl.opt.thefont2,
           "The big font for menu, scores,..." },
         { "buttonheight", 'i' , (void*)&bl.opt.button_height,
           "(-1...16) Height of relief button, -1 is font dependent" },
         { "color", 'i' , (void*)&bl.opt.backcolor,
           "Menu background color\n\
	    0:Black 1:Red     2:Green 3:Yellow\n\
	    4:Blue  5:Magenta 6:Cyan" },
         { "speedtest"       , 'b' , (void*)&bl.opt.speedtest,
           "Use it just to see the speed of drawing" },
         { "clearline"       , 'i' , (void*)&bl.opt.clearline,
           "0: Clear line with a fill rectangle 1:Clear by black redraw\n\
		This only change the display speed" },
         { "linewidth"       , 'i' , (void*)&bl.opt.linewidth,
	    "(0...17) Thickness of the bloc lines (0=fast draw)" },
         { "presskey"       , 'b' , (void*)&bl.opt.presskey,
           "If set, \"Press Key\" message will not appear" },
         { "time_to_demo"   , 'i' , (void*)&bl.opt.time_to_demo,
	    "(1...9999) Number of seconds between demonstration" },
         { "sync", 'b' , (void*)&syncr,
           "Use synchronize mode of X (for debugging)\n\
		-\n\
		----------------------------------------\n\
		Next options can be modified during game\n\
		----------------------------------------\n\
		" },

         { "keyboard", 'i' , (void*)&bl.opt.keyboard,
           "0:Use key table 1:AZERTY 2:QWERTY| 3:AZERTY- 4:QWERTY- 5:SPATIAL" },
	/* Change previous line if NB_KEYBOARD change */
         { "zoo", 'b' , (void*)&bl.menu.showzoo,
           "To see piece set" },
         { "score", 'b' , (void*)&bl.menu.showscore,
           "To see score window" },
         { "draw", 'i' , (void*)&bl.opt.drawmode,
           "0: Wire frame 2: Transparent" },
         { "x", 'i' , (void*)&bl.opt.wx,
           "(3...8) X size of world" },
         { "y", 'i' , (void*)&bl.opt.wy,
           "(3...8) Y size of world" },
         { "z", 'i' , (void*)&bl.opt.wz,
           "(6...18) Depth of world" },
         { "stat", 'b' , (void*)&bl.opt.statis,
           "Various statistique on play" },
         { "smooth", 'b' , (void*)&bl.opt.smooth,
           "Smooth displacement of pieces" },
         { "training", 'b' , (void*)&bl.opt.mode,
           "Bloc don't fall alone" },
         { "shownext", 'b' , (void*)&bl.bloc.nextpiece,
           "Show the next bloc (score are *.7)" },
         { "level", 'i' , (void*)&bl.opt.level,
           "(0...10) Start level of game" },
         { "bloctype", 'i' , (void*)&bl.bloc.typepiece,
           "0: FLAT 1:SIMPLE 2:COMPLEX" },
         { "land", 'i' , (void*)&bl.opt.land,
           "0:User defined land 1:Small land 2:Big land" },
         { "volume", 'i' , (void*)&bl.opt.volume,
           "(0...10) Sound volume" },
         { "scoregeometry", 's' , (void*)bl.opt.scoregeometry,
           "Score window geometry" },
         { "zoogeometry", 's' , (void*)bl.opt.zoogeometry,
           "Zoo window geometry" },
         { "menugeometry", 's' , (void*)bl.opt.menugeometry,
           "Menu window geometry" },
         { "geometry", 's' , (void*)bl.opt.geometry,
           "Game window geometry\n-\n-\n-\n\
Defaults options can be take in XBLOPTIONS shell variable\n\
Or in X Resource Database in server under the name xbl*args" },
         { "" , ' ' , 0 , "" }
      } ;
int i ;
char buf[LINE_LENGTH] ;
char allargs[10*LINE_LENGTH] ;
int buffering ;
char *value ;

bl.x.display = 0 ;

signal(SIGINT,quitprog) ;
signal(SIGQUIT,quitprog) ;
signal(SIGABRT,quitprog) ;
signal(SIGHUP,quitprog) ;

bl.opt.verbose   = 0 ;
bl.opt.newmap    = 0 ;
bl.opt.visual    = 0 ;
bl.opt.use_bw    = 0 ;
bl.opt.buffering = -1 ;
bl.opt.drawmode  = 2 ;
bl.opt.statis    = 0 ;
bl.opt.smooth    = 0 ;
bl.bloc.world    = 0 ;
bl.opt.wx        = 5 ;
bl.opt.wy        = 5 ;
bl.opt.wz        = 15 ;
bl.bloc.typepiece = FLAT ;
bl.opt.mode      = PLAY ;
bl.opt.keyboard  = 1 ;
bl.opt.button_height  = -1 ;
bl.opt.land	 = 0 ;
bl.opt.speedtest = 0 ;
bl.opt.clearline = 0 ;
bl.opt.linewidth = 0 ;
bl.opt.volume    = 5 ;
bl.opt.backcolor = 4 ;
bl.opt.presskey  = 0 ;
bl.opt.time_to_demo = 60 ;
bl.bloc.nextpiece = 0 ;
bl.menu.showzoo  = 0 ;
bl.opt.displayname[0] = '\0' ;
bl.opt.geometry[0] = '\0' ;
bl.opt.scoregeometry[0] = '\0' ;
bl.opt.zoogeometry[0] = '\0' ;
bl.opt.menugeometry[0] = '\0' ;
bl.x.dimx = 513 ;
bl.x.dimy = 513 ;
bl.x.wscore = bl.x.window = bl.menu.zoo = 0 ;
bl.opt.state = STOP ;

strcpy( bl.opt.thefont , "-*-*-*-r-*-*-12-*-*-*-*-*-iso8859-1" ) ;
strcpy( bl.opt.thefont2 , "-*-*-*-r-*-*-24-*-*-*-*-*-iso8859-1" ) ;

bl.boardkey[0] = bl.opt.userkey ;
bl.boardkey[1] = "14789632azqswx \035p\003" ;
bl.boardkey[2] = "14789632qwaszx \035p\003" ;
bl.boardkey[3] = "14789632aqzsed \035p\003" ;
bl.boardkey[4] = "14789632qawsed \035p\003" ;
bl.boardkey[5] = "14789632mihlkj \035\033\003" ;
/* Don't forgot change NB_KEYBOARD if you add one */
strcpy( bl.boardkey[0], bl.boardkey[1]) ;

bl.progname = argv[0] ;
for(i=0;i<argc;i++)
	{
	if ( strcmp(argv[i],"-display")==0 )
		strcpy(bl.opt.displayname,argv[i+1]) ;
	}

initdisp(&bl.opt,&bl.x) ;
buffering = bl.opt.buffering ;

bl.x.icone = XCreateBitmapFromData(bl.x.display,bl.x.root,
           icone_bits,icone_width,icone_height) ;


allargs[0] = '\0' ;

sprintf(buf,"%s/Xbl",RESOURCEDIR) ;
getres( XrmGetFileDatabase(buf) , allargs ) ;

sprintf(buf,"%s/Xbl",SCOREDIR) ;
getres( XrmGetFileDatabase(buf) , allargs ) ;

/*
value = getenv("XENVIRONMENT") ;
if ( value )
	{
	if ( value[0]!='\0' ) getres( XrmGetFileDatabase(value) , allargs ) ;
	}
*/

value = XResourceManagerString(bl.x.display) ;
if ( value ) getres( XrmGetStringDatabase(value) , allargs ) ;

strcat( allargs , " " ) ;
value = getenv("XBLOPTIONS") ;
if ( value ) strcat( allargs , value ) ;

stringoption( optionbl,allargs ) ;

prendoptions( optionbl , &argc , argv ) ; /* Take options of the program */

if ( strlen(bl.opt.userkey)>KEY_LAST )
	{
	fprintf(stderr,"Too many keys defined in keytable\n") ;
	exit(1) ;
	}

if ( bl.opt.buffering==-1 ) bl.opt.buffering = buffering ;

bl.key = bl.boardkey[bl.opt.keyboard] ;


if ( help )
	{
	proptions( optionbl ) ;
	exit(0) ;
	}
if ( bl.opt.verbose ) proptions( optionbl ) ;

if ( syncr ) XSynchronize(bl.x.display,True) ;

initdisp2(&bl.opt,&bl.x) ;
savebell( bl.x.display ) ;
initwin(&bl.opt,&bl.x) ;
initgc(&bl.opt,&bl.x) ;
/*
XSetErrorHandler(myerror) ;
*/
initbuffer(&bl.opt,&bl.x) ;
initbloc(&bl.bloc) ;



initmenu(&bl) ;
bl.x.wscore = scorewin(bl.x.display,bl.x.screen,bl.x.root,bl.x.depth,bl.x.visual,
                bl.x.back_pixel,bl.menu.text,bl.menu.xfont,
                bl.x.icone,bl.x.colormap,bl.opt.scoregeometry) ;

if ( bl.menu.showscore )
        {
        bl.menu.showscore = 0 ;
        fctscore(bl.menu.viewscore,0) ;
        }

for(i=0;i<bl.bloc.complex;i++) bl.bloc.statpiece[i] = 0 ;

srand( (unsigned long)time(0L) ) ;

bl.bloc.thisone = -1 ;

newworld(&bl) ;

XFlush(bl.x.display) ;

for(i=0;i<MAXFRAMESEC;i++) bl.realtime.framesec[i] = 0 ;
bl.realtime.displaytime=0. ;
bl.currentlevel = bl.opt.level ;
inittime(&bl.realtime,0) ;
bl.opt.state = STOP ;
display_button( bl.menu.state,bl.menu.state->x,bl.menu.state->y,
                                bl.menu.state->dx,bl.menu.state->dy ) ;


if ( bl.opt.speedtest ) speedtest(&bl) ;
loop(&bl) ;

quitprog() ;

return(0) ; /* Never come here */
}


void quitprog()
{
int i,j ;

if ( bl.x.display==0 ) exit(0) ;
if ( bl.opt.verbose )
	{
	fprintf(stderr,"QUIT : display= %d\n",(int)bl.x.display) ;
	}
restorebell(bl.x.display) ;
if ( bl.menu.zoo ) XDestroyWindow( bl.x.display,bl.menu.zoo ) ;
if ( bl.x.wscore ) XDestroyWindow( bl.x.display,bl.x.wscore ) ;
if ( bl.x.window ) XDestroyWindow( bl.x.display,bl.x.window ) ;
XSync(bl.x.display,True) ;
XCloseDisplay( bl.x.display) ;

if ( bl.opt.statis )
        {
        i = MAXFRAMESEC-1 ;
	while( bl.realtime.framesec[i]==0 && i>=0 ) i-- ;
        j = 0 ;
	while( bl.realtime.framesec[j]==0 && j<=MAXFRAMESEC ) j++ ;
        for(;i>=j;i--)
                fprintf(stderr,"%3d Frame/Sec : during %3d seconds\n",
                        i,bl.realtime.framesec[i]) ;
        }

exit(0) ;
}

void speedtest(blo)
struct bl *blo ;
{
time_t starttime,currenttime ;
int i ;
XEvent event ;

fprintf(stderr,"THIS TEST WILL TAKE SOME TIME\n") ;

blo->opt.wx = blo->opt.wy = blo->opt.wz = 6 ;
blo->opt.smooth = 1 ;
blo->opt.mode = TRAINING ;

/* Wait window exposition */
do
	{
	XNextEvent(blo->x.display,&event) ;
	}
while( event.type!=Expose ) ;

startgame(blo) ;
newfallingbloc( &blo->bloc,25 ) ;
newfallingbloc( &blo->bloc,25 ) ;
(void)addtransfo( TXP,1,&blo->bloc,1) ;
(void)addtransfo( TXP,1,&blo->bloc,1) ;
(void)addtransfo( TYP,1,&blo->bloc,1) ;
(void)addtransfo( TYP,1,&blo->bloc,1) ;
(void)addtransfo( RZP,100,&blo->bloc,10000) ;
(void)addtransfo( RYP,200,&blo->bloc,20000) ;
inittime(&blo->realtime,1) ;
time( &starttime ) ;

for(i=0;i<1000;i++)
	{
	displaymoving(&blo->opt,&blo->x,&blo->bloc,&blo->draw) ;
	(void)nextime( &blo->bloc ) ;
	XSync(blo->x.display,False) ;
	}
time( &currenttime ) ;

printf("%3ld frames/sec buffering=%d %s clearline=%d\n",
	i/(currenttime-starttime),
	blo->opt.buffering,
	blo->opt.drawmode==0? "Wireframe":"FaceDrawing",
	blo->opt.clearline
	);
quitprog() ;
}

int myerror(d,e)
Display *d ;
XErrorEvent *e ;
{
char buf[LINE_LENGTH] ;

XGetErrorText(d,e->error_code,buf,LINE_LENGTH) ;
fprintf(stderr,"%s\n-------------------------------------------------\n",buf) ;

fprintf(stderr,"If it's a BadAlloc error, use less X memory\n");
fprintf(stderr,"To do so, type : xbl -buffer 1\n");
fprintf(stderr,"            or   xbl -buffer 3\n");
fprintf(stderr,"If nothing work, you can use buffering mode 4 or 5\n") ;
fprintf(stderr,"But display will ``flick''\n") ;
fprintf(stderr,"Sorry\n") ;
exit(1) ;
}
