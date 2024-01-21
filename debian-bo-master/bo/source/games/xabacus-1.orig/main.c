/* xabacus begun on: Mon Jun 24 14:45:32 EDT 1991
 * by Luis Fernandes 
 *
 * Mon Sep  9 15:36:57 EDT 1991
 * re-doing demo-implementaion from hard-code to script-driven
 * trashed SCCS & restarted: base version has all up to -display & q,Q,^c
 * modifying -demo option to be folowed by a path to where the scripts are
 *
 * Mon Oct  7 16:38:39 EDT 1991
 * added ability to override font used in demo from command-line
 *
 * Mon Nov 11 14:39:17 EST 1991
 * deleted un-used functions in demo.c & added checking for MAXLESSON
 * Lesson-1 script modifed to show proper method when carrying
 */

/*
Copyright (c) 1991, Luis Fernandes.

Permission to use, copy, hack, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation.

This software is presented as is without any implied or written warranty.
*/

static char SccsId[]="@(#)main.c	1.4	11/20/91";

#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>

#include "patchlevel.h"
#include "maindefs.h"
#include "demo.h"

/* If more "lessons" are added, modify MAXLESSON accordingly. xabacus is
 * set-up to wrap-around to the beginning after the last lesson.
 */
#define MAXLESSON	3

extern int democount, br;
extern char displaytext[4][64];

typedef unsigned int Column;		/* int's MUST be more than 9 bits wide*/
Column column[MAXCOLS];

Display *display;
int screen;
Window root, child, base, demobase;
Widget topLevel;
unsigned int mask;
XSizeHints size_hints;
XFontStruct *font_info;
unsigned int width=INITWIDTH, demowidth=DEMOINITWIDTH,
            height=INITHEIGHT, demoheight=DEMOINITHEIGHT;
unsigned int posx=INITPOSX,
			 posy=INITPOSY;   /* position of main window */

GC gc[MAXCOLORS];
Cursor Hand;
KeySym key;
char keytext[KEYBUFFERSIZE];

#include "resource.h"

int beadwidth; int beadheight; int framewidth; int ncols;
Boolean demo;
Boolean script;

int midframey=MIDFRAME;	
int colgap=COLGAP;	int rowgap=ROWGAP;

char display_name[COLORSTRLEN<<1];
char colors[MAXCOLORS][COLORSTRLEN];
char demopath[128];
char demofont[128];
FILE *fp;
int demorow, democol, demolines, lessonlen;

Widget      TopLevel;	/* used only to access the resource manger*/

usage(appname)
char *appname;
{
	fprintf (stderr, "%s Release %s\n",appname, RELEASENUM);
	fprintf (stderr, "Usage: %s [ options ... ]\nWhere options are:\n",appname);
    fprintf (stderr,"   -display\t name \tdisplay name\n");
    fprintf (stderr,"      -demo\t path \tpath to script files\n");
    fprintf (stderr,"    -script\t \t(prints row,col to stdout when beads are \
moved)\n");
    fprintf (stderr,"     -ncols\t n \tnumber of columns\n");
    fprintf (stderr," -beadwidth\t w \twidth of the beads (in pixels)\n");
    fprintf (stderr,"-beadheight\t h \theight of the beads (in pixels)\n");
    fprintf (stderr,"-framewidth\t w \twidth of the frame (in pixels)\n\n");
    fprintf (stderr,"-framecolor\t color \tcolor-name for the frame\n");
    fprintf (stderr,"        -bg\t color \tcolor-name for the background \n");
    fprintf (stderr," -beadcolor\t color \tcolor-name for the beads\n");
    fprintf (stderr," -railcolor\t color \tcolor-name for the rails\n\n");
    fprintf (stderr,"      -help\t print this list\n\n");

	exit (1);
}

main(argc, argv)
int argc;
char **argv;
{
XEvent report;

	TopLevel = XtInitialize (NULL, "XAbacus", options, XtNumber (options),
				 &argc, argv);

	if (argc != 1) usage (argv[0]);

    XtGetApplicationResources (TopLevel, (caddr_t) &app_resources,
				resources, XtNumber(resources),
				(ArgList) NULL, (Cardinal) 0);

	transferResources();
	makeWindows(argc, argv, display_name);
	initAbacus();

	if(demo) 
		doDemo();
	else do {                /* the main event loop*/
		XNextEvent(display, &report);
	} while (TakeEvents(report));

}/* main*/

/* This function exists for the simple reason that, when the 
 * code was originally written there were no resources and things would 
 * certainly get out of hand if I sed'ed all my source replacing all 
 * occurences of one var with that of another. 
 * Besides, typing in 'beadwidth' is a lot easier than typing in 
 * 'app_resources.beadwidth'. (Yes, I'm aware of cpp )
 *
 * Also, this is a good place for checking & overriding user-supplied options.
 */
transferResources()
{
int i;

	if(app_resources.demo[0]!=' ') demo=True;

	sprintf(demopath,"%s",app_resources.demo);

	if(app_resources.script==True){
		script=True;
		(void)fprintf(stderr,"Scripting Enabled, output to stdout...\n");
	}

	if(demo){	/* use standardized values*/
		strcpy(demofont, app_resources.demofont);
		beadwidth=BEADWIDTH;
		beadheight=BEADHEIGHT;
		framewidth=FRAMEWIDTH;
		/* if anything, ncols MUST be overriden to the same number as when the
		demo (using -script) was created) */
		ncols=NCOLS;
		midframey=MIDFRAME;
		height=INITHEIGHT;
		width=INITWIDTH;
	}
	else{		/* acknowledge options */
		beadwidth=app_resources.beadwidth;
		beadheight=app_resources.beadheight;
		framewidth=app_resources.framewidth;
		ncols=app_resources.ncols;
		midframey=(framewidth+(NTOPROWS*beadheight)+((NTOPROWS+1)*ROWGAP));
		height=((3*framewidth)+(NROWS*beadheight)+((NROWS+1)*ROWGAP));
		width=((2*framewidth)+(ncols*beadwidth)+((ncols+1)*COLGAP));
	}

	if(app_resources.display[0]==' ' )
		*display_name=NULL;
	else
		strcpy(display_name, app_resources.display);

	for(i=0; i<MAXCOLORS; i++) 
		strcpy(colors[i],app_resources.colors[i]);

}/*transferResources*/


/*-----------------------------------------------------------------
 * TakeEvents, the main event loop
 *
 *			passed: XEvent variable returned from XNextEvent
 *			returns: 1 for every other valid event
 *					 Never returns when the user quits; automatically 
 *					vectored to quit routine
 */

int 
TakeEvents(report)
XEvent report;
{
int cx=0, cy=0;  		/*pointer-position in window when button pressed*/
int window_size=OK;     /* OK, or SMALL */
int root_x, root_y;		/* coodrs of click relative to root*/
int keyretval, i;
static int oldwidth;
#ifdef DEBUG
	(void)fprintf(stderr," DEBUG TakeEvents:\n");
#endif
        switch  (report.type) {

        case Expose:
			 /* get rid of unecessary  Expose events in queue*/
		 	while (XCheckTypedEvent(display, Expose, &report));

            if (window_size == SMALL)
            	fprintf(stderr,"Window is too small");	 
            	else /*refresh all windows */
            		drawAbacus();

            break;

        case ConfigureNotify:
			/* when the window is resized, the abacus should be resized
			 * accordingly;  window has been resized, change width */
			oldwidth=width;
           	width = report.xconfigure.width;
			cx=ncols;	/* temp */
			/* calc number of new cols to be added */
			ncols=((int)width-oldwidth)/(colgap+beadwidth);

			/* take new value only if it's valid*/
			/*ncols=(ncols<=0)?cx:(width-oldwidth)/(colgap+beadwidth);*/
			if(ncols==0) ncols=cx;
			else ncols+=cx;

			/* for bogus/none/retarded WM's*/
            if ((width<size_hints.min_width) || (height<size_hints.min_height))
                window_size = SMALL;
            else{
                window_size = OK;
			}/* else*/
            break;

        case ButtonPress:

			/* get current coords of pointer*/
            XQueryPointer(display, base, &root, &child, &root_x, &root_y, &cx, &cy, &mask);

			/* row,col returned in cx,cy*/
			i=translateXY2RowCol(&cx,&cy);

			if(i){		/*valid row,col...*/
				if(RowOccupied(cy,column[cx])){	/*& it's occupied...*/
#ifdef DEBUG
	(void)fprintf(stderr," DEBUG:(bead present) col=%4d row=%4d\n", cx, cy);
#endif

					if(script) (void)fprintf(stdout,"%d %d\n",cy,cx);

					animateBead((unsigned)cy,(unsigned)cx);
				}
			}
			else{
#ifdef DEBUG
	(void)fprintf(stderr," DEBUG: INVALID\n");
#endif
			/*XBell(display);*/

			}

            break;

        case KeyPress:
			keyretval=XLookupString(&report, keytext, KEYBUFFERSIZE, &key, 0);

			if(keyretval==1){	/* user pressed 1 key...*/
				if(keytext[0]=='q'||keytext[0]=='Q'||keytext[0]=='')
					closeDisplay();
			}

			keyretval=0;
			break;

        default:
            /* all events selected by StructureNotifyMask
             * are thrown away here
			 */
            break;
		}/*switch*/


	return(1);

}/*TakeEvents*/	

/* ------------------------------------------------------------
 * translateXY2RowCol, converts x,y coord returned by XQueryPointer to
 * a row,col coord. The top deck has 3 rows (1 is empty), the bottom
 * deck has 6 rows (1 is empty). Row # 3 is invalid because the middle-frame
 * occupies this position. Coordinates for the bottom deck are adjusted
 * for this anomaly, i.e. row #4 on the screen, is actually index #3
 * into the Column array.
 *
 *		passed: (pointers to) absolute x,y coordinates representing 
 *				the click-location
 *		returns: row (0-9),col (0-NCOLS) index in the same variables passed
 *				 True if click was at any location BUT mid-frame
 *				 False if click was at mid-frame location
 */
int 
translateXY2RowCol(cx,cy)
int *cx, *cy;
{

#ifdef DEBUG
	(void)fprintf(stderr," DEBUG :%d, %d\n", *cx, *cy);
#endif

	*cx=(*cx-framewidth)/(colgap+beadwidth);

	if(*cy<midframey){
		*cy=(*cy-framewidth)/(rowgap+beadheight);
	}
	else{	/* account for the middle-frame (+1 bead height (Jun 29 14:50))*/
		*cy=(*cy-(2*framewidth-beadheight))/(rowgap+beadheight);
	}

	if(*cy==3) return(0);	/* technically, the mid-frame occupies position 3 
							 * & click is invalid*/
	else if(*cy>2){	/* if pos (row) is greater than 3 ...*/
		(*cy)--;		/* ...adjust by 1 row*/
		return(1);
	}
	else 
		return(1);

}/* translateXY2RowCol*/



int 
doDemo()
{
XEvent report;
int keyretval;
/* when rv=doLesson return's FALSE(lesson is finished)  queryrunning=1*/
static int demorunning, queryrunning;
static int lessoncount;

#ifdef DEBUG
	(void)fprintf(stderr," DEBUG: doDemo()\n");
#endif

	do{
		XNextEvent(display, &report);

		switch  (report.type) {

		case Expose:
			/* discard unecessary  Expose events in queue*/
			while (XCheckTypedEvent(display, Expose, &report));

			drawAbacus();

			if(demorunning)
				drawDemoWindow();
			else if(queryrunning) 
				drawQuery();
			else if(!demorunning) 
				drawIntro();

			break;

		case ButtonPress:
			demorunning=1;
			queryrunning=0;
			lessoncount=0;	/* begin demo with lesson 1 (count=0)*/

			initAbacus();
			drawAbacus();
			startLesson(lessoncount);
			doLesson(lessoncount);	
			break;

        case KeyPress:
            keyretval=XLookupString(&report, keytext, KEYBUFFERSIZE, &key, 0);

            if(keyretval==1){   /* user pressed a key...*/
				if(keytext[0]=='r'){
					initAbacus();
					drawAbacus();
					startLesson(lessoncount);/* keep the same lesson */
					doLesson(lessoncount);
					demorunning=1;
					queryrunning=0;
				}
				else if(keytext[0]=='c'){
					initAbacus();
					drawAbacus();

					lessoncount++;	/* go on to next lesson */
					if(lessoncount>=MAXLESSON) 
						lessoncount=0;     /* wrap-around to beginning*/

					startLesson(lessoncount);	

					doLesson(lessoncount);
					demorunning=1;
					queryrunning=0;
				}
				else if(keytext[0]==' '){
					if(!demorunning) break;	/* ignore space if waiting*/

					if( !(doLesson(lessoncount)) ){
						queryrunning=1;	
						demorunning=0;	
					}
				}
				else if(keytext[0]=='q') 
                	closeDisplay();
            }

            keyretval=0;
            break;

		default:
			/* throw away all other events here */
			break;

		}/* switch*/

	}while(1);
}

startLesson(lc)
{
static char fname[128];
int i;
#if 0
	if(lc>=MAXLESSON) lc=0;		/* wrap-around to beginning*/
#endif

	sprintf(fname,"%s/Lesson%d.cmd",demopath,lc+1);

	if((fp=fopen(fname,"r"))==NULL){
		(void)fprintf(stderr,
			"%s: Error Could not open script-file:%s\n",APPNAME,fname);
		exit(1);
	}

	(void)fscanf(fp,"%d", &lessonlen);

	/*load first instance*/
	(void)fscanf(fp,"%d %d %d\n",&demorow,&democol,&demolines);

	for(i=0; i<demolines; i++){
		fgets(displaytext[i], 64, fp);
		displaytext[i][strlen(displaytext[i])-1]='\0';	
	}

}

int
doLesson(lc)
{
int i;

	/* A negative value in demorow signifies that only the text is to be
	 * displayed.
	 */
	if(lessonlen && demorow>=0) 
		animateBead((unsigned)demorow, (unsigned)democol);
	
	drawDemoWindow();

	/*load first instance*/
	(void)fscanf(fp,"%d %d %d\n",&demorow,&democol,&demolines);

	for(i=0; i<demolines; i++){
		(void)fgets(displaytext[i], 64, fp);
		displaytext[i][strlen(displaytext[i])-1]='\0';	
	}

	if(--lessonlen>=0) 
		return(1);	/* lines left*/
	else {
		fclose(fp);

		strcpy(displaytext[0],"Here Endth The Lesson");
        strcpy(displaytext[1]," ");
        strcpy(displaytext[2]," "); strcpy(displaytext[3]," ");
        XClearWindow(display,demobase);
        drawDemoWindow();
        sleep(1);
        drawQuery();

		return(0);	/* lesson over*/
	}
}
