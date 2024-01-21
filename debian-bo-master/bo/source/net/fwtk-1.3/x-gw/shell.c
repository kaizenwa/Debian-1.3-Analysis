/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

 /*
  *      Author: Wei Xu, Trusted Information Systems, Inc.
 */
static  char    RcsId[] = "Header: ";

#include "ulib.h"
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#else
/* USE_AW*/
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#endif


/****************************
 *   PUBLIC DATA            *
 ****************************/
Display     *display;
Widget       topshell;
XtAppContext app_con;


/**********************
 * Functions          *
 **********************/
Widget xInit( application, disp_str, argc, argv )
char *application;
int  *disp_str;
int   argc;
char *argv[];
{
#ifndef USE_MOTIF 
	setenv( "DISPLAY",disp_str,1 );

	topshell = XtInitialize( NULL,application,NULL,0,&argc,argv );
	app_con  = XtWidgetToApplicationContext(topshell);
#else
	XtToolkitInitialize();
	app_con = XtCreateApplicationContext();
	display = XtOpenDisplay(app_con,disp_str,NULL,application,
				NULL,0,&argc,argv);
	topshell = XtAppCreateShell(NULL,application,
					 applicationShellWidgetClass,
					 display, NULL,NULL);
#endif
	return topshell;
}

Widget getShell( w )
Widget w;
{
   Widget  shell=w;

   while(shell) {
      if( XtIsShell(shell) ) break;
      else shell=XtParent(shell);
   }
   return shell;
}


procXEvent()
{
   XEvent event;

   XtAppNextEvent(app_con,&event);
   XtDispatchEvent(&event);
}

/**
 ** the loop check the loopResponse until it is >=0 
 ** or the related proc is not alive.
 **/
int xloop( w,loopResponse,check_ppid )
Widget    w;
int      *loopResponse;
pid_t     check_ppid; /* NULL: not to check the pid of parent 
                       *  > 0: to stop the loop when parent died */
{
    while( *loopResponse < 0 ) {
	    procXEvent();
	    if( check_ppid>0 && getppid() != check_ppid ) break;
    }

    return *loopResponse;
}

