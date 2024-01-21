/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 *
 *      Author: Wei Xu, Trusted Information Systems, Inc.
 */
static  char    RcsId[] = "Header: ";

#include "ulib.h" 

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>

/* USE_AW*/
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

/*****************
 * EXTERN  DATA  *
 *****************/
extern Widget getShell();
extern Widget topshell;

/*****************
 * PRIVATE DATA  *
 *****************/
#ifndef USE_USERDATA
static int  AlertResponse = -1;
#endif


/*************
 * FUNCTIONS *
 *************/
void alert_user_CB(w,user_data,call_data)
Widget w;
caddr_t user_data, call_data;
{
#ifdef USE_USERDATA
        Arg   args[2];
	int   AlertResponse;

	XtSetArg( args[0], XtNuserData, AlertResponse );
	XtSetValues( getShell(w), args, 1 ); 
#endif
	AlertResponse = (int) user_data;
}

Widget getDialog(  w, msg, ok_text,     okvalue, 
                           cancel_text, cancelvalue, 
 			   def,         button_cb )
Widget w;
char  *msg;
char  *ok_text;      /* Null: may not to display the buttn */
int    okvalue;      
char  *cancel_text;  /* Null: may not to display the buttn */
int    cancelvalue;  
int    def;
void (*button_cb)();
{
    int          n=0;
    Arg          arg[4];
    Widget       dialog;
    Widget       box, tmpw;

    dialog  = XtCreateManagedWidget( "dialog", panedWidgetClass,
				      w?w:topshell,       NULL, 0 );

    XtSetArg(arg[n],XtNlabel,(XtArgVal)msg); n++;
    XtSetArg(arg[n],XtNshowGrip,(XtArgVal)FALSE); n++;

    XtCreateManagedWidget( "label",  labelWidgetClass,
		            dialog,  arg, n );

    box = XtCreateManagedWidget( "box", boxWidgetClass,
				 dialog, NULL, 0 );

    if( ok_text ) {
	tmpw = XtCreateManagedWidget( ok_text, commandWidgetClass,
				      box,     NULL, 0 );
        if( button_cb )
        XtAddCallback( tmpw, XtNcallback, button_cb, (caddr_t)okvalue);
    }
    
    if( cancel_text ) {
        tmpw = XtCreateManagedWidget( cancel_text, commandWidgetClass,
				      box,         NULL, 0 );
        if( button_cb )
          XtAddCallback( tmpw, XtNcallback, button_cb, (caddr_t)cancelvalue );
    }

    XtRealizeWidget( topshell );
    return dialog;
}

int loopDialogCB( w, msg, ok_text,     okvalue, 
                          cancel_text, cancelvalue, def,
                          cb,          cb_data )
Widget w;
char  *msg;
char  *ok_text;      /* Null: may not to display the buttn */
int    okvalue;  
char  *cancel_text;  /* Null: may not to display the buttn */
int    cancelvalue;
int    def;   
void (*cb);
int   *cb_data;      /* must be a valid point */
{
    Widget   dialog;
    int		ret;

    dialog = getDialog( w,           msg,
			ok_text,     okvalue,
			cancel_text, cancelvalue,
			def,         cb );
    ret=xloop( dialog, cb_data, getppid() );
    XtDestroyWidget(XtParent(dialog));
    procXEvent();

    return ret;
}

int loopDialog( w, msg, ok_text, okvalue, cancel_text, cancelvalue, def )
Widget w;
char  *msg;
char  *ok_text;      /* Null: may not to display the buttn */
int    okvalue;
char  *cancel_text;  /* Null: may not to display the buttn */
int    cancelvalue;
int    def;
{   
    return( loopDialogCB( w, msg, ok_text, okvalue, cancel_text, cancelvalue,
			  def,    alert_user_CB,    &AlertResponse ) );
}

pid_t childDialogCB( application, display_env,
                   w, msg, 
		   ok_text, okvalue, cancel_text, cancelvalue, def,
		   cb,      cb_data )
char *application;
char *display_env;
Widget w;
char  *msg;
char  *ok_text;      /* Null: may not to display the buttn */
int    okvalue;
char  *cancel_text;  /* Null: may not to display the buttn */
int    cancelvalue;
int    def;  
void (*cb)();
int   *cb_data;
{  
    Widget   dialog;
    pid_t    pid = fork();
    int      argc = 3;
    static   char *argv[] = { NULL, "-display", NULL, NULL };

    if ( pid< 0) {
	 pmsg( "Fork of child failed", TRUE );
	 return -1;
    }
    else if( pid>0 ) return pid;

    argv[0]=application; argv[2]=display_env; 
    close(0); close(1); close(2);
    xInit( application, display_env, argc, argv );

    exit( loopDialogCB( w, msg, ok_text, okvalue, 
			      cancel_text, cancelvalue, def,
		              cb, cb_data ) );
}
pid_t childDialog( application, display_env,
                 w, msg,
		 ok_text, okvalue, cancel_text, cancelvalue, def)
char *application;
char *display_env;
Widget w;
char  *msg;
char  *ok_text;      /* Null: may not to display the buttn */
int    okvalue;
char  *cancel_text;  /* Null: may not to display the buttn */
int    cancelvalue;
int    def;
{
    return
    childDialogCB( application, display_env,
		   w, msg,
		   ok_text, okvalue, cancel_text, cancelvalue, def,
		   alert_user_CB,   &AlertResponse );
}
