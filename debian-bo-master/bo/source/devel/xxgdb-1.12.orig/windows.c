/*****************************************************************************
 *
 *  xdbx - X Window System interface to dbx
 *
 *  Copyright 1989, 1990 The University of Texas at Austin
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  not be used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *  SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS BE LIABLE FOR ANY
 *  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung, The University of Texas at Austin
 *  Created:   	March 10, 1989
 *
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990,1993 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  windows.c:
 *
 *    CreateTitleBar() :	Create title bar.
 *    CreateFileLabel() :	Create file label in file window.
 *    CreateLineLabel() :	Create line label in file window.
 *    CreateFileWindow() :	Create file window.
 *    CreateMessageWindow() :	Create message window.
 *    CreateDisplayWindow() :	Create display window.
 *    CreateSubWindows() :	Create the subwindows.
 *    UpdateFileLabel() :	Update file label.
 *    UpdateLineLabel() :	Update line label.
 *    UpdateMessageWindow() :	Update message window.
 */

#include "global.h"

Widget	fileWindow,			/* parent of fileLabel and lineLabel */
	messageWindow,			/* window for displaying messages */
	separator,			/* separator in vpane */
	displayWindow;			/* area for displaying variables */

static Widget 	fileLabel,		/* filename of displayed text */
		lineLabel;		/* line number of caret position */

/*
 *  Private routines for creating various subwindows for xdbx.
 */

static void CreateFileLabel(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "No Source File");           n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);           		n++;
    fileLabel = XtCreateManagedWidget("fileLabel", labelWidgetClass, 
				      parent, args, n);
}

static void CreateLineLabel(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "");           		n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);           		n++;
    XtSetArg(args[n], XtNfromHoriz, (XtArgVal) fileLabel);          	n++;
    XtSetArg(args[n], XtNhorizDistance, (XtArgVal) 0);          	n++;
    lineLabel = XtCreateManagedWidget("lineLabel", labelWidgetClass, 
				      parent, args, n);
}

static void CreateFileWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    fileWindow = XtCreateManagedWidget("fileWindow", formWidgetClass, 
				       parent, args, n);
    CreateFileLabel(fileWindow);
    CreateLineLabel(fileWindow);
}

static void CreateMessageWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) ""); 			n++;
    XtSetArg(args[n], XtNjustify, (XtArgVal) XtJustifyLeft);          	n++;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    messageWindow = XtCreateManagedWidget("messageWindow", labelWidgetClass,
					  parent, args, n);
}

/*  Create a window for displaying variables as specified by the display
 *  command in dbx.
 */
static void CreateDisplayWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

#ifndef NEW_INTERFACE
    n = 0;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);                    n++;
    XtSetArg(args[n], XtNmin, (XtArgVal) 2); 				n++;
    XtSetArg(args[n], XtNmax, (XtArgVal) 2); 				n++;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    separator = XtCreateWidget("", labelWidgetClass, parent, args, n);
#endif
    n = 0;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextRead);		n++;
#ifdef NEW_INTERFACE
    displayWindow = XtCreateManagedWidget("displayWindow",
					  asciiTextWidgetClass, 
					  parent, args, n);
#else
    displayWindow = XtCreateWidget("displayWindow", asciiTextWidgetClass, 
				   parent, args, n);
#endif

#ifndef NEW_INTERFACE
    if (app_resources.displayWindow) {
	XtManageChild(separator);
	XtManageChild(displayWindow);
    }
#endif
}

#ifdef NEW_INTERFACE
Widget pcWindow;
Widget sourceShell;
Widget sourceToggle;
Widget commandShell;
Widget commandToggle;
Widget displayShell;
Widget displayToggle;

Atom   wm_delete_window;

Widget MatchToggleFromShell(shell)
Widget shell;
{
  if (shell == sourceShell) return sourceToggle;
  if (shell == commandShell) return commandToggle;
  if (shell == displayShell) return displayToggle;
  return NULL;
}

void State_Transient(w, client_data, call_data)
     Widget w;
     Widget client_data;
     XtPointer call_data;
{
  Arg 	args[MAXARGS];
  Cardinal 	n;
  Boolean       ToggleState;

  XtVaGetValues(w, XtNstate, &ToggleState, NULL);
  if (ToggleState)
    XtPopup(client_data, XtGrabNone);
  else
    XtPopdown(client_data);
}

void WMDeleteWindow(w, client_data, msg)
     Widget w;
     XtPointer client_data;
     XClientMessageEvent *msg;
{
    if (msg->type == ClientMessage && msg->data.l[0] == wm_delete_window) {
      w = MatchToggleFromShell(client_data);
      if (w) XawToggleUnsetCurrent(w);
    }
}


void SetupWMProtocol(shell)
Widget shell;
{
  XtRealizeWidget(shell);
  XSetWMProtocols(XtDisplay(shell), XtWindow(shell), &wm_delete_window, 1);
  XtAddEventHandler(shell, NoEventMask, True, WMDeleteWindow, (XtPointer) shell);
}

void CreatePCToggle(parent, toggle, name, label, shell)
Widget	parent;
Widget  *toggle;
char *name;
XtPointer label;
Widget shell;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNresize, (XtArgVal) False);			n++;
    XtSetArg(args[n], XtNlabel, label);          			n++;
    XtSetArg(args[n], XtNwidth, 130);                                   n++;
#ifdef OPEN_ALL_WINDOWS
    XtSetArg(args[n], XtNstate, True);                                   n++;
#endif
    *toggle = XtCreateManagedWidget(name, toggleWidgetClass,
				   parent, args, n);
    XtAddCallback(*toggle, XtNcallback, State_Transient, shell);
}

static void CreatePopupControls(parent)
Widget parent;
{
  Widget QuitButtonTwo;
  Arg 	args[MAXARGS];
  Cardinal 	n;
  
  n = 0;
  XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
  pcWindow = XtCreateManagedWidget("pcWindow", boxWidgetClass, 
				   parent, args, n);
  CreatePCToggle(pcWindow, &sourceToggle , "pcSource"  ,
		 "Source Listing"  , sourceShell );
  CreatePCToggle(pcWindow, &commandToggle, "pcCommand" ,
		 "Command Buttons" , commandShell);
#ifdef GDB
  CreatePCToggle(pcWindow, &displayToggle, "pcDisplay" ,
                 "Display Window"  , displayShell);
#endif
  n = 0;
  QuitButtonTwo = XtCreateManagedWidget("Quit", commandWidgetClass,
					pcWindow, args, n);
  XtAddCallback(QuitButtonTwo, XtNcallback, Quit, NULL);
}

#endif /* NEW_INTERFACE */

/*  PUBLIC ROUTINES */
/*
 *  Top level function for creating all the xdbx subwindows.
 */
void CreateSubWindows(parent)
Widget parent;
{
    Widget	vpane;		/* outer widget containing various subwindows */
#ifdef NEW_INTERFACE
    Widget      listingForm; /* file listing Shell */
    Widget      displayForm; /* file listing Shell */
#endif
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    vpane = XtCreateManagedWidget("vpane", panedWidgetClass, parent, args, n);

#ifdef NEW_INTERFACE
    wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW", False);

    n = 0;
    XtSetArg(args[n], XtNtitle, "xxgdb Source Display");                n++;
    sourceShell = XtCreatePopupShell("sourceShell", 
				     transientShellWidgetClass,
				     toplevel, args, n);
    n=0;
    listingForm = XtCreateManagedWidget("listingForm", 
					formWidgetClass,
					sourceShell, args, n);

    CreateFileWindow(listingForm);
    CreateSourceWindow(listingForm);
    SetupWMProtocol(sourceShell);
#else
    CreateFileWindow(vpane);
    CreateSourceWindow(vpane);
#endif
    CreateMessageWindow(vpane);
#ifdef NEW_INTERFACE
    n = 0;
    XtSetArg(args[n], XtNtitle, "xxgdb Commands");                n++;
    commandShell = XtCreatePopupShell("commandShell", 
				     transientShellWidgetClass,
				     toplevel, args, n);
    CreateCommandPanel(commandShell);
    SetupWMProtocol(commandShell);
#else
    CreateCommandPanel(vpane);
#endif
    CreateDialogWindow(vpane);
#ifdef GDB
#ifdef NEW_INTERFACE
    n = 0;
    XtSetArg(args[n], XtNtitle, "xxgdb Variable Display");                n++;
    displayShell = XtCreatePopupShell("displayShell", 
				      transientShellWidgetClass,
				      toplevel, args, n);
    n=0;
    displayForm = XtCreateManagedWidget("displayForm",
					formWidgetClass,
					displayShell, args, n);

    CreateDisplayWindow(displayForm);
    SetupWMProtocol(displayShell);
#else
    CreateDisplayWindow(vpane);
#endif
#else /* not GDB */
#ifndef BSD
    CreateDisplayWindow(vpane);
#endif
#endif	/* not GDB */
#ifdef NEW_INTERFACE
    CreatePopupControls(vpane);
#endif
} 

/*
 *  Routines for updating fields for the filename and line number
 *  in the file window, and the execution status in the message window.
 */

void UpdateFileLabel(string)
char *string;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) string);        		n++;
    XtSetValues(fileLabel, args, n);
}

void UpdateLineLabel(line)
Cardinal line;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;
    char 	string[10];

    n = 0;
    if (line > 0)
    	sprintf(string, "%d", line);
    else
	strcpy(string, "");
    XtSetArg(args[n], XtNlabel, (XtArgVal) string);        	n++;
    XtSetValues(lineLabel, args, n);
}

/*--------------------------------------------------------------------------+
|																			|
|	Note : UpdateMessageWindow assumes that the format string				|
|			can only contain one %s specifier.								|
|		arg is either NULL or is a string.									|
|		format is a string (never NULL).									|
|																			|
+--------------------------------------------------------------------------*/
void UpdateMessageWindow(format, arg)
char *format, *arg;
{
	char *message;
	char string[LINESIZ];
	int fulllength;
    Arg 	args[MAXARGS];
    Cardinal 	n;

	/* fix bug where if a debugged program function arg is a string
that looks like "%s", and UpdateMesssageWindow is passed that string
in format (with a NULL arg), then UpdateMessageWindow expects another
arg.  We fix by seeing if arg is NULL.  If so, then make format "%s"
and arg whatever the format string was. */

	if (arg == NULL) {
	    arg = format;
	    format = "%s";
	}

	fulllength = strlen ("  ") + strlen (format) + 1 + ((arg == NULL) ? 0 : strlen (arg));
	
	if (fulllength > LINESIZ)
		message = (char*) XtMalloc (fulllength);
	else
		message = string;
		
    strcpy(message, "  ");
    sprintf(message + strlen(message), format, arg);
    
    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) message);		n++;
    XtSetValues(messageWindow, args, n);
    
	if (fulllength > LINESIZ)
		XtFree(message);
}

void ClearMessageWindow()
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "");			n++;
    XtSetValues(messageWindow, args, n);
}

