/*
 * File:	endprompt.c
 * Purpose:	Implement the "save+exit / exit / cancel" dialog box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: endprompt.c,v 1.4 1996/12/22 20:06:11 liw Exp $"
 */


#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>

#include <publib.h>

#include "endprompt.h"
#include "cmd.h"
#include "win.h"
#include "x.h"
#include "error.h"



/*
 * Structure:	endprompt
 * Purpose:	Descriptor for the exit dialog box.
 * Fields:	shell	the top level widget for the dialog
 */
struct endprompt {
	struct win *win;
	Widget shell;
	unsigned positioned:1;
};



/*
 * Prototypes for local functions.
 */

static void delete_ep(Widget);
static void do_cancel(Widget, XtPointer, XtPointer);
static void do_exit(Widget, XtPointer, XtPointer);
static void do_save_and_exit(Widget, XtPointer, XtPointer);



int endprompt_create(struct endprompt **ep, struct win *win) {
	Widget form, msg, save_and_exit, exit, cancel;
	const int buttondist = 10;

	*ep = malloc(sizeof(struct endprompt));
	if (*ep == NULL) {
		error(win, "malloc failed, can't create endprompt");
		return -1;
	}

	(*ep)->win = win;
	(*ep)->positioned = 0;

	(*ep)->shell = XtVaCreatePopupShell("endprompt",
		transientShellWidgetClass, win_toplevel(win), 
		XtNallowShellResize, True,
		XtNinput, True,
		NULL);

	form = XtVaCreateManagedWidget(
		"form",
		formWidgetClass,
		(*ep)->shell,
		NULL);

	msg = XtVaCreateManagedWidget(
		"msg",
		labelWidgetClass,
		form,
		XtNlabel, "All edits have not been saved",
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	save_and_exit = XtVaCreateManagedWidget(
		"save_and_exit",
		commandWidgetClass,
		form,
		XtNlabel, "Save and exit",
		XtNfromVert, msg,
		XtNvertDistance, buttondist,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	exit = XtVaCreateManagedWidget(
		"exit",
		commandWidgetClass,
		form,
		XtNlabel, "Exit (don't save)",
		XtNfromVert, msg,
		XtNvertDistance, buttondist,
		XtNfromHoriz, save_and_exit,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	cancel = XtVaCreateManagedWidget(
		"cancelbutton",
		commandWidgetClass,
		form,
		XtNlabel, "Cancel (don't exit)",
		XtNfromVert, msg,
		XtNvertDistance, buttondist,
		XtNfromHoriz, exit,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		NULL);

	XtAddCallback(save_and_exit, XtNcallback, do_save_and_exit, win);
	XtAddCallback(exit, XtNcallback, do_exit, 0);
	XtAddCallback(cancel, XtNcallback, do_cancel, *ep);

	XtRealizeWidget((*ep)->shell);
	x_set_wm_protocols((*ep)->shell, delete_ep);

	return 0;
}



void endprompt_destroy(struct endprompt *ep) {
	XtDestroyWidget(ep->shell);
}




/*
 * Function:	endprompt_popup
 * Purpose:	Pop up a dialog box.
 * Arguments:	ep	the dialog box
 * Return:	Nothing.
 */
void endprompt_popup(struct endprompt *ep) {
	Position x, y;
	Dimension width, height;
	Widget top;

	if (!ep->positioned) {
		top = win_toplevel(ep->win);
		XtVaGetValues(top, XtNwidth, &width, XtNheight, &height, NULL);
		XtTranslateCoords(top, (Position) 30, (Position) 30, &x, &y);
		XtVaSetValues(ep->shell, XtNx, x, XtNy, y, NULL);
		ep->positioned = 1;
	}
	XtPopup(ep->shell, XtGrabNone);
}



/*
 * Function:	endprompt_popdown
 * Purpose:	Close a dialog box.
 * Arguments:	ep	the dialog
 * Return:	Nothing.
 */
void endprompt_popdown(struct endprompt *ep) {
	XtPopdown(ep->shell);
}



/*
 * Function:	endprompt_contains
 * Purpose:	Check whether a dialog contains a widget.
 * Arguments:	ep	the dialog
 *		wid	the widget
 * Return:	True or false.
 */
int endprompt_contains(struct endprompt *ep, Widget wid) {
	return ep->shell == wid;
}




/***********************************************************************
 * Local functions follow.
 */



/*
 * Function:	delete_ep
 * Purpose:	React to WM_DELETE by closing the dialog box.
 * Arguments:	wid	the widget that got WM_DELETE
 * Return:	Nothing.
 */
static void delete_ep(Widget wid) {
	XtPopdown(wid);
}



/*
 * Pop down a dialog box.
 */
static void do_cancel(Widget w, XtPointer client_data, XtPointer foo) {
	endprompt_popdown((struct endprompt *) client_data);
}



/*
 * Just exit, don't save.
 */
static void do_exit(Widget w, XtPointer client_data, XtPointer foo) {
	exit(0);
}



/*
 * Save everything, then exit.
 */
static void do_save_and_exit(Widget w, XtPointer client_data, XtPointer foo) {
	if (cmd_save_all((struct win *) client_data) != -1)
		exit(0);
}
