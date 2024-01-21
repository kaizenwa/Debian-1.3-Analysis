/*
 * File:	msgwin.c
 * Purpose:	Implement the message box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: msgwin.c,v 1.4 1996/12/22 20:06:12 liw Exp $"
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

#include "msgwin.h"
#include "cmd.h"
#include "win.h"
#include "x.h"
#include "error.h"



/*
 * Structure:	msgwin
 * Purpose:	Descriptor for the message box.
 * Fields:	shell	the top level widget for the dialog
 */
struct msgwin {
	struct win *win;
	Widget shell, msg, dismiss;
	unsigned positioned:1;
};



/*
 * Prototypes for local functions.
 */

static void delete_ep(Widget);
static void do_dismiss(Widget, XtPointer, XtPointer);



int msgwin_create(struct msgwin **ep, struct win *win) {
	Widget form;
	XtTranslations trans;
	const int buttondist = 10;

	*ep = malloc(sizeof(struct msgwin));
	if (*ep == NULL) {
		error(win, "malloc failed, can't create msgwin");
		return -1;
	}

	(*ep)->win = win;
	(*ep)->positioned = 0;

	(*ep)->shell = XtVaCreatePopupShell("msgwin",
		transientShellWidgetClass, win_toplevel(win), 
		XtNallowShellResize, True,
		XtNinput, True,
		NULL);

	form = XtVaCreateManagedWidget(
		"form",
		formWidgetClass,
		(*ep)->shell,
		XtNresizable, True,
		NULL);

	(*ep)->msg = XtVaCreateManagedWidget(
		"msg",
		labelWidgetClass,
		form,
		XtNlabel, "Message has not been set yet (bug?)",
		XtNwidth, 300,
		XtNborderWidth, 0,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainRight,
		NULL);

	(*ep)->dismiss = XtVaCreateManagedWidget(
		"dismiss",
		commandWidgetClass,
		form,
		XtNlabel, "Dismiss",
		XtNfromVert, (*ep)->msg,
		XtNvertDistance, buttondist,
		XtNtop, XtChainTop,
		XtNbottom, XtChainTop,
		XtNleft, XtChainLeft,
		XtNright, XtChainLeft,
		XtNinput, True,
		NULL);

	XtAddCallback((*ep)->dismiss, XtNcallback, do_dismiss, *ep);

	trans = XtParseTranslationTable(
		"<Key>Return: dismiss_message() \n"
		"<Key>Escape: dismiss_message() \n");
	XtOverrideTranslations((*ep)->dismiss, trans);

	XtRealizeWidget((*ep)->shell);
	x_set_wm_protocols((*ep)->shell, delete_ep);
	XtSetKeyboardFocus((*ep)->shell, (*ep)->dismiss);

	return 0;
}



void msgwin_destroy(struct msgwin *mw) {
	XtDestroyWidget(mw->shell);
}




/*
 * Function:	msgwin_popup
 * Purpose:	Pop up a dialog box.
 * Arguments:	ep	the dialog box
 * Return:	Nothing.
 */
void msgwin_popup(struct msgwin *ep) {
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
 * Function:	msgwin_popdown
 * Purpose:	Close a dialog box.
 * Arguments:	ep	the dialog
 * Return:	Nothing.
 */
void msgwin_popdown(struct msgwin *ep) {
	XtPopdown(ep->shell);
}



/*
 * Function:	msgwin_contains
 * Purpose:	Check whether a dialog contains a widget.
 * Arguments:	ep	the dialog
 *		wid	the widget
 * Return:	True or false.
 */
int msgwin_contains(struct msgwin *ep, Widget wid) {
	return ep->shell == wid || ep->msg == wid || ep->dismiss == wid;
}



/*
 * Function:	msgwin_set
 * Purpose:	Set message displayed in message window.
 * Arguments:	mw	the message window
 *		str	the message
 * Return:	Nothing.
 */
void msgwin_set(struct msgwin *mw, const char *str) {
	XtVaSetValues(mw->msg, XtNlabel, str, XtNwidth, 9*strlen(str), NULL);
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
static void do_dismiss(Widget w, XtPointer client_data, XtPointer foo) {
	msgwin_popdown((struct msgwin *) client_data);
}
