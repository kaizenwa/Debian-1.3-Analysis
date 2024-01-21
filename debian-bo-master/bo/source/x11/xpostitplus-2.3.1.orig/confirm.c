#ifndef lint
static char	*RCSid = "$Header: /home/src/X/xpostit/xpostit/confirm.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * confirm.c - handle confirming requests made by the user.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * $Log: confirm.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 * Revision 1.2  90/06/14  11:18:52  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:47:27  davy
 * Initial revision
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Shell.h>
#include <stdio.h>

#include "xpostit.h"

static Widget	confirmwidget;

/*
 * ConfirmIt - put up a window asking for confirmation.
 */
void
ConfirmIt(confirm_callbacks, cancel_callbacks)
XtCallbackRec *confirm_callbacks, *cancel_callbacks;
{
	Arg args[4];
	Window root, child;
	unsigned int buttons;
	register int nargs, nwidgets;
	static Boolean inited = False;
	static Widget form, widgets[3];
	int root_x, root_y, child_x, child_y;

	/*
	 * Find out where the mouse is, so we can put the confirmation
	 * box right there.
	 */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * If we need to construct the confirmation box do that,
	 * otherwise just reset the position and callbacks and
	 * put it up again.
	 */
	if (!inited) {
		nargs = 0;
		SetArg(XtNx, root_x);
		SetArg(XtNy, root_y);

		/*
		 * The confirmation box will be a pop-up widget.
		 */
		confirmwidget = XtCreatePopupShell("Confirm",
						   overrideShellWidgetClass,
						   toplevel, args, nargs);

		/*
		 * Make a form to put the buttons in.
		 */
		form = XtCreateWidget("Buttons", formWidgetClass,
				      confirmwidget, NULL, 0);

		nwidgets = -1;

		/*
		 * Confirmation button.
		 */
		nargs = 0;
		SetArg(XtNcallback, confirm_callbacks);
		widgets[++nwidgets] = XtCreateWidget("Confirm",
						     commandWidgetClass,
						     form, args, nargs);

		/*
		 * Cancellation button.
		 */
		nargs = 0;
		SetArg(XtNcallback, cancel_callbacks);
		SetArg(XtNfromHoriz, widgets[nwidgets]);
		widgets[++nwidgets] = XtCreateWidget("Cancel",
						     commandWidgetClass,
						     form, args, nargs);

		/*
		 * Let the shell widget know we're here.
		 */
		/* XtManageChildren(widgets, XtNumber(widgets)); */
		XtManageChildren(widgets, nwidgets+1);
		XtManageChild(form);

		XtRealizeWidget(confirmwidget);
		inited = True;
	}
	else {
		/*
		 * Reset the confirmation box position.
		 */
		nargs = 0;
		SetArg(XtNx, root_x);
		SetArg(XtNy, root_y);
		XtSetValues(confirmwidget, args, nargs);

		/*
		 * Reset the callbacks.
		 */
		nargs = 0;
		SetArg(XtNcallback, confirm_callbacks);
		XtSetValues(widgets[0], args, nargs);

		nargs = 0;
		SetArg(XtNcallback, cancel_callbacks);
		XtSetValues(widgets[1], args, nargs);
	}

	/*
	 * Pop up the confirmation box.
	 */
	XtPopup(confirmwidget, XtGrabExclusive);
}

/*
 * ClearConfirm - get rid of the confirmation box.
 */
void
ClearConfirm()
{
	XtPopdown(confirmwidget);
}
