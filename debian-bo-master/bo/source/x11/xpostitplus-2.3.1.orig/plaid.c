#ifndef lint
static char	*RCSid = "$Header: /home/src/X/xpostit/xpostit/plaid.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * plaid.c - routines for manipulating the plaid widget.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * $Log: plaid.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 * Revision 1.3  90/06/14  11:20:42  davy
 * Ported to X11 Release 4.  Got rid of button callback from the Plaid widget,
 * since it handles its own actions now.
 * 
 * Revision 1.2  89/01/10  09:30:30  davy
 * Fixed menu call-up so that the menu is always on the screen.
 * 
 * Revision 1.1  89/01/10  09:13:59  davy
 * Initial revision
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <stdio.h>

#include "xpostit.h"
#include "Plaid.h"

Widget		plaidwidget;

/*
 * CreatePlaidWidget - create the plaid widget.
 */
void
CreatePlaidWidget()
{
	Arg args[4];
	register int nargs;
	Dimension width, height;

	/*
	 * Create the plaid widget.
	 */
	plaidwidget = XtCreateWidget("Plaid", plaidWidgetClass, toplevel,
				     NULL, 0);

	XtAddCallback(plaidwidget, XtNlowerCallback, LowerAllNotes, 0);
	XtAddCallback(plaidwidget, XtNraiseCallback, RaiseAllNotes, 0);
	XtAddCallback(plaidwidget, XtNtearoffCallback, MakeNewNote, 0);
	XtAddCallback(plaidwidget, XtNhideCallback, HideAllNotes, 0);
	XtAddCallback(plaidwidget, XtNshowCallback, UnHideAllNotes, 0);
	XtAddCallback(plaidwidget, XtNtshowCallback, ToggleShow, 0);
	XtAddCallback(plaidwidget, XtNtraiseCallback, ToggleRaise, 0);
	XtAddCallback(plaidwidget, XtNquitCallback, ByeBye, 0);
	
	/*
	 * Get the width and height of the widget.
	 */
	XtSetArg(args[0], XtNwidth, &width);
	XtSetArg(args[1], XtNheight, &height);
	XtGetValues(plaidwidget, args, 2);

	XtSetArg(args[0], XtNwidth, width);
	XtSetArg(args[1], XtNheight, height);

	/*
	 * If the user didn't set them, then we
	 * should set them to the defaults.
	 */
	nargs=0;
	if ((args[0].value == 0) || (args[1].value == 0)) {
		if (args[0].value == 0) {
			XtSetArg(args[0], XtNwidth, DefaultPlaidWidth);
			nargs++;
		}

		if (args[1].value == 0) {
			XtSetArg(args[1], XtNheight, DefaultPlaidHeight);
			nargs++;
		}

		XtSetValues(plaidwidget, args, nargs);
	}

	/*
	 * Inform the application shell we're here.
	 */
	XtManageChild(plaidwidget);
}
