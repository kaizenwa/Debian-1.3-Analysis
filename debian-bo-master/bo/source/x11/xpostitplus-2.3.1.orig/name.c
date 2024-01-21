#ifndef lint
static char	*RCSid = "$Id: name.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * name.c - handle note naming requests made by the user.
 *
 * Michael J. Hammel
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.org
 *
 * $Log: name.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Text.h>
#include <X11/Shell.h>
#include <stdio.h>

#include "xpostit.h"

Widget				dialogwidget;
Widget				namewidget;

/*
 * NameIt - put up a window prompt for the notes name
 */
void
NameIt(pn, confirm_callbacks, cancel_callbacks)
PostItNote *pn;
XtCallbackRec *confirm_callbacks, *cancel_callbacks;
{
	Arg 			args[10];
	Window 			root, child;
	unsigned int 		buttons;
	register int 		nargs, nwidgets;
	static Boolean 		inited = False;
	static Widget 		widgets[3];
	int 			root_x, root_y, child_x, child_y;

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
		namewidget = XtCreatePopupShell("Name",
				transientShellWidgetClass,
				toplevel, args, nargs);

		/*
		 * Make a dialog to put the buttons in.
		 */
		nargs = 0;
		SetArg(XtNlabel, PostItNoteDialog);
		if ( pn->pn_name == NULL )
		{
			SetArg(XtNvalue, "");
		}
		else
		{
			SetArg(XtNvalue, pn->pn_name);
		}
		SetArg(XtNresize, XawtextResizeWidth);
		SetArg(XtNresizable, TRUE);
		dialogwidget = XtCreateManagedWidget("Dialog", dialogWidgetClass,
				      namewidget, args, nargs);

		nwidgets = -1;

		/*
		 * Confirmation button.
		 */
		nargs = 0;
		SetArg(XtNcallback, confirm_callbacks);
		widgets[++nwidgets] = XtCreateWidget("Confirm",
					commandWidgetClass,
					dialogwidget, args, nargs);

		/*
		 * Cancellation button.
		 */
		nargs = 0;
		SetArg(XtNcallback, cancel_callbacks);
		SetArg(XtNfromHoriz, widgets[nwidgets]);
		widgets[++nwidgets] = XtCreateWidget("Cancel",
					commandWidgetClass,
					dialogwidget, args, nargs);

		/*
		 * Let the shell widget know we're here.
		 */
		XtManageChildren(widgets, nwidgets+1);

		inited = True;
	}
	else {
		/*
		 * Reset the confirmation box position.
		 */
		nargs = 0;
		SetArg(XtNx, root_x);
		SetArg(XtNy, root_y);
		XtSetValues(namewidget, args, nargs);

		/* clear the dialog text */
		nargs = 0;
		if ( pn->pn_name == NULL )
		{
			SetArg(XtNvalue, "");
		}
		else
		{
			SetArg(XtNvalue, pn->pn_name);
		}
		XtSetValues(dialogwidget, args, nargs);

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
	XtPopup(namewidget, XtGrabNonexclusive);

	/*
	 * Reset the width of the dialog box - this sucks, since I can't tell
	 * the width, in pixels, of the font being used for the text field.
	 * The hack is to allow the user to specify a width via a resource.
	 * The problem is a limitation in the Athena Widget set.
	 */

	if ( pn->pn_name == NULL )
	{
		nargs = 0;
		SetArg(XtNwidth, (app_res.name_width*30)); 
		XtSetValues(namewidget, args, nargs);
	}
	else
	{
		nargs = 0;
		SetArg(XtNwidth, (app_res.name_width*(strlen((char *)pn->pn_name)+10)));
		XtSetValues(namewidget, args, nargs);
	}

}

/*
 * ClearName - get rid of the confirmation box.
 */
void
ClearName()
{
	XtPopdown(namewidget);
}
