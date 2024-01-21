#ifndef lint
static char	*RCSid = "$Id: list.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * list.c - routines to handle the list of hidden notes.
 *          also includes code for creating and handling the Error Dialog
 *
 * Michael J. Hammel
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.net
 *
 * $Log: list.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Shell.h>
#include <stdio.h>
#include <string.h>

#include "xpostit.h"

/* globals */
static Widget 		error, errdialog;	/* error dialog box */

/* local variables */

/* external variables */
extern Widget			toplevel;
extern Widget			notelist[DefaultMaxNotes];
extern PostItNote		*notes;

/* external routines */
void ErrPopDown();
void ErrPopUp();


/*
 * PopUpList - brings up the list of hidden notes
 */
void
PopUpList(
)
{
	CreateFindNotePrompt(notes, True);
}


/*
 * CreateErrorDialog - create an error dialog box
 */
void
CreateErrorDialog(string)
char	*string;
{
	Arg 		args[3];
	register int 	nargs;
	Widget		errconfirm;
	XtCallbackRec	callbacks[2];
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItErrDialog);
	error = XtCreatePopupShell(
			"ErrorShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/*
	 * Create the error dialog box
	 */
	errdialog = XtCreateManagedWidget(
				"errorDialog",
				dialogWidgetClass,
				error,
				NULL, 0);

	/*
	 * Create the error dialog confirmation button
	 */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(ErrPopDown, (XtPointer)error);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Continue");
	errconfirm = XtCreateManagedWidget(
				"errconfirm",
				commandWidgetClass,
				errdialog,
				args, nargs);

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

	nargs = 0;
	SetArg(XtNlabel, string);
	XtSetValues(errdialog, args, nargs);

	nargs = 0;
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	XtSetValues(error, args, nargs);
	XtPopup( error, XtGrabNone );
}


/*
 * ErrPopDown - removes the Error Dialog box
 */
void
ErrPopDown(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	if ( (Widget)client_data != NULL )
	{
		/* XtPopdown( (Widget)client_data );
		 */
		XtDestroyWidget((Widget)client_data);
	}
}

/*
 * ErrPopUp - pops up the Error Dialog Box
 * The original version of this had CreateErrorDialog create the shell
 * widget and this routine to fill in the text.  There were many reasons
 * for not doing it that way, but instead of changing all the old calls
 * to this routine, I just have it call CreateErrorDialog() now.
 */
void
ErrPopUp(
	char	*string
)
{
	CreateErrorDialog(string);
}

