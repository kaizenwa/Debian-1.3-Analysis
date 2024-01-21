#ifndef lint
static char	*RCSid = "$Id$";
#endif

/*
 * email.c - routines to handle the email feature.
 *
 * Michael J. Hammel
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.net
 *
 * $Log$
 *
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Shell.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "xpostit.h"

/* globals */

/* local variables */
static Widget		email_widget, email_text;

/* external variables */
extern Widget		toplevel;

/* external routines */
extern void ErrPopUp();

/* prototypes */
static void EmailPopDown();
static void EmailNote();

/*
 * CreateEmailNotePrompt - create a window in which 
 * the email address can be entered.
 */
void
CreateEmailNotePrompt(pn)
PostItNote	*pn;
{
	Arg 		args[15];
	register int 	nargs;
	XtCallbackRec	callbacks[2];
	Widget		email_form, email_label, 
			email_cancel, email_accept;
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;
	Dimension	width;

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
	      &root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItEmailPrompt);
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	email_widget = XtCreatePopupShell(
			"EmailShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	email_form = XtCreateManagedWidget(
			"EmailForm", 
			formWidgetClass,
			email_widget, 
			args, nargs);

	/* the label window */
	nargs = 0;
	SetArg(XtNlabel, "Enter the email address of the intended recipient");
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	email_label = XtCreateManagedWidget(
			"EmailLabel", 
			labelWidgetClass,
			email_form, 
			args, nargs);
	nargs = 0;
	SetArg(XtNwidth, &width);
	XtGetValues(email_label, args, nargs);

	nargs = 0;
	SetArg(XtNborderWidth, 1);
	SetArg(XtNwidth, width);
	SetArg(XtNstring, "");
	SetArg(XtNvertDistance, 0);
	SetArg(XtNfromVert, email_label);
	SetArg(XtNeditType, XawtextEdit);
	SetArg(XtNwrap, XawtextWrapNever);
	SetArg(XtNscrollVertical, XawtextScrollNever);
	SetArg(XtNscrollHorizontal, XawtextScrollWhenNeeded);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	email_text = XtCreateManagedWidget(
			"EmailText", 
			asciiTextWidgetClass,
			email_form, 
			args, nargs);

	/* an accept button */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(EmailNote, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNfromVert, email_text);
	SetArg(XtNlabel, "Accept");
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNtop, XtChainBottom);
	SetArg(XtNbottom, XtChainBottom);
	email_accept = XtCreateManagedWidget(
			"EmailAccept", 
			commandWidgetClass,
			email_form, 
			args, nargs);

	/* a cancel button */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(EmailPopDown, NULL);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNfromVert, email_text);
	SetArg(XtNfromHoriz, email_accept);
	SetArg(XtNlabel, "Cancel");
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNtop, XtChainBottom);
	SetArg(XtNbottom, XtChainBottom);
	email_cancel = XtCreateManagedWidget(
			"EmailCancel", 
			commandWidgetClass,
			email_form, 
			args, nargs);

	XtPopup( email_widget, XtGrabNonexclusive );
}

/*
 * EmailPopDown - close the Email window
 */
static void
EmailPopDown(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	XtPopdown(email_widget);
	if ( email_widget != NULL )
	{
		XtDestroyWidget(email_widget);
		email_widget = NULL;
	}
}



/*
 * EmailNote - email a note.
 */
static void
EmailNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	FILE	*fp;
	int	len;
	char	tmpfile[512];
	char	syscmd[1024];
	char	emailcmd[1024];
	String	addressee;
	Arg	args[5];
	int	nargs;

	pn = (PostItNote *)client_data;

	/*
	 * Save the text to a temporary file
	 */
	sprintf(tmpfile,"%s/xp%d", app_res.tmp_dir, (int)getpid());
	if ((fp = fopen(tmpfile, "w")) == NULL) {
		fprintf(stderr, "xpostit: ");
		perror(tmpfile);
		sprintf(syscmd,"Can't open tempoary file for\nwriting: %s", tmpfile);
		ErrPopUp(syscmd);
		return;
	}
	len = strlen(pn->pn_text);
	if (len)
		fwrite(pn->pn_text, sizeof(char), len, fp);
	else
	{
		ErrPopUp("No text in note to email!");
		return;
	}
	fclose(fp);

	/*
	 * Use the emailcmd resource to determine how to email the note
	 */
	nargs=0;
	SetArg(XtNstring, &addressee);
	XtGetValues(email_text, args, nargs);

	sprintf (emailcmd, app_res.email_cmd, pn->pn_name, addressee);
	sprintf (syscmd, "cat %s | %s", tmpfile, emailcmd );
	system ( syscmd );

	/*
	 * clean up the temporary file
	 */
	sprintf( syscmd, "rm %s", tmpfile );
	system ( syscmd );

	EmailPopDown(NULL, NULL, NULL);
}
