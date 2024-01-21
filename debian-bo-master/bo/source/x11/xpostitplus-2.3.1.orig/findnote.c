#ifndef lint
static char	*RCSid = "$Id$";
#endif

/*
 * findnote.c - routines to handle the alarm feature.
 *
 * Michael J. Hammel
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.net
 *
 * $Log$
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Shell.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "xpostit.h"

/* globals */

/* local variables */
Widget		find_widget, find_viewport, find_viewform;

/* external variables */
extern Widget			toplevel;

/* external routines */
extern void ErrPopUp();

/* prototypes */
void FindPopDown();
void FindPopUpChild();

/*
 * CreateFindNotePrompt - create a scrollable window in which 
 * all notes can be listed
 */
void
CreateFindNotePrompt(notes, hidden)
PostItNote	*notes;
Boolean		hidden;
{
	Arg 		args[15];
	register int 	nargs;
	XtCallbackRec	callbacks[2];
	Boolean		first;
	Widget		entry[DefaultMaxNotes];
	Widget		find_form, find_cancel;
	PostItNote	*pn;
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;
	int		maxwidth, i, last;
	Dimension	width;

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
	      &root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	if ( hidden )
	{
		SetArg(XtNtitle, PostItNoteList);
	}
	else
	{
		SetArg(XtNtitle, PostItFindPrompt);
	}
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	find_widget = XtCreatePopupShell(
			"FindShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	find_form = XtCreateManagedWidget(
			"FindForm", 
			formWidgetClass,
			find_widget, 
			args, nargs);

	/* the scrollable window */
	nargs = 0;
	SetArg(XtNallowHoriz, TRUE);
	SetArg(XtNallowVert, TRUE);
	SetArg(XtNuseBottom, TRUE);
	find_viewport = XtCreateManagedWidget(
			"FindViewport", 
			viewportWidgetClass,
			find_form, 
			args, nargs);

	/*
	 * create the form inside which all the buttons go
	 */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	find_viewform = XtCreateManagedWidget(
			"FindViewForm", 
			formWidgetClass,
			find_viewport, 
			args, nargs);


	/* now create each button */
	first = True;
	maxwidth=0;
	i=0;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		if ( hidden ) 
		{
			if ( pn->pn_hidden )
			{
				nargs = 0;
				SetArg(XtNborderWidth, 1);
				SetArg(XtNlabel, pn->pn_name);
				SetArg(XtNshapeStyle, XmuShapeRectangle);
				SetArg(XtNhorizDistance, 0);
				if ( !first )
				{
					SetArg(XtNfromVert, entry[i-1]);
					SetArg(XtNvertDistance, 0);
				}
				entry[i] = XtCreateManagedWidget(
						"FindButton", 
						commandWidgetClass,
						find_viewform, 
						args, nargs);
				XtAddCallback(entry[i], XtNcallback, 
					FindPopUpChild, (XtPointer)pn);
		
				nargs = 0;
				SetArg(XtNwidth, &width);
				XtGetValues(entry[i], args, nargs);
				if (width>maxwidth)
					maxwidth=width;
		
				first=False;
				i++;
			}
		}
		else
		{
			nargs = 0;
			SetArg(XtNborderWidth, 1);
			SetArg(XtNlabel, pn->pn_name);
			SetArg(XtNshapeStyle, XmuShapeRectangle);
			SetArg(XtNhorizDistance, 0);
			if ( !first )
			{
				SetArg(XtNfromVert, entry[i-1]);
				SetArg(XtNvertDistance, 0);
			}
			entry[i] = XtCreateManagedWidget(
					"FindButton", 
					commandWidgetClass,
					find_viewform, 
					args, nargs);
			XtAddCallback(entry[i], XtNcallback, 
				FindPopUpChild, (XtPointer)pn);
	
			nargs = 0;
			SetArg(XtNwidth, &width);
			XtGetValues(entry[i], args, nargs);
			if (width>maxwidth)
				maxwidth=width;
	
			first=False;
			i++;
		}
	}

	if ( first )
	{
		if ( hidden )
			ErrPopUp("There are no hidden notes." );
		else
			ErrPopUp("You have no notes.");
		FindPopDown();
		return;
	}

	/* readjust the widths of all buttons */
	last=i;
	for (i=0;i<last;i++)
	{
		nargs = 0;
		SetArg(XtNwidth, width);
		SetArg(XtNleft, XtChainLeft);
		SetArg(XtNright, XtChainRight);
		SetArg(XtNtop, XtChainTop);
		SetArg(XtNbottom, XtChainTop);
		XtSetValues(entry[i], args, nargs);
	}

	/* reset height of viewport, so its not too big */
	nargs = 0;
	SetArg(XtNheight, 100);
	SetArg(XtNwidth, 200);
	XtSetValues(find_viewport,args, nargs);


	/* a cancel button */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(FindPopDown, NULL);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNfromVert, find_viewport);
	SetArg(XtNlabel, "Cancel");
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNtop, XtChainBottom);
	SetArg(XtNbottom, XtChainBottom);
	find_cancel = XtCreateManagedWidget(
			"FindCancel", 
			commandWidgetClass,
			find_form, 
			args, nargs);

	XtPopup( find_widget, XtGrabNonexclusive );
}

/*
 * FindPopDown - close the Find window
 */
void
FindPopDown(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	XtPopdown(find_widget);
	if ( find_widget != NULL )
	{
		XtDestroyWidget(find_widget);
		find_widget = NULL;
	}
}

/*
 * FindPopUpChild - bring the note to the current location
 */
void
FindPopUpChild(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	Arg		args[3];
	int		nargs;
	PostItNote	*pn;
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;

	pn = (PostItNote *)client_data;

	/*
	 * if its a hidden note, then just unhide it
	 */
	if ( pn->pn_hidden )
	{
		/* XtPopup( pn->pn_shellwidget, XtGrabNonexclusive );
		 */
		XtPopup(pn->pn_shellwidget, XtGrabNone);
		pn->pn_hidden = False;
		FindPopDown(NULL, NULL, NULL);
		return;
	}	

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
	      &root_x, &root_y, &child_x, &child_y, &buttons);

	/* pop up the note at the cursor */
	nargs = 0;
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	XtSetValues(pn->pn_shellwidget, args, nargs);
	pn->pn_shellx = root_x;
	pn->pn_shelly = root_y;
	XtPopup( pn->pn_shellwidget, XtGrabNonexclusive );

	/* and close the Find window */
	FindPopDown(NULL, NULL, NULL);
}

