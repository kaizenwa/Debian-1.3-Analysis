#ifndef lint
static char	*RCSid = "$Id$";
#endif

/*
 * insert.c - routines to handle special inserts into a note
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
#include <X11/Shell.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "xpostit.h"

/* globals */

/* local variables */
static Widget 		insert_widget;
static Widget		insert_boxes[12];
static Widget		insert_buttons[3];
static Widget		insert_leftarrows[11];
static int			active_item;

/* external variables */
extern Widget			toplevel;

/* external routines */
extern PostItNote	*FindNote();
extern InsertText();
void InsertPopDown();
void InsertAccept();
void InsertCancel();
void InsertLeftArrowCB();

/*
 * CreateInsertPrompt - create a prompt for insert information
 */
void
CreateInsertPrompt(pn)
PostItNote 	*pn;
{
	Arg 				args[30];
	register int 	nargs;
	XtCallbackRec	callbacks[2];

	Widget		insert_form;
	Widget		insert_forms[12];
	Widget		insert_labels[11];
	Dimension	width;
	Window 		root, child;
	int 			root_x, root_y, child_x, child_y;
	int			i;
	unsigned int 	buttons;

	/* initialize marker */
	active_item=0;

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItInsertPrompt);
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	insert_widget = XtCreatePopupShell(
			"InsertShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	insert_form = XtCreateManagedWidget(
			"InsertForm", 
			formWidgetClass,
			insert_widget, 
			args, nargs);

	/* the boxes, one for each line of input */
	for ( i=0; i<12; i++)
	{
		nargs = 0;
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNorientation, XtorientHorizontal ); 
		SetArg( XtNvSpace, 0 ); 
		SetArg( XtNhSpace, 0 ); 
		if ( i!= 0 )
		{
			SetArg( XtNfromVert, insert_boxes[i-1] ); 
		}
		insert_boxes[i] = XtCreateManagedWidget(
				"InsertBox", 
				boxWidgetClass,
				insert_form, 
				args, nargs);
	}

	/*
	 * the forms, one for each box, which provides constraints
	 * for other windows inside that box.
	 */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	for ( i=0; i<11; i++)
	{
		insert_forms[i] = XtCreateManagedWidget(
			"InsertForm", 
			formWidgetClass,
			insert_boxes[i], 
			args, nargs);
	}

	/*
	 * The selection button in each box
	 */
	for ( i=0; i<11; i++)
	{
		bzero(callbacks, sizeof(callbacks));
		SetCallback(InsertLeftArrowCB, (XtPointer)i);
		nargs = 0;
		SetArg( XtNcallback, callbacks);
		SetArg( XtNlabel, "-");
		SetArg( XtNborderWidth, 1);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNshapeStyle, XmuShapeRectangle ); 
		insert_leftarrows[i] = XtCreateManagedWidget(
			"InsertArrow", 
			commandWidgetClass,
			insert_forms[i], 
			args, nargs);
	}

	/* The labels of each box */
	for ( i=0; i<11; i++)
	{
		nargs = 0;
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNfromHoriz, insert_leftarrows[i] ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNshapeStyle, XmuShapeRectangle ); 
		switch(i)
		{
			case 0: SetArg(XtNlabel, "mm/dd/yy"); break;
			case 1: SetArg(XtNlabel, "<mm/dd/yy>"); break;
			case 2: SetArg(XtNlabel, "(mm/dd/yy)"); break;
			case 3: SetArg(XtNlabel, "mm/dd/yy - hh:mm"); break;
			case 4: SetArg(XtNlabel, "mm/dd/yy(hh:mm)"); break;
			case 5: SetArg(XtNlabel, "Jan 1, 1996"); break;
			case 6: SetArg(XtNlabel, "Jan 1, 1996 - hh:mm"); break;
			case 7: SetArg(XtNlabel, "Jan 1, 1996 (hh:mm)"); break;
			case 8: SetArg(XtNlabel, "Tuesday Jan 1, 1996"); break;
			case 9: SetArg(XtNlabel, "Tuesday Jan 1, 1996 - hh:mm"); break;
			case 10: SetArg(XtNlabel, "Tuesday Jan 1, 1996 (hh:mm)"); break;
		}
		insert_labels[i] = XtCreateManagedWidget(
			"InsertLabel", 
			labelWidgetClass,
			insert_forms[i], 
			args, nargs);
	}

	/*
	 * now make all the labels the same width
	 */
	nargs = 0;
	SetArg( XtNwidth, &width);

	/* ugly - hard code the longest box! */
	XtGetValues ( insert_labels[9], args, nargs );

	nargs = 0;
	SetArg( XtNwidth, width);
	SetArg( XtNjustify, XtJustifyLeft);
	for ( i=0; i<11; i++)
	{
		XtSetValues ( insert_labels[i], args, nargs );
	}
	

	/*
	 * create the Cancel/Accept command buttons 
	 */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(InsertAccept, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Accept");
	insert_buttons[1] = XtCreateManagedWidget(
		"InsertButtons", 
		commandWidgetClass,
		insert_boxes[11], 
		args, nargs);

	bzero(callbacks, sizeof(callbacks));
	SetCallback(InsertCancel, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Cancel");
	insert_buttons[0] = XtCreateManagedWidget(
		"InsertButtons", 
		commandWidgetClass,
		insert_boxes[11], 
		args, nargs);

	XtPopup( insert_widget, XtGrabNonexclusive );
}

/*
 * InsertAccept - Accept selected date value
 *	This routine has the potential to be very OS specific.
 */
void
InsertAccept(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn = (PostItNote *)client_data;
	char			cmd[1024];
	time_t		time_tp;
	struct tm	*tm_tp;
	char			dayname[12];
	char			year[5];
	char			month[10];

	/* get todays date and current time */
	time(&time_tp);
	tm_tp = localtime(&time_tp);

	/*
	 * convert numeric values to strings now, even if we don't use them,
	 * since we need these in more than one place.
	 */
	switch ( tm_tp->tm_wday )
	{
		case 0:	sprintf(dayname, "Sunday"); break;
		case 1:	sprintf(dayname, "Monday"); break;
		case 2:	sprintf(dayname, "Tuesday"); break;
		case 3:	sprintf(dayname, "Wednesday"); break;
		case 4:	sprintf(dayname, "Thursday"); break;
		case 5:	sprintf(dayname, "Friday"); break;
		case 6:	sprintf(dayname, "Saturday"); break;
	}
	switch ( tm_tp->tm_mon )
	{
		case 0: sprintf(month, "Jan"); break;
		case 1: sprintf(month, "Feb"); break;
		case 2: sprintf(month, "Mar"); break;
		case 3: sprintf(month, "Apr"); break;
		case 4: sprintf(month, "May"); break;
		case 5: sprintf(month, "Jun"); break;
		case 6: sprintf(month, "Jul"); break;
		case 7: sprintf(month, "Aug"); break;
		case 8: sprintf(month, "Sept"); break;
		case 9: sprintf(month, "Oct"); break;
		case 10: sprintf(month, "Nov"); break;
		case 11: sprintf(month, "Dec"); break;
	}
	if ( tm_tp->tm_year > 100 )
		sprintf(year,"20");
	else
		sprintf(year,"19");

	/*
	 * increment the numeric month, since localtime()
	 * returns the month from 0 to 11.
	 */
	tm_tp->tm_mon++;

	switch (active_item)
	{
		case 0:
			sprintf (cmd, 
				"%02d/%02d/%02d", tm_tp->tm_mon, tm_tp->tm_mday, tm_tp->tm_year );
			break;

		case 1:
			sprintf (cmd, 
				"<%02d/%02d/%02d>", tm_tp->tm_mon, tm_tp->tm_mday, tm_tp->tm_year );
			break;

		case 2:
			sprintf (cmd, 
				"(%02d/%02d/%02d)", tm_tp->tm_mon, tm_tp->tm_mday, tm_tp->tm_year );
			break;

		case 3:
			sprintf (cmd, 
				"%02d/%02d/%02d - %02d:%02d", 
				tm_tp->tm_mon, tm_tp->tm_mday, tm_tp->tm_year, 
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

		case 4:
			sprintf (cmd, 
				"%02d/%02d/%02d(%02d:%02d)", 
				tm_tp->tm_mon, tm_tp->tm_mday, tm_tp->tm_year, 
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

		case 5:
			sprintf (cmd, 
				"%s %d, %s%d", month, tm_tp->tm_mday, year, tm_tp->tm_year);
			break;

		case 6:
			sprintf (cmd, 
				"%s %d, %s%d - %02d:%02d", 
				month, tm_tp->tm_mday, year, tm_tp->tm_year,
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

		case 7:
			sprintf (cmd, 
				"%s %d, %s%d (%02d:%02d)", 
				month, tm_tp->tm_mday, year, tm_tp->tm_year,
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

		case 8:
			sprintf (cmd, 
				"%s %s %d, %s%d", 
				dayname, month, tm_tp->tm_mday, year, tm_tp->tm_year);
			break;

		case 9:
			sprintf (cmd, 
				"%s %s %d, %s%d - %02d:%02d", 
				dayname, month, tm_tp->tm_mday, year, tm_tp->tm_year,
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

		case 10:
			sprintf (cmd, 
				"%s %s %d, %s%d (%02d:%02d)", 
				dayname, month, tm_tp->tm_mday, year, tm_tp->tm_year,
				tm_tp->tm_hour, tm_tp->tm_min );
			break;

	}

	/* stuff the output into the note */
	InsertText(pn, cmd, False, TEXT_TYPE);

	XtSetSensitive ( pn->pn_savewidget, True );
	InsertPopDown();
}

/*
 * InsertCancel - Cancel an insert
 */
void
InsertCancel(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	InsertPopDown();
}

/*
 * InsertPopDown - removes the Insert Dialog box
 */
void
InsertPopDown()
{
	XtPopdown( insert_widget );
	XtDestroyWidget(insert_widget);

}


/*
 * InsertLeftArrowcB - handle a left arrow click
 */
void
InsertLeftArrowCB(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	int	nargs;
	Arg	args[3];

	nargs=0;
	SetArg( XtNlabel, ">");
	XtSetValues ( insert_leftarrows[(int)client_data], args, nargs );

	active_item=(int)client_data;
}
