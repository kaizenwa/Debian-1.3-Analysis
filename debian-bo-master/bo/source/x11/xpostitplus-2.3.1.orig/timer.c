#ifndef lint
static char	*RCSid = "$Id: list.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * timer.c - routines to handle the alarm feature.
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

#include "xpostit.h"

/* globals */

/* local variables */
static Widget 		timer_widget;
static Widget		timer_texts[5];
static Widget		timer_boxes[5];
static Widget		timer_buttons[3];

/* external variables */
extern Widget			toplevel;
extern Pixmap			alarmon_pixmap;
extern Pixmap			alarmoff_pixmap;

/* external routines */
extern PostItNote	*FindNote();
void TimerPopDown();
void TimerAccept();
void TimerCancel();
void TimerLeftArrowCB();
void TimerRightArrowCB();

/*
 * CreateTimerPrompt - create a prompt for alarm information
 */
void
CreateTimerPrompt(pn)
PostItNote 	*pn;
{
	Arg 		args[30];
	register int 	nargs;
	XtCallbackRec	callbacks[2];

	Widget		timer_form;
	Widget		timer_forms[5];
	Widget		timer_labels[5];
	Widget		timer_leftarrows[5], timer_rightarrows[5];
	Dimension	width, height;
	char		buf[10];
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;

	int		i, maxwidth;

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItTimerPrompt);
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	timer_widget = XtCreatePopupShell(
			"TimerShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	timer_form = XtCreateManagedWidget(
			"TimerForm", 
			formWidgetClass,
			timer_widget, 
			args, nargs);

	/* the boxes, one for each line of input */
	for ( i=0; i<5; i++)
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
		SetArg( XtNvSpace, 2 ); 
		SetArg( XtNhSpace, 2 ); 
		if ( i!= 0 )
		{
			SetArg( XtNfromVert, timer_boxes[i-1] ); 
		}
		timer_boxes[i] = XtCreateManagedWidget(
				"TimerBox", 
				boxWidgetClass,
				timer_form, 
				args, nargs);
	}


	/*
	 * the forms, one for each box, which provides constraints
	 * for other windows inside that box.
	 */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	for ( i=0; i<4; i++)
	{
		timer_forms[i] = XtCreateManagedWidget(
			"TimerForm", 
			formWidgetClass,
			timer_boxes[i], 
			args, nargs);
	}

	/* The labels of each box */
	for ( i=0; i<4; i++)
	{
		nargs = 0;
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		switch(i)
		{
			case 0: SetArg(XtNlabel, "Month"); break;
			case 1: SetArg(XtNlabel, "Day"); break;
			case 2: SetArg(XtNlabel, "Hour"); break;
			case 3: SetArg(XtNlabel, "Minute"); break;
		}
		timer_labels[i] = XtCreateManagedWidget(
			"TimerLabel", 
			labelWidgetClass,
			timer_forms[i], 
			args, nargs);
	}

	/*
	 * now make all the labels the same width
	 */
	nargs = 0;
	SetArg( XtNwidth, &width);
	XtGetValues ( timer_labels[3], args, nargs );
	nargs = 0;
	SetArg( XtNwidth, width);
	SetArg( XtNjustify, XtJustifyCenter);
	for ( i=0; i<4; i++)
	{
		XtSetValues ( timer_labels[i], args, nargs );
	}
	

	/*
	 * The arrow buttons and text fields in each box
	 */
	maxwidth=0;
	for ( i=0; i<4; i++)
	{
		bzero(callbacks, sizeof(callbacks));
		SetCallback(TimerLeftArrowCB, (XtPointer)i);
		nargs = 0;
		SetArg( XtNcallback, callbacks);
		SetArg( XtNlabel, "<");
		SetArg( XtNborderWidth, 1);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNfromHoriz, timer_labels[i] ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNshapeStyle, XmuShapeRectangle ); 
		timer_leftarrows[i] = XtCreateManagedWidget(
			"TimerArrow", 
			commandWidgetClass,
			timer_forms[i], 
			args, nargs);

		nargs = 0 ;
		SetArg ( XtNheight, &height ); 
		XtGetValues ( timer_leftarrows[i], args, nargs );
 
		/*
		 * Set the text fields to the notes stored values
		 */
		switch (i)
		{
			case 0: sprintf(buf, "%d", pn->pn_alarm_mon+1); break;
			case 1: sprintf(buf, "%d", pn->pn_alarm_day); break;
			case 2: sprintf(buf, "%d", pn->pn_alarm_hour); break;
			case 3: sprintf(buf, "%d", pn->pn_alarm_min); break;
			default : sprintf(buf, "   "); break;
		}
	
		nargs = 0;
		SetArg( XtNlabel, buf);
		SetArg( XtNborderWidth, 1 ); 
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNfromHoriz, timer_leftarrows[i] ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNleftMargin, 6 ); 
		SetArg( XtNrightMargin, 6 ); 
		timer_texts[i] = XtCreateManagedWidget(
			"TimerText",
			labelWidgetClass,
			timer_forms[i],
			args, nargs );
		nargs = 0;
		SetArg( XtNwidth, &width);
		XtGetValues(timer_texts[i], args, nargs);
		if ( width > maxwidth )
			maxwidth=width;

		bzero(callbacks, sizeof(callbacks));
		SetCallback(TimerRightArrowCB, (XtPointer)i);
		nargs = 0;
		SetArg( XtNcallback, callbacks);
		SetArg( XtNheight, height ); 
		SetArg( XtNborderWidth, 1);
		SetArg( XtNlabel, ">");
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNfromHoriz, timer_texts[i] ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNshapeStyle, XmuShapeRectangle ); 
		timer_rightarrows[i] = XtCreateManagedWidget(
			"TimerArrow", 
			commandWidgetClass,
			timer_forms[i], 
			args, nargs);
	}

	/* readjust labels and buttons to look consistant */
	nargs = 0;
	SetArg( XtNheight, &height);
	XtGetValues(timer_texts[0], args, nargs);

	for ( i=0; i<4; i++)
	{
		nargs = 0;
		SetArg( XtNwidth, width);
		XtSetValues(timer_texts[i], args, nargs);
	}
	for ( i=0; i<4; i++)
	{
		nargs = 0;
		SetArg( XtNheight, height);
		XtSetValues(timer_texts[i], args, nargs);
		XtSetValues(timer_leftarrows[i], args, nargs);
		XtSetValues(timer_rightarrows[i], args, nargs);
		XtSetValues(timer_labels[i], args, nargs);
	}

	/*
	 * create the Cancel/Accept command buttons 
	 */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(TimerAccept, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Accept");
	timer_buttons[1] = XtCreateManagedWidget(
		"TimerButtons", 
		commandWidgetClass,
		timer_boxes[4], 
		args, nargs);

	bzero(callbacks, sizeof(callbacks));
	SetCallback(TimerCancel, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Cancel");
	timer_buttons[0] = XtCreateManagedWidget(
		"TimerButtons", 
		commandWidgetClass,
		timer_boxes[4], 
		args, nargs);

	XtPopup( timer_widget, XtGrabNonexclusive );
}

/*
 * TimerAccept - Accept a new alarm
 */
void
TimerAccept(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn = (PostItNote *)client_data;
	int			i, value, nargs;
	Arg			args[3];
	String		string;

	for (i=0; i<4; i++)
	{
		nargs = 0;
		SetArg(XtNlabel, &string);
		XtGetValues ( timer_texts[i], args, nargs );
		value = (int)atoi(string);
		switch (i)
		{
			/* Month has to be adjusted to what localtime() uses. */
			case 0: pn->pn_alarm_mon = value-1; break;
			case 1: pn->pn_alarm_day = value; break;
			case 2: pn->pn_alarm_hour = value; break;
			case 3: pn->pn_alarm_min = value; break;
		}
	}

	pn->pn_alarm = True;

	XtSetSensitive ( pn->pn_unsettimewidget, True );
	XtSetSensitive ( pn->pn_settimewidget, False );
	XtSetSensitive ( pn->pn_savewidget, True );

	/* set the alarm icon on for this note */
	nargs = 0;
	SetArg(XtNlabel, " ");
	SetArg(XtNbitmap, alarmon_pixmap);
	XtSetValues(pn->pn_alarmwidget, args, nargs);

	XtSetSensitive ( pn->pn_savewidget, True );
	TimerPopDown();
}

/*
 * TimerCancel - Cancel an alarm
 */
void
TimerCancel(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	TimerPopDown();
}

/*
 * TimerPopDown - removes the Error Dialog box
 */
void
TimerPopDown()
{
	XtPopdown( timer_widget );
	XtDestroyWidget(timer_widget);

}


/*
 * TimerRightArrowcB - handle a right arrow click
 */
void
TimerRightArrowCB(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	int	index = (int) client_data;
	int	max=0, min=0;
	String	string;
	char	buf[10];
	int	nargs, value;
	Arg	args[3];

	/* set limits based on which field is being updated */
	switch ( index )
	{
		case 0: max=12; min=1; break; /* month */
		case 2: max=23; min=0; break; /* hour */
		case 3: max=59; min=0; break; /* minute */

		/*
		 * setting the day max is dependent on which month
		 * is currently set
		 */
		case 1: 
		min = 1;
		nargs = 0;
		/* SetArg(XtNstring, &string);
		 */
		SetArg(XtNlabel, &string);
		XtGetValues ( timer_texts[0], args, nargs );
		value = (int)atoi(string);
		switch ( value )
		{
			case 1:
			case 3:
			case 5:
			case 7:
			case 8:
			case 10:
			case 12: max=31; break;
			case 2: max=29; break;
			default: max=30; break;
		}
	}

	/* increment text field */
	nargs = 0;
	/* SetArg(XtNstring, &string);
	 */
	SetArg(XtNlabel, &string);
	XtGetValues ( timer_texts[index], args, nargs );
	value = (int)atoi(string) + 1;
	if ( value > max )
		value = min;
	sprintf ( buf, "%d", value);
	nargs = 0;
	/* SetArg(XtNstring, buf);
	 */
	SetArg(XtNlabel, buf);
	XtSetValues ( timer_texts[index], args, nargs );
		

}

/*
 * TimerLeftArrowcB - handle a left arrow click
 */
void
TimerLeftArrowCB(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	int	index = (int) client_data;
	int	max=0, min=0;
	String	string;
	char	buf[10];
	int	nargs, value;
	Arg	args[3];

	/* set limits based on which field is being updated */
	switch ( index )
	{
		case 0: max=12; min=1; break; /* month */
		case 2: max=23; min=0; break; /* hour */
		case 3: max=59; min=0; break; /* minute */

		/*
		 * setting the day max is dependent on which month
		 * is currently set
		 */
		case 1:
		min=1;  
		nargs = 0;
		/* SetArg(XtNstring, &string);
		 */
		SetArg(XtNlabel, &string);
		XtGetValues ( timer_texts[0], args, nargs );
		value = (int)atoi(string);
		switch ( value )
		{
			case 1:
			case 3:
			case 5:
			case 7:
			case 8:
			case 10:
			case 12: max=31; break;
			case 2: max=29; break;
			default: max=30; break;
		}
	}

	/* decrement text field */
	nargs = 0;
	/* SetArg(XtNstring, &string);
	 */
	SetArg(XtNlabel, &string);
	XtGetValues ( timer_texts[index], args, nargs );
	value = (int)atoi(string) - 1;
	if ( value < min )
		value = max;
	sprintf ( buf, "%d", value);
	nargs = 0;
	/* SetArg(XtNstring, buf);
	 */
	SetArg(XtNlabel, buf);
	XtSetValues ( timer_texts[index], args, nargs );
		

}
