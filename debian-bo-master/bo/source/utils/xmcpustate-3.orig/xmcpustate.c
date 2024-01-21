/*
*  Displays CPU state distribution
*/
#ifndef lint
static char rcsid[] = "$Header: /ai/lambda/X.V11R4/contrib/clients/xcpustate/RCS/xcpustate.c,v 1.2 90/02/20 00:33:36 xwindows Exp $";
#endif /*lint*/

#include <stdio.h>
#include <stdlib.h>

#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <X11/Xmu/Xmu.h>
#include "s.h"

#define DEF_INTERVAL	1
#define DEF_COUNT	-1
/* Incomplete display occurs when DEF_WIDTH < 350 */
#define DEF_WIDTH	350	
#define DEF_HEIGHT	20		/* default height of a bar */
#define MAX_COLORS	5	/* max no of colors supported */

/* Struct used for drawing the bars */
typedef struct {
	Widget bar;	/* bar id */
	Pixmap pixmap;	/* pixmap for this bar */
}bar_data;

/* Sets foreground color on a particular bar */
void set_color(Widget widget, char *color);

/* Callback in case of an expose event */
void redraw(Widget bar, int bar_id, XmDrawingAreaCallbackStruct *cbs);

char *progname;
static int defaultInterval 	= DEF_INTERVAL;
static int defaultCount 	= DEF_COUNT;
static int defaultWidth 	= DEF_WIDTH;
static int defaultHeight 	= DEF_HEIGHT;
static int count, interval, width, height;
static String idlecol, usercol, kernelcol, nicecol, systemcol;
static String *colors;
static String legenda[]={"Idle", "User", "Kernel", "Nice", "System"}; 

static int nbars;
static bar_data *bar;
static char **barnames;

GC gc;			/* Graphics context for this bar */

/* Application Resources - no particular widget */
static XtResource application_resources[] = {
	{"interval", "Interval", XtRInt, sizeof(int),
		(Cardinal)&interval, XtRInt, (caddr_t) &defaultInterval},
	{"count", "Count", XtRInt, sizeof(int),
		(Cardinal)&count, XtRInt, (caddr_t) &defaultCount},
	{"barwidth", "Barwidth", XtRInt, sizeof(int),
		(Cardinal)&width, XtRInt, (caddr_t) &defaultWidth},
	{"barheight", "Barheight", XtRInt, sizeof(int),
		(Cardinal)&height, XtRInt, (caddr_t) &defaultHeight},
	{"idlecol", "Idlecol", XtRString, sizeof(String),
		(Cardinal)&idlecol, XtRString, "green"},  
	{"usercol", "Usercol", XtRString, sizeof(String),
		(Cardinal)&usercol, XtRString, "magenta"},  
	{"kernelcol", "Kernelcol", XtRString, sizeof(String),
		(Cardinal)&kernelcol, XtRString, "yellow"},  
	{"nicecol", "Nicecol", XtRString, sizeof(String),
		(Cardinal)&nicecol, XtRString, "DarkRed"},  
	{"systemcol", "Systemcol", XtRString, sizeof(String),
		(Cardinal)&systemcol, XtRString, "black"},  
};

/*
*  Command line options table. The command line is parsed for these,
*  and it sets/overrides the appropriate values in the resource
*  database
*/
static XrmOptionDescRec optionDescList[] = {
	{"-interval",	".interval",	XrmoptionSepArg, (caddr_t)NULL},
	{"-count",	".count",	XrmoptionSepArg, (caddr_t)NULL},
	{"-barwidth",	".barwidth",	XrmoptionSepArg, (caddr_t)NULL},
	{"-barheight",	".barheight",	XrmoptionSepArg, (caddr_t)NULL},
	{"-idlecol",	".idlecol",	XrmoptionSepArg, (caddr_t)NULL},
	{"-usercol",	".usercol",	XrmoptionSepArg, (caddr_t)NULL},
	{"-kernelcol",	".kernelcol",	XrmoptionSepArg, (caddr_t)NULL},
	{"-nicecol",	".nicecol",	XrmoptionSepArg, (caddr_t)NULL},
	{"-systemcol",	".systemcol",	XrmoptionSepArg, (caddr_t)NULL},
};

/*
* Other default resources
*/
static String fallbackResources[]={
"*fontList:	 *-*-lucida-bold-r-*-*-*-120-*-*-p-*-*-*",
"*background:   SlateBlue",
NULL};


void
usage(void)
{
	fprintf(stderr, 
		"%s: display system resource information\n" 
		"\t[Xt options] [-count iterations] [-interval delay_seconds]\n"
		"\t[-barwidth width] [-barheight width]\n"
		"\t[-idlecol color] [-usercol color] [-kernelcol color]\n"
		"\t[-nicecol color] [-systemcol color]\n", progname);
	exit(-1);
}

char *
xmalloc(int n)
{
	extern char *malloc();
	char *cp = malloc((unsigned) n);
	if (cp == NULL) {
		(void) fprintf(stderr, "Couldn't malloc %d bytes\n", n);
		exit(-1);
	}
	return cp;
}

/*****
* This event handler sets the maximum and minimum size of a dialog
* to the same value, resulting in a non-resizable dialog.
* If this handler is not installed, the dialog is resizeable,
* but that messes up the proper displaying of the bars.
*****/
void
SetShellSize(Widget shell, XtPointer dummy, XConfigureEvent *Event)
{
	if (Event->type !=ConfigureNotify)
		return;
	XtVaSetValues(shell,
		XmNminWidth, Event->width, 
		XmNminHeight, Event->height, 
		XmNmaxWidth, Event->width,
		XmNmaxHeight, Event->height,
		NULL);
	XtRemoveEventHandler(shell, StructureNotifyMask, False,
		(XtEventHandler)SetShellSize, NULL);
}

int 
main(int argc, char **argv)
{
	int i = 0;
	static void update_display();
	Widget topLevel;
	Widget form;
	Widget frame = NULL, subform = NULL;
	Widget label = NULL;
	XGCValues gcv;
	XtAppContext context;

	extern char *strchr(/* const char *, char */);

	if ((progname = strchr(argv[0], '/')) == NULL)
		progname = argv[0];
	else
		progname++;

	nbars = num_bars();
	if (nbars == 0) 
	{
		fprintf(stderr, "num_bars returned 0 - something is wrong\n");
		exit(EXIT_FAILURE);
	}
	bar = (bar_data*) xmalloc(nbars * sizeof(bar_data));
	barnames = label_bars(nbars);

	/* Toplevel window */
	topLevel = XtAppInitialize(&context, progname, 
			optionDescList, XtNumber(optionDescList), &argc, argv,
			fallbackResources, NULL, 0); 

	if (argc > 1)
		usage();
	
	/* Load default resources */
	XtGetApplicationResources(topLevel, 0, application_resources,
				  XtNumber(application_resources), NULL, 0 );

	/* Set the color table */
	colors = (String*)xmalloc(MAX_COLORS*sizeof(String));
	colors[0] = idlecol;
	colors[1] = usercol;
	colors[2] = kernelcol;
	colors[3] = nicecol;
	colors[4] = systemcol;

	/* Main layout form */
	form = XtVaCreateManagedWidget("form", 
			xmFormWidgetClass, topLevel,
			XmNhorizontalSpacing, 5, 
			XmNverticalSpacing, 5, 
			NULL);

	/* Create a form to display the color legend */
	subform = XtVaCreateManagedWidget("colorLegend",
		xmFormWidgetClass, form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

	gcv.foreground = WhitePixelOfScreen(XtScreen(form));
	/* Create a gc for this bar */
	gc = XCreateGC(XtDisplay(form), RootWindowOfScreen(XtScreen(form)),
		GCForeground, &gcv);

	for(i = MAX_COLORS ; i > 0 ; i--)
	{
		XmString xm_legend;
		Pixel pixel;
		Display *display = XtDisplay(form);
		Colormap cmap = DefaultColormapOfScreen(XtScreen(form));
		XColor col, dummy;

		/* Stuff required for setting the foreground color on a label */
		if(!XAllocNamedColor(display, cmap, colors[i-1], &col, &dummy))
			pixel = WhitePixel(display, DefaultScreen(display));
		else
			pixel = col.pixel;

		xm_legend = XmStringCreateLocalized(legenda[i-1]);
		label = XtVaCreateManagedWidget(legenda[i-1],
			xmLabelWidgetClass, subform,
			XmNtopAttachment, XmATTACH_FORM,
			XmNforeground, pixel,
			XmNlabelString, xm_legend,
			XmNrightAttachment, ( i == 1 ? XmATTACH_FORM : XmATTACH_WIDGET),
			XmNrightWidget, label,
			XmNrightOffset, 5,
			NULL);
		XmStringFree(xm_legend);
	}
	
	/* 
	* Create the drawbuttons in which to display the different
	* status areas
	*/
	for(i = 0; i < nbars; i++) 
	{
#define FORMNAMEFMT	"form%d"
		char formname[sizeof(FORMNAMEFMT) + 32];
#define BARNAMEFMT	"bar%d"
		char barname[sizeof(BARNAMEFMT) + 32];

		/* Create form which is to act as parent for the bar */
		sprintf(formname, FORMNAMEFMT, i);
		subform = XtVaCreateManagedWidget(formname,
			xmFormWidgetClass, form,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, subform,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL);
		if(i+1 == nbars)
			XtVaSetValues(subform,
				XmNbottomAttachment, XmATTACH_FORM,
				NULL);
		/* Label to display */
		label = XtVaCreateManagedWidget(barnames[i], 
			xmLabelWidgetClass, subform,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightPosition, 15,
			XmNbottomAttachment, XmATTACH_FORM, 
			XmNrecomputeSize, False,
			NULL);
		frame = XtVaCreateManagedWidget("frame",
			xmFrameWidgetClass, subform,
			XmNtopAttachment, XmATTACH_FORM,	/* fully constrained */
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 16,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNshadowType, XmSHADOW_IN,
			XmNunitType, XmPIXELS,
			XmNwidth, width,
			XmNheight, height,
			NULL);
			
		/* Create the bar in which to draw */
		sprintf(barname, BARNAMEFMT, i);
		bar[i].bar = XtVaCreateManagedWidget(barname, 
			xmDrawingAreaWidgetClass, frame,
			XmNunitType, XmPIXELS,
			XmNwidth, width,
			XmNheight, height,
			XmNresizePolicy, XmNONE,
			NULL);

		/* 
		* Add an expose callback to redraw the bar in case something gets 
		* done with it 
		*/
		XtAddCallback(bar[i].bar, XmNexposeCallback, (XtCallbackProc)redraw, 
			(XtPointer)i);

		/* Create a pixmap the same size as the drawing area */
		bar[i].pixmap = XCreatePixmap(XtDisplay(bar[i].bar), 
			RootWindowOfScreen(XtScreen(bar[i].bar)), width, height, 
			DefaultDepthOfScreen(XtScreen(bar[i].bar)));

		/* Default background color is idle */
		set_color(bar[i].bar, colors[0]);
		/* Set initial color */
		XFillRectangle(XtDisplay(bar[i].bar), bar[i].pixmap, gc, 0, 0, 
			width, height);
	}
	/* Install an event handler to prevent the user from resizing this dialog */
	XtAddEventHandler(topLevel, StructureNotifyMask, False,
		(XtEventHandler)SetShellSize, NULL);

	XtRealizeWidget(topLevel);

	init_bars(nbars);
	
	XtAppAddTimeOut(context, (unsigned long) (interval * 1000), 
		(XtTimerCallbackProc)update_display, (XtPointer)context);

	XtAppMainLoop(context);
	return(EXIT_SUCCESS);
}

/*
* redraw a bar after an expose event 
*/
/* ARGSUSED */
void
redraw(Widget dummy, int bar_id, XmDrawingAreaCallbackStruct *cbs)
{
	XCopyArea(cbs->event->xexpose.display, bar[bar_id].pixmap, cbs->window, 
		gc, 0, 0, width, height, 0, 0);
}

/* ARGSUSED */
static void
update_display(XtPointer data, XtIntervalId *id)
{
	display_bars(nbars);
	
	if (count > 0) {
	if (--count == 0)
		return;
	}
	XtAppAddTimeOut((XtAppContext)data, (unsigned long) (interval * 1000), 
			update_display, data);
}

/*
* This routine draws the actual bars
* nstates indicates the number of items in states
* The first item always is the idle part of something
* The second is user space usage
* The third is kernel space usage
* The fourth item is nice usage
* The last item is system space usage
*/
void
draw_bar(bar_id, states, nstates)
int bar_id;
int states[];
int nstates;
{
	int i;
	short x = 0;
	unsigned short display_width; 
	long total_len = 0;

	/* total length of all components in states */
	for(i = 0 ; i < nstates; i++)
		total_len += states[i];

	/* Draw the pixmap with the different status levels */
	for(i = 0 ; i < nstates; i++)
	{
		/* Set color value */
		set_color(bar[bar_id].bar, colors[i]); 

		/* calculate width of this ``sub'' bar */
		display_width = 
			(unsigned short)(((float)states[i]/(float)total_len)*width);

		/* draw it */
		XFillRectangle(XtDisplay(bar[bar_id].bar), bar[bar_id].pixmap, gc, x, 0,
			display_width, height);

		/* set new offset */
		x+=display_width;
	}
	/* display it */
	XCopyArea(XtDisplay(bar[bar_id].bar), bar[bar_id].pixmap, 
		XtWindow(bar[bar_id].bar), gc, 0, 0, width, height, 0, 0); 
}

/*
* Set a foreground color in the gc of the requested bar
*/
void
set_color(Widget widget, char *color)
{
	Display *display = XtDisplay(widget);
	Colormap cmap = DefaultColormapOfScreen(XtScreen(widget));
	XColor col, dummy;

	if(!XAllocNamedColor(display, cmap, color, &col, &dummy))
	{
		char buf[32];
		sprintf(buf, "Can't allocate color %s", color);
		XtWarning(buf);
		return;
	}
	XSetForeground(display, gc, col.pixel);
}
