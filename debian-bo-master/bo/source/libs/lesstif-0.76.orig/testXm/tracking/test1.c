/**
 *
 * checks XmTrackingLocate
 *
 **/

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <X11/cursorfont.h>
#include <stdio.h>

Widget toplevel, one, two, rc;

void
do_tracking(Widget wid,
	    XtPointer client_data,
	    XtPointer call_data)
{
    Cursor help_cursor = XCreateFontCursor(XtDisplay(wid),
					   XC_question_arrow);

    Widget w = XmTrackingLocate(one,
				help_cursor,
				False);

    printf ("The widget you selected is %p\n", w);
}

int
main(int argc,
     char **argv)
{
    XtAppContext app;
   
    toplevel = XtVaAppInitialize(&app, 
				 "Label", 
				 NULL, 0, 
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("RC",
                                 xmRowColumnWidgetClass,
                                 toplevel, NULL);

    one = XtVaCreateManagedWidget("One",
				  xmPushButtonWidgetClass,
				  rc,
				  NULL);

    two = XtVaCreateManagedWidget("Two",
				  xmPushButtonWidgetClass,
				  rc,
				  NULL);

    XtAddCallback(one, XmNactivateCallback, do_tracking, NULL);

    XtRealizeWidget(toplevel);

    printf ("Press the button labeled \"One\" and then select a widget\n");

    XtAppMainLoop(app);
    exit(0);
}

