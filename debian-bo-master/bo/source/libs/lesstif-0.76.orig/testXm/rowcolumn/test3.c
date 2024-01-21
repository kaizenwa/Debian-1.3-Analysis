/** test3 -- horizontal tight layout of widgets with XmNadjustLast = False.

    resulting layout should be:

    button1 button2 button3 button4 button5

    and if the window is resized smaller, the layout should change to something like:

    button1 button2 button3 
    button4 button5 

**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test3", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmHORIZONTAL,
				 XmNpacking, XmPACK_TIGHT,
				 XmNadjustLast, False,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    XtRealizeWidget(toplevel);

    XtAppMainLoop(theApp);    
    exit(0);
}


