/** test6 -- a menu bar with two pulldown menu, all made of widgets
**/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>

void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    unsigned char x;

    XtVaGetValues(XtParent(w), XmNpacking, &x, NULL);
    fprintf(stderr, "* Widget = %s - Activated\n", XtName(w));
    fprintf(stderr, "# RC Packing is %d\n", x);
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Armed\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1, cascade2, cascade3;
    Widget pane1, pane2, pane3;
    Widget button, button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test7", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XmCreateMenuBar(toplevel,
			 "menubar",
			 NULL, 0);

    pane1 = XmCreatePulldownMenu(rc,
				 "pane1",
				 NULL, 0);

    pane2 = XmCreatePulldownMenu(rc,
				 "pane2",
				 NULL, 0);

    cascade1 = XtVaCreateManagedWidget("cascade1",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane1,
				       NULL);

    cascade2 = XtVaCreateManagedWidget("cascade2",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane2,
				       NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      pane1,
				      NULL);

    pane3 = XmCreatePulldownMenu(pane1,
                                 "pane3",
                                 NULL, 0);

    cascade3 = XtVaCreateManagedWidget("cascade3",
				       xmCascadeButtonWidgetClass,
				       pane1,
				       XmNsubMenuId, pane3,
				       NULL);
                                       
    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      pane2,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      pane2,
				      NULL);

    XtAddCallback(button1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button3, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button4, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button5, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button2, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button3, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button4, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button5, XmNarmCallback, pb_arm_callback, NULL);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

    XtAppMainLoop(theApp);    
}


