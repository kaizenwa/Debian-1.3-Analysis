#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/ScrolledWP.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel;
    Widget one, two, three;

    shell = XtVaAppInitialize(&theApp, "scrolledW", NULL, 0,
				 &argc, argv, NULL, NULL);

    toplevel = XmCreateScrolledWindow(shell, "ScrolledWindow", NULL, 0);
    XtManageChild(toplevel);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				  NULL);

    two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);

    three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);

    XtRealizeWidget(shell);

    XtAppMainLoop(theApp);

    exit(0);
}

