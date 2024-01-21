#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

void Doit(Widget w, XtPointer client, XtPointer call)
{
	Widget		l = (Widget)client;
	XmString	x;

	fprintf(stderr, "Longer ...\n");

	x = XmStringCreateSimple("This is a much longer string");
	XtVaSetValues(l, XmNlabelString, x, NULL);
	XmStringFree(x);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel;
    Arg		al[10];
    int		ac;
    Widget	f, l, b;
    XmString	x;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, NULL, NULL);

    f = XmCreateForm(toplevel, "form", NULL, 0);
    XtManageChild(f);
    
    ac = 0;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_END); ac++;
    b = XmCreatePushButton(f, "button", al, ac);
    XtManageChild(b);

    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNrightWidget, b); ac++;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
    l = XmCreateLabel(f, "MainLabel", al, ac);
    XtManageChild(l);

    XtAddCallback(b, XmNactivateCallback, Doit, (XtPointer)l);

    x = XmStringCreateSimple("Push");
    XtVaSetValues(b, XmNlabelString, x, NULL);
    XmStringFree(x);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(theApp);    
    exit(0);
}
