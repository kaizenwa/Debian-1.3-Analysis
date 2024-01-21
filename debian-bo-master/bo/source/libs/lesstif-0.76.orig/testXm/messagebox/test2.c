/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>

Widget toplevel, box, push;

void Push(Widget w, XtPointer client, XtPointer call)
{
	Widget	dialog = (Widget)client;

	XtManageChild(dialog);
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	xms;
	Arg		args[3];
	int		nargs;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	nargs=0;
	xms = XmStringCreateSimple("Yow, buddy");
	XtSetArg(args[nargs], XmNmessageString, xms); nargs++;
	box = XmCreateQuestionDialog(toplevel, "Box", args, nargs);

	XtAddCallback(push, XmNactivateCallback, Push, box);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

	/*XdbPrintTree(toplevel);*/
	XtAppMainLoop(app);
	exit(0);
}
