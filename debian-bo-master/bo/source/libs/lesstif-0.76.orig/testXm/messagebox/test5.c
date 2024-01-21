/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>
#include <Xm/MessageBP.h>

char *fallback[] = {
   "*Box*fontList:      -*-times-bold-i-*--24-240-*=chset1,"
   "                    -*-times-bold-i-*--12-120-*=chset2,"
   "                    -misc-fixed-medium-r-*--10-100-*=chset3,"
   "                    fixed",
   NULL
};

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
	XmString	xs, xs1, xs2, xs3;
	Arg		args[3];
	int		nargs;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, fallback, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
		      XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 10,
		      NULL);

	nargs=0;
	xs1 = XmStringCreateLtoR("Yow, buddy, ", "chset1");
        xs2 = XmStringCreateLtoR("this is a test\nof fontlists, too.\n", "chset2");
        xs3 = XmStringCreateLtoR("With three charsets, even.", "chset3");
	xs = XmStringConcat(xs1, xs2);
	xs = XmStringConcat(xs, xs3);
	box = XmCreateQuestionDialog(toplevel, "Box", args, nargs);
	XtVaSetValues(box, XmNmessageString, xs, NULL);

	XtAddCallback(push, XmNactivateCallback, Push, box);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

	/*XdbPrintTree(toplevel);*/
	XtAppMainLoop(app);
	exit(0);
}
