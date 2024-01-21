#include <Xm/XmAll.h>
#include <stdio.h>

XtAppContext app;
Widget toplevel, box, push, tf;

void
activate(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Activated\n");
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	box = XmCreateBulletinBoardDialog(toplevel, "Box", NULL, 0);
	tf = XmCreateTextField(box, "tf", NULL, 0);
	XtAddCallback(tf, XmNactivateCallback, activate, NULL);
	XtManageChild(tf);
	XtManageChild(box);
}

int
main(int argc, char **argv)
{

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
		&argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	XtAddCallback(push, XmNactivateCallback, pushme, NULL);

	XtManageChild(push);

	XtRealizeWidget(toplevel);
	XtAppMainLoop(app);
	exit(0);
}
