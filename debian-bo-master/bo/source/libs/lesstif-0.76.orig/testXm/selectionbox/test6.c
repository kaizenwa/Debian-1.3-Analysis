#include <stdio.h>
#include <Xm/XmAll.h>

XtAppContext app;
Widget toplevel, box, push, tf;

void
ok(Widget w, XtPointer client, XtPointer call)
{
	XmSelectionBoxCallbackStruct	*p = (XmSelectionBoxCallbackStruct *)call;
	char	*s = NULL;

	XmStringGetLtoR(p->value, XmSTRING_DEFAULT_CHARSET, &s);
	fprintf(stderr, "Ok '%s'\n", s);
}

void
activate(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Activated\n");
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	box = XmCreateSelectionDialog(toplevel, "Box", NULL, 0);
	tf = XmCreateTextField(box, "tf", NULL, 0);
	XtAddCallback(tf, XmNactivateCallback, activate, NULL);
	XtAddCallback(box, XmNokCallback, ok, NULL);
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
