/*
 * Simulate the nedit problem
 *
 * Nedit 4.0.1 destroys the FileSB (i.e. the dialog box), but not the shell.
 * So yes, this means shell widgets keep lurking all over the place.
 *
 * Unfortunately, this thing works and nedit doesn't.
 */

#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <stdio.h>

XtAppContext app;
Widget toplevel, box, push;

int	popdown;

void
cb(Widget w, XtPointer client, XtPointer call)
{
	popdown = 1;
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	box = XmCreateFileSelectionDialog(toplevel, "Box", NULL, 0);
	XtAddCallback(box, XmNokCallback, cb, NULL);

	popdown = 0;
	XtManageChild(box);

	while (popdown == 0) {
	        XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMAll);
	}

	fprintf(stderr, "Destroying the FSB\n");
	XtDestroyWidget(box);
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
