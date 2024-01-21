#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Command.h>

void Print(Widget w, XtPointer client, XtPointer call)
{
	XmCommandCallbackStruct *p = (XmCommandCallbackStruct *)call;
	char			*t;

	XmStringGetLtoR(p->value, XmFONTLIST_DEFAULT_TAG, &t);
	fprintf(stderr, "Command : '%s'\n", t);
	XtFree(t);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  box = XmCreateCommand(toplevel, "Box", NULL, 0);

  XtAddCallback(box, XmNcommandEnteredCallback, Print, NULL);
  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
