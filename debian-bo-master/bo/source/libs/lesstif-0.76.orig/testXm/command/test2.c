#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Command.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

void Print(Widget w, XtPointer client, XtPointer call)
{
	XmCommandCallbackStruct *p = (XmCommandCallbackStruct *)call;
	char			*t;

	XmStringGetLtoR(p->value, XmFONTLIST_DEFAULT_TAG, &t);
	fprintf(stderr, "Command : '%s'\n", t);
	XtFree(t);
}

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

void SetError(Widget w, XtPointer client, XtPointer call)
{
	Widget		c = (Widget)client;
	XmString	xms = XmStringCreateSimple("This is error text");

	XmCommandError(c, xms);
	XmStringFree(xms);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, rc, w;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  rc = XmCreateRowColumn(toplevel, "rc", NULL, 0);
  XtManageChild(rc);

  box = XmCreateCommandDialog(rc, "Box", NULL, 0);

  w = XmCreatePushButton(rc, "Quit", NULL, 0);
  XtAddCallback(w, XmNactivateCallback, Quit, NULL);
  XtManageChild(w);

  w = XmCreatePushButton(rc, "Set Error", NULL, 0);
  XtAddCallback(w, XmNactivateCallback, SetError, box);
  XtManageChild(w);

  XtAddCallback(box, XmNcommandEnteredCallback, Print, NULL);
  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
