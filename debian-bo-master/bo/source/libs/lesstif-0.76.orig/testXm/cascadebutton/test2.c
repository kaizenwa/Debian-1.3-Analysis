#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>

Widget toplevel, one, cb, pb, rc;

int	i = 0;

void Doit(Widget w, XtPointer client, XtPointer call)
{
	XmCascadeButtonHighlight(cb, i);

	i = (i + 1) % 2;
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "CascadeTest", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, toplevel,
	NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, one,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		XmNx,		0,
		XmNy,		0,
	NULL);

  cb = XtVaCreateManagedWidget("cascade", xmCascadeButtonWidgetClass, rc,
		XmNx,	0,
		XmNy,	0,
	NULL);

  pb = XtVaCreateManagedWidget("push", xmPushButtonWidgetClass, one,
		XmNx,	0,
		XmNy,	100,
	NULL);
  XtAddCallback(pb, XmNactivateCallback, Doit, 0);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
