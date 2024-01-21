#include <stdio.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

Widget rowcol, pull;

void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  XmString s1, s2, s3;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  s1 = XmStringCreateSimple("button1");
  s2 = XmStringCreateSimple("button2");
  pull = XmVaCreateSimplePulldownMenu(toplevel, "ppane", 0, cb,
				    XmVaPUSHBUTTON, s1, 0, NULL, NULL,
				    XmVaPUSHBUTTON, s2, 0, NULL, NULL,
				    NULL);
  s3 = XmStringCreateSimple("option");
  rowcol = XmVaCreateSimpleOptionMenu(toplevel, "optionMenu",
				    s3, 0, 1, cb,
#if 0
				    XmNsubMenuId, pull,
#endif
				    NULL);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
