#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

Widget rowcol;

void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  XmString s1, s2;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  s1 = XmStringCreateSimple("check1");
  s2 = XmStringCreateSimple("check2");
  rowcol = XmVaCreateSimpleCheckBox(toplevel, "checkBox", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    NULL);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
