#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
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
  XmString button1 = XmStringCreateLocalized("button number 1");
  XmString button2 = XmStringCreateLocalized("button number 2");
  XmString button3 = XmStringCreateLocalized("button number 3");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  rowcol = XmVaCreateSimpleMenuBar(toplevel, "radioBox",
                                   XmVaCASCADEBUTTON, button1, 'b',
                                   XmVaCASCADEBUTTON, button2, 'u',
                                   XmVaCASCADEBUTTON, button3, 't',
				   XmNbuttonCount, 8,
                                   NULL);

  XmVaCreateSimplePulldownMenu(rowcol, "b_menu", 0, cb,
                               XmVaPUSHBUTTON, button1, 'b', NULL, NULL,
                               NULL);

  XmStringFree(button1);
  XmStringFree(button2);
  XmStringFree(button3);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
