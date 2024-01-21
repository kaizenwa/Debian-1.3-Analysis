#include <Xm/Xm.h>
#include "SpinButton.h"

int
main(int argc, char **argv)
{
  Widget toplevel, widget;
  XtAppContext app;
  XmString item;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  widget = DtCreateSpinButton(toplevel, "spinb", NULL, 0);

  item = XmStringCreateSimple("Item 3");
  DtSpinButtonAddItem(widget, item, 3);
  item = XmStringCreateSimple("Item 2");
  DtSpinButtonAddItem(widget, item, 2);
  item = XmStringCreateSimple("Item 1");
  DtSpinButtonAddItem(widget, item, 1);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
