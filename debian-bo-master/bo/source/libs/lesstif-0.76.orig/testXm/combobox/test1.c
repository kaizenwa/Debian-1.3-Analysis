#include "ComboBox.h"
#include <Xm/BulletinB.h>

int
main(int argc, char **argv)
{
  Widget toplevel, widget, bb;
  XtAppContext app;
  XmString item;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  widget = DtCreateComboBox(bb, "combo", NULL, 0);

  item = XmStringCreateSimple("Item 1");
  DtComboBoxAddItem(widget, item, 0, True);
  item = XmStringCreateSimple("Item 2");
  DtComboBoxAddItem(widget, item, 0, True);
  item = XmStringCreateSimple("Item 3");
  DtComboBoxAddItem(widget, item, 0, True);
  item = XmStringCreateSimple("Item 4");
  DtComboBoxAddItem(widget, item, 0, True);
  item = XmStringCreateSimple("Item 5");
  DtComboBoxAddItem(widget, item, 0, True);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
