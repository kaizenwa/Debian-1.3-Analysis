/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, dummy, real;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  box = XmCreateSelectionBox(toplevel, "Box", NULL, 0);

  dummy = XtVaCreateWidget("dummy", xmPushButtonWidgetClass, box, NULL);
  real = XtVaCreateManagedWidget("Gyre", xmPushButtonWidgetClass, box, NULL);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
