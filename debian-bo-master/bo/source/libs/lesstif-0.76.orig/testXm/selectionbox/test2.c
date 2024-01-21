/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  box = XmCreateSelectionBox(toplevel, "Box", NULL, 0);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
