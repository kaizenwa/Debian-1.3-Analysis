/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;
  Arg a;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  XtSetArg(a, XmNdialogType, XmDIALOG_PROMPT);
  box = XmCreateSelectionBox(toplevel, "Box", &a, 1);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
