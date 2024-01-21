/*
 * this test is pretty cool.  Turns out that objects need not have a Composite
 * parent.  Try using editres on this -- "One" will not appear in the Widget
 * hierarchy, just as in M*tif.
 */

#include <Xm/BulletinB.h>
#include <Xm/SeparatoG.h>
#include <Xm/ExtObjectP.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  Widget gadget;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ExtObj", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel,
				XmNmarginWidth, 10, XmNmarginHeight, 10,
				NULL);

  gadget = XtVaCreateManagedWidget("Sep", xmSeparatorGadgetClass, two, NULL);

  one = XtVaCreateWidget("One", xmExtObjectClass, gadget, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
