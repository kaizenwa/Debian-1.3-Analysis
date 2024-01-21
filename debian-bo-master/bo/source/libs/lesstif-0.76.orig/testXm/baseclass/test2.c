/* fast subclass test */

#include <Xm/BaseClassP.h>
#include <Xm/Label.h>
/* believe it or not, we just tested the fast subclassing.  Just to make sure */
#include <Xm/XmP.h>
#include <Xm/PushBP.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, toplevel, NULL);

  if (XmIsLabel(one))
	printf("IS LABEL\n");
  else
	printf("IS NOT LABEL\n");

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
