#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

void arm_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf ("armed\n");
}

void disarm_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf ("disarmed\n");
}

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf ("activate\n");
}


int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("One", xmPushButtonGadgetClass, two, NULL);

  XtAddCallback(one, XmNactivateCallback, activate_callback, NULL);
  XtAddCallback(one, XmNarmCallback, arm_callback, NULL);
  XtAddCallback(one, XmNdisarmCallback, disarm_callback, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
