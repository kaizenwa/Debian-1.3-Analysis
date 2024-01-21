
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer userData, XtPointer cbs) {
  XmArrowButtonCallbackStruct *abcs = (XmArrowButtonCallbackStruct *)cbs;

  printf("ArrowBG Activated: click count: %d\n", abcs->click_count);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ABG", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel,
				XmNmarginWidth, 10, XmNmarginHeight, 10,
				NULL);

  one = XtVaCreateManagedWidget("One", xmArrowButtonGadgetClass, two,
				XmNwidth, 100, XmNheight, 100, NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
