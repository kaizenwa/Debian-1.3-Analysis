
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;
  Dimension w,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL,
		NULL);

  two = XtVaCreateManagedWidget("Two", xmFormWidgetClass, toplevel,
				NULL);

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, two,
				NULL);
  XtVaSetValues(two,
		XmNunitType, Xm1000TH_INCHES,
		XmNwidth, 2000, XmNheight, 2000,
		NULL);

  XtVaSetValues(one,
		XmNunitType, Xm1000TH_INCHES,
		XmNwidth, 1000, XmNheight, 1000,
		NULL);

  /* test export */
  XtVaGetValues(two, XmNwidth, &w, XmNheight, &h, NULL);

  printf("Width: %d Height: %d\n", w, h);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
