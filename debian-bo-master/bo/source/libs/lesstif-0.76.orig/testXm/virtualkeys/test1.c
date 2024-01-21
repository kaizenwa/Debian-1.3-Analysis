#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea;
  Dimension height;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmPushButtonWidgetClass,
                                       toplevel,
	                               NULL);

  XtVaGetValues(drawingArea, XtNheight, &height, NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(theApp);

  exit(0);
}

