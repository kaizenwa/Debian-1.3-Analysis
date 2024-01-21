#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(theApp);

  exit(0);
}
