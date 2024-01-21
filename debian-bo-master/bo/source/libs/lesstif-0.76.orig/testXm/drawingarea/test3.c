/* test for margin width's and height's -- DEFECT 61 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>


int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button, other_button;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  button = XtVaCreateManagedWidget("button",
                                   xmPushButtonWidgetClass,
                                   drawingArea,
                                   NULL);

  other_button = XtVaCreateManagedWidget("button2",
                                         xmPushButtonWidgetClass,
                                         drawingArea,
                                         XmNx, 100,
                                         XmNy, 100,
                                         NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(theApp);

  exit(0);
}
