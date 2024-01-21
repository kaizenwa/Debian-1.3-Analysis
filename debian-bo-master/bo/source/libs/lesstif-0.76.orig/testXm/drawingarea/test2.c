#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

void push_callback(Widget w, XtPointer clientData, XtPointer callData)
{
   Dimension width, height;

   XtVaGetValues(w,
                 XmNwidth, &width,
                 XmNheight, &height,
                 NULL);

  XtVaSetValues(w, 
                XmNwidth, width + 10,
                XmNheight, height + 10,
                NULL);
}

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  button = XtVaCreateManagedWidget("button",
                                   xmPushButtonGadgetClass,
                                   drawingArea,
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, push_callback, NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(theApp);

  exit(0);
}
