/* resizing this button and then activating it should resize it to the
 * default size
 */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/LabelP.h>


static void
mytest(Widget w, XtPointer p, XtPointer s)
{
    int height, width;
    printf("hi\n");

    XtVaGetValues(w,
		  XmNwidth, &width,
		  XmNheight, &height,
		  NULL);
    printf("height == %d, width == %d\n", height, width);

    XtVaSetValues(w,
		  XmNwidth, 0,
		  XmNheight, 0,
		  NULL);

    XtVaGetValues(w,
		  XmNwidth, &width,
		  XmNheight, &height,
		  NULL);
    printf("height == %d, width == %d\n", height, width);
/*
  XmLabelWidget lw;

  Lab_TextRect_x(lw) = 100;
  Lab_TextRect_y(lw) = 10;
  _XmCalcLabelDimensions(lw);
  printf("values are now x == %d, y == %d\n",
  Lab_TextRect_x(lw),
  Lab_TextRect_y(lw));

  */    
}

String fallbacks[] = {"Hello*allowShellResize: True", NULL};

int
main(int argc, char **argv)
{
    Widget toplevel, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Hello", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    button =
	XtVaCreateManagedWidget("pushme", xmPushButtonWidgetClass,
				toplevel, NULL);

    XtAddCallback(button, XmNactivateCallback, mytest, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
