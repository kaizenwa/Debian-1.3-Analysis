/*
 * test for XmInstallPixmap
 *
 */

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include "xlogo64"

XtAppContext theApp;
Pixmap pix, pix2;
Widget toplevel, pb1, pb2,rc;
XImage *logo_image;

int
main(int argc,
     char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rc",
                                 xmRowColumnWidgetClass,
                                 toplevel, 
                                 XmNorientation, XmHORIZONTAL,
                                 NULL);

    _XmCreateImage(logo_image, XtDisplay(toplevel), (char *)xlogo64_bits, xlogo64_width,
		   xlogo64_height, LSBFirst);

    XmInstallImage(logo_image, "xlogo64");

    pix = XmGetPixmap(DefaultScreenOfDisplay(XtDisplay(toplevel)),
		      "xlogo64",
		      BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
		      WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))));

    pix2 = XmGetPixmapByDepth(DefaultScreenOfDisplay(XtDisplay(toplevel)),
			      "xlogo32",
			      BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
			      WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
			      DefaultDepthOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))));

    pb1 = XtVaCreateManagedWidget("pb1",
				  xmPushButtonWidgetClass,
				  rc,
				  XmNlabelType, XmPIXMAP,
				  XmNlabelPixmap, pix,
				  NULL);
				  
    pb2 = XtVaCreateManagedWidget("pb2",
				  xmPushButtonWidgetClass,
				  rc,
				  XmNlabelType, XmPIXMAP,
				  XmNlabelPixmap, pix2,
				  NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(theApp);
    exit(0);
}
