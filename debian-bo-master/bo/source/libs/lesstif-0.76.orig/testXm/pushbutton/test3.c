#include <Xm/Xm.h>
#include <Xm/PushB.h>

Display *theDisplay;
Window theRootWindow;
Pixmap Pix;
Widget toplevel;

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget label;
  Pixel fg,bg;
  
  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  label= XtVaCreateManagedWidget("Button1", 
                                 xmPushButtonWidgetClass, toplevel, 
				 NULL);
  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);
  theRootWindow = XDefaultRootWindow(theDisplay);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);

  XtVaSetValues(label,
	        XmNlabelPixmap,Pix, 
                XmNlabelType,XmPIXMAP,
                NULL);

  XtAppMainLoop(theApp);

  exit(0);
}
