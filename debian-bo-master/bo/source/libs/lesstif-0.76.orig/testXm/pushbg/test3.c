#include <Xm/Xm.h>
#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>

Display *theDisplay;
Window theRootWindow;
Pixmap Pix;
Widget toplevel;

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, one, two;
  Pixel fg,bg;

  toplevel = XtVaAppInitialize(&theApp, "LabelG", NULL, 0,
			       &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("Button1", xmPushButtonGadgetClass, two,
				XmNwidth, 68, XmNheight, 68, NULL);
  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);

  XtVaSetValues(one,
                XmNlabelType,XmPIXMAP,
	        XmNlabelPixmap,Pix, 
                NULL);

  XtAppMainLoop(theApp);

  exit(0);
}
