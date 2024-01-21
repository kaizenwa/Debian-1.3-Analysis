#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>

#define TEST_CBS 1

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);


int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;
  Pixmap Pix;
  Display *theDisplay;
  Window theRootWindow;
  Pixel fg,bg;

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmToggleButtonWidgetClass, toplevel, 
				XmNindicatorOn, False,
				XmNshadowThickness, 5,
				XmNfillOnSelect, True,
				NULL);

  XtAddCallback(butt,XmNvalueChangedCallback,HiCB,NULL);

  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);
  theRootWindow = XDefaultRootWindow(theDisplay);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);

  XtVaSetValues(butt,
	        XmNlabelPixmap,Pix, 
                XmNlabelType, XmPIXMAP,
                NULL);

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "woman",
                    fg, bg);

  XtVaSetValues(butt,
	        XmNselectPixmap, Pix, 
                NULL);

  XtAppMainLoop(theApp);

  exit(0);
}

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call_data;

    printf("Toggle Me and I'm Yours: %d\n", cbs->set);

#if TEST_CBS
    cbs->set = False;
#endif
}
