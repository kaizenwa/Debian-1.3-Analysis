/* 
 * switches though a bunch of cases where pixmaps and labels are resized
 */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/LabelP.h>


static void
mytest(Widget w, XtPointer p, XtPointer s)
{
    static int index = 0;

    index++;
    switch (index % 7) {
    case 0 : 
	{
	    XmString mystr = XmStringCreateSimple("this is a big test");
	    
	    XtVaSetValues(w,
			  XmNlabelString, mystr,
			  XmNlabelType,XmSTRING,
			  NULL);
	    XmStringFree(mystr);
	    break;
	}
	
    case 1 :
	{
	    Pixmap pixmap;
	    Pixel fg, bg;
	    
	    fg = XBlackPixelOfScreen(XtScreen(w));
	    bg = XWhitePixelOfScreen(XtScreen(w));
	    pixmap = XmGetPixmap(XtScreen(w), "xlogo64", fg, bg);
	    
	    XtVaSetValues(w,
			  XmNlabelType,XmPIXMAP,
			  XmNlabelPixmap, pixmap,
			  NULL);
	    break;
	}
	
    case 2:
	{
	    Pixmap pixmap;
	    Pixel fg, bg;
	    
	    fg = XBlackPixelOfScreen(XtScreen(w));
	    bg = XWhitePixelOfScreen(XtScreen(w));
	    pixmap = XmGetPixmap(XtScreen(w), "xlogo11", fg, bg);
	    
	    XtVaSetValues(w,
			  XmNlabelPixmap, pixmap,
			  XmNlabelType,XmPIXMAP,
			  NULL);
	    break;
	}
	
    case 3:
	{
	    Pixmap pixmap;
	    Pixel fg, bg;
	    
	    XtVaSetValues(w,
			  XmNrecomputeSize, False,
			  NULL);
	    
	    fg = XBlackPixelOfScreen(XtScreen(w));
	    bg = XWhitePixelOfScreen(XtScreen(w));
	    pixmap = XmGetPixmap(XtScreen(w), "xlogo64", fg, bg);
	    
	    XtVaSetValues(w,
			  XmNlabelPixmap, pixmap,
			  XmNlabelType,XmPIXMAP,
			  NULL);
	    
	    XtVaSetValues(w,
			  XmNrecomputeSize, True,
			  NULL);
	    break;
	}
	
    case 4:
	{
	    XmString mystr = XmStringCreateSimple("this is a lot of text " 
						  "to make it really big");
	    
	    XtVaSetValues(w,
			  XmNrecomputeSize, False,
			  NULL);
	    
	    XtVaSetValues(w,
			  XmNlabelString, mystr,
			  XmNlabelType,XmSTRING,
			  NULL);
	    XmStringFree(mystr);
	    
	    XtVaSetValues(w,
			  XmNrecomputeSize, True,
			  NULL);
	    break;
	}

    case 5:
	{
	    XmString mystr = XmStringCreateSimple("this is a lot of text " 
						  "to make it really big");
	    
	    XtVaSetValues(w,
			  XmNlabelString, mystr,
			  XmNlabelType,XmSTRING,
			  NULL);
	    XmStringFree(mystr);
	    break;
	}
	
	
    case 6:
	{
	    XmString mystr = XmStringCreateSimple("a");
	    
	    XtVaSetValues(w,
			  XmNlabelString, mystr,
			  XmNlabelType,XmSTRING,
			  NULL);
	    XmStringFree(mystr);
	    break;
	}


    } /* switch */
}

String fallbacks[] = {"Hello*allowShellResize: True",
                      NULL};

int
main(int argc, char **argv)
{
    Widget toplevel, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Hello", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    button =
	XtVaCreateManagedWidget("hi there", xmPushButtonWidgetClass,
				toplevel, NULL);

    XtAddCallback(button, XmNactivateCallback, mytest, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
