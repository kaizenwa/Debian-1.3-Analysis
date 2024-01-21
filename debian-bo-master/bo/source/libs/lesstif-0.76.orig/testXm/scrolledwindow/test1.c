#include <Xm/Xm.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/DrawingA.h>

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget sw, hsb, vsb, da;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                toplevel, 
                                XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                                XmNscrollBarPlacement, XmBOTTOM_RIGHT,
                                NULL);

  hsb = XtVaCreateManagedWidget("hsb",
                                xmScrollBarWidgetClass,
                                sw,
                                XmNorientation, XmHORIZONTAL,
                                NULL);

  vsb = XtVaCreateManagedWidget("vsb",
                                xmScrollBarWidgetClass,
                                sw,
                                XmNorientation, XmVERTICAL,
                                NULL); 
  
  da = XtVaCreateManagedWidget("da",
                               xmDrawingAreaWidgetClass,
                               sw,
                               NULL);

  XmScrolledWindowSetAreas(sw, hsb, vsb, da);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
