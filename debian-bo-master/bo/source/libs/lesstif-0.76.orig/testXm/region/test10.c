/* test for _XmRegionDrawShadow */

#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/DrawingAP.h>
#include <XmI/MacrosI.h>
#include <stdio.h>

extern void print_region();

Widget toplevel, da;
XtAppContext app;

void
exposeCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmRegion region1 = _XmRegionCreate();
  XRectangle rect;

  printf ("\tRegion 1 -- _XmRegionCreate()\n");
  print_region(region1);

  rect.x = rect.y = 5;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  rect.x = rect.y = 35;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after two _XmUnionRectWithRegion's\n");
  print_region(region1);

  _XmRegionDrawShadow(XtDisplay(da),
		      XtWindow(da),
		      MGR_TopShadowGC(da),
		      MGR_BottomShadowGC(da),
		      region1,
		      0, 2, XmSHADOW_IN);
}

int
main(int argc,
     char **argv) 
{
  toplevel = XtVaAppInitialize(&app, 
			       "Region", 
			       NULL, 0, 
			       &argc, argv, 
			       NULL, NULL);

  da = XmCreateDrawingArea(toplevel, 
			   "da", 
			   NULL, 0);

  XtManageChild(da);

  XtAddCallback(da, XmNexposeCallback, exposeCallback, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
  exit(0);
}
