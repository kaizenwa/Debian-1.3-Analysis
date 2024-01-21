#include <Xm/Xm.h>
#include <Xm/ScrolledW.h>
#include <Xm/ArrowB.h>

Widget toplevel;

void Doit(Widget w, XtPointer client, XtPointer call)
{
#if 0
	XdbPrintTree(toplevel);
#endif
}

int main(int argc, char **argv)
{
  Widget sw, ab;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  sw  = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, toplevel, 
		XmNscrollingPolicy,	XmAUTOMATIC,
		XmNscrollBarPlacement,	XmBOTTOM_RIGHT,
		XmNwidth,		100,
		XmNheight,		100,
	NULL);

  ab = XtVaCreateManagedWidget("ab", xmArrowButtonWidgetClass, sw,
		XmNwidth,	300,
		XmNheight,	300,
	NULL);

  XtAddCallback(ab, XmNactivateCallback, Doit, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
