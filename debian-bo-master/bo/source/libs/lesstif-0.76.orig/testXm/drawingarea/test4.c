#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

void print(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Yeah\n");
}

void incb(Widget w, XtPointer client, XtPointer call)
{
	Widget				menu = (Widget)client;
	XmDrawingAreaCallbackStruct	*cbs = (XmDrawingAreaCallbackStruct *) call;

	if (cbs->event->type != ButtonPress)
		return;
	if (cbs->event->xbutton.button != 3)
		return;

	fprintf(stderr, "Popping up the menu ...\n");

	XmMenuPosition(menu, (XButtonPressedEvent *)cbs->event);
	XtManageChild(menu);
}

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button, m;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea", xmDrawingAreaWidgetClass, toplevel,
	NULL);

  m = XmCreatePopupMenu(drawingArea, "popup", NULL, 0);
  
  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, m,
	NULL);

  XtAddCallback(drawingArea, XmNinputCallback, incb, (XtPointer)m);
  XtAddCallback(button, XmNactivateCallback, print, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(theApp);

  exit(0);
}
