#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two, three, four, five;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Frame", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, toplevel,
				NULL);

  two = XtVaCreateManagedWidget("OuterLabel", xmLabelGadgetClass, one,
				XmNchildType, XmFRAME_TITLE_CHILD,
				XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                                XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
				NULL);

  four = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, one,
				 XmNshadowType, XmSHADOW_IN,
				 XmNchildType, XmFRAME_WORKAREA_CHILD,
				 NULL);

  three = XtVaCreateManagedWidget("three", xmTextWidgetClass, four,
				  XmNchildType, XmFRAME_WORKAREA_CHILD,
				  NULL);

  five = XtVaCreateManagedWidget("InnerLabel", xmLabelWidgetClass, four,
				 XmNchildType, XmFRAME_TITLE_CHILD,
				 XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
				 XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
				 NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
