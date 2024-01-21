/**
 *
 * form1.c
 *
 **/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two, three;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				NULL);

  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_NONE,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

  three = XtVaCreateManagedWidget("three", xmPushButtonGadgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_WIDGET,
				  XmNrightWidget, two,
				  NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}

