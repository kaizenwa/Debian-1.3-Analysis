/**
 *
 * form1.c
 *
 **/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

void
callback(Widget w,
         XtPointer p1,
         XtPointer p2)
{
  XtVaSetValues(w,
                XmNwidth, 50,
                NULL);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				XmNresizable, True, NULL);

  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_NONE,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

  XtAddCallback(two, XmNactivateCallback, callback, NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
