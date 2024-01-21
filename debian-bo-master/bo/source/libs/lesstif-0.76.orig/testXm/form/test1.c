/**
 *
 * form1.c
 *
 **/

#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/FormP.h>

char *fallback[] = {
	"*XmForm.marginWidth:	45",
	"*XmForm.marginHeight:	30",
	"*XmForm.background:	dark slate blue",
	"*XmForm.?.background:	sea green",
	"*foreground:		yellow",
	NULL
};

void
focus(Widget w, XtPointer data, XtPointer cbs)
{
    printf("focus moved\n");
}

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, fallback, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				NULL);
  XtAddCallback(one, XmNfocusCallback, focus, NULL);

  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_FORM,
				NULL);
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, two,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
