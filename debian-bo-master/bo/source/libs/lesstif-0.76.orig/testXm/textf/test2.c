#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget toplevel, field, form, button;

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass,
                                 toplevel, NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  form, 
                                  XmNleftAttachment, XmATTACH_FORM,
                                  XmNrightAttachment, XmATTACH_FORM,
                                  XmNtopAttachment, XmATTACH_FORM,
                                  XmNbottomAttachment, XmATTACH_NONE,
                                  NULL);  

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   form, 
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget, field,
                                   XmNbottomAttachment, XmATTACH_FORM,
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

