#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget toplevel, field, pane, button;

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

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
                                  NULL);  

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   pane, 
                                   NULL);

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   pane, 
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

