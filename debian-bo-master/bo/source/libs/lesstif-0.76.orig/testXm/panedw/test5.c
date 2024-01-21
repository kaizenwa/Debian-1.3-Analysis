#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget button1, button;
Widget toplevel, field, pane;

#define SKIP_ADJUST
void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
  if (!XtIsManaged(button1)) {
	XtManageChild(button1);
  }
  else {
	XtUnmanageChild(button1);
  }
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
	                            pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
				    NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
                                  NULL);  

  button1 = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
                                   pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

