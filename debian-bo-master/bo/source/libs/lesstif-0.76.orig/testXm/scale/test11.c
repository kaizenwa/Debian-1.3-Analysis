#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ScaleP.h>
#include <Xm/Form.h>

void dragCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;

    printf ("dragCallback: %d\n", cbs->value);
}

void valueChangedCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;

    printf ("valueChangedCallback: %d\n", cbs->value);    
}

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;
  unsigned int maximum;
  XmString str;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
                                NULL);

  str = XmStringCreateLtoR("Test Scale", XmFONTLIST_DEFAULT_TAG);

  two = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, one, 
                                XmNtopAttachment,  XmATTACH_FORM,
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNbottomAttachment,  XmATTACH_FORM,
                                XmNorientation, XmHORIZONTAL, 
				XmNshowValue, True,
#if 0
                                XmNscaleWidth, 40,
#endif
				XmNminimum, 100,
				XmNmaximum, 300,
				XmNtitleString, str,
				XmNprocessingDirection, XmMAX_ON_LEFT,
				NULL);

  XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
  XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNmaximum, &maximum, NULL);

  printf ("one.maximum = %u\n", maximum);
  XtAppMainLoop(app);

  exit(0);
}
