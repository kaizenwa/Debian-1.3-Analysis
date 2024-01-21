#include <stdio.h>
#include <Xm/XmAll.h>

Widget  scale, text;

void Doit(Widget w, XtPointer client, XtPointer call)
{
        char    *s;
        int     i;

        s = NULL;
        XtVaGetValues(text, XmNvalue, &s, NULL);
        if (s == NULL)
                return;

        i = atoi(s);
        if (i < 0 || i > 100)
                return;

        if (client)
                XmScaleSetValue(scale, i);
        else
                XtVaSetValues(scale, XmNvalue, i, NULL);
}


int
main(int argc, char **argv)
{
  Widget toplevel, but1, but2, form;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
                XmNx,   400,
                XmNy,   300,
        NULL);

  scale = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, form,
                XmNorientation,         XmHORIZONTAL,
                XmNscaleWidth,          100,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
        NULL);

  text = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, form,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           scale,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
        NULL);

  but1 = XtVaCreateManagedWidget("XtSetValues", xmPushButtonWidgetClass, form,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           text,
                XmNrightAttachment,     XmATTACH_POSITION,
                XmNrightPosition,       50,
                XmNbottomAttachment,    XmATTACH_FORM,
        NULL);
  XtAddCallback(but1, XmNactivateCallback, Doit, 0);

  but2 = XtVaCreateManagedWidget("XmScaleSetValue", xmPushButtonWidgetClass, form,
                XmNleftAttachment,      XmATTACH_WIDGET,
                XmNleftWidget,          but1,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           text,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
        NULL);

  XtAddCallback(but2, XmNactivateCallback, Doit, 1);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);
  exit(0);
}
