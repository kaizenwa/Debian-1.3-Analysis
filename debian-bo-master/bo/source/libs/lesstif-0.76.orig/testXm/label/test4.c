#include <stdio.h>
#include <Xm/XmAll.h>

int
main(int argc, char **argv)
{
  Widget toplevel, rc, w;
  XtAppContext app;
  Arg	al[10];
  int	ac;
  XmString	xms;

  XtSetLanguageProc(NULL, NULL, NULL);
  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel, NULL);

  ac = 0;
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR at initialize", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple at initialize");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR with setvalues", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple with setvalues");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
