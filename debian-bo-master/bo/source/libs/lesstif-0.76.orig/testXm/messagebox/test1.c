#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>

void HiCB (Widget w, XtPointer client_data, XtPointer call_data);

Widget toplevel;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget butt1;

  toplevel = XtVaAppInitialize (&theApp, "drawingArea", NULL, 0,
				&argc, argv, NULL, NULL);

  butt1 = XtVaCreateManagedWidget ("Button1", xmPushButtonWidgetClass, toplevel,
				   NULL, 0);

  XtAddCallback (butt1, XmNactivateCallback, HiCB, NULL);

  XtRealizeWidget (toplevel);

  XtAppMainLoop (theApp);

  exit (0);
}

void
Quit(Widget w, XtPointer client, XtPointer call)
{
    exit(0);
}

void
Again(Widget w, XtPointer client, XtPointer call)
{
    Widget dialog;
    XmString xmstr = XmStringCreateSimple("I have a title"),
	ok = XmStringCreateSimple("Quit"),
	cancel = XmStringCreateSimple("Cancel String");

    Arg al[4];
    int ac;
    
    ac = 0;
    XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
    XtSetArg(al[ac], XmNdefaultPosition, False); ac++;
    XtSetArg(al[ac], XmNokLabelString, ok); ac++;		/* initialize */
    dialog = XmCreateMessageDialog(toplevel, "dialog2", al, ac);

    XtVaSetValues(dialog,
		XmNmessageString,	xmstr,
		XmNcancelLabelString,	cancel,			/* set_values */
	NULL);
    
    XtManageChild (dialog);
    XtAddCallback(dialog, XmNokCallback, Quit, NULL);
}

void 
HiCB (Widget w, XtPointer client_data, XtPointer call_data)
{

  static Widget dialog = NULL;
  XmString xmstr;


  if (!dialog) {
     dialog = XmCreateMessageDialog(toplevel, "MyDialog", NULL, 0);
     xmstr = XmStringCreateLtoR("Hello World\n\nIf you hit OK on this dialog, another one"
				      "\nshould appear, positioned to the lower right of this one.",
				      XmFONTLIST_DEFAULT_TAG);
  
  
     XtVaSetValues(dialog, XmNmessageString, xmstr, NULL);
  
     XtAddCallback(dialog, XmNokCallback, Again, NULL);
  }
  XtManageChild (dialog);
}
