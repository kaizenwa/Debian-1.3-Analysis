#include <Xm/Text.h> 
    
void Doit(Widget w, XtPointer client, XtPointer call)
{
	char	*s = (char *)client;
	String	v = NULL;

	fprintf(stderr, "Callback %s, widget value '%s'\n", s, XmTextGetString(w));

	XtVaGetValues(w, XmNvalue, &v, NULL);
	fprintf(stderr, "GetValues => %s\n", v);
	XtFree(v);
}

char *fallback[] = {
	"*blinkRate:	0",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv, fallback, NULL);

  one = XtVaCreateManagedWidget("one",xmTextWidgetClass,toplevel, 
                                XmNrows, 10, NULL); 

  XtAddCallback(one, XmNmodifyVerifyCallback, Doit, XmNmodifyVerifyCallback);
  XtAddCallback(one, XmNactivateCallback, Doit, XmNactivateCallback);
  XtAddCallback(one, XmNlosingFocusCallback, Doit, XmNlosingFocusCallback);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

