#include <Xm/Xm.h>
#include <Xm/TextF.h> 
    
int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  one = XtVaCreateManagedWidget("one",xmTextFieldWidgetClass,toplevel, 
                                XmNcolumns, 10, NULL); 

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

