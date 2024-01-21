#include <Xm/Text.h> 
    
int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Arg argl[2];
  int ac;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  ac = 0;
  XtSetArg(argl[ac], XmNrows, 10); ac++;
  one = XmCreateScrolledText(toplevel, "one", argl, ac);
  XtManageChild(one);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}

