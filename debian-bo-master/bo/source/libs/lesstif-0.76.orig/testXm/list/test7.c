#include <stdio.h>
#include <Xm/List.h>
#include <Xm/BulletinB.h>

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget	toplevel, listw, bb;
    Arg		al[5];
    int		ac;
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, toplevel,
		XmNwidth,	200,
		XmNheight,	200,
	NULL);

    ac = 0;
    XtSetArg(al[ac], XmNvisibleItemCount, 5); ac++;
    XtSetArg(al[ac], XmNx, 20); ac++;
    XtSetArg(al[ac], XmNy, 20); ac++;
    listw = XmCreateScrolledList(bb, "list", al, ac);
    XtManageChild(listw);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);

    exit(0);
}
