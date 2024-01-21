#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include "Trivial.h"
#include <stdio.h>

Widget toplevel, triv, A, B, C, D;
XtAppContext theApp;

int
main(int argc,
     char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "menu", NULL, 0,
				 &argc, argv, NULL, NULL);

    triv = XtVaCreateManagedWidget("Triv",
				   xmTrivialWidgetClass,
				   toplevel, NULL);

    A = XtVaCreateManagedWidget("A",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    B = XtVaCreateManagedWidget("B",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    C = XtVaCreateManagedWidget("C",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    D = XtVaCreateManagedWidget("D",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(theApp);

    exit(0);
}
