#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
    Widget toplevel, one;
    XtAppContext app;
    int num_tickmarks;
    int i;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

    if (argc > 1)
	num_tickmarks = atoi(argv[1]);
    else
	num_tickmarks = 10;

    one = XtVaCreateManagedWidget("sc", xmScaleWidgetClass, toplevel,
				  NULL);

    for (i = 0; i < num_tickmarks; i++) {
	Widget l;

	l = XtVaCreateManagedWidget("-", xmLabelWidgetClass, one,
				    NULL);
    }

    XtRealizeWidget(toplevel);

    XtAppMainLoop(app);

    exit(0);
}
