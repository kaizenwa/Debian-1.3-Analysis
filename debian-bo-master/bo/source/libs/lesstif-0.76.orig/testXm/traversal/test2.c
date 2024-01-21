#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

void
doit(Widget w, XtPointer data, XtPointer cbs) {
    XmProcessTraversal(w, XmTRAVERSE_HOME);
}

int
main(int argc, char **argv) {
    Widget toplevel, rowcol, pb;
    XtAppContext app;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "travers", NULL, 0,
				 &argc, argv, NULL, NULL);
    rowcol = XtVaCreateManagedWidget("rowcolumn",
				xmRowColumnWidgetClass, toplevel,
				XmNorientation, XmHORIZONTAL, NULL);

    pb = XtVaCreateManagedWidget("Ok",
			xmPushButtonWidgetClass, rowcol, NULL);
    pb = XtVaCreateManagedWidget("Cancel",
			xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, doit, NULL);
    pb = XtVaCreateManagedWidget("Help",
			xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, doit, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
