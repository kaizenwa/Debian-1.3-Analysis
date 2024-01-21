/* test for non-existant fontlist tag */

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <stdio.h>

int 
main(int argc,
     char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmString xmstr;
    Widget label;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    xmstr = XmStringCreate("Hello World", "bleh");

    label = XtVaCreateManagedWidget("label", 
				    xmLabelWidgetClass,
				    toplevel,
				    XmNlabelString, xmstr,
				    NULL);
    XmStringFree(xmstr);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
