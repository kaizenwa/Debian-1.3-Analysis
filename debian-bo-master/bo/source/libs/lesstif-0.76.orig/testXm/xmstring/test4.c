#include <Xm/Xm.h>
#include <Xm/Label.h>

/*
   This tests behavior pointed out by Scott Cramer

   if the tag given XmStringCreate is invalid, Motif1.2 
   uses the font associated with XmFONTLIST_DEFAULT_TAG.
*/

int 
main(int argc,
     char **argv)
{
    Widget toplevel, label;
    XtAppContext app;
    XmString xmstr;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    label = XmCreateLabel(toplevel,
			  "label",
			  NULL, 0);

    XtManageChild(label);

    xmstr = XmStringCreate("Hello World", "INVALID_FONT_LIST_TAG");

    XtVaSetValues(label,
		  XmNlabelString, xmstr,
		  NULL);

    /* free the storage from the XmString */
    XmStringFree(xmstr);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
