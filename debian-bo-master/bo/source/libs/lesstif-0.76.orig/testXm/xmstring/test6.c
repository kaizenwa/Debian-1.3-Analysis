#include <Xm/XmP.h>
#include <stdio.h>

int 
main(int argc,
     char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmString xmstr;
    char *text;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0,
				 &argc, argv, NULL, NULL);

    xmstr = XmStringCreateLtoR("Héllo\nWörld", XmFONTLIST_DEFAULT_TAG);

    XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &text);

    printf("text: %s\n", text);
    exit(0);
}
