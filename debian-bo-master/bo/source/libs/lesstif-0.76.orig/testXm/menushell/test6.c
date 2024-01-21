/*
 * Simulate the Mosaic splash window
 */
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget toplevel, push;
Widget splash, sform, label;

XtAppContext appc;

int
main(int argc,
     char **argv)
{
        toplevel = XtAppInitialize(&appc, "menushell1",
                NULL, 0, &argc, argv, NULL, NULL, 0);

	push = XtVaCreateManagedWidget("push", xmPushButtonWidgetClass,
		toplevel, NULL);

        splash = XtVaCreatePopupShell("Hello, World!", xmMenuShellWidgetClass, toplevel,
                XmNwidth,               100,
                XmNheight,              100,
                XmNx,                   100,
                XmNy,                   100,
                XmNallowShellResize,    False,
        NULL);

        sform = XtVaCreateManagedWidget("sform", xmRowColumnWidgetClass, splash,
                XmNheight,              100,
                XmNwidth,               100,
                XmNx,                   100,
                XmNy,                   100,
        NULL);

        label = XtVaCreateManagedWidget("Label", xmLabelWidgetClass, sform,
#ifdef  notdef
                XmNlabelType,   XmPIXMAP,
                XmNlabelPixmap, splashpix,
#endif
                XmNalignment,   XmALIGNMENT_CENTER,
                XmNx,           100,
                XmNy,           100,
        NULL);

        XtPopup(splash, XtGrabNone);
	XtRealizeWidget(toplevel);
        XtAppMainLoop(appc);
}
