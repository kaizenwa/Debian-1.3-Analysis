/*
 * You should also run this with
 *	test1 -xrm "*tearOffModel: tear_off_enabled"
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>

#include <Xm/XmP.h>

#ifndef	LESSTIF_VERSION
#include <X11/Xmu/Editres.h>
#endif

char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	"*cascade.labelString:			Menu",
	"*cascade.mnemonic:			M",
	"*button1.labelString:			Dialog From Button",
	"*button1.mnemonic:			1",
	"*button1.acceleratorText:		Ctrl-1",
	"*button1.accelerator:			Ctrl<Key>1",
	"*button2.labelString:			Dialog From Pane",
	"*button2.mnemonic:			2",
	"*button2.acceleratorText:		Ctrl-2",
	"*button2.accelerator:			Ctrl<Key>2",
	"*button3.labelString:			Dialog From MenuBar",
	"*button3.mnemonic:			3",
	"*button3.acceleratorText:		Ctrl-3",
	"*button3.accelerator:			Ctrl<Key>3",
	"*button4.labelString:			Dialog From TopLevel",
	"*button4.mnemonic:			4",
	"*button4.acceleratorText:		Ctrl-4",
	"*button4.accelerator:			Ctrl<Key>4",
	"*okLabelString:			This is OK",
	"*cancelLabelString:			Cancel me if you can",
	NULL	/* The end */
};

XtAppContext theApp;
Widget toplevel, rc, cascade, pane, w;


/*
 * The Dialog that is a child of "w" will not work,
 * the one from "toplevel" will.
 */
void Doit(Widget w, XtPointer client, XtPointer call)
{
	Widget	b = NULL;
	int	c = (int)client;

	switch (c) {
	case 1:
		b = XmCreateQuestionDialog(w, "box-1", NULL, 0);
		break;
	case 2:
		b = XmCreateQuestionDialog(pane, "box-2", NULL, 0);
		break;
	case 3:
		b = XmCreateQuestionDialog(rc, "box-3", NULL, 0);
		break;
	case 4:
		b = XmCreateQuestionDialog(toplevel, "box-4", NULL, 0);
		break;
	}
	XtManageChild(b);
}

int
main(int argc, char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv, fallback, NULL);

#ifndef	LESSTIF_VERSION
    XtAddEventHandler(toplevel, (EventMask)0, True, _XEditResCheckMessages, NULL);
#endif

    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);

    pane = XmCreatePulldownMenu(rc, "pane", NULL, 0);

    cascade = XtVaCreateManagedWidget("cascade", xmCascadeButtonWidgetClass, rc,
		XmNsubMenuId,	pane,
	NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane,
	NULL);
    XtAddCallback(w, XmNactivateCallback, Doit, (XtPointer)1);

    w = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass, pane,
	NULL);
    XtAddCallback(w, XmNactivateCallback, Doit, (XtPointer)2);

    w = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass, pane,
	NULL);
    XtAddCallback(w, XmNactivateCallback, Doit, (XtPointer)3);

    w = XtVaCreateManagedWidget("button4", xmPushButtonWidgetClass, pane,
	NULL);
    XtAddCallback(w, XmNactivateCallback, Doit, (XtPointer)4);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(theApp);    
    exit(0);
}
