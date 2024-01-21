#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>

Arg     args[20];
Cardinal        argcount;

main(int argc, char **argv)
{
        XtAppContext    appc;
        Widget          top, rc, t1, t2, t3;

        top = XtAppInitialize(&appc, "test", NULL, 0, &argc, argv, NULL, NULL, 0);

        argcount = 0;
        XtSetArg(args[argcount], XmNradioBehavior, True); argcount++;
        rc = XmCreateRowColumn(top, "rowc", args, argcount);
        XtManageChild(rc);

        argcount = 0;
        t1 = XmCreateToggleButton(rc, "t_b_1", args, argcount);
        XtManageChild(t1);

        argcount = 0;
        t2 = XmCreateToggleButton(rc, "Toggle_button_2", args, argcount);
        XtManageChild(t2);

        argcount = 0;
        t3 = XmCreateToggleButton(rc, "Third_toggle_button", args, argcount);
        XtManageChild(t3);

        XtRealizeWidget(top);
        XtAppMainLoop(appc);
}

