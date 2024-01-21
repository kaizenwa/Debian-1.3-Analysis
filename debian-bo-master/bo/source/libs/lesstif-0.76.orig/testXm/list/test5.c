/* Test for XmListAddItem */

#include <stdio.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

Widget toplevel, listw, formw, button1;

int current_item = 0;

void button_cb1(Widget w, XtPointer clientData, XtPointer callData)
{
    char string[100];
    XmString xmstr;

    sprintf (string, "XmScrolledList Item %d", current_item);

    xmstr = XmStringCreateSimple(string);

    XmListAddItemUnselected(listw, xmstr, 0);

    XmStringFree(xmstr);

    current_item ++;
}

int
main(int argc, char **argv)
{
    XtAppContext app;
    Arg arg[1];
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    formw = XmCreateForm(toplevel, "form", NULL, 0);
    button1 = XmCreatePushButton(formw, "Append Item", NULL, 0);

    XtSetArg(arg[0], XmNlistSizePolicy, XmCONSTANT);
    listw = XmCreateScrolledList(formw, "test_of_scrolled_list", arg, 1); 

    XtVaSetValues(listw, 
		  XmNvisibleItemCount, 5,
                  XmNscrollBarDisplayPolicy, XmSTATIC,
                  XmNselectionPolicy, XmBROWSE_SELECT,
                  NULL);
    
    XtVaSetValues(XtParent(listw),
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, button1,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues(button1,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    XtManageChild(formw);
    XtManageChild(button1);
    XtManageChild(listw);

    XtRealizeWidget(toplevel);

    XtAddCallback(button1, XmNactivateCallback, button_cb1, NULL);

    XtAppMainLoop(app);

    exit(0);
}
