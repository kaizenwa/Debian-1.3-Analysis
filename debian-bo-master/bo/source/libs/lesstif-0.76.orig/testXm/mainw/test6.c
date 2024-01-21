#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/MenuShell.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/Command.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel, menubar, filepane;
    Widget button, button1, button2, sep;
    Widget saveAsPane, button3, button4, button5;
    Widget button6, button7, editpane;
    Widget mb, com;
    Widget one, two, three;
    XmString xmstr;

    shell = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
				 &argc, argv, NULL, NULL);

    toplevel = XmCreateMainWindow(shell, "MainWindow", NULL, 0);
    XtVaSetValues(toplevel,
		  XmNshowSeparator, True,
		  XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE,
		  NULL);
    XtManageChild(toplevel);

    menubar = XmCreateMenuBar(toplevel,
			      "menuBar",
			      NULL,0);
    XtManageChild(menubar);

    filepane = XmCreatePulldownMenu(menubar,
				    "pane",
				    NULL,0);

    xmstr = XmStringCreateSimple("File");
    
    button = XtVaCreateManagedWidget("File",
				     xmCascadeButtonWidgetClass,
				     menubar,
				     XmNsubMenuId, filepane,
                                     XmNlabelString, xmstr,
				     NULL);

    editpane = XmCreatePulldownMenu(menubar,
				    "pane2",
				    NULL, 0);

    button6 = XtVaCreateManagedWidget("Edit",
				      xmCascadeButtonGadgetClass,
				      menubar,
				      XmNsubMenuId, editpane,
				      NULL);

    button7 = XtVaCreateManagedWidget("Cut",
				      xmPushButtonGadgetClass,
				      editpane,
				      NULL);

    button1 = XtVaCreateManagedWidget("Open",
				      xmPushButtonGadgetClass,
				      filepane,
				      NULL);

    sep = XtVaCreateManagedWidget("sep",
				  xmSeparatorGadgetClass,
                                  filepane,
                                  NULL);

    button2 = XtVaCreateManagedWidget("Close",
				      xmPushButtonWidgetClass,
				      filepane,
				      NULL);

    saveAsPane = XmCreatePulldownMenu(filepane,
				      "save_as",
				      NULL, 0);

    button3 = XtVaCreateManagedWidget("Save As",
				      xmCascadeButtonWidgetClass,
				      filepane,
				      XmNsubMenuId, saveAsPane,
				      NULL);

    button4 = XtVaCreateManagedWidget("MS Word",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    button5 = XtVaCreateManagedWidget("LaTeX",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    mb = XtVaCreateManagedWidget("mb", xmMessageBoxWidgetClass, toplevel, NULL);
    XtVaSetValues(toplevel, XmNmessageWindow, mb, NULL);

    com = XtVaCreateManagedWidget("com", xmCommandWidgetClass, toplevel, NULL);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				  NULL);

    two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);

    three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);

    XmMainWindowSetAreas(toplevel, menubar, com, NULL, NULL, one);

    XtRealizeWidget(shell);

    XtAppMainLoop(theApp);

    exit(0);
}

