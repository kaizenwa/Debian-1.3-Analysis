/*
 * demo.c - Demonstration der neuen ToggleButton-Klasse
 * Letzte Modifikation: 15.05.1994
 * Harald Albrecht ( albrecht@igpm.rwth-aachen.de )
 */

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include "NewToggleB.h"
#include <X11/keysym.h>
#include <stdio.h>

void DummyCallback(Widget w, XtPointer clientdata, XtPointer calldata)
{
    exit(0);
} /* DummyCallback */


int main(int argc, char **argv)
{
    XtAppContext AppCtx;
    Widget       TopLevel, Main, Menu, Form, Text, PullDownMenu;
    Widget       Cascade, Butt1, Butt3, Butt4, Butt5;
    Widget       Frame, Label, RowCol;
    Arg          args[10];
    
    TopLevel = XtVaAppInitialize(&AppCtx, "Demo", NULL, 0, 
                                 &argc, argv, NULL, NULL);
    Main = XmCreateMainWindow(TopLevel, "main", NULL, 0);
    XtManageChild(Main);

    Menu = XmCreateMenuBar(Main, "menu", NULL, 0);
    XtManageChild(Menu);
    XtSetArg(args[0], XmNmnemonic, XK_F);
    Cascade = XmCreateCascadeButton(Menu, "File", args, 1);
    XtManageChild(Cascade);
    PullDownMenu = XmCreatePulldownMenu(Cascade, "filemenu", NULL, 0);

    XtVaCreateManagedWidget(
        "Enjoy it", xmNewToggleButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_j,
        NULL);
    XtVaCreateManagedWidget(
        "sepp1", xmSeparatorWidgetClass, PullDownMenu,
        NULL);
    XtVaCreateManagedWidget(
        "One", xmNewToggleButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_O,
        XmNindicatorType, XmONE_OF_MANY,
        NULL);
    XtVaCreateManagedWidget(
        "Two", xmNewToggleButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_T,
        XmNindicatorType, XmONE_OF_MANY,
        NULL);
    XtVaCreateManagedWidget(
        "Fourtytwo", xmNewToggleButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_F,
        XmNindicatorType, XmONE_OF_MANY,
        NULL);
    XtVaCreateManagedWidget(
        "sepp2", xmSeparatorWidgetClass, PullDownMenu,
        NULL);
    Butt1 = XtVaCreateManagedWidget(
        "Exit", xmPushButtonWidgetClass, PullDownMenu,
        XmNmnemonic, XK_E,
        NULL);
    XtAddCallback(Butt1, XmNactivateCallback, 
                  (XtCallbackProc) DummyCallback, NULL);
    
    XtVaSetValues(Cascade, XmNsubMenuId, PullDownMenu, NULL);
    
    Form = XtVaCreateManagedWidget("form", xmFormWidgetClass, Main, NULL);
    Label = XtVaCreateManagedWidget(
        "XmN_OF_MANY", xmLabelWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      24,
	XmNtopAttachment,   XmATTACH_FORM, 
	XmNtopOffset,       8, 
	NULL);
    Frame = XtVaCreateManagedWidget(
        "frame1", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      20, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_POSITION, 
	XmNrightPosition,   50,
	XmNrightOffset,     4,  
	NULL);
    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
        NULL);

    Butt3 = XtVaCreateManagedWidget(
	"I like the OSF", xmNewToggleButtonWidgetClass, RowCol,
	XmNsensitive, False, 
	NULL);
    Butt4 = XtVaCreateManagedWidget(
	"Support Freeware!", xmNewToggleButtonWidgetClass, RowCol,
	XmNset, True,
	NULL);
    Butt5 = XtVaCreateManagedWidget(
	"New Widgets", xmNewToggleButtonWidgetClass, RowCol,
	NULL);

    Label = XtVaCreateManagedWidget(
        "XmONE_OF_MANY", xmLabelWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_POSITION, 
	XmNleftPosition,    50,
	XmNleftOffset,      4,
	XmNtopAttachment,   XmATTACH_FORM, 
	XmNtopOffset,       8, 
	NULL);
    Frame = XtVaCreateManagedWidget(
        "frame2", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_POSITION, 
	XmNleftPosition,    50,
	XmNleftOffset,      4, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_FORM, 
	XmNrightOffset,     20,  
	NULL);
    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
	XmNradioBehavior, True,
        XmNradioAlwaysOne,  True,
        NULL);

    Butt3 = XtVaCreateManagedWidget(
	"Athena", xmNewToggleButtonWidgetClass, RowCol,
        XmNindicatorType, XmONE_OF_MANY,
        XmNset, True,
	NULL);
    Butt4 = XtVaCreateManagedWidget(
	"OSF/Motif(=Hades)", xmNewToggleButtonWidgetClass, RowCol,
        XmNindicatorType, XmONE_OF_MANY,
	NULL);
    Butt5 = XtVaCreateManagedWidget(
	"Harry's Motif Widgets", xmNewToggleButtonWidgetClass, RowCol,
        XmNindicatorType, XmONE_OF_MANY,
	NULL);

    Label = XtVaCreateManagedWidget(
	"XmNtriState = True", 
	xmLabelWidgetClass, Form, 
	XmNleftAttachment,   XmATTACH_FORM, 
	XmNleftOffset,	     20, 
	XmNtopAttachment,    XmATTACH_WIDGET, 
	XmNtopWidget,        Frame, 
	XmNtopOffset,        8, 
	NULL);

    Frame = XtVaCreateManagedWidget(
        "frame3", xmFrameWidgetClass, Form, 
	XmNleftAttachment,  XmATTACH_FORM, 
	XmNleftOffset,      20, 
	XmNtopAttachment,   XmATTACH_WIDGET, 
	XmNtopWidget,       Label, 
	XmNrightAttachment, XmATTACH_FORM, 
	XmNrightOffset,     20,  
	NULL);

    RowCol = XtVaCreateManagedWidget(
	"RowCol", xmRowColumnWidgetClass, Frame, 
        NULL);
    Butt3 = XtVaCreateManagedWidget(
	"Click on me several times.", xmNewToggleButtonWidgetClass, RowCol,
        XmNtriState, True,
        XmNset, XmTOGGLE_DONTKNOW,
	NULL);
	
    Text = XtVaCreateManagedWidget(
	"The new xmNewToggleButtonWidgetClass", 
	xmLabelWidgetClass, Form, 
	XmNleftAttachment,   XmATTACH_FORM, 
	XmNleftOffset,	     20, 
	XmNrightAttachment,  XmATTACH_FORM, 
	XmNrightOffset,	     20, 
	XmNtopAttachment,    XmATTACH_WIDGET, 
	XmNtopWidget,        Frame, 
	XmNtopOffset,        20, 
	XmNbottomAttachment, XmATTACH_FORM, 
	XmNbottomOffset,     20, 
	NULL);

    XmMainWindowSetAreas(Main, Menu, NULL, NULL, NULL, Form);
    XtRealizeWidget(TopLevel);
    XtAppMainLoop(AppCtx);
    return 0;   
} /* main */
