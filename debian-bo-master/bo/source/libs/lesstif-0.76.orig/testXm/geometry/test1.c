#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <stdio.h>

/* two rowcolumns, each containing two pushbuttons,
 * inside a form.
 *
 * the rowcolumns are both attached to 50% of the form's width
 * and to the border of the form
 */

Widget toplevel, rc1, rc2, form, A, B, C, D;
XtAppContext theApp;

int
main(int argc,
     char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "menu", NULL, 0,
				 &argc, argv, NULL, NULL);

    form = XtVaCreateManagedWidget("form",
				   xmFormWidgetClass,
				   toplevel,
				   NULL);

    rc1 = XtVaCreateManagedWidget("rc1",
				  xmRowColumnWidgetClass,
				  form,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_POSITION,
				  XmNrightPosition, 50,
				  XmNrowColumnType, XmWORK_AREA,
				  XmNpacking, XmPACK_TIGHT,
				  XmNorientation, XmHORIZONTAL,
				  NULL);

    rc2 = XtVaCreateManagedWidget("rc2",
				  xmRowColumnWidgetClass,
				  form,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_POSITION,
				  XmNleftPosition, 50,
				  XmNrowColumnType, XmWORK_AREA,
				  XmNpacking, XmPACK_TIGHT,
				  XmNorientation, XmHORIZONTAL,
				  NULL);

    A = XtVaCreateManagedWidget("A",
				xmPushButtonWidgetClass,
				rc1,
				NULL);

    B = XtVaCreateManagedWidget("B",
				xmPushButtonWidgetClass,
				rc1,
				NULL);

    C = XtVaCreateManagedWidget("C",
				xmPushButtonWidgetClass,
				rc2,
				NULL);

    D = XtVaCreateManagedWidget("D",
				xmPushButtonWidgetClass,
				rc2,
				NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(theApp);
    exit(0);
}
