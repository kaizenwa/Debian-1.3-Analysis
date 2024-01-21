#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

void Push(Widget w, XtPointer client, XtPointer call)
{
	int		ac;
	Arg		al[5];
	Widget		fd, fr, f2, l, tf, sep, b1, b2, b3, b4, f3;
	XmString	xms;

	fd = XmCreateFormDialog(w, "formdialog", NULL, 0);
	fr = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, fd,
			XmNshadowType,		XmSHADOW_OUT,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		0,
			XmNrightOffset,		0,
		NULL);
	f2 = XtVaCreateManagedWidget ("form 2", xmFormWidgetClass, fr,
		NULL);
	xms = XmStringCreateLtoR("URL To Open: ", XmSTRING_DEFAULT_CHARSET);
	l = XtVaCreateManagedWidget("label", xmLabelWidgetClass, f2,
			XmNlabelString,		xms,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_NONE,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_NONE,
			XmNtopOffset,		14,
			XmNbottomOffset,	0,
			XmNleftOffset,		10,
			XmNrightOffset,		0,
		NULL);
	XmStringFree(xms);

	ac = 0;
	XtSetArg(al[ac], XmNwidth, 310); ac++;
	tf = XmCreateTextField(f2, "textfield", al, ac);
	XtManageChild(tf);
	XtVaSetValues(tf,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_NONE,
			XmNleftAttachment,	XmATTACH_WIDGET,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		0,
			XmNleftWidget,		l,
			XmNrightOffset,		0,
		NULL);

	sep = XmCreateSeparatorGadget(f2, "separator", NULL, 0);
	XtManageChild(sep);

	f3 = XtVaCreateManagedWidget("form3", xmFormWidgetClass, f2,
			XmNverticalSpacing,	8,
			XmNfractionBase,	4,
			XmNtopAttachment,	XmATTACH_NONE,
			XmNtopOffset,		0,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNbottomOffset,	0,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		0,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		0,
		NULL);

	XtVaSetValues(sep,
			XmNtopAttachment,	XmATTACH_WIDGET,
			XmNtopOffset,		10,
			XmNtopWidget,		tf,
			XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,	f3,
			XmNbottomOffset,	0,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		0,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		0,
		NULL);

	b1 = XtVaCreateManagedWidget("b1", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		8,
			XmNrightOffset,		4,
			XmNleftPosition,	0,
			XmNrightPosition,	1,
		NULL);
	b2 = XtVaCreateManagedWidget("b2", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		4,
			XmNleftPosition,	1,
			XmNrightPosition,	2,
		NULL);
	b3 = XtVaCreateManagedWidget("b3", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		4,
			XmNleftPosition,	2,
			XmNrightPosition,	3,
		NULL);
	b4 = XtVaCreateManagedWidget("b4", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		8,
			XmNleftPosition,	3,
			XmNrightPosition,	4,
		NULL);

	XtManageChild(fd);
}

int
main(int argc, char **argv)
{
  Widget	top, w;
  XtAppContext	app;

  XtSetLanguageProc(NULL, NULL, NULL);

  top = XtVaAppInitialize(&app, "Form", NULL, 0, &argc, argv, NULL, NULL);

  w = XtCreateManagedWidget("Push", xmPushButtonWidgetClass, top,
	NULL, 0);
  XtAddCallback(w, XmNactivateCallback, Push, 0);

  XtRealizeWidget(top);
  XtAppMainLoop(app);
  exit(0);
}
