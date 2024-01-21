/*
 * Geometry should look like Danny's phone tool.
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/List.h>

void Quit(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Suicide ... \n");
	exit(0);
}

int
main(int argc, char **argv)
{
  Widget	toplevel, form, mb, tf, sl, st, cb, menu, pb;
  XtAppContext	app;
  Arg		al[10];
  int		ac;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Phone", NULL, 0, &argc, argv, NULL, NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		XmNwidth,		450,
		XmNheight,		300,
		XmNresizable,		True,
		XmNfractionBase,	3,
	NULL);

  mb = XtVaCreateManagedWidget("mb", xmRowColumnWidgetClass, form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		0,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		0,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		0,
		XmNrowColumnType,	XmMENU_BAR,
	NULL);

  tf = XtVaCreateManagedWidget("tf", xmTextFieldWidgetClass, form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopOffset,		0,
		XmNtopWidget,		mb,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		0,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		0,
	NULL);

  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopWidget, tf); ac++;
  XtSetArg(al[ac], XmNtopOffset, 0); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_POSITION); ac++;
  XtSetArg(al[ac], XmNrightPosition, 1); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 0); ac++;
  sl = XmCreateScrolledList(form, "sl", al, ac);
  XtManageChild(sl);

  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopWidget, tf); ac++;
  XtSetArg(al[ac], XmNtopOffset, 0); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNleftWidget, XtParent(sl)); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 0); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 0); ac++;
  st = XmCreateScrolledText(form, "st", al, ac);
  XtManageChild(st);

  ac = 0;
  menu = XmCreatePulldownMenu(mb, "menu", al, ac);

  cb = XtVaCreateManagedWidget("cb", xmCascadeButtonGadgetClass, mb,
		XmNsubMenuId,	menu,
	NULL);

  pb = XtVaCreateManagedWidget("quit", xmPushButtonGadgetClass, menu,
	NULL);
  XtAddCallback(pb, XmNactivateCallback, Quit, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
