#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>

Widget form, rc1, rc2;

void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  XmString s1, s2;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  form = XmCreateForm(toplevel, "form", NULL, 0);
  XtManageChild(form);

  s1 = XmStringCreateSimple("check1");
  s2 = XmStringCreateSimple("check2");
  rc1 = XmVaCreateSimpleCheckBox(form, "checkBox1", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    XmNleftAttachment, XmATTACH_FORM,
				    NULL);

  rc2 = XmVaCreateSimpleCheckBox(form, "checkBox2", cb,
				    XmNspacing, 2,
				    XmNmarginHeight, 4,
				    XmVaCHECKBUTTON, s1, 0, NULL, NULL,
				    XmVaCHECKBUTTON, s2, 0, NULL, NULL,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, rc1,
				    NULL);
  XtManageChild(rc1);
  XtManageChild(rc2);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
