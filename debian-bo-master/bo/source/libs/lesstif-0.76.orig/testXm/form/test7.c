#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

int
main(int argc, char **argv)
{
  Widget	top, form, w;
  XtAppContext	app;
  int		i, j;
  char		n[5];

  XtSetLanguageProc(NULL, NULL, NULL);

  top = XtVaAppInitialize(&app, "Form", NULL, 0, &argc, argv, NULL, NULL);

/* Note : no size given ! */
  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, top,
		XmNfractionBase,	3,
	NULL);

  for (i=0; i<3; i++)
    for (j=0; j<3; j++) {
      sprintf(n, "b%d%d", i, j);
      w = XtVaCreateManagedWidget(n, xmPushButtonWidgetClass, form,
		XmNtopAttachment,	XmATTACH_POSITION,
		XmNbottomAttachment,	XmATTACH_POSITION,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNtopPosition,		i,
		XmNbottomPosition,	i+1,
		XmNleftPosition,	j,
		XmNrightPosition,	j+1,
	NULL);
    }

  XtRealizeWidget(top);
  XtAppMainLoop(app);
  exit(0);
}
