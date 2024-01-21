
#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer userData, XtPointer cbs) {
  XmPushButtonCallbackStruct *pbcs = (XmPushButtonCallbackStruct *)cbs;

  printf("PushButton activated: click_count: %d\n", pbcs->click_count);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;
  Dimension w,i,s,l,t,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("One", xmPushButtonGadgetClass, two,
				NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);

  XtVaGetValues(one,
		XmNhighlightThickness, &i,
		XmNshadowThickness, &s,
		XmNmarginWidth, &w,
		XmNmarginHeight, &h,
		XmNmarginLeft, &l,
		XmNmarginTop, &t,
		NULL);
printf("highlight: %d shad: %d marWid: %d marHei: %d marLeft: %d marTop: %d\n",
	 i, s, w, h, l, t);

  XtAppMainLoop(app);

  exit(0);
}
