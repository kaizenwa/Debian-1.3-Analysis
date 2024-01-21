/* test for showAsDefault and defaultButtonShadowThickness resources */

#include <Xm/Xm.h>
#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1;
  Dimension w,i,s,l,t,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nDefault\nButton", "MY_FONT");

  two = XtVaCreateManagedWidget("Button1", xmBulletinBoardWidgetClass,
				 toplevel, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonGadgetClass, 
                                two, 
				XmNfontList, fontlist, 
				XmNlabelString, xmstr1, 
				XmNshowAsDefault, 1,
				XmNdefaultButtonShadowThickness, 3,
				NULL);

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
