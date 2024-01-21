/* test for showAsDefault and defaultButtonShadowThickness resources */

#include <Xm/Xm.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nDefault\nButton", "MY_FONT");

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, 
				XmNfontList, fontlist, 
				XmNlabelString, xmstr1, 
				XmNshowAsDefault, 1,
				XmNdefaultButtonShadowThickness, 10,
				NULL);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app);

  exit(0);
}
