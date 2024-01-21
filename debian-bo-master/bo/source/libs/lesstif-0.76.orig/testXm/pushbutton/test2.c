/* test for multi font labels */

#include <Xm/Xm.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreate("Here is a ", "MY_FONT1");
  XmString xmstr2 = XmStringCreate("different font", "MY_FONT");

  XmString xmstr = XmStringConcat(xmstr1, xmstr2);

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  fontlist = XmFontListAppendEntry(fontlist,
			   XmFontListEntryCreate("MY_FONT1",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr, NULL);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
