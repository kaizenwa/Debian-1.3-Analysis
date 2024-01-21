
#include <Xm/LabelGP.h>
#include <Xm/BulletinB.h>

static String fallback[] = {
	"*XmBulletinBoard.background:	green",
	"*XmLabelGadget.background:	red",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;
  XmFontList fontlist;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, fallback, NULL);

  fontlist = XmFontListAppendEntry(NULL,
				   XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
							 XmFONT_IS_FONT,
							 XLoadQueryFont(XtDisplay(toplevel), 
									"-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("One", xmLabelGadgetClass, two, NULL); /*XmNfontList, fontlist, NULL);*/

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
