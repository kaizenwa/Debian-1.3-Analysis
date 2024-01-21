
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget rowcol;

void one_callback(Widget w, XtPointer clientData, XtPointer callData)
{
  XtUnmanageChild(w);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
				   XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
							 XmFONT_IS_FONT,
							 XLoadQueryFont(XtDisplay(toplevel), 
									"-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

  rowcol = XtVaCreateManagedWidget("rowcolumn", 
                                   xmRowColumnWidgetClass, 
                                   toplevel, 
				   NULL);

  printf ("before set values \n");

  XtVaSetValues(rowcol,
		XmNentryAlignment, XmALIGNMENT_CENTER,
                XmNisAligned, True,
		XmNorientation, XmHORIZONTAL, 
		XmNpacking, XmPACK_COLUMN,
		XmNnumColumns, 2,
                XmNadjustLast, True,
		NULL);

  one = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, rowcol, XmNfontList, fontlist, NULL);
  (void)XtVaCreateManagedWidget("Two", xmPushButtonWidgetClass, rowcol, XmNfontList, fontlist, NULL);
  (void)XtVaCreateManagedWidget("Three", xmPushButtonWidgetClass, rowcol, XmNfontList, fontlist, NULL);
  (void)XtVaCreateManagedWidget("Four", xmPushButtonWidgetClass, rowcol, XmNfontList, fontlist, NULL);

  XtAddCallback(one, XmNactivateCallback, one_callback, NULL);
  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  exit(0);
}
