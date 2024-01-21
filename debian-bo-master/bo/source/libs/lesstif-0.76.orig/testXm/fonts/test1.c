/**
 *
 * test1 of font list
 *
 **/

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/LabelP.h>

int
main(int argc, char **argv)
{
    Widget toplevel;
    XtAppContext app;
    XmFontList fontlist;
    XmFontList foo;

    XtSetLanguageProc(NULL, NULL, NULL);
    
    toplevel = XtVaAppInitialize(&app, "Font", NULL, 0, &argc, argv, NULL, NULL);

    fontlist = XmFontListAppendEntry(NULL,
				     XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
							   XmFONT_IS_FONT,
							   XLoadQueryFont(XtDisplay(toplevel), 
									  "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

    foo = _XmGetDefaultFontList(toplevel, XmBUTTON_FONTLIST);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
    exit(0);
}
