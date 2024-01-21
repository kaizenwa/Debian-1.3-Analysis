#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>

Widget toplevel;

int
main (int argc, char **argv)
{
	XtAppContext	appc;
	Widget	rc, tmp, tmpe;

	toplevel = XtVaAppInitialize (&appc, "Boxes", NULL, 0,
	&argc, argv, NULL, NULL);

	rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
		XmNnumColumns,	2,
	    NULL);
	tmp = XtVaCreateManagedWidget("question", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "question", sizeof(char *),
		XmNdialogType,	XmDIALOG_QUESTION,
		XmNwidth, 200, XmNheight, 100,
	    NULL);

	tmp = XtVaCreateManagedWidget("message", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "message", sizeof(char *),
		XmNdialogType,	XmDIALOG_MESSAGE,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("error", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "error", sizeof(char *),
		XmNdialogType,	XmDIALOG_ERROR,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("work", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "work", sizeof(char *),
		XmNdialogType,	XmDIALOG_WORKING,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("information", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "information", sizeof(char *),
		XmNdialogType,	XmDIALOG_INFORMATION,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmpe = XtVaCreateManagedWidget("warning", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "warning", sizeof(char *),
		XmNdialogType,	XmDIALOG_WARNING,
		XmNwidth, 200, XmNheight, 100,
	    NULL);

	XtRealizeWidget (toplevel);

#if 0
    {
/*
 * don't ask, you don't want to know
 */
	Pixmap pix, npix;
	unsigned int tmpi, tmpw, tmph;
	int tmpx, tmpy;
	Window tmpwin;
	unsigned depth;
	GC gc;
	XGCValues values;


	XtVaGetValues(XmMessageBoxGetChild(tmpe, XmDIALOG_SYMBOL_LABEL),
		      XmNlabelPixmap, &pix,
		      XmNforeground, &values.foreground,
		      XmNbackground, &values.background,
		      NULL);
	XGetGeometry(XtDisplay(tmpe),
                 pix,
                 &tmpwin,
                 &tmpx, &tmpy,
                 &tmpw, &tmph,
                 &tmpi,&depth);

	npix =  XCreatePixmap(XtDisplay(tmpe), RootWindowOfScreen(XtScreen(tmpe)), tmpw, tmph, 1);

	values.function = GXcopyInverted;
        gc = XCreateGC(XtDisplay(tmpe), npix,
			 GCForeground|GCBackground|GCFunction, &values);

            XCopyPlane(XtDisplay(tmpe),
                       pix,
                       npix,
                       gc,
                       0,0,
                       tmpw, tmph,
                       0,0,
                       2);

	XWriteBitmapFile(XtDisplay(toplevel), "error.xbm", npix, tmpw, tmph, 0, 0);
    }
#endif
	XtAppMainLoop(appc);
	exit (0);
}
