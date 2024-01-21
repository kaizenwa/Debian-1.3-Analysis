#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>

int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	Arg             al[5];
	register int    ac = 0;
	XrmValue        from_value, to_value;
	XtAppContext    app_context;
	Display        *display;
	Widget          shell = (Widget) NULL;
	Widget          text = (Widget) NULL;
	/* Use default language procedure to set locale */
	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL,
			  (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "XApplication",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	XtSetArg(al[ac], XmNallowShellResize, True);
	ac++;
	XtSetArg(al[ac], XmNargc, argc);
	ac++;
	XtSetArg(al[ac], XmNargv, argv);
	ac++;
	/* Set inputMethod to use xwnmo */
	XtSetArg(al[ac], XmNinputMethod, "_XWNMO");
	ac++;
	shell = XtAppCreateShell(argv[0], "XApplication",
			      applicationShellWidgetClass, display, al, ac);
	/* Let Xlib find a font set appropriate to the locale */
	ac = 0;
	from_value.addr = "-*-*-*-*-*-*-16-*-*-*-*-*-*-*;:";
	from_value.size = strlen(from_value.addr) + 1;
	to_value.addr = NULL;
	XtConvertAndStore(shell, XmRString, &from_value,
			  XmRFontList, &to_value);
	XtSetArg(al[ac], XmNfontList, *(XmFontList *) to_value.addr);
	ac++;
	text = XmCreateText(shell, "text", al, ac);
	XtManageChild(text);
	XtRealizeWidget(shell);
	XtAppMainLoop(app_context);
	exit(0);
}
