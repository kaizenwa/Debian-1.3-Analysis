/*
 * main.cc -- PIC simulator main routine
 */
#define	__main__
#include "picsim.hh"

static String fallback_resources[] = {
	"*XPICsim*quit.translations: #override "
	"	<Btn1Up>:		notify() unset() quit()",
	NULL
} ;

static volatile void	 xusage( void );
static void				 copyright( Widget w, XtPointer closure,
								   XtPointer call_data );
static void				 echo( Widget w, XEvent *ev,
							  String *prms, Cardinal *nprm );
static void				 quit( Widget w, XEvent *ev,
							  String *prms, Cardinal *nprm );

static XtActionsRec	actions[] = {
	{ "echo", echo },
	{ "ic-refresh", ic_refresh },
	{ "info-refresh", info_refresh },
	{ "ipointer-refresh", ipointer_refresh },
	{ "ipointer-seek", ipointer_seek },
	{ "instr-refresh", instr_refresh },
	{ "load-file", load_file },
	{ "quit", quit },
	{ "reg-refresh", reg_refresh },
	{ "reg-switch", reg_switch },
	{ "sim-step", sim_step },
	{ "sim-reset", sim_reset },
	{ "toggle-run", sim_toggle_run }
} ;

#define Offset(field) XtOffsetOf(OptionsRec, field)
static XtResource resources[] =
{
	{ "nitpic-resources", "NitpicResources", XtRBoolean, sizeof(Boolean),
	  Offset(nitpic_resources), XtRString, (XtPointer)"false"},
	{ "wdt-color-1", "WDT-Color", XtRPixel, sizeof(Pixel),
	  Offset(wdt_color_1), XtRString, (XtPointer)"black"},
	{ "wdt-color-2", "WDT-Color", XtRPixel, sizeof(Pixel),
	  Offset(wdt_color_2), XtRString, (XtPointer)"black"},
	{ "wdt-color-3", "WDT-Color", XtRPixel, sizeof(Pixel),
	  Offset(wdt_color_3), XtRString, (XtPointer)"black"},
	{ "wdt-min", "WDT-Time", XtRFloat, sizeof(float),
	  Offset(wdt_min), XtRString, (XtPointer)"7e-3"},
	{ "wdt-nominal", "WDT-Time", XtRFloat, sizeof(float),
	  Offset(wdt_nom), XtRString, (XtPointer)"18e-3"},
	{ "wdt-max", "WDT-Time", XtRFloat, sizeof(float),
	  Offset(wdt_max), XtRString, (XtPointer)"33e-3"},
	{ "info-bits", XtCFont, XtRFontStruct, sizeof(XFontStruct*),
	  Offset(info_bits), XtRString, (XtPointer)XtDefaultFont },
	{ "led-on", "LED-Color", XtRPixel, sizeof(Pixel),
	  Offset(led_on), XtRString, (XtPointer)"black" },
	{ "led-off", "LED-Color", XtRPixel, sizeof(Pixel),
	  Offset(led_off), XtRString, (XtPointer)"white" },
	{ "left-pointer", "Bitmap", XtRBitmap, sizeof(Pixmap),
	  Offset(left_pointer), XtRString, (XtPointer)"lsolid.xbm" },
	{ "left-hollow", "Bitmap", XtRBitmap, sizeof(Pixmap),
	  Offset(left_hollow), XtRString, (XtPointer)"lhollow.xbm" },
	{ "right-pointer", "Bitmap", XtRBitmap, sizeof(Pixmap),
	  Offset(right_pointer), XtRString, (XtPointer)"rsolid.xbm" },
	{ "right-hollow", "Bitmap", XtRBitmap, sizeof(Pixmap),
	  Offset(right_hollow), XtRString, (XtPointer)"rhollow.xbm" },
};
#undef Offset

int
main( int argc, char **argv )
{
	W.toplevel = XtVaAppInitialize( &AppContext, "XPICsim",
								   NULL, 0,
								   &argc, argv,
								   fallback_resources,
								   NULL );

	XtVaGetApplicationResources( W.toplevel, (XtPointer)(&Options),
								resources, XtNumber(resources),
								NULL );

	if (!Options.nitpic_resources) xusage( );
	
	W.main = XtVaCreateManagedWidget( "main", formWidgetClass,
									 W.toplevel,
									 NULL );
	W.title = XtVaCreateManagedWidget( "title", labelWidgetClass,
									  W.main,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainTop,
									  NULL );
	W.copyright = XtVaCreateManagedWidget( "copyright", commandWidgetClass,
										  W.main,
										  XtNtop, XawChainTop,
										  XtNbottom, XawChainTop,
										  XtNfromVert, W.title,
										  XtNlabel,
										  ((XtPointer)
										   "copyright (c) 1994, 1995 dhm"),
										  NULL );
	XtAddCallback( W.copyright, "callback", copyright, NULL );
	
	W.info = XtVaCreateManagedWidget( "info", labelWidgetClass,
									 W.main,
									 XtNlabel, (XtPointer)"",
									 XtNfromVert, W.copyright,
									 XtNtop, XawChainTop,
									 NULL );
	W.ic = XtVaCreateManagedWidget( "ic", labelWidgetClass,
								   W.main,
								   XtNfromVert, (XtPointer)W.info,
								   XtNlabel, (XtPointer)"",
								   XtNbottom, XawChainBottom,
								   NULL );
	W.instr = XtVaCreateManagedWidget( "instr", formWidgetClass,
									  W.main,
									  XtNfromHoriz, (XtPointer)W.info,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainBottom,
									  NULL );
	W.ipointer = XtVaCreateManagedWidget( "ipointer", labelWidgetClass,
										 W.instr,
										 XtNtop, XawChainTop,
										 XtNbottom, XawChainBottom,
										 XtNright, XawChainLeft,
										 NULL );
	W.itext = XtVaCreateManagedWidget( "itext", labelWidgetClass,
									  W.instr,
									  XtNlabel, (XtPointer)"",
									  XtNfromHoriz, (XtPointer)W.ipointer,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainBottom,
									  XtNright, XawChainRight,
									  XtNleft, XawChainLeft,
									  NULL );
	W.iscroll = XtVaCreateManagedWidget( "iscroll", scrollbarWidgetClass,
										W.instr,
										XtNfromHoriz, (XtPointer)W.itext,
										XtNlabel, (XtPointer)"",
										XtNtop, XawChainTop,
										XtNbottom, XawChainBottom,
										XtNleft, XawChainRight,
										NULL );
	XtAddCallback( W.iscroll, "scrollProc", instr_scroll, NULL );
	XtAddCallback( W.iscroll, "jumpProc", instr_jump, NULL );
	W.regs = XtVaCreateManagedWidget( "regs", formWidgetClass,
									 W.main,
									 XtNfromHoriz, (XtPointer)W.instr,
									 XtNtop, XawChainTop,
									 NULL );
	W.rprev = XtVaCreateManagedWidget( "rprev", commandWidgetClass,
									  W.regs,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainTop,
									  XtNleft, XawChainLeft,
									  XtNright, XawChainLeft,
									  NULL );
	W.rpage = XtVaCreateManagedWidget( "rpage", labelWidgetClass,
									  W.regs,
									  XtNlabel, (XtPointer)"Page 0",
									  XtNfromHoriz, W.rprev,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainTop,
									  XtNleft, XawChainLeft,
									  XtNright, XawChainRight,
									  NULL );
	W.rnext = XtVaCreateManagedWidget( "rnext", commandWidgetClass,
									  W.regs,
									  XtNfromHoriz, W.rpage,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainTop,
									  XtNleft, XawChainRight,
									  XtNright, XawChainRight,
									  NULL );
	W.rdata = XtVaCreateManagedWidget( "rdata", labelWidgetClass,
									  W.regs,
									  XtNlabel, (XtPointer)"",
									  XtNfromVert, W.rpage,
									  XtNtop, XawChainTop,
									  XtNbottom, XawChainBottom,
									  XtNleft, XawChainLeft,
									  XtNright, XawChainRight,
									  NULL );
	W.data = XtVaCreateManagedWidget( "data", labelWidgetClass,
									 W.main,
									 XtNfromHoriz, (XtPointer)W.instr,
									 XtNfromVert, (XtPointer)W.regs,
									 XtNlabel, (XtPointer)"",
									 XtNbottom, XawChainBottom,
									 NULL );
	W.step = XtVaCreateManagedWidget( "step", commandWidgetClass,
									 W.main,
									 XtNfromVert, (XtPointer)W.ic,
									 XtNtop, XawChainBottom,
									 XtNbottom, XawChainBottom,
									 NULL );
	W.run = XtVaCreateManagedWidget( "run", toggleWidgetClass,
									W.main,
									XtNfromVert, (XtPointer)W.ic,
									XtNfromHoriz, (XtPointer)W.step,
									XtNtop, XawChainBottom,
									XtNbottom, XawChainBottom,
									NULL );
	W.interrupt = XtVaCreateManagedWidget( "int", commandWidgetClass,
										  W.main,
										  XtNfromVert, (XtPointer)W.ic,
										  XtNfromHoriz, (XtPointer)W.run,
										  XtNtop, XawChainBottom,
										  XtNbottom, XawChainBottom,
										  NULL );
	W.reset = XtVaCreateManagedWidget( "reset", commandWidgetClass,
									  W.main,
									  XtNfromVert, (XtPointer)W.ic,
									  XtNfromHoriz,(XtPointer)W.interrupt,
									  XtNtop, XawChainBottom,
									  XtNbottom, XawChainBottom,
									  NULL );
	W.file = XtVaCreateManagedWidget( "file", commandWidgetClass,
									 W.main,
									 XtNfromVert, (XtPointer)W.ic,
									 XtNfromHoriz,(XtPointer)W.reset,
									 XtNtop, XawChainBottom,
									 XtNbottom, XawChainBottom,
									 NULL );
	W.quit = XtVaCreateManagedWidget( "quit", commandWidgetClass,
									 W.main,
									 XtNfromVert, (XtPointer)W.ic,
									 XtNfromHoriz, (XtPointer)W.file,
									 XtNtop, XawChainBottom,
									 XtNbottom, XawChainBottom,
									 NULL );

	XtAppAddActions( AppContext, actions, XtNumber( actions ) );

	XtRealizeWidget( W.toplevel );

	sim_init( (argc > 1) ? argv[1] : "<none>" );

	XtAppMainLoop( AppContext );
}

static void
xusage_ok( Widget w, XtPointer closure, XtPointer call_data )
{
	exit( 0 );
}

static volatile void
xusage( void )
{
	Widget	 w, msg, ok;
	
	w = XtVaCreateManagedWidget( "main", formWidgetClass,
								W.toplevel,
								NULL );
	msg = XtVaCreateManagedWidget( "msg", labelWidgetClass, w,
								  XtNlabel,
								  (XtPointer)("Couldn't load resource file.\n"
											  "Can't continue; sorry."),
								  NULL );
	ok = XtVaCreateManagedWidget( "ok", commandWidgetClass, w,
								 XtNlabel, (XtPointer)"OK",
								 XtNfromVert, (XtPointer)msg,
								 NULL );
	XtAddCallback( ok, "callback", xusage_ok, 0 );

	XtRealizeWidget( W.toplevel );

	XtAppMainLoop( AppContext );

	exit( 0 );
}

static void
copyright( Widget w, XtPointer closure, XtPointer call_data )
{
	dialog("This program was written by Dave Madden, <dhm@proteon.com>. "
		   "Unauthorized Reproduction and Dissemination is encouraged.",
		   1, "OK" );
}

static void
echo( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	Cardinal	 i;

	fputs( "echo: ", stderr );
	for (i = 0; i < *nprm; i++) fputs( *prms++, stderr ), fputc( ' ', stderr );
	fputc( '\n', stderr );
}

static void
quit( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	fputs( "Goodbye.\n", stderr );
	exit( 0 );
}

