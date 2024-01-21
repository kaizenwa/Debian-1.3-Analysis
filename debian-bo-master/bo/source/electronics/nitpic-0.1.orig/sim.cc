/*
 * sim.cc -- do the simulation
 */
#include "picsim.hh"

static SimAR	 sim_nil( SimFunc func, SimAR arg );

/*
 * Load the specified PicTools file & set the part info
 */
void
sim_init( const char *fname )
{
	int		 i, j;
	SimAR	 a;
	char	 buf[64];

	Pic.isize = 0;
	Pic.nstack = 0;
	Pic.reg_pages = 0;
	Pic.neeprom = 0;
	for (i = 0; i < 4; i++) {
		for (j = 0; j < 128; j++) {
			Pic.reg_file[i][j].redirect_page = i;
			Pic.reg_file[i][j].redirect_reg = j;
			Pic.reg_file[i][j].implemented = 0;
		}
	}

	Pic.simulate = sim_nil;
	
	Pic.wdt_min = Options.wdt_min;
	Pic.wdt_nominal = Options.wdt_nom;
	Pic.wdt_max = Options.wdt_max;
	Pic.uinfo.clock = 1;
	if (Pic.wdt_min > Pic.wdt_nominal || Pic.wdt_nominal > Pic.wdt_max) {
		fputs( "Watchdog timeout values must follow this relation:\n"
			  "WDTmin <= WDTnominal <= WDTmax\n", stderr );
		if (Pic.wdt_nominal < Pic.wdt_min)
			Pic.wdt_nominal = Pic.wdt_min * 1.1;
		if (Pic.wdt_max < Pic.wdt_nominal)
			Pic.wdt_max = Pic.wdt_nominal * 1.1;
	}

	if (PU_Read( fname, &Pic.uinfo, &i ) != PU_OK) {
		XtVaSetValues( W.title, XtNlabel, (XtPointer)"Nit-PIC", NULL );
		return;
	}

	switch (Pic.uinfo.pictype) {
	  case 84:
		Pic.simulate = pic16c84;
		XtVaSetValues( W.title, XtNlabel, (XtPointer)"Nit-PIC 16C84", NULL );
		break;
	  default:
		sprintf( buf, "PIC type %d not supported\n", Pic.uinfo.pictype );
		dialog( buf, 1, "OK" );
		break;
	}

	Pic.simulate( Init, a );
	redraw( );
}

/*
 * Reset the simulator
 */
void
sim_reset( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	SimAR	 a;
	
	Pic.running = 0;
	ipointer_erase( );
	Pic.simulate( Reset, a );
	ipointer_seek( w, ev, prms, nprm );
	redraw( );
}

/*
 * Step the simulator
 */
void
sim_step( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	Boolean	 ip_was_visible;
	SimAR	 a;

	if (Pic.running)
		ip_was_visible = False;
	else
		ip_was_visible = ipointer_erase( );

	a = Pic.simulate( Step, a );
	
	info_update( );
	reg_update( );
	
	if (ip_was_visible) {
		Cardinal bogus_nprm = 0;
		ipointer_seek( W.ipointer, ev, NULL, &bogus_nprm );
		ipointer_refresh( W.itext, ev, prms, nprm );
	}
}

static Boolean
sim_run_proc( XtPointer p )
{
	if (Pic.running) {
		SimAR	 a;
		a = Pic.simulate( Step, a );
		info_quick( );
		return False;
	} else {
		ipointer_seek( W.ipointer, NULL, NULL, NULL );
		ipointer_refresh( W.itext, NULL, NULL, NULL );
		info_refresh( W.info, NULL, NULL, NULL );
		reg_refresh( W.info, NULL, NULL, NULL );
		return True;
	}
}
	

/*
 * Toggle Run mode on the simulator.  If Run is on, start the
 * repeating stepper.
 */
void
sim_toggle_run( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	Pic.running = !Pic.running;

	if (Pic.running) {
		XtVaSetValues( W.run, XtNlabel, (XtPointer)"Pause", NULL );
		ipointer_erase( );
		reg_update( );
		XtAppAddWorkProc( AppContext, sim_run_proc, NULL );
	} else {
		XtVaSetValues( W.run, XtNlabel, (XtPointer)"Run", NULL );
	}
}

/*
 * simulator stub routine
 */
static SimAR
sim_nil( SimFunc func, SimAR arg )
{
	SimAR		 a;
	static char	 buf[32];
	
	switch (func) {
	  case Init:
		return a;
	  case Reset:
		return a;
	  case GetPage:
		a.i = 0;
		return a;
	  case GetInfo:
		a.p = NULL;
		return a;
	  case Disassemble:
		sprintf( buf, "PICx%d unsupported", Pic.uinfo.pictype );
		a.p = buf;
		return a;
	  case Step:
		return a;
	  default:
		return a;
	}
}

