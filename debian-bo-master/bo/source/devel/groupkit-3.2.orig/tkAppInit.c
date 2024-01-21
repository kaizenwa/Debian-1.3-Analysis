/* 
 * tkAppInit.c --
 *
 *	Application initiation function for GroupKit (gkwish).
 */

#include <tk.h>

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;

static int withTk = 1;

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
	if ((argc>=2) && (strcmp(argv[1], "-notk")==0)) {
		withTk = 0;
		Tcl_Main(argc, argv, Tcl_AppInit);
	} else {
		Tk_Main(argc, argv, Tcl_AppInit);
	}
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
	if (withTk) {
		if (Tk_Init(interp) == TCL_ERROR) {
			return TCL_ERROR;
		}
		Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);
	}

    if (Tgk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    return TCL_OK;
}
