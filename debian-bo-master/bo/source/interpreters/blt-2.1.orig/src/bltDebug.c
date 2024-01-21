
/*
 * bltDebug.c --
 *
 * Copyright 1993-1996 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#include "bltInt.h"

#define DEBUG_VERSION "0.9"

/*ARGSUSED*/
static void
DebugProc(clientData, interp, level, command, proc, cmdClientData,
    argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* not used */
    int level;			/* Current level */
    char *command;		/* Command before substitution */
    int (*proc) ();		/* not used */
    ClientData cmdClientData;	/* not used */
    int argc;
    char **argv;		/* Command after parsing, but before
				 * evaluation */
{
    register int i;

    fprintf(stderr, "%d>\t%s\n\t", level, command);
    for (i = 0; i < argc; i++) {
	fprintf(stderr, "%s ", argv[i]);
    }
    fputs("\n", stderr);
}

/*ARGSUSED*/
static int
DebugCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    static Tcl_Trace token;
    static int level;
    int newLevel;

    if (argc > 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " ?level?\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (argc == 1) {
	sprintf(interp->result, "%d", level);
	return TCL_OK;
    }
    if (Tcl_GetInt(interp, argv[1], &newLevel) != TCL_OK) {
	return TCL_ERROR;
    }
    if (newLevel < 0) {
	newLevel = 0;
    }
    if (token != 0) {
	Tcl_DeleteTrace(interp, token);
    }
    if (newLevel > 0) {
	token = Tcl_CreateTrace(interp, newLevel, DebugProc, (ClientData)0);
    }
    level = newLevel;
    return TCL_OK;
}

int
Blt_DebugInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec = { "bltdebug", DEBUG_VERSION, DebugCmd, };

    return (Blt_InitCmd(interp, &cmdSpec));
}
