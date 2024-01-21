/*
 * scotty.c
 *
 * This is scotty, a simple tcl interpreter with some special commands
 * to get information about TCP/IP networks. 
 *
 * Copyright (c) 1993, 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "scotty.h"

#ifdef HAVE_TCLX
extern int
TclX_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

static int ined = 0;            /* dont initialize for tkined */
static char *progname;          /* the name of the game */

static Tcl_Interp *interp;      /* the main tcl interpreter */

static Tcl_DString command;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */

static char *rcFileName = "~/.scottyrc";

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
EventLoop		_ANSI_ARGS_((Tcl_Interp *interp));

/*
 * Taken from the tkMain.c file:
 */

static void
Prompt			_ANSI_ARGS_((Tcl_Interp *interp, int partial));

static void		
StdinProc		_ANSI_ARGS_((ClientData clientData, int mask));

/*
 * This is the main event loop. It does one tk event and checks whether
 * there are command left send from the tkined editor.
 */

static void
EventLoop (interp)
     Tcl_Interp *interp;
{
    while (Tk_DoOneEvent (0)) {
	/* empty loop body */
    }
}

/*
 * Initialize a new Tcl interpreter.
 */

int
Tcl_AppInit (interp)
    Tcl_Interp *interp;
{
    Tcl_SetVar(interp, "scotty_RcFileName", 
	       rcFileName ? rcFileName : "", TCL_GLOBAL_ONLY);

    if (Tcl_Init (interp) != TCL_OK) {
        return TCL_ERROR;
    }

#ifdef HAVE_TCLX
    if (TclX_Init (interp) != TCL_OK) {
        return TCL_ERROR;
    }
#endif

    if (Tk_EventInit (interp) != TCL_OK) {
	return TCL_ERROR;
    }

    if (Scotty_Init (interp) != TCL_OK) {
        return TCL_ERROR;
    }
    
    return TCL_OK;
}

/*
 * here we start
 */

int
main(argc, argv)
    int argc;
    char **argv;
{
    char *fileName = NULL;
    char *query = NULL;
    char buffer[20];
    Tcl_DString sc_argv;
    int i, c;
    int sc_argc = 0;
    int errflg = 0;
    int res;
    int expectOption = 1;

    if ((progname = strrchr(argv[0], '/')) == NULL) {
	progname = argv[0];
    } else {
	progname++;
    }

    Tcl_DStringInit (&sc_argv);

    for (i = 1; i < argc; i++) {
        if (!expectOption || argv[i][0] != '-') {
	    expectOption = 0;
	    Tcl_DStringAppendElement (&sc_argv, argv[i]); 
	    sc_argc++;
	    continue;
	}
	for (c = 1; (i < argc) && (c < strlen(argv[i])); c++) {
	    switch (argv[i][c]) {
	      case 'c' :
		if ( (argv[i][c+1] == '\0') && (++i < argc) ) {
		    query = argv[i];
		    c = strlen(argv[i]);
		} else errflg++;
		break;
	      case 'f':
		if ( (argv[i][c+1] == '\0') && (++i < argc) ) {
		    fileName = argv[i]; 
		    c = strlen(argv[i]);
		} else errflg++;
		break;
	      case 'n':
		rcFileName = NULL;
		break;
	      case 'i':
		ined++;
		break;
	      case '?':
		errflg++;
		break;
	      case '-':
		c = strlen(argv[i]);
		for (i++; i < argc; i++) {
		    Tcl_DStringAppendElement (&sc_argv, argv[i]);
		    sc_argc++;
		}
		break;
	      default:
		c = strlen(argv[i]);
		Tcl_DStringAppendElement (&sc_argv, argv[i]);
		sc_argc++;
		break;
	    }
	}
    }

    if (errflg) {
	fprintf (stderr, "usage: %s [-c query] [-f file] [-n] [-i]\n", 
		 progname);
	return 2;
    }


    /* Create and initialize the Tcl interpreter. */

    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    /* Set the tcl_interactive variable. */

    tty = isatty (0);
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    /* Register all new commands. */

    if (Tcl_AppInit(interp) == TCL_ERROR) {
	fprintf(stderr, "Tcl_AppInit failed: %s\n", interp->result);
        return TCL_ERROR;
    }

    /* Set the global variables argc and argv. */

    Tcl_SetVar (interp, "argv", Tcl_DStringValue(&sc_argv), TCL_GLOBAL_ONLY);
    Tcl_SetVar (interp, "argv0", (fileName != NULL) ? fileName : argv[0],
		TCL_GLOBAL_ONLY);
    Tcl_DStringFree (&sc_argv);
    sprintf (buffer, "%d", sc_argc);
    Tcl_SetVar (interp, "argc", buffer, TCL_GLOBAL_ONLY);

    /*
     * Process a single command.
     */

    if (query) {
	int result = Tcl_Eval (interp, query);
	if (result == TCL_OK) {
	    if (*interp->result != 0) {
		printf ("%s\n", interp->result);
	    }
	} else {
	    fprintf (stderr, "Error: %s\n%s\n", interp->result,
		     Tcl_GetVar (interp, "errorInfo", TCL_GLOBAL_ONLY));
	}
	EventLoop (interp);
	return 0;
    }
    
    /*
     * Process queries from a script file.
     */

    if (fileName) {

	if (ined) {
	    if (Tcl_Eval (interp, "ined size") != TCL_OK) {
		fprintf (stderr, "Fatal: can not talk to tkined: %s\n",
			 interp->result);
		exit (32);
	    }
	}

	res = Tcl_EvalFile (interp, fileName);
	if (res != TCL_OK) {
	    fprintf (stderr, "Error: %s\n", interp->result);
	}

	EventLoop (interp);
	return res;
    }
    
    /*
     * We are interactive -- that's where the fun beginns.
     */

    if (tty) {
	Tcl_SetVar (interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY);
    }
    fflush (stdout);

    Tcl_DStringInit(&command);
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData) 0);
    if (tty) {
	Prompt(interp, 0);
    }

    EventLoop (interp);
    return 0;
}


/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;				/* Not used. */
{
#define BUFFER_SIZE 4000
    char input[BUFFER_SIZE+1];
    static int gotPartial = 0;
    char *cmd;
    int code, count;

    count = read(fileno(stdin), input, BUFFER_SIZE);
    if (count <= 0) {
	if (!gotPartial) {
	    if (tty) {
		Tcl_Eval(interp, "exit");
		exit(1);
	    } else {
		Tk_DeleteFileHandler(0);
	    }
	    return;
	} else {
	    count = 0;
	}
    }
    cmd = Tcl_DStringAppend(&command, input, count);
    if (count != 0) {
	if ((input[count-1] != '\n') && (input[count-1] != ';')) {
	    gotPartial = 1;
	    goto prompt;
	}
	if (!Tcl_CommandComplete(cmd)) {
	    gotPartial = 1;
	    goto prompt;
	}
    }
    gotPartial = 0;

    /*
     * Disable the stdin file handler while evaluating the command;
     * otherwise if the command re-enters the event loop we might
     * process commands from stdin before the current command is
     * finished.  Among other things, this will trash the text of the
     * command being evaluated.
     */

    Tk_CreateFileHandler(0, 0, StdinProc, (ClientData) 0);
    code = Tcl_RecordAndEval(interp, cmd, 0);
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData) 0);
    Tcl_DStringFree(&command);
    if (*interp->result != 0) {
	if ((code != TCL_OK) || (tty)) {
	    /*
	     * The statement below used to call "printf", but that resulted
	     * in core dumps under Solaris 2.3 if the result was very long.
	     */

	    puts(interp->result);
	}
    }

    /*
     * Output a prompt.
     */

    prompt:
    if (tty) {
	Prompt(interp, gotPartial);
    }
    Tcl_ResetResult(interp);
}

/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script
 *	to issue the prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated
 *	in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(interp, partial)
    Tcl_Interp *interp;			/* Interpreter to use for prompting. */
    int partial;			/* Non-zero means there already
					 * exists a partial command, so use
					 * the secondary prompt. */
{
    char *promptCmd;
    int code;

    promptCmd = Tcl_GetVar(interp,
	partial ? "tcl_prompt2" : "tcl_prompt1", TCL_GLOBAL_ONLY);
    if (promptCmd == NULL) {
	defaultPrompt:
	if (!partial) {
	    fputs("% ", stdout);
	}
    } else {
	code = Tcl_Eval(interp, promptCmd);
	if (code != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		    "\n    (script that generates prompt)");
	    fprintf(stderr, "%s\n", interp->result);
	    goto defaultPrompt;
	}
    }
    fflush(stdout);
}
