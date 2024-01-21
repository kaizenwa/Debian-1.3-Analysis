/*
 * event.c
 *
 * The event command allows to do event driven programming inside of
 * Tcl scripts easily. The basic idea is that you can raise an event
 * which will invoke all event handlers that match the events tag.
 * The idea was born as network management scripts usually contain
 * a part to detect error situations and scripts to handle errors
 * It is easy to glue things together by binding (multiple) scripts
 * to handle events.
 *
 * Copyright (c) 1995
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

/*
        event bind
	event bind tag 
	event bind tag script
	event bind tag +script
	event raise <tag>
 */

/*
 * Structure used to describe an event.
 */

typedef struct Event {
    Tcl_Interp *interp;		/* The Tcl interpreter to use. */
    char *cmd;			/* The command to evaluate. */
    char *args;			/* The arguments to the command. */
} Event;

/*
 * The following hash table keeps a record for each existing binding.
 */

static Tcl_HashTable tagTable;

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
EventProc		_ANSI_ARGS_((ClientData clientData));

static int
BindEvent		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));

static int
RaiseEvent		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));


/*
 * This is the callback that actually handles a raised event.
 * Issue a background error if the callback fails for some reason.
 */

static void
EventProc (clientData)
     ClientData clientData;
{
    Event *evPtr = (Event *) clientData;
    int code;
    char *cmd = ckalloc(strlen(evPtr->cmd) + strlen(evPtr->args) + 2);
    sprintf(cmd, "%s %s", evPtr->cmd, evPtr->args);

    Tcl_AllowExceptions(evPtr->interp);
    code = Tcl_GlobalEval(evPtr->interp, cmd);
    if (code == TCL_ERROR) {
	Tcl_AddErrorInfo(evPtr->interp, "\n    (event callback)");
        Tk_BackgroundError(evPtr->interp);
    }

    ckfree(cmd);

    ckfree(evPtr->cmd);
    ckfree(evPtr->args);
    ckfree((char *) evPtr);
}


/*
 * BindEvent() creates or returns the binding for a particular tag.
 * Binding to an empty string will remove an existing binding.
 */

static int
BindEvent (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;

    if (argc == 2) {
        Tcl_HashSearch search;
        entryPtr = Tcl_FirstHashEntry(&tagTable, &search);
	while (entryPtr) {
	    Tcl_AppendElement(interp, Tcl_GetHashKey (&tagTable, entryPtr));
	    entryPtr = Tcl_NextHashEntry(&search);
	}
    } else if (argc == 3) {
        entryPtr = Tcl_FindHashEntry(&tagTable, argv[2]);
	if (entryPtr) {
	    Tcl_SetResult(interp, (char *) Tcl_GetHashValue(entryPtr), 
			  TCL_STATIC);
	}
    } else if (argc == 4) {
        int isNew, append = argv[3][0] == '+';
	char *newCmd, *oldCmd = NULL;
	if (append) {
	    argv[3]++;
	}
	entryPtr = Tcl_FindHashEntry(&tagTable, argv[2]);
	if (entryPtr) {
	    oldCmd = (char *) Tcl_GetHashValue(entryPtr);
	}
	if (argv[3][0] == '\0') {
	    if (entryPtr) {
	        Tcl_DeleteHashEntry(entryPtr);
	    }
	} else {
	    if (append && oldCmd) {
	        newCmd = ckalloc(strlen(oldCmd) + strlen(argv[3]) + 2);
		sprintf(newCmd, "%s\n%s", oldCmd, argv[3]);
	    } else {
	        newCmd = ckstrdup(argv[3]);
		if (! entryPtr) {
		   entryPtr = Tcl_CreateHashEntry(&tagTable, argv[2], &isNew);
		}
	    }
	    Tcl_SetHashValue(entryPtr, (ClientData) newCmd);
	}
	if (oldCmd) {
	    ckfree(oldCmd);
	}
    } else {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " bind ?pattern? ?command?\"", (char *) NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}


/*
 * RaiseEvent() creates an event and prepares to triggers all event
 * handlers that are created for the given tag. Some issues here:
 * Should be allow a tag list? And should be allow to match the tag
 * against those tags in the tagTable?
 */

static int
RaiseEvent (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;

    if (argc < 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " raise tag ?args?\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    entryPtr = Tcl_FindHashEntry(&tagTable, argv[2]);
    if (entryPtr) {
        Event *evPtr = (Event *) ckalloc(sizeof(Event));
	evPtr->interp = interp;
	evPtr->cmd = ckstrdup((char *) Tcl_GetHashValue(entryPtr));
	evPtr->args = Tcl_Merge(argc-3, argv+3);
#if 0
	Tk_CreateTimerHandler(0, EventProc, (ClientData) evPtr);
/*	Tk_DoWhenIdle(EventProc, (ClientData) evPtr); */
#else
	EventProc((ClientData) evPtr);
	Tcl_ResetResult(interp);
#endif
    }
  
    return TCL_OK;
}


/*
 * Scotty_EventCmd() implements the event command as described in the
 * scotty documentation. 
 */

int
Scotty_EventCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    int length;
    char c;
    static int initialized = 0;

    if (!initialized) {
        Tcl_InitHashTable(&tagTable, TCL_STRING_KEYS);
        initialized = 1;
    }

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    c = argv[1][0];
    length = strlen(argv[1]);

    if (strncmp(argv[1], "bind", length) == 0) {
	return BindEvent(interp, argc, argv);

    } else if (strncmp(argv[1], "raise", length) == 0) {
        return RaiseEvent(interp, argc, argv);

    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], 
		     "\": should be bind, or raise",
		     (char *) NULL);
    return TCL_ERROR;
}
