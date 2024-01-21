/*
 * syslog.c
 *
 * A command to access the syslog facility.
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
 *
 */

#include "scotty.h"

#include <syslog.h>

/*
 * Extend a tcl command interpreter with a syslog command.
 */

int
Scotty_SyslogCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int level = -1;
    int length;

    if (argc == 1 || argc > 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " level message\"", (char *) NULL);
	return TCL_ERROR;
    }

    length = strlen(argv[1]);

    if (!strcmp(argv[1], "LOG_EMERG") 
	|| !(strncmp(argv[1], "emergency", length))) {
	level = LOG_EMERG;
    } else if (!strcmp(argv[1], "LOG_ALERT") 
	       || !(strncmp(argv[1], "alert", length))) {
	level = LOG_ALERT;
    } else if (!strcmp(argv[1], "LOG_CRIT") 
	       || !(strncmp(argv[1], "critical", length))) {
	level = LOG_CRIT;
    } else if (!strcmp(argv[1], "LOG_ERR") 
	       || !(strncmp(argv[1], "error", length))) {
	level = LOG_ERR;
    } else if (!strcmp(argv[1], "LOG_WARNING") 
	       || !(strncmp(argv[1], "warning", length))) {
	level = LOG_WARNING;
    } else if (!strcmp(argv[1], "LOG_NOTICE") 
	       || !(strncmp(argv[1], "notice", length))) {
	level = LOG_NOTICE;
    } else if (!strcmp(argv[1], "LOG_INFO") 
	       || !(strncmp(argv[1], "info", length))) {
	level = LOG_INFO;
    } else if (!strcmp(argv[1], "LOG_DEBUG") 
	       || !(strncmp(argv[1], "debug", length))) {
	level = LOG_DEBUG;
    }

    if (argc == 2 && level > 0) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " level message\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (level < 0) {
	Tcl_AppendResult(interp, "bad level \"", argv[1], 
			 "\": should be emergency, alert, critical, error,",
			 " warning, notice, info, or debug", (char *) NULL);
	return TCL_ERROR;
    }

#ifdef ultrix
    openlog("scotty", LOG_PID);
#else
    openlog("scotty", LOG_PID, LOG_USER);
#endif
    syslog(level, "%s", argv[2]);
    closelog();

    return TCL_OK;
}
