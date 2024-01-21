/*
 * clock.c
 *
 * This file includes the getclock and the getdate command.
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

#include <time.h>

/*
 * Get the current time.
 */

int
Scotty_GetclockCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    time_t clock;

    if (argc > 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", 
			 argv[0], " ?date?\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    if (argc == 2) {
	
	struct tm stm;
	char *day, *month;
	int res;

	static char *months[] = {
	    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
	    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	};
	
	day = ckstrdup(argv[1]);
	month = ckstrdup(argv[1]);
	
	res = sscanf(argv[1], "%s%s%2d%2d:%2d:%2d%4d", 
		     day, month, &stm.tm_mday, 
		     &stm.tm_hour, &stm.tm_min, &stm.tm_sec, &stm.tm_year);

	if (res != 7) {
	    ckfree (day);
	    ckfree (month);
	    Tcl_AppendResult(interp, "illegal date \"",
			     argv[1], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	
	stm.tm_year -= 1900;

	for (stm.tm_mon = 0; stm.tm_mon < 12; stm.tm_mon++) {
	    if (strcmp(months[stm.tm_mon], month) == 0) break;
	}
	if (stm.tm_mon == 12) {
	    Tcl_AppendResult(interp, "unknown month \"",
			     month, "\"", (char *) NULL);
	    ckfree(day);
	    ckfree(month);
	    return TCL_ERROR;
	}
	
	stm.tm_wday = 0;
	stm.tm_yday = 0;
	stm.tm_isdst = -1;
	
	ckfree(day);
	ckfree(month);	
	
	clock = mktime(&stm);

    } else {
        clock = time((time_t *) NULL);
    }

    sprintf(interp->result, "%ld", clock);
    return TCL_OK;
}

/*
 * Get the current date.
 */

int
Scotty_GetdateCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    time_t clock;
    char *p;

    if (argc > 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", 
			 argv[0], " ?clock?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 2) {
	if (Tcl_GetInt(interp, argv[1], (int *) &clock) != TCL_OK)
		return TCL_ERROR;
    } else {
	clock = time((time_t *) NULL);
    }
    
    interp->result = ctime(&clock);
    for (p = interp->result; *p != '\0'; p++) if (*p == '\n') *p = '\0';

    return TCL_OK;
}
