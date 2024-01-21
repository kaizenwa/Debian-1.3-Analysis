/*
 * icmp.c
 *
 * Extend a tcl command interpreter an icmp command. This module depends
 * on ntping written by Erik Schoenfelder (schoenfr@ibr.cs.tu-bs.de).
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

/*
 * The default filename where we will find the ntping binary. This
 * is normally overwritten in the Makefile.
 */

#ifndef NTPING
#define NTPING "/usr/local/bin/ntping"
#endif

/*
 * The following defined are used to distinguish the options
 * provided by the icmp command.
 */

#define ICMP_ECHO	1
#define ICMP_MASK	2
#define ICMP_TIMESTAMP	3
#define ICMP_TTL	4
#define ICMP_TRACE	5

/*
 * The following to variables hold the file descriptor used to
 * talk to the ntping process.
 */

static int ntpIn  = -1;
static int ntpOut = -1;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
xread		_ANSI_ARGS_((int fd, char *buf, int len));

static int
xwrite		_ANSI_ARGS_((int fd, char *buf, int len));

static int
ForkNtping	_ANSI_ARGS_((Tcl_Interp *interp));

static int
SendRequest	_ANSI_ARGS_((Tcl_Interp *interp, char *host, int type, int ttl,
			     int timeout, int retries, int delay, int size));
static int
RecvResponse	_ANSI_ARGS_((Tcl_Interp *interp));

/*
 * This wrapper for write() is needed to fix broken SYS V semantics.
 */

static int
xwrite (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    do {
	while ((rc = write(fd, buf, len)) < 0
	       && (errno == EINTR || errno == EAGAIN))
		continue;
	len -= rc;
	buf += rc;
    } while ((len > 0) && (rc > 0));

    return rc;
}

/*
 * This wrapper for read() is needed to fix broken SYS V semantics.
 */

static int 
xread (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    while ((rc = read(fd, buf, len)) < 0
	   && (errno == EINTR || errno == EAGAIN))
	    continue;

    return rc;
}

/*
 * Open the connection to ntping.
 */

static int
ForkNtping (interp)
     Tcl_Interp *interp;
{
    int *pidArray, code, argc = 2;
    static char *argv[3] = { NULL, "-b", 0 };

    if (! argv[0]) {
        argv[0] = getenv("SCOTTY_NTPING");
	if (! argv[0]) {
	    argv[0] = NTPING;
	}
	argv[0] = ckstrdup(argv[0]);
    }

    code = Tcl_CreatePipeline(interp, argc, argv, &pidArray, 
			      &ntpIn, &ntpOut, NULL);
    if (code != 1) {
        ckfree(argv[0]);
        argv[0] = NULL;
	return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * SendRequest() send a request to ntping. An error message is left
 * in interp->result if the operation fails.
 */

static int
SendRequest (interp, hosts, type, ttl, timeout, retries, delay, size)
     Tcl_Interp *interp;
     char *hosts;
     int type, ttl, timeout, retries, delay, size;
{
    char buffer[256];
    int rc;

    sprintf(buffer, "-t %d -r %d -d %d -s %d ", 
	    timeout, retries, delay, size);

    switch (type) {
    case ICMP_ECHO:
      break;
    case ICMP_MASK:
      strcat(buffer, "-mask ");
      break;
    case ICMP_TIMESTAMP:
      strcat(buffer, "-timestamp ");
      break;
    case ICMP_TTL:
      sprintf(buffer + strlen(buffer), "-ttl %d ", ttl);
      break;
    case ICMP_TRACE:
      sprintf(buffer + strlen(buffer), "-trace %d ", ttl);
      break;
    }

    rc = xwrite(ntpIn, buffer, strlen(buffer)); 
    if (rc > 0) {
        rc = xwrite(ntpIn, hosts, strlen(hosts));
	if (rc > 0) {
	    rc = write(ntpIn, "\n", 1);
	}
    }

    if (rc < 0) {
	Tcl_AppendResult(interp, "ntping: ", Tcl_PosixError(interp),
			 (char *) NULL);
        return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * RecvResponse() read the answer returned from the ntping server.
 * The result is written to interp->result and TCL_OK is returned.
 * An error message is left in interp->result if there is an error
 * while reading the answer send by ntping.
 */

static int
RecvResponse (interp)
     Tcl_Interp *interp;
{
    char buffer[1024];
    char *p = buffer;
    Tcl_DString dst;

    Tcl_DStringInit(&dst);

    while (xread(ntpOut, buffer, sizeof(buffer)) > 0) {
	p = buffer;
	while (*p != '\n' && p - buffer < sizeof(buffer)) {
	    p++;
	}
	Tcl_DStringAppend(&dst, buffer, p - buffer);
	if (*p == '\n') break;
    }

    if (*p != '\n') {
	Tcl_AppendResult(interp, "ntping: ", Tcl_PosixError(interp),
			 (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_DStringResult(interp, &dst);
    return TCL_OK;
}

/* 
 * Extend a tcl command interpreter with an icmp command. The
 * result will be a list of hosts and their round trip times
 * including -1 for unreachable hosts. The icmp command will fail,
 * if there are wrong arguments. When using the time to live (ttl)
 * feature, the command will return the host that answerd the udp
 * packet together with the round trip time.
 */

int
Scotty_IcmpCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    char *cmd = argv[0];
    int length;

    static int timeout = 5;	/* default timeout in s */
    static int retries = 2;	/* default # of retries: */
    static int size = 64;	/* default size in bytes */
    static int delay = 0;	/* default delay between packets */

    int actTimeout = -1;	/* actually used timeout */
    int actRetries = -1;	/* actually used retries */
    int actSize = -1;		/* actually used size */
    int actDelay = -1;		/* actually used delay */

    int type = 0;		/* the request type */
    int ttl = -1;		/* the time to live field */

    /*
     * Start ntping if not done yet.
     */

    if ((ntpIn == -1) || (ntpOut == -1)) {
	if (ForkNtping(interp) != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    if (argc == 1) {
      icmpWrongArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", cmd,
			 " ?-retries n? ?-timeout n? ?-size n? ?-delay n?",
			 " option ?arg? hosts\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Parse the options.
     */

    argc--; argv++;
    while (argc > 0 && (*argv[0] == '-')) {
	length = strlen (argv[0]);
	if (strncmp(argv[0], "-retries", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
		sprintf(interp->result, "%d", retries);
                return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv[0], &actRetries) != TCL_OK)
	        return TCL_ERROR;
	    if (actRetries < 0) {
                Tcl_SetResult(interp, "negative retries", TCL_STATIC);
                return TCL_ERROR;
            }
	    argc--, argv++;
	} else 	if (strncmp(argv[0], "-timeout", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
		sprintf(interp->result, "%d", timeout);
                return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv[0], &actTimeout) != TCL_OK)
                return TCL_ERROR;
	    if (actTimeout < 1) {
                Tcl_SetResult(interp, "negative timeout", TCL_STATIC);
                return TCL_ERROR;
            }
	    argc--, argv++;
	} else 	if (strncmp(argv[0], "-size", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
		sprintf(interp->result, "%d", size);
		return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv[0], &actSize) != TCL_OK)
                return TCL_ERROR;
	    if (actSize < 0) {
		Tcl_SetResult(interp, "negative size", TCL_STATIC);
                return TCL_ERROR;
	    }
	    argc--, argv++;
	} else  if (strncmp(argv[0], "-delay", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
		sprintf(interp->result, "%d", delay);
		return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv[0], &actDelay) != TCL_OK)
                return TCL_ERROR;
	    if (actDelay < 0) {
		Tcl_SetResult(interp, "negative delay", TCL_STATIC);
                return TCL_ERROR;
	    }
	    argc--, argv++;
	} else {
	    Tcl_AppendResult(interp, "unknown option \"", argv [0], "\"",
			     (char *) NULL);
            return TCL_ERROR;
	}
    }

    /*
     * No arguments left? Set the default values and return.
     */

    if (argc == 0) {
        if (actRetries >= 0) {
            retries = actRetries;
        }
        if (actTimeout > 0) {
            timeout = actTimeout;
        }
	if (actSize > 0) {
	    size = actSize;
	}
	if (actDelay >= 0) {
	    delay = actDelay;
	}
        return TCL_OK;
    }

    /*
     * Now we should have at least two arguments left!
     */

    if (argc < 2) {
	goto icmpWrongArgs;
    }

    actRetries = actRetries < 0 ? retries : actRetries;
    actTimeout = actTimeout < 0 ? timeout : actTimeout;
    actSize  = actSize  < 0 ? size  : actSize;
    actDelay = actDelay < 0 ? delay : actDelay;

    /*
     * Get the query type.
     */

    length = strlen(argv[0]);

    if (strncmp(argv [0], "echo", length) == 0) {
        type = ICMP_ECHO;
    } else if (strncmp(argv [0], "mask", length) == 0) {
        type = ICMP_MASK;
    } else if (strncmp(argv [0], "timestamp", length) == 0) {
        type = ICMP_TIMESTAMP;
    } else if (strncmp(argv [0], "ttl",length) == 0) {
        type = ICMP_TTL;
	argc--, argv++;
	if (argc < 2) {
	    goto icmpWrongArgs;
	}
	if (Tcl_GetInt(interp, argv[0], &ttl) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (ttl < 1) {
	    Tcl_SetResult(interp, "negative ttl", TCL_STATIC);
	    return TCL_ERROR;
	}
    } else if (strncmp(argv [0], "trace",length) == 0) {
        type = ICMP_TRACE;
	argc--, argv++;
	if (argc < 2) {
	    goto icmpWrongArgs;
	}
	if (Tcl_GetInt(interp, argv[0], &ttl) != TCL_OK) {
            return TCL_ERROR;
        }
        if (ttl < 1) {
            Tcl_SetResult(interp, "negative ttl", TCL_STATIC);
            return TCL_ERROR;
        }
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[0], "\": should be ",
			 "echo, mask, timestamp, ttl, or trace",
			 (char *) NULL);
	return TCL_ERROR;
    }
    argc--, argv++;

    /*
     * Append the hostnames left in argv and send the 
     * command to ntping.
     */

    if (argc != 1) {
	goto icmpWrongArgs;
    }
    
    if (SendRequest(interp, argv[0], type, ttl, 
		    actTimeout, actRetries, actDelay, actSize) != TCL_OK) {
        return TCL_ERROR;
    }

    return RecvResponse(interp);
}
