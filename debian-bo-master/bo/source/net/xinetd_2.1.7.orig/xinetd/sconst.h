/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef SCONST_H
#define SCONST_H

/*
 * $Id: sconst.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

/*
 * Names of internal non-visible services
 */
#define INTERCEPT_SERVICE_NAME			"intercept"
#define LOG_SERVICE_NAME					"logging"
#define SHUTDOWN_SERVICE_NAME				"shutdown"


/*
 * Log entry ids
 */
#define START_ENTRY							"START"
#define FAIL_ENTRY							"FAIL"
#define EXIT_ENTRY							"EXIT"
#define DATA_ENTRY							"DATA"
#define USERID_ENTRY							"USERID"
#define NOID_ENTRY							"NOID"

#endif 	/* SCONST_H */

