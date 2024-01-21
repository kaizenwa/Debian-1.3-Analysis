/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef CONF_H
#define CONF_H

/*
 * $Id: conf.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

#include "pset.h"

#include "sconf.h"

struct configuration
{
	pset_h cnf_service_confs ;
	struct service_config *cnf_defaults ;
} ;

#define CNF_DEFAULTS( confp )			(confp)->cnf_defaults
#define CNF_SERVICE_CONFS( confp )	(confp)->cnf_service_confs

status_e 					cnf_get() ;
status_e						cnf_init() ;
void 							cnf_free() ;
void							cnf_dump() ;
struct service_config	*cnf_extract() ;
unsigned						cnf_start_services() ;

#endif 	/* CONF_H */

