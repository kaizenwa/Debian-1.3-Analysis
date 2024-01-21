/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef STATE_H
#define STATE_H

/*
 * $Id: state.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

#include <setjmp.h>

#include "pset.h"
#include "xlog.h"

#include "mask.h"
#include "service.h"

struct read_only_state
{
	int		orig_max_descriptors ;	/* before we change the resource limit */
	int		max_descriptors ;			/* as returned by getdtablesize() 		*/
	int		process_limit ;			/* if 0, there is no limit 				*/
	unsigned loop_rate ;
	char		*config_file ;
	int		is_superuser ;
	char		**Argv ;
	int		Argc ;
} ;


struct defaults
{
	struct service_config 	*def_settings ;
   xlog_h						def_log ;
   bool_int						def_log_creation_failed ;
} ;


struct read_write_state
{
	int 					descriptors_free ;
	int 					available_services ;		/* # of available services 		*/
	int 					active_services ;			/* services with descriptors set */
															/* in socket mask						*/
	fd_set 				socket_mask ;
	int 					mask_max ;
	pset_h 				servers ;					/* table of running servers		*/
	pset_h 				retries ;					/* table of servers to retry		*/
	pset_h 				services ;					/* table of services					*/
	struct service		*logging ;
	struct service		*shutdown ;
	struct defaults 	defs ;
	xlog_h				program_log ;
	jmp_buf				env ;
	bool_int				env_is_valid ;
} ;

struct program_state
{
	mask_t						flags ;
	struct read_only_state	ros ;
	struct read_write_state rws ;
} ;

#define DEFAULTS( ps )						(ps).rws.defs.def_settings
#define DEFAULT_LOG( ps )					(ps).rws.defs.def_log
#define DEFAULT_LOG_ERROR( ps )			(ps).rws.defs.def_log_creation_failed
#define LOG_SERVICE( ps )					(ps).rws.logging
#define SHUTDOWN_SERVICE( ps )			(ps).rws.shutdown
#define SERVICES( ps )						(ps).rws.services
#define SERVERS( ps )						(ps).rws.servers
#define RETRIES( ps )						(ps).rws.retries

extern struct program_state ps ;

#endif	/* STATE_H */
