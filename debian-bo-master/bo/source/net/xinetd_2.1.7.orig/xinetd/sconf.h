/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef SCONF_H
#define SCONF_H

/*
 * $Id: sconf.h,v 1.3 1995/09/10 22:02:12 chuck Exp $
 */
#ifdef BIND_IF
#include <sys/types.h>
#if defined(linux)
#undef IPPROTO_TCP
#endif /* linux */
#include <netinet/in.h>
#endif

#include "pset.h"
#include "env.h"
#include "sio.h"

#include "defs.h"
#include "log.h"
#include "mask.h"
#include "builtin.h"

/*
 * Service types
 */
#define ST_RPC          			1
#define ST_INTERNAL     			2
#define ST_UNLISTED     			3
#define ST_SPECIAL					4

/*
 * Service flags
 */
#define SF_INTERCEPT					1
#define SF_REUSE						2
#define SF_NORETRY					3
#define SF_IDONLY						4

/*
 * Values for log options
 */
#define LO_HOST      1
#define LO_DURATION  3
#define LO_ATTEMPT   4
#define LO_EXIT      5
#define LO_RECORD    6
#define LO_PID			7
#define LO_START     8
#define LO_USERID		9



struct rpc_data
{
   unsigned long rd_min_version ;
   unsigned long rd_max_version ;
   unsigned long rd_program_number ;
} ;

#define RD_MINVERS( rdp )				(rdp)->rd_min_version
#define RD_MAXVERS( rdp )				(rdp)->rd_max_version
#define RD_PROGNUM( rdp )			 	(rdp)->rd_program_number

typedef enum { NO_ENV = 0, STD_ENV, DEF_ENV, CUSTOM_ENV } environ_e ;

struct environment
{
   environ_e	env_type ;
	env_h			env_handle ;
} ;


/*
 * NOTE: Clearing the structure will give all its fields their default values
 */
struct service_config
{
   mask_t 		sc_specified_attributes ;  /* specified attributes          */
   mask_t 		sc_attributes_present ;    /* includes those from defaults  */
   mask_t 		sc_type ;                  /* RPC, UNLISTED etc.            */
	mask_t 		sc_flags ;				   /* REUSE, INTERCEPT etc.			*/
   char 		*sc_name ;                 /* e.g. "echo"                   */
   char 		*sc_id ;                   /* e.g. "echo-stream"            */
   unsigned 	sc_port ;                  /* in host byte order            */
   int 			sc_socket_type ;           /* e.g. SOCK_DGRAM               */
   struct name_value sc_protocol ;         /* e.g. "TCP", IPPROTO_TCP       */
   boolean_e 	sc_wait ;
   int			sc_uid ;
   int			sc_user_gid ;              /* gid corresponding to uid      */
   int			sc_gid ;                   /* gid corresponding to group    */
   char			*sc_server ;
   char			**sc_server_argv ;
   int			sc_instances ;
	int			sc_nice ;				   /* argument for nice(3)			*/
   pset_h		sc_env_var_defs ;          /* list of env strings           */
   pset_h		sc_pass_env_vars ;         /* env vars to pass to server    */
   pset_h		sc_access_times ;
   pset_h		sc_only_from ;
   pset_h		sc_no_access ;
   mask_t		sc_log_on_success ;
   mask_t		sc_log_on_failure ;
   struct log	sc_log ;
   struct rpc_data sc_rd ;
   pset_h		sc_disabled ;              /* used only by the default entry */
   struct environment sc_environment ;
   builtin_s	*sc_builtin ;
#ifdef BIND_IF
   struct in_addr		sc_interface ;		/* NV - interface to bind service */
#endif
} ;

#define SCP( p )						((struct service_config *)(p))

/*
 * Field access macros
 */
#define SC_LOG( scp )				(&(scp)->sc_log)
#define SC_RPCDATA( scp )        (&(scp)->sc_rd)
#define SC_ENV( scp )            (&(scp)->sc_environment)
#define SC_BUILTIN( scp )			(scp)->sc_builtin
#define SC_PORT( scp )				(scp)->sc_port
#define SC_NICE( scp )				(scp)->sc_nice
#define SC_SOCKET_TYPE( scp )		(scp)->sc_socket_type
#define SC_ID( scp )					(scp)->sc_id
#define SC_NAME( scp )				(scp)->sc_name
#define SC_PROTOVAL( scp )			(scp)->sc_protocol.value
#define SC_PROTONAME( scp )		(scp)->sc_protocol.name
#define SC_INSTANCES( scp )		(scp)->sc_instances
#define SC_UID( scp )				(scp)->sc_uid
#define SC_SERVER( scp )			(scp)->sc_server
#define SC_SERVER_ARGV( scp )		(scp)->sc_server_argv
#define SC_ONLY_FROM( scp )		(scp)->sc_only_from
#define SC_NO_ACCESS( scp )		(scp)->sc_no_access
#define SC_ACCESS_TIMES( scp )	(scp)->sc_access_times
#define SC_LOG_ON_SUCCESS( scp )	(scp)->sc_log_on_success
#define SC_LOG_ON_FAILURE( scp )	(scp)->sc_log_on_failure

#ifdef BIND_IF
#define SC_INTERFACE( scp )			(scp)->sc_interface
#endif

/*
 * Field set macros
 */
#define sc_set_port( scp, port )	     (scp)->sc_port = (port)
#ifdef BIND_IF
#define sc_set_interface( scp, address ) (scp)->sc_interface = (address)
#endif

/*
 * Predicate checking macros
 */
#define SC_FORKS( scp )				( ! SC_IS_INTERNAL( scp ) ||						\
													builtin_forks( (scp)->sc_builtin ) )
#define SC_WAITS( scp )				( (scp)->sc_wait == YES )
#define SC_RETRY( scp ) 			( M_IS_CLEAR( (scp)->sc_flags, SF_NORETRY ) )
#define SC_REUSE_ADDRESS( scp )	M_IS_SET( (scp)->sc_flags, SF_REUSE )
#define SC_MUST_IDENTIFY( scp )	M_IS_SET( (scp)->sc_flags, SF_IDONLY )

#define SC_IS_RPC( scp )			( M_IS_SET( (scp)->sc_type, ST_RPC ) )
#define SC_IS_INTERNAL( scp )		( M_IS_SET( (scp)->sc_type, ST_INTERNAL ) )
#define SC_IS_SPECIAL( scp )		( M_IS_SET( (scp)->sc_type, ST_SPECIAL ) )
#define SC_IS_UNLISTED( scp )		( M_IS_SET( (scp)->sc_type, ST_UNLISTED ) )
#define SC_IS_INTERCEPTED( scp )	( M_IS_SET( (scp)->sc_flags, SF_INTERCEPT ) )

#define LOGS_USERID( scp, flags )															\
		( M_IS_SET( (scp)->flags, LO_USERID ) && SC_ACCEPTS_CONNECTIONS( scp ) )

#define LOGS_ANY( scp, flags )				( ! M_ARE_ALL_CLEAR( (scp)->flags ) )

#define SC_LOGS_ON_SUCCESS( scp )			LOGS_ANY( scp, sc_log_on_success )
#define SC_LOGS_ON_FAILURE( scp )			LOGS_ANY( scp, sc_log_on_failure )
#define SC_LOGS_USERID_ON_FAILURE( scp )	LOGS_USERID( scp, sc_log_on_failure )
#define SC_LOGS_USERID_ON_SUCCESS( scp )	LOGS_USERID( scp, sc_log_on_success )
#define SC_LOGS_ON_EXIT( scp )																\
				( M_IS_SET( (scp)->sc_log_on_success, LO_DURATION ) ||				\
				M_IS_SET( (scp)->sc_log_on_success, LO_EXIT ) )
#define SC_RECORDS( scp )		M_IS_SET( (scp)->sc_log_on_failure, LO_RECORD ) 
#define SC_LOGS_PID( scp )		M_IS_SET( (scp)->sc_log_on_success, LO_PID )
#define SC_LOGS_EXITS( scp )	M_IS_SET( (scp)->sc_log_on_success, LO_EXIT )
#define SC_LOGS_DURATION( scp )		\
										M_IS_SET( (scp)->sc_log_on_success, LO_DURATION )


#define SC_MUST_LISTEN( scp )    ( (scp)->sc_socket_type == SOCK_STREAM )

#define SC_ACCEPTS_CONNECTIONS( scp )           \
      ( (scp)->sc_wait == NO && (scp)->sc_socket_type == SOCK_STREAM )

#define SC_SPECIFIED( scp, attr )   \
               M_IS_SET( (scp)->sc_specified_attributes, (attr) )
#define SC_SPECIFY( scp, attr )     \
               {                                                  	\
                  M_SET( (scp)->sc_specified_attributes, (attr) ) ;  \
                  SC_PRESENT( (scp), (attr) ) ;                      \
               }
#define SC_UNSPECIFY( scp, attr )   \
               M_CLEAR( (scp)->sc_specified_attributes, (attr) )

#define SC_IS_PRESENT( scp, attr )  \
               M_IS_SET( (scp)->sc_attributes_present, (attr) )
#define SC_PRESENT( scp, attr )     \
               M_SET( (scp)->sc_attributes_present, (attr) )
#define SC_ABSENT( scp, attr )      \
               {                                                  	\
                  M_CLEAR( (scp)->sc_attributes_present, (attr) ) ;  \
                  SC_UNSPECIFY( (scp), (attr) ) ;                   	\
               }

#define sc_getgid( scp )			( SC_SPECIFIED( scp, A_GROUP ) 					\
													? (scp)->sc_gid : (scp)->sc_user_gid )
#define sc_internal( scp, serp )	builtin_invoke( (scp)->sc_builtin, serp )
#define sc_make_external( scp )	M_CLEAR( (scp)->sc_type, ST_INTERNAL )

status_e						sc_init() ;
struct service_config 	*sc_alloc() ;
void 							sc_free() ;
void 							sc_dump() ;
struct service_config	*sc_make_special() ;
bool_int						sc_different_confs() ;

#endif	/* SCONF_H */

