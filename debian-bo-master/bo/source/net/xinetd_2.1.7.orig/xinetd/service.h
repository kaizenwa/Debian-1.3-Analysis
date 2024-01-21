/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef SERVICE_H
#define SERVICE_H

#include <sys/types.h>
#include <netinet/in.h>

/*
 * $Id: service.h,v 1.2 1995/09/10 14:26:36 chuck Exp $
 */

#include "xlog.h"

#include "defs.h"
#include "sconf.h"


/*
 * NOTE: A service can be disabled but not deleted if it has any servers
 *       running
 */
typedef enum                     /* service states */
   {
      SVC_NOT_STARTED = 0,      /* no attempt to start it yet       */
      SVC_ACTIVE,               /* service is available             */
	  SVC_SUSPENDED,			/* service is suspended					*/
      SVC_DISABLED              /* service disabled                 */
   } state_e ;


/*
 * NOTE: Clearing the structure will give all its fields their default values
 */
struct service
{
	state_e		svc_state ;

	int 			svc_ref_count ;			/* number of pointers to this struct */

	struct service_config *svc_conf ;	/* service configuration				 */

	int			svc_fd ;

	unsigned 	svc_running_servers ;
	unsigned 	svc_retry_servers ;

	unsigned 	svc_attempts ;			/* # of attempts to start a server     */
	time_t		svc_start_time ;		/* since this time							*/

	/*
	 * 	svc_shutdown:		invoked to playback the protocol and get information
	 *								from the other end. It also sets up the connection
	 *								for an orderly close.
	 *		svc_handler:		invoked to handle new requests
	 */
	voidfunc		svc_shutdown_func ;	/* ARGS: int sd, char **msgp           */
	statfunc		svc_handler_func ;	/* ARGS: service *, connection *       */

	/*
	 * These fields are used for access control; they either point to the
	 * service access control lists or to the default access control lists
	 */
	pset_h		svc_no_access ;
	pset_h		svc_only_from ;

	/*
	 * These fields are used to avoid generating too many messages when
	 * receiving datagrams from a bad address.
	 */
	struct sockaddr_in	svc_last_dgram_addr ;
	time_t					svc_last_dgram_time ;

	xlog_h		svc_log ;

#ifdef BIND_IF
    /*
     * N Verdon (5/6/95) - interface to bring up this service on
     */
    struct in_addr	svc_interface;
#endif
} ;

#define SP( p )						( (struct service *) (p) )

/*
 * Field access macros
 */
#define SVC_CONF( sp )				( (sp)->svc_conf )
#define SVC_FD( sp )					( (sp)->svc_fd )
#define SVC_RUNNING_SERVERS( sp )	(sp)->svc_running_servers
#define SVC_RETRIES( sp )			(sp)->svc_retry_servers
#define SVC_LOG( sp )				(sp)->svc_log
#define SVC_REFCOUNT( sp )			(sp)->svc_ref_count
#define SVC_ID( sp )					SC_ID( SVC_CONF( sp ) )
#define SVC_SOCKET_TYPE( sp )		SC_SOCKET_TYPE( SVC_CONF( sp ) )

#define SVC_IS_ACTIVE( sp )		( (sp)->svc_state == SVC_ACTIVE )
#define SVC_IS_SUSPENDED( sp )	( (sp)->svc_state == SVC_SUSPENDED )
#define SVC_IS_AVAILABLE( sp )	( SVC_IS_ACTIVE(sp) || SVC_IS_SUSPENDED(sp) )
#define SVC_IS_DISABLED( sp )		( (sp)->svc_state == SVC_DISABLED )

#ifdef BIND_IF
#define SVC_INTERFACE( sp )		( (sp)->svc_interface )
#endif

/*
 * Predicate checking macros
 */
#define SVC_LOGUSER( sp )			SC_LOGUSER( SVC_CONF( sp ) )
#define SVC_FORKS( sp )				SC_FORKS( SVC_CONF( sp ) )
#define SVC_RETRY( sp )				SC_RETRY( SVC_CONF( sp ) )
#define SVC_WAITS( sp )				SC_WAITS( SVC_CONF( sp ) )
#define SVC_IS_INTERCEPTED( sp )	SC_IS_INTERCEPTED( SVC_CONF( sp ) )
#define SVC_ACCEPTS_CONNECTIONS( sp )	\
											SC_ACCEPTS_CONNECTIONS( SVC_CONF( sp ) )

#define SVC_IS_LOGGING( sp )		( (sp)->svc_log != NULL )
#define SVC_LOGS_ON_SUCCESS( sp )			\
		( SVC_IS_LOGGING( sp ) && SC_LOGS_ON_SUCCESS( SVC_CONF( sp ) ) )
#define SVC_LOGS_ON_FAILURE( sp )			\
		( SVC_IS_LOGGING( sp ) && SC_LOGS_ON_FAILURE( SVC_CONF( sp ) ) )
#define SVC_LOGS_ON_EXIT( sp )				\
		( SVC_IS_LOGGING( sp ) && SC_LOGS_ON_EXIT( SVC_CONF( sp ) ) )
#define SVC_LOGS_USERID_ON_SUCCESS( sp )	\
		( SVC_IS_LOGGING( sp ) && SC_LOGS_USERID_ON_SUCCESS( SVC_CONF( sp ) ) )
#define SVC_LOGS_USERID_ON_FAILURE( sp )	\
		( SVC_IS_LOGGING( sp ) && SC_LOGS_USERID_ON_FAILURE( SVC_CONF( sp ) ) )
#define SVC_RECORDS( sp )						\
		( SVC_IS_LOGGING( sp ) && SC_RECORDS( SVC_CONF( sp ) ) )

/*
 * Reference counting macros
 */
#define SVC_HOLD( sp )				(sp)->svc_ref_count++
#define SVC_RELE( sp )	\
	( ( (sp)->svc_ref_count <= 1 ) ? svc_release( sp ) : --(sp)->svc_ref_count )


#define svc_handle( sp, cp )			(*(sp)->svc_handler_func)( sp, cp )
#define svc_internal( sp, serp )		sc_internal( SVC_CONF( sp ), serp )
#define svc_make_external( sp )		sc_make_external( SVC_CONF( sp ) )

#define svc_dec_running_servers( sp )														\
	{																									\
		if ( SVC_RUNNING_SERVERS( sp ) > 0 )												\
			(sp)->svc_running_servers-- ;														\
		else																							\
			msg( LOG_ERR, func,																	\
				"Service %s: server exit with 0 running servers", SVC_ID( sp ) ) ;\
	}

#define svc_inc_running_servers( sp )		(sp)->svc_running_servers++

#define svc_inc_retries( sp )					(sp)->svc_retry_servers++
#define svc_dec_retries( sp )					(sp)->svc_retry_servers--

status_e 			svc_init() ;
struct service		*svc_new() ;
struct service		*svc_make_special() ;
void					svc_free() ;
status_e				svc_activate() ;
void					svc_setup_address_control() ;
void					svc_deactivate() ;
void					svc_suspend() ;
void					svc_resume() ;
int					svc_release() ;
void					svc_dump() ;
void					svc_request() ;
status_e				svc_access_control() ;
void					svc_shutdown() ;
void					svc_log_success() ;
void					svc_log_failure() ;
void					svc_log_exit() ;
void					svc_logprint() ;
void					svc_postmortem() ;

#endif	/* SERVICE_H */
