/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: log.c,v 1.7 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#ifndef BSD
#include <sys/wait.h>
#endif
#include <syslog.h>
#include <time.h>

#include "sio.h"
#include "str.h"

#include "connection.h"
#include "defs.h"
#include "access.h"
#include "sconst.h"
#include "service.h"
#include "server.h"


void msg() ;

#define LOGBUF_SIZE						1024


time_t time() ;


PRIVATE int log_common() ;

/*
 * This function writes log records of the form:
 *
 *		START: service [pid] [from_address]
 */
void svc_log_success( sp, cp, pid )
	register struct service	*sp ;
	connection_s				*cp ;
	pid_t							pid ;
{
	char										buf[ LOGBUF_SIZE ] ;
	int										bufsize ;
	register struct service_config	*scp = SVC_CONF( sp ) ;
	register int							len ;
	register int							cc ;

	if ( ! SVC_LOGS_ON_SUCCESS( sp ) )
		return ;
	
	bufsize = sizeof( buf ) ;
	len = 0 ;
	
	cc = strx_nprint( buf, bufsize, "%s: %s", START_ENTRY, SC_ID( scp ) ) ;
	len += cc ;
	bufsize -= cc ;

	if ( SC_LOGS_PID( scp ) )
	{
		cc = strx_nprint( &buf[ len ], bufsize, " pid=%d", pid ) ;
		len += cc ;
		bufsize -= cc ;
	}

	cc = log_common( &SC_LOG_ON_SUCCESS( scp ), &buf[len], bufsize, cp ) ;
	len += cc ;
	bufsize -= cc ;

	xlog_write( sp->svc_log, buf, len, XLOG_NOFLAGS ) ;
}


/*
 * This function writes log records of the form:
 *
 *		FAIL: service failure-type [from_address]
 *
 */
void svc_log_failure( sp, cp, access_failure )
	register struct service	*sp ;
	connection_s				*cp ;
	access_e						access_failure ;
{
	char										buf[ LOGBUF_SIZE ] ;
	int										bufsize ;
	register struct service_config	*scp = SVC_CONF( sp ) ;
	register int							len = 0 ;
	register int							cc ;
	
	if ( ! SVC_LOGS_ON_FAILURE( sp ) )
		return ;
	
	bufsize = sizeof( buf ) ;
	cc = strx_nprint( buf, bufsize, "%s: %s", FAIL_ENTRY, SC_ID( scp ) ) ;
	len += cc ;
	bufsize -= cc ;

	cc = strx_nprint( &buf[ len ], bufsize,
								" %s", access_explain( access_failure ) ) ;
	len += cc ;
	bufsize -= cc ;

	cc = log_common( &SC_LOG_ON_FAILURE( scp ), &buf[ len ], bufsize, cp ) ;
	len += cc ;
	bufsize -= cc ;

	xlog_write( sp->svc_log, buf, len, XLOG_NOFLAGS ) ;
}



PRIVATE int log_common( logmask, buf, bufsize, cp )
	mask_t			*logmask ;
	char				*buf ;
	int				bufsize ;
	connection_s	*cp ;
{
	register int len = 0 ;
	int cc ;

   if ( M_IS_SET( *logmask, LO_HOST ) )
   {
      cc = strx_nprint( &buf[ len ], bufsize, " from=%s", conn_addrstr( cp ) ) ;
      len += cc ;
		bufsize -= cc ;
   }
	return( len ) ;
}



void svc_log_exit( sp, serp )
	register struct service	*sp ;
	struct server				*serp ;
{
	char										buf[ LOGBUF_SIZE ] ;
	int										bufsize ;
	register int							cc ;
	register int							len ;
	int										exit_status = SERVER_EXITSTATUS( serp ) ;
	register struct service_config	*scp = SVC_CONF( sp ) ;
	char										*func = "log_exit" ;

	if ( ! SVC_LOGS_ON_EXIT( sp ) )
		return ;

	bufsize = sizeof( buf ) ;
	len = 0 ;

	cc = strx_nprint( buf, bufsize, "%s: %s", EXIT_ENTRY, SC_ID( scp ) ) ;
	bufsize -= cc ;
	len += cc ;

	/*
	 * If the EXIT flag was used, log the exit status or the signal that
	 * killed the process. We assume that these are the only reasons
	 * for process termination.
	 */
	if ( SC_LOGS_EXITS( scp ) )
	{
		int num ;
		char *s ;

		if ( PROC_EXITED( exit_status ) )
		{
			s = "status" ;
			num = PROC_EXITSTATUS( exit_status ) ;
		}
		else if ( PROC_SIGNALED( exit_status ) )
		{
			s = "signal" ;
			num = PROC_TERMSIG( exit_status ) ;
		}
		else
		{
			msg( LOG_ERR, func, "Bad exit status" ) ;
			s = NULL ;
		}

		if ( s )
		{
			cc = strx_nprint( &buf[ len ], bufsize, " %s=%d", s, num ) ;
			len += cc ;
			bufsize -= cc ;
		}
	}

	if ( SC_LOGS_PID( scp ) )
	{
		cc = strx_nprint( &buf[ len ], bufsize, " pid=%d", SERVER_PID( serp ) ) ;
		len += cc ;
		bufsize -= cc ;
	}

	if ( SC_LOGS_DURATION( scp ) )
	{
		time_t current_time ;

		(void) time( &current_time ) ;
		cc = strx_nprint( &buf[ len ], bufsize, " duration=%d(sec)", 
									current_time - SERVER_STARTTIME( serp ) ) ;
		len += cc ;
		bufsize -= cc ;
	}
	xlog_write( sp->svc_log, buf, len, XLOG_NOFLAGS ) ;
}



/*
 * Used by other parts of xinetd that want to log something without
 * going through the proper channels (i.e. log_{success,failure} and log_exit)
 */
/* VARARGS3 */
void svc_logprint( sp, line_id, fmt, va_alist )
	register struct service	*sp ;
	char							*line_id ;
	char							*fmt ;
	va_dcl
{
	char		buf[ LOGBUF_SIZE ] ;
	int		bufsize = sizeof( buf ) ;
	int		len ;
	int		cc ;
	va_list	ap ;

	if ( ! SVC_IS_LOGGING( sp ) )
		return ;

	len = strx_nprint( buf, bufsize, "%s: %s ", line_id, SVC_ID( sp ) ) ;
	va_start( ap ) ;
	cc = strx_nprintv( &buf[ len ], bufsize, fmt, ap ) ;
	va_end( ap ) ;
	xlog_write( sp->svc_log, buf, len+cc, XLOG_NO_SIZECHECK ) ;
}

