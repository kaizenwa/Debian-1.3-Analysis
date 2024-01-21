/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: logctl.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/stat.h>
#include <syslog.h>
#include <fcntl.h>

#include "sio.h"
#include "xlog.h"

#include "config.h"
#include "state.h"
#include "defs.h"
#include "log.h"
#include "service.h"

void msg() ;



PRIVATE xlog_h start_filelog( id, flp )
	char				*id ;
	struct filelog *flp ;
{
	xlog_h	xh ;
	int		fd ;
	int		log_file_mode = ( debug.on ) ? 0644 : LOG_FILE_MODE ;
	char		*func = "start_filelog" ;

	xh = xlog_create( XLOG_FILELOG, id, XLOG_NOFLAGS,
							flp->fl_filename, LOG_OPEN_FLAGS, log_file_mode ) ;
	if ( xh == NULL )
	{
		msg( LOG_ERR, func, "creation of %s log failed", id ) ;
		return( NULL ) ;
	}

	if ( xlog_control( xh, XLOG_GETFD, &fd ) != XLOG_ENOERROR ||
						fcntl( fd,  F_SETFD, 1 ) == -1 )
	{
		msg( LOG_ERR, func, "Failed to set close-on-exec flag for log file" ) ;
		xlog_destroy( xh ) ;
		return( NULL ) ;
	}

	ps.rws.descriptors_free-- ;

	if ( FILELOG_SIZE_CONTROL( flp ) )
		(void) xlog_control( xh,
							XLOG_LIMITS, flp->fl_soft_limit, flp->fl_hard_limit ) ;

	return( xh ) ;
}


/*
 * This function is invoked when a xlog detects an error (for example,
 * exceeding the file size limit).
 * The function just enters a log message.
 * 
 * NOTE: We could destroy the xlog at this point but we choose not to.
 */
PRIVATE void log_in_error( xh, error_code, arg )
	xlog_h	xh ;
	int		error_code ;
	void		*arg ;
{
	struct service		*sp		= SP( arg ) ;
	char					*log_id	= ( sp == NULL ) ? "common" : SVC_ID( sp ) ;
	char					*func		= "log_in_error" ;

#ifdef lint
	xh = xh ;
#endif
	if ( error_code == XLOG_ESIZE )
		msg( LOG_ERR, func, "Size of %s log exceeded hard limit", log_id ) ;
	else
		msg( LOG_ERR, func, "Error in %s log: %d", log_id, error_code ) ;
}

/*
 * Start logging for the specified service.
 * The current configuration is used to determine the common log file.
 */
status_e log_start( sp, xhp )
	struct service		*sp ;
	xlog_h				*xhp ;
{
	xlog_h		xh ;
	char			*sid	= SVC_ID( sp ) ;
	struct log	*lp	= SC_LOG( SVC_CONF( sp ) ) ;
	char			*func = "log_start" ;

	switch ( lp->l_type )
	{
		case L_NONE:
			xh = NULL ;
			break ;

		case L_SYSLOG:
			xh = xlog_create( XLOG_SYSLOG, sid, XLOG_NOFLAGS, 
						log_syslog( lp )->sl_facility, log_syslog( lp )->sl_level ) ;
			if ( xh == NULL )
			{
				msg( LOG_ERR, func, "failed to create a log for service %s", sid ) ;
				return( FAILED ) ;
			}
			xlog_control( xh, XLOG_CALLBACK, log_in_error, (void *)sp ) ;
			break ;
		
		case L_FILE:
			/*
			 * NOTE: if the same file is specified for more than one service,
			 *			it will be opened as many times.
			 *			Furthermore, size control will not be accurate.
			 */
			xh = start_filelog( sid, log_filelog( lp ) ) ;
			if ( xh == NULL )
				return( FAILED ) ;
			(void) xlog_control( xh, XLOG_CALLBACK, log_in_error, (void *)sp ) ;
			break ;
		
		case L_COMMON_FILE:
			if ( DEFAULT_LOG( ps ) == NULL )
				if ( DEFAULT_LOG_ERROR( ps ) )
					return( FAILED ) ;
				else
				{
					xh = start_filelog( "default", 
										log_filelog( SC_LOG( DEFAULTS( ps ) ) ) ) ;
					if ( xh == NULL )
					{
						DEFAULT_LOG_ERROR( ps ) = TRUE ;
						return( FAILED ) ;
					}
					DEFAULT_LOG( ps ) = xh ;
					(void) xlog_control( xh,
									XLOG_CALLBACK, log_in_error, VOID_NULL ) ;
				}
			else
				xh = DEFAULT_LOG( ps ) ;
			break ;

		default:			/* SHOULDN'T HAPPEN */
			msg( LOG_ERR, func, "bad log type (%d) for service %s",
				(int) log_get_type( lp ), sid ) ;
			return( FAILED ) ;
	}
	*xhp = xh ;
	return( OK ) ;
}


void log_end( lp, xh )
	struct log *lp ;
	xlog_h xh ;
{
	char *func = "log_end" ;

	if ( xh == NULL )		/* shouldn't be NULL but just in case */
	{
		msg( LOG_NOTICE, func, "%s called with NULL handle", func ) ;
		return ;
	}

	switch ( log_get_type( lp ) )
	{
		case L_FILE:
			ps.rws.descriptors_free++ ;
			/* FALL THROUGH */
		
		case L_SYSLOG:
			xlog_destroy( xh ) ;
	}
}

