/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: filelog.c,v 1.1 1995/11/11 23:08:58 chuck Exp root $" ;

#include <sys/types.h>
#include <sys/stat.h>
#include <varargs.h>
#include <fcntl.h>
#include <time.h>
#ifndef NO_SYSLOG
#include <syslog.h>
#else
#define LOG_ALERT				0
#endif

#include "sio.h"

#include "xlog.h"
#include "impl.h"
#include "filelog.h"

PRIVATE int filelog_init() ;
PRIVATE void filelog_fini() ;
PRIVATE int filelog_control() ;
PRIVATE int filelog_write() ;
PRIVATE int filelog_parms() ;
PRIVATE int limit_checks() ;

struct xlog_ops __xlog_filelog_ops = 
	{
		filelog_init,
		filelog_fini,
		filelog_write,
		filelog_control,
		filelog_parms
	} ;


PRIVATE int filelog_init( xp, ap )
	xlog_s	*xp ;
	va_list	ap ;
{
	int				fd ;
	struct stat		st ;
	struct filelog *flp ;
	char				*filename	= va_arg( ap, char * ) ;
	int				flags			= va_arg( ap, int ) ;

	flp = NEW( struct filelog ) ;
	if ( flp == NULL )
		return( XLOG_ENOMEM ) ;

	/*
	 * For safety, don't open a logfile if it's a symlink - cdm
	 */
	if ( lstat ( filename, &st ) != 0 )
		return( XLOG_EOPEN ) ;
	if ( ( st.st_mode & S_IFLNK ) == S_IFLNK )
	{
#ifndef NO_SYSLOG
		syslog(LOG_WARNING, "Log file %s is a symlink - open abort", filename);
#endif /* NO_SYSLOG */
		return( XLOG_EOPEN ) ;
	}

	if ( flags & O_CREAT )
		fd = open( filename, flags, va_arg( ap, int ) ) ;
	else
		fd = open( filename, flags ) ;

	if ( fd == -1 )
	{
		FREE( flp ) ;
		return( XLOG_EOPEN ) ;
	}
	
	FILELOG_DISABLE_SIZE_CONTROL( flp ) ;
	(void) Sbuftype( fd, SIO_LINEBUF ) ;
	flp->fl_fd = fd ;
	flp->fl_state = FL_OPEN ;
	xp->xl_data = flp ;
	return( XLOG_ENOERROR ) ;
}


PRIVATE void filelog_fini( xp )
	xlog_s *xp ;
{
	struct filelog *flp = FILELOG( xp ) ;

	if ( flp->fl_state != FL_CLOSED )
	{
		(void) close( flp->fl_fd ) ;
		flp->fl_state = FL_CLOSED ;
	}
	FREE( flp ) ;
	xp->xl_data = NULL ;
}



PRIVATE int filelog_control( xp, cmd, ap )
	xlog_s		*xp ;
	xlog_cmd_e	cmd ;
	va_list		ap ;
{
	struct stat		st ;
	struct filelog *flp		= FILELOG( xp ) ;
	int				status	= XLOG_ENOERROR ;

	if ( flp->fl_state == FL_ERROR )
		return( flp->fl_error ) ;

	switch ( cmd )
	{
		case XLOG_GETFD:
			if ( flp->fl_state == FL_OPEN )
				*va_arg( ap, int * ) = flp->fl_fd ;
			else
				status = XLOG_ENOERROR ;
			break ;

		case XLOG_LIMITS:
			flp->fl_soft_limit = va_arg( ap, unsigned ) ;
			flp->fl_hard_limit = va_arg( ap, unsigned ) ;
			flp->fl_issued_warning = FALSE ;
			FILELOG_ENABLE_SIZE_CONTROL( flp ) ;
			flp->fl_state = FL_OPEN ;
			/* FALL THROUGH */

		case XLOG_SIZECHECK:
			if ( ! FILELOG_SIZE_CONTROL( flp ) )
				break ;
			if ( fstat( flp->fl_fd, &st ) == -1 )
			{
				FILELOG_DISABLE_SIZE_CONTROL( flp ) ;
				flp->fl_state = FL_ERROR ;
				flp->fl_error = status = XLOG_EFSTAT ;
			}
			else
			{
				flp->fl_size = st.st_size ;
				if ( flp->fl_size > flp->fl_soft_limit )
					status = limit_checks( xp ) ;
			}
			break ;
	}
	return( status ) ;
}


PRIVATE int limit_checks( xp )
	xlog_s *xp ;
{
	struct filelog *flp = FILELOG( xp ) ;
	char buf[ 100 ] ;

	if ( ! flp->fl_issued_warning )
	{
		if ( xp->xl_use != NULL )
			xlog_write( (xlog_h) xp->xl_use, buf,
				strx_nprint( buf, sizeof( buf ),
									"soft limit exceeded on '%s'", xp->xl_id ),
					XLOG_NOFLAGS, LOG_ALERT ) ;
		flp->fl_issued_warning = TRUE ;
	}

	if ( flp->fl_size <= flp->fl_hard_limit )
		return( XLOG_ENOERROR ) ;
	
	if ( xp->xl_use != NULL )
		xlog_write( (xlog_h) xp->xl_use, buf,
			strx_nprint( buf, sizeof( buf ),
						"hard limit exceeded on '%s'; log closed", xp->xl_id ),
				XLOG_NOFLAGS, LOG_ALERT ) ;
	flp->fl_state = FL_ERROR ;
	return( XLOG_ESIZE ) ;
}


PRIVATE int filelog_write( xp, buf, len, flags, ap )
	xlog_s	*xp ;
	char		buf[] ;
	int		len ;
	int		flags ;
	va_list	ap ;
{
	struct filelog	*flp				= FILELOG( xp ) ;
	int 				action_flags	= ( xp->xl_flags | flags ) ;
	int				msglen			= 0 ;
	char				*percent_m_pos ;
	struct stat		st ;
	int				cc ;
	int				status ;
	time_t 			current_time ;
	struct tm		*tmp ;
	time_t			time() ;

	if ( flp->fl_state != FL_OPEN )
		return( flp->fl_error ) ;

	/*
	 * For safety, don't write a logfile if it's a symlink - cdm
	 * BOGUS - this should really be done by a separate daemon
	 */

#if 0
	if ( fstat ( flp->fl_fd, &st ) != 0 )
	{
		flp->fl_state = FL_ERROR ;
		flp->fl_error = XLOG_EFSTAT ;
	}
	if ( ( st.st_mode & S_IFLNK ) == S_IFLNK )
	{
		flp->fl_state = FL_ERROR ;
		flp->fl_error = XLOG_EFSTAT ;
	}
#endif

	(void) time( &current_time ) ;
	tmp = localtime( &current_time ) ;
	cc = Sprint( flp->fl_fd, "%d/%d/%d@%02d:%02d:%02d",
							tmp->tm_year, tmp->tm_mon+1, tmp->tm_mday,
							tmp->tm_hour, tmp->tm_min, tmp->tm_sec ) ;
	msglen += cc ;

	if ( action_flags & XLOG_PRINT_ID )
	{
		cc = Sprint( flp->fl_fd, " %s", xp->xl_id ) ;
		msglen += cc ;
	}

	if ( action_flags & XLOG_PRINT_PID )
	{
		cc = Sprint( flp->fl_fd, "[%d]", getpid() ) ;
		msglen += cc ;
	}

	cc = Sprint( flp->fl_fd, ": " ) ;
	msglen += cc ;

	if ( ( action_flags & XLOG_NO_ERRNO ) ||
						( percent_m_pos = __xlog_add_errno( buf, len ) ) == NULL )
	{
		cc = Swrite( flp->fl_fd, buf, len ) ;
		msglen += cc ;
	}
	else
	{
		char errno_buf[ 100 ] ;
		unsigned size = sizeof( errno_buf ) ;
		int cc_before_errno = percent_m_pos - buf ;
		char *ep ;

		/*
		 * The reason for the repetition of "msglen += cc ;" is that in the
		 * future we may want to check cc for SIO_ERR
		 */
		ep = __xlog_explain_errno( errno_buf, &size ) ;
		cc = Swrite( flp->fl_fd, buf, cc_before_errno ) ;
		msglen += cc ;
		cc = Swrite( flp->fl_fd, ep, (int)size ) ;
		msglen += cc ;
		cc = Swrite( flp->fl_fd, percent_m_pos+2, len-cc_before_errno-2 ) ;
		msglen += cc ;
	}
	/*
	 * Writing a newline will cause a buffer flush since we asked for
	 * line-buffered output
	 */
	Sputchar( flp->fl_fd, '\n' ) ;
	msglen++ ;

	/*
	 * NOTE: we don't check if XLOG_NO_SIZECHECK is set in xp->xl_flags
	 *			because size control is off by default and in order to
	 *			be enabled XLOG_LIMITS must be used which overrides xp->xl_flags
	 */
	if ( ! FILELOG_SIZE_CONTROL( flp ) || ( flags & XLOG_NO_SIZECHECK ) )
		return( XLOG_ENOERROR ) ;

	flp->fl_size += msglen ;
	if ( flp->fl_size <= flp->fl_soft_limit || 
					( status = limit_checks( xp ) ) == XLOG_ENOERROR )
		return( XLOG_ENOERROR ) ;
	
	flp->fl_state = FL_SIZE ;
	return( status ) ;
}


PRIVATE int filelog_parms( ap )
	va_list ap ;
{
	return( XLOG_ENOERROR ) ;
}

