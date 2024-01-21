/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: xlog.c,v 2.1 1993/05/06 07:39:39 panos Exp $" ;
static char version[] = VERSION ;

#include <varargs.h>

#include "xlog.h"
#include "impl.h"

char *malloc() ;

extern struct xlog_ops __xlog_filelog_ops ;
#ifndef NO_SYSLOG
extern struct xlog_ops __xlog_syslog_ops ;
#endif

struct lookup_table
{
	struct xlog_ops	*ops ;
	xlog_e				type ;
} ;

static struct lookup_table ops_lookup_table[] =
	{
		{ &__xlog_filelog_ops,				XLOG_FILELOG	},
#ifndef NO_SYSLOG
		{ &__xlog_syslog_ops,				XLOG_SYSLOG		},
#endif
		{ NULL }
	} ;


#define CALLBACK( xp, status ) 																\
		if ( (xp)->xl_callback ) 																\
			(*(xp)->xl_callback)( (xlog_h)(xp), status, (xp)->xl_callback_arg )


PRIVATE void xlog_link() ;
PRIVATE void xlog_unlink() ;


PRIVATE struct xlog_ops *xlog_ops_lookup( type )
	register xlog_e type ;
{
	register struct lookup_table *ltp ;

	for ( ltp = &ops_lookup_table[ 0 ] ; ltp->ops ; ltp++ )
		if ( ltp->type == type )
			break ;
	return( ltp->ops ) ;
}



/* VARARGS3 */
xlog_h xlog_create( type, id, flags, va_alist )
	xlog_e	type ;
	char		*id ;
	int		flags ;
	va_dcl
{
	xlog_s				*xp ;
	va_list				ap ;
	struct xlog_ops	*xops ;
	int					status ;

	if ( ( xp = NEW( xlog_s ) ) == NULL )
		return( NULL ) ;
	
	if ( id == NULL || ( xp->xl_id = __xlog_new_string( id ) ) == NULL )
	{
		FREE( xp ) ;
		return( NULL ) ;
	}

	xops = xlog_ops_lookup( type ) ;
	
	if ( xops != NULL )
	{
		va_start( ap ) ;
		xp->xl_ops = xops ;
		status = XL_INIT( xp, ap ) ;
		va_end( ap ) ;

		if ( status == XLOG_ENOERROR )
		{
			xp->xl_flags = flags ;
			xp->xl_type = type ;
			xp->xl_clients = XLOG_NULL ;
			xp->xl_use = XLOG_NULL ;
			return( (xlog_h) xp ) ;
		}
	}

	free( xp->xl_id ) ;
	FREE( xp ) ;
	return( NULL ) ;
}



PRIVATE void xlog_link( client, server )
	xlog_s *client, *server ;
{
	client->xl_use = server ;
	if ( server == NULL )
		return ;

	if ( server->xl_clients == XLOG_NULL )
	{
		INIT_LINKS( client, xl_other_users ) ;
		server->xl_clients = client ;
	}
	else
		LINK( server, client, xl_other_users ) ;
}


PRIVATE void xlog_unlink( xp )
	xlog_s *xp ;
{
	xlog_s *server = xp->xl_use ;

	/*
	 * Step 1: remove from server chain
	 */
	if ( server != XLOG_NULL )
	{
		if ( server->xl_clients == xp )
			if ( NEXT( xp, xl_other_users ) == xp )
				server->xl_clients = XLOG_NULL ;
			else
				server->xl_clients = NEXT( xp, xl_other_users ) ;
		else
			UNLINK( xp, xl_other_users ) ;
	}

	/*
	 * Step 2: If we have users, clear their link to us.
	 */
	if ( xp->xl_clients != NULL )
	{
		xlog_s *xp2 = xp->xl_clients ;

		do
		{
			xp2->xl_use = XLOG_NULL ;
			xp2 = NEXT( xp2, xl_other_users ) ;
		}
		while ( xp2 != xp->xl_clients ) ;
	}
}


PRIVATE void xlog_flags( xp, cmd, ap )
	xlog_s		*xp ;
	xlog_cmd_e	cmd ;
	va_list		ap ;
{
	int	flag			= va_arg( ap, int ) ;
	int	old_value	= ( ( xp->xl_flags & flag ) != 0 ) ;
	int	*valp			= va_arg( ap, int * ) ;

	if ( cmd == XLOG_SETFLAG )
		if ( *valp )
			xp->xl_flags |= flag ;
		else
			xp->xl_flags &= ~flag ;
	*valp = old_value ;
}


void xlog_destroy( xlog )
	xlog_h xlog ;
{
	xlog_s *xp = XP( xlog ) ;

	xlog_unlink( xp ) ;
	XL_FINI( xp ) ;
	free( xp->xl_id ) ;
	FREE( xp ) ;
}


/* VARARGS4 */
void xlog_write( xlog, buf, len, flags, va_alist )
	xlog_h	xlog ;
	char		buf[] ;
	int		len ;
	int		flags ;
	va_dcl
{
	xlog_s	*xp = XP( xlog ) ;
	va_list	ap ;
	int		status ;

	va_start( ap ) ;
	status = XL_WRITE( xp, buf, len, flags, ap ) ;
	va_end( ap ) ;

	if ( status != XLOG_ENOERROR )
	{
		CALLBACK( xp, status ) ;
	}
}


/* VARARGS2 */
int xlog_control( xlog, cmd, va_alist )
	xlog_h		xlog ;
	xlog_cmd_e	cmd ;
	va_dcl
{
	va_list	ap ;
	xlog_s	*xp		= XP( xlog ) ;
	int		status	= XLOG_ENOERROR ;

	va_start( ap ) ;

	switch ( cmd )
	{
		case XLOG_LINK:
			xlog_unlink( xp ) ;
			xlog_link( xp, va_arg( ap, xlog_s * ) ) ;
			xp->xl_callback_arg = va_arg( ap, void * ) ;
			break ;
		
		case XLOG_CALLBACK:
			xp->xl_callback = va_arg( ap, voidfunc ) ;
			break ;
			
		case XLOG_GETFLAG:
		case XLOG_SETFLAG:
			xlog_flags( xp, cmd, ap ) ;
			break ;

		default:
			status = XL_CONTROL( xp, cmd, ap ) ;
	}

	va_end( ap ) ;

	return( status ) ;
}


int xlog_parms( type, va_alist )
	xlog_e type ;
	va_dcl
{
	va_list	ap ;
	int		status ;

	va_start( ap ) ;
	switch ( type )
	{
#ifndef NO_SYSLOG
		case XLOG_SYSLOG:
			status = (*__xlog_syslog_ops.parms)( ap ) ;
			break ;
#endif
		case XLOG_FILELOG:
			status = (*__xlog_filelog_ops.parms)( ap ) ;
			break ;
		
		default:
			status = XLOG_ENOERROR ;
	}
	va_end( ap ) ;
	return( status ) ;
}

