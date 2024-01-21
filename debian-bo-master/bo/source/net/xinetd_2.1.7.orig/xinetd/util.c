/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: util.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
/*
 * The following ifdef is for TIOCNOTTY
 */
#ifndef NO_TERMIOS
#include <sys/termios.h>
#else
#include <sys/ioctl.h>
#endif
#include <fcntl.h>

#include <memory.h>
#include <syslog.h>
#include <errno.h>
#include <varargs.h>

#include "pset.h"
#include "misc.h"
#include "sio.h"

#include "defs.h"
#include "config.h"

char *malloc() ;

void msg() ;


void out_of_memory( func )
	char *func ;
{
	msg( LOG_CRIT, func, ES_NOMEM ) ;
}


struct name_value *nv_find_value( nv_array, name )
	struct name_value nv_array[] ;
	register char *name ;
{
	register struct name_value *nvp ;

	for ( nvp = nv_array ; nvp->name ; nvp++ )
		if ( EQ( name, nvp->name ) )
			return( nvp ) ;
	return( NULL ) ;
}


struct name_value *nv_find_name( nv_array, value )
	struct name_value nv_array[] ;
	register int value ;
{
	register struct name_value *nvp ;

	for ( nvp = nv_array ; nvp->name ; nvp++ )
		if ( value == nvp->value )
			return( nvp ) ;
	return( NULL ) ;
}


char *nv_get_name( nv_array, value )
	struct name_value nv_array[] ;
	register int value ;
{
	register struct name_value *nvp ;

	for ( nvp = nv_array ; nvp->name ; nvp++ )
		if ( value == nvp->value )
			return( nvp->name ) ;
	return( nvp->value ? nvp[1].name : NULL ) ;
}



char **argv_alloc( count )
	unsigned count ;
{
	unsigned argv_size = (count + 1) * sizeof( char *) ;
	char **argv ;
	char *func = "new_argv" ;

	argv = (char **) malloc( argv_size ) ;
	if ( argv == NULL )
	{
		out_of_memory( func ) ;
		return( NULL ) ;
	}
	(void) memset( (char *)argv, 0, (int) argv_size ) ;
	return( argv ) ;
}



/*
 * If size is 0, the pset holds strings
 */
status_e copy_pset( from, to, size )
	pset_h from ;
	pset_h *to ;
	unsigned size ;
{
	unsigned u ;
	char *func = "copy_pset" ;

	if ( *to == NULL )
	{
		*to = pset_create( pset_count( from ), 0 ) ;
		if ( *to == NULL )
		{
			out_of_memory( func ) ;
			return( FAILED ) ;
		}
	}

	for ( u = 0 ; u < pset_count( from ) ; u++ )
	{
		char *p = (char *) pset_pointer( from, u ) ;
		char *new ;
		
		if ( size == 0 )
			new = make_string( 1, p ) ;
		else
			new = malloc( size ) ;

		if ( new == NULL )
		{
			out_of_memory( func ) ;
			return( FAILED ) ;
		}

		if ( size != 0 )
			(void) memcpy( new, p, (int) size ) ;

		if ( pset_add( *to, new ) == NULL )
		{
			free( new ) ;
			out_of_memory( func ) ;
			return( FAILED ) ;
		}
	}
	return( OK ) ;
}


/*
 * Disassociate from controlling terminal
 */
void no_control_tty()
{
   int fd ;
   char *func = "no_control_tty" ;

   if ( ( fd = open( "/dev/tty", O_RDWR ) ) == -1 )
      msg( LOG_WARNING, func, "open of /dev/tty failed: %m" ) ;
   else
   {
      if ( ioctl( fd, TIOCNOTTY, (caddr_t)0 ) == -1 )
         msg( LOG_WARNING, func, "ioctl on /dev/tty failed: %m" ) ;
      (void) close( fd ) ;
   }
   (void) setpgrp( getpid(), 0 ) ;
}


/*
 * Write the whole buffer to the given file descriptor ignoring interrupts
 */
status_e write_buf( fd, buf, len )
	int fd ;
	char *buf ;
	int len ;
{
	register char *p ;
	register int cc ;

	for ( p = buf ; len > 0 ; p += cc, len -= cc )
	{
		cc = write( fd, p, len ) ;
		if ( cc == -1 )
		{
			if ( errno != EINTR )
				return( FAILED ) ;
			cc = 0 ;
		}
	}
	return( OK ) ;
}


void tabprint( fd, tab_level, fmt, va_alist )
   int fd ;
   int tab_level ;
   char *fmt ;
   va_dcl
{
   va_list ap ;
   int i ;

   for ( i = 0 ; i < tab_level ; i++ )
      Sputchar( fd, '\t' ) ;

   va_start( ap ) ;
   Sprintv( fd, fmt, ap ) ;
   va_end( ap ) ;
}


/*
 * Receive a single IP packet worth of data.
 */
void drain( sd )
   int sd ;
{
   char buf[ 1 ] ;
   char cc ;

   cc = recv( sd, buf, sizeof( buf ), 0 ) ;
   if ( cc == -1 )
      msg( LOG_WARNING, "drain", "recv: %m" ) ;
}

