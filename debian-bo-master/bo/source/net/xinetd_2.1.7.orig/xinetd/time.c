/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: time.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <syslog.h>
#include <ctype.h>
#include <time.h>

#include "pset.h"

#include "defs.h"

time_t time() ;
char *malloc() ;
int free() ;

void parsemsg() ;
void out_of_memory() ;


#define IN_RANGE( val, low, high )     ( (low) <= (val) && (val) <= (high) )

struct time_interval
{
	short min_start ;
	short min_end ;
} ;

#define TIP( p )      			( (struct time_interval *) (p) )

#define NEW_TI()					NEW( struct time_interval )
#define FREE_TI( tip )			FREE( tip )


/*
 * Returns TRUE if the current time is within at least one of the intervals
 */
bool_int ti_current_time_check( intervals )
	pset_h intervals ;
{
	time_t					current_time ;
	register unsigned 	u ;
	register short			min_current ;
	struct tm				*tmp ;

	(void) time( &current_time ) ;
	tmp = localtime( &current_time ) ;
	min_current = tmp->tm_hour * 60 + tmp->tm_min ;

	for ( u = 0 ; u < pset_count( intervals ) ; u++ )
	{
		register struct time_interval *tip ;
		
		tip = TIP( pset_pointer( intervals, u ) ) ;
		if ( IN_RANGE( min_current, tip->min_start, tip->min_end ) )
			return( TRUE ) ;
	}
	return( FALSE ) ;
}


PRIVATE char *get_num( nump, min_val, max_val, s, stop_char )
   register int	*nump ;
   int				min_val ;
	int				max_val ;
   char				*s ;
   char				stop_char ;
{
   register char *p = s ;
   char *func = "get_num" ;

   for ( *nump = 0 ; isdigit( *p ) ; p++ )
   {
      *nump *= 10 ;
      *nump += *p - '0' ;
   }

   if ( *p != stop_char )
   {
      parsemsg( LOG_ERR, func, "incorrect time interval" ) ;
      return( NULL );
   }

   if ( ! IN_RANGE( *nump, min_val, max_val ) )
   {
      parsemsg( LOG_ERR, func, "invalid time interval" ) ;
      return( NULL ) ;
   }
   return( p+1 ) ;
}



/*
 * Each interval should have the form:
 *    hour:min-hour:min
 * Example: 2:30-4:15
 */
status_e ti_add( iset, interval_str )
	pset_h iset ;
	char *interval_str ;
{
	struct time_interval	*tip ;
	int						hours ;
	int						minutes ;
	register char			*p ;
	int						min_start ;
	int						min_end ;
	char						*func = "add_interval" ;

	if ( ( p = get_num( &hours, 0, 23, interval_str, ':' ) ) == NULL )
		return( OK ) ;
	if ( ( p = get_num( &minutes, 0, 59, p, '-' ) ) == NULL )
		return( OK ) ;
	min_start = hours * 60 + minutes ;

	if ( ( p = get_num( &hours, 0, 23, p, ':' ) ) == NULL )
		return( OK ) ;
	if ( ( p = get_num( &minutes, 0, 59, p, NUL ) ) == NULL )
		return( OK ) ;
	min_end = hours * 60 + minutes ;
	if ( min_start >= min_end )
	{
		parsemsg( LOG_ERR, func, "invalid time interval: %s", interval_str ) ;
		return( OK ) ;
	}

	tip = NEW_TI() ;
	if ( tip == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	tip->min_start = min_start ;
	tip->min_end = min_end ;
	if ( pset_add( iset, tip ) == NULL )
	{
		FREE_TI( tip ) ;
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}


void ti_dump( iset, fd )
	pset_h iset ;
	int fd ;
{
	register unsigned u ;

	for ( u = 0 ; u < pset_count( iset ) ; u++ )
	{
		register struct time_interval *tip = TIP( pset_pointer( iset, u ) ) ;

		Sprint( fd, " %02d:%02d-%02d:%02d",
			tip->min_start / 60, tip->min_start % 60,
			tip->min_end / 60, tip->min_end % 60 ) ;
	}
}


void ti_free( iset )
	pset_h iset ;
{
	pset_apply( iset, free, NULL ) ;
}

