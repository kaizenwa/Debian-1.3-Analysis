/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: ops.c,v 3.1 1993/03/13 23:21:53 panos Exp $" ;

#include "pset.h"

#define PRIVATE				static
#define POINTER				__pset_pointer

#ifndef NULL
#define NULL					0
#endif


/*
 * Remove all NULL pointers from a pset
 */
void pset_compact( pset )
	register pset_h pset ;
{
	register unsigned u ;

	for ( u = 0 ; u < pset_count( pset ) ; )
		if ( pset_pointer( pset, u ) != NULL )
			u++ ;
		else
			pset_remove_index( pset, u ) ;
}


/*
 * Apply a function to all pointers of a pset
 */
void pset_apply( pset, func, arg )
	register pset_h pset ;
	register void (*func)() ;
	register void *arg ;
{
	register unsigned u ;

	for ( u = 0 ; u < pset_count( pset ) ; u++ )
		if ( arg )
			(*func)( arg, pset_pointer( pset, u ) ) ;
		else
			(*func)( pset_pointer( pset, u ) ) ;
}

