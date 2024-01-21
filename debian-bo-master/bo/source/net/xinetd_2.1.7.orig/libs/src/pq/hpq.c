/*
 * (c) Copyright 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: hpq.c,v 1.5 1995/09/10 18:33:46 chuck Exp $" ;
static char version[] = VERSION ;

#include <sys/param.h>
#include "pq.h"
#include "hpqimpl.h"

#define PRIVATE			static

#ifndef NULL
#define NULL 0
#endif

#define PARENT( i )		( i >> 1 )
#define LEFT( i )			( i << 1 )
#define RIGHT( i )		( LEFT( i ) + 1 )

int pq_errno ;

#define INITIAL_ARRAY_SIZE				100				/* entries */

#if defined(linux) || defined(BSD)
PRIVATE int grow( header_s *hp ) ;
PRIVATE void restore_heap( register header_s *hp , unsigned start ) ;
#endif

pq_h __hpq_create( func, flags, errnop )
	int (*func)() ;
	int flags ;
	int *errnop ;
{
	register header_s *hp ;
	int *errp = ( errnop == NULL ) ? &pq_errno : errnop ;
	char *malloc() ;

	/*
	 * Check if the user has provided the necessary comparison functions
	 */
	if ( func == NULL )
		HANDLE_ERROR( flags, NULL, errp, PQ_ENOFUNC,
				"HPQ __hpq_create: missing object-object comparator\n" ) ;

	hp = HHP( malloc( sizeof( header_s ) ) ) ;
	if ( hp == NULL )
		HANDLE_ERROR( flags, NULL, errp, PQ_ENOMEM,
												"HPQ __hpq_create: malloc failed\n" ) ;
	
	/*
	 * Allocate object array
	 */
	hp->objects = (pq_obj *) malloc( INITIAL_ARRAY_SIZE * sizeof( pq_obj ) ) ;
	if ( hp->objects == NULL )
	{
		free( (char *)hp ) ;
		HANDLE_ERROR( flags, NULL, errp, PQ_ENOMEM, 
												"HPQ __hpq_create: malloc failed\n" ) ;
	}

	/*
	 * Initialize the header
	 */
	hp->is_better = func ;
	hp->errnop = errp ;
	hp->flags = flags ;
	hp->max_size = INITIAL_ARRAY_SIZE ;
	hp->cur_size = 0 ;
	return( (pq_h) hp ) ;
}


void __hpq_destroy( handle )
	pq_h handle ;
{
	header_s *hp = HHP( handle ) ;

	free( (char *) hp->objects ) ;
	free( (char *)hp ) ;
}


int __hpq_insert( handle, object )
	pq_h handle ;
	pq_obj object ;
{
	register header_s *hp = HHP( handle ) ;
	register unsigned i, parent ;

	if ( object == NULL )
		HANDLE_ERROR( hp->flags, PQ_ERR, hp->errnop, PQ_ENULLOBJECT,
											"HPQ __hpq_insert: NULL object\n" ) ;

	/*
	 * Make sure there is room to store the object
	 */
	if ( hp->cur_size >= hp->max_size && grow( hp ) == PQ_ERR )
		return( PQ_ERR ) ;

	i = hp->cur_size++ ;
	parent = PARENT( i ) ;
	while ( i > 0 && (*hp->is_better)( object, hp->objects[ parent ] ) )
	{
		hp->objects[ i ] = hp->objects[ parent ] ;
		i = parent ;
		parent = PARENT( i ) ;
	}
	hp->objects[ i ] = object ;
	return( PQ_OK ) ;
}


#define CUTOFF								(INITIAL_ARRAY_SIZE * 128)
#define INCREMENT							CUTOFF

/*
 * Grow the table.
 * Algorithm:
 *			while the table_size is less than CUTOFF, double the size.
 * 		if it grows greater than CUTOFF, increase the size by INCREMENT
 *			(these number are in entries, not bytes)
 */
PRIVATE int grow( hp )
	header_s *hp ;
{
	unsigned new_size ;
	char *new_objects ;
	char *realloc() ;

	if ( hp->max_size < CUTOFF )
		new_size = hp->max_size * 2 ;
	else
		new_size = hp->max_size + INCREMENT ;

	new_objects = realloc( (char *)hp->objects, new_size * sizeof( pq_obj ) ) ;
	if ( new_objects == NULL )
		HANDLE_ERROR( hp->flags, PQ_ERR, hp->errnop, PQ_ENOMEM,
										"HPQ grow: out of memory\n" ) ;
	
	hp->max_size = new_size ;
	hp->objects = (pq_obj *) new_objects ;
	return( PQ_OK ) ;
}


pq_obj __hpq_extract_head( handle )
	pq_h handle ;
{
	register header_s *hp = HHP( handle ) ;
	pq_obj object ;
	void restore_heap() ;

	if ( hp->cur_size == 0 )
		return( NULL ) ;
	
	object = hp->objects[ 0 ] ;
	hp->objects[ 0 ] = hp->objects[ --hp->cur_size ] ;
	restore_heap( hp, 0 ) ;
	return( object ) ;
}


#define EXISTS( hp, i )				( i < hp->cur_size )
#define IS_BETTER( hp, i, j )		\
			( (*hp->is_better)( hp->objects[ i ], hp->objects[ j ] ) )
#define SWAP( hp, i, j )								\
		{														\
			pq_obj t = hp->objects[ i ] ;				\
			hp->objects[ i ] = hp->objects[ j ] ;	\
			hp->objects[ j ] = t ;						\
		}

PRIVATE void restore_heap( hp, start )
	register header_s *hp ;
	unsigned start ;
{
	register unsigned current = start ;
	register unsigned better = current ;

	for ( ;; )
	{
		register unsigned left = LEFT( current ) ;
		register unsigned right = RIGHT( current ) ;

		/*
		 * Meaning of variables:
		 *
		 *		current:		the current tree node
		 *		left:			its left child
		 *		right:		its right child
		 *		better: 		the best of current,left,right
		 *
		 * We start the loop with better == current
		 *
		 * The code takes advantage of the fact that the existence of
		 * the right child implies the existence of the left child.
		 * It works by finding the better of the two children (and puts
		 * that in better) and comparing that against current.
		 */
		if ( EXISTS( hp, right ) )
			better = IS_BETTER( hp, left, right ) ? left : right ;
		else if ( EXISTS( hp, left ) )
			better = left ;

		if ( better == current || IS_BETTER( hp, current, better ) )
			break ;
		else 
		{
			SWAP( hp, current, better ) ;
			current = better ;
		}
	}
}


int __hpq_delete( handle, object )
	pq_h handle ;
	register pq_obj object ;
{
	register header_s *hp = HHP( handle ) ;
	register unsigned i ;

	if ( object == NULL )
		HANDLE_ERROR( hp->flags, PQ_ERR, hp->errnop, PQ_ENULLOBJECT,
											"HPQ __hpq_delete: NULL object\n" ) ;

	/*
	 * First find it
	 */
	for ( i = 0 ;; i++ )
	{
		if ( i < hp->cur_size )
			if ( object == hp->objects[ i ] )
				break ;
			else
				continue ;
		else
			HANDLE_ERROR( hp->flags, PQ_ERR, hp->errnop, PQ_ENOTFOUND,
					"HPQ __hpq_delete: object not found\n" ) ;
	}

	hp->objects[ i ] = hp->objects[ --hp->cur_size ] ;
	restore_heap( hp, i ) ;
	return( PQ_OK ) ;
}

