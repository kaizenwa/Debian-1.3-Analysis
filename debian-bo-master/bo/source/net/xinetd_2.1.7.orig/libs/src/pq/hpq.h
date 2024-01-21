/*
 * (c) Copyright 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef __HPQ_H
#define __HPQ_H

/*
 * $Id: hpq.h,v 1.2 1993/04/01 02:15:32 panos Exp $
 */

/*
 * Implementation of the PQ interface
 */

#define pq_create				__hpq_create
#define pq_destroy			__hpq_destroy
#define pq_insert				__hpq_insert
#define pq_delete				__hpq_delete
#define pq_head				__hpq_head
#define pq_extract_head		__hpq_extract_head

/*
 * The is_better function takes 2 arguments which are pointers to objects
 * and returns:
 *       1     if the first object is "better" than the second
 *       0     otherwise
 */

struct __hpq_header
{
	int (*is_better)() ;
	int *errnop ;
	int flags ;
	pq_obj *objects ;			/* array of objects */
	unsigned cur_size ;		/* # of objects in array */
	unsigned max_size ;		/* max # of objects that can fit in array */
} ;


#ifndef __ARGS
#  ifdef PROTOTYPES
#     define __ARGS( s )               s
#  else
#     define __ARGS( s )               ()
#  endif
#endif

pq_h __hpq_create			__ARGS( ( int (*func)(), int flags, int *errnop ) ) ;
void __hpq_destroy		__ARGS( ( pq_h handle ) ) ;
int  __hpq_insert			__ARGS( ( pq_h handle, pq_obj object ) ) ;
pq_obj __hpq_extract_head __ARGS( ( pq_h handle ) ) ;
int  __hpq_delete 		__ARGS( ( pq_h handle, pq_obj object ) ) ;

#define __hpq_head( handle )																\
				(																					\
					((struct __hpq_header *)(handle))->cur_size						\
							? ((struct __hpq_header *)(handle))->objects[ 0 ] 		\
							: (pq_obj) 0														\
				)

#endif __HPQ_H

