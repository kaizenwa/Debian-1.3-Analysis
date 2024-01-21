/*
 * (c) Copyright 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

/*
 * $Id: hpqimpl.h,v 1.1 1992/11/23 16:25:25 panos Exp $
 */

#include "hpq.h"

typedef struct __hpq_header header_s ;

#define HHP( p )			((header_s *)p)

#define HANDLE_ERROR( flags, retval, errp, errval, msg )		\
				if ( flags & PQ_RETURN_ERROR )						\
				{																\
					*errp = errval ;										\
					return( retval ) ;									\
				}																\
				else															\
				{																\
					char *s = msg ;										\
																				\
					(void) write( 2, s, strlen( s ) ) ;				\
					abort() ;												\
					_exit( 1 ) ; 											\
					/* NOTREACHED */										\
				}


