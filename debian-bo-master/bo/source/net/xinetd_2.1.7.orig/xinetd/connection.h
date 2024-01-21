/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef CONNECTION_H
#define CONNECTION_H

/*
 * $Id: connection.h,v 1.2 1995/09/10 15:42:38 chuck Exp $
 */

#include <sys/types.h>
#ifdef BSD
#include <netinet/in.h>
#endif

#include "mask.h"
#include "service.h"

#define MAX_ALTERNATIVES				3

typedef enum { CONN_CLOSED = 0, CONN_OPEN } conn_state_e ;

#define COF_HAVE_ADDRESS				1
#define COF_SHUTDOWN						2
#define COF_CLEANUP						3
#define COF_NEW_DESCRIPTOR				4

struct connection
{
	conn_state_e 			co_state ;
	struct service 		*co_sp ;
	int 						co_descriptor ;
	mask_t 					co_flags ;
	struct sockaddr_in	co_remote_address ;
	unsigned 				co_alternative_count ;
	unsigned 				co_next_alternative ;
	struct service 		*co_alternatives[ MAX_ALTERNATIVES ] ;
} ;

typedef struct connection connection_s ;

#define COP( p )							((connection_s *)(p))

#define CONN_NULL							COP( NULL )

/*
 * Field access macros
 */
#define CONN_DESCRIPTOR( cp )			(cp)->co_descriptor
#define CONN_SERVICE( cp )				(cp)->co_sp

#define conn_set_flag( cp, flag )	M_SET( (cp)->co_flags, flag )

#define conn_shutdown( cp )			conn_set_flag( cp, COF_SHUTDOWN )
#define conn_cleanup( cp )				conn_set_flag( cp, COF_CLEANUP )

#define conn_setaddr( cp, sinp )															\
							{																		\
								conn_set_flag( cp, COF_HAVE_ADDRESS ) ;				\
								(cp)->co_remote_address = *(sinp) ;						\
							}
#define conn_set_descriptor( cp, fd )		(cp)->co_descriptor = (fd)

#define conn_address( cp )																	\
									(																\
										M_IS_SET( (cp)->co_flags, COF_HAVE_ADDRESS )	\
													? &(cp)->co_remote_address 			\
													: SOCKADDRIN_NULL							\
									)

char *inet_ntoa() ;

#define conn_addrstr( cp )																	\
								M_IS_SET( (cp)->co_flags, COF_HAVE_ADDRESS )			\
									? inet_ntoa( (cp)->co_remote_address.sin_addr )	\
									: "<no address>"

status_e 		conn_init() ;
connection_s 	*conn_new() ;
void 				conn_close() ;
void 				conn_free() ;
status_e 		conn_add_alternative() ;
status_e 		conn_start_alternative() ;
void 				conn_dump() ;

#endif	/* CONNECTION_H */

