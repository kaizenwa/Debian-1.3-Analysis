/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef INT_H
#define INT_H

/*
 * $Id: int.h,v 1.2 1995/09/10 15:42:38 chuck Exp $
 */

#include <sys/types.h>
#ifdef BSD
#include <netinet/in.h>
#endif

#include "pset.h"

#include "defs.h"
#include "server.h"

typedef enum { GOOD_CHANNEL, BAD_CHANNEL } channel_state_e ;

struct channel
{
	channel_state_e		ch_state ;
	struct sockaddr_in	ch_from ;
	int						ch_local_socket ;
	int						ch_remote_socket ;
} ;

typedef struct channel channel_s ;

#define CHP( p )                  ((struct channel *)(p))

#define CHANNEL_NULL					CHP( NULL )

char *malloc() ;

#define NEW_CHANNEL()            NEW( channel_s )
#define FREE_CHANNEL( chp )      FREE( chp )


channel_s *int_lookupconn() ;
channel_s *int_newconn() ;


struct intercept_common
{
	bool_int					ic_intercept ;
	int						ic_remote_socket ;
	struct sockaddr_in	ic_local_addr ;
	pset_h 					ic_connections ;
	struct server			ic_server ;
} ;


struct intercept_ops
{
	void (*mux)() ;
	void (*exit)() ;
} ;


struct intercept
{
	int 								int_socket_type ;
	struct intercept_common 	int_common ;
	void 								*int_priv ;
	struct intercept_ops 		*int_ops ;
} ;

#define INT_TYPE( p )					((p)->int_socket_type)
#define INT_SERVER( p )					(&(p)->int_common.ic_server)
#define INT_LOCALADDR( p )				(&(p)->int_common.ic_local_addr)
#define INT_REMOTE( p )					((p)->int_common.ic_remote_socket)
#define INT_ALLOCATOR( p )				((p)->int_common.ic_channel_allocator)
#define INT_CONNECTIONS( p )			((p)->int_common.ic_connections)
#define INTERCEPT( p )					((p)->int_common.ic_intercept)

void				int_fail() ;
int				int_select() ;
void				int_exit() ;
void				int_init() ;
channel_s		*int_newconn() ;
channel_s		*int_lookupconn() ;

#endif	/* INT_H */
