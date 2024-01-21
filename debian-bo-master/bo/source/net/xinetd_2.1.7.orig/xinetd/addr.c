/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: addr.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <syslog.h>
#include <netdb.h>
#include <memory.h>

#include "pset.h"

#include "defs.h"
#include "addr.h"

unsigned long inet_addr() ;
char *malloc() ;
int free() ;

void msg() ;
void parsemsg() ;
void out_of_memory() ;

#define OPEN_CURLY_BRACKET			'{'
#define CLOSED_CURLY_BRACKET		'}'
#define COMMA							','
#define DOT								'.'

/*
 * address types denote how the actual	numeric address was obtained.
 * Currently they are only useful for debugging.
 * Note that NUMERIC_ADDR includes both simple (e.g. 128.138.91.1) and
 * factorized symbolic addresses (e.g. 128.138.91.{1,2,3}).
 */
typedef enum {	NUMERIC_ADDR, NET_ADDR, HOST_ADDR } address_e ;

typedef enum { CANT_PARSE, PARSED, ERROR } result_e ;

struct comp_addr
{
	address_e		addr_type ;
	unsigned long	addr ;
	unsigned long	mask ;
} ;

#define CAP( p )        		( (struct comp_addr *) (p) )


#define NEW_CAP()					NEW( struct comp_addr )
#define FREE_CAP( cap )			FREE( cap )


/*
 * Try to match the given address 'addr' with the specified address list.
 * Returns TRUE if the address matched, FALSE otherwise.
 * If the address matched, '*matchp' will hold the mask of the matching
 * address (a greater numerical value for the mask implies a more
 * precise match).
 */
bool_int addrlist_match( addr_list, addr, matchp )
	pset_h			addr_list ;
	struct in_addr *addr ;
	unsigned long	*matchp ;
{
	register unsigned long remote_addr = ntohl( addr->s_addr ) ;
	register unsigned u ;

	for ( u = 0 ; u < pset_count( addr_list ) ; u++ )
	{
		register struct comp_addr *cap = CAP( pset_pointer( addr_list, u ) ) ;

		if ( ( remote_addr & cap->mask ) == cap->addr )
		{
			*matchp = cap->mask ;
			return( TRUE ) ;
		}
	}
	return( FALSE ) ;
}



void addrlist_dump( addr_list, fd )
	pset_h	addr_list ;
	int		fd ;
{
	register unsigned u ;
	struct in_addr inaddr ;
	char *inet_ntoa() ;

	for ( u = 0 ; u < pset_count( addr_list ) ; u++ )
	{
		struct comp_addr *cap = CAP( pset_pointer( addr_list, u ) ) ;
		char *type ;

		inaddr.s_addr = htonl( cap->addr ) ;
		if ( cap->addr_type == NUMERIC_ADDR )
			type = "NUMERIC" ;
		else if ( cap->addr_type == NET_ADDR )
			type = "NET" ;
		else if ( cap->addr_type == HOST_ADDR )
			type = "HOST" ;
		else
			type = "BAD" ;
		
		Sprint( fd, " %s(%s)", inet_ntoa( inaddr ), type ) ;
	}
}


void addrlist_free( addr_list )
	pset_h addr_list ;
{
	pset_apply( addr_list, free, NULL ) ;
}



/*
 * Add an address to the address list
 */
PRIVATE status_e add( addr_list, cap )
	pset_h				addr_list ;
	struct comp_addr	*cap ;
{
	struct comp_addr *new_cap ;
	char *func = "add" ;

	new_cap = NEW_CAP() ;
	if ( new_cap == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}

	*new_cap = *cap ;
	if ( pset_add( addr_list, new_cap ) == NULL )
	{
		out_of_memory( func ) ;
		FREE_CAP( new_cap ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}


/*
 * Find the address and remove it from the list
 * Since there is no check when we add entries that an
 * address is not entered twice, in this function we remove all
 * addresses that match.
 *
 * XXX: we need to work on the way two cap's are considered equal
 */
PRIVATE status_e remove( addr_list, cap )
	pset_h addr_list ;
	register struct comp_addr *cap ;
{
	register unsigned u = 0 ;
	register struct comp_addr *old_cap ;

	for ( u = 0 ; u < pset_count( addr_list ) ; u++ )
	{
		old_cap = CAP( pset_pointer( addr_list, u ) ) ;

		if ( old_cap->addr == cap->addr && old_cap->mask == cap->mask )
		{
			pset_pointer( addr_list, u ) = NULL ;
			FREE_CAP( old_cap ) ;
		}
	}
	pset_compact( addr_list ) ;
	return( OK ) ;
}


/*
 * Try to parse 'str_addr' as a symbolic net name
 */
PRIVATE result_e net_addr( str_addr, op, addr_list )
	char			*str_addr ;
	statfunc		op ;
	pset_h		addr_list ;
{
	/*
	 *
	 *  The following table demonstrates how the mask is determined
	 *  given a net number N and following the relation:
	 *     net #1 <= N <= #2
	 *
	 *     net #1      net #2      mask
	 *        0           0        FFFFFFFF    (this should be rejected)
	 *        1           FF       FF000000
	 *        100         FFFF     FFFF0000
	 *        10000       FFFFFF   FFFFFF00
	 *        1000000     FFFFFFFF FFFFFFFF
	 */
	static struct { int lim, mask, shift ; } net_to_mask[] =
	{
		{ 0, 				0xFF000000,	24	},
		{ 0xFF,			0xFFFF0000,	16	},
		{ 0xFFFF,		0xFFFFFF00,	8	},
		{ 0xFFFFFF,		0xFFFFFFFF,	0	},
		{ 0xFFFFFFFF,	0,				0	}
	} ;
	struct comp_addr			ca ;
	struct netent				*nep ;
	register unsigned long	net_num ;
	int							i ;
	char							*func = "net_addr" ;

	nep = getnetbyname( str_addr ) ;
	if ( nep == NULL || nep->n_addrtype != AF_INET || nep->n_net == 0 )
		return( CANT_PARSE ) ;

	for ( i = 0, net_num = (unsigned long) nep->n_net ;; i++ )
	{
		if ( net_to_mask[ i ].mask == 0 )
		{
			msg( LOG_CRIT, func,
				"INTERNAL ERROR: Cannot process net number %ld", net_num ) ;
			return( ERROR ) ;
		}
		if ( net_to_mask[i].lim < net_num && net_num <= net_to_mask[i+1].lim )
		{
			ca.addr_type = NET_ADDR ;
			ca.addr = net_num << net_to_mask[ i ].shift ;
			ca.mask = net_to_mask[ i ].mask ;
			return( ( (*op)( addr_list, &ca ) == OK ) ? PARSED : ERROR ) ;
		}
	}
}



/*
 * Try to parse 'str_addr' as a numeric address
 */
PRIVATE result_e numeric_addr( str_addr, op, addr_list )
	char *str_addr ;
	status_e (*op)() ;
	pset_h addr_list ;
{
	register unsigned long addr ;
	register unsigned long mask ;
	struct comp_addr ca ;

	addr = ntohl( inet_addr( str_addr ) ) ;
	if ( addr == (unsigned long) -1 )
		return( CANT_PARSE ) ;

	if ( addr == 0 )
		mask = 0 ;			/* i.e. matches everything */
	else
	{
		for ( mask = 0xFF ;; )
		{
			if ( addr & mask )
				break ;
			mask <<= 8 ;
			mask |= 0xFF ;
		}
		mask = ~( mask >> 8 ) ;
	}
	ca.mask = mask ;
	ca.addr = addr ;
	ca.addr_type = NUMERIC_ADDR ;
	return( ( (*op)( addr_list, &ca ) == OK ) ? PARSED : ERROR ) ;
}



/*
 * Try to parse 'str_addr' as a symbolic host name
 * Apply 'op' to the 'addrlist' for *all* IP addresses of the host
 */
PRIVATE result_e host_addr( str_addr, op, addr_list )
	char *str_addr ;
	status_e (*op)() ;
	pset_h addr_list ;
{
	struct hostent *hep ;
	struct comp_addr ca ;
	char **ap ;

	hep = gethostbyname( str_addr ) ;
	if ( hep == NULL || hep->h_addrtype != AF_INET )
		return( CANT_PARSE ) ;

	ca.addr_type = HOST_ADDR ;
	for ( ap = hep->h_addr_list ; *ap ; ap++ )
	{
		struct in_addr inaddr ;

		/*
		 * Copy the address to avoid alignment problems
		 */
		(void) memcpy( (char *) &inaddr, *ap, hep->h_length ) ;

		ca.addr = ntohl( inaddr.s_addr ) ;
		ca.mask = 0xFFFFFFFF ;
		if ( (*op)( addr_list, &ca ) == FAILED )
			return( ERROR ) ;
	}
	return( PARSED ) ;
}


/*
 * Try to parse 'str_addr' as a factorized address
 * (for example, 128.138.{200,201})
 *
 * XXX: It is unclear whether this function should exist. It is really doing
 * 	  the job of a preprocessor.
 */
PRIVATE result_e factorized_addr( str_addr, op, addr_list )
	char		*str_addr ;
	status_e (*op)() ;
	pset_h	addr_list ;
{
	char					*p ;
	char					*fact_start ;
	int					pass ;
	char					last	= DOT ;
	unsigned				num	= 0 ;
	int					shift = 24 ;	/* because we assume a 32-bit IP address */
	register unsigned long addr = 0 ;
	struct comp_addr	ca ;
	char					*func = "factorized_addr" ;

	for ( p = str_addr ; *p != OPEN_CURLY_BRACKET ; last = *p++ )
	{
		if ( isdigit( *p ) )
		{
			num = num * 10 + *p - '0' ;
			continue ;
		}
		switch ( *p )
		{
			case DOT:
				if ( last == DOT )
				{
					parsemsg( LOG_ERR, func,
						"Bad address: %s. Consecutive dots", str_addr ) ;
					return( ERROR ) ;
				}
				addr = addr * 256 + num ;
				num = 0 ;
				shift -= 8 ;
				break ;
			
			default:
				return( CANT_PARSE ) ;
		}
	}

	ca.addr_type = NUMERIC_ADDR ;
	fact_start = p+1 ;
	if ( addr != 0 )
		addr <<= ( shift+8 ) ;

	/*
	 * First pass is for syntax checking
	 */
	for ( pass = 0 ; pass < 2 ; pass++ )
	{
		num = 0 ;
		for ( p = fact_start, last = COMMA ;; last = *p++ )
		{
			if ( isdigit( *p ) )
			{
				num = num * 10 + *p - '0' ;
				continue ;
			}
			switch ( *p )
			{
				case COMMA:
				case CLOSED_CURLY_BRACKET:
					if ( last == COMMA )
					{
						parsemsg( LOG_ERR, func,
							"Bad address: %s. Consecutive commas", str_addr ) ;
						return( ERROR ) ;
					}

					if ( pass == 1 )
					{
						ca.addr = addr + ( num << shift ) ;
						ca.mask = ~( ( 1 << shift ) - 1 ) ;
						if ( (*op)( addr_list, &ca ) == FAILED )
							return( ERROR ) ;
						num = 0 ;
					}
					break ;
				
				default:
					parsemsg( LOG_ERR, func, "Bad address: %s", str_addr ) ;
					return( ERROR ) ;
			}
			if ( *p == CLOSED_CURLY_BRACKET )
			{
				if ( p[1] != NUL )
				{
					parsemsg( LOG_ERR, func, "Bad address: %s", str_addr ) ;
					return( ERROR ) ;
				}

				if ( pass == 0 )
					break ;
				else
					return( PARSED ) ;
			}
		}
	}
	/* NOTREACHED */
}


/*
 * Try to parse 'str_addr' using all known methods.
 * Try until one of the methods succeeds.
 * A successful method will apply 'op' with the parsed address to the 
 * 'addr_list'. The 'op' can be either 'add' or 'remove'
 * This means that the parsed address will be either added or removed
 * from the addr_list.
 */
PRIVATE status_e addrlist_op( addr_list, op, str_addr )
	pset_h addr_list ;
	status_e (*op)() ;
	char *str_addr ;
{
	int i ;
	static result_e (*addr_parser[])() =
		{
			numeric_addr,
			factorized_addr,
			net_addr,
			host_addr,
			NULL
		} ;
	char *func = "addrlist_op" ;

	for ( i = 0 ; addr_parser[ i ] != NULL ; i++ )
		switch ( (*addr_parser[ i ])( str_addr, op, addr_list ) )
		{
			case PARSED:
				return( OK ) ;
			
			case ERROR:
				return( FAILED ) ;
		}

	parsemsg( LOG_ERR, func, "failed to parse %s", str_addr ) ;
	return( OK ) ;
}


status_e addrlist_add( addr_list, str_addr ) 
	pset_h addr_list ;
	char *str_addr ;
{
	return( addrlist_op( addr_list, add, str_addr ) ) ;
}


status_e addrlist_remove( addr_list, str_addr ) 
	pset_h addr_list ;
	char *str_addr ;
{
	return( addrlist_op( addr_list, remove, str_addr ) ) ;
}


status_e addrlist_copy( from, to )
	pset_h from ;
	pset_h *to ;
{
	status_e copy_pset() ;

	return( copy_pset( from, to, sizeof( struct comp_addr ) ) ) ;
}

