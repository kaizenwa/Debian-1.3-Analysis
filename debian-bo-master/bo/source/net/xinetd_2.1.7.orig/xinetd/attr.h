/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef ATTR_H
#define ATTR_H

/*
 * $Id: attr.h,v 1.2 1995/09/10 14:26:36 chuck Exp $
 */

/*
 * Attribute IDs
 */
#define A_NONE             	0
#define A_WAIT             	1
#define A_SOCKET_TYPE      	2
#define A_PROTOCOL         	3
#define A_USER             	4
#define A_GROUP            	5
#define A_SERVER           	6
#define A_SERVER_ARGS      	7
#define A_INSTANCES        	8
#define A_ID			9
#define A_ONLY_FROM 		10
#define A_ACCESS_TIMES 		11
#define A_RPC_VERSION		12
#define A_LOG_TYPE		13
#define A_NO_ACCESS		14
#define A_TYPE			15
#define A_LOG_ON_FAILURE	16
#define A_LOG_ON_SUCCESS	17
#define A_ENV			18
#define A_PORT			19
#define A_PASSENV		20
#define A_FLAGS			21
#define A_RPC_NUMBER		22
#define A_NICE			23

#ifdef BIND_IF
#define A_INTERFACE		24
#endif

/*
 * SERVICE_ATTRIBUTES is the number of service attributes and also
 * the number from which defaults-only attributes start.
 */
#define SERVICE_ATTRIBUTES		( A_NICE + 1 )

#define A_DISABLED			( SERVICE_ATTRIBUTES )


/*
 * Mask of attributes that must be specified.
 */
#define NECESSARY_ATTRS     			( MASK( A_SOCKET_TYPE ) + MASK( A_WAIT ) )

#define NECESSARY_ATTRS_EXTERNAL			MASK( A_SERVER )
#define NECESSARY_ATTRS_RPC_LISTED		MASK( A_PROTOCOL )
#define NECESSARY_ATTRS_RPC_UNLISTED	MASK( A_PROTOCOL ) + MASK( A_RPC_NUMBER )
#define NECESSARY_ATTRS_LISTED    		MASK( A_USER )
#define NECESSARY_ATTRS_UNLISTED  		( MASK( A_PROTOCOL ) + MASK( A_PORT ) )

#endif	/* ATTR_H */
