/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/route.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>

#ifdef __svr4__
#include <sys/sockio.h>
#endif __svr4__

#include <atalk/ddp.h>
#include <atalk/rtmp.h>

#include "interface.h"
#include "gate.h"
#include "rtmp.h"
#include "zip.h"
#include "list.h"
#include "atserv.h"

rtmp_packet( ap, from, data, len )
    struct atport	*ap;
    struct sockaddr_at	*from;
    char		*data;
    int			len;
{
    struct rtmp_head	rh;
    struct rtmp_tuple	rt, xrt;
    struct gate		*gate;
    struct interface	*iface;
    struct rtmptab	*rtmp;
    u_short		tonet;
    char		*end, packet[ 587 ];
    extern int		debug;

    end = data + len;

    if ( data >= end ) {
	syslog( LOG_INFO, "rtmp_packet no data" );
	return;
    }

    iface = ap->ap_iface;

    /* ignore our own packets */
    if ( from->sat_addr.s_net == iface->i_addr.sat_addr.s_net &&
	    from->sat_addr.s_node == iface->i_addr.sat_addr.s_node  ) {
	return;
    }

    switch( *data++ ) {
    case DDPTYPE_RTMPRD :
	/*
	 * Response and Data.
	 */
	if ( data + sizeof( struct rtmprdhdr ) > end ) {
	    syslog( LOG_INFO, "rtmp_packet no data header" );
	    return;
	}
	bcopy( data, &rh, sizeof( struct rtmprdhdr ));
	data += sizeof( struct rtmprdhdr );

	/* check rh address against from address */
	if ( rh.rh_nodelen != 8 ) {
	    syslog( LOG_INFO, "rtmp_packet bad node len (%d)", rh.rh_nodelen );
	    return;
	}
	if (( from->sat_addr.s_net != 0 && from->sat_addr.s_net != rh.rh_net )||
		from->sat_addr.s_node != rh.rh_node ) {
	    syslog( LOG_INFO, "rtmp_packet address mismatch" );
	    return;
	}

	if (( iface->i_flags & ( IFACE_ADDR|IFACE_CONFIG )) == IFACE_ADDR ) {
	    if ( iface->i_flags & IFACE_NOROUTER ) {
		/* remove addr to loopback route */
		if ( looproute( iface, RTMP_DEL ) < 0 ) {
		    syslog( LOG_ERR, "rtmp_packet: can't remove loopback: %m" );
		}

		iface->i_flags &= ~IFACE_NOROUTER;
		iface->i_time = 0;
		syslog( LOG_INFO, "rtmp_packet router has become available" );
	    }
	    if ( iface->i_flags & IFACE_PHASE1 ) {
		rtmp_config( &rh, iface );
	    } else {
		zip_getnetinfo( iface );
	    }
	    return;
	}

	if (( iface->i_flags & IFACE_CONFIG ) == 0 ) {
	    return;
	}

	/*
	 * Parse first tuple.  For phase 2, verify that net is correct.
	 */
	if ( data + SZ_RTMPTUPLE > end ) {
	    syslog( LOG_INFO, "rtmp_packet missing first tuple" );
	    return;
	}
	bcopy( data, &rt, SZ_RTMPTUPLE );
	data += SZ_RTMPTUPLE;

	if ( rt.rt_net == 0 ) {
	    if ( rt.rt_dist != 0x82 ) {
		syslog( LOG_INFO, "rtmp_packet bad phase 1 version" );
		return;
	    }

	    /*
	     * Grab the next tuple, since we don't want to pass the version
	     * number to the parsing code.  We're assuming that there are
	     * no extended tuples in this packet.
	     */
	    if ( data + SZ_RTMPTUPLE > end ) {
		syslog( LOG_INFO, "rtmp_packet missing second tuple" );
		return;
	    }
	    bcopy( data, &rt, SZ_RTMPTUPLE );
	    data += SZ_RTMPTUPLE;
	} else if ( rt.rt_dist & 0x80 ) {
	    if ( data + SZ_RTMPTUPLE > end ) {
		syslog( LOG_INFO, "rtmp_packet missing first range-end" );
		return;
	    }
	    bcopy( data, &xrt, SZ_RTMPTUPLE );
	    data += SZ_RTMPTUPLE;

	    if ( xrt.rt_dist != 0x82 ) {
		syslog( LOG_INFO, "rtmp_packet bad phase 2 version" );
		return;
	    }

	    /*
	     * Check for net range conflict.
	     */
	    if ( rt.rt_net != iface->i_rt->rt_firstnet ||
		    xrt.rt_net != iface->i_rt->rt_lastnet ) {
		syslog( LOG_INFO, "rtmp_packet interface mismatch" );
		return;
	    }
	} else {
#ifdef PHASE1NET
	    /*
	     * Gatorboxes put a net number in the first tuple, even on
	     * phase 1 nets.  This is wrong, but since we've got it, we
	     * might just as well check it.
	    if ( rt.rt_net != iface->i_rt->rt_firstnet ||
		    rt.rt_net != iface->i_rt->rt_lastnet ) {
		syslog( LOG_INFO, "rtmp_packet phase 1 interface mismatch" );
		return;
	    }
	     */
#else PHASE1NET
	    syslog( LOG_INFO, "rtmp_packet bad first tuple" );
	    return;
#endif PHASE1NET
	}

	/*
	 * Find gateway.
	 */
	for ( gate = iface->i_gate; gate; gate = gate->g_next ) {
	    if ( gate->g_sat.sat_addr.s_net == from->sat_addr.s_net &&
		    gate->g_sat.sat_addr.s_node == from->sat_addr.s_node ) {
		break;
	    }
	}
	if ( !gate ) {	/* new gateway */
	    if (( gate = (struct gate *)malloc( sizeof( struct gate ))) == 0 ) {
		syslog( LOG_ERR, "malloc: %m" );
		exit( 1 );
	    }
	    gate->g_next = iface->i_gate;
	    gate->g_prev = 0;
	    gate->g_rt = 0;
	    gate->g_iface = iface;	/* need this? */
	    gate->g_sat = *from;
	    if ( iface->i_gate ) {
		iface->i_gate->g_prev = gate;
	    }
	    iface->i_gate = gate;
	    syslog( LOG_INFO, "rtmp_packet gateway %u.%u up",
		    ntohs( gate->g_sat.sat_addr.s_net ),
		    gate->g_sat.sat_addr.s_node );
	}

	/*
	 * Reset the timeout on this gateway.  We'll remove the gateway
	 * entry, if the timeout gets to RTMPTAB_BAD.
	 */
	gate->g_state = RTMPTAB_GOOD;

	/*
	 * Parse remaining tuples.
	 */
	for (;;) {
	    /*
	     * Is route on this gateway?
	     */
	    for ( rtmp = gate->g_rt; rtmp; rtmp = rtmp->rt_next ) {
		if ( ntohs( rtmp->rt_firstnet ) <= ntohs( rt.rt_net ) &&
			ntohs( rtmp->rt_lastnet ) >= ntohs( rt.rt_net )) {
		    break;
		}
		if (( rt.rt_dist & 0x80 ) &&
			ntohs( rtmp->rt_firstnet ) <= ntohs( xrt.rt_net ) &&
			ntohs( rtmp->rt_lastnet ) >= ntohs( xrt.rt_net )) {
		    break;
		}
	    }

	    if ( rtmp ) {	/* found it */
		/*
		 * Check for range conflicts.  (This is getting a little
		 * ugly.)
		 */
		if ( rtmp->rt_firstnet != rt.rt_net ) {
		    syslog( LOG_INFO, "rtmp_packet firstnet mismatch %u!=%u",
			    ntohs( rtmp->rt_firstnet ), ntohs( rt.rt_net ));
		    return;
		}
		if ( rt.rt_dist & 0x80 ) {
		    if (( rtmp->rt_flags & RTMPTAB_EXTENDED ) == 0 ) {
			syslog( LOG_INFO, "rtmp_packet extended mismatch %u",
				ntohs( rtmp->rt_firstnet ));
			return;
		    }
		    if ( rtmp->rt_lastnet != xrt.rt_net ) {
			syslog( LOG_INFO, "rtmp_packet lastnet mismatch %u!=%u",
			    ntohs( rtmp->rt_lastnet ), ntohs( xrt.rt_net ));
			return;
		    }
		} else {
		    if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
			syslog( LOG_INFO, "rtmp_packet !extended mismatch %u",
				ntohs( rtmp->rt_firstnet ));
			return;
		    }
		    if ( rtmp->rt_lastnet != rt.rt_net ) {
			syslog( LOG_INFO, "rtmp_packet lastnet mismatch %u!=%u",
			    ntohs( rtmp->rt_lastnet ), ntohs( rt.rt_net ));
			return;
		    }
		}

		rtmp->rt_state = RTMPTAB_GOOD;

		/*
		 * Check hop count.  If the count has changed, update
		 * the routing database.
		 */
		if (( rtmp->rt_hops != ( rt.rt_dist & 0x7f ) + 1 ) &&
			( rtmp->rt_hops != RTMPHOPS_POISON ||
			( rt.rt_dist & 0x7f ) + 1 <= RTMPHOPS_MAX )) {
		    if ( rtmp->rt_iprev ) {	/* route is in use */
			if ( rtmp->rt_hops > ( rt.rt_dist & 0x7f ) + 1 ) {
			    /*
			     * If this was POISON, we've deleted it from
			     * the kernel.  Add it back in.
			     */
			    if ( rtmp->rt_hops == RTMPHOPS_POISON ) {
				gateroute( RTMP_ADD, rtmp );
			    }
			    rtmp->rt_hops = ( rt.rt_dist & 0x7f ) + 1;
			} else {
			    /*
			     * Hop count has gone up for this route.
			     * Search for a new best route.  If we can't
			     * find one, just keep this route.  "poison"
			     * route are deleted in as_timer().
			     */
			    if (( rt.rt_dist & 0x7f ) + 1 > RTMPHOPS_MAX ) {
				rtmp->rt_hops = RTMPHOPS_POISON;
			    } else {
				rtmp->rt_hops = ( rt.rt_dist & 0x7f ) + 1;
			    }
			    (void)rtmp_replace( rtmp );
			}
		    } else {			/* route not in use */
			rtmp->rt_hops = ( rt.rt_dist & 0x7f ) + 1;
			if ( rtmp->rt_hops > ( rt.rt_dist & 0x7f ) + 1 ) {
			    rtmp_new( rtmp );
			}
		    }
		}

		/*
		 * Make the *next* node the head, since
		 * we're not likely to be asked for the same tuple twice
		 * in a row.
		 */
		if ( rtmp->rt_next != 0 ) {
		    gate->g_rt->rt_prev->rt_next = gate->g_rt;
		    gate->g_rt = rtmp->rt_next;
		    rtmp->rt_next = 0;
		}
	    } else if (( rt.rt_dist & 0x7f ) + 1 > RTMPHOPS_MAX ) {
		syslog( LOG_INFO, "rtmp_packet bad hop count from %u.%u for %u",
			ntohs( from->sat_addr.s_net ), from->sat_addr.s_node,
			ntohs( rt.rt_net ));
	    } else {		/* new for router */
		if (( rtmp = newrt()) == 0 ) {
		    syslog( LOG_ERR, "newrt: %m" );
		    exit( 1 );
		}
		rtmp->rt_firstnet = rt.rt_net;
		if ( rt.rt_dist & 0x80 ) {
		    rtmp->rt_lastnet = xrt.rt_net;
		    rtmp->rt_flags = RTMPTAB_EXTENDED;
		} else {
		    rtmp->rt_lastnet = rt.rt_net;
		}
		rtmp->rt_hops = ( rt.rt_dist & 0x7f ) + 1;
		rtmp->rt_state = RTMPTAB_GOOD;
		rtmp->rt_gate = gate;

		/*
		 * Add rtmptab entry to end of list (leave head alone).
		 */
		if ( gate->g_rt == 0 ) {
		    rtmp->rt_prev = rtmp;
		    gate->g_rt = rtmp;
		} else {
		    rtmp->rt_prev = gate->g_rt->rt_prev;
		    gate->g_rt->rt_prev->rt_next = rtmp;
		    gate->g_rt->rt_prev = rtmp;
		}

		rtmp_new( rtmp );
	    }

	    if ( data + SZ_RTMPTUPLE > end ) {
		break;
	    }
	    bcopy( data, &rt, SZ_RTMPTUPLE );
	    data += SZ_RTMPTUPLE;
	    if ( rt.rt_dist & 0x80 ) {
		if ( data + SZ_RTMPTUPLE > end ) {
		    syslog( LOG_INFO, "rtmp_packet missing range-end" );
		    return;
		}
		bcopy( data, &xrt, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;
	    }
	}

	/*
	 * Make sure we've processed the whole packet.
	 */
	if ( data != end ) {
	    syslog( LOG_INFO, "rtmp_packet length and count mismatch" );
	}
	break;

    case DDPTYPE_RTMPR :
	/*
	 * Request and RDR.
	 */
	if ( ninterfaces == IFBASE || iface->i_rt->rt_zt == 0 ||
		( iface->i_flags & IFACE_CONFIG ) == 0 ) {
	    return;
	}
	if ( *data == 1 ) {
	    data = packet;
	    *data++ = DDPTYPE_RTMPRD;
	    rh.rh_net = iface->i_addr.sat_addr.s_net;
	    rh.rh_nodelen = 8;
	    rh.rh_node = iface->i_addr.sat_addr.s_node;
	    bcopy( &rh, data, sizeof( struct rtmp_head ));
	    data += sizeof( struct rtmp_head );

	    if ( iface->i_flags & IFACE_PHASE2 ) {
		rt.rt_net = iface->i_rt->rt_firstnet;
		rt.rt_dist = 0x80;
		bcopy( &rt, data, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;

		rt.rt_net = iface->i_rt->rt_lastnet;
		rt.rt_dist = 0x82;
		bcopy( &rt, data, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;
	    }
	    if ( sendto( ap->ap_fd, packet, data - packet, 0,
		    (struct sockaddr *)from,
		    sizeof( struct sockaddr_at )) < 0 ) {
		syslog( LOG_ERR, "as_timer sendto: %m" );
	    }
	} else if ( *data == 2 || *data == 3 ) {
#ifdef DEBUG
	    printf( "rtmp_packet rdr (%d) from %u.%u\n",
		    *data, ntohs( from->sat_addr.s_net ),
		    from->sat_addr.s_node );
#endif DEBUG
	} else {
	    syslog( LOG_INFO, "rtmp_packet unknown request from %u.%u\n",
		    ntohs( from->sat_addr.s_net ), from->sat_addr.s_node );
	}
	break;

    default :
	syslog( LOG_INFO, "rtmp_packet bad ddp type from %u.%u",
		    ntohs( from->sat_addr.s_net ), from->sat_addr.s_node );
	return;
    }
}

rtmp_request( iface )
    struct interface	*iface;
{
    struct sockaddr_at	sat;
    struct atport	*ap;
    char		*data, packet[ 2 ];
    int			i;

    syslog( LOG_INFO, "rtmp_request for %s", iface->i_name );

    for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
	if ( ap->ap_packet == rtmp_packet ) {
	    break;
	}
    }
    if ( ap == 0 ) {
	syslog( LOG_ERR, "rtmp_request can't find rtmp socket!" );
	exit( 1 );
    }

    data = packet;
    *data++ = DDPTYPE_RTMPR;
    *data++ = RTMPROP_REQUEST;

    /*
     * There is a problem with the net zero "hint" hack.
     */
    bzero( &sat, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    sat.sat_family = AF_APPLETALK;
    sat.sat_addr.s_net = iface->i_addr.sat_addr.s_net;
    sat.sat_addr.s_node = ATADDR_BCAST;
    sat.sat_port = ap->ap_port;
    if ( sendto( ap->ap_fd, packet, data - packet, 0, (struct sockaddr *)&sat,
	    sizeof( struct sockaddr_at )) < 0 ) {
	syslog( LOG_ERR, "rtmp_request sendto: %m" );
	exit( 1 );
    }
    return;
}

/*
 * Complete configuration for phase 1 interface using RTMP information.
 */
rtmp_config( rh, iface )
    struct rtmp_head	*rh;
    struct interface	*iface;
{
    extern int		stabletimer;

    /*
     * If we're configuring a phase 2 interface, don't complete
     * configuration with RTMP.
     */
    if ( iface->i_flags & IFACE_PHASE2 ) {
	syslog( LOG_INFO, "rtmp_config ignoring data" );
	return;
    }

    /*
     * Check our seed information, and reconfigure.
     */
    if ( rh->rh_net != iface->i_addr.sat_addr.s_net ) {
	if (( iface->i_flags & IFACE_SEED ) &&
		rh->rh_net != iface->i_caddr.sat_addr.s_net) {
	    syslog( LOG_ERR, "rtmp_config net mismatch %u != %u",
		    ntohs( rh->rh_net ),
		    ntohs( iface->i_addr.sat_addr.s_net ));
	    exit( 1 );
	}
	iface->i_addr.sat_addr.s_net = rh->rh_net;

	/*
	 * It is possible that we will corrupt our route database
	 * by just forcing this change.  XXX
	 */
	iface->i_rt->rt_firstnet = iface->i_rt->rt_lastnet = rh->rh_net;

	setaddr( iface, IFACE_PHASE1, iface->i_addr.sat_addr.s_net,
		iface->i_addr.sat_addr.s_node, rh->rh_net, rh->rh_net );
	stabletimer = UNSTABLE;
    }

    /* add addr to loopback route */
    if ( looproute( iface, RTMP_ADD ) < 0 ) {
	syslog( LOG_ERR, "rtmp_config: can't route %u.%u to loopback: %m",
		ntohs( iface->i_addr.sat_addr.s_net ),
		iface->i_addr.sat_addr.s_node );
    }

    syslog( LOG_INFO, "rtmp_config configured %s", iface->i_name );
    iface->i_flags |= IFACE_CONFIG;
    if ( iface == ciface ) {
	ciface = ciface->i_next;
	bootaddr( ciface );
    }
}

looproute( iface, cmd )
    struct interface	*iface;
    int			cmd;
{
    struct sockaddr_at	dst, loop;

    if ( cmd == RTMP_DEL && ( iface->i_flags & IFACE_LOOP ) == 0 ) {
	syslog( LOG_ERR, "looproute panic no route" );
	abort();
    }

    if ( cmd == RTMP_ADD && ( iface->i_flags & IFACE_LOOP )) {
	syslog( LOG_ERR, "looproute panic two routes" );
	abort();
    }

    bzero( &dst, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    dst.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    dst.sat_family = AF_APPLETALK;
    dst.sat_addr.s_net = iface->i_addr.sat_addr.s_net;
    dst.sat_addr.s_node = iface->i_addr.sat_addr.s_node;
    bzero( &loop, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    loop.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    loop.sat_family = AF_APPLETALK;
    loop.sat_addr.s_net = htons( ATADDR_ANYNET );
    loop.sat_addr.s_node = ATADDR_ANYNODE;

    if ( route( cmd, &dst, &loop, RTF_UP | RTF_HOST )) {
	return( -1 );
    }
    if ( cmd == RTMP_ADD ) {
	iface->i_flags |= IFACE_LOOP;
    }
    if ( cmd == RTMP_DEL ) {
	iface->i_flags &= ~IFACE_LOOP;
    }
    return( 0 );
}

gateroute( command, rtmp )
    int			command;
    struct rtmptab	*rtmp;
{
    struct sockaddr_at	dst, gate;
    unsigned short	net;

    if ( command == RTMP_DEL && ( rtmp->rt_flags & RTMPTAB_ROUTE ) == 0 ) {
	return( -1 );
    }
    if ( command == RTMP_ADD && ( rtmp->rt_flags & RTMPTAB_ROUTE )) {
	return( -1 );
    }

    /*
     * Since we will accept routes from gateways who advertise their
     * address as 0.YY, we must munge the gateway address we give to
     * the kernel.  Otherwise, we'll get a bunch of routes to the loop
     * back interface, and who wants that?
     */
    bzero( &gate, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    gate.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    gate.sat_family = AF_APPLETALK;
    gate.sat_addr.s_net = rtmp->rt_gate->g_sat.sat_addr.s_net;
    gate.sat_addr.s_node = rtmp->rt_gate->g_sat.sat_addr.s_node;
    if ( gate.sat_addr.s_net == 0 ) {
	gate.sat_addr.s_net = net;
    }

    bzero( &dst, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    dst.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    dst.sat_family = AF_APPLETALK;
    dst.sat_addr.s_node = ATADDR_ANYNODE;

    net = ntohs( rtmp->rt_firstnet );
    do {
	dst.sat_addr.s_net = htons( net );
	if ( route( command, &dst, &gate, RTF_UP | RTF_GATEWAY )) {
	    syslog( LOG_ERR, "route: %u -> %u.%u: %m", net,
		    ntohs( gate.sat_addr.s_net ), gate.sat_addr.s_node );
	    continue;
	}
    } while ( net++ < ntohs( rtmp->rt_lastnet ));

    if ( command == RTMP_ADD ) {
	rtmp->rt_flags |= RTMPTAB_ROUTE;
    }
    if ( command == RTMP_DEL ) {
	rtmp->rt_flags &= ~RTMPTAB_ROUTE;
    }

    return( 0 );
}

rtmp_new( rtmp )
    struct rtmptab	*rtmp;
{
    struct interface	*i;
    struct rtmptab	*r;
    extern int		newrtmpdata;

    newrtmpdata = 1;

    /*
     * Do we already have a gateway for this route?
     */
    for ( i = interfaces; i; i = i->i_next ) {
	for ( r = i->i_rt; r; r = r->rt_inext ) {
	    /* Should check RTMPTAB_EXTENDED here. XXX */
	    if (( ntohs( r->rt_firstnet ) <= ntohs( rtmp->rt_firstnet ) &&
		    ntohs( r->rt_lastnet ) >= ntohs( rtmp->rt_firstnet )) ||
		    ( ntohs( r->rt_firstnet ) <= ntohs( rtmp->rt_lastnet ) &&
		    ntohs( r->rt_lastnet ) >= ntohs( rtmp->rt_lastnet ))) {
		break;
	    }
	}
	if ( r ) {
	    break;
	}
    }

    /*
     * This part of this routine is almost never run.
     */
    if ( i ) {	/* can we get here without r being set? */
	if ( r->rt_firstnet != rtmp->rt_firstnet ||
		r->rt_lastnet != rtmp->rt_lastnet ) {
	    syslog( LOG_INFO, "rtmp_new netrange mismatch %u-%u != %u-%u",
		    ntohs( r->rt_firstnet ), ntohs( r->rt_lastnet ),
		    ntohs( rtmp->rt_firstnet ), ntohs( rtmp->rt_lastnet ));
	    return;
	}

	/*
	 * Note that our whole methodology is wrong, if we want to do
	 * route "load balancing."  This entails changing our route
	 * each time we receive a tuple of equal value.  In fact, we can't
	 * do this, using our method, since we only check against in-use
	 * routes when a tuple is new from a router.
	 */
	if ( r->rt_hops < rtmp->rt_hops ) {
	    return;
	}

	rtmp_copyzones( rtmp, r );
	rtmp_delinuse( r );
    }

    rtmp_addinuse( rtmp );
}

/*
 * Find a replacement for "replace".  If we can't find a replacement,
 * return 1.  If we do find a replacement, return 0.
 */
rtmp_replace( replace )
    struct rtmptab	*replace;
{
    struct interface	*iface;
    struct gate		*gate;
    struct rtmptab	*rtmp, *found = 0;

    for ( iface = interfaces; iface; iface = iface->i_next ) {
	for ( gate = iface->i_gate; gate; gate = gate->g_next ) {
	    for ( rtmp = gate->g_rt; rtmp; rtmp = rtmp->rt_next ) {
		if ( rtmp->rt_firstnet == replace->rt_firstnet &&
			rtmp->rt_lastnet == replace->rt_lastnet ) {
		    if ( found == 0 || rtmp->rt_hops < found->rt_hops ) {
			found = rtmp;
		    }
		    break;
		}
	    }
	}
    }

    if ( found != replace ) {
	rtmp_copyzones( found, replace );
	rtmp_delinuse( replace );
	rtmp_addinuse( found );
	if ( replace->rt_state == RTMPTAB_BAD ) {
	    rtmp_free( replace );
	}
	return( 0 );
    } else {
	if ( replace->rt_hops == RTMPHOPS_POISON ) {
	    gateroute( RTMP_DEL, replace );
	}
	return( 1 );
    }
}

/*
 * Remove rtmp from the in-use table and the per-gate table.
 * Free any associated space.
 */
rtmp_free( rtmp )
    struct rtmptab	*rtmp;
{
    struct gate		*gate;

    if ( rtmp->rt_iprev ) {
	rtmp_delinuse( rtmp );
    }

    /* remove from per-gate */
    gate = rtmp->rt_gate;
    if ( gate->g_rt == rtmp ) {				/* first */
	if ( rtmp->rt_prev == rtmp ) {			/* only */
	    gate->g_rt = 0;
	} else {
	    gate->g_rt = rtmp->rt_next;
	    rtmp->rt_next->rt_prev = rtmp->rt_prev;
	}
    } else {
	if ( rtmp->rt_next == 0 ) {			/* last */
	    rtmp->rt_prev->rt_next = 0;
	    gate->g_rt->rt_prev = rtmp->rt_prev;
	} else {
	    rtmp->rt_prev->rt_next = rtmp->rt_next;
	    rtmp->rt_next->rt_prev = rtmp->rt_prev;
	}
    }

    free( rtmp );
}

/*
 * Delete rtmp from the per-interface in-use table, remove all
 * zone references, and remove the route from the kernel.
 */
rtmp_delinuse( rtmp )
    struct rtmptab	*rtmp;
{
    struct rtmptab	*irt;
    struct ziptab	*zt;
    struct list		*lz, *flz, *lr, *flr;

    irt = rtmp->rt_gate->g_iface->i_rt;
    if ( irt->rt_inext == rtmp ) {			/* first */
	if ( rtmp->rt_iprev == rtmp ) {			/* only */
	    irt->rt_inext = 0;
	} else {
	    irt->rt_inext = rtmp->rt_inext;
	    rtmp->rt_inext->rt_iprev = rtmp->rt_iprev;
	}
    } else {
	if ( rtmp->rt_inext == 0 ) {			/* last */
	    rtmp->rt_iprev->rt_inext = 0;
	    irt->rt_inext->rt_iprev = rtmp->rt_iprev;
	} else {
	    rtmp->rt_iprev->rt_inext = rtmp->rt_inext;
	    rtmp->rt_inext->rt_iprev = rtmp->rt_iprev;
	}
    }
    rtmp->rt_iprev = 0;
    rtmp->rt_inext = 0;

    /* remove zone map */
    lz = rtmp->rt_zt;
    while ( lz ) {					/* for each zone */
	zt = (struct ziptab *)lz->l_data;
	lr = zt->zt_rt;
	while ( lr ) {					/* for each route */
	    if ( (struct rtmptab *)lr->l_data == rtmp ) {
		if ( lr->l_prev == 0 ) {		/* head */
		    if ( lr->l_next == 0 ) {		/* last route in zone */
			if ( zt->zt_prev == 0 ) {
			    ziptab = zt->zt_next;
			} else {
			    zt->zt_prev->zt_next = zt->zt_next;
			}
			if ( zt->zt_next == 0 ) {
			    ziplast = zt->zt_prev;
			} else {
			    zt->zt_next->zt_prev = zt->zt_prev;
			}
			free( zt->zt_bcast );
			free( zt->zt_name );
			free( zt );
		    } else {
			zt->zt_rt = lr->l_next;
		    }
		} else {
		    lr->l_prev->l_next = lr->l_next;
		}
		if ( lr->l_next != 0 ) {
		    lr->l_next->l_prev = lr->l_prev;
		}
		flr = lr;
		lr = lr->l_next;
		free( flr );
	    } else {
		lr = lr->l_next;
	    }
	}
	flz = lz;
	lz = lz->l_next;
	free( flz );
    }
    rtmp->rt_zt = 0;

    /* remove old route */
    gateroute( RTMP_DEL, rtmp );

    return;
}

/*
 * Add rtmp to the per-interface in-use table.  No verification is done...
 */
rtmp_addinuse( rtmp )
    struct rtmptab	*rtmp;
{
    struct rtmptab	*irt;

    gateroute( RTMP_ADD, rtmp );

    irt = rtmp->rt_gate->g_iface->i_rt;
    if ( irt->rt_inext == 0 ) {	/* emtpy list */
	rtmp->rt_inext = 0;
	rtmp->rt_iprev = rtmp;
	irt->rt_inext = rtmp;
    } else {
	rtmp->rt_inext = irt->rt_inext;
	rtmp->rt_iprev = irt->rt_inext->rt_iprev;
	irt->rt_inext->rt_iprev = rtmp;
	irt->rt_inext = rtmp;
    }
    return;
}

/*
 * Change the zone mapping to replace "from" with "to".  This code assumes
 * the consistency of both the route -> zone map and the zone -> route map.
 * This is probably a bad idea.  How can we insure that the data is good
 * at this point?  What do we do if we get several copies of a route in
 * an RTMP packet?
 */
rtmp_copyzones( to, from )
    struct rtmptab	*to, *from;
{
    struct list		*lz, *lr;

    to->rt_zt = from->rt_zt;
    from->rt_zt = 0;
    if ( from->rt_flags & RTMPTAB_HASZONES ) {
	to->rt_flags |= RTMPTAB_HASZONES;
    }
    for ( lz = to->rt_zt; lz; lz = lz->l_next ) {
	for ( lr = ((struct ziptab *)lz->l_data)->zt_rt; lr; lr = lr->l_next ) {
	    if ( (struct rtmptab *)lr->l_data == from ) {
		lr->l_data = (void *)to;	/* cast BS */
		break;
	    }
	}
	if ( lr == 0 ) {
	    syslog( LOG_ERR, "rtmp_copyzones z -> r without r -> z, abort" );
	    abort();
	}
    }

    return;
}

    struct rtmptab *
newrt()
{
    struct rtmptab	*rtmp;

    if (( rtmp = (struct rtmptab *)malloc(sizeof(struct rtmptab)))
	    == 0 ) {
	return( 0 );
    }

    bzero( rtmp, sizeof( struct rtmptab ));
    return( rtmp );
}
