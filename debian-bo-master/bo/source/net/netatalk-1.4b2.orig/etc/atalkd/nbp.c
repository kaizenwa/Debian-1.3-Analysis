/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netatalk/at.h>
#include <atalk/ddp.h>
#include <atalk/nbp.h>

#ifdef __svr4__
#include <sys/sockio.h>
#endif __svr4__

#include "atserv.h"
#include "interface.h"
#include "list.h"
#include "rtmp.h"
#include "gate.h"
#include "zip.h"
#include "nbp.h"
#include "multicast.h"

extern u_char	*zone_mcast();
extern int	transition;

struct nbptab	*nbptab = 0;

nbp_packet( ap, from, data, len )
    struct atport	*ap;
    struct sockaddr_at	*from;
    char		*data;
    int			len;
{
    struct nbphdr	nh;
    struct nbptuple	nt;
    struct nbpnve	nn;
    struct sockaddr	sa;
    struct sockaddr_at	sat;
    struct nbptab	*ntab;
    struct ziptab	*zt;
    struct interface	*iface;
    struct list		*l;
    struct rtmptab	*rtmp;
    char		*end, *nbpop, *zonep, packet[ 587 ];
    int			n, i, cc, locallkup = 0;
    u_short		tonet;

    end = data + len;
    if ( data >= end ) {
	syslog( LOG_INFO, "nbp_packet malformed packet" );
	return;
    }
    if ( *data++ != DDPTYPE_NBP ) {
	syslog( LOG_INFO, "nbp_packet bad ddp type" );
	return;
    }

    if ( data + SZ_NBPHDR + SZ_NBPTUPLE > end ) {
	syslog( LOG_INFO, "nbp_packet: malformed packet" );
	return;
    }
    bcopy( data, &nh, SZ_NBPHDR );
    nbpop = data;			/* remember for fwd and brrq */
    data += SZ_NBPHDR;
    if ( nh.nh_cnt != 1 ) {
	syslog( LOG_INFO, "nbp_packet: bad tuple count (%d/%d)", nh.nh_cnt,
		nh.nh_op );
	return;
    }

    bcopy( data, &nt, SZ_NBPTUPLE );
    data += SZ_NBPTUPLE;

    bzero( &nn.nn_sat, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    nn.nn_sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    nn.nn_sat.sat_family = AF_APPLETALK;
    nn.nn_sat.sat_addr.s_net = nt.nt_net;
    nn.nn_sat.sat_addr.s_node = nt.nt_node;
    nn.nn_sat.sat_port = nt.nt_port;

    /* object */
    if ( data >= end || ( *data < 0 || *data > 32 ) || data + *data > end ) {
	syslog( LOG_INFO, "nbp_packet: malformed packet" );
	return;
    }
    nn.nn_objlen = *data++;
    bcopy( data, nn.nn_obj, nn.nn_objlen );
    data += nn.nn_objlen;

    /* type */
    if ( data >= end || ( *data < 0 || *data > 32 ) || data + *data > end ) {
	syslog( LOG_INFO, "nbp_packet: malformed packet" );
	return;
    }
    nn.nn_typelen = *data++;
    bcopy( data, nn.nn_type, nn.nn_typelen );
    data += nn.nn_typelen;

    /* zone */
    if ( data >= end || ( *data < 0 || *data > 32 ) || data + *data > end ) {
	syslog( LOG_INFO, "nbp_packet: malformed packet" );
	return;
    }
    zonep = data;			/* remember for fwd */
    nn.nn_zonelen = *data++;
    bcopy( data, nn.nn_zone, nn.nn_zonelen );
    data += nn.nn_zonelen;

    if ( data != end ) {
	syslog( LOG_INFO, "nbp_packet: malformed packet" );
	return;
    }

    switch ( nh.nh_op ) {

    case NBPOP_RGSTR :
	/*
	 * Find the ziptab entry for the zone we're trying to register in.
	 */
	if ( nn.nn_zonelen == 0 ||
		( nn.nn_zonelen == 1 && *nn.nn_zone == '*' )) {
	    if ( interfaces->i_next->i_rt->rt_zt ) {
		zt = (struct ziptab *)interfaces->i_next->i_rt->rt_zt->l_data;
	    } else {
		zt = 0;
	    }
	} else {
	    for ( zt = ziptab; zt; zt = zt->zt_next ) {
		if ( zt->zt_len == nn.nn_zonelen && strndiacasecmp( zt->zt_name,
			nn.nn_zone, zt->zt_len ) == 0 ) {
		    break;
		}
	    }
	    if ( zt == 0 ) {
		nbp_ack( ap->ap_fd, NBPOP_ERROR, (int)nh.nh_id, from );
		return;
	    }
	}

	/*
	 * Observe that we don't have to do any local-zone verification
	 * if the zone aleady has a multicast address set.
	 */
	if ( zt != 0 && zt->zt_bcast == 0 ) {
	    /*
	     * Check if zone is associated with any of our local interfaces.
	     */
	    for ( iface = interfaces; iface; iface = iface->i_next ) {
		for ( l = iface->i_rt->rt_zt; l; l = l->l_next ) {
		    if ( zt == (struct ziptab *)l->l_data ) {
			break;
		    }
		}
		if ( l != 0 ) {
		    break;
		}
	    }
	    if ( iface == 0 ) {
		nbp_ack( ap->ap_fd, NBPOP_ERROR, (int)nh.nh_id, from );
		return;
	    }

	    /* calculate and save multicast address */
	    if (( zt->zt_bcast =
		    (u_char *)malloc( sizeof( ethermulti ))) == 0 ) {
		syslog( LOG_ERR, "nbp rgstr malloc: %m" );
		exit( 1 );
	    }
	    bcopy( zone_mcast( zt->zt_name, zt->zt_len ), zt->zt_bcast,
		    sizeof( ethermulti ));
	    bzero( &sa, sizeof( struct sockaddr ));
	    bcopy( zt->zt_bcast, sa.sa_data, sizeof( ethermulti ));

	    for ( iface = interfaces; iface; iface = iface->i_next ) {
		if (( iface->i_flags & IFACE_PHASE2 ) == 0 ) {
		    continue;
		}
		for ( l = iface->i_rt->rt_zt; l; l = l->l_next ) {
		    if ( zt == (struct ziptab *)l->l_data ) {
			/* add multicast */
			if ( ifconfig( iface->i_name, SIOCADDMULTI, &sa )) {
			    syslog( LOG_ERR, "addmulti: %m" );
			    exit( 1 );
			}
		    }
		}
	    }
	}

	if (( ntab = (struct nbptab *)malloc( sizeof( struct nbptab )))
		== 0 ) {
	    syslog( LOG_ERR, "malloc: %m" );
	    exit( 1 );
	}
	bcopy( &nn, &ntab->nt_nve, sizeof( struct nbpnve ));
	ntab->nt_next = nbptab;
	ntab->nt_prev = 0;
	if ( nbptab != 0 ) {
	    nbptab->nt_prev = ntab;
	}
	nbptab = ntab;

	nbp_ack( ap->ap_fd, NBPOP_OK, (int)nh.nh_id, from );
	break;

    case NBPOP_UNRGSTR :
	/* remove from our data, perhaps removing a multicast address */
	for ( ntab = nbptab; ntab; ntab = ntab->nt_next ) {
	    if ( ntab->nt_nve.nn_objlen != nn.nn_objlen ||
		    strndiacasecmp( ntab->nt_nve.nn_obj, nn.nn_obj,
		    nn.nn_objlen )) {
		continue;
	    }
	    if ( ntab->nt_nve.nn_typelen != nn.nn_typelen ||
		    strndiacasecmp( ntab->nt_nve.nn_type, nn.nn_type,
		    nn.nn_typelen )) {
		continue;
	    }
	    /*
	     * I *think* we really do check the zone, here.
	     */
	    if ((( ntab->nt_nve.nn_zonelen == 1 &&
		    *ntab->nt_nve.nn_zone == '*' ) ||
		    ntab->nt_nve.nn_zonelen == 0 ) &&
		    (( nn.nn_zonelen == 1 && *nn.nn_zone == '*' ) ||
		    nn.nn_zonelen == 0 )) {
		break;
	    }
	    if ( ntab->nt_nve.nn_zonelen != nn.nn_zonelen ||
		    strndiacasecmp( ntab->nt_nve.nn_zone, nn.nn_zone,
		    nn.nn_zonelen )) {
		break;
	    }
	}
	if ( ntab == 0 ) {
	    nbp_ack( ap->ap_fd, NBPOP_ERROR, (int)nh.nh_id, from );
	    return;
	}

	if ( ntab->nt_next != 0 ) {
	    ntab->nt_next->nt_prev = ntab->nt_prev;
	}
	if ( ntab->nt_prev != 0 ) {
	    ntab->nt_prev->nt_next = ntab->nt_next;
	}
	if ( ntab == nbptab ) {
	    nbptab = ntab->nt_next;
	}

	/*
	 * Check for another nbptab entry with the same zone.  If
	 * there isn't one, find the ziptab entry for the zone and
	 * remove the multicast address from the appropriate interfaces.
	 * XXX
	 */

	nbp_ack( ap->ap_fd, NBPOP_OK, (int)nh.nh_id, from );
	break;

    case NBPOP_BRRQ :
	/*
	 * Couple of things:  1. Unless we have the -t flag (which is sort
	 * of a misnomer, since you need it if you're doing any phase 1
	 * work), always send NBPOP_FWD.  2. If we get a zone of '*',
	 * and we know what the sender meant by '*', we copy the real
	 * zone into the packet.
	 */
	if ( nn.nn_zonelen == 0 ||
		( nn.nn_zonelen == 1 && *nn.nn_zone == '*' )) {
	    iface = ap->ap_iface;
	    if ( iface && iface->i_rt->rt_zt ) {
		zt = (struct ziptab *)iface->i_rt->rt_zt->l_data;
	    } else if ( interfaces->i_next->i_rt->rt_zt ) {
		zt = (struct ziptab *)interfaces->i_next->i_rt->rt_zt->l_data;
	    } else {
		zt = 0;
	    }

	    /*
	     * Copy zone into packet.  Note that we're changing len, data, and
	     * nbpop.  Later, we'll use ( data - len ) to mean the beginning
	     * of this packet.
	     */
	    if ( zt ) {
		bcopy( data - len, packet, len );
		nbpop = packet + ( len - ( data - nbpop ));
		data = packet + ( len - ( data - zonep ));
		*data++ = zt->zt_len;
		bcopy( zt->zt_name, data, zt->zt_len );
		data += zt->zt_len;
		len = data - packet;
	    }
	} else {
	    for ( zt = ziptab; zt; zt = zt->zt_next ) {
		if ( zt->zt_len == nn.nn_zonelen && strndiacasecmp( zt->zt_name,
			nn.nn_zone, zt->zt_len ) == 0 ) {
		    break;
		}
	    }
	    if ( zt == 0 ) {
		nbp_ack( ap->ap_fd, NBPOP_ERROR, (int)nh.nh_id, from );
		return;
	    }
	}

	locallkup = 0;

	/*
	 * If we've got no zones, send out LKUP on the local net.
	 * Otherwise, look through the zone table.
	 */
	if ( zt == 0 ) {
#ifdef BSD4_4
	    sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
	    sat.sat_family = AF_APPLETALK;
	    sat.sat_port = ap->ap_port;

	    nh.nh_op = NBPOP_LKUP;
	    bcopy( &nh, nbpop, SZ_NBPHDR );
	    sat.sat_addr.s_net = 0;			/* XXX */
	    sat.sat_addr.s_node = ATADDR_BCAST;

	    /* Find the first non-loopback ap */
	    for ( iface = interfaces; iface; iface = iface->i_next ) {
		if (( iface->i_flags & IFACE_LOOPBACK ) == 0 ) {
		    break;
		}
	    }
	    if ( iface == 0 ) {
		return;
	    }
	    for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
		if ( ap->ap_packet == nbp_packet ) {
		    break;
		}
	    }

	    if ( sendto( ap->ap_fd, data - len, len, 0, (struct sockaddr *)&sat,
		    sizeof( struct sockaddr_at )) < 0 ) {
		syslog( LOG_ERR, "nbp brrq sendto: %m" );
	    }

	    locallkup = 1;
	} else {
#ifdef BSD4_4
	    sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
	    sat.sat_family = AF_APPLETALK;
	    sat.sat_port = ap->ap_port;
	    for ( l = zt->zt_rt; l; l = l->l_next ) {
		rtmp = (struct rtmptab *)l->l_data;

		if ( rtmp->rt_gate == 0 ) {
		    for ( iface = interfaces; iface;
			    iface = iface->i_next ) {
			if ( iface->i_rt == rtmp ) {
			    break;
			}
		    }
		    if ( !iface ) {
			syslog( LOG_ERR, "Can't find route's interface!" );
			abort();
		    }
		    ap = iface->i_ports;
		} else {
		    ap = rtmp->rt_gate->g_iface->i_ports;
		}
		for ( ; ap; ap = ap->ap_next ) {
		    if ( ap->ap_packet == nbp_packet ) {
			break;
		    }
		}
		if ( !ap ) {
		    syslog( LOG_ERR, "Can't find port!" );
		    abort();
		}

		if ( transition &&
			( rtmp->rt_flags & RTMPTAB_EXTENDED ) == 0 ) {
		    if ( rtmp->rt_gate == 0 ) {
			locallkup = 1;
		    }
		    nh.nh_op = NBPOP_LKUP;
		    bcopy( &nh, nbpop, SZ_NBPHDR );
		    sat.sat_addr.s_net = rtmp->rt_firstnet;
		    sat.sat_addr.s_node = ATADDR_BCAST;
		} else {
		    if ( rtmp->rt_gate == 0 ) {
			nh.nh_op = NBPOP_LKUP;
			bcopy( &nh, nbpop, SZ_NBPHDR );
			sat.sat_addr.s_net = 0;
			sat.sat_addr.s_node = ATADDR_BCAST;
			locallkup = 1;
		    } else {
			nh.nh_op = NBPOP_FWD;
			bcopy( &nh, nbpop, SZ_NBPHDR );
			sat.sat_addr.s_net = rtmp->rt_firstnet;
			sat.sat_addr.s_node = 0;
		    }
		}

		if ( sendto( ap->ap_fd, data - len, len, 0,
			(struct sockaddr *)&sat,
			sizeof( struct sockaddr_at )) < 0 ) {
		    syslog( LOG_ERR, "nbp brrq sendto %u.%u: %m",
			    ntohs( sat.sat_addr.s_net ), sat.sat_addr.s_node );
		    continue;
		}
	    }
	}

	if ( !locallkup ) {
	    break;
	}
	/*FALL THROUGH*/

    case NBPOP_FWD :
	/* send lkup on net */
	/* should we check that we're a router? XXX */
	if ( !locallkup && ninterfaces > IFBASE ) {
	    nh.nh_op = NBPOP_LKUP;
	    bcopy( &nh, nbpop, SZ_NBPHDR );
	    from->sat_addr.s_net = 0;
	    from->sat_addr.s_node = ATADDR_BCAST;
	    if ( sendto( ap->ap_fd, data - len, len, 0, (struct sockaddr *)from,
		    sizeof( struct sockaddr_at )) < 0 ) {
		syslog( LOG_ERR, "nbp fwd sendto %u.%u: %m",
			ntohs( from->sat_addr.s_net ), from->sat_addr.s_node );
		return;
	    }
	}
	/*FALL THROUGH*/

    case NBPOP_LKUP :
	/* search our data */
	n = i = 0;
	data = packet + 1 + SZ_NBPHDR;
	end = packet + sizeof( packet );

	for ( ntab = nbptab; ntab; ntab = ntab->nt_next ) {
	    if ( nn.nn_objlen != 1 || *nn.nn_obj != '=' ) {
		if ( ntab->nt_nve.nn_objlen != nn.nn_objlen ||
			strndiacasecmp( ntab->nt_nve.nn_obj, nn.nn_obj,
			nn.nn_objlen )) {
		    continue;
		}
	    }

	    if ( nn.nn_typelen != 1 || *nn.nn_type != '=' ) {
		if ( ntab->nt_nve.nn_typelen != nn.nn_typelen ||
			strndiacasecmp( ntab->nt_nve.nn_type, nn.nn_type,
			nn.nn_typelen )) {
		    continue;
		}
	    }

	    if ( nn.nn_zonelen != 0 &&
		    ( nn.nn_zonelen != 1 || *nn.nn_zone != '*' )) {
		if ( ntab->nt_nve.nn_zonelen == 0 ||
			( ntab->nt_nve.nn_zonelen == 1 &&
			*ntab->nt_nve.nn_zone == '*' )) {
		    if ( interfaces->i_next->i_rt->rt_zt ) {
			zt = (struct ziptab *)interfaces->i_next->i_rt->
				rt_zt->l_data;
			if ( zt->zt_len != nn.nn_zonelen ||
				strndiacasecmp( zt->zt_name, nn.nn_zone,
				zt->zt_len )) {
			    continue;
			}
		    }
		} else {
		    if ( ntab->nt_nve.nn_zonelen != nn.nn_zonelen ||
			    strndiacasecmp( ntab->nt_nve.nn_zone, nn.nn_zone,
			    nn.nn_zonelen )) {
			continue;
		    }
		}
	    }

	    /*
	     * Another tuple won't fit. Send what we've already
	     * got, and start the next packet.
	     */
	    if ( data + SZ_NBPTUPLE + 3 + ntab->nt_nve.nn_objlen +
		    ntab->nt_nve.nn_typelen + ntab->nt_nve.nn_zonelen > end ) {
		nh.nh_op = NBPOP_LKUPREPLY;
		nh.nh_cnt = n;
		cc = data - packet;
		data = packet;
		*data++ = DDPTYPE_NBP;
		bcopy( &nh, data, SZ_NBPHDR );

		if ( sendto( ap->ap_fd, packet, cc, 0,
			(struct sockaddr *)&nn.nn_sat,
			sizeof( struct sockaddr_at )) < 0 ) {
		    syslog( LOG_ERR, "nbp lkup sendto %u.%u: %m",
			    ntohs( nn.nn_sat.sat_addr.s_net ),
			    nn.nn_sat.sat_addr.s_node );
		    return;
		}

		n = 0;
		data = packet + 1 + SZ_NBPHDR;
		end = packet + sizeof( packet );
	    }

	    nt.nt_net = ntab->nt_nve.nn_sat.sat_addr.s_net;
	    nt.nt_node = ntab->nt_nve.nn_sat.sat_addr.s_node;
	    nt.nt_port = ntab->nt_nve.nn_sat.sat_port;
	    /*
	     * Right now, we'll just give each name a unique enum.  In
	     * the future, we might need to actually assign and save
	     * an enum, based on the associated address.  For the moment,
	     * the enums will be unique and constant, since the order
	     * is fixed.
	     */
	    nt.nt_enum = i++;

	    bcopy( &nt, data, SZ_NBPTUPLE );
	    data += SZ_NBPTUPLE;

	    *data++ = ntab->nt_nve.nn_objlen;
	    bcopy( ntab->nt_nve.nn_obj, data, ntab->nt_nve.nn_objlen );
	    data += ntab->nt_nve.nn_objlen;

	    *data++ = ntab->nt_nve.nn_typelen;
	    bcopy( ntab->nt_nve.nn_type, data, ntab->nt_nve.nn_typelen );
	    data += ntab->nt_nve.nn_typelen;

	    /*
	     * Macs won't see something with a zone of 0 length.  We
	     * will always return '*' instead.  Perhaps we should
	     * unconditionally return the real zone?
	     */
	    if ( ntab->nt_nve.nn_zonelen ) {
		*data++ = ntab->nt_nve.nn_zonelen;
		bcopy( ntab->nt_nve.nn_zone, data, ntab->nt_nve.nn_zonelen );
		data += ntab->nt_nve.nn_zonelen;
	    } else {
		*data++ = 1;
		*data++ = '*';
	    }

	    n++;
	}

	if ( n != 0 ) {
	    nh.nh_op = NBPOP_LKUPREPLY;
	    nh.nh_cnt = n;
	    cc = data - packet;
	    data = packet;
	    *data++ = DDPTYPE_NBP;
	    bcopy( &nh, data, SZ_NBPHDR );

	    if ( sendto( ap->ap_fd, packet, cc, 0,
		    (struct sockaddr *)&nn.nn_sat,
		    sizeof( struct sockaddr_at )) < 0 ) {
		syslog( LOG_ERR, "nbp lkup sendto %u.%u: %m",
			ntohs( nn.nn_sat.sat_addr.s_net ),
			nn.nn_sat.sat_addr.s_node );
		return;
	    }
	}
	break;

    default :
	syslog( LOG_INFO, "nbp_packet: bad op (%d)", nh.nh_op );
    }
}

nbp_ack( fd, nh_op, nh_id, to )
    int			fd;
    int			nh_op;
    int			nh_id;
    struct sockaddr_at	*to;
{
    struct nbphdr	nh;
    char		*data, packet[ SZ_NBPHDR + 1 ];

    nh.nh_op = nh_op;
    nh.nh_cnt = 0;
    nh.nh_id = nh_id;
    data = packet;
    *data++ = DDPTYPE_NBP;
    bcopy( &nh, data, SZ_NBPHDR );
    data += SZ_NBPHDR;
    if ( sendto( fd, packet, data - packet, 0, (struct sockaddr *)to,
	    sizeof( struct sockaddr_at )) < 0 ) {
	syslog( LOG_ERR, "sendto: %m" );
	return;
    }
}
