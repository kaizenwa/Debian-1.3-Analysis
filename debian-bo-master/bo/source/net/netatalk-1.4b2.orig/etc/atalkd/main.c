/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#if defined( sun ) && defined( __svr4__ )
#include </usr/ucbinclude/sys/file.h>
#else sun __svr4__
#include <sys/file.h>
#endif sun __svr4__
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/errno.h>
#include <net/if.h>
#include <net/route.h>

#include <signal.h>
#include <syslog.h>
#include <stdio.h>
#include <strings.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>

#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/zip.h>
#include <atalk/rtmp.h>
#include <atalk/ddp.h>
#include <atalk/paths.h>

#ifdef __svr4__
#include <sys/sockio.h>
#include <termios.h>
#endif __svr4__

#include "interface.h"
#include "gate.h"
#include "list.h"
#include "rtmp.h"
#include "zip.h"
#include "atserv.h"
#include "multicast.h"

#ifndef WEXITSTATUS
#define WEXITSTATUS(x)	((x).w_retcode)
#endif WEXITSTATUS

#define elements(a)	(sizeof(a)/sizeof((a)[0]))

#define PKTSZ	1024

extern int	rtmp_packet();
extern int	nbp_packet();
extern int	aep_packet();
extern int	zip_packet();

int		rtfd;

struct atserv	atserv[] = {
    { "rtmp",		1,	rtmp_packet },		/* 0 */
    { "nbp",		2,	nbp_packet },		/* 1 */
    { "echo",		4,	aep_packet },		/* 2 */
    { "zip",		6,	zip_packet },		/* 3 */
};
int		atservNATSERV = elements( atserv );

struct interface	*interfaces = NULL, *ciface = NULL;

int		debug = 0, quiet = 0, chatty = 0;
char		*configfile = NULL;
int		ziptimeout = 0, transition = 0;
int		stabletimer, stable = 0, newrtmpdata = 0, noparent = 0;
int		ninterfaces;
int		defphase = IFACE_PHASE2;
int		nfds = 0;
fd_set		fds;
char		Packet[ PKTSZ ];
char		*version = VERSION;

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
as_timer()
{
    struct sockaddr_at	sat;
    struct ziphdr	zh;
    struct rtmp_head	rh;
    struct rtmp_tuple	rt;
    struct atport	*ap, *zap, *rap;
    struct interface	*iface, *iface2;
    struct gate		*gate, *fgate = NULL;
    struct rtmptab	*rtmp, *frtmp;
    struct ziptab	*zt;
    char		*data, *end, packet[ 587 ];
    int			sentzipq = 0;
    int			n, cc, i;

    bzero( (caddr_t)&sat, sizeof( struct sockaddr_at ));

    for ( iface = interfaces; iface; iface = iface->i_next ) {
	if ( iface->i_flags & IFACE_LOOPBACK ) {
	    continue;
	}
	for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
	    if ( ap->ap_packet == zip_packet ) {
		zap = ap;
	    }
	    if ( ap->ap_packet == rtmp_packet ) {
		rap = ap;
	    }
	}

	if (( iface->i_flags & ( IFACE_ADDR|IFACE_CONFIG|IFACE_NOROUTER )) ==
		IFACE_ADDR ) {
	    if ( iface->i_time < 3 ) {
		if ( iface->i_flags & IFACE_PHASE1 ) {
		    rtmp_request( iface );
		    newrtmpdata = 1;
		} else {
		    zip_getnetinfo( iface );
		    sentzipq = 1;
		}
		iface->i_time++;
	    } else {
		iface->i_flags |= IFACE_NOROUTER;
		if ( ninterfaces > IFBASE ) {
		    if (( iface->i_flags & IFACE_SEED ) == 0 ) {
			/*
			 * No seed info, and we've got multiple interfaces.
			 * Wait forever.
			 */
			syslog( LOG_INFO,
				"as_timer multiple interfaces, no seed" );
			syslog( LOG_INFO, "as_timer can't configure %s",
				iface->i_name );
			syslog( LOG_INFO, "as_timer waiting for router" );
			iface->i_time = 0;
			continue;
		    } else {
			/*
			 * Complete configuration for iface, and boot next
			 * interface.
			 */
			iface->i_flags |= IFACE_CONFIG;
			for ( zt = iface->i_czt; zt; zt = zt->zt_next ) {
			    addzone( iface->i_rt, zt->zt_len, zt->zt_name );
			}
			if ( iface->i_rt->rt_zt ) {
			    iface->i_rt->rt_flags &= ~RTMPTAB_ZIPQUERY;
			    iface->i_rt->rt_flags |= RTMPTAB_HASZONES;
			}
			if ( iface->i_flags & IFACE_PHASE1 ) {
			    syslog( LOG_INFO,
				    "as_timer configured %s phase 1 from seed",
				    iface->i_name );
			    setaddr( iface, IFACE_PHASE1,
				    iface->i_caddr.sat_addr.s_net,
				    iface->i_addr.sat_addr.s_node,
				    iface->i_caddr.sat_addr.s_net,
				    iface->i_caddr.sat_addr.s_net );
			} else {
			    syslog( LOG_INFO,
				    "as_timer configured %s phase 2 from seed",
				    iface->i_name );
			}

			if ( looproute( iface, RTMP_ADD ) < 0 ) {
			    syslog( LOG_ERR,
				    "as_timer: can't route %u.%u to loop: %m",
				    ntohs( iface->i_addr.sat_addr.s_net ),
				    iface->i_addr.sat_addr.s_node );
			    exit( 1 );
			}
			if ( iface == ciface ) {
			    ciface = ciface->i_next;
			    bootaddr( ciface );
			}
		    }
		} else {
		    /*
		     * Configure for no router operation.  Wait for a route
		     * to become available in rtmp_packet().
		     */
		    syslog( LOG_INFO, "config for no router" );
		    if ( iface->i_flags & IFACE_PHASE2 ) {
			iface->i_rt->rt_firstnet = 0;
			iface->i_rt->rt_lastnet = htons( STARTUP_LASTNET );
			setaddr( iface, IFACE_PHASE2,
				iface->i_addr.sat_addr.s_net,
				iface->i_addr.sat_addr.s_node,
				0, htons( STARTUP_LASTNET ));
		    }
		    if ( looproute( iface, RTMP_ADD ) < 0 ) {
			syslog( LOG_ERR,
				"as_timer: can't route %u.%u to loopback: %m",
				ntohs( iface->i_addr.sat_addr.s_net ),
				iface->i_addr.sat_addr.s_node );
			exit( 1 );
		    }
		}
	    }
	}

	for ( gate = iface->i_gate; gate; gate = gate->g_next ) {
	    if ( fgate ) {
		free( (caddr_t)fgate );
		fgate = NULL;
	    }

	    n = 0;
	    data = packet + 1 + sizeof( struct ziphdr );
	    end = packet + sizeof( packet );

	    sat = gate->g_sat;
	    sat.sat_port = zap->ap_port;

	    /*
	     * Perform timeouts on routers.  If we've only got one
	     * interface, we'll use these timeouts to decide that
	     * our zone has gone away.
	     */
	    if ( ++gate->g_state >= RTMPTAB_BAD ) {
		syslog( LOG_INFO, "as_timer gateway %u.%u down",
			ntohs( gate->g_sat.sat_addr.s_net ),
			gate->g_sat.sat_addr.s_node );
		rtmp = gate->g_rt;
		while ( rtmp ) {
		    frtmp = rtmp->rt_next;
		    if ( rtmp->rt_hops == RTMPHOPS_POISON ||
			    rtmp->rt_iprev == 0 ) {
			rtmp_free( rtmp );
		    } else {
			rtmp->rt_hops = RTMPHOPS_POISON;
			if ( rtmp_replace( rtmp ) > 0 ) {
			    gate->g_state = rtmp->rt_state = RTMPTAB_GOOD;
			}
		    }
		    rtmp = frtmp;
		}
		if ( gate->g_rt == 0 ) {
		    if ( gate->g_prev == 0 ) {
			gate->g_iface->i_gate = gate->g_next;
		    } else {
			gate->g_prev->g_next = gate->g_next;
		    }
		    if ( gate->g_next != 0 ) {
			gate->g_next->g_prev = gate->g_prev;
		    }
		    fgate = gate;	/* can't free here, just mark it */
		}
		/*
		 * If this is the last router on the only interface,
		 * reconfigure our netrange.  By marking the interface
		 * as having no router, we will notice when a router
		 * comes back up.
		 */
		if ( gate->g_iface->i_gate == 0 && ninterfaces == IFBASE ) {
		    gate->g_iface->i_flags |= IFACE_NOROUTER;
		    gate->g_iface->i_flags &= ~IFACE_CONFIG;
		    syslog( LOG_INFO, "as_timer last gateway down" );

		    /* Set netrange to 0-fffe.  */
		    if ( gate->g_iface->i_flags & IFACE_PHASE2 ) {
			gate->g_iface->i_rt->rt_firstnet = 0;
			gate->g_iface->i_rt->rt_lastnet =
				htons( STARTUP_LASTNET );
			setaddr( iface, IFACE_PHASE2,
				iface->i_addr.sat_addr.s_net,
				iface->i_addr.sat_addr.s_node,
				0, htons( STARTUP_LASTNET ));
		    }
		}
		continue;
	    }

	    /*
	     * If we don't have a zone for our interface yet, ask for
	     * it from any router (all routers) on the interface.
	     */
	    if (( iface->i_rt->rt_flags & RTMPTAB_HASZONES ) == 0 ) {
		iface->i_rt->rt_flags |= RTMPTAB_ZIPQUERY;
		bcopy( &iface->i_rt->rt_firstnet, data, sizeof( u_short ));
		data += sizeof( u_short );
		n++;
	    }

	    rtmp = gate->g_rt;
	    while ( rtmp ) {
		/*
		 * Delete old routing tuples.
		 */
		if ( rtmp->rt_state != RTMPTAB_PERM ) {
		    rtmp->rt_state++;
		}

		/*
		 * We've not been updated for this route in a while.  If
		 * it's not in use, go ahead and remove it.  If it is in
		 * use, mark the route as down (POISON), and look for a
		 * better route.  If one is found, delete this route and use
		 * the new one.  If it's not found, mark the route as GOOD
		 * (so we'll propogate our poison) and delete it the next
		 * time it becomes BAD.
		 */
		if ( rtmp->rt_state >= RTMPTAB_BAD ) {
		    frtmp = rtmp->rt_next;
		    if ( rtmp->rt_iprev == 0 ) {	/* not in use */
			rtmp_free( rtmp );
		    } else {				/* in use */
			if ( rtmp->rt_hops == RTMPHOPS_POISON ) {
			    rtmp_free( rtmp );
			} else {
			    rtmp->rt_hops = RTMPHOPS_POISON;
			    if ( rtmp_replace( rtmp ) > 0 ) {
				rtmp->rt_state = RTMPTAB_GOOD;
			    }
			}
		    }
		    rtmp = frtmp;
		    continue;
		}

		/*
		 * Do ZIP lookups.
		 */
		if ( rtmp->rt_iprev &&
			( rtmp->rt_flags & RTMPTAB_HASZONES ) == 0 ) {
		    if ( data + sizeof( u_short ) > end || n == 255 ) {
			/* send what we've got */
			zh.zh_op = ZIPOP_QUERY;
			zh.zh_count = n;
			cc = data - packet;
			data = packet;
			*data++ = DDPTYPE_ZIP;
			bcopy( &zh, data, sizeof( struct ziphdr ));

			if ( sendto( zap->ap_fd, packet, cc, 0,
				(struct sockaddr *)&sat,
				sizeof( struct sockaddr_at )) < 0 ) {
			    syslog( LOG_ERR, "as_timer sendto: %m" );
			}
			sentzipq = 1;

			n = 0;
			data = packet + 1 + sizeof( struct ziphdr );
			end = packet + sizeof( packet );
		    }

		    /*
		     * rt_nzq is number of ZIP Queries we've issued for a
		     * given netrange.  If we've got ziptimeout on, we
		     * will only ask 3 times for any given netrange.
		     * Interestingly enough, since rt_nzq is a u_char,
		     * it will overflow after a while.  This means we will
		     * periodically ask for nets that we've decided not to
		     * ask about, and warn that we can't get it's zone.
		     */
		    if ( rtmp->rt_nzq++ == 3 ) {
			syslog( LOG_INFO, "as_timer can't get zone for %u",
				ntohs( rtmp->rt_firstnet ));
		    }
		    if ( rtmp->rt_nzq > 3 ) {
			if ( ziptimeout ) {
			    rtmp = rtmp->rt_next;
			    continue;
			}
		    } else {
			sentzipq = 1;
		    }
		    rtmp->rt_flags |= RTMPTAB_ZIPQUERY;
		    bcopy( &rtmp->rt_firstnet, data, sizeof( u_short ));
		    data += sizeof( u_short );
		    n++;
		}
		rtmp = rtmp->rt_next;
	    }

	    /* send what we've got */
	    if ( n > 0 ) {
		zh.zh_op = ZIPOP_QUERY;
		zh.zh_count = n;
		cc = data - packet;
		data = packet;
		*data++ = DDPTYPE_ZIP;
		bcopy( &zh, data, sizeof( struct ziphdr ));

		if ( sendto( zap->ap_fd, packet, cc, 0, (struct sockaddr *)&sat,
			sizeof( struct sockaddr_at )) < 0 ) {
		    syslog( LOG_ERR, "as_timer sendto: %m" );
		}
	    }
	}
	if ( fgate ) {
	    free( (caddr_t)fgate );
	    fgate = NULL;
	}

	/*
	 * Send RTMP broadcasts.
	 */
	if ( ninterfaces > IFBASE ) {
#ifdef BSD4_4
	    sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
	    sat.sat_family = AF_APPLETALK;
	    sat.sat_addr.s_net = 0;
	    sat.sat_addr.s_node = ATADDR_BCAST;
	    sat.sat_port = rap->ap_port;

	    data = packet;
	    end = data + sizeof( packet );
	    *data++ = DDPTYPE_RTMPRD;
	    rh.rh_net = iface->i_addr.sat_addr.s_net;
	    rh.rh_nodelen = 8;
	    rh.rh_node = iface->i_addr.sat_addr.s_node;
	    bcopy( &rh, data, sizeof( struct rtmp_head ));
	    data += sizeof( struct rtmp_head );
	    n = 0;


	    if ( iface->i_flags & IFACE_PHASE1 ) {
		rt.rt_net = 0;
		rt.rt_dist = 0x82;
		bcopy( &rt, data, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;
	    } else {
		rt.rt_net = iface->i_rt->rt_firstnet;
		rt.rt_dist = 0x80;
		bcopy( &rt, data, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;

		rt.rt_net = iface->i_rt->rt_lastnet;
		rt.rt_dist = 0x82;
		bcopy( &rt, data, SZ_RTMPTUPLE );
		data += SZ_RTMPTUPLE;
	    }

	    for ( iface2 = interfaces; iface2; iface2 = iface2->i_next ) {
		if (( iface2->i_flags & IFACE_CONFIG ) == 0 ||
			iface == iface2 ) {
		    continue;
		}
		/*
		 * Fill in tuples.  Always send the same thing, regardless
		 * of the phase of the destination.  Routers who don't
		 * understand extended rtmp packets will toss extended
		 * tuples because their distance will have the high bit set.
		 */
		for ( rtmp = iface2->i_rt; rtmp; rtmp = rtmp->rt_inext ) {
		    /* don't broadcast routes we have no zone for */
		    if ( rtmp->rt_zt == 0 ||
			    ( rtmp->rt_flags & RTMPTAB_ZIPQUERY ) ||
			    ( rtmp->rt_flags & RTMPTAB_HASZONES ) == 0 ) {
			continue;
		    }

		    if ((( rtmp->rt_flags & RTMPTAB_EXTENDED ) &&
			    data + 2 * SZ_RTMPTUPLE > end ) ||
			    data + SZ_RTMPTUPLE > end ) {
			if ( sendto( rap->ap_fd, packet, data - packet, 0,
				(struct sockaddr *)&sat,
				sizeof( struct sockaddr_at )) < 0 ) {
			    syslog( LOG_ERR, "as_timer sendto %u.%u (%u): %m",
				    ntohs( sat.sat_addr.s_net ),
				    sat.sat_addr.s_node,
				    ntohs( iface->i_rt->rt_firstnet ));
			}

			if ( iface->i_flags & IFACE_PHASE2 ) {
			    data = packet + 1 + sizeof( struct rtmp_head ) +
				    2 * SZ_RTMPTUPLE;
			} else {
			    data = packet + 1 + sizeof( struct rtmp_head ) +
				    SZ_RTMPTUPLE;
			}
			n = 0;
		    }

		    rt.rt_net = rtmp->rt_firstnet;
		    rt.rt_dist = rtmp->rt_hops;
		    if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
			rt.rt_dist |= 0x80;
		    }
		    bcopy( &rt, data, SZ_RTMPTUPLE );
		    data += SZ_RTMPTUPLE;

		    if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
			rt.rt_net = rtmp->rt_lastnet;
			rt.rt_dist = 0x82;
			bcopy( &rt, data, SZ_RTMPTUPLE );
			data += SZ_RTMPTUPLE;
		    }
		    n++;
		}
	    }

	    /* send rest */
	    if ( n ) {
		if ( sendto( rap->ap_fd, packet, data - packet, 0,
			(struct sockaddr *)&sat,
			sizeof( struct sockaddr_at )) < 0 ) {
		    syslog( LOG_ERR, "as_timer sendto %u.%u (%u): %m",
			    ntohs( sat.sat_addr.s_net ),
			    sat.sat_addr.s_node,
			    ntohs( iface->i_rt->rt_firstnet ));
		}
	    }
	}
    }

    /*
     * Check if we're stable.  Each time we configure an interface, we
     * sent stabletimer to UNSTABLE.  If stabletimer ever gets to
     * STABLEANYWAY, we give up and decide to "be" stable anyway.
     * Normally, we wait for stabletimer get <= STABLE with no new rtmp
     * data and all zip data complete.
     */
    if ( !stable ) {
	if ( stabletimer <= STABLE && !newrtmpdata && !sentzipq ) {
	    /* write out config file */
	    stable = 1;
	    writeconf( configfile );
	} else {
	    if ( stabletimer-- <= STABLEANYWAY ) {
		stable = 1;
	    }
	}
	newrtmpdata = 0;

	if ( stable && !noparent ) {
	    noparent = 1;
	    syslog( LOG_INFO, "ready %d/%d/%d", stabletimer, newrtmpdata,
		    sentzipq );
	    if ( !debug ) {
		/*
		 * Seems like we could get here more than once...
		 */
		if ( kill( getpid(), SIGSTOP ) < 0 ) {
		    syslog( LOG_ERR, "as_timer: kill-self failed!" );
		    exit( 1 );
		}
	    }
	}
    }

#ifdef DEBUG
    consistency();
#endif DEBUG

    return;
}

#ifdef DEBUG
/*
* Consistency check...
*/
consistency()
{
    struct rtmptab	*rtmp;
    struct list		*lr, *lz;
    struct ziptab	*zt;

    for ( zt = ziptab; zt; zt = zt->zt_next ) {
	for ( lr = zt->zt_rt; lr; lr = lr->l_next ) {
	    rtmp = (struct rtmptab *)lr->l_data;
	    if ( rtmp->rt_iprev == 0 && rtmp->rt_gate != 0 ) {
		syslog( LOG_ERR, "%.*s has %u-%u (unused)\n",
			zt->zt_len, zt->zt_name, ntohs( rtmp->rt_firstnet ),
			ntohs( rtmp->rt_lastnet ));
		abort();
	    }
	    for ( lz = rtmp->rt_zt; lz; lz = lz->l_next ) {
		if ( zt == (struct ziptab *)lz->l_data ) {
		    break;
		}
	    }
	    if ( lz == 0 ) {
		syslog( LOG_ERR, "no map from %u-%u to %.*s\n", 
			ntohs( rtmp->rt_firstnet ),
			ntohs( rtmp->rt_lastnet ),
			zt->zt_len, zt->zt_name );
		abort();
	    }
	}
    }
}
#endif DEBUG

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
as_debug()
{
    struct interface	*iface;
    struct list		*l;
    struct ziptab	*zt;
    struct gate		*gate;
    struct rtmptab	*rt;
    FILE		*rtmpdebug;

    if (( rtmpdebug = fopen( _PATH_ATALKDEBUG, "w" )) == NULL ) {
	syslog( LOG_ERR, "rtmp: %m" );
	return;
    }

    for ( iface = interfaces; iface; iface = iface->i_next ) {
	fprintf( rtmpdebug, "interface %s %u.%u ", iface->i_name,
		ntohs( iface->i_addr.sat_addr.s_net ),
		iface->i_addr.sat_addr.s_node );
	if ( iface->i_flags & IFACE_PHASE1 ) {
	    putc( '1', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_PHASE2 ) {
	    putc( '2', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_SEED ) {
	    putc( 'S', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_ADDR ) {
	    putc( 'A', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_CONFIG ) {
	    putc( 'C', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_NOROUTER ) {
	    putc( 'N', rtmpdebug );
	}
	if ( iface->i_flags & IFACE_LOOP ) {
	    putc( 'L', rtmpdebug );
	}
	putc( '\n', rtmpdebug );

	if ( iface->i_rt ) {
	    fprintf( rtmpdebug, "\t%u-%u ",
		    ntohs( iface->i_rt->rt_firstnet ),
		    ntohs( iface->i_rt->rt_lastnet ));
	    if ( iface->i_rt->rt_flags & RTMPTAB_ZIPQUERY ) {
		putc( 'q', rtmpdebug );
	    }
	    if ( iface->i_rt->rt_flags & RTMPTAB_HASZONES ) {
		putc( 'z', rtmpdebug );
	    }
	    if ( iface->i_rt->rt_flags & RTMPTAB_EXTENDED ) {
		putc( 'x', rtmpdebug );
	    }
	    putc( 'i', rtmpdebug );
	    for ( l = iface->i_rt->rt_zt; l; l = l->l_next ) {
		zt = (struct ziptab *)l->l_data;
		fprintf( rtmpdebug, " '%.*s'", zt->zt_len, zt->zt_name );
	    }
	    fprintf( rtmpdebug, "\n" );
	}

	for ( gate = iface->i_gate; gate; gate = gate->g_next ) {
	    fprintf( rtmpdebug, "gate %u.%u %X\n",
		    ntohs( gate->g_sat.sat_addr.s_net ),
		    gate->g_sat.sat_addr.s_node, gate->g_state );
	    for ( rt = gate->g_rt; rt; rt = rt->rt_next ) {
		fprintf( rtmpdebug, "\t%u-%u ", ntohs( rt->rt_firstnet ),
			ntohs( rt->rt_lastnet ));
		if ( rt->rt_flags & RTMPTAB_ZIPQUERY ) {
		    putc( 'q', rtmpdebug );
		}
		if ( rt->rt_flags & RTMPTAB_HASZONES ) {
		    putc( 'z', rtmpdebug );
		}
		if ( rt->rt_flags & RTMPTAB_EXTENDED ) {
		    putc( 'x', rtmpdebug );
		}
		if ( rt->rt_iprev ) {
		    putc( 'i', rtmpdebug );
		}
		for ( l = rt->rt_zt; l; l = l->l_next ) {
		    zt = (struct ziptab *)l->l_data;
		    fprintf( rtmpdebug, " '%.*s'", zt->zt_len, zt->zt_name );
		}
		fprintf( rtmpdebug, "\n" );
	    }
	}
    }

    fclose( rtmpdebug );
    return;
}

/*
 * Called when SIGTERM is recieved.  Remove all routes and then exit.
 */
#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
as_down()
{
    struct interface	*iface;
    struct gate		*gate;
    struct rtmptab	*rt;

    for ( iface = interfaces; iface; iface = iface->i_next ) {
	for ( gate = iface->i_gate; gate; gate = gate->g_next ) {
	    for ( rt = gate->g_rt; rt; rt = rt->rt_next ) {
		if ( rt->rt_iprev ) {
		    if ( gateroute( RTMP_DEL, rt ) < 0 ) {
			syslog( LOG_ERR, "as_down remove %u-%u failed: %m",
				ntohs( rt->rt_firstnet ),
				ntohs( rt->rt_lastnet ));
		    }
		}
	    }
	}
	if ( iface->i_flags & IFACE_LOOP ) {
	    if ( looproute( iface, RTMP_DEL ) < 0 ) {
		syslog( LOG_ERR, "as_down remove %s %u.%u failed: %m",
			iface->i_name, ntohs( iface->i_addr.sat_addr.s_net ),
			iface->i_addr.sat_addr.s_node );
	    }
	}

#ifdef BSD4_4
	if ( ifconfig( iface->i_name, SIOCDIFADDR, &iface->i_addr )) {
	    syslog( LOG_ERR, "difaddr: %m" );
	    exit( 1 );
	}
#endif BSD4_4
    }

    syslog( LOG_INFO, "done" );
    exit( 0 );
}

main( ac, av )
    int		ac;
    char	**av;
{
    struct sockaddr_at	sat;
    struct sigaction	sv;
    struct itimerval	it;
    struct interface	*iface;
    int			status;
    struct atport	*ap;
    fd_set		readfds;
    int			fd, i, mask, c, fromlen, pidfd;
    char		*prog, *pidfile = NULL;
    extern int		errno, optind;
    extern char		*optarg;

    while (( c = getopt( ac, av, "12qsdtf:P:" )) != EOF ) {
	switch ( c ) {
	case '1' :
	    defphase = IFACE_PHASE1;
	    break;

	case '2' :
	    defphase = IFACE_PHASE2;
	    break;

	case 'd' :
	    debug++;
	    break;

	case 'f' :
	    configfile = optarg;
	    break;

	case 'q' :	/* don't seed */
	    quiet++;
	    break;

	case 's' :	/* seed */
	    chatty++;
	    break;

	case 't' :	/* transition */
	    transition++;
	    break;

	case 'P' :	/* pid file */
	    pidfile = optarg;
	    break;

	default :
	    fprintf( stderr, "Unknown option -- '%c'\n", c );
	    exit( 1 );
	}
    }
    if ( optind != ac ) {
	fprintf( stderr, "Too many arguments.\n" );
	exit( 1 );
    }

    if (( prog = rindex( av[ 0 ], '/' )) == NULL ) {
	prog = av[ 0 ];
    } else {
	prog++;
    }

    /*
     * Configure loop back address first, so appearances of "lo0" in
     * the config file fail.  Also insures that lo0 gets configured,
     * even if there's some hangup during configuration of some
     * other interface.
     */
    if (( interfaces = newiface( LOOPIFACE )) == NULL ) {
	perror( "newiface" );
	exit( 1 );
    }
    interfaces->i_flags |= IFACE_PHASE2 | IFACE_LOOPBACK;

    /*
     * Check our initial configuration before we fork. This way we can
     * complain about syntax errors on stdout.
     *
     * Basically, if we're going to read our config file, we should read
     * it and initialize our data structures. If we're not going to read
     * our config file, use GIFCONF to initialize our data structures.
     */
    if ( readconf( configfile ) < 0 && getifconf() < 0 ) {
	fprintf( stderr, "%s: can't get interfaces, exiting.\n", prog );
	exit( 1 );
    }

    /*
     * At this point, we have (at least partially) initialized data
     * structures. Fill in what we can and verify that nothing is obviously
     * broken.
     */
    for ( ninterfaces = 0, iface = interfaces; iface;
	    iface = iface->i_next, ninterfaces++ ) {
	/* Apply the default phase */
	if (( iface->i_flags & IFACE_PHASE1 ) == 0 &&
		( iface->i_flags & IFACE_PHASE2 ) == 0 ) {
	    iface->i_flags |= defphase;
	}

	/* Set default addresses */
	if ( iface->i_rt == NULL ) {
	    if (( iface->i_rt = newrt()) == NULL ) {
		perror( "newrt" );
		exit( 1 );
	    }

	    if ( iface->i_flags & IFACE_PHASE1 ) {
		iface->i_rt->rt_firstnet = iface->i_rt->rt_lastnet =
			iface->i_caddr.sat_addr.s_net;
	    } else {
		if ( iface->i_caddr.sat_addr.s_net != ATADDR_ANYNET ||
			( iface->i_flags & IFACE_LOOPBACK )) {
		    iface->i_rt->rt_firstnet = iface->i_rt->rt_lastnet =
			    iface->i_caddr.sat_addr.s_net;
		} else {
		    iface->i_rt->rt_firstnet = htons( STARTUP_FIRSTNET );
		    iface->i_rt->rt_lastnet = htons( STARTUP_LASTNET );
		}
	    }
	}

	if (( iface->i_flags & IFACE_PHASE1 ) == 0 ) {
	    iface->i_rt->rt_flags |= RTMPTAB_EXTENDED;
	}

	if ( iface->i_caddr.sat_addr.s_net == ATADDR_ANYNET ) {
	    iface->i_caddr.sat_addr.s_net = iface->i_rt->rt_firstnet;
	}

	if ( debug ) {
	    dumpconfig( iface );	/* probably needs args */
	}
    }

    /*
     * A little consistency check...
     */
    if ( ninterfaces < IFBASE ) {
	fprintf( stderr, "%s: zero interfaces, exiting.\n", prog );
	exit( 1 );
    }

    if ( pidfile ) {
	if (( pidfd = open( pidfile, O_CREAT | O_WRONLY, 0644 )) < 0 ) {
	    perror( pidfile );
	    exit( 1 );
	}
	if ( flock( pidfd, LOCK_EX | LOCK_NB ) < 0 ) {
	    if ( errno == EWOULDBLOCK ) {
		fprintf( stderr, "atalkd is already running\n" );
	    } else {
		perror( "flock" );
	    }
	    exit( 1 );
	}
	if ( ftruncate( pidfd, 0 ) < 0 ) {
	    perror( "ftruncate" );
	    exit( 1 );
	}
    }

    /*
     * Disassociate. The child will send itself a signal when it is
     * stable. This indicates that other processes may begin using
     * AppleTalk.
     */
    if ( !debug ) {
	switch( i = fork()) {
	case 0 :
	    i = getdtablesize();
	    for ( fd = 0; fd < i; fd++ ) {
		if ( pidfile && fd != pidfd ) {
		    (void)close( fd );
		}
	    }
	    if (( fd = open( "/dev/tty", O_RDWR )) >= 0 ) {
		(void)ioctl( fd, TIOCNOTTY, (char *)0 );
		(void)close( fd );
	    }
	    (void)setpgid( 0, getpid());
	    break;

	case -1 :
	    perror( "fork" );
	    exit( 1 );

	default :
	    /*
	     * Wait for the child to send itself a SIGSTOP, after which
	     * we send it a SIGCONT and exit ourself.
	     */
	    if ( wait3( &status, WUNTRACED, (struct rusage *)0 ) != i ) {
		perror( "wait3" );	/* Child died? */
		exit( 1 );
	    }
	    if ( !WIFSTOPPED( status )) {
		fprintf( stderr, "AppleTalk not up!" );
		if ( WIFEXITED( status )) {
		    fprintf( stderr, " Child exited with %d.\n",
			    WEXITSTATUS( status ));
		} else {
		    fprintf( stderr, " Child died.\n" );
		}
		exit( 1 );
	    }
	    if ( kill( i, SIGCONT ) < 0 ) {
		perror( "kill" );
		exit( 1 );
	    }
	    exit( 0 );
	}
    }

    if ( pidfile ) {
	FILE	*pf;

	if (( pf = fdopen( pidfd, "w" )) == NULL ) {
	    fprintf( stderr, "Can't fdopen pidfile!\n" );
	    exit( 1 );
	}
	fprintf( pf, "%d\n", getpid());
	fflush( pf );
    }

#ifdef ultrix
    openlog( prog, LOG_PID );
#else ultrix
    openlog( prog, LOG_PID, LOG_DAEMON );
#endif ultrix

    syslog( LOG_INFO, "restart (%s)", version );

#ifdef __svr4__
    if ( plumb() < 0 ) {
	syslog( LOG_ERR, "can't establish STREAMS plumbing" );
	exit( 1 );
    }
#endif __svr4__

    /*
     * Socket for use in routing ioctl()s. Can't add routes to our
     * interfaces until we have our routing socket.
     */
#ifdef BSD4_4
    if (( rtfd = socket( PF_ROUTE, SOCK_RAW, AF_APPLETALK )) < 0 ) {
	syslog( LOG_ERR, "route socket: %m" );
	exit( 1 );
    }
    if ( shutdown( rtfd, 0 ) < 0 ) {
	syslog( LOG_ERR, "route shutdown: %m" );
	exit( 1 );
    }
#else BSD4_4
    if (( rtfd = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	syslog( LOG_ERR, "route socket: %m" );
	exit( 1 );
    }
#endif BSD4_4

    sv.sa_handler = as_down;
    sigemptyset( &sv.sa_mask );
    sigaddset( &sv.sa_mask, SIGUSR1 );
    sigaddset( &sv.sa_mask, SIGALRM );
    sigaddset( &sv.sa_mask, SIGTERM );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGTERM, &sv, (struct sigaction *)0 ) < 0 ) {
	syslog( LOG_ERR, "sigterm: %m" );
	exit( 1 );
    }

    sv.sa_handler = as_debug;
    sigemptyset( &sv.sa_mask );
    sigaddset( &sv.sa_mask, SIGUSR1 );
    sigaddset( &sv.sa_mask, SIGALRM );
    sigaddset( &sv.sa_mask, SIGTERM );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGUSR1, &sv, (struct sigaction *)0 ) < 0 ) {
	syslog( LOG_ERR, "sigusr1: %m" );
	exit( 1 );
    }

    sv.sa_handler = as_timer;
    sigemptyset( &sv.sa_mask );
    sigaddset( &sv.sa_mask, SIGUSR1 );
    sigaddset( &sv.sa_mask, SIGALRM );
    sigaddset( &sv.sa_mask, SIGTERM );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGALRM, &sv, (struct sigaction *)0 ) < 0 ) {
	syslog( LOG_ERR, "sigalrm: %m" );
	exit( 1 );
    }

    it.it_interval.tv_sec = 10L;
    it.it_interval.tv_usec = 0L;
    it.it_value.tv_sec = 10L;
    it.it_value.tv_usec = 0L;
    if ( setitimer( ITIMER_REAL, &it, (struct itimerval *)0 ) < 0 ) {
	syslog( LOG_ERR, "setitimer: %m" );
	exit( 1 );
    }

    ciface = interfaces;
    bootaddr( ciface );
    for (;;) {
	readfds = fds;
	if ( select( nfds, &readfds, (fd_set *)0, (fd_set *)0,
		(struct timeval *)0 ) < 0 ) {
	    if ( errno == EINTR ) {
		errno = 0;
		continue;
	    } else {
		syslog( LOG_ERR, "select: %m" );
		exit( 1 );
	    }
	}

	for ( iface = interfaces; iface; iface = iface->i_next ) {
	    for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
		if ( FD_ISSET( ap->ap_fd, &readfds )) {
		    if ( ap->ap_packet ) {
			fromlen = sizeof( struct sockaddr_at );
			if (( c = recvfrom( ap->ap_fd, Packet, sizeof( Packet ),
				0, (struct sockaddr *)&sat, &fromlen )) < 0 ) {
			    syslog( LOG_ERR, "recvfrom: %m" );
			    continue;
			}
#ifdef DEBUG
			if ( debug ) {
			    printf( "packet from %u.%u on %s (%x) %d (%d)\n",
				    ntohs( sat.sat_addr.s_net ),
				    sat.sat_addr.s_node, iface->i_name,
				    iface->i_flags, ap->ap_port, ap->ap_fd );
			    bprint( Packet, c );
			}
#endif DEBUG
#ifdef __svr4__
			if ( sighold( SIGALRM ) || sighold( SIGUSR1 )) {
			    syslog( LOG_ERR, "sighold: %m" );
			    exit( 1 );
			}
#else __svr4__
			mask = sigsetmask( sigmask( SIGALRM ) |
				sigmask( SIGUSR1 ));
#endif __svr4__
			( *ap->ap_packet )( ap, &sat, Packet, c );

#ifdef DEBUG
			consistency();
#endif DEBUG
#ifdef __svr4__
			if ( sigrelse( SIGUSR1 ) || sigrelse( SIGALRM )) {
			    syslog( LOG_ERR, "sigrelse: %m" );
			    exit( 1 );
			}
#else __svr4__
			sigsetmask( mask );
#endif __svr4__
		    }
		}
	    }
	}
    }
}

/*
 * This code is called (from main(), as_timer(), zip_packet(),
 * and rtmp_packet()) to set the initial "bootstrapping" address
 * on an interface.
 */
bootaddr( iface )
    struct interface	*iface;
{
    struct sockaddr	sa;

    if ( iface == 0 ) {
	return;
    }

    /* consistency */
    if ( iface->i_flags & IFACE_ADDR ) {
	syslog( LOG_ERR, "bootaddr OOPS!" );
	abort();
    }

    if ( iface->i_flags & IFACE_PHASE1 ) {
	setaddr( iface, IFACE_PHASE1, 0,
		iface->i_caddr.sat_addr.s_node, 0, 0 );

	if ( iface->i_flags & IFACE_LOOPBACK ) {
	    iface->i_flags |= IFACE_CONFIG | IFACE_ADDR;
	    if ( ciface == iface ) {
		ciface = ciface->i_next;
		bootaddr( ciface );
	    }
	} else {
	    rtmp_request( iface );
	}

    } else {
	setaddr( iface, IFACE_PHASE2, iface->i_caddr.sat_addr.s_net,
		iface->i_caddr.sat_addr.s_node,
		iface->i_rt->rt_firstnet, iface->i_rt->rt_lastnet );

	if ( iface->i_flags & IFACE_LOOPBACK ) {
	    iface->i_flags |= IFACE_CONFIG | IFACE_ADDR;
	    if ( ciface == iface ) {
		ciface = ciface->i_next;
		bootaddr( ciface );
	    }
	} else {
	    /* configure multicast for this interface */
	    bzero( &sa, sizeof( struct sockaddr ));
	    bcopy( ethermulti, sa.sa_data, sizeof( ethermulti ));
	    if ( ifconfig( iface->i_name, SIOCADDMULTI, &sa )) {
		syslog( LOG_ERR, "addmulti: %m" );
		exit( 1 );
	    }

	    zip_getnetinfo( iface );
	}
    }
    ++iface->i_time;
    iface->i_flags |= IFACE_ADDR;
    stabletimer = UNSTABLE;
    return;
}

/*
 * Change setaddr()
 * to manage the i_ports field and the fds for select().
 */
setaddr( iface, phase, net, node, first, last )
    struct interface	*iface;
    u_char		phase;
    u_short		net;
    u_char		node;
    u_short		first, last;
{
    int			i;
    struct atserv	*as;
    struct atport	*ap;
    struct servent	*se;
    struct sockaddr_at	sat;
    struct netrange	nr;

    if ( iface->i_ports == NULL ) {	/* allocate port structures */
	for ( i = 0, as = atserv; i < atservNATSERV; i++, as++ ) {
	    if (( se = getservbyname( as->as_name, "ddp" )) == NULL ) {
		syslog( LOG_INFO, "%s: service unknown", as->as_name );
	    } else {
		as->as_port = ntohs( se->s_port );
	    }
	    if (( ap = (struct atport *)malloc( sizeof( struct atport ))) ==
		    NULL ) {
		syslog( LOG_ERR, "malloc: %m" );
		exit( 1 );
	    }
	    ap->ap_fd = 0;
	    ap->ap_next = iface->i_ports;
	    ap->ap_iface = iface;
	    ap->ap_port = as->as_port;
	    ap->ap_packet = as->as_packet;

	    iface->i_ports = ap;
	}
    } else {				/* close ports */
	for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
	    (void)close( ap->ap_fd );
	}
    }

#ifdef BSD4_4
    iface->i_addr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    iface->i_addr.sat_family = AF_APPLETALK;
    iface->i_addr.sat_addr.s_net = net;
    iface->i_addr.sat_addr.s_node = node;

    nr.nr_phase = phase;
    nr.nr_firstnet = first;
    nr.nr_lastnet = last;
    bcopy( &nr, iface->i_addr.sat_zero, sizeof( struct netrange ));

    if ( ifconfig( iface->i_name, SIOCSIFADDR, &iface->i_addr )) {
	syslog( LOG_ERR, "setifaddr: %s: %m", iface->i_name );
	exit( 1 );
    }
    if ( ifconfig( iface->i_name, SIOCGIFADDR, &iface->i_addr )) {
	syslog( LOG_ERR, "getifaddr: %s: %m", iface->i_name );
	exit( 1 );
    }

    /* open ports */
    for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
	if (( ap->ap_fd = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	    syslog( LOG_ERR, "socket: %m" );
	    exit( 1 );
	}

	bzero( &sat, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
	sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
	sat.sat_family = AF_APPLETALK;
	sat.sat_addr.s_net = iface->i_addr.sat_addr.s_net;
	sat.sat_addr.s_node = iface->i_addr.sat_addr.s_node;
	sat.sat_port = ap->ap_port;

	if ( bind( ap->ap_fd, (struct sockaddr *)&sat,
		sizeof( struct sockaddr_at )) < 0 ) {
	    syslog( LOG_ERR, "bind %u.%u:%u: %m", ntohs( sat.sat_addr.s_net ),
		    sat.sat_addr.s_node, sat.sat_port );
	    exit( 1 );
	}
    }

    /* recalculate nfds and fds */
    FD_ZERO( &fds );
    for ( nfds = 0, iface = interfaces; iface; iface = iface->i_next ) {
	for ( ap = iface->i_ports; ap; ap = ap->ap_next ) {
	    FD_SET( ap->ap_fd, &fds );
	    if ( ap->ap_fd > nfds ) {
		nfds = ap->ap_fd;
	    }
	}
    }
    nfds++;

    return;
}

ifconfig( iname, cmd, sa )
    char		*iname;
    int			cmd;
    struct sockaddr_at	*sa;
{
    struct ifreq	ifr;
    int			s;

    strcpy( ifr.ifr_name, iname );
    ifr.ifr_addr = *(struct sockaddr *)sa;

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	return( 1 );
    }
    if ( ioctl( s, cmd, &ifr ) < 0 ) {
	return( 1 );
    }
    close( s );
    if ( cmd == SIOCGIFADDR ) {
	*(struct sockaddr *)sa = ifr.ifr_addr;
    }
    return( 0 );
}

dumpconfig( iface )
    struct interface	*iface;
{
    struct list		*l;

    printf( "%s", iface->i_name );
    if ( iface->i_flags & IFACE_SEED ) {
	printf( " -seed" );
    }
    printf( " -phase" );
    if ( iface->i_flags & IFACE_PHASE1 ) {
	printf( " 1" );
    } else {
	printf( " 2" );
    }
    printf( " -net %d", ntohs( iface->i_rt->rt_firstnet ));
    if ( iface->i_rt->rt_lastnet != iface->i_rt->rt_firstnet ) {
	printf( "-%d", ntohs( iface->i_rt->rt_lastnet ));
    }
    printf( " -addr %u.%u", ntohs( iface->i_addr.sat_addr.s_net ),
	    iface->i_addr.sat_addr.s_node );
    printf( " -caddr %u.%u", ntohs( iface->i_caddr.sat_addr.s_net ),
	    iface->i_caddr.sat_addr.s_node );
    for ( l = iface->i_rt->rt_zt; l; l = l->l_next ) {
	printf( " -zone %.*s", ((struct ziptab *)l->l_data)->zt_len,
		((struct ziptab *)l->l_data)->zt_name );
    }
    printf( "\n" );
}

#ifdef DEBUG
dumproutes()
{
    struct interface	*iface;
    struct rtmptab	*rtmp;
    struct list		*l;
    struct ziptab	*zt;

    for ( iface = interfaces; iface; iface = iface->i_next ) {
	for ( rtmp = iface->i_rt; rtmp; rtmp = rtmp->rt_inext ) {
	    if ( rtmp->rt_gate == 0 ) {
		if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
		    printf( "%u-%u", ntohs( rtmp->rt_firstnet ),
			    ntohs( rtmp->rt_lastnet ));
		} else {
		    printf( "%u", ntohs( rtmp->rt_firstnet ));
		}
	    } else {
		if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
		    printf( "%u.%u for %u-%u",
			    ntohs( rtmp->rt_gate->g_sat.sat_addr.s_net ),
			    rtmp->rt_gate->g_sat.sat_addr.s_node,
			    ntohs( rtmp->rt_firstnet ),
			    ntohs( rtmp->rt_lastnet ));
		} else {
		    printf( "%u.%u for %u",
			    ntohs( rtmp->rt_gate->g_sat.sat_addr.s_net ),
			    rtmp->rt_gate->g_sat.sat_addr.s_node,
			    ntohs( rtmp->rt_firstnet ));
		}
	    }

	    if ( rtmp->rt_iprev == 0 && rtmp != iface->i_rt ) {
		printf( " *" );
	    }

	    for ( l = rtmp->rt_zt; l; l = l->l_next ) {
		zt = (struct ziptab *)l->l_data;
		printf( " %.*s", zt->zt_len, zt->zt_name );
	    }

	    printf( "\n" );
	}
    }

    printf( "\n" );
    fflush( stdout );
}

dumpzones()
{
    struct interface	*iface;
    struct rtmptab	*rtmp;
    struct list		*l;
    struct ziptab	*zt;

    for ( zt = ziptab; zt; zt = zt->zt_next ) {
	printf( "%.*s", zt->zt_len, zt->zt_name );
	for ( l = zt->zt_rt; l; l = l->l_next ) {
	    rtmp = (struct rtmptab *)l->l_data;
	    if ( rtmp->rt_flags & RTMPTAB_EXTENDED ) {
		printf( " %u-%u", ntohs( rtmp->rt_firstnet ),
			ntohs( rtmp->rt_lastnet ));
	    } else {
		printf( " %u", ntohs( rtmp->rt_firstnet ));
	    }
	    if ( rtmp->rt_iprev == 0 && rtmp->rt_gate != 0 ) {
		printf( "*" );
	    }
	}
	printf( "\n" );
    }

    printf( "\n" );
    fflush( stdout );
}
#endif DEBUG
