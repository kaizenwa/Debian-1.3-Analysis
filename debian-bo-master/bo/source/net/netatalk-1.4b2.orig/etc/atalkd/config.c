/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/syslog.h>
#include <sys/param.h>
#include <net/if.h>
#include <netatalk/at.h>
#include <netatalk/endian.h>
#include <atalk/paths.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <fcntl.h>

#ifdef __svr4__
#include <sys/sockio.h>
#include <sys/stropts.h>
#endif __svr4__

#include "interface.h"
#include "rtmp.h"
#include "zip.h"
#include "list.h"

int	seed(), phase(), net(), addr(), zone();

struct param {
    char	*p_name;
    int		(*p_func)();
} params[] = {
    { "seed",	seed },
    { "phase",	phase },
    { "net",	net },
    { "addr",	addr },
    { "zone",	zone },
};

    char **
parseline( line )
    char	*line;
{
    char	*p;
    int		argc = 0;
    static char	*argv[ 128 ];
    static char	buf[ 1024 ];

    if ( *line == '#' ) {
	return( NULL );
    }
    argc = 0;

    strcpy( buf, line );
    p = buf;

    /*
     * This parser should be made more powerful -- it should
     * handle various escapes, e.g. \" and \031.
     */
    while ( *p != '\0' ) {
	while ( *p != '\0' && *p != '"' && isspace( *p )) {
	    p++;
	}
	if ( *p == '\0' ) {
	    argv[ argc ] = 0;
	    break;
	}
	if ( *p == '"' ) {
	    argv[ argc ] = ++p;
	    while ( *p != '\0' && *p != '"' ) {
		p++;
	    }
	} else {
	    argv[ argc ] = p;
	    while ( *p != '\0' && !isspace( *p )) {
		p++;
	    }
	}
	*p++ = '\0';
	argc++;
    }
    if ( argv[ 0 ] == '\0' || *argv[ 0 ] == '#' ) {
	return( NULL );
    }
    return( argv );
}

writeconf( cf )
    char	*cf;
{
    struct stat		st;
    char		*path, *p, newpath[ MAXPATHLEN ], line[ 1024 ];
    char		**argv;
    FILE		*conf, *newconf;
    struct interface	*iface;
    struct list		*l;
    int			mode = 0644, fd;

    if ( cf == NULL ) {
	path = _PATH_ATALKDCONF;
    } else {
	path = cf;
    }

    /* check if old conf is writable */
    if ( stat( path, &st ) == 0 ) {
	if (( st.st_mode & S_IWUSR ) == 0 ) {
	    syslog( LOG_INFO, "%s not writable, won't rewrite", path );
	    return( -1 );
	}
	 mode = st.st_mode;
    }

    if (( p = rindex( path, '/' )) == NULL ) {
	strcpy( newpath, _PATH_ATALKDTMP );
    } else {
	sprintf( newpath, "%.*s/%s", p - path, path, _PATH_ATALKDTMP );
    }
    if (( fd = open( newpath, O_WRONLY|O_CREAT|O_TRUNC, mode )) < 0 ) {
	syslog( LOG_ERR, "%s: %m", newpath );
	return( -1 );
    }
    if (( newconf = fdopen( fd, "w" )) == NULL ) {
	syslog( LOG_ERR, "fdreopen %s: %m", newpath );
	return( -1 );
    }

    if (( conf = fopen( path, "r" )) == NULL && cf ) {
	syslog( LOG_ERR, "%s: %m", path );
	return( -1 );
    }

    iface = interfaces->i_next;

    while ( conf == NULL || fgets( line, sizeof( line ), conf ) != NULL ) {
	if ( conf != NULL && ( argv = parseline( line )) == NULL ) {
	    if ( fputs( line, newconf ) == EOF ) {
		syslog( LOG_ERR, "fputs: %m" );
		return( -1 );
	    }
	    continue;
	}

	/* write real lines */
	if ( iface ) {
	    fprintf( newconf, "%s", iface->i_name );
	    if ( iface->i_flags & IFACE_SEED ) {
		fprintf( newconf, " -seed" );
	    }
	    fprintf( newconf, " -phase %d",
		    ( iface->i_flags & IFACE_PHASE1 ) ? 1 : 2 );
	    fprintf( newconf, " -net %d", ntohs( iface->i_rt->rt_firstnet ));
	    if ( iface->i_rt->rt_lastnet != iface->i_rt->rt_firstnet ) {
		fprintf( newconf, "-%d", ntohs( iface->i_rt->rt_lastnet ));
	    }
	    fprintf( newconf, " -addr %u.%u",
		    ntohs( iface->i_addr.sat_addr.s_net ),
		    iface->i_addr.sat_addr.s_node );
	    for ( l = iface->i_rt->rt_zt; l; l = l->l_next ) {
		fprintf( newconf, " -zone \"%.*s\"",
			((struct ziptab *)l->l_data)->zt_len,
			((struct ziptab *)l->l_data)->zt_name );
	    }
	    fprintf( newconf, "\n" );

	    iface = iface->i_next;
	    if ( conf == NULL && iface == NULL ) {
		break;
	    }
	}
    }
    if ( conf != NULL ) {
	fclose( conf );
    }
    fclose( newconf );

    if ( rename( newpath, path ) < 0 ) {
	syslog( LOG_ERR, "rename %s to %s: %m", newpath, path );
	return( -1 );
    }
    return( 0 );
}

/*
 * Read our config file. If it's not there, return -1. If it is there,
 * but has syntax errors, exit. Format of the file is as follows:
 *
 *	interface [ -seed ] [ -phase number ] [ -net net-range ]
 *	[ -addr net.node ] [ -zone zonename ]...
 * e.g.
 *	le0 -phase 1 -net 7938 -zone Argus
 * or
 *	le0 -phase 2 -net 8043-8044 -zone Argus -zone "Research Systems"
 *	le0 -phase 1 -net 7938 -zone Argus
 *
 * Pretty much everything is optional. Anything that is unspecified is
 * searched for on the network.  If -seed is not specified, the
 * configuration is assumed to be soft, i.e. it can be overridden by
 * another router. If -seed is specified, atalkd will exit if another
 * router disagrees.  If the phase is unspecified, it defaults to phase
 * 2 (the default can be overridden on the command line).  -addr can
 * replace -net, if the network in question isn't a range.  The default
 * zone for an interface is the first zone encountered for that
 * interface.
 */
readconf( cf )
    char		*cf;
{
    struct ifreq	ifr;
    struct interface	*iface, *niface;
    char		line[ 1024 ], **argv, *p;
    int			i, j, s, cc;
    FILE		*conf;

    if ( cf == NULL ) {
	p = _PATH_ATALKDCONF;
    } else {
	p = cf;
    }
    if (( conf = fopen( p, "r" )) == NULL ) {
	if ( cf ) {
	    perror( cf );
	    exit( 1 );
	} else {
	    return( -1 );
	}
    }

#ifndef __svr4__
    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	perror( "socket" );
	exit( 1 );
    }
#endif __svr4__

    while ( fgets( line, sizeof( line ), conf ) != NULL ) {
	if (( argv = parseline( line )) == NULL ) {
	    continue;
	}

#ifndef __svr4__
	/*
	 * Check that av[ 0 ] is a valid interface.
	 * Not possible under sysV.
	 */
	strcpy( ifr.ifr_name, argv[ 0 ] );
	if ( ioctl( s, SIOCGIFFLAGS, &ifr ) < 0 ) {
	    perror( argv[ 0 ] );
	    exit( 1 );
	}
	if ( ifr.ifr_flags & ( IFF_LOOPBACK | IFF_POINTOPOINT )) {
	    fprintf( stderr, "%s: can't configure.\n", ifr.ifr_name );
	    exit( 1 );
	}
#endif __svr4__

	if (( niface = newiface( argv[ 0 ] )) == NULL ) {
	    perror( "newiface" );
	    exit( 1 );
	}

	for ( i = 1; argv[ i ]; i += cc ) {
	    if ( argv[ i ][ 0 ] == '-' ) {
		argv[ i ]++;
	    }
	    for ( j = 0; j < sizeof( params ) / sizeof( params[ 0 ] ); j++ ) {
		if ( strcmp( argv[ i ], params[ j ].p_name ) == 0 ) {
		    if ( params[ j ].p_func != NULL ) {
			cc = (*params[ j ].p_func)( niface, &argv[ i + 1 ] );
			break;
		    }
		}
	    }
	    if ( j >= sizeof( params ) / sizeof( params[ 0 ] )) {
		fprintf( stderr, "%s: attribute not found.\n", argv[ i ] );
		exit( 1 );
	    }
	}

	for ( iface = interfaces; iface; iface = iface->i_next ) {
	    if ( strcmp( niface->i_name, iface->i_name ) == 0 &&
		    ((( niface->i_flags & iface->i_flags &
		    ( IFACE_PHASE1|IFACE_PHASE2 )) != 0 ) ||
		    niface->i_flags == 0 || iface->i_flags == 0 )) {
		break;
	    }
	}
	if ( iface ) {	/* Already have this interface and phase */
	    fprintf( stderr, "%s already configured!\n", niface->i_name );
	    exit( 1 );
	}

	if ( interfaces == NULL ) {
	    interfaces = niface;
	} else {
	    for ( iface = interfaces; iface->i_next; iface = iface->i_next )
		;
	    iface->i_next = niface;
	}
	niface->i_next = NULL;
    }

#ifndef __svr4__
    close( s );
#endif __svr4__
    fclose( conf );

    /*
     * Note: we've added lo0 to the interface list previously, so we must
     * have configured more than one interface...
     */
    for ( iface = interfaces, cc = 0; iface; iface = iface->i_next, cc++ )
	;
    if ( cc >= IFBASE ) {
	return( 0 );
    } else {
	return( -1 );
    }
}

/*ARGSUSED*/
seed( iface, av )
    struct interface	*iface;
    char		**av;
{
    /*
     * Check to be sure "-seed" is before "-zone".
     */
    if ( iface->i_czt ) {
	fprintf( stderr, "Must specify -seed before -zone.\n" );
	exit( 1 );
    }

    iface->i_flags |= IFACE_SEED;
    return( 1 );
}

phase( iface, av )
    struct interface	*iface;
    char		**av;
{
    int			n;
    char		*pnum;

    if (( pnum = av[ 0 ] ) == NULL ) {
	fprintf( stderr, "No phase.\n" );
	exit( 1 );
    }

    switch ( n = atoi( pnum )) {
    case 1 :
	iface->i_flags |= IFACE_PHASE1;
	break;

    case 2 :
	iface->i_flags |= IFACE_PHASE2;
	break;

    default :
	fprintf( stderr, "No phase %d.\n", n );
	exit( 1 );
    }
    return( 2 );
}

net( iface, av )
    struct interface	*iface;
    char		**av;
{
    char		*nrange;
    char		*stop;
    int			net;

    if (( nrange = av[ 0 ] ) == NULL ) {
	fprintf( stderr, "No network.\n" );
	exit( 1 );
    }

    if (( stop = index( nrange, '-' )) != 0 ) {
	stop++;
    }
    net = atoi( nrange );
    if ( net < 0 || net >= 0xffff ) {
	fprintf( stderr, "Bad network: %d\n" );
	exit( 1 );
    }

    if ( iface->i_rt == NULL && ( iface->i_rt = newrt()) == NULL ) {
	perror( "newrt" );
	exit( 1 );
    }

    if ( iface->i_flags & IFACE_PHASE1 ) {
	if ( stop != 0 ) {
	    fprintf( stderr, "Phase 1 doesn't use an address range.\n" );
	    exit( 1 );
	}
	if ( iface->i_caddr.sat_addr.s_net != ATADDR_ANYNET &&
		ntohs( iface->i_caddr.sat_addr.s_net ) != net ) {
	    fprintf( stderr, "Net-range (%u) doesn't match net %u.\n",
		    net, ntohs( iface->i_caddr.sat_addr.s_net ));
	    exit( 1 );
	}
	iface->i_rt->rt_firstnet = iface->i_rt->rt_lastnet = htons( net );
    } else if ( iface->i_flags & IFACE_PHASE2 ) {
	iface->i_rt->rt_firstnet = htons( net );
	if ( stop != 0 ) {
	    net = atoi( stop );
	    if ( net < 0 || net >= 0xffff ) {
		fprintf( stderr, "Bad network: %d\n" );
		exit( 1 );
	    }
	}
	iface->i_rt->rt_lastnet = htons( net );
	if ( iface->i_caddr.sat_addr.s_net != ATADDR_ANYNET &&
		( ntohs( iface->i_rt->rt_firstnet ) >
		ntohs( iface->i_caddr.sat_addr.s_net ) ||
		ntohs( iface->i_rt->rt_lastnet ) <
		ntohs( iface->i_caddr.sat_addr.s_net ))) {
	    fprintf( stderr, "Net-range (%u-%u) doesn't contain net (%u).\n",
		    ntohs( iface->i_rt->rt_firstnet ),
		    ntohs( iface->i_rt->rt_lastnet ),
		    ntohs( iface->i_caddr.sat_addr.s_net ));
	    exit( 1 );
	}
	if ( iface->i_rt->rt_firstnet != iface->i_rt->rt_lastnet ) {
	    iface->i_rt->rt_flags |= RTMPTAB_EXTENDED;
	}
    } else {
	fprintf( stderr, "Must specify phase before networks.\n" );
	exit( 1 );
    }
    return( 2 );
}

addr( iface, av )
    struct interface	*iface;
    char		**av;
{
    if ( av[ 0 ] == NULL ) {
	fprintf( stderr, "No address.\n" );
	exit( 1 );
    }
    if ( atalk_aton( av[ 0 ], &iface->i_caddr.sat_addr ) == 0 ) {
	fprintf( stderr, "Bad address, %s\n", av[ 0 ] );
	exit( 1 );
    }

    if ( iface->i_rt ) {
	if ( ntohs( iface->i_rt->rt_firstnet ) >
		ntohs( iface->i_caddr.sat_addr.s_net ) ||
		ntohs( iface->i_rt->rt_lastnet ) <
		ntohs( iface->i_caddr.sat_addr.s_net )) {
	    fprintf( stderr, "Net (%u) not in net-range (%u-%u).\n",
		    ntohs( iface->i_caddr.sat_addr.s_net ),
		    ntohs( iface->i_rt->rt_firstnet ),
		    ntohs( iface->i_rt->rt_lastnet ));
	    exit( 1 );
	}
    } else {
	if (( iface->i_rt = newrt()) == NULL ) {
	    perror( "newrt" );
	    exit( 1 );
	}
	iface->i_rt->rt_firstnet = iface->i_rt->rt_lastnet =
		iface->i_caddr.sat_addr.s_net;
    }

    return( 2 );
}

zone( iface, av )
    struct interface	*iface;
    char		**av;
{
    struct ziptab	*zt;
    char		*zname;

    if (( zname = av[ 0 ] ) == NULL ) {
	fprintf( stderr, "No zone.\n" );
	exit( 1 );
    }

    if ( iface->i_rt == NULL ) {
	fprintf( stderr, "Must specify net-range before zones.\n" );
	exit( 1 );
    }

    /*
     * Only process "-zone" if this interface has "-seed".  We keep our
     * list of configured zones in the interface structure.  Then we can
     * check that the network has given us good zones.
     */
    if ( iface->i_flags & IFACE_SEED ) {
	if (( zt = newzt( strlen( zname ), zname )) == NULL ) {
	    perror( "newzt" );
	    exit( 1 );
	}
	if ( iface->i_czt == NULL ) {
	    iface->i_czt = zt;
	} else {
	    zt->zt_next = iface->i_czt->zt_next;
	    iface->i_czt->zt_next = zt;
	}
    }
    return( 2 );
}

/*
 * Get the configuration from the kernel. Only called if there's no
 * configuration.
 */
getifconf()
{
    struct ifconf	ifc;
    struct ifreq	ifrs[ 64 ], *ifr, *nextifr;
    struct interface	*iface, *niface;
#ifdef BSD4_4
    struct sockaddr	*sa;
#endif BSD4_4
    int			s, ifrsize;

#ifdef __svr4__
    /*
     * Seems like under sysV, we can't find out what interfaces
     * are available.
     */

    return( -1 );
#else __svr4__

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	perror( "socket" );
	exit( 1 );
    }

    bzero( &ifc, sizeof( struct ifconf ));
    ifc.ifc_len = sizeof( ifrs );
    bzero( ifrs, sizeof( ifrs ));
    ifc.ifc_buf = (caddr_t)ifrs;
    if ( ioctl( s, SIOCGIFCONF, &ifc ) < 0 ) {
	perror( "gifconf" );
	exit( 1 );
    }

    for ( ifr = ifc.ifc_req; ifc.ifc_len >= sizeof( struct ifreq );
	    ifc.ifc_len -= ifrsize, ifr = nextifr ) {
#ifdef BSD4_4
	sa = &ifr->ifr_addr;
	ifrsize = IFNAMSIZ + sa->sa_len;
#else BSD4_4
	ifrsize = sizeof( struct ifreq );
#endif BSD4_4
	nextifr = (struct ifreq *)((caddr_t)ifr + ifrsize );

	if ( ioctl( s, SIOCGIFFLAGS, ifr ) < 0 ) {
	    perror( ifr->ifr_name );
	    exit( 1 );
	}
	if ( ifr->ifr_flags & ( IFF_LOOPBACK | IFF_POINTOPOINT )) {
	    continue;
	}

	for ( iface = interfaces; iface; iface = iface->i_next ) {
	    if ( strcmp( iface->i_name, ifr->ifr_name ) == 0 ) {
		break;
	    }
	}
	if ( iface ) {	/* Already have this interface name */
	    continue;
	}

	if (( niface = newiface( ifr->ifr_name )) == NULL ) {
	    perror( "newiface" );
	    exit( 1 );
	}
	/*
	 * Could try to get the address from the kernel...
	 */

	if ( interfaces == NULL ) {
	    interfaces = niface;
	} else {
	    for ( iface = interfaces; iface->i_next; iface = iface->i_next )
		;
	    iface->i_next = niface;
	}
	niface->i_next = NULL;
    }
    if ( ifc.ifc_len != 0 ) {
	fprintf( stderr, "Funky gifconf return.\n" );
	exit( 1 );
    }

    (void)close( s );
    return( 0 );
#endif __svr4__
}

/*
 * Allocate a new interface structure.  Centralized here so we can change
 * the interface structure and have it updated nicely.
 */
    struct interface *
newiface( name )
    char		*name;
{
    struct interface	*niface;

    if (( niface = (struct interface *)malloc( sizeof( struct interface )))
	    == NULL ) {
	return( NULL );
    }
    niface->i_next = NULL;
    strcpy( niface->i_name, name );
    niface->i_flags = 0;
    niface->i_time = 0;
#ifdef BSD4_4
    niface->i_addr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    niface->i_addr.sat_family = AF_APPLETALK;
    niface->i_addr.sat_addr.s_net = htons( ATADDR_ANYNET );
    niface->i_addr.sat_addr.s_node = ATADDR_ANYNODE;
#ifdef BSD4_4
    niface->i_caddr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    niface->i_caddr.sat_family = AF_APPLETALK;
    niface->i_caddr.sat_addr.s_net = htons( ATADDR_ANYNET );
    niface->i_caddr.sat_addr.s_node = ATADDR_ANYNODE;
    niface->i_rt = NULL;
    niface->i_gate = NULL;
    niface->i_ports = NULL;

    return( niface );
}

#ifdef __svr4__
    int
plumb()
{
    struct interface	*iface;
    char		device[ MAXPATHLEN ], *p;
    struct {		/* this is a little fucked */
	u_long	ppa;
	u_long	sap;
	u_char	testaddr[ 6 ];
    }			netif;
    int			fd, ppa;

    for ( iface = interfaces; iface != NULL; iface = iface->i_next ) {
	if ( strcmp( iface->i_name, LOOPIFACE ) == 0 ) {
	    continue;
	}

	strcpy( device, "/dev/" );
	strcat( device, iface->i_name );
	if (( p = strpbrk( device, "0123456789" )) == NULL ) {
	    syslog( LOG_ERR, "plumb: invalid device: %s", device );
	    exit( 1 );
	}
	ppa = atoi( p );
	*p = '\0';

	if (( fd = open( device, O_RDWR, 0 )) < 0 ) {
	    syslog( LOG_ERR, "%s: %m", device );
	    exit( 1 );
	}
	if ( ioctl( fd, I_PUSH, "ddp" ) < 0 ) {
	    syslog( LOG_ERR, "I_PUSH: %m" );
	    exit( 1 );
	}
	netif.ppa = ppa;
	netif.sap = 1500;
	if ( ioctl( fd, IF_UNITSEL, &netif ) < 0 ) {
	    syslog( LOG_ERR, "IF_UNITSEL: %m" );
	    exit( 1 );
	}

	syslog( LOG_INFO, "plumbed %s%d", device, ppa );
    }

    return( 0 );
}
#endif __svr4__
