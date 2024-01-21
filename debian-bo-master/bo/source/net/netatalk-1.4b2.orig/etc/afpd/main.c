/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/uio.h>
#if defined( sun ) && defined( __svr4__ )
#include </usr/ucbinclude/sys/file.h>
#else sun __svr4__
#include <sys/file.h>
#endif sun __svr4__
#include <sys/syslog.h>
#include <sys/time.h>

#include <fcntl.h>
#include <termios.h>
#include <signal.h>
#include <stdio.h>
#include <strings.h>
#include <netdb.h>
#include <unistd.h>

#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <atalk/afp.h>
#include <atalk/adouble.h>
#include <atalk/paths.h>

#include "switch.h"
#include "icon.h"
#include "globals.h"
#include "fork.h"

#ifdef ATP_MAXDATA
#undef ATP_MAXDATA
#endif
#define ATP_MAXDATA		578

char	replybuf[ ATP_MAXDATA * 8 ];			/* 8 atp packets */
int	debug = 0;
int	uservolfirst = 0;
int	nologin = 0;
int	connections = 5;
char	*defaultvol = _PATH_AFPDDEFVOL;
char	*systemvol = _PATH_AFPDSYSVOL;
char	*guest = "nobody";
char	*Obj, *Type = "AFPServer", *Zone = "*";
char	*version = VERSION;
ASP	child;

extern struct oforks	*writtenfork;

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
afp_goaway( sig )
    int		sig;
{
    syslog( LOG_INFO, "shutting down on signal %d", sig );
    switch( sig ) {
    case SIGTERM :
	if ( asp_attentionall( htons((short)0x8000 )) < 0 ) {
	    syslog( LOG_ERR, "afp_goaway: asp_attentionall: %m" );
	}
	break;
    case SIGHUP :
	nologin = 1;
	if ( asp_attentionall( htons((short)0x800A )) < 0 ) {
	    syslog( LOG_ERR, "afp_goaway: asp_attentionall: %m" );
	}
	break;
    default :
	syslog( LOG_ERR, "afp_goaway: bad signal" );
    }
    if ( asp_kill( sig ) < 0 ) {
	syslog( LOG_ERR, "afp_goaway: asp_kill: %m" );
    }
    if ( sig == SIGTERM ) {
	if ( nbp_unrgstr( Obj, Type, Zone ) < 0 ) {
	    syslog( LOG_ERR, "afp_goaway: nbp_unrgstr: %m" );
	}
	exit( 0 );
    }
    return;
}

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
afp_die()
{
    syslog( LOG_DEBUG, "afp_die: child %X", child );
    if ( asp_shutdown( child ) < 0 ) {
	syslog( LOG_ERR, "afp_die: asp_shutdown: %m" );
    }

    /*
     * During shutdown, we can receive a TERM before our parent.  Sleep
     * a little bit, so the parent has a chance to get its TERM.
     */
    sleep( 10 );
    exit( 0 );
}

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
afp_timedown()
{
    struct sigaction	sv;
    struct itimerval	it;

    it.it_interval.tv_sec = 0;
    it.it_interval.tv_usec = 0;
    it.it_value.tv_sec = 600;
    it.it_value.tv_usec = 0;
    if ( setitimer( ITIMER_REAL, &it, 0 ) < 0 ) {
	syslog( LOG_ERR, "afp_timedown: setitimer: %m" );
	exit( 1 );
    }
    sv.sa_handler = afp_die;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGALRM, &sv, 0 ) < 0 ) {
	syslog( LOG_ERR, "afp_timedown: sigaction: %m" );
	exit( 1 );
    }
}

main( ac, av )
    int		ac;
    char	**av;
{
    ATP			atp;
    ASP			asp;
    struct sigaction	sv;
    char		*p, *buf, data[ ATP_MAXDATA ], *status;
    int			c, buflen, func, rbuflen, ccnt = 0, err = 0, pidfd;
    char		hostname[ MAXHOSTNAMELEN ];
    char		*server = 0, *pidfile = NULL;
    extern char		*optarg;
    extern int		optind;

    umask( 0 );		/* so inherited file permissions work right */
    if ( gethostname( hostname, sizeof( hostname )) < 0 ) {
	perror( "gethostname" );
	exit( 1 );
    }
    if (( p = index( hostname, '.' )) != 0 ) {
	*p = '\0';
    }

    if (( p = rindex( av[ 0 ], '/' )) == NULL ) {
	p = av[ 0 ];
    } else {
	p++;
    }

    while (( c = getopt( ac, av, "dn:f:s:uc:g:ACKGP:" )) != EOF ) {
	switch ( c ) {
	case 'd' :
	    debug++;
	    break;
	case 'n' :
	    server = optarg;
	    break;
	case 'f' :
	    defaultvol = optarg;
	    break;
	case 's' :
	    systemvol = optarg;
	    break;
	case 'u' :
	    uservolfirst++;
	    break;
	case 'c' :
	    connections = atoi( optarg );
	    break;
	case 'g' :
	    guest = optarg;
	    break;

	/* Note that the following must match the afp_uams table */
	case 'A' :
	    uam_off( "AFS Kerberos" );
	    break;
	case 'C' :
	    uam_off( "Cleartxt Passwrd" );
	    break;
	case 'K' :
	    uam_off( "Kerberos IV" );
	    break;
	case 'G' :
	    uam_off( "No User Authent" );
	    break;

	case 'P' :
	    pidfile = optarg;
	    break;

	default :
	    err++;
	}
    }
    if ( err || optind != ac ) {
	fprintf( stderr,
		"Usage:\t%s [ -dGKCA ] [ -n nbpname ] [ -f defvols ]\n",  p );
	fprintf( stderr,
		"\t[ -s sysvols ] [ -u ] [ -c maxconn ] [ -g guest ]\n" );
	exit( 1 );
    }

    if ( pidfile ) {
	extern int	errno;

	if (( pidfd = open( pidfile, O_CREAT | O_WRONLY, 0644 )) < 0 ) {
	    perror( pidfile );
	    exit( 1 );
	}
	if ( flock( pidfd, LOCK_EX | LOCK_NB ) < 0 ) {
	    if ( errno == EWOULDBLOCK ) {
		fprintf( stderr, "afpd is already running\n" );
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
     * Disassociate from controlling tty.
     */
    if ( !debug ) {
	int		i, dt;

	switch ( fork()) {
	case 0 :
	    dt = getdtablesize();
	    for ( i = 0; i < dt; i++ ) {
		if ( pidfile && i != pidfd ) {
		    (void)close( i );
		}
	    }
	    if (( i = open( "/dev/tty", O_RDWR )) >= 0 ) {
		(void)ioctl( i, TIOCNOTTY, 0 );
		setpgid( 0, getpid());
		(void)close( i );
	    }
	    break;
	case -1 :
	    perror( "fork" );
	    exit( 1 );
	default :
	    exit( 0 );
	}
    }

    if ( pidfile ) {
	FILE		*pf;

	if (( pf = fdopen( pidfd, "w" )) == NULL ) {
	    fprintf( stderr, "Can't fdopen pidfile!\n" );
	    exit( 1 );
	}
	fprintf( pf, "%d\n", getpid());
	fflush( pf );
    }

#ifdef ultrix
    openlog( p, LOG_PID );
#else ultrix
    openlog( p, LOG_NDELAY|LOG_PID, LOG_DAEMON );
#endif ultrix

    if (( status = (char *)malloc( ATP_MAXDATA )) == NULL ) {
	syslog( LOG_ERR, "main: malloc: %m" );
	exit( 1 );
    }

    /*
     * This must be before status_server(), so we can get the name correct.
     */
    Obj = hostname;
    if ( nbp_name( server, &Obj, &Type, &Zone )) {
	syslog( LOG_ERR, "main: can't parse %s", server );
	exit( 1 );
    }

    /*
     * These routines must be called in order -- earlier calls
     * set the offsets for later calls.
     */
    status_flags( status );
    status_server( status, Obj );
    status_machine( status );
    status_versions( status );
    status_uams( status );
    c = status_icon( status );

    if (( atp = atp_open( 0 )) == NULL ) {
	syslog( LOG_ERR, "main: atp_open: %m" );
	exit( 1 );
    }
    if (( asp = asp_init( atp )) == NULL ) {
	syslog( LOG_ERR, "main: asp_init: %m" );
	exit( 1 );
    }

    asp_setstatus( asp, status, c );

    if ( nbp_rgstr( atp_sockaddr( atp ), Obj, Type, Zone ) < 0 ) {
	syslog( LOG_ERR, "Can't register %s:%s@%s", Obj, Type, Zone );
	exit( 1 );
    }
    syslog( LOG_INFO, "%s:%s@%s started on %u.%u:%u (%s)", Obj, Type, Zone,
	    ntohs( atp_sockaddr( atp )->sat_addr.s_net ),
	    atp_sockaddr( atp )->sat_addr.s_node,
	    atp_sockaddr( atp )->sat_port, version );

    sv.sa_handler = afp_goaway;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGHUP, &sv, 0 ) < 0 ) {
	syslog( LOG_ERR, "main: sigaction: %m" );
	exit( 1 );
    }
    if ( sigaction( SIGTERM, &sv, 0 ) < 0 ) {
	syslog( LOG_ERR, "main: sigaction: %m" );
	exit( 1 );
    }

    if (( child = asp_getsession( asp, connections )) == NULL ) {
	syslog( LOG_ERR, "main: asp_getsession: %m" );
	exit( 1 );
    }

    sv.sa_handler = afp_timedown;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGHUP, &sv, 0 ) < 0 ) {
	syslog( LOG_ERR, "main: sigaction: %m" );
	exit( 1 );
    }

    sv.sa_handler = afp_die;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGTERM, &sv, 0 ) < 0 ) {
	syslog( LOG_ERR, "main: sigaction: %m" );
	exit( 1 );
    }

    syslog( LOG_INFO, "session from %u.%u:%u on %u.%u:%u",
	    ntohs( child->asp_sat.sat_addr.s_net ),
	    child->asp_sat.sat_addr.s_node, child->asp_sat.sat_port,
	    ntohs( atp_sockaddr( child->asp_atp )->sat_addr.s_net ),
	    atp_sockaddr( child->asp_atp )->sat_addr.s_node,
	    atp_sockaddr( child->asp_atp )->sat_port );

    for (;;) {
	buf = data;
	buflen = sizeof( data );
	switch (( c = asp_getrequest( child, &buf, &buflen ))) {
	case ASPFUNC_CLOSE :
	    asp_close( child );
	    syslog( LOG_INFO, "done" );
	    if ( debug ) {
		printf( "done\n" );
	    }
	    exit( 0 );
	    break;
	case ASPFUNC_CMD :
#ifdef AFS
	    if ( writtenfork ) {
		if ( flushfork( writtenfork ) < 0 ) {
		    syslog( LOG_ERR, "main flushfork: %m" );
		}
		writtenfork = NULL;
	    }
#endif AFS
	    func = (u_char)buf[ 0 ];
	    if ( debug ) {
		printf( "command: %d\n", func );
		bprint( buf, buflen );
	    }
	    if ( afp_switch[ func ] != NULL ) {
		/*
		 * The function called from afp_switch is expected to
		 * read it's parameters out of buf, put it's
		 * results in replybuf (updating rbuflen), and
		 * return an error code.
		 */
		rbuflen = sizeof( replybuf );
		c = (*afp_switch[ func ])( buf, buflen, replybuf, &rbuflen );
	    } else {
		syslog( LOG_ERR, "bad function %X", func );
		rbuflen = 0;
		c = AFPERR_NOOP;
	    }
	    if ( debug ) {
		printf( "reply: %d, %d\n", c, ccnt++ );
		bprint( replybuf, rbuflen );
	    }
	    if ( asp_cmdreply( child, htonl( c ), replybuf, rbuflen ) < 0 ) {
		syslog( LOG_ERR, "asp_cmdreply: %m" );
		exit( 1 );
	    }
	    break;
	case ASPFUNC_WRITE :
	    func = (u_char)buf[ 0 ];
	    if ( debug ) {
		printf( "(write) command: %d\n", func );
		bprint( buf, buflen );
	    }
	    if ( afp_switch[ func ] != NULL ) {
		rbuflen = sizeof( replybuf );
		c = (*afp_switch[ func ])( buf, buflen, replybuf, &rbuflen,
			child );
	    } else {
		syslog( LOG_ERR, "(write) bad function %X", func );
		rbuflen = 0;
		c = AFPERR_NOOP;
	    }
	    if ( debug ) {
		printf( "(write) reply code: %d, %d\n", c, ccnt++ );
		bprint( replybuf, rbuflen );
	    }
	    if ( asp_wrtreply( child, htonl( c ), replybuf, rbuflen ) < 0 ) {
		syslog( LOG_ERR, "asp_wrtreply: %m" );
		exit( 1 );
	    }
	    break;
	default:
	    /*
	     * Bad asp packet.  Probably should have asp filter them,
	     * since they are typically things like out-of-order packet.
	     */
	    syslog( LOG_INFO, "main: asp_getrequest: %d", c );
	    break;
	}
	if ( debug ) {
#ifdef notdef
	    pdesc( stdout );
#endif notdef
	    pforkdesc( stdout );
	    fflush( stdout );
	}
    }
}

status_server( data, server )
    char	*data, *server;
{
    struct afp_status	*status;
    int			len;

    status = (struct afp_status *)data;
    data += sizeof( struct afp_status );
    len = strlen( server );
    *data++ = len;
    bcopy( server, data, len );
    data += len;
    status->as_machoff = htons( data - (char *)status );
}

status_machine( data )
    char	*data;
{
    struct afp_status	*status;
    int			len;
#ifdef AFS
    char		*machine = "afs";
#else !AFS
    char		*machine = "unix";
#endif

    status = (struct afp_status *)data;
    data += ntohs( status->as_machoff );
    len = strlen( machine );
    *data++ = len;
    bcopy( machine, data, len );
    data += len;
    status->as_versoff = htons( data - (char *)status );
}

status_icon( data )
    char	*data;
{
    struct afp_status	*status;
    int			ret;

    status = (struct afp_status *)data;
    if ( icon == NULL ) {
	ret = ntohs( status->as_iconoff );
	status->as_iconoff = 0;
	return( ret );
    } else {
	data += ntohs( status->as_iconoff );
	bcopy( icon, data, sizeof( icon ));
	data += sizeof( icon );
	return( data - (char *)status );
    }
}

status_flags( data )
    char	*data;
{
    struct afp_status	*status;

    status = (struct afp_status *)data;

    status->as_flags = AFPSRVRINFO_COPY;
#if defined( AFS ) && defined( UAM_AFSKRB )
    status->as_flags |= AFPSRVRINFO_PASSWD;
#endif AFS UAM_AFSKRB
    status->as_flags |= AFPSRVRINFO_FASTBOZO;
    status->as_flags = htons( status->as_flags );
}

#ifdef notdef
pdesc( f )
    FILE	*f;
{
    int		i, o, d;
    extern int	errno;

    d = getdtablesize();

    for ( i = 0; i < d; i++ ) {
	if ( ioctl( i, FIOGETOWN, &o ) < 0 ) {
	    if ( errno == EBADF ) {
		fputs( "0", f );
	    } else {
		fputs( "1", f );
	    }
	} else {
	    fputs( "1", f );
	}
    }
    fputs( "\n", f );
    fflush( f );
}
#endif notdef
