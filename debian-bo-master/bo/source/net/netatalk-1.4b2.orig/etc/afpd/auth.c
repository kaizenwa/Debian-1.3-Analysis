/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/syslog.h>
#include <sys/time.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/afp.h>
#include <atalk/paths.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <limits.h>
#include <strings.h>
#include <string.h>
#include <ctype.h>
#include <pwd.h>

#ifdef SOLARIS
#define SHADOWPW
#endif SOLARIS

#ifdef SHADOWPW
#include <shadow.h>
#endif SHADOWPW

#include "auth.h"
#include "globals.h"
#include "switch.h"

#if defined( KRB ) || defined( AFS ) || defined( UAM_AFSKRB )
#include <netinet/in.h>
#endif KRB AFS UAM_AFSKRB

#if defined( KRB ) || defined( UAM_AFSKRB )
#include <krb.h>
#include <des.h>
#include <prot.h>

C_Block			seskey;
Key_schedule		seskeysched;
static char		realm[ REALM_SZ ];
#endif KRB UAM_AFSKRB

#ifdef UAM_AFSKRB
static int		validseskey = 0;
static int		logged = 0;
static char		*tktfile;
static char		instance[ INST_SZ ], name[ ANAME_SZ ];
#endif UAM_AFSKRB

#ifdef AFS
#include <afs/stds.h>
#include <rx/rxkad.h>
#include <afs/afs.h>
#include <afs/venus.h>
#include <afs/afsint.h>

char *ka_LocalCell();

struct ClearToken {
    long AuthHandle;
    char HandShakeKey[8];
    long ViceId;
    long BeginTimestamp;
    long EndTimestamp;
};
#endif AFS

int	afp_version = 11;
uid_t	uuid;
#if defined( __svr4__ ) && !defined( NGROUPS )
#define NGROUPS NGROUPS_MAX
#endif __svr4__ NGROUPS
#if defined( sun ) && !defined( __svr4__ ) || defined( ultrix )
int	groups[ NGROUPS ];
#else sun __svr4__ ultrix
#if defined( __svr4__ ) && !defined( NGROUPS )
#define NGROUPS	NGROUPS_MAX
#endif __svr4__ NGROUPS
gid_t	groups[ NGROUPS ];
#endif sun ultrix
int	ngroups;
char	*username = NULL;
char	*mktemp();

/*
 * These numbers are scattered throughout the code.
 */
struct afp_versions	afp_versions[] = {
    { "AFPVersion 1.1",	11 },
    { "AFPVersion 2.0",	20 },
};

/* Note that these names must match those in main() */
struct afp_uams		afp_uams[] = {
#ifdef KRB
    { "Kerberos IV",		krb4_login,	krb4_logincont,		0 },
#endif KRB
    { "Cleartxt Passwrd",	clrtxt_login,	NULL,			0 },
#ifdef UAM_AFSKRB
    { "AFS Kerberos",		afskrb_login,	afskrb_logincont,	0 },
#endif UAM_AFSKRB
    { "No User Authent",	noauth_login,	NULL,			0 },
};
struct afp_uams		*afp_uam = NULL;

/*
 * Mark a UAM as off.
 */
uam_off( uam )
    char	*uam;
{
    int		i;

    for ( i = 0; i < sizeof( afp_uams ) / sizeof( afp_uams[ 0 ] ); i++ ) {
	if ( strcmp( uam, afp_uams[ i ].au_name ) == 0 ) {
	    afp_uams[ i ].au_flags |= AU_OFF;
	    return;
	}
    }
    return;
}

status_versions( data )
    char	*data;
{
    struct afp_status	*status;
    int			len, num, i;

    status = (struct afp_status *)data;
    num = sizeof( afp_versions ) / sizeof( afp_versions[ 0 ] );
    data += ntohs( status->as_versoff );
    *data++ = num;
    for ( i = 0; i < num; i++ ) {
	len = strlen( afp_versions[ i ].av_name );
	*data++ = len;
	bcopy( afp_versions[ i ].av_name , data, len );
	data += len;
    }
    status->as_uamsoff = htons( data - (char *)status );
}

status_uams( data )
    char	*data;
{
    struct afp_status	*status;
    int			len, num, i;

    status = (struct afp_status *)data;
    for ( num = i = 0; i < sizeof( afp_uams ) / sizeof( afp_uams[ 0 ] ); i++ ) {
	if (( afp_uams[ i ].au_flags & AU_OFF ) == 0 ) {
	    num++;
	}
    }
    data += ntohs( status->as_uamsoff );
    *data++ = num;
    for ( i = 0; i < sizeof( afp_uams ) / sizeof( afp_uams[ 0 ] ); i++ ) {
	if (( afp_uams[ i ].au_flags & AU_OFF ) == 0 ) {
	    len = strlen( afp_uams[ i ].au_name );
	    *data++ = len;
	    bcopy( afp_uams[ i ].au_name, data, len );
	    data += len;
	}
    }
    status->as_iconoff = htons( data - (char *)status );
}

afp_login( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    int		len, i, num;

    if ( nologin ) {
	*rbuflen = 0;
	return( AFPERR_SHUTDOWN );
    }

    ibuf++;
    ibuflen--;
    len = *ibuf++;
    ibuflen--;
    num = sizeof( afp_versions ) / sizeof( afp_versions[ 0 ]);
    for ( i = 0; i < num; i++ ) {
	if ( strncmp( ibuf, afp_versions[ i ].av_name , len ) == 0 ) {
	    afp_version = afp_versions[ i ].av_number;
	    break;
	}
    }
    if ( i == num ) {				/* An inappropo version */
	*rbuflen = 0;
	return( AFPERR_BADVERS );
    }
    ibuf += len;
    ibuflen -= len;

    len = *ibuf++;
    ibuflen--;
    num = sizeof( afp_uams ) / sizeof( afp_uams[ 0 ]);
    for ( i = 0; i < num; i++ ) {
	if ( afp_uams[ i ].au_flags & AU_OFF ) {
	    continue;
	}
	if ( strndiacasecmp( ibuf, afp_uams[ i ].au_name, len ) == 0 ) {
	    afp_uam = &afp_uams[ i ];
	    break;
	}
    }
    if ( i == num ) {
	*rbuflen = 0;
	return( AFPERR_BADUAM );
    }
    ibuf += len;
    ibuflen -= len;

    *rbuflen = 0;
    return( afp_uam->au_login( ibuf, ibuflen, rbuf, rbuflen ));
}

afp_logincont( ibuf, ibuflen, rbuf, rbuflen, asp )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
    ASP		asp;
{
    if ( afp_uam == NULL || afp_uam->au_logincont == NULL ) {
	*rbuflen = 0;
	return( AFPERR_NOTAUTH );
    }
    return( afp_uam->au_logincont( ibuf, ibuflen, rbuf, rbuflen, asp ));
}

noauth_login( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct passwd	*pwent;

    *rbuflen = 0;
    syslog( LOG_INFO, "login noauth" );

    /* this shit should be done by login() XXX */
    if (( pwent = getpwnam( guest )) == NULL ) {
	syslog( LOG_ERR, "noauth_login: getpwname( %s ): %m", guest );
	return( AFPERR_BADUAM );
    }

    if ( setgid( pwent->pw_gid ) < 0 || setuid( pwent->pw_uid ) < 0 ) {
	syslog( LOG_ERR, "noauth_login: setreugid: %m" );
	return( AFPERR_BADUAM );
    }

    uuid = pwent->pw_uid;
    ngroups = 0;

#ifdef AFS
    if ( setpag() < 0 ) {
	syslog( LOG_ERR, "noauth_login: setpag: %m" );
	return( AFPERR_BADUAM );
    }
#endif AFS
    afp_switch = postauth_switch;
    return( AFP_OK );
}

login( name, uid, gid )
    char	*name;
    uid_t	uid;
    gid_t	gid;
{
    if ( uid == 0 ) {	/* don't allow root login */
	syslog( LOG_ERR, "login: root login denied!" );
	return( AFPERR_NOTAUTH );
    }

    syslog( LOG_INFO, "login %s (uid %d, gid %d)", name, uid, gid );
    if ( initgroups( name, gid ) < 0 || setgid( gid ) < 0 ||
	    setuid( uid ) < 0 ) {
	syslog( LOG_ERR, "login: %m" );
	return( AFPERR_BADUAM );
    }

    if (( ngroups = getgroups( NGROUPS, groups )) < 0 ) {
	syslog( LOG_ERR, "login: getgroups: %m" );
	return( AFPERR_BADUAM );
    }
    uuid = uid;

    afp_switch = postauth_switch;
    return( AFP_OK );
}

lcase( p )
    char	*p;
{
    for (; *p; p++ ) {
	if ( isupper( *p )) {
	    *p = tolower( *p );
	}
    }
    return;
}

ucase( p )
    char	*p;
{
    for (; *p; p++ ) {
	if ( islower( *p )) {
	    *p = toupper( *p );
	}
    }
    return;
}

#ifdef KRB

#define KRB4CMD_HELO	1
#define KRB4RPL_REALM	2
#define KRB4WRT_SESS	3
#define KRB4RPL_DONE	4
#define KRB4RPL_PRINC	5
#define KRB4WRT_TOKEN	6
#define KRB4WRT_SKIP	7
#define KRB4RPL_DONEMUT	8


krb4_login( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    char		*p;
    int			len;

    if ( *ibuf != KRB4CMD_HELO ) {
	*rbuflen = 0;
	syslog( LOG_INFO, "krb4_login: bad command %d", *ibuf );
	return( AFPERR_NOTAUTH );
    }

    p = rbuf;
    if ( krb_get_lrealm( realm, 1 ) != KSUCCESS ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "krb4_login: can't get local realm!" );
	return( AFPERR_NOTAUTH );
    }

    *p++ = KRB4RPL_REALM;
    *p++ = 1;
    len = strlen( realm );
    *p++ = len;
    strcpy( p, realm );
    p += len + 1;

#ifdef AFS
    if ( setpag() < 0 ) {
	*rbuflen = 0;
	syslog( LOG_ERR, "krb_login: setpag: %m" );
	return( AFPERR_BADUAM );
    }
#endif AFS

    *rbuflen = p - rbuf;
    return( AFPERR_AUTHCONT );
}

krb4_logincont( ibuf, ibuflen, rbuf, rbuflen, asp )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
    ASP		asp;
{
    struct passwd	*pwd;
    KTEXT_ST		tkt;
    static AUTH_DAT	ad;
    int			rc;
    short		len;
    char		*p;
    CREDENTIALS		cr;
#ifdef AFS
    struct ViceIoctl	vi;
    struct ClearToken	ct;
#endif AFS
    char		buf[ 1024 ];
    int			aint;

    if ( asp_wrtcont( asp, rbuf, rbuflen ) < 0 ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }

    p = rbuf;

    switch ( rc = *p++ ) {
    case KRB4WRT_SESS :
	bcopy( p, &len, sizeof( short ));
	tkt.length = ntohs( len );
	p += sizeof( short );

	if ( tkt.length <= 0 || tkt.length > MAX_KTXT_LEN ) {
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}
	bcopy( p, tkt.dat, tkt.length );
	p += tkt.length;

	if (( rc = krb_rd_req( &tkt, "afpserver", Obj, 0, &ad, "" ))
		!= RD_AP_OK ) {
	    syslog( LOG_ERR, "krb4_logincont: krb_rd_req: %s",
		    krb_err_txt[ rc ] );
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}

	syslog( LOG_INFO, "krb4_login: %s.%s@%s", ad.pname, ad.pinst,
		ad.prealm );
	bcopy( ad.session, seskey, sizeof( C_Block ));
	key_sched( seskey, seskeysched );

	username = ad.pname;
	p = rbuf;

#ifndef AFS
	*p = KRB4RPL_DONE;	/* XXX */
	*rbuflen = 1;

	if (( pwd = getpwnam( ad.pname )) == NULL ) {
	    return( AFPERR_NOTAUTH );
	}
	return( login( pwd->pw_name, pwd->pw_uid, pwd->pw_gid ));
#else AFS
	/* get principals */
	*p++ = KRB4RPL_PRINC;
	len = strlen( realm );
	*p++ = len + 1;
	*p++ = '@';
	strcpy( p, realm );
	p += len + 1;
	*rbuflen = p - rbuf;
	return( AFPERR_AUTHCONT );

    case KRB4WRT_TOKEN :
	bcopy( p, &len, sizeof( short ));
	len = ntohs( len );
	p += sizeof( short );
	bcopy( p, &cr, len );

	pcbc_encrypt((C_Block *)&cr, (C_Block *)&cr, len, seskeysched,
		seskey, DES_DECRYPT );

	p = buf;
	cr.ticket_st.length = ntohl( cr.ticket_st.length );
	bcopy( &cr.ticket_st.length, p, sizeof( int ));
	p += sizeof( int );
	bcopy( cr.ticket_st.dat, p, cr.ticket_st.length );
	p += cr.ticket_st.length;

	ct.AuthHandle = ntohl( cr.kvno );
	bcopy( cr.session, ct.HandShakeKey, sizeof( cr.session ));
	ct.ViceId = 0;
	ct.BeginTimestamp = ntohl( cr.issue_date );
	ct.EndTimestamp = krb_life_to_time( ntohl( cr.issue_date ),
		ntohl( cr.lifetime ));

	aint = sizeof( struct ClearToken );
	bcopy( &aint, p, sizeof( int ));
	p += sizeof( int );
	bcopy( &ct, p, sizeof( struct ClearToken ));
	p += sizeof( struct ClearToken );

	aint = 0;
	bcopy( &aint, p, sizeof( int ));
	p += sizeof( int );

	lcase( realm );
	strcpy( p, realm );
	p += strlen( realm ) + 1;

	vi.in = buf;
	vi.in_size = p - buf;
	vi.out = buf;
	vi.out_size = sizeof( buf );
	if ( pioctl( 0, VIOCSETTOK, &vi, 0 ) < 0 ) {
	    syslog( LOG_ERR, "krb4_logincont: pioctl: %m" );
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}
	/* FALL THROUGH */

    case KRB4WRT_SKIP :
	p = rbuf;
	*p = KRB4RPL_DONE;	/* XXX */
	*rbuflen = 1;

	if (( pwd = getpwnam( ad.pname )) == NULL ) {
	    return( AFPERR_NOTAUTH );
	}
	return( login( pwd->pw_name, pwd->pw_uid, pwd->pw_gid ));
#endif AFS

    default :
	syslog( LOG_INFO, "krb4_logincont: bad command %d", rc );
	*rbuflen = 0;
	return( AFPERR_NOTAUTH );
	break;
    }
}

#endif KRB

extern char	*crypt();

static char	clrtxtname[ 31 ];

clrtxt_login( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    struct passwd	*pwd;
#ifdef SHADOWPW
    struct spwd		*sp;
#endif SHADOWPW
    int			len;
    char		*p, *getusershell();

    *rbuflen = 0;

    len = *ibuf++;
    if ( len > 31 ) {
	return( AFPERR_PARAM );
    }
    bcopy( ibuf, clrtxtname, len );
    ibuf += len;
    clrtxtname[ len ] = '\0';
    username = clrtxtname;
    if (( pwd = getpwnam( clrtxtname )) == NULL ) {
	return( AFPERR_NOTAUTH );
    }

    if ( pwd->pw_shell != NULL && pwd->pw_shell[ 0 ] != '\0' ) {
	while (( p = getusershell()) != NULL ) {
	    if ( strcmp( p, pwd->pw_shell ) == 0 ) {
		break;
	    }
	}
	endusershell();
	if ( p == NULL ) {
	    syslog( LOG_INFO, "illegal shell %s for %s",
		    pwd->pw_shell, clrtxtname );
	    return( AFPERR_NOTAUTH );
	}
    }

#ifdef SHADOWPW
    if (( sp = getspnam( clrtxtname )) == NULL ) {
	syslog( LOG_INFO, "no shadow passwd entry for %s", clrtxtname );
	return( AFPERR_NOTAUTH );
    }
    pwd->pw_passwd = sp->sp_pwdp;
#endif SHADOWPW

    if ( pwd->pw_passwd != NULL ) {
	if ( *ibuf == '\0' ) {
	    ++ibuf;
	}
	ibuf[ 8 ] = '\0';
#ifdef AFS
	if ( kcheckuser( pwd, ibuf ) == 0 ) {
	    return( login( pwd->pw_name, pwd->pw_uid, pwd->pw_gid ));
	}
#endif AFS
	p = crypt( ibuf, pwd->pw_passwd );
	if ( strcmp( p, pwd->pw_passwd ) == 0 ) {
	    return( login( pwd->pw_name, pwd->pw_uid, pwd->pw_gid ));
	}
    }
    return( AFPERR_NOTAUTH );
}

#ifdef AFS
#include <rx/rxkad.h>
#include <afs/afsint.h>

char *ka_LocalCell();

void
addrealm(realm,cells)
    char *realm;
	char ***cells;
{
    char **ptr;
	int temp;

	ptr= *cells;

    for(;*ptr != 0 ;ptr++)
        if(!strcmp(realm,*ptr))
            return;

	temp=ptr- *cells;
	*cells=(char**)realloc(*cells,((2+temp)*sizeof(char*)));
	ptr= *cells+temp;

    *ptr=(char*)malloc(strlen(realm)+1);
    strcpy(*ptr++,realm);
	*ptr=0;
    return;
}

int kcheckuser(pwd,passwd)
	struct passwd *pwd;
	char *passwd;
{
	long code;
	char *instance="";
	char realm[MAXKTCREALMLEN];
	char lorealm[MAXKTCREALMLEN];
	char *cell;
	Date lifetime=MAXKTCTICKETLIFETIME;
	int rval;
	char **cells=(char **)malloc(sizeof(char*));
	char *temp;
	int rc,cellNum;
	struct ktc_principal serviceName;

	*cells=0;

	code = ka_Init(0);

	{
		char *temp,*temp1;
		temp=(char*)malloc(strlen(pwd->pw_dir)+1);
		strcpy(temp,pwd->pw_dir);
		temp1=temp;
		temp=strtok(temp,"/");
		temp=strtok('\0',"/");
		ka_CellToRealm(temp,realm,0);
		addrealm(realm,&cells);
		free(temp1);
	}

	setpag();
	authenticate(cells,pwd->pw_name,passwd);
	cellNum=0;
	rc=ktc_ListTokens(cellNum,&cellNum,&serviceName);
	if(rc)
		rval=1;
	else{
		rval=0;
	}

	return(rval);
}

authenticate(cells,name,passwd)
	char **cells;
	char *name;
	char *passwd;
{
	char **ptr=cells;
	char *errorstring;

	while(*ptr){
	    ka_UserAuthenticate(name,/*instance*/"",/*cell*/*ptr++,
		    passwd,/*setpag*/0,&errorstring);
	}
}
#endif AFS

#if defined( UAM_AFSKRB ) && defined( AFS )
afskrb_login( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    KTEXT_ST	authent, rpkt;
    CREDENTIALS	cr;
    char	*p, *q;
    int		len, rc, whoserealm;
    short	slen;

    len = *ibuf++;
    ibuf[ len ] = '\0';
    if (( p = index( ibuf, '@' )) != NULL ) {
	*p++ = '\0';
	strcpy( realm, p );
	ucase( realm );
	whoserealm = 0;
    } else {
	if ( krb_get_lrealm( realm, 1 ) != KSUCCESS ) {
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}
	whoserealm = 1;
    }
    if (( p = index( ibuf, '.' )) != NULL ) {
	*p++ = '\0';
	strcpy( instance, p );
    } else {
	*instance = '\0';
    }
    strcpy( name, ibuf );
    /*
     * We don't have the session key, yet. Get one.
     */
    p = rbuf;
    if ( validseskey == 0 ) {
	if ( setpag() < 0 ) {
	    syslog( LOG_ERR, "krb_login: setpag: %m" );
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}
	krb_set_tkt_string(( tktfile = mktemp( _PATH_AFPTKT )));
	if (( rc =  krb_get_svc_in_tkt( "afpserver", Obj, realm,
		TICKET_GRANTING_TICKET, realm, 255, KEYFILE )) != INTK_OK ) {
	    *rbuflen = 0;
	    syslog( LOG_ERR, "krb_login: can't get ticket-granting-ticket" );
	    return(( whoserealm ) ? AFPERR_BADUAM : AFPERR_PARAM );
	}
	if ( krb_mk_req( &authent, name, instance, realm, 0 ) != KSUCCESS ) {
	    *rbuflen = 0;
	    return( AFPERR_PARAM );
	}
	if ( krb_get_cred( name, instance, realm, &cr ) != KSUCCESS ) {
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}

	if ( unlink( tktfile ) < 0 ) {
	    syslog( LOG_ERR, "krb_login: unlink %s: %m", tktfile );
	    *rbuflen = 0;
	    return( AFPERR_BADUAM );
	}

	bcopy( cr.session, seskey, sizeof( C_Block ));
	key_sched( seskey, seskeysched );
	validseskey = 1;
	username = name;

	bcopy( authent.dat, p, authent.length );
	p += authent.length;
    }

    if ( kuam_get_in_tkt( name, instance, realm, TICKET_GRANTING_TICKET,
	    realm, 255, &rpkt ) != INTK_OK ) {
	*rbuflen = 0;
	return( AFPERR_PARAM );
    }


    q = (char *)rpkt.dat;
    *p++ = *q++;
    *p++ = *q++;
    while ( *q++ )
	;
    while ( *q++ )
	;
    while ( *q++ )
	;
    q += 10;

    len = strlen( realm );
    strcpy( p, realm );
    p += len + 1;
    bcopy( q, &slen, sizeof( short ));
    bcopy( &slen, p, sizeof( short ));
    p += sizeof( short );
    q += sizeof( short );
    bcopy( q, p, slen );
    p += slen;

    *rbuflen = p - rbuf;
    return( AFPERR_AUTHCONT );
}

afskrb_logincont( ibuf, ibuflen, rbuf, rbuflen )
    char	*ibuf, *rbuf;
    int		ibuflen, *rbuflen;
{
    CREDENTIALS		cr;
    struct ViceIoctl	vi;
    struct ClearToken	ct;
    struct passwd	*pwd;
    char		buf[ 1024 ], *p;
    int			aint;
    short		clen;

    *rbuflen = 0;
    ibuf += 2;
    bcopy( ibuf, &clen, sizeof( short ));
    clen = ntohs( clen );
    ibuf += sizeof( short );

    pcbc_encrypt((C_Block *)ibuf, (C_Block *)ibuf,
	    clen, seskeysched, seskey, DES_DECRYPT );
    if ( kuam_set_in_tkt( name, instance, realm, TICKET_GRANTING_TICKET,
	    realm, ibuf ) != INTK_OK ) {
	return( AFPERR_PARAM );
    }

    if ( get_ad_tkt( "afs", "", realm, 255 ) != KSUCCESS ) {
	return( AFPERR_PARAM );
    }
    if ( krb_get_cred( "afs", "", realm, &cr ) != KSUCCESS ) {
	return( AFPERR_PARAM );
    }

    p = buf;
    bcopy( &cr.ticket_st.length, p, sizeof( int ));
    p += sizeof( int );
    bcopy( cr.ticket_st.dat, p, cr.ticket_st.length );
    p += cr.ticket_st.length;

    ct.AuthHandle = cr.kvno;
    bcopy( cr.session, ct.HandShakeKey, sizeof( cr.session ));
    ct.ViceId = 0;
    ct.BeginTimestamp = cr.issue_date;
    /* ct.EndTimestamp = cr.issue_date + ( cr.lifetime * 5 * 60 ); */
    ct.EndTimestamp = krb_life_to_time( cr.issue_date, cr.lifetime );

    aint = sizeof( struct ClearToken );
    bcopy( &aint, p, sizeof( int ));
    p += sizeof( int );
    bcopy( &ct, p, sizeof( struct ClearToken ));
    p += sizeof( struct ClearToken );

    aint = 0;
    bcopy( &aint, p, sizeof( int ));
    p += sizeof( int );

    lcase( realm );
    strcpy( p, realm );
    p += strlen( realm ) + 1;

    vi.in = buf;
    vi.in_size = p - buf;
    vi.out = buf;
    vi.out_size = sizeof( buf );
    if ( pioctl( 0, VIOCSETTOK, &vi, 0 ) < 0 ) {
	syslog( LOG_ERR, "krb_logincont: pioctl: %m" );
	return( AFPERR_BADUAM );
    }

    if ( unlink( tktfile ) < 0 ) {
	syslog( LOG_ERR, "krb_logincont: %s: %m", tktfile );
	return( AFPERR_BADUAM );
    }

    if (( pwd = getpwnam( username )) == NULL ) {
	return( AFPERR_NOTAUTH );
    }
    if ( logged == 0 ) {
	logged = 1;
	syslog( LOG_INFO, "authenticated %s.%s@%s", name, instance, realm );
	return( login( pwd->pw_name, pwd->pw_uid, pwd->pw_gid ));
    }
    syslog( LOG_INFO, "re-authenticated %s.%s@%s", name, instance, realm );
    return( AFP_OK );
}
#endif UAM_AFSKRB AFS
