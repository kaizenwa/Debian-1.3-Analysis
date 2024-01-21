static char rcsid[] = 
"$Id: nntpd.c,v 1.19 1996/06/23 21:33:53 agulbra Exp $";

/* Written by Arnt Gulbrandsen <agulbra@troll.no> and copyright 1995
Troll Tech AS, Postboks 6133 Etterstad, 0602 Oslo, Norway, fax +47
22646949.

Use, modification and distribution is allowed without limitation,
warranty, or liability of any kind.  */

#include <sys/stat.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <syslog.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include "leafnode.h"

int write2(int fd, const char *msg);
int hash (const char *);
FILE * fopenart(const char *);
void list(struct newsgroup *, int);
void rereadactive(void);

const char* rfctime(void);
void parser(void);
void error(const char *msg);
void doarticle(const char *, int);
void dogroup(const char *);
void dohelp(void);
void domove(int);
void domove(int);
void dolist(const char *);
void donewgroups(const char *);
void donewnews(const char *);
void dopost( void );
void doxhdr(char *);
void doxover(const char *);
void dolistgroup( const char * );
void markinterest( void );


struct newsgroup * group; /* current group, initially none */
struct newsgroup * xovergroup;
int artno = 0; /* current article number, initially 0 and almost unused */
char *cmd; /* current command line */
struct newsgroup * active;
time_t activetime;
char version[10];
char fqdn[64];
static int justaftergroup;

const char* rfctime(void) {
    static char date[128];
    const char * months[] = { "Jan", "Feb", "Mar", "Apr",
	"May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    const char * days[] = { "Sun", "Mon", "Tue", "Wed",
	"Thu", "Fri", "Sat" };
    time_t now;
    struct tm gmt;
    struct tm local;
    struct tm conv;
    signed int diff;

    /* get local and Greenwich times */
    now = time(0);
    gmt   = *(gmtime(&now));
    local = *(localtime(&now));
    conv  = *(localtime(&now));

    /* if GMT were local, what would it be? */
    conv.tm_year = gmt.tm_year;
    conv.tm_mon  = gmt.tm_mon;
    conv.tm_mday = gmt.tm_mday;
    conv.tm_hour = gmt.tm_hour;
    conv.tm_min  = gmt.tm_min;
    conv.tm_sec  = gmt.tm_sec;
    conv.tm_wday = gmt.tm_wday;
    conv.tm_yday = gmt.tm_yday;

    /* see how far the real local is from GMT-as-local */
    /* should convert to signed int from double properly */
    diff = (mktime(&local) - mktime(&conv)) / 60.0;

    /* finally print the string */
    sprintf(date, "%3s, %d %3s %4d %02d:%02d:%02d %+03d%02d",
	    days[local.tm_wday], local.tm_mday, months[local.tm_mon],
	    local.tm_year+1900, local.tm_hour, local.tm_min, local.tm_sec, 
	    diff/60, diff%60);

    return (date);
}


void rereadactive(void) {
    struct stat st;

    strcpy(s, libdir);
    strcat(s, "/groupinfo");

    if ( (!stat(s, &st) && (st.st_mtime > activetime)) ||
	 (active==NULL) ) {
	readactive();
	activetime = st.st_mtime;
    }
}



void error(const char *msg) {
    printf("%s: %s\r\n", msg, 
	   (errno > sys_nerr) ? "unknown error" : sys_errlist[errno]);
}


void parser(void) {
    char *arg;
    int n;

    while(1) {
	if (!(cmd=getaline( stdin ))) {
	    error("421 Network error");
	    return;
	}

	n = 0;
	while(isalpha(cmd[n]) && n<1000) {
	    cmd[n] = tolower(cmd[n]);
	    n++;
	}

	if (n>999 || n==0) {
	    printf("400 Dazed and confused\r\n");
	    return;
	}

	syslog( LOG_DEBUG, "%s", cmd );

	while(isspace(cmd[n]) && n<1000)
	    cmd[n++] = '\0';

	arg = cmd+n;

	while(cmd[n] && n<1000)
	    n++;
	n--;

	while(isspace(cmd[n]))
	    cmd[n--] = '\0';

	if (!strcasecmp(cmd, "article")) {
	    doarticle(arg, 3);
	} else if (!strcasecmp(cmd, "head")) {
	    doarticle(arg, 1);
	} else if (!strcasecmp(cmd, "body")) {
	    doarticle(arg, 2);
	} else if (!strcasecmp(cmd, "stat")) {
	    printf("500 STAT is neither sensible nor fully specified\r\n");
	} else if (!strcasecmp(cmd, "help")) {
	    dohelp();
	} else if (!strcasecmp(cmd, "ihave")) {
	    printf("500 IHAVE is for big news servers\r\n");
	} else if (!strcasecmp(cmd, "last")) {
	    domove(-1);
	} else if (!strcasecmp(cmd, "next")) {
	    domove(1);
	} else if (!strcasecmp(cmd, "list")) {
	    dolist(arg);
	} else if (!strcasecmp(cmd, "mode")) {
	    printf("200 Modal interfaces are for losers\r\n");
	} else if (!strcasecmp(cmd, "newgroups")) {
	    donewgroups(arg);
	} else if (!strcasecmp(cmd, "newnews")) {
	    donewnews(arg);
	} else if (!strcasecmp(cmd, "post")) {
	    dopost();
	} else if (!strcasecmp(cmd, "quit")) {
	    printf("205 Always happy to serve!\r\n");
	    return;
	} else if (!strcasecmp(cmd, "slave")) {
	    printf("202 Cool - I always wanted a slave\r\n");
	} else if (!strcasecmp(cmd, "xhdr")) {
	    doxhdr(arg);
	} else if (!strcasecmp(cmd, "xover")) {
	    doxover(arg);
	} else if (!strcasecmp(cmd, "listgroup")) {
	    dolistgroup(arg);
	} else if (!strcasecmp(cmd, "group")) {
	    dogroup(arg);
	} else
	    printf("500 Unknown command\r\n");
	fflush(stdout);
    }
}


/* open an article by number or message-id */
FILE * fopenart(const char * arg) {
    unsigned long int a;
    FILE *f;
    char *t;

    t = NULL;
    a = strtol(arg, &t, 10);
    if (t && !*t && group) {
	f = fopen(arg, "r");
	if (f)
	    artno = a;
	markinterest();
	/* else try message-id lookup */
    } else if ( arg && *arg=='<' ) {
	f = fopen(lookup(arg), "r");
    } else if (group && artno) {
	sprintf(s, "%d", artno);
	f = fopen(s, "r");
	markinterest();
    } else {
	f = NULL;
    }
    return f;
}
    


/* display an article or somesuch */
void doarticle(const char *arg, int what) {
    FILE *f;
    char *p;
    long yuck;

    f = fopenart(arg);
    if (!f) {
	if ( strlen( arg ) )
	    printf("430 No such article: %s\r\n", arg );
	else
	    printf("430 No such article: %d\r\n", artno );
	return;
    }

    yuck = strtol( arg, &p, 10 );
    if ( !p || *p )
	yuck = 0;

    printf("%3d %ld Text follows, then a line consisting of a single '.'\r\n",
	   what==3 ? 220 : 220+what, yuck ); /* nice idea but... */

    while (fgets(s, 1024, f) && *s && (*s!='\n')) {
	if (what & 1) {
	    p = s;
	    while(p && *p)
		p++;
	    if (*--p == '\n')
		*p = '\0';

	    printf("%s%s\r\n", *s=='.' ? "." : "", s); /* . on headers :( */
	}
    }

    if (what==3)
	printf("\r\n"); /* empty separator line */

    while ((what & 2) && fgets(s, 1024, f) && *s) {
	p = s;
	while(p && *p)
	    p++;
	if (*--p == '\n')
	    *p = '\0';

	printf("%s%s\r\n", *s=='.' ? "." : "", s);
    }
    printf(".\r\n");
    fclose(f);

    return; /* OF COURSE there were no errors */
}

/* note bug.. need not be _immediately after_ GROUP */
void markinterest( void ) {
    int f;

    if ( !justaftergroup )
	return;
    sprintf( s, "%s/interesting.groups/%s", spooldir, group->name );
    if ( (f=open( s, O_WRONLY|O_CREAT|O_TRUNC, 0644)) >= 0 )
	close(f);
    justaftergroup = FALSE;
}
    


/* change to group - note no checks on group name */
void dogroup(const char *arg) {
    struct newsgroup * g;

    if ( justaftergroup && group && group->name ) {
	int f;
	/* mark the previous group as _possibly_ interesting */
	sprintf( s, "%s/interesting.groups/.%s", spooldir, group->name );
	if ( (f=open( s, O_WRONLY|O_CREAT|O_TRUNC, 0644)) >= 0 )
	    close(f);
    }

    rereadactive();
    g = findgroup(arg);
    if (g) {
	group = g;
	chdirgroup( g->name );
	printf("211 %d %d %d %s group selected\r\n", 
	       g->last >= g->first ? g->last-g->first+1 : 0, 
	       g->first, g->last, g->name);
	artno = g->first;
	fflush( stdout );
	justaftergroup = TRUE;
    } else {
	printf("411 No such group\r\n");
    }
}


void dohelp(void) {
    printf("500 Read RFC 977 and 1036 for elucidation\r\n");
}


void domove( int by ) {
    if ( group ) {
	if (artno) {
	    artno += by;
	    if ( artno > group->last )
		artno = group->last;
	    if ( artno < group->first )
		artno = group->first;
	    printf( "223 Article number is now %d\r\n", artno);
	} else {
	    printf( "420 There is no current article\r\n" );
	}
    } else {
	printf( "412 No newsgroup selected\r\n" );
    }
}



/* LIST ACTIVE if what==0, else LIST NEWSGROUPS */
void list(struct newsgroup *g, int what) {
    if (g) {
	list (g->right, what);
	if (what)
	    printf( "%s %s\r\n", g->name, g->desc ? g->desc : "" );
	else
	    printf( "%s %010d %010d y\r\n", g->name, g->last, g->first);
	list (g->left, what);
    }
}
	

void dolist(const char *arg) {

    rereadactive();
    if (chdir(spooldir))
	printf("503 News spool directory does not exist!\r\n");
    else if (!active)
	printf("503 Group information file does not exist!\r\n");
    else {
	if ( !*arg || !strcasecmp(arg, "active") ) {
	    printf("215 Brace yourself\n");
	    list ( active, 0 );
	    printf(".\r\n");
	} else if ( !strcasecmp(arg, "newsgroups") ) {
	    printf("215 Brace yourself\n");
	    list ( active, 1 );
	    printf(".\r\n");
	} else {
	    printf( "502 Syntax error\r\n" );
	}
    }
}

	
void donewgroups(const char *arg) {
    printf( "231 Command not supported\r\n.\r\n" );
}

void donewnews(const char *arg) {
    printf( "500 NEWNEWS is meaningless for this server\r\n" );
}


/* next bit is copied from INN 1.4 and modified ("broken") by agulbra

   mail to Rich $alz <rsalz@uunet.uu.net> bounced */

/* Scale time back a bit, for shorter Message-ID's. */
#define OFFSET	673416000L

static char ALPHABET[] = "0123456789abcdefghijklmnopqrstuv";

char * generateMessageID( void );

char * generateMessageID( void ) {
    static char buff[80];
    static time_t then;
    static unsigned int fudge;
    time_t now;
    char * p;
    int n;

    now = time(0); /* might be 0, in which case fudge will almost fix it */
    if ( now != then )
	fudge = 0;
    else
	fudge++;
    then = now;

    p = buff;
    *p++ = '<';
    n = (unsigned int)now - OFFSET;
    while( n ) {
	*p++ = ALPHABET[(int)(n & 31)];
	n >>= 5;
    }
    *p++ = '.';
    n = fudge * 32768 + (unsigned long)getpid();
    while( n ) {
	*p++ = ALPHABET[(int)(n & 31)];
	n >>= 5;
    }
    sprintf( p, ".ln@%s>", fqdn );
    return buff;
}

/* the end of what I stole from rsalz and then mangled */


void dopost( void ) {
    char * line;
    int havefrom = 0;
    int havepath = 0;
    int havedate = 0;
    int havenewsgroups = 0;
    int havemessageid = 0;
    int havesubject = 0;
    int err = 0;
    int len;
    int out;
    char outname[80];
    static int postingno; /* starts as 0 */

    sprintf( outname, "%s/out.going/%d-%d-%d", 
	     spooldir, (int)getpid(), (int)time(NULL), ++postingno );

    if ( 0 && upstream && ( nntpout || nntpconnect() ) ) {
	int r;
	fprintf( nntpout, "POST\r\n" );
	fflush( nntpout );
	r = nntpreply();
	printf( "%3d reply received from %s\r\n", r, upstream );
	fflush( stdout );
	if ( r == 340 ) {
	    do {
		int inheaders = 1;

		if ( (line = getaline( stdin )) )
		    if ( inheaders && !havepath && 
			 !strncmp( line, "Path: ", 6 ) ) {
			fprintf( nntpout, "Path: %s!%s\r\n",
				 fqdn, line[6] ? line+6 : "nobody" );
			havepath = 1;
		    } else if ( inheaders && !*line ) {
			inheaders = 0;
			if ( !havepath ) {
			    fprintf( nntpout, "Path: %s!nobody\r\n\r\n", fqdn);
			    havepath++; /* duh? why? */
			} else {
			    fprintf( nntpout, "\r\n" );
			}
		    } else {
			fprintf( nntpout, "%s\r\n", line );
		    }
	    } while ( line && strcmp( line, "." ) );
	    fflush( nntpout );
	    line = getaline( nntpin );
	    printf( "%s\r\n", line );
	    syslog( LOG_INFO, "relayed posting, reply %s", line );
	    return;
	}
    }

    out = open( outname, O_WRONLY|O_EXCL|O_CREAT, 0444 );
    if ( out < 0 ) {
	char dir[80];
	sprintf( dir, "%s/out.going", spooldir );
	mkdir( dir, 0755 );
	out = open( outname, O_WRONLY|O_EXCL|O_CREAT, 0444 );
	if ( out < 0 ) {
	    printf( "440 Unable to open spool file %s\r\n", outname );
	    return;
	}
    }

    printf( "340 Go ahead..\r\n" );
    fflush( stdout );

    do {
	int len;
	line = getaline( stdin );
	if ( !line ) {
	    unlink( outname );
	    exit( 0 );
	}
	if ( !strncasecmp( line, "From: ", 6 ) ) {
	    if ( havefrom )
		err = TRUE;
	    else
		havefrom = TRUE;
	}
	if ( !strncasecmp( line, "Path: ", 6 ) ) {
	    if ( havepath )
		err = TRUE;
	    else
		havepath = TRUE;
	}
	if ( !strncasecmp( line, "Message-ID: ", 12 ) ) {
	    if ( havemessageid )
		err = TRUE;
	    else
		havemessageid = TRUE;
	}
	if ( !strncasecmp( line, "Subject: ", 9 ) ) {
	    if ( havesubject )
		err = TRUE;
	    else
		havesubject = TRUE;
	}
	if ( !strncasecmp( line, "Newsgroups: ", 12 ) ) {
	    if ( havenewsgroups )
		err = TRUE;
	    else
		havenewsgroups = TRUE;
	}
	if ( !strncasecmp( line, "Date: ", 6 ) ) {
	    if ( havedate )
		err = TRUE;
	    else
		havedate = TRUE;
	}
	len = strlen( line );
	if ( len && line[len-1]=='\n' )
	    line[--len] = '\0';
	if ( len && line[len-1]=='\r' )
	    line[--len] = '\0';
	if ( len && line[len-1]=='\r' )
	    line[--len] = '\0';
	if ( len )
	    write( out, line, len );
	else {
	    if ( !havepath ) {
		write( out, "Path: ", 6 );
		write( out, fqdn, strlen( fqdn ) );
		write( out, "!nobody\r\n", 9 );
	    }
	    if ( !havedate ) {
		const char * l = rfctime();
		write( out, "Date: ", 6 );
		write( out, l, strlen( l ) );
		write( out, "\r\n", 2 );
	    }
	    if ( !havemessageid ) {
		char tmp[80];
 		sprintf( tmp, "Message-ID: %s\r\n",
 			 generateMessageID() );
		write( out, tmp, strlen( tmp ) );
	    }
	}
	write( out, "\r\n", 2 );
    } while ( *line );

    do {
	line = getaline( stdin );
	if ( !line ) {
	    unlink( outname );
	    exit( 1 );
	}

	len = strlen( line );
	if ( len && line[len-1]=='\n' )
	    line[--len] = '\0';
	if ( len && line[len-1]=='\r' )
	    line[--len] = '\0';
	if ( len && line[len-1]=='\r' )
	    line[--len] = '\0';
	if ( len )
	    write( out, line, len );
	write( out, "\r\n", 2 );
    } while ( line[0] != '.' || line[1] != '\0' );
    close( out );

    if ( havefrom && havesubject && havenewsgroups && !err ) {
	printf( "240 Article posted, now be patient\r\n" );
	return;
    }

    unlink( outname );
    printf( "440 Formatting error, article not posted\r\n" );

}





void doxhdr(char *arg) {
    char * h[] = { "Subject", "From", "Date", "Message-ID", "References",
		   "Bytes", "Lines" };
    int n = 7;
    char * l;
    unsigned long int a,b,c;
    char buffer[1024];

    l = arg;
    while( l && *l && !isspace(*l) )
	l++;
    if (l && *l)
	*l++ = '\0';
    while( l && *l && isspace(*l) )
	l++;

    if (l && *l=='<') { /* handle message-id form (well) */
	FILE * f;
	f = fopenart(l);
	if (!f) {
	    printf("430 No such article\r\n");
	    return;
	}
	do {
	    l = getaline( f );
	} while (l && *l && 
		 strncasecmp(arg, l, strlen(arg)) && 
		 l[strlen(arg)]!=':'); /* uh, space before : allowed? */
	while( l && *l && !isspace(*l) )
	    l++;
	while( l && *l && isspace(*l) )
	    l++;
	printf( "221 First line of %s header follows:\r\n%s\r\n.\r\n",
		arg, l ? l : "" ); /* dubious - if not found, "" */
	fclose( f );
	return;
    }

    if (l && *l) {
	b = a = strtol( l, &l, 10 );
	if (a && l && *l) {
	    while (l && isspace(*l))
		l++;
	    if ( *l=='-' )
		b = strtol( ++l, &l, 10 );
	    while (l && isspace(*l))
		l++;
	    if ( l && *l ) {
		printf( "502 Usage: XHDR header first[-last] or XHDR header message-id\r\n" );
		return;
	    }
	    if ( b==0 )
		b = xlast;
	}
    } else {
	a = b = artno;
    }

    if (!group) {
	printf( "412 Use the GROUP command first\r\n" );
	return;
    }

    do {
	n--;
    } while( n>-1 && strncasecmp(arg, h[n], strlen(h[n])) );

    markinterest();

    if ( xovergroup != group ) {
	getxover();
	xovergroup = group;
    }

    if ( a < xfirst )
	a = xfirst;
    if ( b > xlast )
	b = xlast;
    if ( a > b )
	a = b;

    printf( "221 %s header (from overview) for postings %lu-%lu:\r\n", 
	    h[n], a, b );
    buffer[1023] = '\0';
    for( c=a; c<=b; c++ ) {
	if ( xoverinfo[c-xfirst].text ) {
	    char * l = xoverinfo[c-xfirst].text;
	    int d;
	    for( d=0; l && d<=n; d++ )
		l = strchr(l+1, '\t');
	    if (l) {
		char * p;
		strncpy( buffer, ++l, 1023 );
		p = strchr(buffer, '\t');
		if (p)
		    *p = '\0';
	    }
	    printf("%lu %s\r\n", c, l && *l ? buffer : "");
	}
    }

    printf( ".\r\n" );
    return;
}


void doxover(const char *arg) {
    char * l;
    unsigned long int a,b, art;

    if (!group) {
	printf( "412 Use the GROUP command first\r\n" );
	return;
    }

    markinterest();

    l = NULL;
    b = a = strtol( arg, &l, 10 );
    if (a && l && *l) {
	while (l && isspace(*l))
	    l++;
	if ( *l=='-' )
	    b = strtol( ++l, &l, 10 );
	while (l && isspace(*l))
	    l++;
	if ( l && *l ) {
	    printf( "502 Usage: XOVER first[-last]\r\n" );
	    return;
	}
    }

    if ( xovergroup != group && !getxover() ) {
	printf( "224 Overview information not available\r\n.\r\n" );
	return;
    }
    xovergroup = group;

    if ( b==0 )
	b = xlast;

    if ( a > xlast )
	b = xlast;
    if ( b > xlast )
	b = xlast;
    if ( a < xfirst )
	a = xfirst;
    if ( b < xfirst )
	b = xfirst;

    printf( "224 Overview information for postings %lu-%lu:\r\n", a, b );
    for( art=a; art<=b; art++ ) {
	if ( xoverinfo[art-xfirst].text )
	    printf( "%s\r\n", xoverinfo[art-xfirst].text );
    }
    printf( ".\r\n" );
}



void dolistgroup( const char * arg ) {
    struct newsgroup * g;
    unsigned long int art;

    if ( arg && strlen(arg) ) {
	g = findgroup( arg );
	if ( !g ) {
	    printf( "481 No such group: %s\r\n", arg );
	    return;
	}
    } else if ( group )
	g = group;
    else {
	printf( "481 No group specified\r\n" );
	return;
    }

    markinterest();
    chdirgroup( g->name );

    if ( (xovergroup != group || xovergroup == NULL) && !getxover() ) {
	printf( "481 Overview information not available\r\n" );
	return;
    }
    xovergroup = group;

    printf( "211 Article list for %s follows\r\n", g->name );
    for( art=xfirst; art<=xlast; art++ ) {
	if ( xoverinfo[art-xfirst].text )
	    printf( "%lu \r\n", art );
    }
    printf( ".\r\n" );

    if ( group )
	chdirgroup( group->name );
}
	


int main( int argc, char ** argv ) {
    char * p;
    int fodder;
    struct hostent *he;
    struct sockaddr_in sa;

    p = strstr( rcsid, ",v " );
    if ( p ) {
	strncpy( version, p+3, 9 );
	p = strchr( version, ' ' );
	if ( p )
	    *p = '\0';
	else
	    exit( 1 );
    } else {
	exit( 1 );
    }

    fodder = sizeof(struct sockaddr_in);
    if (getsockname(0, (struct sockaddr *)&sa, &fodder)) {
	strcpy( fqdn, "localhost" );
    } else {
	he = gethostbyaddr((char *)&sa.sin_addr.s_addr,
			   sizeof(sa.sin_addr.s_addr),
			   AF_INET);
	strncpy( fqdn, 
		 he && he->h_name ? he->h_name : inet_ntoa(sa.sin_addr),
		 63 );
    }

    umask(2);

    openlog( "leafnode", LOG_PID|LOG_CONS, LOG_NEWS );

    printf("200 Leafnode NNTP Daemon, RCS version %s running at %s\r\n", 
	   version, fqdn);
    fflush(stdout);
 
    readconfig();

    rereadactive();

    parser();
    fflush(stdout);
    if ( nntpout )
	fprintf( nntpout, "QUIT\r\n" );

    if ( justaftergroup && group && group->name ) {
	int f;
	/* mark the previous group as _possibly_ interesting */
	sprintf( s, "%s/interesting.groups/.%s", spooldir, group->name );
	if ( (f=open( s, O_WRONLY|O_CREAT|O_TRUNC, 0644)) >= 0 )
	    close(f);
    }

    exit(0);
}
