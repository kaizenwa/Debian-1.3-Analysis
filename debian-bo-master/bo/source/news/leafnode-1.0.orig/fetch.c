/* $Id: fetch.c,v 1.31 1996/06/23 21:33:52 agulbra Exp $ 

Written by Arnt Gulbrandsen <agulbra@troll.no> and copyright 1995
Troll Tech AS, Postboks 6133 Etterstad, 0602 Oslo, Norway, fax +47
22646949.

Use, modification and distribution is allowed without limitation,
warranty, or liability of any kind.  */

#include <sys/types.h>
#ifdef BSD
#include <sys/errno.h>
#endif
#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <malloc.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pwd.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <syslog.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include "leafnode.h"

int artno;

time_t now;

struct newsgroup *cg;

int reals, fakes;

int extraarticles = 0;

void store( const char * filename,
	    FILE * filehandle,
	    const size_t bytes,
	    char * newsgroups,
	    const char * subject,
	    const char * from,
	    const char * date,
	    const char * msgid,
	    const char * references,
	    const char * lines);

int getgroup ( struct newsgroup *, int );
int postarticles( void );
void fixxover( void );
void ihave( void );

void  store( const char * filename,
	     FILE * filehandle,
	     size_t bytes,
	     char * newsgroups,
	     const char * subject,
	     const char * from,
	     const char * date,
	     const char * msgid,
	     const char * references,
	     const char * lines)
{
    char tmp[10];
    char xrefincase[4096]; /* 1024 for newsgroups, plus article numbers */
    char * p;
    char * q;
    char * x;
    int n;

    x = xrefincase;
    n = 0;

    if ( verbose == 3 )
	printf( "storing %s\n", msgid );
    else if ( verbose > 3 )
	printf( "storing %s: %s\n", msgid, newsgroups );

    p = newsgroups;
    while (p && *p) {
	n++;
	q = strchr( p, ',' );
	if (q)
	    *q++ = '\0';
	if ( *p ) {
	    if (!cg || strcmp(cg->name, p)) {
		cg = findgroup( p );
		if (cg)
		    chdirgroup( p );
	    }
	    if ( cg ) {
		do {
		    sprintf(tmp, "%d", ++cg->last);
		    errno = 0;
		    if ( verbose > 2 )
			printf( " ...as article %d in %s\n",
			       cg->last, cg->name );
		} while ( link( filename, tmp )<0 && errno==EEXIST );
		if ( errno )
		    syslog( LOG_ERR, "error linking %s into %s: %m",
			    filename, p );
		else {
		    sprintf( x, " %s:%d", cg->name, cg->last );
		    x += strlen( x );
		}
	    }
	}
	p = q;
    }
    if ( n > 1 )
	fprintf( filehandle, "Xref: %s%s\n", fqdn, xrefincase );
}
		       



int getgroup ( struct newsgroup * g, int server ) {
    static char *hd[10];
    char *hnames[10] = {"Path: ", "Message-ID: ", "From: ", "Newsgroups: ",
			"Subject: ", "Date: ", "References: ", "Lines: ", 
			"Xref: ", ""};
    int h;
    int n;
    int last;
    int window; /* last ARTICLE n command sent */
    char *l;
    FILE *f;
    const char *c;
    struct stat st;
    int outstanding;
    static char * stufftoget;

    if ( !g )
	return server;

    if (g->first > g->last)
	g->last = g->first;

    reals = fakes = 0;
    chdirgroup( g->name );

    fprintf( nntpout, "GROUP %s\r\n", g->name );
    fflush( nntpout );

    l = getaline( nntpin );
    if ( !l )
	return server;

    if (sscanf(l, "%3d %d %d %d ", &n, &h, &window, &last) < 4
	|| n != 211 )
	return server;

    if ( extraarticles ) {
	int i;
	i = server - extraarticles;
	if ( i < window )
	    i = window;
	if ( verbose > 1 )
	    printf( "backing up from %d to %d\n",
		    server, i );
	server = i;
    }

    if ( server > last+1 ) {
	if ( verbose )
	    printf( "switched upstream servers? %d > %d\n",
		   server-1, last );
	server = window; /* insane - recover thoroughly */
    }
    if ( artlimit && last-server > artlimit ) {
	if ( verbose )
	    printf( "skipping articles %d-%d inclusive\n", 
		    server, last-artlimit-1 );
	server = last-artlimit;
    }

    sprintf( s, "%s/interesting.groups/%s", spooldir, g->name );
    if ( stat( s, &st ) )
	return last; /* race condition: I hope this is proper */

    if ( ((st.st_mtime==st.st_ctime) && (now-st.st_ctime > TIMEOUT_SHORT) &&
	  (server <= last) )
	  || (now-st.st_mtime > TIMEOUT_LONG) ) {
	if ( verbose )
	    printf("skipping %s from now on\n", g->name);
	syslog( LOG_INFO, "skip %s: %d, %d", g->name, server, last );
	unlink( s );
    }

    if ( window < server )
	window = server;
    if ( window < 1 )
	window = 1;
    server = window;

    if ( server <= last ) {
	if ( verbose )
	    printf( "%s: considering articles %d-%d\n", 
		    g->name, server, last );
	fprintf( nntpout, "XHDR Message-ID %d-%d\r\n",
		 server, last );
	fflush( nntpout );
	if ( nntpreply() == 221 ) {
	    stufftoget = (char*) realloc( stufftoget, last + 1 - server );
	    if ( stufftoget )
		memset( stufftoget, 0, last + 1 - server );
	    while ( (l = getaline( nntpin )) && strcmp( l, "." ) ) {
		int art;
		char * t;
		art = strtol(l, &t, 10);
		if (t && isspace( *t )) {
		    while ( isspace( *t ) )
			t++;
		    if ( stufftoget &&
			 art >= server &&
			 art <= last &&
			 stat( lookup( t ), &st ) != 0 )
			stufftoget[ art - server ] = 'y'; /* anything */
		    if ( verbose > 1 && stufftoget[ art - server ] )
			printf( "%s: will fetch %d (%s)\n", g->name, art, t );
		}
	    }
	    if ( !l )
		return server;
	} else {
	    stufftoget = realloc( stufftoget, 0 );
	}
    } else if ( verbose ) {
	printf( "%s: no new articles\n", g->name );
    }

    outstanding = 0;
    while ( window <= last || outstanding ) {
	while ( outstanding < 47 && window <= last ) {
	    while ( stufftoget &&
		    window <= last &&
		    !stufftoget[ window - server ] ) {
		if ( verbose > 3 )
		    printf( "%s: skipping %d\n", g->name, window );
		window++;
	    }
	    if ( window <= last ) {
		outstanding++;
		if ( verbose > 1 ) 
		    printf( "%s: ARTICLE %d (%d outstanding)\n",
			    g->name, window, outstanding );
		fprintf(nntpout, "ARTICLE %d\r\n", window++);
	    }
	}
	fflush(nntpout);

	if ( outstanding > 0 ) {
	    l = getaline( nntpin );
	    if ( !l )
		return server;

	    outstanding--;
	    if (sscanf(l, "%3d %d", &n, &h) < 2 || n != 220 ) {
		if ( verbose )
		    printf( "%s: reply %03d (%d outstanding)\n",
			    g->name, n, outstanding );
		continue;
	    }

	    if ( verbose )
		printf( "%s: receiving article %d (%d outstanding)\n",
			g->name, h, outstanding );

	    for ( h=0; h<10; h++ ) {
		if (hd[h])
		    free(hd[h]);
		hd[h] = strdup("");
	    }
	    h = 9;
	    c = NULL;

	    while ( ((l=getaline(nntpin)) != NULL) && strcmp(l, ".") ) {
		if ( !*l ) { /* end of headers */
		    for ( h=0; hd[h] && *(hd[h]) && h<4; h++ )
			;
		    f = NULL;
		    if ( h == 4 ) {
			c = lookup(strchr( hd[1], '<'));
			if ( stat( c, &st ) && errno == ENOENT )
			    f = fopen( c, "w" );
			/* ie. discard cross-posted stuff and other garbage */
		    }
		    if (f) {
			for ( h=0; h<10; h++ )
			    if ( h!=8 && hd[h] && *(hd[h]))
				fprintf( f, "%s", hd[h] );
			h = 0;
			while ( h<8 ) {
			    char * p1;
			    char * p2;
			    p1 = p2 = hd[h];
			    while (p1 && *p1) {
				if ( isspace(*p1) ) {
				    *p2 = ' ';
				    do {
					p1++;
				    } while ( isspace(*p1) );
				} else {
				    *p2 = *p1++;
				}
				if (*p1)
				    p2++;
				else
				    *p2 = '\0';
			    }
			    h++;
			}
			store( c, f, st.st_size,
			       *hd[3] ? hd[3]+strlen(hnames[3]) : "", 
			       *hd[4] ? hd[4]+strlen(hnames[4]) : "", 
			       *hd[2] ? hd[2]+strlen(hnames[2]) : "", 
			       *hd[5] ? hd[5]+strlen(hnames[5]) : "", 
			       *hd[1] ? hd[1]+strlen(hnames[1]) : "", 
			       *hd[6] ? hd[6]+strlen(hnames[6]) : "", 
			       *hd[7] ? hd[7]+strlen(hnames[7]) : "");
		    }
		    do {
			if (f)
			    fprintf(f, "%s\n", l);
		    } while ( ((l=getaline(nntpin)) != NULL) &&
			      strcmp(l, ".") );
		    if ( f )
			fclose( f );
		    if ( !l ) {
			unlink( c );
		    } else {
			if ( hd[7] && 
			     ((c=strstr(hd[7], "\nSupersedes:" )) != NULL) ) {
			    /* that isn't strictly correct, it's case sensitive
			       and lacks trailing space */
			    c += strlen( "\nSupersedes:" );
			    while ( c && isspace( *c ) )
				c++;
#ifdef NOTYET
			    if ( *c == '<' && ( (c=lookup( c )) != NULL ))
				/* supersede( c ) */ ;
#endif
			}
		    }
		    break;
		} else { /* still within headers */
		    if ( isspace( *l ) ) {
			hd[h] = critrealloc( hd[h], 
					     strlen(hd[h])+strlen(l)+2,
					     "Fetching article header" );
			strcat( hd[h], l );
			strcat( hd[h], "\n" );
		    } else {
			n = 0;
			while ( strncasecmp( l, hnames[n],
					     strlen(hnames[n]) ) )
			    n++;
			if ( n<9 && hd[n] && *(hd[n]) )
			    /* second occurance is "other header" */
			    n = 9;
			hd[n] = critrealloc( hd[n],
					     strlen(hd[n])+strlen(l)+2,
					     "Fetching article header" );
			strcat( hd[n], l );
			strcat( hd[n], "\n" );
			h = n;
		    }
		}
	    }
	}
    }

    return last +1;
}



static void nntpactive( void ) {
    char * l;
    int first;
    struct stat st;

    sprintf( s, "%s/active.read", spooldir );
    if ( stat( s, &st )==0 && now-st.st_mtime < TIMEOUT_LONG ) {
	if ( verbose )
	    printf( "LIST ACTIVE done only %d seconds ago, skipping\n",
		    (int)(now-st.st_mtime) );
	return;
    }

    fprintf(nntpout, "LIST ACTIVE\r\n");
    fflush(nntpout);

    if ( nntpreply() != 215 )
	return;

    while ( (l=getaline(nntpin)) && ( *l != '.' ) ) {
	char * p;

	p = l;
	while (!isspace(*p) )
	    p++;
	if (*p)
	    *p++ = '\0';
	first = strtol( p, &p, 10); /* to be discarded */
	if ( p && !findgroup( l )) {
	    first = strtol( p, NULL, 10 );
	    insertgroup( l, first, first-1, "", first );
	    syslog( LOG_INFO, "Registered group %s", l );
	}
    }

    sprintf( s, "%s/active.read", spooldir );
    unlink( s );
    close( open( s, O_WRONLY|O_CREAT, 0664 ) );
}


/*
 * post all spooled articles
 *
 * if all postings succeed, returns 1
 * if there are no postings to post, returns 1
 * if a posting is strange for some reason, closes the nntp connection
 *  and returns 0.  this is to recover from unknown states
 *
 * a posting is deleted once it has been posted, whether it succeeded
 * or not: we don't want to re-do an error.
 *
 */

int postarticles( void ) {
    char * line;
    DIR * d;
    struct dirent * de;
    FILE * f;
    struct stat st;
    int r;

    if ( chdir( spooldir ) || chdir ( "out.going" ) ) {
	syslog( LOG_ERR, "Unable to cd to outgoing directory: %m" );
	return 1;
    }

    d = opendir( "." );
    if ( !d ) {
	syslog( LOG_ERR, "Unable to opendir out.going: %m" );
	return 1;
    }

    while ( (de=readdir( d )) != NULL ) {
	if ( !stat(de->d_name, &st) &&
	     S_ISREG( st.st_mode ) &&
	     (f=fopen( de->d_name, "r" )) != NULL ) {
	    fprintf( nntpout, "POST\r\n" );
	    fflush( nntpout );
	    r = nntpreply();
	    if ( r == 340 ) {
		do {
		    line = getaline( f );
		    if ( line )
			fprintf( nntpout, "%s\r\n", line );
		} while ( line && strcmp( line, "." ) );
		fflush( nntpout );
		if ( !line ) {
		    sprintf( s, "%s/failed.postings", spooldir );
		    mkdir( s, 0775 );
		    sprintf( s, "%s/failed.postings/%s", 
			     spooldir, de->d_name );
		    syslog( LOG_ERR, 
			    "unable to post (%s), moving to %s",
			    line, s );
		    if ( rename( de->d_name, s ) )
			syslog( LOG_ERR,
				"unable to move failed posting to %s: %m",
				s );
		    closedir( d );
		    fclose( nntpout );
		    return 0; /* strange state, so re-connect */
		}
		line = getaline( nntpin );
		if ( line && !strncmp( line, "240", 3 ) ) {
		    syslog( LOG_INFO, "posted article" ); /* useless */
		    unlink( de->d_name);
		} else {
		    if ( line && !strncmp( line, "441 435", 7 ) ) {
			unlink( de->d_name );
		    } else {
			sprintf( s, "%s/failed.postings", spooldir );
			mkdir( s, 0775 );
			sprintf( s, "%s/failed.postings/%s", 
				 spooldir, de->d_name );
			syslog( LOG_ERR, 
				"unable to post (%s), moving to %s",
				line, s );
			if ( rename( de->d_name, s ) )
			    syslog( LOG_ERR,
				    "unable to move failed posting to %s: %m",
				    s );
			closedir( d );
			fclose( nntpout );
			return 0;
		    }
		}
	    } else
		syslog( LOG_ERR, "unable to post: %03d", r );
	    fclose( f );
	}
    }
    closedir( d );
    return 1;
}



/*
 * try to IHAVE all spooled articles
 *
 * a posting is _not_ deleted once it has been posted
 *
 */

void ihave( void ) {
    char * line;
    DIR * d;
    struct dirent * de;
    FILE * f;
    struct stat st;

    if ( chdir( spooldir ) || chdir ( "out.going" ) ) {
	syslog( LOG_ERR, "Unable to cd to outgoing directory: %m" );
	return;
    }

    d = opendir( "." );
    if ( !d ) {
	syslog( LOG_ERR, "Unable to opendir out.going: %m" );
	return;
    }

    while ( (de=readdir( d )) != NULL ) {
	if ( !stat(de->d_name, &st) &&
	     S_ISREG( st.st_mode ) &&
	     (f=fopen( de->d_name, "r" )) != NULL ) {
	    do {
		line = getaline( f );
		if ( line && !*line ) {
		    syslog( LOG_ERR, "No message-id in %s", de->d_name );
		    sprintf( s, "%s/failed.postings", spooldir );
		    (void) mkdir( s, 0775 );
		    sprintf( s, "%s/failed.postings/%s", 
			     spooldir, de->d_name );
		    if ( rename( de->d_name, s ) )
			syslog( LOG_ERR,
				"unable to move failed posting to %s: %m", s );
		    break;
		}
	    } while ( line && strncasecmp( line, "Message-ID: ", 12 ) );
	    if ( line && strlen( line+12 ) ) {
		char * msgid;
		/* bad assumption here - IHAVE\n\t<asdf@asdf> is legal
		    according to the RFCs */
		msgid = strdup( line+12 );    
		fprintf( nntpout, "IHAVE %s\r\n", msgid );
		fflush( nntpout );
		if ( nntpreply() == 335 ) {
		    fseek( f, 0, SEEK_SET );
		    do {
			line = getaline( f );
			if ( line )
			    fprintf( nntpout, "%s\r\n", line );
		    } while ( line && strcmp( line, "." ) );
		    fflush( nntpout );
		    syslog( LOG_INFO, "IHAVE %s received %03d reply",
			    msgid, nntpreply() );
		}
	    }
	}
    }
    closedir( d );
}




static void processactive( void ) {
    DIR * d;
    struct dirent * de;
    struct newsgroup * g;

    sprintf( s, "%s/interesting.groups", 
	     spooldir );
    d = opendir( s );
    if ( !d ) {
	syslog( LOG_ERR, "opendir %s: %m", s );
	return;
    }

    while ( (de=readdir(d)) ) {
	if ( isalpha( *(de->d_name) ) ) {
	    g = findgroup( de->d_name );
	    if ( g ) {
		g->alive = 1;
		g->server = getgroup( g, g->server );
	    }
	}
    }
    rewinddir( d );
    while ( (de=readdir(d)) ) {
	if ( '.' == *(de->d_name) && isalpha( (de->d_name)[1] ) ) {
	    /* someone did GROUP but didn't read anything.. feed them a fake */
	    g = findgroup( 1+(de->d_name) );
	    if ( g && !g->alive ) {
		g->last++;
		unlink( de->d_name );
	    }
	}
    }
    closedir( d );
}



static void processsupplement( const char * supplement ) {
    char newfile[20];
    FILE * i;
    FILE * o;
    struct newsgroup * root;

    root = NULL;
    sprintf( s, "%s/%s", libdir, supplement );
    i = fopen( s, "r" );
    if ( i ) {
	DIR * d;
	struct dirent * de;
	struct newsgroup * g;

	strcpy( newfile, ".supplement.XXXXX" );
	o = fopen( mktemp( newfile ), "w" );
	
	sprintf( s, "%s/interesting.groups", 
		 spooldir );
	d = opendir( s );
	if ( !d ) {
	    syslog( LOG_ERR, "opendir %s: %m", s );
	    return;
	}

	while ( (de=readdir(d)) ) {
	    if ( isalpha( *(de->d_name) ) ) {
		g = findgroup( de->d_name );
		if ( g ) {
		    g->alive = 1;
		}
	    }
	}
	closedir( d );
    }
}






void fixxover( void ) {
    DIR * d;
    struct dirent * de;

    sprintf( s, "%s/interesting.groups", 
	     spooldir );
    d = opendir( s );
    if ( !d ) {
	syslog( LOG_ERR, "opendir %s: %m", s );
	return;
    }

    while ( (de=readdir(d)) ) {
	if ( isalpha( *(de->d_name) ) && findgroup( de->d_name ) ) {
	    chdirgroup( de->d_name );
	    getxover();
	}
    }
    closedir( d );
}
    


int main( int argc, char ** argv ) {
    struct passwd * pw;
    int option;

    verbose = 0;

    pw = getpwnam( "news" );
    if ( !pw ) {
	fprintf( stderr, "no such user: news\n" );
	exit( 1 );
    }

    setgid( pw->pw_gid );
    setreuid( pw->pw_uid, pw->pw_uid );

    if ( getuid() != pw->pw_uid || getgid() != pw->pw_gid ) {
	fprintf( stderr, "%s: must be run as news or root\n", argv[0] );
	exit( 1 );
    }

    while ( (option=getopt( argc, argv, "vx:" )) != -1 ) {
	if ( option == 'v' ) {
	    verbose++;
	} else if ( option == 'x' ) {
	    char *nptr, *endptr;
	    nptr = optarg;
	    endptr = NULL;
	    extraarticles = strtol( nptr, &endptr, 0 );
	    if ( !nptr || !*nptr || !endptr || *endptr || !extraarticles ) {
		fprintf( stderr, "Usage: fetch [-v] [-x #]\n"
			 "  -v: more verbose (may be repeated)\n"
			 "  -x: check for # extra articles in each group\n" );
		exit( 1 );
	    }
	}
    }

    if ( verbose )
	printf( "verbosity level is %d\n", verbose );

    whoami();

    now = time(NULL);

    umask(2);

    openlog( "fetch", LOG_PID|LOG_CONS, LOG_NEWS );

    readconfig();
    if ( upstream == NULL ) {
	syslog( LOG_ERR, 
		"no server name in %s", s );
	exit( 2 );
    }

    do {
	if (!nntpconnect()) {
	    syslog( LOG_ERR, "unable to connect to %s", upstream );
	    fprintf( stderr, "unable to connect to %s\n", upstream );
	    exit(1);
	}
	ihave();
	fprintf( nntpout, "MODE READER\r\n" );
	fflush( nntpout );
	if ( nntpreply() == 498 )
	    continue;
    } while ( !postarticles() );

    readactive();
    nntpactive();
    processactive();

    fprintf(nntpout, "QUIT\r\n"); /* say it, then just exit :) */
    fflush(nntpout);
    writeactive();

    while ( supplement ) {
	if ( nntpout )
	    fclose( nntpout );
	if ( nntpin )
	    fclose( nntpin );
	nntpin = nntpout = NULL;
	upstream = supplement->name;
	if ( nntpconnect() ) {
	    syslog( LOG_INFO, "supplementing from %s", supplement->name );
	    ihave();
	    fprintf( nntpout, "MODE READER\r\n" );
	    fflush( nntpout );
	    if ( nntpreply() != 498 ) {
		(void) postarticles();
		processsupplement( supplement->name );
	    }
	    fprintf(nntpout, "QUIT\r\n"); /* say it, then just exit :) */
	    fflush(nntpout);
	}
	supplement = supplement->next;
    }
	
    syslog( LOG_INFO, "done" );

    if ( verbose || (fork() <= 0) ) {
#if defined(PRIO_MAX)
	setpriority( PRIO_PROCESS, 0, PRIO_MAX/2 );
#endif
	fixxover();
    }

    exit(0);
}
