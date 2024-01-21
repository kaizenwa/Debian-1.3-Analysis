/* $Id: texpire.c,v 1.11 1996/06/23 21:33:53 agulbra Exp $

Written by Arnt Gulbrandsen <agulbra@troll.no> and copyright 1995
Troll Tech AS, Postboks 6133 Etterstad, 0602 Oslo, Norway, fax +47
22646949.

Use, modification and distribution is allowed without limitation,
warranty, or liability of any kind.  */

#include <ctype.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

#include "leafnode.h"

time_t now;

struct exp {
    char * xover;
    int art;
    int kill;
};


/* return 1 if xover is a legal overview line, 0 else */

static int legal ( char * xover ) {
    char * p;
    char * q;

    if ( !xover )
	return 0;

    /* anything that isn't tab, printable ascii, or latin-* ? then killit */

    p = xover;
    while ( *p ) {
	int c = (unsigned char)*p++;

	if ( ( c != '\t' && c < ' ' ) || ( c > 126 && c < 160 ) )
	    return 0;
    }

    p = xover;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* article number */

    while( p != q ) {
	if ( !isdigit( *p ) )
	    return 0;
	p++;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* subject: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* from: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* date: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* message-id: <*@*> */

    if ( *p != '<' )
	return 0;
    while ( p != q && *p != '@' && *p != '>' && *p != ' ' )
	p++;
    if ( *p != '@' )
	return 0;
    while ( p != q && *p != '>' && *p != ' ' )
	p++;
    if ( *p != '>' )
	return 0;
    if ( ++p != q )
	return 0;

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* references: a series of <*@*> */

    while ( p != q ) {
	if ( *p != '<' )
	    return 0;
	while ( p != q && *p != '@' && *p != '>' && *p != ' ' )
	    p++;
	if ( *p != '@' )
	    return 0;
	while ( p != q && *p != '>' && *p != ' ' )
	    p++;
	if ( *p++ != '>' )
	    return 0;
	while ( p != q && *p == ' ' )
	    p++;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* byte count */

    while( p != q ) {
	if ( !isdigit( *p ) )
	    return 0;
	p++;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( q )
	*q = '\0'; /* kill any extra fields */

    /* line count */

    while( p && *p ) {
	if ( !isdigit( *p ) )
	    return 0;
	p++;
    }

    return 1;
}


static void dogroup ( struct newsgroup * g ) {
    char gdir[PATH_MAX];
    char *p;
    char *q;
    DIR *d;
    struct dirent * de;
    struct stat st;
    unsigned long int first, last, art;
    struct exp * articles;
    int n;
    int fd;
    char * overview; /* xover: read then free */

    int deleted,kept;

    deleted = kept = 0;
    clearidtree();

    /* eliminate empty groups */

    strcpy( gdir, spooldir );
    p = gdir + strlen( gdir );
    *p++ = '/';
    strcpy( p, g->name );
    while(*p) {
	if (*p=='.')
	    *p = '/';
	else
	    *p = tolower(*p);
	p++;
    }
    if ( chdir( gdir ) )
	return;

    /* find low-water and high-water marks */

    d = opendir( "." );
    if ( !d ) {
	syslog( LOG_ERR, "opendir: %m" );
	return;
    }

    first = INT_MAX;
    last = 0;
    while ( (de=readdir(d)) ) {
	art = strtol( de->d_name, &p, 10 );
	if (p && !*p) {
	    if ( art < first )
		first = art;
	    if ( art > last )
		last = art;
	}
    }
    if ( last < first ) {
	closedir( d );
	return;
    }
    rewinddir( d );

    /* allocate and clear article array */

    articles = (struct exp *)critmalloc( (last-first+1)*sizeof(struct exp),
					 "Reading articles to expire" );
    for ( art = 0; art <= last-first; art++ ) {
	articles[art].xover = NULL;
	articles[art].kill = 0;
    }

    /* read in overview info, to be purged and written back */

    overview = NULL;

    if ( !stat( ".overview", &st ) ) {
	/* could use mmap() here but I don't think it would help */
	overview = critmalloc( st.st_size+1, "Reading article overview info" );
	if ( (fd=open( ".overview", O_RDONLY))<0 ||
	     (read( fd, overview, st.st_size ) < st.st_size) ) {
	    syslog( LOG_ERR, "can't open/read %s/.overview: %m", gdir );
	    *overview = '\0';
	    if ( fd > -1 )
		close( fd );
	} else {
	    close( fd );
	    overview[st.st_size] = '\0'; /* 0-terminate string */
	}

	p = overview;
	while ( p && *p ) {
	    while ( p && isspace( *p ) )
		p++;
	    art = strtol( p, NULL, 10 );
	    if ( art>= first && art<=last && 
		 !articles[art-first].xover ) {
		articles[art-first].xover = p;
		articles[art-first].kill = 1;
	    }
	    p = strchr( p, '\n' );
	    if ( p ) {
		*p = '\0';
		if ( p[-1]=='\r' )
		    p[-1] = '\0';
		p++;
	    }
	}
    }

    /* check the syntax of the .overview info, and delete all illegal stuff */

    for ( art = first; art <= last; art++ ) {
	if ( articles[art-first].xover &&
	     !legal( articles[art-first].xover ) ) {	
	    syslog( LOG_INFO, "bad overview line for %s/%lu", g->name, art );
	    articles[art-first].xover = NULL;
	}
    }
    

    /* insert articles in tree, and clear 'kill' for new or read articles */

    while ( (de=readdir(d)) ) {
	art = strtol( de->d_name, &p, 10 );
	if (p && !*p) {
	    articles[art-first].kill = 1;
	    /* note that stat() errno is used in the else clause */
	    if ( (!stat(de->d_name, &st)) && 
		 (S_ISREG(st.st_mode)) &&
		 ((st.st_mtime > expire) ||
		  (st.st_atime > expire)) ) {
		articles[art-first].kill = 0;
		p = articles[art-first].xover;
		for ( n=0; n<4; n++ ) 
		    if ( p && (p=strchr( p+1, '\t' )))
			p++;
		q = p ? strchr( p, '\t' ) : NULL;
		if ( p && q ) {
		    *q = '\0';
		    if ( findmsgid( p ) ) { /* another file with same msgid? */
			articles[art-first].kill = 1;
		    } else {
			insertmsgid( p, art );
			if ( st.st_nlink < 2 ) { /* repair fs damage */
			    if ( link( de->d_name, lookup( p ) ) ) {
				if ( errno == EEXIST )
				    /* exists, but points to another file */
				    articles[art-first].kill = 1;
				else 
				    syslog( LOG_ERR, 
					    "relink of %s failed: %m (%s)", 
					    p, lookup( p ) );
			    }
			    else
				syslog( LOG_INFO, "relinked message %s", p );
			}
			*q = '\t';
		    }
		} else if ( articles[art-first].xover ) {
		    /* data structure inconsistency: delete and be rid of it */
		    articles[art-first].kill = 1;
		} else {
		    /* possibly read the xover line into memory? */
		}
	    }
	}
    }
    closedir( d );

    /* compute new low-water mark */

    art = first;
    while ( art<=last && articles[art-first].kill )
	art++;
    g->first = art;

    /* remove old postings */

    for ( art = first; art <= last; art++ ) {
	char name[20];
	if ( articles[art-first].kill ) {
	    sprintf( name, "%lu", art );
	    if (!unlink( name )) {
		deleted++;
	    } else if ( errno != ENOENT ) {
		kept++;
		syslog( LOG_ERR, "unlink %s/%ld: %m", gdir, art );
	    } else {
		/* deleted by someone else */
	    }
	} else {
	    kept++;
	}
    }
    free( (char*)articles );
    free( overview );

    if ( last > g->last ) /* try to correct insane newsgroup info */
	g->last = last;

    if ( deleted || kept )
	printf("%s: %d articles deleted, %d kept\n", g->name, deleted, kept);

    if ( !kept && !chdir("..") )
	(void) rmdir( gdir );
}
    

static void expiregroup( struct newsgroup * g ) {
    if ( g ) {
	expiregroup( g->right );
	dogroup( g );
	expiregroup( g->left );
    }
}


static void expiremsgid( void ) {
    int n;
    DIR * d;
    struct dirent * de;
    struct stat st;
    int deleted, kept;

    deleted = kept = 0;

    for ( n=1; n<1000; n++ ) {
	sprintf( s, "%s/message.id/%03d", spooldir, n );
	if ( chdir( s ) ) {
	    if ( errno == ENOENT )
		mkdir( s, 0755 ); /* file system damage again */
	    if ( chdir( s ) ) {
		syslog( LOG_ERR, "chdir %s: %m", s );
		continue;
	    }
	}

	d = opendir( "." );
	if ( !d )
	    continue;
	while ( (de=readdir(d)) ) {
	    if ( !stat( de->d_name, &st ) )
		if ( st.st_nlink<2 && !unlink( de->d_name ) )
		    deleted++;
		else
		    kept++;
	}
	closedir( d );
    }

    if ( kept || deleted )
	printf("total: %d articles deleted, %d kept\n", deleted, kept);
}




int main( int argc, char ** argv ) {
    struct passwd * pw;

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

    expire = 0;
    readconfig();
    if ( expire == 0 ) {
	fprintf( stderr, "%s: no expire time\n", argv[0] );
	exit( 2 );
    }

    openlog( "expire", LOG_PID|LOG_CONS, LOG_NEWS );

    now = time(NULL);

    readactive();
    expiregroup( active );
    writeactive();
    expiremsgid();
    return 0;
}

	
