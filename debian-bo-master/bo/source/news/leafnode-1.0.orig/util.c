/* $Id: util.c,v 1.15 1996/06/23 21:33:53 agulbra Exp $

Written by Arnt Gulbrandsen <agulbra@troll.no> and copyright 1995
Troll Tech AS, Postboks 6133 Etterstad, 0602 Oslo, Norway, fax +47
22646949.

Use, modification and distribution is allowed without limitation,
warranty, or liability of any kind. */

#include <fcntl.h>
#include <sys/uio.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <malloc.h>
#include <netdb.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <dirent.h>

#include "leafnode.h"

#if !defined(UIO_MAXIOV)
#define UIO_MAXIOV 8
#endif

char fqdn[256];
int verbose = 0;

char s[PATH_MAX+1024]; /* long string, here to cut memory usage */

FILE *nntpin;
FILE *nntpout;

static jmp_buf timeout;

static void timer(int sig) {
    longjmp(timeout, 1);
    exit(sig);
}

/*
 * decode an NNTP reply number
 * reads a line from the server and returns an integer
 *
 * 498 is used to mean "protocol error", like smail
 *
 * the text returned is discarded
 */

int nntpreply(void) {
    char *response;
    int r = 0;
    int c = 1;

    while (c) {
	response=getaline(nntpin);
	if (!response) {
	    syslog( LOG_ERR, "NNTP server went away" );
	    return 498;
	}
	if (strlen(response)>2 
	    && isdigit(response[0]) 
	    && isdigit(response[1])
	    && isdigit(response[2])
	    && ( (response[3]==' ')
		|| (response[3]=='\0')
		|| (response[3]=='-') ) ) {
	    int rl;
	    rl = atoi(response);
	    if (r>0 && r!=rl)
		r = 498;    /* protocol error */
	    else
		r = rl;
	    c = (response[3]=='-');
	} else {
	    c = 0;
	    r = 498;	/* protocol error */
	}
    }
    return r;
}

extern int errno;
extern int h_errno;
extern struct state _res;

#define incopy(a)       *((struct in_addr *)a)

int nntpconnect( void ) {
    struct hostent *hp;
    struct servent *sp;
    struct sockaddr_in s_in;
    int sock;
    register int i;

    memset((void *)&s_in, 0, sizeof(s_in));
    sp = getservbyname("nntp", "tcp");
    if (sp == NULL) {
	syslog( LOG_ERR, "unable to find service NNTP" );
	return FALSE;
    }

/* Fetch the ip addresses of the given host. */
    hp = gethostbyname( upstream );
    if (hp) {

/* Try to make connection to each of the addresses in turn. */
	for (i = 0; (int *)(hp->h_addr_list)[i]; i++) {
	    s_in.sin_family = hp->h_addrtype;
	    s_in.sin_port = sp->s_port;
	    s_in.sin_addr = incopy(hp->h_addr_list[i]);

	    sock = socket(AF_INET, SOCK_STREAM, 0);
	    if (sock < 0)
		break;

	    if (setjmp(timeout) != 0) {
		(void) close(sock);
		continue;
	    }

	    (void) signal(SIGALRM, timer);
	    (void) alarm((unsigned)10); /* 10 seconds to open conn */
	    if (connect(sock, (struct sockaddr *)&s_in, sizeof(s_in)) < 0)
		break;
	    (void) alarm((unsigned)0);

	    nntpout = fdopen(sock, "w");
	    if (nntpout == NULL)
		break;

	    nntpin  = fdopen(dup(sock), "r");
	    if (nntpin == NULL) 
		break;

	    switch(nntpreply()) {
		case 200: 
		case 201: 
		    syslog( LOG_INFO, "connected to %s",
			    inet_ntoa( s_in.sin_addr ) );
		    return TRUE;
	    }
	    shutdown(fileno(nntpout), 0);
	}/* end of IP-addresses for loop */
    }
    return FALSE;
}/* end of connect function */


/*
 * read size bytes into buf from file fp,
 * with four hours timeout
 * return a pointer to buf, or NULL in case of error
 */
static char *sfgets(char *buf, size_t size, FILE *fp) {
    register char *p;

    if (setjmp(timeout)) {
	return(NULL);
    }

    (void) signal(SIGALRM, timer);
    (void) alarm(14400U);    /* four hours read timeout */
    p = fgets(buf, size, fp);
    if (errno == EINTR)
	errno = ETIMEDOUT;
    (void) alarm(0U);
    return p;
}


/*
 * Lars Wirzenius: read a line into memory, with no max length
 * return a pointer to the line, or NULL in case of error
 *
 * strip \r at EOL
 */
char *getaline(FILE *f) {
    static char *buf;       /* buffer for line */
    static size_t size;     /* size of buffer */
    size_t len;             /* # of chars stored into buf before '\0' */
    char * p;

    len = 0;
    if (!size)
	size = 256;
    if (!buf)
	buf = critmalloc( size, "reading line" );

    while ((p=sfgets(buf+len, size-len, f)) != NULL) {
	len += strlen(buf+len);
        if (len > 0 && buf[len-1] == '\n')
	    break;          /* the whole line has been read */

	size += size+100;
	buf = critrealloc(buf, size, "reading line" );
    }

    if ( len == 0 )
	return NULL;

    if (len && (buf[len-1] == '\n')) { /* go back on top of the newline */
	--len;
	if (len && (buf[len-1] == '\r')) /* also delete CR */
	    --len;
    }

    buf[len] = '\0';        /* unconditionally terminate string,
                               possibly overwriting newline */

    return buf;
}


/* no good but this server isn't going to be scalable so shut up */
const char * lookup (const char *msgid) {
    static char * name = NULL;
    static int namelen = 0;
    unsigned int r;
    unsigned int i;

    if (!msgid || !*msgid)
	return NULL;

    i = strlen(msgid)+strlen(spooldir)+30;

    if (!name) {
	name = (char *)malloc(i);
	namelen = i;
    } else if (i > namelen) {
	name = (char *)realloc(name, i);
	namelen = i;
    }

    if (!name) {
	syslog( LOG_ERR, "malloc(%d) failed", i );
	exit(1);
    }

    strcpy( name, spooldir );
    strcat( name, "/message.id/000/" );
    i = strlen( name );
    strcat( name, msgid );

    r = 0;
    do {
	if ( name[i] == '/' )
	    name[i] = '@';
	else if ( name[i] == '>' )
	    name[i+1] = '\0';
	r += (int)(name[i]);
	r += ++i;
    } while ( name[i] );

    i = strlen( spooldir )+14; /* to the last digit */
    r = (r%999)+1;
    name[i--] = '0'+(char)(r%10); r /= 10;
    name[i--] = '0'+(char)(r%10); r /= 10;
    name[i] = '0'+(char)(r);
    return name;
}



/*
 * replacement for malloc, syslogs allocation failures
 * and exits with the error message
 */
char * critmalloc(size_t size, const char* message) {
    char * a;

    a = malloc(size);
    if (!a) {
	syslog(LOG_ERR, "malloc(%d) failed: %s", (int)size, message);
	fprintf(stderr, "malloc(%d) failed: %s\n", (int)size, message);
	exit(1);
    }
    return a;
}

/*
 * replacement for realloc, syslogs allocation failures
 * and exits with the error message
 */
char * critrealloc(char *a, size_t size, const char* message) {
    a = realloc(a, size);
    if (!a) {
	syslog(LOG_ERR, "realloc(%d) failed: %s", (int)size, message);
	fprintf(stderr, "realloc(%d) failed: %s\n", (int)size, message);
	exit(1);
    }
    return a;
}

#define LM_SIZE 65536

static void makedir( char * d ) {
    char * p;
    char * q;

    if (!d || *d != '/' || chdir("/"))
	return;
    q = d;
    do {
	*q = '/';
	p = q;
	q = strchr( ++p, '/' );
	if (q)
	    *q = '\0';
	if (!chdir(p))
	    continue; /* ok, I do use it sometimes :) */
	if (errno==ENOENT)
	    if (mkdir(p, 0775)) {
		syslog( LOG_ERR, "mkdir %s: %m", d );
		exit( 1 );
	    }
	if (chdir(p)) {
	    syslog( LOG_ERR, "chdir %s: %m", d );
	    exit( 1 );
	}
    } while ( q );
}
	

/* chdir to the directory of the argument if it's a valid group
   return TRUE if it is valid, FALSE otherwise */
void chdirgroup(const char *group) {
    char *p;

    if (group && *group) {
	strcpy(s, spooldir);
	p = s + strlen(s);
	*p++ = '/';
	strcpy(p, group);
	while(*p) {
	    if (*p=='.')
		*p = '/';
	    else
		*p = tolower(*p);
	    p++;
	}
	if (!chdir(s))
	    return;
	makedir( s );
	return;
    }
}


struct newsgroup * active;

/* insert or update a newsgroup in the global list */
void insertgroup(const char * name, 
		 int first, int last,
		 const char * desc, int server ) {
    struct newsgroup ** a;
    int c;

    if ( !desc )
	desc = "";

    a = &active;
    while(a) {
	if (*a) {
	    c = strcmp((*a)->name, name);
	    if (c<0)
		a = &((*a)->left);
	    else if (c>0)
		a = &((*a)->right);
	    else { 
		if (first >= 0)
		    (*a)->first = first;
		if (last >= 0)
		    (*a)->last = last;
		if (server >= 0)
		    (*a)->server = server;
		(*a)->desc = (desc && *desc) ? strdup( desc ) : "";
		return;
	    }
	} else {
	    *a = (struct newsgroup *) 
		 critmalloc(sizeof(struct newsgroup) + 2 +
			    strlen( name ) + strlen( desc ),
			    "Building newsgroups info list");
	    if (!*a)
		return;
	    (*a)->left = (*a)->right = NULL;
	    (*a)->first = first;
	    (*a)->last = last;
	    (*a)->server = server;
	    (*a)->alive = 0;
	    (*a)->name = (char*)(1+*a);
	    strcpy( (*a)->name, name);
	    (*a)->desc = strlen((*a)->name)+1+((*a)->name);
	    strcpy( (*a)->desc, desc );
	    return;
	}
    }
}


/* find a group by name */
struct newsgroup * findgroup(const char* name) {
    struct newsgroup * a;
    int c;

    a = active;
    while ( a ) {
	c = strcmp( a->name, name );
	if ( c < 0 )
	    a = a->left;
	else if ( c > 0 )
	    a = a->right;
	else 
	    return a;
    }
    return a;
}


static void freeactive( struct newsgroup * g ) {
    if ( g ) {
	freeactive( g->right );
	freeactive( g->left );
	free( (char*)g );
    }
}


/* this uses a nifty technique for building a binary tree from a
   sorted list... the basic observation is that if you count from one
   end, with 1 being the rightmost node, then the number of nodes
   between N and the leaf nodes is equal to the number of zero bits
   at the small end of N.  this tree probably doesn't make it obvious:

   1 2 3 4 5 6 7

         1
     1   0   1
   1 0 1 0 1 0 1
   | | | | | | + no zeroes: leaf node
   | | | | | +-- one zero: one step up
   | | | | +---- no zeroes: leaf node
   | | | +------ two zeroes: two steps up
   | | +-------- no zeroes: leaf node
   | +---------- one zero: one step up
   +------------ no zeroes: leaf node

   this can be used to build a smallest-height tree, and with a bit more
   care, a _perfectly_ balanced tree */

void readactive( void ) {
    char * p;
    char * q;
    int first, last, server;
    int fd;
    struct newsgroup * g;
    struct stat st;

    int line;
    struct newsgroup *levels[32]; /* theoretically finite size :) */
    int l;

    static char * stuff;

    if ( stuff ) {
	freeactive( active );
	active = NULL;
	free( (char*)stuff );
	stuff = NULL;
    }

    strcpy(s, libdir);
    strcat(s, "/groupinfo");
    fd = open( s, O_RDONLY );
    if ( stat( s, &st ) ) {
	syslog( LOG_ERR, "can't stat %s: %m", s );
	return;
    }
    stuff = critmalloc( st.st_size+1, "Reading group info" );
    if ( (fd=open( s, O_RDONLY))<0 ||
	 (read( fd, stuff, st.st_size ) < st.st_size) ) {
	syslog( LOG_ERR, "can't open/read %s: %m", s );
	return;
    } else {
	close( fd );
	stuff[st.st_size] = '\0'; /* 0-terminate string */
    }

    line = 1;
    for( l=0; l<32; levels[l++] = NULL )
	;

    p = stuff;
    while ( p && *p ) {
	q = p;
	while (p && *p && !isspace(*p))
	    p++;
	if (*p)
	    *p++ = '\0';
	if ( *q ) {
	    last = p ? strtol( p, &p, 10 ) : 0;
	    first = p ? strtol( p, &p, 10 ) : 0;
	    server = p ? strtol( p, &p, 10 ) : 0;
	    while (p && *p && isspace(*p) && p++)
		;
	    if (first < 1)
		first = 1;

	    g = (struct newsgroup *) critmalloc(sizeof(struct newsgroup),
						"reading groupinfo");
	    g->right = g->left = NULL;
	    g->first = first;
	    g->last = last;
	    g->server = server;
	    g->name = q;
	    g->desc = p;

	    for( l=0; (line&(1<<l))==0; l++ )
		;
	    if ( l ) {
		g->right = levels[l-1];
	    }
	    if ( active == g->right )
		active = g;
	    if ( l<31 && levels[l+1] && !levels[l+1]->left )
		levels[l+1]->left = g;
	    levels[l] = g;

	    p = strchr( p, '\n' );
	    if ( p )
		*p++ = '\0';
	    line++;
	} else {
	    p = strchr( p, '\n' );
	    if ( p )
		p++;
	}
    }
    g = NULL;
    for ( l=0; l<31; l++ ) {
	if ( levels[l] ) {
	    if ( g && levels[l] && 
		 levels[l]->left == NULL && levels[l]->right != g ) {
		levels[l]->left = g;
		g = NULL;
	    }
	    if ( !levels[l+1] || 
		 ( levels[l] != levels[l+1]->left &&
		   levels[l] != levels[l+1]->right ) ) {
		if ( g )
		    syslog( LOG_ERR, "2+5=2" );
		g = levels[l];
	    }
	}
    }
}


static void helpwriteactive ( struct newsgroup *g, FILE * f ) {
    if (g) {
	helpwriteactive (g->right, f);
	fprintf(f, "%s %d %d %d %s\n", g->name, g->last, g->first, g->server,
		g->desc && *(g->desc) ? g->desc : "-x-" );
	helpwriteactive (g->left, f);
    }
}    
    

void writeactive( void ) {
    FILE * a;
    char c[PATH_MAX];

    strcpy(s, libdir);
    strcat(s, "/groupinfo.new");
    a = fopen( s, "w" );
    if (!a)
	return;
    helpwriteactive( active, a );
    fclose( a );
    strcpy(c, libdir);
    strcat(c, "/groupinfo");
    rename( s, c );
}
	

/* get the fully qualified domain name of this box into fqdn */

void whoami( void ) {
    struct hostent * he;

    if (!gethostname(fqdn, 255) && (he = gethostbyname(fqdn))!=NULL) {
	strncpy( fqdn, he->h_name, 255 );
	if (strchr(fqdn, '.') == NULL) {
	    char ** alias;
	    alias = he->h_aliases;
	    while( alias && *alias )
		if (strchr(*alias, '.') && (strlen(*alias)>strlen(fqdn)))
		    strncpy( fqdn, *alias, 255 );
		else
		    alias++;
	    }
    } else
	*fqdn = '\0';
}

/* next few routines implement a mapping from message-id to article
   number, and clearing the entire space */

struct msgidtree {
    struct msgidtree * left;
    struct msgidtree * right;
    unsigned long int art;
    char msgid[1];
};

static struct msgidtree * head; /* starts as NULL */

void insertmsgid( const char * msgid, int art ) {
    struct msgidtree ** a;
    int c;

    if ( strchr( msgid, '@' ) == 0 )
	return;

    a = &head;
    while (a) {
	if (*a) {
	    /* comparing only by msgid is uncool because the tree becomes
	       very unbalanced */
	    c = strcmp(strchr((*a)->msgid, '@'), strchr(msgid, '@'));
	    if ( c == 0 )
		c = strcmp((*a)->msgid, msgid);
	    if (c<0)
		a = &((*a)->left);
	    else if (c>0)
		a = &((*a)->right);
	    else { 
		return;
	    }
	} else {
	    *a = (struct msgidtree *) 
		 critmalloc(sizeof(struct msgidtree) + strlen(msgid),
			    "Building expiry database");
	    (*a)->left = (*a)->right = NULL;
	    strcpy((*a)->msgid, msgid);
	    (*a)->art = art;
	    return;
	}
    }
}

unsigned long int findmsgid( const char* msgid ) {
    struct msgidtree * a;
    int c;
    char * domainpart;

    /* domain part differs more than local-part, so try it first */

    domainpart = strchr( msgid, '@' );
    if ( domainpart == NULL )
	return 0;

    a = head;
    while (a) {
	c = strcmp(strchr(a->msgid, '@'), domainpart);
	if ( c == 0 )
	    c = strcmp(a->msgid, msgid);
	if ( c < 0 )
	    a = a->left;
	else if ( c > 0 )
	    a = a->right;
	else 
	    return a->art;
    }
    return 0;
}

static void begone( struct msgidtree * m ) {
    if ( m ) {
	begone( m->right ) ;
	begone( m->left );
	free( (char *) m );
    }
}

void clearidtree( void ) {
    if ( head ) {
	begone( head->right ) ;
	begone( head->left );
	free( (char *) head );
    }
    head = NULL;
}


const char * upstream;
int artlimit = 0;
time_t expire;
struct supplement * supplement;

void readconfig( void ) {
    FILE * f;
    char * l;
    char * p;

    upstream = NULL;
    artlimit = 0;
    
    sprintf( s, "%s/config", libdir );
    if ( (f=fopen( s, "r" )) ) {
	while ( (l=getaline( f )) ) {
	    p = strchr( l, '#' );
	    if ( p )
		*p = '\0';
	    while ( isspace(*l) )
		l++;

	    if ( strncasecmp( l, "server", 6 )==0 ) {
		l += 6;
		while ( isspace(*l) )
		    l++;
		if ( *l=='=' ) {
		    l++;
		    while ( isspace(*l) )
			l++;
		    p = l;
		    while ( p && *p && !isspace(*p) )
			p++;
		    if ( p )
			*p = '\0';
		    upstream = strdup( l );
		} else {
		    syslog( LOG_ERR, 
			    "unable to parse %s (server line)", s );
		    exit( 2 );
		}

	    } else if ( strncasecmp( l, "expire", 6 )==0 ) {
		l += 6;
		while ( isspace(*l) )
		    l++;
		if ( *l=='=' ) {
		    unsigned long i;
		    l++;
		    while ( isspace(*l) )
			l++;
		    p = l;
		    while ( p && *p && !isspace(*p) )
			p++;
		    if ( p )
			*p = '\0';
		    i = strtoul( l, &p, 0 );
		    if ( l && *l && p && !*p )
			expire = time(NULL)-(time_t)(86400*i);
		    else {
			syslog( LOG_ERR,
				"unable to parse %s (expire number)", s );
			exit( 2 );
		    }
		} else {
		    syslog( LOG_ERR, 
			    "unable to parse %s (expire has no =)", s );
		    exit( 2 );
		}
	    } else if ( strncasecmp( l, "maxfetch", 8 )==0 ) {
		l += 8;
		while ( isspace(*l) )
		    l++;
		if ( *l=='=' ) {
		    unsigned long i;
		    l++;
		    while ( isspace(*l) )
			l++;
		    p = l;
		    while ( p && *p && !isspace(*p) )
			p++;
		    if ( p )
			*p = '\0';
		    i = strtoul( l, &p, 0 );
		    if ( l && *l && p && !*p )
			artlimit = i;
		    else {
			syslog( LOG_ERR,
				"unable to parse %s (maxfetch number)", s );
			exit( 2 );
		    }
		} else {
		    syslog( LOG_ERR, 
			    "unable to parse %s (maxfetch has no =)", s );
		    exit( 2 );
		}
	    } else if ( strncasecmp( l, "supplement", 10 )==0 ) {
		l += 10;
		while( isspace(*l) || *l=='=' )
		    l++;
		if ( isalpha( *l ) ) {
		    char * b = l;
		    while ( isalnum( *l ) || *l=='.' || *l=='-' || *l=='/' )
			l++;
		    if ( b != l ) {
			struct supplement * ns = (struct supplement *) critmalloc( sizeof(struct supplement) + (int)l-(int)b, "allocating space for supplementary server name" );
			*l = '\0';
			ns->next = supplement;
			strcpy( ns->name, b );
			supplement = ns;
		    } else {
			syslog( LOG_ERR, 
			    "impossibility 122", s );
			exit( 2 );
		    }
		} else {
		    syslog( LOG_ERR, 
			    "no host name in %s", s );
		    exit( 2 );
		}
	    }
	}
    } else {
	syslog( LOG_ERR, 
		"cannot open %s", s );
    }
}



/* return 1 if xover is a legal overview line, 0 else */

int legalxoverline ( const char * xover ) {
    const char * p;
    const char * q;

    return 1;

    if ( !xover )
	return 0;

    /* anything that isn't tab, printable ascii, or latin-* ? then killit */

    p = xover;
    while ( *p ) {
	int c = (unsigned char)*p++;

	if ( ( c != '\t' && c < ' ' ) || ( c > 126 && c < 160 ) ) {
	    if ( verbose > 2 )
		printf( "illegal character: %s\n", xover );
	    return 0;
	}
    }

    p = xover;
    q = strchr( p, '\t' );
    if ( !q )
	return 0;

    /* article number */

    while( p != q ) {
	if ( !isdigit( *p ) ) {
	    if ( verbose > 2 )
		printf( "non-digit in article number: %s\n", xover );
	    return 0;
	}
	p++;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before subject: %s\n", xover );
	return 0;
    }

    /* subject: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before from: %s\n", xover );
	return 0;
    }
    if ( !q )
	return 0;

    /* from: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before date: %s\n", xover );
	return 0;
    }
    if ( !q )
	return 0;

    /* date: no limitations */

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before message-id: %s\n", xover );
	return 0;
    }
    if ( !q )
	return 0;

    /* message-id: <*@*> */

    if ( *p != '<' ) {
	if ( verbose > 2 )
	    printf( "message-id does not start with '<': %s\n", xover );
	return 0;
    }
    while ( p != q && *p != '@' && *p != '>' && *p != ' ' )
	p++;
    if ( *p != '@' ) {
	if ( verbose > 2 )
	    printf( "message-id does not contain '@': %s\n", xover );
	return 0;
    }
    while ( p != q && *p != '>' && *p != ' ' )
	p++;
    if ( *p != '>' ) {
	if ( verbose > 2 )
	    printf( "message-id does not contain '>': %s\n", xover );
	return 0;
    }
    if ( ++p != q ) {
	if ( verbose > 2 )
	    printf( "message-id does not end with '>': %s\n", xover );
	return 0;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before references: %s\n", xover );
	return 0;
    }
    if ( !q )
	return 0;

    /* references: a series of <*@*> */

    while ( p != q ) {
	if ( *p != '<' ) {
	    if ( verbose > 2 )
		printf( "id in references does not start with '<': %s\n",
			xover );
	    return 0;
	}
	while ( p != q && *p != '@' && *p != '>' && *p != ' ' )
	    p++;
	if ( *p != '@' ) {
	    if ( verbose > 2 )
		printf( "id in references does not contain '@': %s\n",
			xover );
	    return 0;
	}
	while ( p != q && *p != '>' && *p != ' ' )
	    p++;
	if ( *p++ != '>' ) {
	    if ( verbose > 2 )
		printf( "id in references does not end with '>': %s\n",
			xover );
	    return 0;
	}
	while ( p != q && *p == ' ' )
	    p++;
    }

    p = q+1;
    q = strchr( p, '\t' );
    if ( !q ) {
	if ( verbose > 2 )
	    printf( "premature end before byte count: %s\n", xover );
	return 0;
    }
    if ( !q )
	return 0;

    /* byte count */

    while( p != q ) {
	if ( !isdigit( *p ) ) {
	    if ( verbose > 2 )
		printf( "illegal digit in byte count: %s\n", xover );
	    return 0;
	}
	p++;
    }

    p = q+1;
    q = strchr( p, '\t' );

    /* line count */

    while( p && *p && p != q ) {
	if ( !isdigit( *p ) ) {
	    if ( verbose > 2 )
		printf( "illegal digit in line count: %s\n", xover );
	    return 0;
	}
	p++;
    }

    return 1;
}



void stripspace( char * p );
void stripspace( char * p ) {
    char * p1;
    char * p2;

    p1 = p2 = p;
    while (p1 && *p1) {
	if ( isspace(*p1) ) {
	    *p2 = ' ';
	    do {
		p1++;
	    } while ( isspace(*p1) );
	    if ( *p1 )
		p2++;
	} else {
	    *p2++ = *p1++;
	}
    }
    *p2 = '\0';
}




char * getxoverline( const char * filename );
char * getxoverline( const char * filename ) {
    char * l;
    FILE * f;

    if ( (f=fopen( filename, "r" )) ) {
	char * from;
	char * subject;
	char * date;
	char * msgid;
	char * references;
	int bytes;
	int lines;
	char * xref;
	char ** h;
	int body;

	from = subject = date = msgid = references = xref = NULL;
	bytes = lines = 0;
	h = NULL;
	body = 0;

	do {
	    l = getaline( f );
	    if ( l ) {
		lines++; /* most luxurious - but it doesn't happen often */
		bytes += strlen( l ) + 2;
	    }
	    if ( body || !l ) {
		/* nothing */
	    } else if ( !body && !*l ) {
		lines = 0;
		body = 1;
	    } else if ( h && isspace( *l ) ) {
		stripspace( l );
		if ( *l ) {
		    (*h) = critrealloc( *h, strlen( *h ) + strlen( l ) + 1,
					"extending header" );
		    strcat( *h, l );
		}
	    } else if ( !from && !strncasecmp( "From: ", l, 6 ) ) {
		l += 6;
		stripspace( l );
		if ( *l ) {
		    from = strdup( l );
		    h = &from;
		}
	    } else if ( !subject && !strncasecmp( "Subject: ", l, 9 ) ) {
		l += 9;
		stripspace( l );
		if ( *l ) {
		    subject = strdup( l );
		    h = &subject;
		}
	    } else if ( !date && !strncasecmp( "Date: ", l, 6 ) ) {
		l += 6;
		stripspace( l );
		if ( *l ) {
		    date = strdup( l );
		    h = &date;
		}
	    } else if ( !msgid && !strncasecmp( "Message-ID: ", l, 12 ) ) {
		l += 12;
		stripspace( l );
		if ( *l ) {
		    msgid = strdup( l );
		    h = &msgid;
		}
	    } else if ( !references && 
			!strncasecmp( "References: ", l, 12 ) ) {
		l += 12;
		stripspace( l );
		if ( *l ) {
		    references = strdup( l );
		    h = &references;
		}
	    } else if ( !xref && !strncasecmp( "Xref: ", l, 6 ) ) {
		l += 6;
		stripspace( l );
		if ( *l ) {
		    xref = strdup( l );
		    h = &xref;
		}
	    }
	} while ( l && !feof(f) );
	if ( from && date && subject && msgid && bytes && lines && body ) {
	    char * result;

	    result = critmalloc( strlen( from ) + strlen( date ) +
				 strlen( subject ) + strlen( msgid ) +
				 (references ? strlen( references ) : 0 ) +
				 100 + ( xref ? strlen( xref) : 0 ),
				 "computing overview line" );
	    sprintf( result, "%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d",
		     filename, subject, from, date, msgid,
		     references ? references : "" ,
		     bytes, lines );
	    if ( xref ) {
		strcat( result, "\tXref: " );
		strcat( result, xref );
	    }
	    free ( from );
	    free ( date );
	    free ( subject );
	    free ( msgid );
	    free ( references );
	    free ( xref );
	    fclose ( f );
	    return result;
	} else {
	    fclose ( f );
	}
	free ( from );
	free ( date );
	free ( subject );
	free ( msgid );
	free ( references );
	free ( xref );
    }
    return NULL;
}


/* utility routine to pull the xover info into memory
   returns 0 if there's some error, non-zere else */

struct xoverinfo * xoverinfo;
int xfirst, xlast;

int getxover( void ) {
    DIR *d;
    struct dirent * de;
    int fd;
    struct stat st;
    int art;
    static char * overview;
    int error;
    char * p, * q;

    /* free any memory left over from last time */

    error = 0;

    if ( xoverinfo ) {
	for( art=xfirst; art<=xlast; art++ ) {
	    if ( xoverinfo[art-xfirst].mallocd &&
		 xoverinfo[art-xfirst].text ) {
		free( xoverinfo[art-xfirst].text );
		xoverinfo[art-xfirst].text = NULL;
	    }
	}
    }

    if ( overview ) {
	free( overview );
	overview = NULL;
    }

    /* find article range, without locking problems */

    d = opendir( "." );
    if ( !d ) {
	syslog( LOG_ERR, "opendir: %m" );
	return 0;
    }


    xfirst = INT_MAX;
    xlast = 0;
    while ( (de=readdir(d)) ) {
	art = strtol( de->d_name, &p, 10 );
	if (p && !*p) {
	    if ( art < xfirst )
		xfirst = art;
	    if ( art > xlast )
		xlast = art;
	}
    }
    if ( xlast < xfirst ) {
	closedir( d );
	return 0;
    }

    /* next, read .overview, correct it if it seems too different from
       what the directory implies, and write the result back */

    rewinddir( d );

    xoverinfo = (struct xoverinfo *)
		critrealloc( (char*)xoverinfo,
			     sizeof(struct xoverinfo) * (xlast+1-xfirst),
			     "allocating overview array" );
    memset( xoverinfo, 0, sizeof(struct xoverinfo) * (xlast+1-xfirst) );

	if ( (fd=open( ".overview", O_RDONLY)) >= 0 &&
	     fstat( fd, &st )==0 &&
	     (overview=(char*)malloc( st.st_size + 1 )) != NULL &&
	 read( fd, overview, st.st_size ) == st.st_size ) {

	close( fd );
	overview[st.st_size] = '\0';

	/* okay, we have the content, so let's parse it roughly */

	p = overview;
	while ( p && *p ) {
	    while ( p && isspace( *p ) )
		p++;
	    q = strchr( p, '\n' );
	    if ( q )
		*q++ = '\0';

	    art = strtol( p, NULL, 10 );
	    if ( art > xlast || art < xfirst ) {
		error++;
	    } else if ( xoverinfo[art-xfirst].text ) {
		error++;
		xoverinfo[art-xfirst].text = NULL;
	    } else {
		xoverinfo[art-xfirst].text = p;
	    }

	    p = q;
	}
    }

    /* so, what was missing? */

    while ( (de=readdir(d)) ) {
	art = strtol( de->d_name, &p, 10 );
	if (p && !*p && art>=xfirst && art<=xlast) {
	    if ( !xoverinfo[art-xfirst].text ) {
		if ( verbose > 2 )
		    printf( "reading XOVER info from %s/%s\n",
			    getcwd( s, 1024 ), de->d_name );
		error++;
		if ( (xoverinfo[art-xfirst].text = 
		      getxoverline( de->d_name )) != NULL &&
		     legalxoverline( p ) ) {
		    xoverinfo[art-xfirst].mallocd = 1;
		} else {
		    printf( "illegal article: %s/%s\n", 
			    getcwd( s, 1024 ), de->d_name );
		    (void) unlink( de->d_name ); /* most dubious, this */
		}
	    }
	}
	if ( art >= xfirst && art <= xlast && xoverinfo[art-xfirst].text ) {
	    xoverinfo[art-xfirst].exists = 1;
	} else if ( verbose ) {
	    if ( art == 0 && strcmp( de->d_name, ".overview" ) &&
		 strcmp( de->d_name, "." ) && strcmp( de->d_name, ".." ) ) {
		if ( verbose && unlink( de->d_name ) &&
		     errno != EISDIR && errno != EPERM )
		    printf( "attempted to delete %s in case it's junk: %s\n",
			    de->d_name, errno ? strerror(errno) : "OK" );
	    } else if ( art > 0 && art < xfirst )
		printf( "article %d is below the low-water mark (%d)\n",
			art, xfirst );
	    else if ( art > xlast )
		printf( "article %d is above the high-water mark (%d)\n",
			art, xlast );
	    else if ( p && !*p )
		printf( "article %d contained illegal headers\n",
			art );
	}
    }

    /* if something had to be fixed, write a better file to disk for
       next time - race conditions here, but none dangerous */

    if ( error ) {
	int wfd;
	char newfile[20];

	if ( verbose )
	    printf( "corrected %d lines in %s/.overview\n",
		    error, getcwd( s, 1024 ) );

	strcpy( newfile, ".overview.XXXXXX" );
	if ( (wfd=open( mktemp(newfile), O_WRONLY|O_CREAT|O_EXCL, 0664 )) ) {
	    struct iovec oooh[UIO_MAXIOV];
	    int vi, vc, va;

	    vi = vc = va = 0;
	    for ( art=xfirst; art<=xlast; art++ ) {
		if ( xoverinfo[art-xfirst].exists &&
		     xoverinfo[art-xfirst].text ) {
		    oooh[vi].iov_base = xoverinfo[art-xfirst].text;
		    oooh[vi].iov_len = strlen( xoverinfo[art-xfirst].text );
		    vc += oooh[vi++].iov_len + 1;
		    oooh[vi].iov_base = "\n";
		    oooh[vi++].iov_len = 1;
		    if ( vi >= (UIO_MAXIOV - 1) ) {
			if ( writev( wfd, oooh, vi ) != vc ) {
			    syslog( LOG_ERR,
				    "writev() for .overview failed: %m" );
			    art = xlast+1; /* so the loop will stop */
			}
			vi = vc = 0;
			va = 1;
		    }
		}
	    }
	    if ( vi ) {
		if ( writev( wfd, oooh, vi ) != vc ) {
		    syslog( LOG_ERR, "writev() for .overview failed: %m" );
		} else {
		    va = 1;
		}
	    }
	    fchmod( wfd, 0664 );
	    close( wfd );
	    if ( va ) {
		if ( rename( newfile, ".overview" ) ) {
		    if ( unlink( newfile ) )
			syslog( LOG_ERR,
				"rename() and unlink() both failed: %m" );
		    else
			syslog( LOG_ERR,
				"rename(%s/%s, .overview) failed: %m",
				getcwd(s, 1024), newfile );
		} else {
		    if ( verbose )
			printf( "wrote %s/.overview\n",
				getcwd( s, 1024 ) );
		}
	    } else {
		unlink( newfile );
		/* the group must be newly empty: I want to keep the old
		   .overview file I think */
	    }
	} else {
	    syslog( LOG_ERR, "open(O_WRONLY|O_CREAT|O_EXCL) of new "
		    ".overview failed: %m" );
	}
    }
    return 1;
}


#ifndef HAVE_STRDUP
char * strdup(const char *s) {
    char * s1;
    s1 = malloc(strlen(s)+1);
    if (!s1)
	return 0;
    strcpy (s1,s);
    return s1;
}
#endif
