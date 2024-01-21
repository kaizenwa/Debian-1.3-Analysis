/* $Id: leafnode.h,v 1.8 1996/06/23 21:33:53 agulbra Exp $ */

#ifndef LEAFNODE_H
#define LEAFNODE_H

/* I wish the world were a happy place */
#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#include <errno.h>
#include <sys/errno.h>

/* uncomment this if you get errors 

extern     int sys_nerr;
extern     char *sys_errlist[];
extern     int errno;

also complain to your vender */

#define HAVE_STRDUP

#ifdef ultrix
#undef HAVE_STRDUP
#endif

#ifndef HAVE_STRDUP
char * strdup (const char *);
#endif


/* limits.h is supposed to contain PATH_MAX, we include sys/param.h too */
#include <limits.h>
#ifndef PATH_MAX
#include <sys/param.h>
#define PATH_MAX MAXPATHLEN
#endif

/* add LOG_NEWS where it doesn't exist */
#include <syslog.h>
#if !defined( LOG_NFACILITIES )
#error "Leafnode does not compile with BSD 4.2 syslog"
#endif
#if !defined( LOG_NEWS )
#define LOG_NEWS LOG_DAEMON
#endif
#if !defined( LOG_CONS )
#define LOG_CONS 0 /* if it isn't supported, make do without */
#endif

#include <stdio.h> /* FILE * */


/* don't fetch threads that nobody've read in this many seconds */
#define TIMEOUT_LONG 604800
/* don't fetch threads that have been "read" only once in this many seconds */
#define TIMEOUT_SHORT 172800

/* verbosity level, for fetch and later texpire */
extern int verbose;

/* handy malloc wrappers from util.c */

char * critmalloc(size_t size, const char* message);
char * critrealloc(char *a, size_t size, const char* message);

/* converts a message-id to a file name, the return value points into
   a static array */
const char * lookup (const char *);
/* reads one line, regardless of length */
char * getaline(FILE *f);

/* changes (and optionally creates) directory */
void chdirgroup(const char *group);

/* newsgroup management */

struct newsgroup {
    struct newsgroup * left;
    struct newsgroup * right;
    int first;
    int last;
    int server;
    int alive;
    char * desc;
    char * name;
};


void insertgroup(const char * name, 
			int first, int last, 
			const char * desc, int server );
struct newsgroup * findgroup(const char* name);
void readactive( void );
void writeactive( void );

extern struct newsgroup * active;

/* supplementary servers */

struct supplement {
    struct supplement * next;
    char name[1];
};

extern struct supplement * supplement;

/* translation from message-id to article number, used in fetch and expire */
void clearidtree( void );
void insertmsgid( const char * msgid, int art );
unsigned long int findmsgid( const char* msgid );

/* xover stuff */

struct xoverinfo {
    char * text;
    int mallocd;
    int exists;
};

extern struct xoverinfo * xoverinfo;
extern int xfirst;
extern int xlast;

/* return 1 if xover is a legal overview line, 0 else */
int legalxoverline( const char * xover );
/* set xoverinfo, return 0 if there's an error, nonzero else */
int getxover( void );

/* spool and library directories, from util.c */

extern const char * spooldir;
extern const char * libdir;
extern char s[];

/* my name, and my naming myself */
extern char fqdn[];
void whoami( void );


extern FILE *nntpin;
extern FILE *nntpout;

/*
 * decode an NNTP reply number
 * reads a line from the server and returns an integer
 *
 * 498 is used to mean "protocol error", like smail
 *
 * the text returned is discarded
 */

int nntpreply(void);

/*
 * connect to upstream nntp server
 *
 * returns non-zero if a connection could be established
 */
int nntpconnect( void );

/* upstream news server */
extern const char * upstream;
/* max number of articles to read on one group in one go */
extern int artlimit;
/* articles not read or written since this time get deleted */
extern time_t expire;

/* read the config file */
void readconfig( void );

#endif
