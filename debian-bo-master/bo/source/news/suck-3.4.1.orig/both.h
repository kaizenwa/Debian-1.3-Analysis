#ifndef _SUCK_BOTH_H
#define _SUCK_BOTH_H 1

#include <netdb.h> /* for struct hostent */
#include "suck_config.h" /* for debug etc stuff */

/* declarations */
int sgetline(int fd, char **sbuf);
int sputline(int fd, const char *outbuf);
int connect_to_nntphost(const char *host, struct hostent **, FILE *, unsigned short int);
char *number(char *sp, int *intPtr);
struct hostent *get_hostent(const char *host);
void signal_block(int);
void error_log(int mode, const char *fmt, ...);
void MyPerror(const char *);
void free_args(int, char *[]);
char **build_args(const char *, int *);
char **read_array(FILE *, int, int);
void free_array(int, char **);
char *do_a_phrase(char []);
void print_phrases(FILE *, const char *, ...);
char *str_int(int);

enum { MYSIGNAL_SETUP, MYSIGNAL_BLOCK, MYSIGNAL_UNBLOCK, MYSIGNAL_ADDPIPE };
enum { ERRLOG_SET_FILE, ERRLOG_SET_STDERR, ERRLOG_REPORT };

#define SOCKET_PROTOCOL 0	/* so testhost.c can get at it */

/* debugging options. no need to change it */
#ifdef DEBUG1
#define DEBUG 1
#endif

#ifdef DEBUG2
#define DEBUG 1
#define N_DEBUG "debug.suck"
#endif

#ifdef DEBUG3
#define N_DEBUG "debug.rpost"
#define DEBUG 1
#endif

#ifdef DEBUG
void do_debug(const char *fmt, ...);
#ifndef N_DEBUG 
#define N_DEBUG "debug.both"
#endif
#endif

#ifndef FALSE
#define FALSE 0
#define TRUE !FALSE
#endif
#endif /* _SUCK_BOTH_H */
