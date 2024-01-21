/* filename: rlprd.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlprd.h,v 1.3 1996/12/20 18:12:16 meem Exp $
 * content: general definitions/declarations for rlprd.c
 *
 * Time-stamp: <1996/11/17 02:26 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPRD_H
#define RLPRD_H

/* function prototypes */

typedef struct daemon_options {
  u_short   port;               /* port number to listen on */
  int       timeout;            /* how long before giving up on a client */
  int       daemon:1;           /* user requested daemon */
} daemon_options;

#define DEFAULT_TIMEOUT  20     /* seconds */

static void         daemon_init(void);
static void         register_sigchld(void);
static void         register_sigalrm(void);
static RETSIGTYPE   sigchld(int unused);
static RETSIGTYPE   sigalrm(int unused);
static const char * hostname(struct sockaddr_in *sin);
static int          parse_args(int argc, char *argv[]);
static void         init_options(void);
static int          readstr(int fd, char * buf, int bufsz);
static void         converse(int client, int server);
#endif /* RLPRD_H */
