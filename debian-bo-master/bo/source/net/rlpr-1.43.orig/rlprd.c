/* filename: rlprd.c
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlprd.c,v 1.13 1997/01/16 16:44:11 meem Exp $
 * contents: daemon which does "reflecting" of messages from non-privileged
 *           to privileged ports
 *
 * Time-stamp: <1997/01/16 10:40 -- meem@sherilyn.wustl.edu>
 */

/* copyright (c) 1996, 1997 meem, meem@gnu.ai.mit.edu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include "config.h"

#include <sys/types.h>          /* for pid_t */
#include <signal.h>

#ifndef STDC_HEADERS
#error there is currently no support for compiling on machines without ANSI headers. \
       write meem@gnu.ai.mit.edu if you are interested in adding it.
#endif

#ifdef __GNU_LIBRARY__                    
#undef __GNU_LIBRARY__
#include "getopt.h"
#define __GNU_LIBRARY__
#else  /* not __GNU_LIBRARY__ */
#include "getopt.h"
#endif /* not __GNU_LIBRARY__ */

#include "wait.h"

#include <syslog.h>                   /* for openlog() (for now) */
#include <stdlib.h>                   /* for EXIT_* constants */
#include <netdb.h>                    /* for gethostbyname() */
#include <sys/time.h>                 /* select() and macros */
#include <sys/stat.h>                 /* for umask() */
#include <unistd.h>                   /* for setsid() */
#include <string.h>                   /* for strerror() */
#include <errno.h>
#include <stdio.h>                    /* BUFSIZ */
#include <netinet/in.h>               /* for sockaddr_in */
#include <stdarg.h>

#include "rlprd.h"
#include "rlpr-common.h"

static daemon_options d_opts_;

char * name;                          /* program name */

int main(int argc, char *argv[]) {
  struct sockaddr_in sin_in;          /* where we will listen */
  struct sockaddr_in sin_out;         /* our projected identity (to lpd) */
  struct sockaddr_in sin_lpd;         /* the connection we will send to lpd */  
  struct sockaddr_in sin_from;        /* the connection that came in */
  int    inc_rlprd, out_lpd;          /* socket descriptors */
  int    listen_fd;                   /* passive descriptor listening connxns */
  int    sin_fromlen = sizeof sin_from; /* the length of the incoming desc */
  char   printhost[MAXHOSTNAMELEN + 1]; /* name of printhost */
  char   localhost[MAXHOSTNAMELEN + 1]; /* for finding out our name */
  pid_t  childpid;                    /* for fork() */
  int    orig_argc = argc;            /* original # args */

  name = *argv;

  argc -= parse_args(orig_argc, argv);
  argv += orig_argc - argc;

  if (geteuid() != 0)
    rlpr_msg(FATAL, NO_ERRNO, "must be run root or setuid root!");

  toggle_euid();                          /* lose root */

  if (d_opts_.daemon) daemon_init();      /* if daemon, do the magic */

  if (get_local_hostname(localhost, sizeof(localhost)) == -1)
    rlpr_msg(FATAL, NO_ERRNO, "unable to resolve your local hostname!");

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
    rlpr_msg(FATAL, ERRNO, "socket");
  
  /* initialize and fill in what we can up front */
  
  init_sockaddr(&sin_in,   NULL,      d_opts_.port); 
  init_sockaddr(&sin_out,  localhost, 0); 
  init_sockaddr(&sin_from, NULL,      0);

  sin_in.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd,  (struct sockaddr *) &sin_in,  sizeof(sin_in)) < 0)
    rlpr_msg(FATAL, ERRNO, "bind to port %hi failed", d_opts_.port);
  
  register_sigchld();               /* register the reaper */
  register_sigalrm();               /* register the alarm */

  rlpr_msg(DEBUG, NO_ERRNO, "listening on port %hi...", d_opts_.port);
  listen(listen_fd, 5);             /* don't rely on more than 5 */

  for (;;) {

    inc_rlprd = accept(listen_fd, (struct sockaddr *) &sin_from, &sin_fromlen);
    if (inc_rlprd < 0) {
#ifndef SA_RESTART
      if (errno == EINTR) continue;
#endif  /* NOT SA_RESTART */
      rlpr_msg(WARNING, ERRNO, "accept");
      continue;
    }

    rlpr_msg(DEBUG, NO_ERRNO, "accepted incoming connection");

    switch (fork()) {
    case  0: break;
    case -1: rlpr_msg(FATAL, ERRNO, "fork");
    default: close(inc_rlprd);                           /* parent */
             continue;
    }

    /* CHILD */

    alarm(d_opts_.timeout);   /* set timeout */
    
    if (readstr(inc_rlprd, printhost, MAXHOSTNAMELEN) < 0)
      rlpr_msg(FATAL, NO_ERRNO, "unable to read proxy hostname");     

    rlpr_msg(INFO, NO_ERRNO,
             "proxy from %s to %s", hostname(&sin_from), printhost);

    /* bind our local socket so we come from a privileged port */

    if ((out_lpd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
      rlpr_msg(FATAL, ERRNO, "socket");

    toggle_euid();                            /* gain root */

    if (bind_try_range(&sin_out, LO_LPD_FROM_PORT, HI_LPD_FROM_PORT, out_lpd) < 0)
      rlpr_msg(FATAL, ERRNO,
               "bind to ports %hi-%hi", LO_LPD_FROM_PORT, HI_LPD_FROM_PORT);

    toggle_euid();                            /* lose root */
      
    /* initialize sin_lpd and connect to the host running lpd */

    init_sockaddr(&sin_lpd, printhost, LPD_TO_PORT);

    if (connect(out_lpd, (struct sockaddr *) &sin_lpd, sizeof(sin_lpd)) < 0)
      rlpr_msg(FATAL, ERRNO, "connect");

    /* converse */
    converse(inc_rlprd, out_lpd);
  
    exit(EXIT_SUCCESS);
  }

  exit(EXIT_SUCCESS);
}

void converse(int client, int server) {
  int         maxfd = (client > server ? client : server) + 1;
  static char buf[BUFSIZ * 4];        /* could be a more optimal size */  
  int         nc, nfds;               /* data moved in read/write */
  fd_set      readfds;                /* read file descriptor set */

  for (;;) {                  

    FD_ZERO(&readfds);                      /* initialize fds structure */
    FD_SET(client, &readfds);
    FD_SET(server, &readfds);

    if ((nfds = select(maxfd, &readfds, NULL, NULL, NULL)) < 0)
      rlpr_msg(FATAL, ERRNO, "select");
        
    if (nfds)
      if (FD_ISSET(client, &readfds)) { 
        /* data from rlpr client */
        
        if ((nc = read(client, buf, sizeof buf)) < 0)
          rlpr_msg(FATAL, ERRNO, "read from rlpr client");
        else if (!nc) break;
        else writen(server, buf, nc);

      } else {
        /* data from lpd server */

        if ((nc = read(server, buf, sizeof buf)) < 0)
          rlpr_msg(FATAL, ERRNO, "write to lpd server");
        else if (!nc) break;
        else writen(client, buf, nc);
      }
  }
}

/* daemon_init()
 *
 * purpose: initialize the server to run in daemon mode.
 *   input: void
 *  output: void
 */ 

static void daemon_init(void) {

  if (getppid() > 1) {  
    /* we weren't started by init.  let's fork ourselves silly */
    
    /* turn on the logger */
#ifdef HAVE_SYSLOG_H
    openlog(name, LOG_PID, LOG_LPR);
#endif /* HAVE_SYSLOG_H */
    props_.syslog = 1;

    switch (fork()) {
      case  0: break;
      case -1: rlpr_msg(FATAL, ERRNO, "fork");
      default: exit(EXIT_SUCCESS);
    }

    /* child */

    setsid();                   /* now unique session, PGID, no ctty */

    /* fork again to make sure we don't reacquire a ctty */

    switch (fork()) {
      case  0: break;
      case -1: rlpr_msg(FATAL, ERRNO, "fork");
      default: exit(EXIT_SUCCESS);
    }

    /* second child */
  }

  chdir("/");                   /* in case we were on a mounted partition */
  umask(0);                     /* clear file mode creation mask */
}

static const char * hostname(struct sockaddr_in *sin) {
  struct hostent *hp;
  hp = gethostbyaddr((char *) &sin->sin_addr, sizeof(struct in_addr), AF_INET);
  if (!hp) rlpr_msg(FATAL, NO_ERRNO, "gethostbyaddr failed: %s", h_strerror());
  return hp->h_name;
}
     

static RETSIGTYPE sigchld(int unused) {
  int olderrno = errno;
  while (waitpid( -1, NULL, WNOHANG) > 0) /* NULL BODY */;
  errno = olderrno;
}

static RETSIGTYPE sigalrm(int unused) {
  rlpr_msg(FATAL, NO_ERRNO, "connection timed out");
}

/* register_sigchld()
 *
 * purpose: to register the SIGCHLD signal with the reaper
 *   input: void
 *  output: void
 */ 

static void register_sigchld(void) {
  struct sigaction sa;
  sa.sa_handler = sigchld;
  sa.sa_flags   = 0;
#ifdef SA_RESTART
  sa.sa_flags  |= SA_RESTART;
#endif /* SA_RESTART */
  sigemptyset(&sa.sa_mask);

  if (sigaction(SIGCHLD, &sa, NULL) == -1)
    rlpr_msg(FATAL, ERRNO, "sigaction");
}

static void register_sigalrm(void) {
  if (signal(SIGALRM, sigalrm) == SIG_ERR)
    rlpr_msg(FATAL, ERRNO, "signal");
}

/* read at most bufsz characters into buf, stopping at the first \n */

static int readstr(int fd, char * buf, int bufsz) {
  int i;
  
  for (i = 0; i < bufsz; i++) {
    if (read(fd, &buf[i], 1) < 0) return -1;
    if (buf[i] == '\n') break;
  }
  buf[i] = '\0';
  return i;
}

/* parse_args()
 *
 * purpose: to handle command line arguments
 *   input: argc  the number of elements in argv
 *          argv  an array of command line arguments
 *  output: void
 */ 

static int parse_args(int argc, char *argv[]) {
  int c;
  int num_args = 1;

  static struct option long_opts[] = {
    { "debug",        0, NULL, 'd' },
    { "no-daemon",    0, NULL, 'n' },
    { "port",         1, NULL, 'p' },
    { "quiet",        0, NULL, 'q' },
    { "silent",       0, NULL, 'q' },
    { "timeout",      1, NULL, 't' },
    { "version",      0, NULL, 'V' },
    { 0, 0, 0, 0 }
  };

  init_options();
  

  while ((c = getopt_long(argc, argv, "np:qtvV", long_opts, NULL)) != EOF) {
    num_args++;
    switch(c) {
    case 'd':
      props_.debug    = 1;
      break;
    case 'n':
      d_opts_.daemon  = 0;
      break;
    case 'p':
      d_opts_.port    = atoi(optarg);
      break;
    case 'q':
      props_.quiet    = 1;
      break;
    case 't':
      d_opts_.timeout = atoi(optarg);
      break;
    case 'V':
      fprintf(stdout, "%s: version "VERSION" from "__DATE__" "__TIME__ 
              " -- meem@gnu.ai.mit.edu\n", name);
      exit(EXIT_SUCCESS);
    case '?':
      fprintf(stderr,"please see manpage for help\n");
      break;
    default:
      break; /* just in case */
    }
  }
  return num_args;
}


/* init_options()
 *
 * purpose: to provide reasonable defaults for the d_opts_ struct
 *   input: void (uses evil global variables)
 *  output: void
 */ 

void init_options(void) {
  props_.debug    = 0;
  props_.quiet    = 0;
  props_.syslog   = 0;
  d_opts_.port    = DEFAULT_RLPRD_TO_PORT;
  d_opts_.timeout = DEFAULT_TIMEOUT;
  d_opts_.daemon  = 1;          /* daemon on */
}
