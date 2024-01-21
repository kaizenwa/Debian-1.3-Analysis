/* filename: rlpr-net.c
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-net.c,v 1.19 1997/01/14 01:10:18 meem Exp $
 * contents: network-related parts of rlpr (all the socket-based functions)
 *
 * Time-stamp: <1997/01/13 19:07 -- meem@sherilyn.wustl.edu>
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

#include <sys/types.h>  
#include <netinet/in.h>               /* struct socketaddr_in definition */
#include <sys/stat.h>
#include <fcntl.h>
#include <netdb.h>                    /* network-specific functions here */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>                 /* for timeval in select() calls */
#include <unistd.h>                   /* for unlink(), etc */
#include <errno.h>
#include "rlpr-common.h"
#include "rlpr-net.h"
#include "rlpr-client.h"              /* common to parts of the rlpr client */

/* LOCAL FUNCTIONS */
static void get_and_verify_ack(int sock, char *caller);

int open_connection(void)
{
  struct sockaddr_in sin_local;       /* to bind a socket to a number */
  int                sockfd;          /* returned open socket */
  
  init_sockaddr(&sin_local, net_.localhost, 0);
  
  /* obtain a socket descriptor */
  if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) /* 0 == auto proto config */
    rlpr_msg(FATAL, ERRNO, "socket");

  /* if we're going to connect to lpd, we need to come from a privileged port */

  if (net_.proxyhost == NULL) {
    toggle_euid();                    /* gain root */

    /* see if we're root. if we're not, give up */
    if (geteuid() != 0)
      rlpr_msg(FATAL, NO_ERRNO, 
               "%s isn't setuid root - if you cannot install it setuid root,\n"
               "please read %s(1) about using the rlprd proxy agent", name, name);
    
    if (bind_try_range(&sin_local, LO_LPD_FROM_PORT, HI_LPD_FROM_PORT, sockfd) < 0)
      rlpr_msg(FATAL, ERRNO, 
               "bind to ports %hi-%hi", LO_LPD_FROM_PORT, HI_LPD_FROM_PORT);

    toggle_euid();                    /* lose root */
  }

  { /* we either want to connect to the proxy or the actual lpd. we
     * will try to connect to the machine specified by the RLPR_PROXYHOST
     * variable if it's set
     */

    struct sockaddr_in sin_to;        /* who we actually connect() to */

    if (net_.proxyhost == NULL) {
      init_sockaddr(&sin_to, net_.printhost, LPD_TO_PORT);
      rlpr_msg(DEBUG, NO_ERRNO, "connecting to printhost %s...", net_.printhost);
    }
    else /* USE PROXY */ {
      init_sockaddr(&sin_to, net_.proxyhost, net_.port);
      rlpr_msg(DEBUG, NO_ERRNO, "connecting to proxy %s...", net_.proxyhost);
    }

    if (connect(sockfd, (struct sockaddr *) &sin_to, sizeof(sin_to)) < 0)
      rlpr_msg(FATAL, ERRNO, "connect to port %hi", ntohs(sin_to.sin_port));

    rlpr_msg(DEBUG, NO_ERRNO, "connected");
    
    if (net_.proxyhost) {       /* if proxy, send out final destination */
      rlpr_msg(DEBUG, NO_ERRNO, "directing proxy to %s..", net_.printhost);
      safe_writen(sockfd, net_.printhost, strlen(net_.printhost));
      safe_writen(sockfd, "\n", 1);
    }
  }
  return sockfd;
}

void close_connection(int sockfd)
{
  if (close(sockfd) < 0)
    rlpr_msg(FATAL, ERRNO, "close on socket connection");
  rlpr_msg(DEBUG, NO_ERRNO, "connection closed");
}

void send_recvj_req(int sockfd)
{
  char c = RECVJ;

  rlpr_msg(DEBUG, NO_ERRNO, "sending job request to lpd...");

  safe_writen(sockfd, &c, 1);
  safe_writen(sockfd, opts_.printer, strlen(opts_.printer));
  safe_writen(sockfd, "\n", 1);
  get_and_verify_ack(sockfd, __FUNCTION__);

  rlpr_msg(DEBUG, NO_ERRNO, "job request sent successfully");
}

static void get_and_verify_ack(int sockfd, char *caller)
{
  char ack;
  
  if (read(sockfd, &ack, 1) < 0)      /* check acknolwedgement */
    rlpr_msg(FATAL, ERRNO, "%s: while reading ack from lpd", caller);

  if (ack != 0)
    rlpr_msg(FATAL, NO_ERRNO, 
             "%s: lpd refused -- are we in its /etc/hosts.lpd?", caller);
}

void send_cf(int sockfd, int cfd, char *cfname)
{
  char  buf[BUFSIZ];            /* this is okay: user cannot overflow it */
  off_t sz;                     /* cfd size */

  rlpr_msg(DEBUG, NO_ERRNO, "sending control file...");

  if (lseek(cfd, 0, SEEK_SET) < 0)
    rlpr_msg(FATAL, ERRNO, "lseek on control file");

  /* send header for control file */
  if ((sz = filesz(cfd)) == (off_t)-1)
    rlpr_msg(FATAL, ERRNO, "fstat on control file");
  
  sprintf(buf, "%c"OFF_T_S" %s\n", RECVCF, sz, cfname);
  safe_writen(sockfd, buf, strlen(buf));
  get_and_verify_ack(sockfd, __FUNCTION__);
  
  /* send control file */
  if (read_fd_to_fd(cfd, sockfd) < 0)
    rlpr_msg(FATAL, ERRNO, "sending control file to lpd");

  safe_writen(sockfd, "\0", 1);       /* just being explicit */
  get_and_verify_ack(sockfd, __FUNCTION__);
  rlpr_msg(DEBUG, NO_ERRNO, "control file sent successfully");
}

/* if filename == NULL, then we're reading from STDIN */

void send_df(int sockfd, char *filename, char *dfname)
{
  char  buf[BUFSIZ];                  /* temporary buffer */
  int   dfd;                          /* descriptor for datafile */
  off_t sz;                           /* dfd size */
  
  if (filename == NULL) /* STDIN */ {

    /* we need to make a dummy file to send, so that we know the size
     * of stdin for machines that are not local. this is annoying but
     * apparently required for most lpd's (even though RFC 1179 seems
     * to say otherwise)
     */

    filename = rlpr_malloc(strlen(dfname) + strlen(opts_.tmpdir) + 2);
    sprintf(filename, "%s/%s", opts_.tmpdir, dfname);

    if ((dfd = open(filename, O_RDWR|O_TRUNC|O_CREAT, 0600)) < 0)
      rlpr_msg(FATAL, ERRNO, "%s - cannot create", filename);
    if (unlink(filename) < 0)
      rlpr_msg(WARNING, ERRNO, "%s - cannot unlink", filename);

    if (read_fd_to_fd(STDIN_FILENO, dfd) < 0)
      rlpr_msg(FATAL, ERRNO, "copying `stdin' to temporary file");

    if (lseek(dfd, 0, SEEK_SET) < 0)
      rlpr_msg(WARNING, ERRNO, "%s - cannot rewind", filename);

    free(filename);
    filename = "stdin";
    
    /* else have a filename from the commandline */

  } else {
    if ((dfd = open(filename, O_RDONLY)) < 0)
      rlpr_msg(FATAL, ERRNO, "%s - cannot open", filename);
  }

  /* COMMON */

  rlpr_msg(DEBUG, NO_ERRNO, "sending data file `%s'", filename);

  if ((sz = filesz(dfd)) == (off_t)-1)
    rlpr_msg(FATAL, ERRNO, "fstat on data file");

  sprintf(buf, "%c"OFF_T_S" %s\n", RECVDF, sz, dfname);
  safe_writen(sockfd, buf, strlen(buf));
  get_and_verify_ack(sockfd, __FUNCTION__);
  
  if (read_fd_to_fd(dfd, sockfd) < 0)
    rlpr_msg(FATAL, ERRNO, "sending data from %s to server", filename);

  safe_writen(sockfd, "\0", 1);
  get_and_verify_ack(sockfd, __FUNCTION__);

  rlpr_msg(DEBUG, NO_ERRNO, "data file `%s' sent successfully", filename);
}
