/* filename: rlpr-net.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-net.h,v 1.6 1996/12/20 18:13:24 meem Exp $
 * contents: prototypes for network constants and rlpr functions relating to
 *           networking...
 *
 * Time-stamp: <1996/12/20 12:13 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPR_NET_H
#define RLPR_NET_H

enum { DONE = 0, PRINTQ, RECVJ, SENDQS, SENDQL, REMJ };

enum { ABORTJ = 1, RECVCF = 2, RECVDF = 3};

int  open_connection(void);                     /* initializes a connection */
void close_connection(int sockfd);                   /* closes a connection */
void send_recvj_req(int sockfd);              /* tells lpd to "receive job" */
void send_cf(int sockfd, int cfd, char *cfname);   /* sends lpd controlfile */
void send_df(int sockfd, char *fname, char *dfname);  /* sends lpd datafile */

#endif /* RLPR_NET_H */

