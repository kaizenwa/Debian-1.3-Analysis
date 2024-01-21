/*
 * Copyright (c) 1992/3 Theo de Raadt <deraadt@fsa.ca>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * $Log: ypset.c,v $
 * Revision 2.4  1995/01/24 12:25:14  swen
 * Added RCS keywords.
 *
 */

#ifndef LINT
static char rcsid[] = "ypset.c,v 1.3 1993/06/12 00:02:37 deraadt Exp";
#endif

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <getopt.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#include <arpa/inet.h>
#include "xdr_yp.h"

void usage(void);
int bind_tohost(struct sockaddr_in *, char *, char *, char *);

void
usage(void)
{
  fprintf(stderr, "Usage:\n");
  fprintf(stderr, "\typset [-h host ] [-d domain] server\n");
  exit(1);
}

int
bind_tohost(struct sockaddr_in *saddr, char *host, char *dom, char *server)
{
  struct ypbind_setdom ypsd;
  struct timeval tv;
  struct hostent *hp;
  CLIENT *client;
  int sock, port;
  int r;
  unsigned long server_addr;
  port=htons(getrpcport(server, YPPROG, YPPROC_NULL, IPPROTO_UDP));
  if(0 == port)
    {
      fprintf(stderr, "%s not running ypserv.\n", server);
      exit(1);
	}
  
  bzero(&ypsd, sizeof ypsd);
  hp = gethostbyname (server);
  if(NULL != hp)
    {
          /* is this the most compatible way?? */
      bcopy (hp->h_addr_list[0], &ypsd.ypsetdom_addr,
             sizeof (ypsd.ypsetdom_addr));
	}
  else if((long)(server_addr = inet_addr (server)) == -1)
    {
      fprintf(stderr, "can't find address for %s\n", server);
      exit(1);
	}
  else
    bcopy (&server_addr, &ypsd.ypsetdom_addr, sizeof (server_addr));
  
  ypsd.ypsetdom_domain = dom;
  ypsd.ypsetdom_port = port;
  ypsd.ypsetdom_vers = YPVERS;
  
  tv.tv_sec = 15;
  tv.tv_usec = 0;
  sock = RPC_ANYSOCK;
  client = clntudp_create(saddr, YPBINDPROG, YPBINDVERS, tv, &sock);
  if (client==NULL)
    {
      fprintf(stderr, "can't yp_bind: Reason: %s\n", yperr_string(YPERR_YPBIND));
      return YPERR_YPBIND;
	}
  client->cl_auth = authunix_create_default();
  
  r = clnt_call(client, YPBINDPROC_SETDOM,
                xdr_ypbind_setdom, &ypsd,
                xdr_void, NULL,
                tv);
  if(r)
    {
      fprintf(stderr, "Cannot ypset for domain %s on host %s.\n", dom, host);
      clnt_destroy(client);
      return YPERR_YPBIND;
	}
  clnt_destroy(client);
  return 0;
}

int
main(int argc, char **argv)
{
  struct sockaddr_in saddr;
  struct hostent *hent;
  char *domainname;
  char *hostname = "localhost";
  int c;
  
  yp_get_default_domain(&domainname);
  
  bzero(&saddr, sizeof saddr);
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(0x7f000001);
  
  while( (c=getopt(argc, argv, "h:d:")) != -1)
    switch(c)
      {
      case 'd':
        domainname = optarg;
        break;
      case 'h':
        if( (long)(saddr.sin_addr.s_addr=inet_addr(optarg)) == -1)
          {
            hent = gethostbyname(optarg);
            if(hent==NULL)
              {
                fprintf(stderr, "ypset: host %s unknown\n",	optarg);
                exit(1);
              }
            bcopy(hent->h_addr_list[0], &saddr.sin_addr, sizeof saddr.sin_addr);
          }
	  hostname = optarg;
        break;
      default:
        usage();
      }
  
  if(optind + 1 != argc )
    usage();
  
  if (bind_tohost(&saddr, hostname, domainname, argv[optind]))
    exit(1);
  exit(0);
}
