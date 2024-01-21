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
 * $Log: ypwhich.c,v $
 * Revision 1.1.1.1  1996/08/11 17:35:22  kukuk
 * NYS ypserv package
 *
 * Revision 2.6  1995/07/26 12:49:23  swen
 * Rewritten to not rely on yp_maplist() to make it work with the NYS library.
 * Added fix for broken xdr-routines in the NYS library.
 *
 * Revision 2.5  1995/01/24  12:25:19  swen
 * Added RCS keywords.
 *
 */

/*
 * 1996/08/04 Thorsten Kukuk <kukuk@uni-paderborn.de>
 * Fix the core dump, if ypwhich -m is called without a running ypbind
 * ypwhich -m will use a broadcast call to search a NIS server, if 
 * ypbind do not run. This is necessary for hosts which have a NYS libc
 * and don't need ypbind, or when installing a slave ypserv.
 *
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <getopt.h>
#include <ctype.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include <rpc/pmap_clnt.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#include <arpa/inet.h>
#include "xdr_yp.h"

extern int yp_maplist(char *, struct ypmaplist **);

struct ypalias {
  const char *alias;
  const char *name;
} ypaliases[] = {
  { "passwd", "passwd.byname" },
  { "group", "group.byname" },
  { "networks", "networks.byaddr" },
  { "hosts", "hosts.byaddr" },
  { "protocols", "protocols.bynumber" },
  { "services", "services.byname" },
  { "aliases", "mail.aliases" },
  { "ethers", "ethers.byname" },
};

struct sockaddr_in *ss_addr = NULL;

void usage(void);
int bind_host(char *, struct sockaddr_in *, int);

void
usage(void)
{
  fprintf(stderr, "Usage:\n");
  fprintf(stderr, "\typwhich [-d domain] [[-t] -m [mname] | host]\n");
  fprintf(stderr, "\typwhich -x\n");
  exit(1);
}


/*
 * Like yp_bind except can query a specific host
 */
int bind_host(char *dom, struct sockaddr_in *saddr, int doprint)
{
  struct hostent *hent = NULL;
  struct ypbind_resp ypbr;
  struct timeval tv;
  CLIENT *client;
  int sock, r;
  
  sock = RPC_ANYSOCK;
  tv.tv_sec = 15;
  tv.tv_usec = 0;
  client = clntudp_create(saddr, YPBINDPROG, YPBINDVERS, tv, &sock);
  if(NULL == client)
    {
      if(doprint)
	fprintf(stderr, "can't clntudp_create: %s\n", yperr_string(YPERR_YPBIND));
      return YPERR_YPBIND;
    }
  
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  r = clnt_call(client, YPBINDPROC_DOMAIN,
                (xdrproc_t)xdr_domainname, dom,
                (xdrproc_t)xdr_ypbind_resp, &ypbr,
                tv);
  if( r != RPC_SUCCESS)
    {
      if(doprint)
	fprintf(stderr, "can't clnt_call: %s\n", yperr_string(YPERR_YPBIND));
      clnt_destroy(client);
      return YPERR_YPBIND;
    }
  else
    {
      if (ypbr.ypbind_status != YPBIND_SUCC_VAL)
        {
          if(doprint)
	    fprintf(stderr, "can't yp_bind: Reason: %s\n",
		    yperr_string(ypbr.ypbind_status));
          clnt_destroy(client);
          return r;
	}
    }
  clnt_destroy(client);

  if (!ss_addr)
    ss_addr = (struct sockaddr_in *) malloc(sizeof(struct sockaddr_in));

  ss_addr->sin_family = AF_INET;
  ss_addr->sin_port = ypbr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port;
  ss_addr->sin_addr = ypbr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_addr;
  /*printf("%08x\n", ss_addr);*/
  if (doprint)
    {
      hent = gethostbyaddr((char *)&ss_addr->sin_addr.s_addr,
                           sizeof(ss_addr->sin_addr.s_addr), AF_INET);
      if (hent)
        printf("%s\n", hent->h_name);
      else
        printf("%s\n", inet_ntoa(ss_addr->sin_addr));
    }
  return 0;
}

/*
** Collects all the answers for our query for hosts running ypserv
*/
static struct sockaddr_in *ypserver = NULL;
static bool_t eachresult(bool_t *resp, struct sockaddr_in *addr)
{
  if (*resp)
    {
      if (ypserver == NULL) 
	{
	  ypserver = (struct sockaddr_in *) malloc(sizeof(struct sockaddr_in));
	  ypserver->sin_family = AF_INET;
	  ypserver->sin_port = addr->sin_port;
	  ypserver->sin_addr.s_addr = addr->sin_addr.s_addr;
	}
      return 1;
    }
  else
    {
      return 0;
    }
}

int main(int argc, char **argv)
{
  char *domainname, *master;
  const char *map;
  struct ypresp_maplist ypml;
  struct ypmaplist *y;
  struct hostent *hent;
  struct sockaddr_in saddr;
  struct timeval tv;
  CLIENT *client;
  int notrans, mode, getmap, sock;
  int c, r, i;
  
  yp_get_default_domain(&domainname);
  
  map = NULL;
  getmap = notrans = mode = 0;
  while( (c=getopt(argc, argv, "xd:mt")) != -1)
    switch(c)
      {
      case 'x':
	for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
	  printf("Use \"%s\" for \"%s\"\n",
		 ypaliases[i].alias, ypaliases[i].name);
	exit(0);
      case 'd':
	domainname = optarg;
	break;
      case 't':
	notrans++;
	break;
      case 'm':
	mode++;
	break;
      default:
	usage();
      }
  
  if(0 == mode)
    {
      switch(argc-optind)
        {
	case 0:
          bzero(&saddr, sizeof saddr);
          saddr.sin_family = AF_INET;
          saddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
          
          if(bind_host(domainname, &saddr, 1))
            exit(1);
          break;
	case 1:
          bzero(&saddr, sizeof saddr);
          saddr.sin_family = AF_INET;
          saddr.sin_addr.s_addr=inet_addr(argv[optind]);
          if(-1 == saddr.sin_addr.s_addr)
            {
              hent = gethostbyname(argv[optind]);
              if(NULL == hent)
                {
                  fprintf(stderr, "ypwhich: host %s unknown\n",
                          argv[optind]);
                  exit(1);
		}
              bcopy((char *)hent->h_addr_list[0],
		    (char *)&saddr.sin_addr, sizeof saddr.sin_addr);
	    } 
          if(bind_host(domainname, &saddr, 1))
            exit(1);
          break;
	default:
          usage();
	}
      exit(0);
    }
  
  if( argc-optind > 1)
    usage();
  
  if(argv[optind])
    {
      map = argv[optind];
      for(i=0; (!notrans) && i<sizeof ypaliases/sizeof ypaliases[0]; i++)
        if( strcmp(map, ypaliases[i].alias) == 0)
	  map = ypaliases[i].name;
      r = yp_master(domainname, map, &master);
      switch(r)
        {
	case 0:
          printf("%s\n", master);
          free(master);
          break;
	case YPERR_YPBIND:
          fprintf(stderr, "ypwhich: not running ypbind\n");
          exit(1);
	default:
          fprintf(stderr, "Can't find master for map %s. Reason: %s\n",
                  map, yperr_string(r));
          exit(1);
	}
      exit(0);
    }
  

  bzero(&saddr, sizeof saddr);
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (!ss_addr)
    if(bind_host(domainname, &saddr, 0))
      {
	bool_t out;
	int status;

	/* Try a broadcast to find a NIS server */
	status = clnt_broadcast(YPPROG, YPVERS, YPPROC_DOMAIN_NONACK,
				(xdrproc_t) xdr_domainname, domainname,
				(xdrproc_t) xdr_bool, (void *)&out,
				(resultproc_t) eachresult);
	if (status != RPC_SUCCESS)
	  {
	    fprintf(stderr,"broadcast: %s.", clnt_sperrno(status));
	    exit(1);
	  }
	ss_addr = ypserver;
      }
  sock = RPC_ANYSOCK;
  tv.tv_sec = 15;
  tv.tv_usec = 0;
  client = clntudp_create (ss_addr, YPPROG, YPVERS, tv, &sock);
  if(NULL == client)
    {
      fprintf(stderr, "can't clntudp_create: %s\n", yperr_string(YPERR_YPBIND));
      exit (1);
    }

  bzero((char *)&ypml, sizeof ypml);
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  r = clnt_call(client, YPPROC_MAPLIST,
		(xdrproc_t)xdr_domainname, domainname,
		(xdrproc_t)xdr_ypresp_maplist, &ypml,
		tv);

  if (RPC_SUCCESS != r)
    {
      fprintf(stderr, "ypwhich: Can't get maplist for domain %s\n", domainname);
      exit (1);
    }
  
  for(y = ypml.list; NULL != y; )
    {
      struct ypmaplist *tmp;
      tmp = y;
      r = yp_master(domainname, y->ypml_name, &master);
      switch(r)
        {
        case 0:
          printf("%s %s\n", y->ypml_name, master);
          free(master);
          break;
        default:
          fprintf(stderr,
                  "YP: can't find the master of %s: Reason: %s\n",
                  y->ypml_name, yperr_string(r));
          break;
        }
      y = y->ypml_next;
      free(tmp);
    }
  exit(0);
}
