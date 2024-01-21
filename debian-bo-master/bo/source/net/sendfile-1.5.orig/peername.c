/*
** peername v1.1
**
** Prints out the peername for stdin
**
** Copyright (c) 27.7.94 by Andreas Ley <ley@rz.uni-karlsruhe.de>
**
** Permission to use, copy, modify, and distribute this software for any
** purpose and without fee is hereby granted, provided that the above
** copyright notice appears in all copies. This software is provided "as is"
** and without any express or implied warranties.
**
** This program has been tested on a HP9000/720 with HP-UX A.08.07
** In this environment, neither lint -u nor gcc -Wall produce any messages.
** If you encounter any errors or need to make any changes to port it
** to another platform, please contact me.
**
** Version history
**
** Version 1.0 - 27.7.94
**	Initial version
** Version 1.0.1 - 20 Jul 95 framstag@rus.uni-stuttgart.de
**	debugged for AIX, IRIX, Linux and Solaris
** Version 1.1 - 28 Feb 96 framstag@rus.uni-stuttgart.de
**	Bug fixes and code cleanup
*/


#define __USE_BSD
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#ifndef IRIX
  #include <sys/param.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "peername.h"
#include "string.h"

#if defined(AIX) || defined(ULTRIX)
  #include "bsd.h"
#endif

#ifdef IRIX
  extern char *strdup(const char *);
  int tolower(int);
#endif

#if defined(LINUX) || defined(SOLARIS2)
  char *strdup(const char *);
#endif


char *peername(int fd)
{ int len;
  struct sockaddr addr;
  struct in_addr *iaddr;
  struct hostent *hptr;
  char *remote_name;
  char **haddr=NULL;
  static char remote_host[256];

  len=sizeof(struct sockaddr);
  remote_name="UNKNOWN_HOST";
  *remote_host=0;

  if ((getpeername(fd,&addr,&len)) < 0) return(remote_name);

  iaddr=&(((struct sockaddr_in *)&addr)->sin_addr);
  hptr=gethostbyaddr((char *)iaddr, sizeof(struct in_addr), AF_INET);
  if(hptr)
  { strncpy(remote_host,hptr->h_name,256);
    remote_name=str_tolower(remote_host);
  }

  /* Grrr. Check THAT name to make sure it's really the name of the addr. */
  /* Code from Harald Hanche-Olsen <hanche@imf.unit.no> */
  if(*remote_host)
  { hptr=gethostbyname(remote_host);
    if (hptr)
      for(haddr=hptr->h_addr_list;*haddr;haddr++)
        if(((struct in_addr *)(*haddr))->s_addr == iaddr->s_addr) break;
    if((!hptr) || (!(*haddr))) *remote_host=0;
  }

  if(!(*remote_host)) remote_name=inet_ntoa(*iaddr);

  return(remote_name);
}



/*
void usage(image)
char *image;
{
	(void)fprintf(stderr,"Usage: %s [-h] [-v] [filename...]\n",image);
	exit(1);
}


main(argc,argv)
int	argc;
char	*argv[];
{
	int		c;
	extern char	*optarg;
	extern int	optind;
	char		error[2*MAXPATHLEN+14];
	FILE		*src;
	char header[]="peername v1.0\n(c) 1994 by Andreas Ley\n";

	while ((c=getopt(argc,argv,"vh?")) != EOF)
		switch ((char)c) {
		case 'v':
			(void)fprintf(stderr,header);
			exit(0);
		case 'h':
			(void)fprintf(stderr,header);
		case '?':
			usage(argv[0]);
		}

	printf("%s\n",peername(0));
	return(0);
}
*/
