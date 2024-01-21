/*
 * icmpinfo
 *    It is a tool to look at the icmp you receive
 *    Its source comes from a modified BSD ping source by Laurent Demailly
 *
 *              it comes AS IS - no warranty, etc...
 *                    <dl@hplyot.obspm.fr>
 *
 * see the README for usage infos...etc...
 *
 */
/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n augmented 4/94 by dl\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ping.c	4.10 (Berkeley) 10/10/88 - $Author: icmpinfo-1.10- Laurent Demailly <dl@hplyot.obspm.fr>$";
#endif /* not lint */

#define DCLARE /* def : */

#include  "defs.h"

/*
 *			P I N G . C
 *
 * Using the InterNet Control Message Protocol (ICMP) "ECHO" facility,
 * measure round-trip-delays and packet loss across network paths.
 *
 * Author -
 *	Mike Muuss
 *	U. S. Army Ballistic Research Laboratory
 *	December, 1983
 * Modified at Uc Berkeley
 *
 * Status -
 *	Public Domain.  Distribution Unlimited.
 *
 * Bugs -
 *	More statistics could always be gathered.
 *	This program has to run SUID to ROOT to access the ICMP socket.
 */

char	usage[] = "Usage:  icmpinfo [-v[v[v]]] [-s] [-n] [-p] [-l]\n   -v : more and more info\n   -s : show local interface address\n   -n : no name query (dot ip only)\n   -p : no port -> service name query\n   -l : fork + syslog output\nv1.10 - 8/1994 - dl";
char	*pname;

int main(argc, argv)
int	argc;
char	**argv;
{
	int			sockoptions, on;
	struct protoent		*proto;

	on = 1;
	pname = argv[0];
	argc--;
	argv++;

	sockoptions=nonamequery=noportquery=syslogdoutput=showsrcip=0;
	while (argc > 0 && *argv[0] == '-') {
		while (*++argv[0]) switch (*argv[0]) {
			case 'd':
				sockoptions |= SO_DEBUG;
				break;
			case 'r':
				sockoptions |= SO_DONTROUTE;
				break;
			case 'v':
				verbose++;
				break;
			case 'n':
				nonamequery++;
				break;
			case 'p':
				noportquery++;
				break;
			case 'l':
				syslogdoutput++;
				break;
			case 's':
				showsrcip++;
				break;
			case 'h':
		        default :
				err_quit(usage);
		}
		argc--, argv++;
	}
	if (argc!=0) err_quit(usage);
	if ( (proto = getprotobyname("icmp")) == NULL)
		err_quit("unknown protocol: icmp");
	if ( (sockfd = socket(AF_INET, SOCK_RAW, proto->p_proto)) < 0)
		err_sys("can't create raw socket (root and/or bit s needed)");
	if (sockoptions & SO_DEBUG)
		if (setsockopt(sockfd, SOL_SOCKET, SO_DEBUG, &on,
								sizeof(on)) < 0)
			err_sys("setsockopt SO_DEBUG error");
	if (sockoptions & SO_DONTROUTE)
		if (setsockopt(sockfd, SOL_SOCKET, SO_DONTROUTE, &on,
								sizeof(on)) < 0)
			err_sys("setsockopt SO_DONTROUTE error");

	if (syslogdoutput) {
	  if (getuid()!=0)
	    err_quit("You need root id to use the syslog/daemon -l option");
	  if (fork()) {exit(0);}
	  /* Can't check openlog & syslog retcodes 'cause lot of
             unixes have void openlog(); and void syslog(); !! */
	  openlog("icmpinfo",0,LOG_DAEMON);
	  syslog(LOG_NOTICE,"started, PID=%d.",getpid());
	  setsid();
	  close(0);
	  close(1);
	  close(2);
	} else {
	    printf("icmpinfo: Icmp monitoring in progress...\n");
	}
	recv_ping();	/* and start the receive */
	/* NOTREACHED */
	return(0);
}
