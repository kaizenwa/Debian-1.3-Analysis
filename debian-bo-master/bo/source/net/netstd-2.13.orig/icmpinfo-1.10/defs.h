/*
 * Includes, defines and global variables used between functions.
 */

#include <stdio.h>
#include <errno.h>
extern int	errno;

#include <sys/time.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>

/* On Linux you might also need to symlink /usr/include/netinet/in_system.h
   to /usr/src/linux/include/linux/in_system.h */
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
/* maybe change this when linux will include a complete include tree : */
#ifdef linux
#include "linux_ip_icmp.h"
#else
#include <netinet/ip_icmp.h>
#endif
#include <netinet/tcp.h>
#include <netdb.h>

#include <syslog.h>
#include <unistd.h>

/*
 * Beware that the outgoing packet starts with the ICMP header and
 * does not include the IP header (the kernel prepends that for us).
 * But, the received packet includes the IP header.
 */

#define	MAXPACKET	4096	/* max packet size */

#ifndef DCLARE
#define DCLARE extern
#endif

DCLARE int		verbose;	/* enables additional error messages */

DCLARE u_char		recvpack[MAXPACKET];	/* the received packet */

DCLARE int			sockfd;	/* socket file descriptor */

char		*inet_ntoa();	/* BSD library routine */

DCLARE int     nonamequery;  /*  flag for query/noquery of ip -> name */
DCLARE int     showsrcip;    /*  flag for showing or not src ip */
DCLARE int     syslogdoutput; /* flag for stdoutput / syslogd output */
DCLARE int     noportquery;   /* flag for query/noquery of port -> serv name */

/* on some hosts (linux) netinet/ip_icmp.h is missing/empty : */
#ifndef ICMP_MINLEN
int bug=You_need_an_non_empty_netinet_ip_icmp_h;
#endif
