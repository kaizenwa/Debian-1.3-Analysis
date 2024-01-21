#include	<sys/signal.h>
#include	<syslog.h>

#include	"firewall.h"

static	char	RcsId[] = "Header: daemon.c,v 1.1 93/10/20 11:13:41 mjr rel ";

daemonize()
{
	(void)signal(SIGHUP,SIG_IGN);
	(void)signal(SIGALRM,SIG_IGN);
	if(fork())
		exit(0);
#ifdef	HAVE_SETSID
	(void)setsid();
#endif
	syslog(LLEV,"daemon running");
}

