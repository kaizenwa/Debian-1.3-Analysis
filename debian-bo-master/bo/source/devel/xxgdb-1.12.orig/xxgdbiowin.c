/*
 * iand	94/02/10 SVR4 port
 *
 */
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#if ( defined(SYSV) || defined(SVR4) ) && !defined(HPUX)
#define signal sigset
#endif

static void 
handler (sig)
	int sig;
{
}

main ()
{
    int sock;
    struct sockaddr_un name;
    char ttypid[40];
#if 0 /* for debug only */
{
	char *myname;
	char buf[10000];
	fprintf(stderr, "\n\nHello ! !\n\n");
	fprintf(stderr, "\n\nisatty == %d\n\n", isatty(0));
	myname = ttyname(0);
	if (myname == 0) {
		fprintf (stderr, "\n\n ERROR ttyname == 0 \n\n\n");
	} else {
		fprintf (stderr, "\n\n ttyname == %s\n\n\n", myname);
	}
    sprintf(ttypid, "%s,%d", myname, getpid());
}
#else
    sprintf(ttypid, "%s,%d", ttyname(0), getpid());
#endif
    sock = socket(AF_UNIX, SOCK_DGRAM, 0);
    name.sun_family = AF_UNIX;
    strcpy(name.sun_path, "/tmp/iowindowtty");
    sendto(sock, ttypid, 40, 0, 
	   (struct sockaddr*)&name, sizeof(struct sockaddr_un));
    close(sock);

    signal(SIGINT,  handler);
    signal(SIGQUIT, handler);
    signal(SIGTSTP, handler);
#ifdef SVR4
    setpgid(0,0);
#else
    setpgrp(0,0);
#endif
    close(0);
    close(1);
    while (1) pause();
}

