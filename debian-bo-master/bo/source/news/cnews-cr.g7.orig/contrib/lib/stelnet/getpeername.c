/* prints the name of the machine, assuming stdin is a socket */
#ifndef lint
static char rcsid[] = "$Header: skel.c,v 1.2 89/02/24 19:32:32 moraes Exp $";
#endif /*lint*/

#include <stdio.h>
#include <string.h>	/* strings.h for BSD4.2, define strchr and strrchr */
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>

char *progname;

int
main(argc, argv)
int argc;
char **argv;
{
	extern char *getrealpeername();
	struct sockaddr_in ibuf;
	char *remhname;
	
	progname = argv[0] ? argv[0] : "(no-argv[0])";
	if ((remhname = getrealpeername(0, &ibuf)) == NULL) {
		fprintf(stderr, "%s: couldn't get name of remote host\n",
			progname);
		exit(1);
	}
	fputs(remhname, stdout);
	putc('\n', stdout);
	return 0;
}

/*
 * Calls getpeername(s) to get the rank and serial number of the socket (but
 * not the name - of course not - why would a routine called getpeername()
 * ever return the name...), then calls gethostbyaddr() to get the real name,
 * returns NULL if it fails in any way.
 */
char *
getrealpeername(s, ia)
int s;
struct sockaddr_in *ia;
{
	struct sockaddr_in sin;
	struct hostent *hp;
	int sin_len;
	extern char *inet_ntoa();

	sin_len = sizeof(sin);
	if (getpeername(s, (struct sockaddr *) &sin, &sin_len) == -1)
		return NULL;
	*ia = sin;
	hp = gethostbyaddr((char *)&sin.sin_addr,
		sizeof (struct in_addr), AF_INET);
	if (hp == NULL) {
		extern char	*inet_ntoa();
		char *addr = inet_ntoa(sin.sin_addr);
		
		return addr;
	}
	return (hp ? hp->h_name : NULL);
}
