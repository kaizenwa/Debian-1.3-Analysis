#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

/*
 * Calls getpeername(s) to get the rank and serial number of the socket (but
 * not the name - of course not - why would a routine called getpeername()
 * ever return the name...), then calls gethostbyaddr() to get the real name,
 * returns NULL if it fails in any way.
 */
char *
getrealpeername(s)
int s;
{
	register struct hostent *hp;
	struct sockaddr_in sin;
	int sin_len = sizeof sin;

	if (getpeername(s, (struct sockaddr *) &sin, &sin_len) == -1)
		return NULL;
	hp = gethostbyaddr((char *)&sin.sin_addr, sizeof(struct in_addr),
			   AF_INET);
	return (hp != NULL? hp->h_name: NULL);
}
