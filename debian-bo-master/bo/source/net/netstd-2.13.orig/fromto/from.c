/*
 * from		A simple program that can be used as the "receiver"
 *		part of a TCP network stream connection.  It could
 *		be used for setting up inter-machine network pipes
 *		for programs like "tar"...
 *
 * Usage:	from [-p] [-v] portno
 *
 * Version:	@(#)from		1.00	11/06/92	FvK
 *
 * Author:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>


static char *Version = "@(#)from 1.00 (11/06/92)";


extern int getopt(), optind, opterr;


void pr_sock(struct sockaddr_in *sip, int ports)
{
  if (ports == 1) {
	fprintf(stderr, "%u,%u,%u,%u,%u,%u\n",
		((sip->sin_addr.s_addr >> 0) & 255),
		((sip->sin_addr.s_addr >> 8) & 255),
		((sip->sin_addr.s_addr >> 16) & 255),
		((sip->sin_addr.s_addr >> 24) & 255),
		((sip->sin_port >> 0) & 255),
		((sip->sin_port >> 8) & 255));
  } else fprintf(stderr, "%s:%u\n",
		inet_ntoa(sip->sin_addr), ntohs(sip->sin_port));
}


void usage(void)
{
  fprintf(stderr, "Usage: from [-p] [-v] port\n");
  exit(-1);
}


int main(argc, argv)
int argc;
char *argv[];
{
  char hostname[128], buff[1024];
  struct sockaddr_in sin, him;
  struct hostent *hp;
  int opt_p = 0;
  int opt_v = 0;
  int i, j, s;

  opterr = 0;
  while((i = getopt(argc, argv, "pv")) != EOF) switch(i) {
	case 'p':
		opt_p = 1;
		break;
	case 'v':
		opt_v = 1;
		break;
	default:
		usage();
  }

  /* One more argument required. */
  if (optind != (argc - 1)) usage();

  /* Fetch port number. */
  i = atoi(argv[optind]);

  /* Fetch our hostname. */
  if (gethostname(hostname, 128) < 0) {
	perror("gethostname");
	exit(-1);
  }

  /* Convert that to an IP address. */
  if ((hp = gethostbyname(hostname)) == (struct hostent *)NULL) {
	herror(hostname);
	exit(-1);
  }
  memcpy((char *) &sin.sin_addr, hp->h_addr_list[0], hp->h_length);
  strcpy(hostname, hp->h_name);
  sin.sin_family = AF_INET;
  sin.sin_port = htons(i);
  if (opt_v ==1 ) {
	fprintf(stderr, "Listening to ");
	pr_sock(&sin, opt_p);
  }

  /* Create a socket. */
  if ((s = socket(sin.sin_family, SOCK_STREAM, 0)) < 0) {
	perror("socket");
	exit(-1);
  }

  /* All done.  Bind it to our address. */
  if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
	perror("bind");
	(void) close(s);
	exit(-1);
  }

  /* Bind was OK.  Now post a listen... */
  if (listen(s, 1) < 0) {
	perror("listen");
	(void) close(s);
	exit(-1);
  }

  /* Done.  Await a connection! */
  i = sizeof(him);
  if ((j = accept(s, (struct sockaddr *) &him, &i)) < 0) {
	perror("accept");
	(void) close(s);
	exit(-1);
  }

  /* We now have a valid connection. */
  if (opt_v == 1) {
	fprintf(stderr, "Incoming connection from ");
	pr_sock(&him, opt_p);
  }

  /* Start receiving data until there is no more... */
  do {
	if ((i = read(j, buff, 1024)) <= 0) break;
	(void) write(1, buff, i);
  } while(1);

  /* Check for any errors.. */
  if (i != 0) {
	fprintf(stderr, "Transfer aborted: %s\n", sys_errlist[errno]);
  }

  /* Close up! */
  (void) close(j);
  (void) close(s);

  return(0);
}
