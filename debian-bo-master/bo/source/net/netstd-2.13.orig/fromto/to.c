/*
 * to		A simple program that can be used as the "transmitter"
 *		part of a TCP network stream connection.  It could
 *		be used for setting up inter-machine network pipes
 *		for programs like "tar"...
 *
 * Usage:	to [-p] [-v] host portno
 *
 * Version:	@(#)to		1.00	11/06/92	FvK
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


static char *Version = "@(#)to 1.00 (11/06/92)";


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
  fprintf(stderr, "Usage: to [-p] [-v] host port\n");
  exit(-1);
}


int main(argc, argv)
int argc;
char *argv[];
{
  char hostname[128], buff[1024];
  struct sockaddr_in him;
  struct hostent *hp;
  int opt_p = 0;
  int opt_v = 0;
  int i, j, s;
  long count;

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

  /* Two more arguments required. */
  if (optind != (argc - 2)) usage();

  /* Fetch hostname and port number. */
  strncpy(hostname, argv[optind++], 128);
  i = atoi(argv[optind]);

  /* Convert hostname to an IP address. */
  if ((hp = gethostbyname(hostname)) == (struct hostent *)NULL) {
	herror(hostname);
	exit(-1);
  }
  memcpy((char *) &him.sin_addr, hp->h_addr_list[0], hp->h_length);
  strcpy(hostname, hp->h_name);
  him.sin_family = AF_INET;
  him.sin_port = htons(i);
  if (opt_v ==1 ) {
	fprintf(stderr, "Sending to ");
	pr_sock(&him, opt_p);
  }

  /* Create a socket. */
  if ((s = socket(him.sin_family, SOCK_STREAM, 0)) < 0) {
	perror("socket");
	exit(-1);
  }

  /* All done. Connect! */
  if (connect(s, (struct sockaddr *) &him, sizeof(him)) < 0) {
	perror("connect");
	(void) close(s);
	exit(-1);
  }

  /* Start sending data until there is no more... */
  count = 0L;
  do {
	if ((i = read(0, buff, 1024)) <= 0) break;
	printf("\rSent %08ld bytes", count);
	(void) fflush(stdout);
	j = write(s, buff, i);
	if (j != i) {
		fprintf(stderr, "Huh?  Bad write %d of %d (%d)\n",
						j, i, errno);
		break;
	}
	count += (long) j;
  } while(1);

  /* Check for any errors.. */
  if (i != 0) {
	fprintf(stderr, "Transfer aborted: %s\n", sys_errlist[errno]);
  }

  /* Close up! */
  (void) close(s);

  return(0);
}
