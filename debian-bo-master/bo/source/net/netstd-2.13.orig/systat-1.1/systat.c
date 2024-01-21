/* systat -- show the system/network status of a remote host

   Copyright (C) 1994 Peter Tobias <tobias@et-inf.fho-emden.de>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/param.h>

#define NETSTAT "netstat"
#define SYSTAT  "systat"
#define TIME  "daytime"

static char *program_name;
static const char *version_string = "systat 1.1";

static void usage(void);

static void usage(void)
{
  printf("Usage: %s [OPTION]... hostname\n\n\
  -n, --netstat                display the network status of the remote host\n\
  -p, --port port              use a different tcp port\n\
  -s, --systat                 display the system status of the remote host\n\
  -t, --time                   display the local time of the remote host\n\
  -h, --help                   display this help and exit\n\
  -v, --version                output version information and exit\n\
\n\
Systat allows you to get system informations from remote hosts using their\n\
netstat, systat or daytime service. By default it will use the systat service.\n\
\n\
The only mandatory parameter is the destination host name or IP number.\n",
     program_name);
}

int main(int argc, char **argv)
{
	int c, br, bw, s, s_port = 0;
	int option_index = 0;

	extern char *optarg;

	char hname[MAXHOSTNAMELEN+1];
	char buffer[1024];
	char *s_name = SYSTAT;
	struct hostent *hp;
	struct protoent *pp;
	struct servent *sp;
	struct sockaddr_in sa;

	static const struct option long_options[] =
	{
		{"netstat", no_argument, 0, 'n'},
		{"systat", no_argument, 0, 's'},
		{"time", no_argument, 0, 't'},
		{"port", required_argument, 0, 'p'},
		{"help", no_argument, 0, 'h'},
		{"version", no_argument, 0, 'v'},
		{0, 0, 0, 0}
	};

	program_name = (rindex(argv[0], '/')) ? rindex(argv[0], '/') + 1 : argv[0];

	c = getopt_long(argc, argv, "hnp:stv", long_options, &option_index);

	switch(c)
	{
		case 'n':
			s_name = NETSTAT;
			break;
		case 's':
			s_name = SYSTAT;
			break;
		case 't':
			s_name = TIME;
			break;
		case 'p':
			s_port = atoi(optarg);
			break;
		case 'h':
			usage();
			exit(0);
			break;
		case 'v':
			printf("%s\n", version_string);
			break;
		case '?':
			fprintf(stderr,"Try `%s --help' for more information.\n", program_name);
			exit(1);
			break;
		default:
	};

	if (optind != (argc-1)) {
		usage();
		exit(1);
	}

	strncpy(hname,argv[optind],MAXHOSTNAMELEN);

	if (!(hp = gethostbyname(hname))) {
		herror(program_name);
		exit(1);
	} else {
		sa.sin_family = hp->h_addrtype;
		bcopy (hp->h_addr_list[0], (char *)&sa.sin_addr, hp->h_length);
	}
 
	if (s_port) {
		if (!(sp = getservbyport(htons(s_port), "tcp"))) {
			fprintf(stderr, "%s: unknown service `%d/tcp'\n", program_name, s_port);
			exit(1);
		}
	} else {
		if (!(sp = getservbyname(s_name, "tcp"))) {
			fprintf(stderr, "%s: unknown service `%s/tcp'\n", program_name, s_name);
			exit(1);
		}
	}

	if (!(pp = getprotobyname(sp->s_proto))) {
		fprintf(stderr, "%s: could not get protocol `%s'\n", program_name, sp->s_proto);
		exit(1);
	}

	if ((s = socket(sa.sin_family, SOCK_STREAM, pp->p_proto)) == -1) {
		fprintf(stderr, "%s: could not create socket (%s)\n",
		    program_name, strerror(errno));
		exit(1);
	}
	sa.sin_port = sp->s_port;

	if (connect(s, (struct sockaddr *) &sa, sizeof(sa)) == -1) {
		fprintf(stderr, "%s: could not connect socket (%s)\n",
		    program_name, strerror(errno));
		exit(1);
	}

	while ((br = read(s, buffer, 1024)) > 0) {
		bw = write(1, buffer, br);
		if (bw != br) {
			fprintf(stderr, "%s: write error (%s)\n", program_name,
			    strerror(errno));
			break;
		}
	};

	close(s);
	exit(0);
}
