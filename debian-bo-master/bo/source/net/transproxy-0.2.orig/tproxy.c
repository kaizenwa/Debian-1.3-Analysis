/* vim:ai ts=4 sw=4
 * tproxy.c:
 *
 * This is a transparent proxy server. It can be started from inetd to
 * service HTTP requests, or run as a stand-alone daemon. HTTP requests
 * are transparently accepted and passed to the WWW proxy cache for
 * handling. The HTTP request line is modified into a form usable by
 * the WWW proxy cache. The destination WWW site name is extracted from
 * the HTTP headers to avoid DNS lookups.
 */

#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <syslog.h>
#include <signal.h>
#include <pwd.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * Typedefs.
 */
typedef unsigned long	ipaddr_t;

/*
 * Macros.
 */
#define FD_MAX(a,b)		((a) > (b) ? (a) : (b))

/*
 * Function prototypes.
 */
static void usage(char *prog, char *opt);
static short getportnum(char *portnum);
static ipaddr_t getipaddress(char *ipaddr);
static uid_t getuserid(char *user);
static uid_t getgroupid(uid_t uid);
static int bind_to_port(ipaddr_t bind_ip, short bind_port);
static int connect_to_proxy(ipaddr_t ip, short port);
static void lookup_hostname(struct sockaddr_in *addr, char *hostname,
							int hostlen, int needed);
static void server_main_loop(int sock, ipaddr_t server_ip, short server_port);
static void trans_proxy(int sock, struct sockaddr_in *addr,
						ipaddr_t server_ip, short server_port);
static int	get_site_from_headers(int sock, char *hostname, int hostlen,
								  char *headers, int header_max,
								  int *header_len);

/*
 * Command line switches.
 */
static int				fully_transparent = 0;

/*
 * Main loop.
 */
int main(int argc, char **argv)
{
	int					arg;
	int					run_as_server = 0;
	ipaddr_t			bind_ip = INADDR_ANY;
	short				bind_port = -1;
	ipaddr_t			server_ip = INADDR_NONE;
	short				server_port = -1;
	uid_t				run_uid = -1;
	gid_t				run_gid = -1;
	int					sock;
	struct sockaddr_in	addr;
	int					len;

	/*
	 * Parse the command line arguments.
	 */
	while ((arg = getopt(argc, argv, "ts:r:b:")) != EOF)
	{
		switch (arg)
		{
		case 't':
			fully_transparent = 1;
			break;

		case 's':
			run_as_server = 1;
			bind_port = getportnum(optarg);
			break;

		case 'r':
			run_uid = getuserid(optarg);
			run_gid = getgroupid(run_uid);
			break;

		case 'b':
			bind_ip = getipaddress(optarg);
			break;

		case '?':
			usage(argv[0], NULL);
			break;
		}
	}

	/*
	 * Process the remaining command line arguments.
	 */
	for (; optind < argc; ++optind)
	{
		if (server_ip == INADDR_NONE)
		{
			server_ip = getipaddress(argv[optind]);
		}
		else if (server_port == -1)
		{
			server_port = getportnum(argv[optind]);
		}
		else
		{
			usage(argv[0], "Extra arguments were specified.");
		}
	}

	/*
	 * Test to make sure required args were provided and are valid.
	 */
	if (server_ip == INADDR_NONE)
	{
		usage(argv[0], "No proxyhost was specified (or it was invalid).");
	}
	if (server_port == -1)
	{
		usage(argv[0], "No proxyport was specified (or it was invalid).");
	}
	if (run_as_server && (bind_ip == INADDR_NONE))
	{
		usage(argv[0], "The server ipaddr is invalid.");
	}
	if (run_as_server && (bind_port == -1))
	{
		usage(argv[0], "No server port was specified (or it was invalid).");
	}

	/*
	 * See if we should run as a server.
	 */
	if (run_as_server)
	{
		/*
		 * Start by binding to the port, the child inherits this socket.
		 */
		sock = bind_to_port(bind_ip, bind_port);

		/*
		 * Start a server process. When DEBUG is defined, just run
		 * in the foreground.
		 */
#ifdef DEBUG
		switch (0)
#else
		switch (fork())
#endif
		{
		case -1:
			perror("fork()");
			exit(1);

		case 0:
			/*
			 * Open syslog for logging errors.
			 */
			openlog("tproxy", LOG_PID, LOG_DAEMON);

			/*
			 * Ignore some signals.
			 */
			signal(SIGHUP, SIG_IGN);
#ifndef DEBUG
			signal(SIGINT, SIG_IGN);
#endif
			signal(SIGQUIT, SIG_IGN);
			signal(SIGTSTP, SIG_IGN);
			signal(SIGCONT, SIG_IGN);
			signal(SIGPIPE, SIG_IGN);

			/*
			 * Drop back to an untrusted user.
			 */
			setgid(run_gid);
			setuid(run_uid);

			/*
			 * Start a new session and group.
			 */
			setsid();
			setpgrp();

			/*
			 * Handle the server main loop.
			 */
			server_main_loop(sock, server_ip, server_port);

			/*
			 * Should never exit.
			 */
			closelog();
			exit(1);
		}

		/*
		 * Parent exits at this stage.
		 */
		exit(0);
	}

	/*
	 * Open syslog for logging errors.
	 */
	openlog("tproxy", LOG_PID, LOG_DAEMON);

	/*
	 * We are running from inetd so find the peer address.
	 */
	len = sizeof(addr);
	if (getpeername(STDIN_FILENO, (struct sockaddr *)&addr, &len) < 0)
	{
		syslog(LOG_ERR, "getpeername(): %m");
		closelog();
		exit(1);
	}

	/*
	 * We are running from inetd so process stdin.
	 */
	trans_proxy(STDIN_FILENO, &addr, server_ip, server_port);
	closelog();

	return (0);
}

/*
 * Print some basic help information.
 */
static void usage(char *prog, char *opt)
{
	if (opt)
	{
		fprintf(stderr, "%s: %s\n", prog, opt);
	}
	fprintf(stderr, "usage: %s [-t] [-s port [-r user] [-b ipaddr]] proxyhost proxyport\n", prog);
	fprintf(stderr, "    -t          Act fully transparently (default is HTTP translate).\n");
	fprintf(stderr, "    -s port     Run as a server bound to the specified port.\n");
	fprintf(stderr, "    -r user     Run as the specified user in server mode.\n");
	fprintf(stderr, "    -b ipaddr   Bind to the specified ipaddr in server mode.\n");
	exit(1);
}

/*
 * Return the port number, in network order, of the specified service.
 */
static short getportnum(char *portnum)
{
	char			*digits = portnum;
	struct servent	*serv;
	short			port;

	for (port = 0; isdigit(*digits); ++digits)
	{
		port = (port * 10) + (*digits - '0');
	}

	if ((*digits != '\0') || (port <= 0))
	{
		if ((serv = getservbyname(portnum, "tcp")) != NULL)
		{
			port = ntohs(serv->s_port);
		}
		else
		{
			port = -1;
		}
		endservent();
	}

#ifdef DEBUG
	fprintf(stderr, "Port lookup %s -> %hd\n", portnum, port);
#endif

	return (port);
}

/*
 * Return the IP address of the specified host.
 */
static ipaddr_t getipaddress(char *ipaddr)
{
	struct hostent	*host;
	ipaddr_t		ip;

	if (((ip = inet_addr(ipaddr)) == INADDR_NONE)
	    &&
		(strcmp(ipaddr, "255.255.255.255") != 0))
	{
		if ((host = gethostbyname(ipaddr)) != NULL)
		{
			memcpy(&ip, host->h_addr, sizeof(ip));
		}
		endhostent();
	}

#ifdef DEBUG
	fprintf(stderr, "IP lookup %s -> 0x%08lx\n", ipaddr, ip);
#endif

	return (ip);
}

/*
 * Find the userid of the specified user.
 */
static uid_t getuserid(char *user)
{
	struct passwd	*pw;
	uid_t			uid;

	if ((pw = getpwnam(user)) != NULL)
	{
		uid = pw->pw_uid;
	}
	else if (*user == '#')
	{
		uid = (uid_t)atoi(&user[1]);
	}
	else
	{
		uid = -1;
	}

#ifdef DEBUG
	fprintf(stderr, "User lookup %s -> %d\n", user, uid);
#endif

	endpwent();

	return (uid);
}

/*
 * Find the groupid of the specified user.
 */
static uid_t getgroupid(uid_t uid)
{
	struct passwd	*pw;
	gid_t			gid;

	if ((pw = getpwuid(uid)) != NULL)
	{
		gid = pw->pw_gid;
	}
	else
	{
		gid = -1;
	}

#ifdef DEBUG
	fprintf(stderr, "Group lookup %d -> %d\n", uid, gid);
#endif

	endpwent();

	return (gid);
}

/*
 * Bind to the specified ip and port.
 */
static int bind_to_port(ipaddr_t bind_ip, short bind_port)
{
	struct sockaddr_in	addr;
	int					sock;

	/*
	 * Allocate a socket.
	 */
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		perror("socket()");
		exit(1);
	}

#ifdef DEBUG
	/*
	 * Set the SO_REUSEADDR option for debugging.
	 */
	{
	 	int	one = 1;

		setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one));
	}
#endif

	/*
	 * Set the address to listen to.
	 */
	addr.sin_family = AF_INET;
	addr.sin_port = htons(bind_port);
	addr.sin_addr.s_addr = bind_ip;

	/*
	 * Bind our socket to the above address.
	 */
	if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0)
	{
		perror("bind()");
		exit(1);
	}

	/*
	 * Establish a large listen backlog.
	 */
	if (listen(sock, SOMAXCONN) < 0)
	{
		perror("listen()");
		exit(1);
	}

	return (sock);
}

/*
 * Connect to the proxy server.
 */
static int connect_to_proxy(ipaddr_t ip, short port)
{
	struct sockaddr_in	addr;
	int					sock;

	/*
	 * Allocate a socket.
	 */
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		syslog(LOG_WARNING, "socket(): %m");
		return (-1);
	}

	/*
	 * Set the address to connect to.
	 */
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = ip;

	/*
	 * Connect our socket to the above address.
	 */
	if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0)
	{
		syslog(LOG_WARNING, "connect(): %m");
		return (-1);
	}

	return (sock);
}

/*
 * Translate a sockaddr_in structure into a usable ASCII hostname.
 */
static void lookup_hostname(struct sockaddr_in *addr, char *hostname,
							int hostlen, int needed)
{
	struct hostent	*host;

	/*
	 * Get the hostname IP in dotted decimal in case the lookup fails.
	 */
	strncpy(hostname, inet_ntoa(addr->sin_addr), hostlen);
	hostname[hostlen - 1] = '\0';

#ifdef DNS_LOOKUPS
	/*
	 * If the needed flag is set then always do a lookup.
	 */
	if (needed)
	{
		if ((host = gethostbyaddr((char *)&addr->sin_addr,
								  sizeof(addr->sin_addr), AF_INET)) != NULL)
		{
			strncpy(hostname, host->h_name, hostlen);
			hostname[hostlen - 1] = '\0';
		}
		else
		{
			syslog(LOG_INFO, "DNS lookup for %s failed: %m", hostname);
		}
	}
#ifdef USELESS_DNS_LOOKUPS
	else
	{
		if ((host = gethostbyaddr((char *)&addr->sin_addr,
								  sizeof(addr->sin_addr), AF_INET)) != NULL)
		{
			strncpy(hostname, host->h_name, hostlen);
			hostname[hostlen - 1] = '\0';
		}
		else
		{
			syslog(LOG_INFO, "DNS lookup for %s failed: %m", hostname);
		}
	}
#endif
#endif
}

/*
 * This is the main loop when running as a server.
 */
static void server_main_loop(int sock, ipaddr_t server_ip, short server_port)
{
	int					new_sock;
	struct sockaddr_in	addr;
	int					len;

	/*
	 * Ignore dead servers so no zombies should be left hanging.
	 */
	signal(SIGCLD, SIG_IGN);

	for (;;)
	{
		/*
		 * Accept an incoming connection.
		 */
		len = sizeof(addr);
		while ((new_sock = accept(sock, (struct sockaddr *)&addr, &len)) < 0)
		{
			/*
			 * Connection resets are common enough to log them as debug only.
			 */
			syslog((errno == ECONNRESET ? LOG_DEBUG : LOG_ERR), "accept(): %m");
		}

		/*
		 * Create a new process to handle the connection.
		 */
		switch (fork())
		{
		case -1:
			/*
			 * Under load conditions just ignore new connections.
			 */
			break;

		case 0:
			/*
			 * Start the proxy work in the new socket.
			 */
			trans_proxy(new_sock, &addr, server_ip, server_port);
			close(new_sock);
			closelog();
			exit(0);
		}

		/*
		 * Close the socket as the child does the handling.
		 */
		close(new_sock);
	}
}

/*
 * Perform the transparent proxy activity.
 */
static void trans_proxy(int sock, struct sockaddr_in *from_addr,
						ipaddr_t server_ip, short server_port)
{
	struct sockaddr_in	to_addr;
	int					to_len;
	int					proxy;
	char				from_host[64];
	char				to_host[64];
	static char			headers[16384 + 1];
	int					header_len;
	char				*uri_pos;
	int					max_fd;
	fd_set				read_fd;
	int					read_len;

	/*
	 * The first thing we do is get the IP address that the client was
	 * trying to connected to. Here lies part of the magic. Normally
	 * getsockname returns our address, but not with transparent proxying.
	 */
	to_len = sizeof(to_addr);
	if (getsockname(sock, (struct sockaddr *)&to_addr, &to_len) < 0)
	{
		syslog(LOG_ERR, "getsockname(): %m");
		return;
	}

	/*
	 * Connect to the proxy server.
	 */
	proxy = connect_to_proxy(server_ip, server_port);

	/*
	 * Fully transparent saves a lot of trouble, but then again it doesn't
	 * support the transparent proxy feature. If the daemon is operating in
	 * HTTP translation mode then translate the request line.
	 */
	if (fully_transparent)
	{
		/*
		 * Lookup an ASCII representation of the host's IP address.
		 */
		lookup_hostname(from_addr, from_host, sizeof(from_host), 0);
		lookup_hostname(&to_addr, to_host, sizeof(to_host), 1);

		/*
		 * Log the facts about the connection.
		 */
		syslog(LOG_INFO, "Transparent %s -> %s", from_host, to_host);
	}
	else
	{
		/*
		 * Lookup an ASCII representation of the from host's IP address.
		 */
		lookup_hostname(from_addr, from_host, sizeof(from_host), 0);

		/*
		 * See if the destination site can be found in the HTTP headers.
		 */
		if (get_site_from_headers(sock, to_host, sizeof(to_host),
								  headers, sizeof(headers) - 1,
								  &header_len))
		{
			/*
			 * Log the facts about the connection.
			 */
			syslog(LOG_INFO, "Request NoDNS %s -> %s", from_host, to_host);
		}
		else
		{
			/*
			 * Fallback is to use the DNS to find the destination site.
			 */
			lookup_hostname(&to_addr, to_host, sizeof(to_host), 1);

			/*
			 * Log the facts about the connection.
			 */
			syslog(LOG_INFO, "Request %s -> %s", from_host, to_host);
		}

#ifdef PAY_THE_PENALTY
		/*
		 * Not that I'm mean or anything, but if they don't configure to use the
		 * proxy directly then they pay a little penalty.
		 */
		sleep(2);
#endif

		/*
		 * Parse the request line which is of the form METHOD /URI HTTP/1.0
		 * We split the line between the METHOD and /URI. If the URI starts
		 * with a / then we have transparently trapped a request, if it doesn't
		 * then assume that we have been nominated as a proxy by the browser.
		 */
		if (((uri_pos = strchr(headers, ' ')) != NULL) &&
			(uri_pos[1] == '/'))
		{
			*uri_pos++ = '\0';
			write(proxy, headers, strlen(headers));
			write(proxy, " http://", 8);
			write(proxy, to_host, strlen(to_host));
			write(proxy, uri_pos, header_len - strlen(headers) - 1);

			/*
			 * Dump a nice log message showing what we have done.
			 */
			syslog(LOG_DEBUG, "Translated '%s http://%s'", headers, to_host);
		}
		else
		{
			write(proxy, headers, header_len);

			/*
			 * Dump a nice log message showing what we have done.
			 */
			headers[32] = '\0';
			syslog(LOG_DEBUG, "Untranslated '%s'", headers);
		}
	}

	/*
	 * Continue by passing data back and forth between the client and proxy.
	 */
	for (;;)
	{
		/*
		 * Construct a select read mask from both file descriptors.
		 */
		FD_ZERO(&read_fd);
		FD_SET(sock, &read_fd);
		FD_SET(proxy, &read_fd);
		max_fd = FD_MAX(sock, proxy);

		/*
		 * Wait for some data to be read.
		 */
		if (select(max_fd + 1, &read_fd, NULL, NULL, NULL) < 0)
		{
			syslog(LOG_ERR, "select(): %m");
			close(proxy);
			return;
		}

		/*
		 * See if any data can be read from the client.
		 */
		if (FD_ISSET(sock, &read_fd))
		{
			switch (read_len = read(sock, headers, sizeof(headers) - 1))
			{
			case -1:
				syslog((errno == ECONNRESET ? LOG_DEBUG : LOG_WARNING),
						"read(client) failed: %m");
				close(proxy);
				return;

			case 0:
				close(sock);
				close(proxy);
				return;

			default:
				if (write(proxy, headers, read_len) < 0)
				{
					syslog(LOG_WARNING, "write(proxy) failed: %m");
					close(sock);
					return;
				}
				break;
			}
		}

		/*
		 * See if any data can be read from the proxy.
		 */
		if (FD_ISSET(proxy, &read_fd))
		{
			switch (read_len = read(proxy, headers, sizeof(headers) - 1))
			{
			case -1:
				syslog((errno == ECONNRESET ? LOG_DEBUG : LOG_WARNING),
						"read(proxy) failed: %m");
				close(sock);
				return;

			case 0:
				close(proxy);
				close(sock);
				return;

			default:
				if (write(sock, headers, read_len) < 0)
				{
					syslog(LOG_WARNING, "write(client) failed: %m");
					close(proxy);
					return;
				}
				break;
			}
		}
	}
}

/*
 * Read in the headers for the HTTP reset and store in the headers array.
 * Then search the headers for a Host: hostname[:port] header.
 */
static int	get_site_from_headers(int sock, char *hostname, int hostlen,
								  char *headers, int header_max,
								  int *header_len)
{
	int		read_len;
	char	*start;
	char	*end;

	/*
	 * Read in a buffers worth of data, hopefully all of the headers
	 * will be contained in this initial read. This is tagged with a
	 * big !!FIX ME!!
	 */
	switch (read_len = read(sock, headers, header_max))
	{
	case -1:
		syslog((errno == ECONNRESET ? LOG_DEBUG : LOG_WARNING),
				"read(client) failed: %m");
		return (-1);

	case 0:
		return (0);
	}
	*header_len = read_len;

	/*
	 * Terminate the buffer so the string functions don't run
	 * of the end.
	 */
	headers[read_len] = '\0';

	/*
	 * See if the specified string exists in the buffer.
	 */
	if ((start = strstr(headers, "\r\nHost: ")) != NULL)
	{
		/*
		 * We found it, so now skip over it.
		 */
		start += strlen("\r\nHost: ");

		/*
		 * Now see if the field we want is terminated by one
		 * the termination characters.
		 */
		if ((end = strpbrk(start, ":\r\n\t ")) != NULL)
		{
			/*
			 * We found the end, now copy the field to the output.
			 */
			while ((start < end) && (hostlen-- > 1))
			{
				*hostname++ = *start++;
			}
			*hostname = '\0';

			/*
			 * Return success.
			 */
			return (1);
		}
	}

	/*
	 * Return failure.
	 */
	return (0);
}
