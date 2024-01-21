/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USER'S OWN RISK.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/wait.h>

#undef  False
#define False 0
#undef  True
#define True  1

#define DEFAULT_REMOTE_PORT	119

/*
 *  Define this if <unistd.h> doesn't declare getopt() and friends.
 */
#define BOGUS_UNISTD

static void perror_exit(const char *msg)
{
    perror(msg);
    _exit(1);
}

static void* xrealloc(void *ptr, size_t size)
{
    if (!ptr)
	ptr = malloc(size); /* Hack for broken C libraries */
    else
	ptr = realloc(ptr, size);

    if (!ptr)
	perror_exit("malloc");

    return ptr;
}

static struct in_addr	*allowed_hosts = NULL;
static long		 n_hosts       = 0;
static long		 n_alloced     = 0;

static void allow_host(char *host)
{
    struct hostent	*hent;

    hent = gethostbyname(host);
    if (!hent) {
	fprintf(stderr, "No such host: %s\n", host);
	return;
    }

    if (n_hosts >= n_alloced)
	allowed_hosts = xrealloc(allowed_hosts,
				 (n_alloced = 2 * (n_alloced + 1)) *
				 sizeof allowed_hosts[0]);

    memcpy(&allowed_hosts[n_hosts++], hent->h_addr, hent->h_length);
}

static int is_allowed(struct sockaddr_in *addr)
{
    long	i;

    for (i = 0 ; i < n_hosts ; i++)
	if (memcmp(&addr->sin_addr, &allowed_hosts[i],
		   sizeof addr->sin_addr) == 0)
	    return True;

    return False;
}

static struct sockaddr_in get_remote_host(char *host)
{
    struct sockaddr_in	addr;
    struct hostent	*hent;
    char		*c;

    memset(&addr, 0, sizeof addr);
    addr.sin_family = PF_INET;
    addr.sin_port   = htons(DEFAULT_REMOTE_PORT);
    c = strchr(host, ':');
    if (c) {
	*c++ = '\0';
	/* FIXME: error check? */
	addr.sin_port = htons(atoi(c));
    }

    hent = gethostbyname(host);
    if (!hent) {
	fprintf(stderr, "No such host: %s\n", host);
	_exit(1);
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);

    return addr;
}

static int do_rw(int from, int to)
{
    static char buffer[16384];
    char	*c;
    long	i, j, n;

    do {
	n = read(from, buffer, sizeof buffer);
    } while (n < 0 && errno == EINTR);

    c = buffer;
    i = n;
    while (i > 0) {
	j = write(to, c, i);
	if (j < 0)
	    if (errno == EINTR)
		continue;
	    else
		return -1;
	if (j == 0)
	    return 0;
	c += j;
	i -= j;
    }

    return n;
}

static int open_serv_socket(struct sockaddr_in *serv_addr)
{
    int	ss, tmp;

    ss = socket(PF_INET, SOCK_STREAM, 0);
    if (ss < 0)
	return -1;

    do {
	tmp = connect(ss, (struct sockaddr *)serv_addr, sizeof *serv_addr);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("connect");
	close(ss);
	return -1;
    }

    return ss;
}

static int child(struct sockaddr_in *serv_addr, int cs)
{
    fd_set	fds;
    int		max, ss, tmp = 0;

    ss = open_serv_socket(serv_addr);
    if (ss < 0)
	return -1;

    FD_ZERO(&fds);
    FD_SET(cs, &fds);
    FD_SET(ss, &fds);
    max = (cs > ss ? cs : ss) + 1;

    for (;;) {
	fd_set	rd_fds = fds;

	tmp = select(max, &rd_fds, NULL, NULL, NULL);
	if (tmp < 0)
	    if (errno == EINTR)
		continue;
	    else
		break;

	if (FD_ISSET(cs, &rd_fds)) {
	    tmp = do_rw(cs, ss);
	    if (tmp <= 0)
		break;
	}

	if (FD_ISSET(ss, &rd_fds)) {
	    tmp = do_rw(ss, cs);
	    if (tmp <= 0)
		break;
	}
    }

    if (tmp < 0)
	perror("read/write");

    close(ss);

    return tmp;
}

static void doit(struct sockaddr_in *serv_addr, int port, int single)
{
    struct sockaddr_in	local_addr, cli_addr;
    int			as;
    int			yes = 1;

    memset(&local_addr, 0, sizeof local_addr);
    local_addr.sin_family = PF_INET;
    local_addr.sin_port = port;

    as = socket(PF_INET, SOCK_STREAM, 0);
    if (as < 0)
	perror_exit("socket");
    if (setsockopt(as, SOL_SOCKET, SO_REUSEADDR, (char *)&yes, sizeof yes) < 0)
	perror_exit("setsockopt(SO_REUSEADDR)");
    if (bind(as, (struct sockaddr *)&local_addr, sizeof local_addr) < 0)
	perror_exit("bind");
    if (listen(as, 5) < 0)
	perror_exit("listen");

    for (;;) {
	int	cs, len;

	do {
	    len = sizeof cli_addr;
	    cs = accept(as, (struct sockaddr *)&cli_addr, &len);
	} while (cs < 0 && errno == EINTR);

	if (cs < 0)
	    perror_exit("accept");

	if (!is_allowed(&cli_addr)) {
	    static char deny[] = "502 You have no permission to talk!\r\n";

	    if (serv_addr->sin_port == htons(119))
		write(cs, deny, sizeof deny - 1);
	    shutdown(cs, 2);
	    close(cs);
	    continue;
	}

	if (single)
	    child(serv_addr, cs);
	else
	    switch (fork()) {
	    case -1:
		perror_exit("fork");
		break;
	    case 0:
		if (child(serv_addr, cs) < 0)
		    _exit(1);
		_exit(0);
	    default:
		break;
	    }

	close(cs);
    }
}

/************************************************************************/

static void sig_chld(int no)
{
    while (waitpid(-1, NULL, WNOHANG) > 0);
}

static void install_sig_handlers(void)
{
    struct sigaction	sig_act;

    sig_act.sa_handler = sig_chld;
    sig_act.sa_flags   = 0;
    sigfillset(&sig_act.sa_mask);
    if (sigaction(SIGCHLD, &sig_act, NULL) < 0)
	perror_exit("sigaction(SIGCHLD)");

    sig_act.sa_handler = SIG_IGN;
    sig_act.sa_flags = 0;
    sigemptyset(&sig_act.sa_mask);
    if (sigaction(SIGPIPE, &sig_act, NULL) < 0)
	perror_exit("sigaction(SIGPIPE)");
}

static void usage(char *name)
{
    fprintf(stderr, "Usage: %s -p port [-s] -h host [...] remotehost:port\n",
	    name);
    _exit(1);
}

int main(int argc, char *argv[])
{
    struct sockaddr_in	serv_addr;
    int			single = False;
    int			port = 0;
    int			c;
#ifdef BOGUS_UNISTD
    extern int	 getopt(int, char *const[], const char*);
    extern char	*optarg;
    extern int	 optind, opterr;
#endif

    freopen("/dev/null", "r", stdin);
    freopen("/dev/null", "w", stdout);

    install_sig_handlers();

    opterr = 0;
    while ((c = getopt(argc, argv, "p:h:s")) != -1)
	switch (c) {
	case 'p': /* port to listen on */
	    if (sscanf(optarg, "%d", &port) != 1 || port < 0)
		usage(argv[0]);
	    port = htons(port);
	    break;
	case 's':
	    single = True;
	    break;
	case 'h':
	    allow_host(optarg);
	    break;
	default:
	    usage(argv[0]);
	    break;
	}

    if (optind != argc - 1 || port == 0)
	usage(argv[0]);

    if (n_hosts <= 0) {
	fprintf(stderr, "No allowed hosts!\n");
	_exit(1);
    }

    serv_addr = get_remote_host(argv[optind]);
    doit(&serv_addr, port, single);

    return 0;
}
