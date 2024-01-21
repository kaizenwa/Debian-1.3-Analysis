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
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "../configure.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "server.h"
#include "sysdeps.h"

#undef  PURIFY_HACK
/*#define PURIFY_HACK*/

/*
 *  The error return from inet_atoi().
 */
#ifndef INADDR_NONE
#  define INADDR_NONE (0xfffffffful)    /* a hack, hope it works */
#endif

#ifndef USE_POLL
#  if defined(SYSV) && !defined(__hpux)
#    define USE_POLL 1
#  else
#    define USE_POLL 0
#  endif
#endif

#if USE_POLL
#  include <sys/poll.h>
#endif

/*
 *  Some kind of weird hack...
 */
#ifdef __hpux
#  define SELECT_HACK
#endif

/*
 *  Some systems don't support Posix style non-blocking I/O
 *  on sockets; we need to use the FIONBIO ioctl instead.
 */
#ifndef USE_FIONBIO
#  if defined(sco) && sco
#    define USE_FIONBIO  1
#  else
#    define USE_FIONBIO  0
#  endif
#endif

#if USE_FIONBIO
#  include <sys/ioctl.h>
#endif

static long set_nonblock(int fd)
{
#if !USE_FIONBIO
    long	flags;

    flags = fcntl(fd, F_GETFL);
    if (flags >= 0)
	flags = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    if (flags < 0)
	perror("knews: fcntl");

    return flags;
#else
    int	yes = 1;

    yes = ioctl(fd, FIONBIO, &yes);
    if (yes < 0)
	perror("knews: ioctl");

    return yes;
#endif
}

/*
 *  FreeBSD doesn't define EINPROGRESS #if _POSIX_SOURCE...
 */
int would_block(int fd, int err_no)
{
    return (
#ifdef EWOULDBLOCK
	    err_no == EWOULDBLOCK ||
#endif
#ifdef EINPROGRESS
	    err_no == EINPROGRESS ||
#endif
	    err_no == EAGAIN);
}

int timed_out(int err_no)
{
#ifdef ETIMEDOUT
    return err_no == ETIMEDOUT;
#else
    return False;
#endif
}

char *error_string(int err_no)
{
    switch (err_no) {
#ifdef ECONNREFUSED
    case ECONNREFUSED:
	return "Connection refused";
#endif
#ifdef ENETUNREACH
    case ENETUNREACH:
	return "Network unreachable";
#endif
#ifdef ETIMEDOUT
    case ETIMEDOUT:
	return "Connection timed out";
#endif
#ifdef EADDRNOTAVAIL
    case EADDRNOTAVAIL:
	return "Address not available";
#endif
#ifdef EHOSTUNREACH
    case EHOSTUNREACH:
	return "Host is unreachable";
#endif
    default:
	break;
    }

    return NULL;
}

/********************************************************************/

extern XtAppContext	app_cont;
extern Display		*display;

struct wait_data {
    int		fd;
    QuitFunc	func;
    void	*data;
};
static struct wait_data	*wait_data;

#define MASK (XtIMXEvent | XtIMTimer | XtIMAlternateInput)

int do_wait(int *fd, int rd, void (*quit_func)(void*), void *data)
{
    struct wait_data	wd;
    int			tmp;
    int			disp_fd = ConnectionNumber(display);
#if USE_POLL
    struct pollfd	fds[2];
#else
    fd_set		read_fds;
    fd_set		write_fds;
    int			maxfdp1;

    maxfdp1 = (*fd > disp_fd ? *fd : disp_fd) + 1;
#endif

    wd.fd   = *fd;
    wd.func = quit_func;
    wd.data = data;

    for (;;) {
	while (XtAppPending(app_cont) & MASK) {
	    wait_data = &wd;
	    XtAppProcessEvent(app_cont, MASK);
	    wait_data = NULL;
	    if (*fd < 0)
		return -1;
	}

#if USE_POLL
	fds[0].fd = *fd;
	fds[0].events = rd ? POLLIN : POLLOUT;
	fds[0].revents = 0;
	fds[1].fd = disp_fd;
	fds[1].events = POLLIN;
	fds[1].revents = 0;

	do {
	    tmp = poll(fds, 2, -1);
	} while (tmp < 0 && errno == EINTR);

	if (tmp < 0) {
	    perror("knews: poll");
	    return -1;
	}

	if (fds[0].revents)
	    break;
	if (fds[1].revents) {
	    wait_data = &wd;
	    XtAppProcessEvent(app_cont, MASK);
	    wait_data = NULL;
	    if (*fd < 0)
		return -1;
	}
#else
	FD_ZERO(&read_fds);
	FD_ZERO(&write_fds);
	FD_SET(disp_fd, &read_fds);
	if (rd)
	    FD_SET(*fd, &read_fds);
	else
	    FD_SET(*fd, &write_fds);

	do {
	    tmp = select(maxfdp1,
#ifdef SELECT_HACK  /* don't ask */
			 (int *)&read_fds, rd ? NULL : (int *)&write_fds,
#else
			 &read_fds, rd ? NULL : &write_fds,
#endif
			 NULL, NULL);
	} while (tmp < 0 && errno == EINTR);

	if (tmp < 0) {
	    perror("knews: select");
	    return -1;
	}

	if (FD_ISSET(*fd, &read_fds) || FD_ISSET(*fd, &write_fds))
	    break;
	if (FD_ISSET(disp_fd, &read_fds)) {
	    wait_data = &wd;
	    XtAppProcessEvent(app_cont, MASK);
	    wait_data = NULL;
	    if (*fd < 0)
		return -1;
	}
#endif
    }

    return 0;
}

void abort_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    if (!wait_data || wait_data->fd < 0 || !wait_data->func)
	return;

    wait_data->func(wait_data->data);
    wait_data = NULL;
}

/***********************************************************************/

struct SERV_ADDR {
    struct in_addr	addr;
    unsigned short	port;
};

/*
 * host:	either "hostname:port" or "ip.ip.ip.ip:port",
 *		where port is optional.
 * def_port:	port to use is 'host' doesn't have one.
 * byte_swap:	whether to apply htons() on def_port.
 *
 * Returns malloced memory: caller must free.
 */
SERV_ADDR *get_host(char *host, unsigned short def_port, int byte_swap)
{
    SERV_ADDR		*ret;
    struct in_addr	addr;
    unsigned short	port;
    unsigned long	l;
    char		*c;

#ifdef PURIFY_HACK
    static unsigned char	news[] = {130, 237, 72, 211};

    ret = (SERV_ADDR *)XtMalloc(sizeof *ret);
    ret->port = htons(119);
    memcpy(&ret->addr, news, 4);

    return ret;
#endif

    port = 0;
    c = strchr(host, ':');
    if (c) {
	*c = '\0';
	if (c[1] >= '0' && c[1] <= '9')
	    port = atoi(c + 1);
    }

    if (port != 0)
	port = htons(port);
    else if (byte_swap)
	port = htons(def_port);
    else
	port = def_port;

    l = inet_addr(host);
    if (l != INADDR_NONE)
	addr.s_addr = l;
    else {
	struct hostent	*hp;

#ifdef h_addr
#  define ADDR(hp) ((hp)->h_addr)
#else
#  define ADDR(hp) ((hp)->h_addr_list[0])
#endif
	hp = gethostbyname(host);
	if (!hp || !ADDR(hp)) {
	    if (c)
		*c = ':';
	    return NULL;
	}
	memcpy(&addr, ADDR(hp), hp->h_length);
#undef ADDR
    }

    if (c)
	*c = ':';

    ret = (SERV_ADDR *)XtMalloc(sizeof *ret);
    memset(ret, 0, sizeof *ret);
    ret->addr = addr;
    ret->port = port;

    return ret;
}

int open_socket(void)
{
    int	fd, tmp;

    do {
	fd = socket(PF_INET, SOCK_STREAM, 0);
    } while (fd < 0 && errno == EINTR);

    if (fd < 0) {
	perror("knews: socket");
	return -1;
    }

    if (set_nonblock(fd) < 0) {
	close(fd);
	return -1;
    }

    if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0)
	perror("fcntl");

    do {
	int	a = 1;

	tmp = setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char *)&a, sizeof a);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("knews: setsockopt");
	close(fd);
	return -1;
    }

    return fd;
}

int connect_socket(int fd, SERV_ADDR *addr)
{
    struct sockaddr_in	serv_addr;
    int			tmp;

    if (!addr) {
	fputs("knews: connect_socket: addr is NULL!!!\n", stderr);
	return -1;
    }

    memset(&serv_addr, 0, sizeof serv_addr);
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port   = addr->port;
    serv_addr.sin_addr   = addr->addr;

    do {
	tmp = connect(fd, (struct sockaddr *)&serv_addr, sizeof serv_addr);
    } while (tmp < 0 && errno == EINTR);

    return tmp;
}

#if 0 /* Misc stuff for ftp routines */
/*
 *  Creates a socket bound to an arbitrary port, and listens.
 */
int bind_and_listen(int backlog)
{
    struct sockaddr_in	addr;
    int			fd, tmp;

    fd = open_socket();
    if (fd < 0)
	return -1;

    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_port = 0;
    addr.sin_addr.s_addr = INADDR_ANY; /* ??? */

    do {
	tmp = bind(fd, &addr, sizeof addr);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("knews: bind");
	close(fd);
	return -1;
    }

    do {
	tmp = listen(fd, backlog);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("knews: listen");
	close(fd);
	return -1;
    }

    return fd;
}

/*
 *  Returns the address of a socket.
 */
SERV_ADDR *get_sock_name(int fd)
{
    struct sockaddr_in	addr;
    SERV_ADDR		*ret;
    int			tmp, len = sizeof addr;

    do {
	tmp = getsockname(fd, &addr, &len);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("knews_ getsockname");
	return NULL;
    }

    ret = (SERV_ADDR *)XtMalloc(sizeof *ret);
    ret->addr = addr.sin_addr;
    ret->port = addr.sin_port;

    return ret;
}

static void accept_abort(void *data)
{
    int	*as = data;

    if (as && *as >= 0) {
	close(*as);
	*as = -1;
    }
}

/*
 *  Accepts a connection on a socket returned from bind_and_listen.
 *  Will return -1 with errno == EINTR if user aborts, in which case
 *  the accepting socket has been closed and *as == 1.
 */
int do_accept(int *as)
{
    struct sockaddr_in	addr;
    int			tmp, len;

    for (;;) {
	tmp = accept(*as, (struct sockaddr *)&addr, &len);

	if (tmp >= 0)
	    break;
	if (errno == EINTR)
	    continue;
	if (!would_block(*as, errno))
	    break;
	if (do_wait(as, True, accept_abort, as) == 0)
	    continue;
	if (*as < 0)
	    errno = EINTR;
	break;
    }

    return tmp;
}

/*
 *  Prints an address on the form h1,h2,h3,h4,p1,p2
 */
void print_addr_ftp(SERV_ADDR *addr, char *buf)
{
    unsigned long	l = addr->addr.s_addr;
    unsigned short	p = addr->port;

    sprintf(buf, "%lu,%lu,%lu,%lu,%lu,%lu",
	    (l >> 24) & 0xfful, (l >> 16) & 0xfful,
	    (l >>  8) & 0xfful, (l      ) & 0xfful,
	    (p >>  8) & 0xfful, (p      ) & 0xfful);
}
#endif

int open_duplex(int *fd)
{
#if !defined(HAVE_SOCKETPAIR) || HAVE_SOCKETPAIR
    int	tmp;

    do {
	tmp = socketpair(PF_UNIX, SOCK_STREAM, 0, fd);
    } while (tmp < 0 && errno == EINTR);

    if (tmp < 0) {
	perror("knews: socketpair");
	return -1;
    }

    if (fcntl(fd[0], F_SETFD, FD_CLOEXEC) < 0)
	perror("fcntl");

    if (set_nonblock(fd[0]) < 0) {
	close(fd[0]);
	close(fd[1]);
	return -1;
    }

    return 0;
#else
    fputs("knews: compiled without HAVE_SOCKETPAIR, "
	  "can't open connection!\n", stderr);
    return -1;
#endif
}

/*************************************************************************/

char *get_mailhostname(void)
{
    struct utsname	  un = {{0,},};
    char		 *host = NULL, *domain;

#ifdef PURIFY_HACK
    return NULL;
#endif

    if (uname(&un) < 0) {
	perror("uname");
	return NULL;
    }

    if (un.nodename[0] == '\0')
	return NULL;

    if (strchr(un.nodename, '.'))
	host = XtNewString(un.nodename);
    else {
	struct hostent	 *hent;
	char		**loop;

	hent = gethostbyname(un.nodename);
	if (!hent)
	    return NULL;

	if (hent->h_name && strchr(hent->h_name, '.'))
	    host = XtNewString(hent->h_name);

	if (!host && hent->h_aliases)
	    for (loop = hent->h_aliases ; *loop ; loop++)
		if (strchr(*loop, '.')) {
		    host = XtNewString(*loop);
		    break;
		}
    }

#if !defined(DOMAIN_HACK) || DOMAIN_HACK
    /*
     *  If we have "host.subdomain.domain" and "subdomain.domain"
     *  has a DNS record, assume it is an MX...
     */
    if (host && (domain = strchr(host, '.')) &&
	strchr(++domain, '.') && gethostbyname(domain)) {
	domain = XtNewString(domain);
	XtFree(host);
	host = domain;
    }
#endif

    return host;
}

/*************************************************************************/

/*
 *  Dirty stinking hack for Sun's C library.
 */

#if defined(HAVE_MEMMOVE) && !HAVE_MEMMOVE
void *memmove(void *dest, const void *src, size_t n)
{
    bcopy(src, dest, n);
    return dest;
}
#endif

/*************************************************************************/

void sigusr1_handler(int s)
{
    int	o_errno = errno;
    int	i;

    for (i = 0 ; i < 256 ; i++) {
	struct stat	stat_buf;
	char		*type;

	if (fstat(i, &stat_buf) < 0) {
	    if (errno != EBADF) {
		char	buf[16];

		sprintf(buf, "fd %d", i);
		perror(buf);
	    }
	    continue;
	}

	if (S_ISDIR(stat_buf.st_mode))
	    type = "Directory";
	else if (S_ISREG(stat_buf.st_mode))
	    type = "Regular file";
#ifdef S_ISSOCK
	else if (S_ISSOCK(stat_buf.st_mode))
	    type = "Socket";
#endif
#ifdef S_ISCHR
	else if (S_ISCHR(stat_buf.st_mode))
	    type = "Character special file";
#endif
#ifdef S_ISFIFO
	else if (S_ISFIFO(stat_buf.st_mode))
	    type = "Pipe";
#endif
	else
	    type = "Unknown type";

	fprintf(stderr, "fd %d: %s\n", i, type);
    }

    errno = o_errno;
}
