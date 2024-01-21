/*
 * ipc routines, modelled on ipc(3x) of the Tenth Edition Research Unix manual,
 * for vanilla 4.2+BSD (no connection server)
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/param.h>

#include "ipc.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	128
#endif

#define STRLEN(s) (sizeof(s) - 1)	/* s must be an array */

/* imports */
extern char *memset(), *memcpy();
extern char *strsave(), *str3save();

extern int tcpopen(), tcplisten(), tcpconn();
extern int unixopen(), unixlisten(), unixconn();

/* private */
static char cspath[] = "/cs/";
static struct network {
	char	*name;
	int	(*open)();
	int	(*creat)();
	int	(*conn)();
} netsw[] = {
	{ "tcp",  tcpopen, tcplisten, tcpconn },
	{ "unix", unixopen, unixlisten, unixconn },
	{ NULL }
};
struct parsedname {
	char	*name;
	char	*service;
	struct network *np;
};

char *errstr;

static int
count(s, c)
register char *s;
register char c;
{
	register int cnt = 0;

	while (*s != '\0')
		if (*s++ == c)
			cnt++;
	return cnt;
}

/*
 * return "/cs/" prepended to:
 *	name, if it contains two or more bangs (network!host...!service);
 *	name!service, if name contains only one bang (network!host);
 *	network!name!service, if name contains no bangs (host).
 */
char *
ipcpath(name, network, service)
char *name, *network, *service;
{
	register char *res, *s1;
	static char *lastres;

	if (lastres != NULL)
		free(lastres);
	switch (count(name, '!')) {
	case 0:				/* name = host */
		s1 = str3save(network, "!", name);
		res = str3save(s1, "!", service);
		free(s1);
		break;
	case 1:				/* name = network!host */
		res = str3save(name, "!", service);
		break;
	default:			/* name = network![..!]host!service */
		res = strsave(name);
		break;
	}
	lastres = str3save(cspath, res, "");
	free(res);
	return lastres;
}
/*
 * parse `name' argument to ipcopen and ipccreat;
 * returns host, service & network.
 * if initial component is a relative file name, interpret it relative to /cs.
 * a missing service defaults to login (rlogin, telnet, etc.).
 */
static struct parsedname *
parsename(aname)
char *aname;
{
	register char *name;
	register struct network *np;
	static struct parsedname pn;
	register struct parsedname *pp = &pn;
	register int pfxlen = 0;
	static char *sname;

	if (sname != NULL)
		free(sname);		/* from last time */
	sname = name = strsave(aname);
	if (strncmp(name, cspath, STRLEN(cspath)) == 0)
		name += STRLEN(cspath);
	for (np = netsw; np->name != NULL; np++) {
		pfxlen = strlen(np->name);
		if (strncmp(name, np->name, pfxlen) == 0 &&
		    name[pfxlen] == '!')
			break;
	}
	pp->np = np;
	if (np->name == NULL) {
		errno = EINVAL;
		errstr = "unknown network; sorry";
		return NULL;
	}
	name += pfxlen + 1;		/* skip "network!" */
	pp->name = name;
	name = strchr(name, '!');
	if (name == NULL) {
		errno = EINVAL;
		errstr = "no service named";
		return NULL;
	}
	*name++ = '\0';
	pp->service = name;
	return pp;
}

/*
 * initiate connection to network end point `name' and return open
 * file descriptor for the connection.  `param' gives properties
 * needed: "heavy", "light", "delim", "hup".
 */
/* ARGSUSED param */
int
ipcopen(name, param)			/* place an outbound call */
register char *name;
char *param;				/* ignored */
{
	register struct parsedname *pp = parsename(name);

	if (pp == NULL)
		return -1;		/* errno, errstr already set */
	errstr = "ipcopen";
	return (*pp->np->open)(pp->name, pp->service);
}

#ifndef NOFILE
#define NOFILE 64
#endif

static struct network *fdnet[NOFILE];

/*
 * arguments are as per ipcopen.
 * advertise service `name'.
 */
/* ARGSUSED param */
int
ipccreat(name, param)			/* arrange for inbound calls */
register char *name;
char *param;				/* ignored */
{
	register struct parsedname *pp = parsename(name);
	register int fd;

	if (pp == NULL)
		return -1;		/* errno, errstr already set */
	errstr = "ipccreat";
	fd = (*pp->np->creat)(pp->service);
	if (fd >= 0 && fd < NOFILE) {
		fdnet[fd] = pp->np;	/* save for ipclisten */
		return fd;
	} else {
		int saverrno = errno;

		if (fd >= 0)
			(void) close(fd);
		errno = saverrno;
		return -1;
	}
}

static int newfd;

/*
 * wait for call on `fd' and return ipcinfo about it.
 */
ipcinfo *
ipclisten(fd)
int fd;
{
	static ipcinfo info;
	register ipcinfo *infop = &info;

	infop->machine = infop->name = infop->param = "";
	infop->user = "nobody";
	infop->uid = infop->gid = 1;
	newfd = (*fdnet[fd]->conn)(fd, infop);
	return (newfd < 0? NULL: infop);
}

/*
 * accept call corresponding to `ip'.
 */
/* ARGSUSED ip */
int
ipcaccept(ip)
ipcinfo *ip;
{
	return newfd;
}

/*
 * reject call corresponding to `ip' with `erno' and `erstr' returned
 * to caller as errno and errstr.
 */
/* ARGSUSED ip erno */
int
ipcreject(ip, erno, erstr)
ipcinfo *ip;
int erno;
char *erstr;
{
	/* errno = erno; */
	(void) write(newfd, erstr, strlen(erstr));
	(void) close(newfd);
	newfd = -1;
}

/* higher-level routines */

/* is it intended that anyone be able to use this?  assume so. */
/* ARGSUSED name param cmd */
int
ipcexec(name, param, cmd)
char *name, *param, *cmd;
{
	/* TODO: can this be implemented on 4.2? */
	return -1;
}

#include <pwd.h>

extern char *getpass();

int
ipclogin(fd)
int fd;
{
	int cnt;
	char login[8+1], passwd[8+1];
	char buf[4];

	cnt = read(fd, buf, 2);
	if (cnt < 2)
		return -1;
	if (buf[0] == 'O' && buf[1] == 'K')
		return 0;		/* other side likes us */
	if (buf[0] != 'N' || buf[1] != 'O')
		return -1;		/* protocol error */

	(void) strcpy(login, getpass("login: "));
	(void) strcpy(passwd, getpass("password: "));
	if (write(fd, login, strlen(login)) != strlen(login) ||
	    write(fd, ",", 1) != 1 ||
	    write(fd, passwd, strlen(passwd)) != strlen(passwd) ||
	    write(fd, "\n", 1) != 1)
		return -1;

	cnt = read(fd, buf, 2);
	if (cnt < 2)
		return -1;
	if (buf[0] == 'O' && buf[1] == 'K')
		return 0;		/* other side likes us */
	return -1;			/* other side hates us */
}

int
ipcrlogin(fd, opt)
int fd;
char *opt;
{
	register int userlen;
	char *user;
	struct passwd *pwp;

	pwp = getpwuid(getuid());
	if (pwp == NULL)
		return -1;
	user = pwp->pw_name;
	userlen = strlen(user) + 1;	/* include NUL */
	if (write(fd, user, userlen) != userlen ||
	    write(fd, user, userlen) != userlen ||
	    write(fd, opt, strlen(opt)) != strlen(opt))
		return -1;
	return 0;
}

#ifdef TEST
char *progname;

int
main()
{
	int fd;
	int cc;
	char *path;
	char buf[1024];

	path = ipcpath("relay2.uu.net", "tcp", "smtp");
	fd = ipcopen(path, "");
	if (fd < 0) {
		perror("ipcopen");
		printf("routine that failed: %s\n", errstr);
		exit(1);
	}
	cc = read(fd, buf, sizeof buf);
	if (cc < 0) {
		perror("read");
		exit(1);
	}
	write(1, buf, cc);
	(void) close(fd);
	return 0;
}
#endif
