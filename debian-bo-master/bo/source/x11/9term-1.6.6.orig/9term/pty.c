/*
 *  Copyright © 1993 James Farrow, University of Sydney - all rights reserved
 *
 *  9term pseudo-tty interface
 */

#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>

#include <utmp.h>
#include <errno.h>
#ifdef SUNOS
#	undef _POSIX_SOURCE
#	include <sys/termio.h>
#	include <stropts.h>
	/* Isn't POSIX and portability so much fun? */
#	define VRPRNT	VREPRINT
#endif
#if defined(IRIX)
#	include <termios.h>
#endif
#if defined(OSF1)
#	include <sys/tty.h>
#	include <sys/stat.h>
#	include <stropts.h>
#endif
#if defined(SOLARIS)
#	include <sys/stat.h>
#	define __EXTENSIONS__
#	include <stdlib.h>
#	include <stropts.h>
#	define	HASPIXELSIZE
#endif
#ifdef PCKT
#	include <sys/stream.h>
#endif
#include "9term.h"

#ifdef	RISCOS
#	include <sys/stat.h>
#	include </usr/include/ctype.h>		/* to defeat posix version */
#	define  VLNEXT V_LNEXT
#	define 	VDSUSP V_DSUSP
#	define	VRPRNT V_RPRNT
#	define	VWERASE V_WERAS
#else
#	include <ctype.h>
#endif

#ifdef	OSF1
#	define 	V_START VSTART
#	define 	V_STOP VSTOP
#	define 	V_SUSP VSUSP
#	define 	V_DSUSP VDSUSP
#	define	V_RPRNT VREPRINT
#	define	V_WERAS VWERASE
#	define	V_FLUSH VFLUSH
#endif

#ifdef	linux
#	define	V_START VSTART
#	define	V_STOP VSTOP
#	define	V_SUSP VSUSP
#	define	V_DSUSP VDSUSP
#	define	V_RPRNT VREPRINT
#	define	V_WERAS VWERASE
#	define	V_FLUSH VFLUSH
#endif

#ifdef	HPUX
#	define 	VEOL2 _VEOL2
#	define 	V_START VSTART
#	define 	V_STOP VSTOP
#	define 	V_SUSP VSUSP
#endif

#ifdef POSIXPTYS
#	include <sys/termios.h>
#	define	termio termios
#else
#	include <sys/termio.h>
#endif

#ifdef SOLARIS
#	define	IOSETATTR(fd, p)	ioctl((fd), TCSETSW, (p))
#	define	IOGETATTR(fd, p)	ioctl((fd), TCGETS, (p))
#else
#if defined(POSIXPTYS) || defined(SUNOS) || defined(__alpha)
#if defined(__ultrix) || defined(SUNOS) || defined(__alpha) || defined(linux)
	/* TCSADRAIN on Ultrix fouls up creatively. */
#	define	IOSETATTR(fd, p)	tcsetattr((fd), TCSANOW, (p))
#else
#	define	IOSETATTR(fd, p)	tcsetattr((fd), TCSADRAIN, (p))
#endif
#	define	IOGETATTR(fd, p)	tcgetattr((fd), (p))
#else
#	define	IOSETATTR(fd, p)	ioctl((fd), TCSETAW, (p))
#	define	IOGETATTR(fd, p)	ioctl((fd), TCGETA, (p))
#endif
#endif

#ifdef	__ultrix
	/* Ultrix handling of line disciplines is, to put it mildly,
	   *fucked*. */
#include	<sys/ioctl.h>
#endif

/*
 *	Termio/termios struct element order varies from system
 *	to system, so we initialize it programatically in
 *	parsettymodes() and gttymodes().
 */

#define ctrl(c) (c&0x1f)

typedef struct modenames	modenames;
struct modenames {
	char *name;
	int len;
	int symbol;
	char udef, p9def;
};

#if defined(SUNOS) || defined(IRIX) || defined(SOLARIS)
#	define V_START VSTART
#	define V_STOP VSTOP
#	define V_SUSP VSUSP
#	define V_DSUSP VDSUSP
#	ifdef SOLARIS
#		define V_RPRNT VREPRINT
#	else
#		define V_RPRNT VRPRNT
#	endif
#	define V_WERAS VWERASE
#	define V_FLUSH VDISCARD
#endif

modenames ttymodelist[] = {
	{ "intr",	4, VINTR,	0177,		0177 },
	{ "quit",	4, VQUIT,	ctrl('\\'),	ctrl('\\') },
	{ "erase",	5, VERASE,	ctrl('h'),	ctrl('h') },
	{ "kill",	4, VKILL,	ctrl('u'),	ctrl('u') },
	{ "eof",	3, VEOF,	ctrl('d'),	ctrl('d') },
	{ "eol",	3, VEOL,	0,		0 },
#ifndef _POSIX_SOURCE
	{ "eol2",	4, VEOL2,	0,		0 },
#endif
#if	!defined(_OSF_SOURCE) && !defined(_POSIX_SOURCE)
	{ "swtch", 	5, VSWTCH,	0,		0 },
#endif
	{ "start",	5, V_START,	ctrl('q'),	ctrl('q') },
	{ "stop",	4, V_STOP,	ctrl('s'),	ctrl('s') },
	{ "susp",	4, V_SUSP,	ctrl('z'),	ctrl('z') },
#ifndef HPUX
#ifndef IRIX
#ifndef linux
	{ "dsusp",	5, V_DSUSP,	ctrl('y'),	ctrl('y') },
#endif
#endif
	{ "rprnt",	5, V_RPRNT,	ctrl('r'),	ctrl('r') },
	{ "weras",	5, V_WERAS,	ctrl('w'),	ctrl('w') },
	{ "lnext",	5, VLNEXT,	ctrl('v'),	ctrl('v') },
#endif
#ifdef V_FLUSH
	{ "flush",	5, V_FLUSH,	ctrl('o'),	ctrl('o') },
#endif
#ifdef	VQUOTE
	{ "quote",	5, VQUOTE,	'\\',		'\\' },
#endif
	{ 0,		0, 0 ,		0,		0 },
	NULL
};
/* #undef ctrl */

static struct termio ttmode;

Rune		intrchar, p9_intrchar;	/* special characters */
Rune		quitchar, p9_quitchar;
Rune		erasechar, p9_erasechar;
Rune		killchar, p9_killchar;
Rune		eofchar, p9_eofchar;
Rune		eolchar, p9_eolchar;
Rune		wordchar, p9_wordchar;

int		canonical;	/* ioctl modes */
int		echo;
int		isig;

/*
 *	parse a tty mode override
 */
void
parsettymodes(int mode, char *s)
{
	modenames *mp;
	int c;

	while (1) {
		while (*s && isascii(*s) && isspace(*s))
			s++;
		if (!*s)
			break;

		for (mp = ttymodelist; mp->name; mp++)
	    		if (strncmp (s, mp->name, mp->len) == 0)
				break;
		s += mp->len;
		if (mp->name) {
			while (*s && isascii(*s) && isspace(*s))
				s++;
			if (!*s)
				return;
			if (*s == '^') {
	    			s++;
	    			c = ((*s == '?') ? 0177 : *s & 31);	 /* keep control bits */
			} else
	    			c = *s;
			if (mode == UNIX)
				mp->udef = c;
			else
				mp->p9def = c;
		}
		s++;
	}
}

char
p9cc(char *s)
{
	modenames *mp;

	for (mp = ttymodelist; mp->name; mp++)
		if (strcmp(s, mp->name) == 0)
			return mp->p9def;
	return 0;
}

void
p9ttymodes(void)
{
	p9_erasechar = p9cc("erase");
	p9_quitchar = p9cc("quit");
	p9_killchar = p9cc("kill");
	p9_intrchar = p9cc("intr");
	p9_wordchar = p9cc("weras");
	p9_eofchar = p9cc("eof");
	p9_eolchar = p9cc("eol2");
}

/*
 *	set the window size after a change.
 */
void
tty_set_size(int fd, int width, int height, int width_pixels, int height_pixels)
{
	struct winsize wsize;

	if (fd < 0)
		return;
	wsize.ws_row = height;
	wsize.ws_col = width;
#ifdef HASPIXELSIZE
	wsize.ws_xpixel = width_pixels;
	wsize.ws_ypixel = height_pixels;
#endif
	ioctl(fd, TIOCSWINSZ, (char *)&wsize);
}

/*
 *	Set modes modes in the pseudo-tty
 */
void
sttymodes(int fd)
{
	if (IOSETATTR(fd, &ttmode) < 0)
		perror("Can't set pty mode");
#ifdef	__ultrix
	{
		long l = ttmode.c_line;
		/* Force Ultrix, kicking and screaming, to preserve our
		   desired line discipline. You see, Ultrix ignores
		   requests to change the line discipline via POSIX
		   stuff -- and calls this a FEATURE. Bah. - cks */
		if (ioctl(fd, TIOCSETD, (char *)&l) < 0)
			perror("TIOCSETD");
	}
#endif
}

/*
 *	Get the original modes from a (pseudo)-tty.
 *	Supply a default value if we can't.
 */
void
gttymodes(int fd)
{
	modenames *mp;

	if (IOGETATTR(fd, &ttmode) >= 0) {
		/* Insure some sanity. */
		ttmode.c_lflag |= ECHO;
		ttmode.c_oflag &= ~(ONLCR);
		ttmode.c_oflag |= ONLRET;
	} else {
		ttmode.c_iflag = BRKINT | IGNPAR | ICRNL | IXON;
		ttmode.c_oflag = OPOST | ONLRET;
		ttmode.c_cflag = B9600 | PARENB | CS8 | CREAD;
		ttmode.c_lflag = ISIG | ICANON | ECHO | ECHOK;
#ifdef	__ultrix
		ttmode.c_line = 2;
#endif
	}
	for (mp = ttymodelist; mp->name; mp++)
		ttmode.c_cc[mp->symbol] = (kbdmode == UNIX) ? mp->udef : mp->p9def;
}

void
saveunixspecials(int fd)
{
	modenames *mp;

	if (IOGETATTR(fd, &ttmode) < 0)
		return;
	for (mp = ttymodelist; mp->name; mp++)
		mp->udef = ttmode.c_cc[mp->symbol];
}

/*
 *	get the current set of special characters from ioctl
 *		BACKSPACE -> erasechar
 *		LINE KILL -> killchar
 *		QUIT -> quitchar
 *		INTERRUPT -> interchar
 *		WORD KILL -> wordchar
 *		EOF -> eofchar
 *		ALTERNATE EOL -> eolchar
 *		ICANON -> canonical (boolean)
 *		ECHO -> echo (boolean)
 */
void
specialchars(int fd)
{
		/* In plan 9 and unix modes, canonical & echo
		 * depend on the pty driver's current state.
		 */
	IOGETATTR(fd, &ttmode);
	canonical = (ttmode.c_lflag & ICANON) ? 1 : 0;
	echo = (ttmode.c_lflag & ECHO) ? 1 : 0;
	isig = (ttmode.c_lflag & ISIG) ? 1 : 0;
			/* plan 9 special characters are fixed */
	if (kbdmode == PLAN9) {
		erasechar = p9_erasechar;
		quitchar = p9_quitchar;
		killchar = p9_killchar;
		intrchar = p9_intrchar;
		wordchar = p9_wordchar;
		eofchar = p9_eofchar;
		eolchar = p9_eolchar;
	} else {	/* unix users drown in choices */
		erasechar = ttmode.c_cc[VERASE];
		quitchar = ttmode.c_cc[VQUIT];
		killchar = ttmode.c_cc[VKILL];
		intrchar = ttmode.c_cc[VINTR];
#ifdef	VWERASE
		wordchar = ttmode.c_cc[VWERASE];
#else
		wordchar = ctrl('w');
#endif
		eofchar = ttmode.c_cc[VEOF];
		eolchar = ttmode.c_cc[VEOL2];
	}
}
#undef ctrl

/*
 *	send the indicated number of Runes to the command interpreter
 */
void
sendrunes(Rune *r, ulong len)
{
	char		s[128*UTFmax+1];
	char		*p;
	static Rune	firstrune = 0;

	if (!r || len <= 0)
		return;
#if !defined(PCKT) && !defined(REMOTE)
		/* disable echo mode - we echo locally, when possible */
	IOGETATTR(slave_fd, &ttmode);
	echo = (ttmode.c_lflag & ECHO) ? 1 : 0;
	if (echo) {
		ttmode.c_lflag &= ~ECHO;
		IOSETATTR(slave_fd, &ttmode);
	}
#endif
		/* write it */
	p = s;
	while(len--) {
		if (kbdmode == UNIX && *r == killchar && canonical)
			*p++ = '\\';
#ifdef REMOTE
		if (*r == eofchar && canonical)
			r++;
		else
			p += runetochar(p, r++);
#else
		p += runetochar(p, r++);
#endif
		if (r[-1] == eofchar || p > s+sizeof(s)-UTFmax) {
			while (write(comm_fd, s, p-s) < 0 && errno == EAGAIN)
					;
			p = s;
		}
	}
	if (p > s)
		while (write(comm_fd, s, p-s) < 0 && errno == EAGAIN)
				;
#if !defined(PCKT) && !defined(REMOTE)
		/* reinstate echo mode, if we disabled it above */
	tcdrain(comm_fd);
	if (echo) {
		ttmode.c_lflag |= ECHO;
		IOSETATTR(slave_fd, &ttmode);
	}
#endif
}

/*
 *	flushstream: get rid of typeahead
 */

void
flushstream(void)
{
#ifdef I_FLUSH
	ioctl(comm_fd, I_FLUSH, FLUSHW);
#endif
}

void
setttyecho(int newecho)
{
	if (newecho) {
		ttmode.c_lflag |= ECHO;
	} else {
		ttmode.c_lflag &= ~ECHO;
	}
	IOSETATTR(slave_fd, &ttmode);
	echo = newecho;
}

/***********************************************************************/

/*
 *	Some vendors supply _getpty to find a pseudo-tty, open the master
 *	end and return the name of the slave end.  MIPS and SUN can't be
 *	bothered and each supplies a potentially incompatible method of
 *	locating an available pseudo-tty.
 */

#ifdef RISCOS

#define MASTERCLONE	"/dev/ptc"
#define TTYPREFIX	"/dev/ttyq"
/*
 *	open the master end of a pseudo-tty and return the name
 *	of the slave end (Mips/SYSV version).
 */
char *
_getpty(int *fd, int mode, int perm, int flag)
{

	struct stat stat;

	static char slave[256];

	*fd = open(MASTERCLONE, mode);
	if (*fd < 0)
		return 0;

	if (fstat(*fd, &stat) < 0)
		return 0;
				/* use minor device number to build name */
	sprintf(slave, "%s%d", TTYPREFIX, stat.st_rdev&0xFF);
	chmod(slave, perm);
	return slave;
}

#endif	/* defined(RISCOS) */

#ifdef OSF1
/*
 *	open the master end of a pseudo-tty and return the name
 *	of the slave end (Mips/SYSV version).
 */
char *
_getpty(int *fd, int mode, int perm, int flag)
{
	int slavepty;

	static char slave[256];

	openpty(fd, &slavepty, slave, NULL, NULL);
	if (*fd < 0)
		return 0;
	fchmod(slavepty, perm);
	close(slavepty);
	return slave;
}

#endif /* defined(OSF1) */

#ifdef SOLARIS

#define MASTERCLONE	"/dev/ptmx"
#define TTYPREFIX	"/dev/pts/"
/*
 *	open the master end of a pseudo-tty and return the name
 *	of the slave end (Solaris version).
 */
char *
_getpty(int *fd, int mode, int perm, int flag)
{
#ifdef PCKT
	struct strbuf msg;
	struct stroptions opts;
#endif

	*fd = open(MASTERCLONE, mode);
	if ((grantpt(*fd) < 0) || (unlockpt(*fd) < 0))
		return 0;
	fchmod(*fd, perm);
#ifdef PCKT
	ioctl(*fd, I_PUSH, "pckt");        /* push ptem */
	ioctl(*fd, I_SRDOPT, RMSGN|RPROTDAT);
	msg.len = sizeof(struct stroptions);
	msg.buf = (void *)&opts;
	opts.so_flags = SO_MREADON;
	putmsg(*fd, NULL, &msg, M_SETOPTS);
#endif
	return (char *)ptsname(*fd);
}
#endif	/* SOLARIS */

#ifdef BSDPTYS
/*
 *	open the master end of a pseudo-tty and return the name
 *	of the slave end (BSD version).
 */
char *
_getpty(int *fd, int mode, int perm, int flag)
{
	char *c1, *c2;
	int sfd;

	static char slave[]= "/dev/ptyXX";

	slave[5] = 'p';
	for (c1 = "pqrstuvwxyzABCDE"; *c1; c1++) {
		slave[8] = *c1;
		for (c2 = "0123456789abcdef"; *c2; c2++) {
			slave[9] = *c2;
			*fd = open(slave, mode);
			if (*fd < 0)
				continue;
			fchmod(*fd, perm);
			slave[5] = 't';
			sfd = open(slave, mode);
			if (sfd >= 0) {
				close(sfd);
				return slave;
			}
			close(*fd);
			slave[5] = 'p';
		}
	}
	return 0;
}
		
#endif
/****************************************************************************/

/*
 *	The formats of the entries in BSD-style and SYSV-style utmp files
 *	are completely different.  SYS V also supplies functions for
 *	manipulating the file.  Sun lets everyone grubby their hands
 *	with the details of the file organization.
 */

#ifdef BSDPTYS
static	int slot = -1;

/*
 *	update the utmp file for the tty with the user info (BSD version)
 */
void
updateutmp(char *ttyname, int pid)
{
	char *user, *display, *cp;
	struct utmp utmp;
	int fd;
	int save_fd;

	user = getuser();
	fd = open("/etc/utmp", O_RDWR);
	if (fd < 0)
		return;
#ifndef SUNOS
	/*
	 * search for existing entry or add a new entry
	 * to the end of the file if one is not found.
 	 */
	slot = 0;
	while(read(fd, &utmp, sizeof(utmp)) == sizeof(utmp)) {
		if (!strncmp(ttyname, utmp.ut_line, sizeof(utmp.ut_line))) {
			lseek(fd, -sizeof(utmp), 1);
			break;
		}
		slot++;
	}
#else /* SUNOS */
	/* XXX - ttyslot assumes that fd 0 is already attached to the 
	   9term pty, so we jump thru some hoops to make it so, and
	   then put it back.  Since we don't use stdin, we could probably
	   leave off the last bit. */
#undef dup
	save_fd = dup(0);
	if (save_fd < 0)
		goto done;
        dup2(slave_fd, 0);
	slot = ttyslot();
	dup2(save_fd, 0);
	close(save_fd);
	if (slot <= 0)
		goto done;
	
	lseek (fd, slot*sizeof(utmp), SEEK_SET);	
#endif /* SUNOS */

		/* build the entry and write it */
	strncpy(utmp.ut_line, ttyname, sizeof(utmp.ut_line));
	strncpy(utmp.ut_name, user, sizeof(utmp.ut_name));
	display = getenv("DISPLAY");
	if (display) {
		cp = strchr(display, ':');
		if (cp)
			*cp = 0;
		strncpy(utmp.ut_host, display, sizeof(utmp.ut_host));
	}
	time(&utmp.ut_time);
	write(fd, &utmp, sizeof(utmp));
done:
	close(fd);
}

void
clearutmp(char *ttyname, int pid)
{
	int fd;
	struct utmp utmp;

	if (slot < 0)		/* never written */
		return;
	fd = open("/etc/utmp", O_RDWR);
	if (fd < 0)
		return;
	lseek(fd, slot*sizeof(struct utmp), 0);
	memset(&utmp, 0, sizeof(struct utmp));
	write(fd, &utmp, sizeof(struct utmp));
	close(fd);
}
#else

/*
 *	update the utmp file for the tty with the user info (SYS V version)
 */
void
updateutmp(char *ttyname, int pid)
{
	char *user;
	struct utmp utmp;

	user = getuser();

	/*
	 * the utmp file is modeled as a magnetic tape; this
	 * is preferrable to modeling it as a punched card.
	 */
	setutent();		/* rewind() */
	utmp.ut_type = DEAD_PROCESS;
	bzero(utmp.ut_id, sizeof(utmp.ut_id));
	strncpy(utmp.ut_id, ttyname+3, sizeof(utmp.ut_id));
	getutid(&utmp);		/* search() */

	utmp.ut_type = USER_PROCESS;
	utmp.ut_exit.e_exit = 2;
	strncpy(utmp.ut_user, user, sizeof(utmp.ut_user));
	strncpy(utmp.ut_line, ttyname, sizeof(utmp.ut_line));
	utmp.ut_pid = pid;
	time(&utmp.ut_time);
	pututline(&utmp);	/* write() */
	endutent();		/* unload() */
}

void
clearutmp(char *ttyname, int pid)
{
	struct utmp utmp, *up;

	setutent();		/* rewind() */
	utmp.ut_type = USER_PROCESS;
	bzero(utmp.ut_id, sizeof(utmp.ut_id));
	strncpy(utmp.ut_id, ttyname+3, sizeof(utmp.ut_id));
	up = (struct utmp *)getutid(&utmp);	/* search() */
	if (up && up->ut_pid == pid) {
		up->ut_type = DEAD_PROCESS;
		up->ut_time = 0;
		pututline(up);	/* write() */
	}
	endutent();		/* unload() */
}
	
#endif		/*ifdef BSDPTYS*/

#ifdef PCKT
/*
 *	Handle control messages comming from the pckt module
 */
void
ctlmsg(Event *e)
{
	struct iocblk ioc;

	switch(e->data[0]) {
/*
	case M_PROT0:
		fprintf(stderr, "M_PROT0\n");
		break;
*/
	case M_PCPROTO:
		fprintf(stderr, "M_PCPROTO\n");
		break;
	case M_STOP:
	case M_START:
	case M_STOPI:
	case M_STARTI:
		break;
	case M_IOCTL:
		memcpy(&ioc, &e->data[1], sizeof(struct iocblk));
		fprintf(stderr, "M_IOCTL\n");
		switch (ioc.ioc_cmd) {
			case TIOCSWINSZ:
				fprintf(stderr, "\tTIOCSWINSZ\n");
				break;
			case TCSETSW:
				fprintf(stderr, "\tTCSETSW\n");
				break;
			default:
				fprintf(stderr, "\tunknown %x\n", ioc.ioc_cmd);
		}
		break;
	case M_DATA:
		/* can't happen ;-) */
		break;
	case M_FLUSH:
		fprintf(stderr, "M_FLUSH\n");
		break;
	case M_READ:
		fprintf(stderr, "M_READ\n");
		break;
	default:
		fprintf(stderr, "unknown %02x\n", e->data[0]);
	}
}
#endif

#ifdef USE_SYSCONF
int
getdtablesize()
{
    return sysconf(_SC_OPEN_MAX);
}
#endif
