/*
**	$Id: funcs.c,v 2.0 90/09/19 19:53:19 paul Rel $
**
**	Miscellaneous routines.
*/

/*
**	Copyright 1989,1990 by Paul Sutcliffe Jr.
**
**	Permission is hereby granted to copy, reproduce, redistribute,
**	or otherwise use this software as long as: there is no monetary
**	profit gained specifically from the use or reproduction or this
**	software, it is not sold, rented, traded or otherwise marketed,
**	and this copyright notice is included prominently in any copy
**	made.
**
**	The author make no claims as to the fitness or correctness of
**	this software for any use whatsoever, and it is provided as is. 
**	Any use of this software is at the user's own risk.
*/


#include "getty.h"
#include "table.h"
#include <ctype.h>
#include <time.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <stdarg.h>
#include <syslog.h>
#include <errno.h>
#include "debug.h"

#ifndef	MAXBUF
#define	MAXBUF	512	/* buffer size */
#endif	/* MAXBUF */

#ifndef	EXPFAIL
#define	EXPFAIL	30	/* default num seconds to wait for expected input */
#endif	/* EXPFAIL */

#define	EXPECT	0	/* states for chat() */
#define	SEND	1

#define	AUTOBD	((char)0376)	/* marker for AutoBaud digits */

extern char *fido;
extern char *emsi;

char	*unquote();
int	expect(), send();
boolean	expmatch();
sig_t	expalarm();


/*
**	Fputs() - does fputs() with '\' and '@' expansion
**
**	Returns EOF if an error occurs.
*/

int
Fputs(s, stream)
register char *s;
register FILE *stream;
{
	char c, n, tbuf[16], ubuf[32];
	time_t clock;
	struct tm *lt, *localtime();
	char *month_name[] = { "January", "February", "March", "April",
			       "May", "June", "July", "August", "September",
			       "October", "November", "December" };

	while ((c = *s++)) {
		if ((c == '@') && (n = *s++)) {
			switch (n) {
			case 'B':	/* speed (baud rate) */
				if (*Speed && Fputs(Speed, stream) == EOF)
					return(EOF);
				break;
			case 'D':	/* date */
				(void) time(&clock);
				lt = localtime(&clock);
				(void) sprintf(tbuf, "%d %s %02d",
						lt->tm_mday,
						month_name[lt->tm_mon],
						lt->tm_year);
				if (Fputs(tbuf, stream) == EOF)
					return(EOF);
				break;
			case 'L':	/* line */
				if (*Device && Fputs(Device, stream) == EOF)
					return(EOF);
				break;
			case 'S':	/* system node name */
				if (*SysName && Fputs(SysName, stream) == EOF)
					return(EOF);
				break;
			case 'T':	/* time */
				(void) time(&clock);
				lt = localtime(&clock);
				(void) sprintf(tbuf, "%02d:%02d:%02d",
						lt->tm_hour,
						lt->tm_min, lt->tm_sec);
				if (Fputs(tbuf, stream) == EOF)
					return(EOF);
				break;
			case 'U':	/* number of active users */
				(void) sprintf(ubuf, "%d", Nusers);
				if (Fputs(ubuf, stream) == EOF)
					return(EOF);
				break;
			case 'V':	/* version */
				if (*Version && Fputs(Version, stream) == EOF)
					return(EOF);
				break;
			case '@':	/* in case '@@' was used */
				if (fputc(n, stream) == EOF)
					return(EOF);
				break;
			default:
				if ((fputc('@', stream) == EOF) ||
				  (fputc(n, stream) == EOF))
					return(EOF);
			}
		} else {
			if (c == '\\')
				s = unquote(s, &c);
			/* we're in raw mode: send CR before every LF
			 */
			if (c == '\n' && (fputc('\r', stream) == EOF))
				return(EOF);
			if (c && fputc(c, stream) == EOF)
				return(EOF);
		}
	}
	return(SUCCESS);
}


/*
**	getuname() - retrieve the system's node name
*/

char *
getuname()
{
	static char name[80];

	if (gethostname(name, 80))
		name[0] = '\0';
	return(name);
}


/*
**	settermio() - setup tty according to termio values
*/

void
settermio(termio, state)
register TERMIO *termio;
int state;
{
	register int i;
	static TERMIO setterm;

	char Cintr = CINTR;

#ifdef	MY_ERASE
	char Cerase = MY_ERASE;
#else
	char Cerase = CERASE;
#endif	/* MY_ERASE */

#ifdef	MY_KILL
	char Ckill = MY_KILL;
#else
	char Ckill = CKILL;
#endif	/* MY_KILL */

	(void) ioctl(STDIN, TCGETS, &setterm);

	switch (state) {
	case INITIAL:
		setterm.c_iflag = termio->c_iflag;
		setterm.c_oflag = termio->c_oflag;
		setterm.c_cflag = termio->c_cflag;
		setterm.c_lflag = termio->c_lflag;
		setterm.c_line  = termio->c_line;

		/* single character processing
		 */
		setterm.c_lflag &= ~(ICANON);
		setterm.c_cc[VMIN] = 1;
		setterm.c_cc[VTIME] = 0;

		/* sanity check
		 */
		if ((setterm.c_cflag & CBAUD) == 0)
			setterm.c_cflag |= B9600;
		if ((setterm.c_cflag & CSIZE) == 0)
			setterm.c_cflag |= DEF_CFL;
		setterm.c_cflag |= (CREAD | HUPCL);

		(void) ioctl(STDIN, TCSETSF, &setterm);
		break;

	case FINAL:
		setterm.c_iflag = termio->c_iflag;
		setterm.c_oflag = termio->c_oflag;
		setterm.c_cflag = termio->c_cflag;
		setterm.c_lflag = termio->c_lflag;
		setterm.c_line  = termio->c_line;

		/* sanity check
		 */
		if ((setterm.c_cflag & CBAUD) == 0)
			setterm.c_cflag |= B9600;
		if ((setterm.c_cflag & CSIZE) == 0)
			setterm.c_cflag |= DEF_CFL;
		setterm.c_cflag |= CREAD;

		/* set c_cc[] chars to reasonable values
		 */
		for (i=0; i < NCC; i++)
			setterm.c_cc[i] = CNUL;
		setterm.c_cc[VINTR] = Cintr;
		setterm.c_cc[VQUIT] = CQUIT;
		setterm.c_cc[VERASE] = Cerase;
		setterm.c_cc[VKILL] = Ckill;
		setterm.c_cc[VEOF] = CEOF;
#ifdef	CEOL
		setterm.c_cc[VEOL] = CEOL;
#endif	/* CEOL */
		setterm.c_cc[VMIN] = 1;

		(void) ioctl(STDIN, TCSETSW, &setterm);
		break;

	}
}


/*
**	chat() - handle expect/send sequence to Device
**
**	Returns FAIL if an error occurs.
*/

int
chat(s)
char *s;
{
	register int state = EXPECT;
	boolean finished = FALSE, if_fail = FALSE;
	char c, *p;
	char word[MAXLINE+1];		/* buffer for next word */

	debug(D_INIT, "chat(%s) called", s);

	while (!finished) {
		p = word;
		while (((c = (*s++ & 0177)) != '\0') && c != ' ' && c != '-')
			/*
			 *  SMR - I don't understand this, because if c is \0
			 *  then it is 0, isn't it?  If so we end the loop and
			 *  terminate the word anyway.
			 *
			*p++ = (c) ? c : '\177';
			 */
			*p++ = c;

		finished = (c == '\0');
		if_fail = (c == '-');
		*p = '\0';

		switch (state) {
		case EXPECT:
			if (expect(word) == FAIL) {
				if (if_fail == FALSE)
					return(FAIL);	/* no if-fail seq */
			} else {
				/* eat up rest of current sequence
				 */
				if (if_fail == TRUE) {
					while ((c = (*s++ & 0177)) != '\0' &&
						c != ' ')
						;
					if (c == '\0')
						finished = TRUE;
					if_fail = FALSE;
				}
			}
			state = SEND;
			break;
		case SEND:
			if (send(word) == FAIL)
				return(FAIL);
			state = EXPECT;
			break;
		}
		continue;
	}
	debug(D_INIT, "chat() successful");
	return (SUCCESS);
}


/*
**	unquote() - decode char(s) after a '\' is found.
**
**	Returns the pointer s; decoded char in *c.
*/

char	valid_oct[] = "01234567";
char	valid_dec[] = "0123456789";
char	valid_hex[] = "0123456789aAbBcCdDeEfF";

char *
unquote(s, c)
char *s, *c;
{
	int value, base;
	char n, *valid;

	n = *s++;
	switch (n) {
	case 'b':
		*c = '\b';	break;
	case 'c':
		if ((n = *s++) == '\n')
			*c = '\0';
		else
			*c = n;
		break;
	case 'f':
		*c = '\f';	break;
	case 'n':
		*c = '\n';	break;
	case 'r':
		*c = '\r';	break;
	case 's':
		*c = ' ';	break;
	case 't':
		*c = '\t';	break;
	case '\n':
		*c = '\0';	break;	/* ignore NL which follows a '\' */
	case '\\':
		*c = '\\';	break;	/* '\\' will give a single '\' */
	default:
		if (isdigit(n)) {
			value = 0;
			if (n == '0') {
				if (*s == 'x') {
					valid = valid_hex;
					base = 16;
					s++;
				} else {
					valid = valid_oct;
					base = 8;
				}
			} else {
				valid = valid_dec;
				base = 10;
				s--;
			}
			while (strpbrk(s, valid) == s) {
				value = (value * base) + (int) (*s - '0');
				s++;
			}
			*c = (char) (value & 0377);
		} else {
			*c = n;
		}
		break;
	}
	return(s);
}


/*
**	send() - send a string to stdout
*/

int
send(s)
register char *s;
{
	register int retval = SUCCESS;
	char ch;

	sprintf(MsgBuf, "SEND: (");

	if (strequal(s, "\"\"")) {	/* ("") used as a place holder */
		strcat(MsgBuf, "[nothing])");
		debug(D_INIT, MsgBuf);
		return(retval);
	}

	while ((ch = *s++)) {
		if (ch == '\\') {
			switch (*s) {
			case 'p':		/* '\p' == pause */
				strcat(MsgBuf, "[pause]");
				(void) sleep(1);
				s++;
				continue;
			case 'd':		/* '\d' == delay */
				strcat(MsgBuf, "[delay]");
				(void) sleep(2);
				s++;
				continue;
			case 'K':		/* '\K' == BREAK */
				strcat(MsgBuf, "[break]");
				(void) ioctl(STDOUT, TCSBRK, 0);
				s++;
				continue;
			default:
				s = unquote(s, &ch);
				break;
			}
		}
		sprintf(MsgBuf + strlen(MsgBuf), ch < ' ' ? "^%c" : "%c",
			ch < ' ' ? ch | 0100 : ch);
		if (putc(ch, stdout) == EOF) {
			retval = FAIL;
			break;
		}
	}
	strcat(MsgBuf, ") -- ");
	if(retval == SUCCESS)
		strcat(MsgBuf, "OK");
	else
		sprintf(MsgBuf + strlen(MsgBuf), 
			"Failed: %s", strerror(errno));
	debug(D_INIT, MsgBuf);
	return(retval);
}


/*
**	expect() - look for a specific string on stdin
*/

jmp_buf env;	/* here so expalarm() sees it */

int
expect(s)
register char *s;
{
	register int i;
	register int expfail = EXPFAIL;
	register retval = FAIL;
	char ch, *p, word[MAXLINE+1], buf[MAXBUF];
	sig_t (*oldalarm)() = NULL;

	if (strequal(s, "\"\"")) {	/* ("") used as a place holder */
		debug(D_INIT, "EXPECT: ([nothing])");
		return(SUCCESS);
	}

	/* look for escape chars in expected word
	 */
	for (p = word; (ch = (*s++ & 0177));) {
		if (ch == '\\') {
			if (*s == 'A') {	/* spot for AutoBaud digits */
				*p++ = AUTOBD;
				s++;
				continue;
			} else if (*s == 'T') {	/* change expfail timeout */
				if (isdigit(*++s)) {
					s = unquote(s, &ch);
					/* allow 3 - 255 second timeout */
					if ((expfail = ((unsigned char) ch)) < 3)
						expfail = 3;
				}
				continue;
			} else
				s = unquote(s, &ch);
		}
		*p++ = (ch) ? ch : '\177';
	}
	*p = '\0';

	if (setjmp(env)) {	/* expalarm returns non-zero here */
		debug(D_INIT, "[timed out after %d seconds]", expfail);
		(void) signal(SIGALRM, oldalarm);
		return(FAIL);
	}

	oldalarm = signal(SIGALRM, expalarm);
	(void) alarm((unsigned) expfail);

	sprintf(MsgBuf, "EXPECT: <%d> (%s), GOT: ", expfail, dprint(word));

	p = buf;
	while ((ch = getc(stdin)) != EOF) {
		sprintf(MsgBuf + strlen(MsgBuf), ch < ' ' ? "^%c" : "%c",
			ch < ' ' ? ch | 0100 : ch);
		*p++ = (char) ((int) ch & 0177);
		*p = '\0';
		if (strlen(buf) >= strlen(word)) {
			for (i=0; buf[i]; i++)
				if (expmatch(&buf[i], word)) {
					retval = SUCCESS;
					break;
				}
		}
		if (retval == SUCCESS)
			break;
	}
	(void) alarm((unsigned) 0);
	(void) signal(SIGALRM, oldalarm);
	debug(D_INIT, "%s -- %s", MsgBuf, 
		(retval == SUCCESS) ? "got it" : "Failed");
	return(retval);
}


/*
**	expmatch() - compares expected string with the one gotten
*/

#ifdef	TELEBIT
char	valid[] = "0123456789FAST";
#else	/* TELEBIT */
char	valid[] = "0123456789";
#endif	/* TELEBIT */

boolean
expmatch(got, exp)
register char *got;
register char *exp;
{
	register int ptr = 0;

	while (*exp) {
		if (*exp == AUTOBD) {	/* substitute real digits gotten */
			while (*got && strpbrk(got, valid) == got) {
				AutoBaud = TRUE;
				if (ptr < (sizeof(AutoRate) - 2))
					AutoRate[ptr++] = *got;
				got++;
			}
			if (*got == '\0')
				return(FALSE);	/* didn't get it all yet */
			AutoRate[ptr] = '\0';
			exp++;
			continue;
		}
		if (*got++ != *exp++)
			return(FALSE);		/* no match */
	}
	return(TRUE);
}


/*
**	expalarm() - called when expect()'s SIGALRM goes off
*/

sig_t
expalarm()
{
	longjmp(env, 1);
}


/*
**	getlogname() - get the users login response
**
**	Returns int value indicating success.
**	If a fido-protocol is detected, return the name of the protocol.
*/

int
getlogname(termio, name, size)
TERMIO *termio;
register char *name;
int size;
{
	register int count;
	register int lower = 0;
	register int upper = 0;
	int tsynccount=0;
	int yoohoocount=0;
	char ch, *p;
	ulong lflag;

#ifdef	MY_ERASE
	char Erase = MY_ERASE;
#else
	char Erase = CERASE;
#endif	/* MY_ERASE */
#ifdef	MY_KILL
	char Kill = MY_KILL;
#else
	char Kill = CKILL;
#endif	/* MY_KILL */

	debug(D_RUN, "getlogname() called");
	fflush(stdout);

	(void) ioctl(STDIN, TCGETS, termio);
	lflag = termio->c_lflag;

	termio->c_iflag = 0;
	termio->c_oflag = 0;
	termio->c_cflag = 0;
	termio->c_lflag = 0;

	p = name;	/* point to beginning of buffer */
	count = 0;	/* nothing entered yet */

	do {
		if (read(STDIN, &ch, 1) != 1)	/* nobody home */
			exit(0);
		/* if ((ch = (char) ((int) ch & 0177)) == CEOF) */
		if (ch == CEOF)
			if (p == name)		/* ctrl-d was first char */
				exit(0);
		if (ch == CQUIT)		/* user wanted out, i guess */
			exit(0);
		if (ch == '\0') {
			debug(D_RUN, "returned (BADSPEED)");
			return(BADSPEED);
		}
#ifdef FIDO
		if (fido && (ch == TSYNC))
		if (tsynccount++ > 1) {
			strcpy(name,"tsync");
			return (FIDOCALL);
		}
		if (fido && (ch == YOOHOO))
		if (yoohoocount++ > 1) {
			strcpy(name,"yoohoo");
			return (FIDOCALL);
		}
#endif
		if (!(lflag & ECHO)) {
			(void) putc(ch, stdout);
			(void) fflush(stdout);
		}
		if (ch == Erase) {
			if (count) {
				(void) fputs((Erase == '\010' ? 
					" \b" : "\b \b"), stdout);
				(void) fflush(stdout);
				--p;
				--count;
			}
		} else if (ch == Kill) {
			for( ;count ;count--)
				(void) fputs("\b \b", stdout);
			(void) fflush(stdout);
			p = name;
		} else if (((unsigned)ch >= ' ') &&
			   (ch != TSYNC) &&
			   (ch != YOOHOO)) {
			*p++ = ch;
			count++;
			if (islower(ch))
				lower++;
			if (isupper(ch))
				upper++;
		}
	} while ((ch != '\n') && (ch != '\r') && (count < size));

	*(p) = '\0';	/* terminate buffer */

	if (ch == '\r') {
		(void) putc('\n', stdout);
		(void) fflush(stdout);
		termio->c_iflag |= ICRNL;	/* turn on cr/nl xlate */
		termio->c_oflag |= ONLCR;
	} else if (ch == '\n') {
		(void) putc('\r', stdout);
		(void) fflush(stdout);
	}

	if (strlen(name) == strspn(name," ")) {
		debug(D_RUN, "returned (NONAME)");
		return(NONAME);
	}

#ifdef FIDO
	for (p=name;(*p) && (*p == '*');p++);
	if ((strncmp(p,"EMSI_",5) == 0) ||
	    (strncmp(p,"emsi_",5) == 0))
	if (emsi && (strcmp(emsi,"yes") == 0))
	{
		debug(D_RUN, "returned EMSI call");
		return(FIDOCALL);
	}
	else
	{
		debug(D_RUN, "EMSI call,return (NONAME)");
		return(NONAME);
	}
#endif

	if (upper && !lower) {
#ifdef	WARNCASE
		if (WarnCase) {
			WarnCase = FALSE;
			debug(D_RUN, "returned (BADCASE)");
			return(BADCASE);
		}
#endif	/* WARNCASE */
		for (p=name; *p; p++)		/* make all chars UC */
			*p = toupper(*p);
		termio->c_iflag |= IUCLC;
		termio->c_oflag |= OLCUC;
		termio->c_lflag |= XCASE;
	}

	debug(D_RUN, "returned (SUCCESS), name=(%s)", name);
	return(SUCCESS);
}


/*
**	logerr() - display an error message
*/

void logerr(char *fmt, ...)
{
	va_list args;
#if !defined(SYSLOG) || !defined(SYSL_ERROR)
	register FILE *co;
	char *errdev;
	time_t clock;
#endif

	va_start(args, fmt);
#if defined(SYSLOG) && defined(SYSL_ERROR)
	openlog(MyName, LOG_PID | LOG_CONS, SYSL_FACIL);
	vsyslog(SYSL_ERROR, fmt, args);
	closelog();
#else
	errdev = (Check) ? "/dev/tty" : CONSOLE;

	time (&clock);
	if (((co = fopen(errdev, "a")) != (FILE *) NULL) ||
	    ((co = fopen(errdev, "w")) != (FILE *) NULL)) {
		(void) fprintf(co, "%s\t%s (%s): ", 
			asctime(localtime(&clock)), MyName, Device);
		(void) vfprintf(co, fmt, args);
		(void) fputc('\n', co);
		(void) fclose(co);
	}
#endif
	va_end(args);
}


#ifdef	DEBUG
/*
**	debug() - an fprintf to the debug file
**
**	Only does the output if the requested level is "set."
*/


void debug(int lvl, char *fmt, ...)
{
	va_list	args;
	char msg[1024] = "";

	va_start(args, fmt);
#if defined(SYSLOG) && defined(SYSL_DEBUG)
	if(Debug & lvl) {
#else
	if((Dfp) && (Debug & lvl)) {
#endif
		switch(lvl) {
			case D_OPT: sprintf(msg, "D_OPT: "); break;
			case D_DEF: sprintf(msg, "D_DEF: "); break;
			case D_UTMP: sprintf(msg, "D_UTMP: "); break;
			case D_INIT: sprintf(msg, "D_INIT: "); break;
			case D_GTAB: sprintf(msg, "D_GTAB: "); break;
			case D_RUN: sprintf(msg, "D_RUN: "); break;
			case D_RB: sprintf(msg, "D_RB: "); break;
			case D_LOCK: sprintf(msg, "D_LOCK: "); break;
			case D_SCH: sprintf(msg, "D_SCH: "); break;
			case D_ALL: break;
		}
		(void) vsprintf(msg + strlen(msg), fmt, args);
#if defined(SYSLOG) && defined(SYSL_DEBUG)
		openlog(MyName, LOG_PID | LOG_CONS, SYSL_FACIL);
		syslog(SYSL_DEBUG, msg);
		closelog();
#else
		(void) strcat(msg, "\n");
		fprintf(Dfp, msg);
		(void) fflush(Dfp);
#endif
	}
	va_end(args);
}


/*
**	dprint() - return a string with control characters escaped
*/

char *
dprint(word)
char *word;
{
	char *p, *fmt, ch;
	static char msg[1024];

	msg[0] = '\0';
	p = word;
	while ((ch = *p++)) {
		if (ch == AUTOBD) {
			strcat(msg, "[speed]");
			continue;
		} else if (ch < ' ') {
			fmt = "^%c";
			ch = ch | 0100;
		} else {
			fmt = "%c";
		}
		sprintf(msg + strlen(msg), fmt, ch);
	}
	return(msg);
}

#endif	/* DEBUG */
