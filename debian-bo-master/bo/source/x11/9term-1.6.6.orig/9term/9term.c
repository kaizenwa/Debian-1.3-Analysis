/*
 *  Copyright © 1993 James Farrow, University of Sydney - all rights reserved
 *
 *  9term initialisation and pty communication
 */

#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>

#ifdef SOLARIS
#include <sys/termios.h>
#else
#include <sys/termio.h>
#endif
#include <signal.h>
#include <sys/stat.h>

#include "9term.h"

int		flushing;
int		suspended;
Event		e;
ulong		Erc;
int		highwater = 50000;
int		lowwater = 40000;
int		waterquantum;
int		beepmask;
int		ninewm;
int		markset;

static char	*items[] = { "cut", "paste", "snarf", "mark", "send", "scroll", 0 };
static Menu	edit = {items};
enum {	mCUT,
	mPASTE,
	mSNARF,
	mMARK,
	mSEND,
	mSCROLL
};

static	char	*modenames[] = {"fwd","bkwd","suspend","view","review",0,0,0};
static	Menu	kmode = {modenames} ;
enum {	mFWD,
	mBKWD,
	mSUSP,
	mVIEW,
	mREVIEW,
#ifndef REMOTE
	mECHO,
#endif
	mMODE
};

int	kbdmode = PLAN9;

static char		*prog;
static char		*shell[] = { "/bin/sh", 0 };

static	void		button1(Mouse*);
static	void		button2(Mouse*);
static	void		button3(Mouse*);
static	void		run(void);
static	void		shelloutput(ulong, Event*);
static	void		domouse(Event*);
static	void		flush(Text*);
static	void		exportsnarf(Text*);
static	void		importsnarf(Text*);
static	void		inputrune(Text*, Rune*, int);
static	void		eraseline(Text*);
static	void		suspend(Text*);
static	void		resume(Text*);
static	void		interrupt(Text*, Rune);
static	void		backspace(Text*);
static	void		worderase(Text*);
static	void		keyboard(Event*);
static	void		search(Text*, int);
static	int		lookfwd(Text*, long, long);
static	int		lookbkwd(Text*, long, long);

static char *options[] = {
	"-unix			start in Unix mode",
	"-name <name>		name of X resource file",
	"-label <label>		intial label of 9term window",
	"-s			enable scroll mode",
	"-ls			run a login shell",
	"-ut			insert an entry in utmp",
	"-e <command> <args>	execute command with arguments - must be last",
	0
};

void dumpsnarf(Text *t)
{
	int i;

	fprintf(stderr, "length: %d\n", t->length-t->pout);
	for (i = t->pout; i < t->length; i++) {
		fprintf(stderr,"%c", t->text[i]);
	}
}
	
/*
 *  Print usage message and exit.
 */
static void
usage(char *s)
{
	char **cp;

	fprintf(stderr, "Usage: %s: %s\n", prog, s);
	for (cp = options; *cp; cp++)
		fprintf(stderr,"\t%s\n", *cp);
	exit(1);
}

/*
 *	Parse args, initialize display, spin off command interpreter
 *	and start handling keyboard, mouse, and command output events.
 */
void
main(int argc,char **argv)
{
	int i, j;
	char *cp, *resource;
	char **cmd;

	prog = argv[0];
	resource = prog;
	cp = strrchr(prog, '/');
	if (cp)
		resource = cp+1;

	cp = (char *)getenv("SHELL");
	if (cp && *cp)
		shell[0] = cp;
	cmd = shell;
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i],"-V") == 0) {
			fprintf(stderr, "9term version %s\n",VERSION);
			exit(0);
		}
		else if (strcmp(argv[i],"-?") == 0) {
			usage("missing X resource following -name");
			exit(0);
		}
		else if (strcmp(argv[i],"-name") == 0) {
			if (!argv[++i])
				usage("missing X resource following -name");
			resource = argv[i];
		}
		else if (strcmp(argv[i], "-e") == 0) {
			argv[i] = 0;
			argc = i++;
			if (!argv[i])
				usage("missing command following -e");
			cmd = &argv[i];
		}
	}

	setenv("TERM", "9term", 1);
	/* Cope with BSD-oid systems. - cks */
	setenv("TERMCAP", "9term: :am:bl=^G:do=^J:nl=^J:", 1);
	signal(SIGINT, SIG_IGN);

	init_display(&argc, argv, cmd, resource);
	cursorswitch(0);
	init_command(cmd[0], cmd);
	setborder();

	waterquantum = highwater - lowwater;
	run();
}

/*
 *	Register command output stream and handle events
 */
#define MAXMSG		128
#define MAXFDATA	8192

static
void run(void)
{
	ulong type;

	Erc = estart(0, comm_fd, MAXMSG + MAXFDATA);
	for (;;) {
		/* disable shell output when more than one buffer ahead */
/*
		if (!text->scrolling
				&& text->length > HIGHWATER
				&& text->base < text->length/2)
*/
		if (!flushing && !text->scrolling
				&& text->length > text->end+MAXFDATA)
			type = eread(~Erc, &e);
		else
			type = eread(~0, &e);
		/* events are either: mouse, keyboard, or command output */
		if (type == Ekeyboard)
			keyboard(&e);
		else if (type == Emouse)
			domouse(&e);
		else if (type == Erc) {
#ifdef PCKT
			if (e.data[0]) {
				ctlmsg(&e);
				continue;
			}
#endif
			shelloutput(Erc, &e);
		}
	}
}

static Rune *
scrinsert(Rune *buf, Rune *ebuf)
{
	ulong end;

		/* delete extra characters to make room if necessary */
	while (text->length + (ebuf-buf) > highwater) {
		end = text->length - (text->alloced - waterquantum);
		if (!text->scrolling) {
			if (end > text->base)
				end = text->base;
			if (end > text->pout)
				end = text->pout;
		}
		end &= ~3;
		if (end <= 0)
			break;
		textdelete(text, 0, end);
	}
		/* insert the chars and show them, if necessary */
	textinsert(text, buf, ebuf, text->pout);
	textshow(text, text->pout, text->scrolling);
	return buf;
}

/*
 *	Handle command output event
 */
static void
shelloutput(ulong Erc, Event *e)
{
	int n, w, tot;
	uchar *p;
	Rune *q;
	Rune buf[EMAXMSG];

	static char partial[UTFmax];
	static int plen;

#define	FLUSHBUF()	if (q >= &buf[EMAXMSG]) q = scrinsert(buf, q)

	q = buf;
	tot = 0;
	for (;;) {
		if (!(n = e->n))
			return;
		tot += n;
		p = e->data;
		if (plen) {		/* partial rune accumulated */
			memcpy(partial+plen, p, UTFmax-plen);
			w = plen+n;
			if (w > UTFmax)
				w = UTFmax;
			if (!fullrune(partial, w)) {
				plen += n;
				return;
			}
			FLUSHBUF();
			w = chartorune(q++, partial)-plen;
			p += w;
			n -= w;
			plen = 0;
		}

		for (; n > 0; p += w, n -= w) {
			w = 1;
			switch(*p) {
			case '\0':		/* nulls break text package */
				break;
			case '\r':		/* Replace NL/CR with NL */
				if (kbdmode == PLAN9)
					if (q > buf && *(q-1) == '\n')
						break;
				FLUSHBUF();
				*q++ = *p;
				break;
			case '\n':		/* replace CR/NL with NL */
				if (kbdmode == PLAN9)
					if  (q > buf && *(q-1) == '\r')
						q--;
				FLUSHBUF();
				*q++ = *p;
				break;
			case 0x07:
				if (kbdmode & beepmask)
					beep();
				else {
					if (q >= &buf[EMAXMSG])	/* buffer full? */
						q = scrinsert(buf, q);
					*q++ = *p;
				}
				break;
			case '\b':		/* backspace at output point */
				if (q > buf)
					q--;
				else 
					textdelete(text, text->pout-1, text->pout);
				break;
			default:
				FLUSHBUF();
				if (*p < Runeself)
					*q++ = *p;
				else if (fullrune((char*)p, n))
					w = chartorune(q++, (char*)p);
				else {
					if (n <= UTFmax) {	/* skip garbage runes */
						plen = n;
						memcpy(partial, p, plen);
						w = n;		/* double break */
					} else
						w = UTFmax;
				}
				break;
			}
		}
			/* disable when more than one buffer ahead of display */
		if (!text->scrolling && text->length+q-buf > text->end+MAXFDATA)
			break;
			/* 4096 is a random cutoff to allow other events to occur */
		if (tot > 4096 || !ecanread(Erc))
			break;
		if (eread(Erc, e) != Erc)
			break;
#ifdef PCKT
		if (e->data[0]) {
			ctlmsg(e);
			break;
		}
#endif
	}
	scrinsert(buf, q);
}

/*
 *	handle mouse event
 */
static void
domouse(Event *e)
{
	Mouse *m;

	m = &e->mouse;
		/* click in scroll bar */
	if (ptinrect(m->xy, text->scroll->r)) {
		if (scrollhit(text->scroll, e))
			textscroll(text, text->scroll->buttons);
		return;
	}
		/* clicks outside scroll bar */
	if (m->buttons & BUTTON1)
		button1(m);
	else if (m->buttons & BUTTON2)
		button2(m);
	else if (m->buttons & BUTTON3)
		button3(m);
}
/*
 *	handle text selection
 */
static void
button1(Mouse *m)
{
	ulong p0, p1;

	static ulong clicktime = 0;

	p0 = text->p0;
	p1 = text->p1;
	textselect(text, m);

	if (p0 == p1 &&	m->msec - clicktime < 500
		&& text->base+frcharofpt(&text->f, m->xy) == p0) {
		doubleclick(text, text->p0);
		texthighlight(text, text->p0, text->p1, F&~D);
		clicktime = 0;
	}
	else
		clicktime = m->msec;
}
/*
 *	handle cut/paste/snarf/mark/send/scroll menu.
 */
static void
button2(Mouse *m)
{
	Rune *r;
	ulong p0, p1;

	static Rune nl[] = { '\n', 0 };

	specialchars(slave_fd);
	edit.item[mMARK] = markset?"extend":"mark";
	edit.item[mSCROLL] = text->scrolling?"noscroll":"scroll";
	switch (menuhit(2, m, &edit))
	{
	case mCUT:
		textsnarf(text, text->p0, text->p1);
		exportsnarf(text);
		textdelete(text, text->p0, text->p1);
		break;
	case mSNARF:
		textsnarf(text, text->p0, text->p1);
		exportsnarf(text);
		break;
	case mPASTE:
		importsnarf(text);
		if (text->snarfed) {
			textdelete(text, text->p0, text->p1);
			inputrune(text, text->snarfed, text->snarflen);
		}
		break;
	case mMARK:
		if (!markset) {
			text->pmark = text->p0;
			markset = 1;
		} else {
			p0 = text->p0;
			p1 = text->p1;
			if (text->pmark < text->p0)
				p0 = text->pmark;
			else
				p1 = text->pmark;
			texthighlight(text, p0, p1, F&~D);
			markset = 0;
		}
		break;
	case mSEND:
		if (text->p0 != text->p1) {
			textsnarf(text, text->p0, text->p1);
			exportsnarf(text);
		} else
			importsnarf(text);
		if (text->snarfed) {
			texthighlight(text, text->length, text->length, F&~D);
			inputrune(text, text->snarfed, text->snarflen);
			if (text->snarfed[text->snarflen-1] != '\n')
				inputrune(text, nl, 1);
		}
		break;
	case mSCROLL:			/* toggle scroll state */
		text->scrolling = !text->scrolling;
		textshow(text, text->p0, text->scrolling);
		break;
	}
}
/*
 *	handle suspend/view/mode menu
 */
static void
button3(Mouse *m)
{
	gttymodes(slave_fd);
#ifndef REMOTE
	kmode.item[mECHO] = echo ?"noecho":"echo";
#endif
	kmode.item[mMODE] = (kbdmode == PLAN9) ?"unix":"plan 9";
	kmode.item[mSUSP] = suspended ? "resume" : "suspend";
	switch (menuhit(3, m, &kmode))
	{
	case mFWD:
		search(text, 1);
		break;
	case mBKWD:
		search(text, 0);
		break;
	case mSUSP:
		if (suspended)
			resume(text);
		else
			suspend(text);
		break;
	case mVIEW:
		textjump(text, text->f.maxlines/2);
		break;
	case mREVIEW:
		textset(text, _backnl(text, text->base, text->f.maxlines/2));
		break;
#ifndef REMOTE
	case mECHO:
		if (echo)
			setttyecho(0);
		else
			setttyecho(1);
		break;
#endif
	case mMODE:
		if (kbdmode == UNIX)
			saveunixspecials(slave_fd);
		kbdmode = (kbdmode == PLAN9) ? UNIX : PLAN9;
		sttymodes(slave_fd);
		break;
	}
}
/*
 *	look for a pattern
 */
static void
search(Text *t, int fwd)
{
	if (t->p0 != t->p1) {
		textsnarf(t, t->p0, t->p1);
		exportsnarf(t);
	} else
		importsnarf(t);
	if (!t->snarfed)
		return;
	if (fwd) {
		if (!lookfwd(t, t->p1, t->length+1) && !lookfwd(t, 0, t->p0))
			return;
	} else {
		if (!lookbkwd(text, t->p0, 0) && !lookbkwd(t, t->length+1, t->p1))
			return;
	}
	if (t->p0 < t->base || t->p1 > t->end)
		textset(t, _backnl(t, t->p0, 3));
	texthighlight(t, t->p0, t->p1, F & ~D);
}
/*
 *	search forward for a pattern
 */
static int
lookfwd(Text *t, long p0, long p1)
{
	Rune r;
	int n;

	n = (t->snarflen-1)*sizeof(Rune);
	r = t->snarfed[0];
	p1 -= t->snarflen;
	while (p0 < p1) {
		if (t->text[p0] == r && !memcmp(t->text+p0+1, t->snarfed+1, n)) {
			t->p0 = p0;
			t->p1 = p0+t->snarflen;
			return 1;
		}
		p0++;
	}
	return 0;
}
/*
 *	search backward for a pattern
 */
static int
lookbkwd(Text *t, long p0, long p1)
{
	Rune r;
	int n;

	n = (t->snarflen-1)*sizeof(Rune);
	r = t->snarfed[0];
	p0 -= t->snarflen;
	while (p0 >= p1) {
		if (t->text[p0] == r && !memcmp(t->text+p0+1, t->snarfed+1, n)) {
			t->p0 = p0;
			t->p1 = p0+t->snarflen;
			return 1;
		}
		p0--;
	}
	return 0;
}
/*
 *	send the text from the output point to the last newline
 *	to the command interpreter.
 */
static void
flush(Text *t)
{
	ulong p;
	int c;

	/* don't send when suspended or before the output point */
	p = t->pout;
	if (suspended || p >= t->p0)
		return;

	while (p < t->p0) {
		c = t->text[p++];
		if (c == '\n' || c == eolchar || c == eofchar) {
			sendrunes(t->text+t->pout, p-t->pout);
			t->pout = p;
		}
	}
}
/*
 *	handle a keystroke
 */
static void
keyboard(Event *e)
{
	Rune r;
	Text *t = text;
	static Rune lastchar;

	if (!(r = e->kbdc))
		return;
		/* in plan 9 mode, ESC and the arrow keys are special */
	if (kbdmode == PLAN9) {
		if (r == 0x1b) {
			if (suspended)
				resume(text);
			else
				suspend(text);
			lastchar = r;
			return;
		} else if (r == 0x80) {
			textjump(text, text->f.maxlines/2);
			lastchar = r;
			return;
		} else if (r == 0x81) {
			textset(text, _backnl(text, text->base, text->f.maxlines/2));
			lastchar = r;
			return;
		}
	} else if (r == 0x80 || r == 0x81) {
		/* ignore cursor keys in unix mode */
		lastchar = r;
		return;
	}
	specialchars(slave_fd);
		/* interrupt processing */
	if (isig && canonical && (r == intrchar || r == quitchar)) {
		interrupt(t, r);
		lastchar = r;
		return;
	}
		/* snarf and delete any selected text */
	if (t->p1 != t->p0) {
		textsnarf(t, t->p0, t->p1);
		textdelete(t, t->p0, t->p1);
	}
	/*
	 * in raw mode send each input rune as it occurs when
	 * the focus is at or beyond the output point.
	 */
	if ((!canonical || !echo) && t->p0 >= t->pout) {
		if (t->p0 > t->pout) {
			sendrunes(t->text+t->pout, t->p0-t->pout);
			t->pout = t->p0;
		}
		sendrunes(&r, 1);
		lastchar = r;
		textshow(t, t->p0, 1);
		return;
	}
		/* erase/kill processing (if not escaped) */
	if (lastchar != '\\') {
		lastchar = r;
		if (r == killchar) {			/* kill line */
			eraseline(text);
			return;
		} else if (r == erasechar) {		/* kill char */
			backspace(text);
			return;
		} else if (r == wordchar) {		/* kill word */
			worderase(text);
			return;
		}
	}
	lastchar = r;
	/* insert the char, preserving the output point */
	textinsert(t, &r, (&r)+1, t->p0);
	if (t->pout == t->p0) {
		if (--t->pout < t->base)
			t->pout = t->base;
	}
	textshow(t, t->p0, 1);
	/* send accumulated text when term char found */
	if (r == eofchar || r == eolchar || r == '\n')
		flush(t);
}
/*
 *	input a rune stream - no escape processing is needed
 */
static
void inputrune(Text *t, Rune *r, int n)
{
	if ((!canonical || !echo) && t->p0 >= t->pout) {
		if (t->p0 > t->pout) {
			sendrunes(t->text+t->pout, t->p0-t->pout);
			t->pout = t->p0;
		}
		sendrunes(r, n);
	} else {
		/* insert the char, preserving the output point */
		textinsert(t, r, r+n, t->p0);
		if (t->pout == t->p0) {
			t->pout -= n;
			if (t->pout < t->base)
				t->pout = t->base;
		}
		flush(t);
		textshow(t, t->p0, 1);
	}
}
/*
 *	enable suspend mode - only when canonical with echo
 */
static void
suspend(Text *t)
{
	specialchars(slave_fd);
	if (!canonical || !echo)	/* if raw */
		return;
	suspended = 1;
	setborder();
}
/*
 *	disable suspend mode
 */
static void
resume(Text *t)
{
	/* leave suspend mode flushing accumulated text */
	suspended = 0;
	setborder();
	flush(t);
}
/*
 *	send interrupt char to the command interpreter
 */
static void
interrupt(Text *t, Rune r)
{
	texthighlight(t, t->length, t->length, F&~D);
#ifdef REMOTE
	flushstream();
	_killpg(r == quitchar ? SIGQUIT : SIGINT);
#else
	sendrunes(&r, 1);
#endif
	eflush(Erc);
	if (canonical && t->pout != t->length)
		t->pout = t->length;
	textshow(t, t->p0, 1);		/* force visibility */
#if FLOW_CONTROL
	flushing = 1;
#endif
}
/*
 *	erase character before cursor
 */
static void
backspace(Text *t)
{
		/* snarf and delete any selected text */
	if (t->p1 != t->p0) {
		textsnarf(t, t->p0, t->p1);
		textdelete(t, t->p0, t->p1);
	}
		/* don't back up past the output point */
	if (t->p0 == t->pout)
		return;
		/* delete char before current cursor position */
	if (t->p0 > 0) {
		/* scroll back 3 lines when at top of screen */
		if (t->p0 == t->base)
			textset(t, _backnl(t, t->base-1, 3));
		textdelete(t, t->p0-1, t->p0);
	}
	textshow(t, t->p0, 1);		/* force visibility */
}
/*
 *	erase to beginning of line containing cursor
 */
static void
eraseline(Text *t)
{
	int n;

	n = _backnl(t, t->p0, 1);
	if (t->p0 >= t->pout && t->pout > n)		/* delete only to output point */
		n = t->pout;
	textdelete(t, n, t->p0);
	textshow(t, t->p0, 1);		/* force visibility */
}
/*
 *	erase word before cursor
 */
static void
worderase(Text *t)
{
	int p0;
	int c;

		/* never back up past output point */
	p0 = t->p0;
	if (p0 <= 0 || p0 == t->pout)
		return;
		/* skip over special chars then skip until special char */
	c = t->text[--p0];
	if (c  != '\n') {
		while (p0 > 0 && p0 != t->pout) {
			if (alnum(c))
				break;
			c = t->text[--p0];
			if (c == '\n') {
				p0++;
				break;
			}
		}
		if (c != '\n')
			while (p0 > 0 && p0 != t->pout && alnum(t->text[p0-1]))
				p0--;
	}
		/* delete the span and force focus */
	textdelete(t, p0, t->p0);	
	textshow(t, t->p0, 1);
}
/*
 *	export a name-value pair to environment
 */
int
setenv(char *name, char *value, int overwrite)
{
	char *p;

	if(getenv(name) && overwrite == 0)
		return;
	p = (char *)malloc(strlen(name)+strlen(value)+2);
	sprintf(p, "%s=%s",  name, value);
	putenv(p);
}
/*
 *	export the current selection to the X selection buffer
 */
static void
exportsnarf(Text *t)
{
	int	n;
	Rune	*r;
	char	*p, *snarfbuf;

	if (t->p0 != t->p1) {	/* only when something selected */
		/* calculate size of selection in characters */
		n = 0;
		for (r = t->text+t->p0; r < t->text+t->p1; r++)
			n += runelen(*r);
		snarfbuf = (char *)malloc(n+1);
		if (!snarfbuf)
			return;
		p = snarfbuf;
		/* copy selection to temporary buffer */
		for (r = t->text+t->p0; r < t->text+t->p1; r++)
			p += runetochar(p, r);
		*p = '\0';
		/* export our selection and import X selection */
		snarfswap(snarfbuf, n, &p);
		if (p)		/* pitch X selection */
			XtFree(p);
		free(snarfbuf);
	}
}
/*
 *	import current contents of X selection buffer
 */
static void
importsnarf(Text *t)
{
	int	n;
	Rune	*r;
	char	*p, *snarfbuf;

		/* get current X selection */
	n = snarfswap("", 0, &snarfbuf);
	if (!n)
		return;
	if (t->snarfed)
		free(t->snarfed);
		/* allocate a buffer and copy it in */
	t->snarfed = (Rune *)calloc(n+1, sizeof(Rune));
	r = t->snarfed;
	for (p = snarfbuf; *p; r++)
		p += chartorune(r, p);
	*r = '\0';
	t->snarflen = r - t->snarfed;
	XtFree(snarfbuf);
}
/*
 *	format and print an error message
 */
#ifdef NEEDVARARG
#include <varargs.h>
void
error(va_alist)
va_dcl
{
	va_list args;
	char *fmt;

	fprintf(stderr, "%s: ", prog);
	va_start(args);
	fmt = va_arg(args, char *);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	exit(1);
}
#else
#include <stdarg.h>
void
error(char *fmt, ...)
{
	va_list args;

	fprintf(stderr, "%s: ", prog);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	exit(1);
}
#endif
