/*
 *  Copyright © 1993 James Farrow, University of Sydney - all rights reserved
 */

#define VERSION		"1.6.6 Nov 1995"

enum	{PLAN9=1, UNIX=2};		/* Mode names */

extern ulong		Erc;
extern Event		e;
extern int		suspended;
extern Text		*text;
extern int		comm_fd;
extern int		slave_fd;
extern int		kbdmode;
extern int		utmpentry;
extern int		highwater;
extern int		lowwater;
extern int		beepmask;
extern int		ninewm;

extern Rune		intrchar;
extern Rune		quitchar;
extern Rune		erasechar;
extern Rune		killchar;
extern Rune		eofchar;
extern Rune		eolchar;
extern Rune		wordchar;
extern int		canonical;
extern int		echo;
extern int		isig;

extern void		specialchars(int);
extern int		setenv(char *, char *, int);
extern void		init_display(int *, char **, char**, char*);
#ifdef NEEDVARARG
extern void		error();
#else
extern void		error(char*, ...);
#endif
extern void		ereshaped(Rectangle);
extern void		frgetmouse(void);
extern void		scr_get_size(int *, int *);
extern void		sendrunes(Rune *, ulong);
extern void		setttyecho(int);
extern void		beep(void);
extern void		catch_child(void);
extern void 		tty_set_size(int,int,int,int,int);
extern void 		init_command(char *,char **);
extern void		updateutmp(char*, int);
extern void		clearutmp(char*, int);
extern void		p9ttymodes(void);
extern char		p9cc(char *);
extern void		sttymodes(int);
extern void		gttymodes(int);
extern void		quit(int);
extern void		flushstream(void);
extern void		setborder(void);
extern void		ctlmsg(Event *);

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifdef	SUNOS
#define	POSIXPTYS
#define BSDPTYS
#endif

#ifdef	__ultrix
	/* Maybe only Ultrix 4.3. Joy. */
#define	POSIXPTYS
#define BSDPTYS
#endif

char	*_getpty(int*, int, int, int);

#if	defined(linux)
#define	POSIXPTYS
#define	BSDPTYS
#endif

#if	defined(RISCOS)
#define O_NONBLOCK	O_NDELAY
#endif

#if	defined(SOLARIS)
#define	POSIXPTYS
#define	REMOTE
#endif
