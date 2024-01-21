/*
 * term.c: termcap stuff... 
 *
 * Written By Michael Sandrof
 * HP-UX modifications by Mark T. Dame (Mark.Dame@uc.edu) 
 * Termio modifications by Stellan Klebom (d88-skl@nada.kth.se) 
 * Copyright(c) 1990 
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * i want to clean this file up, but i can't be bothered.  anyone
 * wanna do it for me ?
 */

#include "irc.h"
#include "vars.h"

#ifdef XD88
# include <sys/termios.h>
#endif /* XD88 */

#ifdef IRIX
#undef __STDC__
#endif

#ifdef SVR3
# include <sys/stat.h>
#if 0
# include <sgtty.h>
#endif
# include <sys/stream.h>
# ifdef HAVE_SYS_PTEM_H
#  include <sys/ptem.h>
# endif
# define CBREAK RAW
# define USE_TERMIO /* bah */
#endif /* SVR3 */

#ifdef SVR4
# include <sys/stat.h>
# include <termios.h>
# include <sys/ttold.h>
# include <sys/stream.h>
/* uhhhhh. like... sysVr4 doesnt *have* sys/ttcompat.h
 *		-verified by someone at novell
#  include <sys/ttcompat.h>
 */
# define CBREAK RAW
# define USE_TERMIO
#endif /* SVR4 */

#ifdef M_UNIX
# include <sys/stat.h>
# include <sys/stream.h>
# include <sys/ptem.h>
# include <termio.h>
#endif /* M_UNIX */

#ifdef HPUX
# define _TERMIOS_INCLUDED
# define _INCLUDE_TERMIO
# include <sys/termio.h>
# define USE_TERMIO
#endif /* HPUX */

#ifndef WINNT
#include <sys/ioctl.h>
#endif
#include "ircterm.h"

#ifdef ISC
# include <sys/sioctl.h>
# define USE_TERMIO
#endif /* ISC22 */

#ifdef USE_TERMIO
# include <sys/termio.h>
#endif /* USE_TERMIO */

#ifdef ISC22
# undef TIOCSETC
# undef TIOCGETC
#endif /* ISC22 */

#ifdef USING_CURSES
# include <curses.h>
#endif /* USING_CURSES */

#if defined(POSIX) && !defined(sun) && !defined(_IBMR2)
# ifdef HAVE_SYS_TTOLD_H
#  include <sys/ttold.h>
# endif /* HAVE_SYS_TTOLD_H */
#endif /* POSIX && !sun && !_IBMR2 */

#ifdef __linux__
# define USE_TERMIO
#endif

/*
 * net/2 defined this as 0.5, and 4.4 as 1..
 * i hope other systems based on this don't change it (netbsd,
 * bsdi, freebsd ..)
 */
#ifdef BSD4_4
# include <termios.h>
# define termio termios
# define USE_TERMIO
#endif

/* Missing on ConvexOS, idea picked from SunOS */
#if defined(__convex__) && !defined(LPASS8)
# define LPASS8 (L004000>>16)
#endif

#include "window.h"
#include "screen.h"
#include "output.h"

#ifndef	STTY_ONLY
extern	char	*tgetstr();
extern	int	tgetent();
extern	char	*getenv();
extern	void	new_stty();

static	int	term_CE_clear_to_eol _((void));
static	int	term_CS_scroll _((int, int, int));
static	int	term_ALDL_scroll _((int, int, int));
static	int	term_param_ALDL_scroll _((int, int, int));
static	int	term_IC_insert _((char));
static	int	term_IMEI_insert _((char));
static	int	term_DC_delete _((void));
static	int	term_null_function _((void));
static	int	term_BS_cursor_left _((void));
static	int	term_LE_cursor_left _((void));
static	int	term_ND_cursor_right _((void));
#endif /* STTY_ONLY */

static	int	tty_des;		/* descriptor for the tty */

#ifndef WINNT
#ifndef USE_TERMIO
static	struct	ltchars	oldltchars,
			newltchars = { -1, -1, -1, -1, -1, -1};
static	struct	tchars	oldtchars,
			newtchars = { '\003', -1, -1, -1, -1, -1};
static	struct	sgttyb	oldb,
			newb;
#else
static	struct	termio	oldb,
			newb;
#endif /* USE_TERMIO */
#else
#include <output.h>
#endif

#ifndef STTY_ONLY

static	char	termcap[1024];

/*
 * Function variables: each returns 1 if the function is not supported on the
 * current term type, otherwise they do their thing and return 0 
 */
int	(*term_scroll) _((int, int, int));	/* this is set to the best scroll available */
int	(*term_insert) _((char));	/* this is set to the best insert available */
int	(*term_delete) _((void));	/* this is set to the best delete available */
int	(*term_cursor_left) _((void));	/* this is set to the best left available */
int	(*term_cursor_right) _((void)); /* this is set to the best right available */
int	(*term_clear_to_eol) _((void)); /* this is set... figure it out */

/* The termcap variables */
char	*CM, *CE, *CL, *CR, *NL, *AL, *DL, *CS, *DC, *IC, *IM, *EI, 
	*SO, *SE, *US, *UE, *MD, *ME, *SF, *SR, *ND, *LE, *BL;
int	CO = 79,
	LI = 24,
	SG;

/*
 * term_reset_flag: set to true whenever the terminal is reset, thus letter
 * the calling program work out what to do 
 */
int	term_reset_flag = 0;

static	int	term_echo_flag = 1;

#ifndef WINNT
static	int	li,
		co;
#else
extern	int	li,
		co;
#endif
#endif /* STTY_ONLY */
#ifdef TIOCLSET
static	int	old_local_modes,
		new_local_modes;
#endif

#ifndef STTY_ONLY
void	term_intr (char c)
{
#if defined(TIOCSETC) && !defined(USE_TERMIO)
	newtchars.t_intrc = c;
	ioctl(tty_des, TIOCSETC, &newtchars);
#endif /* TIOCSETC */
}

void	term_susp (char c)
{
#ifndef WINNT
#ifndef USE_TERMIO
	newltchars.t_suspc = c;
	ioctl(tty_des, TIOCSLTC, &newltchars);
#endif /* USE_TERMIO */
#endif
}

/*
 * term_echo: if 0, echo is turned off (all characters appear as blanks), if
 * non-zero, all is normal.  The function returns the old value of the
 * term_echo_flag 
 */
int	term_echo (int flag)
{
	int	echo;

	echo = term_echo_flag;
	term_echo_flag = flag;
	return (echo);
}

/*
 * term_putchar: puts a character to the screen, and displays control
 * characters as inverse video uppercase letters.  NOTE:  Dont use this to
 * display termcap control sequences!  It won't work! 
 *
 * Um... well, it will work if DISPLAY_ANSI_VAR is set to on... (hop)
 */
void	term_putchar (unsigned char c)
{
	if (term_echo_flag)
	{
		/* Sheer, raving paranoia */
		if (get_int_var(EIGHT_BIT_CHARACTERS_VAR) == 0)
			if (c & 128) c &= ~128;

		/* The only control character in ascii sequences
		 * is 27, which is the escape -- all the rest of
		 * them are printable (i think).  So we should
		 * print all the rest of the control seqs as
		 * reverse like normal (idea from Genesis K.)
		 *
		 * Why do i know im going to regret this?
		 */
		if ((c != 27) || !get_int_var(DISPLAY_ANSI_VAR))
		{
			if (c < 32)
			{
				term_standout_on();
				c = (c & 127) | 64;
				fputc(c, (current_screen?current_screen->fpout:stdout));
				term_standout_off();
			}
			else if (c == '\177')
			{
				term_standout_on();
				c = '?';
				fputc(c, (current_screen?current_screen->fpout:stdout));
				term_standout_off();
			}
			else
				fputc((int)c, (current_screen?current_screen->fpout:stdout));
		}
		else
			fputc((int)c, (current_screen?current_screen->fpout:stdout));
	}
	else
	{
		c = ' ';
		fputc((int)c, (current_screen?current_screen->fpout:stdout));
	}
}

/* term_puts: uses term_putchar to print text */
int	term_puts(char *str, int len)
{
	int	i;

	for (i = 0; *str && (i < len); str++, i++)
		term_putchar(*str);
	return (i);
}

/* putchar_x: the putchar function used by tputs */
int putchar_x (int c)
{
#ifndef NetBSD
	return 
#endif
	fputc(c, (current_screen?current_screen->fpout:stdout));
}

void term_flush _((void))
{
	fflush((current_screen?current_screen->fpout:stdout));
}

/*
 * term_reset: sets terminal attributed back to what they were before the
 * program started 
 */
void term_reset _((void))
{
#ifndef WINNT
#ifndef USE_TERMIO
	ioctl(tty_des, TIOCSLTC, &oldltchars);
	ioctl(tty_des, TIOCSETC, &oldtchars);
	ioctl(tty_des, TIOCSETP, &oldb);
#else
# ifdef TCSETA
	ioctl(tty_des, TCSETA, &oldb);
# else
	tcsetattr(tty_des, TCSADRAIN, &oldb);
# endif /* TCSETA */
#endif /* USE_TERMIO */

#if (defined(mips) && !defined(ultrix)) || defined(ISC22) || defined(_HPUX_SOURCE)
	new_stty("cooked");
#endif /* mips */

#ifdef TIOCLSET
	ioctl(tty_des, TIOCLSET, &old_local_modes);
#endif
#endif /* WINNT */
	if   (CS)
		tputs_x(tgoto(CS, LI - 1, 0));
	term_move_cursor(0, LI - 1);
	term_reset_flag = 1;
	term_flush();
}

/*
 * term_cont: sets the terminal back to IRCII stuff when it is restarted
 * after a SIGSTOP.  Somewhere, this must be used in a signal() call 
 */
RETSIGTYPE term_cont _((int unused))
{
#ifdef SYSVSIGNALS
	(void) MY_SIGNAL(SIGCONT, term_cont, 0);	/* sysv has dumb signals */
#endif /* SYSVSIGNALS */

#if defined(SIGSTOP) && defined(SIGTSTP)	/* munix doesn't have sigstop sigtstp */
# ifdef USE_TERMIO
#  ifdef TCSETA
	ioctl(tty_des, TCSETA, &newb);
#  else
	tcsetattr(tty_des, TCSADRAIN, &newb);
#  endif /* TCSETA */
# else
	ioctl(tty_des, TIOCSLTC, &newltchars);
	ioctl(tty_des, TIOCSETC, &newtchars);
	ioctl(tty_des, TIOCSETP, &newb);
# endif /* USE_TERMIO */
# if defined(_HPUX_SOURCE) || defined(ISC22)
	new_stty("opost");
# endif /* HPUX || ISC22 */
# if defined(mips) && !defined(ultrix) /*ultrix/mips silliness*/
	new_stty("raw -echo");
# endif /* mips */
#endif /* SIGSTOP && SIGTSTP */

#ifdef TIOCLSET
	ioctl(tty_des, TIOCLSET, &new_local_modes);
#endif

	refresh_screen('\0', NULL);
}

/*
 * term_pause: sets terminal back to pre-program days, then SIGSTOPs itself. 
 */
extern void term_pause (char unused, char *not_used)
{
#ifndef WINNT
#if !defined(SIGSTOP) || !defined(SIGTSTP) || defined(_RT) || defined(ESIX)
	say("The STOP_IRC function does not work on this system type.");
#else
	term_reset();
	kill(getpid(), SIGSTOP);
#endif /* MUNIX */
#endif
}
#endif /* STTY_ONLY */

/*
 * term_init: does all terminal initialization... reads termcap info, sets
 * the terminal to CBREAK, no ECHO mode.   Chooses the best of the terminal
 * attributes to use ..  for the version of this function that is called for
 * wserv, we set the termial to RAW, no ECHO, so that all the signals are
 * ignored.. fixes quite a few problems...  -phone, jan 1993..
 */
void term_init _((void))
{
#ifndef WINNT
#ifndef	STTY_ONLY
	char	bp[1024],
		*term,
		*ptr;

	if ((term = getenv("TERM")) == (char *) 0)
	{
		fprintf(stderr, "irc: No TERM variable set!\n");
		fprintf(stderr,"irc: You may still run irc by using the -d switch\n");
		exit(1);
	}
	if (tgetent(bp, term) < 1)
	{
		fprintf(stderr, "irc: No termcap entry for %s.\n", term);
		fprintf(stderr,"irc: You may still run irc by using the -d switch\n");
		exit(1);
	}
#endif
	if (getenv("IRC_DEBUG")|| (tty_des = open("/dev/tty", O_RDWR, 0)) == -1)
		tty_des = 0;

#ifndef USE_TERMIO
	ioctl(tty_des, TIOCGLTC, &oldltchars);
	ioctl(tty_des, TIOCGETC, &oldtchars);
	ioctl(tty_des, TIOCGETP, &oldb);

	newb = oldb;
	newb.sg_flags &= ~CRMOD;

# ifdef TIOCLSET
	ioctl(tty_des, TIOCLGET, &old_local_modes);
	new_local_modes = old_local_modes | LDECCTQ | LLITOUT | LNOFLSH;
	ioctl(tty_des, TIOCLSET, &new_local_modes);
# endif

# ifndef STTY_ONLY
	if (use_flow_control)
	{
		newtchars.t_startc = oldtchars.t_startc;
		newtchars.t_stopc = oldtchars.t_stopc;
	}
	newb.sg_flags |= CBREAK;
# else
	newb.sg_flags |= RAW;
# endif /* STTY_ONLY */

# if !defined(_HPUX_SOURCE)
	newb.sg_flags &= (~ECHO);
# endif /* _HPUX_SOURCE */

	ioctl(tty_des, TIOCSLTC, &newltchars);
	ioctl(tty_des, TIOCSETC, &newtchars);
	ioctl(tty_des, TIOCSETP, &newb);

#else /* USE_TERMIO */

# ifdef TCGETA
	ioctl(tty_des, TCGETA, &oldb);
# else
	tcgetattr(tty_des, &oldb);
# endif
	newb = oldb;
	newb.c_lflag &= ~(ICANON | ECHO);	/* set equivalent of
						 * CBREAK and no ECHO */
	newb.c_cc[VMIN] = 1;	/* read() satified after 1 char */
	newb.c_cc[VTIME] = 0;	/* No timer */

#ifndef  _POSIX_VDISABLE
#define _POSIX_VDISABLE 0
#endif
	newb.c_cc[VQUIT] = _POSIX_VDISABLE;
# ifdef VDISCARD
	newb.c_cc[VDISCARD] = _POSIX_VDISABLE;
# endif
# ifdef VDSUSP
	newb.c_cc[VDSUSP] = _POSIX_VDISABLE;
# endif
# ifdef VSUSP
	newb.c_cc[VSUSP] = _POSIX_VDISABLE;
# endif

# ifndef STTY_ONLY
	if (!use_flow_control)
		newb.c_iflag &= ~IXON;	/* No XON/XOFF */
# endif /* STTY_ONLY */

# ifdef TCSETA
	ioctl(tty_des, TCSETA, &newb);
# else
	tcsetattr(tty_des, TCSADRAIN, &newb);
# endif

#endif /* USE_TERMIO */

#ifndef STTY_ONLY
#if defined(mips) && !defined(ultrix)
	new_stty("raw -echo");
#endif /* mips */
#endif /* STTY_ONLY */

#ifndef STTY_ONLY
	if ((co = tgetnum("co")) == -1)
		co = 80;
	if ((li = tgetnum("li")) == -1)
		li = 24;
	ptr = termcap;

	/*
	 * Thanks to Max Bell (mbell@cie.uoregon.edu) for info about TVI
	 * terminals and the sg terminal capability 
	 */
	SG = tgetnum("sg");
	CM = tgetstr("cm", &ptr);
	CL = tgetstr("cl", &ptr);
	if ((CM == (char *) 0) ||
	    (CL == (char *) 0))
	{
		fprintf(stderr, "This terminal does not have the necessary capabilities to run IRCII\nin full screen mode. You may still run irc by using the -d switch\n");
		exit(1);
	}
	if ((CR = tgetstr("cr", &ptr)) == (char *) 0)
		CR = "\r";
	if ((NL = tgetstr("nl", &ptr)) == (char *) 0)
		NL = "\n";

	if ((CE = tgetstr("ce", &ptr)) != NULL)
		term_clear_to_eol = term_CE_clear_to_eol;
#if 0
	else
		term_clear_to_eol = (int (*) _((void)))term_null_function;
#endif

	if ((ND = tgetstr("nd", &ptr)) != NULL)
		term_cursor_right = term_ND_cursor_right;
#if 0
	else
		term_cursor_right = (int (*) _((void)))term_null_function;
#endif

	/* if ((LE = tgetstr("le", &ptr)) || (LE = tgetstr("kl", &ptr))) */
	if ((LE = tgetstr("le", &ptr)) != NULL)
		term_cursor_left = term_LE_cursor_left;
	else if (tgetflag("bs"))
		term_cursor_left = term_BS_cursor_left;
	else
		term_cursor_left = (int (*) _((void)))term_null_function;

	SF = tgetstr("sf", &ptr);
	SR = tgetstr("sr", &ptr);

	if ((CS = tgetstr("cs", &ptr)) != NULL)
		term_scroll = term_CS_scroll;
	else if ((AL = tgetstr("AL", &ptr)) && (DL = tgetstr("DL", &ptr)))
		term_scroll = term_param_ALDL_scroll;
	else if ((AL = tgetstr("al", &ptr)) && (DL = tgetstr("dl", &ptr)))
		term_scroll = term_ALDL_scroll;
	else
		term_scroll = (int (*) _((int, int, int)))term_null_function;

	if ((IC = tgetstr("ic", &ptr)) != NULL)
		term_insert = term_IC_insert;
	else
	{
		if ((IM = tgetstr("im", &ptr)) && (EI = tgetstr("ei", &ptr)))
			term_insert = term_IMEI_insert;
		else
			term_insert = (int (*) _((char)))term_null_function;
	}

	if ((DC = tgetstr("dc", &ptr)) != NULL)
		term_delete = term_DC_delete;
#if 0
	else
		term_delete = (int (*) _((void)))term_null_function;
#endif
	SO = tgetstr("so", &ptr);
	SE = tgetstr("se", &ptr);
	if ((SO == (char *) 0) || (SE == (char *) 0))
	{
		SO = empty_string;
		SE = empty_string;
	}
	US = tgetstr("us", &ptr);
	UE = tgetstr("ue", &ptr);
	if ((US == (char *) 0) || (UE == (char *) 0))
	{
		US = empty_string;
		UE = empty_string;
	}
	MD = tgetstr("md", &ptr);
	ME = tgetstr("me", &ptr);
	if ((MD == (char *) 0) || (ME == (char *) 0))
	{
		MD = empty_string;
		ME = empty_string;
	}
	if ((BL = tgetstr("bl", &ptr)) == (char *) 0)
		BL = "\007";
#endif /* STTY_ONLY */
#else
	{
//		nt_term_init();
		term_clear_to_eol = nt_ClearEOL;
		term_cursor_right = nt_cursor_right;
		term_cursor_left = nt_cursor_left;
		term_scroll = nt_scroll;
		term_insert = term_null_function;
		term_delete = term_null_function;
	}
#endif
}

#ifndef STTY_ONLY
/*
 * term_resize: gets the terminal height and width.  Trys to get the info
 * from the tty driver about size, if it can't... uses the termcap values. If
 * the terminal size has changed since last time term_resize() has been
 * called, 1 is returned.  If it is unchanged, 0 is returned. 
 */
int term_resize _((void))
{
	static	int	old_li = -1,
			old_co = -1;

#ifndef TIOCGWINSZ

	LI = li;
	CO = co;

#else
	struct	winsize window;

	if (ioctl(tty_des, TIOCGWINSZ, &window) < 0)
	{
		LI = li;
		CO = co;
	}
	else
	{
		if ((LI = window.ws_row) == 0)
			LI = li;
		if ((CO = window.ws_col) == 0)
			CO = co;
	}
#endif /* TIOCGWINSZ */
	CO--;
	if ((old_li != LI) || (old_co != CO))
	{
		old_li = LI;
		old_co = CO;
		return (1);
	}
	return (0);
}

/*
 * term_null_function: used when a terminal is missing a particulary useful
 * feature, such as scrolling, to warn the calling program that no such
 * function exists 
 */
static int term_null_function _((void))
{
	return (1);
}

/* term_CE_clear_to_eol(): the clear to eol function, right? */
static	int term_CE_clear_to_eol _((void))
{
	tputs_x(CE);
	return (0);
}

#if 0
/* * term_space_erase: this can be used if term_CE_clear_to_eol() returns 1.
 * This will erase from x to the end of the screen uses space.  Actually, it
 * doesn't reposition the cursor at all, so the cursor must be in the correct
 * spot at the beginning and you must move it back afterwards 
 */
void	term_space_erase (int x)
{
	char	c = ' ';
	int	i,
		cnt;

	cnt = CO - x;
	for (i = 0; i < cnt; i++)
		fputc(c, (current_screen?current_screen->fpout:stdout));
}
#endif
/*
 * term_CS_scroll: should be used if the terminal has the CS capability by
 * setting term_scroll equal to it 
 */
static	int	term_CS_scroll (int line1, int line2, int n)
{
	int	i;
	char	*thing;

	if (n > 0)
		thing = SF ? SF : NL;
	else if (n < 0)
	{
		if (SR)
			thing = SR;
		else
			return 1;
	}
	else
		return 0;
	tputs_x(tgoto(CS, line2, line1));  /* shouldn't do this each time */
	if (n < 0)
	{
		term_move_cursor(0, line1);
		n = -n;
	}
	else
		term_move_cursor(0, line2);
	for (i = 0; i < n; i++)
		tputs_x(thing);
	tputs_x(tgoto(CS, LI - 1, 0));	/* shouldn't do this each time */
	return (0);
}

/*
 * term_ALDL_scroll: should be used for scrolling if the term has AL and DL
 * by setting the term_scroll function to it 
 */
static	int	term_ALDL_scroll (int line1, int line2, int n)
{
	int	i;

	if (n > 0)
	{
		term_move_cursor(0, line1);
		for (i = 0; i < n; i++)
			tputs_x(DL);
		term_move_cursor(0, line2 - n + 1);
		for (i = 0; i < n; i++)
			tputs_x(AL);
	}
	else if (n < 0)
	{
		n = -n;
		term_move_cursor(0, line2-n+1);
		for (i=0; i < n; i++)
			tputs_x(DL);
		term_move_cursor(0, line1);
		for (i=0; i < n; i++)
			tputs_x(AL);
	}
	return (0);
}

/*
 * term_param_ALDL_scroll: Uses the parameterized version of AL and DL 
 */
static	int	term_param_ALDL_scroll (int line1, int line2, int n)
{
	if (n > 0)
	{
		term_move_cursor(0, line1);
		tputs_x(tgoto(DL, n, n));
		term_move_cursor(0, line2 - n + 1);
		tputs_x(tgoto(AL, n, n));
	}
	else if (n < 0)
	{
		n = -n;
		term_move_cursor(0, line2-n+1);
		tputs_x(tgoto(DL, n, n));
		term_move_cursor(0, line1);
		tputs_x(tgoto(AL, n, n));
	}
	return (0);
}

/*
 * term_IC_insert: should be used for character inserts if the term has IC by
 * setting term_insert to it. 
 */
static	int	term_IC_insert (char c)
{
	tputs_x(IC);
	term_putchar(c);
	return (0);
}

/*
 * term_IMEI_insert: should be used for character inserts if the term has IM
 * and EI by setting term_insert to it 
 */
static	int	term_IMEI_insert (char c)
{
	tputs_x(IM);
	term_putchar(c);
	tputs_x(EI);
	return (0);
}

/*
 * term_DC_delete: should be used for character deletes if the term has DC by
 * setting term_delete to it 
 */
static	int term_DC_delete _((void))
{
	tputs_x(DC);
	return (0);
}

/* term_ND_cursor_right: got it yet? */
static	int term_ND_cursor_right _((void))
{
	tputs_x(ND);
	return (0);
}

/* term_LE_cursor_left:  shouldn't you move on to something else? */
static	int term_LE_cursor_left _((void))
{
	tputs_x(LE);
	return (0);
}

static	int term_BS_cursor_left _((void))
{
	char	c = '\010';

	fputc(c, (current_screen ? current_screen->fpout : stdout));
	return (0);
}

extern	void term_beep (void)
{
	if (get_int_var(BEEP_VAR))
	{
		tputs_x(BL);
		fflush(current_screen ? current_screen->fpout : stdout);
	}
}


extern	void	copy_window_size (int *nlines, int *cols)
{
	*nlines = LI;
	*cols = CO;
}

int term_eight_bit _((void))
{
#ifndef WINNT
#if defined(USE_TERMIO)
	return (((oldb.c_cflag) & CSIZE) == CS8) ? 1 : 0;
#else
	return (old_local_modes & LPASS8) ? 1 : 0;
#endif /* USE_TERMIO */
#endif
}
#endif /* STTY_ONLY */

extern	void	set_term_eight_bit (int value)
{
#ifndef WINNT
#if defined(USE_TERMIO)
	/*
	 * XXX - maybe this should be ISTRIP as well??
	 */
	if (value == ON)
	{
		newb.c_cflag |= CS8;
		/* bug found by CrowMan when he reported he couldnt
		 * turn 8 bit mode back on under linux. Doh.
		 */
		newb.c_iflag &= ~ISTRIP;
	}
	else
	{
		newb.c_cflag &= ~CS8;
		newb.c_iflag |= ISTRIP;
	}
# ifdef TCSETA
	ioctl(tty_des, TCSETA, &newb);
# else
	tcsetattr(tty_des, TCSADRAIN, &newb);
# endif /* TCSETA */
#else
	if (value == ON)
		new_local_modes |= LPASS8;
	else
		new_local_modes &= ~LPASS8;
	ioctl(tty_des, TIOCLSET, &new_local_modes);
#endif /* USE_TERMIO */
#endif
}
