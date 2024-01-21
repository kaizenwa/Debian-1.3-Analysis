/*
 * term.h: header file for term.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: term.h,v 1.8 1994/07/02 02:38:10 mrg Exp $
 */

#ifndef _TERM_H_
# define _TERM_H_

#ifdef MUNIX
# include <sys/ttold.h>
#endif

#include "irc_std.h"

extern	int	term_reset_flag;

#ifndef WINNT
extern	char	*CM, *DO, *CE, *CL, *CR, *NL, *SO, *SE, *US, *UE, *MD, *ME, *BL;
extern	int	CO, LI, SG; 

/* 
 * fix suggested by <insert person's name here> that we shouldnt prototype
 * the term stuff on IRIX cause its broke. whee.
 */
#ifdef IRIX
# undef _
#endif

/* If this applies to your system too, then just add it to the list. */
#if defined(NetBSD)
#define PUTCHAR_X_RETURN void
#else
#define PUTCHAR_X_RETURN int
#endif

extern PUTCHAR_X_RETURN putchar_x _((int));

/* 
 * On some systems (ie, netbsd) this stuff is defined in <curses.h>
 * which for some reason we dont include... I plan on investigating
 * how linux, sunos, and other major UNIXes handle these, and will
 * change this header file accordingly.  In the meantime, i will hope
 * that including these decs doesnt break anything. :P
 *
 * This will make gcc shut up on netbsd.  Other system will still
 * get the warnings unless you port this to your system.  (if you
 * do, let me know =)
 */
#if defined(NetBSD) || defined(_AIX)
extern void  tputs _((char *, int, PUTCHAR_X_RETURN (*) _((int)) ));
extern char *tgoto _((char *, int, int));
extern int   tgetnum _((char *));
extern int   tgetflag _((char *));
#endif

#define tputs_x(s)		(tputs(s, 0, putchar_x))
#define term_underline_on()	(tputs_x(US))
#define term_underline_off()	(tputs_x(UE))
#define term_standout_on()	(tputs_x(SO))
#define term_standout_off()	(tputs_x(SE))
#define term_clear_screen()	(tputs_x(CL))
#define term_move_cursor(c, r)	(tputs_x(tgoto(CM, (c), (r))))
#define term_cr()		(tputs_x(CR))
#define term_newline()		(tputs_x(NL))
/* Idea for not having the client beep you when /set BEEP off
 * from Genesis K.
 */
#define term_beep()		if (get_int_var(BEEP_VAR))	\
				{				\
				      tputs_x(BL);		\
				      fflush(current_screen ? 	\
					current_screen->fpout : \
					stdout);		\
				}
#define	term_bold_on()		(tputs_x(MD))
#define	term_bold_off()		(tputs_x(ME))

#endif /*WINNT */

extern	RETSIGTYPE	term_cont _((int));
extern	int	term_echo _((int));
extern	void	term_init _((void));
#ifndef STTY_ONLY
extern	int	term_resize _((void));
#endif
extern	void	term_pause _((char, char *));
extern	void	term_putchar _((unsigned char));
extern	int	term_puts _((char *, int));
extern	void	term_flush _((void));
extern	int	(*term_scroll) _((int, int, int));
extern	int	(*term_insert) _((char));
extern	int	(*term_delete) _((void));
extern	int	(*term_cursor_right) _((void));
extern	int	(*term_cursor_left) _((void));
extern	int	(*term_clear_to_eol) _((void));
extern	void	term_space_erase _((int));
extern	void	term_reset _((void));

extern  void    copy_window_size _((int *, int *));
extern	int	term_eight_bit _((void));
extern	void	set_term_eight_bit _((int));

#if defined(ISC22) || defined(MUNIX)
/* Structure for terminal special characters */
struct	tchars
{
	char	t_intrc;	/* Interrupt			*/
	char	t_quitc;	/* Quit 			*/
	char	t_startc;	/* Start output 		*/
	char	t_stopc;	/* Stop output			*/
	char	t_eofc;		/* End-of-file (EOF)		*/
	char	t_brkc;		/* Input delimiter (like nl)	*/
}

struct ltchars
{
	char	t_suspc;	/* stop process signal		*/
	char	t_dsuspc;	/* delayed stop process signal	*/
	char	t_rprntc;	/* reprint line			*/
	char	t_flushc;	/* flush output (toggles)	*/
	char	t_werasc;	/* word erase			*/
	char	t_lnextc;	/* literal next character	*/
};
#endif /* ISC22 || MUNIX */

#if defined(_HPUX_SOURCE)

#ifndef _TTY_CHARS_ST_
#define _TTY_CHARS_ST_

/* Structure for terminal special characters */
struct tchars
{
	char	t_intrc;	/* Interrupt			*/
	char	t_quitc;	/* Quit 			*/
	char	t_startc;	/* Start output 		*/
	char	t_stopc;	/* Stop output			*/
	char	t_eofc;		/* End-of-file (EOF)		*/
	char	t_brkc;		/* Input delimiter (like nl)	*/
};

#endif /* _TTY_CHARS_ST_ */

#ifndef TIOCSETC
# define TIOCSETC	_IOW('t', 17, struct tchars)	/* set special chars */
#endif /* TIOCSETC */

#ifndef TIOCGETC
# define TIOCGETC	_IOR('t', 18, struct tchars)	/* get special chars */
#endif /* TIOCGETC */

#ifndef CBREAK
# define CBREAK		0x02	/* Half-cooked mode */
#endif /* CBREAK */

#ifndef SIGWINCH
# define    SIGWINCH    SIGWINDOW
#endif /* SIGWINCH */

#endif /* _HPUX_SOURCE */

/* well, it works */
/* This is lame. This means you cant do window create on ultrix/irix/etc */
#if 0
#ifdef mips
# define fputc(c,f) write(1,&(c),1)
# define fwrite(buffer,len,cnt,f) write(1,buffer,len)
#endif /*mips*/
#endif


/**************************** PATCHED by Flier ******************************/
/****** Patched by Zakath ******/
/* Linux/Alpha has a problem with incompat betw termio/termios */
#if defined(linux) && defined(__alpha__)
# undef TCGETA
# define TCGETA TCGETS
# undef TCSETA
# define TCSETA TCSETS
# undef TCSETAW 
# define TCSETAW TCSETSW
# undef termio
# define termio termios
#endif /* linux && __alpha__ */
/****** ***************** ******/
/****************************************************************************/

#endif /* _TERM_H_ */
