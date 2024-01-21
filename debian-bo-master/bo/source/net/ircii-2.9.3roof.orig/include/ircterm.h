/*
 * term.h: header file for term.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: ircterm.h,v 1.5.2.3 1996/06/26 13:20:45 mrg Exp $
 */

#ifndef __ircterm_h_
# define __ircterm_h_

#ifdef INCLUDE_CURSES_H
# include <curses.h>
#endif

/* this is really busted on solaris -mrg */
#if 0
#ifdef INCLUDE_TERM_H
# include <term.h>
# ifdef lines
#  undef lines
# endif
# ifdef columns
#  undef columns
# endif
#endif
#endif

#ifdef MUNIX
# include <sys/ttold.h>
#endif

extern	int	term_reset_flag;
extern	char	*CM,
		*DO,
		*CE,
		*CL,
		*CR,
		*NL,
		*SO,
		*SE,
		*US,
		*UE,
		*MD,
		*ME,
		*BL;
extern	int	CO,
		LI,
		SG;

#ifdef HPUX
# define TPUTSRETVAL int
# define TPUTSARGVAL char
#else /* HPUX */
# ifdef __sgi
#  include <sys/param.h>
#  ifdef SEEKLIMIT32	/* XXX HACK HACK HACK */
			/* this define is _NOT_ in irix5 */
#   define TPUTSRETVAL int
#   define TPUTSARGVAL char
/*
 * XXX
 *
 * if this causes your compile to fail, then just delete it.  and
 * please tell me (use the `ircbug' command).  thanks.
 */
char *tgetstr(char *, char **);
#  else
#   define TPUTSRETVAL void
#   define TPUTSARGVAL int
#  endif /* SEEKLIMIT32 */
# else /* __sgi */
#  define TPUTSRETVAL void
#  define TPUTSARGVAL int
# endif /* __sgi */
#endif /* HPUX */

	TPUTSRETVAL putchar_x _((TPUTSARGVAL));

#define tputs_x(s)		(tputs(s, 0, putchar_x))

#define term_underline_on()	(tputs_x(US))
#define term_underline_off()	(tputs_x(UE))
#define term_standout_on()	(tputs_x(SO))
#define term_standout_off()	(tputs_x(SE))
#define term_clear_screen()	(tputs_x(CL))
#define term_move_cursor(c, r)	(tputs_x(tgoto(CM, (c), (r))))
#define term_cr()		(tputs_x(CR))
#define term_newline()		(tputs_x(NL))
#define term_beep()		(tputs_x(BL),fflush(current_screen ? \
					current_screen->fpout : stdout))
#define	term_bold_on()		(tputs_x(MD))
#define	term_bold_off()		(tputs_x(ME))

	RETSIGTYPE	term_cont _((void));
	int	term_echo _((int));
	void	term_init _((void));
	int	term_resize _((void));
	void	term_pause _((unsigned char, char *));
	void	term_putchar _((unsigned char));
	int	term_puts _((char *, int));
	void	term_flush _((void));
	void	term_space_erase _((int));
	void	term_reset _((void));
	void    copy_window_size _((int *, int *));
	int	term_eight_bit _((void));
	void	set_term_eight_bit _((int));
	
extern int	 (*term_scroll) _((int, int, int));
extern int	 (*term_insert) _((char));
extern int	 (*term_delete) _((void));
extern int	 (*term_cursor_right) _((void));
extern int	 (*term_cursor_left) _((void));
extern int	 (*term_clear_to_eol) _((void));

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
#ifdef mips
# define fputc(c,f) write(1,&(c),1)
# define fwrite(buffer,len,cnt,f) write(1,buffer,len)
#endif /*mips*/

#endif /* __ircterm_h_ */
