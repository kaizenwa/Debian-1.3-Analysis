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

#include "irc_std.h"

extern	int	term_reset_flag;
extern	char	*CM, *DO, *CE, *CL, *CR, *NL, *SO, *SE, *US, *UE, *MD, *ME, *BL;
extern	int	CO, LI, SG; 

extern int putchar_x _((int));

#define tputs_x(s)			(tputs(s, 0, putchar_x))
#define term_underline_on()		(tputs_x(US))
#define term_underline_off()		(tputs_x(UE))
#define term_standout_on()		(tputs_x(SO))
#define term_standout_off()		(tputs_x(SE))
#define term_clear_screen()		(tputs_x(CL))
#define term_move_cursor(c, r)		(tputs_x(tgoto(CM, (c), (r))))
#define term_cr()			(tputs_x(CR))
#define term_newline()			(tputs_x(NL))
#define	term_bold_on()			(tputs_x(MD))
#define	term_bold_off()			(tputs_x(ME))

extern	RETSIGTYPE term_cont 		_((int));
extern 	void 	term_beep 		_((void));
extern	int	term_echo 		_((int));
extern	void	term_init 		_((void));
extern	int	term_resize 		_((void));
extern	void	term_pause 		_((char, char *));
extern	void	term_putchar 		_((unsigned char));
extern	int	term_puts 		_((char *, int));
extern	void	term_flush 		_((void));
extern	int	(*term_scroll) 		_((int, int, int));
extern	int	(*term_insert) 		_((char));
extern	int	(*term_delete) 		_((void));
extern	int	(*term_cursor_right) 	_((void));
extern	int	(*term_cursor_left) 	_((void));
extern	int	(*term_clear_to_eol) 	_((void));
extern	void	term_space_erase 	_((int));
extern	void	term_reset 		_((void));

extern  void    copy_window_size 	_((int *, int *));
extern	int	term_eight_bit 		_((void));
extern	void	set_term_eight_bit 	_((int));

#endif /* _TERM_H_ */
