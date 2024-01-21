/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Terminal interface definitions.
 */

#ifndef _NN_TERM_H
#define _NN_TERM_H 1

extern void init_term();
extern void home();
extern void otoxy();
extern void clrdisp();
extern void clrpage();
extern void clrline();
extern void tprintf();
extern void tvprintf();
extern void tputc();

extern so_gotoxy();
extern void so_printf();
extern void so_end();

/* extern no_raw(), raw(), unset_raw(); */

extern int Lines, Columns;
extern int cookie_size;
extern int STANDOUT;

extern char *get_s();

#define	NONE		(char *)NULL /* no default string etc. */

#define	GET_S_BUFFER	256 	/* if caller want to reuse get_s buffer */

extern get_c();

/* special keys returned by get_c() */

#define	K_interrupt	CONTROL_('G')

#define	K_up_arrow	0x0081
#define	K_down_arrow	0x0082
#define K_left_arrow	0x0083
#define K_right_arrow	0x0084

#define	K_function(n)	(0x0085 + n)
#define K_m_d1		0x008f
#define K_m_d2		0x0090
#define K_m_d3		0x0091
#define K_m_u1		0x0092
#define K_m_u2		0x0093
#define K_m_u3		0x0094

#define	GETC_COMMAND	0x4000	/* bit set by get_c to return a command */


/*
 *	prompt_line = ...
 *	prompt( [P_COMMAND], ] [ format [, arg1 ... , arg4] ] );
 *
 *	P_MOVE:		just move to prompt line
 *	P_REDRAW:	redraw prompt
 *      P_VERSION:	print version on prompt line
 */


extern void prompt();

extern int prompt_line;	/* prompt line */

#define	P_MOVE		(char *)1
#define P_REDRAW	(char *)5
#define	P_VERSION	(char *)9
#define P_SAVE		(char *)13
#define P_RESTORE	(char *)17

extern void display_file();

#define	CLEAR_DISPLAY	0x01
#define	CONFIRMATION	0x02

#endif /* _NN_TERM_H */
