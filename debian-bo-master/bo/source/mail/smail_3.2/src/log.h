/* @(#) log.h,v 1.3 1992/07/11 11:49:29 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * log.h:
 *	interface file for routines in log.c
 */

/* macros used in log.c */
#define LOG_SYS		0x0001		/* write_log sends to system log */
#define LOG_CONS	0x0002		/* write_log sends to console */
#define LOG_MLOG	0x0004		/* write_log sends to per-msg log */
#define LOG_PANIC	0x0008		/* write_log sends to panic log */
#define LOG_TTY		0x0010		/* write_log sends to stderr */
