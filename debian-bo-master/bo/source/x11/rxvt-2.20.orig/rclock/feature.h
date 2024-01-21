/*--------------------------------*-C-*---------------------------------*
 * file:	feature.h
 *
 * Compile-time configuration.
 * ----------------------------------------------------------------------
 * Copyright 1996
 * mj olesen <olesen@me.QueensU.CA> Queen's Univ at Kingston
 *
 * You can do what you like with this source code provided you don't make
 * money from it and you include an unaltered copy of this message
 * (including the copyright).  As usual, the author accepts no
 * responsibility for anything, nor does he guarantee anything whatsoever.
 *----------------------------------------------------------------------*/
#ifndef _FEATURE_H
#define _FEATURE_H

/*----------------------------------------------------------------------*
 * #define ICONWIN
 *	to enable fancy (active) icon
 *
 * #define REMINDERS
 *	to enable the appointment reminder functions
 *
 * #define NO_REMINDER_EXEC
 *      to disable the execution of a program on an appointment
 *
 * #define MAIL
 *	to enable xbiff-type mail reminders
 *
 * #define MAIL_BELL
 *	to enable xbiff-type mail reminders with a beep
 *
 * #define MAIL_SPAWN	"xmh\ -font\ 7x14\&"
 *	to define a mail program to run
 *
 * #define MAIL_SPOOL	"/var/spool/mail/"
 *	to define the mail spool when the $MAIL variable isn't set
 *
 * program size approximately doubles from no options to all options
 *----------------------------------------------------------------------*/
#define ICONWIN
#define REMINDERS
/* #define NO_REMINDER_EXEC */
#define MAIL
/* #define MAIL_BELL */
/* #define MAIL_SPAWN	"xmh\ -font\ 7x14\&" */
/* #define MAIL_SPOOL	"/var/spool/mail/" */

/*----------------------------------------------------------------------*
 * #define CLOCKUPDATE	30
 * 	to define the frequency (seconds) to update the clock
 *
 * #define MAILUPDATE	60
 * 	to define the frequency (seconds) to check for new mail
 *
 * #define REMINDERS_TIME	10
 *	to define the frequency (minutes) to check ~/.rclock
 *
 * #define DEFER_TIME	3
 *	to define the amount (minutes) to defer a message
 *
 * #define ADJUST_TIME
 *	to add -adjust command-line option
 *
 * #define CENTURY	2000
 *	to set the base century for 2 digit year short-hand
 *----------------------------------------------------------------------*/
#define	CLOCKUPDATE	30
#define MAILUPDATE	60
#define REMINDERS_TIME	10
#define DEFER_TIME	3
#define ADJUST_TIME
/* #define CENTURY	2000 */

/*----------------------------------------------------------------------*
 * #define FONT_NAME	"7x14"
 * 	to define the font to be used for appointment reminders
 *
 * #define FG_COLOR_NAME	"black"
 * #define BG_COLOR_NAME	"white"
 * 	to define the foreground/background colors to use
 *----------------------------------------------------------------------*/
#define FONT_NAME	"7x14"
#define FG_COLOR_NAME	"black"
#define BG_COLOR_NAME	"white"

/*----------------------------------------------------------------------*
 * #define DAY_NAMES	"umtwrfs*"
 *	define this string appropriate for any language.
 *
 *	It starts with a symbol for Sunday, ends with Saturday, then '*'
 *	NOTE: 8 characters total - 7 days of the week plus '*'
 *----------------------------------------------------------------------*/
#define DAY_NAMES	"umtwrfs*"

/*----------------------------------------------------------------------*
 * #define SUBTICKS
 *	to show additional minute/second markings
 *----------------------------------------------------------------------*/
#define SUBTICKS

/*----------------------------------------------------------------------*
 * sort out conflicts
 *----------------------------------------------------------------------*/
#if defined (MAIL_BELL) || defined (MAIL_SPAWN) || defined (MAIL_SPOOL)
# ifndef MAIL
#  define MAIL
# endif
#endif

#endif	/* whole file */
/*----------------------- end-of-file (C header) -----------------------*/
