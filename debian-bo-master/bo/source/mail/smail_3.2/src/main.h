/*
#ident	"@(#)smail/src:RELEASE-3_2:main.h,v 1.6 1996/02/26 18:07:42 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * main.h:
 *	interface file for functions in main.c.
 */

#ifndef MAIN_H
#define MAIN_H

/* functions defined in main.c */
void main();

/* macros used in main.c */

/* values for operation_mode */
enum op_mode {
    MODE_DEFAULT,		/* default operation */
    PRINT_VERSION,		/* print the current smail version */
    DAEMON_MODE,		/* operate as a daemon */
    DELIVER_MAIL,		/* deliver mail normally */
    FREEZE_CONFIG,		/* freeze a config file */
    PRINT_QUEUE,		/* print the spool directories */
    REBUILD_ALIASES,		/* rebuild the system alias file */
    SMTP_MODE,			/* read smtp on standard input */
    BATCH_SMTP_MODE,		/* batch smtp on standard input */
    TEST_MODE,			/* addressing parsing tests */
    VERIFY_ADDRS,		/* verify use of addresses */
    NOOP_MODE,			/* do nothing (run queue, if applicable) */
    ROGUE_MODE,			/* display the rogue tombstone mode */
    COPYING_MODE,		/* print the COPYING file */
    PRINT_VARS_MODE		/* print configuration variables */
};

/* values for error_processing */
enum er_proc {
    ERROR_DEFAULT,		/* use defaults */
    DEV_NULL,			/* don't say anything on errors */
    MAIL_BACK,			/* send mail back to sender */
    TERMINAL,			/* write to stderr */
    WRITE_BACK			/* write using write(1) */
};

/* mode used in delivering mail */
enum deliver_mode {
    DELIVER_DEFAULT,		/* use defaults */
    FOREGROUND,			/* deliver mail in foreground */
    BACKGROUND,			/* deliver mail in background */
    QUEUE_MESSAGE		/* don't perform delivery, just queue */
};

/* types of operation based on program name */
enum prog_type {
    PROG_SMAIL,			/* normal operation */
    PROG_RMAIL,			/* invoked as "rmail" */
    PROG_PATHTO,		/* invoked as "pathto" */
    PROG_OPTTO,			/* invoked as "optto" */
    PROG_UUPATH,		/* invoked as "uupath" */
    PROG_NEWALIASES,		/* invoked as "newaliases" */
    PROG_SMAILCONF,		/* invoked as "smailconf" */
    PROG_MAILQ,			/* invoked as "mailq" */
    PROG_RUNQUEUE,		/* invoked as "runqueue" */
    PROG_RSMTP,			/* invoked as "rsmtp" */
    PROG_SMTPD,			/* invoked as "smtpd" */
    PROG_ROGUE,			/* invoked as "rogue" */
    PROG_EXECMAIL		/* invoked as "execmail" (for SCO XENIX) */
};

/* how dots are treated on incoming messages */
enum dot_usage {
    NO_DOT_PROTOCOL,		/* don't do anything special with dots */
    HIDDEN_DOTS,		/* use the hidden-dot algorithm on input */
    DOT_ENDS_MESSAGE,		/* a dot on a line by itself ends message */
    SMTP_DOTS			/* like HIDDEN_DOTS, though EOF is error */
};

#endif	/* MAIN_H */
