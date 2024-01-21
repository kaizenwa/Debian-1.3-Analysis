/* @(#) pipe.h,v 1.4 1995/05/30 16:16:34 nm4 Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pipe.h:
 *	interface file for transport driver in pipe.c.
 */

/* structure for pipe driver's private data */
struct pipe_private {
    char *cmd;
    char *user;				/* run as this user */
    char *group;			/* run as this group */
    int umask;				/* umask for child process */
};

/* transport flags private to pipe.c */
#define PIPE_AS_USER	    0x00010000	/* use uid/gid from addr structure */
#define PIPE_IGNORE_STATUS  0x00020000	/* ignore exit status of program */
#define PIPE_AS_SENDER	    0x00040000	/* use uid of sender */
#define PIPE_LOG_OUTPUT	    0x00080000	/* log program output */
#define PIPE_PARENT_ENV     0x00100000	/* stuff env from parent addr */
#define PIPE_IGNORE_WRERRS  0x00200000	/* ignore write errors */
#define PIPE_DEFER_ERRORS   0x00400000	/* defer rather than fail on errors */
#define PIPE_STATUS_2SENDER 0x00800000	/* report exit status to sender */

