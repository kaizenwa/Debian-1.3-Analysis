/* @(#) uuname.h,v 1.3 1992/07/11 11:50:52 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * uuname.h:
 *	interface file for router driver in uuname.c
 */

/* flag attributes */
#define UU_CACHED	0x00010000	/* internal - cached once */
#define UU_FILE_FAIL	0x00020000	/* internal - cached FILE_FAIL */
#define UU_FILE_AGAIN	0x00040000	/* internal - cached FILE_AGAIN */

/* private data structure for uuname driver */
struct uuname_private {
    char *cmd;				/* command to display names */
    char *domain;			/* domains to strip from names */
    char *required;			/* required domain names */
    char *statfile;			/* file associated with command */
    char *cmd_output;			/* internal - cached output from cmd */
    char *cmd_output_end;		/* internal - end of cached output */
    char *error_text;			/* internal - processing error */
    int stat_ino;			/* internal - st_ino from stat */
    int stat_mtime;			/* internal - st_mtime from stat */
};
