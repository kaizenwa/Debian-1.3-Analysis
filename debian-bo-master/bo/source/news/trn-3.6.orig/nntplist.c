/* $Id: nntplist.c,v 3.5 1993/04/18 20:18:23 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "nntpclient.h"

void Usage _((void));
void finalize _((int));
char nntp_handle_timeout _((bool_int));

int debug = 0;			/* make nntpclient.c happy */

#ifdef USE_GENAUTH
char *loginName;
#endif

int
main(argc, argv)
int argc;
char *argv[];
{
    char command[32];
    char *action = NULL;
    char *wildarg = NULL;
    register FILE *out_fp = NULL;

    while (--argc) {
	if (**++argv == '-') {
	    switch (*++*argv) {
	    case 'o':
		if (out_fp || !--argc)
		    Usage();
		out_fp = fopen(*++argv, "w");
		if (out_fp == NULL) {
		    perror(*argv);
		    exit(1);
		}
		break;
	    case 'x':
		if (wildarg || !--argc)
		    Usage();
		wildarg = *++argv;
		break;
	    default:
		Usage();
		/* NO RETURN */
	    }
	}
	else if (!action)
	    action = *argv;
	else
	    Usage();
    }
    if (!action)
	action = "ACTIVE";
    if (!out_fp)
	out_fp = stdout;

#ifdef USE_GENAUTH
    /* get login name */
    loginName = getenv("USER");
    if (loginName == Nullch) {
	loginName = getenv("LOGNAME");
#ifdef GETLOGIN
	if (loginName == Nullch)
	    loginName = getlogin();
#endif
    }
#endif

    if (!nntp_connect(0))
	exit(1);
    sprintf(command,"LIST %s",action);
    if (wildarg)
	sprintf(command+strlen(command)," %s",wildarg);
    nntp_command(command); 
#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	fprintf(stderr,"nntplist: Can't get %s file from server.\n",action);
	fprintf(stderr, "Server said: %s\n", ser_line);
	finalize(1);
    }

    while (nntp_gets(ser_line, sizeof ser_line) >= 0) {
	if (NNTP_LIST_END(ser_line))	/* while there's another line */
	    break;			/* get it and write it to */
	fputs(ser_line, out_fp);
	putc('\n', out_fp);
    }

#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    nntp_close(TRUE);
    return 0;
}

void
Usage()
{
    fprintf(stderr, "Usage: nntplist [-x WildSpec] [-o OutputFile] [type]\n\
\nWhere type is any of the LIST command arguments your server accepts.\n");
    exit(1);
}

void
finalize(num)
int num;
{
    nntp_close(TRUE);
    exit(num);
}

char
nntp_handle_timeout(strict)
bool_int strict;
{
    nntp_error("\n503 Server timed out.\n");
    if (strict)
	finalize(1);
    return NNTP_CLASS_FATAL;
}

static char nomem[] = "trn: out of memory!\n";

/* paranoid version of malloc */

char *
safemalloc(size)
MEM_SIZE size;
{
    char *ptr;

    ptr = malloc(size ? size : (MEM_SIZE)1);
    if (ptr == Nullch) {
	fputs(nomem,stdout) FLUSH;
	finalize(1);
    }
    return ptr;
}

/* paranoid version of realloc.  If where is NULL, call malloc */

char *
saferealloc(where,size)
char *where;
MEM_SIZE size;
{
    char *ptr;

    ptr = realloc(where, size ? size : (MEM_SIZE)1);
    if (!ptr) {
	fputs(nomem,stdout) FLUSH;
	finalize(1);
    }
    return ptr;
}
