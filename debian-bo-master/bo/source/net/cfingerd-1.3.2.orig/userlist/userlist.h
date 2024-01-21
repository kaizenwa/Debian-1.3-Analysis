/*
 * USERLIST - Configurable "rwho"-type replacement, shows only users online.
 * Header file
 */

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <utmp.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>

#define	BOOL	int

#define	TRUE	1
#define	FALSE	0

/* Display types */
#define	DISPLAY_CFINGERD	1
#define	DISPLAY_RFC1288		2
#define	DISPLAY_LINUX		3
#define	DISPLAY_GNU		4
#define	DISPLAY_BSDOS		5
#define	DISPLAY_UNIX5		6

typedef struct {
    char *username;
    char *tty;
    char *locale;
    char *line;
    long ip_addr;
    time_t time;
} TTY_FROM;

#include "config.h"

extern TTY_FROM tty_list[MAX_TTYS];
extern int times_on, display_type;
