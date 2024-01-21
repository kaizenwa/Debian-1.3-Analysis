#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <utmp.h>
#include <paths.h>

#include <pwdb/pwdb_public.h>
#include <pwdb/_pwdb_macros.h>

char *pwdb_posix_getlogin()
{
    static char *logname=NULL;
    const char *terminal=NULL;
    struct utmp ut, *utr;

    D(("called."));

    /* clean up */
    if (logname) {
	logname = _pwdb_delete_string(logname);
    }

    /* identify the current terminal */
    if (isatty(STDIN_FILENO)) {
	D(("terminal"));
	terminal = ttyname(STDIN_FILENO);
	if (terminal && strncmp("/dev/", terminal, 5) == 0) {
	    terminal += 5;
	}
    } else {
	/* if no terminal, we check for a $DISPLAY variable */
	D(("testing for $DISPLAY"));
	terminal = getenv("DISPLAY");
    }

    /* did we get something? */
    D(("terminal = %s", terminal));
    if (terminal == NULL)
	return NULL;

    /* obtain the id of the user from their utmp entry */
    memset(&ut, 0, sizeof(ut));
    strncpy(ut.ut_line, terminal, sizeof(ut.ut_line));
    setutent();
    utr = getutline(&ut);
    if (utr != NULL) {
	logname = malloc(1+sizeof(ut.ut_user));
	D(("located an entry for pid=%d", utr->ut_pid));
	if (logname != NULL) {
	    strncpy(logname, utr->ut_user, sizeof(ut.ut_user));
	    logname[sizeof(ut.ut_user)] = '\0';
	    D(("user is %s", logname));
	} else {
	    D(("out of memory."));
	}
    }
    endutent();

    /* return the name */

    return logname;
}
