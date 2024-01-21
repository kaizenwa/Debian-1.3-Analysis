/*
 * USERLIST
 * Initialization routines
 */

#include "userlist.h"
#include "proto.h"
#include "config.h"
#ifdef	BSD
#include "../src/getutent.h"
#endif

#include <ctype.h>

TTY_FROM tty_list[MAX_TTYS];
int times_on;

char *remove_crap(char *str)
{
    char *ret;
    int i, x = 0;

    ret = (char *) malloc(strlen(str));
    for (i = 0; i < strlen(str); i++)
	if (isalnum(str[i]) || (str[i] == '.') || (str[i] == '-'))
	    ret[x++] = str[i];

    return(ret);
}

void initialize_userlist(void)
{
    struct utmp *ut;

    times_on = 0;

    while((ut = getutent()) != NULL)
#ifdef	BSD
	{
#else
	if (ut->ut_type == USER_PROCESS) {
#endif
	    tty_list[times_on].username = malloc(8);
	    tty_list[times_on].tty = malloc(2);
	    tty_list[times_on].locale = malloc(16);
	    tty_list[times_on].line = malloc(12);

#ifdef	BSD
	    strncpy(tty_list[times_on].username, (char *) ut->ut_name, 16);
#else
	    strncpy(tty_list[times_on].username, (char *) ut->ut_user, 8);
	    strncpy(tty_list[times_on].tty, (char *) ut->ut_id, 2);
	    tty_list[times_on].ip_addr = ut->ut_addr;
#endif
	    strncpy(tty_list[times_on].locale, (char *) ut->ut_host, 16);
	    strncpy(tty_list[times_on].line, (char *) ut->ut_line, 12);

	    tty_list[times_on].time = ut->ut_time;
	    times_on++;
	}
}
