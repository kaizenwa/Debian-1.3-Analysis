/*
 * USERLIST
 * Display management routines
 */

#include "userlist.h"
#include "proto.h"

char *inettos(long addr)
{
    char *ret; 
    int p1, p2, p3, p4;

    ret = (char *) malloc(20);
    p1 = (addr & 0x000000FF);
    p2 = (addr & 0x0000FF00) >> 8;
    p3 = (addr & 0x00FF0000) >> 16;
    p4 = (addr & 0xFF000000) >> 24;

    sprintf(ret, "%d.%d.%d.%d", p1, p2, p3, p4);
    return(ret);
}

char *get_localhost(void)
{
#ifndef	LOCAL_NAME
    char *ret;
    char hostname[80];
    char *p;

    bzero(hostname, 80);

    gethostname((char *) hostname, (size_t) 80);
    if ((p = index(hostname, '.')) != NULL)
      *p = '\0';

    ret = (char *) malloc(strlen((char *) hostname) + 1);

    sprintf(ret, "%s", (char *) hostname);
    return(ret);
#else
    return(LOCAL_NAME);
#endif
}

int exist(char *filename)
{
    FILE *file;

    file = fopen(filename, "r");
    if (file) {
	fclose(file);
	return(1);
    } else
	return(0);
}

void process_display(void)
{
    int i;
    char *our_host;

    our_host = get_localhost();
    printf("Username Real name                      Idletime TTY Remote console\n");

    for (i = 0; i < times_on; i++) {
	char console[30];
	struct passwd *pwent;

	if (strlen((char *) tty_list[i].locale) == 0)
	    sprintf(console, "(%s)", our_host);
	else
	    sprintf(console, "(%s)", tty_list[i].locale);

	if (strlen((char *) tty_list[i].username) > 1) {
	    char username[80], dummy[80], ru[8], fn[80];

	    bzero(username, 80);
	    bzero(dummy, 80);
	    bzero(ru, 8);
	    bzero(fn, 80);

	    strcpy(ru, tty_list[i].username);
	    ru[8] = 0;

	    pwent = getpwnam((char *) ru);

	    if (pwent) {
		sscanf(pwent->pw_gecos, "%[^,],%[^\r\n]\r\n", username, dummy);
		sprintf(fn, "%s/.nofinger", pwent->pw_dir);
	    }

	    if (pwent) {
		if (!exist((char *) fn)) {
		    printf("%-8.8s %-30.30s ",
			ru, username);

		    show_idle((char *) tty_list[i].line);

		    printf("%3.3s %-25.25s\n",
			(char *) tty_list[i].tty, console);
		}
	    } else {
		printf("%-8.8s %-30.30s ",
		    ru, ru);

		show_idle((char *) tty_list[i].line);

		printf("%3.3s %-25.25s\n",
			(char *) tty_list[i].tty, console);
	    }
	}

	fflush(stdout);
    }
}
