/* write.c - poe@daimi.aau.dk */
/* also source for wall(1), make a link wall -> write */
/* bugfixed 1-Dec-92 by poe after bugreport from ... */

#include <sys/types.h>
#include <utmp.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include "pathnames.h"
#include <sys/param.h>

#ifdef SHADOW_PWD
#include <shadow.h>
#endif

#define BSIZ 8192

char message[BSIZ];
char *prog;
int broadcast;

char hostname[MAXHOSTNAMELEN];
char mytty[8];	/* 8 should do it 8-) - st */

void usage()
{
	fprintf(stderr, "Usage: %s user ...\n", prog);
	exit(0);
}

void get_message()
{
	int cnt = BSIZ - 1;
	char *ptr = message;
	int c;
	int state = 0;

	while(--cnt >= 0 && (c = getchar()) && c != EOF) {
	    /* Test for control chars added Fri Mar 10 19:49:30 1995,
	       faith@cs.unc.edu */
	    c = toascii(c);
	    if (!isprint(c) && !isspace(c) && c != '\007') {
		*ptr++ = '^';
		*ptr++ = c ^ 0x40; /* DEL to ?, others to alpha */
		cnt--;
	    } else {
		*ptr++ = c;
	    }
	    if (state == 0 && c == '\n') state = 1;
	    if (state == 1 && c == '.') state = 2;
	    if (state == 1 && c == '\n') state = 1;
	    if (state == 1 && (c != '.' && c != '\n')) state = 0;
	    if (state == 2 && c == '\n') break;
	    if (state == 2 && c != '\n') state = 0;
	}
	*ptr = '\0';
}

void write_line(struct utmp *ut)
{
	FILE *f;
	char term[40] = {'/','d','e','v','/',0};
	char *user;
	struct passwd *pw;
	extern char *getlogin();

	time_t time_now;
	char *timestamp;
	
	/* get time now for message timestamp... */
	time(&time_now);
	timestamp=ctime(&time_now);
	timestamp+=11;
	
	(void) strcat(term, ut->ut_line);

	if(!(user = getlogin())) {
	    if((pw = getpwuid(getuid()))) user = pw->pw_name;
	    else user = "somebody";
	}

	if((f = fopen(term, "w"))) {
		fprintf(f, "\r\n\007\007\007%sessage from %s@%s on %s at %5.5s ...\r\n", 
			broadcast ? "Broadcast m" : "M",
			user, hostname, mytty, timestamp);
		fwrite(message, 1, strlen(message), f);
		fputs("\r\n", f);
		fclose(f);
	} else {
		fprintf(stderr, "Couldn't write %-8s at %s\n",
			ut->ut_user, ut->ut_line);
	}
}

void write_user(char *user)
{
	struct utmp *ut;

	utmpname(_PATH_UTMP);
	setutent();
	
	while((ut = getutent())) {
		if(ut->ut_type == USER_PROCESS 
		   && !strncmp(user, ut->ut_user, sizeof(ut->ut_user)))
			write_line(ut);
	}
	endutent();
}

void wall()
{
	/* write to all users. */
	struct utmp *ut;
		
	utmpname(_PATH_UTMP);
	setutent();
	
	while((ut = getutent())) {
		if(ut->ut_type == USER_PROCESS)
			write_line(ut);
	}
	endutent();
}

int main(argc, argv)
	int argc;
	char *argv[];
{
	int i, j;
	char *p;
	
	gethostname(hostname, MAXHOSTNAMELEN);
	strcpy(mytty,(strrchr(ttyname(0), '/'))+1);
	
	prog = argv[0];
	if((p = strrchr(argv[0], '/'))) prog = p + 1;

	if(!strcmp("wall", prog)) {
	        broadcast = 1;
	        if(argc < 2) {
		    get_message();
		} else {
		    message[0] = '\0';
		    for(i = 1; i < argc; i++) {
			strcat(message, argv[i]);
			message[j = strlen(message)] = ' ';
			message[j+1] = '\0';
		    }
		}
		wall();
	} else {
		if(argc < 2) usage();
		broadcast = 0;
		get_message();
	
		for(i = 1; i < argc; i++)
			write_user(argv[i]);
	}
	exit(0);
}
