/*
 * dowall.c	- Write to all users on the system.
 *
 * Author:	  Miquel van Smoorenburg, miquels@drinkel.cistron.nl
 * 
 * Version:	  1.01  18-11-1992 	initial version.
 *		  1.1   31-01-1993 	Made the open() non blocking, so that
 *					false utmp entries will not block wall.
 *		  1.2   13-05-1993	Added some more code to prevent
 *					'hanging' on open()'s or write()'s.
 *		  1.3   06-Jul-1996	Fixed "you don't exist" message for
 *					uid == 0 (root).
 *		  1.4   16-Apr-1997	Use getutent() etc for utmp file access
 *
 *
 *		This file is part of the sysvinit suite,
 *		Copyright 1991-1997 Miquel van Smoorenburg.
 *
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <utmp.h>
#include <pwd.h>
#include <fcntl.h>
#include <signal.h>
#include <setjmp.h>

static jmp_buf jbuf;

#define AEROSMITH

/* Alarm handler */
/*ARGSUSED*/
static void handler(arg)
int arg;
{
  signal(SIGALRM, handler);
  longjmp(jbuf, 1);
}

/*
 * Wall function.
 */
void wall(text, fromshutdown)
char *text;
int fromshutdown;
{
  FILE *tp;
  char line[81];
  char term[32];
  static char *user, ttynm[16], *date;
  static int fd, init = 0;
  struct passwd *pwd;
  char *tty, *p;
  time_t t;
  struct utmp *utmp;
  int uid;

  setutent();

  if (init == 0) {
	uid = getuid();
  	if ((pwd = getpwuid(uid)) == (struct passwd *)0 &&
	    (uid != 0 || fromshutdown)) {
  		fprintf(stderr, "You don't exist. Go away.\n");
  		exit(1);
  	}
  	user = pwd ? pwd->pw_name : "root";
  	if ((p = ttyname(0)) != (char *)0) {
  		if ((tty = strrchr(p, '/')) != NULL)
  			tty++;
  		else
  			tty = p;
  		sprintf(ttynm, "(%s) ", tty);	
  	} else
  		ttynm[0] = 0;
  	init++;
	signal(SIGALRM, handler);
  }
  
  /* Get the time */
  time(&t);
  date = ctime(&t);
  for(p = date; *p && *p != '\n'; p++)
  	;
  *p = 0;

  sprintf(line, "\007\r\nBroadcast message from %s %s%s...\r\n\r\n", user,
  	ttynm, date);

  while((utmp = getutent()) != NULL) {
  	if(utmp->ut_type != USER_PROCESS ||
	   utmp->ut_user[0] == 0) continue;
  	sprintf(term, "/dev/%s", utmp->ut_line);

	alarm(2); /* Sometimes the open/write hangs in spite of the O_NDELAY */
#ifdef O_NDELAY
	/* Open it non-delay */
	if (setjmp(jbuf) == 0 && (fd = open(term, O_WRONLY | O_NDELAY )) > 0) {
  		if ((tp = fdopen(fd, "w")) != NULL) {
  			fputs(line, tp);
  			fputs(text, tp);
#ifdef AEROSMITH
			if (fromshutdown && !strcmp(utmp->ut_user, "tyler"))
			 fputs("(Oh hello Mr. Tyler - going DOWN?)\r\n", tp);
#endif
  			fclose(tp);
  		} else
			close(fd);
		fd = -1;
		alarm(0);
	}
	if (fd >= 0) close(fd);
#else
  	if (setjmp(jbuf) == 0 && (tp = fopen(term, "w")) != NULL) {
  		fputs(line, tp);
  		fputs(text, tp);
		alarm(0);
  		fclose(tp);
		tp = NULL;
  	}
	if (tp != NULL) fclose(tp);
#endif
	alarm(0);
  }
  endutent();
}

