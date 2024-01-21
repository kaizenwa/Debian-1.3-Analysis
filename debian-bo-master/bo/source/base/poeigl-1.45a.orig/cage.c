/* cage.c - setup a cage for potential intruders. Mainly creates a
   chroot()'ed environment and starts the users shell according the
   encaged passwd file. Intended to be used as shell in the real passwd
   file for eg. limited modem accounts.

   In order to be able to use chroot(), cage needs to be setuid root.

   We fake utmp and wtmp inside the cage so last(1) and getlogin(3) will
   work.

   Unfortunately, ttyname(3) won't work inside the cage.

   Created 12-Nov-93 by Peter Orbaek <poe@daimi.aau.dk>
   Developed with GCC 2.4.3 under Linux 0.99.13
*/

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <memory.h>
#include <pwd.h>
#include <utmp.h>
#include <sys/syslog.h>
#include <sys/file.h>
#include "pathnames.h"

/* default for the CAGEDIR define, prefer compilerflag definition */
#ifndef CAGEDIR
# define CAGEDIR "/usr/cage"
#endif

#define DIE(s)      (syslog(LOG_ERR|LOG_PERROR, s),    exit(1), 1)
#define DIE2(s1,s2) (syslog(LOG_ERR|LOG_PERROR,s1,s2), exit(1), 1)
#define ever (;;)

void
read_utmp(ut)
     struct utmp *ut;
{
    char *		p;
    struct utmp *	real_ut;

    /* get our entry from the real utmp, set by login */
    memset((char *)ut, 0, sizeof(struct utmp));
    (p = ttyname(0)) || DIE("can't get name of tty");
    strncpy(ut->ut_line, p+sizeof("/dev/")-1, sizeof(ut->ut_line));

    utmpname(_PATH_UTMP);
    setutent();
    (real_ut = getutline(ut)) || DIE("can't get utmp entry");
    memcpy((char *)ut, (char *)real_ut, sizeof(struct utmp));
    endutent();
}

void 
write_uwtmp(ut)
     struct utmp *ut;
{
    int wtmp;

    /* write fake utmp and wtmp inside cage */
    utmpname(_PATH_UTMP);
    setutent(); pututline(ut); endutent();

    if((wtmp = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0644)) >= 0) {
	flock(wtmp, LOCK_EX);
	write(wtmp, (char *)ut, sizeof(struct utmp));
	flock(wtmp, LOCK_UN);
	close(wtmp);
    }
}

void
utmp_task(ut)
     struct utmp *ut;
{
    /* body of a process that waits until the user logs out of the cage,
       then it removes the relevant entry from the fake utmp and
       exits. */

    int wtmp;

    close(0); close(1); close(2);
    setsid();

    for ever {
	if(getppid() == 1) {
	    /* we have been reparented by init, ie. the cage died, now
	       clear the fake utmp entry */
	    time(&ut->ut_time);
	    memset(&ut->ut_user, 0, UT_NAMESIZE);
	    memset(&ut->ut_host, 0, sizeof(ut->ut_host));
	    ut->ut_type = DEAD_PROCESS;
	    ut->ut_pid = 0;
	    ut->ut_addr = 0;
	    utmpname(_PATH_UTMP);
	    setutent(); 
	    pututline(ut);
	    endutent();
	    if((wtmp = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0644)) >= 0) {
		flock(wtmp, LOCK_EX);
		write(wtmp, (char *)ut, sizeof(struct utmp));
		flock(wtmp, LOCK_UN);
		close(wtmp);
	    }
	    exit(0);
	}
	sleep(30); /* sleep 30 secs between each check */
    }
}

int
main(argc, argv)
     int argc;
     char *argv[];
{
    struct passwd *	pwd;
    char 		shell[100];
    char *		p;
    struct utmp 	ut;

    openlog("cage", LOG_AUTH, LOG_PID);    /* setup syslog */

    read_utmp(&ut); /* read the real utmp entry */

    /* change root to cage */
    (chdir(CAGEDIR) >= 0) || DIE2("can't cd to %s", CAGEDIR);
    (chroot(CAGEDIR) >= 0) || DIE2("cannot chroot to %s", CAGEDIR);

    write_uwtmp(&ut); /* write fake utmp and wtmp entries */

    if(fork() == 0) utmp_task(&ut); /* start utmp cleanser */

    (setuid(getuid()) >= 0) || DIE("setuid failed"); /* drop root privs */

    (pwd = getpwuid(getuid())) 
      || DIE2("can't find user %d in encaged passwd file", getuid());

    /* set various environment variables from encaged passwd file */
    /* most are set by the real login program though */
    pwd->pw_dir && setenv("HOME", pwd->pw_dir, 1);
    pwd->pw_dir && setenv("LOGDIR", pwd->pw_dir, 1);
    pwd->pw_shell && setenv("SHELL", pwd->pw_shell, 1);

    pwd->pw_dir && pwd->pw_dir[0] && chdir(pwd->pw_dir);

    /* we are real paranoid about a botched shell field in the passwd file */
    p = NULL;
    if(pwd->pw_shell && pwd->pw_shell[0]) {
	p = strrchr(pwd->pw_shell, '/');
	if(p == NULL) p = pwd->pw_shell;
    }

    (p != NULL) || DIE2("shell field for user %d is botched", getuid());
    closelog();	/* no more errors to report */

    /* prepend '-' to the shellname to make it a login shell */
    shell[0] = '-';
    strncpy(shell+1, p+1, 98);

    execlp(pwd->pw_shell, shell, NULL);
    fprintf(stderr, "login: no shell: %s\r\n", strerror(errno));
    exit(1);
}
