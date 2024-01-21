/* W - show what logged in users are doing.  Almost entirely rewritten from
 * scratch by Charles Blake circal June 1996.  Some vestigal traces of the
 * original may exist.  That was done in 1993 by Larry Greenfield with some
 * fixes by Michael K. Johnson.
 */
#include "proc/version.h"
#include "proc/whattime.h"
#include "proc/readproc.h"
#include "proc/devname.h"
#include "proc/ps.h"
#include "proc/output.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <utmp.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/param.h>	/* for HZ */

int ignoreuser = 0;		/* for '-u' */
proc_t **procs;			/* our snapshot of the process table */

typedef struct utmp utmp_t;

void    showinfo(utmp_t *login_rec, int formtype, int maxcmd, int from);
proc_t *getproc(utmp_t *login_rec, char *tty, int *jcpu, int *bogus);
void    print_logintime(time_t login_time, FILE* fout);

#ifndef W_SHOWFROM
#   define FROM_STRING "on"
#else
#   define FROM_STRING "off"
#endif

#ifdef NEW_TIME_DEFAULT
int new_time = 1;
#else
int new_time = 0;
#endif

#define USERSZ (sizeof u->ut_user)

int main(int argc, char **argv) {
    int header=1, longform=1, from=1, args, maxcmd = 80, fd;
    char ch, *user = NULL;
    utmp_t *u, *last;
    struct stat sbuf;
    struct winsize win;

#ifdef W_SHOWFROM
    from = 0;
#endif
    set_linux_version();

    for (args=0; (ch = getopt(argc, argv, "ahlusfV")) != EOF; args++)
	switch (ch) {
          case 'a': new_time = !new_time; break;
	  case 'h': header = 0;		  break;
	  case 'l': longform = 1;	  break;
	  case 's': longform = 0;	  break;
	  case 'f': from = !from;	  break;
	  case 'V': display_version();	exit(0);
	  case 'u': ignoreuser = 1;	  break;
	  default:
	    printf("usage: w -ahlsufV [user]\n"
                   "    -a    alternate time format\n"
		   "    -h    skip header\n"
		   "    -l    long listing (default)\n"
		   "    -s    short listing\n"
		   "    -u    ignore uid of processes\n"
		   "    -f    toggle FROM field (default %s)\n"
		   "    -V    display version\n", FROM_STRING);
	    exit(1);
	}

    if (optind < argc) user = argv[optind];

    if (ioctl(1, TIOCGWINSZ, &win) != -1 && win.ws_col > 0)
	maxcmd = win.ws_col;
    if (maxcmd < 71) {
	fprintf(stderr, "%d column window is too narrow\n", maxcmd);
	exit(1);
    }
    maxcmd -= 26 + (from ? 17 : 0) + (longform ? 21 : 0);
    if (maxcmd < 3)
	fprintf(stderr, "warning: screen width %d suboptimal.\n", win.ws_col);

    procs = readproctab(PROC_FILLCMD|PROC_FILLTTY|PROC_FILLSTAT);

    if (header) {				/* print uptime and headers */
	print_uptime();
	printf("USER      TTY   ");
	if (from)
	    printf("FROM             ");
	if (longform)
	    printf(" LOGIN@  IDLE   JCPU   PCPU  WHAT\n");
	else
	    printf("  IDLE  WHAT\n");
    }
    /* open + mmap utmp.  Sure I could us getutent, but I looked at
     * the libc code and it didn't seem to add any real value. */
    if ((fd = open(UTMP_FILE, O_RDONLY)) < 0 ||
	fstat(fd, &sbuf) == -1 ||
	(u = (utmp_t*)mmap(0, sbuf.st_size,
                           PROT_READ, MAP_SHARED, fd, 0)) == (utmp_t*) -1 ||
	close(fd) == -1)
	return fprintf(stderr, "%s: %s during open,fstat,mmap,close\n",
		       UTMP_FILE, sys_errlist[errno]);
    if (u) {
        for (last = u + sbuf.st_size / (sizeof *u); u <= last; u++)
            if (u->ut_type == USER_PROCESS &&
                (user ? !strncmp(u->ut_user, user, strlen(u->ut_user)) : *u->ut_user))
                showinfo(u, longform, maxcmd, from);
    }
    return 0;
}

/* This routine is careful since some programs leave utmp strings unprintable.
 * Always outputs `len' chars padded with spaces on the right if necessary.
 */
void print_host(char* host, int len) {
    char *last = host + len - 1;
    for ( ; host <= last ; host++)
        if (isprint(*host) && *host != ' ' && !(*host & 0x80))
	    fputc(*host, stdout);
	else
	    break;
     /* if *any* unprintables(or blanks), replace rest of line with spaces */
     if (host <= last)
	  for ( ; host <= last ; host++)
	      fputc(' ', stdout);
}

time_t idletime(utmp_t *u, char *tty) {
    struct stat terminfo;
    if (stat(tty, &terminfo) != 0)
	return 0;
    if (terminfo.st_atime > u->ut_time) {
    	return time(NULL) - terminfo.st_atime;
    } else {
	return time(NULL) - u->ut_time;
    }
}

void showinfo(utmp_t *u, int formtype, int maxcmd, int from) {
    int jcpu, i, ut_pid_found, n,len;
    char uname[USERSZ + 1] = "",
	tty[5 + sizeof u->ut_line + 1] = "/dev/";
    proc_t *best;

    for (i=0; i < sizeof u->ut_line; i++)	/* clean up tty if garbled */
	if (isalnum(u->ut_line[i]))
	    tty[i+5] = u->ut_line[i];
	else
	    tty[i+5] = '\0';

    best = getproc(u, tty + 5, &jcpu, &ut_pid_found);

    /* just skip if stale utmp entry (i.e. login proc doesn't exist).  If there
     * is a desire a cmdline flag could be added to optionally show it with a
     * prefix of (stale) in front of cmd or something like that.
     */
    if (!ut_pid_found)
	return;

    strncpy(uname, u->ut_user, USERSZ);		/* force NUL term for printf */
    if (formtype) {
	printf("%-9.8s%-7.7s", uname, u->ut_line);
	if (from)
	    print_host(u->ut_host, sizeof u->ut_host + 1);
	print_logintime(u->ut_time, stdout);
	if (*u->ut_line == ':')			/* idle unknown for xdm logins */
	    printf("       ");
	else {
            putc(' ',stdout);
            if (new_time) {
                n=print_time_ival(idletime(u, tty), 6, 0);
            } else {
                n=print_time_old_ival(idletime(u, tty), 6);
            }
            if (n<0) maxcmd+=(6+n);
        }
        putc(' ',stdout);
        if (new_time){
            print_time_ival(jcpu/HZ, 6, (jcpu%HZ) * 100 / HZ);
        } else {
            print_time_old_ival(jcpu/HZ, 6);
        }
	if (best) {
            int pcpu = best->utime + best->stime;
	    putc(' ', stdout);
            if (new_time){
                n=print_time_ival(pcpu/HZ, 6, (pcpu%HZ) * 100 / HZ );
            } else {
                n=print_time_old_ival(pcpu/HZ, 6);
            }
            if (n<0) maxcmd+=(6+n);
	} else
	    printf("   ?   ");
    } else {
	printf("%-9.8s%-7.7s", u->ut_user, u->ut_line);
	if (from)
	    print_host(u->ut_host, sizeof u->ut_host + 1);
        fputs(" ", stdout);
        if (new_time){
            n=print_time_ival(idletime(u, tty), 6, 0);
        } else {
            n=print_time_old_ival(idletime(u, tty), 6);
        }
        if (n<0) maxcmd+=(6+n);
    }
    fputs(" ", stdout);
    if (best)
	if (best->cmdline)
	    print_strlist(best->cmdline, maxcmd, " ");
	else {
            len= maxcmd-1 > strlen (best->cmd) ? strlen (best->cmd) : maxcmd-1;
	    printf("-%-*.*s", len, len, best->cmd);
        }
    else
	printf("-");
    fputc('\n', stdout);
}

/* 7 character formatted login time */
void print_logintime(time_t logt, FILE* fout) {
    char *weekday[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" },
	 *month  [] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
			"Aug", "Sep", "Oct", "Nov", "Dec" };
    time_t curt;
    struct tm *logtm, *curtm;
    int hour;
    char *merid; /* meridian indicator */

    curt = time(NULL);
    curtm = localtime(&curt);
    logtm = localtime(&logt);
    hour = logtm->tm_hour;
    merid = (hour < 12) ? "am" : "pm";
    if (hour >= 12) hour -= 12;
    if (hour == 0)  hour = 12;
    if (curt - logt > 12*60*60 && logtm->tm_yday != curtm->tm_yday)
	if (curt - logt > 6*24*60*60)
	    fprintf(fout, "%2d%3s%2d", logtm->tm_mday, month[logtm->tm_mon],
		    logtm->tm_year % 100);
	else
	    fprintf(fout, "%3s%2d%s", weekday[logtm->tm_wday], hour, merid);
    else
	fprintf(fout, "%2d:%02d%s", hour, logtm->tm_min, merid);
}

/* This function scans the process table accumulating total cpu times for
   any processes "associated" with this login session.  It also searches
   for the "best" process to report as "(w)hat" the user for that login
   session is doing currently.  This the essential core of 'w'.
 */
proc_t *getproc(utmp_t *u, char *tty, int *jcpu, int *found_utpid) {
    static char line[4];
    proc_t **p, *best = NULL;

    strncpy(line, abbrev_of_tty(tty), sizeof line - 1);
    *jcpu = *found_utpid = 0;
    for (p = procs; *p; p++) {
	if ((**p).pid == u->ut_pid)
	    *found_utpid = 1;
	if (!strcmp((**p).ttyc, line)) {
	    (*jcpu) += (**p).utime + (**p).stime;
	    if ((!ignoreuser || !strncmp((**p).user, u->ut_user, strlen(u->ut_user))) &&
		((**p).pid == (**p).tpgid && (!best || (**p).pid > best->pid)))
		best = *p;
	}
    }
    return best;
}
