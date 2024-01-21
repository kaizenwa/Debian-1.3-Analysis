/*
 * utmp.c	Routines to read/write the utmp and wtmp files.
 *		Contains both the old (buggy) implementation and
 *		the new implementation based on the library functions.
 *
 * Version:	@(#)utmp.c  1.10  16-Apr-1997  miquels@cistron.nl
 *
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/kd.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <termios.h>
#include <utmp.h>
#include <ctype.h>
#include <stdarg.h>
#include <sys/syslog.h>
#include <sys/time.h>

#include "init.h"
#include "initreq.h"
#include "paths.h"

#if defined(__GLIBC__)
#  define HAVE_UPDWTMP 1
#  define USE_OLD_STUFF 0
#  define CLEAN_UTMP 0
#else
#  define HAVE_UPDWTMP 0
#  define USE_OLD_STUFF 0
#  ifdef INIT_MAIN
#    define CLEAN_UTMP 1
#  else
#    define CLEAN_UTMP 0
#  endif
#endif


/*
 *	Log an event ONLY in the wtmp file (reboot, runlevel)
 */
void write_wtmp(
char *user,			/* name of user */
char *id,			/* inittab ID */
int pid,			/* PID of process */
int type,			/* TYPE of entry */
char *line)			/* Which line is this */
{
	int fd;
	struct utmp utmp;

	/*
	 *	Try to open the wtmp file. Note that we even try
	 *	this if we have updwtmp() so we can see if the
	 *	wtmp file is accessible.
	 */
	if ((fd = open(WTMP_FILE, O_WRONLY|O_APPEND)) < 0) return;

#ifdef INIT_MAIN
	/*
	 *	Note if we are going to write a boot record.
	 */
	if (type == BOOT_TIME) wrote_reboot++;

	/*
	 *	See if we need to write a reboot record. The reason that
	 *	we are being so paranoid is that when we first tried to
	 *	write the reboot record, /var was possibly not mounted
	 *	yet. As soon as we can open WTMP we write a delayed boot record.
	 */
	if (wrote_reboot == 0 && type != BOOT_TIME)
  		write_wtmp("reboot", "~~", 0, BOOT_TIME, "~");
#endif

	/*
	 *	Zero the fields and enter new fields.
	 */
	memset(&utmp, 0, sizeof(utmp));
#if defined(__GLIBC__)
	gettimeofday(&utmp.ut_tv, NULL);
#else
	time(&utmp.ut_time);
#endif
	utmp.ut_pid  = pid;
	utmp.ut_type = type;
	strncpy(utmp.ut_name, user, sizeof(utmp.ut_name));
	strncpy(utmp.ut_id  , id  , sizeof(utmp.ut_id  ));
	strncpy(utmp.ut_line, line, sizeof(utmp.ut_line));

#if HAVE_UPDWTMP
	updwtmp(WTMP_FILE, &utmp);
#else
	write(fd, (char *)&utmp, sizeof(utmp));
#endif
	close(fd);
}


#if USE_OLD_STUFF

#if CLEAN_UTMP
/*
 *	Zero out an utmp struct but copy the id field.
 */
static void empty_struct(struct utmp *oldu, struct utmp *newu)
{
	memset(newu, 0, sizeof(struct utmp));
	strncpy(newu->ut_id, oldu->ut_id, sizeof(newu->ut_id));
	strncpy(newu->ut_line, oldu->ut_line, sizeof(newu->ut_line));
	newu->ut_type = DEAD_PROCESS;
}


/*
 *	Find an entry in the utmp file, and rewind the file descriptor
 *	so that we can write there.
 */
static int find_entry(
int fd,				/* utmp file. */
char *user,			/* name of user */
char *id,			/* inittab ID */
int pid,			/* PID of process */
int type,			/* TYPE of entry */
char *line,			/* LINE if used. */
struct utmp *utmp)		/* UTMP entry to be filled in. */
{
	struct utmp tmp;
	struct utmp empty;
	int lineno = 0;
	int found = 0;
	int free_entry = -1;
	int match_entry = -1;
	int n;
#if CLEAN_UTMP
	CHILD *ch;			/* Scratch */
#endif

	while (1) {
		n = read(fd, (char *) &tmp, sizeof(tmp));
		if (n < 0 && errno == EINTR) continue;
		if (n != sizeof(tmp)) break;
#if DEBUG
		log(L_VB, "utmp #%d: pid %d, type %d", lineno / sizeof(tmp),
			tmp.ut_pid, tmp.ut_type);
#endif
		if (user != NULL &&
		    ((tmp.ut_pid == pid && tmp.ut_type != NO_PROCESS) ||
		     (tmp.ut_type == RUN_LVL && type == RUN_LVL))) {
			found = 1;
			memcpy(&utmp, &tmp, sizeof(utmp));
			if (type == DEAD_PROCESS) {
				/*
				 *	Zero all entries with this pid
				 */
				(void) lseek(fd, (long) lineno, SEEK_SET);
				empty_struct(&tmp, &empty);
				write(fd, (char *)&empty, sizeof(struct utmp));
			} else {
				(void) lseek(fd, (long) lineno, SEEK_SET);
				break;
			}
		}
#if CLEAN_UTMP
		/*
		 *	Let's check if this process still exists. If it
		 *	doesn't, clear it's utmp entry.
		 *	FIXME: not race condition free.
		 */
		else if (tmp.ut_type != NO_PROCESS &&
			 tmp.ut_type != RUN_LVL && tmp.ut_pid > 0) {
			/*
			 *	One of our children?
			 */
			for(ch = family; ch; ch = ch->next)
				if (ch->pid == tmp.ut_pid) break;
			if (ch == NULL && kill(tmp.ut_pid, 0) < 0) {
#if DEBUG
				log(L_VB,
				"utmp: cleaning up stale entry (pid %d)",
					tmp.ut_pid);
#endif
				/*
				 *	Zero the entry with this pid
				 */
				(void) lseek(fd, (long) lineno, SEEK_SET);
				empty_struct(&tmp, &empty);
				if (write(fd, (char *)&empty,
					sizeof(struct utmp))
						!= sizeof(struct utmp)) {
						log(L_VB,
						"error writing utmp struct");
						break;
				}
			}
		}
#endif
		/*
		 *	See if this is a unused entry, save it for later
		 *	FIXME: not right, causes races.
		 */
		if ((tmp.ut_pid == 0 && tmp.ut_id[0] == 0
			&& tmp.ut_user[0] == 0 && tmp.ut_type != RUN_LVL)
			|| tmp.ut_type == 0)
				if (free_entry < 0) free_entry = lineno;

		/*
		 *	See if this entry is dead and matches our "id",
		 *	if so save it for later.
		 */
		if (tmp.ut_type == DEAD_PROCESS &&
		    strncmp(tmp.ut_id, id, sizeof(tmp.ut_id)) == 0 &&
		    tmp.ut_user[0] == 0)
			if (match_entry < 0) match_entry = lineno;

		lineno += sizeof(tmp);
	}

#if DEBUG
	log(L_VB, "write_utmp_wtmp(): found=%d, match=%d, free=%d",
		found, match_entry, free_entry);
#endif
	
	if (found)
		return 1;

	if (free_entry < 0 && match_entry < 0)
		return 0;
	
	if (match_entry >= 0)
		(void)lseek(fd, SEEK_SET, match_entry);
	else
		(void)lseek(fd, SEEK_SET, free_entry);

	return 1;
}
#endif /* CLEAN_UTMP */

/*
 *	Log an event into the WTMP and UTMP files.
 *
 *	FIXME: we shouldn't zero out other processes' utmp entries. What we
 *	should do to get/update an utmp entry:
 *	  o see if an entry with matching ut_id field is present
 *	  o if so, re-use it.
 *	  o if not, re-open utmp in O_APPEND mode and add new entry.
 *
 *	Thanks to Michael Haardt for pointing this out.
 *
 *	Also, check if the RUNLEVEL entry is still present and rewrite if
 *	it is missing. (People tend to mess up their utmp files).
 *
 */
void write_utmp_wtmp(
char *user,			/* name of user */
char *id,			/* inittab ID */
int pid,			/* PID of process */
int type,			/* TYPE of entry */
char *line)			/* LINE if used. */
{
	struct utmp utmp;		/* UTMP/WTMP User Accounting */
	int fd = -1;			/* File Descriptor for UTMP */
	int fd2;			/* File Descriptor for WTMP */
	int found = 0;			/* Was the record found in UTMP */
	static int opened = 0;

	/*
	 *	Clear utmp if not yet opened.
	 */
	if (!opened) close(open(UTMP_FILE, O_WRONLY|O_CREAT|O_TRUNC, 0644));
	
	/*
	 *	First read the utmp entry for this process
	 */
	if ((fd = open(UTMP_FILE, O_RDWR)) >= 0) {
#if CLEAN_UTMP
		opened = 1;
		found = find_entry(fd, user, id, pid, type, line, &utmp);
		/*
		 *	Only a call to clean the utmp file?
		 */
		if (user == NULL) {
			(void) close(fd);
			return;
		}
#endif
	}
	
	if (!found) {
		/*
		 *	Enter some defaults
		 */
		memset(&utmp, 0, sizeof(utmp));
		strncpy(utmp.ut_name, user, sizeof(utmp.ut_name));
		strncpy(utmp.ut_id  , id  , sizeof(utmp.ut_id  ));
		strcpy (utmp.ut_line, "");
	}
	
	/*
	 *	Change the values of some fields
	 */
#if defined(__GLIBC__)
	gettimeofday(&utmp.ut_tv, NULL);
#else
	time(&utmp.ut_time);
#endif
	utmp.ut_type = type;
	utmp.ut_pid = pid;
	if (line) strncpy(utmp.ut_line, line, sizeof(utmp.ut_line));
	
	/*
	 *	Write the utmp record, if needed
	 */
	if (fd >= 0)  {
		(void) write(fd, (char *) &utmp, sizeof(struct utmp));
		(void) close(fd);
	}
	
	/*
	 *	Write the wtmp record
	 */
	if ((fd2 = open(WTMP_FILE, O_WRONLY|O_APPEND)) >= 0) {
#ifdef INIT_MAIN
		/* See if we need to write a boot record */
		if (wrote_reboot == 0 && type != BOOT_TIME) {
			write_wtmp("reboot", "~~", 0, BOOT_TIME, "~");
			wrote_reboot++;
		}
#endif
		/*
		 *	Set ut_user to 0 if this is a logout record
		 */
		if (utmp.ut_type == DEAD_PROCESS) utmp.ut_name[0] = 0;
		(void) write(fd2, (char *) &utmp, sizeof(struct utmp));
		(void) close(fd2);
	}
}

#else /* USE_OLD_STUFF */

/*
 *	Proper implementation of utmp handling. This part uses the
 *	library functions for utmp stuff, and doesn't worry about
 *	cleaning up if other programs mess up utmp.
 */
void write_utmp_wtmp(
char *user,			/* name of user */
char *id,			/* inittab ID */
int pid,			/* PID of process */
int type,			/* TYPE of entry */
char *line)			/* LINE if used. */
{
	struct utmp utmp;
	struct utmp tmp;
	struct utmp *utmptr;
	int do_wtmp = 1;
	int fd;

	/*
	 *	For backwards compatibility we just return
	 *	if user == NULL (means : clean up utmp file).
	 */
	if (user == NULL)
		return;

	/*
	 *	Fill out an utmp struct.
	 */
	memset(&utmp, 0, sizeof(utmp));
	utmp.ut_type = type;
	utmp.ut_pid = pid;
	strncpy(utmp.ut_id, id, sizeof(utmp.ut_id));
#if defined(__GLIBC__)
	gettimeofday(&utmp.ut_tv, NULL);
#else
	time(&utmp.ut_time);
#endif
	strncpy(utmp.ut_user, user, UT_NAMESIZE);
	if (line) strncpy(utmp.ut_line, line, UT_LINESIZE);
	
	/*
	 *	We might need to find the existing entry first, to
	 *	find the tty of the process (for wtmp accounting).
	 */
	if (type == DEAD_PROCESS) {
		/*
		 *	Find existing entry for the tty line.
		 */
		do_wtmp = 0;
		setutent();
		tmp = utmp;
		if ((utmptr = getutid(&tmp)) != NULL) {
			do_wtmp = 1;
			strncpy(utmp.ut_line, utmptr->ut_line, UT_LINESIZE);
		}
	}

	/*
	 *	Update existing utmp file.
	 */
	setutent();
	pututline(&utmp);
	endutent();

	/*
	 *	Write the wtmp record if we can open the wtmp file.
	 */
	if ((fd = open(WTMP_FILE, O_WRONLY|O_APPEND)) >= 0) {
#ifdef INIT_MAIN
		/* See if we need to write a boot record */
		if (wrote_reboot == 0 && type != BOOT_TIME) {
			write_wtmp("reboot", "~~", 0, BOOT_TIME, "~");
			wrote_reboot++;
		}
#endif
#if HAVE_UPDWTMP
		updwtmp(WTMP_FILE, &utmp);
#else
		(void) write(fd, (char *) &utmp, sizeof(struct utmp));
#endif
		(void) close(fd);
	}
}

#endif /* USE_OLD_STUFF */
