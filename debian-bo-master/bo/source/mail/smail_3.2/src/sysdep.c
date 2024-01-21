/*
#ident	"@(#)smail/src:RELEASE-3_2:sysdep.c,v 1.48 1996/07/04 17:12:02 woods Exp
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * sysdep.c:
 *	functions which may be operating system dependent.
 *
 *	external functions: time_stamp, get_arpa_date, get_local_year,
 *			    unix_date, compute_local_sender, getfullname,
 *			    fopen_as_user, lock_file, unlock_file,
 *			    compute_hostname, compute_domain,
 *                          open_child, close_child,
 *			    close_all, scan_dir, fcntl_rd_lock, touch,
 *			    fsetsize
 */
#define NEED_SOCKETS
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include "defs.h"
#include <pwd.h>
#include <errno.h>
#include "smail.h"
#include "child.h"
#include "dys.h"
#include "log.h"
#include "exitcodes.h"

#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

#if	defined(UNIX_SYS5) || defined(INCLUDE_SYS_PARAM)
# include <sys/param.h>
#endif

#if defined(UNIX_SYS5) || defined(POSIX_OS) || defined(USE_FCNTL)
# include <fcntl.h>
#else
# if defined(UNIX_BSD)
#  include <sys/file.h>
# endif
#endif

#if defined(POSIX_OS)
# include <limits.h>
# include <unistd.h>
#endif	/* POSIX_OS */

#ifdef	HAVE_UNAME
# include <sys/utsname.h>
#endif

#if     defined(UNIX_BSD) && !defined(POSIX_OS)
# include <sys/dir.h>
#else
# if	defined(UNIX_XENIX) && !defined(POSIX_OS)
#  include <sys/ndir.h>
# else
#  if	defined(HAVE_READDIR)
#   include <dirent.h>
#   define direct dirent
#  else
#   include <sys/dir.h>
#  endif /* HAVE_READDIR */
# endif	/* UNIX_XENIX && !POSIX_OS */
#endif	/* UNIX_BSD && !POSIX_OS */

#if defined(POSIX_OS) || !defined(UNIX_BSD)
# include <time.h>
#else
# include <sys/time.h>
#endif

#if defined(POSIX_OS) || defined(INCLUDE_UTIME_H)
# include <utime.h>
#else
# if defined(DECLARE_UTIMBUF) && !defined(NO_DECLARE_UTIMBUF)
    struct utimbuf {
	time_t actime;
	time_t modtime;
    };
# endif	/* DECLARE_UTIMBUF */
#endif	/* POSIX_OS || INCLUDE_UTIME_H */

#if !defined(UNIX_BSD) && !defined(UNIX_SYS5) && !defined(POSIX_OS)
/* use this for ancient ftime() system call, as a last resort */
# include <sys/timeb.h>
#endif

#if defined(INCLUDE_VFORK_H)
# include <vfork.h>
#endif

#if defined(POSIX_OS) || defined(UNIX_BSD) || defined(WAIT_USE_UNION)
# include <sys/wait.h>
#endif

#if (defined(UNIX_BSD) || defined(WAIT_USE_UNION)) &&!defined(NO_WAIT_USE_UNION)
# define STATUS_TYPE	union wait
#else
# define STATUS_TYPE	int
#endif

#if defined(FORCE_DIRSIZ_VALUE)
#undef DIRSIZ
#define DIRSIZ	FORCE_DIRSIZ_VALUE
#endif

/* functions local to this file */
static char *get_time_zone();
static char *get_time_zone_name();
static void fullname_from_gcos();
static void check_stale();
static int lock_file_by_name();
static void unlock_file_by_name();
static int lockfile_name();

/* imported library functions */
extern off_t lseek();
extern long time();
extern char *getenv();
extern char *getlogin();
extern char *ctime();
#if !defined(USE_TM_TZNAME) && \
    !defined(UNIX_SYS5) && !defined(POSIX_OS) && !defined(USE_TZNAME)
extern char *timezone();			/* only bsd or v6/v7 */
#endif	/* UNIX_SYS5 */
#ifndef ANSI_C
extern struct passwd *getpwnam();
extern struct passwd *getpwuid();
extern struct tm *gmtime();
extern struct tm *localtime();
#endif

/* variables imported from libc */
extern int errno;

/* define these, if they aren't already */
#ifndef O_RDONLY
# define O_RDONLY	0
#endif
#ifndef	O_WRONLY
# define O_WRONLY	1
#endif
#ifndef O_RDWR
# define O_RDWR		2
#endif

/* whence values for lseek(2) */
#ifndef SEEK_SET
# define SEEK_SET	0	/* set file offset to offset */
#endif
#ifndef SEEK_CUR
# define SEEK_CUR	1	/* set file offset to current plus offset */
#endif
#ifndef SEEK_END
# define SEEK_END	2	/* set file offset to EOF plus offset */
#endif


/*
 * time_stamp - return a time stamp string in the form:
 *
 *	mm/dd/yyyy hh:mm:ss
 */
char *
time_stamp()
{
    long sclock = time((long *)0);
    struct tm *ltm = localtime(&sclock);
    static char timebuf[sizeof("mm/dd/yyyy hh:mm:ss")];

    (void) sprintf(timebuf, "%02d/%02d/%04d %02d:%02d:%02d",
		   ltm->tm_mon+1, ltm->tm_mday, ltm->tm_year + 1900,
		   ltm->tm_hour, ltm->tm_min, ltm->tm_sec);
    return timebuf;
}

/*
 * strings used by the date and time routines below
 */
static char *week_day[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
static char *months[] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

/*
 * get_local_year - get the year according to local time
 */
int
get_local_year()
{
	long stamp;		/* local time stamp in seconds form Epoch */

	/* return year */
	stamp = time((long *)0);
	return ((localtime(&stamp))->tm_year + 1900);
}

/*
 * get_arpa_date - return date in RFC822 format
 */
char *
get_arpa_date(clock)
    long clock;
{
    struct tm gtm, *ltm;
    char *tzinfo;
    static char timebuf[sizeof("ddd, dd mmm yyyy hh:mm:ss +tzof (tzn dst) [extra]")];

    /* 
     * We need to do this since most implementations will use the same static
     * buffer for both localtime and gmtime 
     */
    ltm = gmtime(&clock);
    (void) memcpy(&gtm, ltm, sizeof(gtm));
    ltm = localtime(&clock);
    tzinfo = get_time_zone(&gtm, ltm);

    (void) sprintf(timebuf, "%s, %d %s %04d %02d:%02d:%02d %s",
		   week_day[ltm->tm_wday], ltm->tm_mday,
		   months[ltm->tm_mon], ltm->tm_year + 1900,
		   ltm->tm_hour, ltm->tm_min, ltm->tm_sec,
		   tzinfo);
    return timebuf;
}

/*
 * get_time_zone - get the current timezone
 */
/* ARGSUSED */
static char *
get_time_zone(gtm, ltm)
    struct tm *gtm, *ltm;
{
    int tzoff, tzoff_hour, tzoff_min;
    char sign;
    char * ltzname = NULL;
    static char tzbuf[20];

    /* This code works on all systems to get timezone */
    tzoff = ((ltm->tm_min - gtm->tm_min) +
	          (60 * (ltm->tm_hour - gtm->tm_hour)));
    if (ltm->tm_year != gtm->tm_year) {
	tzoff +=  (ltm->tm_year > gtm->tm_year) ? 1440 : -1440;
    } else {
	if (ltm->tm_yday != gtm->tm_yday) {
	        tzoff +=  (ltm->tm_yday > gtm->tm_yday) ? 1440 : -1440;
	    }
    }

    if (tzoff >= 0) {
	sign = '+';
    } else {
	sign = '-';
	tzoff = -tzoff;
    }
    tzoff_min = tzoff % 60;
    tzoff_hour = tzoff / 60;

    ltzname = get_time_zone_name(ltm);
    if (ltzname) {
	sprintf(tzbuf, "%c%02d%02d (%s)", sign, tzoff_hour, tzoff_min, ltzname);
    } else {
	sprintf(tzbuf, "%c%02d%02d", sign, tzoff_hour, tzoff_min);
    }
    return (tzbuf);
}


static char *
get_time_zone_name(ltm)
struct tm *ltm;
{
#if	defined(USE_TM_TZNAME)
    return ltm->tm_zone;
#else	/* not USE_TM_TZNAME */
# if defined(UNIX_SYS5) || defined(POSIX_OS) || defined(USE_TZNAME)
    extern char *tzname[2];

    return tzname[ltm->tm_isdst];
# else	/* not UNIX_SYS5, not POSIX_OS, and not USE_TZNAME */
#  if	(defined(UNIX_BSD) || defined(USE_GETTIMEOFDAY)) 
    struct timeval tv;
    struct timezone tz;

    (void) gettimeofday(&tv, &tz);
    return timezone(tz.tz_minuteswest, ltm->tm_isdst);
#   else	/* (!UNIX_BSD || !USE_GETTIMEOFDAY) */
    struct timeb timeb;

    /* try the V6/V7 system call for getting the time zone */
    (void) ftime(&timeb);
    return timezone(timeb.timezone, ltm->tm_isdst);
#  endif	/* not UNIX_BSD */
# endif	/* not UNIX_SYS5 */
#endif	/* USE_TM_TZNAME */
}

/*
 * unix_date - get the current date as suitable for a From_ line
 *
 * Use ctime format.
 */
char *
unix_date()
{
    long uxclock = time((long *)0);
    char *date = ctime(&uxclock);

    /*
     * XXX - ctime format is quite standard, so just but the nul in
     * the proper spot.
     */
    date[24] = '\0';			/* get rid of that \n */
    return date;
}

/*
 * compute_local_sender - compute the user running this program.
 */
void
compute_local_sender()
{
    struct passwd *pw;

    if (local_sender)
	return;

    islocal = TRUE;			/* only local sender determined here */

    if (sender_env_variable) {
	char *p = getenv(sender_env_variable);

	if (p && *p) {
	    pw = getpwnam(p);
	    if (pw != NULL && pw->pw_uid == real_uid) {
		local_sender = xmalloc(strlen(p) + 1);
		(void) strcpy(local_sender, p);
		return;
	    }
	}
    }

    if ((pw = getpwuid(real_uid)) != NULL) {
	local_sender = xmalloc((unsigned)(strlen(pw->pw_name) + 1));
	(void) strcpy(local_sender, pw->pw_name);
	return;
    }

    local_sender = "postmaster";
    return;
}

/*
 * getfullname - get the full name of the sender
 *
 * The full name comes from the GCOS field in the password entry.
 */
void
getfullname()
{
    struct passwd *pw;
    struct passwd *getpwnam();	/* get a password entry given a username */

    if (islocal) {
	/* only possible for local senders */
	pw = getpwnam(sender);
	if (pw) {
	    fullname_from_gcos(pw->pw_gecos);
	}
    }
    return;
}

/*
 * fullname_from_gcos - fill in sender_name from the given gcos field
 *
 * (Modified to strip 0000-name(0000) USG junk - hargen@pdn 8/20/88
 */
static void
fullname_from_gcos(gecos)
    register char *gecos;
{
    struct str str;
    register struct str *sp = &str;	/* put full name here */
    char *cp;

    STR_INIT(sp);
    if (isdigit(*gecos) && (cp = index(gecos, '-')) != NULL)
	gecos = cp + 1;			/* skip USG-style 0000-Name junk */
    while (*gecos && *gecos != ',' && *gecos != '(') {
	/* name may end with a comma or USG-style (0000) junk */
	if (*gecos == '&') {		/* & means copy sender, capitalized */
	    STR_NEXT(sp, uppercase(sender[0]));
	    STR_CAT(sp, sender+1);
	} else {
	    STR_NEXT(sp, *gecos);
	}
	gecos++;
    }
    STR_NEXT(sp, '\0');
    STR_DONE(sp);
    sender_name = sp->p;
}

/*
 * fopen_as_user - call fopen(3s) within the context of a specific uid/gid
 *
 * given a uid and gid, fopen a file only if the given uid/gid would be
 * able to do so.  Also, for "a" and "w" types:  if the file did not
 * previously exist, the file will be owned by the uid/gid after having
 * being opened (if the mailer is running as root).
 *
 * return value and errno are set as a call to fopen(3s) would set them.
 *
 * NOTE: the mode argument is only required if type is "a" or "w".
 */
/*VARARGS5*//*ARGSUSED*/
FILE *
fopen_as_user(fn, type, uid, gid, mode)
    char *fn;				/* name of file to open */
    char *type;				/* mode for fopen ("r", "w" or "a") */
    int uid;				/* user id */
    int gid;				/* group id */
    int mode;				/* mode for creat() */
{
    FILE *f;				/* opened file */
#if (defined(UNIX_BSD) || defined(HAVE_SETEUID)) && !defined(NO_HAVE_SETEUID)
    int fd;				/* file descriptor for opened file */
#else /* NOT (defined(UNIX_BSD) || defined(HAVE_SETEUID)) */
    int fd;				/* file descriptor for opened file */
    int pid;				/* PID of forked process */
    int i;				/* Used to iternate through processes */
    STATUS_TYPE status;			/* status of exited forked process */
    int fs;				/* temp status variable */
    struct stat stb_root, stb_user;	/* stat of file in root/user contexts */
#endif /* NOT (defined(UNIX_BSD) || defined(HAVE_SETEUID)) */

    /* reject non-root-based paths out of hand, though we shouldn't get any */
    if (fn[0] != '/') {
	write_log(LOG_SYS|LOG_PANIC,
		  "fopen_as_user: given non-root based path: %s",
		  fn);
	errno = EACCES;
	return NULL;
    }

#if (defined(UNIX_BSD) || defined(HAVE_SETEUID)) && !defined(NO_HAVE_SETEUID)
    /*
     * Under BSD, it is easy if we are running as root.  Just set the
     * effective ids to the passed uid and gid and call fopen.  Then
     * set the effective ids back to root.  Having independent seteuid
     * and setruid calls is great.
     *
     * NOTE:  we assume that setgroups(0, (int *)NULL) has been called
     *	      to clear out any groups that may erroneously allow access
     *	      to the file.
     */
    /* W A R N I N G ! ! !
     *
     * This is rather dangerous code, as it permitteds a process
     * dive into user land and then back to being super-user.
     */
    if (geteuid() == 0) {
	int save_gid = getegid();

	/* order here is, oddly enough, mildly important */
	(void) setegid(gid);
	(void) seteuid(uid);

	if (type[0] == 'a') {
	    fd = open(fn, O_WRONLY|O_APPEND|O_CREAT, mode);
	    if (fd < 0) {
		f = NULL;
	    } else {
		f = fdopen(fd, type);
	    }
	} else if (type[0] == 'w') {
	    fd = open(fn, O_WRONLY|O_CREAT|O_TRUNC, mode);
	    if (fd < 0) {
		f = NULL;
	    } else {
		f = fdopen(fd, type);
	    }
	} else {
	    f = fopen(fn, type);
	}

	/* and order is important, again, here */
	(void) seteuid(0);
	(void) setegid(save_gid);

	return f;
    } else
	    return fopen(fn, type);
#else /* NOT (defined(UNIX_BSD) || defined(HAVE_SETEUID)) */
    /*
     * now wasn't that nice and simple?  Well, now we get to do
     * it the hard way!
     *
     */
    /* First see if writing, whether the file already exists */
    /* if (((type[0] == 'w') || (type[0] == 'a')) && (access(fn, 0) == 0)) {
     * substitute with the following two lines by
     * SL <Soenke@escher.north.de>, as proposed by 
     * Eugene Crosser <crosser@pccross.msk.su>:
	I hope that now I've found the cause of appendfile failing when 
	HAVE_SETEUID is not set.  src/sysdep.c:516 reads as following:

	if (((type[0] == 'w') || (type[0] == 'a')) && (access(fn, 0) == 0)) {  

	So, if (access(fn,F_OK) == 0) this is interpreted as the file does 
	*not* exist and must be created.  This is not correct.  Linux manual
 	is unclear about F_OK function, but Solaris manual clearly says that 
	access(...,F_OK) returns -1 if the file does not exist, and sent 
	errno to ENOENT.  This is exactly how both Linux and Solaris access() 
	works.  With the following diff, Smail starts working fine while 
	creating new mailboxes: 
     */
    if (((type[0] == 'w') || (type[0] == 'a')) &&
       (access(fn, 0) == -1) && (errno == ENOENT)) {
	/* We need to fork here to create the file */
	if ((pid = fork()) == -1) 
	    return NULL;
	else if (pid == 0) {
	    if (setgid(gid) != 0) _exit(1);
	    if (setuid(uid) != 0) _exit(1);
	    if ((fd = open(fn, O_WRONLY|O_CREAT, mode)) < 0) _exit(1);
	    close(fd);
	    _exit(0);
	} else {
	    /* wait for the child process to die */
	    while ((i = wait((STATUS_TYPE *)&status)) != pid && i >= 0)
		;

	    if ((i < 0) || (((int) status) != 0)) {
		/* failed to find the child process or abnormal exit */
		return NULL;
	    }
	}
    }

    if (type[0] == 'w') {
	/* No truncate flag here otherwise we'd damage a file before checking access */
	fd = open(fn, O_WRONLY, mode);
    } else if (type[0] == 'a') {
	fd = open(fn, O_WRONLY|O_APPEND, mode);
    } else {
	fd = open(fn, O_RDONLY, mode);
    }

    if (fd < 0) {
	return(NULL);
    }

    if ((fstat(fd, &stb_root) != 0) ||
	((pid = fork()) == -1)) {
	close(fd);
	return NULL;
    } else if (pid == 0) {
	if (setgid(gid) != 0) {
	    _exit(1);
	}
	if (setuid(uid) != 0) {
	    _exit(1);
	}

	if (type[0] == 'w') {
	    /* NB Truncate here to prevent people being able to wipe files */
	    fd = open(fn, O_WRONLY|O_TRUNC, mode);
	} else if (type[0] == 'a') {
	    fd = open(fn, O_WRONLY|O_APPEND, mode);
	} else {
	    fd = open(fn, O_RDONLY, mode);
	}

	if (fd < 0) {
	    _exit(1);
	}

	fs = fstat(fd, &stb_user);
	close(fd);
	if ((fs < 0) || 
	    (stb_root.st_dev != stb_user.st_dev) ||
	    (stb_root.st_ino != stb_user.st_ino)) {
	    _exit(1);
	} else {
	    _exit(0);
	}
    } else {
	/* wait for the child process to die */
	while ((i = wait((STATUS_TYPE *)&status)) != pid && i >= 0) ;

	if ((i < 0) || (((int) status) != 0)) {
	    /* failed to find the child process or abnormal exit */
	    close(fd);
	    return NULL;
	}
	return(fdopen(fd, type));
    }
#endif /* NOT (defined(UNIX_BSD) || defined(HAVE_SETEUID)) */
}

/*
 * lock_file - lock a user's mailbox or other mail file
 *
 * return SUCCEED or FAIL.
 */
#ifdef	lock_fd_wait
/*
 * under 4.3BSD and some 4.2+ operating systems, flock(2) is used
 * to lock mailboxes or other mail files.  FLOCK_MAILBOX should
 * be defined in os.h in this case.  We assume that the file
 * descriptor points to the beginning of the file.
 */
int
lock_file(fn, f)
    char *fn;				/* name of file */
    FILE *f;				/* open file */
{
    off_t offset;
    int success;

    if (! flock_mailbox) {
	/* use filename-based locking for mailbox files */
	return lock_file_by_name(fn);
    }

    offset = lseek(fileno(f), (off_t) 0L, SEEK_CUR);
    (void) lseek(fileno(f), (off_t) 0L, SEEK_SET);
    success = lock_fd_wait(fileno(f));
    (void) lseek(fileno(f), offset, SEEK_SET);
    if (success < 0) {
	/*
	 * This should never fail, but if it does, we will retry delivery
	 * at a later time.
	 */
	return FAIL;
    }
    return SUCCEED;
}

#else	/* not lock_fd_wait */

/*
 * if the lock_fd_wait macro is not available then we must always
 * use the V6-style file-locking.
 */
/*ARGSUSED*/
int
lock_file(fn, f)
    char *fn;				/* name of file */
    FILE *f;				/* open file */
{
    return lock_file_by_name(fn);
}

#endif	/* not lock_fd_wait */

/*
 * V7-style or Xenix-style locking.
 * As far as I know, all flavors of UN*X that do not use 4.3BSD style
 * locking use this method.
 *
 * To lock a file named fn, create a file named fn.lock (or for Xenix,
 * /tmp/basename.mlk) and stuff the pid into it.  If the file already
 * exisits, see if it is stale and remove it if so.  If it was stale,
 * sleep for FNLOCK_INTERVAL and try to lock it again.  Retry at most
 * FNLOCK_RETRIES times.  On non-BSD systems, locking will not be done
 * if the basename for the file is more than 12 chars.  The lenth
 * restriction does not apply to Xenix, because the basename is always
 * truncated to 10 chars, allowing sufficient space for the .mlk
 * suffix.
 *
 * on systems without O_EXCL for the open(2) call, creat is used
 * with a mode that does not allow writing.
 */
static int
lock_file_by_name(fn)
    char *fn;				/* name of file */
{
    int l_fd;				/* open lock file */
    char *l_fn;				/* lock file name */
    int retries;			/* remaining retries */
    char apid[BITS_PER_INT/3 + 1];	/* ASCII representation of pid */

    /* generate lockfile name */
    if (lockfile_name(fn, &l_fn) == FAIL)
	return FAIL;

    /* if no lockfile required, that's it */
    if (l_fn == NULL)
	return SUCCEED;

    /*
     * try to create the lock file at most FNLOCK_RETRIES+1 times.
     * each time a lock fails, read a pid from the lock file and try
     * to remove the lock if that pid does not exist then sleep for
     * FNLOCK_INTERVAL and try again.
     */
    for (retries = fnlock_retries; retries-- >= 0; sleep(fnlock_interval)) {
#ifdef	O_EXCL
	l_fd = open(l_fn, O_WRONLY|O_CREAT|O_EXCL, fnlock_mode);
#else	/* O_EXCL */
	/*
	 * if we can't open for exclusive creat, we will have to
	 * use the creat call.
	 */
	l_fd = creat(l_fn, fnlock_mode & (~0222));
#endif	/* O_EXCL */
	if (l_fd < 0) {
	    /*
	     * examine why this failed
	     */
#ifdef	O_EXCL
	    /*
	     * open with O_EXCL returns this error if file exists
	     */
	    if (errno == EEXIST) {
		check_stale(l_fn);	/* remove stale lock files */
		continue;
	    }
#else	/* O_EXCL */
	    /*
	     * creat returns the ambiguous EACCES if the file exists
	     * and is not writable (the correct condition for a lock)
	     */
	    if (errno == EACCES) {
		check_stale(l_fn);
		continue;
	    }
#endif	/* O_EXCL */
	    /*
	     * some other reason is preventing us from writing
	     * the lock file, thus we won't bother retrying.
	     */
	    xfree(l_fn);
#ifdef	UNIX_BSD
	    if (errno == ENOENT) {
		/*
		 * this is what BSD seems to return on basename too
		 * long, so return SUCCEED in this case because
		 * locking is not possible.  Unfortunately this
		 * error does not uniquely identify the problem.
		 */
		return SUCCEED;
	    }
#endif	/* UNIX_BSD */
	    /*
	     * for anything else, something strange is preventing us
	     * creating the lock file.  FAIL for now, we will try
	     * again later.
	     */
	    return FAIL;
	}
	break;
    }
    xfree(l_fn);			/* don't need this any more */

    /*
     * none of the attempts to create the lock file succeeded
     */
    if (l_fd < 0) {
	return FAIL;
    }
    (void) sprintf(apid, "%d", getpid());
    (void) write(l_fd, apid, strlen(apid));
    (void) close(l_fd);

    return SUCCEED;
}

/*
 * check_stale - see if a lock file is stale.  If so, remove it.
 */
static void
check_stale(l_fn)
    char *l_fn;				/* name of lock file */
{
    char buf[50];
    int ct;
    int fd;
    int pid = 0;
    struct stat statbuf;

    fd = open(l_fn, O_RDONLY);
    if (fd < 0) {
	DEBUG2(DBG_DRIVER_MID, "%s: cannot open lock file: %s",
	       l_fn, strerror(errno));
	return;				/* doesn't exist? */
    }

    for (;;) {
	ct = read(fd, buf, sizeof(buf) - 1);
	if (ct == 0) {
	    DEBUG1(DBG_DRIVER_MID, "%s: zero-length lock file ... ", l_fn);

	    /* sleep 30 seconds and see if new data comes in */
	    (void) sleep(30);

	    /* fine if it went away */
	    if ((fstat(fd, &statbuf) < 0) || (statbuf.st_nlink == 0)) {
		DEBUG(DBG_DRIVER_MID, "lock file went away\n");
		(void) close(fd);
		return;
	    }

	    /* stale if still empty */
	    if (statbuf.st_size == 0) {
		if (statbuf.st_nlink > 0) {
		    /* unlink if it still exists */
		    (void) unlink(l_fn);
		}
		DEBUG(DBG_DRIVER_MID, "still empty, removed\n");
		(void) close(fd);
		return;
	    }
	}
	else {
#ifdef UNIX_XENIX
	    /* For Xenix, simple existence is quite enough. */
	    DEBUG1(DBG_DRIVER_MID, "%s: valid lock file\n", l_fn);
#else  /* !Xenix */
	    buf[ct] = '\0';
	    (void) sscanf(buf, "%d", &pid);
	    /* see if the pid exists */
	    if ((kill(pid, 0) < 0) && (errno == ESRCH)) {
		/* process does not exist, wait 5 seconds before removing */
		(void) sleep(5);
		if ((fstat(fd, &statbuf) == 0) && (statbuf.st_nlink > 0)) {
		    /* remove the file if it still exists */
		    (void) unlink(l_fn);
		    DEBUG1(DBG_DRIVER_MID, "%s: stale lock file, removed\n",
			   l_fn);
		} else {
		    DEBUG1(DBG_DRIVER_MID, "%s: stale lock file vanished\n",
			   l_fn);
		}
	    } else {
		DEBUG1(DBG_DRIVER_MID, "%s: valid lock file\n", l_fn);
	    }
#endif  /* !Xenix */
	    (void) close(fd);
	    return;
	}
    }
}

/*
 * unlock_file - unlock a user's mailbox or other mail file
 *
 * we do not check to make sure we unlocked the file.  What
 * could we do if an unlock failed?
 */
#ifdef	lock_fd_wait
/*
 * for 4.3BSD-style locking, just call flock to unlock the file
 */
void
unlock_file(fn, f)
    char *fn;				/* name of file */
    FILE *f;				/* open file */
{
    off_t offset;

    if (! flock_mailbox) {
	/* use the V6 locking protocol */
	unlock_file_by_name(fn);
	return;
    }
    offset = lseek(fileno(f), (off_t) 0L, SEEK_CUR);
    (void) lseek(fileno(f), (off_t) 0L, SEEK_SET); /* unlock from beginning to end */
    unlock_fd_wait(fileno(f));
    (void) lseek(fileno(f), offset, SEEK_SET);
}
#else	/* not lock_fd_wait */
/*
 * flock not available, so always call the V6-style unlock function.
 */
/*ARGSUSED*/
void
unlock_file(fn, f)
    char *fn;				/* name of file */
    FILE *f;				/* open file */
{
    unlock_file_by_name(fn);
}
#endif	/* not lock_fd_wait */
/*
 * for V6-style locking remove the lock file.  Be careful to
 * make sure that the name passed to unlink(2) won't unlink
 * the main file (i.e., use the same checks for basename size
 * used in creating the lock file)
 */
static void
unlock_file_by_name(fn)
    char *fn;				/* name of file */
{
    char *l_fn;				/* lock file name */

    /* generate lockfile name */
    if (lockfile_name(fn, &l_fn) == FAIL)
	return;

    /* if no lockfile required, that's it */
    if (l_fn == NULL)
	return;

    /*
     * remove the lock file and clean up
     */
    (void) unlink(l_fn);
    xfree(l_fn);
}

/*
 * generate a mailbox lockfile name into the given char **plfn.
 * if returned pointer is NULL, no lockfile is required.
 * return SUCCEED or FAIL.
 */
static int
lockfile_name(fn, plfn)
    char *fn;
    char **plfn;
{
    char *base;

    if (fn[0] != '/') {
	/*
	 * there should not be a way for the software to generate
	 * a filename that does not begin with /
	 */
	panic(EX_SOFTWARE, "lock_by_name: non-rooted filename: %s", fn);
	/*NOTREACHED*/
    }

    /* find basename */
    base = rindex(fn, '/') + 1;
#ifdef lint
    base = base + 1;				/* lint says not used (SunOS) */
#endif

    /* assume that no lockfile is required */
    *plfn = NULL;

#ifdef UNIX_XENIX

    *plfn = xmalloc(sizeof("/tmp/.mlk") + 10);
    (void) sprintf(*plfn, "/tmp/%.10s.mlk", base);
    return SUCCEED;

#else /* not UNIX_XENIX */

    /*
     * will it be possible to create the lock file?  On BSD systems,
     * the open call will tell us if the filename is too long.  On
     * other systems, we need to check this ourselves before hand.
     *
     * On POSIX systems, the only way to know whether truncation
     * would occur is to call pathconf().
     */
# if defined(POSIX_OS)
    {
	int no_trunc_flag;
	int name_max;
	char *p;

	if ((int)strlen(base) > _POSIX_NAME_MAX - 2) {
	    char *dir;

	    p = base;
	    while (p > fn && *(p - 1) == '/')
		--p;
	    dir = xmalloc((p - fn) + 1);
	    (void) memcpy(dir, fn, p - fn);
	    dir[p - fn] = '\0';
	    no_trunc_flag = pathconf(dir, _PC_NO_TRUNC);
	    if (! no_trunc_flag) {
		name_max = pathconf(dir, _PC_NAME_MAX);
	    }
	    xfree(dir);

	    if (no_trunc_flag < 0 || name_max < 0)
		return FAIL;
	    if (! no_trunc_flag && (int)strlen(base) > name_max - 2) {
		/*
		 * If there is not enough room to append a .l, then
		 * always succeed the lock operation.  This is dangerous
		 * but there is probably nothing better to do.
		 */
		return SUCCEED;
	    }
	}
    }

# else /* not POSIX_OS */
#  if !defined(UNIX_BSD) && !defined(UNIX_XENIX)

    /* allow at least enough room for a trailing .l */
    if (strlen(base) > DIRSIZ - 2) {
	/*
	 * always succeed
	 */
	return SUCCEED;
    }

#  endif /* not UNIX_BSD and not UNIX_XENIX */
# endif /* POSIX_OS */

    /* generate the lock filename */

    *plfn = xmalloc(strlen(fn) + sizeof(".lock"));
    (void) sprintf(*plfn, "%s.lock", fn);
    return SUCCEED;

#endif /* not UNIX_XENIX */
}

/*
 * compute_hostname - compute the name of the local host
 *
 * return NULL if we are on an operating system that does not know about
 * hostnames.
 */
#ifdef	HAVE_GETHOSTNAME
char *
compute_hostname()
{
    static char *hostname;
# ifdef	GETHOSTNAME_USE_PTR
    int len;
# endif

    /*
     * My man page says that 255 chars (plus nul byte) is the limit
     * on length of the local host name.  There appears to be no
     * #define for it in 4.2BSD.
     */
    hostname = xmalloc(256);
# ifdef GETHOSTNAME_USE_PTR
    len = 256;
    (void) gethostname(hostname, &len);
# else
    (void) gethostname(hostname, 256);
# endif

    hostname = xrealloc(hostname, strlen(hostname) + 1);

    return hostname;
}
#else	/* not HAVE_GETHOSTNAME */

# ifdef	HAVE_UNAME
char *
compute_hostname()
{
    static struct utsname utsname;

    (void) uname(&utsname);

    /* Is the sysname tag used for something interesting? */
    return utsname.nodename;
}
# else	/* not HAVE_UNAME */
#  ifdef SITENAME_FILE
/* sitename is stored in a file */
char *
compute_hostname()
{
    static struct str hostname;
    static int inited = FALSE;
    register int c;
    FILE *f;

    if (inited) {
	return hostname.p;
    }
    inited = TRUE;
    STR_INIT(&hostname);

    f = fopen(SITENAME_FILE, "r");
    while ((c = getc(f)) != EOF && c != '\n') {
	STR_NEXT(&hostname, c);
    }
    STR_NEXT(&hostname, '\0');
    STR_DONE(&hostname);
    return hostname.p;
}
#  else	/* not SITENAME_FILE */
char *
compute_hostname()
{
    /*
     * perhaps we should call uuname -l rather than punting.
     */
    panic(EX_SOFTWARE,
	  "the local host name is not computable and is not configured");
    /*NOTREACHED*/
}
#  endif /* not SITENAME_FILE */
# endif	/* not HAVE_UNAME */
#endif	/* not HAVE_GETHOSTNAME */



char *
compute_domain ( hostname )
     char * hostname;
{
    char * s = NULL;
    char * p;
#ifndef NO_USEGETHOSTBYNAME
    struct hostent *host_info;
    char * * ptr;
#endif /* NO_USEGETHOSTBYNAME */

#ifdef HAVE_BIND
    s = bind_compute_domain();
#endif /* HAVE_BIND */

#ifndef NO_USEGETHOSTBYNAME
    if (!s || !*s) {
	if (host_info = gethostbyname(hostname)) {
	    /* find the first alias with a '.' in it */
	    for (ptr = host_info->h_aliases, p = *ptr; (*ptr); p = *++ptr) {
		if ((s = index(*ptr, '.')) && 
		    (*s++ = '\0', EQIC(*ptr, hostname))) {
		    s = COPY_STRING(p);
		    break;
		}
	    }
	}
    }
#endif /* NO_USEGETHOSTBYNAME */

    /* Next bit of code removes trailing dots */
    if (s && *s) {
	p = s + strlen(s) - 1;
	while (*p == '.') {
	    *p-- = '\0';
	}
	return(s);
    }
    return (NULL);
}

/*
 * open_child - create a child process and open pipes to it and exec
 *
 * open_child creates a child process, with possible read and write
 * pipes to the child process's stdin and stdout/stderr.  Setuid and
 * setgid are called in the child process to change the uid/gid to
 * whichever uid/gid are given to open_child.
 *
 * open_child does nothing with signals in the parent process.
 * Only signal behaviors modified by exec will be changed in the
 * child process.  If more complex signal behavior desired, call
 * with argv == NULL and handle signals and the exec in the caller
 * routine within the child process.
 *
 * inputs:
 *	argv	- a vector suitable for passing to execve.  argv[0] is
 *		  given as the program to exec.  If argv is NULL, then
 *		  do a return instead of an exec.  In this case as well,
 *		  never to a vfork(), always use fork().
 *	envp	- a vector of environment variables suitable for passing
 *		  to execve.  If envp is null and CHILD_MINENV is not
 *		  set in flags, then the parents environment will be
 *		  passed to the child.
 *	in	- a pointer to a FILE* variable.  If non-NULL, a pipe
 *		  is created, with the write-end returned in this
 *		  variable and with the read-end tied to the stdin of
 *		  the child process.
 *	out	- a pointer to a FILE* variable.  If non-NULL, a pipe
 *		  is created, with thre read-end returned in this
 *		  variable and with the write-end tied to the stdout
 *		  of the child process.
 *	errfd	- if errfd >= 0 then it sepecifies a file descriptor
 *		  to duplicate to the stdout and/or stderr of the
 *		  child.  If out != NULL, errfd is only dup'd to the
 *		  stderr.
 *	flags	- a bitmask of flags affecting open_child's operation.
 *		  Flags are:
 *
 *		CHILD_DUPERR	- duplicate stdout to stderr in the
 *				  child process.
 *		CHILD_DEVNULL	- open "/dev/null" and associate it
 *				  with stdin and/or stdout in the
 *				  child process, if no in and/or out
 *				  variable is specified.
 *		CHILD_RETRY	- retry fork() operation up to FORK_RETRIES
 *				  times at FORK_INTERVAL second intervals,
 *				  if fork returns EAGAIN.
 *		CHILD_MINENV	- give the child process a very minimal
 *				  environment.  This is overridden by giving
 *				  an explicit environment in envp.
 *		CHILD_NOCLOSE	- don't close file descriptors.  If this is
 *				  not set, close all file descriptors
 *				  other than stdin/stdout/stderr.
 *
 *	uid	- do a setuid(uid) in the child
 *	gid	- do a setgid(gid) in the child
 *
 * output:
 *	pid of child process, or 0 if in child process (only if argv==NULL).
 *	Return EOF if pipe() or fork() failed.
 */
int
open_child(argv, envp, in, out, errfd, flags, uid, gid)
    char **argv;			/* arg vector for execve */
    char **envp;			/* environment vector for execve */
    FILE **in;				/* make a stdin file pointer */
    FILE **out;				/* make a stdout file pointer */
    int errfd;				/* fd to use for stdout/stderr */
    int flags;				/* flags affecting operation */
    int uid;				/* user ID for child process */
    int gid;				/* group ID for child process */
{
    int stdin_fd[2];			/* fds from pipe(2) for child stdin */
    int stdout_fd[2];			/* fds from pipe(2) for child stdout */
    /* remember, fd[0] is the read descriptor, fd[1] is the write descriptor */
    int retries = FORK_RETRIES;		/* countdown of retries */
    int pid;				/* pid from fork() */
    static char *min_env[] = {
#ifdef	CHILD_ENVIRON
	CHILD_ENVIRON,
#else	/* CHILD_ENVIRON */
	"HOME=/",
	NULL,				/* leave space for path */
	NULL,				/* leave space for shell */
#endif	/* CHILD_ENVIRON */
	NULL,				/* leave space for timezone */
	NULL,				/* end of environment */
    };
    static char **minp = NULL;

    if (in && pipe(stdin_fd) < 0) {
	return EOF;
    }
    if (out && pipe(stdout_fd) < 0) {
	/* free resources */
	if (in) {
	    (void) close(stdin_fd[0]);
	    (void) close(stdin_fd[1]);
	}
	return EOF;
    }

    /* keep trying to create a fork until we succeed or retries == 0 */
    while (
#if	(defined(UNIX_BSD) || defined(HAVE_VFORK)) && !defined(DONT_USE_VFORK)
	   (pid = (argv? vfork(): fork())) < 0 &&
#else	/* not UNIX_BSD && not HAVE_VFORK */
	   (pid = fork()) < 0 &&
#endif	/* UNIX_BSD || HAVE_VFORK */
	   errno == EAGAIN &&
	   --retries >= 0)
    {
	(void) sleep(FORK_INTERVAL);	/* sleep between retries */
    }
    if (pid < 0) {
	/* free resources */
	if (in) {
	    (void) close(stdin_fd[0]);
	    (void) close(stdin_fd[1]);
	}
	if (out) {
	    (void) close(stdout_fd[0]);
	    (void) close(stdout_fd[1]);
	}
	return EOF;
    }

    if (pid == 0) {
	/* in child process */

	/* close unnecessary file descriptors */
	if (in) {
	    (void) close(stdin_fd[1]);
	}
	if (out) {
	    (void) close(stdout_fd[0]);
	}

	/* if errfd == 0, watch out for the code below */
	if (errfd == 0 || errfd == 1) {
	    int new_errfd;

	    /* search for a descriptor we don't care about */
	    for (new_errfd = 3; new_errfd < 10; new_errfd++) {
		if ((in && new_errfd == stdin_fd[0]) ||
		    (out && new_errfd == stdout_fd[1]))
		{
		    continue;
		}
		break;
	    }
	    (void) dup2(errfd, new_errfd);
	    (void) close(errfd);
	    errfd = new_errfd;
	}
	if (in || (flags & CHILD_DEVNULL)) {
	    /*
	     * setup the child's stdin
	     */
	    if (in) {
		/* pipe from parent process */
		if (stdin_fd[0] != 0) {
		    (void) dup2(stdin_fd[0], 0);
		    (void) close(stdin_fd[0]);
		}
	    } else if (flags & CHILD_DEVNULL) {
		/*
		 * XXX - we rely on pipe() never returning 0 as the
		 *	 write file descriptor, or this could close
		 *	 stdout_fd[1], which would not be nice.
		 */
		/* open /dev/null as the stdin */
		(void) close(0);
		if (open("/dev/null", O_RDONLY) < 0) {
		    /* uggh! failed to open /dev/null, quit */
		    _exit(EX_OSFILE);
		}
	    }
	}

	if (out || errfd >= 0 || (flags & CHILD_DEVNULL)) {
	    /*
	     * setup the child's stdout
	     */
	    if (out) {
		/* pipe to parent process */
		if (stdout_fd[1] != 1) {
		    (void) dup2(stdout_fd[1], 1);
		    (void) close(stdout_fd[1]);
		}
	    } else if (errfd >= 0) {
		/* use the given fd for stdout */
		(void) dup2(errfd, 1);
	    } else if (flags & CHILD_DEVNULL) {
		/* open /dev/null as stdout */
		(void) close(1);
		if (open("/dev/null", O_WRONLY) < 0) {
		    /* uggh! failed to open /dev/null, quit */
		    _exit(EX_OSFILE);
		}
	    }
	}

	if (errfd >= 0) {
	    /* use this fd as the stderr */
	    if (errfd != 2) {
		(void) dup2(errfd, 2);
		(void) close(errfd);
	    }
	} else if (flags & CHILD_DUPERR) {
	    /* duplicate stdout to get the stderr */
	    (void) dup2(1, 2);
	} else {
	    (void) close(2);
	    if (open("/dev/null", O_WRONLY) < 0) {
		/* uggh! failed to open /dev/null, quit */
		_exit(EX_OSFILE);
	    }
	}

	/* close all unnecessary file descriptors */
	if ( !(flags & CHILD_NOCLOSE) ) {
	    close_all();
	}

	/* change uid and gid, if we can, don't bother to check */
	(void) setgid(gid);
	(void) setuid(uid);

	if (argv) {
	    /* execute the program */
	    if (envp) {
		(void) execve(argv[0], argv, envp);
	    } else if (flags & CHILD_MINENV) {
		if (minp == NULL) {
		    /* pass through the TZ environment variable */
		    char *tz = getenv("TZ");

		    for (minp = min_env; *minp; minp++)
			;
#ifndef CHILD_ENVIRON
#ifdef SHELL_EXEC_PATH
		    *minp++ = xprintf("SHELL=%s", SHELL_EXEC_PATH);
#else
		    *minp++ = "SHELL=/bin/sh";
#endif
		    *minp++ = xprintf("PATH=%s", SECURE_PATH);
#endif	/* CHILD_ENVIRON */
		    if (tz)
			*minp++ = xprintf("TZ=%s", tz);
		}
		(void) execve(argv[0], argv, min_env);
	    } else {
		(void) execv(argv[0], argv);
	    }

	    /* Oh well, all this work and we can't even exec the file */
	    _exit(EX_OSFILE);
	}
    } else {
	/* in parent process */
	if (in) {
	    /* close the child stdin, return the write-channel */
	    (void) close(stdin_fd[0]);
	    *in = fdopen(stdin_fd[1], "w");
	}
	if (out) {
	    /* close the child stdout, return the read-channel */
	    (void) close(stdout_fd[1]);
	    *out = fdopen(stdout_fd[0], "r");
	}
    }

    return pid;
}

/*
 * close_child - close down the child process started with open_child.
 *
 * if non-zero file pointers are passed, the given files are closed.
 * Then, wait for the specified pid to exit and return its status.
 *
 * NOTE:  we do not keep the exit status of processes other than the
 *	  one we are waiting.  Thus, only one process should be open
 *	  at a time by open_child, unless the caller wishes to handle
 *	  the performing the waits itself.
 *
 * return EOF, on error, or exit status.
 */
int
close_child(in, out, pid)
    FILE *in;				/* pipe to child's stdin */
    FILE *out;				/* pipe to child's stdout */
    int pid;				/* pid of child process */
{
    int i;				/* return value from wait */
    int status;				/* status from wait */

    if (in) {
	(void) fclose(in);		/* close the pipe to stdin */
    }

    if (out) {
	(void) fclose(out);		/* close the pipe to stdout */
    }

    /* wait for the child process to die */
    while ((i = wait((STATUS_TYPE *)&status)) != pid && i >= 0) ;

    if (i < 0) {
	/* failed to find the child process */
	return FAIL;
    }
    return status;			/* everything went okay */
}

/*
 * close_all - close all file descriptors other than 0, 1 and 2
 *
 * At several key points smail needs to know that no extra file descriptors
 * being used, such as when execing other processes (to ensure that
 * child processes cannot read or write protected information left open in
 * smail) and at startup (to prevent smail running out of file descriptors).
 */
void
close_all()
{
    register int i;
    int max_nfd;
    /* so just how many file descriptors do we have? */
#ifdef POSIX_OS
    max_nfd = sysconf(_SC_OPEN_MAX);
#else	/* not POSIX_OS */
# ifdef	OPEN_MAX
    max_nfd = OPEN_MAX;
# else	/* not POSIX_OS && not OPEN_MAX */
#  ifdef NOFILE
    max_nfd = NOFILE;
#  else	/* not POSIX_OS && not OPEN_MAX && not NOFILE */
    max_nfd = 20;
#  endif
# endif
#endif

    for (i = 3; i  < max_nfd; i++)
	(void) close(i);
}

#ifndef	HAVE_RENAME
/*
 * rename - emulate the BSD/System V.3 rename(2) system call
 *
 * This subroutine is not atomic, so functions which are worried about
 * consistency across system failures or in the face of multiple processes
 * should consider the situation when HAVE_RENAME is not defined.
 *
 * The errno returned will not always correspond to what would have
 * been produced by a true rename(2) system call.
 */
int
rename(from, to)
#ifdef ANSI_C
    const char *from;			/* old file name */
    const char *to;			/* new file name */
#else
    char *from;				/* old file name */
    char *to;				/* new file name */
#endif
{
    struct stat statbuf;

    if (stat(from, &statbuf) < 0) {
	/* `from' is inaccessible, so forget it */
	return -1;
    }

    if (unlink(to) && errno != ENOENT) {
	/* could not unlink `to', though it may exist */
	return -1;
    }

    if (link(from, to) < 0) {
	/* failed to link, however, an existing `to' file was removed */
	return -1;
    }

    if (unlink(from) < 0) {
	/* could not unlink, the file exists under two names */
	return -1;
    }
    return 0;
}
#endif	/* HAVE_RENAME */

#if !defined(HAVE_MKDIR)
/*
 * for OS's that lack a mkdir system call, call the mkdir program
 */
int
mkdir(dir, mode)
    char *dir;
    int mode;
{
    static char *mkdir_argv[] = {
	"/bin/mkdir",
	NULL,				/* space for directory name */
	NULL,
    };
    /* set the umask so that the directory will have the correct perms */
    int oumask = umask((~mode)&0777);
    int pid;

    mkdir_argv[1] = dir;

    /* start up the mkdir program */
    pid = open_child(mkdir_argv, (char **)NULL,
		     (FILE **)NULL, (FILE **)NULL, -1, CHILD_DEVNULL, 0, 0);

    (void) umask(oumask);

    return close_child((FILE *)NULL, (FILE *)NULL, pid);
}
#endif	/* !defined(HAVE_MKDIR) */

#ifndef	HAVE_DUP2
/*
 * dup2 - this provides the dup2 function as described by the Posix Draft
 *        for those sites which do not have it.
 *
 * Mark Colburn (mark@jhereg.mn.org)
 */
int
dup2(fildes, fildes2)
        int     fildes, fildes2;
{
        int     fid;

        if (fcntl(fildes, F_GETFL, 0) == -1)
                return(EBADF);
        if (fildes != fildes2)
                close(fildes2);
        fid = fcntl(fildes, F_DUPFD, fildes2);
        return(fid);
}
#endif	/* HAVE_DUP2 */

#ifndef HAVE_READDIR
/*
 * for OS's that lack the directory facilities, emulate
 */
struct direct_with_null {
    struct direct d;
    int trailing_null;			/* make sure name ends in a nul byte */
};

typedef FILE DIR;
#define opendir(dn)	(fopen(dn, "r"))
#define closedir(dn)	(fclose(dn))

static struct direct *
readdir(dirp)
    DIR *dirp;
{
    static struct direct_with_null ret;

    ret.trailing_null = 0;
    do {
	if (fread(&ret.d, sizeof(ret.d), 1, (FILE *)dirp) < 1) {
	    return NULL;
	}
    } while (ret.d.d_ino == 0);
    return &ret.d;
}
#endif	/* HAVE_READDIR */

/*
 * scan_dir - scan through a directory, looking for filenames
 *
 * if given a non-NULL directory argument, opens the directory and
 * returns the first file, or NULL if it contains no files.
 * successive calls, with a NULL argument, return successive files
 * in the directory.  On the call after the last directory, a NULL
 * is returned and the directory is closed.
 *
 * scan_dir returns static data which is reused on subsequent calls.
 */
char *
scan_dir(dn)
    char *dn;				/* directory name, or NULL */
{
    static DIR *dirp = NULL;		/* opened directory */
    struct direct *dp;			/* current directory entry */

    if (dn) {
	if (dirp) {
	    (void) closedir(dirp);
	}
	dirp = opendir(dn);
    }
    if (dirp == NULL) {
	return NULL;			/* no directory, so no entries */
    }
    dp = readdir(dirp);
    if (dp == NULL) {
	(void) closedir(dirp);
	return NULL;
    }

    return dp->d_name;
}

#ifdef	USE_FCNTL_RD_LOCK
/*
 * fcntl_rd_lock - get a shared lock using a System V-style fcntl.
 */
int
fcntl_rd_lock(fd)
    int fd;
{
    struct flock l;

    l.l_type = F_RDLCK;
    l.l_whence = 0;
    l.l_start = 0L;
    l.l_len = 0L;
    if (fcntl(fd, F_SETLKW, &l) < 0) {
	return FAIL;
    }
    return SUCCEED;
}
#endif

/* touch - update the access and modified times of the given path;
 *	   if both values are zero, the current time is used for both
 */
int
touch(fname, atime, mtime)
    char *fname;
    time_t atime, mtime;
{
    struct utimbuf times;
    time_t cur_time;

    bzero((char *)&times, sizeof(times));
    cur_time = time(NULL);
    times.actime  = atime ? atime : cur_time;
    times.modtime = mtime ? mtime : cur_time;
    return utime(fname, &times);
}

/* fsetsize - set file size
 *
 * If truncation is possible, set file to wantsize.
 * Otherwise, write NULs after oversize.
 */
/* ARGSUSED */
int
fsetsize(fd, wantsize, oversize)
    int fd;			/* fd to adjust */
    off_t wantsize;		/* desired size */
    off_t oversize;		/* bytes not to overwrite UNUSED ifndef F_FREESP */
{
    off_t cursize;

    /* determine current file size */
    if ((cursize = lseek(fd, (off_t) 0L, SEEK_END)) == -1) {
	return -1;
    }

    /* maybe lengthen... */
    if (cursize < wantsize) {
	if (lseek(fd, wantsize - 1, 0) == -1 || write(fd, "", 1) == -1) {
	    return -1;
	}
	return 0;
    }

    /* maybe shorten... */
    if (wantsize < cursize) {

#ifdef HAVE_FTRUNCATE
	return ftruncate(fd, wantsize);
#else
# ifdef HAVE_CHSIZE
	return chsize(fd, wantsize);
# else
#  ifdef F_FREESP

	/*
	 * Thanks to Larry Wall for this hack.
	 * It relies on the UNDOCUMENTED F_FREESP argument to
	 * fcntl(2), which truncates the file so that it ends
	 * at the position indicated by fl.l_start.
	 */
	struct flock fl;

	fl.l_whence = 0;
	fl.l_len = 0;
	fl.l_start = wantsize;
	fl.l_type = F_WRLCK;    /* write lock on file space */

	return fcntl(fd, F_FREESP, &fl);

#  else /* !F_FREESP */

	/*
	 * No truncation features are available.
	 * Zero out unwanted bytes (i.e. those past oversize).
	 */

	if (oversize < cursize) {
	    off_t left;

	    if (lseek(fd, oversize, SEEK_SET) == -1) {
		return -1;
	    }
	    left = cursize - oversize;
	    while (left > 0) {
		static char x[64];
		size_t n;

		if ((n = sizeof(x)) > left) {
		    n = left;
		}
		if (write(fd, x, n) == -1) {
		    return -1;
		}
		left -= n;
	    }
	}
	return 0;

#  endif /* F_FREESP */
# endif /* HAVE_CHSIZE */
#endif /* HAVE_FTRUNCATE */

    }

    /* no adjustment required */
    return 0;
}
