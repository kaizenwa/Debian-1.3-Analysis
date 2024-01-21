/*
#ident	"@(#)smail/src:RELEASE-3_2:spool.c,v 1.16 1996/06/25 23:26:33 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * spool.c:
 *	message spooling and retrieval.  This source file implements a
 *	reliable spooling system which is resiliant against inaccessible
 *	directories, create errors and write errors.  The algorithms here
 *	are set up such that alternate directories can be used in the case
 *	that smail is not able to complete spooling to a primary spool
 *	directory.
 *
 *	NOTE:  This section will probably require substantial
 *	       modifications to work with a non-UN*X operating
 *	       system.
 *
 *	external functions:  creat_spool, write_spool, open_spool,
 *			     close_spool, unlink_spool, seek_spool,
 *			     tell_spool, send_spool, read_spool,
 *			     log_spool_errors, new_grade, defer_message,
 *			     message_date
 */
#include "defs.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "smail.h"
#include "spool.h"
#include "transport.h"
#include "log.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

#if defined(POSIX_OS) || !defined(UNIX_BSD)
# include <time.h>
#else	/* not UNIX_BSD */
# include <sys/time.h>
#endif	/* not UNIX_BSD */

#if defined(UNIX_SYS5) || defined(POSIX_OS) || defined(USE_FCNTL)
# include <fcntl.h>
#else
# ifdef UNIX_BSD
#  include <sys/file.h>
# endif
#endif

#ifdef STANDALONE
# define xmalloc malloc
# define xfree free
extern char *malloc();
int force_write_error = FALSE;
#endif	/* STANDALONE */

/* variables exported from this file */
char *message_id = NULL;		/* unique ID for this message */
char *spool_dir;			/* directory used to spool message */
char *spool_fn = NULL;			/* basename of open spool file */
char *input_spool_fn = NULL;		/* name in input/ directory */
int spoolfile = -1;                     /* open spool file */
char *lock_fn;				/* name of lock file for spool_fn */
char *msg_buf;				/* i/o buffer for spool file */
char *msg_ptr;				/* read placeholder in msg_buf */
char *msg_max;				/* last valid char in msg_buf */
char *end_msg_buf;			/* end of msg_buf */
off_t msg_foffset;			/* file offset for msg_buf contents */
off_t msg_size;				/* size of spool file */

/* types local to this file */
struct log_msgs {
    struct log_msgs *succ;
    char msg[1];
};
enum locker { l_creat, l_open, l_lock };	/* who is trying to lock */

/* variables local to this file */
static char *temp_fn = NULL;		/* temp spool file name */
static char *funct_name;		/* name of current function */
static char *old_spool_dir;		/* saved value of spool_dir */
static char *old_spool_fn;		/* saved value of spool_fn */
static char *old_input_spool_fn;	/* saved value of input_spool_fn */
static int old_spoolfile;		/* saved value of spoolfile */
static struct log_msgs *log_msgs;	/* message lines to send to log */
static struct log_msgs **next_log_msg;	/* where to put next log message */

/* functions local to this file */
static int int_creat_spool();
static void build_spool_fn();
static void build_message_id();
static int set_alt_spool();
static int lock_spoolfile();
static void copy_old_names();
static void failed_write();
static int copy_old_spool();
static void log_message();

/* functions imported from libc */
off_t lseek();
struct tm *localtime();
struct tm *gmtime();
char *ctime();

/* variables imported from libc */
extern int errno;

/* trap undefined open flags */
#ifndef	O_RDONLY
# define O_RDONLY	0
#endif
#ifndef O_WRONLY
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
 * creat_spool - create a spool file
 *
 * This is external entrypoint for spool file creation.  It initializes some
 * state and then calls int_creat_spool() to do the real work.
 *
 * If spoolfile creation succeeded, return SUCCEED.  Also, the following
 * external variables will be set:
 *
 *	spool_dir	- the directory in which the spoolfile was created
 *	spool_fn	- the basename of the generated file
 *	input_spool_fn	- the name of the spool file in the input directory
 *	spoolfile	- the file descriptor (used by the PUTSPOOL macro)
 *	message_id	- a unique message ID computed from the filename
 *	msg_buf,msg_ptr	- points to a region in which the message can be
 *			  written into memory
 *
 * In addition, the current directory will be set to spool_dir, and the file
 * will be locked against premature attempts at delivery by background
 * processes.
 *
 * If spoolfile creation failed, return FAIL.  In this case, spool_fn,
 * input_spool_fn, spool_dir and message_id will all be set to NULL.
 */
int
creat_spool()
{
    DEBUG(DBG_SPOOL_HI, "create_spool called\n");
    funct_name = "creat_spool";
    temp_fn = NULL;
    spool_fn = NULL;
    input_spool_fn = NULL;
    message_id = NULL;
    lock_fn = NULL;
    spoolfile = -1;
    log_msgs = NULL;
    next_log_msg = &log_msgs;
    msg_foffset = 0;
    msg_size = 0;
    if (msg_buf == NULL) {
	/* allocate the spool i/o buffer if it does not yet exist */
	msg_buf = xmalloc(message_bufsiz);
    }
    msg_max = msg_buf;			/* start at beginning of buffer */
    msg_ptr = msg_buf;
    end_msg_buf = msg_buf + message_bufsiz;
    if (int_creat_spool() == FAIL) {
	DEBUG(DBG_SPOOL_LO, "creat_spool failed!\n");
	return FAIL;
    }
    DEBUG2(DBG_SPOOL_LO, "new spool file is %s/%s\n",
	   spool_dir, input_spool_fn);
    return SUCCEED;
}

/*
 * int_creat_spool - create a spool file (internal form)
 *
 * try to create a spool file in either the primary or alternate spool
 * directory.  The file will be locked and empty at the end of creat_spool.
 *
 * return SUCCEED or FAIL.  If FAIL, then the spool file and lock file
 * will not exist unless they could not be unlinked.
 * log_spool_errors should be called once the log files are opened, as
 * well.
 */
static int
int_creat_spool()
{
    int attempts = 0;			/* # open attempts in a directory */
    char build_temp_fn[sizeof("input/msg.dddddddddd")];

    /* first try the primary spool directory */
    spool_dir = NULL;

    /* get the first "alternate" directory, which is the primary directory */
    if (set_alt_spool((char *)NULL) == FAIL) {
	return FAIL;
    }

    /*
     * create the temporary spool file in the first spool directory
     * that the open succeeds in.
     */
    (void) sprintf((temp_fn = build_temp_fn), "input/msg.%d", getpid());
    for (;;) {
	/*
	 * unfortunately, the errno returned by creat(2) does not
	 * distinguish between directory and file access errors, so
	 * we use the open call, which does, for systems that
	 * support it
	 *
	 * for systems with O_SYNC but without fsync() use O_SYNC to
	 * ensure that the file is on disk before we return any responses
	 * to the user.
	 */
#ifdef O_EXCL
#if defined(O_SYNC) && !defined(HAVE_FSYNC) && !defined(NO_SPOOL_O_SYNC)
	spoolfile = open(temp_fn, O_RDWR|O_CREAT|O_EXCL|O_SYNC, spool_mode);
#else
	spoolfile = open(temp_fn, O_RDWR|O_CREAT|O_EXCL, spool_mode);
#endif	/* O_SYNC && !HAVE_FSYNC && !NO_SPOOL_O_SYNC */
#else	/* O_EXCL */
	/*
	 * if we must use creat, then the mode should not allow writing.
	 * As well, we will have to reopen for reading later on in
	 * read_spool, if we try to read on the file without closing
	 * and then opening again.
	 */
	spoolfile = creat(temp_fn, spool_mode&(~0222));
	if (spoolfile < 0 && errno == EACCES) {
	    /* we assume that EACCES does not represent a directory error */
	    errno = EEXIST;
	}
#endif	/* O_EXCL */
	if (spoolfile < 0 && errno == ENOENT) {
	    /* NOENT means the directory did not exist, try to make it */
	    DEBUG1(DBG_SPOOL_LO, "make directory %s/input\n", spool_dir);
	    (void) mkdir("input", auto_mkdir_mode);
#ifdef O_EXCL
#if defined(O_SYNC) && !defined(HAVE_FSYNC) && !defined(NO_SPOOL_O_SYNC)
	    spoolfile = open(temp_fn, O_RDWR|O_CREAT|O_EXCL|O_SYNC, spool_mode);
#else
	    spoolfile = open(temp_fn, O_RDWR|O_CREAT|O_EXCL, spool_mode);
#endif	/* O_SYNC && !HAVE_FSYNC && !NO_SPOOL_O_SYNC */
#else
	    spoolfile = creat(temp_fn, spool_mode&(~0222));
	    if (spoolfile < 0 && errno == EACCES) {
		errno = EEXIST;
	    }
#endif
	}
	if (spoolfile < 0) {
	    if (errno == EEXIST && attempts == 0) {
		/* if the spool file existed, then try to unlink it */
		DEBUG2(DBG_SPOOL_MID,
		       "int_creat_spool: %s/input/%s exists, unlinking\n",
		       spool_dir, temp_fn);
		attempts++;		/* only attempt unlink once */
		continue;
	    }

	    /* otherwise we need to try an alternate spool directory */
	    if (set_alt_spool("cannot create spool file") == FAIL) {
		return FAIL;
	    }
	    attempts = 0;		/* new directory, reset try count */
	    continue;
	}

	/*
	 * we have an open temp file.  Now create a lock file
	 * and move the temp file to its more permanent name.
	 */
	build_spool_fn(spool_grade);
#ifndef lock_fd
	if (lock_spoolfile(l_creat) == FAIL) {
	    /* we need to try an alternate directory */
	    (void) close(spoolfile);
	    (void) unlink(temp_fn);
	    if (set_alt_spool("cannot lock_spoolfile() message") == FAIL) {
		return FAIL;
	    }
	    continue;
	}
#else /* lock_fd */
	if ((lock_by_name && lock_spoolfile(l_creat) == FAIL) ||
	    (! lock_by_name && lock_fd(spoolfile) == FAIL))
	{
	    (void) close(spoolfile);
	    (void) unlink(temp_fn);
	    if (set_alt_spool(lock_by_name ?
			      "cannot lock_spoolfile() message" :
			      "cannot lock_fd() message"
			      ) == FAIL) {
		return FAIL;
	    }
	    continue;
	}
#endif /* lock_fd */
	if (rename(temp_fn, input_spool_fn) < 0) {
	    /* why would rename fail? */
	    if (lock_by_name) {
		(void) unlink(lock_fn);
	    }
	    (void) close(spoolfile);
	    (void) unlink(temp_fn);
	    if (set_alt_spool("failed to rename temp file") == FAIL) {
		return FAIL;
	    }
	    continue;
	}

	/* we have a valid locked spool file */
	return SUCCEED;
    }
}

/*
 * build_spool_fn - form the spool filename
 *
 * as a side effect, generate the lock filename, if needed, and
 * generate the message_id.
 *
 * the grade_char represents a priority which is appended to the
 * filename.
 */
static void
build_spool_fn(grade_char)
    int grade_char;
{
    /* store the actual spool file basename here */
    static char fn[SPOOL_FN_LEN + 1];
    /* name of spool file in input directory */
    static char ifn[sizeof("input/") + SPOOL_FN_LEN];
    /* store the lock file name here */
    static char lfn[sizeof("lock/") + SPOOL_FN_LEN];
    extern long time();
    long clock = time((long *)0);
    struct stat statbuf;
    char a_inode[8];			/* ASCII base 62 inode number (+ spare byte) */

    /* get the inode number in base 62 information */
    (void) fstat(spoolfile, &statbuf);
    (void) strcpy(a_inode, base62((long)statbuf.st_ino));

    /* this is 14 chars which should work on all UN*X systems */
    spool_fn = fn;
    (void) sprintf(spool_fn, "%s-%s%c", base62(clock), a_inode, grade_char);

    /* use this to build the path in the input directory */
    input_spool_fn = ifn;
    (void) sprintf(input_spool_fn, "input/%s", spool_fn);

    /* have the message-ID computed from the computed spool file name */
    build_message_id();
    if (lock_by_name) {
	lock_fn = lfn;
	(void) sprintf(lock_fn, "lock/%s", spool_fn);
    }
    DEBUG2(DBG_SPOOL_HI, "build_spool_fn: try spool file %s/%s\n",
	   spool_dir, input_spool_fn);
}

/*
 * build_message_id - build message_id from the value of spool_fn
 *
 * the only difference is that the message-Id begins with the letter `m', to
 * make it a valid local-addr vis-a-vis RFC822.
 */
static void
build_message_id()
{
    static char m_id[sizeof("m") + SPOOL_FN_LEN];

    message_id = m_id;
    (void) sprintf(message_id, "m%s", spool_fn);
}


/*
 * lock_message - lock the current message
 *
 * When forking a process to deliver mail, locks are released by the
 * parent process and then regained in the child process by calling
 * lock_message().  If the lock cannot be regained then the child
 * process should exit, assuming that some other process has decided
 * to attempt delivery.  This method is necessary when using lock
 * files, as pids are used to determine whether the locking process
 * still exists.  It is also necessary when using the System V lockf
 * call as this does not preserve locks in a child of the process that
 * made the lock.
 *
 * Returns SUCCEED if the lock was obtained, FAIL otherwise.
 */
int
lock_message()
{
    struct stat statbuf;
    int success;
    off_t offset;

#ifdef lock_fd
    if (lock_by_name) {
	success = lock_spoolfile(l_lock);
    } else {
	offset = lseek(spoolfile, (off_t) 0L, SEEK_CUR);
	(void) lseek(spoolfile, (off_t) 0L, SEEK_SET);
	success = lock_fd(spoolfile);
	(void) lseek(spoolfile, offset, SEEK_SET);
    }
#else
    success = lock_spoolfile(l_lock);
#endif
    if (success == FAIL) {
	return FAIL;
    }
    (void) fstat(spoolfile, &statbuf);

    /*
     * If the file has been removed, then don't process it.
     */
    if (statbuf.st_nlink == 0) {
	DEBUG2(DBG_SPOOL_MID,
	       "open_spool: %s/%s: spool file was removed\n",
	       spool_dir, input_spool_fn);
	close_spool();
	return FAIL;
    }
    return SUCCEED;
}

/*
 * unlock_message - unlock the current message
 *
 * The parent process calls this before forking a process to perform
 * delivery.  See lock_message() for an explanation.
 */
void
unlock_message()
{
    off_t offset;

#ifdef lock_fd
    if (lock_by_name) {
#endif
	if (lock_fn) {
	    DEBUG2(DBG_SPOOL_HI, "close_spool: unlinking (lock) %s/%s\n",
		   spool_dir, lock_fn);
	    (void) unlink(lock_fn);
	}
#ifdef lock_fd
    } else {
	offset = lseek(spoolfile, (off_t) 0L, SEEK_CUR);
	(void) lseek(spoolfile, (off_t) 0L, SEEK_SET);
	unlock_fd(spoolfile);
	(void) lseek(spoolfile, offset, SEEK_SET);
    }
#endif
}

/*
 * lock_spoolfile - create a lock file for the spool file
 *
 * algorithm:
 * 1.  create lock file exclusively.  If this fails, goto step 3.
 * 2.  write ASCII process id, followed by newline.  If this succeeds,
 *     then the lock succeeded, close the file and return.
 * 3.  stat the lock file
 * 4.  if st_size != 0, the proceed to step 10
 * 5.  The lock file is empty because the locking process has
 *     not yet written out its process id, or because the
 *     process or system crashed prior to the write.
 *     If the current time < Oct 28, 1986, then proceed to step 7.
 *     If st_ctime > the current time then proceed to step 8.
 *     If st_ctime < `now' - 2 hours proceed to step 9.
 * 6.  The empty lock file is too new to touch (the system could
 *     be very heavely loaded) and so we simply conclude that
 *     some process has it locked and go on to do something else.
 * 7.  Since this version of smail did not exist on Oct 28, 1986,
 *     the current time must be set wrong.  At this point
 *     we give up all hope of stale lock file detection,
 *     consider the file locked and go on to do something else.
 * 8.  The empty lock file was created in the future so we don't
 *     know if the empty lock file is stale or not.  We first
 *     bring the st_ctime of the file back into reality by
 *     doing a chmod (to the same permission). This forces
 *     the st_ctime to be set to `now'.  Next we send the
 *     following error message to the system log:
 *
 *	   time warp on zero length lock file: lock/0571338010a72y
 *
 *     We consider the lock file active for now.
 * 9.  The empty lock file is very likely stale.  We will
 *     remove the lock file but we will still consider it
 *     locked, allowing some future process to operate
 * 10. The lock file contains the process id of the locking
 *     process.  We open the lock file and read it to find
 *     the associated pid.
 *     If pid == 0, then proceed to step 13.
 *     If pid is not a process, then proceed to step 12.
 * 11. The process pid is a valid and currently
 *     running process, and thus the lock file is really
 *     valid.  At this point we go on to do something else.
 * 12. The lock file is very likely stale.  We will remove the
 *     lock file but we will still consider it locked, allowing
 *     some future process to operate on the spool file.  We
 *     now go on to do something else.
 * 13. If pid == 0, then the lock file is a
 *     permanent lock file.  (perhaps set by the system
 *     administrator to freeze a mail message)  At this
 *     point we go on to do something else.
 *
 * return either SUCCEED or FAIL, and set errno to EEXIST if
 * the spoolfile is already locked.
 */
static int lock_creat();
static int check_empty_lock();
static int verify_lock();

static int
lock_spoolfile(who)
    enum locker who;			/* who is locking, open or creat */
{
    struct stat statbuf;		/* temp buf for stats */
    int attempts = 0;
    int mkdir_tried = FALSE;		/* TRUE if mkdir("lock") tried */

    DEBUG1(DBG_SPOOL_HI, "lock_spoolfile called, lock_fn = %s\n", lock_fn);
    /*
     * loop until we have a lock or we fail to get a lock
     */
    for (;;) {
	/*
	 * try to create one
	 */
	if (lock_creat() == FAIL) {
	    if (errno == ENOENT && !mkdir_tried && auto_mkdir) {
		mkdir_tried = TRUE;
		DEBUG1(DBG_SPOOL_LO, "make directory %s/lock\n",
		       spool_dir);
		(void) mkdir("lock", auto_mkdir_mode);
		continue;
	    }
	    if (errno != EEXIST) {
		/* it failed, but not because it exists, can't handle this */
		DEBUG1(DBG_SPOOL_HI, "create failed: %s\n", strerror(errno));
		return FAIL;
	    }
	    if (who == l_creat) {
		/*
		 * for creat_spool, no lock file should exist because
		 * of inode uniqueness on a filesystem.  Just unlink
		 * the lock file and try again (once).
		 */
		if (attempts == 0) {
		    (void) unlink(lock_fn);
		    attempts++;
		    continue;
		}
		DEBUG3(DBG_SPOOL_MID,
		       "lock_spoolfile: %s/%s: lock failed: %s\n",
		       spool_dir, lock_fn, strerror(errno));
		return FAIL;
	    }
	} else {
	    /* the create worked, great! */
	    return SUCCEED;
	}

	if (stat(lock_fn, &statbuf) < 0) {
	    /* failed to stat the file, hmmm */
	    DEBUG3(DBG_SPOOL_LO,
		   "lock_spoolfile: %s/%s: stat failed: %s\n",
		   spool_dir, lock_fn, strerror(errno));
	    return FAIL;
	}

	if (statbuf.st_size == 0) {
	    if (check_empty_lock(&statbuf) == SUCCEED) {
		errno = EEXIST;		/* lock was upheld, for now */
		return FAIL;
	    }
	} else {
	    if (verify_lock() == SUCCEED) {
		errno = EEXIST;		/* lock was upheld */
		return FAIL;
	    }
	}

	/*
	 * we consider the lock to be stale, so unlink it,
	 * and let a future process try again.
	 */
	(void) unlink(lock_fn);
	DEBUG2(DBG_SPOOL_LO,
	       "lock_spoolfile: %s/%s: stale lock file removed\n",
	       spool_dir, lock_fn);
	errno = EEXIST;
	return FAIL;
    }
}

/*
 * lock_creat - try to create a lock and write into it the current pid
 *
 * this is only called from lock_spoolfile and returns SUCCEED or FAIL.
 */
static int
lock_creat()
{
    int lfd;
    char apid[BITS_PER_INT/3 + 2];	/* should hold the process id */
    int ct;

#ifdef O_EXCL
    lfd = open(lock_fn, O_RDWR|O_CREAT|O_EXCL, lock_mode);
#else	/* O_EXCL */
    /*
     * it is time for the silly creat trick again
     */
    lfd = creat(lock_fn, lock_mode&(~0222));
    if (lfd < 0 && errno == EACCES) {
	errno = EEXIST;
    }
#endif	/* O_EXCL */
    if (lfd < 0) {
	/* we failed to creat the lock the file */
	return FAIL;
    }
    (void) sprintf(apid, "%u\n", getpid());
    ct = strlen(apid);
    if (write(lfd, apid, ct) < ct) {
	/* we failed to write the pid into the lock file, quit */
	(void) unlink(lock_fn);
	(void) close(lfd);
	return FAIL;
    }
    (void) close(lfd);
    return SUCCEED;
}

/*
 * check_empty_lock - decide if zero-length lock file is stale
 *
 * return SUCCEED if not stale and should be kept, FAIL otherwise.
 */
static int
check_empty_lock(stp)
    struct stat *stp;			/* statbuf from lock_spoolfile */
{
    struct tm *ctm;
    extern long time();
    long now = time((long *)0);		/* verify based on age */

    ctm = gmtime(&now);
    if (ctm->tm_year <= 86 && ctm->tm_mon <= 10 && ctm->tm_mday < 28) {
	/*
	 * it is before chongo's birthday, but on a year that must be
	 * in the past.  Since this version of smail has not been
	 * written yet, punt.
	 */
	write_log(LOG_SYS|LOG_CONS, "machine is in a time warp, check date");
	return SUCCEED;
    }

    if (stp->st_ctime > now) {
	/*
	 * file created in the future, bring it back to the present, but
	 * otherwise assume it is a valid lock.
	 */
	(void) chmod(lock_fn, stp->st_mode);
	write_log(LOG_SYS, "time warp on zero length lock file: %s", lock_fn);
	return SUCCEED;
    }
    if (stp->st_ctime < now - 2*3600/*two hours in seconds*/) {
	/* lock not upheld, file can be removed */
	return FAIL;
    }

    /* a new lock file, leave it alone for now */
    DEBUG(DBG_SPOOL_HI, "new zero-length lock file, let it stand\n");
    return SUCCEED;
}

/*
 * verify_lock - decide if non-zero lockfile is stale
 *
 * return SUCCEED if not stale and should be kept, FAIL otherwise.
 */
static int
verify_lock()
{
    int lfd;
    char rpid[BITS_PER_INT/3 + 2];
    int pid = 0;
    int ct;

    lfd = open(lock_fn, O_RDONLY);
    if (lfd < 0) {
	/* failed to open the lock file, don't consider it stale yet */
	DEBUG3(DBG_SPOOL_HI, "verify_lock: %s/%s: %s\n",
	       spool_dir, lock_fn, strerror(errno));
	return SUCCEED;
    }

    /* read in the pid */
    ct = read(lfd, rpid, sizeof(rpid)-1);
    if (ct <= 0) {
	/* failed to read anything from the lock file, ignore it for now */
	(void) close(lfd);
	DEBUG3(DBG_SPOOL_HI, "verify_lock: %s/%s: read failed: %s\n",
	       spool_dir, lock_fn, strerror(errno));
	return SUCCEED;
    }
    (void) close(lfd);

    rpid[ct] = '\0';			/* firewall */
    (void) sscanf(rpid, "%d", &pid);
    if (pid == 0) {
	/* pid is 0 or not a valid number, consider the lock valid */
	DEBUG(DBG_SPOOL_HI, "verify_lock: pid==0 lock considered valid\n");
	return SUCCEED;
    }

    /*
     * does the process exist?  before you look at the man page,
     * kill(pid,0) detects process existence on all versions of
     * UN*X I know of including v6 and v7
     */
    if (kill(pid, 0) < 0 && errno == ESRCH) {
	/* process does not exist, lock is stale */
	return FAIL;
    }

    /* the lock is valid */
    DEBUG2(DBG_SPOOL_HI, "verify_lock: %s/%s: valid lock\n",
	   spool_dir, lock_fn);
    return SUCCEED;
}


/*
 * write_spool - write completed block to the spool file
 *
 * write the contents of the spool i/o buffer out to the spool file,
 * at the current i/o position.  But otherwise don't change anything.
 *
 * If the write fails then try to create an alternate spool file and
 * copy into it the contents previously written to the old spool file.
 *
 * NOTE:  writing to the spool file should be done through the macro
 *	  PUTSPOOL(c) defined in spool.h, except that the final write
 *	  should be done with a call to write_spool.  PUTSPOOL will
 *	  manage the various associated pointers.
 */
int
write_spool()
{
    old_spool_fn = NULL;		/* set to old name if new created */
    old_input_spool_fn = NULL;
    old_spool_dir = NULL;
    old_spoolfile = spoolfile;

    funct_name = "write_spool";
    DEBUG(DBG_SPOOL_HI, "write_spool called\n");

    /*
     * loop until we have successfully written the block to something,
     * or until we cannot recover from past errors
     */
    for (;;) {
	if (write(spoolfile, msg_buf, (size_t) (msg_max - msg_buf)) == msg_max - msg_buf
#ifdef STANDALONE
	    /* it is a bit tough to get random write errors while testing */
	    && (!force_write_error || (force_write_error = FALSE))
#endif	/* STANDALONE */
	    )
	{
	    break;
	} else {
	    /*
	     * write failed, unlink the old spool file and copy contents
	     * to a new spool file in an alternate spool directory.
	     */
	    copy_old_names();
	    failed_write();
	    for (;;) {
		/* get an alternate spool directory */
		if (set_alt_spool("write to spool failed") == FAIL) {
		    (void) close(old_spoolfile);
		    DEBUG(DBG_SPOOL_LO,
			  "write_spool: no more spool dirs, write failed\n");
		    return FAIL;
		}
		if (int_creat_spool() == FAIL) {
		    (void) close(old_spoolfile);
		    DEBUG(DBG_SPOOL_LO,
			  "write_spool: can't create new spool file, write failed\n");
		    return FAIL;
		}
		if (msg_size == 0) {
		    break;
		} else {
		    int code = copy_old_spool();

		    if (code == READ_FAIL) {
			DEBUG(DBG_SPOOL_LO,
			      "write_spool: failed to copy old spool file: read failed\n");
			return FAIL;
		    } else if (code == SUCCEED) {
			break;
		    }
		    DEBUG(DBG_SPOOL_LO,
			  "write_spool: failed to copy old spool file: write failed\n");
		}
	    }
	}
    }
    if (old_spool_fn) {
	/* we succeeded in copying to a new spool file, don't need these */
	xfree(old_spool_fn);
	xfree(old_input_spool_fn);
	xfree(old_spool_dir);
    }
    msg_size += msg_max - msg_buf;

    return SUCCEED;
}

/*
 * copy_old_names - make a copy of the old spool file names
 */
static void
copy_old_names()
{
    if (old_spool_fn == NULL) {
	/* save a copy of the old names if needed later */
	old_spool_fn = COPY_STRING(spool_fn);
	old_input_spool_fn = COPY_STRING(input_spool_fn);
	old_spool_dir = COPY_STRING(spool_dir);
    }
}

/*
 * failed_write - remove old files after a write error
 *
 * NOTE: we aren't closing them, so the data in them can still be read.
 */
static void
failed_write()
{
    if (spool_fn) {
	(void) unlink(input_spool_fn);
	spool_fn = NULL;
	input_spool_fn = NULL;
    }
    if (lock_fn) {
	(void) unlink(lock_fn);
    }
}

/*
 * copy_old_spool - copy old spool file to the new one
 *
 * return READ_FAIL on read error, SUCCEED on success, and
 * WRITE_FAIL on write error
 */
static int
copy_old_spool()
{
    char *tempbuf = xmalloc(message_bufsiz);
    off_t copied = 0;			/* count of chars copied to new file */

    /*
     * previous contents exist, we will need to copy them
     */
    (void) lseek(old_spoolfile, (off_t) 0L, SEEK_SET);
    while (copied < msg_size) {
	register int ct;

	/* fill up as much of the temp buffer as reasaonble */
	if (msg_size - copied < message_bufsiz) {
	    ct = msg_size - copied;
	} else {
	    ct = message_bufsiz;
	}

	ct = read(old_spoolfile, tempbuf, ct);
	if (ct <= 0) {
	    failed_write();
	    log_message("read failed in copying to new file");
	    (void) close(spoolfile);
	    (void) close(old_spoolfile);
	    return READ_FAIL;		/* read error - can't recover */
	} else {
	    if (write(spoolfile, tempbuf, ct) < ct) {
		failed_write();
		return WRITE_FAIL;	/* write error - maybe can recover */
	    }
	}
	copied += ct;
    }

    return SUCCEED;			/* we have recovered */
}


/*
 * open_spool - open and possibly lock the given spool file
 *
 * the filename is assumed to be absolute, so a chdir is done to
 * the directory name, and spool_fn is set to the basename.  This
 * process is destructive of the spool file string passed.
 *
 * As a side effect, msg_buf will be loaded with the first part of the
 * spool file.
 *
 * return SUCCEED or FAIL.  If fail, load error with error message.
 * errno will be 0 if no system error was involved.
 */
int
open_spool(fn, lock, error)
    char *fn;				/* basename of spool file */
    int lock;				/* if TRUE, lock the file */
    char **error;			/* return error message here */
{
    static char lfn[sizeof("lock/") + SPOOL_FN_LEN];
    struct stat statbuf;

    DEBUG1(DBG_SPOOL_HI, "open_spool(%s) called\n", fn);
    funct_name = "open_spool";
    message_id = NULL;
    lock_fn = NULL;
    spoolfile = -1;
    log_msgs = NULL;
    msg_foffset = 0;
    msg_size = 0;
    errno = 0;
    if (msg_buf == NULL) {
	/* allocate the spool i/o buffer if it does not yet exist */
	msg_buf = xmalloc(message_bufsiz);
    }
    msg_max = msg_buf;			/* start at beginning of buffer */
    end_msg_buf = msg_buf + message_bufsiz;
    if (fn[0] != '/') {
	DEBUG1(DBG_SPOOL_LO, "open_spool: %s: relative pathname!\n", fn);
	*error = "filename not absolute";
	return FAIL;
    }
    spool_dir = fn;		/* get directory from fn */
    spool_fn = rindex(fn, '/');		/* get basename from fn */
    input_spool_fn = spool_fn - sizeof("/input") + 1;
    spool_fn++;
    if (input_spool_fn < spool_dir ||
	strncmp(input_spool_fn, "/input", sizeof("/input") - 1))
    {
	DEBUG1(DBG_SPOOL_LO, "open_spool failed: %s: invalid spool filename",
	       spool_dir);
	*error = "invalid spool filename";
	return FAIL;
    }
    *input_spool_fn++ = '\0';

    if (chdir(spool_dir) < 0) {
	DEBUG2(DBG_SPOOL_LO, "open_spool failed: %s: chdir failed: %s",
	       spool_dir, strerror(errno));
	*error = "chdir failed";
	return FAIL;
    }

    /*
     * attempt to open the file
     *
     * some of the OS locking algorithms require that the file be
     * opened for writing.  For such systems, the spool_mode must have
     * at least ownership write permission.
     */
#ifdef LOCK_REQUIRES_WRITE
    if (lock && ! lock_by_name) {
	spoolfile = open(input_spool_fn, O_RDWR);
    } else
#endif
    {
	spoolfile = open(input_spool_fn, O_RDONLY);
    }
    if (spoolfile < 0) {
	DEBUG3(DBG_SPOOL_LO, "open_spool: %s/%s: open failed: %s\n",
	       spool_dir, input_spool_fn, strerror(errno));
	*error = "open failed";
	return FAIL;
    }

    if (lock) {
#ifndef lock_fd
	/* only lock_by_name is possible */
	(void) sprintf(lfn, "lock/%s", spool_fn);
	lock_fn = lfn;
	if (lock_spoolfile(l_open) == FAIL) {
	    DEBUG2(DBG_SPOOL_MID, "open_spool: %s/%s: lock_by_name failed\n",
		   spool_dir, input_spool_fn);
	    (void) close(spoolfile);
	    *error = "lock_spoolfile() failed";
	    return FAIL;
	}
#else /* lock_fd */
	if (lock_by_name) {
	    (void) sprintf(lfn, "lock/%s", spool_fn);
	    lock_fn = lfn;
	    if (lock_spoolfile(l_open) == FAIL) {
		DEBUG2(DBG_SPOOL_MID, "open_spool: %s/%s: lock_spoolfile failed\n",
		       spool_dir, input_spool_fn);
		(void) close(spoolfile);
		*error = "lock_spoolfile() failed";
		return FAIL;
	    }
	} else {
	    if (lock_fd(spoolfile) == FAIL) {
		DEBUG2(DBG_SPOOL_MID, "open_spool: %s/%s: lock failed\n",
		       spool_dir, input_spool_fn);
		(void) close(spoolfile);
		*error = "lock_fd() failed";
		return FAIL;
	    }
	}
#endif /* lock_fd */
    }

    /*
     * Get statistics on spool file.
     */
    (void) fstat(spoolfile, &statbuf);

    /*
     * If the file has been removed, then don't process it.
     */
    if (statbuf.st_nlink == 0) {
	DEBUG2(DBG_SPOOL_MID,
	       "open_spool: %s/%s: spool file was removed\n",
	       spool_dir, input_spool_fn);
	close_spool();
	return FAIL;
    }

    /*
     * now that the file itself is locked, we need to setup the
     * message_id and initialize the msg_buf pointers so that
     * read_spool will be called the first time the GETSPOOL
     * macro is called.
     */
    build_message_id();
    if (msg_buf == NULL) {
	msg_buf = xmalloc(message_bufsiz);
	end_msg_buf = msg_buf + message_bufsiz;
    }
    msg_max = msg_buf;
    msg_ptr = msg_buf;
    (void) fstat(spoolfile, &statbuf);
    msg_size = statbuf.st_size;

    if (lock) {
	DEBUG2(DBG_SPOOL_LO, "opened and locked spool file %s/%s\n",
	       spool_dir, input_spool_fn);
    } else {
	DEBUG2(DBG_SPOOL_LO, "opened spool file %s/%s\n",
	       spool_dir, input_spool_fn);
    }
    return SUCCEED;
}


/*
 * close_spool - unlock and close the spool file
 */
void
close_spool()
{
    DEBUG(DBG_SPOOL_HI, "close_spool called\n");
    if (lock_fn) {
	DEBUG2(DBG_SPOOL_HI, "close_spool: unlinking %s/%s\n",
	       spool_dir, lock_fn);
	(void) unlink(lock_fn);
    }
    (void) close(spoolfile);
    spoolfile = -1;
    spool_fn = NULL;
    input_spool_fn = NULL;
}

/*
 * unlink_spool - unlock, close and unlink the spool file
 */
void
unlink_spool()
{
    extern long time();
    char *save_input_spool_fn = input_spool_fn;

    DEBUG(DBG_SPOOL_HI, "unlink_spool called\n");
    if (message_date() == time((long *)0)) {
	DEBUG(DBG_SPOOL_HI, "pausing to prevent spool file clash\n");
	sleep(2);
    }
    if (save_input_spool_fn) {
	DEBUG2(DBG_SPOOL_HI, "unlink_spool: unlinking: %s/%s\n",
	       spool_dir, save_input_spool_fn);
	(void) unlink(save_input_spool_fn);
    }
    close_spool();
}

/*
 * defer_message - put the spool file in a defer directory
 *
 * some processing errors may need attention from the postmaster,
 * and may be correctable through a change in configuration, or other
 * such things.  Rather than mail such problems to the sender or to
 * the postmaster, put them in a special (error/) directory.  When
 * the problem that caused the error is resolved, the postmaster can
 * then simply move the spool file back into the spool directory and
 * delivery will be reattempted.
 */
void
defer_message()
{
    char error_fn[sizeof("error/") + SPOOL_FN_LEN];
    int success;
    char *save_input_spool_fn = input_spool_fn;
    char *save_spool_fn = spool_fn;

    (void) sprintf(error_fn, "error/%s", spool_fn);
    spool_fn = NULL;
    success = link(save_input_spool_fn, error_fn);
    if (success < 0 && auto_mkdir && errno == ENOENT) {
	DEBUG1(DBG_SPOOL_LO, "make directory %s/error\n", spool_dir);
	(void) mkdir("error", auto_mkdir_mode);
	success = link(save_input_spool_fn, error_fn);
    }
    if (success < 0) {
	write_log(LOG_PANIC|LOG_SYS,
		  "defer_message: error linking %s/%s to error/%s: %s",
		  spool_dir, save_input_spool_fn, spool_fn, strerror(errno));
    } else {
	if (errfile) {
	    write_log(LOG_TTY|LOG_SYS, "mail moved to %s/error/%s",
		      spool_dir, save_spool_fn);
	}
	input_spool_fn = save_input_spool_fn;
	spool_fn = save_spool_fn;
	unlink_spool();
    }
}


/*
 * seek_spool - seek to the specified absolute spool file offset
 *
 * causes the aligned block around that point to be read into
 * msg_buf.
 */
int
seek_spool(offset)
    register off_t offset;
{
    DEBUG1(DBG_SPOOL_HI, "seek_spool(%ld) called\n", (long) offset);
    if (offset > msg_size) {
	/* attempt to seek past the end of message */
	msg_ptr = msg_max + 1;		/* will cause EOF from GETSPOOL */
	DEBUG(DBG_SPOOL_LO, "seek_spool failed, seek past end of file\n");
	return FAIL;
    }

    /* is the offset in the current block? */
    if (msg_foffset <= offset && offset < msg_foffset + (msg_max - msg_buf)) {
	/* yes, just adjust the current loc pointer */
	msg_ptr = msg_buf + (offset - msg_foffset);
	return SUCCEED;
    }

    /* compute the beginning of the block containing the offset */
    msg_foffset = offset - offset%message_bufsiz;

    /* actually read in the message */
    (void) lseek(spoolfile, msg_foffset, SEEK_SET);
    /* set the buffer to zero-length so read_spool won't advance foffset */
    msg_max = msg_buf;
    if (read_spool() == FAIL) {
	return FAIL;
    }

    /* set up the ptr to point to the byte for the offset */
    msg_ptr = msg_buf + (offset - msg_foffset);
    if (msg_ptr > msg_max) {
	DEBUG(DBG_SPOOL_LO, "seek_spool failed, read came up short\n");
	return FAIL;
    }
    return SUCCEED;
}

/*
 * tell_spool - return the current offset in the spool file.
 */
off_t
tell_spool()
{
    return msg_foffset + msg_ptr - msg_buf;
}


/*
 * send_spool - write the spool file to a stdio FILE pointer
 *
 * send the spool file to an open file, starting at the current
 * offset and ending at the end of the spool file.  If PUT_DOTS is set
 * then use the hidden dot algorithm, prepending a dot to each
 * line that begins with a dot.  If PUT_CRLF is set than put a carriage
 * return before each newline.  If UNIX_FROM_HACK then put > in front of
 * any line beginning with From.
 *
 * return SUCCEED, READ_FAIL or WRITE_FAIL.
 */
int
send_spool(f, flags)
    register FILE *f;			/* file to write on */
    long flags;				/* transport flags */
{
    register char *p;
    int eof = FALSE;			/* TRUE if found end of file */
    register int last_c = '\n';
    long dot = flags & PUT_DOTS;
    long crlf = flags & PUT_CRLF;
    long uucp_hack = flags & UNIX_FROM_HACK;

    DEBUG(DBG_SPOOL_HI, "send_spool called\n");
    while (!eof) {
	for (p = msg_ptr; p < msg_max; p++) {
	    if (last_c == '\n') {
		if (dot && *p == '.') {
		    putc('.', f);		/* hidden dot algorithm */
		}
		if (uucp_hack && *p == 'F') {
		    int ct = msg_max - p;

		    /* look ahead to see if this line begins with From */
		    if (ct > sizeof("From ") - 1) {
			if (strncmp("From ", p, sizeof("From ") - 1) == 0) {
			    putc('>', f);
			}
		    } else if (strncmp("From ", p, ct) == 0) {
			/*
			 * we have to look ahead to the next block to
			 * determine if this is a From, but make sure
			 * there is another block, first.
			 */
			if (msg_foffset + (msg_max - msg_buf) < msg_size) {
			    int i;

			    msg_ptr = p;
			    if (read_spool() == FAIL) {
				return READ_FAIL;
			    }
			    p = msg_ptr;
			    if (strncmp("From " + ct, p,
					sizeof("From ") - ct - 1) == 0)
			    {
				putc('>', f);
			    }
			    for (i = 0; i < ct; i++) {
				putc("From "[i], f);
			    }
			}
		    }
		}
	    }
	    if (*p == '\n' && crlf) {
		putc('\r', f);		/* ARPAnet CR/LF */
	    }
	    if (putc(last_c = *p, f) == EOF) {
		DEBUG(DBG_SPOOL_LO, "send_spool: write failed\n");
		return WRITE_FAIL;
	    }
	}
	msg_ptr = p;
	if (msg_foffset + (msg_max - msg_buf) < msg_size) {
	    if (read_spool() == FAIL) {
		return READ_FAIL;
	    }
	} else {
	    eof = TRUE;
	}
    }

    if (last_c != '\n') {
	if (crlf) {
	    putc('\r', f);
	}
	if (putc('\n', f) == EOF) {
	    DEBUG(DBG_SPOOL_LO, "send_spool: write failed\n");
	    return WRITE_FAIL;
	}
    }

    return SUCCEED;
}


/*
 * read_spool - read in a block from the spool file
 *
 * read a block starting at the current file position into msg_buf
 * and update the various associated variables.
 *
 * return SUCCEED or FAIL.
 */
int
read_spool()
{
    register int ct;

    /* advance the current file offset by the amount previously read */
    msg_foffset += msg_max - msg_buf;
    if (message_bufsiz > msg_size - msg_foffset) {
	/* the remaining size is less then a whole buffer */
	ct = msg_size - msg_foffset;
	if (ct < 0) {
	    errno = ENXIO;		/* reading beyond end of the file */
	    DEBUG(DBG_SPOOL_LO, "read_spool: read failed, past end of file");
	    return FAIL;
	}
	if (ct == 0) {
	    /* end of file */
	    msg_max = msg_ptr = msg_buf;
	    DEBUG(DBG_SPOOL_MID, "end of file on spool file\n");
	    return FAIL;
	}
    } else {
	/* there is enough data to fit in a complete buffer */
	ct = message_bufsiz;
    }

    /* now, where were we? */
    (void) lseek(spoolfile, msg_foffset, SEEK_SET);

    /* read in a buffer */
    ct = read(spoolfile, msg_buf, ct);
    if (ct < 0) {
	DEBUG1(DBG_SPOOL_LO, "read_spool: read of spool file failed: %s\n",
	       strerror(errno));
	return FAIL;
    }

    /* point to the end and beginning of the used region */
    msg_max = msg_buf + ct;
    msg_ptr = msg_buf;

    return SUCCEED;
}


/*
 * log_spool_errors - write spooling errors to system log
 *
 * while spooling, errors are not written out to the spool files.  This
 * avoid having to deal with the fact that those writes may fail.  Thus,
 * when finished spooling the message, we must write out any errors to the
 * panic file.
 */
void
log_spool_errors()
{
    struct log_msgs *next;

    DEBUG(DBG_SPOOL_HI, "log_spool_errors called, saving any errors from spool\n");
    while (log_msgs) {
	next = log_msgs->succ;
	write_log(LOG_PANIC, "%s", log_msgs->msg);
	xfree((char *)log_msgs);
	log_msgs = next;
    }
}

/*
 * log_message - save a message to be written to the log file
 *
 * the message is not in printf format, but filename and directory
 * information will be added to it.
 */
static void
log_message(m)
    char *m;				/* message */
{
    char *p, *q;			/* temp */
    register unsigned a;		/* how much to alloc */
    char *save;

    a = sizeof(*log_msgs) + strlen(m);
    if (spool_dir) {
	a += sizeof(", dir=") + strlen(spool_dir);
    }
    if (input_spool_fn) {
	a += sizeof(", spoolfile=") + strlen(input_spool_fn);
    }
    if (lock_fn) {
	a += sizeof(", lockfile=") + strlen(lock_fn);
    }
    *next_log_msg = (struct log_msgs*)xmalloc(a);
    (*next_log_msg)->succ = NULL;
    save = p = (*next_log_msg)->msg;
    next_log_msg = &(*next_log_msg)->succ;
    /* copy message */
    for (q = m; *q; *p++ = *q++) ;
    if (spool_dir) {
	/* put spool directory in message */
	for (q = ", dir="; *q; *p++ = *q++) ;
	for (q = spool_dir; *q; *p++ = *q++) ;
    }
    if (input_spool_fn) {
	/* put spoolfile name in message */
	for (q = ", spoolfile="; *q; *p++ = *q++) ;
	for (q = input_spool_fn; *q; *p++ = *q++) ;
    }
    if (lock_fn) {
	/* put lockfile name in message */
	for (q = ", lockfile="; *q; *p++ = *q++) ;
	for (q = lock_fn; *q; *p++ = *q++) ;
    }
    *p++ = '\0';
    DEBUG2(DBG_SPOOL_MID, "%s: %s\n", funct_name, save);
}


/*
 * set_alt_spool - set a primary or alternate spool directory
 *
 * step through the list of spool directories, advancing once per
 * call to set_alt_spool and resetting when the current spool directory
 * was not set by set_alt_spool.  Do a chdir(2) to the directory
 * set spool_dir and return SUCCEED on the first one for chdir
 * succeeds.  If none remain, return FAIL.
 */
static int
set_alt_spool(m)
    char *m;				/* message to put in log */
{
    static char altname[SIZE_FILNAM];
    static char *altlist = NULL;
    char *p;

    /* log the message using the old spool dir then set the new one */
    if (m) {
	log_message(m);
    }

    if (spool_dir != altname) {
	/* previous spool dir not one of ours, reset to start */
	altlist = spool_dirs;
    }
    spool_dir = altname;

    /* loop until we have a valid alternate spool directory, or none left */
    while (altlist && *altlist) {
	/* copy a directory name into altname, may end in ':' or nul */
	for (p = altname; *altlist && *altlist != ':'; *p++ = *altlist++) ;
	if (*altlist == ':') {
	    /* next try gets the next directory */
	    altlist++;
	}
	*p++ = '\0';			/* terminate directory name */

	if (altname[0] != '/') {
	    /* directory must begin with '/', ignore it */
	    log_message("config error: alt directory doesn't begin with /");
	    continue;
	}

	if (chdir(altname) < 0) {
	    /* Okay, try to make the directory and then chdir again */
	    if (auto_mkdir && errno == ENOENT) {
		DEBUG1(DBG_SPOOL_LO, "make directory %s\n", altname);
		(void) mkdir(altname, auto_mkdir_mode);
		if (chdir(altname) < 0) {
		    log_message("chdir failed");
		    continue;
		}
	    } else {
		log_message("chdir failed");
		continue;
	    }
	}

	/* we found a valid alternate directory */
	return SUCCEED;
    }

    /* we failed, *sigh* */
    return FAIL;
}

/*
 * new_grade - assign a new grade to the spool file
 *
 * assign a new grade, or precedence, code to the spool file.  This
 * involves linking to a new name with a different precedence character.
 * We go through build_spool_fn to compute new names and keep trying
 * new names until we have one that doesn't already exist.  If links
 * fail for some reason other than a name not existing then don't bother
 * trying to change the grade.
 *
 * return SUCCEED or FAIL, though a FAIL should be ignored as there is
 * no good way to recover and it is not generally that important.
 */
int
new_grade(grade)
    int grade;				/* grade character */
{
    /*
     * NOTE: the above must be the same size as the names
     * built by build_spool_fn
     */
    char save_spool_fn[SPOOL_FN_LEN + 1];
    char save_input_spool_fn[sizeof("input/") + SPOOL_FN_LEN + 1];
    char save_message_id[SPOOL_FN_LEN + 2];
    char save_lock_fn[sizeof("lock/") + SPOOL_FN_LEN + 1];

    DEBUG1(DBG_SPOOL_HI, "new_grade(%c) called\n", grade);
    /*
     * make a copy of the old names to use as args to link, and so that
     * the old names can be restored
     */
    (void) memcpy(save_spool_fn, spool_fn, sizeof(save_spool_fn));
    (void) memcpy(save_input_spool_fn, input_spool_fn,
		  sizeof(save_input_spool_fn));
    (void) memcpy(save_message_id, message_id, sizeof(save_message_id));
    if (lock_by_name) {
	(void) memcpy(save_lock_fn, lock_fn, sizeof(save_lock_fn));
    }

    /*
     * loop until we link both lock file and spool file to new names
     * and unlink the old ones.
     */
    for (;;) {
	build_spool_fn(grade);

	/* try to link the lock file first so that it is initially locked */
	if (lock_by_name && link(save_lock_fn, lock_fn) < 0) {
	    if (errno != EEXIST) {
		DEBUG1(DBG_SPOOL_LO, "new_grade: failed to link lock: %s\n",
		       strerror(errno));
		break;
	    }
	    DEBUG1(DBG_SPOOL_MID,
		   "new_grade: failed to link lock:%, try another\n",
		   strerror(errno));
	    continue;
	}

	/* now try to link the actual spool file */
	if (link(save_input_spool_fn, input_spool_fn) < 0) {
	    if (errno != EEXIST) {
		DEBUG1(DBG_SPOOL_LO, "new_grade: failed to link spool: %s\n",
		       strerror(errno));
		if (lock_by_name) {
		    (void) unlink(lock_fn);
		}
		break;
	    }
	    DEBUG1(DBG_SPOOL_MID,
		   "new_grade: failed to link spool file: %s, try another\n",
		   strerror(errno));
	    continue;
	}

	/* success, now unlink the old names */
	(void) unlink(save_input_spool_fn);
	if (lock_by_name) {
	    (void) unlink(save_lock_fn);
	}
	return SUCCEED;
    }

    /* we failed, copy the old names back */
    (void) memcpy(spool_fn, save_spool_fn, sizeof(save_spool_fn));
    (void) memcpy(input_spool_fn, save_input_spool_fn,
		  sizeof(save_input_spool_fn));
    (void) memcpy(message_id, save_message_id, sizeof(save_message_id));
    if (lock_by_name) {
	(void) memcpy(lock_fn, save_lock_fn, sizeof(save_lock_fn));
    }

    return FAIL;
}

/*
 * message_date - return the date that the message was spooled
 *
 * The message spool date is encoded in the actual Message Id as a
 * base 62 number.  Return the date as the number of seconds since
 * the epoch.
 */
long
message_date()
{
    long clck;
    register int digit;
    register char *p;

    clck = 0;
    for (p = message_id + 1; *p; p++) {
	if (isdigit(*p)) {
	    digit = *p - '0';
	} else if (isupper(*p)) {
	    digit = *p - ('A' - 10);
	} else if (islower(*p)) {
	    digit = *p - ('a' - 36);
	} else {
	    break;
	}
	clck = (clck * 62) + digit;
    }
    return clck;
}


#ifdef HAVE_DF_SPOOL
#ifdef HAVE_STATVFS
#include <sys/statvfs.h>
#else /* not HAVE_STATVFS */
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#else /* not HAVE_SYS_STATFS_H */
#include <sys/vfs.h>
#endif /* not HAVE_SYS_STATFS_H */
#endif /* not HAVE_STATVFS */

long
spool_max_free_space ()
{
    char *dirs = spool_dirs;
    char dirname[SIZE_FILNAM];
    char *p;
    long max_free_bytes = -1, free_bytes;
    int result;
#ifdef HAVE_STATVFS
    struct statvfs buf;
#else
    struct statfs buf;
#endif

    while (dirs && *dirs) {
	for (p = dirname; *dirs && *dirs != ':'; *p++ = *dirs++) ;
	if (*dirs == ':') {
	    /* next try gets the next directory */
	    dirs++;
	}
	*p++ = '\0';			/* terminate directory name */
#ifdef HAVE_STATVFS
	result = statvfs (dirname, &buf);
#else
	result = statfs (dirname, &buf, sizeof buf, 0);
#endif
	if (result == -1)
	    continue;
#ifdef HAVE_STATVFS
	free_bytes = buf.f_bfree * buf.f_frsize;
#else
	free_bytes = buf.f_bfree * buf.f_bsize;
#endif
	if (free_bytes > max_free_bytes)
	    max_free_bytes = free_bytes;
    }
    return max_free_bytes;
}
#else /* not HAVE_DF_SPOOL */
long
spool_max_free_space ()
{
    return -1;
}
#endif /* not HAVE_DF_SPOOL */

#ifdef STANDALONE

char *program = "spool";
char *spool_dirs = "/usr/spool/smail:/wd1h/spool/smail:/usr/tmp/spool/smail";
int debug = 0;
int message_bufsiz = MESSAGE_BUF_SIZE;
int spool_mode = 0444;
int spool_grade = 'C';
int lock_mode = 0444;

/*
 * standalone main for spooling subsystem.
 * usage:
 *	spool [-ddebug] [-msize] o|c[w][u] [filename]
 *
 * -d sets the debug level, -m sets message_bufsiz.  An arg of `o'
 * causes a spool file in the given filename to be locked, written to
 * standard out and closed.  An arg of `c' causes a spool file to be
 * read from standard input and put in a new spool file.  If `w'
 * follows `o' or `c' then write the spool file to standard out before
 * closing.  If `u' then unspool the message.
 */
void
main(argc, argv)
    int argc;
    char *argv[];
{
    char *p;
    int mode = 0;
    int dowrite = FALSE;
    int dounspool = FALSE;

    program = *argv++;
    --argc;

    while ((*argv)[0] == '-') {
	switch ((*argv)[1]) {
	case 'd':
	    debug = atoi(*argv + 2);
	    break;
	case 'm':
	    message_bufsiz = atoi(*argv + 2);
	    break;
	default:
	    stand_spool_usage();
	    /*NOTREACHED*/
	}
	argv++;
	--argc;
    }

    if (argc <= 0) {
	stand_spool_usage();
	/*NOTREACHED*/
    }
    for (p = *argv++, --argc; *p; p++) {
	switch (*p) {
	case 'o':
	    if (mode) {
		stand_spool_usage();
		/*NOTREACHED*/
	    }
	    mode = 'o';
	    break;
	case 'c':
	    if (mode) {
		stand_spool_usage();
		/*NOTREACHED*/
	    }
	    mode = 'c';
	    break;
	case 'w':
	    dowrite = TRUE;
	    break;
	case 'u':
	    dounspool = TRUE;
	    break;
	default:
	    stand_spool_usage();
	    /*NOTREACHED*/
	}
    }
    if (!mode) {
	stand_spool_usage();
	/*NOTREACHED*/
    }
    if (mode == 'o' && argc <= 0) {
	(void) fprintf(stderr, "%s: open mode requires filename\n", program);
	exit(EX_USAGE);
    }
    if (mode == 'o') {
	if (open_spool(*argv) < 0) {
	    (void) fprintf(stderr, "%s: open failed\n", program);
	    exit(EX_OSFILE);
	}
    } else {
	register int c;

	if (creat_spool(*argv) < 0) {
	    (void) fprintf(stderr, "%s: creat failed\n", program);
	    exit(EX_OSFILE);
	}
	while ((c = getchar()) != EOF) {
	    if (c == '@') {
		force_write_error = TRUE;
	    }
	    if (PUTSPOOL(c) == FAIL) {
		(void) fprintf(stderr, "%s: write failed\n", program);
		exit(EX_IOERR);
	    }
	}
	if (write_spool() == FAIL) {
	    (void) fprintf(stderr, "%s: write failed\n", program);
	}
	log_spool_errors();
    }

    if (dowrite) {
	if (seek_spool((off_t) 0L) != SUCCEED) {
	    (void) fprintf(stderr, "seek_spool failed\n");
	}
	if (send_spool(stdout, TRUE, FALSE) != SUCCEED) {
	    (void) fprintf(stderr, "seek_spool failed\n");
	}
    }
    if (dounspool) {
	unlink_spool();
    } else {
	close_spool();
    }
    exit(EX_OK);
}

stand_spool_usage()
{
    (void) fprintf(stderr, "usage: %s [-ddebug] [-msize] o|c[w][u] [file]\n",
		   program);
    exit(EX_USAGE);
}

#endif	/* STANDALONE */
