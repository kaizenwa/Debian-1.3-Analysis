/* $Id: lock.c,v 1.7 1993/10/28 17:08:40 chip Exp $
 *
 * Mailbox locking.
 * Local hacks for mailbox access should be grafted here.
 *
 * $Log: lock.c,v $
 * Revision 1.7  1993/10/28  17:08:40  chip
 * Depend on _POSIX_VERSION instead of _PC_NAME_MAX.
 *
 * Revision 1.6  1991/12/20  18:38:20  chip
 * Lockfile creation only retries if errno is EEXIST.
 *
 * Revision 1.5  1991/08/26  18:00:32  chip
 * Make minimum name size explicit.
 *
 * Revision 1.4  1991/08/26  15:19:36  chip
 * Yet more NFS machinations.  Check for link() results, not status.
 *
 * Revision 1.3  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <sys/stat.h>
#include <errno.h>

/*
 * Support for the lockf() system call.
 */

#ifdef LOCK_LOCKF
#include <unistd.h>
#define SIMPLE_LOCK "lockf"
#define LOCKFD(fd)	lockf(fd, F_LOCK, 0L)
#define UNLOCKFD(fd)	lockf(fd, F_ULOCK, 0L)
#endif

/*
 * Support for the flock() system call.
 */

#ifdef LOCK_FLOCK
#include <sys/file.h>
#define SIMPLE_LOCK "flock"
#define LOCKFD(fd)	flock(fd, LOCK_EX)
#define UNLOCKFD(fd)	flock(fd, LOCK_UN)
#endif

/*
 * Support for the locking() system call.
 */

#ifdef LOCK_LOCKING
#include <sys/locking.h>
#define SIMPLE_LOCK "locking"
#define LOCKFD(fd)	locking(fd, LK_LOCK, 0L)
#define UNLOCKFD(fd)	locking(fd, LK_UNLCK, 0L)
#endif

/*
 * Local functions.
 */

#ifdef ML_DOTLOCK
static char *dotlock_name();
#endif
#ifdef ML_DOTMLK
static char *dotmlk_name();
#endif

/*
 * Local data.
 */

#define MIN_NAMESIZE 8		/* Minimum reasonable filename size limit. */

/*----------------------------------------------------------------------
 * Lock a mailbox by fd.
 */

int
mbox_fdlock(fd)
int fd;
{
#ifdef ML_KERNEL
    return k_lock(fd);
#else
    return 0;
#endif
}

/*----------------------------------------------------------------------
 * Unlock a mailbox by fd.
 */

int
mbox_fdunlock(fd)
int fd;
{
#ifdef ML_KERNEL
    return k_unlock(fd);
#else
    return 0;
#endif
}

/*----------------------------------------------------------------------
 * Lock a mailbox by name.
 *
 * This code looks quite hairy with all the ifdefs.  In fact, the only
 * somewhat strange thing here is that neither, either, or both of
 * ML_DOTLOCK and ML_DOTMLK may be defined, and we have to allow for it.
 */

int
mbox_nlock(name)
char *name;
{
#ifdef ML_DOTLOCK
    char *dotlock;
#endif
#ifdef ML_DOTMLK
    char *dotmlk;
#endif

#ifdef SAFE_UPDATE
    if (strcmp(basename(name), MBX_UNDEL) == 0)
	return 0;
#endif

#ifdef ML_DOTLOCK
    if ((dotlock = dotlock_name(name)) == NULL
	|| create_lockfile(dotlock) < 0)
	return -1;
#endif	/* ML_DOTLOCK */

#ifdef ML_DOTMLK
    if ((dotmlk = dotmlk_name(name)) == NULL
	|| create_lockfile(dotmlk) < 0)
    {
#ifdef ML_DOTLOCK
	(void) remove_lockfile(dotlock);	/* don't leave me hanging */
#endif
	return -1;
    }
#endif	/* ML_DOTMLK */

    return 0;
}

/*----------------------------------------------------------------------
 * Unlock a mailbox by name.
 */

int
mbox_nunlock(name)
char *name;
{
    int ret = 0;

#ifdef ML_DOTLOCK
    char *dotlock;
#endif
#ifdef ML_DOTMLK
    char *dotmlk;
#endif

#ifdef SAFE_UPDATE
    if (strcmp(name, MBX_UNDEL) == 0)
	return 0;
#endif

#ifdef ML_DOTLOCK
    if ((dotlock = dotlock_name(name)) == NULL
	|| remove_lockfile(dotlock) < 0)
	ret = -1;
#endif	/* ML_DOTLOCK */

#ifdef ML_DOTMLK
    if ((dotmlk = dotmlk_name(name)) == NULL
	|| remove_lockfile(dotmlk) < 0)
	ret = -1;
#endif	/* ML_DOTMLK */

    return ret;
}

/*----------------------------------------------------------------------
 * Lock and unlock the Deliver logs.
 *
 * If kernel locking and modern open() are available, these
 * routines are no-ops, since logs will be kernel-locked before
 * they are written.
 */

int
log_lock()
{
#ifdef SAFE_UPDATE
    return 0;
#else
    return create_lockfile(LOGLOCK);
#endif
}

int
log_unlock()
{
#ifdef SAFE_UPDATE
    return 0;
#else
    return remove_lockfile(LOGLOCK);
#endif
}

/*----------------------------------------------------------------------
 * Return the name of the appropriate ".lock" file for a mailbox.
 */

#ifdef ML_DOTLOCK

static char *
dotlock_name(name)
char *name;
{
    static char *lname = NULL;
    static unsigned lsize = 0;
    char *p;
    unsigned n, maxlen;

    n = strlen(name);
    if (lsize < n + 8)
    {
	if (lname)
	    free(lname);
	lsize = n + 32;
	lname = zalloc(lsize);
    }

    /* Figure maximum filename length in the given directory. */

    maxlen = MAX_NAMESIZE;

#ifdef _POSIX_VERSION
    {
	long pc;

	(void) strcpy(lname, name);
	(void) strcpy(basename(lname), ".");
	if ((pc = pathconf(lname, _PC_NAME_MAX)) >= MIN_NAMESIZE)
	    maxlen = pc;
    }
#endif

    /* We want as much of `basename.lock' as will fit in maxlen characters. */

    (void) strcpy(lname, name);
    p = basename(lname);
    if ((n = strlen(p)) > (maxlen - 5))
	n = maxlen - 5;
    (void) strcpy(p + n, ".lock");

    return lname;
}

#endif	/* ML_DOTLOCK */

/*----------------------------------------------------------------------
 * Return the name of the appropriate ".mlk" file for a mailbox.
 */

#ifdef ML_DOTMLK

static char *
dotmlk_name(name)
char *name;
{
    static char lname[MAX_NAMESIZE + 16];
    char *p, *d;
    int i;

    /*
     * To explain the below:  If we ass_u_me that MAX_NAMESIZE is 14,
     * then this code is like `printf(lname, "/tmp/%.10s.mlk", ...)'.
     * In other words, we want as much of `basename.mlk' as will fit
     * in a string MAX_NAMESIZE long.
     */
    d = lname;
    for (p = "/tmp/"; *p;)
	*d++ = *p++;
    for (i = 0, p = basename(name); (i < MAX_NAMESIZE - 4) && (*p); ++i)
	*d++ = *p++;
    (void) strcpy(d, ".mlk");

    return lname;
}

#endif	/* ML_DOTMLK */

/*----------------------------------------------------------------------
 * Create a lockfile.
 */

int
create_lockfile(name)
char *name;
{
#ifndef SAFE_CREATE
    static char *othername;
    struct stat st1, st2;
#endif
    int fd, tries, errno_save;

#ifndef SAFE_CREATE

    if (othername)
	free(othername);
    if ((othername = unique(name)) == NULL)
    {
	error("can't generate unique temp for %s", name);
	return -1;
    }
    if ((fd = creat(othername, 0)) == -1)
    {
	syserr("can't create %s", othername);
	return -1;
    }
    (void) close(fd);
    if (verbose)
	message("created pre-lockfile %s\n", othername);

#endif /* not SAFE_CREATE */

    for (tries = 0; tries < 10; ++tries)
    {
	if (tries)
	    snooze(3);

#ifdef SAFE_CREATE

	if ((fd = open(name, O_RDWR | O_CREAT | O_EXCL, 0)) >= 0)
	{
	    if (verbose)
		message("created lockfile %s\n", name);
	    (void) close(fd);
	    return 0;
	}

	/* The only error that can be retried is: File exists. */

	if ((errno_save = errno) != EEXIST)
	    break;

#else	/* not SAFE_CREATE */

	/*
	 * KLUDGE ALERT
	 * NFS can report failure on successful operations if those
	 * operations are unrepeatable.  Such is link().  Therefore,
	 * we ignore the error status and check identities.
	 */

	(void) link(othername, name);

	if (stat(othername, &st1) == 0
	 && stat(name, &st2) == 0
	 && st1.st_nlink == 2
	 && st2.st_nlink == 2
	 && st1.st_dev == st2.st_dev
	 && st1.st_ino == st2.st_ino)
	{
	    if (verbose)
		message("created lockfile %s\n", name);
	    if (unlink(othername) == -1)
		syserr("can't remove %s", othername);
	    return 0;
	}

	/* We can't trust errno; so, assume the most benign error. */

	errno_save = EEXIST;

#endif	/* not SAFE_CREATE */

	if (verbose && (tries == 0))
	    message("waiting to create %s\n", name);
    }

#ifndef SAFE_CREATE
    unlink(othername);
#endif

    errno = errno_save;
    syserr("can't create lockfile %s", name);
    return -1;
}

/*----------------------------------------------------------------------
 * Remove a lockfile.
 */

int
remove_lockfile(name)
char *name;
{
    if (unlink(name) == -1)
    {
	syserr("can't remove lockfile %s", name);
	return -1;
    }

    if (verbose)
	message("removed lockfile %s\n", name);

    return 0;
}

/*----------------------------------------------------------------------
 * Lock a file descriptor using kernel locking.
 */

int
k_lock(fd)
int fd;
{
#ifdef LOCK_FCNTL
    struct flock fl;

    fl.l_type = F_WRLCK;
    fl.l_whence = 0;
    fl.l_start = 0L;
    fl.l_len = 0L;

    if (fcntl(fd, F_SETLKW, &fl) == -1)
    {
	syserr("can't lock with fcntl()");
	return -1;
    }

    if (verbose)
	message("locked with fcntl()\n");
#endif	/* LOCK_FCNTL */

#ifdef SIMPLE_LOCK
    long pos;

    if ((pos = lseek(fd, 0L, 0)) == -1)
    {
	syserr("can't seek before %s()", SIMPLE_LOCK);
	return -1;
    }
    if (LOCKFD(fd) == -1)
    {
	syserr("can't lock with %s()", SIMPLE_LOCK);
	return -1;
    }
    if (lseek(fd, pos, 0) == -1)
    {
	syserr("can't seek after %s()", SIMPLE_LOCK);
	return -1;
    }

    if (verbose)
	message("locked with %s()\n", SIMPLE_LOCK);
#endif	/* SIMPLE_LOCK */

    /* Default: success */
    return 0;
}

/*----------------------------------------------------------------------
 * Unlock a file descriptor using kernel locking.
 */

int
k_unlock(fd)
int fd;
{
#ifdef LOCK_FCNTL
    struct flock fl;

    fl.l_type = F_UNLCK;
    fl.l_whence = 0;
    fl.l_start = 0L;
    fl.l_len = 0L;

    if (fcntl(fd, F_SETLKW, &fl) == -1)
    {
	syserr("can't unlock with fcntl()");
	return -1;
    }

    if (verbose)
	message("unlocked with fcntl()\n");
#endif	/* LOCK_FCNTL */

#ifdef SIMPLE_LOCK
    long pos;

    if ((pos = lseek(fd, 0L, 0)) == -1)
    {
	syserr("can't seek before %s()", SIMPLE_LOCK);
	return -1;
    }
    if (UNLOCKFD(fd) == -1)
    {
	syserr("can't unlock with %s()", SIMPLE_LOCK);
	return -1;
    }
    if (lseek(fd, pos, 0) == -1)
    {
	syserr("can't seek after %s()", SIMPLE_LOCK);
	return -1;
    }

    if (verbose)
	message("unlocked with %s()\n", SIMPLE_LOCK);
#endif	/* SIMPLE_LOCK */

    /* Default: success */
    return 0;
}
