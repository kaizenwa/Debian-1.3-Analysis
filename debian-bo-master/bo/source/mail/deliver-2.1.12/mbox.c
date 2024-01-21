/* $Id: mbox.c,v 1.10 1993/10/28 16:49:51 chip Exp $
 *
 * Finally!  Put the message in the specified mailbox(es).
 *
 * $Log: mbox.c,v $
 * Revision 1.10  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.9  1991/10/30  21:50:44  chip
 * Add support for MMDF.
 *
 * Revision 1.8  1991/08/26  17:42:24  chip
 * Don't declare errno.
 *
 * Revision 1.7  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.6  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.5  1991/05/29  17:23:32  chip
 * Rearrange global vars.  Make "sysmb_gid" global; set it in main().
 *
 * Revision 1.4  1991/05/21  17:49:10  chip
 * Take a shortcut when opening a user mailbox on a system
 * with a three-parameter open.
 *
 * Revision 1.3  1991/05/21  17:08:09  chip
 * Disallow symbolic links as system mailboxes.
 *
 * Revision 1.2  1991/05/13  22:04:01  chip
 * System mailbox must be owned by user.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <sys/stat.h>
#include <errno.h>

#ifdef S_IFLNK
# ifndef S_ISLNK
#  define S_ISLNK(mode)  (((mode) & S_IFMT) == S_IFLNK)
# endif
#endif

#ifdef S_ISLNK
# define SYMLINK
#endif

/*
 * Local functions.
 */

static void prog_one();
static void mbox_one();
static int mbox_write();
static int mbox_open();
static int mbox_create();
static int mbox_fdsafe();
static int mbox_fdwrite();
static int mbox_delim();
static int mbox_ewrite();

/*----------------------------------------------------------------------
 * Deliver mail to all valid program (pipe) destinations.
 * Return count of programs to which we tried to write.
 */

int
prog_deliver()
{
    DEST *d;
    int progcount;

    progcount = 0;
    for (d = first_dest(); d; d = next_dest(d))
    {
	if (d->d_state != ST_WORKING)
	    continue;

	if (d->d_class == CL_PROG)
	{
	    prog_one(d);
	    ++progcount;
	}
    }

    return progcount;
}

/*----------------------------------------------------------------------
 * Deliver mail to one program.
 */

static void
prog_one(d)
DEST *d;
{
    CONTEXT *ct;
    char *av[4];
    int fd;

    if (printaddrs)
	(void) printf("%s|%s\n", d->d_name, d->d_param);

    if (dryrun)
    {
	d->d_state = ST_DONE;
	return;
    }

    if ((ct = name_context(d->d_name)) == NULL)
    {
	dest_err(d, E_NSUSER);
	return;
    }

    if (!ok_context(eff_uid, real_uid, real_gid, ct))
    {
	dest_err(d, E_CTPERM);
	return;
    }

    av[0] = SHELL;
    av[1] = "-c";
    av[2] = d->d_param;
    av[3] = NULL;
    if ((fd = ct_openv(ct, av[0], av, O_WRONLY)) == -1)
    {
	dest_err(d, E_PIPE);
	return;
    }

    (void) lseek(tfd[T_HDR], 0L, 0);
    (void) lseek(tfd[T_BODY], 0L, 0);
    if (copyfd(tfd[T_HDR], fd) < 0 || copyfd(tfd[T_BODY], fd) < 0)
	dest_err(d, E_PIPE);

    if (ct_close(fd))
	dest_err(d, E_PROG);
    else
	d->d_state = ST_DONE;
}

/*----------------------------------------------------------------------
 * Deliver mail to all valid mailbox destinations.
 * Return count of mailboxes to which we tried to write.
 */

int
mbox_deliver()
{
    DEST *d;
    int mboxcount;

    mboxcount = 0;
    for (d = first_dest(); d; d = next_dest(d))
    {
	if (d->d_state != ST_WORKING)
	    continue;

	if (d->d_class == CL_USER || d->d_class == CL_MBOX)
	{
	    mbox_one(d);
	    ++mboxcount;
	}
    }

    return mboxcount;
}

/*----------------------------------------------------------------------
 * Deliver mail to one mailbox destination.
 */

static void
mbox_one(d)
DEST *d;
{
    CONTEXT *ct;
    int ret = 0;

    if (printaddrs)
    {
	(void) printf("%s", d->d_name);
	if (d->d_class == CL_MBOX)
	    (void) printf(":%s", d->d_param);
	(void) printf("\n");
    }

    if (dryrun)
    {
	d->d_state = ST_DONE;
	return;
    }

    if ((ct = name_context(d->d_name)) == NULL)
    {
	dest_err(d, E_NSUSER);
	return;
    }

    if (!ok_context(eff_uid, real_uid, real_gid, ct))
    {
	dest_err(d, E_CTPERM);
	return;
    }

    if (d->d_class == CL_MBOX)
    {
	if (sfork() == 0)
	{
	    if (become(ct, !boxdelivery) < 0)
		_exit(1);
	    if (mbox_write(d->d_param, ct, FALSE) < 0)
		_exit(1);
	    _exit(0);
	}

	if (await_child() != 0)
	    ret = -1;
    }
    else
    {
	char mailbox[100];

	(void) sprintf(mailbox, "%s/%s",
#ifdef MBX_DIR
		       MBX_DIR, d->d_name
#else
		       ct->ct_home, MBX_NAME
#endif
	    );

	if (mbox_write(mailbox, ct, TRUE) < 0)
	    ret = -1;
    }

    if (ret >= 0)
	d->d_state = ST_DONE;
    else
	dest_err(d, E_MBOX);
}

/*----------------------------------------------------------------------
 * Write mail to the named mailbox.
 * If we have to create the mailbox, give it to the specified user.
 * If "is_sys" is true, then we're writing to a system mailbox.
 * If kernel locking and modern open() are available, no file locking
 * is performed on delivery of undelivered mail.
 */

static int
mbox_write(mailbox, ct, is_sys)
char *mailbox;
CONTEXT *ct;
int is_sys;
{
    int fd;
    int ret = -1;

    if (verbose)
    {
	message("As %s, delivering to %s mailbox %s\n",
		ct->ct_name, (is_sys ? "system" : "user"), mailbox);
    }

    if (mbox_nlock(mailbox) == -1)
	return -1;

    if ((fd = mbox_open(mailbox, ct, is_sys)) >= 0)
    {
	if (mbox_fdlock(fd) == 0)
	{
	    /* Seek to end (n.b. mailbox may be a special file). */

	    (void) lseek(fd, 0L, 2);

	    /* If given fd is safe, copy message to it. */

	    if (mbox_fdsafe(fd, ct, is_sys, mailbox)
		&& mbox_fdwrite(fd, mailbox) == 0)
		ret = 0;

	    if (mbox_fdunlock(fd) < 0)
		ret = -1;
	}

	if (close(fd) == -1)
	{
	    syserr("can't close %s", mailbox);
	    ret = -1;
	}
    }

    if (mbox_nunlock(mailbox) < 0)
	ret = -1;

    if (verbose)
    {
	if (ret >= 0)
	    message("wrote message to %s\n", mailbox);
    }

    return ret;
}

/*----------------------------------------------------------------------
 * Open the given mailbox for writing.
 */

static int
mbox_open(mailbox, ct, is_sys)
char *mailbox;
CONTEXT *ct;
int is_sys;
{
    int fd, tries;

#ifdef SAFE_CREATE

    /*
     * When opening a user mailbox on a system with a three-parameter
     * open, a simple shortcut is available.
     */

    if (!is_sys)
    {
	int um;

	um = umask(0);		/* save umask */
	fd = open(mailbox, O_WRONLY | O_CREAT, MBX_MODE);
	(void) umask(um);	/* restore umask; ass_u_me errno unchanged */

	if (fd == -1)
	    syserr("can't open %s", mailbox);
	return fd;
    }

#endif /* SAFE_CREATE */

    /*
     * On a system without a three-parameter open, or when opening 
     * a system mailbox, we must either (1) open it when it already
     * exists, or (2) create it under a temporary name, fix its
     * permissions, and then rename it.
     */

    tries = 0;
    for (;;)
    {
	int createrr;

	if ((fd = open(mailbox, O_WRONLY)) >= 0)
	    return fd;

	if (errno != ENOENT)
	{
	    syserr("can't open %s", mailbox);
	    break;
	}

	if ((fd = mbox_create(mailbox, ct, is_sys, &createrr)) >= 0)
	    return fd;

	/*
	 * Only retriable error is "file already there."
	 * Other errors indicate that something is very wrong
	 * (perhaps a symlink pointing to deep space).
	 */

	if (createrr != EEXIST || ++tries >= 3)
	{
	    if ((errno = createrr) != 0)
		syserr("can't create %s", mailbox);
	    break;
	}

	/* Wait a little before retrying. */

	snooze(2);
    }

    /* Loop exit implies failure. */

    return -1;
}

/*----------------------------------------------------------------------
 * Create a mailbox that doesn't exist.
 * On error return, createrr is possibly transient error code.
 * Permanent error codes are reported, not returned.
 */

static int
mbox_create(mailbox, ct, is_sys, createrr)
char *mailbox;
CONTEXT *ct;
int is_sys;
int *createrr;
{
    struct stat st;
    char *mbt;
    int fd, um, mbox_uid, mbox_gid;

    /* Ass_u_me no problem. */
    *createrr = 0;

    /*
     * Note that in the absence of name locking, it is necessary
     * to create a mailbox under a unique name, set the ownership
     * correctly, and then link it to its proper name.
     */

    if ((mbt = unique(mailbox)) == NULL)
    {
	*createrr = EEXIST;
	return -1;
    }

    /*
     * Calculate correct mailbox ownership.
     */

    mbox_uid = ct->ct_uid;
    mbox_gid = ct->ct_gid;

#ifdef MBX_GROUP
    if (is_sys)
	mbox_gid = sysmb_gid;
#endif

    um = umask(0);		/* save umask */
#ifdef SAFE_CREATE
    fd = open(mbt, O_WRONLY | O_CREAT | O_EXCL, MBX_MODE);
#else
    /* no protection against temp file collision -- what can be done? */
    fd = creat(mbt, MBX_MODE);
#endif
    (void) umask(um);		/* restore umask; ass_u_me errno unchanged */

    if (fd == -1)
    {
	syserr("can't create mailbox temp %s", mbt);
	free(mbt);
	return -1;
    }

    /* Determine current modes of temp mailbox */

    if (fstat(fd, &st) == -1)
    {
	syserr("can't fstat %s after creation?!", mbt);
	(void) close(fd);
	(void) unlink(mbt);
	free(mbt);
	return -1;
    }

    /* Change mailbox ownership if it's not already correct */

    if ((st.st_uid != mbox_uid || st.st_gid != mbox_gid)
	&& chown(mbt, mbox_uid, mbox_gid) == -1)
    {
	/* Print a message, but keep going? XXX */
	syserr("can't chown %s to %d,%d", mbt, mbox_uid, mbox_gid);
    }

    /* Link temporary name to permanent name. */

    if (link(mbt, mailbox) == -1)
    {
	/* Get rid of mailbox temp that we couldn't link. */

	*createrr = errno;
	(void) close(fd);
	(void) unlink(mbt);
	free(mbt);
	return -1;
    }

    /* Unlink extra name and return open fd. */

    (void) unlink(mbt);
    free(mbt);

    return fd;
}

/*----------------------------------------------------------------
 * Verify that the given open mailbox is safe to write to.
 */

static int
mbox_fdsafe(fd, ct, is_sys, mailbox)
int fd;
CONTEXT *ct;
int is_sys;
char *mailbox;
{
    struct stat st;
#ifdef SYMLINK
    struct stat linkst;
#endif

    /*
     * Non-system mailboxes need not be checked for security,
     * because they are opened and written in user context.
     */

    if (!is_sys)
	return TRUE;

    /* Get statistics on open mailbox. */

    if (fstat(fd, &st) == -1)
    {
	syserr("can't fstat open system mailbox %s", mailbox);
	return FALSE;
    }

#ifdef SYMLINK

    /* Get statistics on mailbox path. */

    if (lstat(mailbox, &linkst) == -1)
    {
	syserr("can't lstat open system mailbox %s", mailbox);
	return FALSE;
    }

    /* Symlinks aren't allowed. */

    if (S_ISLNK(linkst.st_mode))
    {
	error("system mailbox %s is a symbolic link", mailbox);
	return FALSE;
    }

    /* Avoid race condition in symlink check. */

    if (st.st_dev != linkst.st_dev || st.st_ino != linkst.st_ino)
    {
	error("system mailbox %s was renamed after open", mailbox);
	return FALSE;
    }

#endif /* SYMLINK */

    /* System mailboxes must be owned by the given user. */

    if (st.st_uid != ct->ct_uid)
    {
	error("system mailbox %s is not owned by %s",
	      mailbox, ct->ct_name);
	return FALSE;
    }

    /* System mailboxes must have exactly one hard link. */

    if (st.st_nlink != 1)
    {
	error("system mailbox %s has %d links",
	      mailbox, st.st_nlink);
	return FALSE;
    }

    /* No problems found. */

    return TRUE;
}

/*----------------------------------------------------------------
 * Write mail to the mailbox on the the given file descriptor.
 */

static int
mbox_fdwrite(fd, mailbox)
int fd;
char *mailbox;
{
    char buf[BUFSIZ];
    unsigned valid;
    int t, rd;
    char prevch;

    /* All checking is done!  Copy header and body. */

    for (t = T_HDR; t <= T_BODY; ++t)
    {
	if (lseek(tfd[t], 0L, 0) == -1)
	{
	    syserr("lseek in %s file %s",
		   ttype[t], tfile[t]);
	    return -1;
	}
    }

    if (mbox_delim(fd, FALSE, mailbox) < 0)
	return -1;

    if (copyfd(tfd[T_HDR], fd) < 0)
	return -1;

    valid = 0;
    prevch = '\n';
    while ((rd = read(tfd[T_BODY], buf + valid, sizeof(buf) - valid)) > 0)
    {
	unsigned written, scanned, n;

	valid += rd;

	written = 0;
	for (scanned = 0; scanned + FROMSIZE < valid; ++scanned)
	{
	    if (prevch == '\n' && skipfrom(buf + scanned))
	    {
		if (scanned > written
		    && mbox_ewrite(fd, buf + written,
				   scanned - written,
				   mailbox) == -1)
		    return -1;
		if (mbox_ewrite(fd, ">", 1, mailbox) == -1)
		    return -1;
		written = scanned;
	    }
	    prevch = buf[scanned];
	}

	if (written < scanned)
	{
	    if (mbox_ewrite(fd, buf + written, scanned - written,
			    mailbox) == -1)
		return -1;
	    written = scanned;
	}

	valid -= written;
	for (n = 0; n < valid; ++n)
	    buf[n] = buf[written + n];
    }

    if (rd == -1)
	syserr("read from %s file %s", ttype[T_BODY], tfile[T_BODY]);

    if (valid && mbox_ewrite(fd, buf, valid, mailbox) == -1)
	return -1;

    if (mbox_delim(fd, TRUE, mailbox) < 0)
	return -1;

    return 0;
}

/*----------------------------------------------------------------
 * Write a message delimiter.
 */

static int
mbox_delim(fd, done, mailbox)
int fd;
int done;
char *mailbox;
{
#ifdef MBX_MMDF
    static char mmdf_delim[] = { '\1', '\1', '\1', '\1', '\n' };
    return mbox_ewrite(fd, mmdf_delim, sizeof(mmdf_delim), mailbox);
#else
    return 0;
#endif
}

/*----------------------------------------------------------------
 * Write data to the given mailbox fd.
 * Report errors.
 */

static int
mbox_ewrite(fd, buf, size, mailbox)
int fd;
char *buf;
unsigned size;
char *mailbox;
{
    int wr;

    wr = write(fd, buf, size);
    if (wr == -1)
    {
	syserr("write to mailbox %s", mailbox);
	return -1;
    }
    if (wr != size)
    {
	error("short write to mailbox %s: %u instead of %u",
	      mailbox, (unsigned) wr, size);
	return -1;
    }
    return wr;
}
