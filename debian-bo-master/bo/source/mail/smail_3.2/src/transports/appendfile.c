/*
#ident	"@(#)smail/src/transports:RELEASE-3_2:appendfile.c,v 1.30 1996/02/26 18:39:08 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * appendfile.c:
 *	deliver mail by appending the message to a file.  Be very
 *	careful to keep things secure by checking writable of the
 *	file by an appropriate user and group.
 *
 *	Alternately, the appendfile can be used as a queueing driver
 *	which installs new files in a particular directory.  The
 *	driver can be used for very simple queueing if a `dir'
 *	attribute is given rather than a `file' attribute.
 *
 * Specifications for the appendfile transport driver:
 *
 *	private attribute data:
 *	    file (string):  a string which will be expanded to the filename
 *		to append the message to.  Some variable expansion will
 *		be done, using expand_string.
 *	    dir (string):  a string which will be expanded to a directory
 *		in which to put the message.  If this is given, then the
 *		appendfile driver will actually be considered a very simple
 *		queueing driver.
 *	    user (name):  the name of the user to setuid to in the child
 *		process.  If not specified then the addr structure is
 *		searched for a uid.  If none is found there, use the
 *		nobody_uid.
 *	    group (name):  the name of the group to setgid to in the child
 *		process.  The algorithm used for uid is used here as well.
 *	    prefix (string):  a string to be prepended before the message
 *		in the file.
 *	    suffix (string):  a string to be appended after the message
 *		in the file.
 *	    mode (integer):  defines the mode to give newly created files.
 *
 *	private attribute flags:
 *	    append_as_user: if set, default uid and gid are taken from
 *			    the addr structure.
 *	    expand_user: if set, expand username before expanding the
 *			 file name.  This is useful for the stock "file"
 *			 transport which requires ~ expansions, at a
 *			 minimum, on the username.
 *	    check_user: if set, verify that the $user expansion will not
 *			contain any `/' characters, which might cause a
 *			reference out of the desired directory.  if the
 *			verification fails, delivery to the address fails.
 *
 * NOTE:  the appendfile driver can only deliver to one address per call.
 *
 * NFS NOTE:
 *	If a file may be opened over NFS, care must be taken that smail is
 *	running as the desired user during the entire transaction of
 *	opening the mailbox, locking the mailbox and writing out th
 *	message.  At least this is true of NFS in SunOS3.x.  Define
 *	HAVE_SETEUID to get the necessary behavior.
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>
#include "defs.h"
#if defined(UNIX_SYS5) || defined(POSIX_OS)
# include <fcntl.h>
#endif
#if defined(UNIX_BSD) && !defined(POSIX_OS)
# include <sys/file.h>
#endif
#ifndef HAVE_BSD_NETWORKING
# undef HAVE_COMSAT
#endif
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../log.h"
#include "../spool.h"
#include "../child.h"
#include "../transport.h"
#include "../exitcodes.h"
#include "../dys.h"
#include "appendfile.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
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

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static void get_appendfile_ugid();
static time_t get_atime();
static void restore_atime();
#ifdef HAVE_COMSAT
static void notify_comsat();
#endif

static struct attr_table appendfile_attributes[] = {
    { "file", t_string, NULL, NULL, OFFSET(appendfile_private, file) },
    { "dir", t_string, NULL, NULL, OFFSET(appendfile_private, dir) },
    { "user", t_string, NULL, NULL, OFFSET(appendfile_private, user) },
    { "group", t_string, NULL, NULL, OFFSET(appendfile_private, group) },
    { "prefix", t_string, NULL, NULL, OFFSET(appendfile_private, prefix) },
    { "suffix", t_string, NULL, NULL, OFFSET(appendfile_private, suffix) },
    { "mode", t_mode, NULL, NULL, OFFSET(appendfile_private, mode) },
    { "append_as_user", t_boolean, NULL, NULL, APPEND_AS_USER },
    { "expand_user", t_boolean, NULL, NULL, APPEND_EXPAND_USER },
    { "check_user", t_boolean, NULL, NULL, APPEND_CHECK_USER },
    { "notify_comsat", t_boolean, NULL, NULL, APPEND_COMSAT },
};

static struct attr_table *end_appendfile_attributes = ENDTABLE(appendfile_attributes);



/*
 * tdp_appendfile - appendfile transport driver
 */
void
tpd_appendfile(addr, succeed, defer, fail)
    struct addr *addr;			/* recipient addresses for transport */
    struct addr **succeed;		/* successful deliveries */
    struct addr **defer;		/* defer until a later queue run */
    struct addr **fail;			/* failed deliveries */
{
    register struct appendfile_private *priv;
    struct transport *tp = addr->transport;
    char *fn;				/* expanded filename */
    char *temp_fn;			/* temp queue filename */
    FILE *f;				/* open file */
    char *user;				/* expanded username */
    char *save_user = NULL;		/* save old username */
    int uid;				/* uid for creating file */
    int gid;				/* gid for creating file */
    time_t atime_saved;			/* preserved access time */
#ifdef HAVE_COMSAT
    long msg_offset;			/* offset of mbox end */
#endif
#ifdef HAVE_SETEUID
    int save_gid;			/* saved gid from before setegid() */
#endif

    DEBUG1(DBG_DRIVER_HI, "tpd_appendfile called: addr = %s\n", addr->in_addr);
    priv = (struct appendfile_private *)tp->private;

    /* subject addresses to retry interval and duration limits */
    addr = retry_addr_before(addr, defer, fail);
    if (addr == NULL) {
	return;
    }

    if (tp->flags & APPEND_EXPAND_USER) {
	/* expand the username exactly once, if required */
	user = expand_string(addr->next_addr, addr,
			     (char *)NULL, (char *)NULL);
	if (user) {
	    save_user = addr->next_addr;
	    addr->next_addr = COPY_STRING(user);
	} else {
	    /*
	     * ERR_128 - username expansion failed
	     *
	     * DESCRIPTION
	     *      The expand_user attribute is set for the appendfile
	     *      driver but, the expand_string() encountered an error in
	     *      expansion.
	     *
	     * ACTIONS
	     *      The input addresses are failed and an error is sent to
	     *      the address owner or to the postmaster.  The parent
	     *      address is reported (if one exists), as it may be
	     *      significant.
	     *
	     * RESOLUTION
	     *      This is generally useful only for file-form addresses.
	     *      Thus, the errant address should be tracked down and
	     *      corrected.
	     */
	    register struct error *er;
	    register char *s;

	    if (addr->parent) {
		s = xprintf("transport %s: could not expand user (parent %s)",
			    tp->name,
			    addr->parent->in_addr);
	    } else {
		s = xprintf("transport %s: could not expand user",
			    tp->name);
	    }
	    er = note_error(ERR_NPOWNER|ERR_128, s);

	    insert_addr_list(addr, fail, er);
	    return;
	}
    }

    if (tp->flags & APPEND_CHECK_USER) {
	/*
	 * make sure the username can't cause an expansion to move to
	 * another directory
	 */
	if (index(addr->next_addr, '/')) {
	    /*
	     * ERR_129 - username contains /
	     *
	     * DESCRIPTION
	     *      If the check_user attribute is set, the `/' character is
	     *      illegal in usernames, as it can cause files to be
	     *      accessed in other than the configured directory.
	     *
	     * ACTIONS
	     *      The address is failed and an error is sent to the address
	     *      owner or to the sender.
	     *
	     * RESOLUTION
	     *      The sender or owner should ensure that any addresses
	     *      sent to this host do not contain the / character.
	     */
	    register struct error *er;

	    er = note_error(ERR_NSOWNER|ERR_129,
			    xprintf("transport %s: username contains /",
				    tp->name));
	    insert_addr_list(addr, fail, er);
	    return;
	}
    }

    /* build the expanded file name */
    if (priv->file) {
	fn = expand_string(priv->file, addr, (char *)NULL, (char *)NULL);
    } else if (priv->dir) {
	fn = expand_string(priv->dir, addr, (char *)NULL, (char *)NULL);
    } else {
	/*
	 * ERR_130 - file or dir attribute required
	 *
	 * DESCRIPTION
	 *      A file or dir attribute is required for transports that use
	 *      the appendfile attribute.
	 *
	 * ACTIONS
	 *      The message is deferred with a configuration error.
	 *
	 * RESOLUTION
	 *      The transport file should be corrected to specify a file or
	 *      directory for the transport entry.
	 */
	register struct error *er;

	er = note_error(ERR_CONFERR|ERR_130,
			xprintf("transport %s: file or dir attribute required",
				tp->name));
	insert_addr_list(addr, defer, er);
	return;
    }
    if (save_user) {
	/* restore the unexpanded username */
	xfree(addr->next_addr);
	addr->next_addr = save_user;
    }
    if (fn == NULL) {
	/*
	 * ERR_131 - failed to expand file or dir
	 *
	 * DESCRIPTION
	 *      Failed to expand the file or dir attribute into the
	 *      destination file or directory.
	 *
	 * ACTIONS
	 *      Defer the message with a configuration error.
	 *
	 * RESOLUTION
	 *      The postmaster should correct the transport entry to ensure
	 *      that the file or dir attribute is always expandable.
	 */
	register struct error *er;

	er = note_error(ERR_CONFERR|ERR_131,
			xprintf("transport %s: failed to expand %s",
				tp->name, priv->file));
	insert_addr_list(addr, defer, er);
	return;
    }
    if (fn[0] != '/') {
	/*
	 * ERR_132 - appendfile pathname not absolute
	 *
	 * DESCRIPTION
	 *      The file or directory name to which the message is to be
	 *      deliveried is a relative path.  This does not make sense in
	 *      the mailer.
	 *
	 * ACTIONS
	 *      This is most likely a configuration error, as it should not
	 *      be possible for a relative file-form address to be produced,
	 *      thus defer the message with a configuration error.
	 *
	 * RESOLUTION
	 *      Most likely, the transport file entry needs to be repaired
	 *      to ensure that the file or dir attribute yields an absolute
	 *      pathname.
	 */
	register struct error *er;

	er = note_error(ERR_CONFERR|ERR_132,
			xprintf("transport %s: pathname not absolute",
				tp->name));
	insert_addr_list(addr, defer, er);
	return;
    }

    /* get the user id and group id */
    get_appendfile_ugid(tp, addr, &uid, &gid);

    /*
     * special case /dev/null, and don't do any more if dont_deliver
     * is set
     */
    if (dont_deliver || EQ(fn, "/dev/null")) {
	insert_addr_list(addr, succeed, (struct error *)NULL);
	return;
    }

    /*
     * when appending, save (and later restore) the access time of the
     * file, so that mail readers can use access time vs. modify time
     * comparisons to determine if new mail has arrived.
     */

    if (priv->file) {
	atime_saved = get_atime(fn);
    }

    fn = COPY_STRING(fn);
#ifdef HAVE_SETEUID
    /*
     * If we are using NFS, then assume that the seteuid call is available
     * and become the desired user here.  It is not enough to merely be
     * the desired user when opening the file (what a pain!).
     */
    /* order is important here */
    save_gid = getegid();
    (void) setegid(gid);
    (void) seteuid(uid);

    f = NULL;
    if (priv->file) {
	int fd;

	/* open and lock the mailbox file */
	temp_fn = COPY_STRING(fn);	/* copy expanded string */
	fd = open(temp_fn, O_WRONLY|O_APPEND|O_CREAT, priv->mode);
	if (fd >= 0) {
	    f = fdopen(fd, "a");
	}
    } else {
	int fd;

	fn = COPY_STRING(fn);
	temp_fn = xprintf("%s/temp.%d", fn, getpid());
	fd = open(temp_fn, O_WRONLY|O_CREAT, priv->mode);
	if (fd >= 0) {
	    f = fdopen(fd, "a");
	}
    }
#else	/* not HAVE_SETEUID */
    if (priv->file) {
	/* open and lock the mailbox file */
	temp_fn = COPY_STRING(fn);	/* copy expanded string */
	f = fopen_as_user(fn, "a", uid, gid, priv->mode);
    } else {
	temp_fn = xprintf("%s/temp.%d", fn, getpid());
	f = fopen_as_user(temp_fn, "w", uid, gid, priv->mode);
    }
#endif	/* not HAVE_SETEUID */
    DEBUG1(DBG_DRIVER_LO, "  appendfile: write to file %s\n", temp_fn);

    if (f == NULL) {
	/*
	 * ERR_133 - appendfile failed to open file
	 *
	 * DESCRIPTION
	 *      The appendfile driver failed to open the output file.  The
	 *      reason for failure should be in errno.
	 *
	 * ACTIONS
	 *      Send a message to the address owner or the postmaster.  This
	 *      is not the kind of error the sender should have to deal
	 *      with.
	 *
	 * RESOLUTION
	 *      The pathnames produced by the transport from the input
	 *      address should be checked against the filesystem and
	 *      permissions (including directory search path permissions).
	 *      Keep in mind that opening a file is done within the context
	 *      of a particular user, possibly the nobody user, and that all
	 *      directories up to that point must be searchable by that user
	 *      or group, and that the last directory may need to be
	 *      writable.
	 */
	register struct error *er;

	er = note_error(ERR_NPOWNER|ERR_133,
			xprintf("transport %s: failed to open output file: %s",
				tp->name, strerror(errno)));
	insert_addr_list(addr, fail, er);
	xfree(temp_fn);			/* no longer need filename */
	xfree(fn);

#ifdef HAVE_SETEUID
	/* order is important here */
	(void) seteuid(0);
	(void) setegid(save_gid);
#endif

	return;
    }

    /* lock necessary only when appending */
    if (priv->file && lock_file(temp_fn, f) == FAIL) {
	/*
	 * ERR_134 - failed to lock mailbox
	 *
	 * DESCRIPTION
	 *      lock_file() was unable to lock the output file, possibly
	 *      within a timeout interval.  Note that any lock file created
	 *      by lock_file() is created within the context of the user
	 *      smail is running as, not as the user that is used in opening
	 *      the mailbox file itself.
	 *
	 * ACTIONS
	 *      The error is recoverable by just attempting delivery at a
	 *      later time when, perhaps, the lock_file() attemp succeeds.
	 *	Thus, delivery to the input addresses is deferred.
	 *
	 * RESOLUTION
	 *      Hopefully, a later attempt at delivery will succeed.
	 */
	register struct error *er;

	er = note_error(ERR_134,
			xprintf("transport %s: failed to lock mailbox",
				tp->name));
	insert_addr_list(addr, defer, er);
	(void) fclose(f);
	if (priv->file) {
	    restore_atime(fn, atime_saved);
	}
	xfree(temp_fn);
	xfree(fn);

#ifdef HAVE_SETEUID
	/* order is important here */
	(void) seteuid(0);
	(void) setegid(save_gid);
#endif

	return;
    }

#ifdef HAVE_COMSAT
    msg_offset = lseek(fileno(f), (off_t) 0L, SEEK_END);
#else
    (void) lseek(fileno(f), (off_t) 0L, SEEK_END);
#endif

    /* write out the message */
    if ((priv->prefix && 
	 fputs(expand_string(priv->prefix, addr, (char *) NULL, (char *) NULL), f) == EOF) ||
	write_message(f, tp, addr) != SUCCEED ||
	(priv->suffix && fputs(priv->suffix, f) == EOF) ||
	fflush(f) == EOF)
    {
	/*
	 * ERR_135 - write to mailbox failed
	 *
	 * DESCRIPTION
	 *      A write to the mailbox file failed.  This is unusual, unless
	 *      the filesystem is full.
	 *
	 * ACTIONS
	 *      As this probably means the filesystem is temporarily full,
	 *      defer delivery to the input addresses and attempt delivery
	 *      later when, hopefully, the filesystem has gained some space.
	 *      Unfortunately, the mailbox file may contain only part of a
	 *      message, which is undesirable though difficult to prevent.
	 *
	 * RESOLUTION
	 *      Hopefully, a later attempt to write the file will succeed.
	 */
	register struct error *er;

	er = note_error(ERR_135,
			xprintf("transport %s: write to mailbox failed: %s",
				tp->name, strerror(errno)));
	insert_addr_list(addr, defer, er);
	(void) fclose(f);
	if (priv->file) {
	    restore_atime(fn, atime_saved);
	}
	xfree(temp_fn);
	xfree(fn);

#ifdef HAVE_SETEUID
	/* order is important here */
	(void) seteuid(0);
	(void) setegid(save_gid);
#endif

	return;
    }

#ifdef HAVE_FSYNC
    (void) fsync(fileno(f));
#endif

    if (priv->file) {
	/* unlock and close the file */
	unlock_file(temp_fn, f);
	(void) fclose(f);
	restore_atime(fn, atime_saved);
#ifdef HAVE_COMSAT
	if (tp->flags & APPEND_COMSAT) {
	    notify_comsat(tp, temp_fn, msg_offset);
	}
#endif
    } else {
	/*
	 * move the queue file to a permanent name the name
	 * is formed from the clock time plus the inode number,
	 * all in ASCII base 62
	 *
	 *    "q"tttttt-iiiiii
	 */
	char *a_inode;
	char *perm_fn;
	struct stat statbuf;

	(void) fstat(fileno(f), &statbuf);
	a_inode = COPY_STRING(base62((unsigned long) statbuf.st_ino));
	perm_fn = xprintf("%s/q%s-%s", fn,
			  base62((unsigned long) statbuf.st_atime),
			  a_inode);
	(void) fclose(f);
	if (rename(temp_fn, perm_fn) < 0) {
	    /*
	     * ERR_136 - rename failed for queue file
	     *
	     * DESCRIPTION
	     *      rename() failed to move the output file to its final
	     *      name.  This is an unusual problem, since a write already
	     *      succeeded to the directory and a rename() requires only
	     *      write access to the directory.
	     *
	     * ACTIONS
	     *      Fail the address and send an error to the postmaster.
	     *      This is not likely to be an error that an address owner
	     *      can deal with more effectively than the site
	     *      administrator, so don't bother sending to any owners.
	     *
	     * RESOLUTION
	     *      Suggestions are a full filesystem that could not handle
	     *      a small expansion in the size of a directory, or
	     *      possibly, a chmod() or directory rename() occured at
	     *      just the wrong time.
	     */
	    register struct error *er;

	    er = note_error(ERR_NPOSTMAST|ERR_136,
		      xprintf("transport %s: rename failed for queue file: %s",
			      tp->name, strerror(errno)));
	    insert_addr_list(addr, fail, er);
	    xfree(temp_fn);
	    xfree(perm_fn);
	    xfree(fn);

#ifdef HAVE_SETEUID
	/* order is important here */
	(void) seteuid(0);
	(void) setegid(save_gid);
#endif

	    return;
	}
	xfree(perm_fn);
    }

    /* everything went okay, link into the succeed list */
    insert_addr_list(addr, succeed, (struct error *)NULL);
    xfree(temp_fn);			/* free temporary storage */
    xfree(fn);

#ifdef HAVE_SETEUID
	/* order is important here */
	(void) seteuid(0);
	(void) setegid(save_gid);
#endif

    return;
}

/*
 * tpb_appendfile - read the configuration file attributes
 */
char *
tpb_appendfile(tp, attrs)
    struct transport *tp;		/* transport entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct appendfile_private appendfile_template = {
	NULL,				/* file */
	NULL,				/* dir */
	NULL,				/* user */
	NULL,				/* group */
	NULL,				/* prefix */
	NULL,				/* suffix */
	0666,				/* mode */
    };
    struct appendfile_private *priv;	/* new appendfile_private structure */

    /* copy the template private data */
    priv = (struct appendfile_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&appendfile_template, sizeof(*priv));

    tp->private = (char *)priv;

    /* set default flags */
    tp->flags |= APPEND_AS_USER;

    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &tp->flags,
			    appendfile_attributes,
			    end_appendfile_attributes);

    if (error) {
	return error;
    }
    return NULL;
}



/*
 * tpp_appendfile - dump the configuration attributes
 */
void
tpp_appendfile(f, tp)
     FILE * f;
     struct transport *tp;
{
    (void) dump_standard_config(f,
				tp->private,
				tp->name,
				tp->flags,
				appendfile_attributes,
				end_appendfile_attributes);
}



/*
 * get_appendfile_ugid - return the uid and gid to use in creating file
 */
static void
get_appendfile_ugid(tp, addr, uid, gid)
    struct transport *tp;		/* associated transport structure */
    struct addr *addr;			/* associated addr structures */
    int *uid;				/* store uid here */
    int *gid;				/* store gid here */
{
    struct appendfile_private *priv = (struct appendfile_private *)tp->private;
    /*
     * determine the uid to use for delivery
     */
    if (priv->user == NULL) {
	if ((tp->flags & APPEND_AS_USER) && addr->uid != BOGUS_USER) {
	    *uid = addr->uid;
	} else {
	    *uid = nobody_uid;
	}
    } else {
	struct passwd *pw = getpwbyname(priv->user);

	if (pw == NULL) {
	    write_log(LOG_PANIC,
		      "transport %s: warning: user %s unknown, using nobody",
		      tp->name, priv->user);
	    priv->user = NULL;
	    *uid = nobody_uid;
	} else {
	    *uid = pw->pw_uid;
	    *gid = pw->pw_gid;
	}
    }

    /* determine the gid for use for delivery */
    if (priv->group) {
	struct group *gr = getgrbyname(priv->group);

	if (gr == NULL) {
	    write_log(LOG_PANIC,
		      "transport %s: warning: group %s unknown, ignored",
		      tp->name, priv->group);
	    priv->group = NULL;
	} else {
	    *gid = gr->gr_gid;
	}
    }
    if (priv->group == NULL) {
	if (priv->user == NULL) {
	    if ((tp->flags & APPEND_AS_USER) && addr->gid != BOGUS_GROUP) {
		*gid = addr->gid;
	    } else {
		*gid = nobody_gid;
	    }
	}
    }
}


/*
 * Functions to save and restore file access times.
 * Originally provided by bob@omni.com.
 */

/*
 * get_atime - get the access time of a file, so that we can later restore it
 */
static time_t
get_atime(fname)
    char *fname;
{
    struct stat statbuf;

    if (stat(fname, &statbuf) != 0)
	return 0;
    return statbuf.st_atime;
}

/*
 * restore_atime - restore access time, if one was found by get_atime
 */
static void
restore_atime(fname, atime)
    char *fname;
    time_t atime;
{
    struct stat statbuf;

    if (atime && stat(fname, &statbuf) == 0) {
	(void) touch(fname, atime, statbuf.st_mtime);
    }
}

#ifdef HAVE_COMSAT

/*
 * notify_comsat - tell comsat daemon that mail has been delivered
 *
 * Before the mail message is appended to the users mailbox, the
 * offset (size of the file) is saved.  After the message is appended, 
 * this connects with inetd and sends a one-line message to in.comsat,
 * which displays a portion of the new mail on the users terminal.
 *
 * The comsat daemon expects the message "mboxname@offset"
 * where 'mboxname' is a string specifying the mailbox name,
 * and 'offset' is an integer string specifying the length of
 * the mailbox before the new mail was appended.
 *
 * 'temp_fn' is the mailbox pathname,  the last element being
 * the mailbox itself.
 */
/* ARGSUSED */
static void
notify_comsat(tp, mbox, offset)
struct transport *tp;				/* UNUSED! */
char *mbox;
off_t offset;
{
    static char me[] = "notify_comsat";
    struct sockaddr_in sa;
    struct hostent *hp;
    struct servent *sp;
    char *base, *msg;
    int fd, ret;

    if ((hp = gethostbyname("localhost")) == NULL) {
	DEBUG1(DBG_DRIVER_LO, "%s: localhost: unknown host", me);
        return;
    }
    if (hp->h_addrtype != AF_INET) {
	DEBUG1(DBG_DRIVER_LO, "%s: localhost: not TCP/IP", me);
	return;
    }
    if ((sp = getservbyname("biff", "udp")) == NULL) {
	DEBUG1(DBG_DRIVER_LO, "%s: biff/udp: unknown service", me);
	return;
    }

    sa.sin_port = sp->s_port;
    sa.sin_family = hp->h_addrtype;
    memcpy((char *) &sa.sin_addr, (char *) hp->h_addr, (size_t) hp->h_length);

    if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
	DEBUG2(DBG_DRIVER_LO, "%s: socket: %s\n", me, strerror(errno));
	return;
    }

    if ((base = strrchr(mbox, '/')) == NULL) {
	base = mbox;
    } else {
	++base;
    }
    msg = xprintf("%s@%d\n", base, offset);
    DEBUG2(DBG_DRIVER_HI, "%s: sending msg: %s", me, msg);
    ret = sendto(fd, msg, strlen(msg) + 1, 0, (struct sockaddr *)&sa, sizeof(sa));
    xfree(msg);
    if (ret == -1) {
	DEBUG2(DBG_DRIVER_LO, "%s: send: %s\n", me, strerror(errno));
    }

    (void) close(fd);
}

#endif /* HAVE_COMSAT */
