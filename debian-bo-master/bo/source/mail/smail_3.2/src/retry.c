/*
#ident	"@(#)smail/src:RELEASE-3_2:retry.c,v 1.26 1996/02/26 18:15:00 woods Exp"
 */

/*
 * retry.c:
 *      Control how often addresses are retried, and how long
 *      retries are attempted until Smail gives up.
 *
 *	This file was contributed by Chip Salzenberg <chip@tct.com>.
 *	It has been modified.
 *
 *	external functions: read_retry_file, retry_addr_before,
 *			    retry_addr_after, retry_addr_finished,
 *			    retry_host_lock, retry_host_unlock
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include "defs.h"
#include "smail.h"
#include "addr.h"
#include "transport.h"
#include "log.h"
#include "parse.h"
#include "dys.h"
#include "lookup.h"
#include "jump.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

#if defined(UNIX_SYS5) || defined(POSIX_OS) || defined(USE_FCNTL)
# include <fcntl.h>
#else
# if defined(UNIX_BSD)
#  include <sys/file.h>
# endif
#endif

#if defined(POSIX_OS) || defined(UNIX_SYS5)
# include <unistd.h>			/* for _PC_NAME_MAX (Jim Mercer, FreeBSD 2.0.5) */
#endif	/* POSIX_OS || UNIX_SYS5 */

/*
 * the retry mechanism requires three-parameter open() and
 * kernel locking.  if either of these features is missing,
 * then all retry features are stubbed out.
 */

#if defined(O_CREAT) && defined(lock_fd_wait)

#define RETRY_MODE   0644		/* file creation modes */

#ifdef O_SYNC
# define O_SWYM O_SYNC
#else
# define O_SWYM 0
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

#ifdef ANSI_C
# define P_(x)  x
# define VOLATILE volatile
#else
# define P_(x)  ()
# define VOLATILE /**/
#endif

/* variables imported from libc */
extern int errno;

struct retry {
    struct retry *succ;			/* next in chain */
    char    *domains;			/* target domain, or "*" */
    time_t  interval;			/* minimum time between tries */
    time_t  duration;			/* how long to keep trying */
};

static struct retry *retries = NULL;	/* array of control structures */

static int locked_fd = -1;
static char *locked_path = NULL;
static struct retry *locked_retry = NULL;
static JUMP_ENVBUF tryfile_jmp;

static struct retry *default_retry P_((void));
static int match_retry_domain P_((char *, char *));
static int allow_now P_((int, char *, time_t, time_t, struct retry *,
			 struct error **));
static int allow_later P_((time_t, time_t, struct retry *, struct error **));
static char *tryfile_name P_((struct transport *, char *, int));
static int tryfile_cat P_((struct str *, char *, unsigned, int, int, int));
static int tryfile_mkdir P_((struct str *));
static int tryfile_lock P_((int, long));
static void tryfile_unlock P_((int));
static void tryfile_alarm P_((int));
static void write_error P_((int, char *, struct error *));
static void read_error P_((int, char *, struct error *));

/*
 * read_retry_file - read and remember contents of retry file
 */

char *
read_retry_file()
{
    struct stat statbuf;
    struct retry *r, *nr;
    FILE *f;
    char *entry, *error;

    /*
     * forget any previously-read retry data
     */
    for (r = retries; r; r = nr) {
	nr = r->succ;
	xfree(r->domains);
	xfree((char *)r);
    }
    retries = NULL;

    /*
     * try to open retry file, stat file if possible
     */
    if (retry_file == NULL || EQ(retry_file, "-")) {
	return NULL;
    }
    f = fopen(retry_file, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("cannot open %s: %s", retry_file, strerror(errno));
	}

	add_config_stat(retry_file, (struct stat *)NULL);
	retries = default_retry();
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(retry_file, &statbuf);

    /*
     * loop and read all of the table entries in the retry file
     */
    error = NULL;
    nr = NULL;
    while ((entry = read_entry(f))) {
	struct attribute *new;
	char *name, *value, *p;
	time_t interval, duration;
	long n;

	new = parse_table(entry, &error);
	if (new == NULL) {
	    break;
	}
	name = new->name;
	value = new->value;
	xfree((char *)new);

	/* parse duration first, since it appears second */
	duration = 0;
	p = index(value, '/');
	if (p) {
	    *p++ = '\0';
	    n = ivaltol(p);
	    if (n < 0) {
		error = xprintf("invalid retry interval: %s", p);
		xfree(name);
		break;
	    }
	    duration = n;
	}

	/* parse interval */
	interval = 0;
	if (*value) {
	    n = ivaltol(value);
	    if (n < 0) {
		error = xprintf("invalid retry duration: %s", value);
		xfree(name);
		break;
	    }
	    interval = n;
	}

	/* allocate new structure */
	r = (struct retry *) xmalloc(sizeof(struct retry));

	/* assign new structure's domain */
	r->domains = COPY_STRING(name);

	/* assign new structure's limits */
	r->interval = interval;
	r->duration = duration;

	/* append new structure to chain */
	r->succ = NULL;
	if (nr == NULL) {
	    retries = r;
	} else {
	    nr->succ = r;
	}
	nr = r;

	/* free string returned by parse_table */
	xfree(name);

	/* wildcard obviates need to read further */
	if (EQ(name, "*")) {
	    break;
	}
    }
    (void) fclose(f);

    /*
     * if no errors, and there is no default record, and if
     * global configuration requires a default record, add it.
     */
    if ((nr == NULL || !EQ(nr->domains, "*"))
	&& (r = default_retry()) != NULL)
    {
	if (nr == NULL) {
	    retries = r;
	} else {
	    nr->succ = r;
	}
    }

    /* return error message, or NULL for success */
    return error;
}

/*
 * dump_retry_config - dump out the current retry configuration
 */
void
dump_retry_config(f)
     FILE *f;
{
    struct retry *rp;

    (void)fputs("#\n# -- retry configuration\n#\n", f);
    for (rp = retries; (rp); rp = rp->succ) {
	(void)fprintf(f, "%s\t%s",
		      rp->domains,
		      ltoival(rp->interval));
	/* 
	 * ltoival uses static memory which means that this fprintf has to
	 * broken into 2 halves, otherwise both interval & duration
	 * appear the same!
	 */
	(void)fprintf(f, "/%s\n",
		      ltoival(rp->duration));
    }
    (void)fputs("#\n# -- end of retry configuration\n#\n", f);
}


/*
 * default_retry - build default retry record from global configuration
 */
static struct retry *
default_retry()
{
    struct retry *r;

    if (!retry_interval && !retry_duration) {
	return NULL;
    }

    r = (struct retry *) xmalloc(sizeof(struct retry));
    r->domains = COPY_STRING("*");
    r->interval = retry_interval;
    r->duration = retry_duration;
    r->succ = NULL;
    return r;
}

/*
 * retry_addr_before - retry processing before transport attempt
 */
struct addr *
retry_addr_before(in, defer, fail)
    struct addr *in;
    struct addr **defer;
    struct addr **fail;
{
    time_t spooled = message_date();
    struct addr *out, *cur, *next;

    out = NULL;
    for (cur = in; cur; cur = next) {
	struct addr **where = &out;
	struct retry *r;

	next = cur->succ;

	/* skip if no next host */
	if (cur->next_host == NULL) {
	    cur->succ = out;
	    out = cur;
	    continue;
	}

	for (r = retries; r; r = r->succ) {
	    if (match_retry_domain(r->domains, cur->next_host)) {
		char *tryname;
		int may_deliver, fd;
		time_t started;

		/* calculate path of retry file */
		if ((tryname = tryfile_name(cur->transport,
					    cur->next_host, FALSE)) == NULL)
		{
		    break;
		}

		DEBUG1(DBG_RETRY_LO, "examine %s\n", tryname);

		/* examine the retry file: lock; ignore empties */
		for (;;) {
		    int lk;

		    if ((fd = open(tryname, O_RDWR|O_SWYM)) == -1) {
			time(&started);
			may_deliver = TRUE;
			break;
		    }
		    lk = tryfile_lock(fd, 0L);
		    if (lk == DB_AGAIN) {
			(void) close(fd);
			continue;
		    }

		    /* the rest happens only once */
		    if (lk == DB_SUCCEED) {
			time(&started);
			may_deliver = allow_now(fd, tryname, spooled, started,
						r, &cur->error);
			(void) tryfile_unlock(fd);
		    }
		    (void) close(fd);
		    break;
		}

		if (may_deliver) {
		    /* try it; if it fails, we'll touch the retry file */
		    cur->flags |= ADDR_RETRY_FILE;
		} else {
		    /* retry interval has not yet expired */
		    where = allow_later(spooled, started, r, &cur->error)
			    ? defer : fail;
		}

		/* stop looking for retry specs after first match */
		break;
	    }
	}

	cur->succ = *where;
	*where = cur;
    }

    return out;
}

/*
 * retry_addr_after - retry processing after delivery attempt,
 *		      possibly failing addresses deferred by transport
 */
struct addr *
retry_addr_after(started, in, fail)
    time_t started;
    struct addr *in;
    struct addr **fail;
{
    time_t spooled = message_date();
    struct addr *out, *cur, *next;

    out = NULL;
    for (cur = in; cur; cur = next) {
	struct addr **where = &out;
	struct retry *r;

	next = cur->succ;

	if (cur->flags & ADDR_RETRY_FILE) {
	    for (r = retries; r; r = r->succ) {
		if (match_retry_domain(r->domains, cur->next_host)) {

		    /* if total retry duration exceeded, fail address */
		    if (! allow_later(spooled, started, r, &cur->error)) {
			where = fail;
		    }

		    /* stop looking for retry specs after first match */
		    break;
		}
	    }
	}

	cur->succ = *where;
	*where = cur;
    }

    return out;
}

/*
 * retry_addr_finished - record success or failure
 */
void
retry_addr_finished(addr)
    struct addr *addr;
{
    char *prev_host = NULL;
    struct addr *cur;

    for (cur = addr; cur; cur = cur->succ) {
	if (cur->flags & ADDR_RETRY_FILE) {
	    struct error *error = cur->error;
	    char *tryname;
	    int fd;

	    /* don't bother writing to the same file twice in a row */
	    if (prev_host && EQIC(prev_host, cur->next_host)) {
		continue;
	    }
	    prev_host = cur->next_host;

	    /* calculate path of retry file */
	    if ((tryname = tryfile_name(cur->transport, cur->next_host,
					(error != NULL))) == NULL)
	    {
		continue;
	    }

	    DEBUG2(DBG_RETRY_LO, "%s error message in %s\n",
		   error ? "writing" : "removing", tryname);

	    /* write to, or remove, retry file */
	    for (;;) {
		int lk;

		if ((fd = open(tryname, O_WRONLY | (error ? O_CREAT : 0),
			       RETRY_MODE)) == -1)
		{
		    break;
		}
		lk = tryfile_lock(fd, 0L);
		if (lk == DB_AGAIN) {
		    (void) close(fd);
		    continue;
		}

		/* the rest happens only once */
		if (lk == DB_SUCCEED) {
		    write_error(fd, tryname, error);
		    tryfile_unlock(fd);
		}
		(void) close(fd);
		break;
	    }
	}
    }
}

/*
 * retry_host_lock - lock the target host for delivery the given addresses
 *
 * return SUCCEED if lock succeeded, FAIL if it failed.
 * if failure is temporary, set *defer_failure.
 *
 * some transports will use this function.  those that do so won't
 * call retry_addr_before(), so retry_addr_*() will do nothing.
 */
int
retry_host_lock(transport, host, defer_failure, error)
    struct transport *transport;
    char *host;
    int *defer_failure;
    struct error **error;
{
    time_t spooled = message_date();
    time_t started;
    struct retry *r;
    char *tryname;
    int fd, dfok, ret;

    /* safety: unlock previously locked retry file */
    if (locked_fd >= 0) {
	tryfile_unlock(locked_fd);
	(void) close(locked_fd);
	locked_fd = -1;
    }
    if (locked_path) {
	xfree(locked_path);
	locked_path = NULL;
    }

    /* calculate path of retry file, creating directories */
    if ((tryname = tryfile_name(transport, host, TRUE)) == NULL) {
	return FAIL;
    }

    DEBUG1(DBG_RETRY_LO, "lock %s\n", tryname);

    /* open and lock retry file */
    for (;;) {
	int lk;

	if ((fd = open(tryname, O_RDWR|O_CREAT|O_SWYM, RETRY_MODE)) == -1) {
	    return FAIL;
	}
	lk = tryfile_lock(fd, host_lock_timeout);
	if (lk == DB_SUCCEED) {
	    break;
	}
	if (lk == DB_AGAIN) {
	    (void) close(fd);
	    continue;
	}

	DEBUG1(DBG_RETRY_LO, "lock %s TIMED OUT\n", tryname);
	(void) close(fd);
	if (*error == NULL) {
	    *error = note_error(ERR_175, "host retry file locked");
	}
	*defer_failure = TRUE;
	return FAIL;
    }

    /* now is the official start of attempt */
    time(&started);

    /* assume the best */
    ret = SUCCEED;
    dfok = TRUE;

    /* if host has retry parameters, enforce them */
    for (r = retries; r; r = r->succ) {
	if (match_retry_domain(r->domains, host)) {

	    /* notice if it's too soon to try delivery */
	    ret = allow_now(fd, tryname, spooled, started, r, error)
		  ? SUCCEED : FAIL;

	    /* we can defer if further retries would be soon enough */
	    dfok = allow_later(spooled, started, r, error);

	    /* stop looking for retry specs after first match */
	    break;
	}
    }

    /* keep human informed */
    DEBUG3(DBG_RETRY_LO, "lock %s (%s) %s\n",
	   (ret == SUCCEED) ? "succeeded" : "failed",
	   dfok ? "will defer failure" : "will not defer failure",
	   tryname);

    /* if retry control allows deferral, inform caller */
    if (dfok) {
	*defer_failure = TRUE;
    }

    if (ret == SUCCEED) {
	/* remember name, fd and retry parameters */
	locked_fd = fd;
	locked_path = COPY_STRING(tryname);
	locked_retry = r;
    } else {
	/* we won't be needing the lock file after all */
	tryfile_unlock(fd);
	(void) close(fd);
    }

    return ret;
}

/*
 * retry_host_unlock - unlock the target host for delivery, recording status
 *
 * set *fail if retry time has expired.
 *
 * some transports will use this function.  those that do so won't
 * call retry_addr_before(), so retry_addr_*() will do nothing.
 */
/* ARGSUSED */
void
retry_host_unlock(started, error)
    time_t started;			/* started not used! */
    struct error *error;
{
    if (locked_fd < 0) {
	return;
    }

    DEBUG2(DBG_RETRY_LO, "unlock (%s) %s\n",
	   error ? "error" : "success", locked_path);

    /* write the error */
    write_error(locked_fd, locked_path, error);

    /* unlock and close the retry file */
    tryfile_unlock(locked_fd);
    (void) close(locked_fd);

    /* forget that we ever saw the lock file */
    locked_fd = -1;
    xfree(locked_path);
    locked_path = NULL;
    locked_retry = NULL;
}

/*
 * match_retry_domain - determine if domain pattern matches given host
 */
static int
match_retry_domain(domains, target)
    char *domains;			/* colon separated list of domains */
    char *target;			/* target to test against */
{
    register char *cur;			/* current domain being checked */

    if (EQ(domains, "*")) {
	return TRUE;
    }

    for (cur = strcolon(domains); cur; cur = strcolon((char *)NULL)) {
	if (is_suffix(cur, target, FALSE)) {
	    return TRUE;
	}
    }

    return FALSE;
}

/*
 * allow_now - can we deliver now?  and if failure, can we defer?
 */
static int
allow_now(fd, tryname, spooled, started, r, error)
    int fd;
    char *tryname;
    time_t spooled, started;
    struct retry *r;
    struct error **error;
{
    struct stat statbuf;
    time_t last;

    last = 0;
    if (fstat(fd, &statbuf) == 0 && statbuf.st_size) {
	last = statbuf.st_mtime;
    }

    if (last && last <= started && (started - last) < r->interval) {

	/* retry interval has not passes, so we won't deliver */

	if ((started - spooled) > r->duration) {
	    /* your time is up */
	    if (*error == NULL) {
		struct error err;

		read_error(fd, tryname, &err);
		*error = note_error(ERR_NSOWNER | err.info, err.message);
	    }
	} else {
	    /* just wait a little longer */
	    if (*error == NULL) {
		*error = note_error(ERR_DONTLOG | ERR_174,
				    "retry interval not reached");
	    }
	}
	return FALSE;
    }

    return TRUE;
}

/*
 * allow_later - can we make more retries after this one?
 */
static int
allow_later(spooled, started, r, error)
    time_t spooled, started;
    struct retry *r;
    struct error **error;
{
    /* if this was the last try... */
    if ((started - spooled) > r->duration) {
	if (*error == NULL) {
	    *error = note_error(ERR_173, "retry duration exceeded");
	}
	return FALSE;
    }

    /* there is still time for more retries */
    return TRUE;
}

/*
 * tryfile_name - figure name of file used to hold timestamp for
 *		  the given host.
 *
 * Note: returns address of static area.
 */
static char *
tryfile_name(transport, host, create_dirs)
    struct transport *transport;
    char *host;
    int create_dirs;
{
    static long pathmax = 0;
    static struct str path;
    static int inited = FALSE;

#ifdef POSIX_OS
    if (pathmax == 0) {
	long n = pathconf(".", _PC_NAME_MAX);
	if (n != -1) {
	    pathmax = n;
	}
    }
#else
#ifdef UNIX_BSD
    if (pathmax == 0) {
	pathmax = 250;
    }
#endif /* UNIX_BSD */
#endif /* POSIX_OS */
    if (pathmax == 0) {
	pathmax = 14;
    }

    if (!inited) {
	STR_INIT(&path);
	inited = TRUE;
    }
    path.i = 0;

    str_cat(&path, "retry");
    if (create_dirs && tryfile_mkdir(&path) == FAIL) {
	return NULL;
    }

    STR_NEXT(&path, '/');
    str_cat(&path, (transport->retry_dir && transport->retry_dir[0])
		   ? transport->retry_dir : transport->name);
    if (create_dirs && tryfile_mkdir(&path) == FAIL) {
	return NULL;
    }

    if ((int)strlen(host) <= pathmax) {
	if (tryfile_cat(&path, host, (unsigned) strlen(host), TRUE, FALSE,
			create_dirs) == FAIL)
	{
	    return NULL;
	}
    } else {
	char *p, *q;
	int first, partial;

	first = TRUE;
	p = host + strlen(host);
	for (;;) {
	    while (p > host && *(p - 1) == '.') {
		--p;
	    }
	    if (p == host) {
		break;
	    }
	    q = p;
	    partial = FALSE;
	    while (p > host && *(p - 1) != '.') {
		if ((q - p) >= (pathmax - 1)) {
		    partial = TRUE;
		    break;
		}
		--p;
	    }
	    if (tryfile_cat(&path, p, (unsigned) (q - p), first, partial,
			    create_dirs) == FAIL)
	    {
		return NULL;
	    }
	    first = FALSE;
	}
    }
    STR_NEXT(&path, '\0');
    return path.p;
}

static int
tryfile_cat(sp, p, len, first, partial, create_dirs)
    struct str *sp;
    char *p;
    unsigned len;
    int first, partial, create_dirs;
{
    unsigned i;

    if (!first && sp->i && sp->p[sp->i - 1] != '_') {
	STR_NEXT(sp, '.');
    }

    if (create_dirs && tryfile_mkdir(sp) == FAIL) {
	return FAIL;
    }

    STR_NEXT(sp, '/');
    for (i = 0; i < len; ++i) {
	int c = *(p + i) & 0xFF;
	if (isupper(c)) {
	    c = tolower(c);
	}
	STR_NEXT(sp, c);
    }
    if (partial) {
	STR_NEXT(sp, '_');
    }

    return SUCCEED;
}

static int
tryfile_mkdir(sp)
    struct str *sp;
{
    struct stat statbuf;
    int ret = SUCCEED;

    STR_NEXT(sp, '\0');
    if (stat(sp->p, &statbuf) == -1) {
	DEBUG1(DBG_RETRY_LO, "make directory %s\n", sp->p);
	(void) mkdir(sp->p, auto_mkdir_mode);
	if (stat(sp->p, &statbuf) == -1) {
	    write_log(LOG_SYS, "can't create directory %s", sp->p);
	    ret = FAIL;
	}
    }
    sp->i--;

    return ret;
}

/*
 * tryfile_lock - lock an open retry file; optionally, time out
 *
 * Returns DB_SUCCESS (good), DB_FAIL (bad),
 * or DB_AGAIN (if file was removed).
 */
static int
tryfile_lock(fd, timeout)
    int fd;
    long timeout;
{
    VOLATILE JUMPSIG old_sigalrm;
    VOLATILE int old_alarm;
    int ret;

    if (timeout && JUMP_SETJMP(tryfile_jmp)) {
	ret = DB_FAIL;
    } else {
	/* lock retry file with optional timeout */
	if (timeout) {
	    old_alarm = alarm(0);
	    JUMP_SETSIG(SIGALRM, tryfile_alarm, &old_sigalrm);
	    (void) alarm(timeout < 2 ? 2 : (unsigned)timeout);
	}

	if (lock_fd_wait(fd) == FAIL) {
	    ret = DB_FAIL;
	} else {
	    struct stat st;

	    ret = (fstat(fd, &st) == 0 && st.st_nlink > 0)
		   ? DB_SUCCEED : DB_AGAIN;
	}
	if (timeout) {
	    (void) alarm(0);
	    JUMP_CLEARSIG(SIGALRM, &old_sigalrm);
	    if (old_alarm) {
		(void) alarm(old_alarm);
	    }
	}
    }

    return ret;
}

/*
 * tryfile_unlock - unlock a retry file locked with timeout
 */
static void
tryfile_unlock(fd)
    int fd;
{
    unlock_fd_wait(fd);
}

/*
 * tryfile_alarm - alarm function
 */
/* ARGSUSED */
static void
tryfile_alarm(sig)
     int sig;
{
    JUMP_LONGJMP(tryfile_jmp, 1);
}

/*
 * write_error - write an error description to a retry file, or remove it
 */
static void
write_error(fd, tryfile, error)
    int fd;
    char *tryfile;
    struct error *error;
{
    if (error == NULL) {
	/* no error: remove file */
	(void) unlink(tryfile);
    } else {
	char *e;
	int elen;

	/* write error code and message */
	e = xprintf("%ld %s\n", error->info & ERR_MASK, error->message);
	elen = (int)strlen(e);
	(void) lseek(fd, (off_t) 0L, SEEK_SET);
	(void) write(fd, e, (size_t) (elen + 1));
	(void) xfree(e);

	/* eliminate remaining bytes, to avoid confusing human readers */
	(void) fsetsize(fd, (off_t)elen, (off_t)elen + 1);

	/* flush changes */
#if defined(HAVE_FSYNC) && !defined(O_SYNC)
	(void) fsync(fd);
#endif
    }
}

/*
 * read_error - read an error description from a retry file
 */
/* ARGSUSED */
static void
read_error(fd, tryfile, error)
    int fd;
    char *tryfile;				/* UNUSED! */
    struct error *error;
{
    struct stat statbuf;
    char *buf, *errmsg, *p;
    long errcode;
    int len;

    /* assume that we can't read the file */
    error->info = ERR_173;
    error->message = "retry time expired";

    /* read the whole file */
    if (fstat(fd, &statbuf) == -1 || statbuf.st_size > 0x7FFF) {
	return;
    }
    len = (int)statbuf.st_size;
    buf = xmalloc(len + 1);
    if (read(fd, buf, (size_t) (len + 1)) != len) {
	xfree(buf);
	return;
    }
    buf[len] = '\0';

    /* parse the error number and message */
    errcode = 0;
    errmsg = NULL;

    p = buf;
    while (isdigit(*p)) {
	++p;
    }
    if (*p == ' ') {
	*p++ = '\0';
	errcode = atoi(buf);
	errmsg = p;

	if (*errmsg) {
	    p = errmsg + strlen(errmsg) - 1;
	    if (*p == '\n') {
		*p = '\0';
	    }
	}
    }

    /* if code and message are acceptable, use them */
    if (errcode && errmsg && *errmsg) {
	error->info = errcode;
	error->message = COPY_STRING(errmsg);
    }

    /* free copy of file */
    xfree(buf);
}

#else	/* O_CREAT && lock_fd_wait */

/*
 * without three-parameter open and kernel locking,
 * stub out all retry functions
 */

char *
read_retry_file()
{
    /* STUB */
    return NULL;
}

struct addr *
retry_addr_before(in, defer, fail)
    struct addr *in;
    struct addr **defer;
    struct addr **fail;
{
    /* STUB */
    return in;
}

struct addr *
retry_addr_after(started, in, fail)
    time_t started;
    struct addr *in;
    struct addr **fail;
{
    /* STUB */
    return in;
}

void
retry_addr_finished(addr)
    struct addr *addr;
{
    /* STUB */
}

int
retry_host_lock(transport, host, defer_failure, error)
    struct transport *transport;
    char *host;
    int *defer_failure;
    struct error **error;
{
    /* STUB */
    return SUCCEED;
}

void
retry_host_unlock(started, error)
    time_t started;
    struct error *error;
{
    /* STUB */
}

#endif	/* O_CREAT && lock_fd_wait */
