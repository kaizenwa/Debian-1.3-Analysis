/* $Id: procs.c,v 1.9 1993/10/28 16:49:51 chip Exp $
 *
 * Process management and misc support.
 *
 * $Log: procs.c,v $
 * Revision 1.9  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.8  1991/10/28  16:17:59  chip
 * Check return values on pipe(), dup() and open("/dev/null").
 *
 * Revision 1.7  1991/10/23  20:42:15  chip
 * Regularize closing of pipes during spawn procedure.
 *
 * Revision 1.6  1991/10/23  20:07:03  chip
 * Depend on close-on-exec for temp and log files.
 *
 * Revision 1.5  1991/10/23  19:29:14  chip
 * Eliminate race condition when spawning children.
 *
 * Revision 1.4  1991/08/26  17:42:24  chip
 * Don't declare errno.
 *
 * Revision 1.3  1991/08/05  19:03:49  chip
 * Add setgrvec().
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <errno.h>

/*
 * Local data.
 */

static int child_pid = -1;
static SIGTYPE (*saved_sigpipe)() = SIG_DFL;

#ifdef DELHOME
#define HBSIZE	(sizeof(DELHOME) + sizeof("/bin"))
#else
#define HBSIZE	0
#endif

/*----------------------------------------------------------------------
 * Like popen(), but execute the child in a specific context.
 * Also, the argument list is already a vector.
 */

FILE *
ct_fopenv(ct, prog, av, mode)
CONTEXT *ct;
char *prog;
char **av;
char *mode;
{
    FILE *fp;
    int fd, m;

    if (mode && mode[0] == 'r' && mode[1] == 0)
	m = O_RDONLY;
    else if (mode && mode[0] == 'w' && mode[1] == 0)
	m = O_WRONLY;
    else
	return NULL;

    if ((fd = ct_openv(ct, prog, av, m)) == -1)
	return NULL;

    if ((fp = fdopen(fd, mode)) == NULL)
	(void) ct_close(fd);

    return fp;
}

/*----------------------------------------------------------------------
 * Close the stream opened by ct_fopen().
 */

int
ct_fclose(fp)
FILE *fp;
{
    int fd;

    if (fp)
    {
	fd = dup(fileno(fp));
	(void) fclose(fp);
    }
    else
	fd = -1;

    return ct_close(fd);
}

/*----------------------------------------------------------------------
 * Like popen(), but execute the child in a specific context.
 * Also, the argument list is already a vector.
 * And return a file descriptor instead of a FILE *.
 */

int
ct_openv(ct, prog, av, mode)
CONTEXT *ct;
char *prog;
char **av;
int mode;
{
    char ch;
    int child, parent, rd;
    int pfd[2], coord[2];

    if (!ct || !prog || !av)
	return NULL;

    if (mode == O_RDONLY)
	child = 1, parent = 0;
    else if (mode == O_WRONLY)
	child = 0, parent = 1;
    else
    {
	error("in ct_open: invalid mode");
	return -1;
    }

    /* We can't have more than one child at a time. */

    if (child_pid >= 0)
    {
	error("in ct_open: a process is already open");
	return -1;
    }

    /* Make a stab at predicting uid-related failure. */

    if (!ok_context(eff_uid, real_uid, real_gid, ct))
    {
	error("in ct_open: no permissions to become %s", ct->ct_name);
	return -1;
    }

    /* Pipes?  Like, tubular, fer shur! */

    if (pipe(pfd) == -1 || pipe(coord) == -1)
    {
	syserr("can't create a pipe");
	return -1;
    }

    if (clexec(coord[0], TRUE) == -1 || clexec(coord[1], TRUE) == -1)
	return -1;

    /* Generate a debugging message. */

    if (verbose)
    {
	int a;

	message("%s: spawning", progname);
	for (a = 0; av[a]; ++a)
	    message(" %s", av[a]);
	message("\n");
    }

    /* Flush all output streams. */

    (void) fflush(stdout);
    (void) fflush(stderr);
    if (log)
	(void) fflush(log);
    if (errlog)
	(void) fflush(errlog);

    /* Handle the child case */

    if (sfork() == 0)
    {
	if (child == 0)
	{
	    (void) close(0);
	    if (dup(pfd[0]) == -1)
	    {
		syserr("can't dup pipe for reading");
		(void) write(coord[1], "d", 1);
		_exit(127);
	    }
	}
	else
	{
	    (void) close(0);
	    if (open("/dev/null", O_RDONLY) == -1)
	    {
		syserr("can't open /dev/null for reading");
		(void) write(coord[1], "n", 1);
		_exit(127);
	    }

	    (void) close(1);
	    if (dup(pfd[1]) == -1)
	    {
		syserr("can't dup pipe for writing");
		(void) write(coord[1], "d", 1);
		_exit(127);
	    }
	}

	(void) close(pfd[0]);
	(void) close(pfd[1]);

	if (become(ct, TRUE) < 0)
	    (void) write(coord[1], "b", 1);
	else
	{
	    (void) execv(prog, av);
	    syserr("can't execute %s", prog);
	    (void) write(coord[1], "e", 1);
	}

	_exit(127);
    }

    /* Parent doesn't need child's pipe fd. */

    (void) close(pfd[child]);

    /* Make sure that a broken pipe won't kill us */

    saved_sigpipe = signal(SIGPIPE, SIG_IGN);

    /* The child must fail to report trouble before we continue. */

    (void) close(coord[1]);
    rd = read(coord[0], &ch, 1);
    (void) close(coord[0]);

    /* If child reported trouble, close parent's pipe fd, and fail. */

    if (rd)
    {
	(void) close(pfd[parent]);
	(void) await_child();
	return -1;
    }

    /* All is well.  Return parent's pipe fd. */

    return pfd[parent];
}

/*----------------------------------------------------------------------
 * Close the file descriptor opened by ct_open().
 */

int
ct_close(fd)
int fd;
{
    if (fd != -1)
	(void) close(fd);
    return await_child();
}

/*----------------------------------------------------------------------
 * Assume the identity of the given user.
 * If chd is non-zero, change to the user's directory.
 */

int
become(ct, chd)
CONTEXT *ct;
int chd;
{
    char env_path[8 + HBSIZE + sizeof(SAFEPATH)];

    /*
     * Assume a new identity.
     * Note the importance of doing the setuid() last.
     */

#ifdef GROUP_VECTOR
    /*
     * We don't bother with the group vector if we're not
     * the superuser; it wouldn't work anyway.
     */
    if (eff_uid == 0)
    {
	if (setgrvec(ct->ct_numgroups, ct->ct_groups) == -1)
	{
	    syserr("can't set group vector of user %s",
		   ct->ct_name);
	    return -1;
	}
    }
#endif

    if (setgid(ct->ct_gid) == -1)
    {
	syserr("can't setgid to %d", ct->ct_gid);
	return -1;
    }
    eff_gid = getegid();
    real_gid = getgid();

    if (setuid(ct->ct_uid) == -1)
    {
	syserr("can't setgid to %u", ct->ct_uid);
	return -1;
    }
    eff_uid = geteuid();
    real_uid = getuid();

    /*
     * Be slightly paranoid about setuid(), because it has been
     * known to fail silently on SCO XENIX systems.  Sigh.
     */

    if (eff_uid != ct->ct_uid || real_uid != ct->ct_uid
	|| eff_gid != ct->ct_gid || real_gid != ct->ct_gid)
    {
	error("setuid/setgid failure: try=%d/%d eff=%d/%d real=%d/%d",
	      ct->ct_uid, ct->ct_gid,
	      eff_uid, eff_gid,
	      real_uid, real_gid);
	return -1;
    }

    /*
     * Change to user's home directory if requested.
     */

    if (chd && chdir(ct->ct_home) == -1)
    {
	syserr("can't chdir to %s", ct->ct_home);
	return -1;
    }

    /*
     * Set up a safe environment.
     */

    env_path[0] = '\0';
#ifdef DELHOME
    (void) strcat(env_path, delhome);
    (void) strcat(env_path, "/bin:");
#endif
    if (ct->ct_uid == 0)
	(void) strcat(env_path, "/etc:");
    (void) strcat(env_path, SAFEPATH);

    alloc_env("HOME", ct->ct_home);
    alloc_env("PATH", env_path);

    /* I guess it worked. */

    return 0;
}

/*----------------------------------------------------------------------
 * Set the group vector.
 */

#ifdef GROUP_VECTOR

int
setgrvec(numgroups, grvec)
int numgroups;
GRVEC_T *grvec;
{
    GRVEC_T dummy;		/* workaround for bug in SCO UNIX 3.2 */

    return SETGROUPS(numgroups, numgroups ? grvec : &dummy);
}

#endif /* GROUP_VECTOR */

/*----------------------------------------------------------------------
 * Safe fork.  If it doesn't work, it exits.
 */

int
sfork()
{
    int tries;

    /*
     * A few safety measures.
     */

    (void) await_child();
    (void) fflush(stdout);
    (void) fflush(stderr);
    if (log)
	(void) fflush(log);
    if (errlog)
	(void) fflush(errlog);

    /*
     * Be patient in waiting for a fork().
     */

    tries = 0;
    for (;;)
    {
	if (tries)
	    snooze(3);
	if ((child_pid = fork()) >= 0)
	    return child_pid;
	if (errno != EAGAIN || ++tries >= 10)
	{
	    syserr("can't fork");
	    leave(1);
	    /* NOTREACHED */
	}
    }
}

/*----------------------------------------------------------------------
 * Wait for our child (if any) to exit.
 * Returns child's exit status or -1 if there is a problem.
 */

int
await_child()
{
    int wpid, st;

    if (child_pid < 0)
	return -1;

    while ((wpid = wait(&st)) >= 0)
    {
	if (wpid == child_pid)
	    break;
    }

    child_pid = -1;
    if (wpid == -1)
	syserr("waiting for child");

    (void) signal(SIGPIPE, saved_sigpipe);
    saved_sigpipe = SIG_DFL;

    if (wpid == -1)
	return -1;

    if (st & 0xFF)
    {
	error("child process died%s due to signal %d.",
	      ((st & 0x80) ? " and dumped core" : ""),
	      (st & 0x7F));
	return -1;
    }

    if (verbose)
    {
	message("%s: child process exited with status %d.\n",
		progname, (st >> 8) & 0xFF);
    }

    return ((st >> 8) & 0xFF);
}
