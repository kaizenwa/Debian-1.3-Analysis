/*
#ident	"@(#)smail/src/transports:RELEASE-3_2:pipe.c,v 1.17 1996/03/09 23:50:10 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pipe.c:
 *	deliver mail over a pipe to a program exec'd in a child
 *	process.  Be very careful to keep things secure by setting
 *	the uid and gid in the child process to something appropriate.
 *
 * Specifications for the pipe transport driver:
 *
 *	private attribute data:
 *	    cmd (string):  a string which will be parsed for vectors
 *		to pass to execv.  Some variable expansion will be done,
 *		using the build_cmd_line function.
 *	    user (name):  the name of the user to setuid to in the child
 *		process.  If not specified then the addr structure is
 *		searched for a uid.  If none is found there, use the
 *		nobody_uid.
 *	    group (name):  the name of the group to setgid to in the child
 *		process.  The algorithm used for uid is used here as well.
 *	    umask (int):  umask for child process.
 *
 *	private attribute flags:
 *	    pipe_as_user:  if set, become an appropriate user in the
 *		child process.  Otherwise, become the nobody user.
 *	    ignore_status:  if set, ignore the exit status of the
 *		child process.  Otherwise complain if it is non-zero.
 *	    pipe_as_sender:  become the sender, based on the saved
 *		sender's real uid.
 *	    log_output:  log the stdout and stderr of the child process
 *		to the message log file.  This can then be returned back
 *		to the sender of the message.
 *	    log_output:  log the stdout and stderr of the child process
 *		to the per-message log file.
 *	    ignore_write_errors:  if set, ignore write errors.  Otherwise
 *		complain about them.
 *	    defer_child_errors:  generally, only child failures from the
 *		signal SIGTERM are retried.  If this is set, then retries
 *		are performed if the exit code is non-zero, or if the
 *		write failed on the pipe.
 *          status_to_sender: if set and the exit status of the child process
 *              is non-zero, then report back to the sender.  This interacts
 *              with the ignore_status flag which controls reporting of error
 *              status to the postmaster.
 */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>
#include <signal.h>
#include <errno.h>
#include "defs.h"
#include "../smail.h"
#include "../dys.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../log.h"
#include "../spool.h"
#include "../child.h"
#include "../transport.h"
#include "../exitcodes.h"
#include "pipe.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static char **get_pipe_env();
static void get_pipe_ugid();

static struct attr_table pipe_attributes[] = {
    { "cmd", t_string, NULL, NULL, OFFSET(pipe_private, cmd) },
    { "user", t_string, NULL, NULL, OFFSET(pipe_private, user) },
    { "group", t_string, NULL, NULL, OFFSET(pipe_private, group) },
    { "umask", t_mode, NULL, NULL, OFFSET(pipe_private, umask) },
    { "pipe_as_user", t_boolean, NULL, NULL, PIPE_AS_USER },
    { "pipe_as_sender", t_boolean, NULL, NULL, PIPE_AS_SENDER },
    { "ignore_status", t_boolean, NULL, NULL, PIPE_IGNORE_STATUS },
    { "log_output", t_boolean, NULL, NULL, PIPE_LOG_OUTPUT },
    { "parent_env", t_boolean, NULL, NULL, PIPE_PARENT_ENV },
    { "ignore_write_errors", t_boolean, NULL, NULL, PIPE_IGNORE_WRERRS },
    { "defer_child_errors", t_boolean, NULL, NULL, PIPE_DEFER_ERRORS },
    { "status_to_sender", t_boolean, NULL, NULL, PIPE_STATUS_2SENDER },
};
static struct attr_table *end_pipe_attributes = ENDTABLE(pipe_attributes);


/*
 * tdp_pipe - pipe transport driver
 */
void
tpd_pipe(addr, succeed, defer, fail)
    struct addr *addr;			/* recipient addresses for transport */
    struct addr **succeed;		/* successful deliveries */
    struct addr **defer;		/* defer until a later queue run */
    struct addr **fail;			/* failed deliveries */
{
    register struct pipe_private *priv;	/* pipe driver's private data */
    struct transport *tp = addr->transport;
    char **argv;			/* args to pass to open_pipe */
    FILE *child;			/* child process' stdin */
    int status;				/* exit status from child process */
    int pid;				/* pid of child process */
    char **pipe_env;			/* environment to give to children */
    int uid;				/* uid for child process */
    int gid;				/* gid for child process */
    int errfd;				/* child process output file */
    char *error;			/* error from build_cmd_line() */
    struct error *write_error = NULL;	/* pipe write error message */
    int save_umask;			/* saved value from umask() */

    DEBUG1(DBG_DRIVER_HI, "tpd_pipe called: addr = %s\n", addr->in_addr);
    priv = (struct pipe_private *)tp->private;

    /* subject addresses to retry interval and duration limits */
    addr = retry_addr_before(addr, defer, fail);
    if (addr == NULL) {
	return;
    }

    if (priv->cmd == NULL) {
	/*
	 * ERR_137 - no cmd attribute for pipe
	 *
	 * DESCRIPTION
	 *      No cmd attribute was specified for a pipe transport.  This
	 *      attribute is required.
	 *
	 * ACTIONS
	 *      The message is deferred with a configuration error.
	 *
	 * RESOLUTION
	 *      Correct the entry in the transport file.
	 */
	register struct error *er;

	er = note_error(ERR_CONFERR|ERR_137,
			xprintf("transport %s: no cmd attribute for pipe",
				tp->name));
	insert_addr_list(addr, defer, er);
	return;
    }
    /* get the argument vectors for the execv call */
    argv = build_cmd_line(priv->cmd, addr, "", &error);
    if (argv == NULL) {
	/*
	 * ERR_138 - error in cmd attribute
	 *
	 * DESCRIPTION
	 *      build_cmd_line() encountered an error while parsing the cmd
	 *      attribute.  The specific error was returned in `error'.
	 *
	 * ACTIONS
	 *      Defer the message with a configuration error.
	 *
	 * RESOLUTION
	 *      Correct the entry in the transport file to have a valid,
	 *      expandable cmd attribute.
	 */
	struct error *er;

	er = note_error(ERR_CONFERR|ERR_138,
			xprintf("transport %s: error in cmd attribute: %s",
				tp->name, error));
	insert_addr_list(addr, defer, er);
	return;
    }
    if (argv[0][0] != '/') {
	/*
	 * ERR_139 - absolute path for cmd required
	 *
	 * DESCRIPTION
	 *      The first vector from the cmd attribute must be an absolute
	 *      pathname.  Search paths are not used and relative paths do
	 *      not make sense in smail.
	 *
	 * ACTIONS
	 *      Defer the message with a configuration error.
	 *
	 * RESOLUTION
	 *      Correct the entry in the transport file to ensure that the
	 *      first part of the cmd attribute represents an abolute
	 *      pathname.
	 */
	register struct error *er;

	er = note_error(ERR_CONFERR|ERR_139,
			xprintf("transport %s: absolute path for cmd required",
				tp->name));
	insert_addr_list(addr, defer, er);
	return;
    }

    /* get the environment for the child process */
    pipe_env = get_pipe_env(tp, addr);

    /* get the user id and group id */
    get_pipe_ugid(tp, addr, &uid, &gid);

    /*
     * if we are logging output, tie the output to the per-message log file
     */
    if (tp->flags & PIPE_LOG_OUTPUT) {
	if (! msg_logfile) {
	    open_msg_log();
	}
	fflush(msg_logfile);
	errfd = fileno(msg_logfile);
    } else {
	errfd = -1;
    }

    if (dont_deliver) {
	/* succeed everything */
	insert_addr_list(addr, succeed, (struct error *)NULL);
	return;
    }

    /*
     * create the child process with the set environment.
     * allow writes to process' stdin, redirect stdout/stderr to errfd,
     * or to DEV_NULL.
     */
#ifndef NODEBUG
    if (debug >= DBG_DRIVER_LO && errfile) {
	char **argp;
	char *cp;
	int c;

	fputs("  pipe: exec:", errfile);
	for (argp = argv; *argp; argp++) {
	    fputs(" \"", errfile);
	    for (cp = *argp; (c = *cp++); ) {
		if (c == '\\' || c == '"') {
		    fprintf(errfile, "\\%c", c);
		} else if (isprint(c)) {
		    putc(c, errfile);
		} else {
		    fprintf(errfile, "\\%03o", c & 0xff);
		}
	    }
	    putc('"', errfile);
	}
	putc('\n', errfile);
    }
#endif	/* NODEBUG */
    save_umask = umask(priv->umask);
    pid = open_child(argv, pipe_env, &child, (FILE **)NULL, errfd,
		     CHILD_DEVNULL, uid, gid);
    (void) umask(save_umask);
    if (pid < 0) {
	/*
	 * ERR_140 - could not create process
	 *
	 * DESCRIPTION
	 *      open_child() failed to create a child process.  This is most
	 *      likely a result of fork() failing due to a lack of available
	 *      process slots, or pipe() failing due to a lack of available
	 *      file descriptors.  The specific error should be available in
	 *      errno.
	 *
	 * ACTIONS
	 *      It will probably be possible for fork() or pipe() to succeed
	 *      sometime in the future, unless a configuration error has
	 *      caused too many file descriptors to remain open, which will
	 *      require a configuration change.  Thus, defer the input
	 *      addresses.
	 *
	 * RESOLUTION
	 *      Hopefully delivery will exceed on a later attempt.
	 */
	register struct error *er;

	er = note_error(ERR_140,
			xprintf("transport %s: could not create process: %s",
				tp->name, strerror(errno)));
	insert_addr_list(addr, defer, er);
	return;
    }

    /* write out the message */
    if (write_message(child, addr->transport, addr) ||
	fflush(child) == EOF)
    {
	if (tp->flags & PIPE_IGNORE_WRERRS) {
	    /*
	     * this is a warning only, but log for each input address.
	     * Generally, ignore_write_errors is only turned on for delivery
	     * to shell-command addresses, so this will ususally only
	     * generate one entry in the system log.
	     */
	    struct addr *cur;

	    for (cur = addr; cur; cur = cur->succ) {
		write_log(LOG_SYS,
			 "note: %s ... transport %s: write on pipe failed: %s",
			  addr->in_addr, tp->name, strerror(errno));
	    }
	} else {
	    /*
	     * ERR_141 - write on pipe failed
	     *
	     * DESCRIPTION
	     *      A write to a process created with open_child() failed,
	     *      and ignore_write_errors is not set in the transport.
	     *      This is generally caused by a process exiting before
	     *      reading up to an EOF on standard input.  Sometimes shell
	     *      commands run directly will have side effects but will
	     *      not actually read its stdin.  For example, a forward
	     *      file containing:
	     *
	     *		"|mailx -s \"gone fishing\" $SENDER < $HOME/.fishing"
	     *
	     *      will send a message to the originator of a message
	     *      stating that the recipient is on vacation.  However, as
	     *      it does not actually read the message, a write error
	     *      will occur on the pipe.
	     *
	     *      The above problem can be dealt with by setting the
	     *      ignore_write_errors attribute for the "pipe" transport,
	     *      or be changing the forward file to:
	     *
	     *		"|cat>/dev/null;
	     *		 mailx -s \"gone fishing\" $SENDER < $HOME/.fishing"
	     *
	     *      which will read and ignore the standard input.
	     *
	     *      The last possibility is that a command failed before
	     *      reading all of stdin, or the exec() failed in
	     *      open_child().
	     *
	     * ACTIONS
	     *      Fail the input addresses and send an error to the
	     *      address owner or to the postmaster.  However, if
	     *      close_child() returns a non-zero exit status, this error
	     *      has precedence over write-errors, as it represents a
	     *      more important error.  The exception is that if
	     *      ignore_status is set, then the write-error message is
	     *      always returned.
	     *
	     *	    If defer_child_errors is set, then defer rather than
	     *	    fail.
	     *
	     * RESOLUTION
	     *      If the problem is a shell command, either scold the user
	     *      who had the offending forward file, or set
	     *      ignore_write_errors in the transport file entry.
	     */
	    write_error =
		note_error(ERR_NPOWNER|ERR_141,
			   xprintf("transport %s: write error on pipe: %s",
				   tp->name, strerror(errno)));
	}
    }

    status = close_child(child, (FILE *)NULL, pid);
    if (status == EOF && (tp->flags & PIPE_IGNORE_STATUS) == 0) {
	/*
	 * ERR_142 - failed to reap child process
	 *
	 * DESCRIPTION
	 *      For some reason, close_pipe() failed to find the child
	 *      process.  This should never happen.
	 *
	 * ACTIONS
	 *	Fail the input addresses and Notify the postmaster of the
	 *	error.  Note: write errors have precedence over this error.
	 *
	 * RESOLUTION
	 *      This is most likely a bug in either smail or the UNIX
	 *      kernel.
	 */
	if (write_error == NULL) {
	    register struct error *er;

	    er = note_error(ERR_NPOSTMAST|ERR_142,
		      xprintf("transport %s: failed to reap child process: %s",
			      tp->name, strerror(errno)));
	    insert_addr_list(addr, fail, er);
	    return;
	}
    } else if (status != 0) {
	if (status & 0xff) {
	    /*
	     * ERR_143 - process killed by signal
	     *
	     * DESCRIPTION
	     *      The exit status returned by close_child() revealed that
	     *      the child process was killed by a signal.  This could be
	     *      due to the machine being brought down, if the signal is
	     *      SIGTERM.  Other signals may represent genuine problems.
	     *
	     * ACTIONS
	     *      If the child was killed with SIGTERM, defer the input
	     *      addresses, otherwise fail the addresses and notify the
	     *      postmaster.  This is probably not a problem that address
	     *      owners should have to deal with.  If ignore_status is
	     *      set, then the problem is ignored (except for SIGTERM).
	     *
	     * RESOLUTION
	     *      Time to use your skill in tracking down unusual
	     *      problems, exept in the case of SIGTERM.
	     */
	    char *errstr = xprintf("transport %s: process killed by signal %d",
				  tp->name, status & 0x7f);
	    if ((status & 0x7f) == SIGTERM) {
		insert_addr_list(addr, defer, note_error(ERR_143, errstr));
		return;
	    } if (tp->flags & PIPE_IGNORE_STATUS) {
		/*
		 * log errors in the system log file, but don't do anything
		 * about them, if ignore_status is set
		 */
		struct addr *cur;

		for (cur = addr; cur; cur = cur->succ) {
		    write_log(LOG_SYS, "note: %s ... %s", cur->in_addr, errstr);
		}
	    } else {
		insert_addr_list(addr,
				 (tp->flags&PIPE_DEFER_ERRORS)? defer: fail,
				 note_error(ERR_NPOSTMAST|ERR_143, errstr));
		return;
	    }
	} else {
	    /*
	     * ERR_144 - process returned non-zero status
	     *
	     * DESCRIPTION
	     *      close_child() reaped a non-zero exit status from the
	     *      child process.  This could be okay or it could be bad,
	     *      it is difficult to tell.
	     *
	     * ACTIONS
	     *      If the ignore_status attribute is set, then this
	     *      situation is ignored, except that it is logged in the
	     *      system file.  Otherwise, the addresses are failed and a
	     *      message is returned to the address owner or the
	     *      postmaster.  The status_to_sender flag (late addition)
	     *      modifies this action in that a message is additional
	     *      sent to the sender.
	     *
	     * RESOLUTION
	     *      The program run by the transport should be checked to
	     *      determine what the various exit codes mean, and if the
	     *      status is significant.  If the exit status is not
	     *      significant, then ignore_status should be set for the
	     *      transport.
	     */
	    char *errstr =
		xprintf("transport %s: child returned status %s (%d)",
			tp->name, strsysexit(status>>8), status>>8);

	    if ((tp->flags & (PIPE_IGNORE_STATUS | PIPE_STATUS_2SENDER)) == PIPE_IGNORE_STATUS) {
		/*
		 * log errors in the system log file, but don't do anything
		 * about them, if ignore_status is set
		 */
		struct addr *cur;

		for (cur = addr; cur; cur = cur->succ) {
		    write_log(LOG_SYS, "note: %s ... %s", cur->in_addr, errstr);
		}
	    } else {
		insert_addr_list(addr,
				 (tp->flags&PIPE_DEFER_ERRORS)? defer: fail,
				 note_error(((tp->flags & PIPE_STATUS_2SENDER) ? ERR_NSOWNER : ERR_NPOWNER)
					    |ERR_144, 
					    errstr));
		return;
	    }
	}
    }

    if (write_error) {
	/* write_error, but no high-precedence error, was found, fail */
	insert_addr_list(addr,
			 (tp->flags&PIPE_DEFER_ERRORS)? defer: fail,
			 write_error);
    } else {
	/* everything went okay, link into the succeed list */
	insert_addr_list(addr, succeed, (struct error *)NULL);
    }
    return;
}

/*
 * tpb_pipe - read the configuration file attributes
 */
char *
tpb_pipe(tp, attrs)
    struct transport *tp;		/* transport entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct pipe_private pipe_template = {
	NULL,				/* cmd */
	NULL,				/* user */
	NULL,				/* group */
	0,				/* umask */
    };
    struct pipe_private *priv;		/* new pipe_private structure */

    /* copy the template private data */
    priv = (struct pipe_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&pipe_template, sizeof(*priv));

    /* set default flags */
    tp->flags |= PIPE_AS_USER | PIPE_LOG_OUTPUT;

    tp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &tp->flags,
			    pipe_attributes,
			    end_pipe_attributes);

    if (error) {
	return error;
    }
    return NULL;
}



/*
 * tpp_pipe - dump the configuration attributes
 */
void
tpp_pipe(f, tp)
     FILE * f;
     struct transport *tp;
{
    (void) dump_standard_config(f,
				tp->private,
				tp->name,
				tp->flags,
				pipe_attributes,
				end_pipe_attributes);
}



/*
 * get_pipe_env - return an environment suitable for the child process
 */
static char **
get_pipe_env(tp, addr)
    struct transport *tp;		/* transport being used */
    struct addr *addr;			/* addrs being delivered to */
{
    static char *pipe_env[50];		/* lots of space for variables */
    static int inited = FALSE;		/* true if pipe_env is set up */
    char **next_env;			/* for stepping through pipe_env */
    static char **per_address_env;	/* start of per-address environment */
    char *p;
    struct addr *cur;			/* temp */

    if (! inited) {
	extern char *getenv();

	inited = TRUE;

	/* load environment variables which vary per-message, not per addr */

	next_env = pipe_env;
#ifdef SHELL_EXEC_PATH
	*next_env++ = xprintf("SHELL=%s", SHELL_EXEC_PATH);
#else
	*next_env++ = COPY_STRING("SHELL=/bin/sh");
#endif

#ifdef SECURE_PATH
	*next_env++ = xprintf("PATH=%s", SECURE_PATH);
#else
	*next_env++ = "PATH=/bin:/usr/bin";
#endif

	p = getenv("TZ");
	if (p) {
	    *next_env++ = xprintf("TZ=%s", p);
	}
	*next_env++ = xprintf("SENDER=%s", get_sender_addr(tp));
	*next_env++ = xprintf("MESSAGE_ID=%s", message_id);
	*next_env++ = xprintf("GRADE=%c", msg_grade);
	*next_env++ = xprintf("UUCP_NAME=%s", uucp_name);
	*next_env++ = xprintf("PRIMARY_NAME=%s", primary_name);
	*next_env++ = xprintf("VISIBLE_NAME=%s", visible_name);
	*next_env++ = xprintf("BASENAME=%s", spool_fn);
	*next_env++ = xprintf("SPOOL_FILE=%s/%s", spool_dir, input_spool_fn);

	per_address_env = next_env;
    } else {
	/* free up per-invocation storage used previously */
	char **pp;

	for (pp = per_address_env; *pp; pp++) {
	    xfree(*pp);
	    *pp = NULL;
	}
    }

    /* load environment variables which change per addr, get from first addr */
    next_env = per_address_env;

    if ((tp->flags & PIPE_PARENT_ENV) && addr->parent) {
	/* use parent addr structure for information stuffed in environment */
	cur = addr->parent;
	if (cur->remainder) {
	    *next_env++ = xprintf("ADDR=%s", cur->remainder);
	}
	if (cur->target && cur->target[0]) {
	    *next_env++ = xprintf("HOST=%s", cur->target);
	}
	*next_env++ = xprintf("HOME=%s", cur->home? cur->home: "/");
    } else {
	cur = addr->parent;
	if (cur && cur->remainder) {
		*next_env++ = xprintf("DESTMBOX=%s", cur->remainder);
	}
	if (addr->next_addr) {
	    *next_env++ = xprintf("ADDR=%s", addr->next_addr);
	}
	if (addr->next_host) {
	    *next_env++ = xprintf("HOST=%s", addr->next_host);
	}
	*next_env++ = xprintf("HOME=%s", addr->home? addr->home: "/");
    }

    /* is there a suitable username to go into the environment */
    for (cur = addr; cur && ! (cur->flags & ADDR_ISUSER); cur = cur->parent) ;
    if (cur) {
	/* yes */
	register char *user = cur->remainder;

	*next_env++ = xprintf("USER=%s", user);
	*next_env++ = xprintf("LOGNAME=%s", user);
    }

    /* end of environment */
    *next_env = NULL;

    return pipe_env;
}

/*
 * get_pipe_ugid - return the uid and gid to use for the child process
 */
static void
get_pipe_ugid(tp, addr, uid, gid)
    struct transport *tp;		/* associated transport structure */
    struct addr *addr;			/* associated addr structures */
    int *uid;				/* store uid here */
    int *gid;				/* store gid here */
{
    struct pipe_private *priv = (struct pipe_private *)tp->private;
    /*
     * determine the uid to use for delivery
     */
    if (priv->user == NULL) {
	if ((tp->flags & PIPE_AS_SENDER)) {
	    *uid = real_uid;
	} else if ((tp->flags & PIPE_AS_USER) && addr->uid != BOGUS_USER) {
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
	    if ((tp->flags & PIPE_AS_SENDER)) {
		*gid = prog_egid;
	    } else if ((tp->flags & PIPE_AS_USER) && addr->gid != BOGUS_GROUP) {
		*gid = addr->gid;
	    } else {
		*gid = nobody_gid;
	    }
	}
    }
}
