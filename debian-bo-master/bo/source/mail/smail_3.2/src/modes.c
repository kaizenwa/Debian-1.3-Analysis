/*
#ident	"@(#)smail/src:RELEASE-3_2:modes.c,v 1.54 1996/05/29 14:50:20 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * modes.c:
 *	routines to handle the various modes of operation.  Typically,
 *	these are major functions called directly from main.
 *
 *	external functions: build_host_strings, compute_nobody,
 *			    input_signals, processing_signals,
 *			    deliver_signals, test_addresses,
 *			    perform_deliver_mail, deliver_mail,
 *			    daemon_mode, noop_mode, verify_addresses,
 *			    print_version, print_copying_file,
 *			    print_variables, print_queue, smtp_mode,
 *			    fork_wait
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "defs.h"
#include <sys/stat.h>
#include <pwd.h>
#include <signal.h>
#include <errno.h>

#ifndef ISC_SOCKET_TIME_BUG
# include <time.h>
#endif
#if defined(UNIX_BSD) && !defined(POSIX_OS)
# include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include "smail.h"
#include "alloc.h"
#include "dys.h"
#include "addr.h"
#include "hash.h"
#include "main.h"
#include "log.h"
#include "transport.h"
#include "child.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

#if defined(POSIX_OS) || defined(UNIX_BSD) || defined(WAIT_USE_UNION)
# include <sys/wait.h>
#endif

#if (defined(UNIX_BSD) || defined(WAIT_USE_UNION)) && !defined(NO_WAIT_USE_UNION)
# define STATUS_TYPE	union wait
#else
# define STATUS_TYPE	int
#endif

#ifdef ANSI_C
# define VOLATILE volatile
# define P_(x) x
#else
# define VOLATILE /**/
# define P_(x) ()
#endif

#if !defined(SIGCHLD) && defined(SIGCLD)

/* System V uses a different name */
# define SIGCHLD SIGCLD

#endif

/* variables exported from this file */
int daemon_pid = 0;

/* variables local to this file */
#ifdef SIGCHLD
static VOLATILE int smtp_accept_count;
static int smtp_accept_slots;
static VOLATILE int *smtp_pids;
#endif

/* variables imported from main.c */
extern char *smtp_service_name;

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static int start_daemon();
static void bg_run_queue();
static void do_run_queue();
static void sig_unlink();
static void sig_close();
static void set_queue_only();
static void do_smtp();
static void error_resolve_timeout P_((struct addr ** defer, struct addr ** fail));

/*
 * Interactive UNIX 2.2 has a bug in accept().  If accept() is
 * interrupted by an alarm signal, accept() does not return from
 * waiting for a connection with errno set to EINTR.  Unfortunately
 * this is necessary for smail to process its mail queues at regular
 * intervals, as specified with the -q option.
 *
 * Interactive's select() does work correctly, however.  Thus,
 * we use select() to determine when to call accept(), and catch
 * alarm signals out of select(), instead of out of accept().
 */

#ifdef ISC_ACCEPT_BUG
fd_set	fds_used, fds_read;
#endif


/*
 * build_host_strings - build the various types of hostnames
 *
 * always build primary_name.  Build, if NULL, uucp_name, hostnames,
 * and visible_name.
 */
void
build_host_strings()
{
    char *s;

    if (hostnames == NULL || uucp_name == NULL) {
	char *real_domain = NULL;
	char *real_hostname = compute_hostname();

	if (real_hostname == NULL) {
	    /* the machine doesn't know who he is */
	    panic(EX_SOFTWARE,
		  "build_host_strings: Configuration error: hostname unknown");
	    /*NOTREACHED*/
	}

	if ((s = index(real_hostname, '.')) != NULL) {
	    *s++ = '\0';
	    real_domain = s;
	}

	if (uucp_name == NULL) {
	    /* uucp_name is exactly the real hostname by default */
	    uucp_name = real_hostname;
	}
	if (hostnames == NULL) {
	    /*
	     * by default hostnames is constructed from the real hostname
	     * and the visible_domains list.  If visible_domains is NULL,
	     * then hostnames is exactly the real hostname.
	     * But first lets try to get a domain if we need one...
	     */
	    if (visible_domains == NULL || visible_domains[0] == '\0') {
		if (real_domain) {
		    visible_domains = real_domain;
		} else {
		    visible_domains = compute_domain(real_hostname);
		}
	    }
	    if (visible_domains == NULL || visible_domains[0] == '\0') {
		hostnames = real_hostname;
	    } else {
		register char *domain = strcolon(visible_domains);
		struct str str;		/* build hostnames here */

		STR_INIT(&str);
		str_printf(&str, "%s.%s", real_hostname, domain);
		while ((domain = strcolon((char *)NULL))) {
		    str_printf(&str, ":%s.%s", real_hostname, domain);
		}
		STR_NEXT(&str, '\0');
		STR_DONE(&str);

		hostnames = str.p;
	    }
	}
    }

    /* primary_name is always the first hostname value */
    primary_name = hostnames;

    s = index(hostnames, ':');
    if (s) {
	/* In ANSI C string literals can be put in unwritable text space.
	 * Thus, rather than just put a nul byte to separate primary_name
	 * and hostnames, we must malloc something and build the
	 * primary_name */
	char *new_pd = xmalloc((unsigned) (s - primary_name + 1));

	(void) memcpy(new_pd, primary_name, (size_t) (s - primary_name));
	new_pd[s - primary_name] = '\0';
	primary_name = new_pd;
    }

    /* visible_name is the primary_name by default */
    if (visible_name == NULL) {
	visible_name = primary_name;
    }
}

/*
 * compute_nobody - figure out the nobody uid/gid
 *
 * if `nobody_uid' and `nobody_gid' are defined, use them, otherwise
 * use the login name in `nobody' to determine nobody_uid/gid.
 */
void
compute_nobody()
{
    if (nobody_uid != BOGUS_USER && nobody_gid != BOGUS_GROUP) {
	return;
    }

    if (nobody == NULL || nobody[0] == '\0') {
	/*
	 * nobody uid/gid not defined.  use something likely to not be
	 * in use
	 */
	nobody_uid = 32767;
	nobody_gid = 32767;
    } else {
	struct passwd *pw;		/* passwd entry for `nobody' */

	pw = getpwbyname(nobody);
	if (pw == NULL) {
	    nobody_uid = 32767;
	    nobody_gid = 32767;
	} else {
	    nobody_uid = pw->pw_uid;
	    nobody_gid = pw->pw_gid;
	}
    }
}


/*
 * input_signals - setup signals to use when reading a message from stdin
 *
 * when reading in a message (for DELIVER_MAIL mode), the spool file should
 * be removed if a SIGHUP or SIGINT comes in, as this supposedly indicates
 * that the user did not complete his input message.  If a SIGTERM comes
 * in then set the queue_only flag, to avoid taking up lots of time.
 */
void
input_signals()
{
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGHUP, sig_unlink);
    }
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGINT, sig_unlink);
    }
    (void) signal(SIGALRM, SIG_IGN);
    (void) signal(SIGTERM, set_queue_only);
}

/*
 * processing_signals - signals to use when processing a message
 *
 * in this case, ignore hangups but still allow the user to send an
 * interrupt (if mode is DELIVER_MAIL), up until the time delivery is
 * started.  SIGTERM will close the spool file for now.
 */
void
processing_signals()
{
    (void) signal(SIGHUP, SIG_IGN);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
	if (operation_mode == DELIVER_MAIL) {
	    (void) signal(SIGINT, sig_unlink);
	} else {
	    (void) signal(SIGINT, sig_close);
	}
    }
    (void) signal(SIGALRM, SIG_IGN);
    (void) signal(SIGTERM, sig_close);
}

/*
 * delivery_signals - signals to use when delivering a message
 *
 * in this case, ignore everything to avoid stopping in awkward states.
 *
 * TODO: perhaps SIGTERM should set a flag to cause smail to exit between
 *	 calls to transport drivers.  Inbetween calls, the state will not
 *	 be inconsistent and it should be okay to call close_spool().
 */
void
delivery_signals()
{
    (void) signal(SIGHUP, SIG_IGN);
    (void) signal(SIGINT, SIG_IGN);
    (void) signal(SIGINT, SIG_IGN);
    (void) signal(SIGTERM, SIG_IGN);
}

/*
 * sig_unlink - handle a signal by unlinking the spool file.
 *
 * we assume this means that a user didn't really want to send a message
 * after all so we remove the spooled message and exit.
 */
static void
sig_unlink(sig)
    int sig;
{
    (void) signal(sig, SIG_IGN);
    unlink_spool();
    write_log(LOG_TTY, "interrupt: mail message removed");
    exit(EX_OSERR);
}

/*
 * set_queue_only - handle a signal by setting the flag queue_only
 *
 * this will cause the message to be read in, but not processed.  Thus,
 * the amount of time spent on processing the message is minimized, while
 * full message processing can be attempted later.
 */
static void
set_queue_only(sig)
    int sig;
{
    (void) signal(sig, set_queue_only);
    queue_only = TRUE;
}

/*
 * sig_close - handle a signal by closing the spool file.
 *
 * this will cause processing to stop for the current message.  However,
 * it should be restartable later from a queue run.
 */
static void
sig_close(sig)
    int sig;
{
    (void) signal(sig, SIG_IGN);
    close_spool();
    exit(0);				/* this is not yet an error */
}


/*
 * test_addresses - read addrs from stdin and route them, for fun
 *
 * Call parse_address and route_remote_addrs to determine which transport
 * is going to be used, and what it will be given, for addresses given on
 * stdin.
 */
void
test_addresses()
{
    char line[4096];			/* plenty of space for input lines */
    int stdin_is_a_tty = isatty(0);

    X_PANIC_OKAY();
    if (primary_name == NULL) {
	/* setup all of the hostname information */
	build_host_strings();
    }

    while (stdin_is_a_tty && fputs("> ", stdout), 
	   fgets(line, sizeof(line), stdin)) {
	struct addr *cur = alloc_addr();
	struct addr *done;
	struct addr *retry;
	struct addr *defer;
	struct addr *fail;
	char *error;
	int form;
	char *lp = line;

	strip_rfc822_comments(lp);
	if (*lp == '\0')
		continue;

	cur->in_addr = lp;
	if ((cur->work_addr = preparse_address(lp, &error)) == NULL) {
	    write_log(LOG_TTY, "syntax error in address: %s", error);
	    continue;
	}

	done = NULL;
	retry = NULL;
	defer = NULL;
	fail = NULL;
	while (cur &&
	       (form = parse_address(cur->work_addr, &cur->target,
				     &cur->remainder, &cur->parseflags)
		) != FAIL && form != LOCAL)
	{
	    cur->flags &= ~ADDR_FORM_MASK;
	    cur->flags |= form;

	    done = NULL;
	    retry = NULL;
	    defer = NULL;
	    fail = NULL;
	    if (islocalhost(cur->target)) {
		cur->work_addr = cur->remainder;
		continue;
	    }
	    route_remote_addrs(cur, &done, &retry, &defer, &fail);
	    cur = retry;
	}

	if (defer) {
	    (void) fprintf(stderr, "%s ... temporary failure: %s\n",
			   defer->in_addr, defer->error->message);
	    continue;
	}
	if (fail) {
	    (void) fprintf(stderr, "%s ... failed: %s\n",
			   fail->in_addr, fail->error->message);
	    continue;
	}
	if (done) {
	    (void) printf("host: %s\naddr: %s\ntransport: %s\n",
			  done->next_host? done->next_host: "(local)",
			  done->next_addr,
			  done->transport->name);
	    continue;
	}

	switch (form) {
	case FAIL:
	    (void) fprintf(stderr, "%s ... parse error: %s\n",
			   cur->in_addr, cur->remainder);
	    break;

	case LOCAL:
	    (void) printf("addr: %s\ntransport: local\n", cur->remainder);
	    break;

	default:
	    (void) fprintf(stderr,
			   "%s ... internal error in resolve_addr_list\n",
			   line);
	    break;
	}
    }
}


/*
 * perform_deliver_mail - read in a message and call deliver_mail()
 *
 * Build a queue file using a message on stdin.  Then, if we are
 * performing immediate delivery of messages, call deliver_mail() to
 * deliver the message.
 */
void
perform_deliver_mail()
{
    char *error;

    /* setup signals to remove the spool file on errors */
    X_NO_PANIC();
    input_signals();
    if (queue_message(stdin, dot_usage, recipients, &error) == FAIL) {
	open_system_logs();
	log_spool_errors();
	panic(EX_OSFILE, "incoming mail lost: %s: %s", error, strerror(errno));
	/*NOTREACHED*/
    }
    X_PANIC_OKAY();

    /*
     * if we are running as rmail or rsmtp, then always return a zero
     * exitstatus for errors that occur after successfully spooling
     * the message.  Otherwise, the UUCP subsystem (which calls rmail
     * or rsmtp for mail delivery) may return error messages to the
     * sender, even though smail will now be in complete control of
     * error handling on this message.
     */
    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
	force_zero_exitvalue = TRUE;
    }

    if (read_message() == NULL) {
	panic(EX_OSFILE, "failed to read queued message");
	/*NOTREACHED*/
    }

    /*
     * if a precedence: header is given
     * then change the grade for the mail message
     */
    check_grade();

    /*
     * up til now keyboard signals would have caused mail to be
     * removed.  Now that we actually have the message, setup
     * signals appropriate for guarranteeing delivery or panics
     * on errors.
     */
    processing_signals();

    /*
     * open the system and per message log files.
     * Do this after spool_message so that panic errors while opening
     * the log files do not dump the mail on the floor.
     */
    open_system_logs();

    /*
     * make a log entry for the new message
     */
    log_incoming();

    /* log errors generated in spooling the message */
    log_spool_errors();

    /* if we are only queuing, we have gone as far as we need to */
    if (queue_only || deliver_mode == QUEUE_MESSAGE 
	|| (msg_grade < min_delivery_grade) 
	|| (msg_grade > max_delivery_grade)) {
	if (debug && dont_deliver) {
	    /* unless we are debugging as well */
	    DEBUG(DBG_MAIN_LO,
		  "debugging is on, -Q (queue_only) flag ignored\n");
	} else {
	    if (debug) {
		DEBUG(DBG_MAIN_LO,
		      "-Q (queue_only) specified and message is queued\n");
	    }
	    close_spool();
	    return;
	}
    }

    /*
     * if we are delivering in background, fork a child to perform
     * delivery and exit.  Ignore this when debugging.
     */
    if (deliver_mode ==  BACKGROUND) {
	int pid;

	/* unlock the message in the parent, see lock_message() for details */
	delivery_signals();		/* disassociate from terminal */
	if (error_processing != TERMINAL) {
	    (void) fclose(stdin);
	}
	unlock_message();
	pid = fork();
	if (pid < 0) {
	    /* fork failed, just leave the queue file there and exit */
	    write_log(LOG_TTY, "fork failed: %s, message queued", strerror(errno));
	    close_spool();
	    return;
	}
	if (pid > 0) {
	    /* in parent process, just return */
	    return;
	}
#ifdef POSIX_OS
	(void) setsid();
#else	/* not POSIX_OS */
	(void) setpgrp(0, getpid());
#endif	/* POSIX_OS */
	if (lock_message() == FAIL) {
	    /* somebody else grabbed the lock, let them deliver */
	    return;
	}
    }

    /* read the various configuration files */
    if ((error = read_transport_file()) ||
	(error = read_router_file()) ||
	(error = read_director_file()) ||
	(error = read_qualify_file()) ||
	(error = read_retry_file()))
    {
	panic(EX_OSFILE, "%s", error);
    }

    /* setup all of the hostname information */
    build_host_strings();

    /* figure out the nobody uid/gid */
    compute_nobody();

    /*
     * process the message, find all of the recipients and
     * perform delivery.
     */
    deliver_mail();

    /*
     * close the system-wide log files
     */
    close_system_logs();
}

/*
 * deliver_mail - oversee the delivery of mail (default mailer operation)
 *
 * Spool the mail, process the header, process the addresses, route,
 * alias expand, deliver remote mail, deliver local mail, process errors.
 */
void
deliver_mail()
{
    struct addr *cur;			/* addr being processed */
    struct addr *next;			/* next addr to process */
    struct addr *fail= NULL;		/* list of failed addrs */
    struct addr *route_list = NULL;	/* list of addrs to route */
    struct addr *defer = NULL;		/* list of deferred addrs */
    struct addr *deliver;		/* addr structures ready to deliver */
    /* transport instances */
    struct assign_transport *assigned_transports = NULL;
    char *error;
    struct identify_addr *sent_errors;	/* addresses previously sent errors */
    struct defer_addr *defer_errors;	/* previous defer messages */

    /*
     * This code attempts to optimise the reprocessing of queued mail
     * by putting sucessfully delivered addresses into the hash table
     * to prevent them being put through the routers.
     */
    hash_predelivered_addresses();
    open_msg_log();			/* open msglog in case needed */

    /*
     * preparse all of the recipient addresses given as arguments.
     * If we are extracting addresses from the header, then
     * these addresses are NOT to receive the mail.  To accomplish
     * this, add them to the hash table so they will be ignored
     * later.
     */
    route_list = NULL;
    for (cur = recipients; cur; cur = next) {
	next = cur->succ;
	split_addr_list(cur->in_addr, &route_list);
    }
    for (cur = route_list, route_list = NULL; cur; cur = next) {
	next = cur->succ;
	if ((cur->work_addr =
	     preparse_address(cur->in_addr, &error)) == NULL)
	{
	    /*
	     * ERR_147 - parse error in input address
	     *
	     * DESCRIPTION
	     *      preparse_address() encountered a parsing error in one of
	     *      the addresses supplied by the sender.  The specific
	     *      error was returned in `error'.
	     *
	     * ACTIONS
	     *      Fail the address and send an error to the sender.
	     *
	     * RESOLUTION
	     *      The sender should supply a valid address.
	     */
	    cur->error = note_error(ERR_NSENDER|ERR_147,
				    xprintf("parse error %s", error));
	    cur->flags &= ~ADDR_FORM_MASK;
	    cur->flags |= PARSE_ERROR;
	    cur->succ = fail;
	    fail = cur;
	    continue;
	}
	if (extract_addresses) {
	    (void) add_to_hash(cur->work_addr, (char *)NULL, 0, hit_table);
	    xfree(cur->work_addr);	/* don't need it anymore */
	    xfree((char *)cur);
	    continue;
	}
	cur->succ = route_list;
	route_list = cur;
    }

    if (extract_addresses) {
	route_list = NULL;		/* don't need them anymore */
    }

    /*
     * process_header will perform a preliminary analysis of the
     * header fields.  It will note which standard fields exist
     * and may take addresses from the header.  It will perform
     * some initial processing of the From: lines and, depending
     * upon configuration, may put `,' characters between addresses.
     * Also, some required fields which do not exist will be
     * added, (i.e., From: and To: and Message-Id:).
     */
    if (extract_addresses) {
	error = process_header(&route_list);
    } else {
	error = process_header((struct addr **)NULL);
    }
    if (error) {
	write_log(LOG_MLOG, "error in header: %s", error);
	if (extract_addresses) {
	    return_to_sender = TRUE;
	    /* notify people of errors, ignoring previously reported errors */
	    sent_errors = NULL;
	    defer_errors = NULL;
	    (void) process_msg_log((struct addr *)NULL, &sent_errors,
				   &defer_errors);
	    notify((struct addr *)NULL,	/* no defer or fail list */
		   (struct addr *)NULL,
		   sent_errors);
	    unlink_spool();
	}
	return;
    }

    /*
     * given the list of recipient addresses, turn those
     * addresses into more specific destinations, including
     * the transport that is to be used, in the case of
     * addresses destined remote
     */
    deliver = NULL;
    resolve_addr_list(route_list, &deliver, &defer, &fail, TRUE);

    if (deliver == NULL && defer == NULL) {
	write_log(LOG_MLOG, "no valid recipients were found for this message");
	return_to_sender = TRUE;
    }

    if (defer != NULL) {
	long message_age = (long) time((long *) NULL) - (long) message_date();
	if (message_age > resolve_timeout) {
	    /*
	     * This message has been waiting for delivery longer than the
	     * resolve_timeout, so we convert all defers into errors
	     */
	    error_resolve_timeout(&defer, &fail);
	    return_to_sender = TRUE;
	}
    }

    /*
     * remove addresses to which we have already delivered mail and
     * note addresses for which we have already delivered error messages
     */
    sent_errors = NULL;
    defer_errors = NULL;
    deliver = process_msg_log(deliver, &sent_errors, &defer_errors);

    /*
     * log failures right now
     */
    if (fail) {
	fail_delivery(fail);
    }

    /*
     * assign instances of transports for remote addresses
     */
    assigned_transports = assign_transports(deliver);

    /*
     * deliver all of the assigned mail.  Note: call_transport
     * will already have handled log entries for failed addresses.
     */
    delivery_signals();
    call_transports(assigned_transports, &defer, &fail);
    if (defer) {
	defer_delivery(defer, defer_errors);
    }

    /*
     * perform error notification for all failed and perhaps some deferred
     * addresses.  Addresses for which we have already sent error messages
     * are ignored.
     */
    notify(defer, fail, sent_errors);

    /*
     * tidy up before going away
     */
    if (call_defer_message) {
	/*
	 * leave a file in an error/ directory for the system
	 * administrator to look at.  This is used for failed error
	 * mail and for problems resulting from configuration errors.
	 */
	defer_message();
    } else if (some_deferred_addrs) {
	/*
	 * leave the file around to be processed by a later queue run.
	 * Use this for temporary problems such as being blocked by a
	 * locked file, or timeouts waiting for a response from a
	 * remote system.
	 */
	close_spool();
    } else {
	/*
	 * if no requests for deferring of addresses or of the message
	 * occured, then we are done with the message.  Thus, unlink
	 * the message and the per-message log file.
	 */
	write_log(LOG_SYS, "Completed.");
	unlink_spool();
	unlink_msg_log();
    }
}


static void daemon_sighup();
static void daemon_sigalrm();
#ifdef SIGCHLD
static void daemon_sigchld();
static void reap_child();
#endif
static int got_sighup;
static int got_sigalrm;

#if	defined(HAVE_BSD_NETWORKING)
static void do_daemon_accept();

/*
 * daemon_mode - be a daemon waiting for requests
 *
 * Listen on the smtp port for connections.  Accept these connections and
 * read smtp commands from them.
 */
void
daemon_mode()
{
    int ls;				/* listen socket */
    int as;				/* accept socket */
    struct sockaddr_in sin, from;	/* from is currently  */
    struct servent *smtp_service;	/* smtp service file entry */
    struct hostent *hostentp;		/* host file entry */
    int port;
    int accept_err_cnt = 0;
#ifdef ISC_ACCEPT_BUG
    int nsel;
#endif
    int optval = 1;

    X_PANIC_OKAY();

    /*
     * don't use background delivery mode.  Since forked smtp connection
     * handlers are in background anyway, the extra child process level
     * could only serve to mess up the count of child processes kept for
     * comparison with the smtp_accept_max value.
     */

    if (deliver_mode == BACKGROUND)
	deliver_mode = FOREGROUND;

    /*
     * we aren't interested in the old stdin or stdout, substitute
     * /dev/null
     */
    (void) close(0);
    (void) close(1);
    open("/dev/null", 0);
    dup(0);

    /* setup the listen socket */
    if (smtp_service_name == NULL) {
	smtp_service_name = "smtp";
    }
    if (isdigit(smtp_service_name[0])) {
	port = htons(atoi(smtp_service_name));
    } else {
	if ((smtp_service = getservbyname(smtp_service_name, "tcp")) == NULL) {
	    write_log(LOG_SYS, "%s/tcp: unknown service", smtp_service_name);
	    exitvalue = EX_UNAVAILABLE;
	    return;
	}
	port = smtp_service->s_port;
    }
    (void) bzero((char *)&sin, sizeof(sin));
    ls = socket(AF_INET, SOCK_STREAM, 0);
    if (ls < 0) {
	write_log(LOG_SYS, "socket(AF_INET, SOCKSTREAM, 0) failed: %s",
		  strerror(errno));
	exitvalue = EX_OSERR;
	return;
    }
    sin.sin_family = AF_INET;
    if (listen_name) {
	hostentp = gethostbyname(listen_name);
	if (!hostentp) {
	    open_system_logs();
	    log_spool_errors();
	    panic(EX_OSFILE, "config error: host %s not found%s", listen_name,
		  strerror(errno));
	    /* NOTREACHED */
	}
	memcpy(&sin.sin_addr, hostentp->h_addr_list[0], sizeof(struct in_addr));
	DEBUG1(DBG_MAIN_LO, "listen on ip addr [%s]\n", inet_ntoa(sin.sin_addr));
    } else {
	sin.sin_addr.s_addr = INADDR_ANY;
    }
    sin.sin_port = port;

    /*
     * set SO_REUSEADDR so that the daemon can be restarted while
     * a connection is being handled.  Without this, a connection
     * alone will prevent reuse of the smtp port number for listening
     * purposes.
     */

    /* XXX: optval argument is (const char *) on Solaris-2 and (void *) in Net/3 */
    if (setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) < 0) {
	write_log(LOG_SYS, "SO_REUSEADDR on failed: %s\n", strerror(errno));
	exitvalue = EX_OSERR;
	return;
    }

    if (bind(ls, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
	write_log(LOG_SYS, "bind() failed: %s", strerror(errno));
	exitvalue = EX_OSERR;
	return;
    }

    /*
     * start smail as a background daemon process.  Return in the
     * parent.
     */
    if (start_daemon() != 0) {
	return;
    }

#ifdef SIGCHLD
    /* NOTE:  If there is no SIGCHLD we can't reap dead kids!  lose */
    (void) signal(SIGCHLD, daemon_sigchld);

    /* assure that max connects is a sane value */
    if (smtp_accept_max < 0 || smtp_accept_max > 4095) {
	smtp_accept_max = 0;
    }
    smtp_accept_slots = smtp_accept_max;
    /* assure that max connects until queue only is a sane value */
    if (smtp_accept_queue < 0 || smtp_accept_queue > 4095) {
	smtp_accept_queue = 0;
    }
    if (smtp_accept_queue > smtp_accept_slots)
	smtp_accept_slots = smtp_accept_queue;

    /* allocate space for listener pids (if we're keeping track) */
    if (smtp_pids == NULL && smtp_accept_slots > 0) {
	int i;

	smtp_pids = (int *) xmalloc((unsigned) (smtp_accept_slots * sizeof(int)));
	for (i = 0; i < smtp_accept_slots; ++i)
	    smtp_pids[i] = 0;
    }
#endif /* SIGCHLD */

    /* if we are doing queue runs, do one queue run first */
    if (process_queue) {
	bg_run_queue(-1);
	if (queue_interval == 0) {
	    /* Already done this, so can switch it out */
	    process_queue = FALSE;
	}
    }

    /* Even if we are not doing regular queue runs, we need to
     * wake up periodically to make sure that the config files
     * are still valid.
     * Hence my nasty hard wiring of queue_interval
     */
    if (queue_interval == 0) {
	/* Set to 10 minutes */
	queue_interval = 600;
    }

    /* SIGHUP means re-exec */
    got_sighup = FALSE;
    (void) signal(SIGHUP, daemon_sighup);

    /* set the alarm for wakeup time */
    got_sigalrm = FALSE;
#if defined(UNIX_BSD4_3) || defined(USE_SIGINTERRUPT)
    /*
     * We need to interrupt the accept, so ask for interrupted
     * system calls from SIGALRMs.
     */
    siginterrupt(SIGALRM, 1);
#endif
    (void) signal(SIGALRM, daemon_sigalrm);
    (void) alarm(queue_interval);

    /* loop processing connect requests or alarm signals */
    (void) listen(ls, 5);
    for (;;) {
	int len = sizeof(from);

	DEBUG1(DBG_MAIN_MID, "listening on port %d...\n",
	       ntohs(port));

#ifdef ISC_ACCEPT_BUG
	FD_ZERO(&fds_used);
	FD_SET(ls, &fds_used);

	memcpy(&fds_read, &fds_used, sizeof(&fds_read));
	nsel = select(ls + 1, &fds_read, (char *)0, (char *)0, (char *)0);
	if (nsel < 0) {
	    if (errno != EINTR) {
		write_log(LOG_PANIC, "select failed: %s", strerror(errno));
		continue;
	    }
	} else {
#endif	/* ISC_ACCEPT_BUG */

	/* get connection */
	as = accept(ls, (struct sockaddr *)&from, &len);
	if (as < 0 && errno != EINTR) {
	    write_log(LOG_PANIC, "accept failed: %s", strerror(errno));

	    /*
	     * for some reason, accept() fails badly (and repeatedly)
	     * on some systems.  To prevent the paniclog from filling
	     * up, exit if this happens too many times.
	     */

	    sleep(5);
	    accept_err_cnt++;
	    if (accept_err_cnt == 10) {
		    write_log(LOG_PANIC, "too many accept errors, quitting");
		    exit(EX_OSERR);
	    }
	    continue;
	}
	accept_err_cnt = 0;
	if (as >= 0) {
#ifdef SIGCHLD
	    if (smtp_accept_max > 0 && smtp_accept_count >= smtp_accept_max) {
		static char *reject[2] = {
		  "421 ", " Too many connections; try again later.\r\n"
		};

		DEBUG1(DBG_MAIN_MID, "rejecting SMTP connection #%d...\n",
		       smtp_accept_count + 1);
		(void) write(as, reject[0], strlen(reject[0]));
		(void) write(as, primary_name, strlen(primary_name));
		(void) write(as, reject[1], strlen(reject[1]));
		(void) close(as);
		continue;
	    }
#endif	/* SIGCHLD */

	    do_daemon_accept(ls, as, &from);
	}

#ifdef ISC_ACCEPT_BUG
	}
#endif

	if (got_sighup) {
	    write_log(LOG_SYS, "pid %d: SIGHUP received, exec(%s)",
		      getpid(), smail);
	    execv(smail, save_argv);
	    panic(EX_UNAVAILABLE, "pid %d: exec of %s failed", getpid());
	}
	if (got_sigalrm) {
	    /* if config file have changed, recycle */
	    DEBUG(DBG_MAIN_LO, "SIGALRM received, check input queue\n");
	    if (is_newconf()) {
		/* re-exec smail */
		write_log(LOG_SYS, "pid %d: new config files, exec(%s)",
			  getpid(), smail);
		execv(smail, save_argv);
		panic(EX_UNAVAILABLE, "pid %d: exec of %s failed", getpid());
		/*NOTREACHED*/
	    }
	    /* reopen the log files so that they can be moved and removed */
	    close_system_logs();
	    open_system_logs();

	    /* recache all of the driver info, to get any changes */
#ifdef SIGCHLD
	    (void) signal(SIGCHLD, SIG_DFL);
#endif
	    cache_directors();
	    cache_routers();
	    cache_transports();
#ifdef SIGCHLD
	    (void) signal(SIGCHLD, daemon_sigchld);
#endif

	    if (process_queue) {
		bg_run_queue(ls);       /* do queue run in child process */
	    }
	    got_sigalrm = FALSE;
	    (void) alarm(queue_interval);
	}
    }
}

/*
 * do_daemon_accept - perform processing for an accepted SMTP connection
 *
 * accept SMTP commands in a separate process.
 */
static void
do_daemon_accept(ls, fd, from)
    int ls;				/* listen socket, must be closed */
    int fd;				/* connected channel */
    struct sockaddr_in *from;		/* address of peer */
{
    int fd2;				/* dup of connected channel */
    int pid;

    DEBUG1(DBG_MAIN_LO, "connection request from [%s]\n",
	   inet_ntoa(from->sin_addr));
    fd2 = dup(fd);
    if (fd2 < 0) {
	FILE *f = fdopen(fd, "w");

	(void) fprintf(f, "421 %s Connection refused: %s\r\n",
		       primary_name, strerror(errno));
	(void) fflush(f);
	(void) close(fd);
	return;
    }
    pid = fork();
    if (pid == 0) {
	FILE *in;			/* input channel */
	FILE *out;			/* output channel */

	/* in child process */
	(void) close(ls);
	in = fdopen(fd, "r");	/* setup the channels */
	out = fdopen(fd2, "w");

	/* no longer ignore kids */
#ifdef SIGCHLD
	(void) signal(SIGCHLD, SIG_DFL);
#endif

	/*
	 * if the number of outstanding child processes exceeds
	 * smtp_accept_queue, then turn on queue_only, so that
	 * mail will not be delivered immediately.
	 */

	if (smtp_accept_queue > 0 && smtp_accept_count >= smtp_accept_queue) {
	    DEBUG1(DBG_MAIN_MID, "many connections, use queue_only #%d...\n",
		   smtp_accept_count + 1);
	    queue_only = TRUE;
	}

	/* do the actual work */
	do_smtp(in, out);

	/* done with that transaction */
	exit(0);
    }
    /* in parent process */
    if (pid < 0) {
	FILE *f = fdopen(fd, "w");

	(void) fprintf(f, "421 %s Connection refused: %s\r\n",
		       primary_name, strerror(errno));
	(void) fflush(f);
    }
    else {
#ifdef SIGCHLD
	int i;

	for (i = 0; i < smtp_accept_slots; ++i) {
	    if (smtp_pids[i] <= 0) {
		smtp_pids[i] = pid;
		++smtp_accept_count;
		break;
	    }
	}
#endif /* SIGCHLD */
    }
    (void) close(fd);
    (void) close(fd2);
}

#else	/* not defined(HAVE_BSD_NETWORKING) */

/*
 * For systems that don't have sockets, turn daemon mode into
 * a call to noop_mode().  This will have the desired affect if a
 * queue run was also requested.  Otherwise, it will simply return.
 */
void
daemon_mode()
{
    if (errfile) {
	(void) fprintf(stderr, "%s: daemon mode not supported\n", program);
	exitvalue = EX_UNAVAILABLE;
    }
    noop_mode();
}

#endif	/* not defined(HAVE_BSD_NETWORKING) */

/*
 * daemon_sighup - note that we received a SIGHUP signal
 */
/*ARGSUSED*/
static void
daemon_sighup(sig)
    int sig;
{
    got_sighup = TRUE;
    (void) signal(SIGHUP, daemon_sighup);
}

#ifdef SIGCHLD
/*
 * daemon_sigchld - reap dead kids
 */
/*ARGSUSED*/
static void
daemon_sigchld(sig)
    int sig;
{
    int pid;

#ifdef POSIX_OS
    while ((pid = waitpid(-1, (int *)0, WNOHANG)) > 0)
#else
#ifdef UNIX_BSD
    while ((pid = wait3((STATUS_TYPE *)0, WNOHANG, 0)) > 0)
#else
    if ((pid = wait((STATUS_TYPE *)0)) > 0)
#endif
#endif
    {
	reap_child(pid);
    }

#ifndef UNIX_BSD
    (void) signal(sig, daemon_sigchld);
#endif
}

static void
reap_child(pid)
int pid;
{
    int i;

    for (i = 0; i < smtp_accept_slots; ++i) {
	if (smtp_pids[i] == pid) {
	    smtp_pids[i] = 0;
	    if (--smtp_accept_count < 0)
		smtp_accept_count = 0;
	    break;
	}
    }
}
#endif	/* SIGCHLD */

/*
 * daemon_sigalrm - note that we received a SIGALRM signal
 */
/*ARGSUSED*/
static void
daemon_sigalrm(sig)
    int sig;
{
    got_sigalrm = TRUE;
    (void) signal(SIGALRM, daemon_sigalrm);
}


/*
 * noop_mode - perform queue runs once or at intervals
 *
 * When the -q flag is specified, or smail is invoked as runq, but -bd
 * is not specified, then noop_mode() is invoked, which does nothing but
 * execute run_queue() in background at intervals.  If no sleep interval
 * is specified, run_queue() is called only once.
 */
void
noop_mode()
{
    X_PANIC_OKAY();

    if (! process_queue) {
	/* queue procesing not requested, nothing to do */
	return;
    }

    /*
     * Turn smail process into a daemon, quit if we are in the parent
     * process.
     */
    if (start_daemon() != 0) {
	return;
    }

    /* arrange signals */
    got_sighup = FALSE;
    got_sigalrm = FALSE;
#ifdef SIGCHLD
    /* NOTE:  If there is no SIGCHLD we can't reap dead kids!  lose */
    (void) signal(SIGCHLD, daemon_sigchld);
#endif
    (void) signal(SIGHUP, daemon_sighup);
    (void) signal(SIGALRM, daemon_sigalrm);

    if (debug && queue_interval == 0) {
#ifdef SIGCHLD
	(void) signal(SIGCHLD, SIG_DFL);
#endif
	do_run_queue();
    } else {
	bg_run_queue(-1);
    }
    if (queue_interval > 0) {
	/* get an alarm at intervals */
	(void) alarm(queue_interval);
	for (;;) {
	    pause();
	    /* watch for SIGHUP to indicate a recycle */
	    if (got_sighup) {
		write_log(LOG_SYS, "pid %d: SIGHUP received, exec(%s)",
		       getpid(), smail);
		execv(smail, save_argv);
		panic(EX_UNAVAILABLE, "pid %d: exec of %s failed", getpid());
		/*NOTREACHED*/
	    }
	    if (! got_sigalrm) {
		continue;
	    }

	    /* reset the alarm condition */
	    got_sigalrm = FALSE;

	    /* if config file have changed, recycle */
	    if (is_newconf()) {
		write_log(LOG_SYS, "pid %d: new config files, exec(%s)",
		       getpid(), smail);
		execv(smail, save_argv);
		panic(EX_UNAVAILABLE, "pid %d: exec of %s failed", getpid());
		/*NOTREACHED*/
	    }
	    /* reopen the log files so that they can be moved and removed */
	    close_system_logs();
	    open_system_logs();

	    /* recache all of the driver info, to get any changes */
#ifdef SIGCHLD
	    (void) signal(SIGCHLD, SIG_DFL);
#endif
	    cache_directors();
	    cache_routers();
	    cache_transports();
#ifdef SIGCHLD
	    (void) signal(SIGCHLD, daemon_sigchld);
#endif

	    /* do another queue run */
	    bg_run_queue(-1);
	    (void) alarm(queue_interval);
	}
    }
}

/*
 * start_daemon - start a daemon smail process for noop_mode() or
 *		  daemon_mode()
 *
 * open system lots, get some system information we can use for
 * processing each message, and put ourselves in background.
 *
 * Return the pid of the child process in the parent, and 0 in the
 * child.
 */
static int
start_daemon()
{
    int pid;
#if defined(UNIX_BSD) && !defined(POSIX_OS)
    int fd;
#endif

    /* cache some interesting things */
    open_system_logs();
    if (primary_name == NULL) {
	build_host_strings();
    }
    compute_nobody();

    /* disconnect from the controlling terminal, if we are not debugging */
    if (debug == 0) {
	pid = fork();
	if (pid < 0) {
	    write_log(LOG_TTY, "fork() failed: %s", strerror(errno));
	    exitvalue = EX_OSERR;
	    return pid;
	}
	if (pid > 0) {
	    /* in parent process, just exit */
	    return pid;
	}
#ifdef POSIX_OS
	(void) setsid();
#else	/* not POSIX_OS */
#ifdef UNIX_BSD
	(void) setpgrp(0, getpid());
	fd = open("/dev/tty", O_RDWR);
	if (fd >= 0) {
	    ioctl(fd, TIOCNOTTY, 0);
	    close(fd);
	}
#else
	(void) setpgrp();
#endif	/* UNIX_BSD */
#endif	/* POSIX_OS */
	if (errfile) {
	    (void) fclose(errfile);
	    errfile = NULL;
	}
	if (isatty(fileno(stdout))) {
	    (void) fclose(stdout);
	}
	if (isatty(fileno(stdin))) {
	    (void) fclose(stdin);
	}
    }

    if (queue_interval > 0) {
	daemon_pid = getpid();
	write_log(LOG_SYS, "pid %d: smail daemon started", daemon_pid);
    }

    /* toss the real uid under which smail was executed */
    real_uid = getuid();

    return 0;
}

/*
 * bg_run_queue - perform a queue run in a child process
 */
static void
bg_run_queue(ls)
    int ls;		/* if >=0, close this descriptor in child */
{
    int pid = fork();

    if (pid == 0) {
	if (ls >= 0)
	    close(ls);
#ifdef POSIX_OS
	(void) setsid();
#else	/* not POSIX_OS */
	(void) setpgrp(0, getpid());
#endif	/* POSIX_OS */
#ifdef SIGCHLD
	/* in child process we care about dying kids */
	(void) signal(SIGCHLD, SIG_DFL);
#endif
	(void) alarm(0);
	do_run_queue();
	exit(0);
    }
}


/*
 * verify_addresses - print resolved addresses
 *
 * Get a list of addresses and return the output of resolve_addr_list() on
 * that list.
 */
void
verify_addresses()
{
    char *error;
    struct addr *cur;			/* temp recipient addr list element */
    struct addr *fail;			/* list of failed addrs */
    struct addr *defer;			/* list of deferred addrs */
    struct addr *deliver;		/* addr structures ready to deliver */
    struct addr **last_addr;		/* pointer to current addr pointer */
    struct addr *next;

    X_PANIC_OKAY();

    if (extract_addresses) {
	/*
	 * read in the message from stdin, if the -t flag was set.
	 */
	input_signals();		/* prepare to remove message */
	if (queue_message(stdin, dot_usage, recipients, &error) == FAIL) {
	    if (errfile) {
		(void) fprintf(errfile,
			       "%s: incoming message lost: %s: %s\n",
			       program,
			       error,
			       strerror(errno));
	    }
	    exitvalue = EX_OSFILE;
	    return;
	}
	if (read_message() == NULL) {
	    unlink_spool();
	    (void) fprintf(errfile, "failed to read queued message\n");
	    exitvalue = EX_OSFILE;
	    return;
	}
	/* don't actually need the message anymore */
	unlink_spool();
    }
    if (primary_name == NULL) {
	/* setup all of the hostname information */
	build_host_strings();
    }
    /* figure out who nobody is */
    compute_nobody();


    /*
     * preparse all of the recipient addresses given as arguments.
     * If we are extracting addresses from the header, then
     * these addresses are NOT to receive the mail.  To accomplish
     * this, add them to the hash table so they will be ignored
     * later.
     */
    for (cur = recipients, recipients = NULL; cur; cur = next) {
	next = cur->succ;
	split_addr_list(cur->in_addr, &recipients);
    }
    last_addr = &recipients;
    for (cur = recipients; cur; cur = next) {
	char *errptr;			/* error from preparse_address */

	next = cur->succ;
	if ((cur->work_addr = preparse_address(cur->in_addr, &errptr)) == NULL)
	{
	    if (errfile) {
		(void) fprintf(errfile,
			       "%s ... syntax error in address: %s\n",
			       cur->in_addr, errptr);
	    }
	    /* patch pointer to look at next address */
	    *last_addr = next;
	    xfree((char *)cur);
	    continue;
	}

	if (extract_addresses) {
	    (void) add_to_hash(cur->work_addr, (char *)NULL, 0, hit_table);
	    xfree(cur->work_addr);	/* don't need it anymore */
	    xfree((char *)cur);
	    continue;
	}

	last_addr = &cur->succ;
    }

    if (extract_addresses) {
	recipients = NULL;		/* don't need them anymore */

	/*
	 * process_header will get the recipients from the header,
	 * among other things we aren't really interested in here.
	 */
	error = process_header(&recipients);
	if (error && errfile) {
	    (void) fprintf(errfile, "error in header: %s\n", error);
	}
    }

    /*
     * given the list of recipient addresses, turn those
     * addresses into more specific destinations, including
     * the transport that is to be used, in the case of
     * addresses destined remote
     */
    deliver = NULL;
    defer = NULL;
    fail = NULL;
    resolve_addr_list(recipients, &deliver, &defer, &fail, TRUE);

    for (cur = deliver; cur; cur = cur->succ) {
	if (cur->next_host) {
	    printf("%s at %s ... deliverable\n",
		   cur->next_addr, cur->next_host);
	} else {
	    printf("%s ... deliverable\n", cur->next_addr);
	}
    }
    for (cur = defer; cur; cur = cur->succ) {
	printf("%s ... error: %s\n",
	       cur->in_addr, cur->error->message);
    }
    for (cur = fail; cur; cur = cur->succ) {
	printf("%s ... not deliverable: %s\n",
	       cur->in_addr, cur->error->message);
    }
    close_system_logs();
}


/*
 * do_run_queue - queue run assuming initial setup has been done
 */
static void
do_run_queue()
{
    char **work;			/* vector of jobs */

    DEBUG(DBG_MAIN_MID, "do_run_queue: called\n");

    /* get work, and do it with child processes */
    work = scan_spool_dirs();
    while (*work) {
	if (errfile) {
	    (void) fflush(errfile);
	}
	if (process_spool_file(*work) == FAIL) {
	    /* fork failed, don't continue */
	    return;
	}

	/* message processed, go on to the next message */
	work++;
	if (debug && errfile) {
	    (void) putc('\n', errfile);
	}
    }
    DEBUG(DBG_MAIN_HI, "do_run_queue: finished\n");
}


/*
 * print_version - display the current version string on stdout
 */
void
print_version()
{
    if (debug > 0) {
	puts(expand_string("\
release:	$version_string\n\
compilation:	#$compile_num on $compile_date",
			   (struct addr *) NULL, (char *) NULL, (char *) NULL));
    } else {
	puts(version());
    }
}

/*
 * print_copying_file - print the COPYING file, detailing distribution rights
 */
void
print_copying_file()
{
    register FILE *f;
    register int c;

    if (copying_file == NULL || (f = fopen(copying_file, "r")) == NULL) {
	(void) fprintf(stderr, "The file `%s' does not exist.\n\
Consult the file COPYING in the smail source directory for information\n\
on copying restrictions and warranty information from the authors\n",
		       copying_file? copying_file: "COPYING");
	exitvalue = EX_UNAVAILABLE;
	return;
    }

    while ((c = getc(f)) != EOF) {
	putchar(c);
    }
    (void) fclose(f);
}

/*
 * print_variables - write configuration variable values to stdout
 *
 * Names of variables are stored in the list of recipients.
 */
void
print_variables()
{
    register struct addr *cur;
    struct addr *new, *next;

    build_host_strings();
    /* first reverse the list */
    new = NULL;
    for (cur = recipients; cur; cur = next) {
	next = cur->succ;
	cur->succ = new;
	new = cur;
    }
    for (cur = new; cur; cur = cur->succ) {
	print_config_variable(cur->in_addr);
    }
}

/*
 * print_queue - list the current messages in the mail queue
 *
 * If debugging is enabled, print msglog associated with each message.
 */
void
print_queue()
{
    char **work;			/* vector of jobs to process */
    int col;				/* current print column */

    X_PANIC_OKAY();

    if (message_bufsiz > 4096) {
	message_bufsiz = 4096;		/* don't need a big buffer */
    }
    work = scan_spool_dirs();

    while (*work) {
	char **argv;			/* arguments from spool file */
	char *error;

	/* open without locking */
	if (open_spool(*work, FALSE, &error) == FAIL) {
	    if (errfile) {
		if (errno == 0) {
		    write_log(LOG_TTY, "%s/%s: %s",
			      spool_dir, input_spool_fn, error);
		} else {
		    write_log(LOG_TTY, "%s/%s: %s: %s",
			      spool_dir, input_spool_fn, error, strerror(errno));
		}
	    }
	    work++;
	    continue;
	}
	sender = NULL;
	argv = read_message();

	if (argv == NULL) {
	    work++;
	    continue;
	}

	(void) printf("%s\tFrom: %s  (in %s/input)\n",
		      message_id,
		      sender, spool_dir);

	(void) printf("\t\tDate: %s\n", get_arpa_date(message_date()));

	/*
	 * print the argument vectors several to a line, trying not to
	 * go past the 76'th column
	 */
	if (*argv) {
	    (void) printf("\t\tArgs: %s", *argv);
	    col = 8 + 8 + sizeof("Args: ")-1 + strlen(*argv++);
	}
	while (*argv) {
	    if (col + (int)strlen(*argv) > 74) {
		col = 8 + 8 + sizeof("Args: ") - 1;
		(void) fputs("\n\t\t      ", stdout);
	    } else {
		putchar(' ');
		col++;
	    }
	    col += strlen(*argv);
	    if (strlen(*argv) == 0 || index(*argv, ' ') != NULL) {
		col += 2;
		printf("'%s'", *argv++, stdout);
	    } else {
		fputs(*argv++, stdout);
	    }
	}
	putchar('\n');
	if (debug > 0) {
	    send_log(stdout, TRUE, "Log of transactions:\n");
	}
	close_spool();

	work++;				/* next assignment */
	if (*work) {
	    putchar('\n');
	}
    }
}


/*
 * smtp_mode - receive and processes smtp transpactions
 *
 * Call receive_smtp() to get incoming messages.  Then, if queue_only mode
 * is not set, deliver those messages.
 */
void
smtp_mode(in, out)
    FILE *in;				/* stream of SMTP commands */
    FILE *out;				/* channel for responses */
{
    open_system_logs();
    if (primary_name == NULL) {
	build_host_strings();
    }
    compute_nobody();

    /* do the real work */
    do_smtp(in, out);
}

/*
 * do_smtp - common routine used by smtp_mode() and daemon_mode() for SMTP
 *
 * NOTE: When receive_smtp is finished, in and out are closed.
 */
static void
do_smtp(in, out)
    FILE *in;
    FILE *out;
{
    char **files;			/* files returned by receive_smtp() */
    int cnt;				/* count of files */
    int entry_grade;
    int i;

    /* cache some interesting things */
    /* send out to process the SMTP transactions */
    if (out) {
	X_PANIC_OKAY();
    } else {
	X_NO_PANIC();
    }
    files = receive_smtp(in, out);
    X_PANIC_OKAY();

    (void) fclose(in);
    if (out) {
	(void) fclose(out);
    }

    /* if we are just queuing input, close and be done with it */
    if (queue_only || deliver_mode == QUEUE_MESSAGE) {
	close_spool();
	return;
    }

    for (cnt = 0; files[cnt] != NULL; cnt++) ;

    /* if delivering more than one mail message, cache driver info */
    if (cnt > 1) {
	if (! cached_directors) {
	    cache_directors();
	}
	if (! cached_routers) {
	    cache_routers();
	}
	if (! cached_transports) {
	    cache_transports();
	}
    }

    /*
     * process the files last first (if the last file is still open) and
     * then first to the second to last This ordering is used because the
     * last one remains open and it requires less overhead if the last
     * file does not have to be reopened.
     */
    for (cnt = 0; files[cnt] != NULL; cnt++) ; /* count the files */

    if (spool_fn) {
	/* last file still open, finish processing it */
	/* Check to see if this grade should be delivered immediately */
	entry_grade = spool_fn[strlen(spool_fn) - 1];
	if ((entry_grade >= min_delivery_grade) 
	    && (entry_grade <= max_delivery_grade)) {

	    char **argv;		/* args from read_message() */
	    int pid;			/* pid of child process */

	    /* make a child process */
	    /* unlock the message in the parent process (see lock_message()) */
	    unlock_message();
	    pid = fork_wait();
	    if (pid < 0) {
		/* can't fork(), try again later for all messages */
		if (errfile) {
		    (void)fprintf(errfile,
				  "%s: fork() failed: %s, try again later\n",
				  program, strerror(errno));
		    (void)fflush(errfile);
		}
		return;
	    }
	    if (pid == 0) {
		/* in child process, process the message */
		if (lock_message() == FAIL) {
		    /* somebody else grabbed the lock, assume they will deliver */
		    exit(0);
		}
		argv = read_message();

		if (argv == NULL) {
		    exit(exitvalue);
		}

		/* process arguments from the spool file */
		process_args(argv);

		/* perform delivery */
		deliver_mail();

		/* close the system-wide log files */
		close_system_logs();

		/* all done with the message */
		exit(exitvalue);
	    }
	}
	/*
	 * in the parent - or if queued instead
	 *
	 * XXX - we need to close the open spool file, but going through
	 *       routines in spool.c would duplicate efforts already
	 *	 done in the child process, so just close it ourselves.
	 */
	(void) close(spoolfile);

	--cnt;				/* decrement the count */
    }

    /*
     * process the remaining files
     */
    for (i = 0; i < cnt; i++) {
	/* Check to see if this grade should be delivered immediately */
	entry_grade = (files[i])[strlen(files[i]) - 1];
	if ((entry_grade >= min_delivery_grade) 
	    && (entry_grade <= max_delivery_grade)) {

	    /* process_spool_file only returns FAIL on fork() failures */
	    if (process_spool_file(files[i]) == FAIL) {
		return ;
	    }
	}
    }
}


/*
 * process_spool_file - open read and process a spool file in a child process
 *
 * fork a child to open read and process an input spool file.  Wait for
 * the child and return when the child has completed processing.
 *
 * Return FAIL if the fork() failed, otherwise return SUCCEED.
 */
int
process_spool_file(spfn)
    char *spfn;				/* spool file name */
{
/* jgt10 - add fstat of file before fork.  If the file isn't there, don't */
#if 0
    int pid = fork_wait(); 
    char *error;
#endif
    int pid;
    char *error;
    struct stat statbuf;

    /*
     * Get statistics on spool file.
     */
    (void) stat(spfn, &statbuf);

    /*
     * If the file has been removed, then don't spawn child to process
     */
    if (statbuf.st_nlink == 0) {
	write_log(LOG_SYS, "process_spool_file: %s: File already processed\n",
		  spfn);
	return FAIL;
    }

    /*
     * Fork to spawn a child process
     */
    if ((pid = fork_wait()) < 0) {
	/* can't fork(), try again later */
	if (errfile) {
	    (void)fprintf(errfile,
			  "%s: fork() failed: %s, try again later\n",
			  program, strerror(errno));
	    (void)fflush(errfile);
	}
	return FAIL;
    }
    if (pid == 0) {
	/* in child process */
	char **argv;		/* arguments from spool file */

	/* message grade is encoded in the last char of the filename */
	msg_grade = spfn[strlen(spfn) - 1];

	/* initialize state before reading state from the spool file */
	initialize_state();

	/* in child process, open the message and attempt delivery */
	if (open_spool(spfn, TRUE, &error) == FAIL) {
	    if (errno == 0) {
		write_log(LOG_SYS, "open_spool: %s/%s: %s",
			  spool_dir, input_spool_fn, error);
	    } else {
		write_log(LOG_SYS, "open_spool: %s/%s: %s: %s",
			  spool_dir, input_spool_fn, error, strerror(errno));
	    }
	    exit(EX_OSFILE);
	}
	argv = read_message();

	if (argv == NULL) {
	    panic(EX_OSFILE, "failed to read queued message in %s/%s",
		  spool_dir, input_spool_fn);
	    /*NOTREACHED*/
	}

	/* process arguments from the spool file */
	process_args(argv);

	/* perform delivery */
	deliver_mail();

	/* close the sytem-wide log files */
	close_system_logs();

	/* all done with the message */
	exit(exitvalue);
    }

    return SUCCEED;
}

/*
 * fork_wait - fork and have the parent wait for the child to complete
 *
 * Return with 0 in the child process.
 * Return with -1 if fork() fails.
 * Return with the pid in the parent, though the wait() will already
 *  have been done.
 */
int
fork_wait()
{
    int pid = fork();
    int i;

    if (pid == 0) {
	return 0;
    }
    if (pid < 0) {
	return -1;
    }
    while ((i = wait((STATUS_TYPE *)0)) >= 0 && i != pid) ;

    return pid;
}

/*
 * error_resolve_timeout
 *
 * Things have hung up in the directors/routers for too long
 * so we are converting all defers to fails, and modifying the
 * error message along the way.
 *
 * For a long list of addresses this will simply chew memory
 * since all the error messages will be duplicated!
 */
static void
error_resolve_timeout(defer, fail)
     struct addr * * defer;
     struct addr * * fail;
{
    struct str wkstr;
    struct addr * failed_addr;
    int base_len;

    DEBUG(DBG_MAIN_LO, "error_resolve_timeout: converting timeout defers to fails\n");
    STR_INIT(&wkstr);
    STR_CAT(&wkstr, "Unable to resolve after timeout(");
    STR_CAT(&wkstr, ltoival(resolve_timeout));
    STR_CAT(&wkstr, ") - ");
    base_len = wkstr.i;
    while(*defer) {
	failed_addr = *defer;
	*defer = (*defer)->succ;
	if (failed_addr->error) {
	    failed_addr->error->info = ERR_184 | ERR_NPOSTMAST | ERR_NSOWNER;
	    STR_CAT(&wkstr, failed_addr->error->message);
	    failed_addr->error->message = COPY_STRING(wkstr.p);
	} else {
	    STR_NEXT(&wkstr, '\0'); /* terminate string */
	    failed_addr->error = note_error(ERR_184 | ERR_NPOSTMAST | ERR_NSOWNER,
					    COPY_STRING(wkstr.p));
	}
	failed_addr->succ = *fail;
	*fail = failed_addr;
	wkstr.i = base_len;	/* reset base string */
    }

}

