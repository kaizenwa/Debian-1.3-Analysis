/*
#ident	"@(#)smail/src:RELEASE-3_2:main.c,v 1.34 1996/06/14 18:55:22 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * main.c:
 *	process arguments, configure environment and process
 *	messages.
 *
 *	external functions: main, initialize_state, process_args
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include "defs.h"
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#if defined(POSIX_OS) || !defined(UNIX_BSD)
# include <time.h>
#else
# include <sys/time.h>
#endif	/* UNIX_BSD */
#if defined(HAVE_RLIMIT)
# include <sys/resource.h>
#endif	/* HAVE_RLIMIT */
#ifdef	UNIX_AIX3
# include <sys/id.h>
#endif	/* UNIX_AIX3 */
#include "config.h"
#include "smail.h"
#include "dys.h"
#include "addr.h"
#include "hash.h"
#include "main.h"
#include "log.h"
#include "transport.h"
#include "child.h"
#include "exitcodes.h"
#include "smailconf.h"
#include "alloc.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

/* exported variables */
int islocal;				/* TRUE if mail originated locally */
int exitvalue;				/* call exit with this value */
char *program;				/* argv[0] from main */
char *sender;				/* sender of message */
char *local_sender;			/* local sender of message */
int error_sender;			/* TRUE if special sender <> given */
char *sender_name;			/* full name of sender */
enum op_mode operation_mode;		/* mode of operation */
int debug = 0;				/* debugging level, 0 is off */
int dont_deliver = FALSE;		/* if TRUE, don't actually deliver */
int process_queue;			/* process spooled files */
unsigned queue_interval;		/* process queues at this interval */
int hop_count;				/* hop count so far for message */
int do_aliasing;			/* do aliasing for local addresses */
int extract_addresses;			/* get recipients from header */
int me_too;				/* sender allowed in aliases */
enum er_proc error_processing;		/* method of displaying errors */
enum dot_usage dot_usage;		/* how do we treat . on input */
enum deliver_mode
    deliver_mode = DELIVER_DEFAULT;	/* foreground, background or queued */
struct addr *recipients;		/* list of recipient addresses */
char *primary_name;			/* primary local name from hostnames */
FILE *errfile = NULL;			/* file to write debug messages to */
int real_uid;				/* saved real uid before ruid setup */
enum prog_type prog_type;		/* type of program we are running as */
char **save_argv;			/* saved pointer to arguments */
int some_deferred_addrs;		/* don't unlink spool file */
					/* as some addrs were deferred */
int prog_euid;				/* effective uid of program */
int prog_egid;				/* effective gid of program */
int force_zero_exitvalue = FALSE;	/* if TRUE always exit with status 0 */
int call_defer_message;			/* if TRUE must call defer_message() */
int sender_is_trusted;			/* TRUE if sender is a trusted user */
char *sender_host = NULL;		/* name of sender's host */
char *sender_host_addr = NULL;		/* inet address of sender's host */
char *sender_proto = NULL;		/* name of sender's sending protocol */
char *sender_program = NULL;		/* name of program that spooled msg */
char *smtp_service_name = NULL;		/* smtp service name from -oX */

/* functions local to this file */
static void panic_if_null();
static void rmail_panic();
static char *escape_newline();
static void parse_grade_range();

/* variables local to this file */
static int report_memory_usage = FALSE;	/* if TRUE, report sbrk(0) when done */
static char *arg_second_config_file = NULL; /* second config set by args */
static char *arg_director_file = NULL;	/* director file set by args */
static char *arg_router_file = NULL;	/* router file set by args */
static char *arg_transport_file = NULL;	/* transport file set by args */
static char *arg_qualify_file = NULL;   /* domain qualification file set by args */
static char *arg_retry_file = NULL;     /* address retry file set by args */
static char *arg_smail_lib_dir = NULL;	/* smail lib dir set by args */
static char *arg_alias_file = NULL;	/* alias file set by -oA */
static char *arg_debug_file = NULL;	/* debug file, defaulting to stderr */
static char *arg_runq_grades = NULL;	/* which grades are processed by runq */

/* variables imported from libc */
extern int errno;

/* functions imported from libc */
char *getenv();


/*
 * main - what to do after being exec'd
 *
 * main decodes the argument list and then performs specified action
 */
/*ARGSUSED*/
void
main(argc, argv)
    int argc;				/* count of arguments passed */
    char **argv;			/* vector of arguments */
{
    char *save_config_file = config_file;
    struct stat statbuf;
    char *error;
    char *utilargs[10];
    int child;
    FILE *new_errfile;

    save_argv = argv;

    /* set up the file for interactive error and debug messages */
    if (!errfile) {
	/* is stderr a valid file descriptor? */
	if (fstat(2, &statbuf) >= 0) {
	    /* yes, use stderr */
	    errfile = stderr;
	} else {
	    /* no, can't output to stderr */
	    errfile = NULL;
	}
    }
    /* close file descriptors left open by others */
    close_all();

    /*
     * get the basename for the program
     */
    program = rindex(*argv, '/');
    if (program == NULL) {
	program = *argv;
    } else {
	program++;
    }
    argv++;
    sender_program = program;

#ifdef	UNIX_SCO
    /* if we don't have a login id, assume one */
    if (getluid() == -1)
	setluid(0);
#endif	/* UNIX_SCO */

#ifdef	UNIX_AIX3
    /* if we don't have a login id, assume one */
    if (getuidx(ID_LOGIN) == -1)
	setuidx(ID_LOGIN, 0);
#endif	/* UNIX_AIX3 */

#ifdef	HAVE_SETGROUPS
    /* clear out all extra groups.  We don't want to have to deal with them */
    {
	gid_t dummy[1];
	(void) setgroups(0, dummy);
    }
#endif	/* HAVE_SETGROUPS */

    /* get rid of any limits that might affect operation */
#if defined(HAVE_ULIMIT) && !defined(HAVE_RLIMIT)
    /* kill limits on file size */
    (void) ulimit(2, ((long)1 << (BITS_PER_LONG - 2))/512);
#endif	/* HAVE_ULIMIT && !defined(HAVE_RLIMIT) */

    /* always get a write error for a SIGPIPE, rather than dying */
    (void) signal(SIGPIPE, SIG_IGN);

#if defined(HAVE_RLIMIT)
    /* kill limits on file size, cpu, data segment, and unreasonable
     * limits on stack size */
    {
	struct rlimit rl;

	rl.rlim_cur = RLIM_INFINITY;
	rl.rlim_max = RLIM_INFINITY;
	(void) setrlimit(RLIMIT_CPU, &rl);
	(void) setrlimit(RLIMIT_FSIZE, &rl);
#if defined(DATA_RLIMIT)
	rl.rlim_cur = DATA_RLIMIT;
#endif	/* DATA_RLIMIT */
	(void) setrlimit(RLIMIT_DATA, &rl);
#if defined(STACK_RLIMIT)
	rl.rlim_cur = STACK_RLIMIT;
#endif	/* STACK_RLIMIT */
	(void) setrlimit(RLIMIT_STACK, &rl);
    }
#endif	/* HAVE_RLIMIT */

#if	defined(UNIX_AIX) && !defined(NO_AIX_CORE_DUMP)
    /* On a segmentation fault or bus error, we need a full core dump.  */
    {
	struct sigaction act;

	act.sa_handler = SIG_DFL;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_FULLDUMP;
	sigaction(SIGSEGV, &act, (struct sigaction *)NULL);
	sigaction(SIGBUS, &act, (struct sigaction *)NULL);
    }
#endif	/* UNIX_AIX && !NO_AIX_CORE_DUMP */

    /* Xenix systems must have TZ in the environment */
#ifdef	REQUIRE_TZ
    /* if no timezone specified, assume GMT */
    if (getenv("TZ") == NULL) {
	(void) putenv("TZ=GMT0");
    }
#endif

    /* we will always be supplying exactly the mode we want */
    (void) umask(0);

    /* set the program type based on the program's basename */
    prog_type = PROG_SMAIL;
    if (EQ(program, "rmail")) {
	prog_type = PROG_RMAIL;
    } else if (EQ(program, "pathto")) {
	prog_type = PROG_PATHTO;
    } else if (EQ(program, "optto")) {
	prog_type = PROG_OPTTO;
    } else if (EQ(program, "uupath")) {
	prog_type = PROG_UUPATH;
    } else if (EQ(program, "newaliases")) {
	prog_type = PROG_NEWALIASES;
    } else if (EQ(program, "smailconf")) {
	prog_type = PROG_SMAILCONF;
    } else if (EQ(program, "mailq")) {
	prog_type = PROG_MAILQ;
    } else if (EQ(program, "runq")) {
	prog_type = PROG_RUNQUEUE;
    } else if ((EQ(program, "smtpd")) ||
	       (EQ(program, "in.smtpd"))) {
	/*
	 * if there is no file on stdout, then dup stdin to stdout.
	 * This is done because processes started from inetd will have
	 * fd 0 set to the socket, but fd 1 will not be set.  It will
	 * need to be dup'd for this to work.
	 */
	if (fstat(1, &statbuf) < 0) {
	    dup2(0, 1);
	}
	prog_type = PROG_SMTPD;
    } else if (EQ(program, "rsmtp")) {
	prog_type = PROG_RSMTP;
    } else if (EQ(program, "rogue") || EQ(program, "hack")) {
	prog_type = PROG_ROGUE;
    } else if (EQ(program, "..execmail") || EQ(program, "execmail")) {
	prog_type = PROG_EXECMAIL;
    }

    /* initialize per-message state information */
    initialize_state();

    /* set state information which depends on program type */
    if (prog_type == PROG_NEWALIASES) {
	operation_mode = REBUILD_ALIASES;
    } else if (prog_type == PROG_SMAILCONF) {
	operation_mode = FREEZE_CONFIG;
    } else if (prog_type == PROG_MAILQ) {
	operation_mode = PRINT_QUEUE;
    } else if (prog_type == PROG_RSMTP) {
	operation_mode = BATCH_SMTP_MODE;
    } else if (prog_type == PROG_SMTPD) {
	operation_mode = SMTP_MODE;
    } else if (prog_type == PROG_PATHTO) {
	pathto(argv);
    } else if (prog_type == PROG_OPTTO) {
	optto(argv);
    } else if (prog_type == PROG_UUPATH) {
	uupath(argv);
    } else if (prog_type == PROG_RMAIL) {
	dot_usage = NO_DOT_PROTOCOL;
    }

    if (getenv("SMAIL_CONFIG")) {
	config_file = getenv("SMAIL_CONFIG");
    }

    /* process the args given by the user */
    process_args(argv);

    if (prog_type == PROG_RUNQUEUE) {
	if (operation_mode == MODE_DEFAULT ||
	    operation_mode == DAEMON_MODE)
	{
	    process_queue = TRUE;
	}
    }
    if (operation_mode == MODE_DEFAULT) {
	/*
	 * when performing a queue run, no other operations are
	 * performed, by default.  Currently no other operations
	 * are allowed, either, though this may change in the
	 * future.
	 */
	if (prog_type == PROG_RUNQUEUE || process_queue) {
	    operation_mode = NOOP_MODE;
	} else {
	    operation_mode = DELIVER_MAIL;
	}
    }

    if (prog_type == PROG_ROGUE) {
	operation_mode = ROGUE_MODE;
    }

    if (config_file != save_config_file || arg_second_config_file ||
	arg_director_file || arg_router_file || arg_transport_file ||
	arg_qualify_file || arg_retry_file || arg_smail_lib_dir ||
	arg_alias_file || operation_mode == REBUILD_ALIASES)
    {
	/*
	 * a config_file was set, or unset from the command args
	 * then watch out for set-uid execs;  i.e., go back to
	 * the real uid under which we were invoked.
	 */
	setgid(getgid());
	setuid(getuid());
    }

    /* read in the config files, if they exists */
    if (arg_smail_lib_dir) {
	smail_lib_dir = arg_smail_lib_dir;
    }
    error = read_config_file((config_file = make_lib_fn(config_file)));
    /* we need to set this again, in case it was changed in the config files */
    if (arg_smail_lib_dir) {
	smail_lib_dir = arg_smail_lib_dir;
    }
    if (arg_second_config_file) {
	second_config_file = arg_second_config_file;
    }
    second_config_file = make_lib_fn(second_config_file);
    if (error == NULL && second_config_file) {
	error = read_config_file(second_config_file);
    }
    /* we need to set this again, in case it was changed in the config files */
    if (arg_smail_lib_dir) {
	smail_lib_dir = arg_smail_lib_dir;
    }
    if (arg_smail_lib_dir) {
	smail_lib_dir = arg_smail_lib_dir;
    }
    if (arg_director_file) {
	director_file = arg_director_file;
    }
    if (arg_router_file) {
	router_file = arg_router_file;
    }
    if (arg_transport_file) {
	transport_file = arg_transport_file;
    }
    if (arg_qualify_file) {
	qualify_file = arg_qualify_file;
    }
    if (arg_retry_file) {
	retry_file = arg_retry_file;
    }
    if (arg_runq_grades) {
        runq_grades = arg_runq_grades;
    }
    parse_grade_range(runq_grades, &min_runq_grade, &max_runq_grade);
    parse_grade_range(delivery_grades, &min_delivery_grade, &max_delivery_grade);
    /* get the config file names within the lib directory */
    director_file = make_lib_fn(director_file);
    router_file = make_lib_fn(router_file);
    transport_file = make_lib_fn(transport_file);
    method_dir = make_lib_fn(method_dir);
    qualify_file = make_lib_fn(qualify_file);
    retry_file = make_lib_fn(retry_file);
    copying_file = make_lib_fn(copying_file);
    smail = make_lib_fn(smail);

    if (error) {
	/*
	 * error in the config file: not a good thing.
	 *
	 * Revert back to the initial values of vital attributes,
	 * and set queue_only to avoid trying to perform delivery
	 * with a potentially bad configuration.
	 */
	max_message_size = MAX_MESSAGE_SIZE;
	log_fn = LOGFILE;
	panic_fn = PANIC_LOG;
	cons_fn = CONSOLE;
	spool_dirs = SPOOL_DIRS;
	spool_mode = SPOOL_MODE;
	lock_mode = LOCK_MODE;
	log_mode = LOG_MODE;
	message_log_mode = MESSAGE_LOG_MODE;
	message_bufsiz = MESSAGE_BUF_SIZE;
	queue_only = TRUE;

	/*
	 * if we are not actually going to be reading in messages,
	 * then panic.  Also, allow some trivial operations.
	 */
	switch (operation_mode) {
	case PRINT_QUEUE:
	case PRINT_VERSION:
	case SMTP_MODE:
	case BATCH_SMTP_MODE:
	case DELIVER_MAIL:
	case PRINT_VARS_MODE:
	case REBUILD_ALIASES:
	    write_log(LOG_TTY|LOG_PANIC, "%s", error);
	    break;

	default:
	    panic(EX_OSFILE, "%s", error);
	    /*NOTREACHED*/
	}
    }

    /*
     * read in the transport, router and director files, if needed
     *
     * NOTE: if queue_only is FALSE and mode is DELIVER_MAIL,
     *	     we will need to read these files, though do this later
     *	     to avoid wasting time on it before the spool file is
     *	     created.
     */
    switch (operation_mode) {
    case NOOP_MODE:
    case DAEMON_MODE:
	/*
	 * stat our binary so we can see if it has been touched later
	 */
	if (stat(smail, &statbuf) < 0) {
	    panic(EX_SOFTWARE, "main: bad stat of smail binary %s", smail);
	} else {
	    add_config_stat(smail, &statbuf);
	}
	/* FALLTHRU */

    case PRINT_VARS_MODE:			/* This is to allow dumping of configs */
    case TEST_MODE:
    case VERIFY_ADDRS:
    case BATCH_SMTP_MODE:
    case SMTP_MODE:
	if ((error = read_transport_file()) ||
	    (error = read_router_file()) ||
	    (error = read_director_file()) ||
	    (error = read_qualify_file()) ||
	    (error = read_retry_file()))
	{
	    panic(EX_OSFILE, "%s", error);
	}
	break;
    }

    switch (operation_mode) {
    case NOOP_MODE:
    case DAEMON_MODE:
    case TEST_MODE:
	cache_directors();
	cache_routers();
	cache_transports();
	break;
    }

    /*
     * Save away the real uid and set the real to the effective.
     * After this point, the real uid is no longer at all interesting.
     * In BSD, if the mailer runs as root, we can now freely set the
     * real or effective uid to whatever we want without worrying about
     * swapping them.  Also, if the mailer runs as a user other than
     * root, we no longer have to worry about child processes being
     * able to do a setuid(getuid) to get root priveledges when root
     * sends mail.
     */
    real_uid = (int) getuid();
    prog_euid = (int) geteuid();		/* keep a copy of the effictive id's */
    prog_egid = (int) getegid();
    setuid(prog_euid);
    setgid(prog_egid);

    /*
     * If the current effective UID does not match the required ID,
     * then mail can be queued (if that succeeds), but mail will
     * not be delivered.  This only applies when receiving mail,
     * and is ignored when running through the queue from a
     * queue run daemon.
     */
#ifdef REQUIRED_EUID
    if (prog_euid != REQUIRED_EUID)
	    queue_only = TRUE;
#endif

    /*
     * change error file to debugging file from -D option, if any
     *
     * JMJ: Change location of this fragment to below the setuid/setgid
     *      calls to allow for use of fopen_as_user() instead of just
     *      fopen().
     *
     *      Side effect: -D now requires full pathname to debug file
     */

    if (arg_debug_file) {
	if (!(new_errfile = fopen_as_user(arg_debug_file, "a", real_uid, (int) getgid(), 0600))) {
	    write_log(LOG_TTY, "Warning: Cannot open debug file %s: %s\n",
		      arg_debug_file, strerror(errno));
	    arg_debug_file = NULL;
	} else {
	    errfile = new_errfile;
	    fprintf(errfile, "\n%s: Debugging started: pid=%ld\n\n",
		    program, (long)getpid());
	}
    }

    /*
     * error processing can be other than TERMINAL only for
     * mail delivery modes
     */
    switch (operation_mode)
    {
    case DELIVER_MAIL:
    case NOOP_MODE:
    case DAEMON_MODE:
    case SMTP_MODE:
    case BATCH_SMTP_MODE:
	if (error_processing == ERROR_DEFAULT) {
	    error_processing = MAIL_BACK;
	}
	break;

    default:
	error_processing = TERMINAL;
	break;
    }

    if (process_queue &&
	operation_mode != NOOP_MODE &&
	operation_mode != DAEMON_MODE)
    {
	if (errfile) {
	    fprintf(errfile,
		    "%s: operation mode not compatible with queue runs\n",
		    program);
	}
	exit(EX_USAGE);
    }

    if (process_queue && recipients && queue_interval) {
	if (errfile) {
	    fprintf(errfile,
		    "%s: cannot have queue run interval and list of messages to process.\n",
		    program);
	}
	exit(EX_USAGE);
    }

    /*
     * setup the delivery mode used for delivering new messages
     */
    if (deliver_mode == DELIVER_DEFAULT) {
	/*
	 * if not set explicity in the arguments, key off the first
	 * letter of the configuration parameter
	 */
	switch (delivery_mode_string[0]) {
	case 'f':
	    deliver_mode = FOREGROUND;
	    break;
	case 'b':
	    deliver_mode = BACKGROUND;
	    break;
	default:
	    deliver_mode = QUEUE_MESSAGE;
	    break;
	}
    }

    /*
     * if debugging to standard error, then don't do background delivery.
     * Otherwise, we might continue writing to standard error after the
     * main process has exited.
     */
    if (debug && arg_debug_file == NULL && deliver_mode == BACKGROUND)
	deliver_mode = FOREGROUND;

    /*
     * invoke the correct mode of operation
     */
    switch (operation_mode) {
    case TEST_MODE:			/* test addresses from stdin */
	test_addresses();		/* read addrs from stdin, for tests */
	break;

    case NOOP_MODE:			/* generally, this means run queue */
	dont_deliver = FALSE;		/* it is too dangerous to allow this */
	noop_mode();
	break;

    case PRINT_QUEUE:			/* print the mail queue */
	print_queue();
	break;

    case PRINT_VERSION:
	print_version();
	break;

    case VERIFY_ADDRS:			/* spit out resoved addresses */
	if (recipients == NULL && !extract_addresses) {
	    if (errfile) {
		(void) fprintf(errfile, "Usage: %s [flags] address...\n",
			       program);
	    }
	    exitvalue = EX_USAGE;
	    break;
	}
	verify_addresses();
	break;

    case SMTP_MODE:			/* read SMTP requests on stdin */
	smtp_mode(stdin, stdout);
	break;

    case BATCH_SMTP_MODE:		/* batched SMTP requests on stdin */
	smtp_mode(stdin, (FILE *)NULL);
	break;

    case DAEMON_MODE:			/* be a daemon waiting for requests */
	dont_deliver = FALSE;		/* it is too dangerous to allow this */
	daemon_mode();
	break;

    case FREEZE_CONFIG:			/* freeze the configuration */
	if (errfile) {
	    (void) fprintf(errfile,
			   "%s: operation not currently supported\n",
			   program);
	}
	exitvalue = EX_UNAVAILABLE;
	break;

    case DELIVER_MAIL:			/* deliver to all addresses found */
	if (recipients == NULL && !extract_addresses) {
	    if (errfile) {
		(void) fprintf(errfile, "Usage: %s [flags] address...\n",
			       program);
	    }
	    exitvalue = EX_USAGE;
	    break;
	}

	perform_deliver_mail();
	break;

    case ROGUE_MODE:			/* print a rogue tombstone */
	silly();
	break;

    case COPYING_MODE:
	print_copying_file();
	break;

    case PRINT_VARS_MODE:
	print_variables();
	break;

    case REBUILD_ALIASES:

	if (smail_util_dir == NULL) {
	    if (errfile) {
		fprintf(errfile, "%s: smail_util_dir attribute not set, -bi not supported\n", program);
	    }
	    exit(EX_UNAVAILABLE);
	}
#ifdef SHELL_EXEC_PATH
	utilargs[0] = SHELL_EXEC_PATH;
#else
	utilargs[0] = "/bin/sh";
#endif
	utilargs[1] = xprintf("%s/mkaliases", smail_util_dir);
	utilargs[2] = NULL;
	if (arg_alias_file) {
	    utilargs[3] = arg_alias_file;
	    utilargs[4] = NULL;
	}
	child = open_child(utilargs, (char **) NULL, (FILE **) NULL, (FILE **) NULL,
			   -1, CHILD_MINENV, (int) getuid(), (int) getgid());
	if (child == -1) {
	    if (errfile) {
		fprintf(errfile, "%s: Cannot start %s: %s\n",
			utilargs[0], strerror(errno));
	    }
	    exit(EX_UNAVAILABLE);
	}
	close_child((FILE *)NULL, (FILE *)NULL, child);
	exit(0);
	/*NOTREACHED*/

    default:
	if (errfile) {
	    (void) fprintf(errfile, "%s: option not supported\n", program);
	}
	exitvalue = EX_UNAVAILABLE;
    }

    /*
     * all done.
     */
    if (report_memory_usage) {
	if (errfile) {
	    (void) fprintf(errfile, "%s: sbrk(0) = %ld\n",
			   program, (long)sbrk(0));
	}
    }
    if (force_zero_exitvalue) {
	exit(0);
    }
    exit(exitvalue);
    /* NOTREACHED */
}


/*
 * initialize_state - set some parameters to their default value
 */
void
initialize_state()
{
    send_to_postmaster = FALSE;
    return_to_sender = FALSE;
    islocal = FALSE;
    exitvalue = EX_OK;
    sender = NULL;
    error_sender = FALSE;
    path_to_sender = NULL;
    sender_name = NULL;
    operation_mode = MODE_DEFAULT;
    process_queue = FALSE;
    queue_interval = 0;
    hop_count = -1;
    do_aliasing = TRUE;
    extract_addresses = FALSE;
    dot_usage = DOT_ENDS_MESSAGE;
    me_too = FALSE;
    error_processing = ERROR_DEFAULT;
    recipients = NULL;
    some_deferred_addrs = FALSE;
    call_defer_message = FALSE;
    sender_is_trusted = TRUE;
    dont_deliver = FALSE;

    /*
     * generate a new address hit table where case is ignored and
     * all data resides in memory
     */
    /* XXX - hit_table should be associated with a block */
    hit_table = new_hash_table(hit_table_len,
			       (struct block *)NULL,
			       HASH_DEFAULT);
}


/*
 * process_args - process the arguments passed to the mailer
 *
 * In general use sendmail semantics, with different argument
 * processing based on name at invocation.
 */
void
process_args(args)
    register char **args;		/* vector of arguments */
{
    struct addr *cur;			/* temp addr list entry */
    char *arg;				/* single string from args */
    char *error;			/* error message */
    char *newsender;			/* temp for process sender */
    static char *end_arg = "";		/* swallow remaining chars in arg */

    /*
     * go through the list of arguments in search of options and
     * addresses.
     */
    while ((arg = *args++)) {

	/* option arguments begin with '-', of course */
	if (arg[0] == '-') {

	    /* switch on each letter */
	    arg++;
	    while (*arg) switch(*arg++) {

	    case 'd':			/* set debug level */
	    case 'v':			/* verbose, same as debug */
		if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
		    rmail_panic();
		}
		if (arg[0]) {
		    char *errptr = NULL;

		    debug = (int)c_atol(arg, &errptr);
		    if (errptr || debug < 0) {
			if (errfile) {
			    (void) fprintf(errfile,
			"%s: -%c flag takes an optional non-negative number\n",
					   program, arg[-1]);
			}
			exit(EX_USAGE);
		    }
		    arg = end_arg;
		} else {
		    debug = 1;		/* if no number, default to 1 */
		}
		break;

	    case 'D':
		arg_debug_file = arg;
		arg = end_arg;
		if (arg_debug_file[0] == '\0') {
		    arg_debug_file = *args++;
		    panic_if_null(arg_debug_file, "D");
		}
		if (debug == 0)
		    debug = 1;
		break;

	    case 'V':
		operation_mode = PRINT_VERSION;
		break;

	    case 'F':			/* set full name of sender */
		sender_name = arg;
		arg = end_arg;		/* terminate args in current argv */
		/* if no string there, take next arg */
		if (sender_name[0] == '\0') {
		    sender_name = *args++;
		    panic_if_null(sender_name, "F");
		}
		break;

	    case 'r':			/* set path to sender */
		/* SCO Execmail '-r' option not applicable to smail */
		if (prog_type == PROG_EXECMAIL)
		    break;
		/* FALLTHRU */

	    case 'f':			/* set path to sender */
		newsender = arg;
		arg = end_arg;		/* terminate args in current argv */
		/* if no string there, take next arg */
		if (newsender[0] == '\0') {
		    newsender = *args++;
		    panic_if_null(newsender, "f");
		}

		/*
		 * don't use this sender if it has been determined that
		 * the local originator of mail is not a user expected
		 * to receive remote mail.
		 */
		if (sender_is_trusted) {
		    sender = newsender;
		    /* special form of sender for SMTP error mail */
		    if (EQ(sender, "<>")) {
			sender = "MAILER-DAEMON";
			error_sender = TRUE;
			islocal = FALSE;
		    } else if (EQ(sender, "<+>")) {
			sender = "MAILER-DAEMON";
			error_sender = TRUE;
			islocal = TRUE;
		    } else {
			newsender = preparse_address(sender, &error);
			if (newsender == NULL) {
			    if (errfile) {
				(void)fprintf(errfile,
					      "%s: error in sender name: %s",
					      program, error);
			    }
			    sender = "MAILER-DAEMON";
			    islocal = FALSE;
			}
			islocal =
			    (parse_address(sender, (char **)0, &error, (int *)0)
				   == LOCAL);
		    }
		}
		break;

	    case 'N':			/* don't deliver message */
		dont_deliver = TRUE;
		break;

	    case 'C':			/* set config file name */
		if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
		    rmail_panic();
		}
		config_file = arg;
		arg = end_arg;		/* terminate args in current argv */
		/* if no string there, take next arg */
		if (config_file[0] == '\0') {
		    config_file = *args++;
		    panic_if_null(config_file, "C");
		}
		break;

	    case 'q':			/* check queue (at interval) */
		if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
		    rmail_panic();
		}
		process_queue = TRUE;
		if (arg[0]) {
		    long l = ivaltol(arg);

		    if (l < 0) {
			if (errfile) {
			    (void) fprintf(errfile,
					   "%s: %s: malformed interval\n",
					   program, arg);
			    exit(EX_USAGE);
			}
		    }
		    queue_interval = (unsigned)l;
		    if (l != queue_interval) {
			(void) fprintf(errfile, "%s: %s: interval too large\n",
				       program, arg);
			exit(EX_USAGE);
		    }
		    arg = end_arg;	/* uses rest of argument */
		}
		break;

	    case 'h':			/* hopcount, number is the count */
	        {
		    char *errptr = NULL;

		    if (arg[0]) {
			hop_count = (int)c_atol(arg, &errptr);
			arg = end_arg;
		    } else {
			if (*args)
			    hop_count = atoi(*args++);
			else
			    hop_count = -1;
		    }
		    if (errptr || hop_count < 0) {
			if (errfile) {
			    (void) fprintf(errfile,
				   "%s: -h flag takes a non-negative number\n",
					   program);
			}
			exit(EX_USAGE);
		    }
		}
		break;

	    case 'n':			/* don't do aliasing */
		if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
		    rmail_panic();
		}
		do_aliasing = FALSE;
		break;

	    case 't':			/* read recipients from message */
		extract_addresses = TRUE;
		break;

	    case 'i':			/* don't treat dots specially */
		dot_usage = NO_DOT_PROTOCOL;
		break;

	    case 'I':			/* use hidden-dot protocol on input */
		dot_usage = HIDDEN_DOTS;
		break;

	    case 'm':			/* author can be included in alias */
		me_too = TRUE;
		break;

	    case 'Q':
		queue_only = TRUE;	/* spool but do not deliver, yet */
		break;

	    case 'e':			/* what to do on errors */
		if (arg[0] == '\0') {
		    arg = *args++;
		    panic_if_null(arg, "e");
		}
		switch(*arg) {

		case 'e':		/* we don't support berkenet */
		case 'm':		/* mail back errors */
		    error_processing = MAIL_BACK;
		    break;
		case 'w':		/* send via "write" */
		    error_processing = WRITE_BACK;
		    break;
		case 'p':		/* print errors on screen */
		    error_processing = TERMINAL;
		    break;
		case 'q':		/* be quiet about errors */
		    error_processing = DEV_NULL;
		    break;
		}
		arg = end_arg;		/* swallows complete argument */
		break;

	    case 'o':			/* set various option */
		if (arg[0] == '\0') {
		    arg = *args++;
		    panic_if_null(arg, "o");
		}
		switch (*arg++) {

		case 'i':		/* same as -i */
		    dot_usage = NO_DOT_PROTOCOL;
		    break;

		case 'A':
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oA");
		    }
		    arg_alias_file = arg;
		    break;

		case 'I':		/* same as -I */
		    dot_usage = HIDDEN_DOTS;
		    break;

		case 'U':
		    report_memory_usage = TRUE;
		    break;

		case 'M':
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oM");
		    }
		    switch (*arg++) {
		    case 's':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMs");
			}
			sender_host = arg;
			break;

		    case 'a':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMa");
			}
			sender_host_addr = arg;
			break;

		    case 'r':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMr");
			}
			sender_proto = arg;
			break;

		    case 'u':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMu");
			}
			ident_sender = arg;
			break;

		    case 'v':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMv");
			}
			ident_method = arg;
			break;

		    case 'P':
			if (*arg == '\0') {
			    arg = *args++;
			    panic_if_null(arg, "oMP");
			}
			sender_program = arg;
			break;
		    }
		    break;

		case 'e':		/* same as -eX */
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oe");
		    }
		    switch (*arg) {

		    case 'm':
			error_processing = MAIL_BACK;
			break;
		    case 'w':
			error_processing = WRITE_BACK;
			break;
		    case 'p':
			error_processing = TERMINAL;
			break;
		    case 'q':
			error_processing = DEV_NULL;
			break;
		    }
		    break;

		case 'd':		/* delivery mode */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "od");
		    }
		    switch (*arg) {

		    case 'f':
			deliver_mode = FOREGROUND;
			break;

		    case 'b':
			deliver_mode = BACKGROUND;
			break;

		    case 'q':
			deliver_mode = QUEUE_MESSAGE;
			break;
		    }
		    break;

		case 'C':		/* name of config file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oC");
		    }
		    config_file = arg;
		    break;

		case 'L':
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oL");
		    }
		    arg_smail_lib_dir = arg;
		    break;

		case 'S':		/* name of secondary config file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oS");
		    }
		    arg_second_config_file = arg;
		    break;

		case 'D':		/* name of director file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oD");
		    }
		    arg_director_file = arg;
		    break;

		case 'E':		/* name of retry file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oE");
		    }
		    arg_retry_file = arg;
		    break;

		case 'G':		/* runq grades */
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oE");
		    }
		    arg_runq_grades = arg;
		    break;

		case 'Q':		/* name of qualify file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oQ");
		    }
		    arg_qualify_file = arg;
		    break;

		case 'R':		/* name of router file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oR");
		    }
		    arg_router_file = arg;
		    break;

		case 'T':		/* name of transport file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oT");
		    }
		    arg_transport_file = arg;
		    break;

		case 'X':
		    if (*arg == '\0') {
			arg = *args++;
			panic_if_null(arg, "oX");
		    }
		    smtp_service_name = arg;
		    break;

		case 'm':		/* same as -m */
		    me_too = TRUE;
		    break;
		}
		arg = end_arg;
		break;

	    case 'b':			/* set operating mode */
		if (*arg == '\0') {
		    arg = *args++;
		    panic_if_null(arg, "b");
		}
		switch (*arg) {

		case 'd':		/* operate as daemon */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    operation_mode = DAEMON_MODE;
		    break;

		case 'i':		/* initialize aliases database */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    operation_mode = REBUILD_ALIASES;
		    break;

		case 'm':		/* just deliver mail */
		    operation_mode = DELIVER_MAIL;
		    break;

		case 'p':		/* print the queue */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    operation_mode = PRINT_QUEUE;
		    break;

		case 't':		/* run in address test mode */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    operation_mode = TEST_MODE;
		    break;

		case 'v':		/* verify addresses only */
		    operation_mode = VERIFY_ADDRS;
		    break;

		case 'V':
		    operation_mode = PRINT_VERSION;
		    break;

		case 's':		/* process smtp on input */
		    operation_mode = SMTP_MODE;
		    break;

		case 'S':		/* batched SMTP mode */
		    operation_mode = BATCH_SMTP_MODE;
		    break;

		case 'z':		/* freeze config file */
		    if (prog_type == PROG_RMAIL || prog_type == PROG_RSMTP) {
			rmail_panic();
		    }
		    operation_mode = FREEZE_CONFIG;
		    break;

	        case 'R':		/* rogue tombstone mode */
		    operation_mode = ROGUE_MODE;
		    break;

		case 'c':		/* print COPYING file */
		    operation_mode = COPYING_MODE;
		    break;

		case 'P':
		    operation_mode = PRINT_VARS_MODE;
		    break;
		}
		arg = end_arg;
		break;
	    }
	} else {

	    /*
	     * not a flag, arg must be a recipient address so insert
	     * it in the list.
	     */
	    cur = alloc_addr();
	    cur->in_addr = escape_newline(arg);
	    cur->uid = nobody_uid;
	    cur->gid = nobody_gid;
	    cur->succ = recipients;
	    recipients = cur;
	}
    }
}

static char *
escape_newline(s)
    register char *s;
{
    struct str str;
    register struct str *sp = &str;
    register int c;

    if (strchr(s, '\n') == NULL)
	return s;
    STR_INIT(sp);

    while ((c = *s++)) {
	if (c == '\n')
	    STR_CAT(sp, "\\n");
	else
	    STR_NEXT(sp, c);
    }
    STR_NEXT(sp, '\0');
    STR_DONE(sp);

    return sp->p;
}

/*
 * panic_if_null - complain with a usage message if the given pointer is NULL
 */
static void
panic_if_null(p, fl)
    char *p;
    char *fl;				/* name of flag to give usage for */
{
    if (p == NULL) {
	if (errfile) {
	    (void) fprintf(errfile, "%s: argument expected after -%s\n",
			   program, fl);
	}
	exit(EX_USAGE);
    }
}

/*
 * rmail_panic - complain about an option not allowed with rmail or rsmtp
 */
static void
rmail_panic()
{
    if (errfile) {
	(void) fprintf(errfile,
		       "%s: usage with rmail and rsmtp is restricted\n",
		       program);
    }
    exit(EX_USAGE);
}

/*
 * parse_grade_range - parse a delivery range into a min & max value
 * No error checking/reporting.
 * Code mostly stolen from read_methods_file() in src/route.c
 */
static void 
parse_grade_range(range, min, max)
     char * range;
     int * min;
     int * max;
{
    int mn, mx;

    mn = 0;
    mx = 255;
    if (range != NULL) {
	if (isalnum(*range)) {
	    mn = *range++;
	    mx = mn;
	}
	if (*range == '-') {
	    range++;
	    mx = 255;
	    if (isalnum(*range)) {
		mx = *range;
	    }
	}
    }

    /* Make sure that min <= max - make life easier! */
    if (mx >= mn) {
	*min = mn;
	*max = mx;
    } else {
	*min = mx;
	*max = mn;
    }
}
