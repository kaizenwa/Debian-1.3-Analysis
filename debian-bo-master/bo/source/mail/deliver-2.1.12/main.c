/* $Id: main.c,v 1.11 1993/10/28 16:49:51 chip Exp $
 *
 * A program to deliver local mail with some flexibility.
 *
 * $Log: main.c,v $
 * Revision 1.11  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.10  1992/11/19  16:25:27  chip
 * Reset SIGCLD (or SIGCHLD) during setup.
 *
 * Revision 1.9  1992/05/12  21:02:17  chip
 * No diagnostic message when sender address is unsafe;
 * it's not the local system's fault, and delivery proceeds.
 *
 * Revision 1.8  1992/01/20  20:35:14  chip
 * Eliminate GCC2 warnings about unparenthesized || and &&.
 *
 * Revision 1.7  1991/10/28  15:45:38  chip
 * Don't throw mail away just because sender address is unsafe.
 *
 * Revision 1.6  1991/08/05  19:03:14  chip
 * Work around SCO bug in setgroups().
 * Simplify usage().  Check sender addresses for safety.
 *
 * Revision 1.5  1991/06/20  12:52:09  chip
 * Check return value of close() on temp files.
 *
 * Revision 1.4  1991/06/17  15:28:38  chip
 * Announce program version after parsing command line options.
 *
 * Revision 1.3  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.2  91/05/29  17:23:32  chip
 * Rearrange global vars.  Make "sysmb_gid" global; set it in main().
 * 
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include "patchlevel.h"

#ifdef SIGCLD
# define CHILD_SIGNAL SIGCLD
#else
# ifdef SIGCHLD
#  define CHILD_SIGNAL SIGCHLD
# endif
#endif

/*
 * External data.
 */

/* Variables set by getopt() [blech] */

extern int optind, opterr;
extern char *optarg;

/*
 * Local data
 */

static char *dfl_sys = NULL;
static char *dfl_post = NULL;
static char *dfl_err = NULL;
static char *dfl_user = NULL;

static char *trusted_users[] = { TRUSTED_USERS, NULL };
static char *mailer_users[] = { MAILER_USERS, NULL };

/*
 * Global data
 */

#ifdef DELHOME
char delhome[] = DELHOME;
#endif

int verbose = FALSE;
int dryrun = FALSE;
int rundfiles = TRUE;
int printaddrs = FALSE;
int leavetemps = FALSE;
int boxdelivery = FALSE;

char *progname = "deliver";
char version[32] = "2.1";
char *shell = SHELL;

int rec_level = 0;
int rec_parent = -1;
char *sys_deliver = NULL;
char *post_deliver = NULL;
char *err_deliver = NULL;
char *user_deliver = NULL;
char *local_sender = NULL;
char *orig_sender = NULL;
char *hostname = NULL;

int eff_uid = -1;
int eff_gid = -1;
int real_uid = -1;
int real_gid = -1;
int maxgroups = 0;

#ifdef MBX_GROUP
int sysmb_gid = -1;
#endif

int trusted_user = FALSE;
int mailer_user = FALSE;

CONTEXT *eff_ct = NULL;
CONTEXT *real_ct = NULL;

char *eff_group = NULL;
char *real_group = NULL;

FILE *log = NULL;
char *t_logfile = NULL;
char *f_logfile = NULL;

FILE *errlog = NULL;
char *t_errlogfile = NULL;
char *f_errlogfile = NULL;

int tty_input = FALSE;
SIGFLAG got_sig = FALSE;

char *ttype[T_MAX] = { "header", "body", "header copy", "body copy" };
char *tfile[T_MAX] = { NULL, NULL, NULL, NULL };
char *tenv[T_MAX] = { NULL, NULL, ENV_HEADER, ENV_BODY };
int tfd[T_MAX] = { -1, -1, -1, -1 };

/*
 * Local macros.
 */

/* How to construct a path relative to DELHOME. */

#ifdef DELHOME
#define HomeRel(path)	relpath(delhome, (path))
#else
#define HomeRel(path)	copystr(path)
#endif

/*
 * Local functions.
 */

static void locsend();
static void main_addrs();
static void main_boxes();
static void main_toodeep();
static void usage();
static void catch_sigs();
static void ignore_sigs();
static SIGTYPE sighup(), sigint(), sigquit();
static int report_errors();
static int uid_member();
static void setup_environ();

/*----------------------------------------------------------------------
 * The Program.
 */

int
main(argc, argv)
int argc;
char **argv;
{
    char *hostopt, *locsendopt, *p;
    int i, c, errcount, copy;

    /* Make sure that stdout and stderr are interleaved correctly */

    Linebuf(stdout);
    Linebuf(stderr);

    /* Figure out the name used to invoke this program. */

    progname = basename(argv[0]);

    /* Determine default paths. */

    sys_deliver =  dfl_sys =  HomeRel(SYS_DELIVER);
    post_deliver = dfl_post = HomeRel(POST_DELIVER);
    err_deliver =  dfl_err =  HomeRel(ERR_DELIVER);
    user_deliver = dfl_user = USER_DELIVER;

#ifdef LOG
    f_logfile = HomeRel(LOG);
#endif
#ifdef ERRLOG
    f_errlogfile = HomeRel(ERRLOG);
#endif

    /* Special hack -- handle the recursion level and parent first. */

    if ((p = getenv(ENV_DLEVEL)) != NULL && (i = atoi(p)) > 0)
	rec_level = i;

    if ((p = getenv(ENV_DPID)) != NULL && (i = atoi(p)) > 0)
	rec_parent = i;

    /* If recursion level is non-zero, append it to the program name. */

    if (rec_level > 0)
    {
	char *np;

	np = zalloc((unsigned) strlen(progname) + 16);
	(void) sprintf(np, "%s[%d]", progname, rec_level);
	progname = np;
    }

    /* What version of the program is this? */

    (void) sprintf(version + strlen(version), ".%02d", PATCHLEVEL);

    /* Let's be sane about the file creation mask. */

    (void) umask(UMASK);

    /* Find effective and real uids and gids. */

    eff_uid = geteuid();
    eff_gid = getegid();
    real_uid = getuid();
    real_gid = getgid();

    /* Make sure that setuidness is reasonable. */

    if (eff_uid != real_uid && eff_uid != 0)
    {
	error("if setuid, must be setuid root");
	leave(1);
    }

    /*
     * Determine whether the given user is someone we trust
     * to have special priviledges.
     */

    if (uid_member(real_uid, mailer_users))
	mailer_user = TRUE;
    if (uid_member(real_uid, trusted_users))
	trusted_user = TRUE;

#ifdef GROUP_VECTOR

    /* Find maximum size of group vector. */

    if ((maxgroups = MAXGROUPS()) == -1)
    {
	syserr("can't get maximum size of group vector");
	leave(1);
    }

    /* If superuser, zap any inherited group vector. */

    if (eff_uid == 0)
    {
	if (setgrvec(0, (GRVEC_T *)NULL) == -1)
	{
	    syserr("can't lose group vector");
	    leave(1);
	}
    }

#endif	/* GROUP_VECTOR */

    /* Process environment: handle recursive invocation. */

    if ((p = getenv(ENV_DFLAGS)) != NULL)
    {
	while (*p)
	{
	    switch (*p++)
	    {
	    case 'v':
		verbose = TRUE;
		break;
	    case 'd':
		verbose = TRUE;
		dryrun = TRUE;
		break;
	    case 'A':
		printaddrs = TRUE;
		dryrun = TRUE;
		break;
	    case 'n':
		rundfiles = FALSE;
		break;
	    case 't':
		leavetemps = TRUE;
		break;
	    }
	}
    }

    if ((p = getenv(ENV_SYSDEL)) != NULL && *p)
	sys_deliver = copystr(p);
    if ((p = getenv(ENV_POSTDEL)) != NULL && *p)
	post_deliver = copystr(p);
    if ((p = getenv(ENV_ERRDEL)) != NULL && *p)
	err_deliver = copystr(p);
    if ((p = getenv(ENV_USERDEL)) != NULL && *p)
	user_deliver = copystr(p);

    if ((p = getenv(ENV_SENDER)) != NULL && *p)
	orig_sender = copystr(p);

    /* Parse command line arguments */

    hostopt = getenv(ENV_HOSTNAME);
    locsendopt = NULL;
    while ((c = getopt(argc, argv, "vdAntbs:p:e:u:l:r:h:")) != EOF)
    {
	switch (c)
	{
	case 'v':
	    verbose = TRUE;
	    break;
	case 'd':
	    verbose = TRUE;
	    dryrun = TRUE;
	    break;
	case 'A':
	    printaddrs = TRUE;
	    dryrun = TRUE;
	    break;
	case 'n':
	    rundfiles = FALSE;
	    break;
	case 't':
	    leavetemps = TRUE;
	    break;
	case 'b':
	    boxdelivery = TRUE;
	    break;
	case 's':
	    if (*optarg)
		sys_deliver = optarg;
	    break;
	case 'p':
	    if (*optarg)
		post_deliver = optarg;
	    break;
	case 'e':
	    if (*optarg)
		err_deliver = optarg;
	    break;
	case 'u':
	    if (*optarg)
		user_deliver = optarg;
	    break;
	case 'l':
	    if (*optarg)
		locsendopt = optarg;
	    break;
	case 'r':
	    if (*optarg)
		orig_sender = optarg;
	    break;
	case 'h':
	    if (*optarg)
		hostopt = optarg;
	    break;
	case '?':
	    usage();
	}
    }

    /* If verbose, announce identity. */

    if (verbose)
	message("%s %s\n", progname, version);

    /* Open temporary log files. */

    openlogs();

    /* If no destinations were given, forget it. */

    if (optind >= argc)
    {
	error("no recipients specified");
	usage();
    }

    /* Figure out the name of this host. */

    if ((hostname = gethost()) == NULL)
    {
	hostname = "unknown";
	error("unable to determine host name; using \"%s\"",
	      hostname);
    }

    if (verbose)
	message("hostname = %s\n", hostname);

    /*
     * Renounce setuid privileges if:
     *   1.  The caller is not trusted,
     *        and he specified non-default delivery file name(s).
     *   2.  The caller is not a mailer,
     *        and he specified an incorrect hostname.
     */

    if ((!trusted_user
	 && (strcmp(dfl_sys, sys_deliver) != 0
	    || strcmp(dfl_post, post_deliver) != 0
	    || strcmp(dfl_err, err_deliver) != 0
	    || strcmp(dfl_user, user_deliver) != 0))
     || (!mailer_user
	 && (hostopt && strcmp(hostopt, hostname) != 0)))
    {
	if (eff_uid != real_uid && setuid(real_uid) == -1)
	{
	    syserr("can't renounce setuid privileges");
	    leave(1);
	}
	if (eff_gid != real_gid && setgid(real_gid) == -1)
	{
	    syserr("can't renounce setgid privileges");
	    leave(1);
	}

	if ((eff_uid = geteuid()) != real_uid
	    || (eff_gid = getegid()) != real_gid)
	{
	    error("kernel bug: can't renounce setuid/setgid");
	    leave(1);
	}

	if (verbose)
	    message("renounced setuid privileges\n");
    }

    /* Get our effective and real user contexts and group names. */

    if ((eff_ct = uid_context(eff_uid)) == NULL)
	error("invalid effective uid %d", eff_uid);

    if ((real_ct = uid_context(real_uid)) == NULL)
	error("invalid real uid %d", real_uid);

    if ((eff_group = group_name(eff_gid)) == NULL)
	error("invalid effective gid %d", eff_gid);

    if ((real_group = group_name(real_gid)) == NULL)
	error("invalid real gid %d", real_gid);

    if (!eff_ct || !real_ct || !eff_group || !real_group)
	leave(1);

    if (verbose)
    {
	message("eff:  user = %s (%d/%d), group = %s (%d)\n",
		eff_ct->ct_name, eff_ct->ct_uid, eff_ct->ct_gid,
		eff_group, eff_gid);
	message("real: user = %s (%d/%d), group = %s (%d)\n",
		real_ct->ct_name, real_ct->ct_uid, real_ct->ct_gid,
		real_group, real_gid);
    }

    /* Assume the user-specified host identity, if any. */

    if (hostopt)
    {
	hostname = hostopt;
	if (verbose)
	    message("pretend host = %s\n", hostname);
    }

    /*
     * Determine the local sender, i.e. the user who is
     * executing Deliver.  Whoever the local sender claims
     * to be, we don't believe him unless his claim jibes
     * with the real uid.  As a last resort, we use the
     * name associated with the real context.
     */

    locsend(locsendopt);
    locsend(getenv(ENV_LOCALSENDER));
    locsend(getenv("LOGNAME"));
    locsend(getenv("USER"));
    locsend(getlogin());
    if (!local_sender)
	local_sender = real_ct->ct_name;

    if (verbose)
	message("local sender = %s\n", local_sender);

    /*
     * What group should own system mailboxes?
     */

#ifdef MBX_GROUP
    if ((sysmb_gid = group_id(MBX_GROUP)) == -1)
    {
	error("%s: bad system mailbox group", MBX_GROUP);
	leave(1);
    }
#endif

    /*
     * Where is the message coming from?
     */

    if (isatty(0))
	tty_input = TRUE;

    /*
     * If we are not going to deliver, or if we are receiving the
     * message from a tty, catch signals so we can remove temp files.
     * Otherwise, ignore signals.
     */

    if (dryrun || tty_input)
	catch_sigs();
    else
	ignore_sigs();

    /*
     * Handle death-of-child signal in the default way.
     */

#ifdef CHILD_SIGNAL
    (void) signal(CHILD_SIGNAL, SIG_DFL);
#endif

    /*
     * Create the temporary files and write the message to them.
     */

    copy = copy_message();

    /*
     * Now that we know the sender address, be sure it's clean;
     * if it's not, use the local sender address instead.
     */

    if (!addr_clean(orig_sender))
	orig_sender = local_sender;

    /*
     * No more signals...
     */

    ignore_sigs();

    /*
     * ... but if we had already caught a signal,
     *     or if copy_msg() had a problem, leave.
     */

    if ((copy < 0) || got_sig)
    {
	if (got_sig)
	    error("caught signal - exiting");
	leave(1);
    }

    /*
     * Set up useful environment variables.
     * Note that this must be done _after_ copy_message(),
     * since that's where the temp files are created.
     */

    setup_environ();

    /*
     * If recursion is too deep, consider mail undeliverable.
     */

    if (rec_level > REC_LIMIT)
	main_toodeep();

    /*
     * Else if the "-b" flag was specified, arguments are mailboxes.
     */

    else if (boxdelivery)
	main_boxes(argc - optind, argv + optind);

    /*
     * Otherwise, arguments are addresses.
     */

    else
	main_addrs(argc - optind, argv + optind);

    /*
     * Do all delivery.
     * If anything happens, print a debugging message.
     */

    i = 0;
    i += (mbox_deliver() > 0);
    i += (prog_deliver() > 0);
    i += (uucp_deliver() > 0);
    if (i > 0 && verbose)
	dumpdests("after delivery");

    /*
     * If we're not in mailbox mode, and if the error delivery file
     * executes, deliver to any destination(s) that it generated.
     */

    if (!boxdelivery
	&& (i = err_dfile()) > 0)
    {
	if (verbose)
	    dumpdests("after running error delivery file");

	i = 0;
	i += (mbox_deliver() > 0);
	i += (prog_deliver() > 0);
	i += (uucp_deliver() > 0);
	if (i > 0 && verbose)
	    dumpdests("after delivery to error destinations");
    }

    /*
     * Report all results in log file.
     */

    logreport(argc - optind, argv + optind);

    /*
     * Report any errors.
     */

    errcount = report_errors(argc - optind, argv + optind);

    /*
     * All done.
     */

    leave(errcount ? 1 : 0);
    /* NOTREACHED */
}

/*----------------------------------------------------------------------
 * If the local sender has not yet been identified, try the
 * given name for correctness.  We only believe it if it is
 * a real user with a user id equal to real_uid.
 */

static void
locsend(name)
char *name;
{
    CONTEXT *ct;

    if (local_sender || !name)
	return;

    if (!addr_clean(name))
	return;

    if ((ct = name_context(name)) != NULL
	&& ct->ct_uid == real_uid)
	local_sender = copystr(name);
}

/*----------------------------------------------------------------------
 * Normal address handling.
 * Call system delivery file, user delivery files,
 * post-user delivery file, error delivery file.
 */

static void
main_addrs(ac, av)
int ac;
char **av;
{
    int n;

    /* Run all destinations though the system delivery file. */

    if (sys_dfile(ac, av) >= 0)
    {
	if (verbose)
	    dumpdests("after running system delivery file");
    }
    else
    {
	int a;

	/*
	 * System delivery file is missing or ignored.
	 * Use the argument list verbatim.
	 */

	for (a = 0; a < ac; ++a)
	    (void) addr_dest(av[a], (CONTEXT *) NULL);

	if (verbose)
	    dumpdests("as taken from argument list");
    }

    /*
     * Run each user destination through his delivery file.
     */

    n = user_dfiles();
    if (n > 0 && verbose)
	dumpdests("after running user delivery files");

    /*
     * Run each remaining destination though the post-user
     * delivery file.
     */

    n = post_dfile();
    if (n > 0 && verbose)
	dumpdests("after running post-user delivery file");
}

/*----------------------------------------------------------------------
 * Consider all arguments as mailbox filenames.
 */

static void
main_boxes(ac, av)
int ac;
char **av;
{
    int a;

    if (verbose)
	message("mailbox delivery as %s\n", real_ct->ct_name);

    for (a = 0; a < ac; ++a)
	(void) dest(real_ct->ct_name, CL_MBOX, av[a]);

    if (verbose)
	dumpdests("(should all be mailboxes)");
}

/*----------------------------------------------------------------------
 * Recursion too deep.  Bail out.
 */

static void
main_toodeep()
{
    error("recursion limit (%d) exceeded; writing to %s:%s",
	  REC_LIMIT, eff_ct->ct_name, MBX_UNDEL);

    dest_undel(eff_ct->ct_name);
}

/*----------------------------------------------------------------------
 * Print a usage message and exit.
 */

static void
usage()
{
    static char *m[] = {
	"-b       All arguments are mailbox filenames.\n",
	"         (Default: arguments are user names.)\n",
	"-A       Resolve addresses but do not deliver.\n",
	"-d       Be verbose but do not deliver.\n",
	"-v       Be verbose and deliver.\n",
	"-n       Do not run any delivery files.\n",
	"-t       Do not remote temp files before exiting.\n",
	"-s file  Specify the system delivery filename.\n",
	"-p file  Specify the post-user delivery filename.\n",
	"-e file  Specify the error delivery filename.\n",
	"-u file  Specify the user delivery filename.\n",
	"-r from  Specify the address to appear in the \"From \" line.\n",
	"-R from  Specify the login name of the local sender.\n",
	"-h host  Specify the host name.\n",
	"args     Addresses, or if \"-b\", mailbox names.\n",
	NULL
    };
    char **u;

    message("Usage: %s [-b][-A][-d][-v][-n][-t]\n", progname);
    message("       %*s [-r sender][-R localsender][-h host] args\n",
	    (int) strlen(progname), "");
    for (u = m; *u; ++u)
	message(*u);
    leave(1);
}

/*----------------------------------------------------------------------
 * Clean up and exit.
 */

NORETURN void
leave(code)
int code;
{
    /* Report vital statistics if something went wrong. */

    logerrinfo();

    /* Discard temporary files. */

    if (!leavetemps)
    {
	int t;

	for (t = 0; t < T_MAX; ++t)
	{
	    if (tfd[t] != -1)
	    {
		if (close(tfd[t]) == -1)
		    syserr("can't close %s file %s", ttype[t], tfile[t]);
		tfd[t] = -1;
	    }
	    if (tfile[t] && unlink(tfile[t]) == -1)
		syserr("can't unlink %s file %s", ttype[t], tfile[t]);
	}
    }

    /* Save temporary logs by appending to real logfiles. */

    savelogs();

    /* Discard temporary logs unless user requested otherwise. */

    if (!leavetemps)
	tosslogs();

    /* "I am outa here." */

    exit(code);
}

/*----------------------------------------------------------------------
 * Catch signals.
 */

static void
catch_sigs()
{
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
	(void) signal(SIGHUP, sighup);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
	(void) signal(SIGINT, sigint);
    if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
	(void) signal(SIGQUIT, sigquit);
}

/*----------------------------------------------------------------------
 * Ignore signals.
 */

static void
ignore_sigs()
{
    (void) signal(SIGHUP, SIG_IGN);
    (void) signal(SIGINT, SIG_IGN);
    (void) signal(SIGQUIT, SIG_IGN);
}

static SIGTYPE
sighup()
{
    (void) signal(SIGHUP, sighup);
    got_sig = TRUE;
}

static SIGTYPE
sigint()
{
    (void) signal(SIGINT, sigint);
    got_sig = TRUE;
}

static SIGTYPE
sigquit()
{
    (void) signal(SIGQUIT, sigquit);
    got_sig = TRUE;
}

/*----------------------------------------------------------------------
 * Report any errors to stderr.
 * Return an error count.
 */

static int
report_errors(ac, av)
int ac;
char **av;
{
    DEST **dv;
    int i, count, errcount;

    if ((dv = dest_array(&count)) == NULL)
	return 0;

    errcount = 0;
    for (i = 0; i < count; ++i)
    {
	DEST *d;
	char *e;
	int len;

	d = dv[i];

	if (d->d_state != ST_ERROR)
	    continue;

	if (++errcount == 1)
	{
	    int a;

	    error("delivery error on host %s", hostname);
	    message("Delivery to %s address%s failed:\n",
		    ac ? "these" : "this",
		    ac ? "es" : "");
	    for (a = 0; a < ac; ++a)
		message("\t%s\n", av[a]);
	    message("Reason(s) for failure:\n");
	}

	message("\t\"%s\"", d->d_name);
	if (d->d_class == CL_MBOX)
	    message(", mailbox \"%s\"", d->d_param);
	else if (d->d_class == CL_PROG)
	    message(", program \"%s\"", d->d_param);

	e = derrmsg(d);
	len = strlen(d->d_name) + strlen(e);
	if (d->d_param)
	    len += strlen(d->d_param);
	message(":%s%s\n", (len > 60) ? "\n\t\t" : " ", derrmsg(d));
    }

    free((char *) dv);

    return errcount;
}

/*----------------------------------------------------------------------
 * Is the given uid a member of the given set?
 */

static int
uid_member(uid, users)
int uid;
char **users;
{
    CONTEXT *ct;
    char **u;

    for (u = users; *u; ++u)
    {
	if ((ct = name_context(*u)) != NULL && uid == ct->ct_uid)
	    return TRUE;
    }

    return FALSE;
}

/*----------------------------------------------------------------------
 * Set up useful environment variables.
 */

static void
setup_environ()
{
    char s[8];
    int f = 0;

    /* Environment variables not specific to Deliver... */

    alloc_env("IFS", " \t\n");
    del_env("ENV");		/* in case SHELL is ksh */

    /* Environment variables a child Deliver will believe... */

    (void) sprintf(s, "%d", getpid());
    alloc_env(ENV_DPID, s);

    (void) sprintf(s, "%d", rec_level + 1);
    alloc_env(ENV_DLEVEL, s);

    s[f++] = '-';
    if (verbose)
	s[f++] = (dryrun ? 'd' : 'v');
    if (printaddrs)
	s[f++] = 'A';
    if (leavetemps)
	s[f++] = 't';
    s[f] = 0;
    alloc_env(ENV_DFLAGS, (f > 1) ? s : "");
    alloc_env(ENV_SYSDEL, sys_deliver);
    alloc_env(ENV_USERDEL, user_deliver);
    alloc_env(ENV_SENDER, orig_sender ? orig_sender : "");
    alloc_env(ENV_LOCALSENDER, local_sender ? local_sender : "");

    /* Environment variables a child Deliver will _not_ believe... */

    alloc_env(ENV_HOSTNAME, hostname);
}
