/* The code should compile with either ANSI C or K&R compilers. */

/*
 *      Copyright (c) 1993 by California Institute of Technology.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

#include "super.h"
#include "version.h"

/*
 * Super allows users to execute other programs, particularly
 * scripts, as root (or another user/group), without unduly
 * compromising security.
 * 
 * Use:
 * 
 *     $0 [-d | -D] [-h | -? | -H | -V] commandname [args...]
 * or
 *     $0 -c [-d | -D] [superfile]
 * 
 * Options:
 *	-c -- check syntax of super file, but don't execute anything.
 *	-d -- debug mode: show what would be done, but don't actually
 *		execute a command.
 *	-D -- verbose debug mode: same as -d, plus tell more about variables.
 *	-h, -? -- help: print list of allowed commands, then exit.
 *	-H -- a long-winded variant of -h.
 *	-f -- (just the facts, ma'm) a version of -H that prints no extra
 *	      info, just a list of what you can execute, using the format:
 *		Cmd FullPath [initial args]
 *		Cmd FullPath [initial args]
 *		...
 *	      Useful for scripts that want a list of what you may execute.
 *	-V -- print version information, then exit.
 * 
 * The super.tab file names each command that super will execute, and
 * says who can use it.  See super.tab.summary for an overview.

 */

char *superfile = SUPERFILE;	/* The super.tab file */

/* Global info */
GlobalInfo globalinfo = {
	"",		/* owner (required owner of file) */
	NULL,		/* chdir_path (char *) */
	0,		/* relative_path (bool) */
	0,		/* group_slash (bool) */
	0,		/* nice_incr (int) */
	0,		/* mask (umask) */

	 {		/* Global password requirements */
		0,	/* required (bool) */
		5,	/* timeout (min) */
		0,	/* renewtime (bool) */
		1,	/* perhost (bool) */
		""},	/* user (user who owns file) */


	{0,NULL,NULL},	/* userbefore: list of u/g/h pats before per-cmd pats */
    	{0,NULL,NULL},	/* userafter: list of u/g/h pats after per-cmd pats */
    	{0,NULL},	/* b_a_text: list of original text for above */
	1,		/* user_clear: (bool: clear if new val seen) */

	{{0,0,0,0},	/* timebefore: permitted times before per-cmd pats */
		NULL},
	{{0,0,0,0},	/* timeafter: permitted times after per-cmd pats */
		NULL},
	1,		/* time_clear: clear if new val seen */

	0,		/* use_after: set to !0 when we see <> */

	{ NULL,		/* log: FILE *fp */
	    "",		/* filename */
	    "",		/* user: value of loguid=xxx */
	    0,		/* uid: UID under which we open logfile */
	    -1,		/* pid: PID of the logger process */
	    0,		/* newfile: !0 if logfile given but not yet used */
	    0 },	/* newuid: !0 if loguid given but not yet used */

	"",		/* mailcmd */
	0,		/* mail_success (bool) */
	1,		/* use gethostbyname (bool) */

    	{0},		/* groups[]: gid's of supplementary groups */
    	GROUPS_NOTSET,	/* ngroups: number of supplementary groups */
    	0,		/* groups_added */
	};

/* The list of currently open files */
FileList *currfile = NULL;

/* The struct of what things we've matched */
Conditions matches;

/* The localinfo struct is filled in a bit at a time until it completely
 * describes the caller, the program to invoke, any options
 * set in the control line, etc.
 */
UserInfo userinfo;

LocalInfo localinfo = {
	{NULL, -1, 0, "", 0, 0},	/* Initialize ProgMatch empty */
					/* Other elements that need
					 * start initializers are done
					 * through explict assignment.
					 */
		};

extern char *s_re_comp __P(( char *));	/* regular-expression compiler */
int shell_compare __P(( char * ));	/* s_re_comp()-style i/f to wildmat() */
extern int s_re_exec __P(( char * ));	/* regular-expression comparison */
char *shell_compile __P(( char * ));	/* s_re_exec()-style i/f to wildmat() */


char *prog;				/* this program */
int debug=0;				/* Set by the debug options flag */
int check_syntax=0;			/* Set by the -c options flag */

/* Routines used to compile/match user/group/host patterns */
char *(*pat_compile) __P((char *)) = s_re_comp;
int (*pat_compare) __P((char *)) = s_re_exec;
int need_re_anchor = 1;


/* For strqtokS */
unsigned char my_qm[256];		/* our quotemarks */
unsigned char my_cc[256];		/* our comment characters */


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
main(argc, argv)
int argc;
char **argv;
{
    int status, n_builtin;
    char *s, *cmd, *path;
    char **arglist, **envp;
    extern char *error_prog;
    int iarg, givehelp, giveversion, verbosity;

    s = strrchr(argv[0], '/');
    prog = (s && *(s+1)) ? s+1 : argv[0];
    error_prog = ONETRUENAME;	/* same as prog, but used by Error() */

#ifdef HAVE_LOCALE_H
    setlocale(LC_ALL, "");
#endif
    error_srcfile = superfile;	/* error messages w/ line nums refer to this */
    error_line = -1;
    if (add_variable("$", "$") == -1)	/* Built-in variable $$ -> "$" */
	return 1;

    if (init_userinfo() == -1)	/* Initialize userinfo struct. */
	return 1;
    init_globalinfo();		/* Initialize globalinfo struct. */
    init_localinfo();		/* Initialize localinfo struct. */

    /* Decide if we were invoked as "super cmd args...", or 
     * as a link: cmd args...
     */
    if (strcmp(prog, ONETRUENAME) == 0) {
	/* Invoked as super [options] cmd args... */
	iarg = do_options(argc, argv, &givehelp, &giveversion, &verbosity);
	if (iarg < 0 || (argv[0] == NULL && !(givehelp || giveversion))) {
	    /* User screwed up; give help */
	    givehelp = 1;
	    cmd = NULL;
	} else {
	    cmd = argv[iarg];
	    argv += iarg;
	    /* Check if there was no cmd given after options. */
	    if (!cmd || !*cmd) {
		givehelp = 1;
		cmd = NULL;
	    }
	}
    } else {
	/* It's been invoked via link to super.  Therefore any options
	 * go to the command, not to super.
	 */
	s = strrchr(argv[0], '/');
	cmd = (s && *(s+1)) ? s+1 : argv[0];
    }

    if (debug)
	debug_hello();

    init_strqtokS();

    if (check_syntax)
	Error(0,0, "Checking syntax of superfile `%s'\n", superfile);

    /* Check if we need to switch to processing a user's super file */
    if (cmd && strchr(cmd, ':'))
	if (user_supertab(cmd) == -1)
	    return 1;

    /* Check for permission to execute, and change uid/gid as necessary */
    if ((path = approve(cmd, givehelp, giveversion, verbosity)) == NULL)
	return 1;
    else if (*path == '\0')
	return 0;

    /* Get the arglist for the command, and null-terminate cmd if not
     * already done so.  Do this before things like get_owner, because
     * newargs() parses paths that look like "command args" and separates
     * out the command part.
     */
    arglist = newargs(path, argv, &n_builtin);

    /* Determine ownership of the program */
    if (get_owner(path, &localinfo.file_uid, &localinfo.file_gid) != 0)
	return 1;

    /* Sanity check */
    if (check_syntax)
	Error(0, 2,
	    "Abort: shouldn't ever get here when check_syntax is set!\n");

    /* Check that the file to execute has proper ownership */
    if (check_owner() != 0)
	return 1;

    /* Button up for security, and get a modified environment */
    envp = buttonup(cmd);
    if (!envp)
	return 1;

    /* Set uid/gid if necessary */
    status = set_u_g();

    /* Change directory if requested */
    if (status != -1)
	status = set_chdir();

    /* Set the umask value */
    set_umask();

    if (debug) {
	debug_print(path, arglist, n_builtin);
	if (status == -1) {
	    fprintf(stderr, "\n\t(This command would not be executed \
due to previously-reported problem)\n");
	} else {
	    fprintf(stderr,
	    "\n\t(Your command is ok, but isn't executed in debug mode.)\n");
	}
	return 0;
    }
    if (status == -1) {
	return 1;
    }

    /* Check password requirements */
    if (check_pass(cmd) != 0)
	return 1;

    /* Log an informational message at LOG_INFO priority, not at
     * the usual error priority.
     */
    {
#ifdef HAVE_SYSLOG
	extern int error_priority;
	int old_pri = error_priority;
	error_priority = LOG_INFO;
#endif
	logmsg(cmd, arglist);
#ifdef HAVE_SYSLOG
	error_priority = old_pri;
#endif
    }
    /* Close the logfile writer, if any: we are done logging, and going
     * to exec the prog.
     */
    close_writer();

    if (set_nice_incr() == -1)
	return 1;

    /* If print option is set, write message before executing command */
    if (localinfo.print)
	puts(localinfo.print);

    if (execve(path, arglist, envp) == -1) {
#ifdef INTERPRETER_HACK
	if (errno == ENOEXEC) {
	    /* Open the file, check for "#!interpreter [argument]" */
	    FILE *fp;
	    char *interp, *argument, line1[1024];

	    if ((fp = fopen(path, "r")) &&		/* open the file */
		fgets(line1, sizeof(line1), fp) &&	/* read first line */
		(strlen(line1) < sizeof(line1)-1) &&	/* not too long? */
		(strncmp(line1, "#!", 2) == 0) &&	/* begins "#!"? */
		(interp = strtok(line1+2, " \t\n"))) {	/* has interpreter? */

		argument = strtok(NULL, " \t\n");	/* get opt argument */

		/* Adjust the arglist -- recall it has room for this */
		if (argument) {
		    arglist -= 2;
		    arglist[0] = arglist[2];
		    arglist[1] = argument;
		    arglist[2] = path;
		} else {
		    arglist--;
		    arglist[0] = arglist[1];
		    arglist[1] = path;
		}
		(void) execve(interp, arglist, envp);
	    }
	}
#endif
	/* If here, we failed to exec the prog.  Re-open the logfile we
	 * closed above and write a message.
	 */
	if (*globalinfo.log.filename != '\0')
	    opensuperlog();
	(void) Error(1,1, "command `%-.500s': Couldn't exec `%s': ", cmd, path);
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Set options; return index of arg that follows options, or -1 on error */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
do_options(argc, argv, givehelp, giveversion, verbosity)
int argc;
char **argv;
int *givehelp, *giveversion, *verbosity;
{
    int iarg;
    char *s;

    debug = check_syntax = *giveversion = *givehelp = 0;
    *verbosity = HELP_BASIC;	/* only matters if *givehelp != 0 */

    for (iarg = 1; iarg < argc && argv[iarg][0] == '-'; iarg++) {
	for (s = &argv[iarg][1]; *s; s++) {
	    switch (*s) {
	    case 'c': check_syntax = 1;
		    /* Check for optional superfile */
		    if (*(s+1)) {
			/* User gave -cfile */
			superfile = s+1;
			s += strlen(s) - 1;
		    } else if ((iarg+1 < argc) && argv[iarg+1][0] != '-') {
			/* User gave -c file */
			iarg++;
			superfile = argv[iarg];
		    }
		    error_srcfile = superfile;
		    break;
	    case 'd': debug = 1; break;
	    case 'D': debug = 2; break;
	    case 'V': *giveversion = 1; break;
	    case 'h': case '?': *givehelp = 1; break;
	    case 'f': *givehelp = 1; *verbosity = HELP_FACTS; break;
	    case 'H': *givehelp = 1;
			if (*verbosity != HELP_FACTS)
			    *verbosity = HELP_FULL;
		    break;
	    default:
		    return Error(0, 0, "Unrecognized option `%c'\n", *s);
	    }
	}
    }
    /* Default operation: give help if no args */
    if (argc == 1)
	*givehelp = 1;
    return iarg;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Print the debug startup lines */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debug_hello()
{
    char *s = dayname(userinfo.ourtime.day);
    fprintf(stderr, "\n\tExecuting: %s -%c:\n", prog, (debug < 2) ? 'd' : 'D');
    fprintf(stderr, "\tYou are: user=%s gid=%d hostname=%s\n\n",
	userinfo.caller.pw_name, userinfo.caller.pw_gid, userinfo.hostname);

    fprintf(stderr, "\tStart time=%d:%d/%s (hr*60+min=%d daycode=%d)\n",
	userinfo.ourtime.min/60, userinfo.ourtime.min%60, s,
	userinfo.ourtime.min, userinfo.ourtime.day);

}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Print the debug info */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debug_print(path, arglist, n_builtin)
char *path;
char **arglist;
int n_builtin;
{
    char *s, **p, **sp;
    char *cdpath;
    int isglobal, j, iarg;

    fprintf(stderr, "\nSuper file is `%s'\n", superfile);
    fprintf(stderr,
	"\n\tPermitted times for execution (in reverse input order):\n");
    if (globalinfo.timebefore.next != 0 || localinfo.time.next != 0
				    || globalinfo.timeafter.next != 0) {
	for (j = 0; j<3; j++) {
	    TimeList *tl;
	    switch (j) {
	    case 0: isglobal=1; tl = globalinfo.timeafter.next; break;
	    case 1: isglobal=0; tl = localinfo.time.next; break;
	    default: isglobal=1; tl = globalinfo.timebefore.next; break;
	    }
	    for ( ; tl ; tl=tl->next) {
		fprintf(stderr, "\t\t%stime~%d:%02d-%d:%02d/%s (%s)\n",
		    tl->te.invert ? "!" : "",
		    tl->te.begin / 60, tl->te.begin % 60,
		    tl->te.end / 60, tl->te.end % 60, dayname(tl->te.day),
		    isglobal ? "global def" : "per-cmd def");
	    }
	}
    } else {
	    fputs(" (unrestricted)\n", stderr);
    }
    fputs("\n", stderr);

    (void) fprintf(stderr, "\tCommand: <%s>\n", arglist[0]);
    (void) fprintf(stderr, "\tPath: <%s>\n", path);
    for (sp=arglist; *sp; sp++)
	;
    (void) fprintf(stderr, "\tArgc: %d\n", (int) (sp - arglist));
    for (sp=arglist; *sp; sp++) {
	iarg = sp - arglist;
	(void) fprintf(stderr, "\tArgv[%d]:  <%s>\n", iarg, *sp);
	if (iarg > n_builtin) {
	    s = StrEltGetPtr(&localinfo.argpats, iarg-n_builtin);
	    if (s)
		fprintf(stderr, "\t\tMust match pattern: %s\n", s);
	}
    }

    if (localinfo.usr_args[0] < 0) {
	(void) fprintf(stderr,
			    "\tAny number of user-entered args allowed.\n");
    } else if (localinfo.usr_args[0] == localinfo.usr_args[1] &&
		    localinfo.usr_args[0] == 0) {
	(void) fprintf(stderr, "\tNo user-entered args are allowed.\n");
    } else if (localinfo.usr_args[0] == localinfo.usr_args[1]) {
	(void) fprintf(stderr,
		    "\t%d user-entered arg%s required.\n",
		    localinfo.usr_args[0],
		    localinfo.usr_args[0] == 1? " is" : "s are");
    } else {
	(void) fprintf(stderr,
		    "\t%d - %d user-entered args are required.\n",
		    localinfo.usr_args[0], localinfo.usr_args[1]);
    }

    (void) fprintf(stderr, "\tCommand executes with nice increment = %d.\n",
		rcl_nice_incr());

    (void) fprintf(stderr, "\tCommand executes with umask set to 0%o.\n",
		rcl_umask());

    cdpath = localinfo.chdir_path ?
			localinfo.chdir_path : globalinfo.chdir_path;
    (void) fprintf(stderr,
			"\tCommand executes with working directory = %s\n",
			cdpath && *cdpath ? cdpath : "<unchanged>");

    (void) fprintf(stderr, "\tAdditional user's environment variables:\n");
	if (localinfo.env == NULL)
	    fprintf(stderr, "\t\t(none)\n");
	else
	    for (p=localinfo.env; *p; p++)
		(void) fprintf(stderr, "%s%s\n",
				p==localinfo.env ? "\t\t" : ",", *p);

    (void) fprintf(stderr,
			"\tEnvironment variables defined with setenv=var=:\n");
    if (localinfo.setenv[0] == NULL) {
	(void) printf("\t\t(none)\n");
    } else {
	for (p=localinfo.setenv; *p; p++)
	    (void) printf("\t\t%s\n", *p);
    }
    (void) fprintf(stderr,
		    "\tFile descriptors not to be closed:\n\t\t0,1,2");
    if (localinfo.fdlist)
	(void) fprintf(stderr, ",%s", localinfo.fdlist);
    (void) fprintf(stderr, "\n\n\tID's:\treal effective\n");
    (void) fprintf(stderr, "\tuid\t%d\t%d\n", getuid(), geteuid());
    (void) fprintf(stderr, "\tgid\t%d\t%d\n", getgid(), getegid());
#ifdef HAVE_GETGROUPS
    {
	GETGROUPS_T groups[NGROUPS_MAX];
	int ng = Getgroups(NGROUPS_MAX, groups);
	(void) fprintf(stderr, "\tSupplementary groups list:\n\t\t");
	if (ng == 0) {
	    (void) fprintf(stderr, "(none)");
	} else {
	    for (j=0; j<ng; j++)
		(void) fprintf(stderr, "%d ", (int) groups[j]);
	}
	(void) fputc('\n', stderr);
    }
#endif

    (void) fprintf(stderr, "\n\tPassword required: %s\n",
			    localinfo.passinfo.required ? "yes" : "no");
    if (localinfo.passinfo.required) {
	(void) fprintf(stderr, "\tPassword timeout: %d min\n",
				localinfo.passinfo.timeout);
	(void) fprintf(stderr,
    "\tUpdate timestamp with each use of a password-requiring command? %s\n",
				localinfo.passinfo.renewtime ? "Yes" : "No");
    }

}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Switch to processing a user's super file, and set uid/gid/groups
 * to that user.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
user_supertab(command)
char *command;		/* user:cmd */
{
    char *s, *user, *cmd;
    struct passwd *pass;
 
    /* Split command into user:cmd */
    if ((cmd = strchr(command, ':')) == NULL)
	return 0;		/* No ":" -- no user superfile */

    user = command;
    *cmd++ = '\0';
    if (*user == '\0'  ||  *cmd == '\0')
	return Error(0, 0, "Commands may not begin or end with `:'\n");

    superfile[0] = '\0';
    pass = getpwnam(user);
    if (pass == (struct passwd *) NULL)
	return Error(0, 0, "No such user as `%s' (from command <%s:%s>)\n",
			user, user, cmd);
 
    if (pass->pw_dir[0] == '\0')
	return Error(0, 0, "No home directory for user `%s'?!\n", user);
    (void) strcpy(superfile, pass->pw_dir);
    s = superfile + strlen(superfile) - 1;
    if (*s != '/')
	*(++s) = '/';
    strcpy(s+1, PERUSER_SUPERFILE);
    error_srcfile = superfile;

    if (initgroups(user, pass->pw_gid) == -1)
	return Error(0, 0,
	    "Couldn't set groups to those of user %s (from command <%s:%s>).\n",
	    user, user, cmd);

    if (setgid(pass->pw_gid) == -1)
	return Error(1, 0,
	    "Couldn't set gid to that of user %s (from command <%s:%s>): ",
	    user, user, cmd);

    if (setuid(pass->pw_uid) == -1)
	return Error(1, 0,
	    "Couldn't set uid to user %s (from command <%s:%s>): ",
	    user, user, cmd);

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Log a super call -- If "args" isn't a null ptr, it's printed inside
 *		parentheses, with whitespace separating the arguments.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
logmsg(cmd, args)
char *cmd;
char **args;
{
    char *ec, *logbuf, **ap;
    int e;
    int loglen = strlen(cmd) + 4;

    /* Determine buffer length needed to hold all arguments */
    if (args)
	for (ap = args; *ap; ap++)
	    loglen += strlen(*ap) + 1;

    if (!(logbuf = malloc(loglen)))
	(void) Error(0, 2, "failed to malloc space for logging command\n");

    if (args) {
	sprintf(logbuf, "%s (", cmd);
	for (ap = args; *ap; ) {
	    strcat(logbuf, *ap++);
	    strcat(logbuf, " ");
	}
	logbuf[loglen-3] = ')';
	logbuf[loglen-2] = '\n';
	logbuf[loglen-1] = '\0';
    } else {
	strcpy(logbuf, cmd);
    }

    /* Log the message using Error(), but
     *	- make sure msg doesn't go to stderr;
     *	- if not mail_success, don't let msg go to error_command, either.
     */
    e = error_stderr;
    ec = error_command;
    if (localinfo.mail_success == 0 ||
	    (localinfo.mail_success == -1 && globalinfo.mail_success==0))
	error_command = NULL;
    error_stderr = 0;
    Error(0, 0, logbuf);
    error_stderr = e;
    error_command = ec;
} 

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Get the arglist for the command, and null-terminate cmd if nec */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char **
newargs(path_plus, argv, n_builtin)
char *path_plus;	/* string = "path [options]".  Null-terminate and put
			 * options into front of arglist. */
char **argv;		/* rest of arguments for arglist (placed after
			 * the options from the path_plus string).
			 */
int *n_builtin;		/* returned w/ number of args in path_plus */
{
    int nuser, nalloc, iarg, nargs, nxtra;
    char **arglist, **ap;
    char *s;

    /* Count user-entered args. */
    for (ap = argv; *ap; )
	ap++;

    /* Check number of user-entered args is ok. */
    nargs = ap - argv - 1;
    if (localinfo.usr_args[0] >= 0) {
	if (nargs < localinfo.usr_args[0] || nargs > localinfo.usr_args[1]) {
	    if (localinfo.usr_args[0] == localinfo.usr_args[1] &&
						localinfo.usr_args[0] == 0)
		    (void) Error(0, 2,
			"you may not give any arguments to `%-.500s'\n",
			argv[0]);
	    else if (localinfo.usr_args[0] == localinfo.usr_args[1])
		(void) Error(0, 2,
		    "You must give %d argument%s to `%-.500s'\n",
		    localinfo.usr_args[0],
		    localinfo.usr_args[0] == 1 ? "" : "s", argv[0]);
	    else
		(void) Error(0, 2,
		    "You must give %d - %d arguments to `%-.500s'\n",
			localinfo.usr_args[0], localinfo.usr_args[1], argv[0]);
	}
    }

    /* Check that each user-entered argument matches its pattern, if given */
    for (iarg = 1; iarg <= nargs; iarg++) {
	s = StrEltGetPtr(&localinfo.argpats, iarg);
	if (s && match_pattern(0, 0, argv[iarg], s) != 1)
	    (void) Error(0, 2,
		"Your argument #%d <%s> must match pattern <%s>\n",
		iarg, argv[iarg], s);
    }

    /* Start off with space for user-entered args + 100 args in super.tab.
     * We'll re-alloc if necessary.
     */
    nuser = (ap - argv) + 3;
    nalloc = nuser + 100;
    arglist = (char **) malloc(sizeof(char *) * nalloc);
    if (!arglist)
	(void) Error(1, 2, 
	    "failed to malloc space for %d ptrs\n", nalloc);

    /* Leave room for two extra args at front, in case we are handling
     * the "#! interpreter [opt]" file for OS's that don't support it.
     */
    arglist += 2;

    /* First copy the command to the new arglist */
    arglist[0] = *argv++;

    /* Then copy the extra args from super.tab to the arglist,
     * re-allocing the arglist as the number of args grows.
     */
    s=strqtokS(path_plus, SEP, QM, "", 1); /* Don't put FullPath into arglist */
    for(nxtra=0, ap = &arglist[1], s=strqtokS(NULL, SEP, NULL, NULL, 1);  s;
				s = strqtokS(NULL, SEP, NULL, NULL, 1)) {
	nxtra++;
	if (nuser + nxtra >= nalloc) {
	    char **newarglist;
	    nalloc *= 2;
	    newarglist = (char **) realloc((void *) arglist, nalloc);
	    if (!newarglist)
		(void) Error(1, 2, 
		    "failed to realloc space for %d ptrs\n", nalloc);
	    ap = newarglist + (ap - arglist);
	    arglist = newarglist;
	}
	*ap++ = s;
    }

    /* Now add the user-supplied args at the end */
    *n_builtin = ap - arglist - 1;
    while (*argv)
	*ap++ = *argv++;
    *ap = NULL;

    return arglist;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Get a safe environment for execution of the command */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char **
buttonup(cmd)
char *cmd;		/* name of command being started */
{
    /* Depending on the ability to close-on-exec, either:
     *	close all descriptors save 0,1,2 and the super.tab-specified fd list;
     *	or mark them close-on-exec.
     * Resets all signal-handling to SIG_DFL.
     * Discards all env. variables save for TERM, LINES, COLUMNS, and
     * any variables listed in the super.tab file.
     * Don't allow TERM to have any but [-/:+._a-zA-Z0-9].
     * Don't allow LINES, COLUMNS to have anything but digits.
     * To these are added reasonable values for IFS, PATH, USER, HOME.
     * USER and HOME refer to the uid under which the command is executed;
     * LOGNAME is set to the same as USER, and SUPERCMD is set to cmd.
     * ORIG_USER, ORIG_LOGNAME, and ORIG_HOME refer to the user who invoked
     * super.
     * Returned:
     *  NULL on error;
     *	otherwise, a pointer to the modified environment list.
     */
    int i, fd, maxfd;
    char **p, *s;

    int fd_log;
    static char *env[200];
    static char User[100];		/* USER */
    static char Logname[100];		/* LOGNAME (alias for USER) */
    static char Home[MAXPATHLEN+5];	/* HOME */
    static char OrigUser[100];		/* ORIG_USER */
    static char OrigLogname[100];	/* ORIG_LOGNAME */
    static char OrigHome[MAXPATHLEN+9];	/* ORIG_HOME */
    static char Cmd[1200];		/* SUPERCMD */
    SIGNAL_T (*signal())();

    /* don't close logfile yet */
    fd_log = globalinfo.log.fp ? fileno(globalinfo.log.fp) : -1;
    maxfd = MAXFD;

#ifdef HAVE_FCNTL_H
#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif
    for (fd=3; fd <= maxfd; fd++)
	if (localinfo.fd[fd] == 0 && fd != fd_log)
	    (void) fcntl(fd, F_SETFD, FD_CLOEXEC);
#else
#ifdef HAVE_IOCTL_FIOCLEX
    for (fd=3; fd <= maxfd; fd++)
	if (localinfo.fd[fd] == 0 && fd != fd_log)
	    (void) ioctl(fd, FIOCLEX, NULL);
#else
    for (fd=3; fd <= maxfd; fd++)
	if (localinfo.fd[fd] == 0 && fd != fd_log)
	    (void) close(fd);
#endif
#endif
    
    for (i=0; i<NSIG; i++)
       (void) signal(i, SIG_DFL);

    s = *localinfo.user ? localinfo.user :
		*localinfo.u_g ? localinfo.u_g : userinfo.caller.pw_name;
    (void) sprintf(OrigUser, "ORIG_USER=%s", userinfo.caller.pw_name);
    (void) sprintf(User, "USER=%s", s);
    (void) sprintf(OrigLogname, "ORIG_LOGNAME=%s", userinfo.caller.pw_name);
    (void) sprintf(Logname, "LOGNAME=%s", s);
    (void) sprintf(Cmd, "SUPERCMD=%s", cmd);
    (void) strcpy(Home, "HOME=");
    (void) getlogdir(s, Home+5);
    (void) sprintf(OrigHome, "ORIG_HOME=%s", userinfo.caller.pw_dir);
    i = 0;
    env[i] = Getenv("TERM");
    if (env[i] && checkenv("TERM", env[i]+5, "^[-/:+._a-zA-Z0-9]*$") != -1) i++;
    env[i] = Getenv("LINES");
    if (env[i] && checkenv("LINES", env[i]+6, "^[0-9]*$") != -1) i++;
    env[i] = Getenv("COLUMNS");
    if (env[i] && checkenv("COLUMNS", env[i]+8, "^[0-9]*$") != -1) i++;
    env[i++] = SAFE_IFS;
    env[i++] = SAFE_PATH;
    env[i++] = User;
    env[i++] = Logname;
    env[i++] = Home;
    env[i++] = Cmd;
    env[i++] = OrigUser;
    env[i++] = OrigLogname;
    env[i++] = OrigHome;

    /* Now add the extra environment variables requested in the
     * super.tab file.
     */
    for (p=localinfo.env; p && *p && i < NELEM(env)-1; p++) {
	env[i] = Getenv(*p);
	if (env[i])
	    i++;
    }

    for (p = localinfo.setenv; *p && i < NELEM(env)-1 ; )
	env[i++] = *p++;

    if (i >= NELEM(env)-1) {
	Error(0, 0, "%t\n\tAsked to save too many \
environment variables (max allowed %d).\n", NELEM(env)-1);
	return NULL;
    }

    env[i] = (char *) NULL;

    return &env[0];
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Initialize the globalinfo struct.  Only call this once; thereafter, use
 * option_global_clear_settings().  Note that very little work is done in
 * this routine, because almost all fields are properly initialized in the
 * structure declaration/initialization statement.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
init_globalinfo()
{
    init_umask(1);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Initialize the localinfo struct.  Only call this once; thereafter, use
 * option_local_clear_settings().
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
init_localinfo()
{
    int i;
    localinfo.info = localinfo.chdir_path =
	    localinfo.die = localinfo.print = NULL;
    localinfo.env = NULL;
    for (i=0; i<=MAXSETENV; i++)
	localinfo.setenv[i]=NULL;
    localinfo.fdlist = NULL;
    localinfo.mask = -1;
    localinfo.file_uid = UID_NOTSET;
    localinfo.file_gid = GID_NOTSET;
    localinfo.nice_incr = globalinfo.nice_incr;
    /* Don't init to global ngroups, because we need to be able to tell
     * later on whether local groups=xxx or global groups=xxx was used.
     */
    localinfo.ngroups = GROUPS_NOTSET;
    localinfo.groups_added = 0;
    StrInit(&localinfo.argpats);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Look up some of the most basic user information. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
init_userinfo()
{
    struct passwd *usrpw;
    char *s;

    userinfo.ourtime.start = time(NULL);
#ifdef HAVE_LOCALTIME
    {
	struct tm *tm_p = localtime(&userinfo.ourtime.start);
	userinfo.ourtime.min = tm_p->tm_hour*60 + tm_p->tm_min;
	userinfo.ourtime.day = tm_p->tm_wday;
    }
#else
    userinfo.ourtime.min = (userinfo.ourtime.start/60) % (24*60);
    userinfo.ourtime.day = daynum(userinfo.ourtime.start);
#endif

    /*
     * We want the hostname (fully-qualified if possible), as well as a
     * lower-cased version (to try and deal with mixed case hostnames).
     */
    if (get_canonical_hostname(userinfo.hostname, sizeof(userinfo.hostname))
	    == -1)
	return 1;
    strcpy(userinfo.lc_hostname, userinfo.hostname);
    strtolower(userinfo.lc_hostname);
    usrpw = getpwuid(getuid());
    if (!usrpw)
	return Error(0, 0, "Couldn't get your password entry.");
    memcpy(&userinfo.caller, (void *) usrpw, sizeof(struct passwd));

    /* Since the string fields that we need are overwritten by later
     * calls to getpwxxx(), make private copies:
     */
    if (!(s = malloc(strlen(userinfo.caller.pw_name)+1)))
	(void) Error(0, 2, "failed to malloc space passwd struct field.\n");
    strcpy(s, userinfo.caller.pw_name);
    userinfo.caller.pw_name = s;
    if (!(s = malloc(strlen(userinfo.caller.pw_passwd)+1)))
	(void) Error(0, 2, "failed to malloc space passwd struct field.\n");
    strcpy(s, userinfo.caller.pw_passwd);
    userinfo.caller.pw_passwd = s;
    if (!(s = malloc(strlen(userinfo.caller.pw_dir)+1)))
	(void) Error(0, 2, "failed to malloc space passwd struct field.\n");
    strcpy(s, userinfo.caller.pw_dir);
    userinfo.caller.pw_dir = s;
    
    error_user = userinfo.caller.pw_name;
    userinfo.orig_mask = umask(022); /* Get orig umask, and set to 022 */
    (void) umask(userinfo.orig_mask);	/* ...so restore curr val */

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Store the desired umask; set the actual umask; recall the desired umask */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
init_umask(is_global)
int is_global;
{
    if (is_global) {
	globalinfo.mask = userinfo.orig_mask;
    } else {
	localinfo.mask = -1;
    }
}

void
store_umask(mask, is_global)
int mask;
int is_global;
{
    if (is_global)
	globalinfo.mask = mask;
    else
	localinfo.mask = mask;
}

void
set_umask()
{
    umask( (localinfo.mask >= 0) ? localinfo.mask : globalinfo.mask);
}

int
rcl_umask()
{
    return (localinfo.mask >= 0) ? localinfo.mask : globalinfo.mask;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Change directory, if required */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
set_chdir()
{
    char *path = localinfo.chdir_path ?
			localinfo.chdir_path : globalinfo.chdir_path;

    if (!path || !*path)
	return 0;

    if (chdir(path) == -1)
	return Error(1, 0, "Failed to change directory to ``%s'': ", path);
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Store/set/recall niceness */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
store_nice_incr(nice_incr, is_global)
int nice_incr;
int is_global;
{
    if (is_global)
	globalinfo.nice_incr = nice_incr;
    else
	localinfo.nice_incr = nice_incr;
}

int
set_nice_incr()
{
    if (localinfo.nice_incr && nice(localinfo.nice_incr) == -1)
	return Error(1, 0,
		"Failed to apply a ``nice'' increment = %d: ",
		    localinfo.nice_incr);
    return 0;
}

int
rcl_nice_incr()
{
    return localinfo.nice_incr;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Frees all elements in a SimpleList, except the one it's given.
 * The "next" field of that element is set NULL.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
free_SimpleList(sl)
SimpleList *sl;
{
    SimpleList *slp;

    if (!sl || !sl->next)
	return;
    slp = sl->next;
    sl->next = NULL;
    for (sl=sl->next ; sl; sl = slp) {
	slp = sl->next;
	free(sl->pat);
	free(sl);
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Frees all elements in a Simple2List, except the one it's given.
 * The "next" field of that element is set NULL.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
free_Simple2List(sl)
Simple2List *sl;
{
    Simple2List *slp;

    if (!sl || !sl->next)
	return;
    slp = sl->next;
    sl->next = NULL;
    for (sl=sl->next ; sl; sl = slp) {
	slp = sl->next;
	free(sl->pat);
	free(sl);
    }
}
