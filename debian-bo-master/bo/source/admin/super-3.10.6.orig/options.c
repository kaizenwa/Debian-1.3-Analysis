/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process a global xxx=yyy option */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "super.h"
#include "options.h"

/* KEEP SORTED IN INCREASING ORDER! */

Option opts[] = {
	{ "DIE",		LOCAL,		option_die },	/* OBSOLETE */
	{ "addgroups",		LIST|GLOBAL|LOCAL,	option_groups },
	{ "arg",		IS_ROOT|LOCAL,	option_arg },
	{ "cd",			GLOBAL|LOCAL,	option_cd },
	{ "die",		LOCAL,		option_die },
	{ "env",		LIST|LOCAL,	option_env },
	{ "fd",			LIST|LOCAL,	option_fd },
	{ "gethostbyname",	GLOBAL,		option_gethostbyname },
	{ "gid",		LOCAL,		option_gid },
	{ "group_slash",	GLOBAL,		option_group_slash },
	{ "groups",		LIST|GLOBAL|LOCAL,	option_groups },
	{ "info",		LOCAL,		option_info },
	{ "logfile",		GLOBAL,		option_logfile },
	{ "loguid",		GLOBAL,		option_loguid },
	{ "mail",		GLOBAL|LOCAL,	option_mail },
	{ "mailany",		GLOBAL|LOCAL,	option_mailany },
	{ "nargs",		LOCAL,		option_nargs },
	{ "nice",		GLOBAL|LOCAL,	option_nice },
	{ "owner",		GLOBAL|LOCAL,	option_owner },
	{ "password",		GLOBAL|LOCAL,	option_password },
	{ "patterns",		GLOBAL,		option_patterns },
	{ "print",		LOCAL,		option_print },
	{ "relative_path",	GLOBAL,		option_relative_path },
	{ "renewtime",		GLOBAL,		option_renewtime },
	{ "rlog_host",		GLOBAL,		option_rlog_host },
	{ "setenv",		LOCAL,		option_setenv },
	{ "syslog",		GLOBAL,		option_syslog },
	{ "timeout",		GLOBAL|LOCAL,	option_timeout },
	{ "timestampbyhost",	GLOBAL,		option_timestampbyhost },
	{ "timestampuid",	GLOBAL,		option_timestampuid },
	{ "u+g",		LOCAL,		option_u_g },
	{ "uid",		LOCAL,		option_uid },
	{ "umask",		GLOBAL|LOCAL,	option_umask },
	{ "",			0,		NULL },
};

/* OK, so it's a really stupid hack, but it does let me have all option_xxx()
 * functions take the same argument lists; those that are LIST types, and
 * need an extra list argument passed, can pick it up here.
 */
static char **optionlist;

void
sort_optlist()
{
    /* Sort the options list.  This is just an insertion sort, but
     * it'll work well for almost-sorted data.
     */
    int i, j;
    int N = NELEM(opts)-1;	/* use NELEM-1 because the last element is
				 * the empty end-of-list marker.
				 */

    for (i=1; i < N; i++) {
	for (j=i; j && strcmp(opts[j-1].name, opts[j].name) > 0; j--) {
	    Option o;
	    /* Swap elements; use memcpy in case struct assignment isn't
	     * allowed by this compiler.
	     */
	    memcpy((void *) &o,         (void *) &opts[j-1], sizeof(opts[0]));
	    memcpy((void *) &opts[j-1], (void *) &opts[j],   sizeof(opts[0]));
	    memcpy((void *) &opts[j],   (void *) &o,         sizeof(opts[0]));
	}
    }

}

/* binary search in options list for option word. */

Option *
find_option(word)
char *word;
{
    int i, l;
    static int firsttime=1;
    Option *lower = &opts[0];
    Option *upper = &opts[NELEM(opts)-1];	/* 1 past last non-empty elt */
    Option *mid;

    if (firsttime) {
	sort_optlist();
	firsttime=0;
    }
    
    while (lower < upper) {
	mid = lower + (upper - lower) / 2;
	l = strlen(mid->name);
	if ((i=strncmp(word, mid->name, l)) < 0) {
	    upper = mid;
	} else if (i > 0) {
	    lower = mid + 1;
	} else if (mid->flags & IS_ROOT) {
	    return mid;	/* Don't care if it's followed by <SEP> */
	} else if (*(word+l) == OPTION_SEP) {
	    return mid;	/* Must be followed by <SEP> */
	} else {
	    /* It didn't compare right... word is longer, hence word > mid... */
	    lower = mid + 1;
	}
    }
    return NULL;
}

int
handle_option(word, s, isglobal)
char *word;	/* The input xxx<SEP>yyy string; NULL means clear settings */
char *s;	/* pts to yyy in word; can be NULL if wd is NULL */
int isglobal;	/* bool: are we processing a global or local entry? */
{

    Option *opt;

    if (debug>1)
	fprintf(stderr, "\thandle_option(): word=<%s> s=<%s>; is %s\n",
				word, s, isglobal ? "global" : "local");
    
    if (!word)
	return option_clear_settings(word, NULL, isglobal);

    if (*word == '!')
	return Error(0, 0, "%t\n\tsuper.tab syntax error: \n\
options cannot be negated: <%s>\n", word);

    opt = find_option(word);

    if (opt) {
	if (isglobal && !(opt->flags & GLOBAL))
	    return Error(0, 0,
		    "%t\n\t`%s' may not be used as a global option\n", word);
	if (!isglobal && !(opt->flags & LOCAL))
	    return Error(0, 0,
		    "%t\n\t`%s' may not be used as a local option\n", word);

	if (opt->flags & LIST) {
	    /* Brace-expand arg list and pass */
	    char **globlist;
	    int i;

	    if ((i=globbraces(s, 1, &globlist)) != 0)	/* Option argument */
		return Error(0, 0, "%tMissing `%c'.\n", i);
				 
	    optionlist = globlist;
	}
	return opt->process(word, s, isglobal);

    } else {
	if (s-word == 5 && strncmp(word, "time", 4) == 0) {
	    return Error(0, 0, "%t\n\tUnrecognized %s option `%s'.\n\
\tPerhaps you meant to use the condition `time~%s'?\n",
			isglobal ? "global" : "local", word, s);
	} else {
	    return Error(0, 0,
			"%t\n\tUnrecognized %s option `%s'.\n",
			isglobal ? "global" : "local", word);
	}
    }
    /* NOTREACHED */
    return 0;	/* Unreachable statement.  Including this shuts up a warning
		 * from some compilers, but generates a warning from others.
		 */
}

int
option_clear_settings(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug>1) fprintf(stderr,
			"\toption_clear_settings(\"%s\", \"%s\", %d)\n",
				word, s, isglobal);
    if (isglobal)
	return option_global_reset_settings();
    else
	return option_local_clear_settings();
    /* NOTREACHED */
}

int
option_global_reset_settings()
{
    /* reset global user/group/host patterns pointers, so that more names
     * form a new list, and are not appended to the old one.
     * Similarly for time~pattern pointers.
     * *** Note that we don't free any pattern space!
     * *** We assume that there is plenty of memory in the computer
     * *** to slurp up all _global_ patterns.
     */

    if (debug>1) fprintf(stderr, "\toption_global_reset_settings()\n");

    globalinfo.user_clear = 1;
    globalinfo.time_clear = 1;
    return 0;
}

int
option_local_clear_settings()
{
    /* Clear local settings */
    int i;
    int maxfd = MAXFD;

    if (debug>1) fprintf(stderr, "\toption_local_clear_settings()\n");

    maxfd = MAXFD;
    if (localinfo.info)
	free(localinfo.info);
    localinfo.info = NULL;
    if (localinfo.die)
	free(localinfo.die);
    localinfo.die = NULL;
    if (localinfo.print)
	free(localinfo.print);
    localinfo.print = NULL;
    if (localinfo.chdir_path)
	free(localinfo.chdir_path);
    localinfo.chdir_path = NULL;
    *localinfo.user = *localinfo.group = *localinfo.u_g = '\0';
    if (localinfo.env)
	blkfree(localinfo.env);
    localinfo.env = NULL;
    localinfo.setenv[0] = NULL;
    for (i=0; i<=MAXSETENV; i++) {
	if (localinfo.setenv[i])
	    free(localinfo.setenv[i]);
	localinfo.setenv[i]=NULL;
    }
    strcpy(localinfo.owner, globalinfo.owner);
    if (localinfo.fdlist)
	free(localinfo.fdlist);
    localinfo.fdlist = NULL;
    if (!localinfo.fd) {
	localinfo.fd = (int *) malloc(sizeof(int) * (maxfd + 1));
	if (!localinfo.fd)
	    return Error(1, 0,
		"%t\n\tFailed to malloc space for file descriptor list: ");
    }
    for (i=0; i<=maxfd; i++) {
	localinfo.fd[i] = 0;
    }
    localinfo.mailcmd[0] = '\0';
    localinfo.mail_success = -1;
    /* Don't let local groups default to global groups, because then we
     * can't tell if local groups were assigned, and we want to set priority
     * to be  (1) local groups=xxx; (2) local u+g=xxx; (3) global groups=xxx.
     */
    localinfo.ngroups = GROUPS_NOTSET;
    localinfo.groups_added = 0;
    error_command = globalinfo.mailcmd[0] ? globalinfo.mailcmd : NULL;
    init_umask(0);
    localinfo.file_uid = UID_NOTSET;
    localinfo.file_gid = GID_NOTSET;
    localinfo.nice_incr = globalinfo.nice_incr;
    free_TimeList(&localinfo.time);
    free_Simple2List(&localinfo.userpats);
    free_SimpleList(&localinfo.origtext);

    localinfo.usr_args[0] = localinfo.usr_args[1] = -1;
    StrEltsUnused(&localinfo.argpats);
    /* should use struct assignment, but that's not portable to
     * all k&r compilers.
     */
    localinfo.passinfo.required = globalinfo.passinfo.required;
    localinfo.passinfo.timeout = globalinfo.passinfo.timeout;
    localinfo.passinfo.renewtime = globalinfo.passinfo.renewtime;
    localinfo.passinfo.perhost = globalinfo.passinfo.perhost;
    strcpy(localinfo.passinfo.user, globalinfo.passinfo.user);

    return 0;
}

/***************************************************************************/
/*                                                                         */
/***************************************************************************/

static int
option_arg(word, pat, isglobal)
char *word;
char *pat;
int isglobal;
{
    /* Must be argMMM-NNN<SEP>SSS or argNNN<SEP>SSS */
    int i, iarg1, iarg2;
    char eq;

    if (debug)
	fprintf(stderr, "\toption:looks like argMMM-NNN=%s (%s)...\n",
				pat, isglobal ? "global" : "local");

    if (*pat == '\0')
	return Error(0, 0,
		"%t\n\tImproper use of argNNN%cXXX option\n", OPTION_SEP);

    /* Check that word matches argNNN=SSS */
    i = sscanf(word, "arg%d-%d%c", &iarg1, &iarg2, &eq);
    if (i != 3 || eq != OPTION_SEP) {
	i = sscanf(word, "arg%d%c", &iarg1, &eq);
	if (i == 2)
	    i = 3;
	iarg2 = iarg1;
    }
    if (debug)
	fprintf(stderr, "\toption:is arg%d-%d=%s (%s)\n",
			iarg1, iarg2, pat, isglobal ? "global" : "local");

    if (i != 3 || eq != OPTION_SEP || iarg1 <= 0 || iarg2 < iarg1)
	return Error(0, 0,
		"%t\n\tImproper use of argNNN%cXXX option\n", OPTION_SEP);

    /* Put pattern into the argpat list. */
    {
	char buf[500];
	int iarg;
	if (strlen(pat) > sizeof(buf)-3)
	    return Error(0, 0,
			    "%t\n\tArgument pattern <%s> too long (max %d)\n",
			    pat, sizeof(buf)-3);
	anchor(pat, buf);
	for (iarg = iarg1; iarg <= iarg2; iarg++) {
	    if (StrEltCpy(&localinfo.argpats, iarg, buf) == -1)
		return Error(0, 0,
		    "Failed to malloc space for argument pattern #%d\n", iarg);
	}
    }
    return 0; 
}

static int
option_cd(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:cd=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (isglobal) {
	globalinfo.chdir_path = strdup(s);
	if (!globalinfo.chdir_path)
	    return Error(0, 0,
		"Failed to malloc space for cd=<%s>\n", s);
    } else {
	localinfo.chdir_path = strdup(s);
	if (!localinfo.chdir_path)
	    return Error(0, 0,
		"Failed to malloc space for cd=<%s>\n", s);
    }
    return 0;
}

static int
option_die(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:die=%s (%s)\n",
				s, isglobal ? "global" : "local");

    localinfo.die = strdup(s);
    if (!localinfo.die)
	return Error(0, 0, "%t\n\tfailed to malloc space for die=<%s>\n", s);
    return 0;
}

static int
option_env(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:env=%s (%s)\n",
				s, isglobal ? "global" : "local");

    localinfo.env = optionlist;
    return 0;
}

static int
option_fd(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    int n;
    int iwd;
    char *p, *wd;
    int maxfd = MAXFD;

    if (debug)
	fprintf(stderr, "\toption:fd=%s (%s)\n",
				s, isglobal ? "global" : "local");

    localinfo.fdlist = strdup(s);
    if (!localinfo.fdlist)
	return Error(0, 0,
	    "Failed to malloc space for copy of fd=<%s>\n", s);

    for (iwd=0; (wd=optionlist[iwd]); iwd++) {
	if (((n=strtol(wd, &p, 0)) >= 0) && n <= maxfd && p != wd)
	    localinfo.fd[n] = 1;
	else
	    return Error(0, 0,
		"%t\n\tRidiculous value in file descriptor list: `%s'\n",
		word);
    }
    blkfree(optionlist);
    return 0;
}

static int
option_gid(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:gid=%s (%s)\n",
				s, isglobal ? "global" : "local");

    stringcopy(localinfo.group, s, sizeof(localinfo.group));
    return 0;
}

static int
option_group_slash(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:group_slash=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "n") == 0) {
	globalinfo.group_slash = 0;
    } else {
	globalinfo.group_slash = 1;
	if (strcmp(s, "y") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value \
in `%s' -- value must be `y' or `n'\n", word);
    }
    return 0;
}

#ifndef HAVE_GETGROUPS
static int
option_groups(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:%s=%s (%s)\n",
				word, s, isglobal ? "global" : "local");

    return Error(0, 0, "Option %s=  not supported on this machine\n", word);
}

#else

static int
option_groups(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    int i, is_addgroups, *ngroups_p;
    int iwd;
    char *wd;
    GETGROUPS_T *group_p;

    if (debug)
	fprintf(stderr, "\toption:%s=%s (%s)\n",
				word, s, isglobal ? "global" : "local");

    if (strcmp(word, "groups") == 0) {
	is_addgroups = 0;
    } else if (strcmp(word, "addgroups") != 0) {
	is_addgroups = 1;
    } else {
	return Error(0, 0, "%tInternal error:\n\
\tDon't know what to do when option isn't groups or addgroups!\n");
    }

    if (isglobal) {
	group_p = globalinfo.groups;
	ngroups_p = &globalinfo.ngroups;
	globalinfo.groups_added = is_addgroups;
    } else {
	group_p = localinfo.groups;
	ngroups_p = &localinfo.ngroups;
	localinfo.groups_added = is_addgroups;
    }
    if (*ngroups_p != GROUPS_NOTSET)
	return Error(0, 0, "%t\n\t\tCan't have multiple addgroups=xxx and/or \
groups=xxx options in one entry\n");

    for (*ngroups_p = 0, iwd=0; (wd=optionlist[iwd]); iwd++) {
	i = findgid(wd);
	if (i == -1) {
	    *ngroups_p = 0;
	    return Error(0, 0,
	    "%t\n\tCan't set gid: no such group as `%s' in group file.\n", wd);

	} else {
	    *group_p++ = (GETGROUPS_T) i;
	    (*ngroups_p)++;
	}
    }
    blkfree(optionlist);
    return (wd) ? Error(0, 0, "%t\n\ttoo many supplementary \
groups (max=%d) in option %s=...", word) : 0;
}
#endif

static int
option_info(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:info=%s (%s)\n",
				s, isglobal ? "global" : "local");

    localinfo.info = strdup(s);
    if (!localinfo.info)
	return Error(0, 0,
	    "Failed to malloc space for info=<%s>\n", s);
    return 0;
}

static int
option_logfile(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:logfile=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* Don't do any logging if just checking syntax! */
    if (check_syntax)
	return 0;

    strcpy(globalinfo.log.filename, s);
    globalinfo.log.newfile = 1;
    return 0;
}

static int
option_loguid(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:loguid=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* Don't do any logging if just checking syntax! */
    if (check_syntax)
	return 0;

    strcpy(globalinfo.log.user, s);
    globalinfo.log.newuid = 1;
    return 0;
}

static int
option_mail(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:mail=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* Don't do any logging if just checking syntax! */
    if (check_syntax)
	return 0;

    if (isglobal) {
	if (strlen(s) > sizeof(globalinfo.mailcmd)-1)
	    return Error(0, 0, "%t\n\tRidiculous length of string: `%s'.\n\
\tMaximum permitted length is %d; the entry used %d\n",
		word, sizeof(globalinfo.mailcmd)-1, strlen(s));

	strcpy(globalinfo.mailcmd, s);
	error_command = globalinfo.mailcmd;
	globalinfo.mail_success = 0;
    } else {
	if (strlen(s) > sizeof(localinfo.mailcmd)-1)
	    return Error(0, 0, "%t\n\tRidiculous length of string: `%s'.\n\
\tMaximum permitted length is %d; the entry used %d\n",
		word, sizeof(localinfo.mailcmd)-1, strlen(s));

	strcpy(localinfo.mailcmd, s);
	error_command = localinfo.mailcmd;
	localinfo.mail_success = 0;
    }
    return 0;
}

static int
option_mailany(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:mailany=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* Don't do any logging if just checking syntax! */
    if (check_syntax)
	return 0;

    if (isglobal) {
	if (strlen(s) > sizeof(globalinfo.mailcmd)-1)
	    return Error(0, 0, "%t\n\tRidiculous length of string: `%s'.\n\
\tMaximum permitted length is %d; the entry used %d\n",
		word, sizeof(globalinfo.mailcmd)-1, strlen(s));

	strcpy(globalinfo.mailcmd, s);
	error_command = globalinfo.mailcmd;
	globalinfo.mail_success = 1;
    } else {
	if (strlen(s) > sizeof(localinfo.mailcmd)-1)
	    return Error(0, 0, "%t\n\tRidiculous length of string: `%s'.\n\
\tMaximum permitted length is %d; the entry used %d\n",
		word, sizeof(localinfo.mailcmd)-1, strlen(s));

	strcpy(localinfo.mailcmd, s);
	error_command = localinfo.mailcmd;
	localinfo.mail_success = 1;
    }
    return 0;
}

static int
option_nargs(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    int i, m, n;

    if (debug)
	fprintf(stderr, "\toption:nargs=%s (%s)\n",
				s, isglobal ? "global" : "local");

    i = sscanf(s, "%u-%u", &m, &n);
    switch(i) {
    case 1:     localinfo.usr_args[0] = localinfo.usr_args[1] = m;
		break;
    case 2:     localinfo.usr_args[0] = m;
		localinfo.usr_args[1] = n;
		break;
    default:
	return Error(0, 0,
		"%t\n\tnargs must be nargs=nnn or  nargs=mmm-nnn.\n", word);
    }
    return 0; 
}

static int
option_nice(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    int nice_incr;
    char *p;
    
    if (debug)
	fprintf(stderr, "\toption:nice=%s (%s)\n",
				s, isglobal ? "global" : "local");

    nice_incr = strtol(s, &p, 0);
    if (p == word)
	return Error(0, 0, "%t\n\tillegal value in `nice=%s'\n", word);
    store_nice_incr(nice_incr, isglobal);
    return 0;
}

static int
option_owner(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:owner=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (isglobal)
	stringcopy(globalinfo.owner, s, sizeof(globalinfo.owner));
    else
        stringcopy(localinfo.owner, s, sizeof(localinfo.owner));
    return 0; 
}

static int
option_password(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    int required;

    if (debug)
	fprintf(stderr, "\toption:password=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "n") == 0) {
	required = 0;
    } else {
	required = 1;
	if (strcmp(s, "y") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value \
in `%s' -- value must be `y' or `n'\n", word);
    }

    if (isglobal)
	globalinfo.passinfo.required = required;
    else
	localinfo.passinfo.required = required;

    return 0;
}

static int
option_patterns(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:patterns=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "shell") == 0) {
	pat_compile = shell_compile;
	pat_compare = shell_compare;
	need_re_anchor = 0;
    } else if (strcmp(s, "regex") == 0) {
	pat_compile = s_re_comp;
	pat_compare = s_re_exec;
	need_re_anchor = 1;
    } else {
	return Error(0, 0, "%t\n\tInvalid pattern type `%s'.  \
Valid: \"shell\" and \"regex\"\n", s);
    }
    return 0;
}

static int
option_print(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:print=%s (%s)\n",
				s, isglobal ? "global" : "local");

    localinfo.print = strdup(s);
    if (!localinfo.print)
	return Error(0, 0, "%t\n\tfailed to malloc space for print=<%s>\n", s);
    return 0;
}

static int
option_relative_path(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:relative_path=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "n") == 0) {
	globalinfo.relative_path = 0;
    } else {
	globalinfo.relative_path = 1;
	if (strcmp(s, "y") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value \
in `%s' -- value must be `y' or `n'\n", word);
    }
    return 0;
}

static int
option_renewtime(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:renewtime=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "y") == 0) {
	globalinfo.passinfo.renewtime = 1;
    } else {
	globalinfo.passinfo.renewtime = 0;
	if (strcmp(s, "n") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value \
in `%s' -- value must be `y' or `n'\n", word);
    }
    return 0; 
}

static int
option_rlog_host(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:rlog_host=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (*s == '\0')
	return Error(0, 0, "%t\n\tInvalid option value in `%s' \
-- empty value not allowed.\n", word);

    error_rlog_host = strdup(s);
    return 0;
}

static int
option_setenv(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
   char **p;
    int i;

    if (debug)
	fprintf(stderr, "\toption:setenv=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* s must have form v=xxx, where v is any string not including `='. */
    if (*s == '=' || !strchr(s+1, '='))
	return Error(0, 0,
	"%t\n\tbad syntax for setenv%cvar=xxx; you used `setenv%c%s'.\n",
	OPTION_SEP, OPTION_SEP, s);

    /* Append this variable: first skip past already-set variables... */
    for (i=0, p=localinfo.setenv; p[i] && i < MAXSETENV; i++)
	;
    /* ...then add this definition */
    if (i == MAXSETENV) {
	return Error(0, 0,
		"%t\n\ttoo many setenv%cvar=xxx entries; max is %d.\n",
		OPTION_SEP, MAXSETENV);
    } else {
	char *t = strdup(s);
	if (!t)
	    return Error(0, 0,
	    "Failed to malloc space for setenv using <%s>\n", s);
	localinfo.setenv[i++] = t;
	localinfo.setenv[i] = NULL;
    }
    return 0;
}

static int
option_syslog(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:syslog=%s (%s)\n",
				s, isglobal ? "global" : "local");

    /* Don't do any logging if just checking syntax! */
    if (check_syntax)
	return 0;

    if (strcmp(s, "n") == 0) {
	error_syslog = 0;
    } else {
	error_syslog = 1;
	if (strcmp(s, "y") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value in `%s' \
-- value must be `y' or `n'.\n", word);
#ifndef HAVE_SYSLOG
	return Error(0, 0, "%t\n\tCan't enable syslog() calls -- \
not compiled with HAVE_SYSLOG defined.\n");
#endif
    }
    return 0;
}

static int
option_timeout(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:timeout=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (isglobal)
	globalinfo.passinfo.timeout = atoi(s);
    else
	localinfo.passinfo.timeout = atoi(s);

    return 0;
}

static int
option_timestampbyhost(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:timestampbyhost=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "n") == 0) {
	globalinfo.passinfo.perhost = 0;
    } else if (strcmp(s, "y") == 0) {
	globalinfo.passinfo.perhost = 1;
    } else {
	return Error(0, 0,
		"%t\n\tInvalid option value in `%s' -- \
value must be `y' or `n'.\n", word);
    }
    return 0;
}

static int
option_timestampuid(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:timestampuid=%s (%s)\n",
				s, isglobal ? "global" : "local");

    strcpy(globalinfo.passinfo.user, s);
    return 0;
}

static int
option_u_g(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:u+g=%s (%s)\n",
				s, isglobal ? "global" : "local");

    stringcopy(localinfo.u_g, s, sizeof(localinfo.u_g));
    return 0;
}

static int
option_uid(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:uid=%s (%s)\n",
				s, isglobal ? "global" : "local");

    stringcopy(localinfo.user, s, sizeof(localinfo.user));
    return 0;
}

static int
option_umask(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    char *p;
    int mask;
    
    if (debug)
	fprintf(stderr, "\toption:umask=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "<caller>") == 0) {
	mask = userinfo.orig_mask;
    } else if (strcmp(s, "<owner>") == 0) {
	return Error(0, 0,
	    "%t\n\tThe <owner> value isn't allowed in the umask=xxx option\n",
	    word);
    } else {
	mask = strtol(s, &p, 0);
	if (mask < 0 || p == word)
	    return Error(0, 0, "%t\n\tillegal value in `%s'\n", word);
    }

    store_umask(mask, isglobal);
    return 0;
}

static int
option_gethostbyname(word, s, isglobal)
char *word;
char *s;
int isglobal;
{
    if (debug)
	fprintf(stderr, "\toption:gethostbyname=%s (%s)\n",
				s, isglobal ? "global" : "local");

    if (strcmp(s, "n") == 0) {
	globalinfo.gethostbyname = 0;
    } else {
	globalinfo.gethostbyname = 1;
	if (strcmp(s, "y") != 0)
	    return Error(0, 0, "%t\n\tInvalid option value \
in `%s' -- value must be `y' or `n'\n", word);
    }
    return 0;
}

