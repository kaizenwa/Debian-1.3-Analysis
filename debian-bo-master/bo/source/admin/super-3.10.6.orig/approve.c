/* The code should compile with either ANSI C or K&R compilers. */

/*
 *      Copyright (c) 1993 by California Institute of Technology.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

#include "super.h"
#include "version.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Look up command "usrcmd" in file "superfile".  Return path to execute
 * if approved, empty string if no action should be taken, NULL ptr
 * if error.  As a ``side effect'': Sets the fields in the superinfo struct.
 * approve() returns:
 *	- NULL ptr if error:
 *		a) username not found;
 *		b) superfile can't be opened for reading;
 *		c) no such command as usrcmd in superfile;
 *		d) user not allowed to execute this command;
 *		e) invalid superfile contents.
 *	- ptr to empty string if all ok, but no program should be executed.
 *	- ptr to path of file to exec, if user allowed to do so.
 *	Any error also generates a message to stderr.

 * New calls to approve() overwrite the buffer containing the returned path.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
approve(usrcmd, givehelp, giveversion, verbosity)
char *usrcmd;		/* command we're to check on. Should NOT be a
			 * command-line option!  Can be a NULL pointer
			 * or empty string, in which case we give help,
			 * print version info, or whatever.
			 */
int givehelp;		/* Don't execute command; give help instead. */
int giveversion;	/* Don't execute command; give version info instead. */
int verbosity;		/* HELP_BASIC, HELP_FULL, or HELP_FACTS */
{
    char *buf, *bbuf;
    char *do_variables();
    int indentok;

    /* Do we (1) print version info, (2) give help, or
     * (3) match a command with this user?
     */

    if ((givehelp && verbosity != HELP_FACTS) || giveversion)
	(void) printf("%s version %s patchlevel %s\n",
				prog, Version, Patchlevel);
    if (giveversion && !givehelp)
	return "";
    if (givehelp)
	printhelp_hello(verbosity, usrcmd);

    currfile = file_open(currfile, superfile);
    if (!currfile)
	return NULL;

    for (localinfo.progs.commandfound=NULL, currfile->line=1;
	(buf=fillbuffer(currfile->fp, &indentok, &currfile->nl)) ||
	    ( (check_syntax || indentok) && currfile->prev != NULL);
					     currfile->line += currfile->nl) {
	if (!buf) {
	    /* Reached EOF on this file -- close it, continue with prev file */
	    currfile = file_close(currfile);
	    continue;
	}
	error_srcfile = currfile->fullname;
	error_line = currfile->line;
	error_nl = currfile->nl;

	if (!indentok) {
	     Error(0, 0, "%t\n\tformat error in super.tab file:\n\
\tContinued line not indented.\n");
	    if (check_syntax)
		continue;
	    else
		return NULL;
	}


	/* Discard empty lines and pure-comment lines */
	bbuf = buf + strspn(buf, SEP);	/* skip leading whitespace */
	if (!*bbuf || *bbuf == '#')
	    continue;

	if (bbuf != buf) {
	    Error(0, 0, "%t\n\tformat error in super(1) file: \n\
\tNon-comments must begin in column 1.\n");
	    if (check_syntax)
		continue;
	    else
		return NULL;
	}

	/* Clean out embedded comments, whitespace-newline-whitespace. */
	buf = clean_buf(buf, NULL);
	if (!buf)
	    return NULL;
	if (debug)
	    fprintf(stderr, "Input text: %s", buf);
    
	/* Do variable expansion */
	buf = do_variables(buf);
	if (!buf) {
	    if (check_syntax)
		continue;
	    else
		return NULL;
	}

	/* Initialize localinfo */
	if (handle_option(NULL, NULL, 0) == -1)
	    if (check_syntax)
		continue;
	    else
		return NULL;

	if (check_syntax) {
	    parseline(0, 1, buf, usrcmd);

	} else if (givehelp) {
	    switch (parseline(1, 0, buf, usrcmd)) {
	    case -1:
		return NULL;
		break;
	    
	    case 0:
		break;
	    
	    default:
		printhelp(verbosity);
	    }

	} else {
	    switch(parseline(0, 0, buf, usrcmd)) {
	    case -1:
		return NULL;
		break;

	    case 0:
		break;

	    default:
		/* Have an acceptable line, unless die=xxx was used */
		return localinfo.progs.cmd_file[localinfo.progs.match].File;
	    }
	}
    }
    file_close(currfile);

    if (!check_syntax && indentok) {
	return NULL;
    } else if (givehelp || check_syntax) {
	return "";
    } else if (!localinfo.progs.evermatched) {
	Error(0, 0, "No such super command as `%-.500s'.\n", usrcmd);
	return NULL;
    } else {
	Error(0, 0, "%-.500s - Permission denied to user %s\n",
					    usrcmd, userinfo.caller.pw_name);
	return NULL;
    }
    /* NOTREACHED */
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Parse one control line (may contain several text lines).
 * Side effect: prints help information if givehelp is set.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*	Return -1 if caller should give up parsing the file;
 *	return 0 if caller should loop to next chunk (because we
 *		are doing help processing, because of nomatch, because
 *		of ":"-command, etc.);
 *	return 1 if success.
 *	The cmd/file list and which one matched (if any) are returned
 *	through the localinfo.progs struct.
 */
int
parseline(givehelp, checksyntax, buf, usrcmd)
int givehelp;		/* request is to give help, not check valid command.  */
int checksyntax;	/* request is to check syntax of super.tab file,
			 * not check valid command.
			 */
char *buf;		/* Input chunk.  Gets modified by strqtokS(). */
char *usrcmd;		/* Command user wants to execute -- can be
			 * NULL if givehelp_or_checksyntax!=0.
			 */
{
    char *word1;
    char *cond_or_opt_wd;
    char *bbuf;
    int i;
    int condition_match;
    /* FullPath is twice MAXPATHLEN, because this allows room for
     * MAXPATHLEN characters of arguments as well as MAXPATHLEN
     * characters for the filename itself.
     */
    static char FullPath[2*MAXPATHLEN];

    localinfo.progs.commandfound = NULL;

    bbuf = buf;
    buf += strspn(buf, SEP);	/* skip leading whitespace */
    if (!*buf || *buf == '#')
	return 0;		/* Discard empty lines and pure-comment lines */

    if (bbuf != buf) {
	return Error(0, 0, "%t\n\tformat error in super(1) file: \n\
\tCommandPatterns must begin in column 1.\n");
    }

    /* Get the list of command/file pairs */
    strqS_qm = my_qm;
    strqS_cc = my_cc;

    if (localinfo.progs.nalloc == 0) {
	int nalloc = 10;
	if ( !(localinfo.progs.cmd_file = (ProgList *)
				malloc(nalloc * sizeof(ProgList))) )
	    return Error(1, 0, "%tFailed to malloc space for Cmd+File list\n");
	localinfo.progs.nalloc = 10;
    }
    localinfo.progs.n = 0;	/* Initialize to 0 cmd/file pairs */

    /* Start by inspecting the first word.  If it begins ":", it is
     * a special builtin command.  Otherwise, if it contains "::", it
     * must be a command/file pair.  Otherwise it's a single command.
     */
    word1 = strqtokS(buf, SEP, NULL, NULL, 1);
    if (!word1)
	return Error(0, 0, "%t\n\tformat error in super.tab file: \
CmdPattern or FullPathName missing.\n");

    if (*word1 == ':' || strcmp(word1, "/") == 0) {
	/* It's a special builtin command beginning ":" (or the obsolete
	 * special command name "/".)
	 */
	int i;

	i = process_colon_cmds(word1);
	return (i < 0) ? -1 : 0;
    }

    /* Build lists of cmd::file entries */
    if ((i=build_cmd_file(usrcmd, givehelp || checksyntax,
					word1, &cond_or_opt_wd)) <= 0)
	return i;


    if (!(givehelp || checksyntax)) {
	int indx = (localinfo.progs.match >= 0) ? localinfo.progs.match : 0;

	/* Check if we are supposed to generate a modified FullPath */
	char *path = localinfo.progs.cmd_file[indx].File;

	/* Adjust fullpath as necessary */
	if (fixup_fullpath(indx, usrcmd, path,
					FullPath, sizeof(FullPath)) == -1)
	    return -1;

    } else {
	/* givehelp or check syntax */
	int indx = (localinfo.progs.match >= 0) ? localinfo.progs.match : 0;
	localinfo.progs.commandfound = localinfo.progs.cmd_file[indx].Cmd;
    }

    if (debug)
	fprintf(stderr, "\t*** commandfound = `%s' path = `%s' ***\n",
	    localinfo.progs.commandfound, localinfo.progs.match >= 0 ?
		localinfo.progs.cmd_file[localinfo.progs.match].File : "-");

    /* Now process command-line conditions and options */
    if ((condition_match=conditions_and_options(cond_or_opt_wd)) == -1)
	return condition_match;


    if (condition_match && localinfo.die) {
	if (checksyntax)
	    return 0;
	else if (givehelp)
	    return -1;
	else
	    return Error(0, 0, "User's command: %s. %s\n",
				    usrcmd, localinfo.die);
    }
    return condition_match;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Builds list of cmd::file pairs for a control line.
 *	Return -1 if caller should give up parsing the file;
 *	return 0 if caller should loop to next chunk (because we
 *		are doing help processing, because of nomatch, etc.
 *	return 1 if success.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
build_cmd_file(usrcmd, givehelp_or_checksyntax, word1, cond_or_opt_wd)
char *usrcmd;			/* User's typed command */
int givehelp_or_checksyntax;	/* Just giving help or checking syntax
				 * of super.tab file?
				 */
char *word1;			/* First wd to process; get rest w/ strqtokS */
char **cond_or_opt_wd;
{
    /* Expect to see either
     *		cmd::file ...
     *	or
     *		cmd   file
     */

    char *s, *coloncolon, *pair;
    extern char *taglines();

    if ((s = strchr(word1, ':')) && *(s+1) == ':') {
	/* It's a Cmd::Filename pair.  Go through the list of
	 * command/filenames, until they are all used up, and record
	 * the matching filename.
	 */
	pair = word1;
	localinfo.progs.match = -1;	/* initialize to no match */
	do {
	    s = strchr(pair, ':');
	    coloncolon = (s && *(s+1) == ':') ? s : NULL;
	    if (!coloncolon) {
		/* Reached end of cmd::file list. */
		*cond_or_opt_wd = pair;
		break;
	    }
	    if (debug)
		fprintf(stderr, "Command::file = `%s'\n", pair);

	    if (coloncolon == pair)
		return Error(0, 0, "%t\n\tFormat error in super.tab file:\n\
\tCmd::File used, but the Cmd part is missing.\n");
	    *coloncolon = '\0';
	    localinfo.progs.n++;	/* Have a new cmd/file pair */
	    if (localinfo.progs.n == localinfo.progs.nalloc) {
		localinfo.progs.nalloc *= 2;
		localinfo.progs.cmd_file =
			    (ProgList *) realloc(localinfo.progs.cmd_file,
				localinfo.progs.nalloc * sizeof(ProgList));
		if (!localinfo.progs.cmd_file)
		    return Error(1, 0,
			"%tFailed to malloc space for Cmd+File list\n");
	    }

	    localinfo.progs.cmd_file[localinfo.progs.n - 1].Cmd = pair;
	    localinfo.progs.cmd_file[localinfo.progs.n - 1].File =
							coloncolon + 2;

	    if ((givehelp_or_checksyntax && !usrcmd) ||
				    match_pattern(0, 2, usrcmd, pair) == 1) {
		/* Matched the command */
		localinfo.progs.match = localinfo.progs.n - 1;
		localinfo.progs.evermatched = 1;
		if (localinfo.progs.cmd_file[localinfo.progs.match].File[0]
				== '\0')
		    return Error(0, 0, "%t\n\tFormat error in super.tab file:\n\
\tCmd::File used, but the File part is missing.\n");

		if (globalinfo.relative_path == 0 &&
		    localinfo.progs.cmd_file[localinfo.progs.match].File[0]
				!= '/')
		    return Error(0, 0, "%t\n\tFormat error in super.tab file:\n\
\tfilename `%s' is not an absolute path.\n\
\tUse global option relative_path=y if you really want such foolishness!\n",
		    localinfo.progs.cmd_file[localinfo.progs.match].File);
	    }

	} while ( (pair = strqtokS(NULL, SEP, NULL, NULL, 1)) );
	if (!pair)
	    *cond_or_opt_wd = NULL;

	if (!(givehelp_or_checksyntax && !usrcmd) &&
				localinfo.progs.match == -1)
	    return 0;		/* no match */

    } else if (s && *s == ':') {
	/* Embedded `:' in command name! */

	return Error(0, 0, "%t\n\tFormat error in super.tab file: \
The CmdPattern (%s) may not contain an embedded colon\n", word1);

    } else {
	/* It was a plain command name.  Get the full path. */
	localinfo.progs.n = 1;
	localinfo.progs.match = -1;	/* initialize to no match */
	localinfo.progs.cmd_file[0].Cmd = word1;
	localinfo.progs.cmd_file[0].File = strqtokS(NULL, SEP, NULL, NULL, 1);
	if (localinfo.progs.cmd_file[0].File == NULL ||
				localinfo.progs.cmd_file[0].File[0] == '\0')
	    return Error(0, 0, "%t\n\tformat error in super.tab file: \
CmdPattern or FullPathName missing.\n");

	    if (globalinfo.relative_path == 0 &&
			localinfo.progs.cmd_file[0].File[0] != '/')
		return Error(0, 0, "%t\n\tFormat error in super.tab file: \
filename `%s' is not an absolute path.\n\
\tUse global option relative_path=y if you really want such foolishness!\n",
		localinfo.progs.cmd_file[0].File);

	if (debug)
	    fprintf(stderr, "\tCmdPattern = `%s' %s\n",
		localinfo.progs.cmd_file[0].Cmd, taglines(1));

	if ((givehelp_or_checksyntax && !usrcmd)  ||
	    match_pattern(0, 2, usrcmd, localinfo.progs.cmd_file[0].Cmd) == 1) {
	    localinfo.progs.match = 0;	/* success */
	    localinfo.progs.evermatched = 1;
	}

	if (!(givehelp_or_checksyntax && !usrcmd) &&
					localinfo.progs.match == -1)
	    return 0;		/* Skip non-matching commands */

	/* The next word has to be ready in advance of the next loop: */
	*cond_or_opt_wd = strqtokS(NULL, SEP, NULL, NULL, 1);
    }
    return 1;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Take user's command, and the path from the control line, and generate
 * the full path that should be executed.
 * Returns -1 on error, indicating file should be parsed no further.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
fixup_fullpath(indx, usrcmd, path, FullPath, l_FullPath)
int indx;		/* Index into cmd_file array whence comes the path */
char *usrcmd;		/* User's entered command */
char *path;		/* Matched path from control line */
char *FullPath;		/* output: the generated full path */
int l_FullPath;		/* length of the FullPath buffer */
{
    register char *p, *q;
    int lpath, n;

    /* Watch out for sneaky users */
    if (strcspn(usrcmd, SEP) != strlen(usrcmd) || strchr(usrcmd, '\\'))
	return Error(0, 0,
	    "You entered `%s',\nbut you may NOT include whitespace or \
backslashes in the cmd!\n", usrcmd);

    /* Determine the length of the filename part of the path -- remember
     * that it may contain whitespace-separated arguments following the
     * filename.
     */
    lpath = strcspn(path, SEP);
    p = strchr(path, '*');	/* Check for asterisk in filename part */
    n = p ? p - path : 0;	/* number of characters before asterisk */
    if (p && n < lpath) {
	/* Wildcard command -- replace asterisk with the usrcmd */

	if ((strlen(path) + strlen(usrcmd)) > l_FullPath)
	    return Error(0, 0,
	    "%t\n\tRidiculously long path would be formed from Cmd \
and FullPathName:\n<%s> + <%s>", path, usrcmd);
	if ((q=strchr(p+1, '*')) && q < (path + lpath))
	    return Error(0, 0,
		"%t\n\tFullPathName can only have 1 asterisk, \
but the superfile entry\nreads `%s'\n", path);
	if (n > 0)
	    strncpy(FullPath, path, n);
	strcpy(FullPath + n, usrcmd);
	strcat(FullPath, p+1);
	localinfo.progs.commandfound = usrcmd;
	localinfo.progs.cmd_file[indx].File = FullPath;
    } else {
	/* Regular command */
	localinfo.progs.commandfound = usrcmd;
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process command-line conditions and options.
 * Returns -1 on syntax and other errors;
 * otherwise returns 0 if conditions fail to match;
 * otherwise conditions match, and returns 1;
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
conditions_and_options(cond_or_opt_wd)
char *cond_or_opt_wd;	/* First word to process; rest we get with strqtokS */
{
    int condition_match;
    int invert;
    register char *p;

    strqS_qm = my_qm;
    strqS_cc = my_cc;
    if (cond_or_opt_wd) {
	do {

	    p = cond_or_opt_wd;
	    if (*p == CONDITION_SEP || *p == OPTION_SEP)
		return Error(0, 0, "%t\n\tBad syntax: <%s>\n", cond_or_opt_wd);

	    for (p++; *p && ((*p != CONDITION_SEP && *p != OPTION_SEP) ||
						    (*(p-1) == '\\')); p++)
		    ;

	    if (*p == OPTION_SEP) {
		/* ************** */
		/* It's an option */
		/* ************** */
		if (handle_option(cond_or_opt_wd, p+1, 0) == -1)
		    return -1;
		continue;

	    } else if (*p == CONDITION_SEP) {
		/* **************** */
		/* It's a condition */
		/* **************** */
		if (InsertCondition(cond_or_opt_wd, p+1, 0) == -1)
		    return -1;

	    } else {
		/* Default is to treat it as user~ pattern.
		 * Insert in user list.
		 */
		int i;
		char *s, **glob;

		invert = *cond_or_opt_wd == '!';
		s = invert ? cond_or_opt_wd+1 : cond_or_opt_wd;

		/* Do brace globbing */
		if ((i=globbraces(s, 1, &glob)) != 0)
		    return Error(0, 0, "%tMissing `%c'.\n", i);

		if (InsertUserList(s, glob,
		    &localinfo.userpats, &localinfo.origtext, invert) == -1)
		    return -1;
	    }
	} while ( (cond_or_opt_wd = strqtokS(NULL, SEP, NULL, NULL, 1)) );
    }

    condition_match = 1;

    /* Look at condition lists in reverse order, and stop as soon
     * as there is a result.
     */
    if (globalinfo.userafter.next ||
	    localinfo.userpats.next ||
	    globalinfo.userbefore.next) {
	/* We have user fields to match */
	matches.user = 0;	/* default is to disallow user */
	match_ugh_user(&globalinfo.userafter, 1);
	if (!matches.user) {
	    match_ugh_user(&localinfo.userpats, 0);
	    if (!matches.user) {
		match_ugh_user(&globalinfo.userbefore, 1);
	    }
	}
    } else {
	return Error(0, 0,
	"%t\n\tNo user patterns given on control line or in :global line.\n");
    }
    if (!matches.user)
	condition_match = 0;

    if (condition_match != 0) {
	if (globalinfo.timeafter.next ||
		localinfo.time.next ||
		globalinfo.timebefore.next) {
	    /* We have time fields to match */

	    /* Default depends on whether all time fields are inverted */
	    matches.time = -1;
	    matches.allinverted = 1;
	    matchtime(&userinfo.ourtime, &globalinfo.timeafter);
	    if (matches.time == -1) {
		matchtime(&userinfo.ourtime, &localinfo.time);
		if (matches.time == -1) {
		    matchtime(&userinfo.ourtime, &globalinfo.timebefore);
		}
	    }
	    if (matches.time == -1) {
		/* Default is permit execution if all patterns were inverting;
		 * deny execution if there were some non-inverting patterns.
		 */
		condition_match = matches.allinverted ? 1 : 0;
		if (debug) {
		    if (matches.allinverted)
			fputs("\tPermission allowed (= default when \
all time patterns are !time~xxx)\n", stderr);
		    else
			fputs("\tPermission denied (= default when \
>=1 time pattern is non-negated)\n", stderr);
		}
	    } else {
		condition_match = matches.time;
	    }
	}
    }
    return condition_match;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Prints help information title */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
printhelp_hello(verbosity, usrcmd)
int verbosity;
char *usrcmd;
{
    if (verbosity == HELP_FACTS)
	return;

    printf("Use:\n\t%s [-d | -D] command [args]\n", prog);
    printf("\t-d,-D enable debugging output (-D is more verbose than -d)\n");
    printf("    or:\n");
    printf("\t%s [-h|-H|-f|-V]\n", prog);
    printf("\t-h=help; -H=long help; -f=just-facts-help; -V=version\n");
    printf("    or:\n");
    printf("\t%s -c\n", prog);
    printf("\t-c=check syntax in super file; don't execute anything.\n\n");
    if (verbosity) {
	printf("Super.tab file: `%s'\n\n", superfile);
    }
    if (usrcmd) {
	if (verbosity == HELP_BASIC)
	    (void) printf("Long help on command `%s' for user %s:\n\n",
				usrcmd, userinfo.caller.pw_name);
	else if (verbosity == HELP_FULL)
	    (void) printf("Terse help on command `%s' for user %s:\n\n",
				usrcmd, userinfo.caller.pw_name);
    } else {
	(void) printf("Commands available to user %s%s:\n\n",
		userinfo.caller.pw_name,
		verbosity ? " (use option `-h' for a terse listing)" :
		" (use option `-H' for a long-winded listing)");
	if (!verbosity) {
	    (void) printf("Command Name    Comments\n");
	    (void) printf("or Pattern              \n");
	    (void) printf("------------    --------\n");
	}
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Prints help information for a command. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
printhelp(verbosity)
int verbosity;
{
    char **p, *s;
    int i, j, n;

    if (verbosity == HELP_BASIC) {
	if (localinfo.progs.n < 1) {
	    ;
	} else if (localinfo.progs.n == 1) {
	    /* The entry contains a single CmdPattern/FullPath pairs */

	    /* If the command contains a wildcard, print the full filename,
	     * otherwise just the command name.
	     */
	    if (strchr(localinfo.progs.cmd_file[0].Cmd, '*')) {
		printf("%s -> %s:\n                -- %s\n",
			localinfo.progs.cmd_file[0].Cmd,
			localinfo.progs.cmd_file[0].File,
			localinfo.info);
	    } else {
		printf("%-15s %s\n", localinfo.progs.cmd_file[0].Cmd,
			localinfo.info ? localinfo.info : "");
	    }
	} else {
	    /* The entry contains multiple CmdPattern/FullPath pairs */

	    /* If any command contains a wildcard, print the
	     * full filenames, otherwise just the command name.
	     */
	    int havewildcard=0;
	    for (i=0; havewildcard==0 && i<localinfo.progs.n; i++) {
		havewildcard =
			(strchr(localinfo.progs.cmd_file[0].Cmd, '*') != NULL);
	    }
	    if (havewildcard) {
		for (i=0; i<localinfo.progs.n-1; i++) {
		    printf("%s -> %s;\n", prog,
			localinfo.progs.cmd_file[i].Cmd,
			localinfo.progs.cmd_file[i].File);
		}
		i = localinfo.progs.n - 1;
		printf("%s -> %s:\n", prog,
		    localinfo.progs.cmd_file[i].Cmd,
		    localinfo.progs.cmd_file[i].File);
		if (localinfo.info && *localinfo.info)
		    (void) printf("                -- %s\n", localinfo.info);

	    } else {
		/* Now wildcards in names */
		printf("%s", localinfo.progs.cmd_file[0].Cmd);
		for (i=1; i<localinfo.progs.n; i++)
		    printf(", %s", localinfo.progs.cmd_file[i].Cmd);
		if (localinfo.info && *localinfo.info)
		    (void) printf(":\n                -- %s\n",
			localinfo.info);
		else
		    (void) putchar('\n');
	    }
	}
	return;

    } else if (verbosity == HELP_FACTS) {
	for (i=0; i<localinfo.progs.n; i++)
	    (void) printf("%s %s\n", localinfo.progs.cmd_file[i].Cmd,
					localinfo.progs.cmd_file[i].File);
	return;

    } else if (verbosity == HELP_FULL) {
	for (i=0; i<localinfo.progs.n; i++)
	    (void) printf("%s %s -> %s\n", prog,
			localinfo.progs.cmd_file[i].Cmd,
			localinfo.progs.cmd_file[i].File);
    } else {
	(void) Error(0, 0, "%s: printhelp(): Unknown verbosity level %d\n",
			verbosity);
	return;
    }

    /* Assert verbosity == HELP_FULL */
    if (localinfo.info)
	printf("\t## %s\n", localinfo.info);

    if (localinfo.die) {
	printf("\tCommand forces exit using 'die' option:\n\t\tdie=\"%s\"",
		localinfo.die);
	return;
    }

    if (localinfo.print)
	printf("\tCommand prints msg before execution:\n\t\t\"%s\"",
		localinfo.print);

    if (globalinfo.timebefore.next ||
	    globalinfo.timeafter.next ||
	    localinfo.time.next) {
	printf("\tPermitted times for execution (in reverse input order):\n");
	for (j = 0; j<3; j++) {
	    TimeList *tl;
	    switch (j) {
	    case 0: tl = globalinfo.timebefore.next; break;
	    case 1: tl = localinfo.time.next; break;
	    default: tl = globalinfo.timeafter.next; break;
	    }
	    for ( ; tl; tl=tl->next) {
		printf("\t\t%stime~%d:%02d-%d:%02d/%s\n",
		    tl->te.invert ? "!" : "",
		    tl->te.begin / 60, tl->te.begin % 60,
		    tl->te.end / 60, tl->te.end % 60, dayname(tl->te.day));
	    }
	}
    }
    if (*localinfo.user ||
	    *localinfo.group ||
	    *localinfo.u_g ||
	    localinfo.fdlist ||
	    *localinfo.owner ||
	    localinfo.env) {
	(void) fputs("\t(Executes with:", stdout);
	if (*localinfo.user) (void) printf(" uid=%s", localinfo.user);
	if (*localinfo.group) (void) printf(" gid=%s", localinfo.group);
	if (*localinfo.u_g) (void) printf(" u+g=%s", localinfo.u_g);
	if (localinfo.env) {
	    for (p=localinfo.env; *p; p++)
		(void) printf(" %s%s", p==localinfo.env ? " env=" : ",", *p);
	}
	if (localinfo.setenv) {
	    for (p=localinfo.setenv; *p; p++)
		(void) printf(" setenv=%s", *p);
	}
	if (localinfo.fdlist) (void) printf(" fdlist=%s", localinfo.fdlist);
	if (*localinfo.owner) (void) printf(" owner=%s", localinfo.owner);
	(void) fputs(")\n", stdout);
    }
    if (localinfo.usr_args[0] < 0)
	;
    else if (localinfo.usr_args[0] == localinfo.usr_args[1] &&
					    localinfo.usr_args[0] == 0)
	(void) printf("\tNo user-entered args are allowed.\n");
    else if (localinfo.usr_args[0] == localinfo.usr_args[1])
	(void) printf("\t%d user-entered arg%s required.\n",
	    localinfo.usr_args[0], localinfo.usr_args[0] == 1? " is" : "s are");
    else
	(void) printf("\t%d - %d user-entered args are required.\n",
	    localinfo.usr_args[0], localinfo.usr_args[1]);
    n = StrLastInUse(&localinfo.argpats);
    if (n >= 0) {
	printf("\tUser-entered arguments must match following patterns:\n");
	for (i = 0; i<=n; i++) {
	    s = StrEltGetPtr(&localinfo.argpats, i);
	    if (s)
		printf("\t\tArg %d must match pattern: %s\n", i, s);
	}
    }

    if (rcl_nice_incr() != 0)
	printf("\tCommand executes with nice increment = %d.\n",
		    rcl_nice_incr());
    if (rcl_umask() != userinfo.orig_mask)
	printf("\tCommand executes with umask set to 0%o.\n", rcl_umask());


    if (localinfo.passinfo.required) {
	if (localinfo.passinfo.timeout == 0)
	    (void) printf("\tPassword is required on each use.\n");
	else
	    (void) printf("\tPassword is required; good for %d minutes%s.\n",
		localinfo.passinfo.timeout, localinfo.passinfo.renewtime ? 
		    ", extended with each use" : "");
    }
    (void) putchar('\n');
}

