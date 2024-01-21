
#include "super.h"
#include "version.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Processes the special ":" builtin commands */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*	Return -1 if caller should give up parsing the file;
 *	return 0 on success;
 */
int
process_colon_cmds(command)
char *command;
{
    if ((strcmp(":global", command) == 0) ||
	(strcmp(":global_options", command) == 0) ||
	(strcmp("/", command) == 0)	/*obsolescent*/
	) {

	/* Process global options */
	return colon_global(command);

    } else if (strcmp(":define", command) == 0) {

	/* Process variable definition */
	return colon_define(command);

    } else if (strcmp(":include", command) == 0) {

	/* Process include-file directive */
	return colon_include(command);
    }

    return Error(0, 0, "%t\n\tUnknown builtin command `%s'.\n", command);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process the :global command */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
colon_global(command)
char *command;
{
    char *wd;

    if (strcmp("/", command) == 0) {
	/* OBSOLESCENT COMMAND */
	/* Get the full path; verify that it is '/', then discard  */
	char *path = strqtokS(NULL, SEP, NULL, NULL, 1);
	if (!command || !*path || strcmp(path, "/") != 0)
	return Error(0, 0, "%t\n\tformat error in super.tab file: \
Cmd == '/' requires FullPathName == '/'.\n");
    }

    /* Some global settings need to be reset on each new :global line. */
    if (handle_option(NULL, NULL, 1) != 0)
	return -1;

    /* Now collect the options and process */
    for (strqS_qm = my_qm, strqS_cc = my_cc,
	    wd = strqtokS(NULL, SEP, NULL, NULL, 1); wd;
				wd = strqtokS(NULL, SEP, NULL, NULL, 1)) {
	if (global_arg(wd) == -1)
	    return -1;
    }


    /* Process logfile requests as soon as possible */
    if (globalinfo.log.newfile || globalinfo.log.newuid)
	if (process_logfile_opt() == -1)
	    return -1;

    return 0;
}

/* Process logfile requests */
int
process_logfile_opt()
{
    if (globalinfo.log.newuid && !globalinfo.log.newfile)
	return Error(0, 0, "%t\n\tformat error in super.tab file: \n\t\
loguid=xxx must be used on the same :global_option line as logfile=yyy.\n");

    globalinfo.log.newfile = globalinfo.log.newuid = 0;
    opensuperlog();
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process a global arg */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
global_arg(word)
char *word;	/* opt=xxx or condition~Pattern or PermittedUserPattern */
{
    /* Return 0 on success, -1 if formatting error. */
    extern int error_syslog;
    int invert, iscondition;
    char **glob;
    char *s;
    char *pattern;

    if (strcmp("\\", word) == 0)
	return 0;

    s = word;
    if (*s == CONDITION_SEP || *s == OPTION_SEP)
	return Error(0, 0, "%t\n\tBad super.tab syntax: <%s>\n", word);
    for (s++; *s && ((*s != CONDITION_SEP && *s != OPTION_SEP) ||
					    (*(s-1) == '\\')); s++)
	    ;
    /* s now points to one of:
     *	- the OPTION_SEP or CONTITION_SEP (if there is one),
     * or
     *	- the null character at the end of a plain PermittedUserPattern.
     */

    invert = (*word == '!');
    if (invert && *s == OPTION_SEP) {
	return Error(0, 0,
	    "%t\n\tsuper.tab syntax error: options cannot be negated: <%s>\n",
	    word);
    } else if (invert) {
	word++;
    }
    /* Note that word has been advanced past the '!' inversion character */

    if (strcmp(word, "<>") == 0) {
	/* End of globalinfo.before list.  Since we accumulate into
	 * globalinfo.after list, just move that list over to
	 * globalinfo.before.
	 */
	if (invert)
	    return Error(0, 0, "%t\n\tInvalid global condition \"!<>\"\n");

	if (globalinfo.use_after != 0)
	    return Error(0, 0, "%t\n\tMultiple use of `<>' in :global list.\n");

	globalinfo.use_after = 1;
	globalinfo.userbefore.next = globalinfo.userafter.next;
	globalinfo.userafter.next = NULL;

	globalinfo.timebefore.next = globalinfo.timeafter.next;
	globalinfo.timeafter.next = NULL;

	return 0;
    }

    iscondition = (*s != OPTION_SEP);
    if (iscondition) {
	/* Do brace globbing.  Remember that if the pattern is a plain
	 * PermittedUserPattern, then s points to the null character.
	 * Use a new pointer to point to the actual pattern.
	 */
	int i;
	if (*s == '\0')
	    pattern = word;	/* s pts to end of plain PermittedUserPattern */
	else
	    pattern = s+1;	/* s points to "~"; advance past to pattern */

	if ((i=globbraces(pattern, 1, &glob)) != 0) {
	    /* Global condition */
	    return Error(0, 0, "%tMissing `%c'.\n", i);
	}
    }
    if (iscondition && STRMATCH3("user", word, s)) {
	/* It's a PermittedUser pattern */

	/* Put the (user/group/host) pattern into the after list.
	 * If we eventually see "<>", we'll move the list to
	 * before list (see above).
	 */
	if (globalinfo.user_clear) {
	    free_Simple2List(&globalinfo.userbefore);
	    free_Simple2List(&globalinfo.userafter);
	    free_SimpleList(&globalinfo.b_a_text);
	    globalinfo.user_clear = 0;
	}
	if (InsertUserList(pattern, glob,
		&globalinfo.userafter, &globalinfo.b_a_text, invert) == -1)
	    return -1;

    } else if (iscondition && STRMATCH3("time", word, s)) {
	/* It's a PermittedTime pattern.  Put it into the after list.
	 * If we eventually see "<>", we'll move the list to
	 * before list (see above).
	 */
	if (globalinfo.time_clear) {
	    free_TimeList(&globalinfo.timebefore);
	    free_TimeList(&globalinfo.timeafter);
	    globalinfo.time_clear = 0;
	}
	if (InsertTimeList(pattern, glob,
			&globalinfo.timeafter, "global", invert) == -1)
	    return -1;

    } else if (iscondition) {
	return Error(0, 0,
	"%t\n\tInternal error -- unrecognized global condition <%s>\n", word);
	
    } else {
	return handle_option(word, s+1, 1);
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process the :define command */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
colon_define(command)
char *command;
{
    extern char *strqS_start;
    char *varname, *varbody;
    char *s;

    varname = strqtokS(NULL, SEP, NULL, NULL, 1);
    if (!varname)
	return Error(0, 0, "%t\n\tformat error in super.tab file: \
variable name missing after ':define'.\n");

    varbody = strqS_start;
    if (!varbody)
	varbody = "";

    /* Skip leading whitespace in variable body */
    while (strchr(SEP, *varbody))
	varbody++;

    /* Delete final newline from varbody */
    s = varbody + strlen(varbody)-1;
    if (s >= varbody && *s == '\n')
	*s = '\0';

    return add_variable(varname, varbody);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Process the :include command */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
colon_include(command)
char *command;
{
    char *filename, *wd, *s;
    extern char *strqS_start;
    FileList *fl;

    filename = strqtokS(NULL, SEP, NULL, NULL, 1);
    if (!filename)
	return Error(0, 0, "%t\n\tformat error in super.tab file: \
filename missing after ':include'.\n");

    wd = strqS_start;
    if (wd && *wd != '\n' && *wd != '\0')
	return Error(0, 0, "%t\n\tformat error in super.tab file: \n\t\
extra text <%s> after :include filename\n", wd);

    /* Check for and delete possible newline from filename */
    s = filename + strlen(filename)-1;
    if (s >= filename && *s == '\n')
	*s = '\0';

    fl = file_open(currfile, filename);
    if (!fl)
	return -1;

    currfile = fl;
    return 0;
}

