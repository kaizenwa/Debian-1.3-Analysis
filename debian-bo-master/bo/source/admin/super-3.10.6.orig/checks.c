/* The code should compile with either ANSI C or K&R compilers. */

/*
 *      Copyright (c) 1993 by California Institute of Technology.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

#include "super.h"
#include "version.h"

#ifdef HAVE_INNETGR
#define netgrp_u_compare(pattern, user) innetgr(pattern, NULL, user, NULL)
#define netgrp_h_compare(pattern, host) innetgr(pattern, host, NULL, NULL)
#else
#define netgrp_u_compare(p, u) 0
#define netgrp_h_compare(p, h) 0
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Check that an environment variable only includes allowed characters */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Returns 0 if pat matched; -1 otherwise.  */
int
checkenv(name, value, pat)
char *name;	/* variable name to check (e.g. "TERM") */
char *value;	/* contents of variable (e.g. "vt100") */
char *pat;	/* pattern that value must match */
{

    if (!value)
	return -1;

    if (debug)
	(void) fprintf(stderr,
	    "\tcheckenv args: name=\"%s\"; value=\"%s\"; pat=\"%s\"\n",
	    name, value, pat);
    
    /* Environment variables are always checked with s_re_comp/s_re_exec:
     * the patterns are fixed internally, not supplied by the user.
     */
    if (s_re_comp(pat))
	return Error(0, 0,
	    "%t\n\tcheckenv(): couldn't compile pattern `%-.500s'.\n", pat);

    if (s_re_exec(value) != 1)
	return Error(0, 0,
	    "checkenv(): $%.100s (=%.100s) doesn't match pattern %-.500s.\n",
			name, value, pat);
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Look up owner of a file, return uid and gid of owner */
/* Return 0 on success, -1 & print message on failure */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
get_owner(file, uid_p, gid_p)
char *file;
uid_t *uid_p;
gid_t *gid_p;
{
    /* Return 0 if file ownership ok; -1 if not ok */
    struct stat st;

    if (!file || *file == '\0')
	return Error(0, 0, "get_owner(): passed null ptr or empty string\n");

    if (stat(file, &st) == -1)
	return Error(1, 0, "stat() failed on file `%s': ", file);

    *uid_p = st.st_uid;
    *gid_p = st.st_gid;

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Check ownership of the file */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
check_owner()
{
    /* Return 0 if file ownership ok; -1 if not ok */
    struct passwd *owner_pw;

    if (*localinfo.owner == '\0')
	return 0;	/* no checks required */

    /* Convert desired-owner string to a uid */
    owner_pw = getpwentry(localinfo.owner);
    if (!owner_pw)
	return -1;

    if (localinfo.file_uid != owner_pw->pw_uid)
	return Error(0, 0,
	    "Actual owner of `%s' is uid %d, but superfile \
requires owner to be %d (%s).\n",
	    localinfo.progs.cmd_file[localinfo.progs.match].File,
	    localinfo.file_uid, owner_pw->pw_uid, owner_pw->pw_name);
    
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Gets an entry from the password file.  Accepts special bracketed
 * name (e.g. <owner> or <caller>).  Accepts text uid's instead of names.
 * Returns ptr to password entry.  The password entry may be that returned
 * by getpwnam(), or it may be one already stored in a super-owned struct;
 * you can't make any assumptions about it -- therefore don't modify it.
 *
 * The returned pointer points to an area that may be overwritten by later
 * calls to the getpwxxx() routines; therefore if the caller wants to save
 * the data, the data must be copied.
 *
 * On error, print message and return NULL.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
struct passwd *
getpwentry(username)
char *username;		/* name to translate */
{
    struct passwd *pw;
    int l = strlen(username);

    if (username[0] == '<' && username[l-1] == '>') {
	/* Translate special name */
	if (strcmp(username, "<owner>") == 0) {
	    if (localinfo.file_uid != UID_NOTSET) {
		pw = getpwuid(localinfo.file_uid);
	    } else {
		Error(0, 0,
	    "%t: getpwentry() Internal Error!\n\tFile owner not yet known!\n");
		return NULL;
	    }
	} else if (strcmp(username, "<caller>") == 0) {
	    pw = &userinfo.caller;
	} else {
	    Error(0, 0, "%t\n\t\tUnknown special name %s\n", username);
	    return NULL;
	}
    } else {
	/* Regular name or number */
	pw = getpwnam(username);
	if (!pw) {
	    char c;
	    int i, numeric;
	    numeric = (sscanf(localinfo.user, "%d%c", &i, &c) == 1);
	    if (numeric)
		pw = getpwuid(i);
	}
    }
    if (!pw) {
	Error(0, 0, "%t\n\tNo such user or uid as `%s' in password file.\n",
	    username);
	return NULL;
    }
    return pw;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Set supplementary groups according to the specified args                */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef HAVE_GETGROUPS
int
set_suppl_groups() { return 0; }
#else
int
set_suppl_groups()
{
    GETGROUPS_T *addgroups_p, *groups_p, groups[NGROUPS_MAX];
    GETGROUPS_T gotten_groups[NGROUPS_MAX];
    int naddgroups, ngroups;
    int i;

    /* Set the supplementary groups */
    if (*localinfo.u_g) {
	/* Default is to take from that user.  (Don't worry if there is no
	 * such user; it'll get noticed later.)  Conflicts with local
	 * option groups=xxx, which is disallowed.
	 */
	if (localinfo.ngroups != GROUPS_NOTSET && !localinfo.groups_added)
	    return Error(0, 0, "%t\n\t\tu+g=xxx conflicts with groups=yyy \
and may not be used in the same entry\n");

	initgroups(localinfo.user, userinfo.new_gid);
	/* Check for local or global addgroups */
	if (localinfo.ngroups != GROUPS_NOTSET) {
	    addgroups_p = localinfo.groups;
	    naddgroups = localinfo.ngroups;
	} else if (globalinfo.ngroups != GROUPS_NOTSET &&
					globalinfo.groups_added) {
	    addgroups_p = globalinfo.groups;
	    naddgroups = globalinfo.ngroups;
	} else {
	    addgroups_p = NULL;
	    naddgroups = 0;
	}
	ngroups = Getgroups(NGROUPS_MAX, gotten_groups);
	if (ngroups == -1)
	    return Error(1, 0, "%t Getgroups() failed: ");
	if (ngroups + naddgroups > NGROUPS_MAX)
	    return Error(1, 0,
			    "%t\n\t\taddgroups=xxx adds too many groups.");
	for (i = 0; i < ngroups; i++)
	    groups[i] = gotten_groups[i];
	for (groups_p = &groups[ngroups], i=0; i < naddgroups; i++)
	    *groups_p++ = *addgroups_p++;
	ngroups += naddgroups;
	    
    }  else if (localinfo.ngroups != GROUPS_NOTSET) {
	/* There are some explicit local groups=xxx */
	if (Setgroups(localinfo.ngroups, localinfo.groups) == -1)
	    return Error(1, 0, "Failed to set supplementary groups list: ");

    } else if (globalinfo.ngroups != GROUPS_NOTSET) {
	/* There is an explicit global groups=xxx or addgroups=xxx */
	if (Setgroups(globalinfo.ngroups, globalinfo.groups) == -1)
	    return Error(1, 0, "Failed to set supplementary groups list: ");
    } else {
	/* Default is no supplementary groups */
	if (Setgroups(0, localinfo.groups) == -1)
	    return Error(1, 0,
			    "Failed to clear supplementary groups list: ");
    }
    return 0;
}
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Set user, group, and supplementary groups according to the specified args */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
set_u_g()
{
    /* Return 0 on success, -1 on failure */
    struct passwd *uid_pw;
    SETGRENT_T setgrent();
    void endgrent();
    int i, found_gid;

    userinfo.new_uid = userinfo.caller.pw_uid;
    userinfo.new_gid = userinfo.caller.pw_gid;

    /* Check gid=xxx */
    if (*localinfo.group) {
	found_gid = findgid(localinfo.group);

	if (found_gid == -1)
	    return Error(0, 0,
		"%t\n\tCan't set gid: no such group as `%s' in group file.\n",
		localinfo.group);
	userinfo.new_gid = found_gid;
    }

    /* Check uid=xxx u+g=yyy */
    if (*localinfo.user || *localinfo.u_g) {
	if (*localinfo.u_g)
	    strcpy(localinfo.user, localinfo.u_g);
	
	uid_pw = getpwentry(localinfo.user);
	if (!uid_pw)
	    return -1;
	userinfo.new_uid = uid_pw->pw_uid;
	if (*localinfo.u_g) {
	    userinfo.new_gid = uid_pw->pw_gid;
	}
    }

    /* Set supplementary groups */
    if (set_suppl_groups() == -1)
	return -1;


    /* Now set uid & gid */
    if (*localinfo.group || *localinfo.u_g)
	if (setgid(userinfo.new_gid) == -1)
	    return Error(1, 0, "setgid(gid=%d) failed: ", userinfo.new_gid);
    if (*localinfo.user || *localinfo.u_g)
	if (setuid(userinfo.new_uid) == -1)
	    return Error(1, 0, "setuid(uid=%d) failed: ", userinfo.new_uid);

    if (localinfo.passinfo.required) {
	/* Get caller's encrypted password */
	i = get_encrypted_pw();
	if (i != 0)
	    return i;
    }

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Puts the encrypted password in userinfo.encr, and the salt in userinfo.salt.
 * Returns 0 on success, -1 on failure to obtain the password.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef SUNOS5 
int
get_encrypted_pw()
{
    /* Shadow passwords are always used on Sunos 5.x */
    struct spwd *caller_pw;
    if (!(caller_pw = getspnam(userinfo.caller.pw_name)))
	return Error(1, 0,
	    "Failed to obtain shadow password entry for user %s: ",
	    userinfo.caller.pw_name);
    strcpy(userinfo.encr, caller_pw->sp_pwdp);
    strncpy(userinfo.salt, caller_pw->sp_pwdp, 2);
    userinfo.salt[2] = '\0';
    return 0;
}

#else
#ifdef _HPUX_SOURCE
int
get_encrypted_pw()
{
    struct passwd *caller_pw;

    /* See if we can do shadow password lookup for HPUX 9.x.
     * The rule is that if /.secure/etc/passwd exists, we have to use it;
     * otherwise, fall through to regular password file lookup.
     */
    static struct stat st;
    if (stat("/.secure/etc/passwd", &st) == 0) {
	/* Shadow password file exists; use it */
	struct s_passwd *caller_pw;
	if (!(caller_pw = getspwnam(userinfo.caller.pw_name)))
	    return Error(1, 0,
		"Failed to obtain shadow password entry for user %s: ",
		userinfo.caller);
	strcpy(userinfo.encr, caller_pw->pw_passwd);
	strncpy(userinfo.salt, caller_pw->pw_passwd, 2);
	userinfo.salt[2] = '\0';
    } else {

	/* Fall through to regular password file lookup. */
	strcpy(userinfo.encr, userinfo.caller.pw_passwd);
	strncpy(userinfo.salt, userinfo.caller.pw_passwd, 2);
	userinfo.salt[2] = '\0';
    }
    return 0;
}

#else
#ifdef SCO
int
get_encrypted_pw()
{
    struct passwd *caller_pw;
    struct spwd *caller_ps;

    if (!(caller_pw = getpwnam(userinfo.caller.pw_name)))
	return Error(0, 0, "No password entry for user %s.\n",
		userinfo.caller.pw_name);

    /* SCO 3.2v4 has "x" in password field to indicate shadow password
     * file has to be consulted.
     */
    if (strcmp(caller_pw->pw_passwd, "x") == 0) {
	/* Shadow password in use... */
	if (!(caller_ps = getspnam(userinfo.caller.pw_name)))
	    return Error(1, 0,
	    "Failed to obtain shadow password entry for user %s: ",
	    userinfo.caller.pw_name);
	strcpy(userinfo.encr, caller_ps->sp_pwdp);
	strncpy(userinfo.salt, caller_ps->sp_pwdp, 2);
	userinfo.salt[2] = '\0';

    } else {
	/* Fall through to regular password file lookup. */
	strcpy(userinfo.encr, caller_pw->pw_passwd);
	strncpy(userinfo.salt, caller_pw->pw_passwd, 2);
	userinfo.salt[2] = '\0';
    }
    return 0;
}

#else

int
get_encrypted_pw()
{
    /* Vanilla password file lookup */

    strcpy(userinfo.encr, userinfo.caller.pw_passwd);
    strncpy(userinfo.salt, userinfo.caller.pw_passwd, 2);
    userinfo.salt[2] = '\0';
    return 0;
}

#endif
#endif
#endif


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Checks if password is needed, and if so, prompts for same.
 * Returns 0 on success, -1 on error.

 * The timestamp directory faces the same problem as the logfile: if the
 * administrator wants to share an NFS-mounted directory across hosts
 * on which root is translated to nobody for NFS access, we have to be
 * able to create the timestamp file under a special uid.  This is done
 * just as in open_writer(): we fork, setuid(), and do the file
 * manipulation in the child.  This allows us to implement a special uid
 * for the timestamp file, without needing the operating system to
 * offer saved uid's or interprocess file-descriptor passing, etc.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
check_pass(cmd)
char *cmd;
{
    char file[MAXPATHLEN];
    struct stat st;
    int l, istat, file_exists, got_pass;
    int timed_out = 0;
    int status;
    pid_t pid, child;

    if (!localinfo.passinfo.required)
	return 0;			/* don't need password */

    /* Create or update the timestamp file even if the lifetime is 0
     * (always ask for password).  We do this because the user may
     * execute _another_ command which has a password expiry > 0,
     * and which will be happy to use the password that was already
     * entered with the 0-lifetime command.
     */
    child = fork();
    if (child == -1) {
	return Error(1, 0, "Failed to create child for timestamp processing: ");

    } else if (child > 0) {
	/* In parent -- wait to see if the child succeeded */
	while ((pid = wait(&status)) > 0 && pid != child) {
	    if (pid == globalinfo.log.pid) {
		Error(0, 0,
	    "Warning: logging process died -- logging to file has stopped.");
		globalinfo.log.pid = -1;
	    } else {
		Error(0, 0,
		    "wait() surprised! check_pass() received pid %d;\n\t\
expected child pid = %d; waiting for correct pid...\n", pid, child);
	    }
	}
	if (pid == -1)    
	    return Error(1, 0, "waiting for timestamp creation process: ");
	else if (status == 0)
	    return 0;
	else
	    return Error(0, 0, "Timestamp creation failed\n");
	
    } else {
	/* In child.  setuid, then make and/or test the directory */
	if (*localinfo.passinfo.user != '\0') {
	    stringcopy(localinfo.user, localinfo.passinfo.user,
						sizeof(localinfo.user));
	    *localinfo.group = '\0';
	    *localinfo.u_g = '\0';
	    if (set_u_g() == -1) {
		return Error(1, 0,
		"failed to setuid %s before setting timestamp file: ",
		localinfo.user);
	    }
	}
	/* Make the timestamp directory name */
	if (!makedirname(globalinfo.passinfo.perhost ? TIMESTAMP_DIR : "",
						userinfo.hostname, file))
	    return -1;

	/* Make the timestamp directory */
	if (makedir(file) == -1)
	    return -1;

	/* Make the file in the timestamp directory */
	l = strlen(file) + 1 + strlen(userinfo.caller.pw_name);
	if (l >= MAXPATHLEN)
	    return Error(1, 0,
	    "Can't create timestamp file: it would exceed MAXPATHLEN = %d\n",
	    MAXPATHLEN);
	strcat(file, "/");
	strcat(file, userinfo.caller.pw_name);

	istat = stat(file, &st);
	if (istat != 0 && errno != ENOENT)
	    return Error(1, 0, "Failed to stat timestamp file `%s': ", file);

	file_exists = (istat == 0);
	if (file_exists)
	    timed_out = (localinfo.passinfo.timeout < 1) ||
			((time(NULL)-st.st_mtime) >
			localinfo.passinfo.timeout*60);

	got_pass=0;
	if (!file_exists || timed_out) {
	    got_pass = (get_password(cmd, userinfo.caller.pw_name,
					userinfo.salt, userinfo.encr) == 1);
	    if (!got_pass)
		return -1;
	}

	/* NOTE: A race condition is possible between two super's, with the
	 * worst-case effect of an error message and failure to run the
	 * requested command.
	 */

	/* If file exists, and we haven't (a) gotten the password again, or
	 * (b) supposed to automatically refresh the timestamp, do nothing to
	 * the file except ensure that we own it.

	 * Otherwise create the file (unlink it first if it exists).
	 */
	if (file_exists && !(got_pass || localinfo.passinfo.renewtime)) {
	    if (st.st_uid != geteuid())
		return Error(0, 0,
	    "Timestamp file `%s' is owned by uid=%d, but expected owner=%d.\n\
\tN.B. If you recently changed the value of timestampuid=xxx, all existing\n\
\tfiles in the timestamp directory _may_ have the wrong owner; delete them.\n\
\t(No security hole appears when you delete a timestamp file.)\n",
		file, st.st_uid, geteuid());

	} else {
	    if (file_exists) {
		if (unlink(file) != 0)
		    return Error(1, 0,
			"Failed to unlink() timestamp file `%s': ", file);
	    }
	    if (open(file, O_WRONLY|O_CREAT|O_TRUNC|O_EXCL, 0200) == -1)
		return Error(1, 0,
		    "Failed to open() timestamp file `%s': ", file);
	}
	exit(0);
    }
    /* UNREACHABLE */
    Error(0, 1, "Unreachable code!\n");
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Gets a user's encrypted password. Returns -1 on failure, +1 on success */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
get_password(cmd, user, salt, encr)
char *cmd, *user, *salt, *encr;
{
    /* No such file or password timed out -- get password */
    int ntry, match;
    char msg[500];
    char *crypt(), *getpass();
    char *encrypted = NULL;
    if (strcmp(encr, "") == 0) {
	return Error(0, 0,
	    "Command requires a password, but user `%s' has no password\n",
    user);
    }
    for (ntry=0, match=0; ntry < MAXTRY && !match; ntry++) {
	if (ntry == 0) {
	    (void) sprintf(msg,
    "Your password is required for super command `%s'...\nPassword: ",
		cmd);
	} else {
	    strcpy(msg, "Password incorrect\nPassword: ");
	}

	encrypted = crypt(getpass( msg ), salt);
	if (encr && encrypted)
	    match = (strcmp(encr, encrypted) == 0);
	else
	    match = 0;
    }
    if (!match)
	return Error(0, 0, "Password incorrect\n");
    return 1;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Looks up a group name or number (as a text string), returns gid.
 * Accepts special names (e.g. <caller> or <owner>).
 * Returns -1 if no such group.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
findgid(grouplabel)
char *grouplabel;	/* name or numeric form */
{
    struct group *gp;
    SETGRENT_T setgrent();
    void endgrent();
    int numeric_gid;
    int found_gid, is_numeric=0;
    int l=strlen(grouplabel);

    char c;

    if (grouplabel[0] == '<' && grouplabel[l-1] == '>') {
	/* Translate special name */
	struct passwd *pw = getpwentry(grouplabel);
	if (!pw)
	    return -1;
	else
	    return pw->pw_gid;
    }

    is_numeric = (sscanf(grouplabel, "%d%c", &numeric_gid, &c) == 1);

    if (is_numeric)
	return numeric_gid;

    /* Grouplabel didn't look like a number (according to sscanf),
     * so look up its name.
     */
    setgrent();
    for (found_gid = -1, gp = getgrent(); gp; gp = getgrent()) {
	if (strcmp(grouplabel, gp->gr_name) == 0) {
	    /* Found the gid in the group file */
	    found_gid = gp->gr_gid;
	    break;
	}
    }
    endgrent();

    return found_gid;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Adds condition to condition list.
 *	returns -1 on syntax error, malloc error, etc;
 *	returns 0 otherwise.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
InsertCondition(condition, s, isglobal)
char *condition;	/* condition to insert: xxx~yyy */
char *s;		/* pts to yyy in condition */
int isglobal;		/* Is this a per-command or global condition? */
{
    char **globlist;
    TimeList *tl;
    int i;
    int invert = (*condition == '!');

    if ( invert )
	condition++;

    /* All conditions accept {a,b,c}  or a,b,c  as lists.
     * Form the globlist and pass along...
     */
    /* Do brace globbing */
    if ((i=globbraces(s, 1, &globlist)) != 0)		/* Local Condition */
	return Error(0, 0, "%tMissing `%c'.\n", i);

    if (STRMATCH3("time", condition, s-1)) {
	tl = isglobal ? &globalinfo.timeafter : &localinfo.time;
	if (InsertTimeList(s, globlist,
			tl, isglobal ? "global" : "local", invert) == -1)
	    return -1;

    } else if (STRMATCH3("user", condition, s-1)) {
	if (InsertUserList(s, globlist,
		    &localinfo.userpats, &localinfo.origtext, invert) == -1)
	    return -1;

    } else {
	return Error(0, 0,
		"%t\n\tInternal error: unrecognized condition <%s>.\n",
		condition);
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Add a user/group/host pattern to a list.
 *	returns -1 on syntax error, malloc error, etc;
 *	returns 0 otherwise.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
InsertUserList(wd, wdlist, sl, otl, invert)
char *wd;	/* Pattern to match; must NOT have leading '!'; braces ok;
		 * disallow '<', '>'.
		 */
char **wdlist;	/* brace-expanded u/g/h list */
Simple2List *sl;/* Insert user list elements (i.e. argument wdlist)
		 * at sl->next.
		 */
SimpleList *otl;/* Insert original text (i.e. argument wd) * at otl->next.  */
int invert;	/* Inverts the test */
{
    int iwd;
    char *tok, *s;
    SimpleList *new;
    Simple2List *new2;

    /* Check for illegal characters */
    if ((s=strchr(wd, '>')) || (s=strchr(wd, '<'))) {
	if (s-wd == 4 && strncmp(wd, "time", 4) == 0) {
	    return Error(0, 0,
	    "%t\n\tPermittedUser patterns may not use '>' or '<';\n\
\tyou used '%s'; perhaps you meant to write 'time~%s'\n", wd, s);
	} else {
	    return Error(0, 0,
	    "%t\n\tPermittedUser patterns may not use '>' or '<';\n\
\tyou used '%s'.\n", wd);
	}
    }
    new = (SimpleList *) malloc(sizeof(SimpleList));
    if (!new)
	return Error(0, 0, "%t\n\tFailed to malloc space for PermittedUser\n");
    new->next = otl->next;
    new->pat = (char *) malloc(strlen(wd) + 1);
    if (!new->pat)
	return Error(0, 0,
		"%t\n\tFailed to malloc space for PermittedUser pat\n");
    strcpy(new->pat, wd);
    otl->next = new;

    for (iwd=0; (tok=wdlist[iwd]); iwd++) {
	new2 = (Simple2List *) malloc(sizeof(Simple2List));
	if (!new2)
	    return Error(0, 0,
		"%t\n\tFailed to malloc space for PermittedUser\n");
	new2->next = sl->next;
	new2->other = otl->next;
	new2->pat = (char *) malloc(strlen(tok) + (invert ? 2 : 1));
	if (!new2->pat)
	    return Error(0, 0,
		"%t\n\tFailed to malloc space for PermittedUser pat\n");
	if (invert) {
	    *new2->pat = '!';
	    strcpy(new2->pat+1, tok);
	} else {
	    strcpy(new2->pat, tok);
	}
	sl->next = new2;
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Match a list of user/group/host pattern against the present user. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
match_ugh_user(sl, isglobal)
Simple2List *sl;/* A list of user pats to match against, starting at sl->next */
int isglobal;	/* !0 means its from a global def */
{
    /* Checks if user is matched against each elt in sl.
     * Sets matches.match_user if user matches; sets it to 1 if the last
     * match is non-inverting, 0 otherwise.

     * BUT! the list created by the InsertUserList function
     * is in reverse order, so we only need to find the first
     * entry in the list that is a match (+ or -) and stop there!
     */

    int invert, match;
    int check_ugh __P((char *, char *));

    for (match=0, sl=sl->next; sl && !match; sl=sl->next) {
	invert = *sl->pat == '!';
	if (check_ugh(sl->other->pat, invert ? sl->pat+1 : sl->pat) == 0) {
	    match = 1;
	    matches.user = invert ? 0 : 1;
	    if (debug)
		(void) fprintf(stderr,
			"\tPermission %s: %s pattern %suser~%s\n",
			invert ? "denied" : "allowed",
			isglobal ? "global" : "per-cmd",
			invert ? "!" : "", sl->pat);
	} else if (debug) {
	    (void) fprintf(stderr, "\tNot applicable: %s pattern %suser~%s\n",
			isglobal ? "global" : "per-cmd",
			invert ? "!" : "", sl->pat);
	}
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Check pattern against a hostname.  If the hostname is fully-qualified,
 * then try stripping off each of the domains to find a match.
 * Return -1 on failure to match; 0 on success.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
check_host(pat, host)
char *pat, *host;
{
    int is_netgroup = 0;
    int match;
    char *p, *dotp;

    if (*pat == '+') {
       is_netgroup = 1;
       ++pat;
    }

    match = (is_netgroup ? netgrp_h_compare(pat, host)
						: (*pat_compare)(host));

    dotp = strrchr (host, '.');
    while (dotp && !match) {
       *dotp = 0;
       match = (is_netgroup ? netgrp_h_compare(pat, host)
						: (*pat_compare)(host));
       p = strrchr (host, '.');
       *dotp = '.';
       dotp = p;
    }
    return (match ? 0 : -1);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Try to match a string to a pattern. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
match_pattern(match, do_glob, str, pattern)
int match;	/* Return input value of match on failure; 1 on success */
int do_glob;	/* 0: no brace globbing;
		 * 1: brace glob;
		 * >1: wrap in braces, then brace-glob.
		 */
char *str;
char *pattern;
{
    int i, ipat, wrap;
    char *tok, tokbuf[1000];
    char **patlist, *pat1list[2];
    char chkbuf[1024];


    if (do_glob != 0) {
	/* Do brace globbing on the pattern */
	wrap = (do_glob > 1) ? 1 : 0;
	if ((i=globbraces(pattern, wrap, &patlist)) != 0) {	/* MatchPat */
	    Error(0, 0, "%tMissing `%c'.\n", i);
	    return match;
	}
    } else {
	pat1list[0] = pattern;
	pat1list[1] = NULL;
	patlist = pat1list;
    }

    for (ipat=0; (tok=patlist[ipat]); ipat++) {
	strcpy(tokbuf, tok);
	anchor(tok, chkbuf);			/* Anchor all matches */
	if ((*pat_compile)(chkbuf) != NULL) {
	    Error(0, 0, "%t\n\tBad command pattern: `%s'.\n", pattern);
	    return match;

	} else if ((*pat_compare)(str) == 1) {
		if (debug)
		    (void) fprintf(stderr,
			"\tMatched user's command=%s to CmdPattern=%s\n",
			str, pattern);
		return 1;

	} else if (debug) {
	    (void) fprintf(stderr,
		    "\tNo match user's command=%s to CmdPattern=%s\n",
			str, pattern);
	}
    }
    return match;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Check a single user/group/host string
 * Return -1 on failure to match; 0 on success.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
check_ugh(origtext, token)
char *origtext;		/* original text -- for error messages */
char *token;		/* user/group/host pattern */
{
    char chkbuf[1024];
    char *userpat, *grouppat, *hostpat;
    int match, i;

    if (userinfo.caller.pw_uid == 0)
	return 0;			/* root is always legit */

    /* Split into user:group@host; check host part first (if it exists) */
    if ((hostpat = strchr(token, '@'))) {
	if (hostpat[1] == 0)
	return Error(0, 0, "%tMissing hostname in pattern `%s'.\n", origtext);
	*hostpat++ = 0;
 
	match = -1;
	if (hostpat[0] == '+') {

#ifdef HAVE_INNETGR
	    if (hostpat[1] == 0)
		return Error(0, 0,
			"%tMissing netgroupname in pattern `%s'.\n", origtext);
	    match = check_host(hostpat, userinfo.hostname);
#else
		return Error(0, 0, 
	"%thostnames may not begin with `+' since this super() was compiled\n\
without -DHAVE_INNETGR.\n");
#endif
	} else {
	    strtolower(hostpat);
	    anchor(hostpat, chkbuf);    /* Force all matches to be anchored */
	    if ((*pat_compile)(chkbuf) != NULL)
		return Error(0, 0, "%tbad host pattern: `%s'.\n", origtext);
	}
 
	if (match == -1)
	    match = check_host(hostpat, userinfo.lc_hostname);

	if (debug > 1)
	    fprintf(stderr,
		"\thost pattern <%s> %s user's host <%s>\n",
		hostpat, (match == -1) ? "did not match" : "matched",
		userinfo.lc_hostname);

	if (match == -1)
	    return -1;
    }

    grouppat = strchr(token, ':');
    userpat = token;
    if (*token == '\0' && !hostpat) {
	/* Nothing in pattern?! */
	return Error(0, 0, "%t\n\tUnacceptable pattern `%s'.\n", origtext);

    } else if (*token == '\0') {
	userpat = grouppat = "^.*$";		/* only hostname given */

    } else if (grouppat && *(grouppat+1)) {	/* pat is "uuu:ggg or ":ggg" */
	if (token == grouppat)
	    userpat = "^.*$";			/* pat is ":ggg" */
	*grouppat++ = '\0';

    } else {					/* pat is "uuu" or "uuu:" */
	if (grouppat)
	    *grouppat = '\0';			/* pat is "uuu:" */
	grouppat = "^.*$";
    }
    if (strchr(grouppat, ':'))
	return Error(0, 0, "%t\n\tGroup pattern `%s' contains a colon!\n",
			grouppat);

    if (globalinfo.group_slash == 0 && strchr(grouppat, '/'))
		return Error(0, 0, "%t\n\tFormat error in super.tab file: \
group pattern `%s' contains a slash.\n\
\tPerhaps you meant to use Cmd::Filename, but forgot one colon,\n\
\tso it looks like User:Group?  If you really need to allow\n\
\tslashes in group patterns, use global option group_slash=y.\n", grouppat);


#ifdef HAVE_INNETGR
    if (userpat[0] == '+') {
	if (userpat[1] == 0)
	    return Error(0, 0,
			"%tMissing netgroupname in pattern `%s'.\n", origtext);
	match = netgrp_u_compare(&userpat[1], userinfo.caller.pw_name);
    } else
#endif
    {
	anchor(userpat, chkbuf);		/* Anchor all matches */
	if ((*pat_compile)(chkbuf) != NULL)
	    return Error(0, 0, "%t\n\tbad user pattern: `%s'.\n", origtext);
	match = (*pat_compare)(userinfo.caller.pw_name);
#ifdef MATCH_DECIMAL_UID
	if (match != 1) {
	    /* Enabling MATCH_DECIMAL_UID allows the userpat to be
	     * numeric, as an alternative to being interpreted as a
	     * user name: after checking the username, we check if the
	     * user's uid, as a decimal text value, matches the user
	     * pattern userpat.
	     */
	    char buf[20];
	    (void) sprintf(buf, "%d", userinfo.caller.pw_uid);
	    match = (*pat_compare)(buf);
	}
#endif
    }
    if (debug > 1)
	fprintf(stderr,
	    "\tuser pattern <%s> %s username <%s>\n",
	    userpat, (match != 1) ? "did not match" : "matched",
	    userinfo.caller.pw_name);

    if (match != 1)
	return -1;

    anchor(grouppat, chkbuf);
    i = ingroup(userinfo.caller.pw_name, userinfo.caller.pw_gid, chkbuf);
    if (i == -1)
	return Error(0, 0, "%t\n\tbad group pattern\n", origtext);

    if (debug > 1)
	fprintf(stderr,
	    "\tuser <%s> is %sa member of group <%s>\n",
	    userinfo.caller.pw_gid, (i != 1) ? "not " : "", grouppat);

    if (i != 1)
	return -1;
    
    return 0;				/* Success! */
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Determines if user's group matches a group pattern. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
ingroup(user, gid, gp_pat)
char *user;
gid_t gid;
char *gp_pat;	/* pattern to match */
{
    /* Use:
     *	ingroup(user, gid, gp_pat)
     * Returns:
     *	1 if the user is in a group matching the regex pattern gp_pat.
     *	0 if the user isn't in a group matching the pattern.
     *	-1 if pattern failed to compile.

     * SIDE-EFFECT: uses pat_compile/pat_compare!
     *			-- messes up caller's use of same!

     * Examples:
     *	ingroup("joe", joes_gid, "xyz")
     * returns !0 if user joe is in group "xyz".
     *	ingroup("joe", joes_gid, "xy.*")
     * returns !0 if user joe is in any group matching "xy.*".

     */

    struct group *gp;
    char **mem;
    char buf[20];
    SETGRENT_T setgrent();
    void endgrent();

    if ((*pat_compile)(gp_pat) != (char *)0 )
	return -1;

    /* Search group file for groups user is in.  For each group of which
     * the user is a member, test a match to the pattern.
     */
    setgrent();
    for (gp = getgrent(); gp; gp = getgrent()) {
	/* The gr_mem list only shows usernames added in the /etc/group file,
	 * and not any users assigned to the group in the passwd file.
	 * Thus discover group membership by first checking the user's
	 * group from the password file (gp->gr_gid) against this group's
	 * gid, then check to see if this user is in the gp->gr_mem list.
	 */
	if (gid != gp->gr_gid) {
	    for (mem = gp->gr_mem; *mem ; mem++)
		if (strcmp(*mem, user) == 0)
		    break;
	    if (!*mem)
		continue;			/* not in group */
	}
	/* if here, the user is in group gp; now check if group
	 * name gp->gr_name matches group pattern gp_pat.
	 */
	if ((*pat_compare)(gp->gr_name) == 1) {
	    /* successful match -- user is in a group that matches gp_pat */
	    endgrent();
	    return 1;
	}
#ifdef MATCH_DECIMAL_GID
	else {
	    /* Enabling MATCH_DECIMAL_GID allows the gp_pat to be
	     * numeric, as an alternative to being interpreted as a
	     * group name: we check if the group id gp->gr_gid, as a
	     * decimal text value, matches the group pattern gp_pat.
	     */
	    (void) sprintf(buf, "%d", gp->gr_gid);
	    if ((*pat_compare)(buf) == 1){
		/* successful match -- user is in a group that matches gp_pat */
		endgrent();
		return 1;
	    }
	}
#endif
    }

#ifdef MATCH_DECIMAL_GID
    /* We haven't found any group from /etc/group to which we belong that
     * matches the pattern.  It is possible that the user's group id from the
     * password file isn't in the /etc/group file at all, in which case the
     * user's group won't have matched the pattern since we've only checked
     * /etc/group entries so far.  Now check the numeric id from the
     * /etc/passwd file against the pattern.
     */
    (void) sprintf(buf, "%d", gid);
    if ((*pat_compare)(buf) == 1){
	endgrent();
	return 1;
    }
#endif

    endgrent();
    return 0;
}

