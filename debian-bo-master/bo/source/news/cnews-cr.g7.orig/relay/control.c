/*
 * NCMP (netnews control message protocol).
 * Implement the Usenet control messages, as per RFCs 1036 and 850.
 * These are fairly infrequent and can afford to be done by
 * separate programs.  They are:
 *
 * control messages that (request a) change (in) the local system:
 *	cancel message-ID(s)		restricted to Sender: else From: (or
 *					root?), in theory
 *	newgroup groupname [moderated]	must be Approved:
 *	rmgroup groupname		must be Approved:;
 *					allow some local control
 *	checkgroups			harass newsadmin about "deviations"
 *					in active; incompletely specified
 *
 * control messages that cause mail back to Reply-To: else From:
 *	sendsys [site]
 *	version
 *
 * the "ihave/sendme" protocol to minimise traffic volume and maximise delay
 * between this site and another
 *	ihave [message-ID-list] remotesys	generate a sendme for remotesys
 *						from message-ID-list
 *	sendme [message-ID-list] remotesys	send articles named to remotesys
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>

#include "libc.h"
#include "news.h"
#include "case.h"
#include "config.h"
#include "headers.h"
#include "relay.h"
#include "active.h"
#include "history.h"

#define DEFMSGIDS 10			/* default msgids for awksplit */

#define NO_FILES ""
#define SUBDIR binfile("ctl")		/* holds shell scripts */

/*
 * These are shell meta-characters, except for /, which is included
 * since it allows people to escape from the control directory.
 */
#define SHELLMETAS "\\<>|^&;\n({$=*?[`'\"/"

/* imports from news */
extern statust snufffiles(); 
extern void ihave(), sendme();

/*
 * In theory (RFC 1036 nee 850), we should verify that the user issuing the
 * cancel (the Sender: of this article or From: if no Sender) is the
 * Sender: or From: of the original article or the local super-user.
 *
 * In practice, this is a lot of work and since anyone can forge news (and
 * thus cancel anything), not worth the effort.  Furthermore, there is no
 * known precise algorithm for matching addresses (given the address
 * mangling performed by old news systems).
 *
 * Ignore ST_ACCESS while cancelling an already-seen article since the
 * article may have been cancelled before or may have a fake history entry
 * because the cancel arrived before the article.
 *
 * If the article being cancelled has not been seen yet, generate a history
 * file entry for the cancelled article in case it arrives after the cancel
 * control.  The history file entry will cause the cancelled article to be
 * rejected as a duplicate.
 */
STATIC statust
cancelart(msgidstr)
char *msgidstr;
{
	register char *msgid = strsave(msgidstr);
	register statust status = ST_OKAY;

	if (msgid[0] == '\0')
		;
	else if (alreadyseen(msgid)) {
		register char *histent, *filelist;

		histent = gethistory(msgid);
		if (histent != NULL && (filelist = findfiles(histent)) != NULL)
			status |= snufffiles(filelist) & ~ST_ACCESS;
	} else {
		status |= fakehist(msgid, DEFEXP, NO_FILES);	/* start log */
		(void) putchar('\n');			/* end log line */
	}
	free(msgid);
	return status;
}

STATIC statust
cancel(msgids)
char *msgids;
{
	register int msgidcnt, i;
	register statust status = ST_OKAY;
	char *msgid[DEFMSGIDS];
	char **msgidp = msgid;
	char *msgidcpy = strsave(msgids);

	msgidcnt = awksplit(msgidcpy, &msgidp, DEFMSGIDS, " \t\n");
	if (msgidp == NULL) {
		persistent(NOART, 'm', "awksplit failed to allocate memory",
			"");
		status |= ST_DROPPED|ST_NEEDATTN;
	} else {
		for (i = 0; i < msgidcnt; i++)
			status |= cancelart(msgidp[i]);
		if (msgidp != msgid)
			free((char *)msgidp);
	}
	free(msgidcpy);
	return status;
}

/*
 * log the failure of cmd with status cmdstat, and _exit with bad status
 * (again avoid stdio buffer flushing in the child).
 */
/* ARGSUSED cmdstat */
STATIC void
bombctlmsg(cmd, cmdstat)
char *cmd;
int cmdstat;
{
	register char *mailcmd;

	mailcmd = str3save("PATH=", newspath(), " ; report 'ctl msg failure'");
	if (mailcmd == NULL) {
		persistent(NOART, 'm', "can't allocate memory in bombctlmsg",
			   "");
		(void) fflush(stderr);
		_exit(1);
	}

	logaudit(NOART, 'c', "control message `%s' failed", cmd);
#ifdef notdef
	{
		/* TODO: don't do this */
		register FILE *mailf = popen(mailcmd, "w");

		if (mailf == NULL)
			mailf = stderr;
		(void) fprintf(mailf,
			       "%s: control message `%s' exited with status 0%o\n",
			       progname, cmd, cmdstat);
		(void) fflush(mailf);
		if (mailf != stderr)
			(void) pclose(mailf);
	}
#endif					/* notdef */

	free(mailcmd);
	_exit(1);
}

boolean
safecmd(cmd)			/* true if it's safe to system(3) cmd */
register char *cmd;
{
	register char *s;

	for (s = cmd; *s != '\0'; s++)
		if (STREQN(s, "..", STRLEN("..")))
			return NO;
	for (s = SHELLMETAS; *s != '\0'; s++)
		if (strchr(cmd, *s) != NULL)
			return NO;
	return YES;
}

/*
 * Execute a non-builtin control message by searching $NEWSCTL/bin and
 * $NEWSBIN/ctl for the command named by the control message.
 * runctlmsg is called from a child of relaynews, so it must always
 * call _exit() rather than exit() to avoid flushing stdio buffers.
 *
 * Enforce at least minimal security: the environment was standardised at
 * startup, including PATH and IFS; close non-standard file descriptors;
 * reject shell metacharacters in ctlcmd.
 */
STATIC void
runctlmsg(ctlcmd, inname)			/* child process */
register char *ctlcmd, *inname;
{
	register char *cmd, *s1, *s2, *s3;
	register int cmdstat;

	nolock();
	closeall(1);
	if (!safecmd(ctlcmd)) {
		errno = 0;
		logaudit(NOART, 'c', "control `%s' looks unsafe to execute",
			 ctlcmd);
		(void) fflush(stderr);
		_exit(0);		/* it's okay; happens all the time */
	}
	s1 = str3save("PATH=", ctlfile("bin"), ":");
	s2 = str3save(SUBDIR, "; ", "");
	s3 = str3save(ctlcmd, " <", inname);
	cmd = str3save(s1, s2, s3);
	free(s1);
	free(s2);
	free(s3);
	/* TODO: use fork, putenv, exec[vl]p here instead of system? */
	cmdstat = system(cmd);
	if (cmdstat != 0)
		bombctlmsg(cmd, cmdstat);
	free(cmd);
	_exit(0);
}

STATIC boolean
ismsgnamed(line, msg)
register char *line;
register char *msg;
{
	register int msglen = strlen(msg);

	return STREQN(line, msg, msglen) &&
	    isascii(line[msglen]) && isspace(line[msglen]);
}

/*
 * Implement control message specified in "art".
 * Because newgroup and rmgroup may modify the active file, for example,
 * we must flush in-core caches to disk first and reload them afterward.
 * We handle cancels in this process for speed and dbm read/write access.
 * We handle ihave & sendme in this process for dbm read access and
 * to work around syntax restrictions (<>).
 *
 * In future, one could pass header values to scripts as arguments or
 * in environment, as NEWS* variables, to save time in the scripts.
 */
void
ctlmsg(art)
register struct article *art;
{
	register char *inname = art->a_tmpf, *ctlcmd = art->h.h_ctlcmd;
	int pid, deadpid;
	int wstatus;
	static char nmcancel[] = "cancel";
	static char nmihave[] =  "ihave";
	static char nmsendme[] = "sendme";

	/* anything to do? */
	if (ctlcmd == NULL)
		ctlcmd = art->h.h_etctlcmd;
	if (ctlcmd == NULL)
		return;

	/* internal ctl msg (cancel, cancel typo, ihave, sendme)? */
	if (ismsgnamed(ctlcmd, nmcancel)) {
		art->a_status |= cancel(ctlcmd + STRLEN(nmcancel));
		return;
	}
	if (ctlcmd[0] == '<' || CISTREQN(ctlcmd, nmcancel, STRLEN(nmcancel)) &&
	    (!isascii(ctlcmd[STRLEN(nmcancel)]) ||
	     !isalpha(ctlcmd[STRLEN(nmcancel)]))) {
		/* should really just log this */
		errno = 0;
		logaudit(NOART, 'c', "malformed cancel `%s'", ctlcmd);
		/* no need to return bad status; happens all the time */
		return;
	}
	if (ismsgnamed(ctlcmd, nmihave)) {
		ihave(ctlcmd + STRLEN(nmihave), art);
		return;
	}
	if (ismsgnamed(ctlcmd, nmsendme)) {
		sendme(ctlcmd + STRLEN(nmsendme), art);
		return;
	}

	/* external ctl msg: flush active and stdio */
	art->a_status |= actsync();
	(void) fflush(stdout);
	(void) fflush(stderr);

	pid = fork();
	if (pid == 0)				/* child? */
		runctlmsg(ctlcmd, inname);
	else if (pid == -1)
		persistent(art, 'f', "fork failed", "");

	/* lint complains about &wstatus on 4.2+BSD; too bad, lint's wrong. */
	while ((deadpid = wait(&wstatus)) != pid && deadpid != -1)
		;

	/* wrong kid returned, fork failed or child screwed up? */
	if (deadpid == -1 || pid == -1 || wstatus != 0)
		transient(art, '\0', "", "");
			/* ctl msg failed; admin got err.msg. by mail */
	/* let lazy evaluation load the caches */
}

char *
hackhybrid(line)
register char *line;
{
	static char stupersedes[] = "Supersedes:";
	static char alsocan[] =     "Also-Control: cancel ";

	return CISTREQN(line, stupersedes, STRLEN(stupersedes))?
	    str3save(alsocan, "", &line[STRLEN(stupersedes)]): strsave(line);
}
