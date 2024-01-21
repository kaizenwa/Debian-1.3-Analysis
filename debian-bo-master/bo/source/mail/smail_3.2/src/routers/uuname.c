/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:uuname.c,v 1.6 1996/02/26 18:38:14 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * uuname.c:
 *	routing driver which obtains names from the "uuname" command.
 *	This driver is useful in default routers that have previously
 *	not used any router programs.  The pathalias router, should
 *	be used in preference to the uuname router.
 *
 * Specifications of the uuname routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.  In general, this should
 *	    be used with a uux transport, such as uux or demand.
 *
 *	private data:
 *	    cmd		- shell command to execute
 *	    domain	- domain to strip from the end of the target
 *	    required	- domain which is required to be on end of the target
 *	    statfile	- a file to stat for detecting change in status
 *
 *	private flags: none.
 *
 *	algorithm:
 *	    Obtain a list of hostnames from the output of the given shell
 *	    command.  If the target ends in the domain, strip it away
 *	    then match (in full) for any hostname in the command output.
 *	    Any initial dot in the target is ignored.
 *
 *	    Always returns one-hop routes; i.e., a next_host value is
 *	    returned, but no route.
 *
 *	notes:
 *	    The output of the command is cached.
 */
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../dys.h"
#include "../lookup.h"
#include "../child.h"
#include "rtlib.h"
#include "uuname.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static int uuname_lookup();
static int scan_uuname();
static int read_command();
static struct error *open_error();
static struct error *scan_error();

static struct attr_table uuname_attributes[] = {
    { "cmd", t_string, NULL, NULL, OFFSET(uuname_private, cmd) },
    { "domain", t_string, NULL, NULL, OFFSET(uuname_private, domain) },
    { "required", t_string, NULL, NULL, OFFSET(uuname_private, required) },
    { "statfile", t_string, NULL, NULL, OFFSET(uuname_private, statfile) },
};
static struct attr_table *end_uuname_attributes =
    ENDTABLE(uuname_attributes);


/*
 * rtd_uuname - uuname router driver
 */
void
rtd_uuname(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, uuname_lookup);
}

/*
 * rtv_uuname - verify that a match exists in the output of a command
 */
void
rtv_uuname(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, uuname_lookup);
}

/*
 * rtf_uuname - free resources used by a uuname router
 */
void
rtf_uuname(rp)
    struct router *rp;			/* router entry */
{
    register struct uuname_private *priv;

    priv = (struct uuname_private *)rp->private;
    rp->flags &= ~(UU_FILE_FAIL | UU_FILE_AGAIN | UU_CACHED);
    if (priv->error_text) {
	xfree(priv->error_text);
    }
    if (priv->cmd_output) {
	xfree(priv->cmd_output);
    }
}

/*
 * rtc_uuname - cache the output of a command, if there is a statfile
 */
void
rtc_uuname(rp)
    struct router *rp;			/* router entry */
{
    struct uuname_private *priv;
    struct stat statbuf;
    int success;

    priv = (struct uuname_private *)rp->private;
    if (priv->statfile) {

	/* we are caching and there is a stat file */
	if (stat(priv->statfile, &statbuf) < 0) {

	    /* the stat failed, try again later */
	    rtf_uuname(rp);
	    priv->error_text =
		xprintf("router %s: cannot stat %s: %s",
			rp->name, priv->statfile, strerror(errno));
	    DEBUG1(DBG_DRIVER_MID, "%s\n", priv->error_text);
	    rp->flags |= UU_FILE_AGAIN;
	    return;
	}
	if (rp->flags & UU_CACHED) {
	    if (statbuf.st_ino == priv->stat_ino &&
		statbuf.st_mtime == priv->stat_mtime)
	    {
		/* stat file has not changed */
		return;
	    }

	    /* free resources used by previous cache */
	    rtf_uuname(rp);
	}
	priv->stat_ino = statbuf.st_ino;
	priv->stat_mtime = statbuf.st_mtime;

	rp->flags |= UU_CACHED;

	/* file has changed, or this is first time, cache command output */
	success = read_command(priv, &priv->error_text);
	switch (success) {

	case FILE_FAIL:
	    DEBUG1(DBG_DRIVER_MID, "%s\n", priv->error_text);
	    rp->flags |= UU_FILE_FAIL;
	    break;

	case FILE_AGAIN:
	    DEBUG1(DBG_DRIVER_MID, "%s\n", priv->error_text);
	    rp->flags |= UU_FILE_AGAIN;
	    break;
	}
    }
}

/*
 * rtb_uuname - read the configuration file attributes
 */
char *
rtb_uuname(rp, attrs)
    struct router *rp;			/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct uuname_private uuname_template = {
	"/usr/bin/uuname",		/* cmd */
	NULL,				/* domain */
	NULL,				/* required */
	NULL,				/* cmd_output - for internal use */
    };
    struct uuname_private *priv;	/* new uuname_private structure */

    /* copy the template private data */
    priv = (struct uuname_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&uuname_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    uuname_attributes,
			    end_uuname_attributes);

    if (error) {
	return error;
    } else if (priv->cmd == NULL) {
	return "No cmd attribute";
    } else {
	return NULL;
    }
}

/*
 * rtp_uuname - dump the configuration attributes
 */
void
rtp_uuname(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				uuname_attributes,
				end_uuname_attributes);
}



/*
 * uuname_lookup - lookup a host in the standard output of a command
 *
 * Use the algorithm described at the top of this source file for
 * finding a match for a target.
 *
 * Return one of the following values:
 *
 * These return codes apply only to the specific address:
 *	DB_SUCCEED	Matched the target host.
 *	DB_NOMATCH	Did not match the target host.
 *	DB_FAIL		Fail the address with the given error.
 *	DB_AGAIN	Try to route with this address again at a
 *			later time.
 *
 * These return codes apply to this router in general:
 *	FILE_AGAIN	The command could not be executed, Try again later.
 *	FILE_FAIL	A major error has been caught in router,
 *			notify postmaster.
 */
/*ARGSUSED*/
static int
uuname_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    register struct uuname_private *priv;
    char *host_part;			/* match the first domain part */
    char *match;			/* the match that was found */
    char *domain_part = NULL;		/* match the second domain part */
    int success;

    priv = (struct uuname_private *)rp->private;

    if (priv->required) {
	if (match_end_domain(priv->required, addr->target) == NULL) {
	    return DB_NOMATCH;
	}
    }

    host_part = addr->target;
    if (host_part[0] == '.') {
	host_part++;		/* ignore initial dot */
    }

    /*
     * strip any optional domain
     */
    if (priv->domain) {
	domain_part = match_end_domain(priv->domain, host_part);
	if (domain_part) {
	    DEBUG1(DBG_DRIVER_HI, "strip \"%s\"\n", domain_part);
	    domain_part[0] = '\0';
	}
    }

    if (rp->flags & UU_FILE_FAIL) {
	success = FILE_FAIL;
    } else if (rp->flags & UU_FILE_AGAIN) {
	success = FILE_AGAIN;
    } else {
	/* look for a match */
	success = scan_uuname(priv, host_part, &match, &priv->error_text);
    }

    /* found a match, finish routing */
    if (domain_part) {
	*domain_part = '.';
    }
    switch (success) {
    case DB_SUCCEED:
	rt_info->next_host = match;
	rt_info->route = NULL;
	rt_info->matchlen = strlen(addr->target);
	/* FALLTHRU */

    case DB_NOMATCH:
	return success;

    case FILE_AGAIN:
	rp->flags |= UU_FILE_AGAIN;
	*error_p = open_error(rp, priv->error_text);
	return success;

    case FILE_FAIL:
	rp->flags |= UU_FILE_FAIL;
	/* FALLTHRU */

    default:
	*error_p = scan_error(rp, priv->error_text);
	return success;
    }
}

/*
 * scan_uuname - scan the output of the command for a specific host.
 *
 * return the matching hostname in *match.  Return an exit code
 * from ../lookup.h describing the result, with an error stored
 * in *error for errors.
 */
static int
scan_uuname(priv, host, match, error)
    struct uuname_private *priv;	/* router's private storage */
    char *host;				/* host to scan for */
    char **match;			/* store match here */
    char **error;			/* store an error here */
{
    register char *p;
    char *last;
    int success;

    if (priv->cmd_output == NULL) {
	/* read it only once */
	success = read_command(priv, error);
	if (success != FILE_SUCCEED) {
	    return success;
	}
    }

    /*
     * scan through until there are no more lines, or until we find a match
     */
    p = priv->cmd_output;
    last = priv->cmd_output_end;
    while (p < last) {
	if (EQIC(host, p)) {
	    /* found a match */
	    *match = p;
	    return DB_SUCCEED;
	}
	p += strlen(p) + 1;
    }

    /* no match */
    return DB_NOMATCH;
}

/*
 * read_uuname - obtain the output of uuname as a vector of names
 */
static int
read_command(priv, error)
    struct uuname_private *priv;	/* router's private storage */
    char **error;			/* return error message here */
{
    FILE *f;
    struct str str;			/* string region */
    register int c;			/* character from uuname command */
    static char *argv[] = {		/* args for shell */
	"/bin/sh",
	"-c",
	NULL,
	NULL,
    };
    int pid;				/* pid of child process */
    int status;				/* exit status from child */
    int ferrorret;			/* return value from ferror() */

    DEBUG1(DBG_DRIVER_MID, "scanning %s output ...", priv->cmd);
    argv[2] = priv->cmd;
    pid = open_child(argv, (char **)NULL, (FILE **)NULL, &f,
		     errfile? fileno(errfile): -1,
		     CHILD_MINENV, nobody_uid, nobody_gid);
    if (pid == EOF) {
	*error = xprintf("process creation for `%s': %s",
			 priv->cmd, strerror(errno));
	DEBUG(DBG_DRIVER_MID, "error\n");
	return FILE_AGAIN;
    }

    /*
     * read in the hostnames and count how many there are
     */
    STR_INIT(&str);
    while ((c = getc(f)) != EOF) {
	if (c == '\n') {
	    STR_NEXT(&str, '\0');
	} else {
	    STR_NEXT(&str, c);
	}
    }
    STR_DONE(&str);
    ferrorret = ferror(f);
    status = close_child((FILE *)NULL, f, pid);
    if (status != 0) {
	STR_FREE(&str);
	*error = xprintf("command `%s' returned exit status %s",
			 priv->cmd, strsysexit(status));
	DEBUG(DBG_DRIVER_MID, "error\n");
	return FILE_FAIL;
    }
    if (ferrorret) {
	STR_FREE(&str);
	*error = xprintf("read error in output from `%s'", priv->cmd);
	DEBUG(DBG_DRIVER_MID, "error\n");
	return FILE_AGAIN;
    }
    if (str.i > 0 && str.p[str.i - 1] != '\0') {
	STR_FREE(&str);
	*error = xprintf("output of `%s' does not end in newline", priv->cmd);
	DEBUG(DBG_DRIVER_MID, "error\n");
	return FILE_FAIL;
    }

    /* save the scanned information for future lookups */
    priv->cmd_output = str.p;
    priv->cmd_output_end = str.p + str.i;

    DEBUG(DBG_DRIVER_MID, "done\n");

    return FILE_SUCCEED;		/* all done */
}


static struct error *
open_error(rp, open_error_text)
    struct router *rp;
    char *open_error_text;
{
    char *error_text;

    /*
     * ERR_170 - process creation error
     *
     * DESCRIPTION
     *      An error was encountered while trying to create the
     *      process for executing the shell command.
     *
     * ACTION
     *      Try again later.
     *
     * RESOLUTION
     *      Generally this implies that the system was out of some
     *      resource, such as process slots, memory or file table
     *      entries.  Such problems go away in time.
     */
    error_text = xprintf("router %s: %s", rp->name, open_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_170, error_text);
}

static struct error *
scan_error(rp, scan_error_text)
    struct router *rp;
    char *scan_error_text;
{
    char *error_text;

    /*
     * ERR_127 - uuname scanning error
     *
     * DESCRIPTION
     *      An error was encountered while scanning the command
     *      output in the uuname driver.  The specific error
     *      message is stored in `error'.
     *
     * ACTIONS
     *      Dependent upon specific error.
     *
     * RESOLUTION
     *      The postmaster should check the validity of the
     *      uuname driver entry.  If this is correct, the
     *      command whose output is scanned should be checked.
     */
    error_text = xprintf("router %s: %s", rp->name, scan_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_127, error_text);
}
