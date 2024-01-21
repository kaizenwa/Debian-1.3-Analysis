/*
#ident	"@(#)smail/src/routers:RELEASE-3_2:queryprog.c,v 1.8 1996/02/28 06:47:56 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * queryprog.c:
 *
 * Specifications of the queryprogram routing driver:
 *
 *	associated transports:
 *	    No specific transport is set.
 *
 *	private data:
 *	    cmd		- command to execute, include $host for hostname
 *	    domain	- domains to strip from the end of the target
 *	    required 	- required domain names
 *	    hash_len	- hash slots in internal hash table
 *
 *	private flags:
 *	    read_path	- read a path from the stdout of the program
 *	    read_transport - read a transport from stdout of the program
 *
 *	algorithm:
 *	    Expand the given command, with $host expanding to the host
 *	    that is being routed.  Then execute that command and check
 *	    the exit status.  A zero exit status represents a match, a
 *	    non-zero exit status represents no match.
 *
 *	    Always returns one-hop routes; i.e., a next_host value is
 *	    returned, but no route.
 *
 *	notes:
 *	    The match status values are cached.
 */
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include "defs.h"
#include "../smail.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../addr.h"
#include "../route.h"
#include "../transport.h"
#include "../hash.h"
#include "../child.h"
#include "../lookup.h"
#include "../dys.h"
#include "rtlib.h"
#include "queryprog.h"
#ifndef DEPEND
# include "../extern.h"
# include "../debug.h"
# include "../error.h"
#endif

/* variables imported from libc */
extern int errno;

/* functions local to this file */
static int queryprogram_lookup();
static int decode_query_output();
static struct error *parse_error();
static struct error *temp_command_error();
static struct error *output_parse_error();

static struct attr_table queryprogram_attributes[] = {
    { "cmd", t_string, NULL, NULL, OFFSET(queryprogram_private, cmd) },
    { "domain", t_string, NULL, NULL, OFFSET(queryprogram_private, domain) },
    { "required", t_string, NULL, NULL, OFFSET(queryprogram_private, required) },
    { "hash_table_len", t_int, NULL, NULL, OFFSET(queryprogram_private, hash_table_len) },
    { "read_path", t_boolean, NULL, NULL, QP_READ_PATH },
    { "read_transport", t_boolean, NULL, NULL, QP_READ_TRANSPORT },
};
static struct attr_table *end_queryprogram_attributes =
    ENDTABLE(queryprogram_attributes);


/*
 * rtd_queryprogram - call a program to verify neighboring hostnames
 */
/* ARGSUSED */
void
rtd_queryprogram(rp, in, out, defer, fail)
    struct router *rp;			/* router table entry */
    struct addr *in;			/* input addr structures */
    struct addr **out;			/* non-failed addr structures */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    rtd_standard(rp, in, out, defer, fail, queryprogram_lookup);
}

/*
 * rtv_queryprogram - verify by calling a program.
 */
/*ARGSUSED*/
void
rtv_queryprogram(rp, in, retry, okay, defer, fail)
    struct router *rp;			/* router entry */
    struct addr *in;			/* input local-form addrs */
    struct addr **retry;		/* output list of unmatched addrs */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    rtv_standard(rp, in, retry, okay, defer, fail, queryprogram_lookup);
}

/*
 * rtb_queryprogram - read the configuration file attributes
 */
char *
rtb_queryprogram(rp, attrs)
    struct router *rp;			/* director entry being defined */
    struct attribute *attrs;		/* list of per-driver attributes */
{
    char *error;
    static struct queryprogram_private queryprogram_template = {
	"/usr/bin/uuinfo -q $host",	/* cmd */
	NULL,				/* domain */
	NULL,				/* required */
	20,				/* hash_table_len */
	NULL,				/* cache - for internal use */
    };
    struct queryprogram_private *priv;	/* new queryprogram_private struct */

    /* copy the template private data */
    priv = (struct queryprogram_private *)xmalloc(sizeof(*priv));
    (void)memcpy((char *)priv, (char *)&queryprogram_template, sizeof(*priv));

    rp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
			    attrs,
			    &rp->flags,
			    queryprogram_attributes,
			    end_queryprogram_attributes);

    if (error) {
	return error;
    } else {
	return NULL;
    }
}

/*
 * rtp_queryprogram - dump the configuration attributes
 */
void
rtp_queryprogram(f, rp)
     FILE * f;
     struct router *rp;
{
    (void) dump_standard_config(f,
				rp->private,
				rp->name,
				rp->flags,
				queryprogram_attributes,
				end_queryprogram_attributes);
}




/*
 * queryprogram_lookup - call a program to route to a target
 *
 * Use the algorithm described at the top of this source file for
 * finding a match for a target.
 *
 * Return one of the following values:
 *
 * These return codes apply only to the specific address:
 *	DB_SUCCEED	Matched the target host.
 *	DB_NOMATCH	Did not match the target host.
 *	DB_FAIL		Encountered a parse error in the command output.
 *
 * These return codes apply to this router in general:
 *	FILE_AGAIN	The process could not be created, retry later.
 *	FILE_FAIL	A major error has been caught in router,
 *			notify postmaster.
 */
/*ARGSUSED*/
static int
queryprogram_lookup(rp, addr, fl, rt_info, error_p)
    struct router *rp;			/* router table entry */
    struct addr *addr;			/* addr structure */
    int fl;				/* flags from rt[dv]_standard */
    struct rt_info *rt_info;		/* return route info here */
    struct error **error_p;		/* return lookup error here */
{
    static struct addr temp;		/* temp address */
    static char *next_host = NULL;	/* returned next_host, can be freed */
    char **cmd;				/* cmd vectors for child process */
    int pid;				/* pid of child process */
    FILE *f = NULL;			/* output of child, if desired */
    char *domain_part = NULL;		/* match the second domain part */
    char *error = NULL;			/* scan error */
    int status;				/* child's exit status */
    char *target = addr->target;
    struct queryprogram_private *priv;

    priv = (struct queryprogram_private *)rp->private;

    if (priv->required) {
	if (match_end_domain(priv->required, target) == NULL) {

	    /* did not end in a required domain */
	    return DB_NOMATCH;
	}
    }

    if (target[0] == '.') {
	target++;			/* ignore initial dot */
    }

    /*
     * strip any optional domain
     */
    if (priv->domain) {
	domain_part = match_end_domain(priv->domain, target);
	if (domain_part) {
	    DEBUG1(DBG_DRIVER_HI, "strip \"%s\"\n", domain_part);
	    domain_part[0] = '\0';
	}
    }

    /* build the query command, using the stripped target name */
    if (next_host) {
	xfree(next_host);
    }
    next_host = COPY_STRING(target);
    temp.next_host = next_host;
    cmd = build_cmd_line(priv->cmd, &temp, (char *) NULL, &error);
    if (domain_part) {
	*domain_part = '.';
    }

    if (cmd == NULL) {
	*error_p = parse_error(rp, priv->cmd, error);
	return FILE_FAIL;
    }

    /* create a child process and get its exit status */
    pid = open_child(cmd, (char **)NULL,
		     (FILE **)NULL,
		     rp->flags & (QP_READ_PATH | QP_READ_TRANSPORT)?
			&f: (FILE **)NULL,
		     -1, CHILD_DEVNULL | CHILD_MINENV | CHILD_NOCLOSE,
		     nobody_uid, nobody_gid);

    if (pid == -1) {
	/* temporary failure, try again at a later time */
	*error_p = temp_command_error(rp);
	return FILE_AGAIN;
    }

    rt_info->next_host = next_host;
    rt_info->matchlen = strlen(addr->target);

    if (f) {
	/* get path and/or transport from input line */
	char *output = read_entry(f);

	if (output && output[0] != '\0') {
	    if (decode_query_output(rp, output, rt_info, &error) == FAIL) {
		(void) close_child((FILE *)NULL, f, pid);
		*error_p = output_parse_error(rp, error);
		return DB_FAIL;
	    }
	}
    }

    /* get the child's status */
    status = close_child((FILE *)NULL, f, pid);

    return status==0? DB_SUCCEED: DB_NOMATCH;
}

/*
 * decode_query_output - decode the output from a queried command
 *
 * decode a transport and or a path returned by a queried command.
 * Return this information in an rt_info structure.
 */
static int
decode_query_output(rp, output, rt_info, error)
    struct router *rp;			/* the router in question */
    char *output;			/* the output of the program */
    struct rt_info *rt_info;		/* return routing info here */
    char **error;			/* return error message here */
{
    register char *p = output;

    if (rp->flags & QP_READ_PATH) {
	char *path;
	char *next_host;
	char *route;

	/* look for a path as first space-delimited field */
	while (isspace(*p)) p++;
	path = p;
	while (*p && !isspace(*p)) p++;
	if (*p) {
	    *p++ = '\0';
	}

	/* break the path into a next_host and route */
	if (parse_address(path, &next_host, &route, (int *)0) == FAIL) {
	    /* remainder contains error msg */
	    *error = route;
	    return FAIL;
	}
	if (next_host == NULL && route != NULL) {

	    /* trap the case of a single host in the path */
	    rt_info->next_host = route;
	} else {
	    rt_info->next_host = next_host;
	    rt_info->route = route;
	}
    }

    if (rp->flags & QP_READ_TRANSPORT) {
	char *transport;

	/* look for a transport as the next field */
	while (isspace(*p)) p++;
	transport = p;
	while (*p && !isspace(*p)) p++;
	*p = '\0';
	if (transport[0]) {
	    struct transport *tp;

	    tp = find_transport(transport);
	    if (tp == NULL) {
		*error = "program returned unknown transport";
		return FAIL;
	    }
	    rt_info->transport = tp;
	}
    }

    return SUCCEED;
}


static struct error *
parse_error(rp, cmd, parse_error_text)
    struct router *rp;
    char *cmd;
    char *parse_error_text;
{
    char *error_text;

    /*
     * ERR_160 - queryprogram cmd expansion error
     *
     * DESCRIPTION
     *      An error was encountered while expanding the command
     *      in the queryprogram driver.  The specific error
     *      message is stored in `error'.
     *
     * ACTIONS
     *      Defer the message with a configuration error.
     *
     * RESOLUTION
     *      The postmaster should check the validity of the
     *      queryprogram driver entry.
     */
    error_text = xprintf("router %s: error expanding `%s': %s",
			 rp->name, cmd, parse_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_160, error_text);
}

static struct error *
temp_command_error(rp)
    struct router *rp;
{
    char *error_text;

    /*
     * ERR_161 - temporary failure in executing a command
     *
     * DESCRIPTION
     *	An error was encountered while creating the child
     *	process.  This is most likely a temporary condition
     *	caused by running out of paging space or process
     *	slots.  Try again later.
     *
     * ACTIONS
     *	Defer the message.
     *
     * RESOLUTION
     *	Hopefully, a later queue run will succeed.
     */
    error_text = xprintf("router %s: command execution failed: %s",
			 rp->name, strerror(errno));
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_161, error_text);
}

static struct error *
output_parse_error(rp, parse_error_text)
    struct router *rp;
    char *parse_error_text;
{
    char *error_text;

    /*
     * ERR_171 - query_program command output format error
     *
     * DESCRIPTION
     *      An error was encountered while parsing the output of the
     *      command executed by the query_program router driver.
     *
     * ACTIONS
     *      Defer the message as a configuration error.
     */
    error_text = xprintf("router %s: parse error in command output: %s",
			 rp->name, parse_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_171, error_text);
}
