/*
#ident	"@(#)smail/src:RELEASE-3_2:transport.c,v 1.28 1996/02/26 18:28:21 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * transport.c:
 *	process and deliver remote addresses.
 *
 *	The routines in this file are used after an address has been
 *	routed and resolved and associated with a particular transport.
 *	This file provides some support routines for transport drivers.
 *
 *	external functions:  assign_transports, call_transports,
 *			     write_message, remote_from_line, local_from_line,
 *			     find_transport, find_transport_driver
 */
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "addr.h"
#include "transport.h"
#include "log.h"
#include "spool.h"
#include "dys.h"
#include "exitcodes.h"
#include "smailconf.h"
#include "parse.h"
#include "hash.h"
#include "alloc.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

#ifdef	UNIX_BSD
# include <sys/file.h>
#endif

/* variables imported from libc */
extern int errno;

/* variables exported from this file */
char *path_to_sender = NULL;		/* uucp-style route to sender */
int cached_transports = FALSE;		/* TRUE if cache_transports() called */

/* functions local to this file */
static int assign_compare();
static int write_bsmtp_prologue();
static int write_bsmtp_epilogue();
static void build_path_to_sender();
static char *tp_remove_header();
static char *tp_insert_header();
static char *tp_append_header();
static char *transport_driv_function();

static char *inet_from_line();


static struct attr_table transport_generic[] = {
    { "driver", t_string, NULL, NULL, OFFSET(transport, driver) },
    { "max_addrs", t_int, NULL, NULL, OFFSET(transport, max_addrs) },
    { "max_hosts", t_int, NULL, NULL, OFFSET(transport, max_hosts) },
    { "max_chars", t_int, NULL, NULL, OFFSET(transport, max_chars) },
    { "retry_dir", t_string, NULL, NULL, OFFSET(transport, retry_dir) },
    { "shadow", t_string, NULL, NULL, OFFSET(transport, shadow) },
    { "error_transport", t_string, NULL, NULL,
	  OFFSET(transport, error_transport) },
    { "strict", t_boolean, NULL, NULL, STRICT_TPORT },
    { "uucp", t_boolean, NULL, NULL, UUCP_XFORM },
    { "inet", t_boolean, NULL, NULL, INET_XFORM },
    { "received", t_boolean, NULL, NULL, PUT_RECEIVED },
    { "return_path", t_boolean, NULL, NULL, PUT_RETURNPATH },
    { "from", t_boolean, NULL, NULL, PUT_FROM },
    { "local", t_boolean, NULL, NULL, LOCAL_TPORT|LOCAL_XFORM },
    { "local_xform", t_boolean, NULL, NULL, LOCAL_XFORM },
    { "crlf", t_boolean, NULL, NULL, PUT_CRLF },
    { "bsmtp", t_boolean, NULL, NULL, BSMTP_TPORT },
    { "hbsmtp", t_boolean, NULL, NULL, HBSMTP_TPORT },
    { "dots", t_boolean, NULL, NULL, PUT_DOTS },
    { "debug", t_boolean, NULL, NULL, DEBUG_TPORT },
    { "unix_from_hack", t_boolean, NULL, NULL, UNIX_FROM_HACK },
    { "uucp_from_hack", t_boolean, NULL, NULL, UNIX_FROM_HACK },
    { "remove_header", t_proc, NULL, (tup *)tp_remove_header, 0 },
    { "insert_header", t_proc, NULL, (tup *)tp_insert_header, 0 },
    { "append_header", t_proc, NULL, (tup *)tp_append_header, 0 },
};


/*
 * assign_transports - assign instances of transports for addresses
 *
 * given a list of addresses as input, produce a list assigning a list
 * of remote addresses to specific instances of a transport.  One
 * instance essentially maps onto exactly one call to the underlying
 * transport driver.
 *
 * NOTE:  This routine breaks the forward links between addr structures.
 */
struct assign_transport *
assign_transports(list)
    struct addr *list;			/* list of addresses */
{
    extern void qsort();
    register struct addr *aq;		/* temp */
    register struct assign_transport *asn; /* list of assigned transports */
    register struct addr **aap;		/* points to array for sorting */
    register int i;			/* index */
    int ct;				/* count of items in input list */
    int host_ct = 0;			/* count of hosts in instance */
    int addr_ct = 0;			/* count of addrs in instance */
    int char_ct = 0;			/* count of chars in instance */
    struct transport *last_transport = NULL; /* last assigned transport */
    char *last_host;			/* last assigned host */
    char *last_owner;                   /* last assigned owner */

    /* count the number of input items */
    for (ct = 0, aq = list; aq; aq = aq->succ) {
	ct++;
    }

    DEBUG1(DBG_REMOTE_HI, "assign_transports called with %d addrs\n", ct);
    /* create an array and copy remote information to it for sorting */
    aap = (struct addr **) xmalloc((unsigned) (ct * sizeof(*aap)));
    for (aq = list, i = 0; aq; aq = aq->succ, i++) {
	aap[i] = aq;
    }

    /* NOTE: qsort does not appear to return anything */
    qsort((char *)aap, ct, sizeof(*aap), assign_compare);

    /*
     * we now have an array sorted by transport and host.
     * pass through that array and assign addr structures to
     * specific transport instances, taking into account
     * max_addr and max_hosts for the transports.
     */
    last_host=(char*)0;
    last_owner=(char *)(-1);     /* can't use 0 as owner can be unset */
    for (asn = NULL, i = ct-1; i >= 0; --i) {
	/* possible reasons to switch to a new instance */
	if (
	    /* we have a different transport */
	    aap[i]->transport != last_transport ||

            /* Same host, but different address owner */
            (aap[i]->next_host && last_host &&
              EQIC(aap[i]->next_host, last_host) &&
              aap[i]->owner != last_owner &&
              (aap[i]->owner == NULL || last_owner == NULL ||
              !EQIC(aap[i]->owner, last_owner))) ||

            /* maximum number of hosts per transport call exceeded */
	    (last_transport->max_hosts &&
	      aap[i]->next_host && last_host &&
	      !EQIC(aap[i]->next_host, last_host) &&
	      last_transport->max_hosts < ++host_ct) ||

	    /* maximum number of addrs per transport call exceeded */
	    (last_transport->max_addrs &&
	      last_transport->max_addrs < ++addr_ct) ||

	    /* maximum number of chars of addr per call exceeded */
	    (last_transport->max_chars &&
	      last_transport->max_chars <
		   (char_ct += (int)strlen(aap[i]->next_addr) + 3)))
	{
	    /* initialize for a new instance */
	    struct assign_transport *newasn;

	    DEBUG1(DBG_REMOTE_HI, "new instance ... transport=<%s>\n",
		   aap[i]->transport->name);
	    newasn = (struct assign_transport *)xmalloc(sizeof(*newasn));
	    last_transport = aap[i]->transport;
	    last_host = aap[i]->next_host;
            last_owner = aap[i]->owner;
	    host_ct = 1;
	    addr_ct = 1;
	    char_ct = strlen(aap[i]->next_addr) + 3;
	    /* fill in the first entry of the instance */
	    aap[i]->succ = NULL;
	    newasn->succ = asn;
	    asn = newasn;
	} else {
	    /* add the addr to the previous instance */
	    aap[i]->succ = asn->addr;
	}
	asn->addr = aap[i];
	DEBUG2(DBG_REMOTE_HI, "adding...host=<%s>, addr=<%s>\n",
	       asn->addr->next_host, asn->addr->next_addr);
    }

    /* all done, free temps and return the list */
    xfree((char *)aap);
    return asn;
}

/*
 * compare two struct addrs for transport and then hostname,
 * and then owner 
 */
static int
assign_compare(a, b)
    struct addr **a;
    struct addr **b;
{
    register int ret = strcmp((*a)->transport->name, (*b)->transport->name);

    if (ret) {
	/* transports are unequal, return value just based on that */
	return ret;
    }

    /* if next_host not defined, the addrs are equal */
    if ((*a)->next_host == NULL || (*b)->next_host == NULL) {
	ret = 0;
    } else {
	ret = strcmpic((*a)->next_host, (*b)->next_host);
    }

    if (ret) {
        /* hosts are unequal, return value just based on that */
        return ret;
    }

    /* compare owner fields */
    if ((*a)->owner == NULL) return ((*b)->owner == NULL)? 0 : (-1);
    return ((*b)->owner == NULL)? 1 :
      strcmpic((*a)->owner, (*b)->owner);
}


/*
 * call_transports - call transport drivers for assigned transports
 *
 * given a list produced by assign_transports, call the transport drivers
 * to actually perform delivery to addresses.
 */
void
call_transports(asn, defer, fail)
    register struct assign_transport *asn; /* list of assigned instances */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct addr *succeed;		/* succeeded addrs from drivers */
    struct addr *tp_defer;		/* defer addrs from drivers */
    struct addr *tp_fail;		/* fail addrs from drivers */
    struct addr *shadow_list = NULL;	/* shadow delivery list */
    struct assign_transport *sh_asn;	/* shadow transport instances */

    for (; asn; asn = asn->succ) {
	time_t started;
	struct transport_driver *driver;
	register struct transport *tp = asn->addr->transport;

	/*
	 * don't deliver to remote transports if the hop_count would
	 * pass beyond maximum.
	 */
	if (hop_count >= max_hop_count && !(tp->flags&LOCAL_TPORT)) {
	    /*
	     * ERR_145 - maximum hop count exceeded
	     *
	     * DESCRIPTION
	     *      If a remote transport were performed, the maximum hop
	     *      count, as set by the max_hop_cound attribute in the
	     *      config file, would be exceeded.  This condition is
	     *      assumed to be a result of a loop condition where two
	     *      machines are sending mail back and forth to each other.
	     *
	     * ACTIONS
	     *      An error message is sent back to the sender stating the
	     *      error.  Note that if one of the sites back to the sender
	     *      has a shorter value for max_hop_count, then the error
	     *      message will fail to reach the sender though, hopefully,
	     *      a message will be sent back to the postmaster at the
	     *      local site.
	     *
	     * RESOLUTION
	     *      Determine which two sites are causing the loop condition
	     *      and remove or correct the forward or alias from one of
	     *      those two sites.  If the problem is not actually a loop
	     *      condition but a long path, a shorter path should be
	     *      used.  It is unlikely that paths longer than 10 or 15
	     *      hops should ever be required.
	     */
	    struct error *loop_error =
		note_error(ERR_NSENDER|ERR_145,
			   "loop detection: maximum hop count exceeded");
	    register struct addr *cur;

	    /* NOTE:  the sequence of operations here IS correct */
	    for (cur = asn->addr; cur; cur = cur->succ) {
		cur->error = loop_error;
	    }
	    fail_delivery(asn->addr);
	    insert_addr_list(asn->addr, fail, loop_error);
	    continue;
	}

	/* lookup the driver */
	driver = find_transport_driver(tp->driver);

	if (driver == NULL) {
	    /* configuration error, the driver does not exist */
	    /*
	     * ERR_146 - transport driver not found
	     *
	     * DESCRIPTION
	     *      The driver name was not in the table of transport
	     *      drivers.
	     *
	     * ACTIONS
	     *      Defer addresses with configuration errors.
	     *
	     * RESOLUTION
	     *      The postmaster must check the transport configuration
	     *      before delivery can be performed.
	     */
	    insert_addr_list(asn->addr,
			     defer,
			     note_error(ERR_CONFERR|ERR_146,
					xprintf(
					   "transport %s: driver %s not found",
						tp->name, tp->driver)));
	    continue;
	}

	/* determine official time of attempt */
	time(&started);

	/* call the transport */
	succeed = NULL;
	tp_defer = NULL;
	tp_fail = NULL;

	DEBUG2(DBG_REMOTE_LO, "transport %s uses driver %s\n",
	       tp->name, tp->driver);
	(*driver->driver)(asn->addr, &succeed, &tp_defer, &tp_fail);

	/* subject transport-deferred addresses to retry duration limits */
	tp_defer = retry_addr_after(started, tp_defer, fail);

	/* look at successful deliveries */
	if (succeed) {
	    retry_addr_finished(succeed);
	    /* log successes */
	    succeed_delivery(succeed);
	    if (tp->shadow) {
		/* on successful delivery, call the shadow transport */
		struct transport *shadow_tp = find_transport(tp->shadow);

		if (shadow_tp == NULL) {
		    send_to_postmaster = TRUE;
		    write_log(LOG_SYS|LOG_MLOG,
			      "transport %s: shadow transport %s not found",
			      tp->name, tp->shadow);
		} else {
		    /* insert in shadow delivery list, with new transport */
		    register struct addr *cur;
		    register struct addr *next;

		    for (cur = succeed; cur; cur = next) {
			next = cur->succ;
			cur->flags |= ADDR_SHADOW;
			cur->transport = shadow_tp;
			cur->succ = shadow_list;
			shadow_list = cur;
		    }
		}
	    }
	}

	/* look at deferred deliveries */
	if (tp_defer) {
	    retry_addr_finished(tp_defer);
	    insert_addr_list(tp_defer, defer, (struct error *)NULL);
	}

	/* look at failed deliveries */
	if (tp_fail) {
	    retry_addr_finished(tp_fail);
	    fail_delivery(tp_fail);	/* log them */
	    if (tp->error_transport == NULL) {
		insert_addr_list(tp_fail, fail, (struct error *)NULL);
	    } else {
		/* on failed delivery, call the error transport */
		struct transport *error_tp =
		    find_transport(tp->error_transport);

		if (error_tp == NULL) {
		    send_to_postmaster = TRUE;
		    write_log(LOG_SYS|LOG_MLOG,
			      "transport %s: error transport %s not found",
			      tp->name, tp->error_transport);
		} else {
		    /* insert in shadow delivery list, with new transport */
		    register struct addr *cur;
		    register struct addr *next;

		    for (cur = tp_fail; cur; cur = next) {
			next = cur->succ;
			cur->transport = error_tp;
			cur->succ = shadow_list;
			shadow_list = cur;
		    }
		}
	    }
	}
    }

    /* if there are shadow deliveries to be made, assign them and transport */
    if (shadow_list == NULL) {
	return;				/* no shadows, just return */
    }

    /* assign shadow deliveries */
    for (sh_asn = assign_transports(shadow_list);
	 sh_asn;
	 sh_asn = sh_asn->succ)
    {
	struct transport_driver *driver;
	register struct transport *tp = sh_asn->addr->transport;
	struct addr *dummy_defer;

	/* lookup the driver */
	driver = find_transport_driver(tp->driver);

	if (driver == NULL) {
	    /* configuration error: for shadows, send to postmaster */
	    send_to_postmaster = TRUE;
	    write_log(LOG_SYS|LOG_MLOG,
		      "shadow transport %s: driver %s not found",
		      tp->name, tp->driver);
	    continue;
	}

	/* call the shadow transport */
	DEBUG2(DBG_REMOTE_LO, "calling driver %s from shadow transport %s\n",
	       tp->driver, tp->name);
	succeed = NULL;
	tp_fail = NULL;
	(*driver->driver)(sh_asn->addr, &succeed, &dummy_defer, &tp_fail);

	/* log failed addresses */
	if (fail) {
	    struct addr *cur;
	    struct addr *next;

	    for (cur = tp_fail; cur; cur = next) {
		next = cur->succ;
		write_log(LOG_SYS, "%s ... shadow transport %s failed: %s",
			  cur->in_addr, cur->transport->name, cur->next_addr);
		if ((cur->flags & ADDR_SHADOW) == 0) {
		    /*
		     * put failed deliveries from an error_transport on
		     * output fail list
		     */
		    cur->succ = *fail;
		    *fail = cur;
		}
	    }
	}
    }
}


/*
 * cache_transports - call cache entrypoints for all transports
 *
 * cache information used by transport drivers.  This can be called
 * when it is determined that there will be an attempt to deliver more
 * than one mail message, to increase the overall efficiency of the
 * mailer.
 *
 * Daemons can call this periodically to recache stale data.
 */
void
cache_transports()
{
    struct transport *tp;		/* temp for stepping thru transports */
    struct transport_driver *driver;

    for (tp = transports; tp; tp = tp->succ) {
	driver = find_transport_driver(tp->driver);
	if (driver && driver->cache) {
	    (*driver->cache)(tp);
	}
    }
    cached_transports = TRUE;
}

#ifdef notyet
/*
 * finish_transports - free resources used by all directors
 *
 * free information that was cached by transports or used by
 * transports in the process of delivering messages.  Transports can
 * cache data for efficiency, or can maintain state between
 * invocations.  This function is called when transports will no
 * longer be needed, allowing transports to free any resources that
 * they were using that will no longer be needed.  For example, it is
 * a good idea for transports to close any files that they opened, as
 * file descriptors are a precious resource in some machines.
 */
void
finish_transports()
{
    struct transports *tp;		/* temp for stepping thru transports */
    struct transport_driver *driver;

    for (tp = transports; tp; tp = tp->succ) {
	driver = find_transport_driver(tp->driver);
	if (driver->finish) {
	    (*driver->finish)(dp);
	}
    }
    cached_transports = FALSE;
}
#endif


/*
 * write_message - write out the current message to an open file
 *
 * this takes a transport file entry and a list of addr structures
 * and writes the message in the manner specified in the transport
 * file entry.  It is intended to be called from transport drivers.
 *
 * If BSMTP_TPORT is set in the transport flags, an SMTP envelope
 * is placed around the message.  However, result codes are not
 * read from the destination so this is not useful for interactive
 * SMTP.  The list of addr structures is only used for writing the
 * SMTP envelope.  HBSMTP_TPORT generates an SMTP envelope without
 * the initial HELO or the final QUIT commands.
 *
 * NOTE:  Everybody still needs to call fflush after calling
 *	  write_message().
 *
 * return READ_FAIL, WRITE_FAIL or SUCCEED
 */
int
write_message(f, tp, addr)
    FILE *f;				/* write on this file */
    struct transport *tp;		/* transport file entry */
    struct addr *addr;			/* list of addresses */
{
    int fail;				/* returned failure codes */

    DEBUG(DBG_HEADER_LO, "\nEnvelope text\n");
    /*
     * put out the initial smtp commands up to the DATA command
     */
    if (tp->flags & (BSMTP_TPORT | HBSMTP_TPORT)) {
#ifndef NODEBUG
	if (debug >= DBG_HEADER_LO && errfile) {
	    (void) write_bsmtp_prologue(errfile, tp, addr);
	}
#endif	/* NODEBUG */
	if (write_bsmtp_prologue(f, tp, addr) == FAIL) {
	    return WRITE_FAIL;
	}
	tp->flags |= PUT_DOTS;		/* force use of SMTP dot protocol */
    }

    /*
     * write out a From<space> line, if required
     */
    if (tp->flags & PUT_FROM) {
	/* but what form do we need? */
	if (tp->flags & INET_XFORM) {
#ifndef NODEBUG
	    if (debug >= DBG_HEADER_LO && errfile) {
		(void)fputs(inet_from_line(tp), errfile);
	    }
#endif	/* NODEBUG */
	    if (fputs(inet_from_line(tp), f) == EOF) {
		return WRITE_FAIL;
	    }
	} else if (tp->flags & LOCAL_XFORM) {
#ifndef NODEBUG
	    if (debug >= DBG_HEADER_LO && errfile) {
		(void)fputs(local_from_line(), errfile);
	    }
#endif	/* NODEBUG */
	    if (fputs(local_from_line(), f) == EOF) {
		return WRITE_FAIL;
	    }
	} else {
#ifndef NODEBUG
	    if (debug >= DBG_HEADER_LO && errfile) {
		(void)fputs(remote_from_line(), errfile);
	    }
#endif	/* NODEBUG */
	    if (fputs(remote_from_line(), f) == EOF) {
		return WRITE_FAIL;
	    }
	}
	DEBUG(DBG_HEADER_LO, "\n");
	/* put the end of line */
	if (tp->flags & PUT_CRLF) {
	    if (putc('\r', f) == EOF) {
		return WRITE_FAIL;
	    }
	}
	if (putc('\n', f) == EOF) {
	    return WRITE_FAIL;
	}
    }

    /*
     * write out the header according to the transport flags
     */
    if (write_header(f, addr) == FAIL) {
	return WRITE_FAIL;
    }
    if ((tp->flags & PUT_CRLF) && putc('\r', f) == EOF) {
	return WRITE_FAIL;
    }
    if (putc('\n', f) == EOF) {
	return WRITE_FAIL;
    }

    if (tp->flags & DEBUG_TPORT) {
	/* if we are debugging, don't dump the body, dump interesting
	 * debugging info instead */
	extern char **save_argv;	/* import this from main */
	char **av;

	(void) fprintf(f, "|---- original flags ----|\n");
	for (av = save_argv; *av; av++) {
	    (void) fprintf(f, "%s\n", *av);
	}
	(void) fprintf(f, "|---- interesting info ----|\n");
	(void) fprintf(f,"\
	message_id=%s\n\
	spool_dir=%s\n\
	spool_fn=%s\n\
	transport=%s\n\
	next_host=%s\n\
	msg_grade=%c\n",
		       message_id,
		       spool_dir,
		       spool_fn,
		       tp->name,
		       (addr && addr->next_host)? addr->next_host: "(null)",
		       msg_grade);

	send_log(f, TRUE, "|---- per-message log ----|\n");
    } else {

	DEBUG(DBG_HEADER_LO, "... message body ...\n");

	/*
	 * write out the body.
	 */
	if ((fail = write_body(f, tp->flags)) != SUCCEED) {
	    return fail;
	}
    }

    /*
     * finish off the SMTP envelope
     */
    if (tp->flags & (BSMTP_TPORT | HBSMTP_TPORT)) {
#ifndef NODEBUG
	if (debug >= DBG_HEADER_LO && errfile) {
	    (void) write_bsmtp_epilogue(errfile, tp);
	}
#endif	/* NODEBUG */
	if (write_bsmtp_epilogue(f, tp) == FAIL) {
	    return WRITE_FAIL;
	}
    }
    DEBUG(DBG_HEADER_LO, "\n");

    return SUCCEED;			/* everything went fine */
}

/*
 * write_bsmtp_prologue - write the initial SMTP commands for BSMTP transports
 *
 * write out the HELO, MAIL FROM, RCPT TO and DATA commands.
 */
static int
write_bsmtp_prologue(f, tp, addr)
    FILE *f;				/* file to write commands to */
    struct transport *tp;		/* transport entry */
    struct addr *addr;			/* recipient addr structures */
{
    /*
     * how do we end a line?
     */
    char *eol = (tp->flags & PUT_CRLF)? "\r\n": "\n";

    if (! (tp->flags & HBSMTP_TPORT) &&
	fprintf(f, "HELO %s%s", primary_name, eol) == EOF)
    {
	return FAIL;
    }

    /*
     * determine which sender form is appropriate
     */
    if (error_sender) {
	/* if the special SMTP sender forms <> or <+> were given, pass along */
	if (fprintf(f, "MAIL FROM:<%s>%s",
		    ((islocal && tp->flags & LOCAL_XFORM)? "+": ""),
		    eol) == EOF) {
	    return FAIL;
	}
    } else {
	if (fprintf(f, "MAIL FROM:<%s>%s", get_sender_addr(tp), eol) == EOF) {
	    return FAIL;
	}
    }

    /*
     * write out a RCPT TO: line for each recipient address
     */
    while (addr) {
	if (fprintf(f, "RCPT TO:<%s>%s", addr->next_addr, eol) == EOF) {
	    return FAIL;
	}
	addr = addr->succ;
    }

    if (fprintf(f, "DATA%s", eol) == EOF) {
	return FAIL;
    }

    return SUCCEED;
}

/*
 * get_sender_addr - return a sender address for use a particular transport
 *
 * Return a transformation of the sender address based on the various
 * transport attributes.
 *
 * The returned storage should not be freed and may be reused in
 * subsequent calls to get_sender_addr.
 */
char *
get_sender_addr(tp)
    struct transport *tp;
{
    static struct str str;
    static int inited = 0;
    char *path;
    int form;
    int flags;
    char *err;

    if (! inited) {
	inited = 1;
	STR_INIT(&str);
    } else {
	str.i = 0;
    }

    if (tp->flags & UUCP_XFORM) {
	path = build_partial_uucp_route(sender, &err, 0);
	if (path) {
	    str_printf(&str, "%s!%s%N", uucp_name, path);
	    xfree(path);
	} else {

	    /*
	     * bad sender path, prepend host! anyway.
	     */

	    str_printf(&str, "%s!%s%N", uucp_name, sender);
	}

	return str.p;
    }

    if (tp->flags & LOCAL_XFORM) {
	return sender;
    }


    /* INET_XFORM or no specified transform */
    form = parse_address(sender, (char **)0, &err, &flags);
    switch (form) {
    case FAIL:
    case RFC_ROUTE:
    case RFC_ENDROUTE:
    case MAILBOX:
    case PARSE_ERROR:
	return sender;

    default:
	str_printf(&str, "%s@%s%N", sender, visible_name);
	return str.p;
    }
}

/*
 * write_bsmtp_epilogue - write tailing commands for BSMTP transports
 *
 * finish off the data command and write out the QUIT command.
 */
static int
write_bsmtp_epilogue(f, tp)
    FILE *f;
    struct transport *tp;
{
    if (tp->flags & HBSMTP_TPORT) {
	if (fprintf(f, (tp->flags&PUT_CRLF)? ".\r\n": ".\n") == EOF) {
	    return FAIL;
	}
	return SUCCEED;
    }
    if (tp->flags & PUT_CRLF) {
	if (fprintf(f, ".\r\nQUIT\r\n") == EOF) {
	    return FAIL;
	}
    } else {
	if (fprintf(f, ".\nQUIT\n") == EOF) {
	    return FAIL;
	}
    }

    return SUCCEED;
}


/*
 * remote_from_line - build a From_ line for remote transports
 */
char *
remote_from_line()
{
    static char *from_line = NULL;	/* saved From_ line */

    if (from_line) {
	return from_line;		/* exists, use it */
    }
    if (path_to_sender == NULL) {
	build_path_to_sender();
    }
    from_line = xprintf("From %s %s remote from %s",
			path_to_sender, unix_date(), uucp_name);
    return from_line;
}

/*
 * local_from_line - build a From_ line for local transports
 */
char *
local_from_line()
{
    static char *from_line = NULL;	/* saved From_ line */

    if (from_line) {
	return from_line;		/* exists, use it */
    }
    if (path_to_sender == NULL) {
	build_path_to_sender();
    }
    from_line = xprintf("From %s %s", path_to_sender, unix_date());
    return from_line;
}

/*
 * inet_from_line - build a From_ line for an inet-based transport
 */
static char *
inet_from_line(tp)
    struct transport *tp;
{
    static char *from_line = NULL;	/* saved From_ line */

    if (from_line) {
	return from_line;		/* exists, use it */
    }
    from_line = xprintf("From %s %s", get_sender_addr(tp), unix_date());
    return from_line;
}

/*
 * build_path_to_sender - build a !-route version of sender address
 */
static void
build_path_to_sender()
{
    char *error;

    path_to_sender = build_partial_uucp_route(sender, &error, 0);
    if (path_to_sender == NULL) {
	write_log(LOG_SYS|LOG_MLOG,
		  "error building path to sender, sender=%s, error=%s",
		  sender, error);
	path_to_sender = "Postmaster";
    }
}


/*
 * find_transport - given a transport's name, return the transport structure
 *
 * return NULL if no transport of that name exists.
 */
struct transport *
find_transport(name)
    register char *name;		/* search key */
{
    register struct transport *tp;	/* temp for stepping thru transports */

    /* loop through all the transports */
    for (tp = transports; tp; tp = tp->succ) {
	if (EQ(tp->name, name)) {
	    /* found the transport in question */
	    return tp;
	}
    }

    return NULL;			/* transport not found */
}

/*
 * find_transport_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct transport_driver *
find_transport_driver(name)
    register char *name;		/* search key */
{
    register struct transport_driver *tdp; /* pointer to table of drivers */

    for (tdp = transport_drivers; tdp->name; tdp++) {
	if (EQ(tdp->name, name)) {
	    return tdp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
}


/*
 * read_transport_file - read transport file
 *
 * read the transport file and build the transport list describing the
 * entries.  Return an error message or NULL.
 *
 * The entries from the transport file are prepended to the built-in
 * transports.
 */
char *
read_transport_file()
{
    FILE *f;				/* open transport file */
    char *error;			/* error from read_standard_file() */
    struct transport **tpp;
    struct stat statbuf;
    struct attr_table *end_transport_generic = ENDTABLE(transport_generic);
    static struct transport transport_template = {
	NULL,				/* name */
	"pipe",				/* driver, a reasonable default */
	NULL,				/* succ will be assigned */
	PUT_RECEIVED,			/* flags */
	1,				/* max_addrs */
	1,				/* max_hosts */
	2000,				/* max_chars, about half of NCARGS? */
	NULL,				/* shadow */
	NULL,				/* error_transport */
	NULL,				/* private */
    };

    /*
     * try to open the transport file, stat file if possible
     */
    if (transport_file == NULL || EQ(transport_file, "-")) {
	return NULL;
    }
    f = fopen(transport_file, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("%s: %s", transport_file, strerror(errno));
	}

	add_config_stat(transport_file, (struct stat *)NULL);
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(transport_file, &statbuf);

    /* call read_standard_file to do the real work */
    error = read_standard_file(f,
			       (char *)&transport_template,
			       sizeof(struct transport),
			       OFFSET(transport, name),
			       OFFSET(transport, flags),
			       OFFSET(transport, succ),
			       transport_generic,
			       end_transport_generic,
			       transport_driv_function,
			       (char **)&transports);

    /* finish up */
    (void) fclose(f);

    for (tpp = &transports; *tpp; tpp = &(*tpp)->succ)
	;
    *tpp = builtin_transports;

    /* return any error message */
    if (error) {
	return xprintf("%s: %s", transport_file, error);
    }
    return NULL;
}


void
dump_transport_config(f)
     FILE * f;
{
    struct transport *tp;		/* temp for stepping thru transports */
    struct attr_table *end_transport_generic = ENDTABLE(transport_generic);
    char * name;			/* name of transport */
    long flags;				/* flags for transport */
    struct transport_driver *driver;	/* drivers for transport */
    struct list *lp;			/* list pointer for header stuff */
    struct hash_table * tp_hash;	/* hash table to find multiple defs */

    DEBUG(DBG_ROUTE_HI, "dump transport configs called\n");
    fputs("#\n# -- transports configuration\n", f);

    tp_hash = new_hash_table(hit_table_len,
			     (struct block *) NULL,
			     HASH_DEFAULT);
    /*
     * work through each transport in turn.
     */
    for (tp = transports; tp; tp = tp->succ) {
	name = tp->name;
	flags = tp->flags;
	if (add_to_hash(name, NULL, 0, tp_hash) == ALREADY_HASHED) {
	    fprintf(f, "#\n# %s: a transport of this name has already been dumped\n",
		    name);
	} else {
	    fprintf(f, "#\n%s:\n", name);
	    (void) dump_standard_config(f,
					(char *) tp,
					name,
					flags,
					transport_generic,
					end_transport_generic);
	    /* The dump config routine cannot handle t_proc, so we do that here */
	    for(lp = tp->hdrremove; (lp); lp = lp->succ)
		fprintf(f, "\tremove_header=%s,\n", quote_string_value(lp->text));
	    for(lp = tp->hdrinsert; (lp); lp = lp->succ)
		fprintf(f, "\tinsert_header=%s,\n", quote_string_value(lp->text));
	    for(lp = tp->hdrappend; (lp); lp = lp->succ)
		fprintf(f, "\tappend_header=%s,\n", quote_string_value(lp->text));
	    fputs("\t;\n", f);
	    driver = find_transport_driver(tp->driver);
	    if (driver->dumper)
		(*driver->dumper)(f, tp);
	}
    }
    fputs("#\n# -- end of transports\n", f);
}


static char *
tp_remove_header(struct_p, attr)
    char *struct_p;			/* passed transport structure */
    struct attribute *attr;		/* attribute from transports file */
{
    struct transport *tp = (struct transport *)struct_p;
    struct list *lp;

    lp = (struct list *)xmalloc(sizeof(*lp));
    lp->text = attr->value;
    lp->succ = tp->hdrremove;
    tp->hdrremove = lp;
    return NULL;
}

static char *
tp_insert_header(struct_p, attr)
    char *struct_p;			/* passed transport structure */
    struct attribute *attr;		/* attribute from transports file */
{
    struct transport *tp = (struct transport *)struct_p;
    struct list *lp;

    lp = (struct list *)xmalloc(sizeof(*lp));
    lp->text = attr->value;
    lp->succ = tp->hdrinsert;
    tp->hdrinsert = lp;
    return NULL;
}

static char *
tp_append_header(struct_p, attr)
    char *struct_p;			/* passed transport structure */
    struct attribute *attr;		/* attribute from transports file */
{
    struct transport *tp = (struct transport *)struct_p;
    struct list *lp;

    lp = (struct list *)xmalloc(sizeof(*lp));
    lp->text = attr->value;
    lp->succ = tp->hdrappend;
    tp->hdrappend = lp;
    return NULL;
}

static char *
transport_driv_function(struct_p, driver_attrs)
    char *struct_p;			/* passed transport structure */
    struct attribute *driver_attrs;	/* driver-specific attributes */
{
    struct transport *tp = (struct transport *)struct_p;
    struct transport_driver *drv;

    if (tp->driver == NULL) {
	return xprintf("transport %s: no driver attribute", tp->name);
    }
    drv = find_transport_driver(tp->driver);
    if (drv == NULL) {
	return xprintf("transport %s: unknown driver: %s",
		       tp->name, tp->driver);
    }
    if (drv->builder) {
	return (*drv->builder)(tp, driver_attrs);
    }

    return NULL;
}
