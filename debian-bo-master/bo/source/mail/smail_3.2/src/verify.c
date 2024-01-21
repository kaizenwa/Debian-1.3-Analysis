/*
#ident	"@(#)smail/src:RELEASE-3_2:verify.c,v 1.6 1996/02/28 06:47:23 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * verify.c:
 *	address verification functions and user interfaces for address
 *	verification and expansion.
 *
 *	external functions: verify_addr_list
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "dys.h"
#include "addr.h"
#include "direct.h"
#include "route.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif


/*
 * verify_addr_list - verify deliverability to a set of addresses
 *
 * call upon the verify entry points to the director and router drivers
 * to perform simple verification for a list of addresses.
 *
 * For directors, verification is only one level deep; i.e., if any
 * director matches a local address, the address verifies true.
 *
 * An error structure is stored in the error fields for entries in the
 * defer and fail output lists.  Only work_addr, target and remainder are
 * set in the okay output list.  If it is desired that work_addr be
 * restored back to the original value, it is the callers responsibility
 * to arrange this.
 *
 * The first router that finds a match for a target wins, as we do not
 * need to actually determine who has the best match, we only need to know
 * if a match exists.
 */
void
verify_addr_list(in, okay, defer, fail)
    struct addr *in;			/* input addr list */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    register struct addr *cur;		/* current address being processed */
    struct addr *next;			/* next address to process */
    struct addr *local;			/* addrs that parsed local */
    struct addr *remote;		/* addrs that parsed remote */

    /*
     * form the local and remote lists, and put parse errors into fail.
     */
    remote = NULL;
    local = NULL;
    for (cur = in; cur; cur = next) {
	int form;			/* form from parse_address() */

	next = cur->succ;

	/* get the form, plus the target and remainder */
	form = parse_address(cur->work_addr, &cur->target, &cur->remainder,
			     &cur->parseflags);

	/* split into lists based on the form */
	switch (form) {
	case FAIL:
	case PARSE_ERROR:
	    /*
	     * ERR_111 - address parse error
	     *
	     * DESCRIPTION
	     *      parse_address() encountered an error while parsing the
	     *      work_addr for this address.  The error is stored in
	     *      cur->remainder.
	     *
	     * ACTIONS
	     *      A message about the parse error should be returned to
	     *      the owner of the address or to the sender.
	     *
	     * RESOLUTION
	     *      The owner or sender should correct the address and
	     *      resubmit the message.
	     */
	    cur->error = note_error(ERR_NSOWNER|ERR_111,
				    xprintf("address parse error: %s",
					    cur->remainder));
	    cur->flags |= PARSE_ERROR;
	    cur->succ = *fail;
	    *fail = cur;
	    break;

	case LOCAL:
	    /*
	     * a local form in quotes must be reparsed,
	     * otherwise put it on the local addr list.
	     */
	    if (strip(cur->remainder)) {
		/* it was in quotes, put it back on the input */
		cur->work_addr = cur->remainder;
		next = cur;
	    } else {
                if (!cur->local_name)
                  cur->local_name = visible_name;
		cur->succ = local;
		local = cur;
	    }
	    break;

	default:
	    /*
	     * a remote form may point to the local host, if so
	     * put it back on the input, otherwise it goes on the
	     * remote list.
	     */
	    if (islocalhost(cur->target)) {
	/* it is the local host, save name & parse it again */
                cur->local_name = copy(cur->target);
		cur->work_addr = cur->remainder;
		next = cur;
	    } else {
		cur->succ = remote;
		remote = cur;
	    }
	    break;
	}
    }

    /* if there are any local addrs, run them through the directors */
    if (local) {
	verify_local_addrs(local, okay, defer, fail);
    }

    /* if there are any remote addrs, run them through the routers */
    if (remote) {
	verify_remote_addrs(remote, okay, defer, fail);
    }
}
