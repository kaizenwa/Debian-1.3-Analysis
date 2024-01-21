/*
#ident	"@(#)smail/src:RELEASE-3_2:resolve.c,v 1.8 1996/02/28 06:47:16 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * resolve.c:
 *	resolve addresses to completed addr structures with transports.
 *
 *	external functions: resolve_addr_list, islocalhost
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "hash.h"
#include "log.h"
#include "addr.h"
#include "route.h"
#include "transport.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "extern.h"
# include "error.h"
#endif

/* exported variables */
struct hash_table *hit_table;		/* table to recognize address hits */


/*
 * resolve_addr_list - resolve addresses to transports and next hosts
 *
 * given a list of user-supplied addresses on input, produce resolved
 * and unresolvable addresses on output.
 *
 * inputs:
 *	in	- the list of input address structures
 *
 * outputs:
 *	out	- the list of completely resolved address structures.
 *		  transport, next_host and next_addr will be properly
 *		  filled in for all of these structures.
 *	defer	- a list of temporarily unresolvable address structures.
 *		  an error structure is stored in the error element.
 *		  These addresses should be retried at a later time.  If
 *		  ERR_CONFERR is set in the error->info element, the
 *		  problem is a configuration error.
 *	fail	- a list of unresolvable address structures.  An error
 *		  structure is stored in the error element.  If
 *		  ERR_NSENDER is set, a note is returned to the sender.
 *		  If ERR_NPOSTMASTER is set, then a note is mailed to
 *		  the postmaster.  If ERR_NSOWNER is set then a note is
 *		  sent to an owner for an address, or to the sender if
 *		  the address has no owner.  If ERR_NPOWNER is set then
 *		  a note is sent to an owner or to the postmaster if the
 *		  address has no owner.
 */
void
resolve_addr_list(in, out, defer, fail, hash_addrs)
    struct addr *in;			/* the address list to resolve */
    struct addr **out;			/* produced addr list w/transports */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
    int hash_addrs;			/* TRUE to prevent duplicate addrs */
{
    struct addr *cur;			/* current address being processed */
    struct addr *local;			/* addrs that parsed local */
    struct addr *remote;		/* addrs that parsed remote */
    struct addr *next;			/* next value for cur */

    /*
     * resolve all addresses in our input queue
     * each step through the loop advances the progress
     * in resolving all addresses to a transport.
     * The loop is done when nothing remains to be processed.
     * as an optimization, remote form processing is not
     * done unless no local processing was required.
     */
    remote = NULL;
    local = NULL;
    while (in || remote) {

	/*
	 * split the input list into local and remote
	 * forms.
	 */
	for (cur = in, in = NULL; cur; cur = next) {
	    int form;			/* form from parse_address */

	    next = cur->succ;

	    if (hash_addrs &&
		!(cur->flags & ADDR_DONTHASH) &&
		add_to_hash(cur->work_addr, (char *)NULL, 0,
			    hit_table) == ALREADY_HASHED)
	    {
		/*
		 * we have already seen this address, don't
		 * process it again
		 */
		continue;
	    }

	    form = parse_address(cur->work_addr, &cur->target,
				 &cur->remainder, &cur->parseflags);
	    switch (form) {
	    case FAIL:
	    case PARSE_ERROR:
		/*
		 * ERR_111 - address parse error
		 *
		 * DESCRIPTION
		 *      parse_address() encountered an error while parsing
		 *      the work_addr for this address.  The error is stored
		 *      in cur->remainder.
		 *
		 * ACTIONS
		 *      A message about the parse error should be returned
		 *      to the owner of the address or to the sender.
		 *
		 * RESOLUTION
		 *      The owner or sender should correct the address and
		 *      resubmit the message.
		 */
		cur->error = note_error(ERR_NSOWNER|ERR_111, cur->remainder);
		cur->flags |= PARSE_ERROR;
		cur->succ = *fail;
		*fail = cur;
		continue;

	    case LOCAL:
		/*
		 * a local-form in quotes must be reparsed
		 */
		if (strip(cur->remainder)) {
		    /* it was in quotes, put it back on the input */
		    next = cur;
		} else {
                    if (!cur->local_name)
                      cur->local_name = visible_name;
		    cur->succ = local;
		    local = cur;
		}
		break;

	    default:
		/* anything else is a remote-form address */
		/* determine if this is actually the local host */
		if (islocalhost(cur->target)) {
		    /* remember the local name */
                    cur->local_name = copy(cur->target);
#if 0 /*def USE_TARGET_DOMAIN */
 		    cur->work_addr = cur->remainder;
#else
		    /* it is the local host, so save the name */
		    (void) strcpy(cur->work_addr, cur->remainder);
#endif
		    /* re-parse the current addr */
		    next = cur;
		    continue;
		}
		cur->flags &= ~(ADDR_FORM_MASK);
		cur->flags |= form;
		cur->succ = remote;
		remote = cur;
		break;
	    }
	}

	/*
	 * either process local or remote addresses.
	 */
	if (local) {
	    direct_local_addrs(local, out, &in, defer, fail);
	    local = NULL;
	} else {
	    route_remote_addrs(remote, out, &in, defer, fail);
	    remote = NULL;
	}
    }
}

/*
 * islocalhost - determine if the given target is the local host
 *
 * given the currently known names for the localhost, determine
 * if the given name matches one of these known names.
 *
 * return TRUE or FALSE.
 */
int
islocalhost(target)
    register char *target;		/* name to match */
{
    if ((uucp_name && EQIC(target, uucp_name)) ||
	(hostnames && is_string_in_list(target, hostnames)) ||
	(more_hostnames && is_string_in_list(target, more_hostnames)))
    {
	return TRUE;
    }
    return FALSE;
}
