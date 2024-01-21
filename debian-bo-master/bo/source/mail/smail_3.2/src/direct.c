/*
#ident	"@(#)smail/src:RELEASE-3_2:direct.c,v 1.14 1996/02/28 14:26:19 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * direct.c:
 *	resolve local form addresses to more addresses or to transports
 *
 *	external functions: direct_local_addrs, verify_local_addrs,
 *			    cache_directors, finish_directors,
 *			    director_user_info, find_director,
 *			    find_direct_driver, read_director_file.
 *                          dump_director_config
 */
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "direct.h"
#include "transport.h"
#include "log.h"
#include "addr.h"
#include "main.h"
#include "dys.h"
#include "exitcodes.h"
#include "smailconf.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
# include "error.h"
#endif

/* enums local to this file */
enum local_form {
    FILE_FORM,				/* filename address */
    PIPE_FORM,				/* shell-command address */
    INCLUDE_FORM,			/* mailing list address */
    OTHER_FORM				/* any other address form */
};
enum other_form_op {
    SEND_TO_NEXT,			/* send addr to next director */
    DROP_ADDRESS,			/* ignore addr */
    ADDRESS_OKAY			/* reparse address */
};

/* variables exported from this file */
int cached_directors = FALSE;		/* TRUE if cache_directors() called */

/* functions local to this file */
static void implicit_redirect();
static enum other_form_op finish_direct_other_form();
static void finish_direct_special_form();
static void cache_director_data();
static void premunge_local_addrs();
static enum local_form addr_type();
static int verify_special();
static char *director_driv_function();

/* Configuration template information */
static struct attr_table director_generic[] = {
    { "driver", t_string, NULL, NULL, OFFSET(director, driver) },
    { "caution", t_boolean, NULL, NULL, CAUTION_DIRECTOR },
    { "nobody", t_boolean, NULL, NULL, NOBODY_DIRECTOR },
    { "sender_okay", t_boolean, NULL, NULL, SENDER_OKAY },
    { "ignore_alias_match", t_boolean, NULL, NULL, IGNORE_ALIAS_MATCH },
    { "owner", t_string, NULL, NULL, OFFSET(director, owner) },
    { "default_user", t_string, NULL, NULL,
	  OFFSET(director, default_user) },
    { "default_group", t_string, NULL, NULL,
	  OFFSET(director, default_group) },
    { "default_home", t_string, NULL, NULL,
	  OFFSET(director, default_home) },
    { "domains", t_string, NULL, NULL, OFFSET(director, domains) },
    { "set_user", t_string, NULL, NULL, OFFSET(director, set_user) },
    { "set_group", t_string, NULL, NULL, OFFSET(director, set_group) },
    { "set_home", t_string, NULL, NULL, OFFSET(director, set_home) },
};


/*
 * direct_local_addrs - produce finished and expanded addrs from local addrs
 *
 * this is called from resolve_addr_list to run local-form addresses
 * through the directors to completely resolve addr structures and
 * to transform them into a new list of addr structures to resolve.
 */
void
direct_local_addrs(in, out, retry, defer, fail)
    struct addr *in;			/* input local-form addrs */
    struct addr **out;			/* output resolved addrs */
    struct addr **retry;		/* addr structures to reparse */
    struct addr **defer;		/* addrs to defer to a later time */
    struct addr **fail;			/* unresolvable addrs */
{
    struct director *dp;		/* temp for stepping thru directors */
    struct addr *cur;			/* temp for stepping through addrs */
    struct addr *newaddrs;		/* new addrs from a director */
    struct addr *includes;		/* mailing list addrs */
    struct addr *next;			/* next value for cur */
    struct addr *other_domains;         /* for domain-specific handling */

    DEBUG(DBG_DIRECT_HI, "direct_local_addrs called\n");

    /* do any munging of the addr structures required before directing */
    premunge_local_addrs(in);

    /*
     * call each director in turn, where each director
     * produces an output list a list of addrs it could
     * not resolve, and a list of addrs it resolved to
     * a list.  The actual return value is the list it
     * could not resolve.
     */
    *retry = NULL;
    includes = NULL;
    while (in) {
	/*
	 * directors are organized as subsections with the subsection
	 * data being the director structures.
	 */
	for (dp = directors; in && dp; dp = dp->succ) {
	    /* look up the director driver by name */
	    struct direct_driver *driver = find_direct_driver(dp->driver);

	    if (driver == NULL) {
		/*
		 * ERR_107 - director driver not found
		 *
		 * DESCRIPTION
		 *      A director driver was not found.
		 *
		 * ACTIONS
		 *      Defer all remaining input addresses with
		 *      configuration errors.  Since it is not known if this
		 *      director would have matched any addresses, we cannot
		 *      pass the addresses on to the next director.  Thus,
		 *      we must defer all addresses not matched by a
		 *      previous director.
		 *
		 * RESOLUTION
		 *      The postmaster must check the director and transport
		 *      configurations before delivery can be performed.
		 */
		insert_addr_list(in,
				 defer,
				 note_error(ERR_CONFERR|ERR_107,
					    xprintf(
				     "director %s: driver %s driver not found",
						    dp->name,
						    dp->driver)));
		exitvalue = EX_DATAERR;
		return;
	    }
	    /* initialize list of new addrs found in this pass */
	    newaddrs = NULL;

            /* If a domain list exists, remove from the input list all those
            addresses that are not for the domains handled by this
            director. Afterwards, put them back on the input list. */

            other_domains = NULL;
            if (dp->domains) {
                struct addr *temp = in;
                struct addr **prev = &in;

                while (temp != NULL) {
                    if (!temp->local_name ||
                      !is_string_in_list(temp->local_name, dp->domains)) {
                        *prev = temp->succ;
                        temp->succ = other_domains;
                        other_domains = temp;
                        temp = *prev;
                    } else {
                        prev = &(temp->succ);
                        temp = temp->succ;
                    }
                }
            }

            /* call the driver if any left */
            if (in != NULL)
              in = (*driver->driver)(dp, in, out, &newaddrs, defer, fail);

            /* put back any that were held back */
            if (other_domains != NULL) {
                if (in == NULL) in = other_domains; else
                   insert_addr_list(other_domains, &in, (struct error *) NULL);
            }

	    /* step through newaddrs to verify them */
	    for (cur = newaddrs; cur; cur = next) {
		enum local_form form = addr_type(cur->work_addr);

		next = cur->succ;	/* note which addr to examine next */

		/*
		 * read in cache data in the director structure, if this
		 * has not yet been done.
		 */
		if (! (dp->flags & CACHED_DIRECTOR) ) {
		    cache_director_data(dp);
		}

		/* setup the uid, gid and home directories for the addr */
		if (dp->default_user) {
		    if (cur->uid == BOGUS_USER) {
			cur->uid = dp->default_uid;
		    }
		    if (cur->gid == BOGUS_GROUP) {
			cur->gid = dp->default_gid;
		    }
		}
		if (dp->default_home && cur->home == NULL) {
		    cur->home = dp->x_default_home;
		}
		if (dp->set_user) {
		    cur->uid = dp->set_uid;
		    cur->gid = dp->set_gid;
		}
		if (dp->set_home) {
		    cur->home = dp->x_set_home;
		}

		if (dp->flags & CAUTION_DIRECTOR) {
		    /* be cautious on all addrs from this director */
		    cur->flags |= ADDR_CAUTION;
		}

		if (form == OTHER_FORM) {
		    switch (finish_direct_other_form(dp, cur)) {
		    case SEND_TO_NEXT:
			/* matched the parent addr, send to next director */
			cur->succ = in;
			in = cur;
			break;
		    case DROP_ADDRESS:
			/* matched the sender, drop the address */
			break;
		    case ADDRESS_OKAY:
			/* address is okay, put back on the input */
			cur->succ = *retry;
			*retry = cur;
			break;
		    }
		} else {
		    finish_direct_special_form(cur, dp, form,
					       retry, out, defer, fail,
					       &includes);
		}
	    }
	}

	/*
	 * log addrs which were not directed anywhere
	 */
	for (; in; in = next) {
	    next = in->succ;

	    if (EQIC(in->remainder, "MAILER-DAEMON")) {
		implicit_redirect(in, retry, "Postmaster");
	    } else if (EQIC(in->remainder, "Postmaster")) {
		implicit_redirect(in, retry, postmaster_address);
	    } else {
		/* no director found the user, fail the address */
		exitvalue = EX_NOUSER;	/* also set exit status */
		/*
		 * ERR_100 - unknown user
		 *
		 * DESCRIPTION
		 *	The remainder structure in this addr structure was
		 *      not matched by any of the directors.
		 *
		 * ACTIONS
		 *      Send a message to the owner of the address, or to
		 *      the sender if there is no owner.
		 *
		 * RESOLUTION
		 *      A list owner should check through the list that he
		 *      or she owns.  A sender may wish to send mail to the
		 *      postmaster of the machine asking about login names
		 *      for a particular user.
		 */
		in->error = note_error(ERR_NSOWNER|ERR_100, "unknown user");
		in->succ = *fail;
		*fail = in;
	    }
	}

	/*
	 * copy any mailing list addrs back to the input
	 * so they can be run back through the directors.
	 */
	in = includes;
	includes = NULL;
    }
}


/*
 * verify_local_addrs - perform quick verify of local addresses
 *
 * form a list of okay (verified) addrs, plus deferred (not currently
 * determinable) addrs and failed (not deliverable) addrs.
 */
void
verify_local_addrs(in, okay, defer, fail)
    struct addr *in;			/* input local addr list */
    struct addr **okay;			/* output list of verified addrs */
    struct addr **defer;		/* temporariliy unverifiable addrs */
    struct addr **fail;			/* unverified addrs */
{
    struct director *dp;		/* temp for stepping thru directors */
    register struct addr *cur;		/* temp for stepping through addrs */
    struct addr *next;			/* next value for cur */
    struct addr *other_domains;		/* for domain-specific handling */

    DEBUG(DBG_ROUTE_HI, "verify_local_addrs called\n");

    /* do any munging of the addr structures required before directing */
    premunge_local_addrs(in);

    /*
     * give the complete input list to each director in turn.
     */
    for (dp = directors; in && dp; dp = dp->succ) {
	/* look up the director driver by name */
	struct direct_driver *driver = find_direct_driver(dp->driver);
	struct addr *retry;		/* set of addrs for next director */

	if (driver == NULL) {
	    /*
	     * ERR_107 - director driver not found
	     *
	     * DESCRIPTION
	     *      A driver name was not found in the table of director
	     *      drivers.
	     *
	     * ACTIONS
	     *      Defer all remaining input addresses with configuration
	     *      errors.  Since it is not known if this director would
	     *      have matched any addresses, we cannot pass the addresses
	     *      on to the next director.  Thus, we must defer all
	     *      addresses not matched by a previous director.
	     *
	     * RESOLUTION
	     *      The postmaster must check the director and transport
	     *      configurations before delivery can be performed.
	     */
	    insert_addr_list(in,
			     defer,
			     note_error(ERR_CONFERR|ERR_107,
					xprintf(
						"director %s: driver %s driver not found",
						dp->name,
						dp->driver)));
	    return;
	}

        /* If a domain list exists, remove from the input list all those
	   addresses that are not for the domains handled by this
	   director. Afterwards, put them back on the input list. */

        other_domains = NULL;
        if (dp->domains) {
            struct addr *temp = in;
            struct addr **prev = &in;

            while (temp != NULL) {
                if (!temp->local_name ||
		    !is_string_in_list(temp->local_name, dp->domains)) {
                    *prev = temp->succ;
                    temp->succ = other_domains;
                    other_domains = temp;
                    temp = *prev;
                } else {
                    prev = &(temp->succ);
                    temp = temp->succ;
                }
            }
        }

        /* call the driver if any left */
        retry = NULL;
        if (in != NULL)
	    (*driver->verify)(dp, in, &retry, okay, defer, fail);

        /* put back any that were held back */
        if (other_domains != NULL) {
            if (retry == NULL) retry = other_domains; else
		insert_addr_list(other_domains, &retry, (struct error *) NULL);
        }

        in = retry;
    }

    for (cur = in; cur; cur = next) {
	next = cur->succ;
	/* no director found the user, fail the address */
	exitvalue = EX_NOUSER;		/* also set exit status */
	/*
	 * ERR_100 - unknown user
	 *
	 * DESCRIPTION
	 *      The remainder structure in this addr structure was not
	 *      matched by any of the directors.
	 *
	 * ACTIONS
	 *      Send a message to the owner of the address, or to the
	 *      sender if there is no owner.
	 *
	 * RESOLUTION
	 *      A list owner should check through the list that he or
	 *      she owns.  A sender may wish to send mail to the
	 *      postmaster of the machine asking about login names for a
	 *      particular user.
	 */
	cur->error = note_error(ERR_NSOWNER|ERR_100, "unknown user");
	cur->succ = *fail;
	*fail = cur;
    }
}


/*
 * cache_directors - call cache entrypoints for all directors
 *
 * cache information used by director drivers.  This can be called
 * when it is determined that there will be an attempt to deliver more
 * than one mail message, to increase the overall efficiency of the
 * mailer.
 *
 * Daemons can call this periodically to recache stale data.
 */
void
cache_directors()
{
    struct director *dp;		/* temp for stepping thru directors */
    struct direct_driver *driver;

    for (dp = directors; dp; dp = dp->succ) {
	driver = find_direct_driver(dp->driver);
	if (driver && driver->cache) {
	    (*driver->cache)(dp);
	}
    }
    cached_directors = TRUE;
}

#ifdef notyet
/*
 * finish_directors - free resources used by all directors
 *
 * free information that was cached by directors or used by directors
 * in the process of resolving local addresses.  Directors can cache
 * data for efficiency, or can maintain state between invocations.
 * This function is called when directors will no longer be needed,
 * allowing directors to free any resources that they were using that
 * will no longer be needed.  For example, it is a good idea for
 * directors to close any files that they opened, as file descriptors
 * are a precious resource in some machines.
 */
void
finish_directors()
{
    struct directors *dp;		/* temp for stepping thru directors */
    struct direct_driver *driver;

    for (dp = directors; dp; dp = dp->succ) {
	driver = find_direct_driver(dp->driver);
	if (driver && driver->finish) {
	    (*driver->finish)(dp);
	}
    }
    cached_directors = FALSE;
}
#endif


/*
 * implicit_redirect - an implicit director to redirect to the given address
 */
static void
implicit_redirect(match, retry, in_addr)
    struct addr *match;			/* addr structure to direct */
    struct addr **retry;		/* attach new addr to this list */
    char *in_addr;			/* the text for the new address */
{
    struct addr *new = alloc_addr();

    new->in_addr = COPY_STRING(in_addr);
    new->work_addr = COPY_STRING(in_addr);
    new->succ = *retry;
    *retry = new;
    DEBUG3(DBG_DIRECT_LO,
	   "smail implicity matched %s\n    directed %s --> %s\n",
	   match->in_addr, match->in_addr, in_addr);
}

/*
 * finish_direct_other_form - finish directing a simple addr
 *
 * return SEND_TO_NEXT if this addr matches the parent addr and should
 *   thus be sent to the next director rather than being put on the
 *   list of new addrs.
 * return DROP_ADDRESS if this addr is in local form and matches the
 *   sender and should thus be ignored.  This value is not returned if
 *   the `sender_okay' attribute is set for the director, or if the
 *   me_too flag was set in the invocation arguments.
 * return ADDRESS_OKAY if the addr should be put on the list of
 *   addresses to be reparsed.
 *
 * XXX - this may change the addr structure as a side effect, so watch out.
 */
static enum other_form_op
finish_direct_other_form(dp, addr)
    struct director *dp;		/* director which returned addr */
    struct addr *addr;			/* addr struct from director */
{
    int parseflags;
    int escaped = 0;

    /* not an address form we need to be worried about */
    if (EQIC(addr->work_addr, addr->parent->remainder) ||
	(addr->work_addr[0] == '\\' &&
	 EQIC(addr->work_addr + 1, addr->parent->remainder)))
    {
	/* for the form \parent-addr drop the \ */
	if (addr->work_addr[0] == '\\') {
	    escaped = 1;
	    (void) strcpy(addr->work_addr, addr->work_addr + 1);
	}

	/*
	 * new addr same as input addr, send to
	 * the next director.  E.g., foo aliased
	 * to foo.  However, if the address was not escaped,
	 * and the ignore_alias_match flags is set on the
	 * director, just drop the address.
	 */
	if (! escaped && (dp->flags & IGNORE_ALIAS_MATCH)) {
	    DEBUG2(DBG_DIRECT_LO,
		  "    directed %s --> %s ... matched, pass to next director\n",
		   addr->parent->remainder,
		   addr->work_addr);
	    return DROP_ADDRESS;
	}
	DEBUG2(DBG_DIRECT_LO,
	       "    directed %s --> %s ... matched, pass to next director\n",
	       addr->parent->remainder,
	       addr->work_addr);
	addr->remainder = addr->work_addr;
	return SEND_TO_NEXT;
    } else {
	/*
	 * XXX - reparse the address looking for references
	 *	 to the the localhost with a remainder equal
	 *	 to the parent address.  This allows the use
	 *	 of username@localhost addresses in alias
	 *	 files, making it possible to share an alias
	 *	 file for redirecting users around a network.
	 *
	 *	 This moves intelligence out of resolve.c that
	 *	 really belongs there.
	 */
	char *temp_target;
	char *temp_remainder;
	char *temp_work_addr;
	int form;

	temp_work_addr = COPY_STRING(addr->work_addr);
	parseflags = addr->parseflags;
	form = parse_address(temp_work_addr, &temp_target, &temp_remainder,
			     &parseflags);

	switch (form) {
	case FAIL:
	case PARSE_ERROR:
	    xfree(temp_work_addr);
	    break;

	case LOCAL:
	    /*
	     * if the me_too flag is not set, then filter the sender
	     * from the output of directors.  A director can
	     * explicitly allow sender addresses to be produced by
	     * including the `sender_okay' attribute.  When expanding
	     * an address list for verification purposes, the sender
	     * may not be specified.  Watch out for this case.
	     */
	    xfree(temp_work_addr);
	    if (! (dp->flags & SENDER_OKAY) &&
		! me_too &&
		sender &&
		EQIC(sender, addr->work_addr))
	    {
		DEBUG2(DBG_DIRECT_LO,
		       "    directed %s --> %s ... match sender, ignore\n",
		       addr->parent->remainder,
		       addr->work_addr);
		return DROP_ADDRESS;
	    }
	    break;

	default:
	    if (islocalhost(temp_target) &&
		EQIC(temp_remainder, addr->parent->remainder))
	    {
		/*
		 * new addr same as input addr, send to
		 * the next director.  E.g., foo aliased
		 * to foo.
		 */
		DEBUG2(DBG_DIRECT_LO,
		   "    directed %s --> %s ... match, pass to next director\n",
		       addr->parent->remainder,
		       addr->work_addr);
		(void) strcpy(addr->work_addr, temp_remainder);
		addr->remainder = addr->work_addr;
		xfree(temp_work_addr);
		return SEND_TO_NEXT;
	    }
	}
    }

    /* we have a new address to reparse */
    DEBUG2(DBG_DIRECT_LO, "    directed %s --> %s\n",
	   addr->parent->remainder,
	   addr->in_addr);

    return ADDRESS_OKAY;
}

/*
 * finish_direct_special_form - direct a file, pipe or mailing list form addr
 */
/*ARGSUSED*/
static void
finish_direct_special_form(addr, dp, form, new, out, defer, fail, includes)
    struct addr *addr;			/* addr structure to finish */
    struct director *dp;		/* matching director */
    enum local_form form;		/* local address form */
    struct addr **new;			/* put new addrs to retry here */
    struct addr **out;			/* resolved addrs */
    struct addr **defer;		/* put config errors here */
    struct addr **fail;			/* put failed addrs here */
    struct addr **includes;		/* include file forms */
{
    /* save a pointer to the pipe transport */
    static struct transport *pipe_transport = NULL;
    /* save a pointer to the file transport */
    static struct transport *file_transport = NULL;

    /*
     * not a regular address, should we keep it or drop it?
     *
     * NOTE:  unsecure addresses are less secure than addresses which
     *	      are not secure.  Addresses which are not secure may have
     *	      accesses checked under the nobody uid/gid.  Unsecure
     *	      pipes, files and mailing lists are are always dropped.
     */
    /* only hash normal-form addresses */
    addr->flags |= ADDR_DONTHASH;
    if ((addr->flags & ADDR_UNSECURE) ||
	((addr->flags & ADDR_CAUTION) && ! (dp->flags & NOBODY_DIRECTOR)))
    {
	/*
	 * ERR_104 - security violation
	 *
	 * DESCRIPTION
	 *      A questionable address was supplied from a director
	 *      that is not allowed to supply a file command or
	 *      mailing list.
	 *
	 * ACTIONS
	 *      mail is sent to the address owner or to the
	 *      postmaster.
	 *
	 * RESOLUTION
	 *      The owner or postmaster should check the source source
	 *      of addresses against any file ownership restrictions
	 *      named in the director.
	 */
	addr->error = note_error(ERR_NPOWNER|ERR_104,
				 xprintf("director %s: security violation",
					 dp->name));
	addr->succ = *fail;
	*fail = addr;
	return;
    }
    if (addr->flags & ADDR_CAUTION) {
	/* be cautious of addresses, but nobody is set */
	if (operation_mode != VERIFY_ADDRS &&
	    operation_mode != TEST_MODE)
	{
	    write_log(LOG_SYS,
		"%s ... director %s: child of %s insecure, access as 'nobody'",
		      addr->in_addr,
		      dp->name,
		      addr->parent->in_addr);
	}
	addr->uid = nobody_uid;
	addr->gid = nobody_gid;
    }

    /* passed security checks, now what do we do? */
    switch (form) {

    case PIPE_FORM:
	/*
	 * pipe form, associate with the "pipe" transport
	 */
	if (pipe_transport == NULL) {
	    pipe_transport = find_transport("pipe");
	    if (pipe_transport == NULL) {
		/* configuration error, try again later */
		/*
		 * ERR_105 - no pipe transport
		 *
		 * DESCRIPTION
		 *      There is no "pipe" transport in the
		 *      table of transports, and a shell command
		 *      address returned by a director.
		 *
		 * ACTIONS
		 *      Defer message as a configuration error.
		 *
		 * RESOLUTION
		 *      The postmaster should add a "pipe"
		 *      transport to the transport file.
		 */
		addr->error = note_error(ERR_CONFERR|ERR_105,
					 "no pipe transport");
		addr->succ = *defer;
		*defer = addr;
		break;
	    }
	}

	addr->transport = pipe_transport;
	/* setup command as $user variable for transports */
	addr->next_addr = COPY_STRING(addr->work_addr + 1);
	DEBUG2(DBG_DIRECT_LO,
	       "    directed %s --> %s ... send to pipe transport\n",
	       addr->parent->remainder,
	       addr->in_addr);
	/* addr finished link into the output queue */
	addr->succ = *out;
	*out = addr;
	break;

    case FILE_FORM:
	/*
	 * file form, associate with the "file" transport
	 */
	if (file_transport == NULL) {
	    file_transport = find_transport("file");
	    if (file_transport == NULL) {
		/* configuration error, must be fixed */
		/*
		 * ERR_106 - no file transport
		 *
		 * DESCRIPTION
		 *      There is no "file" transport in the
		 *      table of transports, and a file form
		 *      address was returned by a director.
		 *
		 * ACTIONS
		 *      Defer message as a configuration error.
		 *
		 * RESOLUTION
		 *      The postmaster should add a "file"
		 *      transport to the transport file.
		 */
		addr->error = note_error(ERR_CONFERR|ERR_106,
					 "no file transport");
		addr->succ = *defer;
		*defer = addr;
		break;
	    }
	}
	addr->transport = file_transport;

	/* setup file as $user variable for transports */
	addr->next_addr = COPY_STRING(addr->work_addr);
	DEBUG2(DBG_DIRECT_LO,
	       "    directed %s --> %s ... send to file transport\n",
	       addr->parent->remainder,
	       addr->in_addr);
	/* addr finished link into the output queue */
	addr->succ = *out;
	*out = addr;
	break;

    case INCLUDE_FORM:
	/* mailing lists go back through all the directors */
	DEBUG2(DBG_DIRECT_LO,
	       "    directed %s --> %s ... mailing list\n",
	       addr->parent->remainder,
	       addr->in_addr);
	/* set local part to the mailing list address */
	addr->remainder = addr->work_addr;
	addr->succ = *includes;
	*includes = addr;
	break;
    }
}


/*
 * cache_director_data - cache in some generic director information
 *
 * perform once on each director to perform some lookup and expansion
 * operations that would otherwise have to be performed each time these
 * values are used.
 */
static void
cache_director_data(dp)
    register struct director *dp;
{
    if (dp->default_user) {
	struct passwd *pw = getpwbyname(dp->default_user);
	if (pw) {
	    dp->default_uid = pw->pw_uid;
	    dp->default_gid = pw->pw_gid;
	} else {
	    dp->default_user = NULL;
	}
    }
    if (dp->default_group) {
	struct group *gr = getgrbyname(dp->default_group);
	if (gr) {
	    dp->default_gid = gr->gr_gid;
	} else {
	    dp->default_group = NULL;
	}
    }
    if (dp->default_home) {
	char *x_home =
	    expand_string(dp->default_home,
			  (struct addr *)NULL,
			  (char *)NULL,
			  (char *)NULL);
	if (x_home && !EQ(x_home, dp->default_home)) {
	    dp->x_default_home = COPY_STRING(x_home);
	} else {
	    dp->x_default_home = dp->default_home;
	}
    }
    if (dp->set_user) {
	struct passwd *pw = getpwbyname(dp->set_user);
	if (pw) {
	    dp->set_uid = pw->pw_uid;
	    dp->set_gid = pw->pw_gid;
	} else {
	    dp->set_user = NULL;
	}
    }
    if (dp->set_group) {
	struct group *gr = getgrbyname(dp->set_group);
	if (gr) {
	    dp->set_gid = gr->gr_gid;
	} else {
	    dp->set_group = NULL;
	}
    }
    if (dp->set_home) {
	char *x_home =
	    expand_string(dp->set_home,
			  (struct addr *)NULL,
			  (char *)NULL,
			  (char *)NULL);
	if (x_home && !EQ(x_home, dp->set_home)) {
	    dp->x_set_home = COPY_STRING(x_home);
	} else {
	    dp->x_set_home = dp->set_home;
	}
    }
}

/*
 * premunge_local_addrs - pre-routing munging on local addr structures
 *
 * Remove any extraneously set flags.
 */
static void
premunge_local_addrs(list)
    struct addr *list;			/* list of remote addrs to premunge */
{
    register struct addr *cur;		/* current address being processed */

    for (cur = list; cur; cur = cur->succ) {
	cur->flags &= ~(ADDR_PUTDOT |
			ADDR_MOVEDOT |
			ADDR_ERROR |
			ADDR_FINISHED |
			ADDR_FULLMATCH |
			ADDR_NOTUSER |
			ADDR_ISUSER);
    }
}

/*
 * addr_type - return the form of a local address string
 *
 * Determine if the address is a pipe, file, mailing list,
 * or other.
 */
static enum local_form
addr_type(s)
    register char *s;
{
    /*
     * file is one of:
     *	  "/string"
     *	  "\/string"
     *	  "~string"
     *	  "\~string"
     *    /string
     *	  \/string
     *	  ~string
     *	  \~string
     */
    if (s[0] == '"' && ((s[1] == '/' || s[1] == '~') ||
			(s[1] == '\\' && (s[2] == '/' || s[2] == '~'))))
    {
	/* file form, but only if the address is in local form */
	register char *p = address_token(s);

	if (p && *p == '\0') {
	    /* address is in local form, strip work_addr inline */
	    (void) strip(s);
	    return FILE_FORM;
	}
    }
    if (s[0] == '/' || s[0] == '~' ||
	(s[0] == '\\' && (s[1] == '/' || s[1] == '~')))
    {
	/* filename not quoted, don't check for local form in this case */
	(void) strip(s);
	return FILE_FORM;
    }

    /*
     * pipe is one of:
     *	  "|shell-command"
     *    "\|shell-command"
     *    |shell-command
     *	  \|shell-command
     */
    if (s[0] == '"' && (s[1] == '|' || (s[1] == '\\' && s[2] == '|'))) {
	/* pipe form, but only if the address is in local form */
	char *p = address_token(s);

	if (p && *p == '\0') {
	    /* address is in local form, strip work_addr inline */
	    (void) strip(s);
	    return PIPE_FORM;
	}
    }
    if (s[0] == '|' || (s[0] == '\\' && s[1] == '|')) {
	/* shell command not quoted, don't check for local form in this case */
	(void) strip(s);
	return PIPE_FORM;
    }

    /*
     * mailing list is one of:
     *	  :include:anything
     *    ":include:anything"
     */
    if (s[0] == '"' && s[1] == ':' &&
	strncmpic(s + 2, "include:", sizeof("include:") - 1) == 0)
    {
	/* mailing list form, but only if the address is in local form */
	char *p = address_token(s);

	if (p && *p == '\0') {
	    /* address is in local form, strip work_addr inline */
	    (void) strip(s);
	    return INCLUDE_FORM;
	}
    }

    if (s[0] == ':' && strncmpic(s+1, "include:", sizeof("include:")-1) == 0)
    {
	/* shell command not quoted, don't check for local form in this case */
	return INCLUDE_FORM;
    }

    return OTHER_FORM;
}

/*
 * director_user_info - fill an addr structure with user information
 *
 *
 * if the remainder field if the addr structure matches a username,
 * fill in fields which pertain to the user's passwd file entry.
 */
void
director_user_info(addr)
    struct addr *addr;			/* addr structure to check */
{
    struct passwd *pw;			/* passwd file entry for user */
    char *dupe = NULL;			/* ptr to lowercased remainter */

    if (addr->flags & (ADDR_NOTUSER|ADDR_ISUSER)) {
	/* a previous call to director_user_info() already took care of this */
	return;
    }

    /* get the passwd entry if one exists */
    pw = getpwbyname(addr->flags & USER_IGNORE_CASE ?
		     str2lower((dupe = COPY_STRING(addr->remainder))) :
		     addr->remainder);
    if (dupe)
	    free(dupe);
    if (pw) {
	/* passwd entry found */
	addr->flags |= ADDR_ISUSER;
	addr->uid = pw->pw_uid;
	addr->gid = pw->pw_gid;
	addr->home = COPY_STRING(pw->pw_dir);
	return;
    }

    /* no passwd entry found */
    addr->flags |= ADDR_NOTUSER;
    return;
}


/*
 * find_director - given a director's name, return the director structure
 *
 * return NULL if no director of that name exists.
 */
struct director *
find_director(name)
    register char *name;		/* search key */
{
    register struct director *dp;	/* temp for stepping thru directors */

    /* loop through all the directors */
    for (dp = directors; dp; dp = dp->succ) {
	if (EQ(dp->name, name)) {
	    /* found the director in question */
	    return dp;
	}
    }

    return NULL;			/* director not found */
}

/*
 * find_direct_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct direct_driver *
find_direct_driver(name)
    register char *name;		/* search key */
{
    register struct direct_driver *ddp;	/* pointer to table of drivers */

    for (ddp = direct_drivers; ddp->name; ddp++) {
	if (EQ(ddp->name, name)) {
	    return ddp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
}


/*
 * read_director_file - read director file
 *
 * read the director file and build a director list describing the
 * entries.  Return an error message or NULL.
 */
char *
read_director_file()
{
    FILE *f;				/* open director file */
    char *error;			/* error from read_standard_file() */
    struct stat statbuf;
    struct attr_table *end_director_generic = ENDTABLE(director_generic);
    static struct director director_template = {
	NULL,				/* name */
	"pathalias",			/* driver, a reasonable default */
	NULL,				/* succ will be assigned */
	NOBODY_DIRECTOR,		/* flags */
	NULL,				/* owner */
	NULL,				/* default_user */
	NULL,				/* default_group */
	NULL,				/* default_home */
        NULL,                           /* domains */
	NULL,				/* set_user */
	NULL,				/* set_group */
	NULL,				/* set_home */
	NULL,				/* private */
	0,				/* cache - default_uid */
	0,				/* cache - default_gid */
	NULL,				/* cache - x_default_home */
	0,				/* cache - set_uid */
	0,				/* cache - set_gid */
	NULL,				/* cache - x_set_home */
    };

    extern int errno;

    /*
     * try to open directory file, stat file if possible
     */
    if (director_file == NULL || EQ(director_file, "-")) {
	return NULL;
    }
    f = fopen(director_file, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("%s:%s", director_file, strerror(errno));
	}

	add_config_stat(director_file, (struct stat *)NULL);
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(director_file, &statbuf);

    /* call read_standard_file to do the real work */
    error = read_standard_file(f,
			       (char *)&director_template,
			       sizeof(struct director),
			       OFFSET(director, name),
			       OFFSET(director, flags),
			       OFFSET(director, succ),
			       director_generic,
			       end_director_generic,
			       director_driv_function,
			       (char **)&directors);

    /* finish up */
    (void) fclose(f);

    /* return any error message */
    if (error) {
	return xprintf("%s: %s", director_file, error);
    }
    return NULL;
}

static char *
director_driv_function(struct_p, driver_attrs)
    char *struct_p;			/* passed director structure */
    struct attribute *driver_attrs;	/* driver-specific attributes */
{
    struct director *dp = (struct director *)struct_p;
    struct direct_driver *drv;

    DEBUG(DBG_ROUTE_HI, "director_driv_function called\n");
    if (dp->driver == NULL) {
	return xprintf("director %s: no driver attribute", dp->name);
    }
    drv = find_direct_driver(dp->driver);
    if (drv == NULL) {
	return xprintf("director %s: unknown driver: %s",
		       dp->name, dp->driver);
    }
    if (drv->builder) {
	return (*drv->builder)(dp, driver_attrs);
    }

    return NULL;
}

void
dump_director_config(f)
     FILE * f;
{
    struct director *dp;		/* temp for stepping thru directors */
    struct attr_table *end_director_generic = ENDTABLE(director_generic);
    char * name;			/* name of director */
    long flags;				/* flags for director */
    struct direct_driver *driver;	/* drivers for director */

    DEBUG(DBG_ROUTE_HI, "dump director configs called\n");
    fputs("#\n# -- directors configuration\n", f);

    /*
     * work through each director in turn.
     */
    for (dp = directors; dp; dp = dp->succ) {
	name = dp->name;
	flags = dp->flags;
	fprintf(f, "#\n%s:\n", name);
	(void) dump_standard_config(f,
				    (char *) dp,
				    name,
				    flags,
				    director_generic,
				    end_director_generic);
	fputs("\t;\n", f);
	driver = find_direct_driver(dp->driver);
	if (driver->dumper)
	    (*driver->dumper)(f, dp);
    }
    fputs("#\n# -- end of directors\n", f);
}
