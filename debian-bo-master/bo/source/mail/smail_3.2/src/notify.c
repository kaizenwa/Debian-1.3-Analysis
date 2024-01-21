/*
#ident	"@(#)smail/src:RELEASE-3_2:notify.c,v 1.23 1996/02/28 14:26:33 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * notify.c:
 *	keep track of errors and handle notification of the appropriate
 *	users for errors.
 *
 *	external functions: defer_delivery, fail_delivery, succeed_delivery,
 *			    error_delivery, process_msg_log, notify,
 *                          hash_predelivered_addresses
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <signal.h>
#include <time.h>
#include "defs.h"
#include "smail.h"
#include "dys.h"
#include "addr.h"
#include "hash.h"
#include "main.h"
#include "log.h"
#include "direct.h"
#include "route.h"
#include "transport.h"
#include "child.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

/* exported variables */
int send_to_postmaster;			/* set TRUE to mail to postmaster */
int return_to_sender;			/* set TRUE to mail log to sender */

/* exported functions */
void hash_predelivered_addresses();

/* functions local to this file */
static char *decode_x_line();
static void put_defer_error();
static void classify_addr();
static void notify_new_message();
static struct addr *remove_addr();
static void write_notify_header();

/* variables local to this file */
static char *log_banner = "\
|------------------------- Message log follows: -------------------------|\n";
static char *addr_error_banner = "\
|------------------------- Failed addresses follow: ---------------------|\n";
static char *text_banner = "\
|------------------------- Message text follows: ------------------------|\n";


/*
 * fail_delivery - log that delivery to the set of addresses failed
 */
void
fail_delivery(addr)
    struct addr *addr;			/* list of failed addrs */
{
    register struct addr *cur;

    if (addr && exitvalue == EX_OK) {
	exitvalue = EX_UNAVAILABLE;
    }

    for (cur = addr; cur; cur = cur->succ) {
#if (SMAIL_LOG_STYLE == 2)
	/* Default new log style (as of 3.1.29) */
	/* This is SMAIL_LOG_STYLE=2 */
	write_log(LOG_SYS,
		  "Failed TO:%s%s%s%s%s%s%s ERROR:(ERR%03ld) %s",
		  cur->in_addr,
		  cur->router? " ROUTER:": "",
		  cur->router? cur->router->name: "",
		  cur->director? " DIRECTOR:": "",
		  cur->director? cur->director->name: "",
		  cur->transport? " TRANSPORT:": "",
		  cur->transport? cur->transport->name: "",
		  cur->error->info & ERR_MASK,
		  cur->error->message);
#else
#if (!defined(SMAIL_LOG_STYLE)) || (SMAIL_LOG_STYLE == 1)
    /* Old log style (as used by 3.1.28) */
    /* This is SMAIL_LOG_STYLE=1 */
	write_log(LOG_SYS,
		  "%s ... failed: (ERR_%03ld) %s",
		  cur->in_addr,
		  cur->error->info & ERR_MASK,
		  cur->error->message);
#else
	ERROR - SMAIL_LOG_STYLE set to invalid value.
#endif
#endif
	/* X entries in msg log will be picked up by queue run scans */
	if (cur->parent) {
	    write_log(LOG_MLOG,
		      "Xfail: <%s> parent: <%s> reason: (ERR%03ld) %s",
		      cur->in_addr,
		      cur->parent->in_addr,
		      cur->error->info & ERR_MASK,
		      cur->error->message);
	} else {
	    write_log(LOG_MLOG,
		      "Xfail: <%s> reason: (ERR%03ld) %s",
		      cur->in_addr,
		      cur->error->info & ERR_MASK,
		      cur->error->message);
	}
    }
}

/*
 * defer_delivery - log that delivery to the set of addresses is deferred
 *
 * don't write an error message, if it would be a repeat of the most
 * recent deferal message written to the message log file for the
 * address.
 */
void
defer_delivery(addr, defer_errors)
    struct addr *addr;			/* list of succeeded addrs */
    struct defer_addr *defer_errors;	/* previous deferals */
{
    struct addr *cur;
    struct defer_addr *link;

    if (addr && exitvalue == EX_OK) {
	exitvalue = EX_TEMPFAIL;
    }

    for (cur = addr; cur; cur = cur->succ) {

	/* if the deferral's only cause is retry interval, don't log it */
	if (cur->error->info & ERR_DONTLOG) {
#ifndef NODEBUG
	    /* but, if debugging, generate a message for human reader */
	    if (debug && errfile && debug > 1) {
		fprintf(errfile,
			"%s ... deferred: (ERR_DONTLOG|ERR%03ld) %s\n",
			cur->in_addr,
			cur->error->info & ERR_MASK,
			cur->error->message);
	    }
#endif	/* ! NODEBUG */
	    continue;
	}

	/* avoid redundant messages */
	for (link = defer_errors; link; link = link->succ) {
	    if (EQ(link->address, cur->in_addr) ||
		(link->parent == NULL && cur->parent == NULL) ||
		(link->parent != NULL && cur->parent != NULL &&
		 EQ(link->parent, cur->parent->in_addr)))
	    {
		break;
	    }
	}
	if (link) {
	    if (link->error == (cur->error->info & ERR_MASK) &&
		EQ(link->message, cur->error->message))
	    {
		break;
	    }
	}

#if (SMAIL_LOG_STYLE == 2)
	/* Default new log style (as of 3.1.29) */
	/* This is SMAIL_LOG_STYLE=2 */
	write_log((cur->error->info & ERR_CONFERR)? (LOG_SYS|LOG_TTY): LOG_SYS,
		  "Deferred TO:%s%s%s%s%s%s%s ERROR:(ERR%03ld) %s",
		  cur->in_addr,
		  cur->router? " ROUTER:": "",
		  cur->router? cur->router->name: "",
		  cur->director? " DIRECTOR:": "",
		  cur->director? cur->director->name: "",
		  cur->transport? " TRANSPORT:": "",
		  cur->transport? cur->transport->name: "",
		  cur->error->info & ERR_MASK,
		  cur->error->message);
#else
#if (!defined(SMAIL_LOG_STYLE)) || (SMAIL_LOG_STYLE == 1)
    /* Old log style (as used by 3.1.28) */
    /* This is SMAIL_LOG_STYLE=1 */
	write_log((cur->error->info & ERR_CONFERR)? (LOG_SYS|LOG_TTY): LOG_SYS,
                  "%s ... deferred: (ERR_%03ld) %s",
                  cur->in_addr,
                  cur->error->info & ERR_MASK,
                  cur->error->message);
#else
	ERROR - SMAIL_LOG_STYLE set to invalid value.
#endif
#endif
	if (cur->parent) {
	    write_log(LOG_MLOG,
		      "Xdefer: <%s> parent: <%s> reason: (ERR%03ld) %s",
		      cur->in_addr,
		      cur->parent->in_addr,
		      cur->error->info & ERR_MASK,
		      cur->error->message);
	} else {
	    write_log(LOG_MLOG,
		      "Xdefer: <%s> reason: (ERR%03ld) %s",
		      cur->in_addr,
		      cur->error->info & ERR_MASK,
		      cur->error->message);
	}
    }
}

/*
 * succeed_delivery - log that delivery to the set of addresses succeeded
 */
void
succeed_delivery(addr)
    struct addr *addr;			/* list of succeeded addrs */
{
    register struct addr *cur;
    register struct addr *top;

    for (cur = addr; cur; cur = cur->succ) {
	/* find the top parent to log the original in_addr */
	for (top = cur; top->parent; top = top->parent);

	if (dont_deliver) {
	    write_log(LOG_SYS, "Debugging: don't deliver %s",
		      top->in_addr);
	} else {
#if (SMAIL_LOG_STYLE == 2)
	    /* Default new log style (as of 3.1.29) */
	    /* This is SMAIL_LOG_STYLE=2 */
	    if (cur->next_host) {
		write_log(LOG_SYS, "Delivered%s%s%s%s%s%s%s%s%s%s",
			  " VIA:", cur->next_host,
			  " TO:", cur->next_addr,
			  " ORIG-TO:", top->in_addr,
			  " ROUTER:", cur->router->name,
			  " TRANSPORT:", cur->transport->name);
	    } else {
		write_log(LOG_SYS, "Delivered%s%s%s%s%s%s%s%s",
			  " TO:", cur->next_addr,
			  " ORIG-TO:", top->in_addr,
			  " DIRECTOR:", cur->director->name,
			  " TRANSPORT:", cur->transport->name);
	    }
#else
#if (!defined(SMAIL_LOG_STYLE)) || (SMAIL_LOG_STYLE == 1)
	    /* Old log style (as used by 3.1.28) */
	    /* This is SMAIL_LOG_STYLE=1 */
	    if (cur->next_host) {
		write_log(LOG_SYS, "delivered%s%s%s%s%s%s%s%s%s%s",
			  "\n|\t      via: ", cur->next_host,
                          "\n|\t       to: ", cur->next_addr,
                          "\n|\t  orig-to: ", top->in_addr,
                          "\n|\t   router: ", cur->router->name,
                          "\n|\ttransport: ", cur->transport->name);
	    } else {
                write_log(LOG_SYS, "delivered%s%s%s%s%s%s%s%s",
                          "\n|\t       to: ", cur->next_addr,
                          "\n|\t  orig-to: ", top->in_addr,
                          "\n|\t director: ", cur->director->name,
                          "\n|\ttransport: ", cur->transport->name);

	    }
#else
	ERROR - SMAIL_LOG_STYLE set to invalid value.
#endif
#endif
	}
	/* X entries will be picked up by queue run scans */
	if (cur->parent) {
	    write_log(LOG_MLOG, "Xsucceed: <%s> parent: <%s>",
		      cur->in_addr, cur->parent->in_addr);
	} else {
	    write_log(LOG_MLOG, "Xsucceed: <%s>", cur->in_addr);
	}
    }
}

/*
 * error_delivery - log that an error was sent for this list of addrs
 *
 * detect mail sent to owners and log the original address.
 */
void
error_delivery(addr)
    struct addr *addr;			/* list of error addrs */
{
    register struct addr *cur;

    for (cur = addr; cur; cur = cur->succ) {
	register struct addr *log_addr;
#if (SMAIL_LOG_STYLE == 2)
	/* Default new log style (as of 3.1.29) */
	/* This is SMAIL_LOG_STYLE=2 */
	if (cur->true_addr) {
	    log_addr = cur->true_addr;
	    write_log(LOG_SYS, "Error sent FOR:%s TO:%s",
		      log_addr->in_addr, cur->in_addr);
	} else {
	    if (cur->error->info & ERR_NSENDER) {
		write_log(LOG_SYS, "Returned error FOR:%s TO:%s",
			  cur->in_addr, sender);
	    }
	    if (cur->error->info & ERR_NPOSTMAST) {
		write_log(LOG_SYS, "Sent error FOR:%s TO:postmaster",
			  cur->in_addr);
	    }
	}
#else
#if (!defined(SMAIL_LOG_STYLE)) || (SMAIL_LOG_STYLE == 1)
	/* Old log style (as used by 3.1.28) */
	/* This is SMAIL_LOG_STYLE=1 */
	if (cur->true_addr) {
	    log_addr = cur->true_addr;
	    write_log(LOG_SYS, "%s ... error sent to %s",
		      log_addr->in_addr, cur->in_addr);
	} else {
	    if (cur->error->info & ERR_NSENDER) {
		write_log(LOG_SYS, "%s ... error returned to %s",
			  cur->in_addr, sender);
	    }
	    if (cur->error->info & ERR_NPOSTMAST) {
		write_log(LOG_SYS, "%s ... error sent to postmaster",
			  cur->in_addr);
	    }
	}
#else
	ERROR - SMAIL_LOG_STYLE set to invalid value.
#endif
#endif
	/* X entries will be picked up by queue run scans */
	if (cur->parent) {
	    write_log(LOG_MLOG, "Xsent_error: <%s> parent: <%s>",
		      cur->in_addr, cur->parent->in_addr);
	} else {
	    write_log(LOG_MLOG, "Xsent_error: <%s>", cur->in_addr);
	}
    }
}


/*
 * process_msg_log - process X lines in the per-message log
 *
 * Lines beginning with X in the per-message log contain information on what
 * mail processing was completed in a previous run on the input spool file.
 * This function takes, as input, the set of addresses which were produced
 * by resolve_addr_list() for delivery and returns a list which does not
 * contain addresses which have already been delivered.
 *
 * It also returns a pointer to information which should be passed to the
 * notify() routine after all the transports have been called.
 */
struct addr *
process_msg_log(in, sent_errors, defer_errors)
    struct addr *in;			/* input resolved addresses */
    struct identify_addr **sent_errors;	/* return errors that have been sent */
    struct defer_addr **defer_errors;	/* last defer message for addrs */
{
    register struct addr *ret;		/* addr list to return */
    char *s;				/* line of text from scan_msg_log() */
    char *m;				/* defer message */

    ret = in;

    /* scan through all of the lines marked X in the message log file */
    for (s = scan_msg_log(TRUE); s; s = scan_msg_log(FALSE)) {
	char *address;			/* the address to remove */
	char *parent;			/* parent of address to remove */

	if (strncmp(s, "Xfail:", sizeof("Xfail:") - 1) == 0) {
	    /* get the failed address and the parent address */
	    (void) decode_x_line(s + sizeof("Xfail:") - 1 , &address, &parent);

	    /* remove any occurance from the input list */
	    if (address) {
		ret = remove_addr(ret, address, parent);
	    }
	} else if (strncmp(s, "Xsucceed:", sizeof("Xsucceed:") - 1) == 0) {
	    /* get the delivered address and the parent address */
	    (void) decode_x_line(s + sizeof("Xsucceed:") - 1,
				&address, &parent);

	    /* remove any occurance from the input list */
	    if (address) {
		ret = remove_addr(ret, address, parent);
	    }
	} else if (strncmp(s, "Xdefer:", sizeof("Xdefer:") - 1) == 0) {
	    /* grab the message and error number of the defered address */
	    m = decode_x_line(s + sizeof("Xdefer:") - 1, &address, &parent);

	    /* put defer message into output defer_error list */
	    if (m) {
		put_defer_error(defer_errors, m, address, parent);
	    }
	} else if (strncmp(s, "Xsent_error:", sizeof("Xsent_error:")-1) == 0) {
	    register struct identify_addr *new_sent;

	    /* get the sent error address and the parent address */
	    (void) decode_x_line(s + sizeof("Xsent_error:") - 1,
				 &address, &parent);

	    if (address == NULL) {
		continue;
	    }
	    /* give this address to notify */
	    new_sent = (struct identify_addr *)xmalloc(sizeof(*new_sent));
	    new_sent->address = COPY_STRING(address);
	    if (parent) {
		new_sent->parent = COPY_STRING(parent);
	    } else {
		new_sent->parent = NULL;
	    }
	    new_sent->succ = *sent_errors;
	    *sent_errors = new_sent;
	}
    }

    return ret;
}

/*
 * decode_x_line - decode an X-line from the per-message log file
 *
 * The input should be of the form:
 *
 *	<address> parent: <parent_address> garbage...
 * or	<address> garbage...
 *
 * this routine returns the address and the parent_address.  It also
 * returns a pointer to the garbage.
 */
static char *
decode_x_line(line, address, parent)
    char *line;				/* input line */
    char **address;			/* return address here */
    char **parent;			/* return parent address here */
{
    int level;				/* count < and > nesting */
    register char *p = line;		/* point to parts of the line */

    /* skip initial white-space */
    while (isspace(*p)) p++;

    /* ignore anything not of the proper form */
    if (*p != '<') {
	*address = NULL;
	return NULL;
    }
    p++;				/* skip the initial < */

    /*
     * extract the address as the text up until before the balancing
     * > character, in <address>.
     */
    *address = p;			/* mark this spot as the address */
    level = 1;				/* start at nesting level 1 */
    if (*p == '<') {
	level++;
    }
    while (level > 0) {
	p = address_token(p);
	if (p == NULL) {
	    *address = NULL;		/* bad input form */
	    return NULL;
	}
	if (*p == '<') {
	    level++;
	} else if (*p == '>') {
	    --level;
	}
    }
    *p++ = '\0';			/* the last > is the end of address */

    /* scan for a possible parent address */
    while (isspace(*p)) p++;		/* skip up until parent: */

    /* is there a parent address? */
    if (strncmp(p, "parent:", sizeof("parent:") - 1) != 0) {
	/* no */
	*parent = NULL;
	return p;
    }
    p += sizeof("parent:") - 1;

    /* skip initial white-space before actual parent address */
    while (isspace(*p)) p++;

    /* ignore anything not of the proper form */
    if (*p != '<') {
	*parent = NULL;
	return NULL;
    }
    p++;				/* skip the initial < */

    /*
     * extract the parent as the text up until before the balancing
     * > character, in <parent_addres>
     */
    *parent = p;
    level = 1;
    if (*p == '<') {
	level++;
    }
    while (level > 0){
	p = address_token(p);
	if (p == NULL) {
	    *address = NULL;
	    return NULL;
	}
	if (*p == '<') {
	    level++;
	} else if (*p == '>') {
	    --level;
	}
    }
    *p++ = '\0';

    while (isspace(*p)) p++;

    return p;
}

/*
 * put_defer_error - put a deferal message for an address in a list
 *
 * remove previous entries for this address from the list.
 */
static void
put_defer_error(list, message, address, parent)
    struct defer_addr **list;
    char *message;
    char *address;
    char *parent;
{
    int error;
    static char reason_pfx[] = "reason: (ERR";
    char *p;
    struct defer_addr *link, *next_link, *new_list;

    if (strncmp(message, reason_pfx, sizeof(reason_pfx) - 1) != 0) {
	/* doesn't match the correct format */
	return;
    }
    message += sizeof(reason_pfx) - 1;
    p = message;
    while (isdigit(*message)) message++;
    if (*message != ')') {
	/* doesn't match the correct format */
	return;
    }
    *message = 0;
    error = atoi(p);
    *message++ = ')';
    while (isspace(*message)) message++;

    new_list = (struct defer_addr *)xmalloc(sizeof(*new_list));
    new_list->succ = NULL;
    new_list->error = error;
    new_list->message = COPY_STRING(message);
    new_list->address = COPY_STRING(address);
    new_list->parent = parent? COPY_STRING(parent): NULL;

    for (link = *list; link; link = next_link) {
	next_link = link->succ;
	if (EQ(link->address, address) ||
	    (link->parent == NULL && parent == NULL) ||
	    (link->parent != NULL && parent != NULL &&
	     EQ(link->parent, parent)))
	{
	    continue;
	}
	link->succ = new_list;
	new_list = link;
    }
    *list = new_list;
}


/*
 * notify - notify all users who are to receive a note about errors
 *
 * also, note any configuration errors and set call_defer_message
 * if any are found.  If any addrs exist in the defer list, set
 * some_deferred_addrs to prevent main from calling unlink_spool().
 */
void
notify(defer, fail, sent_errors)
    struct addr *defer;			/* list of deferred addresses */
    struct addr *fail;			/* list of failed addresses */
    struct identify_addr *sent_errors;	/* addrs already handled */
{
    register struct addr *cur;		/* current addr being processed */
    struct addr *next;			/* next addr to process */
    struct addr *to_owner = NULL;	/* addrs to send to addr owners */
    struct addr *verify = NULL;		/* list of owner addrs to verify */
    struct addr *to_other = NULL;	/* to sender and/or postmaster */
    static char *mailargs[] = {
	NULL,
	"-bS",
	NULL,
    };
    FILE *f;				/* stdin to child process */
    int pid;				/* child process id */
    char *cc1;				/* Cc: address */
    char *cc2;				/* another Cc: address */

    DEBUG(DBG_NOTIFY_HI, "notify called\n");

    /* set the exec name to the smail binary */
    mailargs[0] = smail;

    /* remove any address which have already been taken care of */
    while (sent_errors) {
	defer = remove_addr(defer, sent_errors->address, sent_errors->parent);
	fail = remove_addr(fail, sent_errors->address, sent_errors->parent);
	sent_errors = sent_errors->succ;
    }

    /*
     * for deferred addresses, don't return anything, though note config
     * errors and defer the message if any were found.
     */
    for (cur = defer; cur; cur = next) {
	next = cur->succ;

	some_deferred_addrs = TRUE;	/* main should not call unlink_spool */
	if (cur->error->info & ERR_CONFERR) {
	    call_defer_message = TRUE;	/* config error, defer whole message */
	}
    }

    /*
     * for failed addresses put them in two lists, one list going to
     * the sender or the postmaster, the other list going to addr owners.
     */
    for (cur = fail; cur; cur = next) {
	next = cur->succ;

	classify_addr(cur, &to_other, &verify);
    }

    /* verify all owners.  If some don't verify, retry classify_addr() */
    while (verify) {
	struct addr *new_list = NULL;	/* unverified addrs */

	/*
	 * treat defer and fail lists as identical, since we cannot
	 * reasonably handle defer addresses here.
	 */
	verify_addr_list(verify, &to_owner, &new_list, &new_list);

	/* take any new addrs and send through classify_addr */
	verify = NULL;
	for (cur = new_list; cur; cur = next) {
	    next = cur->succ;
	    classify_addr(cur, &to_other, &verify);
	}
    }

    /* sort the list of verified addresses */
    if (to_owner) {
	to_owner = addr_sort(to_owner, OFFSET(addr, in_addr));
    }

    if (return_to_sender) {
	/*
	 * if return_to_sender is on but the error_processing flag
	 * doesn't call for a return message, then turn the flag off.
	 */
	if (error_processing != MAIL_BACK && error_processing != WRITE_BACK) {
	    return_to_sender = FALSE;
	}
	/* turn it off, as well, for low priority messages */
	if (islower(msg_grade) && msg_grade >= 'n') {
	    return_to_sender = FALSE;
	}
    }

    /*
     * lastly, if the sender was given as the special string <> or <+>,
     * then an error message failed.  Rather than risk an infinite
     * loop here, defer the message rather than delivering a recursive
     * error message.
     *
     * error message loops can still exist if SMTP is not used or if
     * an intermediate site does not correctly handle the special
     * sender string <>.
     */
    if (error_sender) {
	if (return_to_sender || send_to_postmaster || to_owner) {
	    call_defer_message = TRUE;
	    return;
	}
    }

    /* if nobody needs to be notified, then nothing needs to be done */
    if (! return_to_sender &&
	! send_to_postmaster &&
	to_owner == NULL)
    {
	return;
    }

    /*
     * open a channel to a new smail process, operating in bsmtp mode,
     * and send it messages for all of the recipients.
     */
    pid = open_child(mailargs, (char **)NULL, &f, (FILE **)NULL, -1,
		     CHILD_MINENV|CHILD_RETRY, prog_euid, prog_egid);
    if (pid < 0) {
	DEBUG(DBG_NOTIFY_LO, "notify: open_child failed, cannot notify\n");
	/* if we can't notify, let humans interfere later */
	call_defer_message = TRUE;
	return;
    }

    /* start up the session */
    (void) fprintf(f, "HELO %s\n", primary_name);

    /*
     * if any owners are to receive a message, then send to each
     * owner in turn
     */
    cur = to_owner;
    while (cur) {
	char *last_owner;

	/* initiate a new message */
	cc1 = cc2 = NULL;
	if (send_to_postmaster)
	    cc1 = "postmaster";
	if (return_to_sender)
	    cc2 = sender;
	notify_new_message(f, cur->in_addr, cc1, cc2,
			   "sending to address owner");
	send_log(f, FALSE, log_banner);	/* don't include X lines from log */
	(void) fputs(addr_error_banner, f);
	/* write out all of the address errors */
	do {
	    if (cur->true_addr->error) {
		(void) fprintf(f, " %s ... failed: %s\n",
			       cur->true_addr->in_addr,
			       cur->true_addr->error->message);
	    }
	    last_owner = cur->in_addr;
	    cur = cur->succ;
	} while (cur && EQIC(last_owner, cur->in_addr));
	(void) fputs(text_banner, f);
	write_notify_header(f);
	putc('\n', f);
	write_body(f, (long)PUT_DOTS);
	(void) fputs(".\n", f);		/* finish the message */
    }

    /* if the postmaster is to receive mail, then send it to him (her?) */
    if (send_to_postmaster) {
	int wrote_addr_error_banner = FALSE; /* only write banner once */

	cc1 = cc2 = NULL;
	if (return_to_sender)
	    cc2 = sender;
	notify_new_message(f, "postmaster", cc1, cc2,
			   "sending to postmaster");
	send_log(f, TRUE, log_banner);	/* include X lines from log */
	/* write messages in to_other destined to the postmaster */
	for (cur = to_other; cur; cur = cur->succ) {
	    if (cur->error->info & ERR_NPOSTMAST) {
		if (!wrote_addr_error_banner) {
		    (void) fputs(addr_error_banner, f);
		    wrote_addr_error_banner = TRUE;
		}
		(void) fprintf(f, " %s ... %s\n",
			       cur->in_addr, cur->error->message);
	    }
	}
	(void) fputs(text_banner, f);
	write_notify_header(f);
	putc('\n', f);
	write_body(f, (long)PUT_DOTS);
	(void) fputs(".\n", f);		/* finish the message */
    }

    /* if the sender is to receive mail, arrange for that */
    if (return_to_sender) {
	int wrote_addr_error_banner = FALSE; /* only write banner once */

	cc1 = cc2 = NULL;
	if (send_to_postmaster)
	    cc1 = "postmaster";
	notify_new_message(f, sender, cc1, cc2, "returning to sender");
	send_log(f, FALSE, log_banner);	/* don't include X lines from log */
	/* write messages in to_other destined to the sender */
	for (cur = to_other; cur; cur = cur->succ) {
	    if (cur->error->info & ERR_NSENDER) {
		if (!wrote_addr_error_banner) {
		    (void) fputs(addr_error_banner, f);
		    wrote_addr_error_banner = TRUE;
		}
		(void) fprintf(f, " %s ... %s\n",
			       cur->in_addr, cur->error->message);
	    }
	}
	(void) fputs(text_banner, f);

	/*
	 * a message grade between a and z causes body not to be returned.
	 * This is handy when notification of errors is desired, but
	 * when the overhead of sending the entire message back is not
	 * necessary.
	 */
	write_notify_header(f);
	putc('\n', f);
	if (islower(msg_grade)) {
	    fputs("[low-priority message, body not included]\n", f);
	} else {
	    write_body(f, (long)PUT_DOTS);
	}
	(void) fputs(".\n", f);		/* finish the message */
    }

    (void) fputs("QUIT\n", f);

    (void) close_child(f, (FILE *)NULL, pid);

    /* note the addresses for which error messages have been sent */
    error_delivery(to_owner);
    error_delivery(to_other);
}


/*
 * classify_addr - determine who is to be sent an error message
 *
 * If postmaster or the sender is to receive an error, link into the
 * to_other list, setting send_to_postmaster and/or return_to_sender
 * appropriately.  If a config error is found, set call_defer_message.
 *
 * If we need to send to an owner, determine if an owner string is
 * defined and link an addr structure for that owner into the to_owner
 * list.
 */
static void
classify_addr(addr, to_other, to_owner)
    register struct addr *addr;		/* input address */
    struct addr **to_other;		/* list to sender or postmaster */
    struct addr **to_owner;		/* list to address owner */
{
    int link_to_other = FALSE;		/* TRUE to link into to_other list */

    if (error_copy_postmaster)
	addr->error->info |= ERR_NPOSTMAST;

    if (addr->error->info & (ERR_NSENDER|ERR_NPOSTMAST)) {
	/* send to either the sender or the postmaster */
	link_to_other = TRUE;

	/* note which one */
	if (addr->error->info & ERR_NSENDER) {
	    return_to_sender = TRUE;
	}
	if (addr->error->info & ERR_NPOSTMAST) {
	    send_to_postmaster = TRUE;
	}
    }

    if (addr->error->info & (ERR_NSOWNER|ERR_NPOWNER)) {
	/* send to an address owner, if one is defined and expandable */
	register char *s = NULL;	/* expanded owner string */
	register struct addr *cur;	/* addr for scanning parents */
	char *work_addr;		/* output from preparse_address() */

	/*
	 * find the closest parent associated with a director that
	 * defines an owner string.
	 */
	for (cur = addr; cur; cur = cur->parent) {
	    char *error;		/* temp for storing parse error */

	    /*
	     * must have a reasonable, expandable owner address
	     */
	    if (cur->director &&
		cur->director->owner &&
		(s = expand_string(cur->director->owner,
				   (struct addr *)NULL,
				   cur->home,
				   cur->remainder)) &&
		(work_addr = preparse_address(s, &error)))
	    {
		break;
	    }
	}
	if (cur) {
	    /* owner found for the address, build entry in to_owner list */
	    struct addr *new = alloc_addr();

	    new->succ = *to_owner;		/* link */
	    *to_owner = new;
	    new->in_addr = COPY_STRING(s);	/* copy owner address */
	    /*
	     * if this addr has already been seen by classify_addr() use
	     * the true_addr assigned previously, otherwise this is the
	     * first time through, so assign the original addr
	     */
	    if (addr->true_addr) {
		new->true_addr = addr->true_addr;
	    } else {
		new->true_addr = addr;		/* keep track of true addr */
	    }
	    new->work_addr = work_addr;		/* use the preparsed address */
	    /* disable use of smartuser driver */
	    new->flags = ADDR_SMARTUSER;
	    /*
	     * if verify_addr_list() fails to verify this list look higher
	     * up for a parent that has an associated owner.
	     */
	    new->parent = cur->parent;
	} else {
	    /* no owner for the address, send to sender and/or postmaster */
	    if (addr->true_addr) {
		int perhaps_link = FALSE; /* true to see link, if not linked */
		struct error *true_error = addr->true_addr->error;

		if ((addr->error->info & ERR_NSOWNER) ||
		    (addr->error->info & ERR_NPOWNER))
		{
		    perhaps_link = TRUE;
		}

		/* only link if this has not been linked to to_other before */
		if (perhaps_link) {
		    if (! (true_error->info & (ERR_NSENDER|ERR_NPOSTMAST)) ) {
			/* link the true address, not this address */
			addr = addr->true_addr;
			link_to_other = TRUE;
		    }
		}
	    } else {
		link_to_other = TRUE;
	    }
	    if (addr->error->info & ERR_NSOWNER) {
		addr->error->info |= ERR_NSENDER;
		return_to_sender = TRUE;
	    }
	    if (addr->error->info & ERR_NPOWNER) {
		addr->error->info |= ERR_NPOSTMAST;
		send_to_postmaster = TRUE;
	    }
	}
    }

    /* if link_to_other was set anywhere above, link into to_other list */
    if (link_to_other) {
	addr->succ = *to_other;
	*to_other = addr;
    }
}

static void
notify_new_message(f, to, cc1, cc2, subject_to)
    FILE *f;
    char *to;				/* recipient */
    char *cc1, *cc2;			/* show carbon copy addresses */
    char *subject_to;			/* message for inclusion in subject */
{
    /*
     * initiate a new message
     * NOTE:  <+> is a smail-internal address form that causes the
     * sender to be considered local, while the SMTP <> address is
     * considered remote.  This is actually important.
     */
    (void) fprintf(f, "MAIL FROM:<+>\n");

    /* state the recipient */
    (void) fprintf(f, "RCPT TO:<%s>\n", to);

    /* start the message text, with the complete header */
    (void) fprintf(f, "DATA\nFrom: <MAILER-DAEMON@%s>\n", visible_name);
    if (to[0] == '@') {
	/* watch out for route-addrs */
	(void) fprintf(f, "To: <%s>\n", to);
    } else {
	(void) fprintf(f, "To: %s\n", to);
    }
    if (cc1 == NULL && cc2) {
	cc1 = cc2;
	cc2 = NULL;
    }
    if (cc1) {
	if (cc1[0] == '@')
	    fprintf(f, "Cc: <%s>", cc1);
	else
	    fprintf(f, "Cc: %s", cc1);
	if (cc2) {
	    if (cc2[0] == '@')
		fprintf(f, ", <%s>", cc2);
	    else
		fprintf(f, ", %s", cc2);
	}
	putc('\n', f);
    }
    (void) fprintf(f, "Subject: mail failed, %s\nReference: <%s@%s>\n\n",
		   subject_to, message_id, primary_name);
}

/*
 * remove_addr - remove any matched addresses from an input list
 *
 * given an address string and (perhaps) a parent address string and
 * an input address list, remove any occurance of an address in the
 * input list whose in_addr matches the specified address string and
 * whose parent in_addr string matches the specified parent string.
 * If parent is NULL then there must not be a parent address, otherwise
 * there must be a matching parent address.
 */
static struct addr *
remove_addr(in, address, parent)
    struct addr *in;			/* input addr list */
    char *address;			/* address to match against */
    char *parent;			/* parent of address to match */
{
    register struct addr *cur;		/* current address to process */
    struct addr *next;			/* next address to process */
    struct addr *out = NULL;		/* output address list */

    for (cur = in; cur; cur = next) {
	next = cur->succ;

	if (EQ(cur->in_addr, address)) {
	    /* the address does matches */
	    if (parent) {
		/* a matching parent is required for a match */
		if (cur->parent && EQ(parent, cur->parent->in_addr)) {
		    /* match, don't put it on the output queue */
		    DEBUG1(DBG_NOTIFY_LO, "%s ... already delivered\n",
			   cur->in_addr);
		    continue;
		}
	    } else if (cur->parent == NULL) {
		/* match, don't put it on the output queue */
		DEBUG1(DBG_NOTIFY_LO, "%s ... already delivered\n",
		       cur->in_addr);
		continue;
	    }
	}

	/* no match, put the address on the output queue */
	cur->succ = out;
	out = cur;
    }

    return out;				/* return the new list */
}

static void
write_notify_header(f)
    FILE *f;
{
    struct transport tport;
    struct addr addr;

    bzero(&tport, sizeof(tport));
    bzero(&addr, sizeof(addr));
    addr.transport = &tport;
    tport.name = "error";
    tport.flags = PUT_RECEIVED | LOCAL_XFORM;
    write_header(f, &addr);
}


/*
 * This code attempts to optimise the reprocessing of queued mail
 * by putting sucessfully delivered addresses into the hash table
 * to prevent them being put through the routers.
 * This cuts down the time taken doing DNS lookups dramatically
 * especially for mailing list mail.
 *
 * It is also in the wrong place - its just that I use decode_x_lines
 * which is local to this file...!  (sorry)
 */
void
hash_predelivered_addresses()
{
    char *s;				/* working string */
    struct str work_address;
    int  offset;
    char *address;			/* the address to remove */
    char *parent;			/* parent of address to remove */
    char *target;			/* Working info from parsed address */
    char *remainder;			/* Working info from parsed address */
    int  parse_flag;			/* user by parse_address */
    char *error; 			/* error returned by preparse_address() */
    char *preparsed_address;		/* address stripped of non-essencials*/

    DEBUG(DBG_NOTIFY_HI, "hash_predelivered_addresses called...\n");
    STR_INIT(&work_address);
    for (s = scan_msg_log(TRUE); s; s = scan_msg_log(FALSE)) {
	if (strncmp(s, "Xfail:", sizeof("Xfail:") - 1) == 0)
	    offset = sizeof("Xfail:") - 1;
	else if (strncmp(s, "Xsucceed:", sizeof("Xsucceed:") - 1) == 0)
	    offset = sizeof("Xsucceed:") - 1;
	else
	    offset = 0;

	if (offset) {
	    /* get the failed address and the parent address */
	    (void) decode_x_line(s + offset , &address, &parent);
	  
	    /* Strip <> etc */
	    preparsed_address = preparse_address(address, &error);
	    if (preparsed_address == NULL) {
		continue;
	    }

	    /* Find if  address is non-local */
	    if (*preparsed_address != '|') { /* Ensure piped addresses are stripped out */
		work_address.i = 0;
		STR_CAT(&work_address, preparsed_address);
		parse_flag = 0;
		if ((parse_address(work_address.p, &target, &remainder, &parse_flag) != LOCAL) 
		    && (!islocalhost(target))) {
		    DEBUG1(DBG_NOTIFY_LO, "Hashing out %s ... previously processed.\n", address);
		    (void) add_to_hash(address, (char *)NULL, 0, hit_table);
		}
	    }
	}
    }
}

