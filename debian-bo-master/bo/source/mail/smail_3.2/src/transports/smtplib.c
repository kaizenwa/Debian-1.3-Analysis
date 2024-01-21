/*
#ident	"@(#)smail/src/transports:RELEASE-3_2:smtplib.c,v 1.19 1996/03/05 19:03:17 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smtplib.c:
 *	Send mail using the SMTP protocol.  This soure file is a set
 *	of library routines that can be used by transport drivers that
 *	can handle creating the virtual circuit connections.
 */
#include <sys/types.h>
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include "defs.h"
#include "../smail.h"
#include "../dys.h"
#include "../jump.h"
#include "../addr.h"
#include "../transport.h"
#include "../spool.h"
#include "smtplib.h"
#ifndef DEPEND
# include "../extern.h"
# include "../error.h"
# include "../debug.h"
#endif
#if !defined(NO_LOG_EHLO)
#  include "../log.h"
#endif

#ifdef ANSI_C
# define P_(x)  x
# define VOLATILE volatile
#else
# define P_(x)  ()
# define VOLATILE /**/
#endif

/* supported SMTP commands */
#define EHLO(domain)	"EHLO %s", domain
#define HELO(domain)	"HELO %s", domain
#define MAIL_BEGIN	"MAIL FROM:<"
#define MAIL_END	">"
#define RCPT_BEGIN	"RCPT TO:<"
#define RCPT_END	">"
#define DATA		"DATA"
#define DATA_END	"."
#define QUIT		"QUIT"
#define VERB		"VERB"

/* reply code groups, encoded in hex */
#define POSITIVE_DEBUG		0x000	/* debugging messages */
#define POSITIVE_PRELIM		0x100	/* positive preliminary replies */
#define POSITIVE_COMPLETE	0x200	/* positive completion replies */
#define POSITIVE_INTERMEDIATE	0x300	/* positive intermediate replies */
#define NEGATIVE_TRY_AGAIN	0x400	/* transient negative completion */
#define NEGATIVE_FAILED		0x500	/* permanent negative completion */

#define REPLY_GROUP(c)	   ((c)&0xf00)	/* mask out reply code group */

/* specific reply codes */
#define REPLY_READY		0x220	/* SMTP service ready */
#define REPLY_FINISHED		0x221	/* SMTP service finished, closing */
#define REPLY_OK		0x250	/* successful command completion */
#define REPLY_WILLFORWARD	0x251	/* user address will be forwarded */
#define REPLY_START_DATA	0x354	/* okay to start sending message */
#define REPLY_DOWN		0x421	/* remote SMTP closing down */
#define REPLY_STORAGE_FULL	0x552	/* remote storage full */
/* pseudo-reply codes */
#define REPLY_PROTO_ERROR	0x498	/* protocol error on read */
#define REPLY_TIMEOUT		0x499	/* timeout on read, or EOF on read */
#define REPLY_NOT_ACCEPTABLE	0x501	/* request not acceptable */
#define REPLY_SEQUENCE_ERROR	0x503	/* Bad sequence of commands */

/* variables local to this file */
static struct str smtp_out;		/* region for outgoing commands */
static struct str smtp_in;		/* region from incoming responses */
int smtp_init_flag = FALSE;		/* TRUE if SMTP system initialized */
static JUMP_ENVBUF timeout_buf;		/* timeouts jump here */

/* functions local to this file */
#ifdef __STDC__
extern int smtp_startup(struct smtp *, struct error **, int);
extern int smtp_send(struct smtp *, struct addr *, struct addr **, struct addr **, struct addr **, struct error **);
extern void smtp_shutdown(struct smtp *);
static void do_smtp_shutdown(struct smtp *, int);
static int write_command_maybe_wait(struct smtp *, unsigned, char *, int, char **);
static int wait_write_command(struct smtp *, unsigned, char *, int, char **);
static int write_command_nowait(struct smtp *, char *, int);
static int flush_command_stream(struct smtp *, char **);
static int wait_read_response(struct smtp *, unsigned, char **);
static int read_response_internal(struct smtp *, unsigned, char **);
static void catch_timeout(void);
static struct error * no_remote(struct transport *, char *);
static struct error * try_again(struct transport *, char *);
static struct error * fatal_error(struct transport *, char *);
static struct error * remote_full(struct transport *, char *);
static struct error * remote_bad_address(struct transport *, char *);
static struct error * write_failed(struct transport *);
static struct error * read_failed(struct transport *); 
#else /* not __STDC__ */
extern int smtp_startup();
extern int smtp_send();
extern void smtp_shutdown();
static void do_smtp_shutdown();
static int write_command_maybe_wait();
static int wait_write_command();
static int write_command_nowait();
static int flush_command_stream();
static int wait_read_response();
static int read_response_internal();
static void catch_timeout();
static struct error * no_remote();
static struct error * try_again();
static struct error * fatal_error();
static struct error * remote_full();
static struct error * remote_bad_address();
static struct error * write_failed();
static struct error * read_failed(); 
#endif /* not __STDC__ */

/*
 * smtp_startup - initiate contact on an SMTP connection
 *
 * given input and output channels to a remote SMTP process, initiate
 * the session for future mail commands.  Once the startup has been
 * acomplished, smtp_send() can be used to send individual messages.
 *
 * If try_ehlo is non-zero and ESMTP support is configured, perform
 * ESMTP negotiaition using the EHLO greeting command.
 *
 * return:
 *	SMTP_SUCCEED on successful startup
 *	SMTP_FAIL if the connection should not be retried
 *	SMTP_AGAIN if the connection should be retried later
 *	SMTP_EHLO_FAIL if the receiver hung up in response
 *		to an EHLO command.  This can only occur when
 *		try_ehlo is set.
 *
 * For SMTP_FAIL and SMTP_AGAIN (but not SMTP_EHLO_FAIL), return a
 * filled-in error structure.
 */
int
smtp_startup(smtpb, error_p, try_ehlo)
    struct smtp *smtpb;			/* SMTP description block */
    struct error **error_p;		/* error description */
    int try_ehlo;			/* whether to use ESMTP (UNUSED!) */
{
    int reply;
    char *reply_text;
#ifdef HAVE_EHLO
    int tried_rset = 0;
#endif

    if (! smtp_init_flag) {
	STR_INIT(&smtp_in);
	STR_INIT(&smtp_out);
	smtp_init_flag = TRUE;
    }

    /*
     * wait for the sender to say he is ready.
     * Possible reponses:
     *	220 - service ready (continue conversation)
     *  421 - closing down  (try again later)
     */
    if (smtpb->in) {
	reply = wait_read_response(smtpb, smtpb->short_timeout, &reply_text);
    }

    if (reply != REPLY_READY) {
	/* didn't get an OK reponse, try again later */
	*error_p = no_remote(smtpb->tp, reply_text);
	return SMTP_AGAIN;
    }

#ifdef HAVE_EHLO
    if (try_ehlo) {
	/*
	 * say who we are.
	 * Possible responses:
	 *	250 - okay	    (continue conversation)
	 *	421 - closing down  (try again later)
	 *  5xx - fatal error   (try HELO)
	 */
	smtp_out.i = 0;
	(void) str_printf(&smtp_out, EHLO(primary_name));

	reply = wait_write_command(smtpb, smtpb->short_timeout,
				   smtp_out.p, smtp_out.i, &reply_text);

	if (reply == REPLY_TIMEOUT) {
	    /* Some gateways just terminate the conection when they
	       receive an EHLO.  We handle this case by returning a
	       special value here.  Also write a message to the log
	       file because it is interesting to know which mailers
	       expose this problem. */
	    write_log(LOG_SYS, "link broken by EHLO!");
	    return SMTP_EHLO_FAIL;
	}
	if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	    /* remote SMTP closed, try again later */
	    *error_p = try_again(smtpb->tp, reply_text);
	    return SMTP_AGAIN;
	}
	if (reply == REPLY_OK) {
	    char * cp = reply_text;
	    int on_greet_line = 1;

	    smtpb->smtp_flags = ESMTP_basic;
	    /* Parse the EHLO reply to find out
	       what the remote server supports */
	    while (*cp != 0) {
		int skip;
		int keywordlength;

		for (skip = 4; *cp != 0 && skip; --skip, ++cp) {
		    if (skip == 1
			? (*cp != ' ' && *cp != '-')
			: (! isdigit (*cp))) {
			goto malformed_ehlo_reply;
		    }
		}
		if (skip != 0) {
		    goto malformed_ehlo_reply;
		}
		if (on_greet_line) {
		    /*
		     * Ignore greeting on first line
		     */
		    on_greet_line = 0;
		    goto skip_rest_of_line;
		}
		for (keywordlength = 0;
		     *(cp+keywordlength) != 0
		     && !isspace (*(cp+keywordlength));
		     ++keywordlength)
		    ;
		if (strncmpic(cp, "SIZE", keywordlength) == 0) {
		    unsigned long max_size = 0;

		    cp += keywordlength;
		    while (*cp == ' ' || *cp == '\t')
			++cp;
		    if (!isdigit(*cp) && *cp != '\n')
			goto malformed_ehlo_reply;
		    while (isdigit(*cp)) {
			max_size *= 10;
			max_size += *cp - '0';
			++cp;
		    }
		    smtpb->smtp_flags |= ESMTP_size;
		    smtpb->max_size = max_size;
		} else if (strncmpic(cp, "8BITMIME", keywordlength) == 0) {
		    smtpb->smtp_flags |= ESMTP_8bitmime;
		} else if (strncmpic(cp, "PIPELINING", keywordlength) == 0) {
		    smtpb->smtp_flags |= ESMTP_pipelining;
		} else if (strncmpic(cp, "XVRB", keywordlength) == 0) {
		    smtpb->smtp_flags |= ESMTP_verbose;
		} else if (strncmpic(cp, "XONE", keywordlength) == 0) {
		    smtpb->smtp_flags |= ESMTP_one;
		} else if (strncmpic(cp, "XQUE", keywordlength) == 0) {
		    smtpb->smtp_flags |= ESMTP_queue;
		} else {
		}
	      skip_rest_of_line:
		while (*cp != 0 && *cp != '\n')
		    ++cp;
		if (*cp == '\n')
		    ++cp;
	    }
#ifndef NO_LOG_EHLO
	    write_log(LOG_SYS, "destination supports esmtp%s%s%s%s%s%s",
		      smtpb->smtp_flags & ESMTP_8bitmime ? " 8BITMIME" : "",
		      smtpb->smtp_flags & ESMTP_size ? " SIZE" : "",
		      smtpb->smtp_flags & ESMTP_pipelining ? " PIPELINING" : "",
		      smtpb->smtp_flags & ESMTP_verbose ? " XVRB" : "",
		      smtpb->smtp_flags & ESMTP_one ? " XONE" : "",
		      smtpb->smtp_flags & ESMTP_queue ? " XQUE" : "");
#endif /* not NO_LOG_EHLO */
	    if (smtpb->smtp_flags & ESMTP_verbose
		&& debug >= DBG_DRIVER_MID) {
		smtp_out.i = 0;
		(void) str_printf(&smtp_out, VERB);
		reply = wait_write_command(smtpb, smtpb->short_timeout,
					   smtp_out.p, smtp_out.i, &reply_text);
		if (REPLY_GROUP (reply) == NEGATIVE_TRY_AGAIN) {
		    /* remote SMTP closed, try again later */
		    *error_p = try_again(smtpb->tp, reply_text);
		    return SMTP_AGAIN;
		}
	    }
	    return SMTP_SUCCEED;
	  malformed_ehlo_reply:
	    /* This seems to be a reasonable way
	       to handle malformed EHLO replies: */
#ifndef NO_LOG_EHLO
	    write_log(LOG_SYS, "destination supports esmtp, but is buggy (%s)",
		      reply_text);
#endif /* not NO_LOG_EHLO */
	    return SMTP_SUCCEED;
	}
    }
#endif /* HAVE_EHLO */
    /*
     * say who we are.
     * Possible responses:
     *	250 - okay	    (continue conversation)
     *	421 - closing down  (try again later)
     *  5xx - fatal error   (return message to sender)
     */
#ifdef HAVE_EHLO
  try_helo:
#endif
    smtp_out.i = 0;
    (void) str_printf(&smtp_out, HELO(primary_name));

    reply = wait_write_command(smtpb, smtpb->short_timeout,
			       smtp_out.p, (int) smtp_out.i, &reply_text);

    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	/* remote SMTP closed, try again later */
	*error_p = try_again(smtpb->tp, reply_text);
	return SMTP_AGAIN;
    }
    if (reply == REPLY_SEQUENCE_ERROR) {
	/* ignore 503 Bad sequence of commands after EHLO/RSET/HELO */
#ifndef NO_LOG_EHLO
	write_log(LOG_SYS, "503 after EHLO/RSET/HELO (%s)",
		  reply_text);
#endif /* not NO_LOG_EHLO */
    } else if (reply != REPLY_OK) {
#ifdef HAVE_EHLO
	if (! tried_rset) {
	    /* The following */
	    /* fatal error, probably doesn't understand EHLO */
	    smtp_out.i = 0;
	    (void) str_printf(&smtp_out, "RSET");

	    reply = wait_write_command(smtpb, smtpb->short_timeout,
				       smtp_out.p, smtp_out.i, &reply_text);
	    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
		/* remote SMTP closed, try again later */
		*error_p = try_again(smtpb->tp, reply_text);
		return SMTP_AGAIN;
	    } else if (reply == REPLY_OK) {
	    } else {
		/* Some gateways don't accept any commands before they get
		   a HELO, not even RSET.  Fortunately it is usually safe
		   to ignore the error messages. */
#ifndef NO_LOG_EHLO
		write_log(LOG_SYS, "unexpected response to RSET (%s)",
			  reply_text);
#endif /* not NO_LOG_EHLO */
	    }
	    /* the RSET command has been accepted; try again with HELO */
	    tried_rset = 1;
	    goto try_helo;	/* GASP!!! */
	} else {
#endif /* HAVE_EHLO */
	/* fatal error, return message to sender */
	*error_p = fatal_error(smtpb->tp, reply_text);
	return SMTP_FAIL;
#ifdef HAVE_EHLO
	}
#endif
    }

    /* connection established */
    return SMTP_SUCCEED;
}


/*
 * smtp_send - mail a message to a remote SMTP process
 *
 * Using a virtual circuit connection of some kind, transmit
 * the current spooled message to the given set of recipient
 * addresses.  If the circuit has only a write channel, and no read
 * channel, then transmit batch SMTP.
 *
 * Return:
 *	SUCCEED	- more smtp messages can be sent.
 *	FAIL	- don't try to send any more messages.
 *
 * For FAIL, return a filled-in error structure.
 */
int
smtp_send(smtpb, addr, succeed, defer, fail, error_p)
    struct smtp *smtpb;			/* SMTP description block */
    struct addr *addr;			/* list of recipient addresses */
    struct addr **succeed;		/* successful addresses */
    struct addr **defer;		/* addresses to be retried */
    struct addr **fail;			/* failed addresses */
    struct error **error_p;		/* error description */
{
    register int reply;			/* reply code from SMTP commands */
    char *reply_text;			/* text of reply */
    int success;			/* success value from calls */
    struct addr *cur;			/* current address being sent */
    struct addr *next;			/* next addr to send */
    struct addr *okay;			/* partially successful addrs */
    int local_owner = FALSE;            /* TRUE if owner is local */
    struct transport *tp = smtpb->tp;

    if (! smtp_init_flag) {
	STR_INIT(&smtp_in);
	STR_INIT(&smtp_out);
	smtp_init_flag = TRUE;
    }

    /*
     * signal that we are sending a new message,
     * and give the sender address.
     *
     * Possible responses:
     *	250 - okay	    (continue conversation)
     *	4xx - temporary error (try again later)
     *  5xx - fatal error   (return message to sender)
     */

    /*
     * send MAIL command
     */
    smtp_out.i = 0;

    /* The envelope sender is <> for error messages (with <+> used for
    internally generated ones that are being pushed back into smail).
    Otherwise, use the message's original sender, unless there is
    an owner field set for the first address - this field is set for
    addresses generated by mailing list directors, and should be the
    same for all addresses in a given call to the transport.
    
    If the owner contains no "@", flag it as local so that the
    visible name gets added below. */

    local_owner = (!error_sender &&
      addr->owner && strchr(addr->owner, '@') == NULL);

    str_printf(&smtp_out, "%s%s%s%s%s",
               MAIL_BEGIN,
               (error_sender? ((islocal && tp->flags & LOCAL_XFORM)? "+": ""):
               (addr->owner)? addr->owner :
               get_sender_addr(tp)),
               local_owner? "@" : "",
               local_owner? visible_name : "",
	       MAIL_END);

    /*
     * send (a guess of) the size of the message to be transported.
     * Of course the guess could be a bit more educated, but usually
     * it doesn't matter if it is slightly incorrect.  We simply add
     * 2% in order to account for \n -> \r\n conversion.
     */
    if (smtpb->smtp_flags & ESMTP_size) {
	str_printf(&smtp_out, " SIZE=%lu",
		   (unsigned long) (msg_size * 1.02));
    }
#if 0
    /*
     * This is commented out because we are not supposed to send a
     * non-MIME message on 8BITMIME mode.  But sending a non-7bit
     * clean message in 7BIT mode isn't a good idea either.  Sigh.
     * Something more sophisticated is definitely needed here.
     */
    if (smtpb->smtp_flags & ESMTP_8bitmime) {
	str_printf(&smtp_out, " BODY 8BITMIME");
    }
#endif

    /* give all of the recipient addresses to the remote SMTP */
    okay = NULL;

    reply = write_command_maybe_wait(smtpb, smtpb->long_timeout,
				     smtp_out.p, (int) smtp_out.i, &reply_text);

    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	*error_p = try_again(tp, reply_text);
	insert_addr_list(addr, defer, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    } else if (REPLY_GROUP(reply) == NEGATIVE_FAILED) {
	*error_p = fatal_error(tp, reply_text);
	insert_addr_list(addr, fail, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    }

    /* give all of the recipient addresses to the remote SMTP */
    okay = NULL;
    for (cur = addr; cur; cur = next) {
	next = cur->succ;

	/*
	 * each recipient specified individually.
	 * Possible responses:
	 *  250, 251 - okay, or forwarded (continue conversation)
	 *  451, 452 - remote error    (try again later)
	 *  421 - connection closing   (try again later)
	 *  5xx - failure	       (return message to sender)
	 */
	smtp_out.i = 0;

	str_printf(&smtp_out, "%s%s%s", RCPT_BEGIN, cur->next_addr, RCPT_END);

	reply = write_command_maybe_wait(smtpb, smtpb->long_timeout,
					 smtp_out.p, (int) smtp_out.i, &reply_text);

	if (reply == REPLY_STORAGE_FULL) {
	    *error_p = remote_full(tp, reply_text);
	    insert_addr_list(cur, defer, *error_p);
	    next = NULL;
	} else if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	    *error_p = try_again(tp, reply_text);
	    insert_addr_list(cur, defer, *error_p);
	    insert_addr_list(okay, defer, *error_p);
	    do_smtp_shutdown(smtpb, reply);
	    return FAIL;
	} else if (REPLY_GROUP(reply) == NEGATIVE_FAILED) {
	    cur->error = remote_bad_address(tp, reply_text);
	    cur->succ = *fail;
	    *fail = cur;
	    return FAIL;
	} else {
	    /* successful thus far */
	    cur->succ = okay;
	    okay = cur;
	}
    }

    /*
     * say that we will next be sending the actual data
     * Possible responses:
     *	354 - go ahead with the message (continue conversation)
     *  421 - closing connection (try all recipients again later)
     *	4xx - remote error       (try all recipients again later)
     *  5xx - fatal error        (return message to sender)
     */
    reply = write_command_maybe_wait(smtpb, smtpb->long_timeout,
			       DATA, sizeof(DATA) - 1, &reply_text);

    if (smtpb->smtp_flags & ESMTP_pipelining) {

	if (reply != REPLY_OK) {
	    *error_p = try_again(tp, reply_text);
	    insert_addr_list(addr, defer, *error_p);
	    do_smtp_shutdown(smtpb, reply);
	    return FAIL;
	}

	if (flush_command_stream(smtpb, &reply_text) != REPLY_OK) {
	    return FAIL;
	}

	/* Now read all the responses.  First the MAIL FROM: reply */
	reply = wait_read_response(smtpb, smtpb->long_timeout,
				   &reply_text);
    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	    *error_p = try_again(tp, reply_text);
	    insert_addr_list(addr, defer, *error_p);
	    do_smtp_shutdown(smtpb, reply);
	    return FAIL;
	} else if (REPLY_GROUP(reply) == NEGATIVE_FAILED) {
	    *error_p = fatal_error(tp, reply_text);
	    insert_addr_list(addr, fail, *error_p);
	    do_smtp_shutdown(smtpb, reply);
	    return FAIL;
	}

	addr = okay;
	okay = NULL;

	for (cur = addr; cur; cur = next) {
	    next = cur->succ;

	    reply = wait_read_response(smtpb, smtpb->long_timeout,
				       &reply_text);

	    if (reply == REPLY_STORAGE_FULL) {
		*error_p = remote_full(tp, reply_text);
		insert_addr_list(cur, defer, *error_p);
		next = NULL;
	    } else if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
		*error_p = try_again(tp, reply_text);
		insert_addr_list(cur, defer, *error_p);
		insert_addr_list(okay, defer, *error_p);
		do_smtp_shutdown(smtpb, reply);
		return FAIL;
	    } else if (REPLY_GROUP(reply) == NEGATIVE_FAILED) {
		cur->error =  remote_bad_address(tp, reply_text);
		cur->succ = *fail;
		*fail = cur;
	    } else {
		/* successful thus far */
		cur->succ = okay;
		okay = cur;
	    }
	}
	reply = wait_read_response(smtpb, smtpb->long_timeout,
				   &reply_text);
    }

    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	*error_p = try_again(tp, reply_text);
	insert_addr_list(okay, defer, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    }

    if (reply != REPLY_START_DATA && reply != REPLY_OK) {
	/* fatal error, return message to sender */
	*error_p = fatal_error(tp, reply_text);
	insert_addr_list(okay, fail, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    }

    /*
     * send the message, using the hidden dot protocol.
     */
    tp->flags |= PUT_DOTS;
    success = write_message(smtpb->out, tp, addr);

    if (success == WRITE_FAIL) {
	*error_p = write_failed(tp);
	insert_addr_list(okay, defer, *error_p);

	/* assume connection has gone away */
	return FAIL;
    }
    if (success == READ_FAIL) {
	*error_p = read_failed(tp);
	insert_addr_list(okay, defer, *error_p);

	/* okay to advance to the next message */
	return SUCCEED;
    }

    /* finish by sending the final "." */
    reply = wait_write_command(smtpb, smtpb->long_timeout,
			       DATA_END, sizeof(DATA_END)-1, &reply_text);

    if (REPLY_GROUP(reply) == NEGATIVE_TRY_AGAIN) {
	*error_p = try_again(tp, reply_text);
	insert_addr_list(okay, defer, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    }

    if (reply != REPLY_OK) {
	/* fatal error, return message to sender */
	*error_p = fatal_error(tp, reply_text);
	insert_addr_list(okay, fail, *error_p);
	do_smtp_shutdown(smtpb, reply);
	return FAIL;
    }

    insert_addr_list(okay, succeed, (struct error *)NULL);
    return SUCCEED;
}


/*
 * smtp_shutdown - end an interactive SMTP conversation
 */
void
smtp_shutdown(smtpb)
    struct smtp *smtpb;			/* SMTP description block */
{
    char *reply_text;			/* where to store reply text */

    (void) wait_write_command(smtpb, smtpb->short_timeout,
			      QUIT, sizeof(QUIT)-1, &reply_text);
}

/*
 * do_smtp_shutdown - call shutdown if the connection wasn't dropped
 */
static void
do_smtp_shutdown(smtpb, reply)
    struct smtp *smtpb;			/* SMTP description block */
    int reply;				/* response code from command */
{
    switch (reply) {
    case REPLY_DOWN:
    case REPLY_PROTO_ERROR:
    case REPLY_TIMEOUT:
	break;

    default:
	smtp_shutdown(smtpb);
	break;
    }
}

static int
write_command_maybe_wait (smtpb, timeout, text, len, reply_text)
    struct smtp *smtpb;			/* SMTP description block */
    unsigned timeout;			/* read timeout */
    char *text;				/* text of command */
    register int len;			/* length of command */
    char **reply_text;			/* text of response from remote */
{
    if (smtpb->smtp_flags & ESMTP_pipelining) {
	return write_command_nowait (smtpb, text, len);
    } else {
	return wait_write_command (smtpb, timeout, text, len, reply_text);
    }
}

/*
 * wait_write_command - send a command, then wait for the response
 *
 * For batched SMTP, return a response code of REPLY_OK, unless there
 * was a write error.
 */
static int
wait_write_command(smtpb, timeout, text, len, reply_text)
    struct smtp *smtpb;			/* SMTP description block */
    unsigned timeout;			/* read timeout */
    char *text;				/* text of command */
    register int len;			/* length of command */
    char **reply_text;			/* text of response from remote */
{
    int reply;

    reply = write_command_nowait (smtpb, text, len);
    if (reply != REPLY_OK)
	return reply;

    reply = flush_command_stream (smtpb, reply_text);
    if (reply != REPLY_OK)
	return reply;

    /* wait for the response to come back */
    reply = wait_read_response(smtpb, timeout, reply_text);
    return reply;
}

/*
 * write_command_nowait - send a command
 */
static int
write_command_nowait(smtpb, text, len)
    struct smtp *smtpb;			/* SMTP description block */
    char *text;				/* text of command */
    register int len;			/* length of command */
{
    register FILE *f = smtpb->out;
    register char *cp;

#ifndef NODEBUG
    DEBUG(DBG_DRIVER_MID, "SMTP-send:  ");
    if (debug >= DBG_DRIVER_MID) {
	char *temp_cp = text;
	int temp_len = len;

	for (temp_cp = text; temp_len; temp_cp++, --temp_len) {
	    DEBUG1(DBG_DRIVER_MID, "%c", *temp_cp);
	}
	DEBUG(DBG_DRIVER_MID, "\n");
    }
#endif	/* NODEBUG */

    /* send out the command */
    for (cp = text; len; cp++, --len) {
	putc(*cp, f);
    }

    /* terminate the command line */
    for (cp = smtpb->nl; *cp; cp++) {
	putc(*cp, f);
    }
    return REPLY_OK;
}

static int
flush_command_stream(smtpb, reply_text)
    struct smtp *smtpb;			/* SMTP description block */
    char **reply_text;			/* text of response from remote */
{
    register FILE *f = smtpb->out;

    (void) fflush(f);
    if (ferror(f)) {
	*reply_text = "499 write error, remote probably down";
	DEBUG1(DBG_DRIVER_MID, "SMTP-reply: %s\n", *reply_text);
	return REPLY_TIMEOUT;
    }
    return REPLY_OK;
}

/*
 * wait_read_response - wait for a response from the remote SMTP process
 *
 * return the response code, and the response text.  Abort on timeouts
 * or end-of-file or protocol errors.
 */
static int
wait_read_response(smtpb, timeout, reply_text)
    struct smtp *smtpb;			/* SMTP description block */
    unsigned timeout;			/* read timeout */
    char **reply_text;			/* return text of response here */
{
    int result;

    /* If we're in batch mode, always return success. */
    if (! smtpb->in) {
	return REPLY_OK;
    }
    do
      {
	result = read_response_internal (smtpb, timeout, reply_text);
	DEBUG1(DBG_DRIVER_MID, "SMTP-reply: %s\n", *reply_text);
      }
    while (REPLY_GROUP (result) == POSITIVE_DEBUG);
    return result;
}

static int
read_response_internal(smtpb, timeout, reply_text)
    struct smtp *smtpb;			/* SMTP description block */
    unsigned timeout;			/* read timeout */
    char **reply_text;			/* return text of response here */
{
    register int lstart;		/* offset to start of an input line */
    int success;			/* success value from calls */
    VOLATILE unsigned save_alarm;	/* previous alarm value */
    VOLATILE JUMPSIG save_sig;		/* previous alarm signal catcher */
    register int c;

    /* reset the input buffer */
    smtp_in.i = 0;

    save_alarm = alarm(0);
    JUMP_SETSIG(SIGALRM, catch_timeout, &save_sig);

    /* if we timeout, say so */
    if (JUMP_SETJMP(timeout_buf)) {
	(void) alarm(0);
	JUMP_CLEARSIG(SIGALRM, &save_sig);
	if (save_alarm) {
	    (void) alarm(save_alarm);
	}
	*reply_text = "499 timeout on read from remote SMTP process";
	return REPLY_TIMEOUT;
    }

    /* don't let reads block forever */
    (void) alarm(timeout);

    /* loop until the response is completed */
    do {
	lstart = smtp_in.i;
	while ((c = getc(smtpb->in)) != EOF && c != '\n') {
	    STR_NEXT(&smtp_in, c);
	}
	if (smtp_in.i > 0 && smtp_in.p[smtp_in.i - 1] == '\r') {
	    --smtp_in.i;
	}
	STR_NEXT(&smtp_in, '\n');
    } while (c != EOF &&
	     isdigit(smtp_in.p[lstart]) &&
	     isdigit(smtp_in.p[lstart + 1]) &&
	     isdigit(smtp_in.p[lstart + 2]) &&
	     smtp_in.p[lstart + 3] == '-');

    /* replace last newline with a nul byte */
    if (smtp_in.i > 0)
	smtp_in.p[smtp_in.i - 1] = '\0';

    /* restore previous alarm catcher and setting */
    (void) alarm(0);
    JUMP_CLEARSIG(SIGALRM, &save_sig);
    if (save_alarm) {
	(void) alarm(save_alarm);
    }

    if (c == EOF) {
	*reply_text = "499 read error from remote SMTP process";
	return REPLY_TIMEOUT;
    }
    if (! isdigit(smtp_in.p[lstart]) ||
	! isdigit(smtp_in.p[lstart + 1]) ||
	! isdigit(smtp_in.p[lstart + 2]) ||
	! isspace(smtp_in.p[lstart+3]))
    {
	*reply_text = "498 protocol error in reply from remote SMTP process";
	return REPLY_PROTO_ERROR;
    }

    *reply_text = smtp_in.p;

    (void) sscanf(*reply_text, "%3x", &success);
    return success;
}

/* catch_timeout - longjmp after an alarm */
static void
catch_timeout()
{
    JUMP_LONGJMP(timeout_buf, 1);
}

static struct error *
no_remote(tp, reply_text)
    struct transport *tp;
    char *reply_text;
{
    char *error_text;

    /*
     * ERR_172 - no connection to remote SMTP server
     *
     * DESCRIPTION
     *	    No 220 reply was received from the remote SMTP server over
     *	    the virtual circuit, assume that the remote host was not
     *	    really reachable.
     *
     * ACTIONS
     *	    Try again later.
     *
     * RESOLUTION
     *	    Retries should eventually take care of the problem.
     */
    error_text =
	xprintf("transport %s: no connection to remote SMTP server: %s",
		tp->name, reply_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_172, error_text);
}

static struct error *
try_again(tp, reply_text)
    struct transport *tp;
    char *reply_text;
{
    char *error_text;

    /*
     * ERR_151 - temporary SMTP error
     *
     * DESCRIPTION
     *	    wait_write_command() received a temporary error response
     *      while conversing with the remote host.  These can be read
     *	    errors or read timeouts, or they can be negative responses
     *	    from the remote side.
     *
     * ACTIONS
     *      Defer the input addresses.
     *
     * RESOLUTION
     *      Retries should eventually take care of the problem.
     */
    error_text = xprintf("transport %s: %s", tp->name, reply_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_151, error_text);
}

static struct error *
fatal_error(tp, reply_text)
    struct transport *tp;
    char *reply_text;
{
    char *error_text;

    /*
     * ERR_152 - permanent SMTP error
     *
     * DESCRIPTION
     *	    wait_write_command() received a permanent error response
     *      while conversing with the remote host.  These are generally
     *	    permanent negative response codes from the remote side.
     *
     * ACTIONS
     *      Fail the input addresses, and return to the address owner
     *	    or the message originator.
     *
     * RESOLUTION
     *      The resolution depends upon the error message.  See RFC822
     *	    for details.
     */
    error_text = xprintf("transport %s: %s", tp->name, reply_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_152 | ERR_NSOWNER, error_text);
}

static struct error *
remote_full(tp, reply_text)
    struct transport *tp;
    char *reply_text;
{
    char *error_text;

    /*
     * ERR_153 - Remote host's storage is full
     *
     * DESCRIPTION
     *      The remote host returned status indicating that he
     *      cannot take more recipient addresses.
     *
     * ACTIONS
     *      Defer the remaining addresses in hopes that other
     *      storage is not affected.
     *
     * RESOLUTION
     *      Hopefully by sending a few addresses now and a few later
     *      the message will eventually be delivered to all
     *      recipients.
     */
    error_text = xprintf("transport %s: %s", tp->name, reply_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_153, error_text);
}

static struct error *
remote_bad_address(tp, reply_text)
    struct transport *tp;
    char *reply_text;
{
    char *error_text;

    /*
     * ERR_156 - remote host returned bad address status
     *
     * DESCRIPTION
     *      Status from the remote host indicates an error in the
     *      recipient address just sent to it.
     *
     * ACTIONS
     *      Return mail to the address owner or to the sender.
     *
     * RESOLUTION
     *      An alternate address should be attempted or the
     *      postmaster at the site that generated the error message
     *      should be consulted.
     */
    error_text = xprintf("transport %s: %s", tp->name, reply_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_156 | ERR_NSOWNER, error_text);
}

static struct error *
write_failed(tp)
    struct transport *tp;
{
    char *error_text;

    /*
     * ERR_154 - Error writing to remote host
     *
     * DESCRIPTION
     *      An error occured when transmitting the message text to the
     *      remote host.
     *
     * ACTIONS
     *      Defer all of the remaining input addresses.
     *
     * RESOLUTION
     *      Hopefully retries should take care of the problem.
     */
    error_text = xprintf("transport %s: Error writing to remote host",
			 tp->name);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_154, error_text);
}

static struct error *
read_failed(tp)
    struct transport *tp;
{
    char *error_text;

    /*
     * ERR_155 - Failed to read spooled message
     *
     * DESCRIPTION
     *      We failed to read the spooled message on our side while
     *      sending the message text to a remote host.
     *
     * ACTIONS
     *      Defer the message with a configuration error.  If we are
     *      unable to read the spool file, there is little we can do.
     *
     * RESOLUTION
     *      "This should never happen".
     */
    error_text = xprintf("transport %s: Error writing to remote host",
			 tp->name);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_155 | ERR_CONFERR, error_text);
}
