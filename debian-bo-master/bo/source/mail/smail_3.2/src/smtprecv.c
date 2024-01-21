/*
#ident	"@(#)smail/src:RELEASE-3_2:smtprecv.c,v 1.27 1996/03/05 19:04:42 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smtprecv.c:
 *	Receive mail using the SMTP protocol.
 */
#define NEED_SOCKETS			/* Required for IP address lookup */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>
#include "defs.h"
#include "main.h"
#include "smail.h"
#include "addr.h"
#include "dys.h"
#include "log.h"
#include "hash.h"
#include "alloc.h"
#include "iobpeek.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "exitcodes.h"
#endif

#ifdef HAVE_RFC1413
/* Include the declarations for ident protocol lookups */
#include <ident.h>
#endif /* HAVE_RFC1413 */

/* library functions */
extern long time();

/* Declare the ident variables - even if HAVE_RFC1413 is not defined, so that configs
 * can be kept consistant
 */
char * ident_sender = NULL;		/* The calculated identity of the sender */
char * ident_method = NULL;		/* Method used to get identity */

/* types local to this file */
enum e_smtp_commands {
    HELO_CMD,				/* HELO domain */
    EHLO_CMD,				/* EHLO domain */
    MAIL_CMD,				/* MAIL FROM:<sender> */
    RCPT_CMD,				/* RCPT TO:<recipient> */
    DATA_CMD,				/* DATA */
    VRFY_CMD,				/* VRFY */
    EXPN_CMD,				/* EXPN */
    QUIT_CMD,				/* QUIT */
    RSET_CMD,				/* RSET */
    NOOP_CMD,				/* NOOP */
    DEBUG_CMD,				/* DEBUG [level] */
    HELP_CMD,				/* HELP */
    EOF_CMD,				/* end of file encountered */
    OTHER_CMD				/* unknown command */
};

/* functions local to this file */
#ifdef __STDC__
static void reset_state(void);
static enum e_smtp_commands read_smtp_command(FILE *, FILE *);
static void expand_addr(char *, FILE *);
static int verify_addr(char *, FILE *, int);
static void smtp_input_signals(void);
static void smtp_processing_signals(void);
static void set_term_signal(int);
static void smtp_receive_timeout_sig(int);
static void smtp_sig_unlink(int);
#ifdef HAVE_DF_SPOOL
static long compute_max_message_size_from_df_spool (void);
#endif
#else /* not __STDC__ */
static void reset_state();
static enum e_smtp_commands read_smtp_command();
static void expand_addr();
static int verify_addr();
static void smtp_input_signals();
static void smtp_processing_signals();
static void set_term_signal();
static void smtp_receive_timeout_sig();
static void smtp_sig_unlink();
#ifdef HAVE_DF_SPOOL
static long compute_max_message_size_from_df_spool ();
#endif
#endif /* not __STDC__ */

/* variables local to this file */
static char *data;			/* interesting data within input */

static int term_signal;
static int smtp_remove_on_timeout;
static FILE *out_file;
static char *help_msg[] = {
    "250-The following SMTP commands are recognized:",
    "250-",
    "250-   HELO hostname                   - startup and give your hostname",
    "250-   EHLO hostname                   - startup with extension info",
    "250-   MAIL FROM:<sender-address>      - start transaction from sender",
    "250-   RCPT TO:<recipient-address>     - name recipient for message",
#ifndef NO_VERIFY
    "250-   VRFY <address>                  - verify deliverability of address",
    "250-   EXPN <address>                  - expand mailing list address",
#endif
    "250-   DATA                            - start text of mail message",
    "250-   RSET                            - reset state, drop transaction",
    "250-   NOOP                            - do nothing",
    "250-   DEBUG [level]                   - set debugging level, default 1",
    "250-   HELP                            - produce this help message",
    "250-   QUIT                            - close SMTP connection",
    "250-",
    "250-The normal sequence of events in sending a message is to state the",
    "250-sender address with a MAIL FROM command, give the recipients with",
    "250-as many RCPT TO commands as are required (one address per command)",
    "250-and then to specify the mail message text after the DATA command.",
    "250 Multiple messages may be specified.  End the last one with a QUIT."
};

typedef enum {
    BF_unspecified,
    BF_7bit,
    BF_8bitmime
}
BodyFormat;

#define BS_UNSPECIFIED -1L
typedef long BodySize;


/*
 * receive_smtp - receive mail over SMTP.
 *
 * Take SMTP commands on the `in' file.  Send reply messages
 * to the `out' file.  If `out' is NULL, then don't send reply
 * messages (i.e., read batch SMTP commands).
 *
 * return an array of spool files which were created in this SMTP
 * conversation.
 *
 * The last spooled message is left open as an efficiency move, so the
 * caller must arrange to close it or process it to completion.  As
 * well, it is the callers responsibility to close the input and
 * output channels.
 */
char **
receive_smtp(in, out)
    FILE *in;				/* stream of SMTP commands */
    FILE *out;				/* channel for responses */
{
    char *error;			/* temp to hold error messages */
    struct addr *cur;
    static char **files = NULL;
    static int file_cnt = 7;		/* initially put 7 parts in array */
    int file_i = 0;			/* index starts at the beginning */
    /* save important state to restore after initialize_state() */
    enum er_proc save_error_proc = error_processing;
    int save_do_aliasing = do_aliasing;
    int save_dont_deliver = dont_deliver;
    FILE *save_errfile = errfile;
    int save_debug = debug;
    int temp, i, c;
    char *rest;
    int ehlo_p = 0;
    long accepted_msg_size = max_message_size;
#ifdef HAVE_BSD_NETWORKING
    struct sockaddr_in from;
    int from_len = sizeof(from);
#endif /* HAVE_BSD_NETWORKING */

    /* initialize state */
    initialize_state();

    /* restore important state */
    error_processing = save_error_proc;
    do_aliasing = save_do_aliasing;
    dont_deliver = save_dont_deliver;

    term_signal = FALSE;
    out_file = out;
    smtp_processing_signals();
    if (out) {
	(void) signal(SIGALRM, smtp_receive_timeout_sig);
    }

    /* allocate an initial chunk of spool filename slots */
    if (files == NULL) {
	files = (char **)xmalloc((file_cnt + 1) * sizeof(*files));
    }

    /* output the startup banner line */
    if (out) {
	char *s;

	s = expand_string(smtp_banner, (struct addr *)NULL,
			  (char *)NULL, (char *)NULL);
	while (*s) {
	    fprintf(out, "220%c", index(s, '\n') == NULL? ' ': '-');
	    while ((c = *s)) {
		s++;
		if (c == '\n') {
		    break;
		}
		putc(c, out);
	    }
	    putc('\r', out);
	    putc('\n', out);
	}
	fflush(out);
    }

#ifdef HAVE_BSD_NETWORKING
    if ((getpeername(fileno(in), (struct sockaddr *) &from, &from_len) == 0) &&
	(from.sin_family == AF_INET)) {
	/* make inet address available in ASCII representation */
	sender_host_addr = COPY_STRING(inet_ntoa(from.sin_addr));
    }
#endif HAVE_BSD_NETWORKING

    /* Set the protocol used - this used to be in the HELO code, but that gives
     * problems if the sender never says HELO 
     */
    if (sender_proto == NULL) {
        sender_proto = (out? "smtp": "bsmtp");
    }

#ifdef HAVE_RFC1413
    /* 
     * This code does a Ident/RFC1413 lookup on the connecting user (if possible)
     * Its here because this may cause a delay and it seems better to have the
     * delay once the connection is established rather than at the start
     *
     * A call to ident_lookup() could be used to obtain additional info
     * available from an RFC1413 ident call, but I don't know what we would
     * do with the info, so don't bother to obtain it!
     */
    if (rfc1413_query_timeout > 0) {	/* switch off RFC1413 by setting timeout <= 0 */
	static char ident_str[32];
	char * ident_ptr;

	if (ident_ptr = ident_id(fileno(in), (int) rfc1413_query_timeout)) {
	    strncpy(ident_str, ident_ptr, sizeof(ident_str));
	    ident_str[sizeof(ident_str) - 1] = '\0';
	    ident_sender = ident_str;
	    free(ident_ptr);
	    ident_method = "rfc1413";
	} else {
	    ident_sender = NULL;
	    ident_method = NULL;
	}
    }
#endif /* HAVE_RFC1413 */

    while (! term_signal || out == NULL) {
	if (out) {
	    alarm(smtp_receive_command_timeout);
	}
	switch (read_smtp_command(in, out)) {
	case EHLO_CMD:
	    ehlo_p = 1;
	    sender_proto = (out? "esmtp": "ebsmtp");
	    /* FALLTHRU */
	case HELO_CMD:
	    strip_rfc822_comments(data);
	    if (out && data[0] == '\0') {
		fprintf(out, "501 %s requires domain name as operand\r\n",
			ehlo_p ? "EHLO" : "HELO");
		fflush(out);
		break;
	    }
	    if (sender_host == NULL && data[0] != '\0') {
		sender_host = COPY_STRING(data);
	    }
	    if (!ehlo_p) {
		if (out) {
		    fprintf(out, "250 %s Hello %s\r\n", primary_name, data);
		    fflush(out);
		}
	    } else {
		if (out) {
		    fprintf(out, "250-%s Hello %s, here's what we support:\r\n",
			    primary_name, data);
#ifndef NO_VERIFY
		    fprintf(out, "250-EXPN\r\n");
#endif
#ifdef HAVE_DF_SPOOL
		    accepted_msg_size = compute_max_message_size_from_df_spool ();
		    if (accepted_msg_size == -1 
			|| (max_message_size && max_message_size < accepted_msg_size))
			accepted_msg_size = max_message_size;
#endif
		    if (accepted_msg_size && accepted_msg_size != -1) {
			fprintf(out, "250-SIZE %d\r\n", accepted_msg_size);
		    } else {
			fprintf(out, "250-SIZE\r\n");
		    }
#ifdef HAVE_ESMTP_8BITMIME
		    fprintf(out, "250-8BITMIME\r\n");
#endif
#ifdef HAVE_ESMTP_PIPELINING
		    fprintf(out, "250-PIPELINING\r\n");
#endif
		    fprintf(out, "250 HELP\r\n");
		    fflush(out);
		}
	    }
	    reset_state();
	    break;

	case MAIL_CMD:
	    strip_rfc822_comments(data);
	    if (out && data[0] == '\0') {
		fprintf(out, "501 MAIL FROM requires address as operand\r\n");
		fflush(out);
		break;
	    }
	    if (sender) {
		if (out) {
		    fprintf(out, "503 Sender already specified\r\n");
		    fflush(out);
		}
		break;
	    }
	    sender = preparse_address_1(data, &error, &rest);
  	    if (out) {
		if (!sender) {
  		    fprintf(out, "501 <%s> ... %s\r\n", data, error);
		    break;
  		}
  	    }
	    if (sender && sender[0] == '\0') {
		/* special error sender form <> given */
		sender = COPY_STRING("<>");
	    }
	    if (sender && EQ(sender, "+")) {
		/* special smail-internal <+> was given */
		sender = COPY_STRING("<+>");
	    }
	    {
		char * format = 0;
		int format_length = 0;
#ifdef HAVE_ESMTP_8BITMIME
		BodyFormat body_format = BF_unspecified;
#endif
		BodySize body_size = BS_UNSPECIFIED;

		while (rest && *rest != '\0') {
		    /* maybe we have an extended MAIL command */
		    while (*rest != '\0' && isspace (*rest)) {
			++rest;
		    }
		    {
			int restlen = 0;
			while (*(rest+restlen) != 0
			       && !isspace (*(rest+restlen))
			       && *(rest+restlen) != '=') {
			    ++restlen;
			}
			if (strncmpic(rest, "SIZE", restlen) == 0) {
			    rest += restlen;
			    if (*rest != '=') {
				if (out) {
				    *(rest+restlen) = '\0';
				    fprintf(out, "555 missing SIZE parameter\r\n",
					    rest);
				}
				goto fail_mail_cmd;
			    }
			    ++rest;
			    restlen = 0;
			    body_size = 0;
			    while (*(rest+restlen) != 0
				   && isdigit (*(rest+restlen))
				   && (!accepted_msg_size || body_size <= accepted_msg_size)) {
				body_size = 10*body_size+*(rest+restlen)-'0';
				++restlen;
			    }
			    if (accepted_msg_size && body_size > accepted_msg_size) {
				if (out) {
				    fprintf(out, "552 message too large\r\n");
				}
				goto fail_mail_cmd;
			    } else if (*(rest+restlen) != 0
				       && !isspace (*(rest+restlen))) {
				if (out) {
				    while (*(rest+restlen) != 0
					   && !isspace (*(rest+restlen))) {
					++restlen;
				    }
				    *(rest+restlen) = '\0';
				    fprintf(out, "555 malformed SIZE clause %s\r\n",
					    rest);
				}
				goto fail_mail_cmd;
			    }
			} else
#ifdef HAVE_ESMTP_8BITMIME
			if (strncmpic(rest, "BODY", restlen) == 0) {
			    rest += restlen;
			    if (*rest != '=') {
				if (out) {
				    *(rest+restlen) = '\0';
				    fprintf(out, "555 missing BODY parameter\r\n",
					    rest);
				}
				goto fail_mail_cmd;
			    }
			    ++rest;
			    restlen = 0;
			    while (*(rest+restlen) != 0
				   && !isspace (*(rest+restlen))) {
				++restlen;
			    }
			    if (strncmpic(rest, "7BIT", restlen) == 0) {
				body_format = BF_7bit;
			    } else if (strncmpic(rest, "8BITMIME", restlen) == 0) {
				body_format = BF_8bitmime;
			    } else {
				if (out) {
				    *(rest+restlen) = '\0';
				    fprintf(out, "555 unknown BODY type %s\r\n",
					    rest);
				}
				goto fail_mail_cmd;
			    }
			} else 
#endif
			{
			    if (out) {
				*(rest+restlen) = '\0';
				fprintf(out, "555 Unknown MAIL TO: option %s\r\n", rest);
			    }
			    goto fail_mail_cmd;
			}
			rest += restlen;
		    }
		}
		if (out) {
		    if (format && *format != '\0') {
			*(format+format_length) = '\0';
			fprintf(out, "250 <%s> ... Sender Okay, using format %s\r\n",
				sender, format);
		    } else {
			fprintf(out, "250 <%s> ... Sender Okay\r\n",
				sender);
		    }
		}
	    }
	    break;
	  fail_mail_cmd:
	    if (sender) {
		xfree(sender);
		sender = NULL;
	    }
	    break;

	case RCPT_CMD:
	    strip_rfc822_comments(data);
	    if (out && data[0] == '\0') {
		fprintf(out, "501 RCPT TO requires address as operand\r\n");
		break;
	    }
	    cur = alloc_addr();
	    if (out) {
#ifdef VERIFY_RCPTS
		if (! verify_addr (data, out, 1)) {
		    break;
		}
#else /* not VERIFY_RCPTS */
		if ((cur->work_addr = preparse_address(data, &error))) {
		    fprintf(out, "250 <%s> ... Recipient Okay\r\n",
			    cur->work_addr);
		} else {
		    fprintf(out, "501 <%s> ... %s\r\n", data, error);
		    break;
		}
#endif
	    }
	    /*
	     * surround in angle brackets, if the addr begins with `-'.
	     * This will avoid ambiguities in the data dumped to the spool
	     * file.
	     */
	    if (data[0] == '-') {
		cur->in_addr = xprintf("<%s>", data);
	    } else {
		cur->in_addr = COPY_STRING(data);
	    }
	    cur->succ = recipients;
	    recipients = cur;
	    break;

	case DATA_CMD:
	    if (sender == NULL) {
		if (out) {
		    fprintf(out, "503 Need MAIL command\r\n");
		    fflush(out);
		} else {
		    /* sink the message for the sake of further batched cmds */
		    if (spool_fn) {
			close_spool();
		    }
		    swallow_smtp(in);
		}
		break;
	    }
	    if (recipients == NULL) {
		if (out) {
		    fprintf(out, "503 Need RCPT (recpient)\r\n");
		    fflush(out);
		} else {
		    /* sink the message for the sake of further batched cmds */
		    if (spool_fn) {
			close_spool();
		    }
		    swallow_smtp(in);
		}
		break;
	    }
	    if (out) {
		fprintf(out,
		     "354 Enter mail, end with \".\" on a line by itself\r\n");
		fflush(out);
		alarm(0);
	    }

	    /*
	     * if we had the previous spool file opened, close it
	     * before creating a new one
	     */
	    if (spool_fn) {
		close_spool();
	    }
	    if (out) {
		/*
		 * if we are not interactive and cannot send back failure
		 * messages, always try to accept the complete message.
		 */
		smtp_input_signals();
		alarm(smtp_receive_message_timeout);
	    }
	    smtp_remove_on_timeout = 1;
	    if (queue_message(in, SMTP_DOTS, recipients, &error) == FAIL) {
		exitvalue = EX_IOERR;
		log_spool_errors();
		if (out) {
		    fprintf(out, "451 Failed to queue message: %s: %s\r\n",
			    error, strerror(errno));
		    fflush(out);
		} else if (errfile) {
		    fprintf(errfile, "Failed to queue message: %s: %s\r\n",
			    error, strerror(errno));
		}
		unlink_spool();
		reset_state();
		smtp_remove_on_timeout = 0;
		break;
	    }
	    smtp_processing_signals();
	    if (sender == NULL) {
		unlink_spool();
		reset_state();
		smtp_remove_on_timeout = 0;
		break;
	    }
	    if (read_message() == NULL) {
		log_spool_errors();
		unlink_spool();
		if (out) {
		    fprintf(out, "451 error in spooled message\r\n");
		    fflush(out);
		}
		reset_state();
		smtp_remove_on_timeout = 0;
		break;
	    }
	    alarm(0);
	    smtp_remove_on_timeout = 0;

	    check_grade();
	    log_incoming();
	    log_spool_errors();
	    if (out) {
		fprintf(out, "250 Mail accepted\r\n");
		fflush(out);
	    }
	    /* always allow an extra element to store the ending NULL */
	    if (file_i >= file_cnt) {
		/* we need to grow the array of spool file names */
		file_cnt += 8;
		files = (char **)xrealloc((char *)files,
					  (file_cnt + 1) * sizeof(*files));
	    }
	    files[file_i++] = xprintf("%s/input/%s", spool_dir, spool_fn);
	    reset_state();
	    break;

	case VRFY_CMD:
	    if (out) {
#ifdef NO_VERIFY
		fprintf(out, "502 Command not implemented\r\n");
#else
		if (smtp_info) {
		    strip_rfc822_comments(data);
		    verify_addr(data, out, 0);
		} else {
		    fprintf(out, "502 Command disabled\r\n");
		}
#endif
		fflush(out);
	    }
	    break;

	case EXPN_CMD:
	    if (out) {
#ifdef NO_VERIFY
		fprintf(out, "502 Command not implemented\r\n");
#else
		if (smtp_info) {
		    strip_rfc822_comments(data);
		    expand_addr(data, out);
		} else {
		    fprintf(out, "502 Command disabled\r\n");
		}
#endif
		fflush(out);
	    }
	    break;

	case QUIT_CMD:
	    if (out) {
		fprintf(out, "221 %s closing connection\r\n",
			       primary_name);
		fflush(out);
	    }
	    reset_state();
	    files[file_i++] = NULL;
	    errfile = save_errfile;
	    debug = save_debug;
	    return files;

	case RSET_CMD:
	    reset_state();
	    if (out) {
		fprintf(out, "250 Reset state\r\n");
	    }
	    break;

	case NOOP_CMD:
	    if (out) {
		fprintf(out, "250 Okay\r\n");
		fflush(out);
	    }
	    break;

	case DEBUG_CMD:
	    if (out) {
#ifndef NODEBUG
		if (smtp_debug) {
		    strip_rfc822_comments(data);
		    if (data[0]) {
			char *errbuf;

			temp = c_atol(data, &errbuf);
			if (errbuf) {
			    fprintf(out, "500 bad number: %s\r\n", errbuf);
			    fflush(out);
			    break;
			}
		    } else {
			temp = 1;
		    }
		    if (temp == 0) {
			fprintf(out, "250 Debugging disabled\r\n");
		    } else {
			DEBUG(DBG_QUEUE_LO,
			      "debugging output grabbed by SMTP\r\n");
			fprintf(out, "250 Debugging level: %d\r\n", temp);
		    }
		    fflush(out);
		    debug = temp;
		    errfile = out;
		    break;
		}
#endif	/* NODEBUG */
		fprintf(out, "500 I hear you knocking, but you can't come in\r\n");
		fflush(out);
	    }
	    break;

	case HELP_CMD:
	    if (out) {
		for (i = 0; i < TABLESIZE(help_msg); i++) {
		    fprintf(out, "%s\r\n", help_msg[i]);
		}
		fflush(out);
	    }
	    break;

	case EOF_CMD:
	    if (out) {
		fprintf(out, "421 %s Lost input channel\r\n", primary_name);
		fflush(out);
	    }
	    files[file_i++] = NULL;
	    errfile = save_errfile;
	    debug = save_debug;
	    return files;

	default:
	    if (out) {
		fprintf(out, "500 Command unrecognized\r\n");
		fflush(out);
	    }
	    break;
	}
    }

    /*
     * we appear to have received a SIGTERM, so shutdown and tell the
     * remote host.
     */
    fprintf(out, "421 %s Service not available, closing channel\r\n",
	    primary_name);
    fflush(out);

    files[file_i] = NULL;
    errfile = save_errfile;
    debug = save_debug;
    return files;
}

static void
reset_state()
{
    struct addr *cur;
    struct addr *next;

    for (cur = recipients; cur; cur = next) {
	next = cur->succ;
	xfree(cur->in_addr);
	if (cur->work_addr) {
	    /* work_addr is defined only for interactive smtp */
	    xfree(cur->work_addr);
	}
	xfree((char *)cur);
    }
    recipients = NULL;

    if (sender) {
	xfree(sender);
	sender = NULL;
    }
}

static enum e_smtp_commands
read_smtp_command(f, out)
    register FILE *f;			/* SMTP command stream */
    register FILE *out;			/* output, may have to be flushed */
{
    static struct str input;		/* buffer storing recent command */
    static int inited = FALSE;		/* TRUE if input initialized */
    register int c;			/* input char */
    int flushed_p = !out;

    static struct smtp_cmd_list {
	char *name;
	int len;
	enum e_smtp_commands cmd;
    } smtp_cmd_list[] = {
	"HELO", 	sizeof("HELO")-1,	HELO_CMD,
	"EHLO", 	sizeof("EHLO")-1,	EHLO_CMD,
	"MAIL FROM:",	sizeof("MAIL FROM:")-1,	MAIL_CMD,
	"RCPT TO:",	sizeof("RCPT TO:")-1,	RCPT_CMD,
	"DATA",		sizeof("DATA")-1,	DATA_CMD,
	"VRFY",		sizeof("VRFY")-1,	VRFY_CMD,
	"EXPN",		sizeof("EXPN")-1,	EXPN_CMD,
	"QUIT",		sizeof("QUIT")-1,	QUIT_CMD,
	"RSET",		sizeof("RSET")-1,	RSET_CMD,
	"NOOP",		sizeof("NOOP")-1,	NOOP_CMD,
	"DEBUG",	sizeof("DEBUG")-1,	DEBUG_CMD,
	"HELP",		sizeof("HELP")-1,	HELP_CMD,
    };
    struct smtp_cmd_list *cp;

    if (! inited) {
	STR_INIT(&input);
	inited = TRUE;
    } else {
	input.i = 0;
    }
    for (;;) {
	if (!flushed_p && IOB_MAYBE_EMPTY_P (f)) {
	    ++flushed_p;
	    fflush (out);
	}
	c = getc(f);
	if (c == EOF || c == '\n') {
	    break;
	}
	STR_NEXT(&input, c);
    }
    if (input.p[input.i - 1] == '\r') {
	input.p[input.i - 1] = '\0';
    } else {
	STR_NEXT(&input, '\0');
    }

    /* return end of file pseudo command if end of file encountered */
    if (c == EOF) {
	return EOF_CMD;
    }

    for (cp = smtp_cmd_list; cp < ENDTABLE(smtp_cmd_list); cp++) {
	if (strncmpic(cp->name, input.p, cp->len) == 0) {
	    for (data = input.p + cp->len; isspace(*data); data++) ;
	    return cp->cmd;
	}
    }

    return OTHER_CMD;
}

#ifndef NO_VERIFY
/*
 * expand_addr - expand an address
 *
 * display the list of items that an address expands to.
 */
static void
expand_addr(in_addr, out)
    char *in_addr;			/* input address string */
    FILE *out;				/* write expansion here */
{
    struct addr *addr = alloc_addr();	/* get an addr structure */
    struct addr *okay = NULL;		/* list of deliverable addrs */
    struct addr *defer = NULL;		/* list of currently unknown addrs */
    struct addr *fail = NULL;		/* list of undeliverable addrs */
    char *error;			/* hold error message */

    addr->in_addr = in_addr;		/* setup the input addr structure */
    /* build the mungeable addr string */
    addr->work_addr = preparse_address(in_addr, &error);
    if (addr->work_addr == NULL) {
	fprintf(out, "501 %s ... %s\r\n", in_addr, error);
	fflush(out);
	return;
    }

    /* cache directors and routers on the assumption we will need them again */
    if (! queue_only) {
	if (! cached_directors) {
	    cache_directors();
	}
	if (! cached_routers) {
	    cache_routers();
	}
    }

    hit_table = new_hash_table(hit_table_len,
			       (struct block *) NULL,
			       HASH_DEFAULT);
    resolve_addr_list(addr, &okay, &defer, &fail, TRUE);
    if (okay) {
	register struct addr *cur;	/* current addr to display */

	/* display the complete list of resolved addresses */
	for (cur = okay; cur->succ; cur = cur->succ) {
	    fprintf(out, "250-%s\r\n", cur->in_addr);
	    fflush(out);
	}
	/* the last one should not begin with 250- */
	fprintf(out, "250 %s\r\n", cur->in_addr);
    } else {
	/* just say we couldn't find it */
	fprintf(out, "550 %s ... not matched\r\n", in_addr);
    }
}

/*
 * verify_addr - verify an address
 *
 * redisplay the input address if it is a valid address.
 */
static int
verify_addr(in_addr, out, rcpt_p)
    char *in_addr;			/* input address string */
    FILE *out;				/* write expansion here */
    int rcpt_p;				/* non-zero if called from RCPT */
{
    struct addr *addr = alloc_addr();	/* get an addr structure */
    struct addr *okay = NULL;		/* verified address */
    struct addr *defer = NULL;		/* temporarily unverifiable addr */
    struct addr *fail = NULL;		/* unverified addr */
    char *error;			/* hold error message */
    int error_code;			/* reply code for negative result */

    error_code = rcpt_p ? 501 : 550;
    addr->in_addr = in_addr;		/* setup the input addr structure */
    /* build the mungeable addr string */
    addr->work_addr = preparse_address(in_addr, &error);
    if (addr->work_addr == NULL) {
	fprintf(out, "%d %s ... %s\r\n", error_code, in_addr, error);
	return 0;
    }

    /* cache directors and routers on the assumption we will need them again */
    if (! queue_only) {
	if (! cached_directors) {
	    cache_directors();
	}
	if (! cached_routers) {
	    cache_routers();
	}
    }
    verify_addr_list(addr, &okay, &defer, &fail);

    if (okay) {
	fprintf(out, "250 %s\r\n", in_addr);
 	return 1;
    } else if (defer) {
 	fprintf(out, "%d %s ... cannot verify: %s\r\n", 
 		rcpt_p ? 250 : 550, in_addr, defer->error->message);
 	return 1;
    } else if (fail) {
 	fprintf(out, "%d %s ... not matched: %s\r\n",
 		error_code, in_addr, fail->error->message);
	write_log(LOG_SYS, "Mail recepient not matched %s: %s", in_addr, fail->error->message);
 	return 0;
    } else {
  	/* hmmm, it should have been in one of the lists */
 	fprintf(out, "%d %s ... not matched\r\n",
 		error_code, in_addr);
	write_log(LOG_SYS, "Mail recepient not matched %s", in_addr);
 	return 0;
    }
}
#endif	/* NO_VERIFY */


/*
 * smtp_input_signals - setup signals for reading in message with smtp
 *
 * Basically, unlink the message except in the case of SIGTERM, which
 * will cause sig_term and queue_only to be set.
 */
static void
smtp_input_signals()
{
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGHUP, smtp_sig_unlink);
    }
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGINT, smtp_sig_unlink);
    }
    (void) signal(SIGTERM, set_term_signal);
}

/*
 * smtp_processing_signals - setup signals for getting smtp commands
 *
 * basically, everything interesting should cause a call to
 * set_term_signal.
 */
static void
smtp_processing_signals()
{
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGHUP, set_term_signal);
    }
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
	(void) signal(SIGINT, set_term_signal);
    }
    (void) signal(SIGTERM, set_term_signal);
}

/*
 * set_term_signal - set term_signal and queue_only
 *
 * This is used by signals to abort SMTP command processing and to
 * prevent attempting delivery.
 *
 * NOTE:  This doesn't work correctly for systems that lack restartable
 *	  system calls, as read will return EINTR for such systems,
 *	  rather than continuing.  This situation could be improved,
 *	  though it doesn't really seem worth the rather large amount
 *	  of bother required.
 */
static void
set_term_signal(sig)
    int sig;
{
    (void) signal(sig, set_term_signal);
    term_signal = TRUE;
    queue_only = TRUE;
}

/*
 * smtp_receive_timeout_sig - timeout SMTP
 */

/* ARGSUSED */
static void
smtp_receive_timeout_sig(sig)
    int sig;
{
    fprintf(out_file, "421 %s SMTP command timeout, closing channel\r\n",
	    primary_name);
    write_log(LOG_SYS, "SMTP connection timeout%s%s.",
	      sender_host? " while talking with ": "",
	      sender_host? sender_host: "",
	      sender_host_addr? " [": "",
	      sender_host_addr? sender_host_addr: "",
	      sender_host_addr? "]": "");
    if (smtp_remove_on_timeout) {
	unlink_spool();
    }
    exit(EX_TEMPFAIL);
}

/*
 * smtp_sig_unlink - unlink spool file and fast exit.
 *
 * This is useful for handling signals to abort reading a message in
 * with SMTP.
 */
static void
smtp_sig_unlink(sig)
    int sig;
{
    (void) signal(sig, SIG_IGN);
    if (out_file) {
	fprintf(out_file, "421 %s Service not available, closing channel\r\n",
		primary_name);
    }
    unlink_spool();
    exit(EX_OSFILE);
}

#ifdef HAVE_DF_SPOOL
static long
compute_max_message_size_from_df_spool (void)
{
    long free_bytes = spool_max_free_space ();
    const long reserved = 2*1024*1024;
    const long min_max_message_size = 20*1024;

    if (free_bytes == -1)
	return free_bytes;
    return free_bytes < 2*reserved ? -1 : free_bytes - reserved;
}
#endif
