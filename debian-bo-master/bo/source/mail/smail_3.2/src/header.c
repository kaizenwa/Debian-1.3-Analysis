/*
#ident	"@(#)smail/src:RELEASE-3_2:header.c,v 1.22 1996/02/26 18:03:04 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * header.c:
 *	routines to process message headers.
 *
 *	external functions: process_header, read_header, write_header
 */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include "defs.h"
#include "smail.h"
#include "field.h"
#include "addr.h"
#include "transport.h"
#include "main.h"
#include "spool.h"
#include "log.h"
#include "exitcodes.h"
#include "dys.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

/* variables defined in this file */
struct list *header;			/* list of header fields */

/* variables local to this file */
static int use_resent;			/* TRUE if Resent- fields exist */
static int remove_bcc_fields;		/* TRUE to remove Bcc: header fields */
static char *to_fn;			/* name of To: field */
static char *from_fn;			/* name of From: field */
static char *sender_fn;			/* name of the Sender: field */
static char *cc_fn;			/* name of the Cc: field */
static char *bcc_fn;			/* name of the Bcc: field */
static char *message_id_fn;		/* name of the Message-Id: field */
static int found_to;			/* TRUE if To: field was found */
static int found_message_id;		/* TRUE if Message-Id: field found */
static int found_from;			/* TRUE if From: field was found */
static int found_cc;			/* TRUE if Cc: field was found */
static int found_bcc;			/* TRUE if Bcc: field was found */
static int found_date;			/* TRUE if Date: field was found */
static int found_sender;		/* TRUE if Sender: field was found */
static int count_received;		/* count of Received: fields found */
static struct addr *header_from;	/* list of From: addrs */
static struct list *remote_header;	/* simple remote-form header */
static struct list *strict_header;	/* strict RFC822 header */

/* functions local to this file */
static void check_for_resent();
static void scan_header();
static void new_field();
static char *build_to_field();
static char *build_date_field();
static char *build_message_id_field();
static char *build_from_field();
static char *build_received_field();
static char *build_return_path();
static void build_remote_header();
static void build_strict_header();


/*
 * process_header - first pass processing of a header.
 *
 * Note the existence of certain headers and, if input rq is non-nil
 * fill it with a list of recipient addresses taken from either the
 * Resent-To: Resent-Cc: and Resent-Bcc: fields or from the To: Cc:
 * and Bcc: fields.
 */
char *
process_header(rq)
    struct addr **rq;			/* put list of recipients here */
{
    char *error;

    /*
     * initialize the data to be searched for in the header
     */
    use_resent = FALSE;
    found_to = FALSE;			/* nothing found yet */
    found_message_id = FALSE;
    found_from = FALSE;
    found_date = FALSE;
    found_sender = FALSE;
    found_cc = FALSE;
    found_bcc = FALSE;
    count_received = 0;
    header_from = NULL;			/* initialize From: addr list */
    strict_header = NULL;		/* no strict RFC822 header yet */

    /*
     * Bcc: fields are removed only if we are processing the header
     * for addresses, otherwise we assume that the user agent would
     * have removed them if it didn't want them passed along
     */
    remove_bcc_fields = (rq != NULL);

    check_for_resent();			/* setup use_resent */
    error = NULL;
    scan_header(rq, &error);		/* scan for fields and recipients */

    if (rq && error) {
	/* got an error message while extracting recipients, done */
	return error;
    }
    error = NULL;

    /*
     * if no recipient fields given, create a To: field containing
     * the known recipients
     */
    if (! (found_to || found_cc || found_bcc) && rq == NULL) {
	new_field(build_to_field());
    }

    /*
     * if no From: field exists, and no Sender: field exists, create
     * a From: field.
     *
     * otherwise, if no Sender: field exists and From: is not verified
     * to be the actual sender, create a Sender: field.
     */
    if (! (found_from || found_sender)) {
	if (!islocal) {
	    from_fn = use_resent?
			"Apparently-Resent-From":
			"Apparently-From";
	}
	new_field(build_from_field(from_fn));
    } else if (islocal && found_from) {
	/*
	 * With no or multiple addresses in the From: field, always
	 * generate a Sender: field.
	 */
	if (! header_from || header_from->succ) {
	    new_field(build_from_field(sender_fn));
	} else if (!sender_is_trusted) {
	    /*
	     * if the sender is not trusted, allow the From: field only
	     * if it is in a form considered valid for the user.
	     */
	    char *work_addr = COPY_STRING(header_from->work_addr);
	    char *target;
	    char *remain;
	    int parseflags = header_from->parseflags;
	    int form = parse_address(work_addr, &target, &remain, &parseflags);
	    struct passwd *pw;
	    int dobuild = FALSE;

	    switch (form) {
	    case FAIL:
	    case PARSE_ERROR:
		dobuild = TRUE;
		break;

		/*
		 * catch all of the remote cases here
		 */

	    default:
		if (! islocalhost(target) && !EQIC(target, visible_name)) {
		    dobuild = TRUE;
		    break;
		}
		form = parse_address(remain, (char **) NULL, &error, &parseflags);
		if (form != LOCAL) {
		    dobuild = TRUE;
		    break;
		}
		/*FALLTHROUGH*/

	    case LOCAL:
		pw = getpwbyname(remain);	/* FIXME: WARNING: may need lowercase()? */
		if (pw == NULL || pw->pw_uid != real_uid)
		    dobuild = TRUE;
		break;
	    }
	    if (dobuild)
		new_field(build_from_field(sender_fn));
	    xfree(work_addr);
	}
    }

    /*
     * if no date field given, create one
     */
    if (!found_date) {
	new_field(build_date_field());
    }

    /*
     * if no message-id field given, create one
     */
    if (!found_message_id) {
	new_field(build_message_id_field());
    }

    /*
     * note count of received lines if count is not already known
     */
    if (hop_count < 0) {
	hop_count = count_received;
    }

    return error;			/* return any error messages */
}

/*
 * set use_resent if there exist any standard Resent- fields in
 * the header
 */
static void
check_for_resent()
{
    register struct list *q;		/* index to headers */

    /* on the first pass check for Resent- headers */
    for (q = header; q; q = q->succ) {

	/*
	 * some systems (at least pcc for microport/286) can't handle
	 * an expression with four HDREQ macros in one if, so split it
	 * up into two if's.
	 */

	if (HDREQ("resent-to", q->text) ||
	    HDREQ("resent-from", q->text))
	{
	    use_resent = TRUE;
	    break;
	}
	if (HDREQ("resent-cc", q->text) ||
	    HDREQ("resent-bcc", q->text))
	{
	    use_resent = TRUE;
	    break;
	}
    }

    /* set the address field names based on this information */
    if (use_resent) {
	to_fn = "Resent-To";
	from_fn = "Resent-From";
	sender_fn = "Resent-Sender";
	cc_fn = "Resent-Cc";
	bcc_fn = "Resent-Bcc";
	message_id_fn = "Resent-Message-Id";
    } else {
	to_fn = "To";
	from_fn = "From";
	sender_fn = "Sender";
	cc_fn = "Cc";
	bcc_fn = "Bcc";
	message_id_fn = "Message-Id";
    }
}

/*
 * scan_header -  pass through searching for required headers.
 *
 * If we are extracting recipient addresses from the header, do
 * so here.
 */
static void
scan_header(rq, error)
    struct addr **rq;			/* put recipients here, if non-NULL */
    char **error;			/* store error message here */
{
    register struct list *q;		/* temp for scanning header */

    for (q = header; q; q = q->succ) {
	if (hop_count < 0 && HDREQ("Received", q->text)) {
	    count_received++;		/* count Received: fields */
	} else if (HDREQ(to_fn, q->text)) {
	    found_to = TRUE;
	    if (islocal) {
		q->text = process_field(q->text,
					index(q->text,':')+1,
					(char *)NULL,
					(char *)NULL,
					rq,
					islocal?F_LOCAL:0,
					error);
	    } else if (rq) {
		(void) process_field((char *)NULL, index(q->text, ':') + 1,
				     (char *)NULL, (char *)NULL, rq,
				     0, error);
	    }
	} else if (HDREQ("apparently-to", q->text) ||
		   HDREQ("to", q->text) ||
		   HDREQ("resent-to", q->text))
	{
	    found_to = TRUE;
	} else if (HDREQ(message_id_fn, q->text)) {
	    found_message_id = TRUE;
	} else if (HDREQ(from_fn, q->text)) {
	    found_from = TRUE;
	    if (islocal) {
		q->text = process_field(q->text,
					index(q->text, ':')+1,
					(char *)NULL,
					(char *)NULL,
					&header_from,
					islocal?F_LOCAL:0,
					error);
	    } else {
		(void) process_field((char *)NULL, index(q->text, ':') + 1,
				     (char *)NULL, (char *)NULL,
				     &header_from, 0, error);
	    }
	} else if (HDREQ("date", q->text)) {
	    found_date = TRUE;
	} else if (HDREQ(sender_fn, q->text)) {
	    if (!sender_is_trusted) {
		struct list **pq;
		/*
		 * Non-trusted users can't supply their own Sender: fields,
		 * so delete this field.
		 */
		for (pq = &header; *pq && *pq != q; pq = &(*pq)->succ) ;
		if (*pq) {
		    *pq = q->succ;
		}
	    } else {
		found_sender = TRUE;
		if (islocal) {
		    q->text = process_field(q->text,
					    index(q->text, ':')+1,
					    (char *)NULL,
					    (char *)NULL,
					    &header_from,
					    islocal?F_LOCAL:0,
					    error);
		} else {
		    (void) process_field((char *)NULL, index(q->text, ':') + 1,
					 (char *)NULL, (char *)NULL,
					 &header_from, 0, error);
		}
	    }
	} else if (HDREQ(cc_fn, q->text)) {
	    found_cc = TRUE;
	    if (islocal) {
		q->text = process_field(q->text,
					index(q->text, ':') + 1,
					(char *)NULL,
					(char *)NULL,
					rq,
					islocal?F_LOCAL:0,
					error);
	    } else if (rq) {
		(void) process_field((char *)NULL, index(q->text, ':') + 1,
				     (char *)NULL, (char *)NULL, rq,
				     0, error);
	    }
	} else if (HDREQ(bcc_fn, q->text)) {
	    found_bcc = TRUE;
	    if (islocal) {
		q->text = process_field(q->text,
					index(q->text, ':') + 1,
					(char *)NULL,
					(char *)NULL,
					rq,
					islocal?F_LOCAL:0,
					error);
	    } else if (rq) {
		(void) process_field((char *)NULL, index(q->text, ':') + 1,
				     (char *)NULL, (char *)NULL, rq,
				     0, error);
	    }
	} else if (HDREQ("precedence", q->text)) {
	    /*
	     * a precedence field is specified, use it to set the message
	     * priority
	     */
	    int newgrd = parse_precedence(index(q->text, ':'));

	    if (newgrd) {
		msg_grade = newgrd;
	    }
	}
    }
}


/*
 * Create a To: header field
 */
static char *
build_to_field()
{
    register struct addr *rq;		/* recipients to put in To: field */
    struct str str;			/* region for string macros */
    int col;				/* current output line column */

    rq = recipients;

    /*
     * Sendmail inserts Apparently-To: fields because RFC822 people
     * complained about it being against the rules to add a To:
     * line.  We cheat.  If the letter is not originated locally,
     * use the Apparently-To: convention.  Otherwise do it the
     * right way.
     *
     * We'll see if anybody complains
     */
    if (!islocal) {
	if (use_resent) {
	    to_fn = "Apparently-Resent-To";
	} else {
	    to_fn = "Apparently-To";
	}
    }

    /*
     * now we actually form the To: line from the known recipient
     * addresses
     */
    STR_INIT(&str);			/* initialize dynamic string region */

    /* form = To: recipient(, recipient)* */

    rq = recipients;
    /* special case, insert the first address */
    if (rq) {
	if (rq->in_addr[0] == '@') {
	    /* route-addr, put it in angle brackets */
	    str_printf(&str, "%s: <%s>", to_fn, rq->in_addr);
	} else {
	    /* otherwise just use the address */
	    str_printf(&str, "%s: %s", to_fn, rq->in_addr);
	}
	rq = rq->succ;
    }
    col = str.i;
    while (rq) {
	int len = strlen(rq->in_addr);

	STR_NEXT(&str, ',');
	col++;

	/*
	 * put at least 27-chars on a line, try to keep down below 72
	 * NOTE:  we aren't counting TAB characters hidden in the
	 *	  addresses.
	 */
	if (col > 27 && col + len > 72) {
	    STR_NEXT(&str, '\n');
	    STR_NEXT(&str, '\t');
	    col = 8;
	} else {
	    STR_NEXT(&str, ' ');
	    col++;
	}

	if (rq->in_addr[0] == '@') {
	    str_printf(&str, "<%s>", rq->in_addr);
	    len += 2;			/* 2 chars for the angel brackets */
	} else {
	    STR_CAT(&str, rq->in_addr);
	}
	col += len;
	rq = rq->succ;
    }
    STR_NEXT(&str, '\n');
    STR_NEXT(&str, '\0');

    STR_DONE(&str);
    return str.p;			/* return finished To: field */
}

/*
 * build a Date: field
 */
static char *
build_date_field()
{
    register char *s;			/* returned header field */

    if (date_field &&
	(s = expand_string(date_field, (struct addr *)NULL,
			   (char *)NULL, (char *)NULL)))
    {
	return COPY_STRING(s);
    }
    return "";
}

/*
 * Form a Message-Id: header field.
 */
static char *
build_message_id_field()
{
    register char *s;			/* returned header field */

    if (message_id_field &&
	(s = expand_string(message_id_field, (struct addr *)NULL,
			   (char *)NULL, (char *)NULL)))
    {
	if (use_resent) {
	    return xprintf("Resent-%s", s);
	} else {
	    return COPY_STRING(s);
	}
    }
    return "";
}

/*
 * form a From: or Sender: header field
 */
static char *
build_from_field(fn)
    char *fn;				/* field name */
{
    char *s;

    if (from_field)
	s = expand_string(from_field, (struct addr *)0, (char *)0, sender);
    if (s == NULL || *s == '\0')
	return "";
    if (! HDREQ("from", s))
	return COPY_STRING(s);
    return xprintf("%s%s", fn, s + 4);
}

/*
 * link in a brand new field with the given text
 */
static void
new_field(field)
    char *field;			/* new field to add */
{
    register struct list *f;		/* header field entry */

    f = (struct list *)xmalloc(sizeof(struct list)); /* create it */
    f->text = field;
    f->succ = header;			/* link it in */
    header = f;
}

/*
 * parse_precedence - parse a Precedence: field and return it's grade value
 *
 * use tokenize to parse for an atom (the first atom is used, the rest are
 * ignored).  Then lookup the atom in the grades string and return the
 * associated grade, if found, 0 otherwise.
 */
int
parse_precedence(field)
    char *field;
{
    int len;				/* length of token */
    struct token *tok;			/* list of tokens from tokenize */
    register char *p = grades;		/* temp for scanning grades */
    char *error;			/* error message from tokenize */

    DEBUG(DBG_HEADER_HI, "parse_precedence called\n");
    error = tokenize(field, &tok, FALSE, TRUE);
    if (error) {
	/* tokenize didn't like the field for some reason */
	write_log(LOG_MLOG, "Error in precedence field: %s", error);
	return '\0';			/* no precedence value */
    }
    if (tok->form != T_TEXT) {
	return '\0';			/* not a text (atom) token */
    }
    len = strlen(tok->text);
    DEBUG1(DBG_HEADER_MID, "precedence is %s\n", tok->text);
    while (p) {
	if (strncmpic(tok->text, p, len) == 0 && p[len] == ':') {
	    /* we have a match, grab the character */
	    return p[len+1];
	}
	/* skip to end of precedence string */
	p = index(p, ':');
	if (p) {
	    /* skip past grade character */
	    p = index(p, ':');
	    if (p) {
		p++;			/* now at next precedence string */
	    }
	}
    }

    DEBUG(DBG_QUEUE_MID, "unknown precedence\n");
    return '\0';			/* unknown precedence */
}


/*
 * read_header - initialize header structures, reading from spool file
 *
 * Read a header on the spool file.  The header data structure is
 * initialized to contain an entry for each field of the header.
 *
 * returns SUCCEED or FAIL, the actual failure is logged.
 */
int
read_header()
{
    struct str field;			/* input field */
    register int c;			/* current input char */
    int last_c = EOF;			/* last input char */
    struct list **link_addr;		/* forward link for next field */
    off_t line_mark;			/* marks beginning of a line */

    link_addr = &header;

    /*
     * loop until done reading the header
     */
    for (;;) {
	STR_INIT(&field);
	/*
	 * field name is some non-space chars followed by
	 * optional white space up to a ':'
	 */
	if (last_c == ':') {
	    /* we had a read ahead character past the last newline */
	    c = EOF;			/* don't allow null fieldnames */
	} else {
	    if (last_c != EOF) {
		STR_NEXT(&field, last_c);
		line_mark = tell_spool() - 1;
		last_c = EOF;
	    } else {
		/*
		 * save the beginning of the line, so we can go back to
		 * here if this is not a header
		 */
		line_mark = tell_spool();
	    }
	    while ((c = GETSPOOL()) != EOF && c != READ_FAIL &&
		   !isspace(c) && c != ':')
	    {
		STR_NEXT(&field, c);
		last_c = c;
	    }
	    while (c != EOF && c != READ_FAIL &&
		   isspace(c))
	    {
		    STR_NEXT(&field, c);
		    c = GETSPOOL();
	    }
	    if (c == READ_FAIL) {
		write_log(LOG_SYS, "read error in spool file <%s/%s>",
			  spool_dir, spool_fn);
		return FAIL;
	    }
	}

	/* not a valid field name, must be past the header */
	if (c != ':') {
	    break;
	}

	/*
	 * looks like we are in a header field, so scan to the end of it
	 */
	STR_NEXT(&field, c);
	last_c = c;

	/*
	 * header field terminated by new line which does not start
	 * with a SPACE or TAB char, or by EOF.
	 */
	while ((c = GETSPOOL()) != EOF && c != READ_FAIL &&
	       !(last_c == '\n' && c != ' ' && c != '\t'))
	{
	    STR_NEXT(&field, c);
	    last_c = c;
	}
	if (c == READ_FAIL) {
	    write_log(LOG_SYS, "read error in spool file <%s/%s>",
		      spool_dir, spool_fn);
	}

	last_c = c;			/* preserve this for the next field */
	STR_NEXT(&field, '\0');
	STR_DONE(&field);
	*link_addr = (struct list *)xmalloc(sizeof(struct list));
	(*link_addr)->text = field.p;
	link_addr = &(*link_addr)->succ;
	if (c == EOF || c == '\n') {
	    line_mark = -1;		/* don't seek back to previous line */
	    field.p = NULL;
	    field.i = 0;
	    break;
	}
    }

    *link_addr = NULL;			/* terminate header queue */

    /*
     * all done reading the field, cleanup, seek to beginning of
     * message body (after last valid field line)
     */
    if (line_mark >= 0 && seek_spool((off_t) line_mark) == FAIL) {
	write_log(LOG_SYS, "seek error on spool file <%s/%s>",
		  spool_dir, spool_fn);
	return FAIL;
    }
    if (field.p) {
	STR_DONE(&field);		/* free the field that wasn't */
	STR_FREE(&field);
    }
    return SUCCEED;
}

/*
 * write_header - write out the header to a stdio FILE
 *
 * inputs:
 *	file	- file to write to.
 *	addr	- involved address, first in linked list is used
 *
 * outputs:
 *	SUCCEED or FAIL
 */
int
write_header(f, addr)
    FILE *f;				/* file to send header to */
    struct addr *addr;			/* transport */
{
    long flags = addr->transport->flags; /* transport flags */
    struct list *q, *q2;		/* temp for processing header */
    int crlf = (flags & PUT_CRLF)?1:0;	/* put \r\n at end of each line? */
    char *s;

    /* output transport-specific insertion headers */
    for (q2 = addr->transport->hdrinsert; q2; q2 = q2->succ) {
	if (write_field(expand_string(q2->text, addr,
			(char *)NULL, (char *)NULL), f, crlf) == FAIL)
	    return FAIL;
    }

    /* output a Return-Path: header if required */
    if (flags & PUT_RETURNPATH) {
	if (write_field(build_return_path(addr), f, crlf) == FAIL) {
	    return FAIL;
	}
    }

    /* output a Received: header if required */
    if (flags & PUT_RECEIVED) {
	if (write_field(build_received_field(addr), f, crlf) == FAIL) {
	    return FAIL;
	}
    }

    if (use_resent) {
	/*
	 * the situation gets complex if resent- headers are involved,
	 * so ignore the issue by not changing anything.
	 */
	q = header;
    } else if (flags & STRICT_TPORT) {
	/* output a strict header, building one if none exists */
	if (strict_header == NULL) {
	    build_strict_header();
	}
	q = strict_header;
    } else if (flags & LOCAL_XFORM) {
	/* output header as is */
	q = header;
    } else {
	/* output a normal remote header */
	if (remote_header == NULL) {
	    build_remote_header();
	}
	q = remote_header;
    }

    /*
     * output the appropriate header, as selected above
     */
    for (; q; q = q->succ) {
	/*
	 * remote bcc fields if addresses were extracted from the
	 * header.  Otherwise, we expect that the UA would have
	 * removed them if they were not wanted.
	 *
	 * Now replaces bcc: headers with a blank bcc: header
	 * to placate pedantic mailers!
	 * Bcc headers are *always* stripped from locally originating
	 * mail.
	 */
	if ((remove_bcc_fields || islocal) && HDREQ(bcc_fn, q->text)) {
	    char bcc_head[sizeof("resent-bcc: \r\n\0")];
	    strcpy(bcc_head, bcc_fn);
	    strcat(bcc_head, ":\n");
	    if (write_field(bcc_head, f, crlf) == FAIL) {
		return FAIL;
	    }
	    continue;
	}
	/* don't output redundant Return-Path headers */
	if ((flags & PUT_RETURNPATH) && HDREQ("Return-Path", q->text)) {
	    continue;
	}
	/* output transport-specific insertion headers */
	for (q2 = addr->transport->hdrremove; q2; q2 = q2->succ) {
	    s = expand_string(q2->text, addr, (char *)NULL, (char *)NULL);
	    if (s && HDREQ(s, q->text))
		break;
	}
	if (q2 == NULL && write_field(q->text, f, crlf) == FAIL) {
	    return FAIL;
	}
    }

    /* output transport-specific insertion headers */
    for (q2 = addr->transport->hdrappend; q2; q2 = q2->succ) {
	if (write_field(expand_string(q2->text, addr,
			(char *)NULL, (char *)NULL), f, crlf) == FAIL)
	    return FAIL;
    }

    DEBUG(DBG_HEADER_LO, "\n");

    return SUCCEED;
}

/*
 * write_field - output a header field, possibly changing \n to \r\n.
 *
 * return SUCCEED or FAIL.
 */
int
write_field(field, f, crlf)
    register char *field;		/* field to be written */
    FILE *f;				/* file to send to */
    int crlf;				/* TRUE to write \r\n for \n */
{
    register int c;

    if (field == NULL || field[0] == '\0') {
	/* empty field, don't write out anything */
	return SUCCEED;
    }
    DEBUG1(DBG_HEADER_LO, "%s", field);
    while ((c = *field++)) {
	/* write out c, possibly with a '\r' first */
	if ((c == '\n' && crlf && putc('\r', f) == EOF) || putc(c, f) == EOF) {
	    return FAIL;
	}
    }
    /* if field did not end with \n, supply it */
    if (field[-2] != '\n') {
	DEBUG(DBG_HEADER_LO, "\n");
	if (crlf) {
	    if (putc('\r', f) == EOF) {
		return FAIL;
	    }
	}
	if (putc('\n', f) == EOF) {
	    return FAIL;
	}
    }
    return SUCCEED;
}

/*
 * form a Received: header field
 */
static char *
build_received_field(addr)
    struct addr *addr;
{
    register char *s;			/* returned header field */

    if (received_field &&
	(s = expand_string(received_field, addr, (char *)NULL, (char *)NULL)))
    {
	return s;
    }
    return "";
}

/*
 * build a Return-Path: header field.
 */
static char *
build_return_path(addr)
    struct addr *addr;
{
    register char *s;			/* returned header field */

    if (return_path_field &&
	(s = expand_string(return_path_field, addr,
			   (char *)NULL, (char *)NULL)))
    {
	return s;
    }
    return "";
}


/*
 * build_remote_header - build a simple remote-form header
 *
 * For From: and Sender: fields prepend localhost! on addresses which
 * are pure !-routes, if mail is originated local then convert user to
 * user@localdomain and qualify known domain abbreviations.
 */
static void
build_remote_header()
{
    struct list **rhq = &remote_header; /* where to put next field */
    struct list *hq;			/* temp for scanning header */
    char *error;			/* (ignored) error message */

    /*
     * copy the standard header to the remote header, transforming
     * sender and recipient fields as appropriate.
     */
    for (hq = header; hq; hq = hq->succ) {
	*rhq = (struct list *)xmalloc(sizeof(**rhq));
	(*rhq)->succ = NULL;
	if (HDREQ(from_fn, hq->text) || HDREQ(sender_fn, hq->text)) {
	    /*
	     * field containing a sender address:
	     *    prepend uucp_host! in !-routes
	     *    append @visiblename after local-form addresses
	     *    qualify known domain abbreviations, if local
	     */
	    (*rhq)->text = process_field(hq->text,
					 index(hq->text, ':') + 1,
					 visible_name,
					 uucp_name,
					 (struct addr **)NULL,
					 (islocal? F_LOCAL: 0),
					 &error);
	} else if (islocal && (HDREQ(to_fn, hq->text) ||
			       HDREQ(cc_fn, hq->text) ||
			       HDREQ(bcc_fn, hq->text)))
	{
	    /*
	     * locally generated field containing recipient addresses:
	     *    append @visible_name after local-form addresses
	     *    qualify known domain abbreviations
	     */
	    (*rhq)->text = process_field(hq->text,
					 index(hq->text, ':') + 1,
					 visible_name,
					 (char *)NULL,
					 (struct addr **)NULL,
					 F_LOCAL,
					 &error);
	} else {
	    (*rhq)->text = hq->text;
	}

	rhq = &(*rhq)->succ;
    }
}

/*
 * build_strict_header - build a somewhat RFC822 conformant header
 *
 * For addresses which are in local form, in !-route form or in
 * remainder%target form, append @visiblename and prepend a !-route
 * path back to the sender.  This essentially qualifies the address
 * within the context of the current host, which will then be
 * routed through on replies to handle the !-routes.
 */
static void
build_strict_header()
{
    char *error;			/* error messages */
    struct list **shq = &strict_header; /* where to put next field */
    struct list *hq;			/* temp for scanning header */
    char *sender_path;			/* path to sender's machine */

    /*
     * build a path back to the sender's machine, which is a !-route
     * back to the sender with the last component (!username) removed
     * this will be passed to process_field as the uucp_name parameter
     * for recipient fields.
     */
    sender_path = build_uucp_route(sender, &error, 0);
    if (sender_path == NULL) {
	/*
	 * errors will generate a NULL sender path, so nothing will
	 * be prepended to addresses in recipient fields
	 */
	write_log(LOG_MLOG, "error building path to sender <%s>: %s",
		  sender, error);
    } else {
	char *p = sender_path + strlen(sender_path);

	/*
	 * back up over the trailing "!token" and terminate the string
	 */
	p = back_address_token(sender_path, p);
	if (p == NULL) {
	    write_log(LOG_MLOG,
		 "error building path to sender <%s>: unexpected end of token",
		      sender);
	    sender_path = NULL;
	} else if (p == sender_path) {
	    sender_path = NULL;
	} else {
	    p[-1] = '\0';		/* terminate the path */
	}
    }

    /*
     * copy the standard header to a new strict header, performing
     * the necessary transformations on sender and recipient fields
     */
    for (hq = header; hq; hq = hq->succ) {
	*shq = (struct list *)xmalloc(sizeof(**shq));
	(*shq)->succ = NULL;
	if (HDREQ(from_fn, hq->text) || HDREQ(sender_fn, hq->text)) {
	    /*
	     * field containing a sender address:
	     *    append @visiblename after local-form addresses
	     *    qualify known domain abbreviations, if local
	     */
	    (*shq)->text = process_field(hq->text,
					 index(hq->text, ':') + 1,
					 visible_name,
					 (char *)NULL,
					 (struct addr **)NULL,
					 (islocal? F_LOCAL: 0)|F_STRICT,
					 &error);
	} else if (HDREQ(to_fn, hq->text) || HDREQ(cc_fn, hq->text) ||
		   HDREQ(bcc_fn, hq->text))
	{
	    /*
	     * field containing a recipient address:
	     *    append @visiblename after !-route, local-form or u%d form
	     *    prepend sender_path! before the same set of addresses
	     *    qualify known domain abbreviations, if local
	     */
	    (*shq)->text = process_field(hq->text,
					 index(hq->text, ':') + 1,
					 visible_name,
					 EQIC(sender_path,visible_name) ? NULL : sender_path,
					 (struct addr **)NULL,
					 (islocal? F_LOCAL: 0)|F_STRICT,
					 &error);
	} else {
	    (*shq)->text = hq->text;
	}

	shq = &(*shq)->succ;
    }
}
