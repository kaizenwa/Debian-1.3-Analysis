/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "decode.h"
#include "file.h"
#include "p_I.h"
#include "p_attach.h"
#include "p_popup.h"
#include "p_setup.h"
#include "parse.h"
#include "resource.h"
#include "server.h"
#include "util.h"
#include "xutil.h"

static int	n_post_contexts = 0;

int outstanding_posts(void)
{
    return n_post_contexts > 0;
}

void free_post_context(PostContext *context)
{
    n_post_contexts--;
    if (context->file_name) {
	unlink(context->file_name);
	XtFree(context->file_name);
	context->file_name = NULL;
    }
    free((char *)context->art);  /* :-( */
    context->art = NULL;
    XtFree(context->charset);
    context->charset = NULL;
    if (context->widgets) {
	destroy_post_widgets(context->widgets);
	context->widgets = NULL;
    }
    if (context->attachments) {
	int	i, n = context->n_attachments;

	for (i = 0 ; i < n ; i++)
	    free_attachment(context->attachments[i]);
	XtFree((char *)context->attachments);
	context->attachments = NULL;
	context->n_attachments = 0;
    }
    XtFree((char *)context);
}

PostContext *create_post_context(int flags, char *file_name)
{
    PostContext	*context;
    char	*charset;

    n_post_contexts++;
    charset = res_default_charset();
    if (!charset)
	charset = "iso-8859-1";
    if (!(flags & POST))
	flags |= ORIG_MAIL;

    context = (PostContext *)XtMalloc(sizeof *context);
    context->file_name     = XtNewString(file_name);
    context->art	   = NULL;
    context->charset	   = XtNewString(charset);
    context->line          = 0;
    context->n_attachments = 0;
    context->flags         = flags;
    context->busy          = True;
    context->has_8bit      = False;
    context->widgets       = NULL;
    context->attachments   = NULL;
    context->q_str         = res_quote_string();
    context->qq_str        = res_quote_quote_string();

    return context;
}

void append_signature(FILE *fp)
{
    char	*sig_file_name = res_signature_file();
    FILE	*sig;
    int		c;

    if (!sig_file_name)
	return;
    sig = fopen_expand(sig_file_name, "r", True);
    if (!sig)
	return;

    fputs("\n-- \n", fp);
    while ((c = getc(sig)) != EOF)
	putc(c, fp);
    fclose(sig);
}

static int print_references_header(FILE *fp, char *msgid, char *refs)
{
    char	*c;
    int		lines    = 1;
    int		may_fold = False;
    int		col;

    col = fprintf(fp, "References:");
    if (refs)
	while ((refs = strchr(refs, '<')) && (c = strchr(++refs, '>'))) {
	    int	len = c - refs;

	    *c = '\0';
	    if (may_fold && col + len > 72) {
		col = fprintf(fp, "\n   ") - 1;
		lines++;
	    }
	    col += fprintf(fp, " <%s>", refs);
	    may_fold = True;
	    *c = '>';
	}

    if (may_fold && col + strlen(msgid) > 72) {
	fprintf(fp, "\n   ");
	lines++;
    }
    fprintf(fp, " %s\n", msgid);

    return lines;
}

static char *skip_line_count(char *c)
{
    while (IS_SPACE(*c))
	c++;
    while (IS_DIGIT(*c))
	c++;
    while (IS_SPACE(*c))
	c++;

    return c;
}

static int extract_initials(ARTICLE *art, char *buffer, int maxlen, int cap)
{
    char	*c;
    int		pos = 0;

    maxlen -= 4;
    if (art && art->from && (c = art->tree_data.label)) {
	if (res_show_number_lines())
	    c = skip_line_count(c);

	while (*c != '\0') {
	    while (*c != '\0' && !IS_ALPHA(*c))
		c++;
	    if (*c == '\0' || pos > maxlen)
		break;
	    buffer[pos++] = (cap && IS_LOWER(*c)) ? UPPER(*c) : *c;
	    while (*c != '\0' && IS_ALPHA(*c))
		c++;
	}
    }

    buffer[pos] = '\0';

    return pos;
}

static void expand_quote_string(ARTICLE *art, char *quote_string,
				char *buffer, int len)
{
    int		pos = 0;

    *buffer = '\0';
    len -= 4;

    if (!quote_string)
	return;

    while (*quote_string != '\0' && pos < len)
	if (*quote_string != '%')
	    buffer[pos++] = *quote_string++;
	else {
	    int		cap = False;

	    switch (*++quote_string) {
	    case 'I':
		cap = True;
		/* fall through */
	    case 'i':
		pos += extract_initials(art, buffer + pos, len, cap);
		break;
	    case '%':
		buffer[pos++] = '%';
		break;
	    default:
		continue;
	    }
	    quote_string++;
	}

    buffer[pos] = '\0';
}

static int tm_helper(int *inited, struct tm *tm, time_t date, FILE *fp)
{
    int		first = !*inited;
    struct tm	*tmp;

    *inited = True;
    if (date == PARSEDATE_ERROR) {
	if (first)
	    fprintf(fp, "<date error>");
	return False;
    }
    tmp = gmtime(&date);
    if (!tmp)
	return False;
    *tm = *tmp;
    return True;
}

static void print_with_percent(FILE *fp, char *fmt, ARTICLE *art)
{
    struct tm	tm;
    int		tm_inited = False;
    char	*c;

    if (!fp || !fmt || fmt[0] == '\0' || !art)
	return;

#define FILL_TM  tm_helper(&tm_inited, &tm, art->date, fp)

    while (*fmt != '\0')
	if (*fmt != '%')
	    fputc(*fmt++, fp);
	else {
	    unsigned char	a = *++fmt;

	    if (isupper(a))
		a = tolower(a);

	    switch (a) {
	    case '\0':
		continue;
	    case '%': /* literal % */
		fputc('%', fp);
		break;
	    case 'd': /* date: dd mmm */
		if (FILL_TM && (unsigned)tm.tm_mon < 12)
		    fprintf(fp, "%2.2d %3.3s", tm.tm_mday,
			    month_names + 3 * tm.tm_mon);
		break;
	    case 'f': /* from line */
		fputs(art->from, fp);
		break;
	    case 'i': /* initials */
		if (art) {
		    char	buffer[32];

		    extract_initials(art, buffer, sizeof buffer, *fmt == 'I');
		    fputs(buffer, fp);
		}
		break;
	    case 'm': /* message-id */
		fprintf(fp, "<%s>", art->msgid);
		break;
	    case 'n': /* newsgroup */
		if ((global.mode == NewsModeGroup ||
		     global.mode == NewsModeThread) &&
		    global.curr_group)
		    fputs(global.curr_group->name, fp);
		break;
	    case 'r': /* real name */
		c = art->tree_data.label;
		if (c && res_show_number_lines())
		    c = skip_line_count(c);
		if (c)
		    fputs(c, fp);
		break;
	    case 's': /* subject */
		fprintf(fp, "%s%s", PARENT(art) ? "Re: " : "",
			art->subject->subject);
		break;
	    case 't': /* time */
		if (FILL_TM)
		    fprintf(fp, "%02d:%02d:%02d",
			    tm.tm_hour, tm.tm_min, tm.tm_sec);
		break;
	    case 'w': /* week day */
		if (FILL_TM && 0 <= tm.tm_wday && tm.tm_wday < 7) {
		    static char	*wday_names[] = {
			"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
		    };
		    fputs(wday_names[tm.tm_wday], fp);
		}
		break;
	    case 'y': /* year */
		if (FILL_TM)
		    fprintf(fp, "%4d", 1900 + tm.tm_year);
		break;
	    default: /* bogus */
		putc('%', fp);
		putc(a, fp);
		break;
	    }
	    fmt++;
	}
#undef FILL_TM
}

int insert_extra_headers(FILE *fp, ARTICLE *art)
{
    char	*reply_to         = res_reply_to();
    char	*organization     = res_organization();
    char	*extra_headers    = res_extra_headers();
    char	*followup_headers = res_followup_headers();
    char	*distr            = res_distribution();
    int		result = 0;

    if (global.generate_path) {
	fprintf(fp, "Path: %s!%s\n", global.domain_name, global.mail_name);
	result++;
    }

    fprintf(fp, "%s\n", "X-Newsreader: knews " KNEWS_VERSION);
    result++;

    if (distr) {
	fprintf(fp, "Distribution: %s\n", distr);
	result++;
    }

    if (reply_to) {
	fprintf(fp, "Reply-To: %s\n", reply_to);
	result++;
    }

    if (organization) {
	fprintf(fp, "Organization: %s\n", organization);
	result++;
    }

    if (extra_headers && extra_headers[0] != '\0') {
	char	*c;

	fputs(extra_headers, fp);
	for (c = strchr(extra_headers, '\n') ; c ; c = strchr(c + 1, '\n'))
	    result++;
	if (extra_headers[strlen(extra_headers) - 1] != '\n') {
	    result++;
	    fputc('\n', fp);
	}
    }

    if (art && followup_headers && followup_headers[0] != '\0') {
	char	*c;

	print_with_percent(fp, followup_headers, art);
	for (c = strchr(followup_headers,'\n') ; c ; c = strchr(c + 1, '\n'))
	    result++;
	if (followup_headers[strlen(followup_headers) - 1] != '\n') {
	    result++;
	    fputc('\n', fp);
	}
    }

    return result;
}

typedef struct {
    char	*subject;
    char	*newsgroups;
    char	*followup_to;
    char	*from;
    char	*reply_to;
    char	*message_id;
    char	*references;
    char	*content_type;
    char	*content_enc;
} PostHeaders;

static void free_headers(PostHeaders *h)
{
    XtFree(h->subject);
    XtFree(h->newsgroups);
    XtFree(h->followup_to);
    XtFree(h->from);
    XtFree(h->reply_to);
    XtFree(h->message_id);
    XtFree(h->references);
    XtFree(h->content_type);
    XtFree(h->content_enc);
}

static int get_headers(SERVER *server, PostHeaders *h)
{
    char	*buffer;
    char	**curr = NULL;

    while ((buffer = server_read(server)) &&
	   buffer[0] != '\0' && !IS_DOT(buffer))
	if (IS_SPACE(buffer[0])) {
	    long	n, m;

	    if (!curr || !*curr)
		continue;

	    n = strlen(*curr);
	    buffer[0] = ' ';
	    m = strlen(buffer);
	    *curr = XtRealloc(*curr, m + n + 4);
	    memcpy(*curr + n, buffer, m + 1);
	} else {
	    char	*c;
	    int		len;
	    int		incl_name = False;

	    curr = NULL;
	    c = strchr(buffer, ':');
	    if (!c)
		continue;
	    len = c++ - buffer;
	    while (len > 0 && IS_SPACE(buffer[len - 1]))
		len--;
	    while (IS_SPACE(*c))
		c++;

#define IS_HEADER(header)                \
	    (len == sizeof header - 1 && \
	     case_lstrncmp(buffer, header, sizeof header - 1) == 0)
	    if (IS_HEADER("subject"))
		curr = &h->subject;
	    else if (IS_HEADER("newsgroups"))
		curr = &h->newsgroups;
	    else if (IS_HEADER("followup-to"))
		curr = &h->followup_to;
	    else if (IS_HEADER("from"))
		curr = &h->from;
	    else if (IS_HEADER("reply-to"))
		curr = &h->reply_to;
	    else if (IS_HEADER("message-id"))
		curr = &h->message_id;
	    else if (IS_HEADER("references"))
		curr = &h->references;
	    else if (IS_HEADER("content-type"))
		curr = &h->content_type, incl_name = True;
	    else if (IS_HEADER("content-transfer-encoding"))
		curr = &h->content_enc, incl_name = True;
	    else
		continue;
#undef IS_HEADER

	    *curr = incl_name ? XtNewString(buffer) : XtNewString(c);
	}

    if (!buffer) {
	free_headers(h);
	return False;
    }

#define CHECK(header) if (!h->header) h->header = XtNewString("")
    CHECK(subject);
    CHECK(newsgroups);
    CHECK(from);
    CHECK(message_id);
#undef CHECK

    return True;
}

static char *q_string(char *line, regex_t *re, char *q_str, char *qq_str)
{
    if (*line == '\0' && !global.quote_empty)
	return "";

    return re && regexec(re, line, 0, NULL, 0) == 0 ? qq_str : q_str;
}

static int print_multi_line(FILE     *fp,
			    regex_t  *re,
			    char     *q_str,
			    char     *qq_str,
			    char     *line,
			    int       append)
{
    char	*c;

    while ((c = strchr(line, '\n'))) {
	if (c > line && c[-1] == '\r')
	    c[-1] = '\0';
	*c++ = '\0';
	if (!append)
	    fputs(q_string(line, re, q_str, qq_str), fp);
	fprintf(fp, "%s\n", line);
	append = False;
	line = c;
    }

    if (*line != '\0') {
	if (!append)
	    fputs(q_string(line, re, q_str, qq_str), fp);
	fprintf(fp, "%s", line);
	append = True;
    }

    return append;
}

static char *do_quote(SERVER   *server,
		      FILE     *fp,
		      char     *q_str,
		      char     *qq_str,
		      char     *charset,
		      char     *c_type,
		      char     *c_enc,
		      int       quote_sig)
{
    regex_t	*re = res_quote_regexp();
    char	*buffer;
    int		enc = MimeEncNone;
    long	n;
    int		in_sig = False;

    if (c_type && c_enc) {
	MimeArg	args[5] = {{0, }, };
	char	*buf[2];
	char	type[128], subtype[128], *c;
	int	i;

	buf[0] = c_type;
	buf[1] = NULL;
	if (parse_content_type(buf, type, sizeof type,
			       subtype, sizeof subtype,
			       args, XtNumber(args) - 1, False) &&
	    strcmp(type, "text") == 0 &&
	    (c = get_charset(args)) && case_strcmp(c, charset) == 0) {
	    buf[0] = c_enc;
	    buf[1] = NULL;
	    enc = parse_content_enc(buf);
	    if (enc < 0)
		enc = MimeEncNone;
	}

	for (i = 0 ; i < XtNumber(args) && args[i].value ; i++) {
	    XtFree(args[i].name);
	    XtFree(args[i].value);
	}
    }

#define IS_SIG_DELIM(c)                 \
    ((c)[0] == '-' && (c)[1] == '-' &&  \
     (c)[2] == ' ' && (c)[3] == '\0')

    switch (enc) {
    default:
    case MimeEncNone:
	while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
	    char	*c = buffer + (*buffer == '.' ? 1 : 0);

	    if (IS_SIG_DELIM(c))
		in_sig = True;
	    if (quote_sig || !in_sig)
		fprintf(fp, "%s%s\n", q_string(c, re, q_str, qq_str), c);
        }
	break;
    case MimeEncQP:
	{
	    int	append = False;
	    int	soft;

	    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
		n = decode_qp(buffer, buffer + (*buffer == '.' ? 1 : 0),
			      strlen(buffer), &soft, False);
		if (n > 0) {
		    buffer[n] = '\0';
		    if (!append && IS_SIG_DELIM(buffer))
			in_sig = True;
		    if (quote_sig || !in_sig)
			print_multi_line(fp, re, q_str, qq_str,
					 buffer, append);
		    if (!soft)
			fputc('\n', fp);
		}
		append = soft;
	    }
	    if (append)
		fputc('\n', fp);
	}
	break;
    case MimeEncBase64:
	{
	    B64Context	bc = {0, };
	    long	dest_len = 128;
	    char	*dest = XtMalloc(dest_len);
	    int		append = False;

	    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
		n = strlen(buffer);
		if (n + 8 > dest_len)
		    dest = XtRealloc(dest, (dest_len = n + 8));
		n = decode_base64(&bc, dest, buffer, n);
		if (n > 0) {
		    dest[n] = '\0';
		    append = print_multi_line(fp, re, q_str, qq_str,
					      dest, append);
		}
	    }

	    n = decode_base64(&bc, dest, NULL, 0);
	    if (n > 0) {
		dest[n] = '\0';
		append = print_multi_line(fp, re, q_str, qq_str, dest, append);
	    }

	    if (append)
		fputc('\n', fp);

	    XtFree(dest);
	}
	break;
    case MimeEncUue:
	{
	    UueContext	uc = {0, };
	    int		append = False;

	    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
		n = decode_uue(&uc, buffer, buffer, strlen(buffer));
		if (n > 0) {
		    buffer[n] = '\0';
		    append = print_multi_line(fp, re, q_str, qq_str,
					      buffer, append);
		}
	    }
	}
	break;
    }

#undef IS_SIG_DELIM

    return buffer;
}

static int setup_file(PostContext *context,
		      FILE        *fp,
		      ARTICLE     *art,
		      int          quote,
		      int	   quote_sig,
		      int          supersede)
{
    PostHeaders	h = {0, };
    SERVER	*server = NULL;
    char	*full_name         = res_full_name();
    char	*posted_and_mailed = res_posted_and_mailed();
    char	*buffer = "";
    int		line = 1;

    if (!full_name)
	full_name = "";
    if (supersede)
	quote = quote_sig = True;

    server = cache_get_server(art->no, False);
    if (!server) {
	char	command[64];
	int	status;

	server = main_server;
	sprintf(command, "%s %ld\r\n",
		quote ? "ARTICLE" : "HEAD", art->no);
	buffer = server_comm(server, command, True);
	if (!buffer)
	    return -1;
	status = atoi(buffer);
	if (status != NNTP_OK_HEAD && status != NNTP_OK_ARTICLE) {
	    popup_title_notice("Couldn't retrieve article, \n"
			       "message from server is", buffer, True);
	    return False;
	}
    }

    if (!get_headers(server, &h)) {
	if (server == main_server)
	    return -1;
	popup_title_notice(NULL, "Couldn't retrieve article, \n"
			   "error with article cache.", True);
	server_free(server);
	return False;
    }

    line += insert_extra_headers(fp, art);
    line += print_references_header(fp, h.message_id, h.references);

    if (supersede) {
	fprintf(fp, "Supersedes: %s\n", h.message_id);
	line++;
    }

    if ((context->flags & MAIL) && !(context->flags & POST)) {
	fprintf(fp, "In-Reply-To: %s\n", h.message_id);
	line++;
    }

    fprintf(fp, "From: %s@%s (%s)\n",
	    global.mail_name, global.domain_name, full_name);
    line++;

    fprintf(fp, "Subject: Re: %s\n", eat_re(h.subject));
    line++;

    if (h.followup_to)
	if (case_lstrcmp(h.followup_to, "poster") != 0) {
	    XtFree(h.newsgroups);
	    h.newsgroups = h.followup_to;
	    h.followup_to = NULL;
	} else if (context->flags & POST) {
	    context->flags &= ~POST;
	    popup_notice("notice",
			 "The original author has requested\n"
			 "that followups be directed to email.",
			 "Close", NULL, NULL, 5000,
			 NULL, NULL, XtGrabNone);
	    context->flags &= ~POST;
	    context->flags |= MAIL;
	}

    fprintf(fp, "%sNewsgroups: %s\n",
	    context->flags & POST ? "" : "X-Original-", h.newsgroups);
    line++;

    if (context->flags & MAIL) {
	char	*c = h.reply_to ? h.reply_to : h.from;

	fprintf(fp, "To: %s\n",	c);
	line++;		    
    }

    line++;
    fputc('\n', fp);

    if ((context->flags & POST) && (context->flags & MAIL) &&
	posted_and_mailed && posted_and_mailed[0] != '\0') {
	fprintf(fp, "%s\n", posted_and_mailed);
	if (posted_and_mailed[strlen(posted_and_mailed) - 1] != '\n')
	    fputc('\n', fp);
	line += 2;
    }

    context->line = line;

    if (quote) {
	char	q_buf[32], qq_buf[32];

	q_buf[0] = '\0';
	qq_buf[0] = '\0';
	if (!supersede) {
	    print_with_percent(fp, res_attribution(), art);
	    fputc('\n', fp);
	    expand_quote_string(art, res_quote_string(), q_buf, sizeof q_buf);
	    expand_quote_string(art, res_quote_quote_string(),
				qq_buf, sizeof qq_buf);
	}

	buffer = do_quote(server, fp, q_buf, qq_buf, context->charset,
			  h.content_type, h.content_enc, quote_sig);
    }

    free_headers(&h);

    if (!supersede)
	append_signature(fp);

    if (!buffer)
	if (server == main_server)
	    return -1;
	else {
	    popup_title_notice(NULL, "Couldn't retrieve article, \n"
			       "error with article cache.", True);
	    server_free(server);
	    return False;
	}

    if (server != main_server)
	server_free(server);

    return True;
}

void post_setup(PostContext	*context,
		FILE		*fp,
		ARTICLE		*art,
		int		 quote,
		int		 quote_sig,
		int		 supersede)
{
    int		tmp;

    set_busy(True);
    tmp = setup_file(context, fp, art, quote, quote_sig, supersede);
    if (tmp <= 0) {
	free_post_context(context);
	if (tmp < 0)
	    reconnect_server(True);
	unset_busy();
	return;
    }
    unset_busy();
    fork_editor(context);
}
