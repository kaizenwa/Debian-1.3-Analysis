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
#include <pwd.h>
#include "file.h"
#include "newsrc.h"
#include "p_I.h"
#include "p_check.h"
#include "resource.h"
#include "thread.h"
#include "util.h"
#include "../Widgets/ArtText.h"

static int has_8bit(const char *art)
{
    while (*art != '\0')
	if ((unsigned char)*art++ & 0x80)
	    return True;

    return False;
}

static void add_line(Widget w, const char *c, int error)
{
    Pixel	pixel = error ? global.alert_pixel : global.pixel;

    ArtTextAddLine(w, c, NULL, pixel);		       
}

static int max_len(const char *c, char sep)
{
    int		max = 0, len;
    const char	*p;

    do {
	p = strchr(c, sep);
	if (!p)
	    len = strlen(c);
	else {
	    len = p - c;
	    c = p + 1;
	}
	if (len > max)
	    max = len;
    } while (p);

    return max;
}

static void check_quoting(const char *art, char *prefix,
			  long *n_lines, long *n_quoted)
{
    int	n = prefix ? strlen(prefix) : 0;

    *n_lines = 0;
    *n_quoted = 0;

    for (;;) {
	const char	*p = strchr(art, '\n');

	++*n_lines;
	if (n > 0 && strncmp(prefix, art, n) == 0)
	    ++*n_quoted;
	if (!p)
	    break;
	art = p + 1;
    }
}

static int an_admin(void)
{
    static int		result = -1;
    struct passwd	*pwd;
    uid_t		uid;

    if (result < 0)
	result =
	    (uid = getuid()) == 0 ||
	    ((pwd = getpwnam("news")) && uid == pwd->pw_uid);

    return result;
}

static int check_cancel(Widget w, const char *field, const char *end)
{
    while (field < end) {
	ARTICLE		*art;
	const char	*c;

	if (*field++ != '<') {
	    add_line(w, "Syntax error in control message.", True);
	    return False;
	}

	c = field;
	while (c < end && *c != '\0' && *c != '\n' && *c != '>' && *c != '<')
	    c++;
	if (c >= end || *c != '>') {
	    add_line(w, "Syntax error in control message.", True);
	    return False;
	}

	if ((global.mode != NewsModeGroup && global.mode != NewsModeThread) ||
	    !(art = find_article(field, c - field))) {
	    add_line(w, "Error! Failed to authenticate cancel message:", True);
	    add_line(w, "       Couldn't find referenced article.", True);
	    return False;
	}

	/* possible security hole */
	if (!strstr(art->from, global.mail_name) ||
	    !strstr(art->from, global.domain_name)) {
	    add_line(w, "Error! You cannot cancel somebody else's article.",
		     True);
	    return False;
	}

	field = c + 1;
	while (field < end && (IS_SPACE(*field) || *field == '\n'))
	    field++;
    }

    return True;
}

static char *copy_field(const char *start, const char *end)
{
    char	*ret, *c;
    long	n = end - start;

    ret = XtMalloc(n + 1);
    memcpy(ret, start, n);
    ret[n] = '\0';

    for (c = strchr(ret, '\n') ; c ; c = strchr(c, '\n'))
	*c++ = ' ';

    return ret;
}

static int check_article(PostContext  *context,
			 const char   *art,
			 Widget        w,
			 char        **newsgroups,
			 char	     **followupto,
			 char        **subject,
			 char        **to,
			 char        **cc,
			 int          *has_ct,
			 int          *line_len,
			 long         *n_lines,
			 long         *n_quoted)
{
    char	buffer[512];
    int		has_cte = False;
    int		has_from = False;

    *newsgroups = NULL;
    *followupto = NULL;
    *subject    = NULL;
    *to         = NULL;
    *cc         = NULL;
    *has_ct     = False;
    *line_len   = 0;
    *n_lines    = 0;
    *n_quoted   = 0;

#define RETURN_ERROR(msg)        \
    do {                         \
	add_line(w, msg, True);  \
	return False;            \
    } while (0)

    if (*art == '\0')
	RETURN_ERROR("Error!  Empty article.");
    if (*art == '\n')
	RETURN_ERROR("Error!  No headers in article.");

    /*
     * Check headers.
     */
    context->line = 1;
    while (*art != '\n') {
	const char	*field = strchr(art, ':');
	const char	*end, *tmp;
	int		lines, len;
	unsigned char	ch;

	if (!field) {
	    add_line(w, "Syntax error in article header near", True);
	    add_line(w, "", True);
	    buffer[0] = '\0';
	    strncat(buffer, art, 128);
	    add_line(w, buffer, True);
	    add_line(w, "", True);
	    add_line(w, "Remember to put an EMPTY line between the", True);
	    add_line(w, "headers and the body (the article/mail text)!", True);
	    return False;
	}

	len = field - art;
	if (((tmp = strchr(art, ' ')) && tmp < field) ||
	    ((tmp = strchr(art, '\t')) && tmp < field) ||
	    ((tmp = strchr(art, '\n')) && tmp < field)) {
	    strcpy(buffer, "Syntax error in '");
	    if (len > 128)
		len = 128;
	    strncat(buffer, art, len);
	    strcat(buffer, "' header.");
	    add_line(w, buffer, True);
	    add_line(w, "Remember to put an EMPTY line between the", True);
	    add_line(w, "headers and the body (the article/mail text)!", True);
	    return False;
	}

	field++;
	if (*field++ != ' ') {
	    strcpy(buffer, "Syntax error in '");
	    if (len > 128)
		len = 128;
	    strncat(buffer, art, len);
	    strcat(buffer, "' header.");
	    add_line(w, buffer, True);
	    add_line(w, "There must be a space after the colon!", True);
	    return False;
	}

	while (IS_SPACE(*field))
	    field++;
	lines = 1;
	for (end = strchr(field, '\n') ;
	     end && IS_SPACE(end[1]) ;
	     end = strchr(end + 1, '\n'))
	    lines++;

	if (!end)
	    RETURN_ERROR("Error: article is all headers!");

	ch = *art;
	if (isupper(ch))
	    ch = tolower(ch);

#define IS_HEADER(header)                        \
	(len == sizeof(header) - 1 &&            \
	 case_lstrncmp(art, header, len) == 0)

	switch (ch) {
	case 'a':
	    if (IS_HEADER("also-control") && !an_admin())
		if (case_lstrncmp(field, "cancel ", 7) != 0)
		    RETURN_ERROR("Error! No permission to "
				 "send control message.");
		else if (!check_cancel(w, field + 7, end))
		    return False;
	    break;
	case 'c':
	    if (IS_HEADER("cc") && !*cc)
		*cc = copy_field(field, end);
	    else if (IS_HEADER("content-type"))
		if (*has_ct)
		    RETURN_ERROR("Duplicate 'Content-Type:' header.");
		else
		    *has_ct = True;
	    else if (IS_HEADER("content-transfer-encoding"))
		if (has_cte)
		    RETURN_ERROR("Duplicate 'Content-Transfer-Encoding:' "
				 " header.");
		else
		    has_cte = True;
	    else if (IS_HEADER("control") && !an_admin())
		if (case_lstrncmp(field, "cancel ", 7) != 0)
		    RETURN_ERROR("Error! No permission "
				 "to send control message.");
		else if (!check_cancel(w, field + 7, end))
		    return False;
	    break;
	case 'f':
	    if (IS_HEADER("from")) {
		if (has_from)
		    RETURN_ERROR("Error! Duplicate 'From:' header.");
		has_from = True;
		if (!(tmp = strstr(field, global.domain_name)) || tmp > end ||
		    !(tmp = strstr(field, global.user_id)) || tmp > end)
		    context->flags |= NEEDS_SENDER;
	    } else if (IS_HEADER("followup-to")) {
		if (*followupto)
		    RETURN_ERROR("Error! Duplicate 'Followup-To:' header");
		if (lines > 1) {
		    add_line(w, "Error! The 'Followup-To:' header", True);
		    add_line(w, "cannot be continued on several lines", True);
		    return False;
		}
		*followupto = copy_field(field, end);
		if (*field == '\n' || *field == '\0')
		    RETURN_ERROR("Error! Empty 'Followup-To:' header");
		if (((tmp = strchr(field, ' ')) && tmp < end) ||
		    ((tmp = strchr(field, '\t')) && tmp < end)) {
		    add_line(w, "Error! There can be no spaces between", True);
		    add_line(w, "the commas in the 'Followup-To:' header",
			     True);
		    return False;
		}
		break;
	    }
	case 'n':
	    if (IS_HEADER("newsgroups")) {
		if (*newsgroups)
		    RETURN_ERROR("Error! Duplicate 'Newsgroups:' header.");
		if (lines > 1) {
		    add_line(w, "Error! The 'Newsgroups:' header", True);
		    add_line(w, "cannot be continued on several lines.", True);
		    return False;
		}
		*newsgroups = copy_field(field, end);
		if (*field == '\n' || *field == '\0')
		    RETURN_ERROR("Error! Empty 'Newsgroups:' header");
		if (((tmp = strchr(field, ' ')) && tmp < end) ||
		    ((tmp = strchr(field, '\t')) && tmp < end)) {
		    add_line(w, "Error! There can be no spaces between", True);
		    add_line(w, "the commas in the 'Newsgroups:' header",
			     True);
		    return False;
		}
	    }
	    break;
	case 's':
	    if (IS_HEADER("sender"))
		RETURN_ERROR("Error! You cannot specify a 'Sender:' header.");
	    else if (IS_HEADER("supersedes") && !an_admin() &&
		     !check_cancel(w, field, end))
		return False;
	    else if (IS_HEADER("subject")) {
		if (*subject)
		    RETURN_ERROR("Error! Duplicate 'Subject:' header.");
		if (*field == '\n' || *field == '\0')
		    RETURN_ERROR("Error! Empty 'Subject:' header.");
		*subject = copy_field(field, end);
	    }
	    break;
	case 't':
	    if (IS_HEADER("to"))
		*to = copy_field(field, end);
	    break;
	}

#undef IS_HEADER

	context->line += lines;
	art = end + 1;
    }

    art++;

    if (!*subject)
	RETURN_ERROR("Error! Article/mail has no 'Subject:' header.");
    if (!has_from)
	RETURN_ERROR("Error! Article/mail has no 'From:' header.");
    if (!*newsgroups && !*to && !*cc)
	RETURN_ERROR("Error! Article/mail has no "
		     "'Newsgroups:', 'To:' or 'Cc:' header.");

    context->line += 2;
    *line_len = max_len(art, '\n');
    check_quoting(art, res_quote_string(), n_lines, n_quoted);

    if (*line_len > 1024) {
	add_line(w, "Error! The article/mail contains lines", True);
	add_line(w, "longer than 1024 characters.", True);
	add_line(w, "Posting will not be allowed.", True);
	return False;
    }

    return True;
}

static int n_commas(const char *c)
{
    int	n = 0;

    for (c = strchr(c, ',') ; c ; c = strchr(c + 1, ','))
	n++;

    return n;
}

static void format_ok_message(Widget      w,
			      char       *subject,
			      char       *newsgroups,
			      char	 *followupto,
			      char       *to,
			      char       *cc,
			      char	 *charset,
			      int         line_len,
			      long        n_lines,
			      long        n_quoted)
{
    char	buffer[1024];
    char	*art_or_mail = newsgroups ? "article" : "mail";
    int		consider_edit = False;
    int		added_note    = False;

    if (!to) {
	to = cc;
	cc = NULL;
    }

    sprintf(buffer, "Subject: %s", subject);
    add_line(w, buffer, False);
    if (charset) {
	sprintf(buffer, "  (No Content-Type header, assuming "
		"Content-Type: text/plain; charset=%s", charset);
	add_line(w, buffer, True);
    }
    add_line(w, "", False);

    if (line_len > 76) {
	sprintf(buffer,	"Note: This %s contains %d character lines.",
		art_or_mail, line_len);
	add_line(w, buffer, True);
	consider_edit = True;
	added_note = True;
    }

    if (n_lines > 16 && 2 * n_quoted > n_lines) {
	char	*tmp = res_quote_string();

	sprintf(buffer,	"Note: This %s has %ld%% quoted text.",
		art_or_mail, 100 * n_quoted / n_lines);
	add_line(w, buffer, True);
	if (tmp && *tmp != '>')
	    add_line(w, "      [You can fool the server, "
		      "but you can't fool me.]", True);
	consider_edit = True;
	added_note = True;
    }

    if (consider_edit)
	add_line(w, "      You should consider editing it.", True);
    if (added_note)
	add_line(w, "", False);

    if (to) {
	add_line(w, "Mailing to:", False);
	add_line(w, to, False);
	if (cc)
	    add_line(w, cc, False);
	add_line(w, "", False);
    }

    if (newsgroups) {
	int	n;

	if (global.mode == NewsModeDisconnected)
	    add_line(w, "Posting to:  "
		      "[can't check them: not connected]", False);
	else
	    add_line(w, "Posting to:", False);

	n = n_commas(newsgroups);
	if (n > 6 && (!followupto || n_commas(followupto) > 6)) {
	    sprintf(buffer, "%s: Posting to %d groups.",
		    n > 12 ? "WARNING" : "Warning", n);
	    add_line(w, buffer, True);
	    if (!followupto)
		add_line(w, "         Consider adding a Followup-To header.",
			 True);
	    else if (n_commas(followupto) > 6)
		add_line(w, "         Consider cutting down the Followup-To "
			 "header.", True);
	}

	for (;;) {
	    char	*p;
	    GROUP	*group = NULL;
	    char	*status;
	    int		ok;

	    p = strchr(newsgroups, ',');
	    if (p)
		*p++ = '\0';

	    if (global.mode == NewsModeDisconnected)
		ok = True,  status = "[...]";
	    else if (!(group = find_group(newsgroups)))
		ok = False, status = "[???]";
	    else if (group->moderated)
		ok = True,  status = "[mod]";
	    else
		ok = True,  status = "[ok ]";

	    if (strlen(newsgroups) > 512)
		newsgroups[512] = '\0';
	    sprintf(buffer, "%s %s", status, newsgroups);
	    add_line(w, buffer, !ok);

	    if (!p)
		break;
	    newsgroups = p;
	}
    }
}

void check_article_to_post(PostContext *context, Widget w)
{
    const char	*art = context->art;
    char	*newsgroups, *followupto, *subject, *to, *cc;
    int		tmp, has_ct, line_len;
    long	n_lines, n_quoted;

    context->flags &= ~OK_TO_POST;
    if (!art)
	return;

    tmp = check_article(context, art, w, &newsgroups, &followupto,
			&subject, &to, &cc,
			&has_ct, &line_len, &n_lines, &n_quoted);
    if (tmp) {
	char	*charset = NULL;

	context->has_8bit = has_8bit(art);
	if (!has_ct && context->has_8bit)
	    charset = context->charset;

	format_ok_message(w, subject, newsgroups, followupto, to, cc,
			  charset, line_len, n_lines, n_quoted);
	if (to || cc)
	    context->flags |= MAIL;
	else
	    context->flags &= ~MAIL;
	if (newsgroups)
	    context->flags |= POST;
	else
	    context->flags &= ~POST;
	context->flags |= OK_TO_POST;
    }

    XtFree(newsgroups);
    XtFree(subject);
    XtFree(to);
    XtFree(cc);
}
