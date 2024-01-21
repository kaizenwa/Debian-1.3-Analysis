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
#include "file.h"
#include "font.h"
#include "parse.h"
#include "partial.h"
#include "read.h"
#include "resource.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Scrollable.h"

#define PARTIAL_HASH_SIZE	23

typedef struct PCE	PCE;
struct PCE {
    PCE		*next;
    char	*id;
    int		tot;
    int		n_alloc;
    long	*arts;
    Widget	notice;
};

static PCE	*cache[PARTIAL_HASH_SIZE] = {0, };

void partial_clear_cache(void)
{
    PCE	*loop, *next;
    int	i;

    for (i = 0 ; i < PARTIAL_HASH_SIZE ; i++) {
	for (loop = cache[i] ; loop ; loop = next) {
	    next = loop->next;
	    loop->next = NULL;
	    if (loop->notice) {
		XtPopdown(loop->notice);
		XtDestroyWidget(loop->notice);
		loop->notice = NULL;
	    }
	    XtFree(loop->id);
	    XtFree((char *)loop->arts);
	    XtFree((char *)loop);
	}
	cache[i] = NULL;
    }
}

static void get_cache_stats(long *n_complete, long *n_fragmented)
{
    long	n_comp = 0, n_frag = 0;
    PCE		*loop;
    int		i, j;

    for (i = 0 ; i < PARTIAL_HASH_SIZE ; i++)
	for (loop = cache[i] ; loop ; loop = loop->next)
	    if (loop->tot <= 0)
		n_frag++;
	    else {
		for (j = loop->tot ; j > 0 ; j--)
		    if (loop->arts[j] <= 0) {
			n_frag++;
			break;
		    }

		if (j == 0)
		    n_comp++;
	    }

    *n_complete = n_comp;
    *n_fragmented = n_frag;
}


static unsigned int hash(char *c)
{
    unsigned int	result = 0;

    while (*c != '\0')
	result += (unsigned char)*c++;

    return result % PARTIAL_HASH_SIZE;
}

static PCE **partial_cache_find(char *id)
{
    PCE	**cp = cache + hash(id);

    while (*cp && strcmp((*cp)->id, id) != 0)
	cp = &(*cp)->next;

    return cp;
}

static void remove_cache_entry(PCE *c)
{
    PCE	**cp = partial_cache_find(c->id);

    if (*cp != c)
	return;

    *cp = c->next;
    c->next = NULL;
    XtFree(c->id);
    c->id = NULL;
    XtFree((char *)c->arts);
    c->arts = NULL;
    XtFree((char *)c);
}

static void partial_dialogue_callback(Widget, XtPointer, XtPointer);

static void add_to_arts(PCE *c, int num, int no)
{
    int	i;

    if (num >= c->n_alloc) {
	i = c->n_alloc;
	if (c->tot > 0)
	    c->n_alloc = c->tot + 1;
	else
	    c->n_alloc = num + 1;
	c->arts = (long *)XtRealloc((char *)c->arts,
				    c->n_alloc * sizeof c->arts[0]);
	while (i < c->n_alloc)
	    c->arts[i++] = 0;
    }

    if (c->arts[num] > 0) {
	if (c->arts[num] != no)
	    fprintf(stderr,
		    "knews: duplicate part %d in "
		    "message/partial with id=\"%s\".\n",
		    num, c->id);
	return;
    }

    c->arts[num] = no;
}

static void create_cache_notice(PCE *c)
{
    char	message[512];
    ARTICLE	*art;

    art = get_articles(main_thr);
    while (art && art->no < c->arts[1])
	art = art->next;
    if (art->no != c->arts[1])
	art = NULL;

    if (!art || strlen(art->subject->subject) > 200)
	strcpy(message, "Have all parts of message/partial.");
    else {
	if (!art->from || strlen(art->from) > 200)
	    sprintf(message,
		    "Have all parts of message/partial article.\n\n"
		    "Subject: %s",
		    art->subject->subject);
	else
	    sprintf(message,
		    "Have all parts of message/partial article.\n\n"
		    "From: %s\n\n"
		    "Subject: %s",
		    art->from, art->subject->subject);
    }

    c->notice =
	popup_notice("partialnotice", message, "Assemble", NULL, "Forget",
		     0, partial_dialogue_callback, (XtPointer)c, XtGrabNone);
}

static void add_to_cache(long no, char *id, int num, int tot, int notify)
{
    PCE		**cp, *c;
    int		i;

    cp = partial_cache_find(id);
    c = *cp;
    if (c) {
	if (tot > 0)
	    if (c->tot <= 0)
		c->tot = tot;
	    else if (tot != c->tot) {
		fputs("knews: message/partial with conflicting "
		      "'total' parameters, ignoring last one.\n", stderr);
		return;
	    }
	if (c->tot > 0 && num > c->tot) {
	    fputs("knews: message/partial with conflicting "
		  "'total' and 'number' parameters, ignoring.\n", stderr);
	    return;
	}
    } else {
	c = *cp = (PCE *)XtMalloc(sizeof **cp);
	c->next = NULL;
	c->arts = NULL;
	c->n_alloc = 0;
	c->id = XtNewString(id);
	c->tot = tot;
	c->notice = NULL;
    }

    add_to_arts(c, num, no);

    if (!notify || c->tot <= 0 || c->notice)
	return;

    for (i = 1 ; i <= c->tot ; i++)
	if (c->arts[i] <= 0)
	    return;

    create_cache_notice(c);
}

void partial_cache_hook(long no, struct MimeArg *args, int notify)
{
    char	*id     = NULL;
    char	*total  = NULL;
    char	*number = NULL;
    int		num, tot = 0;

    while (args->value) {
	switch (args->name[0]) {
	case 'i':
	    if (strcmp(args->name, "id") == 0)
		id = args->value;
	    break;
	case 'n':
	    if (strcmp(args->name, "number") == 0)
		number = args->value;
	    break;
	case 't':
	    if (strcmp(args->name, "total") == 0)
		total = args->value;
	    break;
	}
	args++;
    }

    if (!id)
	fputs("knews: message/partial without "
	      "'id' parameter, ignoring.\n", stderr);
    else if (!number)
	fputs("knews: message/partial without "
	      "'number' parameter, ignoring.\n", stderr);
    else if (sscanf(number, "%d", &num) != 1 || num <= 0)
	fputs("knews: message/partial with bad "
	      "'number' parameter, ignoring.\n", stderr);
    else if (total && (sscanf(total, "%d", &tot) != 1 || tot <= 0))
	fputs("knews: massage/partial with bad "
	      "'total' parameter, ignoring.\n", stderr);
    else
	add_to_cache(no, id, num, tot, notify);
}

static int is_mime_header(char *header)
{
    unsigned char	c = tolower((unsigned char)*header++);

    switch (c) {
    case 'c':
	return case_lstrncmp(header, "ontent-", 7) == 0;
    case 'e':
	return case_lstrncmp(header, "ncrypted:", 9) == 0;
    case 'm':
	return (case_lstrncmp(header, "ime-version:", 12) == 0 ||
		case_lstrncmp(header, "essage-id:", 10) == 0);
    }

    return False;
}

static char *dump_art(SERVER *server, FILE *fp, int is_first)
{
    char	*reply;
    int		doit = is_first;

    reply = server_read(server);
    while (reply && reply[0] != '\0' && !IS_DOT(reply)) {
	if (!IS_SPACE(reply[0]))
	    doit = is_first && !is_mime_header(reply);
	if (doit)
	    fprintf(fp, "%s\r\n", reply);
	reply = server_read(server);
    }

    if (!reply || IS_DOT(reply))
	return reply;

    if (is_first) {
	doit = True;
	reply = server_read(server);
	while (reply && reply[0] != '\0' && !IS_DOT(reply)) {
	    if (!IS_SPACE(reply[0]))
		doit = is_mime_header(reply);
	    if (doit)
		fprintf(fp, "%s\r\n", reply);
	    reply = server_read(server);
	}

	if (!reply || IS_DOT(reply))
	    return reply;

	fputs("\r\n", fp);
    }

    reply = server_read(server);
    while (reply && !IS_DOT(reply)) {
	fprintf(fp, "%s\r\n", reply);
	reply = server_read(server);
    }

    return reply;
}

static char *snarf_articles(FILE *fp, long *arts, int n)
{
    int		i;

    for (i = 0 ; i < n ; i++) {
	SERVER	*server;
	char	*reply;

	server = cache_get_server(arts[i], False);
	if (!server) {
	    char	command[128];

	    server = main_server;
	    sprintf(command, "ARTICLE %ld\r\n", arts[i]);
	    reply = server_comm(server, command, True);
	    if (!reply || atoi(reply) != NNTP_OK_ARTICLE)
		return reply;
	}

	reply = dump_art(server, fp, i == 0);

	if (server != main_server)
	    server_free(server);
	else if (!reply)
	    return NULL;
    }

    fputs(".\r\n", fp);

    return CODE_TO_STR(NNTP_OK_ARTICLE);
}

static void partial_dialogue_callback(Widget w,
				      XtPointer client_data,
				      XtPointer call_data)
{
    NoticeReply	reply = (NoticeReply)call_data;
    PCE		*c = (PCE *)client_data;

    if (global.busy || (global.mode != NewsModeThread &&
			global.mode != NewsModeGroup))
	return;

    if (reply == NoticeReplyLeft) {
	SERVER	*server;
	char	*buffer;
	FILE	*fp;

	fp = create_temp_file(&buffer);
	if (!fp) {
	    set_message("Failed to create temp file!", True);
	    return;
	}
	unlink(buffer);

	set_busy(True);
	buffer = snarf_articles(fp, c->arts + 1, c->tot);
	if (!buffer) {
	    fclose(fp);
	    reconnect_server(True);
	    unset_busy();
	    return;
	}

	unset_busy();

	if (atoi(buffer) != NNTP_OK_ARTICLE) {
	    char	message[256];

	    if (strlen(buffer) > 128)
		buffer[128] = '\0';
	    sprintf(message,
		    "Couldn't get all parts, message from server is: %s",
		    buffer);
	    set_message(message, True);
	    fclose(fp);
	    return;
	}

	if (fflush(fp) != 0 || fseek(fp, SEEK_SET, 0) != 0) {
	    perror("knews: fseek");
	    set_message("Error: couldn't rewind temp file!", True);
	    fclose(fp);
	    return;
	}

	set_curr_art(NULL, False);

	server = server_create(fileno(fp));

	buffer = server_read(server);
	if (!res_full_header())
	    buffer = do_mime(NULL, server, buffer, False, NULL, 0, NULL);
	else {
	    XFontStruct	*font = ascii_font->header_font;
	    Pixel	pixel = global.header_pixel;

	    ArtTextClearLines(main_widgets.text);
	    ScrollableSuspend(main_widgets.text);
	    while (buffer && !IS_DOT(buffer)) {
		ArtTextAddLine(main_widgets.text, buffer, font, pixel);
		if (buffer[0] == '\0') {
		    font = ascii_font->body_font;
		    pixel = global.pixel;
		}
		buffer = server_read(server);
	    }
	    ScrollableResume(main_widgets.text);
	}

	if (!buffer)
	    set_message("Error with temp file!", True);
	else
	    set_standard_message();

	fclose(fp);
	server_set_fd(server, -1);
	server_free(server);
    }

    XtPopdown(c->notice);
    XtDestroyWidget(c->notice);
    c->notice = NULL;
    remove_cache_entry(c);
}

#define N_ARGS 8
static char *build_cache(ARTICLE **arts, long n_arts)
{
    char	message[128], *msg_end;
    MimeArg	args[N_ARGS + 1];
    char	type[128], subtype[128];
    char	*headers[2];
    char	command[256];
    char	*reply = "", *c, *p;
    long	i, j;

    strcpy(message, "Building message/partial cache...  ");
    msg_end = message + strlen(message);

    for (i = 0 ; i < n_arts ; i++) {
	sprintf(msg_end, "%ld/%ld", i, n_arts);
	set_message(message, False);

	if (!arts[i]->from)
	    continue;

	sprintf(command, "HEAD %ld\r\n", arts[i]->no);
	reply = server_comm(main_server, command, True);
	if (!reply)
	    break;
	if (atoi(reply) != NNTP_OK_HEAD)
	    continue;

	reply = server_read_chunk(main_server);
	if (!reply)
	    break;

	c = reply;
	while (case_lstrncmp(c, "content-type:", 13) != 0) {
	    c = strchr(c, '\n');
	    if (c)
		c++;
	    else
		break;
	}

	if (!c)
	    continue;

	for (p = strchr(c, '\n') ; p ; p = strchr(p, '\n'))
	    if (IS_SPACE(p[1])) {
		if (p > c && p[-1] == '\r')
		    p[-1] = ' ';
		*p++ = ' ';
	    } else {
		p[0] = '\0';
		break;
	    }

	for (j = 0 ; j < N_ARGS + 1 ; j++)
	    args[j].name = args[j].value = NULL;
	headers[0] = c;
	headers[1] = NULL;

	if (parse_content_type(headers, type, sizeof type,
			       subtype, sizeof subtype, args, N_ARGS, False)) {
	    if (strcmp(type, "message") == 0 &&
		strcmp(subtype, "partial") == 0)
		partial_cache_hook(arts[i]->no, args, True);
	    for (j = 0 ; j < N_ARGS ; j++) {
		XtFree(args[j].name);
		XtFree(args[j].value);
		args[j].name = args[j].value = NULL;
	    }
	}
    }

    if (!reply)
	return NULL;

    return CODE_TO_STR(NNTP_OK_HEAD);
}

void partial_build_cache(void)
{
    char	message[256];
    ARTICLE	**arts = get_tagged_articles();
    long	n_arts = no_tagged_articles();
    char	*reply;

    if (!arts || n_arts <= 0) {
	set_message("No tagged articles!", True);
	return;
    }

    set_busy(True);
    reply = build_cache(arts, n_arts);
    if (!reply) {
	reconnect_server(True);
	unset_busy();
	partial_clear_cache();
    }

    unset_busy();
    if (atoi(reply) == NNTP_OK_HEAD) {
	long	n_complete, n_fragmented;

	get_cache_stats(&n_complete, &n_fragmented);
	sprintf(message,
		"Cache built:  %ld complete articles,  "
		"%ld partially complete.",
		n_complete, n_fragmented);
	set_message(message, False);
	clear_tagged_articles();
    } else {
	if (strlen(reply) > 200)
	    reply[200] = '\0';
	sprintf(message, "Error, message from server is: %s", reply);
	set_message(message, True);
    }
}
