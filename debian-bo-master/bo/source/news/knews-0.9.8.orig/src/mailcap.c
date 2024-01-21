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
#include "file.h"
#include "mailcap.h"
#include "util.h"

#define MAILCAP_HASH_SIZE	17

typedef struct MailCapEntry{
    struct MailCapEntry	*next;
    char		*type;
    char		*subtype;
    MailcapData		data;
} MailcapEntry;

static MailcapEntry	*mailcaps[MAILCAP_HASH_SIZE] = {0, };

static unsigned int hash(char *c)
{
    unsigned int	result = 0;

    while (*c != '\0')
	result += (unsigned char)*c++;

    return result % MAILCAP_HASH_SIZE;
}

static char *token_strip(char *token, int lower)
{
    int	n;

    while (IS_SPACE(*token))
	token++;

    n = strlen(token);
    while (n > 0 && IS_SPACE(token[n - 1]))
	token[--n] = '\0';

    if (n == 0)
	return NULL;

    if (lower)
	ascii_nlower(token, n);

    return token;
}

static MailcapEntry *mailcap_hash_create(char *type, char *subtype)
{
    static const MailcapEntry	zero = {0, };
    MailcapEntry	*entry, **loop;

    type = token_strip(type, True);
    if (!type)
	return NULL;

    if (subtype) {
	subtype = token_strip(subtype, True);
	if (strcmp(subtype, "*") == 0)
	    subtype = NULL;
    }

    loop = mailcaps + hash(type);
    while (*loop)
	loop = &(*loop)->next;

    entry = (MailcapEntry *)XtMalloc(sizeof *entry);
    *loop = entry;
    *entry = zero;
    entry->type = XtNewString(type);
    if (subtype)
	entry->subtype = XtNewString(subtype);

    return entry;    
}

char *expn_tmpl(char *src, int n, const char *tmpl, char **expn)
{
    long	dest_len = 128;
    char	*dest = XtMalloc(dest_len);
    long	dest_pos = 0;

    while (*src != '\0') {
	if (*src == '%') {
	    int	i;

	    for (i = 0 ; i < n ; i++)
		if (src[1] == tmpl[i])
		    break;

	    if (i < n) {
		int	expn_len = strlen(expn[i]);

		if (dest_len < dest_pos + expn_len + 8) {
		    dest_len = dest_pos + expn_len + 8;
		    dest = XtRealloc(dest, dest_len);
		}
		memcpy(dest + dest_pos, expn[i], expn_len + 1);
		dest_pos += expn_len;
		src += 2;
		continue;
	    }
	}

	if (dest_len < dest_pos + 8) {
	    dest_len = 2 * (dest_pos + 8);
	    dest = XtRealloc(dest, dest_len);
	}
	dest[dest_pos++] = *src++;
    }

    dest[dest_pos] = '\0';
    dest = XtRealloc(dest, dest_pos + 1);

    return dest;
}

static char *parse_mailcap_mtext(char *mtext)
{
    while (IS_SPACE(*mtext))
	mtext++;

    return mtext;
}

static int parse_mailcap_flag(MailcapEntry *entry, char *flag)
{
    char	first = *flag;

    switch (first) {
    case 'c':
	if (strcmp(flag, "copiousoutput") == 0) {
	    entry->data.copiousoutput = True;
	    return True;
	}
	break;
    case 'n':
	if (strcmp(flag, "needsterminal") == 0) {
	    entry->data.needsterminal = True;
	    return True;
	}
	break;
    }

    return False;
}

static int parse_mailcap_named_field(MailcapEntry *entry,
				     char *fieldname, char *mtext)
{
    char	first = *fieldname;
    char	**field = NULL;

    mtext = parse_mailcap_mtext(mtext);
    if (!mtext)
	return False;

    switch (first) {
    case 'c':
	if (strcmp(fieldname, "compose") == 0)
	    field = &entry->data.compose;
	else if (strcmp(fieldname, "composetyped") == 0)
	    field = &entry->data.compose_typed;
	break;
    case 'd':
	if (strcmp(fieldname, "description") == 0)
	    field = &entry->data.description;
	break;
    case 'e':
	if (strcmp(fieldname, "edit") == 0)
	    field = &entry->data.edit;
	break;
    case 'p':
	if (strcmp(fieldname, "print") == 0)
	    field = &entry->data.print;
	break;
    case 't':
	if (strcmp(fieldname, "test") == 0)
	    field = &entry->data.test;
	else if (strcmp(fieldname, "textualnewlines") == 0) {
	    long	tmp;

	    if (sscanf(mtext, "%ld", &tmp) != 1)
		return False;

	    entry->data.textualnewlines = (tmp != 0);
	    return True;
	}
	break;
    case 'x':
	if (strcmp(fieldname, "x11-bitmap") == 0)
	    field = &entry->data.x11_bitmap;
	break;
    }

    if (!field)
	return False;

    XtFree(*field);
    *field = XtNewString(mtext);

    return True;
}

static int parse_mailcap_line(char *line)
{
    static const char	tmpl = 'C';
    MailcapEntry	*entry;
    char		*type, *subtype;
    char		*view_cmd = NULL, *p;
    int			n;

    p = strchr(line, ';');
    if (!p)
	return False;

    *p++ = '\0';
    type = line;
    line = strchr(line, '/');
    if (!line)
	subtype = NULL;
    else {
	*line++ = '\0';
	subtype = line;
    }
    line = p;

    entry = mailcap_hash_create(type, subtype);
    if (!entry)
	return False;

    for (n = 0 ; ; n++) {
	char	*end;

	p = NULL;
	for (end = line ; *end != '\0' && *end != ';' ; end++)
	    if (n != 0 && end[0] == '=')
		p = end;
	    else if (end[0] == '\\' && end[1] != '\0')
		end++;

	if (*end == ';')
	    *end++ = '\0';

	if (p) {
	    *p++ = '\0';
	    line = token_strip(line, True);
	    if (line)
		parse_mailcap_named_field(entry, line, p);
	} else if (n == 0) {
	    while (IS_SPACE(*line))
		line++;
	    view_cmd = line;
	} else {
	    line = token_strip(line, True);
	    if (line)
		parse_mailcap_flag(entry, line);
	}

	if (*end != '\0')
	    line = end;
	else
	    break;
    }

    if (!view_cmd)
	entry->data.view_command = XtNewString("echo mailcap error");
    else if (entry->data.needsterminal && global.needs_terminal)
	entry->data.view_command =
	    expn_tmpl(global.needs_terminal, 1, &tmpl, &view_cmd);
    else if (entry->data.copiousoutput && global.copious_output)
	entry->data.view_command =
	    expn_tmpl(global.copious_output, 1, &tmpl, &view_cmd);
    else
	entry->data.view_command = XtNewString(view_cmd);

    return True;
}

static void parse_mailcap_file(char *buffer)
{
    char	*p;

    for (p = strchr(buffer, '\n') ; p ; p = strchr(p + 1, '\n'))
	if (p > buffer && p[-1] == '\\') {
	    p[-1] = ' ';
	    p[0] = ' ';
	}

    while ((p = strchr(buffer, '\n'))) {
	*p++ = '\0';
	if (buffer[0] != '#' && buffer[0] != '\0')
	    parse_mailcap_line(buffer);
	buffer = p;
    }
}

void mailcap_init(void)
{
    char	*paths =
	".mailcap:/etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap";
    char	*c;

    c = getenv("MAILCAPS");
    if (c) {
	paths = c;
	if (paths[0] == '\0')
	    return;
    }
    paths = XtNewString(paths);

    c = paths;
    for (;;) {
	char	*p = strchr(c, ':');
	int	fd;

	if (p)
	    *p = '\0';

	fd = open(c, O_RDONLY);
	if (fd < 0) {
	    if (errno != ENOENT)
		perror(c);
	} else {
	    char	*buffer;

	    buffer = snarf_file(fd, NULL);
	    if (!buffer)
		perror(c);
	    else
		parse_mailcap_file(buffer);

	    close(fd);
	    XtFree(buffer);
	}

	if (!p)
	    break;
	*p++ = ':';
	c = p;
    }

    XtFree(paths);
}

const MailcapData *mailcap_lookup(char *type, char *subtype)
{
    MailcapEntry	*loop;

    for (loop = mailcaps[hash(type)] ; loop ; loop = loop->next)
	if (strcmp(type, loop->type) == 0 &&
	    (!loop->subtype || strcmp(subtype, loop->subtype) == 0))
	    return &loop->data;

    return NULL;
}
