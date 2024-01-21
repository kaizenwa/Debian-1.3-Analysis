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
 * SOFTWARE IS AT THE USER'S OWN RISK.
 */
#include "global.h"
#include "expand.h"
#include "parse.h"
#include "util.h"

char *regexp_escape_string(char *src, int anchor)
{
    char	*result, *dest;

    dest = result = XtMalloc(2 * strlen(src) + 3);

    if (anchor)
	*dest++ = '^';
    while (*src != '\0') {
	if (strchr("^.[$()|*+?{\\", *src))
	    *dest++ = '\\';
	*dest++ = *src++;
    }
    if (anchor)
	*dest++ = '$';
    *dest = '\0';

    return XtRealloc(result, strlen(result) + 1);
}

char *expand_view_command(const char	*src,
			  char		*type,
			  char		*subtype,
			  MimeArg	*args,
			  int		 needs_term,
			  int		 copious)
{
    char	*dest;
    const char	*p;
    long	len, pos, i, n;

    pos = 0;
    len = 120;
    dest = XtMalloc(len + 3);
    dest[0] = '\0';

    while (*src != '\0')
	if (*src == '%') {
	    char	c = *++src;

	    switch (c) {
	    case '\0':
		continue;
	    case 's':
		src++;
		dest[pos++] = '%';
		dest[pos++] = 's';
		continue;
	    case 't':
		n = strlen(type) + strlen(subtype) + 8;
		if (pos + n + 8 > len) {
		    len += n + 8;
		    dest = XtRealloc(dest, len + 3);
		}
		sprintf(dest + pos, "%s/%s", type, subtype);
		pos += strlen(dest + pos);
		continue;
	    case '{':
		p = strchr(src, '}');
		if (!p)
		    break;
		src++;

		for (i = 0 ; args[i].value ; i++)
		    if (strlen(args[i].name) == p - src &&
			case_strncmp(src, args[i].name, p - src) == 0)
			break;

		src = p + 1;

		if (!args[i].value)
		    continue;

		n = strlen(args[i].value);
		if (pos + n + 8 > len) {
		    len += n + 8;
		    dest = XtRealloc(dest, len + 3);
		}

		strcpy(dest + pos, args[i].value);
		pos += strlen(dest + pos);
		continue;
	    }

	    dest[pos++] = c;
	    src++;
	} else {
	    if (pos + 8 > len) {
		len *= 2;
		dest = XtRealloc(dest, len + 3);
	    }

	    if (*src != '\\')
		dest[pos++] = *src++;
	    else if (*++src != '\0')
		dest[pos++] = *src++;
	}

    dest[pos] = '\0';

    return dest;
}

char *expand_path(char *file_name)
{
    char	*path = NULL;
    long	len = 0, pos = 0;
    char	ch;

    if (file_name[0] == '~' && file_name[1] == '/')
	file_name += 2;

    for (ch = *file_name++ ; ch != '\0' ; ch = *file_name++) {
	if (pos + 8 > len) {
	    len = pos + 256;
	    path = XtRealloc(path, len);
	}

	if (ch != '%')
	    path[pos++] = ch;
	else {
	    char	*p, *c = NULL;
	    int		cap    = False;
	    int 	slash  = False;
	    int		clen   = 0;

	    ch = *file_name++;
	    switch (ch) {
	    case '%':
		path[pos++] = '%';
		continue; /* don't fall through */
	    case 'a':
	    case 'A':
		if (global.mode != NewsModeGroup &&
		    global.mode != NewsModeThread) {
		    fputs("knews: Not in a newsgroup!\n", stderr);
		    XtFree(path);
		    return NULL;
		}
		if (!global.curr_art) {
		    fputs("knews: No selected article!\n", stderr);
		    XtFree(path);
		    return NULL;
		}
		sprintf(path + pos, "%ld", global.curr_art->no);
		pos += strlen(path + pos);
		continue;
	    case 'g':
		slash  = True;
		break;
	    case 'G':
		cap    = True;
		slash  = True;
		break;
	    case 'n':
		break;
	    case 'N':
		cap    = True;
		break;
	    case 'p':
	    case 'P':
		c = global.nntp_server;
		if (c)
		    c = strchr(c, ':');
		if (!c)
		    continue;
		clen = strlen(c);
		break;
	    case 's':
	    case 'S':
		c = global.nntp_server;
		if (!c) {
		    fputs("knews: nntp_server is NULL!\n", stderr);
		    XtFree(path);
		    return NULL;
		}
		p = strchr(c, ':');
		if (p)
		    clen = p - c;
		else
		    clen = strlen(c);
		break;
	    default:
		fprintf(stderr,
			"knews: %%%c: Unknown format specifier.\n", ch);
		XtFree(path);
		return NULL;
	    }

	    if (!c)
		if (global.curr_group && global.curr_group->name) {
		    c = global.curr_group->name;
		    clen = strlen(c);
		} else {
		    fputs("knews: Not in a newsgroup.\n", stderr);
		    XtFree(path);
		    return NULL;
		}

	    if (clen == 0)
		continue;
	    if (pos + clen + 8 > len) {
		len = pos + clen + 256;
		path = XtRealloc(path, len);
	    }

	    ch = *c++;
	    clen--;

	    if (cap && islower((unsigned char)ch))
		ch = toupper((unsigned char)ch);

	    path[pos++] = ch;
	    while (clen-- > 0) {
		ch = *c++;

		if (ch == '.' && slash)
		    ch ='/';
		path[pos++] = ch;
	    }
	    path[pos] = '\0';
	}
    }
    path[pos] = '\0';

    return path;
}
