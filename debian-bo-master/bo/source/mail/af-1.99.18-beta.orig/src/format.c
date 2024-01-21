/* Format.c - Format string handling for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "format.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "mode.h"
#include "tags.h"
#include "version.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: format.c,v 1.14 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *get_vtext();
extern char *strkey(), *modelist(), *utos();
extern char *strqdate(), *typeonly();
extern int get_vval(), active();
extern int to_reading(), tlines();
extern unsigned cmodes(), position();
extern unsigned to_lines(), to_position();
extern void free(), emsgl(), display();
extern MESSAGE *find_start();

/* Local function declarations */

static char *get_prefix(), *get_contents();
static int get_width(), get_padding();
static int setfill(), setfield();
static CONVERSION *find_conv(), *find_msg_conv();

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
/* These structures store the current line lengths */

static FORMAT_LEN hdr_len = { 0, 0 };
static FORMAT_LEN mode_len = { 0, 0 };

/****************************************************************************/
int set_length(format, modeline)
char *format;
int modeline;
{
	/*
	 * Store the number of character positions and scaled
	 * character positions specified in the format string.
	 * Return TRUE if ok, or FALSE on error, such as bad
	 * formats or nonprinting characters in format string.
	 */

	char *f;
	int width, conv_len;
	int length = 0, scaled = 0;
	CONVERSION *conv;

	/* Scan the format string */

	for (f = format; *f != '\0'; f++) {
		if (ISCONV(*f)) {
			/* Get any specified length of the conversion */

			f = get_prefix(f + 1, &width);

			/* Find the conversion data */

			if ((conv = find_conv(*f)) == NULL) {
				emsgl("Invalid escape %",
				      strkey(*f, SK_DISPLAY),
				      " in format", NULL);
				return(FALSE);
			}

			/* Get the effective length of the conversion */

			conv_len = (width) ? width : conv->width;
			conv_len = (conv_len < 0) ? -conv_len : conv_len;

			/* Add the length to the totals */

			length += conv_len;
			if (conv->scaled && !width) {
				scaled += conv_len;
			}
		} else if (iscntrl(*f)) {
			/* Control characters invalid in formats */

			emsgl("Invalid character %",
			      strkey(*f, SK_DISPLAY),
			      " in format", NULL);
			return(FALSE);
		} else {
			length++;
		}
	}

	/* Store the escape as required */

	if (modeline) {
		mode_len.length = length;
		mode_len.scaled = scaled;
	} else {
		hdr_len.length = length;
		hdr_len.scaled = scaled;
	}

	return(TRUE);
}
/****************************************************************************/
void unformat(win, message, format, line, columns, modeline)
WINDOW *win;
MESSAGE *message;
char *format, *line;
int columns, modeline;
{
	/* Expand format into line, handling escapes */

	char *f, *start, *end, *contents;
	int len = 0, padding = 0, can_pad = TRUE;
	int printall, width;
	FORMAT_LEN *flen;
	CONVERSION *conv;

	/* Set the length structure, null message and window width */

	flen = (modeline) ? &mode_len : &hdr_len;
	printall = (modeline || message != NULL && message->text != NULL);

	/* Initialise the last space in the format */

	start = format;

	/* Copy the format string into the line */

	for (f = format; *f != '\0' && len < columns; f++) {
		if (ISCONV(*f)) {
			/* Set the end of any deferred characters */

			end = f;

			/* Get any prefix and the conversion details */

			f = get_prefix(f + 1, &width);
			conv = find_conv(*f);
			contents = get_contents(conv, win, message, printall);

			/* Set up the field and padding widths */

			width = get_width(conv, flen, width, len, columns);
			padding += get_padding(contents, width);

			/* Add any left-padding required */

			if (width > 0 && can_pad) {
				len += setfill(line + len, padding, modeline);
				padding = 0;
			}

			/* Add any deferred characters to the line */

			while (start < end && len < columns) {
				line[len++] = (printall) ? *start : ' ';
				start++;
			}
			start = f + 1;

			/* Can't pad until after the next space */

			can_pad = FALSE;

			/* Add the field to the line */

			len += setfield(line + len, contents, width);
		} else if (ISFSPACE(*f, modeline)) {
			/* We have found a space to pad on */

			can_pad = TRUE;

			/* Defer the space if not padding */

			end = (padding > 0) ? f + 1 : f;

			/* Add any deferred characters to the line */

			while (start < end && len < columns) {
				line[len++] = (printall) ? *start : ' ';
				start++;
			}
			start = end;

			/* Add any padding to the line */

			len += setfill(line + len, padding, modeline);
			padding = 0;
		}
	}

	/* Add any deferred characters to the line */

	while (start < f && len < columns) {
		line[len++] = (printall) ? *start : ' ';
		start++;
	}

	/* If writing the mode line, fill with MODE_FILL */

	while (modeline && len < columns) {
		line[len++] = MODE_FILL;
	}

	/* Terminate the line and return */

	line[len] = '\0';
	return;
}
/****************************************************************************/
char *unmessage(message, format, fold)
MESSAGE *message;
char *format;
int fold;
{
	/*
	 * Return the expanded message format in a static
	 * buffer, folding lines at column fold.  Only
	 * handles conversions based on messages. Doesn't
	 * pad any conversions unless explicitly requested,
	 * and doesn't truncate strings.
	 */

	/* The static buffer and it's size */

	static char *line = NULL;
	static int linesiz = 0;

	char *f, *contents, *space = NULL;
	int len = 0, width, fwidth = 0;
	CONVERSION *conv;

	/* Initialise the line if not already set */

	if (line == NULL) {
		line = xmalloc(LINEBUFSIZ);
		linesiz = LINEBUFSIZ;
	}
	*line = '\0';

	/* Copy the format string into the line */

	for (f = format; *f != '\0'; f++) {
		if (ISCONV(*f)) {
			/* Get any prefix and the conversion details */

			width = 0;
			f = get_prefix(f + 1, &width);
			if ((conv = find_msg_conv(*f)) == NULL) {
				/* Error in format; abort */

				emsgl("Invalid escape %",
				      strkey(*f, SK_DISPLAY),
				      " in format", NULL);
				return(NULL);
			}

			/* Now expand the conversion */

			contents = get_contents(conv, NULL, message,
						(message != NULL));

			/* Check we can hold the field */

			while (width && len + width >= linesiz || !width
			       && len + strlen(contents) >= linesiz) {
				/* Expand the line to hold the string */

				linesiz += LINEBUFSIZ;
				line = xrealloc(line, linesiz);
			}

			/* Add the field to the line */

			len += setfield(line + len, contents, width);
		} else {
			/* Check we can hold the character */

			if (len + 1 >= linesiz) {
				/* Expand the line to hold the character */

				linesiz += LINEBUFSIZ;
				line = xrealloc(line, linesiz);
			}

			/* Add the character to the line */

			line[len++] = *f;
		}
	}

	/* Now fold the line if we have to */


	for (f = line; fold > 0 && *f != '\0'; f++) {
		/* Increment the width since last fold */

		fwidth++;

		/* Do we need to fold this character */

		if (space != NULL && fwidth > fold) {
			/* Break the line at the last space */

			*space = '\n';
			space = NULL;
			fwidth = 0;
		}

		/* Save the last space if required */

		space = (isspace(*f)) ? f : space;
	}

	/* Terminate and return the line */

	line[len] = '\0';
	return(line);
}
/****************************************************************************/
static CONVERSION *find_conv(conv)
int conv;
{
	/* Return the conversion data for conv, or NULL on error */

	CONVERSION *c;

	for (c = conversions; c->func != NULL; c++) {
		/* Is this the relevant conversion? */

		if (c->conv == conv) {
			return(c);
		}
	}

	/* No such conversion */

	return(NULL);
}
/****************************************************************************/
static CONVERSION *find_msg_conv(conv)
int conv;
{
	/* Return the message-based conversion data or NULL */

	CONVERSION *c;

	for (c = conversions; c->func != NULL; c++) {
		/* Is this the relevant conversion? */

		if (c->conv == conv && c->message) {
			return(c);
		}
	}

	/* No such conversion */

	return(NULL);
}
/****************************************************************************/
static char *get_prefix(conv, width)
char *conv;
int *width;
{
	/* Parse any prefix in conv, returning the next character */

	int neg = FALSE;

	/* Initialise the width */

	*width = 0;

	/* Check for negative value */

	if (*conv == '-') {
		neg = TRUE;
		conv++;
	}

	/* Get the numeric value of the prefix */

	while (isdigit(*conv)) {
		*width *= 10;
		*width += *conv++ - '0';
	}

	/* Handle negative values and return the next character */

	*width = (neg) ? -(*width) : *width;
	return(conv);
}
/****************************************************************************/
static char *get_contents(conv, win, message, printall)
CONVERSION *conv;
WINDOW *win;
MESSAGE *message;
int printall;
{
	/* Return the contents of the conversion field */

	static char *nothing = "";

	return((printall || conv->always) ?
	       conv->func(win, message) :  nothing);
}
/****************************************************************************/
static int get_width(conv, flen, width, len, columns)
CONVERSION *conv;
FORMAT_LEN *flen;
int width, len, columns;
{
	/* Return the actual width to be used for displaying conv */

	int mod, nchar;

	/* Scale or default the field width if required */

	if (width == 0 && conv->scaled) {
		width = conv->width;
		nchar = (width < 0) ? -width : width;
		mod = ((columns - flen->length) * nchar) / flen->scaled;
		mod = (mod < -nchar / 2) ? -nchar / 2 : mod;
		width += (width < 0) ? -mod : mod;
	} else if (width == 0) {
		width = conv->width;
	}

	/* Check the width against the maximum */

	if (width < 0 && -width > columns - len) {
		width = -(columns - len);
	} else if (width > columns - len) {
		width = columns - len;
	}

	return(width);
}
/****************************************************************************/
static int get_padding(contents, width)
char *contents;
int width;
{
	/* Return the padding required for contents in width */

	int flen, clen;

	clen = strlen(contents);
	flen = (width < 0) ? -width : width;

	return((flen > clen) ? flen - clen : 0);
}
/****************************************************************************/
static int setfill(line, width, modeline)
char *line;
int width, modeline;
{
	/* Insert width fill characters into line */

	int i;

	/* Just insert the characters */

	for (i = 0; i < width; i++) {
		line[i] = (modeline) ? MODE_FILL : FILL_CHAR;
	}

	return(width);
}
/****************************************************************************/
static int setfield(line, contents, width)
char *line, *contents;
int width;
{
	/* Insert contents into a width character field at line */

	int clen, flen;

	/* Get the length of the contents and field */

	clen = strlen(contents);
	flen = (width == 0) ? strlen(contents) :
		(width < 0) ? -width : width;

	/* Copy the contents */

	(void) strncpy(line, contents, (flen < clen) ? flen : clen);

	/* Add a continuation character if required */

	if (flen < clen) {
		line[flen - 1] = CONT_CHAR;
	}

	/* Return the number of characters added */

	return((flen < clen) ? flen : clen);
}
/****************************************************************************/
static char *m_arrow(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the arrow pointer for message */

	static char *nothing = "";

	/* Return the user's arrow if active */

	return((!(cmodes(0) & M_TYPEOUT) && cwin != NULL && win == cwin
		&& message == win->point) ? get_vtext(V_ARROW) : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_chars(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the number of characters in message */

	unsigned no_chars = 0;
	MSG_TEXT *t;

	/* Count the characters in the message */

	if (message != NULL) {
		for (t = message->text; t != NULL; t = t->next) {
			no_chars += strlen(t->line) + 1;
		}
	}
	return(utos(no_chars));
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_lines(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the number of lines in the message */

	unsigned no_lines = 0;
	MSG_TEXT *t;

	/* Count the lines in the message */

	if (message != NULL) {
		for (t = message->text; t != NULL; t = t->next) {
			no_lines++;
		}
	}
	return(utos(no_lines));
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_date(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the date of the message */

	static char *nothing = "";

	/* Return the date if the message has one */

	return((message == NULL) ? nothing :
	       strqdate(message->date, get_vval(V_SHOW_LTIME), FALSE));
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_timedate(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the time and date of the message */

	static char *nothing = "";

	/* Return the time and date if the message has one */

	return((message == NULL) ? nothing :
	       strqdate(message->date, get_vval(V_SHOW_LTIME), TRUE));
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_encoding(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the encoding of the message */

	static char *nothing = "";

	/* Generate and return the encoding string */

	return((message != NULL && message->encoding != NULL)
	       ? message->encoding : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_type(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the content type and subtype of the message */

	static char *nothing = "";

	/* Generate and return the content-type string */

	return((message != NULL && message->contype != NULL)
	       ? typeonly(message->contype) : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_fulltype(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the content type and parameters of the message */

	static char *nothing = "";

	/* Generate and return the content-type string */

	return((message != NULL && message->contype != NULL)
	       ? message->contype : nothing);
}
/****************************************************************************/
static char *m_line(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the line position within the buffer */

	return(utos((cmodes(0) & M_TYPEOUT) ? to_position()
		    : position(win->buf->messages, message)));
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_name(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the name of the message's sender */

	static char *nothing = "";

	/* Return the sender if there is one */

	return((message != NULL) ? message->from : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_addr(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the address of the message's sender */

	static char *nothing = "";

	/* Return the address if there is one */

	return((message != NULL) ? message->addr : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_subject(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the subject of the message */

	static char *nothing = "";

	/* Return the subject if there is one */

	return((message != NULL) ? message->subject : nothing);
}
/****************************************************************************/
/*ARGSUSED*/
static char *m_tags(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the tags of the message */

	static char buf[MAX_SYS_TAGS + MAX_USER_TAGS + 1];

	/* Clear the buffer to start with */

	buf[0] = '\0';

	/* Check we have a message to deal with */

	if (message != NULL) {
		/* Add the system and user tags to buf */

		(void) strcpy(buf, (message->sys_tags != NULL)
			      ? message->sys_tags : "");
		(void) strcat(buf, (message->user_tags != NULL)
			      ? message->user_tags : "");
	}
	
	/* And return the tags */

	return(buf);
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_flags(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the buffer's change flags */

	static char buf[BS_LEN + 1];
	char *flag = buf;
	int in_typeout, read_only;

	/* Is the buffer typeout or read-only? */

	in_typeout = (cmodes(0) & M_TYPEOUT);
	read_only = (in_typeout || active(win->buf, M_READONLY));

	/* Set the buffer to the appropriate flags */

	if (!in_typeout && win->buf->mod) {
		*flag++ = (read_only) ? BS_READONLY : BS_CHANGED;
		*flag++ = BS_CHANGED;
	} else if (!in_typeout && win->buf->st_mod) {
		*flag++ = (read_only) ? BS_READONLY : BS_STATUS;
		*flag++ = BS_STATUS;
	} else if (in_typeout || read_only) {
		*flag++ = BS_READONLY;
		*flag++ = BS_READONLY;
	}
	*flag = '\0';

	/* Return the flags */

	return(buf);
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_name(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the name of the buffer */

	static char type_name[] = "Typeout";

	/* Return the name of typeout or the buffer */

	return((cmodes(0) & M_TYPEOUT) ? type_name : win->buf->name);
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_file(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the file associated with the buffer */

	static char *nofile = "No file";

	return(((cmodes(0) & M_TYPEOUT) || win->buf->file == NULL)
	       ? nofile : win->buf->file);
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_modes(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the modes active in the buffer */

	return(modelist(cmodes(0)));
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_lines(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the number of lines in the buffer */

	return(utos((cmodes(0) & M_TYPEOUT)
		    ? to_lines() : win->buf->no_msgs));
}
/****************************************************************************/
/*ARGSUSED*/
static char *b_pos(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the window's position in the buffer */

	static char buf[P_LEN + 1];
	unsigned top_line, nlines, wlines;

	/* Get the position of the first line on the screen */

	if (cmodes(0) & M_TYPEOUT) {
		top_line = to_position();
		nlines = to_lines();
		wlines = tlines() - 2;
	} else {
		top_line = position(win->buf->messages,
				    find_start(win->first));
		nlines = win->buf->no_msgs;
		wlines = win->bottom - win->top;
	}

	/* May be all, top, bottom or percentage of buffer */

	if ((cmodes(0) & M_TYPEOUT) && to_reading()) {
		(void) strcpy(buf, P_MOR);
	} else if (top_line == 1 && nlines < wlines) {
		(void) strcpy(buf, P_ALL);
	} else if (top_line == 1) {
		(void) strcpy(buf, P_TOP);
	} else if ((nlines - top_line) < wlines) {
		(void) strcpy(buf, P_BOT);
	} else {
		(void) strcpy(buf, utos(100 * top_line / (nlines + 1)));
		(void) strcat(buf, "%");
	}

	return(buf);
}
/****************************************************************************/
/*ARGSUSED*/
static char *p_percent(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return a percent character */

	static char *percent = "%";

	return(percent);
}
/****************************************************************************/
/*ARGSUSED*/
static char *p_version(win, message)
WINDOW *win;
MESSAGE *message;
{
	/* Return the version number of af */

	static char *version = VERSION;

	return(version);
}
/****************************************************************************/
