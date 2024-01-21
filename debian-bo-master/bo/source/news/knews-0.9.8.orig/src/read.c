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
#include "charset.h"
#include "codes.h"
#include "connect.h"
#include "decode.h"
#include "expand.h"
#include "mailcap.h"
#include "parse.h"
#include "partial.h"
#include "font.h"
#include "read.h"
#include "resource.h"
#include "save.h"
#include "server.h"
#include "tag.h"
#include "util.h"
#include "viewer.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/ArtTree.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Scrollable.h"

#define STARTS_AS_BOUNDARY(buffer, boundary, len)          \
    (boundary && buffer[0] == '-' && buffer[1] =='-' &&    \
     strncmp(buffer + 2, boundary, len) == 0)

#define IS_BOUNDARY(buffer, boundary, len)                 \
    (STARTS_AS_BOUNDARY(buffer, boundary, len) &&          \
     (buffer[len + 2] == '\0' ||                           \
      (buffer[len + 2] == '-' && buffer[len + 3] == '-' && \
       buffer[len + 4] == '\0')))

#define IS_LAST_BOUNDARY(buffer, boundary, len)            \
    (STARTS_AS_BOUNDARY(buffer, boundary, len) &&          \
     buffer[len + 2] == '-' && buffer[len + 3] == '-' &&   \
     buffer[len + 4] == '\0')

#define N_ARGS		  8
#define MIME_BUF_LEN	127

static void decode_header(char *header)
{
    EncWordData	data;
    XFontStruct	*font = NULL;
    int		append = False;
    char	*c;
    long	len;

    for (;;) {
	for (c = next_enc_word(header, &data) ; c ;
	     c = next_enc_word(c + 1, &data)) {
	    MimeFont	*f;

	    (c + 2)[data.ch_len] = '\0';
	    f = get_font(c + 2);
	    (c + 2)[data.ch_len] = '?';
	    if (f && f->header_font && (!f->funcs || f->head_enc_hack)) {
		if (font && font->fid != f->header_font->fid && append)
		    ArtTextAddLine(main_widgets.text, "    ",
				   f->header_font, global.header_pixel);
		font = f->header_font;
		break;
	    }
	}

	if (!c)
	    break;

	if (c > header) {
	    *c = '\0';
	    if (append)
		ArtTextAppendToLast(main_widgets.text, header);
	    else {
		ArtTextAddLine(main_widgets.text, header,
			       font, global.header_pixel);
		append = True;
	    }
	}

	if (data.is_qp)
	    len = decode_qp(c, data.word, data.len, NULL, True);
	else {
	    B64Context	bc = {0, };

	    len = decode_base64(&bc, c, data.word, data.len);
	    /* ignore error */
	}
	if (len < 0)
	    len = 0;
	c[len] = '\0';

	if (append)
	    ArtTextAppendToLast(main_widgets.text, c);
	else {
	    ArtTextAddLine(main_widgets.text, c, font, global.header_pixel);
	    append = True;
	}

	header = data.end;
    }

    if (!font)
	font = default_font->header_font;
    if (!font)
	font = ascii_font->header_font;

    if (append)
	ArtTextAppendToLast(main_widgets.text, header);
    else
	ArtTextAddLine(main_widgets.text, header, font, global.header_pixel);
}

static int insert_headers(char **headers, int n, int full_header)
{
    MimeFont	*f = default_font;
    char	**loop = res_header_format();
    int		added_some = False;
    int		i, hlen;

    if (loop && *loop && !full_header) {
	if (f->funcs && !f->head_enc_hack) {
	    void	*dec_data;
	    XChar2b	*wbuf;
	    long	len;

	    dec_data = f->funcs->init();

	    do {
		hlen = strlen(*loop);
		for (i = 0 ; i < n ; i++)
		    if (case_strncmp(*loop, headers[i], hlen) == 0)
			break;

		if (i == n)
		    continue;

		do {
		    len = f->funcs->decode(dec_data, headers[i],
					   strlen(headers[i]), True, &wbuf);
		    if (len >= 0) {
			ArtTextAddWLine(main_widgets.text, wbuf, len,
					f->header_font, global.header_pixel);
			added_some = True;
		    }
		} while (++i < n && IS_SPACE(headers[i][0]));
	    } while (*++loop);

	    f->funcs->end(dec_data);
	} else {
	    do {
		hlen = strlen(*loop);
		for (i = 0 ; i < n ; i++)
		    if (case_strncmp(*loop, headers[i], hlen) == 0)
			break;

		if (i == n)
		    continue;

		if (isupper((unsigned char)**loop))
		    do
			decode_header(headers[i]);
		    while (++i < n && IS_SPACE(headers[i][0]));
		else
		    do
			ArtTextAddLine(main_widgets.text, headers[i],
				       f->header_font, global.header_pixel);
		    while (++i < n && IS_SPACE(headers[i][0]));
		added_some = True;
	    } while (*++loop);
	}
    } else if (f->funcs && !f->head_enc_hack) {
	void	*dec_data;
	XChar2b	*wbuf;
	long	len;

	dec_data = f->funcs->init();

	for (i = 0 ; i < n ; i++) {
	    len = f->funcs->decode(dec_data, headers[i],
				   strlen(headers[i]),
				   True, &wbuf);
	    if (len >= 0) {
		ArtTextAddWLine(main_widgets.text, wbuf, len,
				f->header_font, global.header_pixel);
		added_some = True;
	    }
	}

	f->funcs->end(dec_data);
    } else {
	if (n > 0)
	    added_some = True;

	for (i = 0 ; i < n ; i++)
	    ArtTextAddLine(main_widgets.text, headers[i],
			   f->header_font, global.header_pixel);
    }

    if (added_some)
	if (!f->funcs || f->head_enc_hack)
	    ArtTextAddLine(main_widgets.text, "",
			   f->header_font, global.header_pixel);
	else {
	    XChar2b	tmp;

	    ArtTextAddWLine(main_widgets.text, &tmp, 0,
			    f->header_font, global.header_pixel);
	}

    return added_some;
}

static char *get_mime_body(SERVER	 *server,
			   char		 *buffer,
			   char		**datap,
			   long		 *lenp,
			   int		  enc,
			   char		 *bound,
			   long		  bound_len)
{
    long	alloced  = 65536;
    char	*data = XtMalloc(alloced);
    long	data_len = 0;
    long	n;
    int		tmp, soft;

#define REALLOC(data, alloced, data_len, n)        \
    if (n + data_len + 8 >= alloced) {             \
        alloced = 2 * alloced + n + data_len + 8;  \
        data = XtRealloc(data, alloced);           \
    }

    switch (enc) {
    default:
	ArtTextAddLine(main_widgets.text,
		       "[knews: unrecoginzed Content-Transfer-Encoding.]",
		       ascii_font->body_font, global.alert_pixel);
	/*
	 *  Fall through.
	 */
    case MimeEncNone:
	while (buffer && !IS_DOT(buffer) &&
	       !IS_BOUNDARY(buffer, bound, bound_len)) {
	    if (*buffer == '.')
		buffer++;
	    n = strlen(buffer);
	    buffer[n++] = '\n';
	    REALLOC(data, alloced, data_len, n);
	    memcpy(data + data_len, buffer, n);
	    buffer = server_read(server);
	}
	break;
    case MimeEncBase64:
	{
	    B64Context	bc = {0, };

	    while (buffer && !IS_DOT(buffer) &&
                   !IS_BOUNDARY(buffer, bound, bound_len)) {
                n = strlen(buffer);
                REALLOC(data, alloced, data_len, n);
                n = decode_base64(&bc, data + data_len, buffer, n);
                if (n > 0)
                    data_len += n;
                buffer = server_read(server);
            }

            n = decode_base64(&bc, data + data_len, NULL, 0);
            if (n >= 0)
                data_len += n;

            tmp = base64_status(&bc);
            if (tmp & BASE64_ERROR)
                ArtTextAddLine(main_widgets.text,
                               "[knews: base64 decode error.]",
                               ascii_font->body_font, global.alert_pixel);
            if (tmp & BASE64_TRAILING_GARBAGE)
                ArtTextAddLine(main_widgets.text,
                               "[knews: trailing garbage in base64 data.]",
                               ascii_font->body_font, global.alert_pixel);
	}
	break;
    case MimeEncQP:
	while (buffer && !IS_DOT(buffer) &&
	       !IS_BOUNDARY(buffer, bound, bound_len)) {
	    if (*buffer == '.')
		buffer++;
	    n = strlen(buffer);
	    REALLOC(data, alloced, data_len, n);
	    data_len += decode_qp(data + data_len, buffer, n, &soft, False);
	    if (!soft)
		data[data_len++] = '\n';
	    buffer = server_read(server);
	}
	break;
    case MimeEncUue:
	{
	    UueContext	uc = {0, };

            while (buffer && !IS_DOT(buffer) &&
                   !IS_BOUNDARY(buffer, bound, bound_len)) {
		n = strlen(buffer);
		REALLOC(data, alloced, data_len, n);
                n = decode_uue(&uc, data + data_len, buffer, n);
		if (n > 0)
                    data_len += n;
                buffer = server_read(server);
            }

            tmp = uue_status(&uc);
            if (tmp & UUE_NO_BEGIN)
                ArtTextAddLine(main_widgets.text,
                               "[knews: uudecode error: no begin line]",
                               ascii_font->body_font, global.alert_pixel);
            else if (tmp & UUE_NO_END)
                ArtTextAddLine(main_widgets.text,
                               "[knews: uudecode error: no end line]",
                               ascii_font->body_font, global.alert_pixel);
            else if (tmp & UUE_ERROR)
                ArtTextAddLine(main_widgets.text,
                               "[knews: uudecode error]",
                               ascii_font->body_font, global.alert_pixel);
	}
	break;
    }
#undef REALLOC

    if (!buffer) {
	XtFree(data);
	return NULL;
    }

    *datap = data;
    *lenp  = data_len;

    return buffer;
}
static void add_body_line(char *buffer, regex_t *re,
			  MimeFont *f, void *dec_data, int is_lf_term)
{
    if (!f->funcs)
	if (re && regexec(re, buffer, 0, NULL, 0) == 0)
	    ArtTextAddLine(main_widgets.text, buffer,
			   f->quote_font, global.quote_pixel);
	else
	    ArtTextAddLine(main_widgets.text, buffer,
			   f->body_font, global.pixel);
    else {
	XFontStruct	*font;
	XChar2b		*wbuf;
	long		len;
	Pixel		pixel;

	if (re && regexec(re, buffer, 0, NULL, 0) == 0) {
	    font = f->quote_font;
	    pixel = global.quote_pixel;
	} else {
	    font = f->body_font;
	    pixel = global.pixel;
	}

	len = f->funcs->decode(dec_data, buffer, strlen(buffer),
			       is_lf_term, &wbuf);
	if (len >= 0)
	    ArtTextAddWLine(main_widgets.text, wbuf, len, font, pixel);
    }
}

static void add_body_line_multi(char *buffer, regex_t *re,
				MimeFont *f, void *dec_data, int append)
{
    do {
	char	*c = strchr(buffer, '\n');

	if (c)
	    if (c > buffer && c[-1] == '\r')
		c[-1] = '\0';
	    else
		c[0] = '\0';

	if (!append)
	    add_body_line(buffer, re, f, dec_data, c != NULL);
	else if (!f->funcs)
	    ArtTextAppendToLast(main_widgets.text, buffer);
	else {
	    XChar2b	*wbuf;
	    long	len;

	    len = f->funcs->decode(dec_data, buffer, strlen(buffer),
				   c != NULL, &wbuf);
	    if (len >= 0)
		ArtTextWAppendToLast(main_widgets.text, wbuf, len);
	}
	append = False;

	buffer = c;
	if (buffer)
	    buffer++;
    } while (buffer);
}

static char *insert_body(SERVER *server, char *buffer, MimeFont *f,
			 int enc, char *bound, int len)
{
    regex_t	*re = res_quote_regexp();
    long	n;
    int		soft, append, tmp;
    void	*dec_data = NULL;

    if (f->funcs)
	dec_data = f->funcs->init();

    switch (enc) {
    default:
	ArtTextAddLine(main_widgets.text,
		       "[knews: unrecoginzed Content-Transfer-Encoding.]",
		       ascii_font->body_font, global.alert_pixel);
	/*
	 * Fall through
	 */
    case MimeEncNone:
	while (buffer && !IS_DOT(buffer) &&
	       !IS_BOUNDARY(buffer, bound, len)) {
	    add_body_line(*buffer == '.' ? buffer + 1 : buffer,
			  re, f, dec_data, True);
	    buffer = server_read(server);
	}
	break;
    case MimeEncQP:
	append = False;
	while (buffer && !IS_DOT(buffer) &&
	       !IS_BOUNDARY(buffer, bound, len)) {
	    n = decode_qp(buffer, buffer, strlen(buffer), &soft, False);
	    if (n == 0)
		append = soft;
	    else if (n > 0) {
		buffer[n] = '\0';
		add_body_line_multi(buffer, re, f, dec_data, append);
		append = soft;
	    }
	    buffer = server_read(server);
	}
	break;
    case MimeEncBase64:
	{
	    B64Context		bc = {0, };
	    char		*dest = NULL;
	    long		dest_len = 0;

	    append = False;
	    while (buffer && !IS_DOT(buffer) &&
		   !IS_BOUNDARY(buffer, bound, len)) {
		n = strlen(buffer);
		if (n + 8 > dest_len) {
		    dest_len = n + 8;
		    dest = XtRealloc(dest, dest_len);
		}
		n = decode_base64(&bc, dest, buffer, n);
		if (n > 0) {
		    dest[n] = '\0';
		    add_body_line_multi(dest, re, f, dec_data, append);
		    append = True;
		}
		buffer = server_read(server);
	    }

	    n = decode_base64(&bc, dest, NULL, 0);
	    if (n > 0) {
		dest[n] = '\0';
		add_body_line_multi(dest, re, f, dec_data, append);
	    }

	    XtFree(dest);

	    tmp = base64_status(&bc);
	    if (tmp & BASE64_ERROR)
		ArtTextAddLine(main_widgets.text,
			       "[knews: base64 decode error.]",
			       ascii_font->body_font, global.alert_pixel);
	    if (tmp & BASE64_TRAILING_GARBAGE)
		ArtTextAddLine(main_widgets.text,
			       "[knews: trailing garbage in base64 data.]",
			       ascii_font->body_font, global.alert_pixel);
	}
	break;
    case MimeEncUue:
	{
	    UueContext	uc = {0, };

	    append = False;
	    while (buffer && !IS_DOT(buffer) &&
		   !IS_BOUNDARY(buffer, bound, len)) {
		n = decode_uue(&uc, buffer, buffer, strlen(buffer));
		if (n > 0) {
		    add_body_line_multi(buffer, re, f, dec_data, append);
		    append = True;
		}
		buffer = server_read(server);
	    }

	    tmp = uue_status(&uc);
	    if (tmp & UUE_NO_BEGIN)
		ArtTextAddLine(main_widgets.text,
			       "[knews: uudecode error: no begin line]",
			       ascii_font->body_font, global.alert_pixel);
	    else if (tmp & UUE_NO_END)
		ArtTextAddLine(main_widgets.text,
			       "[knews: uudecode error: no end line]",
			       ascii_font->body_font, global.alert_pixel);
	    else if (tmp & UUE_ERROR)
		ArtTextAddLine(main_widgets.text, "[knews: uudecode error]",
			       ascii_font->body_font, global.alert_pixel);
	}
	break;
    }

    if (f->funcs)
	f->funcs->end(dec_data);

    return buffer;
}

static char *do_multipart(SERVER *server, char *buffer, char *bound,
			  int default_to_rfc822, int do_line,
			  char *ext_bound, int ext_len)			  
{
    int	n;

    n = strlen(bound);

    while (buffer && !IS_DOT(buffer) &&
	   !IS_BOUNDARY(buffer, bound, n) &&
	   !IS_BOUNDARY(buffer, ext_bound, ext_len))
	buffer = server_read(server);

    if (buffer)
	if (IS_DOT(buffer))
	    ArtTextAddLine(main_widgets.text,
			   "[knews: premature EOF in multipart.]",
			   ascii_font->body_font, global.alert_pixel);
	else if (IS_BOUNDARY(buffer, ext_bound, ext_len))
	    ArtTextAddLine(main_widgets.text,
			   "[knews: outer boundary in nested multipart.]",
			   ascii_font->body_font, global.alert_pixel);
	else if (IS_LAST_BOUNDARY(buffer, bound, n))
	    ArtTextAddLine(main_widgets.text,
			   "[knews: empty multipart.]",
			   ascii_font->body_font, global.alert_pixel);
	else {
	    do {
		if (do_line)
		    ArtTextAddSeparator(main_widgets.text, 2, 4);
		do_line = True;
		buffer = do_mime(NULL, server, server_read(server),
				 default_to_rfc822, bound, n, NULL);
	    } while (buffer && !IS_DOT(buffer) &&
		     !IS_LAST_BOUNDARY(buffer, bound, n));

	    if (!buffer)
		return NULL;

	    if (IS_DOT(buffer))
		ArtTextAddLine(main_widgets.text,
			       "[knews: premature EOF in multipart.]",
			       ascii_font->body_font, global.alert_pixel);

	    while (buffer && !IS_DOT(buffer) &&
		   !IS_BOUNDARY(buffer, ext_bound, ext_len))
		buffer = server_read(server);
	}

    return buffer;
}

char *do_mime(ARTICLE    *art,
	      SERVER     *server,
	      char       *buffer,
	      int         default_to_rfc822,
	      char       *ext_bound,
	      int         ext_len,
	      char      **mime_hack)
{
    int		enc = MimeEncNone;
    char	**headers;
    char	type_buf[MIME_BUF_LEN + 1], subtype_buf[MIME_BUF_LEN + 1];
    MimeArg	args[N_ARGS + 1] = {{0, },};
    int		no_alloc, n, i, ct_index, cd_index;

    n = 0;
    no_alloc = 16;
    headers = (char **)XtMalloc(no_alloc * sizeof(char *));
    for (i = 0 ; i < no_alloc ; i++)
	headers[i] = NULL;
    while (buffer && buffer[0] != '\0' &&
	   !IS_DOT(buffer) && !IS_BOUNDARY(buffer, ext_bound, ext_len)) {
	if (n + 3 > no_alloc) {
	    i = no_alloc;
	    no_alloc *= 2;
	    headers = (char **)XtRealloc((char *)headers,
					 no_alloc * sizeof(char *));
	    while (i < no_alloc)
		headers[i++] = NULL;
	}

	headers[n] = XtNewString(buffer);
	n++;
	buffer = server_read(server);
    }

    ct_index = -1;
    cd_index = -1;
    for (i = 0 ; i < n ; i++)
	if (case_lstrncmp(headers[i], "content-", 8) == 0)
	    if (case_lstrncmp(headers[i] + 8, "type:", 5) == 0)
		ct_index = i;
	    else if (case_lstrncmp(headers[i] + 8,
				   "transfer-encoding:", 18) == 0)
		enc = parse_content_enc(headers + i);
	    else if (case_lstrncmp(headers[i] + 8, "disposition:", 12)== 0)
		cd_index = i;

    if (mime_hack)
	enc = parse_content_enc(mime_hack + 2);

    if (buffer && buffer[0] == '\0')
	buffer = server_read(server);

    if (!ext_bound) { /* top level */
	if (art && !art->from)
	    realize_fake(art, headers, n);
	if (art && !art->xref && !art->read && res_process_xrefs())
	    fake_xref(art, headers, n);
	ArtTextClearLines(main_widgets.text);
	ScrollableSuspend(main_widgets.text);
    }

    if (mime_hack) {
	if (parse_content_type(mime_hack, type_buf, sizeof type_buf,
			       subtype_buf, sizeof subtype_buf,
			       args, N_ARGS, False))
	    ct_index = 0;
    } else if (ct_index >= 0) {
	if (!parse_content_type(headers + ct_index, type_buf, sizeof type_buf,
				subtype_buf, sizeof subtype_buf,
				args, N_ARGS, False))
	    ct_index = -1;
    } else if (default_to_rfc822) {
	strcpy(type_buf, "message");
	strcpy(subtype_buf, "rfc822");
	ct_index = 0; /* hack */
    }

    if (ct_index < 0) {
	if (buffer) {
	    insert_headers(headers, n, False);
	    buffer = insert_body(server, buffer, default_font,
				 MimeEncNone, ext_bound, ext_len);
	}
    } else do { /* one pass loop to continue out of */
	const MailcapData	*mailcap;
	int		do_line;
	char		*charset, *bound;
	MimeFont	*f;

	ascii_lower(type_buf);
	ascii_lower(subtype_buf);

	do_line = insert_headers(headers, n, False);
	mailcap = mailcap_lookup(type_buf, subtype_buf);

	switch (type_buf[0]) {
	case 't':
	    if (strcmp(type_buf, "text") != 0)
		break;
	    if (strcmp(subtype_buf, "plain") != 0) {
		if (mailcap)
		    break;
		ArtTextAddLine(main_widgets.text,
			       "[knews: no mailcap viewer for 'text/",
			       ascii_font->body_font, global.alert_pixel);
		ArtTextAppendToLast(main_widgets.text, subtype_buf);
		ArtTextAppendToLast(main_widgets.text, "'.]");
	    }

	    charset = get_charset(args);
	    if (!charset)
		f = default_font;
	    else {
		f = get_font(charset);
		if (!f) {
		    ArtTextAddLine(main_widgets.text,
				   "[knews: no font for charset '",
				   ascii_font->body_font, global.alert_pixel);
		    ArtTextAppendToLast(main_widgets.text, charset);
		    ArtTextAppendToLast(main_widgets.text, "'.]");
		    if (default_font->funcs)
			f = ascii_font;
		    else
			f = default_font;
		}
	    }

	    buffer = insert_body(server, buffer, f, enc, ext_bound, ext_len);
	    continue;
	case 'm':
	    if (strcmp(type_buf, "multipart") == 0) {
		if (mailcap &&
		    strcmp(subtype_buf, "mixed") != 0 &&
		    strcmp(subtype_buf, "digest") != 0)
		    break;

		/*
		 * treat all subtypes as multipart/mixed,
		 * alternative is just too much trouble
		 */

		bound = NULL;
		for (i = 0 ; args[i].value ; i++)
		    if (case_lstrcmp(args[i].name, "boundary") == 0) {
			bound = args[i].value;
			break;
		    }

		if (bound) {
		    buffer =
			do_multipart(server, buffer, bound,
				     strcmp(subtype_buf, "digest") == 0,
				     do_line, ext_bound, ext_len);
		} else { /* no boundary, do raw text */
		    ArtTextAddLine(main_widgets.text,
				   "[knews: multipart/* without a "
				   "boundary parameter.]",
				   ascii_font->body_font,
				   global.alert_pixel);
		    buffer = insert_body(server, buffer, default_font,
					 enc, ext_bound, ext_len);
		}
		continue;
	    } else if (strcmp(type_buf, "message") == 0) {
		if (strcmp(subtype_buf, "rfc822") == 0) {
		    buffer = do_mime(art, server, buffer,
				     False, ext_bound, ext_len, NULL);
		    continue;
		} else if (strcmp(subtype_buf, "partial") == 0) {
		    buffer = insert_body(server, buffer,
					 default_font, MimeEncNone,
					 ext_bound, ext_len);
		    if (art && res_assemble_partials())
			partial_cache_hook(art->no, args, True);
		    continue;
		} else if (!mailcap &&
			   strcmp(subtype_buf, "external-body") == 0) {
		    ArtTextAddLine(main_widgets.text,
				   "[knews: no mailcap entry for "
				   "Content-Type: message/external-body.]",
				   ascii_font->body_font, global.alert_pixel);
		    buffer = insert_body(server, buffer,
					 default_font, MimeEncNone,
					 ext_bound, ext_len);
		    continue;
		}
	    }
	    break;
	}

	{
	    char	*view_cmd  = NULL;
	    char	*file_name = NULL;
	    char	*data;
	    long	data_len;

	    if (mailcap && mailcap->view_command)
		view_cmd = expand_view_command(mailcap->view_command,
					       type_buf, subtype_buf, args,
					       mailcap->needsterminal,
					       mailcap->copiousoutput);
	    if (cd_index >= 0 &&
		(file_name = parse_content_disp(headers + cd_index)))
		file_name = XtNewString(file_name);

	    if (!mime_hack) {
		i = ct_index;
		do {
		    ArtTextAddLine(main_widgets.text, headers[i],
				   ascii_font->header_font,
				   global.header_pixel);
		} while (headers[++i] && IS_SPACE(headers[i][0]));
		ArtTextAddLine(main_widgets.text, "",
			       ascii_font->body_font, global.pixel);
	    }

	    if (mailcap && mailcap->description) {
		ArtTextAddLine(main_widgets.text,
			       "[mailcap description: ",
			       ascii_font->header_font,
			       global.header_pixel);
		ArtTextAppendToLast(main_widgets.text,
				    mailcap->description);
		ArtTextAppendToLast(main_widgets.text, "]");
	    }

	    buffer = get_mime_body(server, buffer, &data, &data_len,
				   enc, ext_bound, ext_len);
	    if (buffer)
		do_viewer(type_buf, subtype_buf, view_cmd,
			  file_name, data, data_len);
	}
    } while (0);

    if (!ext_bound) /* toplevel */
	ScrollableResume(main_widgets.text);

    for (i = 0 ; i < N_ARGS ; i++) {
	XtFree(args[i].name);
	XtFree(args[i].value);
    }

    for (i = 0 ; i < no_alloc ; i++)
	XtFree(headers[i]);
    XtFree((char *)headers);

    return buffer;    
}

static char *get_article(ARTICLE  *art,
			 int       full_header,
			 long     *sel_data,
			 char    **mime_hack)
{
    SERVER	*server = main_server;
    FILE	*cf = NULL;
    char	command[256];
    char	*buffer;

    if (art->no > 0) {
	server = cache_get_server(art->no, True);
	if (server)
	    buffer = CODE_TO_STR(NNTP_OK_ARTICLE);
	else {
	    cf = cache_get_file(art->no);
	    server = main_server;
	    sprintf(command, "ARTICLE %ld\r\n", art->no);
	    buffer = server_comm(server, command, True);
	}
    } else if (art->hash_len < 240) {
	sprintf(command, "ARTICLE <%s>\r\n", art->msgid);
	buffer = server_comm(server, command, True);
    } else {
	char	*temp = XtMalloc(art->hash_len + 16);

	sprintf(temp, "ARTICLE <%s>\r\n", art->msgid);
	buffer = server_comm(server, temp, True);
	XtFree(temp);
    }

    if (!buffer || atoi(buffer) != NNTP_OK_ARTICLE) {
	if (cf) {
	    fclose(cf);
	    cache_fetch_failed(art->no);
	}
	ArtTextClearLines(main_widgets.text);
	if (server == main_server)
	    return buffer;

	server_free(server);
	return CODE_TO_STR(NNTP_OK_ARTICLE);
    }

    if (art->lines > 0)
	ArtTextAllocLines(main_widgets.text, art->lines + 32);

    if (cf)
	server_set_bs(server, cf);
    buffer = server_read(server);

    if (!full_header)
	buffer = do_mime(art, server, buffer, False, NULL, 0, mime_hack);
    else {
	long	i;

	ArtTextClearLines(main_widgets.text);
	ScrollableSuspend(main_widgets.text);

	i = -1;
	while (buffer && buffer[0] != '\0' && !IS_DOT(buffer)) {
	    if (sel_data && sel_data[0] == i)
		ArtTextAddSelected(main_widgets.text, buffer,
				   ascii_font->header_font,
				   global.header_pixel,
				   sel_data[1], sel_data[2]);
	    else
		ArtTextAddLine(main_widgets.text, buffer,
			       ascii_font->header_font, global.header_pixel);
	    i--;
	    buffer = server_read(server);
	}

	if (buffer && buffer[0] == '\0') {
	    regex_t	*re = res_quote_regexp();

	    ArtTextAddLine(main_widgets.text, "",
			   ascii_font->body_font, global.pixel);

	    buffer = server_read(server);
	    i = 0;
	    while (buffer && !IS_DOT(buffer)) {
		XFontStruct	*font;
		Pixel		pixel;

		if (re && regexec(re, buffer, 0, NULL, 0) == 0) {
		    font = ascii_font->quote_font;
		    pixel = global.quote_pixel;
		} else {
		    font = ascii_font->body_font;
		    pixel = global.pixel;
		}

		if (sel_data && i == sel_data[0])
		    ArtTextAddSelected(main_widgets.text, buffer,
				       font, pixel, sel_data[1], sel_data[2]);
		else
		    ArtTextAddLine(main_widgets.text, buffer, font, pixel);
		buffer = server_read(server);
		i++;
	    }
	}
    }

    ScrollableResume(main_widgets.text);

    if (cf) {
	server_set_bs(server, NULL);
	if (fclose(cf) == 0 && buffer)
	    cache_fetch_done(art->no);
	else
	    cache_fetch_failed(art->no);
    }

    if (server != main_server)
	server_free(server);
    else if (!buffer)
	return NULL;
    
    if (art->from && !art->read) {
	art->read = True;
	art->subject->no_unread--;
	global.curr_group->no_unread--;

	if (global.mode == NewsModeThread) {
	    ArtTreeNodeSetInner(main_widgets.arttree,
				(ART_TREE_NODE *)art, False);
	    ArtTreeNodeSetPixmap(main_widgets.arttree,
				 (ART_TREE_NODE *)art, None);
	}
	if (art->pixmap != None) {
	    global.n_hot--;
	    update_subj_hot_value(art->subject);
	}
	update_subj_entry(art->subject);

	if (res_process_xrefs())
	    process_xref(art);
    }

    return CODE_TO_STR(NNTP_OK_ARTICLE);
}

int read_article(ARTICLE  *art,
		 int       full_header,
		 long     *sel_data,
		 char    **mime_hack)
{
    char	*reply;

    if (art) {
	set_busy(True);
	reply = get_article(art, full_header || res_full_header(),
			    sel_data, mime_hack);
	if (!reply) {
	    reconnect_server(True);
	    unset_busy();
	    return False;
	}
	unset_busy();

	if (atoi(reply) == NNTP_OK_ARTICLE) {
	    set_standard_message();
	    if (history_peek() != art)
		history_push(art);
	} else {
	    char	message[256];

	    if (strlen(reply) > 200)
		reply[200] ='\0';
	    sprintf(message, "Error!  Message from server is: %s", reply);
	    set_message(message, True);
	}
    } else {
	ArtTextClearLines(main_widgets.text);
	set_standard_message();
    }

    return True;
}

/*********************************************************************/

void action_read_article(Widget    w,
			 XEvent   *event,
			 String   *params,
			 Cardinal *no_params)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (global.curr_art)
	read_article(global.curr_art, *no_params != 0, NULL, NULL);
}

void action_mime_hack(Widget    w,
		      XEvent   *event,
		      String   *params,
		      Cardinal *no_params)
{
    char	*mime_hack[4];
    char	*ct, *cte;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!global.curr_art) {
	set_message("No selected article!", True);
	return;
    } else if (*no_params != 2) {
	set_message("Wrong number of parameters to mime-hack()!", True);
	return;
    }

    ct  = XtMalloc(16 + strlen(params[0]));
    cte = XtMalloc(32 + strlen(params[1]));
    sprintf(ct,  "Content-Type: %s",             params[0]);
    sprintf(cte, "Content-Transfer-Ecoding: %s", params[1]);
    mime_hack[0] = ct;
    mime_hack[1] = NULL;
    mime_hack[2] = cte;
    mime_hack[3] = NULL;

    read_article(global.curr_art, False, NULL, mime_hack);

    XtFree(ct);
    XtFree(cte);
}
