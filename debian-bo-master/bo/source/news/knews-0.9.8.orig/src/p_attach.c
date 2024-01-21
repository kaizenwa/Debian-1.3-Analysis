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
#include "decode.h"
#include "expand.h"
#include "file.h"
#include "font.h"
#include "mailcap.h"
#include "p_I.h"
#include "p_attach.h"
#include "parse.h"
#include "util.h"
#include "xutil.h"

struct PostAttachment {
    char		*file_name;
    char		*type;
    char		*name;
    char		*descr;
    unsigned char	is_inline;
    unsigned char	enc;
    unsigned char	needs_enc;
    unsigned char	has_8bit;
};

static const char	*get_mime_type(const char*);

static int check_file(PostAttachment *pa, char *message)
{
    FILE	*fp;
    long	n_ascii = 0;
    long	n_ctrl  = 0;
    long	n_8bit  = 0;
    int		ch;
    const char	*c;

    fp = fopen(pa->file_name, "r");
    if (!fp) {
	perror(pa->file_name);
	strcpy(message, "Couldn't open file!");
	return False;
    }

    while ((ch = getc(fp)) != EOF)
	if (ch >= 128 + 32)
	    n_8bit++;
	else if (ch >= 128)
	    n_ctrl++;
	else if (ch >= 32)
	    n_ascii++;
	else
	    switch (ch) {
	    case '\t': /*   9 */
	    case '\n': /*  10 */
	    case '\f': /*  12 */
	    case '\r': /*  13 */
	    case 14:   /*  SO */
	    case 15:   /*  SI */
	    case 27:   /* ESC */
		n_ascii++;
		break;
	    default:
		n_ctrl++;
		break;
	    }

    fclose(fp);

    c = strrchr(pa->file_name, '.');
    if (c) {
	c = get_mime_type(c + 1);
	if (c)
	    pa->type = XtNewString(c);
    }

    if (n_ctrl > 0) {  /* binary file */
	pa->enc = MimeEncBase64;
	pa->needs_enc = True;
	pa->has_8bit = True;
	if (!pa->type)
	    strcpy(message, "Couldn't guess content-type type.");
    } else {
	pa->enc = n_8bit > n_ascii ? MimeEncBase64 : MimeEncNone;
	pa->needs_enc = False;
	pa->has_8bit = n_8bit > 0;
	if (!pa->type) {
	    pa->type = XtNewString("text/plain");
	    strcpy(message, "Maybe text/plain?  "
		   "Please give a charset parameter.");
	}
    }

    return True;
}

/*************************************************************************/

static int enc_file_none(PostAttachment *pa, FILE *fin, FILE *fout)
{
    int	c, bol = True;

    while ((c = getc(fin)) != EOF) {
	if (c == '\n')
	    putc('\r', fout);
	else if (bol && c == '.')
	    putc('.', fout);
	putc(c, fout);
	bol = c == '\n';
    }

    if (!bol)
	fputs("\r\n", fout);

    return True;
}

static int enc_file_base64(PostAttachment *pa, FILE *fin, FILE *fout)
{
    unsigned char	buf[4];
    unsigned long	acc;
    int			n, pos = 0;

    while ((n = fread(buf, 1, 3, fin)) == 3) {
	acc = (buf[0] << 16) | (buf[1] << 8) | buf[2];
	buf[0] = base64_alpha[(acc >> 18) & 0x3fu];
	buf[1] = base64_alpha[(acc >> 12) & 0x3fu];
	buf[2] = base64_alpha[(acc >>  6) & 0x3fu];
	buf[3] = base64_alpha[(acc      ) & 0x3fu];
	fwrite(buf, 1, 4, fout);
	pos += 4;
	if (pos >= 76) {
	    fwrite("\r\n", 1, 2, fout);
	    pos = 0;
	}
    }

    switch (n) {
    case 1:
	acc = buf[0] << 16;
	buf[0] = base64_alpha[(acc >> 18) & 0x3ful];
	buf[1] = base64_alpha[(acc >> 12) & 0x3ful];
	buf[2] = '=';
	buf[3] = '=';
	fwrite(buf, 1, 4, fout);
	break;
    case 2:
	acc = (buf[0] << 16) | (buf[1] << 8);
	buf[0] = base64_alpha[(acc >> 18) & 0x3fu];
	buf[1] = base64_alpha[(acc >> 12) & 0x3fu];
	buf[2] = base64_alpha[(acc >>  6) & 0x3fu];
	buf[3] = '=';
	fwrite(buf, 1, 4, fout);
	break;
    }

    if (pos != 0)
	fwrite("\r\n", 1, 2, fout);
    fwrite("\r\n", 1, 2, fout);

    return True;
}

static int enc_file_uue(PostAttachment *pa, FILE *fin, FILE *fout)
{
    char	*name = pa->name;
    char	buf[45];
    int		n;

    if (!name) {
	name = strrchr(pa->file_name, '/');
	if (name)
	    name++;
	else
	    name = pa->file_name;
    }

    fprintf(fout, "begin 0644 %s\r\n", name);

#if 0
#  define UU_CHAR(a)  ((unsigned char)(' ' + ((a) & 0x3fu)))
#else
#  define UU_CHAR(a)  ((unsigned char)(' ' + 1 + (((a) - 1) & 0x3fu)))
#endif

    while ((n = fread(buf, 1, sizeof buf, fin)) > 0) {
	unsigned long	acc;
	unsigned char	*c = (unsigned char *)buf;

	if (' ' + n == '.')
	    putc('.', fout);
	putc(' ' + n, fout);
	while (n > 2) {
	    acc = (c[0] << 16) | (c[1] << 8) | c[2];
	    fprintf(fout, "%c%c%c%c",
		    UU_CHAR(acc >> 18), UU_CHAR(acc >> 12),
		    UU_CHAR(acc >>  6), UU_CHAR(acc      ));
	    /*
	      (unsigned char)(' ' + ((acc >> 18) & 0x3fu)),
	      (unsigned char)(' ' + ((acc >> 12) & 0x3fu)),
	      (unsigned char)(' ' + ((acc >>  6) & 0x3fu)),
	      (unsigned char)(' ' + ((acc      ) & 0x3fu)));
	      */
	    n -= 3;
	    c += 3;
	}

	switch (n) {
	case 1:
	    acc = c[0] << 16;
	    fprintf(fout, "%c%c", UU_CHAR(acc >> 18), UU_CHAR(acc >> 12));
	    /*
	      (unsigned char)(' ' + ((acc >> 18) & 0x3fu)),
	      (unsigned char)(' ' + ((acc >> 12) & 0x3fu)));
	      */
	    break;
	case 2:
	    acc = (c[0] << 16) | (c[1] << 8);
	    fprintf(fout, "%c%c%c",
		    UU_CHAR(acc >> 18), UU_CHAR(acc >> 12), UU_CHAR(acc >> 6));
	    /*
	      (unsigned char)(' ' + ((acc >> 18) & 0x3fu)),
	      (unsigned char)(' ' + ((acc >> 12) & 0x3fu)),
	      (unsigned char)(' ' + ((acc >>  6) & 0x3fu)));
	      */
	    break;
	}

	fputs("\r\n", fout);
    }

    fputs("`\r\nend\r\n\r\n", fout);

    return True;
}

static int enc_file_qp(PostAttachment *pa, FILE *fin, FILE *fout)
{
    static const unsigned char	hex_char[16] = "0123456789ABCDEF";
    int		ch, next, do_enc, col;

    next = getc(fin);
    col = 0;

    while ((ch = next) != EOF) {
	next = getc(fin);

	if (ch == '\n') {
	    fputs("\r\n", fout);
	    col = 0;
	    continue;
	}

	if (col > 68) {
	    fputs("=\r\n", fout);
	    col = 0;
	}

	if (ch < 32 || ch > 126)
	    do_enc = True;
	else
	    switch (ch) {
	    case ' ':
		do_enc = next == '\n' || next == EOF;
		break;
	    case '.':
	    case 'F':
		do_enc = col == 0;
		break;
	    case '=':
		do_enc = True;
		break;
	    default:
		do_enc = False;
		break;
	    }

	if (!do_enc) {
	    putc(ch, fout);
	    col++;
	} else {
	    putc('=', fout);
	    putc(hex_char[(ch >> 4) & 0xfu], fout);
	    putc(hex_char[(ch     ) & 0xfu], fout);
	    col += 3;
	}
    }

    fputs("\r\n\r\n" + (col == 0 ? 2 : 0), fout);

    return True;
}

/*************************************************************************/

void free_attachment(PostAttachment *pa)
{
    XtFree(pa->file_name);
    XtFree(pa->type);
    XtFree(pa->name);
    XtFree(pa->descr);
    pa->file_name = NULL;
    pa->type = NULL;
    pa->name = NULL;
    pa->descr = NULL;
    XtFree((char *)pa);
}

PostAttachment *create_attachment(char *file_name, char *message)
{
    PostAttachment	*pa;
    char		*c;

    message[0] = '\0';
    file_name = expand_path(file_name);
    if (!file_name) {
	strcpy(message, "File name error!");
	return NULL;
    }

    pa = (PostAttachment *)XtMalloc(sizeof *pa);
    pa->file_name = file_name;
    pa->type = NULL;
    pa->name = NULL;
    pa->descr = NULL;
    pa->is_inline = False;
    pa->enc = -1;
    pa->needs_enc = False;
    pa->has_8bit = False;

    if (!check_file(pa, message)) {
	free_attachment(pa);
	return NULL;
    }

    c = strrchr(file_name, '/');
    if (c)
	c++;
    else
	c = file_name;
    pa->name = XtNewString(c);

    return pa;
}

void print_attach_info(PostAttachment *pa, char *buffer)
{
    char	*enc = NULL;
    int		n;

    if (!pa->type)
	strcpy(buffer, "[unknown]");
    else {
	n = strlen(pa->type);
	if (n < 18)
	    strcpy(buffer, pa->type);
	else {
	    memcpy(buffer, pa->type, 15);
	    strcpy(buffer + 15, "...");
	}
    }
    n = strlen(buffer);
    while (n < 20)
	buffer[n++] = ' ';
    buffer[n] = '\0';

    switch (pa->enc) {
    case MimeEncNone:
	enc = "     ";
	break;
    case MimeEncBase64:
	enc = "b64  ";
	break;
    case MimeEncUue:
	enc = "uue  ";
	break;
    case MimeEncQP:
	enc = "Q-P  ";
	break;
    }
    if (enc)
	strcpy(buffer + n, enc);
    n += strlen(buffer + n);

    strncat(buffer + n, pa->file_name, 248 - n);
}

int print_attachment(FILE *fout, PostAttachment *pa)
{
    FILE	*fin;
    int		ok;

    if (!pa->type) {
	popup_title_notice("Unknown Content-Type", pa->file_name, False);
	return False;
    }

    fin = fopen(pa->file_name, "r");
    if (!fin) {
	popup_title_notice("Couldn't open file", pa->file_name, False);
	return False;
    }

    fprintf(fout, "Content-Type: %s\r\n", pa->type);
    if (pa->descr && pa->descr[0] != '\0')
	fprintf(fout, "Content-Description: %s\r\n", pa->descr);
    fprintf(fout, "Content-Disposition: %s",
	    pa->is_inline ? "inline" : "attachment");
    if (pa->name && pa->name[0] != '\0')
	fprintf(fout, "; filename=\"%s\"", pa->name);
    fputs("\r\n", fout);

    switch (pa->enc) {
    case MimeEncNone:
	if (pa->has_8bit)
	    fputs("Content-Transfer-Encoding: 8bit\r\n", fout);
	fputs("\r\n", fout);
	ok = enc_file_none(pa, fin, fout);
	break;
    case MimeEncBase64:
	fputs("Content-Transfer-Encoding: base64\r\n\r\n", fout);
	ok = enc_file_base64(pa, fin, fout);
	break;
    case MimeEncUue:
	fputs("Content-Transfer-Encoding: x-uue\r\n\r\n", fout);
	ok = enc_file_uue(pa, fin, fout);
	break;
    case MimeEncQP:
	fputs("Content-Transfer-Encoding: quoted-printable\r\n\r\n", fout);
	ok = enc_file_qp(pa, fin, fout);
	break;
    default:
	popup_title_notice("Bad encoding", pa->file_name, False);
	ok = False;
	break;
    }

    if (fclose(fin) < 0) {
	perror(pa->file_name);
	if (ok) {
	    popup_title_notice("File error", pa->file_name, False);
	    ok = False;
	}
    }

    return ok;
}

int attach_get_enc(PostAttachment *pa)
{
    return pa ? pa->enc : -1;
}

int attach_is_inline(PostAttachment *pa)
{
    return pa ? pa->is_inline : False;
}

char *attach_get_type(PostAttachment *pa)
{
    return pa && pa->type ? pa->type : "";
}

char *attach_get_name(PostAttachment *pa)
{
    return pa && pa->name ? pa->name : "";
}

char *attach_get_descr(PostAttachment *pa)
{
    return pa && pa->descr ? pa->descr : "";
}

int attach_set_enc(PostAttachment *pa, int enc, char *message)
{
    message[0] = '\0';
    switch (enc) {
    case MimeEncNone:
	if (pa->needs_enc) {
	    strcpy(message, "Can't send binary file without encoding.");
	    return False;
	}
	break;
    case MimeEncBase64:
    case MimeEncUue:
	if (!pa->needs_enc)
	    strcpy(message,
		   "Warning, unnecessary: text file needs no encoding.");
	break;
    case MimeEncQP:
	if (pa->needs_enc)
	    strcpy(message, "QP for binary file?  If you say so...");
	else
	    strcpy(message, "Quoted-printable = quoted-unreadable.");
	break;
    default:
	strcpy(message, "Error!");
	return False;
    }

    pa->enc = enc;
    return True;
}

int attach_set_inline(PostAttachment *pa, int is_inline, char *message)
{
    message[0] = '\0';
    pa->is_inline = is_inline;
    return True;
}

int attach_set_type(PostAttachment *pa, char *type, char *message)
{
    char	*header[2];
    char	type_buf[80], subtype_buf[80];
    MimeArg	args[4] = {{0, }, };
    int		i, ok;

    if (!type)
	type = "";

    message[0] = '\0';
    header[0] = XtMalloc(strlen(type) + 32);
    sprintf(header[0], "Content-Type: %s", type);
    header[1] = NULL;

    ok = parse_content_type(header, type_buf, sizeof type_buf,
			    subtype_buf, sizeof subtype_buf,
			    args, XtNumber(args), True);
    if (!ok)
	strcpy(message, "Parse error!");
    else if (case_lstrcmp(type_buf, "text") == 0) {
	char	*charset = get_charset(args);

	if (!charset)
	    strcpy(message, "Consider sepcifying a charset parameter.");
	else {
	    MimeFont	*font;

	    font = get_font(charset);
	    if (!font)
		strcpy(message, "Warning: No font for that charset.");
	}
    } else {
	const MailcapData	*mcap;

	mcap = mailcap_lookup(type_buf, subtype_buf);
	if (!mcap)
	    strcpy(message, "Warning: No mailcap entry for that type.");
    }

    XtFree(header[0]);
    for (i = 0 ; i < XtNumber(args) ; i++) {
	XtFree(args[i].name);
	XtFree(args[i].value);
    }

    if (ok) {
	XtFree(pa->type);
	pa->type = XtNewString(type);
    }

    return ok;
}

int attach_set_name(PostAttachment *pa, char *name, char *message)
{
    message[0] = '\0';
    XtFree(pa->name);
    pa->name = XtNewString(name);
    return True;
}

int attach_set_descr(PostAttachment *pa, char *descr, char *message)
{
    message[0] = '\0';
    XtFree(pa->descr);
    pa->descr = XtNewString(descr);
    return True;
}

/*************************************************************************/

typedef struct MimeType	{
    const char	*type;
    const char	*suffix;
} MimeType;

static MimeType	*mime_types = NULL;
static long	n_types = -1;

static void load_mime_types(void)
{
    static char	*buffer = NULL;
    char	*c;
    long	n_alloc;

    n_types = 0;
    if (global.mime_types) {
	int	fd = open(global.mime_types, O_RDONLY);

	if (fd < 0)
	    perror(global.mime_types);
	else {
	    buffer = snarf_file(fd, NULL);
	    close(fd);
	}
    }

    n_alloc = 8;
    mime_types = (MimeType *)XtMalloc(n_alloc * sizeof mime_types[0]);

    c = buffer;
    while (c) {
	char	*end = strchr(c, '\n');
	char	*type, *suffix;

	if (end)
	    *end++ = '\0';
	type = strtok(c, " \t");
	if (type)
	    while ((suffix = strtok(NULL, " \t"))) {
		if (n_types + 8 > n_alloc) {
		    n_alloc = 2 * (n_types + 8);
		    mime_types =
			(MimeType *)XtRealloc((char *)mime_types,
					      n_alloc * sizeof mime_types[0]);
		}
		ascii_lower(suffix);
		mime_types[n_types].type   = type;
		mime_types[n_types].suffix = suffix;
		n_types++;
	    }

	c = end;
    }

    mime_types[n_types].type   = "image/jpeg";
    mime_types[n_types].suffix = "jpeg";
    n_types++;
    mime_types[n_types].type   = "image/jpeg";
    mime_types[n_types].suffix = "jpg";
    n_types++;
    mime_types[n_types].type   = "image/gif";
    mime_types[n_types].suffix = "gif";
    n_types++;
    mime_types[n_types].type   = "image/png";
    mime_types[n_types].suffix = "png";
    n_types++;
    mime_types[n_types].type   = "application/postscript";
    mime_types[n_types].suffix = "ps";
    n_types++;

    mime_types = (MimeType *)XtRealloc((char *)mime_types,
				       n_types * sizeof mime_types[0]);
}

static const char *get_mime_type(const char *suffix)
{
    long	n;

    if (n_types < 0)
	load_mime_types();

    for (n = 0 ; n < n_types ; n++)
	if (case_lstrcmp(suffix, mime_types[n].suffix) == 0)
	    return mime_types[n].type;

    return NULL;
}
