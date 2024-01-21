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
#include "charset.h"
#include "decode.h"
#include "util.h"

static const XChar2b ascii_to_big5_table[] = {
#include "big5.h"
};

#define ESC	27
#define SO	14
#define SI	15

/*
 *  Generic XChar2b buffer handling.
 */

typedef struct {
    XChar2b	*wstr;
    long	len;
} WideCharBuf;

#define REALLOC_BUF(buf, new_len)                         \
(buf).wstr = (XChar2b *)XtRealloc((char *)(buf).wstr,     \
				  ((buf).len = new_len) * \
				  sizeof (buf).wstr[0])

typedef struct {
    WideCharBuf		buf;
    unsigned long	pad[4];
} GenericContext;

static void *gen_init(void)
{
    GenericContext	*cont;

    cont = (GenericContext *)XtMalloc(sizeof *cont);
    memset(cont, 0, sizeof *cont);
    cont->buf.len = 256;
    cont->buf.wstr =
	(XChar2b *)XtMalloc(cont->buf.len * sizeof cont->buf.wstr[0]);

    return cont;
}

static void gen_end(void *gcont)
{
    GenericContext	*cont = gcont;

    XtFree((char *)cont->buf.wstr);
    cont->buf.wstr = NULL;
    cont->buf.len = 0;
    XtFree((char *)cont);
}

/*
 *  Unicode-1-1-UTF-7 encoding, rfc 1642.
 */

enum {
    Utf7Ascii = 0,
    Utf7EncWord,
    Utf7ReadPlus
} Utf7State;

typedef struct {
    WideCharBuf		buf;
    unsigned long	data;
    unsigned int	n;
    unsigned char	state;
} Utf7Context;

static XChar2b pull_wide_char(Utf7Context *cont)
{
    XChar2b	ret;

    ret.byte1 = (cont->data >> (cont->n -  8)) & 0xff;
    ret.byte2 = (cont->data >> (cont->n - 16)) & 0xff;
    cont->n -= 16;

    return ret;
}

static long utf7_decode(void *gcont, char *src, long len,
			int is_lf_term, XChar2b **wstr)
{
    Utf7Context	*cont = gcont;
    WideCharBuf	*buf = &cont->buf;
    long	pos = 0;
    int		tmp;

    if (len + 8 > buf->len)
	REALLOC_BUF(*buf, len + 8);

    while (len-- > 0) {
	switch (cont->state) {
	case Utf7Ascii:
	    if (*src == '+')
		cont->state = Utf7ReadPlus;
	    else {
		buf->wstr[pos].byte1 = 0;
		buf->wstr[pos].byte2 = *src;
		pos++;
	    }
	    break;
	case Utf7EncWord:
	    tmp = inv_base64_alpha[(unsigned char)*src];
	    if (tmp < 0) {
		if (*src == '-') {
		    len--;
		    src++;
		}
		while (cont->n >= 16)
		    buf->wstr[pos++] = pull_wide_char(cont);
		cont->state = Utf7Ascii;
	    } else {
		cont->data <<= 6;
		cont->data |= tmp;
		cont->n += 6;
		if (cont->n >= 16)
		    buf->wstr[pos++] = pull_wide_char(cont);
	    }
	    break;
	case Utf7ReadPlus:
	    if (*src == '-') {
		buf->wstr[pos].byte1 = 0;
		buf->wstr[pos].byte2 = '+';
		pos++;
	    } else {
		cont->state = Utf7EncWord;
		cont->data = 0;
		cont->n = 0;
	    }
	    break;
	}
	src++;
    }

    if (is_lf_term) {
	if (cont->state == Utf7EncWord)
	    while (cont->n >= 16)
		buf->wstr[pos++] = pull_wide_char(cont);
	cont->state = Utf7Ascii;
	cont->data = 0;
	cont->n = 0;
    }

    *wstr = cont->buf.wstr;

    return pos;
}

/*
 *  Straight 16 bit, network byte order.  (Don't know if it exists...)
 */

typedef struct {
    WideCharBuf		buf;
    unsigned char	push_back;
    unsigned char	pb_ok;
} Str16Context;

static long str16_decode(void *gcont, char *src, long len,
			 int is_lf_term, XChar2b **wstr)
{
    Str16Context	*cont = gcont;
    WideCharBuf		*buf = &cont->buf;
    long		pos = 0;

    if (len == 0) {
	if (is_lf_term)
	    cont->pb_ok = False;
    } else {
	if (len + 8 > buf->len)
	    REALLOC_BUF(*buf, len + 8);

	if (cont->pb_ok) {
	    buf->wstr[0].byte1 = cont->push_back;
	    buf->wstr[0].byte2 = *src++;
	    pos++;
	    len--;
	    cont->pb_ok = False;
	}

	while (len >= 2) {
	    buf->wstr[pos].byte1 = *src++;
	    buf->wstr[pos].byte2 = *src++;
	    pos++;
	    len -= 2;
	}

	if (len == 0 || is_lf_term)
	    cont->pb_ok = False;
	else {
	    cont->push_back = *src;
	    cont->pb_ok = True;
	}
    }

    *wstr = buf->wstr;

    return pos;
}

/*
 *  Hz-gb-2312 encoding, rfc 1842.
 */

typedef struct {
    WideCharBuf		buf;
    unsigned char	not_in_ascii;
    unsigned char	read_tilde;
    unsigned char	push_back;
    unsigned char	pb_ok;
} HzGbContext;

/*
 *  This is pure guessing.
 */
static XChar2b ascii_to_gb(unsigned char ch)
{
    XChar2b	ret;

    if (ch == ' ') {
	ret.byte1 = 0x21;
	ret.byte2 = 0x21;
    } else {
	ret.byte1 = 0x23;
	if (0x21u <= ch && ch <= 0x7eu)
	    ret.byte2 = ch;
	else
	    ret.byte2 = '@';
    }

    return ret;
}

static XChar2b bytes_to_gb(unsigned char ch1, unsigned char ch2)
{
    XChar2b	ret;

    if (ch1 < 0x21u || 0x77u < ch1 || ch2 < 0x21u || 0x7eu < ch2 )
	return ascii_to_gb('@');

    ret.byte1 = ch1;
    ret.byte2 = ch2;

    return ret;
}

static long hzgb_decode(void *gcont, char *src, long len,
			int is_lf_term, XChar2b **wstr)
{
    HzGbContext	*cont = gcont;
    WideCharBuf	*buf = &cont->buf;
    long	pos = 0;

    if (len + 8 > buf->len)
	REALLOC_BUF(*buf, len + 8);

    while (len-- > 0) {
	if (!cont->not_in_ascii)
	    if (cont->read_tilde) {
		if (*src == '{')
		    cont->not_in_ascii = True;
		else if (*src == '~')
		    buf->wstr[pos++] = ascii_to_gb('~');
		else
		    buf->wstr[pos++] = ascii_to_gb('@');
		cont->read_tilde = False;
	    } else if (*src == '~')
		cont->read_tilde = True;
	    else
		buf->wstr[pos++] = ascii_to_gb(*src);
	else
	    if (cont->read_tilde) {
		if (*src == '}')
		    cont->not_in_ascii = False;
		else
		    buf->wstr[pos++] = ascii_to_gb('@');
		cont->read_tilde = False;
	    } else if (cont->pb_ok) {
		buf->wstr[pos++] = bytes_to_gb(cont->push_back, *src);
		cont->pb_ok = False;
	    } else if (*src == '~')
		cont->read_tilde = True;
	    else {
		cont->push_back = *src;
		cont->pb_ok = True;
	    }
	src++;
    }

    if (is_lf_term) {
	if (cont->read_tilde)
	    buf->wstr[pos++] = ascii_to_gb('@');
	cont->read_tilde = False;
	cont->not_in_ascii = False;
    }

    *wstr = buf->wstr;

    return pos;
}

/*
 *  KSC 5601, rfc 1557.
 */

typedef struct {
    WideCharBuf		buf;
    unsigned char	n_escs;
    unsigned char	not_in_ascii;
    unsigned char	push_back;
    unsigned char	pb_ok;
} KscContext;

static XChar2b ascii_to_ksc(unsigned char ch)
{
    XChar2b	ret;

    if (ch == ' ') {
	ret.byte1 = 0x21;
	ret.byte2 = 0x21;
    } else {
	ret.byte1 = 0x23;
	if (0x21u <= ch && ch <= 0x7eu)
	    ret.byte2 = ch;
	else
	    ret.byte2 = '@';
    }

    return ret;
}

static XChar2b bytes_to_ksc(unsigned char ch1, unsigned char ch2)
{
    XChar2b	ret;

    if (ch1 < 0x21u || 0x7eu < ch1 || ch2 < 0x21u || 0x7eu < ch2)
	return ascii_to_ksc('@');

    ret.byte1 = ch1;
    ret.byte2 = ch2;

    return ret;
}

static long ksc_decode(void *gcont, char *src, long len,
		       int is_lf_term, XChar2b **wstr)
{
    KscContext		*cont = gcont;
    WideCharBuf		*buf = &cont->buf;
    long		pos = 0;

    if (len + 8 > buf->len)
	REALLOC_BUF(*buf, len + 8);

    /* assume we're at beginning of line... */
    if (cont->n_escs != 4) {
	static const char	escbuf[] = {ESC, '$', ')', 'C'};
	const char	*escs = escbuf;
	unsigned int	n;

	n = cont->n_escs;
	escs += n;

	while (len > 0 && n < 4)
	    if (*src != *escs)
		break;
	    else {
		src++;
		escs++;
		len--;
		n++;
	    }

	if (n == 4)
	    cont->n_escs = 4;
	else if (len == 0 && !is_lf_term) {
	    cont->n_escs = n;
	    *wstr = buf->wstr;
	    return 0;
	} else {
	    len += n;
	    src -= n;
	    cont->n_escs = 0;
	}
    }

    while (len-- > 0) {
	if (!cont->not_in_ascii) {
	    if (*src == SO && cont->n_escs == 4)
		cont->not_in_ascii = True;
	    else
		buf->wstr[pos++] = ascii_to_ksc(*src);
	} else if (*src == SI) {
	    if (cont->pb_ok) {
		cont->pb_ok = False;
		buf->wstr[pos++] = ascii_to_ksc('@');
	    }
	    cont->not_in_ascii = False;
	} else if (cont->pb_ok) {
	    buf->wstr[pos++] = bytes_to_ksc(cont->push_back, *src);
	    cont->pb_ok = False;
	} else {
	    cont->push_back = *src;
	    cont->pb_ok = True;
	}
	src++;
    }

    if (is_lf_term) {
	if (cont->not_in_ascii && cont->pb_ok)
	    buf->wstr[pos++] = ascii_to_ksc('@');
	cont->not_in_ascii = False;
	cont->pb_ok = False;
    }

    *wstr = buf->wstr;

    return pos;
}

/*
 *  Big5 encoding.  Pure guesswork.
 */

typedef struct {
    WideCharBuf		buf;
    unsigned char	push_back;
    unsigned char	pb_ok;
} Big5Context;

static XChar2b ascii_to_big5(unsigned char ch)
{
    ch -= ' ';
    if (ch >= (unsigned char)(127 - ' '))
	ch = '@' - ' ';

    return ascii_to_big5_table[ch];
}

static XChar2b bytes_to_big5(unsigned char ch1, unsigned char ch2)
{
    XChar2b	ret;

    if (ch1 < 0xa1u || 0xf9u < ch1 || ch2 < 0x40u || 0xfeu < ch2)
	return ascii_to_big5('@');

    ret.byte1 = ch1;
    ret.byte2 = ch2;

    return ret;
}

static long big5_decode(void *gcont, char *src, long len,
			int is_lf_term, XChar2b **wstr)
{
    Big5Context		*cont = gcont;
    WideCharBuf		*buf = &cont->buf;
    long		pos = 0;

    if (len + 8 > buf->len)
	REALLOC_BUF(*buf, len + 8);

    if (len > 0 && cont->pb_ok) {
	buf->wstr[pos++] = bytes_to_big5(cont->push_back, *src);
	src++;
	len--;
	cont->pb_ok = False;
    }

    while (len-- > 0) {
	if ((unsigned char)*src < 128)
	    buf->wstr[pos++] = ascii_to_big5(*src);
	else if (len > 0) {
	    buf->wstr[pos++] = bytes_to_big5(src[0], src[1]);
	    src++;
	    len--;
	} else {
	    cont->push_back = *src;
	    cont->pb_ok = True;
	}
	src++;
    }

    if (is_lf_term && cont->pb_ok) {
	cont->pb_ok = False;
	buf->wstr[pos++] = ascii_to_big5('@');
    }

    *wstr = buf->wstr;

    return pos;
}

/*********************************************************************/

static const DecodeFuncs err_funcs   = {0, };
static const DecodeFuncs str16_funcs = {gen_init, str16_decode, gen_end};
static const DecodeFuncs utf7_funcs  = {gen_init, utf7_decode,  gen_end};
static const DecodeFuncs hzgb_funcs  = {gen_init, hzgb_decode,  gen_end};
static const DecodeFuncs ksc_funcs   = {gen_init, ksc_decode,   gen_end};
static const DecodeFuncs big5_funcs  = {gen_init, big5_decode,  gen_end};

const DecodeFuncs *get_decode_funcs(char *encoding)
{
    if (!encoding)
	return NULL;
    else if (case_lstrcmp(encoding, "utf-7") == 0 ||
	     case_lstrcmp(encoding, "rfc-1642") == 0)
	return &utf7_funcs;
    else if (case_lstrcmp(encoding, "16-bit") == 0)
	return &str16_funcs;
    else if (case_lstrcmp(encoding, "hz-gb-2312") == 0 ||
	     case_lstrcmp(encoding, "rfc-1842") == 0)
	return &hzgb_funcs;
    else if (case_lstrcmp(encoding, "ksc-5601") == 0 ||
	     case_lstrcmp(encoding, "iso-2022-kr") == 0 ||
	     case_lstrcmp(encoding, "rfc-1557") == 0)
	return &ksc_funcs;
    else if (case_lstrcmp(encoding, "big5") == 0)
	return &big5_funcs;
    else if (case_lstrcmp(encoding, "none") == 0 ||
	     case_lstrcmp(encoding, "8-bit") == 0)
	return NULL;
    else
	return &err_funcs;
}
