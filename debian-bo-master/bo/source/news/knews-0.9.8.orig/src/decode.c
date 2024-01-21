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
#include "util.h"

static int rest_is_whitespace(const char *rest, long len)
{
    while (len-- > 0) {
	if (!IS_SPACE(*rest))
	    return False;
	rest++;
    }

    return True;
}

static int get_hex(char *c)
{
    int		hex_no;

    if (IS_DIGIT(*c))
	hex_no = *c - '0';
    else if ('A' <= *c && *c <= 'F')
	hex_no = *c - 'A' + 10;
    else
	return -1;

    c++;
    hex_no *= 16;

    if (IS_DIGIT(*c))
	hex_no += *c - '0';
    else if ('A' <= *c && *c <= 'F')
	hex_no += *c - 'A' + 10;
    else
	return -1;

    return hex_no;
}

long decode_qp(char *dest, char *src, long len, int *soft, int underscore)
{
    long	src_pos, dest_pos;
    int		hex_no;

    if (soft)
	*soft = False;

    src_pos = dest_pos = 0;
    while (len > 0) {
	if (src[src_pos] == '=') {
	    if (len >= 2 && (hex_no = get_hex(src + src_pos + 1)) >= 0) {
		dest[dest_pos++] = hex_no;
		src += 3;
		len -= 3;
		continue;
	    } else if (rest_is_whitespace(src + src_pos + 1, len - 1)) {
		if (soft)
		    *soft = True;
		break;
	    }
	} else if (src[src_pos] == '_') {
	    if (underscore) {
		dest[dest_pos++] = ' ';
		src++;
		len--;
		continue;
	    }	    
	} else if (IS_SPACE(src[src_pos]) &&
		   rest_is_whitespace(src + src_pos + 1, len - 1))
	    break;

	dest[dest_pos++] = src[src_pos++];
	len--;
    }

    return dest_pos;
}

/*************************************************************************/

const char base64_alpha[64] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

const char inv_base64_alpha[256] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

long decode_base64(B64Context *cont, char *dest, char *src, long len)
{
    unsigned long	data;
    unsigned int	n;
    char		*base = dest;

    if (cont->end) {
	if (src && *src != '\0')
	    cont->trailing_garbage = True;
	return 0;
    }

    data = cont->data;
    n    = cont->n;

    if (!src)
	cont->end = True;

    while (len-- > 0 && !cont->end) {
	int	c = (unsigned char)*src++;

	if (c == '=') {
	    cont->end = True;
	    break;
	}

	c = inv_base64_alpha[c];
	if (c < 0) {
	    cont->err = True;
	    continue;
	}

	data <<= 6;
	data |= c;
	n++;

	if (n == 4) {
	    *dest++ = (data >> 16) & 0xff;
	    *dest++ = (data >>  8) & 0xff;
	    *dest++ =  data        & 0xff;
	    n = 0;
	    data = 0;
	}
    }

    if (cont->end) {
	switch (n) {
	case 3:
	    *dest++ = (data >> 10) & 0xff;
	    *dest++ = (data >>  2) & 0xff;
	    break;
	case 2:
	    *dest++ = (data >>  4) & 0xff;
	    break;
	case 0:
	    break;
	default:
	    cont->err = True;
	    break;
	}
	cont->data = 0;
	cont->n = 0;
    } else {
	cont->data = data;
	cont->n = n;
    }

    return dest - base;
}

int base64_status(B64Context *cont)
{
    int	res = 0;

    if (cont->err)
	res |= BASE64_ERROR;
    if (cont->trailing_garbage)
	res |= BASE64_TRAILING_GARBAGE;

    return res;
}

/*********************************************************************/

enum {
    LookingForBegin = 0,
    Uudecoding,
    FoundEnd
};

long decode_uue(UueContext *cont, char *dest, char *src, long len)
{
    char		*base = dest;
    unsigned long	data;
    unsigned int	n;

    switch (cont->state) {
    case LookingForBegin:
	if (len >= 6 && strncmp(src, "begin ", 6) == 0)
	    cont->state = Uudecoding;
	break;
    case Uudecoding:
	if (len == 3 && strncmp(src, "end", 3) == 0) {
	    cont->state = FoundEnd;
	    break;
	}

	if (len == 0 || *src < ' ' || *src > ' ' + 64) {
	    cont->err = True;
	    break;
	}

	n = *src++ - ' ';
	n &= 63;
	if (n * 4 / 3 > len - 1) {
	    cont->err = True;
	    break;
	}

	len  = n;
	data = 0;
	n    = 0;

	while (len > 0) {
	    unsigned int	tmp = (*src++ - ' ') & 63;

	    data <<= 6;
	    data |= tmp;
	    n++;

	    if (n == 4) {
		if (len < 3)
		    break;
		*dest++ = (data >> 16) & 0xff;
		*dest++ = (data >>  8) & 0xff;
		*dest++ =  data        & 0xff;
		data = 0;
		n = 0;
		len -= 3;
	    }
	}

	if (!cont->err)
	    switch (len) {
	    case 2:
		if (n < 3)
		    cont->err = True;
		else {
		    *dest++ = (data >> 16) & 0xff;
		    *dest++ = (data >>  8) & 0xff;
		}
		break;
	    case 1:
		if (n < 2)
		    cont->err = True;
		else {
		    if (n == 2)
			data <<= 6;
		    *dest++ = (data >> 16) & 0xff;
		}
		break;
	    }

	*dest = '\0';

	return dest - base;
    case FoundEnd:
	break;
    }

    return -1;
}

int uue_status(UueContext *cont)
{
    int	res = 0;

    switch (cont->state) {
    case LookingForBegin:
	res = UUE_NO_BEGIN;
	break;
    case Uudecoding:
	res = UUE_NO_END;
	break;
    }

    if (cont->err)
	res |= UUE_ERROR;

    return res;
}
