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
#include "parse.h"
#include "util.h"

const char month_names[] = "JanFebMarAprMayJunJulAugSepOctNovDec";

char *time_t_to_date(time_t t, char *buffer)
{
    struct tm	*tm;

    tm = gmtime(&t);
    if (!tm) {
	*buffer = '\0';
	return buffer;
    }

    sprintf(buffer, "%d %3.3s %4d %02d:%02d:%02d GMT",
	    tm->tm_mday, month_names + 3 * tm->tm_mon, 1900 + tm->tm_year,
	    tm->tm_hour, tm->tm_min, tm->tm_sec);

    return buffer;
}

/*
 * tm_sec + tm_min*60 + tm_hour*3600 + tm_yday*86400+
 *   (tm_year-70)*31536000 + ((tm_year-69)/4)*86400
 */
static long tm_to_secs(struct tm *tm)
{
    static int	acc_month_days[] = {
	0,
	31,
	31 + 28,
	31 + 28 + 31,
	31 + 28 + 31 + 30,
	31 + 28 + 31 + 30 + 31,
	31 + 28 + 31 + 30 + 31 + 30,
	31 + 28 + 31 + 30 + 31 + 30 + 31,
	31 + 28 + 31 + 30 + 31 + 30 + 31 + 31,
	31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
	31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
	31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30,
    };
    long	i, result = 0;

    i = tm->tm_year - 70;
    if (i < 0)
	return 0;

    switch ((unsigned int)i % 4) {
    case 0:
	result = 0;
	break;
    case 1:
	result = 365;
	break;
    case 2:  /* Leap year, except 2000 */
	if (i != 30 && tm->tm_mon > 1)
	    result = 365 + 365 + 1;
	else
	    result = 365 + 365;
	break;
    case 3:
	result = 365 + 365 + 365 + 1;
	break;
    }

    if (i > 30)  /* past 2000 => we got one day too much ??? */
	result--;

    result += (i / 4) * (365 + 365 + 365 + 365 + 1);
    result += acc_month_days[(unsigned int)tm->tm_mon % 12];
    result += tm->tm_mday - 1;
    result = 24 * result + tm->tm_hour;
    result = 60 * result + tm->tm_min;
    result = 60 * result + tm->tm_sec;

    return result;
}

time_t parsedate(char *str)
{
    struct tm		tm;
    long		offset;
    unsigned int	c0, c1, c2, c3;

    while (*str == ' ')
	str++;

    if (*str < '0')
	return PARSEDATE_ERROR;

    if (*str > '9') {
	if ((!IS_UPPER(str[0]) && !IS_LOWER(str[0])) ||
	    (!IS_UPPER(str[1]) && !IS_LOWER(str[1])) ||
	    (!IS_UPPER(str[2]) && !IS_LOWER(str[2])))
	    return PARSEDATE_ERROR;
	str += 3;
	while (*str == ' ')
	    str++;
	if (*str == ',') {
	    str++;
	    while (*str == ' ')
		str++;
	}

	if (!IS_DIGIT(*str))
	    return PARSEDATE_ERROR;
    }

    tm.tm_mday = *str++ - '0';
    while (IS_DIGIT(*str)) {
	tm.tm_mday *= 10;
	tm.tm_mday += *str++ - '0';
    }

    while (*str == ' ')
	str++;

    if ((c0 = TO_UPPER(str[0])) == '\0' ||
	(c1 = TO_UPPER(str[1])) == '\0' ||
	(c2 = TO_UPPER(str[2])) == '\0')
	return PARSEDATE_ERROR;

    switch (c0) {
    case 'J':
	if (c1 == 'A')
	    if (c2 == 'N')
		tm.tm_mon = 0;
	    else
		return PARSEDATE_ERROR;
	else if (c1 == 'U')
	    if (c2 == 'N')
		tm.tm_mon = 5;
	    else if (c2 == 'L')
		tm.tm_mon = 6;
	    else
		return PARSEDATE_ERROR;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'F':
	if (c1 == 'E' && c2 == 'B')
	    tm.tm_mon = 1;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'M':
	if (c1 == 'A')
	    if (c2 == 'R')
		tm.tm_mon = 2;
	    else if (c2 == 'Y')
		tm.tm_mon = 4;
	    else
		return PARSEDATE_ERROR;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'A':
	if (c1 == 'P')
	    if (c2 == 'R')
		tm.tm_mon = 3;
	    else
		return PARSEDATE_ERROR;
	else if (c1 == 'U')
	    if (c2 == 'G')
		tm.tm_mon = 7;
	    else
		return PARSEDATE_ERROR;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'S':
	if (c1 == 'E' && c2 == 'P')
	    tm.tm_mon = 8;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'O':
	if (c1 == 'C' && c2 == 'T')
	    tm.tm_mon = 9;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'N':
	if (c1 == 'O' && c2 == 'V')
	    tm.tm_mon = 10;
	else
	    return PARSEDATE_ERROR;
	break;
    case 'D':
	if (c1 == 'E' && c2 == 'C')
	    tm.tm_mon = 11;
	else
	    return PARSEDATE_ERROR;
	break;
    default:
	return PARSEDATE_ERROR;
    }

    str += 3;
    while (*str == ' ')
	str++;

    if ((c0 = str[0] - '0') > 9u ||
	(c1 = str[1] - '0') > 9u)
	return PARSEDATE_ERROR;

    c2 = str[2] - '0';

    if (c2 > 9u) {
	tm.tm_year = 10 * c0 + c1;
	str += 2;
    } else {
	c3 = str[3] - '0';
	if (c3 > 9u)
	    return PARSEDATE_ERROR;
	tm.tm_year = 1000 * c0 + 100 * c1 + 10 * c2 + c3 - 1900;
	str += 4;
    }

    while (*str == ' ')
	str++;

    if ((c0 = str[0] - '0') > 9u || (c1 = str[1] - '0') > 9u ||	str[2] != ':')
	return PARSEDATE_ERROR;

    tm.tm_hour = 10 * c0 + c1;
    str += 3;

    if ((c0 = str[0] - '0') > 9u || (c1 = str[1] - '0') > 9u ||
	(str[2] != ':' && str[2] != '\0' && str[2] != ' '))
	return PARSEDATE_ERROR;

    tm.tm_min = 10 * c0 + c1;
    str += 2;

    if (*str == '\0') {
	tm.tm_sec = 0;
	return tm_to_secs(&tm);
    }

    if (*str != ':')
	tm.tm_sec = 0;
    else {
	str++;
	if ((c0 = str[0] - '0') > 9u || (c1 = str[1] - '0') > 9u ||
	    (str[2] != ' ' && str[2] != '\0'))
	    return PARSEDATE_ERROR;

	tm.tm_sec = 10 * c1 + c2;
	str += 2;
    }

    while (*str == ' ')
	str++;

    if (*str == '\0')
	return tm_to_secs(&tm);

    if (*str == '+' || *str == '-') {
	str++;
	if ((c0 = str[0] - '0') > 9u ||
	    (c1 = str[1] - '0') > 9u ||
	    (c2 = str[2] - '0') > 9u ||
	    (c3 = str[3] - '0') > 9u)
	    return PARSEDATE_ERROR;
	offset = 3600 * (10 * c0 + c1) + 60 * c2 + c3;
	if (*str == '-')
	    offset = - offset;
    } else {
	if ((c0 = str[0], !IS_UPPER(c0)) ||
	    (c1 = str[1], !IS_UPPER(c1)) ||
	    (c2 = str[2], !IS_UPPER(c2)))
	    return PARSEDATE_ERROR;

	switch (c0) {
	case 'C':
	    if (c2 == 'T') /* CST, CDT, CET */
		if (c1 == 'S')
		    offset = 6 * 3600;
		else if (c1 == 'D')
		    offset = 5 * 3600;
		else if (c1 == 'E')
		    offset = - 3600;
		else
		    return PARSEDATE_ERROR;
	    else if (c2 == 'S') /* CES */
		if (c1 == 'E')
		    offset = - 3600;
		else
		    return PARSEDATE_ERROR;
	    else
		return PARSEDATE_ERROR;
	    break;
	case 'E':
	    if (c2 == 'T') /* EST, EDT, EET */
		if (c1 == 'S')
		    offset = 5 * 3600;
		else if (c1 == 'D')
		    offset = 4 * 3600;
		else if (c1 == 'E')
		    offset = - 2 * 3600;
		else
		    return PARSEDATE_ERROR;
	    else
		return PARSEDATE_ERROR;
	    break;
	case 'M':
	    if (c2 == 'T') /* MST, MDT, MET */
		if (c1 == 'S')
		    offset = 7 * 3600;
		else if (c1 == 'D')
		    offset = 6 * 3600;
		else if (c1 == 'E')
		    offset = -3600;
		else
		    return PARSEDATE_ERROR;
	    else if (c2 == 'Z') /* MEZ */
		if (c1 == 'E')
		    offset = -3600;
		else
		    return PARSEDATE_ERROR;
	    else
		return PARSEDATE_ERROR;
	    break;
	case 'P':
	    if (c2 == 'T')  /* PST, PDT */
		if (c1 == 'S')
		    offset = 8 * 3600;
		else if (c1 == 'D')
		    offset = 7 * 3600;
		else
		    return PARSEDATE_ERROR;
	    else
		return PARSEDATE_ERROR;
	    break;
	default:
	    offset = 0;
	    break;
	}
    }

    return tm_to_secs(&tm) + offset;
}

/*************************************************************************/

char *eat_re(char *str)
{
    long	n;

    while ((str[0] == 'R' || str[0] == 'r') &&
	   (str[1] == 'E' || str[1] == 'e')) {
	if (str[2] == ':')
	    str += 3;
	else if (str[2] == '^' && IS_DIGIT(str[3]) && str[4] == ':')
	    str += 5;
	else
	    break;

	while (*str == ' ')
	    str++;
    }

    n = strlen(str) - 1;
    while (n > 0 && IS_SPACE(str[n]))
	str[n--] = '\0';

    return str;
}

const char *parse_author(const char *from, long *len)
{
    const char	*c1, *c2;

    if ((c2 = strchr(from, '<'))) {
	c1 = from;
	while (c1 < c2 && (IS_SPACE(*c1) || *c1 == '"'))
	    c1++;
	do {
	    c2--;
	} while (c2 > c1 && (IS_SPACE(*c2) || *c2 == '"'));

	*len = c2 - c1 + 1;
	if (*len > 0)
	    return c1;
    }

    if ((c1 = strchr(from, '(')) && (c2 = strrchr(c1, ')'))) {
	do {
	    c1++;
	} while (c1 < c2 && (IS_SPACE(*c1) || *c1 == '"'));
	do {
	    c2--;
	} while (c2 > c1 && (IS_SPACE(*c2) || *c2 == '"'));

	*len = c2 - c1 + 1;
	if (*len > 0)
	    return c1;
    }

    c1 = from;
    while (IS_SPACE(*c1))
	c1++;
    if (*c1 != '\0') {
	c2 = c1 + strlen(c1);
	do {
	    c2--;
	} while (c2 > c1 && IS_SPACE(*c1));

	*len = c2 - c1 + 1;
	if (*len > 0)
	    return c1;
    }

    *len = 6;
    return "<none>";
}

/*********************************************************************/

static int get_token(char **c, char **p)
{
    int	len;

    if (**c == '"') {
	*p = ++*c;
	while (**p != '\0' && **p != '"')
	    (*p)++;
	if (**p == '\0')
	    len = -1;
	else
	    len = (*p)++ - *c;
    } else {
	*p = *c;
	while (**p != '\0' && **p != ' ' &&
	       **p != '\t' && **p != '/' &&
	       **p != ';' && **p != '(' &&
	       **p != '=' && **p != '"')
	    (*p)++;
	len = *p - *c;
    }

    return len;
}

int parse_content_enc(char **headers)
{
    char	*c, *p;
    int		comment, len;

    c = headers[0] + sizeof "Content-Transfer-Encoding:" - 1;
    comment = 0;
    do {
	for (;;) {
	    while (IS_SPACE(*c))
		c++;

	    if (*c == '\0')
		break;
	    if (*c == '(')
		comment++;
	    if (comment) {
		if (*c == ')')
		    comment--;
		c++;
		continue;
	    }

	    len = get_token(&c, &p);
#define IS_ENC(enc) \
	    (len == sizeof enc - 1 && \
	     case_lstrncmp(c, enc, sizeof enc - 1) == 0)
	    switch (TO_UPPER(*c)) {
	    case '7':
		if (IS_ENC("7bit"))
		    return MimeEncNone;
		break;
	    case '8':
		if (IS_ENC("8bit"))
		    return MimeEncNone;
		break;
	    case 'B':
		if (IS_ENC("binary"))
		    return MimeEncNone;
		else if (IS_ENC("base64"))
		    return MimeEncBase64;
		break;
	    case 'Q':
		if (IS_ENC("quoted-printable"))
		    return MimeEncQP;
		break;
	    case 'U':
		if (IS_ENC("uue") ||
		    IS_ENC("uuencode") ||
		    IS_ENC("uuencoded"))
		    return MimeEncUue;
		break;
	    case 'X':
		if (IS_ENC("x-uue") ||
		    IS_ENC("x-uuencode") ||
		    IS_ENC("x-uuencoded"))
		    return MimeEncUue;
		break;
	    }
#undef IS_ENC

	    return -1;
	}
	c = *++headers;
    } while (c && IS_SPACE(*c));

    return -1;
}

char *parse_content_disp(char **headers)
{
    enum {
	CDLookingForType,
	CDLookingForSemicolon,
	CDLookingForName,
	CDLookingForEqual,
	CDLookingForValue
    }		state = CDLookingForType;
    char	*c, *p;
    int		len, comment, was_filename = False;

    c = headers[0] + sizeof "Content-Disposition:" - 1;
    comment = 0;
    do {
	for (;;) {
	    while (IS_SPACE(*c))
		c++;

	    if (*c == '\0')
		break;
	    if (*c == '(')
		comment++;
	    if (comment) {
		if (*c == ')')
		    comment--;
		c++;
		continue;
	    }

	    switch (state) {
	    case CDLookingForType:
		len = get_token(&c, &p);
		if (len < 0)
		    return NULL;
		/* Check whether inline or attahcment? */
		c = p;
		state = CDLookingForSemicolon;
		break;
	    case CDLookingForSemicolon:
		if (*c++ != ';')
		    return NULL;
		state = CDLookingForName;
		break;
	    case CDLookingForName:
		len = get_token(&c, &p);
		if (len < 0)
		    return NULL;
		if (len == 8 && case_lstrncmp(c, "filename", 8) == 0)
		    was_filename = True;
		c = p;
		state = CDLookingForEqual;
		break;
	    case CDLookingForEqual:
		if (*c++ != '=')
		    return NULL;
		state = CDLookingForValue;
		break;
	    case CDLookingForValue:
		len = get_token(&c, &p);
		if (len < 0)
		    return NULL;
		if (was_filename) {
		    p = XtMalloc(len + 1);
		    memcpy(p, c, len);
		    p[len] = '\0';
		    return p;
		}
		c = p;
		state = CDLookingForSemicolon;
		break;
	    }
	}
	c = *++headers;
    } while (c && IS_SPACE(*c));

    return NULL;
}

int parse_content_type(char **headers,
		       char *type_buf, int type_len,
		       char *subtype_buf, int subtype_len,
		       MimeArg *args, int n_args, int strict)
{
    enum {
	CTLookingForType,
	CTLookingForSlash,
	CTLookingForSubtype,
	CTLookingForSemicolon,
	CTLookingForName,
	CTLookingForEqual,
	CTLookingForValue
    }		state = CTLookingForType;
    char	*c, *p;
    int		len, n, comment;

    c = headers[0] + sizeof "Content-Type:" - 1;
    n = 0;
    comment = 0;
    do {
	for (;;) {
	    while (IS_SPACE(*c))
		c++;

	    if (*c == '\0')
		break;
	    if (*c == '(')
		comment++;
	    if (comment) {
		if (*c == ')')
		    comment--;
		c++;
		continue;
	    }

	    switch (state) {
	    case CTLookingForType:
		len = get_token(&c, &p);
		if (len < 0 || len + 4 > type_len)
		    return False;
		memcpy_lower(type_buf, c, len);
		type_buf[len] = '\0';
		c = p;
		state = CTLookingForSlash;
		break;
	    case CTLookingForSlash:
		if (*c++ != '/')
		    return False;
		state = CTLookingForSubtype;
		break;
	    case CTLookingForSubtype:
		len = get_token(&c, &p);
		if (len < 0 || len + 4 > subtype_len)
		    return False;
		memcpy_lower(subtype_buf, c, len);
		subtype_buf[len] = '\0';
		c = p;
		state = CTLookingForSemicolon;
		break;
	    case CTLookingForSemicolon:
		if (*c++ != ';')
		    return True; /* lenience */
		state = CTLookingForName;
		break;
	    case CTLookingForName:
		if (n >= n_args)
		    return True;
		len = get_token(&c, &p);
		if (len < 0)
		    return !strict; /* lenience */
		args[n].name = XtMalloc(len + 1);
		memcpy_lower(args[n].name, c, len);
		args[n].name[len] = '\0';
		c = p;
		state = CTLookingForEqual;
		break;
	    case CTLookingForEqual:
		if (*c++ != '=')
		    return !strict; /* lenience */
		state = CTLookingForValue;
		break;
	    case CTLookingForValue:
		len = get_token(&c, &p);
		if (len < 0)
		    return !strict; /* lenience */
		args[n].value = XtMalloc(len + 1);
		memcpy(args[n].value, c, len);
		args[n++].value[len] = '\0';
		c = p;
		state = CTLookingForSemicolon;
		break;
	    }
	}
	c = *++headers;
    } while (c && IS_SPACE(*c));

    if (state ==CTLookingForSemicolon || state == CTLookingForName)
	return True;
    else if (state == CTLookingForEqual || state == CTLookingForValue)
	return !strict;
    else
	return False;
}

char *get_charset(MimeArg *args)
{
    while (args->value)
	if (strcmp(args->name, "charset") == 0)
	    return args->value;
	else
	    args++;

    return NULL;
}

char *next_enc_word(char *header, EncWordData *data)
{
    for (header = strstr(header, "=?") ; header ;
	 header = strstr(header + 1, "=?")) {
	char		*c1, *c2;

	c1 = strchr(header + 2, '?');
	if (!c1 || (c1[1] != 'q' && c1[1] != 'Q' &&
		    c1[1] != 'b' && c1[2] != 'B') || c1[2] != '?')
	    continue;
	c2 = strstr(c1 + 3, "?=");
	if (!c2)
	    continue;

	data->word   = c1 + 3;
	data->end    = c2 + 2;
	data->len    = c2 - c1 - 3;
	data->ch_len = c1 - (header + 2);
	data->is_qp  = (c1[1] == 'Q' || c1[1] == 'q');
	break;
    }

    return header;
}

void decode_rfc1522(char *src, const char *charset)
{
    EncWordData	data;
    char	*dest = NULL;
    long	n;

    if (!charset)
	return;

    for (;;) {
	char	*next;

	for (next = next_enc_word(src, &data) ; next ;
	     next = next_enc_word(next + 1, &data)) {
	    int	tmp;

	    (next + 2)[data.ch_len] = '\0';
	    tmp = case_strcmp(next + 2, charset);
	    (next + 2)[data.ch_len] = '?';
	    if (tmp == 0) {
		if (!dest)
		    dest = next;
		else {
		    n = next - src;
		    memmove(dest, src, n);
		    dest += n;
		}
		break;
	    }
	}

	if (!next)
	    break;

	if (data.is_qp)
	    n = decode_qp(dest, data.word, data.len, NULL, True);
	else {
	    B64Context	bc = {0, };

	    n = decode_base64(&bc, dest, data.word, data.len);
	}
	if (n >= 0)
	    dest += n;

	src = data.end;
    }

    if (dest)
	memmove(dest, src, strlen(src) + 1);
}
