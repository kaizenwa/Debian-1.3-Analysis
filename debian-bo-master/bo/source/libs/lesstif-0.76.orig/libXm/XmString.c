/*
 *
 * $Id: XmString.c,v 1.30 1997/01/06 06:50:25 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: XmString.c,v 1.30 1997/01/06 06:50:25 u27113 Exp $";

/*
 * ** Danny had added a flag here, but I think the behavior he added is always
 *  desirable.  What this refers to is checking in XmStringGetLtoR for the
 *  value of XmSTRING_DEFAULT_CHARSET for the tag. - MLM
 *
 * Not defining this breaks certain things badly (Danny 11/8/1996).
 * As I can feel that this statement requires justification by examples,
 *  here' s one :
 *    The XmStringCreateSimple() routine uses tag XmFONTLIST_DEFAULT_TAG.
 * If you use its result with XmStringGetLtoR and use XmSTRING_DEFAULT_CHARSET
 *  for tag, (as pre-1.2 documentation says you should), then an invalid result
 *  would be obtained.
 *
 * ** There is an implicit assumption that charsets come before the text
 *  they are used by in the internal representation.  Don't change this
 *  if you want to live.
 */

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <X11/IntrinsicP.h>
#include <X11/Xfuncs.h>

#include <stdio.h>
#include <locale.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include <XmI/DebugUtil.h>

/*
 * for information on the external encoding, see doc/XmStrings.txt
 */
/*
 * 1 octect tag 1 octet (nominal) len
 */
#define ASN1_HEADER_SIZE	2
/*
 * ASN1 header + XmSTRING header
 */
#define XmSTRING_HEADER_SIZE	(ASN1_HEADER_SIZE * 2)

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

#define DEBUG	1

/**************************** PRIVATE FUNCTIONS ****************************/
#ifdef DEBUG
static int
asn1_dump(unsigned char *string)
{
    unsigned length, i, nlen, j;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;

    printf("STRING: TAG: %02x LEN: %02x\n", str->tag, str->len);
    fflush(stdout);

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH) {
	printf("IS NOT AN XmSTRING\n");
	fflush(stdout);
	return 0;
    }

    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING) {
	printf("IS NOT AN XmSTRING: %d\n", __LINE__);
	fflush(stdout);
	return 0;
    }

    length = 0;

    if (str->len > XmSTRING_LENGTH) {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned)) {
		printf("Invalid XmString\n");
		fflush(stdout);
		return 0;
	    }
	}
    }
    else {
	i = 0;
	length = str->len & ~XmSTRING_LENGTH;
    }

    next = &str->data[i];

    if (length < 0) {
	printf("String is malformed\n");
	fflush(stdout);
	return 0;
    }
    else if (length == 0)
	return 0;

    for (;;) {
        str = (struct __XmStringExtRec *)next;

	/* primitive type -- doesn't recurse */
	nlen = 0;

	if (str->len > XmSTRING_LENGTH) {

	    for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
		nlen <<= 8;
		nlen |= str->data[i];
		if (i > sizeof(unsigned)) {
		    printf("Invalid XmString\n");
		    fflush(stdout);
		    exit (0);
		}
	    }
	}
	else {
	    i = 0;
	    nlen = str->len & ~XmSTRING_LENGTH;
	}

	switch (str->tag) {
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf("UNKNOWN COMPONENT: length %d\n", nlen);
	    fflush(stdout);
	    nlen++;
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    printf("CHARSET:\n");
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    printf("TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    printf("DIRECTION: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    printf("SEPARATOR: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf("LOCALE TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;
	default:
	    printf("invalid tag: %02x\n", str->tag);
	    fflush(stdout);
	    nlen = 1;
	}

	next += nlen;
	length -= nlen;
	if (length < 0) {
	    printf("String is malformed\n");
	    fflush(stdout);
	    return 0;
	}
	else if (length == 0) {
	    printf("\n\n");
	    fflush(stdout);
	    return 0;
	}
    }
}

void
_Xm_dump_fontlist(XmFontList fontlist)
{
    int i;

    printf("Fontlist: %p\n", fontlist);
    for (i = 0;
	 fontlist && fontlist[i].tag && strlen(fontlist[i].tag) != 0;
	 i++) {
	printf("Fontlist entry: %d : tag: %s : type: %d : font: %p\n",
	       i, fontlist[i].tag, fontlist[i].type, fontlist[i].font);
    }
    printf("\n");
}

void
_Xm_dump_fontlist_cache(void)
{
}

void
_Xm_dump_external(XmString str)
{
    asn1_dump(str);
    printf("\n");
}

void
_Xm_dump_internal(_XmString str)
{
    int i;

    if (!str) {
	printf(" NULL internal string\n");
	return;
    }

    for (i = 0; i < str->number_of_components; i++) {
	switch (str->components[i]->type) {
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf(" %d: UNKNOWN component\n", i);
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    printf(" %d: CHARSET: %s\n", i, str->components[i]->data);
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    printf(" %d: TEXT: %s, font: %d\n", i, str->components[i]->data,
		   str->components[i]->font);
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    printf(" %d: DIRECTION: %d\n", i, (int)str->components[i]->data[0]);
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    printf(" %d: SEPARATOR\n", i);
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf(" %d: LOCALE TEXT: %s, font: %d\n", i,
		   str->components[i]->data, str->components[i]->font);
	    break;

	default:
	    break;
	}
    }
    printf("\n");
}

#endif

static _XmString 
__XmAllocNewXmString(int number_of_components)
{
    _XmString newString = (_XmString)XtCalloc(1, sizeof(struct __XmStringRec));
    int i;

    newString->number_of_components = number_of_components;

    if (number_of_components) {
	newString->components =
	    (_XmStringComponent *)XtMalloc(sizeof(_XmStringComponent)
					   * newString->number_of_components);
    }
    
    for (i=0; i<number_of_components; i++)
	newString->components[i] =
		(_XmStringComponent)XtCalloc(1, sizeof(_XmStringComponentRec));

    return newString;
}

static void
__XmGrowXmString(_XmString string)
{
    string->number_of_components++;

    if (string->number_of_components == 1)
	string->components = (_XmStringComponent *)XtMalloc(sizeof(_XmStringComponent));
    else
	string->components =
	    (_XmStringComponent *)XtRealloc((char *)string->components,
					    sizeof(_XmStringComponent)
				            * string->number_of_components);
    
    string->components[string->number_of_components - 1] =
	(_XmStringComponent)XtCalloc(1, sizeof(_XmStringComponentRec));
}

static _XmStringComponent
__XmStringPeekNextComponent(_XmStringContext context)
{
    if (context == NULL)
	return NULL;

    if (context->current_component < (context->string->number_of_components - 1))
	return context->string->components[context->current_component + 1];
    else
	return NULL;
}

static _XmStringComponent
__XmStringGetNextComponent(_XmStringContext context)
{
    if (context == NULL)
	return NULL;

    context->current_component++;

    if (context->current_component < context->string->number_of_components)
	return context->string->components[context->current_component];
    else
	return NULL;
}

static void
__XmStringComponentCopy(struct __XmStringComponentRec *dest,
			struct __XmStringComponentRec *src)
{
    dest->type = src->type;
    dest->length = src->length;
    if (src->data && src->length)
	dest->data = XtNewString(src->data);
    else
	dest->data = src->data;
}

static _XmString
__XmStringFromASN1(XmString string)
{
    unsigned length, i, nlen;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;
    char *charset;
    _XmString intern;

    if (!string)
	return NULL;

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH)
	return NULL;

    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING)
	return NULL;

    length = 0;

    if (str->len > XmSTRING_LENGTH) {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned)) {
		_XmWarning(NULL, "Invalid XmString\n");
		return NULL;
	    }
	}
    }
    else {
	i = 0;
	length = str->len & ~XmSTRING_LENGTH;
    }

    next = &str->data[i];

    if (length < 0)
	return NULL;
    else if (length == 0)
	return NULL;

    intern = __XmAllocNewXmString(0);

    for (;;) {
        str = (struct __XmStringExtRec *)next;

	/* primitive type -- doesn't recurse */
	nlen = 0;

	if (str->len > XmSTRING_LENGTH) {

	    for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
		nlen <<= 8;
		nlen |= str->data[i];
		if (i > sizeof(unsigned)) {
		    _XmWarning(NULL, "Invalid XmString\n");
		    _XmStringFree(intern);
		    return NULL;
		}
	    }
	}
	else {
	    i = 0;
	    nlen = str->len & ~XmSTRING_LENGTH;
	}

	switch (str->tag) {
	case XmSTRING_COMPONENT_UNKNOWN:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN EXTERNAL STRING\n");
	    nlen++;
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components-1]->type =
		XmSTRING_COMPONENT_CHARSET;

	    charset = XtMalloc(nlen + 1);
	    bcopy(&str->data[i], charset, nlen);
	    charset[nlen] = 0;
	    if (_XmStringIsCurrentCharset(charset))
	    {
		intern->components[intern->number_of_components-1]->data =
		    XtNewString(XmFONTLIST_DEFAULT_TAG);
		intern->components[intern->number_of_components-1]->length =
		    strlen(XmFONTLIST_DEFAULT_TAG);
		XtFree(charset);
	    }
	    else
	    {
		intern->components[intern->number_of_components-1]->data =
		    charset;
		intern->components[intern->number_of_components-1]->length =
		    nlen;
	    }
		
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components-1]->type =
		XmSTRING_COMPONENT_TEXT;
	    intern->components[intern->number_of_components-1]->length =
		nlen;
	    intern->components[intern->number_of_components-1]->data =
		XtMalloc(nlen + 1);
	    bcopy(&str->data[i],
		  intern->components[intern->number_of_components-1]->data,
		  nlen);
	    intern->components[intern->number_of_components-1]->data[nlen] = 0;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components-1]->type =
		XmSTRING_COMPONENT_DIRECTION;
	    intern->components[intern->number_of_components-1]->length =
		1;
	    intern->components[intern->number_of_components-1]->data =
		XtMalloc(1);
	    intern->components[intern->number_of_components-1]->data[0] =
		str->data[0];

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components-1]->type =
		XmSTRING_COMPONENT_SEPARATOR;
	    intern->components[intern->number_of_components-1]->length =
		0;
	    intern->components[intern->number_of_components-1]->data = NULL;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components-1]->type =
		XmSTRING_COMPONENT_LOCALE_TEXT;
	    intern->components[intern->number_of_components-1]->length =
		nlen;
	    intern->components[intern->number_of_components-1]->data =
		XtMalloc(nlen + 1);
	    bcopy(&str->data[i],
		  intern->components[intern->number_of_components-1]->data,
		  nlen);
	    intern->components[intern->number_of_components-1]->data[nlen] = 0;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	default:
	    _XmWarning(NULL, "XmString has invalid tag: %02x\n", str->tag);
	    nlen = 1;
	}

	next += nlen;
#if 0
/* length is unsigned, this could create a problem */
	length -= nlen;

	if (length < 0) {
	    _XmWarning(NULL, "XmString is malformed\n");
	    _XmStringFree(intern);
	    return NULL;
	}
	else if (length == 0)
	    return intern;
#else
	if (length < nlen) {
	    _XmWarning(NULL, "XmString is malformed\n");
	    _XmStringFree(intern);
	    return NULL;
	}

	length -= nlen;
	if (length == 0)
	    return intern;
#endif
    }
}

static XmString
__XmStringToASN1(_XmString string)
{
    XmString external;
    struct __XmStringExtRec *str;
    int totlen, i, j, nlen, added, tlen;
    char *tmp;

    if (!string)
	return NULL;

    totlen = 0;
    for (i = 0; i < string->number_of_components; i++) {
	switch (string->components[i]->type) {
	case XmSTRING_COMPONENT_CHARSET:
	    if (strcmp(string->components[i]->data,
		       XmFONTLIST_DEFAULT_TAG) == 0)
		nlen = strlen(_XmStringGetCurrentCharset());
	    else
		nlen = string->components[i]->length;

	    totlen += nlen + ASN1_HEADER_SIZE;

	    if (nlen >= XmSTRING_LENGTH)
	    {
		nlen = string->components[i]->length;
		while (nlen) {
		    totlen++;
		    nlen >>= 8;
		}
	    }
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    totlen += string->components[i]->length + ASN1_HEADER_SIZE;
	    if (string->components[i]->length >= XmSTRING_LENGTH) {
		nlen = string->components[i]->length;
		while (nlen) {
		    totlen++;
		    nlen >>= 8;
		}
	    }
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    totlen += 3;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    totlen += ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN INTERNAL STRING\n");
	    break;
	}
    }

    added = 0;
    /* additional space for length data in extralong strings */
    if (totlen >= XmSTRING_LENGTH) {
	nlen = totlen;
        while (nlen) {
	    added++;
	    nlen >>= 8;
	}
    }

    /* add four bytes for the standard header */
    external = (unsigned char *)XtMalloc(totlen + XmSTRING_HEADER_SIZE + added);

    /* standard header and overall length */
    str = (struct __XmStringExtRec *)external;
    str->tag = XmSTRING_TAG;
    str->len = XmSTRING_LENGTH;
    str = (struct __XmStringExtRec *)str->data;

    str->tag = XmSTRING_COMPONENT_XMSTRING;
    if (totlen >= XmSTRING_LENGTH) {
	str->len = XmSTRING_LENGTH;
	nlen = totlen;
	j = 0;
	while (totlen) {
	    totlen >>= 8;
	    j++;
	} while (totlen);
	str->len += j;
	for (i = j - 1; i >= 0; i--) {
	    str->data[i] = nlen & 0x00FFU;
	    nlen >>= 8;
	}
	str = (struct __XmStringExtRec *)&str->data[j];
    }
    else {
	str->len = totlen;
	str = (struct __XmStringExtRec *)str->data;
    }

    for (i = 0; i < string->number_of_components; i++) {
	switch (string->components[i]->type) {
	case XmSTRING_COMPONENT_CHARSET:

	    str->tag = XmSTRING_COMPONENT_CHARSET;

	    if (strcmp(string->components[i]->data,
		       XmFONTLIST_DEFAULT_TAG) == 0)
	    {
		tmp = _XmStringGetCurrentCharset();
		nlen = strlen(tmp);
	    }
	    else
	    {
		tmp = string->components[i]->data;
		nlen = string->components[i]->length;
	    }

	    if (nlen >= XmSTRING_LENGTH) {
		tlen = nlen;
		totlen = 0;
		while (tlen) {
		    totlen++;
		    tlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		tlen = nlen;
		for (j = totlen - 1; j >= 0; j--) {
		    str->data[j] = tlen & 0x00FFU;
		    tlen >>= 8;
		}
		bcopy(tmp,
		      &str->data[totlen],
		      nlen);
		str = (struct __XmStringExtRec *)&str->data[totlen + nlen];
	    }
	    else {
		str->len = nlen;
		bcopy(tmp, str->data, nlen);
		str = (struct __XmStringExtRec *)&str->data[nlen];
	    }
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    str->tag = XmSTRING_COMPONENT_TEXT;
	    if (string->components[i]->length >= XmSTRING_LENGTH) {
		nlen = string->components[i]->length;
		totlen = 0;
		while (nlen) {
		    totlen++;
		    nlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		nlen = string->components[i]->length;
		for (j = totlen - 1; j >= 0; j--) {
		    str->data[j] = nlen & 0x00FFU;
		    nlen >>= 8;
		}
		bcopy(string->components[i]->data,
		      &str->data[totlen],
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[totlen +
							   string->components[i]->length];
	    }
	    else {
		str->len = string->components[i]->length;
		bcopy(string->components[i]->data,
		      str->data,
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[string->components[i]->length];
	    }
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    str->tag = XmSTRING_COMPONENT_LOCALE_TEXT;

	    if (string->components[i]->length >= XmSTRING_LENGTH) {
		nlen = string->components[i]->length;
		totlen = 0;
		while (nlen) {
		    totlen++;
		    nlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		nlen = string->components[i]->length;
		for (j = totlen - 1; j >= 0; j--) {
		    str->data[j] = nlen & 0x00FFU;
		    nlen >>= 8;
		}
		bcopy(string->components[i]->data,
		      &str->data[totlen],
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[totlen +
							   string->components[i]->length];
	    }
	    else {
		str->len = string->components[i]->length;
		bcopy(string->components[i]->data,
		      str->data,
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[string->components[i]->length];
	    }
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    str->tag = XmSTRING_COMPONENT_DIRECTION;
	    str->len = 1;
	    str->data[0] = string->components[i]->data[0];
	    str = (struct __XmStringExtRec *)&str->data[1];
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    str->tag = XmSTRING_COMPONENT_SEPARATOR;
	    str->len = 0;
	    str = (struct __XmStringExtRec *)str->data;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	default:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN INTERNAL STRING\n");
	    break;
	}
    }

    return external;
}

static Boolean
__XmStringSegmentExtent(XmFontList flist, _XmStringComponent comp,
			Dimension *width, Dimension *height,
			Dimension *ascent, Dimension *descent)
{
    int dc, amax = 0, dmax = 0;
    XCharStruct ov;
    XRectangle ink, log;

    XdbDebug(__FILE__, NULL, "_XmStringSegmentExtent()\n");

    *height = 0;
    *width = 0;
    *ascent = 0;
    *descent = 0;

    if ((comp->type != XmSTRING_COMPONENT_TEXT &&
	 comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
	comp->font == XmUNSPECIFIED)
    {
	XdbDebug(__FILE__, NULL,
		 "__XmStringSegmentExtent: got NULL Font/bad text\n");

	return False;
    }

    switch (flist[comp->font].type) {
    case XmFONT_IS_FONT:
		    
	*width = XTextWidth((XFontStruct *)flist[comp->font].font,
			    comp->data, comp->length);
		    
	XTextExtents((XFontStruct *)flist[comp->font].font,
		     comp->data, comp->length,
		     &dc, &amax, &dmax, &ov);

	*height = amax + dmax;
	*ascent = amax;
	*descent = dmax;
		    
	break;

    case XmFONT_IS_FONTSET:

	XmbTextExtents((XFontSet)flist[comp->font].font,
		       comp->data, comp->length, &ink, &log);
	/* maybe?*/
	break;
    }

    return True;
}

/**************************** INTERNAL FUNCTIONS ***************************/
XFontStruct *
_XmGetFirstFont(XmFontListEntry entry)
{
    XFontStruct **flist;
    char **fnames;
    int num;

    if (entry == NULL)
	return NULL;

    if (entry->type == XmFONT_IS_FONT)
	return (XFontStruct *)entry->font;
    else if (entry->type == XmFONT_IS_FONTSET) {
	num = XFontsOfFontSet((XFontSet)entry->font, &flist, &fnames);
	if (num)
	    return flist[0];
    }

    return NULL;
}

Boolean
_XmFontListGetDefaultFont(XmFontList fontlist,
			  XFontStruct **font_struct)
{
    int i;

    if (fontlist == NULL) {
	XdbDebug(__FILE__, NULL, "_XmFontListGetDefaultFont(NULL, ...)\n");
	return False;
    }

    for (i=0; fontlist[i].tag != NULL; i++)
    {
	if (!strcmp(XmFONTLIST_DEFAULT_TAG, fontlist[i].tag))
	    break;
    }

    if (fontlist[i].tag == NULL)
    {
	*font_struct = NULL;
	return False;
    }

    if (fontlist[i].type == XmFONT_IS_FONT) {
	*font_struct = (XFontStruct*)fontlist[i].font;
	return True;
    }
    else if (fontlist[i].type == XmFONT_IS_FONTSET) {
	*font_struct = _XmGetFirstFont(&fontlist[i]);
	return True;
    }

    *font_struct = NULL;
    return False;
}

Boolean
_XmFontListSearch(XmFontList fontlist,
		  XmStringCharSet charset,
		  short *indx,
		  XFontStruct **font_struct)
{
    int i;

    for (i=0; fontlist[i].tag != NULL; i++)
    {
	if (!strcmp(charset, fontlist[i].tag))
	    break;
    }

    if (fontlist[i].tag == NULL &&
	strcmp(charset, XmFONTLIST_DEFAULT_TAG) != 0)
    {
	for (i=0; fontlist[i].tag != NULL; i++)
	{
	    if (!strcmp(XmFONTLIST_DEFAULT_TAG, fontlist[i].tag))
		break;
	}
    }

    if (fontlist[i].tag == NULL)
    {
	*indx = XmUNSPECIFIED;
	*font_struct = NULL;
	return False;
    }

    *indx = i;
    if (fontlist[i].type == XmFONT_IS_FONT)
    {
	*font_struct = (XFontStruct*)fontlist[i].font;
	return True;
    }
    else if (fontlist[i].type == XmFONT_IS_FONTSET)
    {
	*font_struct = _XmGetFirstFont(&fontlist[i]);
	return True;
    }

    *font_struct = NULL;
    return False;
}

/*
 * See the difference between _XmString and XmString (internal vs.
 *	external representation).
 *
 * Returns True if the argument is external representation.
 */
Boolean
_XmStringIsXmString(XmString string)
{
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;

    if (!string)
	return False;

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH)
	return False;

    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING)
	return False;

    return True;
}

Boolean
_XmStringEmpty(_XmString string)
{
    int i;

    if (string == NULL || string->number_of_components == 0)
	return True;

    for (i = 0; i < string->number_of_components; i++)
    {
	if ((string->components[i]->type == XmSTRING_COMPONENT_TEXT ||
	     string->components[i]->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
	    string->components[i]->length != 0)
	    return False;
    }

    return True;
}

_XmString
_XmStringCreate(XmString cs)
{
    if (!cs)
	return NULL;
    return __XmStringFromASN1(cs);
}

void
_XmStringFree(_XmString string)
{
    int i;

    if (string == NULL)
	return;

    for (i = 0; i < string->number_of_components; i++) {
	if (string->components[i]->data && string->components[i]->length)
	    XtFree((char *)string->components[i]->data);

	XtFree((char *)string->components[i]);
    }

    XtFree((char *)string->components);
    XtFree((char *)string);
}

char *
_XmStringGetCurrentCharset(void)
{
    char *lang;

    if ((lang = getenv("LANG")) == NULL)
	return XmFALLBACK_CHARSET;

    if (strcmp(lang, "C") == 0 ||
	strcmp(lang, "POSIX") == 0 ||
	strcmp(lang, "ISO8859-1") == 0 ||
	strcmp(lang, "ISO-8859-1") == 0)
	return XmSTRING_OS_CHARSET;

    return lang;
}

char *
_XmCharsetCanonicalize(String charset)
{
    return NULL;
}

void
_XmStringUpdate(XmFontList fontlist, _XmString string)
{
    short i, where, def, tf;
    XFontStruct *dummy;

    if (!_XmFontListSearch(fontlist, XmFONTLIST_DEFAULT_TAG, &def, &dummy))
	tf = XmUNSPECIFIED;
    else
	tf = def;
    if (def == XmUNSPECIFIED)
	_XmFontListSearch(fontlist, fontlist[0].tag, &def, &dummy);

    for (i = 0; i < string->number_of_components; i++)
    {
	if (string->components[i]->type == XmSTRING_COMPONENT_CHARSET)
	{
	    if (_XmFontListSearch(fontlist,
				  string->components[i]->data,
				  &where,
				  &dummy))
		tf = where;
	}
	if (string->components[i]->type == XmSTRING_COMPONENT_LOCALE_TEXT)
	    string->components[i]->font = def;

	if (string->components[i]->type == XmSTRING_COMPONENT_TEXT)
	{
	    if (tf != XmUNSPECIFIED)
		string->components[i]->font = tf;
	    else
		string->components[i]->font = def;
	}
    }
}

_XmString
_XmStringCopy(_XmString s)
{
    _XmString newString;
    int i;

    if (s == NULL)
	return NULL;

    newString = __XmAllocNewXmString(s->number_of_components);

    for (i=0; i<newString->number_of_components; i++)
	__XmStringComponentCopy(newString->components[i], s->components[i]);

    return newString;
}

Boolean
_XmStringByteCompare(_XmString a, _XmString b)
{
    _XmStringContext context1 = NULL;
    _XmStringComponent foo1;
    _XmStringContext context2 = NULL;
    _XmStringComponent foo2;

    if (!_XmStringInitContext(&context1, a))
	return False;
    if (!_XmStringInitContext(&context2, b))
	return False;

    while ((foo1 = __XmStringGetNextComponent(context1)))
    {
	if (! (foo2 = __XmStringGetNextComponent(context2)))
	    return False;

	if (foo1->type == XmSTRING_COMPONENT_SEPARATOR) {
	    if (foo2->type == XmSTRING_COMPONENT_SEPARATOR)
		continue;
	    else
		return False;
	}
	else if (foo2->type == XmSTRING_COMPONENT_SEPARATOR)
	    return False;

	if (strcmp(foo1->data, foo2->data))
	    return False;
    }

    _XmStringFreeContext(context1);
    _XmStringFreeContext(context2);

    return True;
}

Boolean
_XmStringHasSubstring(_XmString string, _XmString substring)
{
    int i, j, k;
    Boolean match;

    /* if both empty, match */
    if (_XmStringEmpty(string) && _XmStringEmpty(substring))
	return True;

    /* if first string is empty, never match */
    if (_XmStringEmpty(string))
	return False;

    /* if second empty, match */
    if (_XmStringEmpty(substring))
	return True;

    for (i = 0; i < string->number_of_components; i++)
    {
	if (string->components[i]->type == substring->components[0]->type &&
	    string->components[i]->length == substring->components[0]->length &&
	    ((string->components[i]->data != NULL &&
	      substring->components[0]->data != NULL &&
	      strncmp(string->components[i]->data,
		      substring->components[0]->data,
		      substring->components[0]->length) == 0) ||
	     (string->components[i]->data == NULL &&
	      substring->components[0] == NULL)))
	{
	    match = True;

	    for (j = i + 1, k = 1; k < substring->number_of_components; j++, k++)
	    {
		if (string->components[j]->type !=
		    substring->components[k]->type ||
		    string->components[j]->length !=
		    substring->components[k]->length ||
		    ((string->components[j]->data != NULL &&
		      substring->components[k]->data != NULL &&
		      strncmp(string->components[j]->data,
			      substring->components[k]->data,
			      substring->components[k]->length) != 0) ||
		     (string->components[j]->data != NULL &&
		      substring->components[k] == NULL) ||
		     (string->components[j]->data == NULL &&
		      substring->components[k] != NULL)))
		{
		    match = False;
		    break;
		}
	    }

	    if (match)
		return True;
	}
    }

    return False;
}

XmString
_XmStringCreateExternal(XmFontList fontlist, _XmString cs)
{
    /* need to change the string maybe, so that if a font doesn't exist
     * it changes to default? */
    if (fontlist)
	_XmStringUpdate(fontlist, cs);

    return __XmStringToASN1(cs);
}

char *
_XmStringGetTextConcat(XmString string)
{
    int ilen = 0, len = 0;
    char *ret = NULL;
    _XmString str;
    _XmStringComponent comp;
    _XmStringContext context = NULL;

    str = _XmStringCreate(string);

    _XmStringInitContext(&context, str);

    while ((comp = __XmStringGetNextComponent(context)) != NULL)
    {
	if (comp->type == XmSTRING_COMPONENT_TEXT ||
	    comp->type == XmSTRING_COMPONENT_LOCALE_TEXT)
	{
	    if (len == 0)
	    {
		len = comp->length;
		ret = XtMalloc(len + 1);
	    }
	    else
	    {
		len = comp->length;
		ret = XtRealloc(ret, ilen + len + 1);
	    }
	    bcopy(comp->data, &ret[ilen], len);
	    ret[ilen + len] = 0;
	    ilen += len;
	}
    }

    _XmStringFreeContext(context);

    _XmStringFree(str);

    return ret;
}

Boolean
_XmStringIsCurrentCharset(XmStringCharSet c)
{
    if (!c)
	return False;

    /* returns true if this charset is in the current locale */
    return strcmp(c, _XmStringGetCurrentCharset()) == 0;
}

Boolean
_XmStringSingleSegment(XmString str, char **pTextOut,
                       XmStringCharSet *pCharsetOut)
{
    XmStringContext context;
    Boolean ret = False;
    XmStringDirection ddir;
    Boolean dsep;

    if (!XmStringInitContext(&context, str))
	return False;

    ret = XmStringGetNextSegment(context, pTextOut, pCharsetOut, &ddir, &dsep);

    XmStringFreeContext(context);

    return ret;
}

void
_XmStringUpdateWMShellTitle(XmString xmstr, Widget shell)
{
	XdbDebug(__FILE__, shell, "_XmStringUpdateWMShellTitle is not implemented yet\n");
	/* FIX ME */
}

/* Motif 2.* version of the above */
void
XmeStringUpdateWMShellTitle(XmString xmstr, Widget shell)
{
	_XmStringUpdateWMShellTitle(xmstr, shell);
}

Boolean
_XmStringInitContext(_XmStringContext *context, _XmString string)
{
    _XmStringContext	p;

    if (string && context) {
	p = (_XmStringContext)XtMalloc(sizeof(struct __XmStringContextRec));

	p->string = string;
	p->current_component = -1;

	*context = p;
	return True;
    }
    else
	return False;
}

Boolean
_XmStringGetNextSegment(_XmStringContext context,
			XmStringCharSet *tag,
			XmStringDirection *direction,
			char **text,
                        short *char_count,
                        Boolean *separator)
{
    Boolean valid = False;
    XmStringComponentType type = XmSTRING_COMPONENT_UNKNOWN;
    char *ntext;
    XmStringCharSet ntag;
    XmStringDirection ndir;
    _XmStringComponent next;

    /*
     * note that tag is not zero'd.  That's because tag values need
     * to persist across calls to this func, unless a new tag is seen.
     */
    if (text)
	*text = NULL;
    if (direction)
	*direction = XmSTRING_DIRECTION_DEFAULT;
    if (separator)
	*separator = False;
    ntext = NULL;
    ntag = NULL;
    ndir = XmSTRING_DIRECTION_DEFAULT;

    while ((next = __XmStringGetNextComponent(context)) != NULL)
    {
	valid = True;
	type = next->type;

	switch (type) {
	case XmSTRING_COMPONENT_CHARSET:
	    ntag = next->data;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    ndir = next->data[0];
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    ntext = next->data;
	    *char_count = next->length;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	default:
	    break;
	}

	if (ntext != NULL && text)
	    *text = ntext;
	if (ntag != NULL && tag)
	    *tag = ntag;
	if (ndir != XmSTRING_DIRECTION_DEFAULT && direction)
	    *direction = ndir;

	next = __XmStringPeekNextComponent(context);
	if (next) {
	    type = next->type;

	    if (type == XmSTRING_COMPONENT_SEPARATOR) {
		*separator = True;
		break;
	    }

	    if ((type == XmSTRING_COMPONENT_LOCALE_TEXT ||
		 type == XmSTRING_COMPONENT_TEXT) && text && *text)
		break;

	    if (type == XmSTRING_COMPONENT_CHARSET && tag && *tag)
		break;

	    if (type == XmSTRING_COMPONENT_DIRECTION && direction &&
	        *direction != XmSTRING_DIRECTION_DEFAULT)
		break;
	}
	ntext = NULL;
	ntag = NULL;
	ndir = XmSTRING_DIRECTION_DEFAULT;
    }

    if (tag && !*tag)
	*tag = XmFONTLIST_DEFAULT_TAG;

    return valid;
}

void
_XmStringFreeContext(_XmStringContext context)
{
    XtFree((char *)context);
}

void
_XmStringExtent(XmFontList fontlist, _XmString string,
                Dimension *width, Dimension *height)
{
    _XmStringContext context = NULL;
    Boolean have_seg, have_line_height;
    Dimension current_height = 0, current_width = 0;
    Dimension line_height = 0, default_line_height;
    Dimension amax, dmax, wd, ht;
    int pending_newlines;
    _XmStringComponent comp;

    if (!string) 
    {
       *width = *height = 0;
       return;
    }

    _XmStringUpdate(fontlist, string);

    _XmStringInitContext(&context, string);

    *height = *width = 0;

    have_seg = False;
    have_line_height = False;
    line_height = 0;
    default_line_height = 0;
    pending_newlines = 0;
    while ((comp = __XmStringGetNextComponent(context)) != NULL)
    {
	if (comp->type == XmSTRING_COMPONENT_TEXT ||
	    comp->type == XmSTRING_COMPONENT_LOCALE_TEXT)
	{
	    if (__XmStringSegmentExtent(fontlist, comp,
					 &wd, &ht, &amax, &dmax))
	    {
		have_seg = True;
		if (ht > line_height)
		    line_height = ht;/*amax + dmax;*/
	    }
	    current_width += wd;
	}
	else if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    if (*width < current_width)
		*width = current_width;

	    if (!have_seg && !have_line_height)
		pending_newlines++;
	    else if (have_seg && !have_line_height) {
		have_line_height = True;
		default_line_height = line_height;
		current_height += default_line_height * pending_newlines;
		pending_newlines = 0;
	    }
	    else if (have_seg)
		default_line_height = line_height;

	    if (!have_seg && have_line_height)
		current_height += default_line_height;
	    else
		current_height += line_height;
		
	    current_width = 0;
	    line_height = 0;
	    have_seg = False;
	}
    }

    if (have_seg)
	current_height += line_height;
    else
	current_height += default_line_height;

    if (*height < current_height)
	*height = current_height;

    if (*width < current_width)
	*width = current_width;

    _XmStringFreeContext(context);
}

Dimension
_XmStringWidth(XmFontList fontlist, _XmString string)
{
    Dimension width = 0, dontcare;

    _XmStringExtent(fontlist, string, &width, &dontcare);

    return width;
}

Dimension
_XmStringHeight(XmFontList fontlist, _XmString string)
{
    Dimension height = 0, dontcare;

    _XmStringExtent(fontlist, string, &dontcare, &height);

    return height;
}

Dimension
_XmStringBaseline(XmFontList fontlist, _XmString string)
{
    _XmStringContext context = NULL;
    Dimension current_baseline = 0, wd, ht, asc, desc;
    _XmStringComponent comp;

    if (!_XmStringInitContext(&context, string))
	return (Dimension)0;

    _XmStringUpdate(fontlist, string);

    while ((comp = __XmStringGetNextComponent(context)) != NULL)
    {
	if ((comp->type == XmSTRING_COMPONENT_TEXT ||
	     comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
	    comp->font != XmUNSPECIFIED)
	{
	    __XmStringSegmentExtent(fontlist, comp, &wd, &ht, &asc, &desc);

	    if (asc > current_baseline)
		current_baseline = asc;
	}
	else if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    break;
    }

    _XmStringFreeContext(context);

    return current_baseline;
}

int
_XmStringLineCount(_XmString string)
{
    _XmStringContext context = NULL;
    char *text, *tag;
    short len;
    XmStringDirection dir;
    Boolean sep;
    int lc;

    lc = 0;

    if (!_XmStringInitContext(&context, string))
	return 0;

    while (_XmStringGetNextSegment(context, &tag, &dir, &text, &len, &sep))
    {
	if (sep)
	    lc++;
    }
    lc++;

    _XmStringFreeContext(context);

    return lc;
}

void
_XmStringDraw(Display *d, Window w, XmFontList fontlist, _XmString string,
              GC gc, Position x, Position y, Dimension width,
              unsigned char align, unsigned char lay_dir,
              XRectangle *clip)
{
    _XmStringContext context = NULL;
    _XmStringComponent comp;
    Position current_y;
    Position current_x = 0;
    Boolean have_seg, have_line_height;
    Dimension default_line_height;
    XRectangle ink, log;
    int pending_newlines;

    if (w == 0 || string == 0)
	return;

    XdbDebug(__FILE__, XtWindowToWidget(d, w), "XmStringDraw x %d y %d wid %d\n",
	x, y, width);

    current_y = y;

    _XmStringUpdate(fontlist, string);

    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    default_line_height = 0;

    /* repeat while there's at least one thing in the string */
    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_width, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_width = line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	/*
	 * first, calculate the line extents
	 */
	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		line_width += seg_width;
		if (seg_height > line_height)
		    line_height = seg_height;
		if (seg_ascent > line_ascent)
		    line_ascent = seg_ascent;
		if (seg_descent > line_descent)
		    line_descent = seg_descent;

		have_seg = True;
	    }
	    else if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
		if (!have_seg && !have_line_height)
		    pending_newlines++;
		else if (have_seg && !have_line_height) {
		    default_line_height = line_ascent + line_descent;
		    have_line_height = True;
		    current_y += pending_newlines * default_line_height; 
		}
		else if (have_seg)
		    default_line_height = line_ascent + line_descent;
		break;
	    }
	}

	if (!have_seg && !have_line_height)
	    continue;
	else if (!have_seg && have_line_height) {
	    current_y += default_line_height;
	    continue;
	}
	else
	    current_y += line_ascent;

	context->current_component = start;

	/* now we handle the starting x of this line based on the alignment */
	switch (align)
	{
	case XmALIGNMENT_CENTER:
	    current_x = x + (width - line_width + 1) / 2;
	    break;
	case XmALIGNMENT_BEGINNING:
	    current_x = x;
	    break;
	case XmALIGNMENT_END:
	    current_x = x + width - line_width;
	    break;
	}

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
		break;

	    else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
		      comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
		     comp->font == XmUNSPECIFIED)
		continue;

	    switch (fontlist[comp->font].type) {
	    case XmFONT_IS_FONT:
		XSetFont(d, gc,
			 ((XFontStruct *)fontlist[comp->font].font)->fid);

		XDrawString(d, w, gc, current_x, current_y,
			    comp->data, comp->length);

		current_x += XTextWidth((XFontStruct *)fontlist[comp->font].font,
					comp->data, comp->length);

		break;

	    case XmFONT_IS_FONTSET:
		XmbDrawString(d, w, (XFontSet)fontlist[comp->font].font, gc,
			      current_x, current_y, comp->data, comp->length);

		XmbTextExtents((XFontSet)fontlist[comp->font].font,
			       comp->data, comp->length, &ink, &log);

		/* need to do something with the extents of the font set here. */
		break;
	    }
	}

	current_y += line_descent;
    }
}

void
_XmStringDrawImage(Display *d, Window w,
                   XmFontList fontlist, _XmString string,
                   GC gc, Position x, Position y, Dimension width,
                   unsigned char align, unsigned char lay_dir,
                   XRectangle *clip)
{
    _XmStringContext context = NULL;
    _XmStringComponent comp;
    Position current_y;
    Position current_x = 0;
    Boolean have_seg, have_line_height;
    Dimension default_line_height;
    XRectangle ink, log;
    int pending_newlines;

    current_y = y;

    _XmStringUpdate(fontlist, string);

    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    default_line_height = 0;

    /* repeat while there's at least one thing in the string */
    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_width, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_width = line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	/*
	 * first, calculate the line extents
	 */
	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		line_width += seg_width;
		if (seg_height > line_height)
		    line_height = seg_height;
		if (seg_ascent > line_ascent)
		    line_ascent = seg_ascent;
		if (seg_descent > line_descent)
		    line_descent = seg_descent;

		have_seg = True;
	    }

	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
		if (!have_seg && !have_line_height)
		    pending_newlines++;
		else if (have_seg && !have_line_height) {
		    default_line_height = line_ascent + line_descent;
		    have_line_height = True;
		    current_y += pending_newlines * default_line_height;
		}
		else if (have_seg)
		    default_line_height = line_ascent + line_descent;
		break;
	    }
	}

	if (!have_seg && !have_line_height)
	    continue;
	else if (!have_seg && have_line_height) {
	    current_y += default_line_height;
	    continue;
	}
	else
	    current_y += line_ascent;

	context->current_component = start;

	/* now we handle the starting x of this line based on the alignment */
	switch (align)
	{
	case XmALIGNMENT_CENTER:
	    current_x = x + (width - line_width + 1) / 2;
	    break;
	case XmALIGNMENT_BEGINNING:
	    current_x = x;
	    break;
	case XmALIGNMENT_END:
	    current_x = x + width - line_width;
	    break;
	}

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
		break;

	    else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
		      comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
		     comp->font == XmUNSPECIFIED)
		continue;

	    switch (fontlist[comp->font].type) {
	    case XmFONT_IS_FONT:
		XSetFont(d, gc,
			 ((XFontStruct *)fontlist[comp->font].font)->fid);

		XDrawImageString(d, w, gc, current_x, current_y,
				 comp->data, comp->length);

		current_x += XTextWidth((XFontStruct *)fontlist[comp->font].font,
					comp->data, comp->length);

		break;

	    case XmFONT_IS_FONTSET:
		XmbDrawImageString(d, w, (XFontSet)fontlist[comp->font].font,
				   gc, current_x, current_y,
				   comp->data, comp->length);

		XmbTextExtents((XFontSet)fontlist[comp->font].font,
			       comp->data, comp->length, &ink, &log);

		/* need to do something with the extents of the font set here. */
		break;
	    }
	}

	current_y += line_descent;
    }
}

void
_XmStringDrawUnderline(Display *d, Window w, XmFontList fontlist,
		       _XmString string, GC gc,
		       Position x, Position y, Dimension width,
                       unsigned char align, unsigned char lay_dir,
                       XRectangle *clip, _XmString underline)
{
    _XmStringContext context = NULL,
		     und_c = NULL;
    _XmStringComponent comp, und;
    Position current_y;
    Position current_x = 0;
    int xo;
    char *p, *u;
    Boolean have_seg, have_line_height;
    Dimension default_line_height;
    XRectangle ink, log;
    int pending_newlines;

    if (w == 0)
	return;

    XdbDebug(__FILE__, XtWindowToWidget(d, w), "XmStringDrawUnderline x %d y %d wid %d\n",
	x, y, width);

    /* Currently only works if "underline" is a single-segment LtoR string */
    _XmStringInitContext(&und_c, underline);
    und = __XmStringPeekNextComponent(und_c);

    if (und->type == XmSTRING_COMPONENT_TEXT || und->type == XmSTRING_COMPONENT_LOCALE_TEXT)
	u = und->data;
    else
	u = NULL;

    current_y = y;

    _XmStringUpdate(fontlist, string);

    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    default_line_height = 0;

    /* repeat while there's at least one thing in the string */
    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_width, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_width = line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	/*
	 * first, calculate the line extents
	 */
	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		line_width += seg_width;
		if (seg_height > line_height)
		    line_height = seg_height;
		if (seg_ascent > line_ascent)
		    line_ascent = seg_ascent;
		if (seg_descent > line_descent)
		    line_descent = seg_descent;

		have_seg = True;
	    }

	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
		if (!have_seg && !have_line_height)
		    pending_newlines++;
		else if (have_seg && !have_line_height) {
		    default_line_height = line_ascent + line_descent;
		    have_line_height = True;
		    current_y += pending_newlines * default_line_height;
		}
		else if (have_seg)
		    default_line_height = line_ascent + line_descent;
		break;
	    }
	}

	if (!have_seg && !have_line_height)
	    continue;
	else if (!have_seg && have_line_height) {
	    current_y += default_line_height;
	    continue;
	}
	else
	    current_y += line_ascent;

	context->current_component = start;

	/* now we handle the starting x of this line based on the alignment */
	switch (align)
	{
	case XmALIGNMENT_CENTER:
	    current_x = x + (width - line_width + 1) / 2;
	    break;
	case XmALIGNMENT_BEGINNING:
	    current_x = x;
	    break;
	case XmALIGNMENT_END:
	    current_x = x + width - line_width;
	    break;
	}

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
		break;

	    else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
		      comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
		     comp->font == XmUNSPECIFIED)
		continue;

	    switch (fontlist[comp->font].type) {
	    case XmFONT_IS_FONT:
		XSetFont(d, gc,
			 ((XFontStruct *)fontlist[comp->font].font)->fid);

		XDrawString(d, w, gc, current_x, current_y,
			    comp->data, comp->length);

		/* Now *first* underline, then update current_x */

		if (u && (p = strstr(comp->data, u))) {	/* Underline */
		    int ww, l = p - comp->data;
		    char *f = XtMalloc(l + 1);

		    strncpy(f, comp->data, l);
		    f[l] = '\0';
		    xo = XTextWidth((XFontStruct *)fontlist[comp->font].font,
				    f, l);
		    XtFree(f);
		    ww = XTextWidth((XFontStruct *)fontlist[comp->font].font,
				    u, strlen(u));

		    /*
		     * Subtracted 1 from the width to take the space
		     * between characters * into account. Is there a
		     * decent way to do this ??  Danny 17/4/96
		     */
		    XDrawLine(d, w, gc,
			      current_x + xo, current_y + 1,
			      current_x + xo + ww - 1, current_y + 1);
		}

		current_x += XTextWidth((XFontStruct *)fontlist[comp->font].font,
					comp->data, comp->length);
		break;

	    case XmFONT_IS_FONTSET:
		XmbDrawImageString(d, w, (XFontSet)fontlist[comp->font].font,
				   gc, current_x, current_y,
				   comp->data, comp->length);

		XmbTextExtents((XFontSet)fontlist[comp->font].font,
			       comp->data, comp->length, &ink, &log);

		/* need to do something with the extents of the font set here. */
		break;
	    }
	}

	current_y += line_descent;
    }
}

void
_XmStringDrawMnemonic(Display *d,
		      Window w,
		      XmFontList fontlist,
		      _XmString string,
		      GC gc,
		      Position x,
		      Position y,
		      Dimension width,
		      unsigned char alignment,
		      unsigned char layout_direction,
		      XRectangle *clip,
		      String mnemonic,
		      XmStringCharSet charset)
{
    XmString	und;
    _XmString _und;

    XdbDebug(__FILE__, XtWindowToWidget(d, w), "_XmStringDrawMnemonic(%s, %s)\n",
	XdbXmString2String((XmString)string), mnemonic);

    if (mnemonic == NULL) {
	_XmStringDraw(d, w, fontlist, string, gc, x, y, width, alignment,
		layout_direction, clip);
	return;
    }

    und = XmStringCreate(mnemonic, charset);
    _und = _XmStringCreate(und);

    _XmStringDrawUnderline(d, w, fontlist, string,
			   gc, x, y, width, alignment,
			   layout_direction, clip, _und);

    XmStringFree(und);
    _XmStringFree(_und);
}

/************************* PUBLIC FUNCTIONS *****************************/
Dimension 
XmStringBaseline(XmFontList fontlist, 
		 XmString string)
{
    Dimension baseline;
    _XmString str;

    if (!_XmStringIsXmString(string))
	return 0;

    str = _XmStringCreate(string);
    baseline = _XmStringBaseline(fontlist, str);
    _XmStringFree(str);

    return baseline;
}

Boolean 
XmStringByteCompare(XmString s1, 
		    XmString s2)
{
    _XmString str1, str2;
    Boolean ret;

    if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
	return False;

    str1 = _XmStringCreate(s1);
    str2 = _XmStringCreate(s2);

    ret = _XmStringByteCompare(str1, str2);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}

Boolean 
XmStringCompare(XmString s1, XmString s2)
{
    if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
	return False;

    return XmStringByteCompare(s1, s2);
}

XmString 
XmStringConcat(XmString str1, XmString str2)
{
    _XmString s1, s2;
    _XmString newString;
    int i, base, total;
    XmString ret;

    if (!_XmStringIsXmString(str1) || !_XmStringIsXmString(str2))
	return NULL;

    s1 = _XmStringCreate(str1);
    s2 = _XmStringCreate(str2);

    if (s1)
	total = s1->number_of_components;
    else
	total = 0;
    base = total;
    total += s2->number_of_components;

    newString = __XmAllocNewXmString(total);

    if (s1) {
	for (i = 0; i < base; i++)
	    __XmStringComponentCopy(newString->components[i], s1->components[i]);
    }

    for (i = 0; i < s2->number_of_components; i++)
	__XmStringComponentCopy(newString->components[base+i], s2->components[i]);

    _XmStringFree(s1);
    _XmStringFree(s2);

    ret = _XmStringCreateExternal(NULL, newString);

    _XmStringFree(newString);

    return ret;
}

XmString
XmStringCopy(XmString s)
{
    _XmString s1;
    XmString ret;

    if (!_XmStringIsXmString(s))
	return NULL;

    s1 = _XmStringCreate(s);
    ret = _XmStringCreateExternal(NULL, s1);

    _XmStringFree(s1);

    return ret;
}

XmString 
XmStringNConcat(XmString s1, XmString s2, int num_bytes)
{
    _XmString str1, str2;
    XmString ret;
    int i, nlen, tmp;

    if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
	return NULL;

    str1 = _XmStringCreate(s1);
    str2 = _XmStringCreate(s2);

    i = 0;
    while (num_bytes && i < str2->number_of_components)
    {
	num_bytes -= ASN1_HEADER_SIZE; /* header */
	if (num_bytes < 0)
	    break;

	if (num_bytes == 0)
	{
	    __XmGrowXmString(str1);
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;
	    break;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;

	    i++;
	    continue;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_DIRECTION)
	{
	    num_bytes--;

	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 1;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(1);
	    str1->components[str1->number_of_components - 1]->data[0] =
		str2->components[i]->data[0];

	    i++;
	    continue;
	}

	__XmGrowXmString(str1);
	nlen = str2->components[i]->length;

	if (num_bytes < XmSTRING_LENGTH && nlen >= XmSTRING_LENGTH)
	{
	    nlen = num_bytes;
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    bcopy(str2->components[i]->data,
		  str1->components[str1->number_of_components - 1]->data,
		  nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}
	else
	{
	    if (nlen > num_bytes)
		nlen = num_bytes;

	    if (nlen >= XmSTRING_LENGTH)
	    {
		for (tmp = nlen; tmp >= XmSTRING_LENGTH; tmp >>= 8)
		    num_bytes--;
	    }

	    if (nlen > num_bytes)
		nlen = num_bytes;

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    bcopy(str2->components[i]->data,
		  str1->components[str1->number_of_components - 1]->data,
		  nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}

	num_bytes -= nlen;
    }

    ret = _XmStringCreateExternal(NULL, str1);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}

XmString 
XmStringNCopy(XmString s1, int num_bytes)
{
    _XmString str1, str2;
    XmString ret;
    int i, nlen, tmp;

    if (!_XmStringIsXmString(s1))
	return NULL;

    str1 = __XmAllocNewXmString(0);
    str2 = _XmStringCreate(s1);

    i = 0;
    while (num_bytes && i < str2->number_of_components)
    {
	num_bytes -= ASN1_HEADER_SIZE; /* header */
	if (num_bytes < 0)
	    break;

	if (num_bytes == 0)
	{
	    __XmGrowXmString(str1);
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;
	    break;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;

	    i++;
	    continue;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_DIRECTION)
	{
	    num_bytes--;

	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 1;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(1);
	    str1->components[str1->number_of_components - 1]->data[0] =
		str2->components[i]->data[0];

	    i++;
	    continue;
	}

	__XmGrowXmString(str1);
	nlen = str2->components[i]->length;

	if (num_bytes < XmSTRING_LENGTH && nlen >= XmSTRING_LENGTH)
	{
	    nlen = num_bytes;
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    bcopy(str2->components[i]->data,
		  str1->components[str1->number_of_components - 1]->data,
		  nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}
	else
	{
	    if (nlen > num_bytes)
		nlen = num_bytes;

	    if (nlen >= XmSTRING_LENGTH)
	    {
		for (tmp = nlen; tmp >= XmSTRING_LENGTH; tmp >>= 8)
		    num_bytes--;
	    }

	    if (nlen > num_bytes)
		nlen = num_bytes;

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    bcopy(str2->components[i]->data,
		  str1->components[str1->number_of_components - 1]->data,
		  nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}

	num_bytes -= nlen;
    }

    ret = _XmStringCreateExternal(NULL, str1);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}

XmString 
XmStringSegmentCreate(char *text, 
		      char *tag, 
		      XmStringDirection direction,
		      Boolean separator)
{
    _XmString str = NULL;
    XmString ret;

    if (text && tag) {
	if (strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0 ||
	    strcmp(tag, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    str = __XmAllocNewXmString(2);

 	    str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	    str->components[0]->length = strlen(text);
	    str->components[0]->data = XtMalloc(1);
	    str->components[0]->data[0] = direction;

	    str->components[1]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	    str->components[1]->length = strlen(text);
	    str->components[1]->data = XtNewString(text);
	}
	else
	{
	    str = __XmAllocNewXmString(3);

 	    str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	    str->components[0]->length = strlen(text);
	    str->components[0]->data = XtMalloc(1);
	    str->components[0]->data[0] = direction;

	    str->components[1]->type = XmSTRING_COMPONENT_CHARSET;
	    str->components[1]->length = strlen(tag);
	    str->components[1]->data = XtNewString(tag);

	    str->components[2]->type = XmSTRING_COMPONENT_TEXT;
	    str->components[2]->length = strlen(text);
	    str->components[2]->data = XtNewString(text);
	}
    }
    else if (text) {
	str = __XmAllocNewXmString(2);

	str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	str->components[0]->length = 0;
	str->components[0]->data = XtMalloc(1);
	str->components[0]->data[0] = direction;

	str->components[1]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	str->components[1]->length = strlen(text);
	str->components[1]->data = XtNewString(text);
    }
    else {
	str = __XmAllocNewXmString(1);

	str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	str->components[0]->length = 0;
	str->components[0]->data = XtMalloc(1);
	str->components[0]->data[0] = direction;
    }

    if (separator)
    {
	__XmGrowXmString(str);
	str->components[str->number_of_components-1]->type =
	    XmSTRING_COMPONENT_SEPARATOR;
	str->components[str->number_of_components-1]->length = 0;
	str->components[str->number_of_components-1]->data = 0;
    }
    ret = _XmStringCreateExternal(NULL, str);
    _XmStringFree(str);

    return ret;
}

XmString 
XmStringSeparatorCreate(void)
{
    _XmString newString = __XmAllocNewXmString(1);
    XmString ret;

    newString->components[0]->type = XmSTRING_COMPONENT_SEPARATOR;
    newString->components[0]->length = 0;
    newString->components[0]->data = NULL;

    ret = _XmStringCreateExternal(NULL, newString);
    _XmStringFree(newString);

    return ret;
}

XmString 
XmStringDirectionCreate(XmStringDirection direction)
{
    _XmString newString = __XmAllocNewXmString(1);
    XmString ret;

    newString->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
    newString->components[0]->length = 0;
    newString->components[0]->data = XtMalloc(1);
    newString->components[0]->data[0] = direction;

    ret = _XmStringCreateExternal(NULL, newString);
    _XmStringFree(newString);

    return ret;
}

XmString 
XmStringCreate(char *text, char *tag)
{
    _XmString str;
    XmString ret;

    if (text && tag && strcmp(tag, XmFONTLIST_DEFAULT_TAG) != 0)
    {
	str = __XmAllocNewXmString(2);

	str->components[0]->type = XmSTRING_COMPONENT_CHARSET;
	str->components[0]->length = strlen(tag);
	str->components[0]->data = XtNewString(tag);

	str->components[1]->type = XmSTRING_COMPONENT_TEXT;
	str->components[1]->length = strlen(text);
	str->components[1]->data = XtNewString(text);
    }
    else if (text) {
	str = __XmAllocNewXmString(1);

	str->components[0]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	str->components[0]->length = strlen(text);
	str->components[0]->data = XtNewString(text);
    }
    else
	return NULL;

    ret = _XmStringCreateExternal(NULL, str);
    _XmStringFree(str);

    return ret;
}

XmString 
XmStringCreateLtoR(char *text, char *tag)
{
    XmString ret;
    _XmString str;
    char *t;
    char *p, *b;

    if (text == NULL)
	return NULL;

    t = XtNewString(text);
    p = strstr(t,"\n");

    if (p) /* if there was actually a return character in the string. */
    {
	Boolean at_end;

	if (*(p+1))
	    at_end = False;
	else
	    at_end = True;

	*p = 0;

	ret = XmStringSegmentCreate(t, tag,
				    XmSTRING_DIRECTION_L_TO_R, True);

	str = _XmStringCreate(ret);

	XmStringFree(ret);

	while (!at_end)
	{
	    Boolean need_text;
	    Boolean need_sep;

	    p++;
	    b = p;

	    if ((p = strstr(b, "\n")) == NULL) {
		at_end = True;
		need_sep = False;
	    }
	    else {
		*p = 0;
		need_sep = True;
		if (*(p+1))
		    at_end = False;
		else
		    at_end = True;
	    }

	    if ((p != NULL && p != b) || strlen(b) != 0)
		need_text = True;
	    else
		need_text = False;

	    if (need_text) {

		__XmGrowXmString(str);

		if (strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0)
		    str->components[str->number_of_components-1]->type =
			XmSTRING_COMPONENT_LOCALE_TEXT;
		else
		    str->components[str->number_of_components-1]->type =
			XmSTRING_COMPONENT_TEXT;
		str->components[str->number_of_components-1]->length =
		    strlen(b);
		str->components[str->number_of_components-1]->data =
		    XtNewString(b);
	    }

	    if (need_sep) {
		__XmGrowXmString(str);

		str->components[str->number_of_components-1]->type =
		    XmSTRING_COMPONENT_SEPARATOR;
		str->components[str->number_of_components-1]->length = 0;
		str->components[str->number_of_components-1]->data = NULL;
	    }
	}

	ret = _XmStringCreateExternal(NULL, str);
    }
    else
	ret = XmStringSegmentCreate(text, tag,
				    XmSTRING_DIRECTION_L_TO_R, False);

    XtFree(t);

    return ret;
}

XmString
XmStringLtoRCreate(char *text, char *tag)
{
    return XmStringCreateLtoR(text, tag);
}

XmString 
XmStringCreateLocalized(char *text)
{
    return XmStringCreate(text, XmFONTLIST_DEFAULT_TAG);
}

XmString 
XmStringCreateSimple(char *text)
{
    _XmString str;
    XmString ret;

    if (!text)
	return NULL;

    str = __XmAllocNewXmString(2);

    str->components[0]->type = XmSTRING_COMPONENT_CHARSET;
    str->components[0]->length = strlen(XmFONTLIST_DEFAULT_TAG);
    str->components[0]->data = XtNewString(XmFONTLIST_DEFAULT_TAG);

    str->components[1]->type = XmSTRING_COMPONENT_TEXT;
    str->components[1]->length = strlen(text);
    str->components[1]->data = XtNewString(text);

    ret = _XmStringCreateExternal(NULL, str);

    _XmStringFree(str);

    return ret;
}

void 
XmStringDraw(Display *d, 
	     Window w,
	     XmFontList fontlist,
	     XmString string,
	     GC gc,
	     Position x,
	     Position y,
	     Dimension width,
	     unsigned char alignment,
	     unsigned char layout_direction,
	     XRectangle *clip)
{
    _XmString str;

    if (!_XmStringIsXmString(string))
	return;

    str = _XmStringCreate(string);

    _XmStringDraw(d, w, fontlist, str, gc, x, y, width,
		  alignment, layout_direction, clip);

    _XmStringFree(str);
}

void 
XmStringDrawImage(Display *d,
		  Window w,
		  XmFontList fontlist,
		  XmString string,
		  GC gc,
		  Position x,
		  Position y,
		  Dimension width,
		  unsigned char alignment,
		  unsigned char layout_direction,
		  XRectangle *clip)
{
    _XmString str;

    if (!_XmStringIsXmString(string))
	return;

    str = _XmStringCreate(string);

    _XmStringDrawImage(d, w, fontlist, str, gc, x, y, width,
		       alignment, layout_direction, clip);

    _XmStringFree(str);
}

/*
 * Similar to XmStringDraw, but with an additional parameter
 *
 * If the underline string is contained in string, then that part
 * of string is underlined.
 */
void 
XmStringDrawUnderline(Display *d,
		      Window w,
		      XmFontList fontlist,
		      XmString string,
		      GC gc,
		      Position x,
		      Position y,
		      Dimension width,
		      unsigned char alignment,
		      unsigned char layout_direction,
		      XRectangle *clip,
		      XmString underline)
{
    _XmString str, und;

    if (!_XmStringIsXmString(string))
	return;

    str = _XmStringCreate(string);
    und = _XmStringCreate(underline);

    _XmStringDrawUnderline(d, w, fontlist, str, gc, x, y, width,
			   alignment, layout_direction, clip, und);

    _XmStringFree(str);
}

Boolean 
XmStringEmpty(XmString s1)
{
    _XmString str;
    Boolean ret;

    if (!_XmStringIsXmString(s1))
	return True;

    str = _XmStringCreate(s1);

    ret = _XmStringEmpty(str);

    _XmStringFree(str);

    return ret;
}

void 
XmStringExtent(XmFontList fontlist, 
	       XmString string,
	       Dimension *width,
	       Dimension *height)
{
    _XmString str;

    *width = *height = 0;

    if (!_XmStringIsXmString(string))
	return;

    str = _XmStringCreate(string);

    _XmStringUpdate(fontlist, str);

    _XmStringExtent(fontlist, str, width, height);

    _XmStringFree(str);
}

/*
 * according to Motif, this does only return the first segment in a
 * multi-segment string
 */
Boolean 
XmStringGetLtoR(XmString string,
		XmStringCharSet tag,
		char **text)
{
    Boolean Found = False;
    XmStringContext context = NULL;

    if (!_XmStringIsXmString(string))
	return False;

    XmStringInitContext(&context, string);
    
    while (XmStringGetNextSegment(context, NULL, NULL, NULL, NULL))
    {
       if (context->text && context->charset && tag &&
	   strcmp(context->charset, tag) == 0)
       {
	  *text = XtNewString(context->text);
  	  Found = True;
          break;
       }
	/*
	 * Provide some kind of compatibility between 1.1's
	 * XmSTRING_DEFAULT_CHARSET and 1.2's XmFONTLIST_DEFAULT_TAG
	 */
	if (context->charset &&
	    strcmp(context->charset, XmFONTLIST_DEFAULT_TAG) == 0 &&
	    strcmp(tag, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    *text = XtNewString(context->text);
	    Found = True;
	    break;
	}
	if (context->charset &&
	    strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0 &&
	    strcmp(context->charset, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    *text = XtNewString(context->text);
	    Found = True;
	    break;
	}
    }

    XmStringFreeContext(context);
    return Found;
}

void 
XmStringFree(XmString string)
{
    if (!_XmStringIsXmString(string))
	return;

    XtFree((char *)string);
}

Boolean 
XmStringInitContext(XmStringContext *context,
		    XmString string)
{
    if (!_XmStringIsXmString(string))
	return False;

    *context = (XmStringContext)XtCalloc(1, sizeof(struct _XmtStringContextRec));
    (*context)->string = _XmStringCreate(string);
    (*context)->current_component = -1;

    return True;
}

XmStringComponentType 
XmStringGetNextComponent(XmStringContext context,
			 char **text,
			 XmStringCharSet *tag,
			 XmStringDirection *direction,
			 XmStringComponentType *unknown_tag,
			 unsigned short *unknown_length,
			 unsigned char **unknown_value)
{
    _XmStringComponent r;
 
    if (context == NULL)
	return XmSTRING_COMPONENT_UNKNOWN;

    context->current_component++;

    if (context->current_component < context->string->number_of_components) {
    	r = context->string->components[context->current_component];

    	switch (r->type) {
	case XmSTRING_COMPONENT_CHARSET:
	    if (r->data)
		*text = XtNewString(r->data);
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    if (r->data)
		*tag = XtNewString(r->data);
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    *direction = r->data[0];
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	case XmSTRING_COMPONENT_END:
	case XmSTRING_COMPONENT_USER_BEGIN:
	case XmSTRING_COMPONENT_USER_END:
	    *unknown_tag = r->type;
	    *unknown_length = r->length;
	    *unknown_value = (unsigned char *)XtMalloc(r->length);
	    bcopy(r->data, *unknown_value, r->length);
	    break;

	default:
               _XmWarning(NULL,
			  "XmStringGetNextComponent: unknown type %d\n", r);
	}
	return r->type;
    }
    else
	return XmSTRING_COMPONENT_UNKNOWN;
}

XmStringComponentType 
XmStringPeekNextComponent(XmStringContext context)
{
    if (context == NULL)
	return XmSTRING_COMPONENT_UNKNOWN;

    if (context->current_component < (context->string->number_of_components - 1))
	return context->string->components[context->current_component + 1]->type;
    else
	return XmSTRING_COMPONENT_UNKNOWN;
}

/*
 * Assumption: if you change charsets or directions before a separator,
 * then that is another segment.  I don't know if this is right, but it
 * makes sense? MLM
 */
Boolean 
XmStringGetNextSegment(XmStringContext context,
		       char **text,
		       XmStringCharSet *tag,
		       XmStringDirection *direction,
		       Boolean *separator)
{
    Boolean ret;

    ret = _XmStringGetNextSegment((_XmStringContext)context,
				  &context->charset, &context->direction, 
				  &context->text, &context->textlen,
				  &context->separator);

    if (!ret)
	return False;

    if (text && context->text)
	*text = XtNewString(context->text);

    if (tag)
	*tag = XtNewString(context->charset);

    if (direction)
	*direction = context->direction;

    if (separator)
	*separator = context->separator;

    return True;
}

void 
XmStringFreeContext(XmStringContext context)
{
    _XmStringFree(context->string);

    XtFree((char *)context);
}

Boolean 
XmStringHasSubstring(XmString string,
		     XmString substring)
{
    _XmString str, substr;
    Boolean ret;

    if (!_XmStringIsXmString(string) || !_XmStringIsXmString(substring))
	return False;

    str = _XmStringCreate(string);
    substr = _XmStringCreate(substring);

    ret = _XmStringHasSubstring(str, substr);

    _XmStringFree(str);
    _XmStringFree(substr);

    return ret;
}

Dimension 
XmStringWidth(XmFontList fontlist,
	      XmString string)
{
    Dimension width, height;

    XmStringExtent(fontlist, string, &width, &height);

    return width;
}

Dimension 
XmStringHeight(XmFontList fontlist, XmString string)
{
    Dimension width, height;

    XmStringExtent(fontlist, string, &width, &height);

    return height;
}

int 
XmStringLength(XmString s1)
{
    unsigned length, i;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)s1;
    unsigned char *next;

    if (!_XmStringIsXmString(s1))
	return 0;

    /* skip header */
    next = str->data;
    str = (struct __XmStringExtRec *)next;
    length = 0;

    if (str->len > XmSTRING_LENGTH) {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned))
		return 0;
	}
    }
    else
	length = str->len & ~XmSTRING_LENGTH;

    return length + XmSTRING_HEADER_SIZE;
}

int 
XmStringLineCount(XmString string)
{
    _XmString str;
    int lc;

    if (!_XmStringIsXmString(string))
	return 0;

    str = _XmStringCreate(string);

    lc = _XmStringLineCount(str);

    _XmStringFree(str);

    return lc;
}

