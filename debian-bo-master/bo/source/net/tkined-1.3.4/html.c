/*
 * html.c
 *
 * This file contains all functions that are needed to parse a HTML
 * character stream and to replace it into a list of tagged texts.
 *
 * Copyright (c) 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "tkined.h"

/*
 * Structure used to hold a table of known ampersand escapes:
 */

typedef struct HtmlAmpEsc {
	char *tag;
	char value;
} HtmlEscape;

static HtmlEscape htmlEscapes[] = {
	{"lt;",		'<'},
	{"LT;",		'<'},
	{"gt;",		'>'},
	{"GT;",		'>'},
	{"amp;",	'&'},
	{"AMP;",	'&'},
	{"quot;",	'\"'},
	{"QUOT;",	'\"'},
	{"Agrave;",	'\300'},
	{"Aacute;",	'\301'},
	{"Acirc;",	'\302'},
	{"Atilde;",	'\303'},
	{"Auml;",	'\304'},
	{"Aring;",	'\305'},
	{"AElig;",	'\306'},
	{"Ccedil;",	'\307'},
	{"Egrave;",	'\310'},
	{"Eacute;",	'\311'},
	{"Ecirc;",	'\312'},
	{"Euml;",	'\313'},
	{"Igrave;",	'\314'},
	{"Iacute;",	'\315'},
	{"Icirc;",	'\316'},
	{"Iuml;",	'\317'},
	{"ETH;",	'\320'},
	{"Ntilde;",	'\321'},
	{"Ograve;",	'\322'},
	{"Oacute;",	'\323'},
	{"Ocirc;",	'\324'},
	{"Otilde;",	'\325'},
	{"Ouml;",	'\326'},

	{"?;",		'\327'},	/* ? */

	{"Oslash;",	'\330'},
	{"Ugrave;",	'\331'},
	{"Uacute;",	'\332'},
	{"Ucirc;",	'\333'},
	{"Uuml;",	'\334'},
	{"Yacute;",	'\335'},
	{"THORN;",	'\336'},
	{"szlig;",	'\337'},
	{"agrave;",	'\340'},
	{"aacute;",	'\341'},
	{"acirc;",	'\342'},
	{"atilde;",	'\343'},
	{"auml;",	'\344'},
	{"aring;",	'\345'},
	{"aelig;",	'\346'},
	{"ccedil;",	'\347'},
	{"egrave;",	'\350'},
	{"eacute;",	'\351'},
	{"ecirc;",	'\352'},
	{"euml;",	'\353'},
	{"igrave;",	'\354'},
	{"iacute;",	'\355'},
	{"icirc;",	'\356'},
	{"iuml;",	'\357'},
	{"eth;",	'\360'},
	{"ntilde;",	'\361'},
	{"ograve;",	'\362'},
	{"oacute;",	'\363'},
	{"ocirc;",	'\364'},
	{"otilde;",	'\365'},
	{"ouml;",	'\366'},

	{"?;",		'\367'},	/* ? */

	{"oslash;",	'\370'},
	{"ugrave;",	'\371'},
	{"uacute;",	'\372'},
	{"ucirc;",	'\373'},
	{"uuml;",	'\374'},
	{"yacute;",	'\375'},
	{"thorn;",	'\376'},
	{"yuml;",	'\377'},

	{NULL,		'\0'},
};

/*
 * The list of known markup tags...
 */

#define	M_UNKNOWN	-1
#define	M_NONE		0
#define	M_TITLE		1
#define	M_HEADER_1	2
#define	M_HEADER_2	3
#define	M_HEADER_3	4
#define	M_HEADER_4	5
#define	M_HEADER_5	6
#define	M_HEADER_6	7
#define	M_ANCHOR	8
#define	M_PARAGRAPH	9
#define	M_ADDRESS	10
#define	M_PLAIN_TEXT	11
#define	M_UNUM_LIST	12
#define	M_NUM_LIST	13
#define	M_LIST_ITEM	14
#define	M_DESC_LIST	15
#define	M_DESC_TITLE	16
#define	M_DESC_TEXT	17
#define	M_PREFORMAT	18
#define	M_PLAIN_FILE	19
#define M_LISTING_TEXT	20
#define M_INDEX		21
#define M_MENU		22
#define M_DIRECTORY	23
#define M_IMAGE		24
#define M_FIXED		25
#define M_BOLD		26
#define M_ITALIC	27
#define M_EMPHASIZED	28
#define M_STRONG	29
#define M_CODE		30
#define M_SAMPLE	31
#define M_KEYBOARD	32
#define M_VARIABLE	33
#define M_CITATION	34
#define M_BLOCKQUOTE	35
#define M_STRIKEOUT	36
#define M_INPUT		37
#define M_FORM		38
#define M_HRULE		39
#define M_LINEBREAK	40
#define M_BASE		41
#define M_SELECT	42
#define M_OPTION	43
#define M_TEXTAREA	44

/*
 * ... and the list of tag names.
 */

#define	MT_TITLE	"TITLE"
#define	MT_HEADER_1	"H1"
#define	MT_HEADER_2	"H2"
#define	MT_HEADER_3	"H3"
#define	MT_HEADER_4	"H4"
#define	MT_HEADER_5	"H5"
#define	MT_HEADER_6	"H6"
#define	MT_ANCHOR	"A"
#define	MT_PARAGRAPH	"P"
#define	MT_ADDRESS	"ADDRESS"
#define	MT_PLAIN_TEXT	"XMP"
#define	MT_UNUM_LIST	"UL"
#define	MT_NUM_LIST	"OL"
#define	MT_LIST_ITEM	"LI"
#define	MT_DESC_LIST	"DL"
#define	MT_DESC_TITLE	"DT"
#define	MT_DESC_TEXT	"DD"
#define	MT_PREFORMAT	"PRE"
#define	MT_PLAIN_FILE	"PLAINTEXT"
#define MT_LISTING_TEXT	"LISTING"
#define MT_INDEX	"ISINDEX"
#define MT_MENU		"MENU"
#define MT_DIRECTORY	"DIR"
#define MT_IMAGE	"IMG"
#define MT_FIXED	"TT"
#define MT_BOLD		"B"
#define MT_ITALIC	"I"
#define MT_EMPHASIZED	"EM"
#define MT_STRONG	"STRONG"
#define MT_CODE		"CODE"
#define MT_SAMPLE	"SAMP"
#define MT_KEYBOARD	"KBD"
#define MT_VARIABLE	"VAR"
#define MT_CITATION	"CITE"
#define MT_BLOCKQUOTE	"BLOCKQUOTE"
#define MT_STRIKEOUT	"STRIKE"
#define MT_INPUT	"INPUT"
#define MT_FORM		"FORM"
#define MT_HRULE	"HR"
#define MT_LINEBREAK	"BR"
#define MT_BASE		"BASE"
#define MT_SELECT	"SELECT"
#define MT_OPTION	"OPTION"
#define MT_TEXTAREA	"TEXTAREA"

/*
 * A table used to match tag names to internal tags etc.
 */

typedef struct HtmlTag {
    char *label;
    int type;
    int len;
} HtmlTag;

/*
 * Structure to hold a stack of open HtmlTags.
 */

typedef struct HtmlTagList {
    HtmlTag *tag;
    struct HtmlTagList *nextPtr;
} HtmlTagList;

/*
 * The table of all know HTML tags.
 */

static HtmlTag tagTable[] = {
    { NULL,		M_UNKNOWN,	0},
    { MT_TITLE,		M_TITLE,	0},
    { MT_HEADER_1,	M_HEADER_1,	0},
    { MT_HEADER_2,	M_HEADER_2,	0},
    { MT_HEADER_3,	M_HEADER_3,	0},
    { MT_HEADER_4,	M_HEADER_4,	0},
    { MT_HEADER_5,	M_HEADER_5,	0},
    { MT_HEADER_6,	M_HEADER_6,	0},
    { MT_ANCHOR,	M_ANCHOR,	0},
    { MT_PARAGRAPH,	M_PARAGRAPH,	0},
    { MT_ADDRESS,	M_ADDRESS,	0},
    { MT_PLAIN_TEXT,	M_PLAIN_TEXT,	0},
    { MT_UNUM_LIST,	M_UNUM_LIST,	0},
    { MT_NUM_LIST,	M_NUM_LIST,	0},
    { MT_LIST_ITEM,	M_LIST_ITEM,	0},
    { MT_DESC_LIST,	M_DESC_LIST,	0},
    { MT_DESC_TITLE,	M_DESC_TITLE,	0},
    { MT_DESC_TEXT,	M_DESC_TEXT,	0},
    { MT_PREFORMAT,	M_PREFORMAT,	0},
    { MT_PLAIN_FILE,	M_PLAIN_FILE,	0},
    { MT_LISTING_TEXT,	M_LISTING_TEXT,	0},
    { MT_INDEX,		M_INDEX,	0},
    { MT_MENU,		M_MENU,		0},
    { MT_DIRECTORY,	M_DIRECTORY,	0},
    { MT_IMAGE,		M_IMAGE,	0},
    { MT_FIXED,		M_FIXED,	0},
    { MT_BOLD,		M_BOLD,		0},
    { MT_ITALIC,	M_ITALIC,	0},
    { MT_EMPHASIZED,	M_EMPHASIZED,	0},
    { MT_STRONG,	M_STRONG,	0},
    { MT_CODE,		M_CODE,		0},
    { MT_SAMPLE,	M_SAMPLE,	0},
    { MT_KEYBOARD,	M_KEYBOARD,	0},
    { MT_VARIABLE,	M_VARIABLE,	0},
    { MT_CITATION,	M_CITATION,	0},
    { MT_BLOCKQUOTE,	M_BLOCKQUOTE,	0},
    { MT_STRIKEOUT,	M_STRIKEOUT,	0},
    { MT_INPUT,		M_INPUT,	0},
    { MT_FORM,		M_FORM,		0},
    { MT_HRULE,		M_HRULE,	0},
    { MT_LINEBREAK,	M_LINEBREAK,	0},
    { MT_BASE,		M_BASE,		0},
    { MT_SELECT,	M_SELECT,	0},
    { MT_OPTION,	M_OPTION,	0},
    { MT_TEXTAREA,	M_TEXTAREA,	0},
    { NULL,		0,	0},
};


/*
 * Search for a know tag in the tag table above.
 */

static HtmlTag*
LookupTag (string)
    char *string;
{
    HtmlTag *tag;

    for (tag = tagTable+1; tag->label; tag++) {
	if (strcmp (string, tag->label) == 0) {
	    return tag;
	}
    }

    return tagTable;
}

/*
 * Remove all unneeded white space characters from the text buffer
 * and expand the html ampersand escapes.
 */

static void
RemoveWhiteSpaces (text)
     char *text;
{
    char *start = text;
    char *ptr = text;

    /* 
     * Eat up all white space before the first word.
     */
    
    while (isspace (*ptr)) {
        ptr++;
    }

    /*
     * Skip over the first word and expand html escape sequences.
     */
    
    while (*ptr && !isspace (*ptr)) {
	if (*ptr == '&') {
	    HtmlEscape *escape = htmlEscapes;
	    while (escape->tag) {
		int len = strlen (escape->tag);
		if (strncmp (ptr+1, escape->tag, len) == 0) {
		    *start++ = escape->value;
		    while (*ptr != ';') ptr++;
		    break;
		}
		escape++;
	    }
	    ptr++;
	} else {
	    *start++ = *ptr++;	    
	}
    }

    /*
     * Now loop until we find the end of the string.
     */

    while (*ptr) {

        /*
	 * Eat up all white space before the first word.
	 */
      
        while (isspace (*ptr)) {
	    ptr++;
	}
	if (! *ptr) break;

	/*
	 * Move the next word to the destination and expand escapes.
	 */
	
	if (start[-1] != '>') {	
	    *start++ = ' ';
	}
	while (*ptr && !isspace (*ptr)) {
	    if (*ptr == '&') {
		HtmlEscape *escape = htmlEscapes;
		while (escape->tag) {
		    int len = strlen (escape->tag);
		    if (strncmp (ptr+1, escape->tag, len) == 0) {
			*start++ = escape->value;
			while (*ptr != ';') ptr++;
			break;
		    }
		    escape++;
		}
		ptr++;
	    } else {
		*start++ = *ptr++;
	    }
	}
    }

    *start = '\0';
}

/*
 * Add the text with the given tag to the dynamic string.
 */

static void
AddTaggedText (dst, tag, text, length)
     Tcl_DString *dst;
     char *tag;
     char *text;
     int length;
{
    Tcl_DStringStartSublist (dst);
    Tcl_DStringAppendElement (dst, tag);
    if (text) {
	Tcl_DStringStartSublist (dst);
	Tcl_DStringAppend (dst, text, length);
	Tcl_DStringEndSublist (dst);
    } else {
	Tcl_DStringAppendElement (dst, "");
    }
    Tcl_DStringEndSublist (dst);
}

/*
 * Takes some text and a list of open HTML tags and converts it into a 
 * Tcl list stored in dstPtr. Returns a list of open HtmlTags, so that 
 * subsequent text fragments can be appended.
 */

HtmlTagList*
HTML_Parse (text, tagList, dst)
     char *text;
     HtmlTagList *tagList;
     Tcl_DString *dst;
{
    char *p = text;
    char *textStart = NULL;

    RemoveWhiteSpaces (text);

    buffer[0] = '\0';

    for (p = text; *p; p++) {

	if (*p != '<') {

	    if (!textStart) textStart = p;

	} else {

	    /*
	     * We have found a tag...
	     */

	    int len = 0;
	    char *textEnd = p;
	    p++;
	    while (*p && (*p != '>')) {
		buffer[len++] = toupper (*p++);
	    }
	    buffer[len] = '\0';
	    if (! *p) break;

	    /*
	     * Now we have to examine the tag...
	     */
	    
	    if (buffer[0] == '/') {
		HtmlTag *tag = LookupTag (buffer+1);
		if (tagList && (tagList->tag == tag)) {
		    HtmlTagList *oldTag = tagList;
		    tagList = tagList->nextPtr;
		    if (textStart) {
			AddTaggedText (dst, buffer+1,
				       textStart, textEnd-textStart);
		    }
		    ckfree ((char *) oldTag);
		} else {
		    fprintf (stderr, "unknown closing tag <%s>\n",
			     buffer);
		}
	    } else {
	        HtmlTag *tag = tag;
		HtmlTagList *newTag;
		if (textStart) {
		    AddTaggedText (dst, "", textStart, textEnd-textStart);
		}
		newTag = (HtmlTagList *) ckalloc (sizeof (HtmlTagList));
		newTag->nextPtr = tagList;
		newTag->tag = LookupTag (buffer);
		tagList = newTag;
	    }

	    textStart = NULL;
	}
    }

    if (textStart) {
	AddTaggedText (dst, "", textStart, p-textStart);
    }

    return tagList;
}
