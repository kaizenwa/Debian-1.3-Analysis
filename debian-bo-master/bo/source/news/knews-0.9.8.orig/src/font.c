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
#include "font.h"
#include "resource.h"
#include "util.h"
#include "widgets.h"

#define FONT_HASH_SIZE	7

typedef struct FontHashNode {
    struct FontHashNode	*next;
    char		*name;
    MimeFont		f;
} FontHashNode;

static FontHashNode	*font_table[FONT_HASH_SIZE];
MimeFont		*ascii_font;
MimeFont		*default_font;

static unsigned int hash(char *name)
{
    unsigned int	result = 0;

    while (*name != '\0')
	result += (unsigned char)*name++;

    return result % FONT_HASH_SIZE;
}

static MimeFont *font_hash_find(char *name)
{
    FontHashNode	*loop;

    loop = font_table[hash(name)];

    while (loop) {
	if (loop->name[0] == name[0] && strcmp(loop->name, name) == 0)
	    return &loop->f;
	loop = loop->next;
    }

    return NULL;
}

static MimeFont *font_hash_insert(char *name, MimeFont *font)
{
    FontHashNode	*node, *loop;
    unsigned int	index;

    index = hash(name);

    node = (FontHashNode *)XtMalloc(sizeof *node);
    node->next = NULL;
    node->name = XtNewString(name);
    node->f = *font;

    loop = font_table[index];
    if (!loop)
	font_table[index] = node;
    else {
	while (loop->next)
	    loop = loop->next;
	loop->next = node;
    }

    return &node->f;
}

void init_fonts(Widget top)
{
    static XtResource	ascii_spec[] = {
#define offset(field) XtOffsetOf(MimeFont, field)
	{"bodyFont", "Font", XtRFontStruct, sizeof(XFontStruct*),
	 offset(body_font), XtRString, (XtPointer)XtDefaultFont},
	{"quoteFont", "Font", XtRFontStruct, sizeof(XFontStruct*),
	 offset(quote_font), XtRString, (XtPointer)XtDefaultFont},
	{"headerFont", "Font", XtRFontStruct, sizeof(XFontStruct*),
	 offset(header_font), XtRString, (XtPointer)XtDefaultFont},
	{"listFont", "Font", XtRFontStruct, sizeof(XFontStruct*),
	 offset(list_font), XtRString, (XtPointer)XtDefaultFont},
	{"treeFont", "Font", XtRFontStruct, sizeof(XFontStruct*),
	 offset(tree_font), XtRString, (XtPointer)XtDefaultFont},
#undef offset
    };
    MimeFont	f = {NULL, };

    XtGetSubresources(top, &f, "us-ascii", "US-ASCII",
		      ascii_spec, XtNumber(ascii_spec), NULL, 0);
    ascii_font = font_hash_insert("us-ascii", &f);

    if (!ascii_font->body_font) {
	fputs("knews: Resource knews.us-ascii.font is NULL!  "
	      "This ain't happening!\n", stderr);
	exit(1);
    }

    if (!ascii_font->quote_font)
	ascii_font->quote_font = ascii_font->body_font;
    if (!ascii_font->header_font)
	ascii_font->header_font = ascii_font->body_font;
    if (!ascii_font->list_font)
	ascii_font->list_font = ascii_font->body_font;
    if (!ascii_font->header_font)
	ascii_font->tree_font = ascii_font->body_font;
}

static MimeFont *load_font(char *name)
{
    struct FontNames {
	String	body_font;
	String	quote_font;
	String	header_font;
	String	list_font;
	String	tree_font;
	String	encoding;
	String	head_enc_hack;
    } font_res = {0, };
    static XtResource	font_spec[] = {
#define offset(field) XtOffsetOf(struct FontNames, field)
	{"bodyFont", NULL, XtRString, sizeof(String),
	 offset(body_font), XtRImmediate, (XtPointer)NULL},
	{"quoteFont", NULL, XtRString, sizeof(String),
	 offset(quote_font), XtRImmediate, (XtPointer)NULL},
	{"headerFont", NULL, XtRString, sizeof(String),
	 offset(header_font), XtRImmediate, (XtPointer)NULL},
	{"listFont", NULL, XtRString, sizeof(String),
	 offset(list_font), XtRImmediate, (XtPointer)NULL},
	{"treeFont", NULL, XtRString, sizeof(String),
	 offset(tree_font), XtRImmediate, (XtPointer)NULL},
	{"encoding", "Encoding", XtRString, sizeof(String),
	 offset(encoding), XtRImmediate, (XtPointer)NULL},
	{"headEncHack", "HeadEncHack", XtRString, sizeof(String),
	 offset(head_enc_hack), XtRImmediate, (XtPointer)NULL},
#undef offset
    };
    MimeFont	font;
    XFontStruct	*def;
    char	class[16];
    char	*c;
    int		iso_num;
    int		i;

    if (strncmp(name, "iso-8859-", 9) == 0 &&
	name[9] >= '1' && name[9] <= '9' && name[10] == '\0') {
	iso_num = name[9] - '0';
	sprintf(class, "iso-latin-%d", iso_num);
	c = "IsoFont";
    } else {
	iso_num = 0;
	c = "MimeFont";
    }

    for (i = 0 ; i < 5 ; i++)
	font_spec[i].resource_class = c;

    XtGetSubresources(main_widgets.shell, &font_res,
		      name, iso_num > 0 ? class : name,
		      font_spec, XtNumber(font_spec), NULL, 0);

#define DO(name)                                                   \
    if (!font_res.name)                                            \
	font.name = NULL;                                          \
    else {                                                         \
	font.name = XLoadQueryFont(display, font_res.name);        \
	if (!font.name)                                            \
	    fprintf(stderr, "knews: couldn't load font %s.\n",     \
		    font_res.name);                                \
    }

    DO(body_font);
    DO(quote_font);
    DO(header_font);
    DO(list_font);
    DO(tree_font);
    font.head_enc_hack = font_res.head_enc_hack != NULL;

    font.funcs = get_decode_funcs(font_res.encoding);
    if (font.funcs && !font.funcs->init)
	fprintf(stderr, "knews: unknown encoding %s for charset %s\n",
		font_res.encoding, name);

    def = font.body_font;
    if (!def)
	def = font.quote_font;
    if (!def && !font.head_enc_hack)
	def = font.header_font;

    if (def) {
	if (!font.body_font)
	    font.body_font = def;
	if (!font.quote_font)
	    font.quote_font = def;
	if (!font.header_font)
	    font.header_font = def;
    }

    return font_hash_insert(name, &font);
}

static MimeFont *find_font(char *name)
{
    MimeFont	*font;

    if (!name || *name == '\0')
	return NULL;

    ascii_lower(name);
    font = font_hash_find(name);
    if (!font)
	font = load_font(name);

    return font;
}

MimeFont *get_font(char *name)
{
    MimeFont	*font;

    font = find_font(name);
    if (font && (!font->body_font || (font->funcs && !font->funcs->init)))
	font = NULL;

    return font;
}

void font_enter_group(void)
{
    char	*charset = res_default_charset();
    XFontStruct	*font;
    Arg		arg;

    if (!charset)
	default_font = ascii_font;
    else {
	default_font = find_font(charset);
	if (!default_font) {
	    fprintf(stderr, "knews: no font for charset %s, "
		    "falling back to us-ascii.\n", charset);
	    res_set_default_charset(NULL);
	    default_font = ascii_font;
	}
    }

    font = default_font->list_font;
    if (!font)
	font = ascii_font->list_font;
    XtSetArg(arg, XtNfont, font);
    XtSetValues(main_widgets.thread_list, &arg, 1);
    font = default_font->tree_font;
    if (!font)
	font = ascii_font->tree_font;
    XtSetArg(arg, XtNfont, font);
    XtSetValues(main_widgets.arttree, &arg, 1);

    if (!default_font->body_font)
	default_font = ascii_font;
}
