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
#include "color.h"
#include "k_I.h"
#include "k_node.h"
#include "widgets.h"
#include "xutil.h"

void alloc_hot_pixel(KILL_NODE *node, int popup)
{
    XColor	col;

    if (!node->color || !node->hot)
	return;

    if (!XParseColor(display, global.cmap, node->color, &col)) {
	if (popup)
	    popup_colornotice(True);
	else
	    fprintf(stderr, "knews: Bogus color in kill"
		    "file: \"%s\"\n", node->color);
	node->pixel = global.default_hot_pixel;
	node->alloced_pixel = False;
    } else if (XAllocColor(display, global.cmap, &col)) {
	node->pixel = col.pixel;
	node->alloced_pixel = True;
    } else {
	if (popup)
	    popup_colornotice(False);
	else
	    fprintf(stderr, "knews: Cannot allocate "
		    "colormap entry for \"%s\"\n", node->color);
	node->pixel = get_closest_color(&col);
	node->alloced_pixel = False;
    }

    fix_node_pixmap(node);
}

void fix_node_pixmap(KILL_NODE *node)
{
    GC	gc = DefaultGCOfScreen(XtScreen(main_widgets.shell));

    if (node->pixmap == None)
	node->pixmap =
	    XCreatePixmap(display, XtWindow(main_widgets.shell),
			  HOT_PIXMAP_SIZE, HOT_PIXMAP_SIZE, global.depth);
    XSetForeground(display, gc, node->pixel);
    XFillRectangle(display, node->pixmap, gc, 0, 0,
		   HOT_PIXMAP_SIZE, HOT_PIXMAP_SIZE);
}

KILL_NODE *parse_kill_line(char *line, int is_global)
{
    KILL_NODE	*node;
    char	*group, *expr;
    regex_t	*expr_re, *group_re;
    int		field, scope, hot;

    if (!(group = strstr(line, "||")) || !(expr  = strstr(group + 2, "||"))) {
	fprintf(stderr, "Parse error in kill entry: %s\n", line);
	return NULL;
    }

    switch (line[0]) {
    case 'M':
    case 'm':
	field = KillFieldMsgid;
	break;
    case 'S':
    case 's':
	field = KillFieldSubject;
	break;
    case 'F':
    case 'f':
	field = KillFieldFrom;
	break;
    case 'X':
    case 'x':
	field = KillFieldXref;
	break;
    default:
	fprintf(stderr, "Bad field in kill entry: %s\n", line);
	return NULL;
    }

    switch (line[1]) {
    case 'A':
    case 'a':
	scope = KillScopeArticle;
	break;
    case 'S':
    case 's':
	scope = KillScopeSubject;
	break;
    case 'T':
	scope = KillScopeThread;
	break;
    case 't':
	scope = KillScopeSubthread;
	break;
    default:
	fprintf(stderr, "Bad scope in kill entry: %s\n", line);
	return NULL;
    }

    if (line[2] == '\0') {
	fprintf(stderr, "Bad action in kill entry: %s\n", line);
	return NULL;
    }

    hot = (line[2] == 'h' || line[2] == 'H');

    *group++ = '\0';
    *group++ = '\0';
    *expr++  = '\0';
    *expr++  = '\0';
    if (*group == '\0' || !is_global)
	group = NULL;

    if (field == KillFieldMsgid) {
	expr_re = NULL;
	if (expr[0] != '<' || expr[strlen(expr) - 1] != '>') {
	    fprintf(stderr, "Bad message-id in kill file: %s\n", expr);
	    return NULL;
	}
    } else {
	expr_re = (regex_t *)XtMalloc(sizeof *expr_re);
	if (regcomp(expr_re, expr, REGEXP_COMPILE_FLAGS) != 0) {
	    fprintf(stderr, "Parse error in expression regexp: %s\n", expr);
	    XtFree((char *)expr_re);
	    return NULL;
	}
    }

    if (!group)
	group_re = NULL;
    else {
	group_re = (regex_t *)XtMalloc(sizeof *group_re);
	if (regcomp(group_re, group, REGEXP_COMPILE_FLAGS) != 0) {
	    fprintf(stderr, "Parse error in group regexp: %s\n", group);
	    XtFree((char *)expr_re);
	    XtFree((char *)group_re);
	    return NULL;
	}
    }

    node = (KILL_NODE *)XtMalloc(sizeof *node);
    node->expr_str      = XtNewString(expr);
    node->expr_re       = expr_re;
    node->group_str     = group ? XtNewString(group) : NULL;
    node->group_re      = group_re;
    node->color         = hot ? XtNewString(line + 3) : NULL;
    node->pixel         = global.default_hot_pixel;
    node->pixmap	= None;
    node->field         = field;
    node->scope         = scope;
    node->hot           = hot;
    node->expired       = False;
    node->alloced_pixel = False;

    if (hot)
	alloc_hot_pixel(node, False);

    return node;
}

void fprint_kill_node(FILE *fp, KILL_NODE *node, int expire)
{
    if (expire && node->expired)
	return;

    fprintf(fp, "%c%c%c%s||%s||%s\n",
	    "MSFX"[node->field], "ASTt"[node->scope], "KH"[node->hot],
	    node->color ? node->color : "",
	    node->group_str ? node->group_str : "",
	    node->expr_str ? node->expr_str : "");
}

void sprint_kill_node(KILL_NODE *node, char *buffer, long len)
{
    long	n;

    len--;

    buffer[0] = node->expired ? '!' : ' ';
    buffer[1] = "MSFX"[node->field];
    buffer[2] = "ASTf"[node->scope];
    buffer[3] = "KH"[node->hot];
    buffer[4] = ' ';
    buffer[5] = '\0';
    buffer += 5;
    len -= 5;

#define MAX_GROUP_LEN	20
    if (node->group_str) {
	n = strlen(node->group_str);
	if (n < MAX_GROUP_LEN) {
	    memcpy(buffer, node->group_str, n);
	    memset(buffer + n, ' ', MAX_GROUP_LEN - n);
	} else {
	    memcpy(buffer, node->group_str, MAX_GROUP_LEN - 3);
	    memcpy(buffer + MAX_GROUP_LEN - 3, "...", 3);
	}
	buffer += MAX_GROUP_LEN;
	*buffer++ = ' ';
	*buffer = '\0';
	len -= MAX_GROUP_LEN + 1;
    }

    if (node->expr_str) {
	n = strlen(node->expr_str);
	if (n > len)
	    n = len;
	memcpy(buffer, node->expr_str, n);
	buffer[n] = '\0';
    }
}

void free_kill_node(KILL_NODE *node)
{
    XtFree(node->expr_str);
    XtFree(node->group_str);
    XtFree(node->color);
    node->expr_str  = NULL;
    node->group_str = NULL;
    node->color     = NULL;
    if (node->expr_re) {
	regfree(node->expr_re);
	XtFree((char *)node->expr_re);
	node->expr_re = NULL;
    }
    if (node->group_re) {
	regfree(node->group_re);
	XtFree((char *)node->group_re);
	node->group_re = NULL;
    }
    if (node->alloced_pixel) {
	unsigned long	pixel = node->pixel;

	XFreeColors(display, global.cmap, &pixel, 1, 0);
	node->alloced_pixel = False;
    }
    if (node->pixmap != None) {
	XFreePixmap(display, node->pixmap);
	node->pixmap = None;
    }
    XtFree((char *)node);
}
