/**
 *
 * $Id: DebugUtil.c,v 1.17 1996/12/18 07:26:50 u27113 Exp $
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

static char rcsid[] = "$Id: DebugUtil.c,v 1.17 1996/12/18 07:26:50 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>
#include <X11/Xos.h>
#include <Xm/GadgetP.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <Xm/VendorSEP.h>

/* Protect ourselves (for now) */

#ifdef	PRINT_STATE
static char *_XdbState(Widget w)
{
	if (XtIsRealized(w)) {
		if (XtIsManaged(w))
			return "realized, managed";
		else
			return "realized, not managed";
	} else {
		if (XtIsManaged(w))
			return "not realized, not managed";
		else
			return "not realized, not managed";
	}
}
#endif

static void _XdbPrintTree(Widget w, int level)
{
	int	i;
	CompositeWidget	cw = (CompositeWidget)w;

	if (w == NULL)
		return;

	for (i=0; i<level; i++)
		fprintf(stderr, "\t");
	fprintf(stderr, "%s : %p/%ld", XtName(w), w, XtWindow(w));
	fprintf(stderr, "(%s) geo %d %d %d %d",
		w->core.widget_class->core_class.class_name,
		XtX(w), XtY(w), XtWidth(w), XtHeight(w));
#ifdef	PRINT_STATE
	fprintf(stderr, " state: %s %s",
		_XdbState(w), w->core.mapped_when_managed ? "mwm": "");
#endif
	fprintf(stderr, "\n");
	if (XtIsSubclass(w, compositeWidgetClass))
		for (i=0; i<cw->composite.num_children; i++)
			_XdbPrintTree(cw->composite.children[i], level+1);

	for (i=0; i<cw->core.num_popups; i++)
		_XdbPrintTree(cw->core.popup_list[i], level+1);
}

void XdbPrintTree(Widget w)
{
	_XdbPrintTree(w, 0);
}

void XdbPrintCompleteTree(Widget w)
{
	Widget	ww = w;

	while (ww) {
		w = ww;
		ww = XtParent(w);
	}

	_XdbPrintTree(w, 0);
}

char * XdbGeometryResult2String(XtGeometryResult r)
{
	switch (r) {
	case XtGeometryYes:	return "Yes";
	case XtGeometryNo:	return "No";
	case XtGeometryAlmost:	return "Almost";
	case XtGeometryDone:	return "Done";
	default:		return "(invalid geometry result)";
	}
}

char *
XdbAttachment2String(int a)
{
	switch (a) {
	case XmATTACH_FORM:		return "XmATTACH_FORM";
	case XmATTACH_OPPOSITE_FORM:	return "XmATTACH_OPPOSITE_FORM";
	case XmATTACH_WIDGET:		return "XmATTACH_WIDGET";
	case XmATTACH_OPPOSITE_WIDGET:	return "XmATTACH_OPPOSITE_WIDGET";
	case XmATTACH_NONE:		return "XmATTACH_NONE";
	case XmATTACH_POSITION:		return "XmATTACH_POSITION";
	case XmATTACH_SELF:		return "XmATTACH_SELF";
	default :			return "(invalid attachment)";
	}
}

/*
 * See if 'fn' is one of the elements in 'l'.
 * L is a colon-separated list of source files.
 * The ".c" suffix for sources is not required (i.e. Form is equivalent
 * to Form.c).
 *
 * As of post-0.50, the "fn" can contain a directory prefix.
 */
static Boolean
ValidateSource(char *fn, char *l)
{
#ifndef	LESSTIF_PRODUCTION
	char	*s, *p, *q;
	int	i;

	if (l == NULL)
		return False;

	if (strcmp(l, "all") == 0)
		return True;

/* remove directory prefix */
	s = strrchr(fn, '/');
	if (s && *(s+1) != '\0')
		fn = s+1;
	i = strlen(fn);

	s = l;
	while ((p = strstr(s, fn)) != NULL) {
		q = p + i;
		if (*q == '\0' || *q == '.' || *q == ':')
			return True;
		s = q;
	}
#endif
	return False;
}

/*
 * This is the new style debugging support routine for LessTif.
 *
 * It allows us to choose at run time which sources should spit out
 * their guts.
 */
char *_XdbFindDebugSources(Widget w)
{
	static char *env = NULL;

	if (env == NULL)
		env = getenv("DEBUGSOURCES");

	return env;
}

static char *_XdbPrintWidgetID()
{
	static char *env = NULL;

	if (env == NULL)
		env = getenv("DEBUG_PRINT_WIDGETID");
	return env;
}

void XdbDebug(char *fn, Widget w, char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
	va_list	ap;
	char	*dbs = _XdbFindDebugSources(w);

	if (ValidateSource(fn, dbs)) {
		if (w)
		    if (_XdbPrintWidgetID())
			fprintf(stderr, "%s %s [%p]: ",
			    w->core.widget_class->core_class.class_name,
			    XtName(w), w);
		    else
			fprintf(stderr, "%s %s: ",
			    w->core.widget_class->core_class.class_name, XtName(w));
		else
		    fprintf(stderr, "(null widget): ");
		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
		fflush(stderr);
	}
#endif
}

void XdbDebug2(char *fn, Widget w, Widget c, char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
	va_list	ap;
	char	*dbs = _XdbFindDebugSources(w);

	if (ValidateSource(fn, dbs)) {
		if (w && c) {
		    if (_XdbPrintWidgetID())
			fprintf(stderr, "%s %s [%p] (child %s [%p]): ",
			    w->core.widget_class->core_class.class_name,
			    XtName(w), w, XtName(c), c);
		    else
			fprintf(stderr, "%s %s (child %s): ",
			    w->core.widget_class->core_class.class_name,
			    XtName(w), XtName(c));
		} else if (w) {
		    if (_XdbPrintWidgetID())
			fprintf(stderr, "%s %s [%p] (child NULL): ",
			    w->core.widget_class->core_class.class_name,
			    XtName(w), w);
		    else
			fprintf(stderr, "%s %s (child NULL): ",
			    w->core.widget_class->core_class.class_name,
			    XtName(w));
		} else {
		    fprintf(stderr, "(null widget): ");
		}

		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
	}
#endif
}

void XdbDebug0(char *fn, Widget w, char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
	va_list	ap;
	char	*dbs = _XdbFindDebugSources(w);

	if (ValidateSource(fn, dbs)) {
		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
	}
#endif
}

Boolean XdbInDebug(char *fn, Widget w)
{
#ifdef	LESSTIF_PRODUCTION
	return False;
#else
	return ValidateSource(fn, _XdbFindDebugSources(w));
#endif
}

char *
XdbMenuEnum2String(int f)
{
    switch(f) {
	case XmMENU_POPDOWN:		return "XmMENU_POPDOWN";
	case XmMENU_PROCESS_TREE:	return "XmMENU_PROCESS_TREE";
	case XmMENU_TRAVERSAL:		return "XmMENU_TRAVERSAL";
	case XmMENU_SHELL_POPDOWN:	return "XmMENU_SHELL_POPDOWN";
	case XmMENU_CALLBACK:		return "XmMENU_CALLBACK";
	case XmMENU_BUTTON:		return "XmMENU_BUTTON";
	case XmMENU_CASCADING:		return "XmMENU_CASCADING";
	case XmMENU_SUBMENU:		return "XmMENU_SUBMENU";
	case XmMENU_ARM:		return "XmMENU_ARM";
	case XmMENU_DISARM:		return "XmMENU_DISARM";
	case XmMENU_BAR_CLEANUP:	return "XmMENU_BAR_CLEANUP";
	case XmMENU_STATUS:		return "XmMENU_STATUS";
	case XmMENU_MEMWIDGET_UPDATE:	return "XmMENU_MEMWIDGET_UPDATE";
	case XmMENU_BUTTON_POPDOWN:	return "XmMENU_BUTTON_POPDOWN";
	case XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL:
					return "XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL";
	case XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL:
					return "XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL";
	case XmMENU_RESTORE_TEAROFF_TO_MENUSHELL:
					return "XmMENU_RESTORE_TEAROFF_TO_MENUSHELL";
	case XmMENU_GET_LAST_SELECT_TOPLEVEL:
					return "XmMENU_GET_LAST_SELECT_TOPLEVEL";
	case XmMENU_TEAR_OFF_ARM:	return "XmMENU_TEAR_OFF_ARM";
	default:			return "??";
    }
}

char *
XdbBoolean2String(int b)
{
	if (b)
		return "True";
	return "False";
}

char *
XdbXmString2String(XmString xms)
{
	char *s = NULL;

	XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s);

	return s;
}

char *
XdbPacking2String(unsigned char p)
{
	static char res[40];

	switch(p) {
	case XmPACK_COLUMN:	return "XmPACK_COLUMN";
	case XmPACK_TIGHT:	return "XmPACK_TIGHT";
	case XmPACK_NONE:	return "XmPACK_NONE";
	default :
		sprintf(res, "Invalid packing %d", p);
		return res;
	}
}

char *
XdbRcType2String(unsigned char t)
{
	static char res[40];

	switch(t) {
	case XmWORK_AREA:	return "XmWORK_AREA";
	case XmMENU_BAR:	return "XmMENU_BAR";
	case XmMENU_PULLDOWN:	return "XmMENU_PULLDOWN";
	case XmMENU_POPUP:	return "XmMENU_POPUP";
	case XmMENU_OPTION:	return "XmMENU_OPTION";
	default :
		sprintf(res, "Invalid RC Type %d", t);
		return res;
	}
}

char *
XdbWidgetGeometry2String(XtWidgetGeometry *g)
{
	static char	o1[128], o2[128], b[20], *out = NULL;
	int		i;

	if (g == NULL)
		return "NULL_GEOMETRY";
	if (g->request_mode == 0)
		return "GEOMETRY_NO_FIELDS";

/* Some magic to ensure you can call this sucker twice in one C function call */
	if (out == &o1[0])
		out = &o2[0];
	else
		out = &o1[0];

	out[0] = '\0';
	if (g->request_mode & CWX) {
		sprintf(b, "x %d ", g->x);
		strcat(out, b);
	}
	if (g->request_mode & CWY) {
		sprintf(b, "y %d ", g->y);
		strcat(out, b);
	}
	if (g->request_mode & CWWidth) {
		sprintf(b, "w %d ", g->width);
		strcat(out, b);
	}
	if (g->request_mode & CWHeight) {
		sprintf(b, "h %d ", g->height);
		strcat(out, b);
	}
	if (g->request_mode & CWBorderWidth) {
		sprintf(b, "bw %d ", g->border_width);
		strcat(out, b);
	}
	for (i=0; out[i]; i++) ;
	if (i > 0 && out[i-1] == ' ') out[i-1] = '\0';
	return out;
}

#define	XdbNONE		0
#define	XdbINT		1
#define	XdbSTRING	2
#define	XdbXMSTRING	3
#define	XdbCHAR		4

static struct {
	char	*name;
	int	t;
} XdbTypes [] = {
	{ XmNx, XdbINT },
	{ XmNy, XdbINT },
	{ XmNwidth, XdbINT },
	{ XmNheight, XdbINT },
	{ XmNlabelString, XdbXMSTRING },
	{ XmNrowColumnType, XdbNONE },
	{ XmNbuttonSet, XdbNONE },
	{ XmNbuttonCount, XdbINT },
	{ XmNoptionLabel, XdbXMSTRING },
	{ XmNoptionMnemonic, XdbCHAR },
	{ NULL, 0}	/* the end */
};

static int XdbArgType(char *s)
{
	int	i;

	for (i=0; XdbTypes[i].name; i++)
		if (strcmp(s, XdbTypes[i].name) == 0)
			return XdbTypes[i].t;
	return 0;
}

void
XdbPrintArgList(ArgList al, int n)
{
	int	i;

	for (i=0; i<n; i++)
	    switch (XdbArgType(al[i].name)) {
	    case XdbNONE:
		fprintf(stderr, "Arg[%d] : %s\n", i, al[i].name);
		break;
	    case XdbINT:
		fprintf(stderr, "Arg[%d] : %s %d\n", i, al[i].name, (int)al[i].value);
		break;
	    case XdbSTRING:
		fprintf(stderr, "Arg[%d] : %s %s\n", i, al[i].name, (char *)al[i].value);
		break;
	    case XdbXMSTRING:
		fprintf(stderr, "Arg[%d] : %s %s\n", i, al[i].name,
			XdbXmString2String((XmString)al[i].value));
		break;
	    case XdbCHAR:
		fprintf(stderr, "Arg[%d] : %s %c\n", i, al[i].name, (char)al[i].value);
		break;
	    }
}

char *
XdbAlignment2String(int n)
{
	switch (n) {
	case XmALIGNMENT_BEGINNING:
		return "XmALIGNMENT_BEGINNING";
	case XmALIGNMENT_CENTER:
		return "XmALIGNMENT_CENTER";
	case XmALIGNMENT_END:
		return "XmALIGNMENT_END";
	default:
		return "XmALIGNMENT - illegal";
	}
}

/*
 * For use with dmalloc, a malloc debugging package.
 * Mimick Xt behaviour ...
 */

#ifdef	USE_DMALLOC
#include <dmalloc.h>
#else
#include <X11/Xlibint.h>
#define	_malloc_leap(a,b,c)	Xmalloc(c)
#define	_calloc_leap(a,b,c,d)	Xcalloc(c,d)
#define	_realloc_leap(a,b,c,d)	Xrealloc(c,d)
#define	_free_leap(a,b,c)	Xfree(c)
#endif

extern void _XtAllocError(char *);

XtPointer
XdbMalloc(char *f, int l, int size)
{
	XtPointer	r;

	if (size == 0) size = 1;

	r = _malloc_leap(f, l, size);

	if (r == NULL)
		_XtAllocError("malloc");

	return r;
}

XtPointer
XdbCalloc(char *f, int l, int count, int size)
{
	XtPointer	p;

	if (!size)
		count = size = 1;

	p = _calloc_leap(f, l, count, size);

	if (p == NULL)
		_XtAllocError("calloc");

	return p;
}

XtPointer
XdbRealloc(char *f, int l, XtPointer p, int size)
{
	XtPointer	r;

	if (size == 0) size = 1;
	if (p == NULL)
		return _malloc_leap(f, l, size);
	r = _realloc_leap(f, l, p, size);
	if (r == NULL)
		_XtAllocError("realloc");
	return r;
}

void
XdbFree(char *f, int l, XtPointer p)
{
	if (p)
		_free_leap(f, l, p);
}
