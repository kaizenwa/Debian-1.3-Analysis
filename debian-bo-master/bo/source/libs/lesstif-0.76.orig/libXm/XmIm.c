/**
 *
 * $Id: XmIm.c,v 1.5 1996/12/23 12:32:26 u27113 Exp $
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

static char rcsid[] = "$Id: XmIm.c,v 1.5 1996/12/23 12:32:26 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/VendorSEP.h>
#include <X11/Intrinsic.h>
#include <X11/Xos.h>
#include <stdarg.h>

#include <XmI/DebugUtil.h>

/*
 * This file contains I18N (Internationalisation) Input Method handling.
 *
 * Comments are deduced from the Motif 2.0 manual pages.
 *
 * The current implementation is *Quick and Dirty* and follows the motif
 * specs only slightly. Not to say not at all.
 * Working on it - Danny 4 Feb 96.
 */

/* Forward */
extern Widget _LtFindVendorExt(Widget);

/*
 * This is a private data structure. A pointer to it is in the
 * VendorShell Extension object.
 */
typedef struct XmICStuff {
	XIC			xic;
	XIM			xim;
	Widget			ve, text;	/* Don't know if we need this */
	struct XmICStuff	*next;
} XmICStuff;

/*
 * Find the private data structure and cast it correctly.
 * This is close to the implementation of XmImGetXIM.
 */
static struct XmICStuff *
_XmFindICStuff(Widget w)
{
	XmVendorShellExtObject	v = (XmVendorShellExtObject)_LtFindVendorExt(w);
	XmICStuff	*stuff;

	if (v == NULL)
		return NULL;

	stuff = (XmICStuff *)v->vendor.im_info;

	while (stuff) {
	    if (stuff->text == w)
		return stuff;
	    stuff = stuff->next;
	}
	return NULL;
}

/*
 * XmImGetXIC creates and registers an XIC with the specified arguments for the
 * widget. If XmINHERIT_POLICY is specified for input_policy, then a new XIC
 * will be created only if required by the arguments or by the VendorShell
 * input policy. Any existing XIC registered with the widget is unregistered.
 *
 * If input_policy is XmPER_WIDGET, then a new XIC for this widget is created.
 * If it is XmPER_SHELL, then a new XIC for the shell is created if needed.
 *
 * (Danny's comment :)
 * Reusing an XIC is - in my opinion - not possible between widgets, because
 * the XNClientWindow of an XIC can not be changed after creation.
 *
 * Reusing the XIM is possible though.
 */
XIC
XmImGetXIC(Widget w, XmInputPolicy input_policy, ArgList args, Cardinal num_args)
{
	XmVendorShellExtObject	v = (XmVendorShellExtObject)_LtFindVendorExt(w);

	XIM		xim;		/* the input method */
	XIMStyles	*xim_styles;
	XIMStyle	input_style = 0;
	XmICStuff	*stuff = NULL, *s;
	char		*p, *buf, *b, *k;
	static char	*styles[] = {
			"OverTheSpot",
			"OffTheSpot",
			"Root",
			"Root",
		NULL };
	static XIMStyle	style_bits[] = {
		/* OverTheSpot */	XIMPreeditPosition | XIMStatusArea,
		/* OffTheSpot */	XIMPreeditArea | XIMStatusArea,
		/* Root */		XIMPreeditNothing | XIMStatusNothing,
		/* Not really root */	XIMPreeditNone | XIMStatusNone
	};

	Boolean		found;
	int		i, j;

	if (v == 0) {
		XdbDebug(__FILE__, w, "XmImRegister: no vendor shell extension found\n");
		return 0;
	}

	XdbDebug(__FILE__, w, "XmImRegister: InputMethodString '%s'\n",
		v->vendor.input_method_string
		 ? v->vendor.input_method_string
		 : "(null)");

	if ((stuff = _XmFindICStuff(w)) == NULL) {
		stuff = (XmICStuff *)XtMalloc(sizeof(XmICStuff));
		stuff->ve = (Widget)v;
		stuff->text = w;
		stuff->xim = NULL;
		stuff->xic = NULL;

		stuff->next = v->vendor.im_info;
		v->vendor.im_info = stuff;
	}

	p = v->vendor.input_method_string;
	xim = NULL;
/*
 * Try to reuse an XIM
 */
	for (s = v->vendor.im_info; s != NULL; s = s->next)
		if (s->ve == (Widget)v && s != stuff) {
			xim = s->xim;

			XdbDebug(__FILE__, w, "XmImGetXIC: reuse XIM\n");
			break;
		}

	if (! xim) {
	    if (p) {
		/* Loop over the contents of input_method_string */
		b = p;
		while (*b) {
		    k = strchr(b, ',');
		    if (k)
			*k = '\0';
		    buf = XtMalloc(10 + strlen(b));
		    strcpy(buf, "@im=");
		    strcat(buf, b);
		    if ((p = XSetLocaleModifiers(buf)) != NULL)
			xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
		    XtFree(buf);
		    if (k) {
			*k = ',';
			b = k + 1;
		    } else
			break;

		    if (xim)
			break;
		}
	    } else {
		if ((p = XSetLocaleModifiers("@im=none")) != NULL)
		    xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
	    }

	    if (xim == NULL && (p = XSetLocaleModifiers("")) != NULL)
		xim = XOpenIM(XtDisplay(w), NULL, NULL, NULL);
	}

	if (! xim) {
	    XdbDebug(__FILE__, w, "Failed to open input method\n");
	    return 0;
	}

	if (XGetIMValues(xim, XNQueryInputStyle, &xim_styles, NULL) || xim_styles == NULL) {
		XdbDebug(__FILE__, w, "Input method doesn't support any style\n");
		XCloseIM(xim);
		return 0;
	}

	if (XdbInDebug(__FILE__, w)) {
		XdbDebug(__FILE__, w, "XmImRegister - Supported styles :\n");
		for (j=0; j<xim_styles->count_styles; j++) {
		    XdbDebug0(__FILE__, w, " (%d) %X %s", j,
			xim_styles->supported_styles[j],
			(xim_styles->supported_styles[j] & XIMStatusNothing)
				? "XIMStatusNothing" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMStatusArea)
				? "XIMStatusArea" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMStatusCallbacks)
				? "XIMStatusCallbacks" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMStatusNone)
				? "XIMStatusNone" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditNothing)
				? "XIMPreeditNothing" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditPosition)
				? "XIMPreeditPosition" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditCallbacks)
				? "XIMPreeditCallbacks" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditNone)
				? "XIMPreeditNone" : "");
		    XdbDebug0(__FILE__, w, " %s",
			(xim_styles->supported_styles[j] & XIMPreeditArea)
				? "XIMPreeditArea" : "");

		    XdbDebug0(__FILE__, w, "\n");
		}
	}

/* Check whether we have input styles that match the resource */
/*
 * FIX ME
 *
 * This should really use a "find the best option" algorithm, instead of just
 * looking for the first match.
 */
	for (i=0, found = False; styles[i] && !found; i++) {
		XdbDebug(__FILE__, w, "Trying %s (0x%X)\n", styles[i], style_bits[i]);

		if (strstr(v->vendor.preedit_type_string, styles[i]) == 0)
			continue;
		for (j=0; j<xim_styles->count_styles; j++)
		    if (style_bits[i] == xim_styles->supported_styles[j]) {
			found = True;
			input_style = style_bits[i];
			break;
		    }
	}
	XFree(xim_styles);

	if (! found) {
	    XdbDebug(__FILE__, w, "XmImRegister: input method doesn't support our preedit type\n");
	    XCloseIM(xim);
	    return 0;
	}

#ifdef	notdef
	if (input_style != (XIMPreeditNothing | XIMStatusNothing)
	 || input_style != (XIMPreeditNone | XIMStatusNone)) {
	    XdbDebug(__FILE__, w, "XmImRegister: only Root preedit type is supported\n");
	    XCloseIM(xim);
	    return;
	}
#endif

	stuff->xim = xim;

	if (! XtIsRealized(XtParent(v))) {
	    XdbDebug(__FILE__, w, "XmImRegister: not realized yet\n");
	} else {
	    stuff->xic = XCreateIC(xim,
			XNInputStyle,	input_style,
			XNClientWindow,	XtWindow(w),
			XNFocusWindow,	XtWindow(w),
		NULL);

	    if (stuff->xic)
		XdbDebug(__FILE__, w, "We have an IC\n");
	    else
		XdbDebug(__FILE__, w, "IC Creation failed\n");
	}

	return stuff->xic;
}

/*
 * XmImRegister registers a widget with its input manager. This adds the specified
 * widget to a list of widgets that are supported by the input manager for an input
 * method. If an input method has not been opened by a previous call to XmImRegister,
 * the first time this routine is called it opens an input method using the XmNinputMethod
 * resource for the VendorShell. If the XmNinputMethod is NULL, an input method is
 * opened using the current locale.
 *
 * If an input method cannot be opened in the current locale, XLookupString provides
 * input processing. The application is responsible for unregistering a widget by
 * calling XmImUnregister.
 */
void
XmImRegister(Widget w, unsigned int reserved)
{
	XIC	xic;

	xic = XmImGetXIC(w, XmPER_SHELL, NULL, 0);
}

/*
 * XmImUnregister removes the specified widget from the list of widgets
 * registered for input by the input manager.
 */
void
XmImUnregister(Widget w)
{
	XdbDebug(__FILE__, w, "XmImUnregister is not implemented yet.\n");
}

/*
 * XmImSetFocusValues notifies the input manager that the specified widget
 * has received input focus. This function also updates the attributes of
 * the input context associated with the widget.
 * If the previous parameters for the widget's IC do not allow the previously
 * registered IC to be reused, that IC will be unregistered, and a new one will
 * be created and registered with the widget.
 */
void
XmImSetFocusValues(Widget w, ArgList args, Cardinal num_args)
{
	XmICStuff	*stuff = _XmFindICStuff(w);


	if (stuff == NULL) {
		XdbDebug(__FILE__, w, "XmImSetFocusValues (no IC found)\n");
		return;
	}

	XdbDebug(__FILE__, w, "XmImSetFocusValues\n");

	if (stuff->xic)
		XSetICFocus(stuff->xic);
}

/* Varargs interface to the above. */
void
XmImVaSetFocusValues(Widget w, ...)
{
	va_list	ap;
	Arg	*a;
	int	n, i;

	va_start(ap, w);
	n = 0;
	while (va_arg(ap, char *) != NULL) {
		(void) va_arg(ap, XtPointer);
		n++;
	}
	va_end(ap);

	a = (Arg *)XtCalloc(n, sizeof(Arg));
	va_start(ap, w);
	for (i=0; i<n; i++) {
		a[i].name = va_arg(ap, char *);
		a[i].value = va_arg(ap, XtArgVal);
	}
	va_end(ap);

	XmImSetFocusValues(w, a, n);
	XtFree((XtPointer)a);
}

/*
 * XmImSetValues updates attributes of the input context associated with the
 * specified widget.
 * If the previous parameters for the widget's IC do not allow the previously
 * registered IC to be reused, that IC will be unregistered, and a new one will
 * be created and registered with the widget.
 */
void
XmImSetValues(Widget w, ArgList args, Cardinal num_args)
{
	int		i;
	XmICStuff	*stuff = _XmFindICStuff(w);
	Arg		*al = (Arg *)XtCalloc(sizeof(Arg), num_args);
	int		n = 0;

	XdbDebug(__FILE__, w, "XmImSetValues is not implemented yet\n");

#ifdef	notdef
	for (i=0; i<num_args; i++) {
		if (strcmp(args[i].name, XmNforeground) == 0) {
			XtSetArg(al[n], XNForeground, args[i].value);
			n++;
		} else if (strcmp(args[i].name, XmNbackground) == 0) {
			XtSetArg(al[n], XNBackground, args[i].value);
			n++;
		} else if (strcmp(args[i].name, XmNbackgroundPixmap) == 0) {
			XtSetArg(al[n], XNBackgroundPixmap, args[i].value);
			n++;
		} else if (strcmp(args[i].name, XmNcursor) == 0) {
			XtSetArg(al[n], XNCursor, args[i].value);
			n++;
		} else if (strcmp(args[i].name, XmNfontList) == 0) {
			XtSetArg(al[n], XNFontSet, args[i].value);
			n++;
		} else if (strcmp(args[i].name, XmNfocusWindow) == 0) {
			XtSetArg(al[n], XNFocusWindow, args[i].value);
			n++;
		}
		/* FIX ME - XNLineSpacing, XNColormap, XNStdColormap,
			XNAreaNeeded, XNSpotLocation, XNArea, ... */
	}

/*
 * FIX ME -
 *	should check whether the vendor shell is realized ??
 *	how do you cope with XNVaNestedList ??
 */
	(void)XSetICValues(stuff->xic, XNVaNestedList, al, NULL);
#endif
}

/* Varargs interface to the above. */
void
XmImVaSetValues(Widget w, ...)
{
	va_list	ap;
	Arg	*a;
	int	n, i;

	va_start(ap, w);
	n = 0;
	while (va_arg(ap, char *) != NULL) {
		(void) va_arg(ap, XtPointer);
		n++;
	}
	va_end(ap);

	a = (Arg *)XtCalloc(n, sizeof(Arg));
	va_start(ap, w);
	for (i=0; i<n; i++) {
		a[i].name = va_arg(ap, char *);
		a[i].value = va_arg(ap, XtArgVal);
	}
	va_end(ap);

	XmImSetValues(w, a, n);
	XtFree((XtPointer)a);
}

/*
 * XmImUnsetFocus unsets a specified widget's focus, then notifies the input
 * manager that the specified widget has lost its input focus.
 */
void
XmImUnsetFocus(Widget w)
{
	XmICStuff	*stuff = _XmFindICStuff(w);

	XdbDebug(__FILE__, w, "XmImUnsetFocus\n");

	if (stuff->text == w && stuff->xic)
		XUnsetICFocus(stuff->xic);
}

/*
 * XmImGetXIM retrieves the XIM data structure representing the input method
 * that the input manager has opened for the specified widget.
 * If an input method has not been opened by a previous call to XmImRegister,
 * the first time this routine is called it opens an input method using the
 * XmNinputMethod resource for the VendorShell.
 */
XIM
XmImGetXIM(Widget w)
{
	XmICStuff	*stuff = _XmFindICStuff(w);

	if (stuff)
		return stuff->xim;

/* FIX ME (see comment above) */
	return NULL;
}

/*
 * XmImMbLookupString returns a string composed in the locale associated with
 * the widget's input method and a KeySym that is currently mapped to the
 * keycode in the KeyPressedEvent.
 */
int
XmImMbLookupString(Widget w, XKeyPressedEvent *evp,
		char *buf, int nbytes, KeySym *keysym, int *status)
{
	XmICStuff	*stuff = _XmFindICStuff(w);
	XIC		xic = NULL;

	if (stuff)
		xic = stuff->xic;

	if (xic)
	    return XmbLookupString(xic, evp, buf, nbytes, keysym, status);

	if (status)
	    *status = XLookupBoth;

	return XLookupString(evp, buf, nbytes, keysym, NULL);
}

/*
 * XmImSetXIC registers the X Input Context (XIC) with the specified widget.
 * Any existing XIC registered for the widget is unregistered. The new XIC
 * registered for the widget is returned.
 */
XIC
XmImSetXIC(Widget w, XIC xic)
{
	XmVendorShellExtObject	v = (XmVendorShellExtObject)_LtFindVendorExt(w);
	XmICStuff		*stuff;

	if (v == NULL || xic == NULL)
		return NULL;	/* FIX ME - is this right ? */

	XmImUnregister(w);

	if (_XmFindICStuff(w) == NULL) {
		stuff = (XmICStuff *)XtMalloc(sizeof(XmICStuff));
		stuff->ve = (Widget)v;
		stuff->text = w;
		stuff->xim = XIMOfIC(xic);
		stuff->xic = xic;

		stuff->next = v->vendor.im_info;
		v->vendor.im_info = stuff;
	}

	return xic;
}

/*
 * XmImCloseXIM closes all input contexts associated with the input method of
 * the specified widget. The widget is used to identify the Display that specifies
 * the Input Method opened for the widget. Upon closure, all widgets registered
 * with the input contexts are unregistered. Also the Input Method specified by
 * Display is closed.
 */

/* FIX ME
 * Currently we only tackle stuff under our own shell widget.
 * This is not enough, as the above text (which reflects the Motif 2.0 spec) indicates
 * that we should deal with the "Display".
 * If more than one window is open on the same Display, then we need to find them.
 * This requires the XmDisplay to keep track of top level widgets, or of (Vendor)Shells.
 * FIX ME
 */
void
XmImCloseXIM(Widget w)
{
	XmICStuff	*stuff;
	XIM		xim;

	if (! w)
		return;
	if ((stuff = _XmFindICStuff(w)) == NULL)
		return;

	xim = stuff->xim;
	XmImUnregister(w);
	if (xim)
		XCloseIM(xim);
}

/*
 * XmImFreeXIC unregisters all widgets associated with the specified XIC. The
 * specified widget must be specified with the specified XIC.
 */
void
XmImFreeXIC(Widget w, XIC xic)
{
}

void
_XmImChangeManaged(Widget w)
{
}

void
_XmImRealize(Widget w)
{
}

void
_XmImResize(Widget w)
{
}

void
_XmImRedisplay(Widget w)
{
}

