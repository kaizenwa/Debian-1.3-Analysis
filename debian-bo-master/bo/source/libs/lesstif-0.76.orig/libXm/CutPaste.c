/**
 *
 * $Id: CutPaste.c,v 1.9 1997/01/14 04:16:53 miers Exp $
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

static char rcsid[] = "$Id: CutPaste.c,v 1.9 1997/01/14 04:16:53 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <X11/Xfuncs.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/CutPaste.h>
#include <XmI/CutPasteI.h>
#include <stdio.h>

#include <XmI/DebugUtil.h>

#define UNLOCKED	0
#define LOCKED		1

#define ITEM_INCR	1000

/*
 * internal functions
 */
static Atom _XmClipboardGetAtomFromId(Display *display, int id);
static void _XmClipboardDeleteId(Display *display, int id);
static void _XmClipboardDeleteItemLabel(Display *display, Window window,
					int id);
static void _XmClipboardDeleteMarked(Display *display, Window window,
				     XmClipboard *clip);
static void _XmClipboardDeleteFormat(Display *display, int id);
static int _XmClipboardRegisterFormat(Display *display, char *format_name,
				      int format_len);
static void _XmClipboardDeleteFormats(Display *display, Window window, int id);
static void _XmClipboardDeleteItem(Display *display, Window window,
				   XmClipboard *clip, unsigned item);
static int _XmClipboardGetWindowProperty(Display *display, Window win,
					 Atom item, unsigned **data, int *len,
					 int *type_return, int *format_return,
					 Boolean delete);
static int _XmClipboardSearchForWindow(Display *display, Window root,
				       Window window);
static Time _XmClipboardGetCurrentTime(Display *display);
static Atom _XmClipboardGetAtomFromFormat(Display *display, char *format_name);
static Boolean _XmClipboardGetLenFromFormat(Display *display,
					    char *format_name,
					    int *format);
static int _XmClipboardWindowExists(Display *display, Window window);
static int _XmClipboardFindItem(Display *display, int id,
				unsigned **data, int *len,
				int *format_return, unsigned new_item);
static Boolean _XmClipboardIsMarkedForDelete(Display *display,
					     XmClipboard *clip,
					     int item);
static XmClipboardFormat *_XmClipboardFindFormat(Display *display,
						 XmClipboard *clip,
						 char *format_name,
						 long item_id,
						 int index,
						 int *namelen,
						 int *num_formats,
						 int *format_len);
static void _XmClipboardReplaceItem(Display *display, int id,
				    unsigned *data, int len,
				    int mode, int format, Boolean free);
static int _XmClipboardRetrieveItem(Display *display, int id,
				    int extra_size, int size,
				    unsigned **data, int *len,
				    int *format_return,
				    unsigned new_item, Boolean truncate);
static Boolean _XmClipboardSendMessage(Display *display, Window window,
				    XmClipboardFormat *format, int id);
static void _XmAssertClipboardSelection(Display *display, Window window,
					XmClipboard *clip, Time locktime);
static Boolean _XmClipboardGetSelection(Display *display, Window window,
					char *format_name, XmClipboard *clip,
					unsigned **data, int *len);
static void _XmClipboardMarkItem(Display *display, XmClipboard *clip,
				 unsigned id, Boolean delete_on);
static void _XmClipboardSetNextItemId(Display *display, int item_id);
static unsigned _XmClipboardGetNewItemId(Display *display);
static void _XmClipboardSetAccess(Display *display, Window window);
static XmClipboard *_XmClipboardOpen(Display *display, int extra_size);
static void _XmClipboardClose(Display *display, XmClipboard *clip);
static int _XmClipboardLock(Display *display, Window window);
static int _XmClipboardUnlock(Display *display, Window window,
			      Boolean remove_all_locks);
static void _XmClipboardEventHandler(Widget widget, XtPointer client_data,
				     XEvent *event, Boolean *dontSwallow);
static Window _XmClipboardInitializeSelection(Display *display,
					      XmClipboard *clip,
					      Window window, Time locktime);
static Boolean _XmClipboardWeOwnSelection(Display *display, XmClipboard *clip);


/******************************* low level ******************************/

static Atom
_XmClipboardGetAtomFromId(Display *display, int id)
{
    char buf[128];

    if (id == XmCLIP_HEADER)
	return XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);
    else if (id == XmCLIP_NEXT_ID)
	return XmInternAtom(display, XA_MOTIF_CLIP_NEXT_ID, False);

    sprintf(buf, XA_MOTIF_CLIP_ITEM_N, id);

    return XmInternAtom(display, buf, False);
}

static Boolean
_XmClipboardIsMarkedForDelete(Display *display, XmClipboard *clip, int item)
{
    Boolean ret;
    unsigned *data;
    int len;
    Atom hdr;

    if (item == 0) {
	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "Item label is missing!");

	return False;
    }

    _XmClipboardFindItem(display, item, &data, &len, NULL, XmCLIP_PROP_ITEM);

    ret = ((XmClipboardItem *)data)->marked_for_delete;

    XtFree((char *)data);

    return ret;
}

static void
_XmClipboardDeleteId(Display *display, int id)
{
    Atom atom;

    atom = _XmClipboardGetAtomFromId(display, id);

    XDeleteProperty(display, DefaultRootWindow(display), atom);
}

static void
_XmClipboardDeleteItemLabel(Display *display, Window window, int id)
{
    unsigned *data;
    int len, status;
    XmClipboardItem *item;
    Atom hdr;

    status = _XmClipboardFindItem(display, id, &data, &len,
				  NULL, XmCLIP_PROP_ITEM);

    if (status == XmClipboardFail)
	return;

    if (data == NULL) {

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "Item label is missing!");

	return;
    }

    item = (XmClipboardItem *)data;

    _XmClipboardDeleteId(display, item->label_id);

    XtFree((char *)item);
}

static int
_XmClipboardRegisterFormat(Display *display, char *format_name, int format_len)
{
    Atom fmt;
    int flen;

    fmt = _XmClipboardGetAtomFromFormat(display, format_name);

    if (_XmClipboardGetLenFromFormat(display, format_name, &flen)) {
	if (flen == format_len)
	    return XmClipboardSuccess;
	else
	    return XmClipboardFail;
    }

    XChangeProperty(display, DefaultRootWindow(display), fmt, fmt,
		    32, PropModeReplace, (unsigned char *)&format_len, 1);

    return XmClipboardSuccess;
}

static void
_XmClipboardDeleteFormat(Display *display, int id)
{
    unsigned *data, *idata;
    int len, ilen, item_id;
    Atom hdr;
    XmClipboardFormat *format;
    XmClipboardItem *item;

    _XmClipboardFindItem(display, id, &data, &len, NULL, XmCLIP_PROP_FORMAT);

    if (data == NULL) {

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "Format is missing in delete format");

	return;
    }

    format = (XmClipboardFormat *)data;

    if (format->by_name || format->marked_for_delete != 0)
	return;

    item_id = format->item_id;

    _XmClipboardFindItem(display, item_id, &idata, &ilen,
			 NULL, XmCLIP_PROP_ITEM);

    if (idata == NULL) {

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "Item is missing in delete format");

	return;
    }

    item = (XmClipboardItem *)idata;

    item->del_format_count++;

    if (item->del_format_count == item->format_count)
	item->marked_for_delete = True;

    format->marked_for_delete = True;

    _XmClipboardReplaceItem(display, id, data, len, PropModeReplace,
			    32, True);

    _XmClipboardReplaceItem(display, item_id, idata, ilen, PropModeReplace,
			    32, True);
}

static void
_XmClipboardDeleteFormats(Display *display, Window window, int id)
{
    unsigned *data, *addr, *fdata;
    int len, i;
    Atom hdr;
    XmClipboardItem *item;

    XdbDebug(__FILE__, NULL, "_XmClipboardDeleteFormats\n");

    _XmClipboardFindItem(display, id, &data, &len, NULL, XmCLIP_PROP_ITEM);

    if (data == NULL) {

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "Can't find item to delete formats for");

	return;
    }

    item = (XmClipboardItem *)data;

    addr = (unsigned *)((char *)item + item->item_size);

    for (i = 0; i < item->format_count; i++) {
	XmClipboardFormat *format;

	_XmClipboardFindItem(display, *addr, &fdata, &len,
			     NULL, XmCLIP_PROP_FORMAT);

	if (fdata == NULL) {

	    hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	    XDeleteProperty(display, DefaultRootWindow(display), hdr);

	    _XmWarning(NULL, "Can't find item to delete formats for");

	    return;
	}

	format = (XmClipboardFormat *)fdata;

	if (format->by_name)
	    _XmClipboardSendMessage(display, window, format, XmCLIP_NEXT_ID);

	_XmClipboardDeleteId(display, format->data_id);

	_XmClipboardDeleteId(display, format->format_id);

	XtFree((char *)format);

	*addr = 0;

	addr++;
    }

    XtFree((char *)data);
}

static void
_XmClipboardDeleteItem(Display *display, Window window,
		       XmClipboard *clip, unsigned item)
{
    unsigned *addr = (unsigned *)((char *)clip + clip->clip_size), *addr2;
    int idx, pos, lpos, extra;
    Boolean last;

    if (clip->item_count == 0)
	return;

    addr2 = addr;
    last = False;
    pos = 0;
    idx = 0;
    extra = 0;
    while (idx < clip->item_count) {
	idx++;

	if (*addr == item) {
	    last = True;
	    pos = idx - 2;
	    addr++;
	}
	else {
	    last = False;
	    *addr2 = *addr;
	    addr++;
	    addr2++;
	}
    }
    *addr2 = 0;
    addr = (unsigned *)((char *)clip + clip->clip_size);

    clip->item_count--;

    if (item == clip->current_item) {
	if (last)
	    pos--;

	lpos = pos;
	if (pos >= 0) {

	    while (pos > 0) {

		addr2 = addr + pos;

		if (_XmClipboardIsMarkedForDelete(display, clip, *addr2)) {
		    extra = *addr2;
		    break;
		}
		else
		    pos--;
	    }
	}

	if (extra == 0) {
	    pos = lpos;
	    addr2 = addr + pos;

	    while (pos < clip->item_count) {

		if (_XmClipboardIsMarkedForDelete(display, clip, *addr2)) {
		    extra = *addr2;
		    break;
		}
		else
		    pos++;

		addr2++;
	    }
	    clip->current_item = extra;
	}
	else
	    clip->current_item = extra;

	clip->last_item = 0;
    }

    _XmClipboardDeleteItemLabel(display, window, item);

    _XmClipboardDeleteFormats(display, window, item);

    _XmClipboardDeleteId(display, item);
}

static void
_XmClipboardDeleteMarked(Display *display, Window window, XmClipboard *clip)
{
    unsigned *item;
    int i;

    item = (unsigned *)((char *)clip + clip->clip_size);

    for (i = 0; i < clip->item_count; i++) {

	if (_XmClipboardIsMarkedForDelete(display, clip, *item))
	    _XmClipboardDeleteItem(display, window, clip, *item);

    }
}

static int
_XmClipboardGetWindowProperty(Display *display, Window win, Atom item,
			      unsigned **data, int *len,
			      int *type_return, int *format_return,
			      Boolean delete)
{
    int transfersize;
    long offset;
    Atom actual_type;
    int actual_format;
    unsigned long nitems, alloc_size, alloc_incr;
    unsigned long bytes_after = 1;
    unsigned char *prop;
    unsigned *ret_buf;

    transfersize = XMaxRequestSize(display);
    *data = NULL;
    *len = 0;

    ret_buf = (unsigned *)XtMalloc(8);
    ret_buf[0] = 0;
    offset = 0;
    alloc_size = 0;

    while (bytes_after) {
	if (XGetWindowProperty(display, win, item, offset, transfersize, False,
			       AnyPropertyType, &actual_type, &actual_format,
			       &nitems, &bytes_after, &prop) != Success) {
	    XtFree((char *)ret_buf);
	    return XmClipboardFail;
	}

	if (prop == NULL || nitems == 0) {
	    if (delete)
		XDeleteProperty(display, win, item);
	    if (prop)
		XFree(prop);

	    XtFree((char *)ret_buf);

	    return XmClipboardFail;
	}

	switch (actual_format) {
	case 32:
	    alloc_incr = nitems << 2;
	    break;
	case 16:
	    alloc_incr = nitems << 1;
	    break;
	case 8:
	default:
	    alloc_incr = nitems;
	    break;
	}

	ret_buf = (unsigned *)XtRealloc((char *)ret_buf,
					alloc_size + alloc_incr + 1);
	bcopy(prop, &ret_buf[offset], alloc_incr);
	alloc_size += alloc_incr;

	switch (actual_format) {
	case 32:
	    offset += nitems;
	    break;
	case 16:
	    offset += nitems >> 1;
	    break;
	case 8:
	default:
	    offset += nitems >> 2;
	    break;
	}

	XFree(prop);
    }
    ((char *)ret_buf)[alloc_size] = 0;
    *data = ret_buf;
    *len = alloc_size;

    if (delete)
	XDeleteProperty(display, win, item);

    if (format_return)
	*format_return = actual_format;

    if (type_return)
	*type_return = actual_type;

    return XmClipboardSuccess;
}

static int
_XmClipboardSearchForWindow(Display *display, Window root, Window window)
{
    Window rootret;
    Window parent;
    Window *children;
    unsigned int nchildren, i;
    int status;

    if (XQueryTree(display, root,
		   &rootret, &parent, &children, &nchildren) == 0)
	return XmClipboardFail;

    status = XmClipboardFail;
    for (i = 0; i < nchildren; i++) {
	if (children[i] == window ||
	    _XmClipboardSearchForWindow(display, children[i],
					window) == XmClipboardSuccess) {
	    status = XmClipboardSuccess;
	    break;
	}
    }

    if (children)
	XFree(children);

    return status;
}

static Time
_XmClipboardGetCurrentTime(Display *display)
{
    Window root;
    Atom time;
    XEvent event;

    root = DefaultRootWindow(display);

    XSelectInput(display, root, PropertyChangeMask);

    time = XmInternAtom(display, XA_MOTIF_CLIP_TIME, False);

    XChangeProperty(display, root, time, time, 8L, PropModeAppend,
		    NULL, 0);

    XWindowEvent(display, root, PropertyChangeMask, &event);

    return event.xproperty.time;
}

static Bool
_XmClipboardDataIsReady(Display *display, XEvent *event, XPointer arg)
{
    XDestroyWindowEvent *dev = (XDestroyWindowEvent *)event;
    XmMatchFormat *match = (XmMatchFormat *)arg;
    unsigned *data;
    int len;
    Boolean ret = False;

    if (event->type == DestroyNotify && dev->window == match->window) {
	match->window = None;
	return True;
    }
    else if (event->type == PropertyNotify) {
	_XmClipboardFindItem(display, match->id, &data, &len,
			     NULL, XmCLIP_PROP_FORMAT);

	ret = !((XmClipboardFormat *)data)->by_name;

	XtFree((char *)data);

	return ret;
    } 

    return False;
}

static Boolean
_XmClipboardRequestDataAndWait(Display *display, Window win,
			       XmClipboardFormat *format)
{
    XWindowAttributes attr;
    Window root = DefaultRootWindow(display);
    XEvent event;
    XmMatchFormat arg;
    int status;

    XdbDebug(__FILE__, NULL, "_XmClipboardRequestDataAndWait\n");

    XGetWindowAttributes(display, root, &attr);

    XSelectInput(display, root, PropertyChangeMask|StructureNotifyMask);

    if (!_XmClipboardSendMessage(display, win, format, XmCLIP_HEADER)) {

	XSelectInput(display, root, attr.your_event_mask);

	return False;
    }

    arg.window = win;
    arg.id = format->format_id;

    status = XCheckIfEvent(display, &event, _XmClipboardDataIsReady,
			   (XtPointer)&arg);

    if (arg.window == None)
	return False;

    if (status)
	XIfEvent(display, &event, _XmClipboardDataIsReady, (XPointer)&arg);

    if (arg.window == None)
	return False;

    XSelectInput(display, root, attr.your_event_mask);

    return True;
}

static int
_XmClipboardWindowExists(Display *display, Window window)
{
    Window root;
    Atom atom, rettype;
    int fmtret;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char *prop;
    int status;

    root = DefaultRootWindow(display);

    status = _XmClipboardSearchForWindow(display, root, window);
    if (status == XmClipboardSuccess)
	return XmClipboardSuccess;

    atom = XmInternAtom(display, XA_MOTIF_CLIP_LOCK_ACCESS_VALID, False);

    XGetWindowProperty(display, window, atom, 0L, 10000000L, False,
		       AnyPropertyType, &rettype, &fmtret,
		       &nitems, &bytes_after, &prop);

    if (!prop)
	status = XmClipboardFail;

    if (!nitems)
	status = XmClipboardFail;

    if (prop)
	XFree(prop);

    return status;
}

static int
_XmClipboardFindItem(Display *display, int id, unsigned **data, int *len,
		     int *format_return, unsigned prop_id)
{
    Atom item, hdr;
    Window root;
    int status;

    root = DefaultRootWindow(display);
    item = _XmClipboardGetAtomFromId(display, id);

    status = _XmClipboardGetWindowProperty(display, root, item, data, len,
					   NULL, format_return, False);

    if (status != XmClipboardSuccess)
	return status;

    if (prop_id == XmCLIP_PROP_DONT_CARE)
	return XmClipboardSuccess;

    if (prop_id == (*data)[0])
	return XmClipboardSuccess;

    XtFree((char *)*data);

    hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

    XDeleteProperty(display, root, hdr);

    _XmWarning(NULL, "ClipboardFindItem failed\n");

    return XmClipboardFail;
}

static XmClipboardFormat *
_XmClipboardFindFormat(Display *display, XmClipboard *clip,
		       char *format_name, long item_id, int index,
		       int *namelen, int *num_formats, int *format_len)
{
    unsigned *ldata, *fdata;
    int llen, status, i, flag;
    Atom hdr, fmt;
    XmClipboardItem *item;
    unsigned *addr;
    XmClipboardFormat *format;
    int my_index;

    *namelen = 0;
    *num_formats = 0;

    if (item_id < 0)
	return NULL;

    if (item_id == 0) {
	if (clip->item_count == 0)
	    return NULL;
	else
	    item_id = clip->current_item;
    }

    if (item_id == 0)
	return NULL;

    status = _XmClipboardFindItem(display, item_id, &ldata, &llen,
				  NULL, XmCLIP_PROP_ITEM);

    if (status == XmClipboardFail)
	return NULL;

    if (ldata == NULL) {

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "missing item2");

	return NULL;
    }

    item = (XmClipboardItem *)ldata;

    *num_formats = item->format_count - item->del_format_count;
    if (*num_formats < 0)
	*num_formats = 0;

    addr = (unsigned *)((char *)item + item->item_size);

    *format_len = 0;

    if (format_name == NULL) {
	fmt = 0L;

	flag = XmCLIP_PROP_DONT_CARE;
    }
    else {
	fmt = XmInternAtom(display, format_name, False);

	flag = XmCLIP_PROP_FORMAT;
    }

    format = NULL;

    my_index = 1;
    for (i = 0; i < item->format_count; i++) {
	XmClipboardFormat *fitem;
	Boolean need_free = True;

	status = _XmClipboardFindItem(display, *addr, &fdata, &llen,
				      NULL, XmCLIP_PROP_FORMAT);

	if (status == XmClipboardFail)
	    return NULL;

	if (fdata == NULL) {

	    hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	    XDeleteProperty(display, DefaultRootWindow(display), hdr);

	    _XmWarning(NULL, "missing item2");

	    return NULL;
	}

	fitem = (XmClipboardFormat *)fdata;

	if (!fitem->marked_for_delete) {

	    if (*namelen < fitem->name_len)
		*namelen = fitem->name_len;

	    if (flag != XmCLIP_PROP_DONT_CARE) {
		if (fitem->atom == fmt) {
		    *format_len = llen;
		    format = fitem;
		    need_free = False;
		}
	    }
	    else if (my_index++ == index) {
		need_free = False;
		format = fitem;
		*format_len = llen;
	    }
	}

	if (need_free)
	    XtFree((char *)fdata);

	addr++;
    }

    XtFree((char *)ldata);

    return format;
}

static void
_XmClipboardReplaceItem(Display *display, int id, unsigned *data, int len,
			int mode, int format, Boolean free)
{
    Window root;
    Atom item;
    int nunits, tstart, tlen;
    long transferlen;

    root = DefaultRootWindow(display);
    item = _XmClipboardGetAtomFromId(display, id);

    transferlen = XMaxRequestSize(display);

    switch (format) {
    case 32:
	len >>= 2;
	nunits = transferlen;
	break;
    case 16:
	len >>= 1;
	nunits = transferlen << 1;
	break;
    case 8:
    default:
	nunits = transferlen << 2;
	break;
    }

    tstart = 0;
    do {

	if (len < nunits)
	    tlen = len;
	else
	    tlen = nunits;
	XChangeProperty(display, root, item, item, format, mode,
			(unsigned char *)&data[tstart], tlen);
	len -= tlen;
	mode = PropModeAppend;
	tstart += transferlen;

    } while (len != 0);

    if (free)
	XtFree((char *)data);
}

static int
_XmClipboardRetrieveItem(Display *display, int id, int extra_size, int size,
			 unsigned **data, int *len, int *format_return,
			 unsigned prop_id, Boolean truncate)
{
    unsigned *ldata, *buf;
    int llen, status, format;

    status = _XmClipboardFindItem(display, id, &ldata, &llen,
				  &format, prop_id);

    if (llen != 0 && status == XmClipboardSuccess) {
	if (truncate)
	    llen = 0;
	llen += extra_size;
	*len = llen;
    }
    else
	*len = size;

    buf = (unsigned *)XtMalloc(*len);

    if (status == XmClipboardSuccess)
	bcopy(ldata, buf, *len);

    *data = buf;
    XtFree((char *)ldata);

    if (format_return)
	*format_return = format;

    return status;
}

static unsigned
_XmClipboardGetNewItemId(Display *display)
{
    unsigned *data, ret;
    int len;

    _XmClipboardFindItem(display, XmCLIP_NEXT_ID, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    ret = data[0]++;

    _XmClipboardReplaceItem(display, XmCLIP_NEXT_ID, data, len,
			    PropModeReplace, 32, True);

    return ret;
}

static Atom
_XmClipboardGetAtomFromFormat(Display *display, char *format_name)
{
    char *str;
    Atom atom;

    str = XtMalloc(strlen(format_name) + 32);

    sprintf(str, XA_MOTIF_CLIP_FORMAT_S, format_name);

    atom = XmInternAtom(display, str, False);

    XtFree(str);

    return atom;
}

static Boolean
_XmClipboardGetLenFromFormat(Display *display, char *format_name, int *format)
{
    Atom actual_type;
    int actual_format, status;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char *prop;
    Atom atom;
    Boolean ret = False;

    atom = _XmClipboardGetAtomFromFormat(display, format_name);

    status = XGetWindowProperty(display, DefaultRootWindow(display), atom,
				0, 10000000L, False, AnyPropertyType,
				&actual_type, &actual_format, &nitems,
				&bytes_after, &prop);

    if (prop == NULL || nitems == 0 || status != Success)
	*format = 8;
    else {
	*format = *((int *)(prop));
	ret = True;
    }

    if (prop)
	XFree(prop);

    return ret;
}

static Window
_XmClipboardInitializeSelection(Display *display, XmClipboard *clip,
				Window window, Time locktime)
{
    Atom clipboard;
    Window clipowner;

    clipboard = XmInternAtom(display, XA_CLIPBOARD, False);

    clipowner = XGetSelectionOwner(display, clipboard);

    if (clipowner == window && clip->last_selection_owner == None) {

	XSetSelectionOwner(display, clipboard, None, locktime);

	clipowner = None;
    }

    if (clipowner == None) {

	_XmAssertClipboardSelection(display, window, clip, locktime);

	clipowner = XGetSelectionOwner(display, clipboard);
    }

    return clipowner;
}

static Boolean
_XmClipboardWeOwnSelection(Display *display, XmClipboard *clip)
{
    Atom clipboard;
    Window clipowner;

    clipboard = XmInternAtom(display, XA_CLIPBOARD, False);

    clipowner = XGetSelectionOwner(display, clipboard);

    if (clipowner == clip->last_selection_owner)
	return True;

    return False;
}

static void
_XmAssertClipboardSelection(Display *display, Window window,
			    XmClipboard *clip, Time locktime)
{
    Widget w;
    Atom clipboard;

    clip->copy_lock_time = 0;
    clip->last_selection_owner = 0;

    w = XtWindowToWidget(display, window);

    XdbDebug(__FILE__, w, "_XmAssertClipboardSelection\n");

    if (w == NULL || clip->current_item == 0)
	return;

    clip->copy_lock_time = 0;
    clip->last_selection_owner = window;

    clipboard = XmInternAtom(display, XA_CLIPBOARD, False);

    XSetSelectionOwner(display, clipboard, window, locktime);

    XtAddEventHandler(w, None, True, _XmClipboardEventHandler, NULL);
}

static Bool
_XmClipboardSelectionIsReady(Display *display, XEvent *event, XPointer arg)
{
    XmMatchSelection *match = (XmMatchSelection *)arg;
    Atom ctmp, incr;
    Boolean have_sel = False;
    unsigned *data, *buf;
    int len, type, format, status;

    ctmp = XmInternAtom(display, XA_CLIP_TEMPORARY, False);

    if (event->type == DestroyNotify &&
	event->xdestroywindow.window == match->window) {
	match->selection_owner = None;
	return True;
    }

    if (event->type == SelectionNotify) {
	/* could not be converted */
	if (event->xselection.property == 0)
	    return True;
	/* conversion ok */
	else if (event->xselection.property == ctmp) {
	    match->selection_done = True;
	    have_sel = True;
	}
    }

    if (event->type == PropertyNotify && event->xproperty.atom == ctmp &&
	event->xproperty.state == PropertyNewValue)
	have_sel = True;

    if (!have_sel)
	return False;

    status = _XmClipboardGetWindowProperty(match->display, match->window, ctmp,
					   &data, &len, &type, &format, True);

    if (data == NULL || len == 0 || status != XmClipboardSuccess)
	return True;

    incr = XmInternAtom(display, XA_CLIP_INCR, False);

    if (type == incr) {
	match->type = type;
	match->incr = True;
	return False;
    }

    buf = (unsigned *)XtMalloc(len + match->len);

    bcopy(match->data, buf, match->len);

    XtFree((char *)match->data);

    match->data = buf;

    bcopy(data, (char *)buf + match->len, len);

    XtFree((char *)data);

    match->len += len;

    if (match->incr)
	return False;

    return True;
}

static Boolean
_XmClipboardGetSelection(Display *display, Window window, char *format_name,
			 XmClipboard *clip, unsigned **data, int *len)
{
    Atom format, clipboard, cliptmp;
    Window clipowner;
    XWindowAttributes attr, sattr;
    XmMatchSelection arg;
    XEvent event;
    Bool status;

    format = XmInternAtom(display, format_name, False);

    clipboard = XmInternAtom(display, XA_CLIPBOARD, False);

    clipowner = XGetSelectionOwner(display, clipboard);

    if (clipowner == None)
	return False;

    cliptmp = XmInternAtom(display, XA_CLIP_TEMPORARY, False);

    XGetWindowAttributes(display, clipowner, &attr);

    XSelectInput(display, clipowner,
		 attr.your_event_mask | StructureNotifyMask);

    XGetWindowAttributes(display, window, &sattr);

    XSelectInput(display, window,
		 sattr.your_event_mask | PropertyChangeMask);

    XConvertSelection(display, clipboard, format, cliptmp, window,
		      clip->retrieve_lock_time);

    arg.display = display;
    arg.window = window;
    arg.selection_owner = clipowner;
    arg.retrieve_time = clip->retrieve_lock_time;
    arg.format_name = format_name;
    arg.len = 0;
    arg.data = NULL;
    arg.incr = False;
    arg.selection_done = False;

    status = XCheckIfEvent(display, &event, _XmClipboardSelectionIsReady,
			   (XPointer)&arg);

    if (arg.selection_owner == None) {
	XSelectInput(display, window, sattr.your_event_mask);

	return False;
    }

    if (!status)
	XIfEvent(display, &event, _XmClipboardSelectionIsReady,
		 (XPointer)&arg);

    XSelectInput(display, window, sattr.your_event_mask);

    if (arg.selection_owner == None)
	return False;

    XSelectInput(display, clipowner, attr.your_event_mask);

    *data = arg.data;
    *len = arg.len;

    if (*data && *len)
	return True;

    return False;
}

static void
_XmClipboardSetNextItemId(Display *display, int item_id)
{
    XmClipboard *clip;
    int cur, last, len, rem;
    unsigned *data;

    clip = _XmClipboardOpen(display, 0);

    cur = clip->current_item;
    last = clip->last_item;
    
    _XmClipboardClose(display, clip);

    cur--;
    last--;

    rem = item_id;

    for (;;) {

	rem -= item_id % ITEM_INCR;

	if (rem > item_id)
	    rem = ITEM_INCR;
	else
	    rem += ITEM_INCR;

	if (rem != cur && rem != last)
	    break;

    }

    _XmClipboardFindItem(display, XmCLIP_NEXT_ID, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    data[0] = rem;

    _XmClipboardReplaceItem(display, XmCLIP_NEXT_ID, data, sizeof(int),
			    PropModeReplace, 32, True);
}

static void
_XmClipboardMarkItem(Display *display, XmClipboard *clip,
		     unsigned id, Boolean delete_on)
{
    unsigned *data;
    int len;
    Atom hdr;

    if (id == 0)
	return;

    _XmClipboardFindItem(display, id, &data, &len, NULL, XmCLIP_PROP_ITEM);

    if (data == NULL) {
	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	_XmWarning(NULL, "missing item when marking for delete");

	return;
    }

    ((XmClipboardItem *)data)->marked_for_delete = delete_on;

    _XmClipboardReplaceItem(display, id, data, len,
			    PropModeReplace, 32, True);

}

static void
_XmClipboardSetAccess(Display *display, Window window)
{
    Atom lock_access;

    lock_access = XmInternAtom(display, XA_MOTIF_CLIP_LOCK_ACCESS_VALID, False);

    XChangeProperty(display, window, lock_access, lock_access, 8,
		    PropModeReplace, (unsigned char *)"yes", 3);
}

static XmClipboard *
_XmClipboardOpen(Display *display, int extra_size)
{
    int status, len, dlen;
    unsigned *data, *id_data, next_id;
    XmClipboard *clip;

    status = XmClipboardSuccess;
    if (!extra_size)
	status = _XmClipboardFindItem(display, XmCLIP_HEADER,
				      &data, &len, NULL, XmCLIP_PROP_DONT_CARE);

    if (extra_size || status != XmClipboardSuccess)
	status = _XmClipboardRetrieveItem(display, XmCLIP_HEADER, extra_size,
					  sizeof(XmClipboard), &data, &len,
					  0, 0, False);

    clip = (XmClipboard *)data;
    if (status != XmClipboardSuccess) {
	clip->prop_id = XmCLIP_PROP_HEADER;
	clip->cpad1 = 0;
	clip->static_item_count = 1;
	clip->clip_size = sizeof(XmClipboard);
	clip->current_item = 0;
	clip->last_item = 0;
	clip->next_delete = 0;
	clip->current_item2 = 0;
	clip->by_name_id = 0;
	clip->item_count = 0;
	clip->last_selection_owner = 0;
	clip->copy_lock_time = 0;
	clip->retrieve_lock_time = 0;
	clip->offset = 0;
	clip->copy_locked = 0;
	clip->retrieve_locked = 0;
    }

    status = _XmClipboardFindItem(display, XmCLIP_NEXT_ID,
				  &id_data, &dlen, NULL, XmCLIP_PROP_DONT_CARE);

    if (status != XmClipboardSuccess) {
	next_id = ITEM_INCR;
	_XmClipboardReplaceItem(display, XmCLIP_NEXT_ID, &next_id, sizeof(int),
				PropModeReplace, 32, False);

	return clip;
    }

    XtFree((char *)id_data);

    return clip;
}

static void
_XmClipboardClose(Display *display, XmClipboard *clip)
{
    _XmClipboardReplaceItem(display, XmCLIP_HEADER,
			    (unsigned *)clip, sizeof(XmClipboard) +
					 clip->item_count * sizeof(unsigned),
			    PropModeReplace, 32, True);
}

static int
_XmClipboardLock(Display *display, Window window)
{
    Atom lock, clipatom, hdr;
    Window lockowner, clipowner;
    int flag;
    unsigned *data;
    int len;
    Time time;
    XmClipboard *clip;

    lock = XmInternAtom(display, XA_MOTIF_CLIP_LOCK, False);
    lockowner = XGetSelectionOwner(display, lock);

    if (lockowner != window && lockowner != None)
	return XmClipboardLocked;

    _XmClipboardFindItem(display, XmCLIP_ITEM_2, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    if (!len) {
	data = (unsigned *)XtMalloc(sizeof(unsigned) * 2);
	data[1] = 0;
    }

    if (data[1] == UNLOCKED) {
	data[0] = window;
	data[1] = LOCKED;
	flag = 0;
    }
    else if (window == data[0]) {
	data[1] += LOCKED;
	flag = 1;
    }
    else {
	if (_XmClipboardWindowExists(display, data[0]) == XmClipboardFail) {
	    XtFree((char *)data);
	    return XmClipboardLocked;
	}

	clipatom = XmInternAtom(display, XA_CLIPBOARD, False);

	clipowner = XGetSelectionOwner(display, clipatom);

	time = _XmClipboardGetCurrentTime(display);

	clip = _XmClipboardOpen(display, False);

	if (clipowner == clip->last_selection_owner)
	    XSetSelectionOwner(display, clipatom, None, time);

	_XmClipboardClose(display, clip);

	hdr = XmInternAtom(display, XA_MOTIF_CLIP_HEADER, False);

	XDeleteProperty(display, DefaultRootWindow(display), hdr);

	clip = _XmClipboardOpen(display, False);

	_XmClipboardClose(display, clip);

	data[0] = window;
	data[1] = LOCKED;

	flag = 1;
    }

    if (flag) {
	lockowner = XGetSelectionOwner(display, lock);

	if (lockowner == None) {
	    time = _XmClipboardGetCurrentTime(display);

	    XSetSelectionOwner(display, lock, window, time);

	    lockowner = XGetSelectionOwner(display, lock);

	    if (lockowner != window) {
		XtFree((char *)data);
		return XmClipboardLocked;
	    }
	}
    }

    _XmClipboardReplaceItem(display, XmCLIP_ITEM_2, data, 8,
			    PropModeReplace, 32, False);

    _XmClipboardSetAccess(display, window);

    XtFree((char *)data);

    return XmClipboardSuccess;
}

static int
_XmClipboardUnlock(Display *display, Window window, Boolean remove_all_locks)
{
    Atom lock;
    Window lockowner;
    unsigned *data;
    int len, status;
    Time time;

    lock = XmInternAtom(display, XA_MOTIF_CLIP_LOCK, False);
    lockowner = XGetSelectionOwner(display, lock);

    if (lockowner != window && lockowner != None)
	return XmClipboardFail;

    status = XmClipboardFail;

    _XmClipboardFindItem(display, XmCLIP_ITEM_2, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    if (len == 0)
	return XmClipboardFail;

    if (data[0] != window) {
	XtFree((char *)data);
	return XmClipboardFail;
    }

    if (remove_all_locks)
	data[1] = 0;
    else
	data[1] -= LOCKED;

    if (data[1] > UNLOCKED)
	len = 8;
    else {
	status = XmClipboardSuccess;
	len = 0;
    }

    _XmClipboardReplaceItem(display, XmCLIP_ITEM_2, data, len,
			    PropModeReplace, 32, False);

    XtFree((char *)data);

    if (status == XmClipboardSuccess) {
	time = _XmClipboardGetCurrentTime(display);
	XSetSelectionOwner(display, lock, None, time);
    }

    return XmClipboardSuccess;
}

static void
_XmClipboardEventHandler(Widget widget, XtPointer client_data,
			 XEvent *ev, Boolean *dontSwallow)
{
    int fid, pid, status;
    XClientMessageEvent *event = (XClientMessageEvent *)ev;
    unsigned *data;
    int reason, len;
    XmCutPasteProc cb;

    XdbDebug(__FILE__, widget, "Call to ClipboardEventHandler\n");

    if (event->type != ClientMessage)
	return;

    if (event->message_type != XmInternAtom(event->display,
					    XA_MOTIF_CLIP_MESSAGE,
					    False))
	return;

    fid = event->data.l[1];
    pid = event->data.l[2];

    XdbDebug(__FILE__, widget, "ClipboardEventHandler: Find Item\n");

    status = _XmClipboardFindItem(event->display, fid, &data, &len,
				  NULL, XmCLIP_PROP_FORMAT);

    if (status != XmClipboardSuccess)
	return;

    XdbDebug(__FILE__, widget, "ClipboardEventHandler: Found Item\n");

    cb = ((XmClipboardFormat *)data)->callback;

    XtFree((char *)data);

    if (!cb) {
	XdbDebug(__FILE__, widget, "ClipboardEventHandler: No Callback\n");

	return;
    }

    if (event->data.l[0] == XmInternAtom(event->display,
					 XA_MOTIF_CLIP_DATA_REQUEST,
					 False)) {
	reason = XmCR_CLIPBOARD_DATA_REQUEST;
    }
    else if (event->data.l[0] == XmInternAtom(event->display,
					 XA_MOTIF_CLIP_DATA_DELETE,
					 False)) {
	reason = XmCR_CLIPBOARD_DATA_DELETE;
    }
    else {
	XdbDebug(__FILE__, widget, "ClipboardEventHandler: Bad Atom\n");

	return;
    }

    (*cb)(widget, &fid, &pid, &reason);

    if (reason == XmCR_CLIPBOARD_DATA_REQUEST) {
	XdbDebug(__FILE__, widget, "ClipboardEventHandler: Data Request\n");

	return;
    }

    _XmClipboardFindItem(event->display, XmCLIP_HEADER, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    ((XmClipboard *)data)->by_name_id = 0; 

    _XmClipboardReplaceItem(event->display, XmCLIP_HEADER, data, len,
			    PropModeReplace, 32, True);
}

static Boolean
_XmClipboardSendMessage(Display *display, Window window,
			XmClipboardFormat *format, int id)
{
    XmClipboard *clip;
    XClientMessageEvent event;
    Boolean dummy;
    unsigned *data;
    int len;

    XdbDebug(__FILE__, NULL, "_XmClipboardSendMEssage: %d\n", id);

    if (format->widget_window == None)
	return False;

    event.type = ClientMessage;
    event.window = format->widget_window;
    event.display = format->display;
    event.message_type = XmInternAtom(display, XA_MOTIF_CLIP_MESSAGE, False);
    event.format = 32;

    switch (id) {
    case XmCLIP_HEADER:
	_XmClipboardFindItem(display, XmCLIP_HEADER, &data, &len,
			     NULL, XmCLIP_PROP_DONT_CARE);

	clip = (XmClipboard *)data;
	clip->by_name_id = format->format_id;

	_XmClipboardReplaceItem(display, XmCLIP_HEADER, data, len,
				PropModeReplace, 32, True);

	/* fall thru */
    case XmCLIP_NEXT_ID:
	event.data.l[0] = XmInternAtom(display, XA_MOTIF_CLIP_DATA_REQUEST,
				       False);

	/* fall thru */
    default:
	event.data.l[1] = format->format_id;
	event.data.l[2] = format->private_id;
	if (format->window == window && format->display == display) {
	    XdbDebug(__FILE__, NULL, "Call _XmClipboardEventHandler direct\n");

	    _XmClipboardEventHandler(format->widget, NULL,
				     (XEvent *)&event, &dummy);
	    return True;
	}
	else if (_XmClipboardWindowExists(display, format->widget_window)) {
	    XSendEvent(display, format->widget_window, False, 0,
		       (XEvent *)&event);
	    return True;
	}
    }
    
    return False;
}

/*************************** public functions **************************/

int
XmClipboardBeginCopy(Display *display,
		     Window window,
		     XmString clip_label,
		     Widget widget,
		     VoidProc callback,
		     long *item_id)
{
    return XmClipboardStartCopy(display, window, clip_label, CurrentTime,
				widget, callback, item_id);
}

int
XmClipboardCancelCopy(Display *display,
		      Window window,
		      long item_id)
{
    int status;
    unsigned *data;
    int len;
    XmClipboard *clip;

    status = _XmClipboardLock(display, window);

    if (status == XmClipboardLocked)
	return status;

    _XmClipboardDeleteItemLabel(display, window, item_id);

    _XmClipboardDeleteFormats(display, window, item_id);

    _XmClipboardDeleteId(display, item_id);

    _XmClipboardFindItem(display, XmCLIP_NEXT_ID, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    data[0]--;

    _XmClipboardReplaceItem(display, XmCLIP_NEXT_ID, data, sizeof(int),
			    PropModeReplace, 32, True);

    clip = _XmClipboardOpen(display, 0);

    clip->copy_locked = UNLOCKED;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardCopy(Display *display,
		Window window,
		long item_id,
		char *format_name,
		XtPointer buffer,
		unsigned long length,
		long private_id,
		long *data_id)
{
    XmClipboard *clip;
    int status, formatlen, namelen, numformats, ilen;
    unsigned *fdata;
    unsigned *idata;
    XmClipboardItem *item;
    XmClipboardFormat *format;
    unsigned long buflen;
    int fmt;
    XtPointer nbuf;

    status = _XmClipboardLock(display, window);

    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    if (clip->copy_locked == UNLOCKED) {

	_XmWarning(NULL, "Attempt to copy to unlocked clipboard!");

	_XmClipboardUnlock(display, window, False);

	return XmClipboardFail;
    }

    format = _XmClipboardFindFormat(display, clip, format_name, item_id,
				    0, &namelen, &numformats, &formatlen);

    if (format == NULL) {
	int fid1, fid2;

	status = _XmClipboardRetrieveItem(display, item_id, sizeof(int), 0,
					  &idata, &ilen, NULL, XmCLIP_PROP_ITEM,
					  False);

	if (status != XmClipboardSuccess)
	    return status;

	item = (XmClipboardItem *)idata;
	item->format_count++;

	if (((item->format_count + 1) * 2) >= ITEM_INCR) {

	    _XmWarning(NULL, "Too many formats for item.");
	    
	    XtFree((char *)idata);

	    _XmClipboardUnlock(display, window, False);
	}

	format = (XmClipboardFormat *)XtMalloc(sizeof(XmClipboardFormat));

	formatlen = sizeof(XmClipboardFormat);

	fid1 = _XmClipboardGetNewItemId(display);
	fid2 = _XmClipboardGetNewItemId(display);

	idata[ilen/sizeof(int)-1] = fid1;

	format->prop_id = XmCLIP_PROP_FORMAT;
	format->atom = XmInternAtom(display, format_name, False);
	format->length = 0;
	format->name_len = strlen(format_name);
	format->format_id = fid1;
	format->data_id = fid2;
	format->marked_for_delete = 0;
	format->private_id = private_id;
	format->offset = 0;
	format->item_id = item_id;
	format->widget = item->widget;
	format->widget_window = item->widget_window;
	format->callback = item->callback;
	format->window = item->window;
	format->display = item->display;

	if (buffer == NULL) {
	    format->by_name = 1;
	    item->by_name = 1;
	    buflen = sizeof(int);
	}
	else {
	    format->by_name = 0;
	    buflen = length;
	}

	_XmClipboardReplaceItem(display, item_id, idata, ilen,
				PropModeReplace, 32, True);

	status = _XmClipboardGetLenFromFormat(display, format_name, &fmt);

	if (status == XmClipboardFail) {

	    XmClipboardRegisterFormat(display, format_name, 0);

	    _XmClipboardGetLenFromFormat(display, format_name, &fmt);

	}

	nbuf = XtMalloc(buflen);
    }
    else {
	status = _XmClipboardRetrieveItem(display, format->data_id,
					  length, 0, &fdata, (int *)&buflen,
					  &fmt, XmCLIP_PROP_DONT_CARE, False);

	nbuf = (char *)fdata + buflen - length;
    }

    if (buffer)
	bcopy(buffer, nbuf, length);

    format->length += length;

    _XmClipboardReplaceItem(display, format->data_id, nbuf, buflen,
			    PropModeReplace, fmt, True);

    _XmClipboardReplaceItem(display, format->format_id, (unsigned *)format,
			    formatlen, PropModeReplace, 32, True);

    if (data_id)
	*data_id = format->format_id;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardCopyByName(Display *display,
		      Window window,
		      long data_id,
		      XtPointer buffer,
		      unsigned long length,
		      long private_id)
{
    unsigned *data, *idata, *fdata;
    int len, status, ilen, flen, fmt;
    XmClipboard *clip;
    Boolean not_in_progress = False;
    XmClipboardFormat *format;
    unsigned char *bufstart;

    _XmClipboardFindItem(display, XmCLIP_HEADER, &data, &len,
			 NULL, XmCLIP_PROP_DONT_CARE);

    clip = (XmClipboard *)data;

    if (clip->by_name_id != data_id) {

	status = _XmClipboardLock(display, window);

	if (status == XmClipboardLocked)
	    return status;

	not_in_progress = True;
    }
    else {
	clip->by_name_id = 0;

	_XmClipboardReplaceItem(display, XmCLIP_HEADER, data, len,
				PropModeReplace, 32, False);
    }

    status = _XmClipboardFindItem(display, data_id, &fdata, &flen,
				  NULL, XmCLIP_PROP_FORMAT);

    if (status == XmClipboardSuccess) {
	format = (XmClipboardFormat *)fdata;

	format->private_id = private_id;

	if (format->by_name)
	    format->length = length;
	else
	    format->length += length;

	_XmClipboardRetrieveItem(display, format->data_id, length, 0,
				 &idata, &ilen, &fmt, XmCLIP_PROP_DONT_CARE,
				 format->by_name);

	format->by_name = 0;

	bufstart = (unsigned char *)idata + ilen - length;

	bcopy(buffer, bufstart, length);

	_XmClipboardReplaceItem(display, format->data_id, idata, ilen,
				PropModeReplace, fmt, True);

	_XmClipboardReplaceItem(display, data_id, fdata, flen,
				PropModeReplace, 32, True);
    }

    if (not_in_progress)
	_XmClipboardUnlock(display, window, False);

    XtFree((char *)data);

    return XmClipboardSuccess;
}

int
XmClipboardEndCopy(Display *display,
		   Window window,
		   long item_id)
{
    XmClipboard *clip;
    int status;
    unsigned *data, *buf;
    int len;
    XmClipboardItem *item;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, sizeof(int));

    if (clip->copy_locked == UNLOCKED) {

	_XmWarning(NULL, "Attempt to EndCopy an unlocked clipboard!");

	_XmClipboardUnlock(display, window, False);

	return XmClipboardFail;
    }

    _XmClipboardDeleteMarked(display, window, clip);

    if (clip->item_count >= clip->static_item_count) {
	buf = (unsigned *)((char *)clip + clip->clip_size);

	_XmClipboardMarkItem(display, clip, *buf, True);

	clip->next_delete = *buf;
    }
    else
	clip->next_delete = 0;

    buf = (unsigned *)((char *)clip + clip->clip_size);
    buf += clip->item_count;
    *buf = item_id;

    clip->last_item = clip->current_item;
    clip->current_item = item_id;
    clip->current_item2 = item_id;
    clip->item_count++;
    clip->copy_locked = UNLOCKED;

    _XmClipboardFindItem(display, item_id, &data, &len, NULL, XmCLIP_PROP_ITEM);

    item = (XmClipboardItem *)data;

    if (item->widget) {
	XdbDebug(__FILE__, NULL, "Add EventHandler from EndCopy\n");

	XtAddEventHandler(item->widget, None, True,
			  _XmClipboardEventHandler, NULL);
    }

    XtFree((char *)data);

    _XmAssertClipboardSelection(display, window, clip, clip->copy_lock_time);

    _XmClipboardSetNextItemId(display, item_id);

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardEndRetrieve(Display *display,
		       Window window)
{
    XmClipboard *clip;

    clip = _XmClipboardOpen(display, 0);

    clip->retrieve_locked = UNLOCKED;
    clip->retrieve_lock_time = None;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardInquireCount(Display *display,
			Window window,
			int *count,
			unsigned long *max_length)
{
    XmClipboard *clip;
    int status, namelen, num_formats, format_len, len;
    Time time;
    unsigned *data;
    XmClipboardFormat *format;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    if (clip->retrieve_lock_time == 0)
	time = _XmClipboardGetCurrentTime(display);
    else
	time = clip->retrieve_lock_time;

    _XmClipboardInitializeSelection(display, clip, window, time);

    format = NULL;

    if (_XmClipboardWeOwnSelection(display, clip)) {
	format = _XmClipboardFindFormat(display, clip, NULL, 0, 0, &namelen,
					&num_formats, &format_len);
    }
    else {
	if (!_XmClipboardGetSelection(display, window, XA_MOTIF_TARGETS,
				      clip, &data, &len)) {
	    _XmClipboardClose(display, clip);

	    _XmClipboardUnlock(display, window, False);

	    return XmClipboardFail;
	}

	/* FIXME */
    }

    if (max_length != NULL)
	*max_length = format_len;

    if (count != NULL)
	*count = num_formats;

    if (format)
	XtFree((char *)format);

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardInquireFormat(Display *display,
			 Window window,
			 int index,
			 XtPointer format_name_buf,
			 unsigned long buffer_len,
			 unsigned long *copied_len)
{
    XmClipboard *clip;
    int status, namelen, num_formats, format_len, len;
    Time time;
    XmClipboardFormat *format;
    unsigned *data;
    char *name = NULL;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    time = clip->retrieve_lock_time;

    _XmClipboardInitializeSelection(display, clip, window, time);

    if (_XmClipboardWeOwnSelection(display, clip)) {

	if ((format = _XmClipboardFindFormat(display, clip, NULL, 0, index,
					     &namelen, &num_formats,
					     &format_len)) != NULL) {

	    name = XmGetAtomName(display, format->atom);

	}
    }
    else {
	if (!_XmClipboardGetSelection(display, window, XA_MOTIF_TARGETS,
				      clip, &data, &len)) {
	    _XmClipboardClose(display, clip);

	    _XmClipboardUnlock(display, window, False);

	    return XmClipboardFail;
	}

	/* FIXME */
    }

    if (name) {

	len = strlen(name) <= buffer_len ? strlen(name) : buffer_len;
	strncpy(format_name_buf, name, len);

	if (copied_len)
	    *copied_len = len;

	XtFree(name);
    }
    else {
	if (copied_len)
	    *copied_len = 0;
    }

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardInquireLength(Display *display,
			 Window window,
			 char *format_name,
			 unsigned long *length)
{
    XmClipboard *clip;
    int status, namelen, num_formats, format_len, len, flen;
    Time time;
    unsigned *data;
    XmClipboardFormat *format;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    time = clip->retrieve_lock_time;

    _XmClipboardInitializeSelection(display, clip, window, time);

    flen = 0;

    if (_XmClipboardWeOwnSelection(display, clip)) {
	format = _XmClipboardFindFormat(display, clip, format_name, 0, 0,
					&namelen, &num_formats, &format_len);
	if (format) {
	    flen = format->length;

	    XtFree((char *)format);
	}
    }
    else {
	if (!_XmClipboardGetSelection(display, window, format_name,
				      clip, &data, &len)) {
	    _XmClipboardClose(display, clip);

	    _XmClipboardUnlock(display, window, False);

	    return XmClipboardFail;
	}

	XtFree((char *)data);
	flen = len;
    }

    if (length)
	*length = flen;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardInquirePendingItems(Display *display,
			       Window window,
			       char *format_name,
			       XmClipboardPendingList *item_list,
			       unsigned long *count)
{
    XmClipboard *clip;
    int status;
    XmClipboardPendingList items, item;
    XmClipboardFormat *format;
    unsigned *ilist;
    int i, cnt, namelen, numformats, formatlen;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    if (item_list == NULL) {
	XmClipboardUnlock(display, window, False);
	return XmClipboardSuccess;
    }

    *item_list = NULL;

    clip = _XmClipboardOpen(display, 0);

    ilist = (unsigned *)((char *)clip + clip->clip_size);

    cnt = 0;

    items = (XmClipboardPendingList)XtMalloc(clip->item_count *
						sizeof(XmClipboardPendingRec));

    item = items;

    for (i = 0; i < clip->item_count; i++) {

	format = NULL;

	if (!_XmClipboardIsMarkedForDelete(display, clip, *ilist)) {
	    _XmClipboardFindFormat(display, clip, format_name, *ilist,
				   0, &namelen, &numformats, &formatlen);
	}

	if (format != NULL) {

	    if (format->by_name) {
		item->DataId = format->format_id;
		item->PrivateId = format->private_id;

		cnt++;
		item++;
	    }

	    XtFree((char *)format);
	}
    }

    if (count)
	*count = cnt;

    *item_list = items;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardLock(Display *display,
		Window window)
{
    return _XmClipboardLock(display, window);
}

int
XmClipboardRegisterFormat(Display *display,
			  char *format_name,
			  int format_length)
{
#define REGISTER_IF_KNOWN(dsp, fname, kname, len) \
    if (strcmp(fname, kname) == 0) { \
	_XmClipboardRegisterFormat(dsp, fname, len); \
	return XmClipboardSuccess; \
    }

    if (format_length != 0 && format_length != 8 && format_length != 16 &&
        format_length != 32) {

	_XmWarning(NULL, "Invalid format length");

	return XmClipboardBadFormat;
    }

    if (format_name == NULL || strlen(format_name) == 0)
	_XmWarning(NULL, "Attempt to register NULL or zero length format name");

    if (format_length != 0)
	return _XmClipboardRegisterFormat(display, format_name, format_length);

    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_TARGETS, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_MULTIPLE, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_TIMESTAMP, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_STRING, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_COMPOUND_TEXT, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_LIST_LENGTH, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_PIXMAP, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_DRAWABLE, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_BITMAP, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_FOREGROUND, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_BACKGROUND, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_COLORMAP, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_ODIF, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_OWNER_OS, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_FILE_NAME, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_HOST_NAME, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_CHARACTER_POSITION, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_LINE_NUMBER, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_COLUMN_NUMBER, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_LENGTH, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_USER, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_PROCEDURE, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_MODULE, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_PROCESS, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_TASK, 32)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_CLASS, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_NAME, 8)
    REGISTER_IF_KNOWN(display, format_name, XA_MOTIF_CLIENT_WINDOW, 32)

    return XmClipboardFail;
}

int
XmClipboardRetrieve(Display *display,
		    Window window,
		    char *format_name,
		    XtPointer buffer,
		    unsigned long length,
		    unsigned long *num_bytes,
		    long *private_id)
{
    XmClipboard *clip;
    int namelen, numformats, formatlen, dlen, privid = 0, status, format_id;
    int fetchlen = 0;
    XmClipboardFormat *format;
    Boolean got_format = False;
    unsigned *data;
    unsigned char *bufstart = NULL;
    Boolean partial_copy = False, got_data = False;

    status = _XmClipboardLock(display, window);
    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    _XmClipboardInitializeSelection(display, clip, window,
				    clip->retrieve_lock_time);

    got_format = False;

    if (_XmClipboardWeOwnSelection(display, clip)) {

	format = _XmClipboardFindFormat(display, clip, format_name, 0, 0,
				        &namelen, &numformats, &formatlen);

	got_format = True;

	if (format != NULL) {

	    format_id = format->format_id;

	    if (format->by_name) {

		got_format = _XmClipboardRequestDataAndWait(display, window,
							    format);

		if (got_format) {

		    XtFree((char *)format);

		    _XmClipboardFindItem(display, format_id,
					 &data, &formatlen,
					 NULL, XmCLIP_PROP_FORMAT);

		    format = (XmClipboardFormat *)data;
		}
	    }

	    if (got_format) {

		_XmClipboardFindItem(display, format->data_id, &data, &dlen,
				     NULL, XmCLIP_PROP_DONT_CARE);

		bufstart = (unsigned char *)data + format->offset;

		if (dlen - format->offset > length) {
		    partial_copy = True;
		    fetchlen = length;
		}
		else
		    fetchlen = dlen - format->offset;

		if (clip->retrieve_locked) {

		    if (partial_copy)
			format->offset += fetchlen;
		    else
			format->offset = 0;
	        }

		got_data = True;
	    }

	    privid = format->private_id;

	    _XmClipboardReplaceItem(display, format_id, (unsigned *)format,
				    formatlen, PropModeReplace, 32, True);
	}
    }
    else {
	_XmClipboardGetSelection(display, window, format_name, clip,
				 &data, &dlen);

	bufstart = (unsigned char *)data + clip->offset;

	if (dlen - clip->offset > length) {
	    partial_copy = True;
	    fetchlen = length;
	}
	else
	    fetchlen = dlen - clip->offset;

	if (clip->retrieve_locked) {

	    if (partial_copy)
		clip->offset += fetchlen;
	    else
		clip->offset = 0;
        }

	got_data = True;
    }

    if (got_data) {
	bcopy(bufstart, buffer, fetchlen);

	XtFree((char *)data);
    }

    if (num_bytes)
	*num_bytes = fetchlen;

    if (private_id)
	*private_id = privid;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    if (fetchlen == 0)
	return XmClipboardNoData;

    if (partial_copy)
	return XmClipboardTruncate;

    return XmClipboardSuccess;
}

int
XmClipboardStartCopy(Display *display,
		     Window window,
		     XmString clip_label,
		     Time timestamp,
		     Widget widget,
		     XmCutPasteProc callback,
		     long *item_id)
{
    XmClipboard *clip;
    XmClipboardItem *item;
    unsigned new_item_id, new_label_id;
    int llen;

    if (_XmClipboardLock(display, window) == XmClipboardLocked)
	return XmClipboardLocked;

    clip = _XmClipboardOpen(display, False);
    clip->copy_lock_time = timestamp;
    clip->copy_locked = LOCKED;

    item = (XmClipboardItem *)XtMalloc(sizeof(XmClipboardItem));

    new_item_id = _XmClipboardGetNewItemId(display);
    new_label_id = _XmClipboardGetNewItemId(display);

    item->ipad1 = 0;
    item->item_id = new_item_id;
    item->prop_id = XmCLIP_PROP_ITEM;
    item->display = display;
    item->window = window;
    item->label_id = new_label_id;
    item->item_size = sizeof(XmClipboardItem);
    item->format_count = 0;
    item->del_format_count = 0;
    item->by_name = 0;
    item->marked_for_delete = 0;
    item->ipad2 = 0;
    item->callback = NULL;
    item->widget = NULL;
    item->widget_window = None;

    if (callback != NULL && widget != NULL) {
	item->widget_window = XtWindow(widget);
	item->widget = widget;
	item->callback = callback;
	_XmClipboardSetAccess(display, item->widget_window);
    }

    llen = XmStringLength(clip_label);

    _XmClipboardReplaceItem(display, new_label_id,
			    (unsigned *)clip_label, llen,
			    PropModeReplace, 8, False);

    _XmClipboardReplaceItem(display, new_item_id,
			    (unsigned *)item, sizeof(XmClipboardItem),
			    PropModeReplace, 32, True);

    if (item_id)
	*item_id = new_item_id;

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardStartRetrieve(Display *display,
			 Window window,
			 Time timestamp)
{
    XmClipboard *clip;

    if (_XmClipboardLock(display, window) == XmClipboardLocked)
	return XmClipboardLocked;

    clip = _XmClipboardOpen(display, False);

    clip->retrieve_lock_time = timestamp;
    clip->retrieve_locked = LOCKED;
    clip->offset = 0;

    _XmClipboardClose(display, clip);

    return XmClipboardSuccess;
}

int
XmClipboardUndoCopy(Display *display, Window window)
{
    int status;
    XmClipboard *clip;
    unsigned *data;
    int len;
    XmClipboardFormat *format;
    Boolean item_marked = False;
    int tmp;

    status = _XmClipboardLock(display, window);

    if (status == XmClipboardLocked)
	return status;

    clip = _XmClipboardOpen(display, 0);

    if (clip->current_item2) {

	_XmClipboardFindItem(display, clip->current_item2, &data, &len,
			     NULL, XmCLIP_PROP_FORMAT);

	format = (XmClipboardFormat *)data;

	if (format->window == window && format->display == display) {
	    _XmClipboardMarkItem(display, clip, clip->current_item2, True);
	    item_marked = True;
	}

	XtFree((char *)format);
    }

    if (!item_marked) {
	_XmClipboardMarkItem(display, clip, clip->next_delete, False);

	tmp = clip->current_item2;
	clip->current_item2 = clip->next_delete;
	clip->next_delete = tmp;

	tmp = clip->last_item;
	clip->last_item = clip->current_item;
	clip->current_item = tmp;
    }

    _XmClipboardClose(display, clip);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

int
XmClipboardUnlock(Display *display, Window window, Boolean remove_all_locks)
{
    return _XmClipboardUnlock(display, window, remove_all_locks);
}

int
XmClipboardWithdrawFormat(Display *display,
			   Window window,
			   int data_id)
{
    if (_XmClipboardLock(display, window) == XmClipboardLocked)
	return XmClipboardLocked;

    _XmClipboardDeleteFormat(display, data_id);

    _XmClipboardUnlock(display, window, False);

    return XmClipboardSuccess;
}

