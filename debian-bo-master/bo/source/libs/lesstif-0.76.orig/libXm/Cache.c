/**
 *
 * $Id: Cache.c,v 1.3 1996/11/28 09:20:46 u27113 Exp $
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

static char rcsid[] = "$Id: Cache.c,v 1.3 1996/11/28 09:20:46 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/ExtObjectP.h>
#include <X11/Xfuncs.h>

#include <XmI/DebugUtil.h>

void
_XmCacheDelete(XtPointer data)
{
    XmGadgetCacheRefPtr node;

    node = (XmGadgetCacheRefPtr)DataToGadgetCache(data);
    node->ref_count--;

    XdbDebug(__FILE__, NULL, "Deleting %08x: refcount: %d\n",
	     data, node->ref_count);

    if (node->ref_count == 0) {
        XdbDebug(__FILE__, NULL, "Ref count 0: deleting %08x\n", data);
	node->prev->next = node->next;
	node->next->prev = node->prev;
	XtFree((XtPointer)node);
    }
}

void
_XmCacheCopy(XtPointer src, XtPointer dest, size_t size)
{
    bcopy(src, dest, size);
}

XtPointer
_XmCachePart(XmCacheClassPartPtr cp, XtPointer cpart, size_t size)
{
    XmGadgetCachePtr list;
    XtPointer *newpart;

    XdbDebug(__FILE__, NULL,
	     "Attempting to cache a part.\n");

    /*
     * Guess what.  Motif doesn't save memory and self link the initial node.
     * Big surprise.
     */
    if (ClassCacheHead(cp).next == NULL) {
	ClassCacheHead(cp).prev = &ClassCacheHead(cp);
	ClassCacheHead(cp).next = &ClassCacheHead(cp);
	ClassCacheHead(cp).ref_count = -1;
    }

    /* search cache */
    list = ClassCacheHead(cp).next;

    while (list != &ClassCacheHead(cp)) {
	XmGadgetCachePtr tmp;

	if (ClassCacheCompare(cp)(cpart, CacheDataPtr(list))) {
	    XdbDebug(__FILE__, NULL, "Cache hit: %08x\n", CacheDataPtr(list));
	    if (cpart != CacheDataPtr(list))
		list->ref_count++;
	    return CacheDataPtr(list); 
	}
	tmp = list->next;
	if (cpart == CacheDataPtr(list)) {
	    XdbDebug(__FILE__, NULL,
		     "In cache, but invalid.  Deleting old entry.\n");
	    _XmCacheDelete(cpart);
	}
	list = tmp;
    }

    XdbDebug(__FILE__, NULL,
	     "Not in cache.  Adding new entry of size %d.\n", size);
    /* not in cache, add new entry */
    list = (XmGadgetCachePtr)XtMalloc(sizeof(XmGadgetCache) + size);
    newpart = (XtPointer)((char *)list + sizeof(XmGadgetCache));

    list->prev = ClassCacheHead(cp).prev;
    ClassCacheHead(cp).prev->next = list;
    list->next = &ClassCacheHead(cp);
    ClassCacheHead(cp).prev = list;
    list->ref_count = 1;

    ClassCacheCopy(cp)(cpart, (XtPointer)newpart, size);

    return newpart;
}
