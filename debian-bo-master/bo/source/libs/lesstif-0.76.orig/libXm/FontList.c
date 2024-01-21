/**
 *
 * $Id: FontList.c,v 1.8 1996/11/28 09:21:13 u27113 Exp $
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

static char rcsid[] = "$Id: FontList.c,v 1.8 1996/11/28 09:21:13 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include <XmI/DebugUtil.h>

/**************************** INTERNAL FUNCTIONS **************************/
static XmFontList
__XmFontListAlloc(int numberOfEntries)
{
    XmFontList newFontList = (XmFontList)XtCalloc(numberOfEntries + 1,
						  sizeof(struct _XmFontListRec));
    return newFontList;
}

static int
__XmFontListNumEntries(XmFontList flist)
{
    int cnt;

    for (cnt = 0; flist[cnt].tag != NULL; cnt++)
	;

    return cnt;
}

static void
__XmFontListDealloc(XmFontList list)
{
    int i;

    for (i = 0; list[i].tag != NULL; i++)
	XtFree(list[i].tag);

    XtFree((char*)list);
}

/************************* LOW LEVEL FUNCTIONS **************************/
XmFontList 
_XmFontListCreateDefault(Display *d)
{
    struct _XmFontListRec defaultEntry;
    XmFontList defaultList;
    XFontStruct *fs = XLoadQueryFont(d, XmDEFAULT_FONT);

    defaultEntry.tag = XmFONTLIST_DEFAULT_TAG;
    defaultEntry.type = XmFONT_IS_FONT;
    defaultEntry.font = (XtPointer)fs;

    defaultList = XmFontListAppendEntry(NULL, &defaultEntry);

    XdbDebug(__FILE__, NULL, "_XmFontListCreateDefault() => 0x%X\n", defaultList);

    return defaultList;
}

/************************** PUBLIC FUNCTIONS ***********************/
XmFontList 
XmFontListAppendEntry(XmFontList old,
		      XmFontListEntry entry)
{
    XmFontList newFontList;
    int i;


    if (old == NULL)
    {
	newFontList = __XmFontListAlloc(1);
	i = 0;
    }
    else
    {
	newFontList = __XmFontListAlloc(__XmFontListNumEntries(old) + 1);
	
	for (i=0; old[i].tag != NULL; i++)
	{
	    newFontList[i].tag = XtNewString(old[i].tag);
	    newFontList[i].type = old[i].type;
	    newFontList[i].font = old[i].font;
	}
	
	__XmFontListDealloc(old);
    }

    newFontList[i].tag = XtNewString(entry->tag);
    newFontList[i].type = entry->type;
    newFontList[i].font = entry->font;

    XdbDebug(__FILE__, NULL, "XmFontListAppendEntry() => 0x%X\n", newFontList);

    return newFontList;
}

XmFontList
XmFontListCreate(XFontStruct *font, XmStringCharSet charset)
{
    /* this function is obsolete and should not be used.  Use
       XmFontListAppendEntry instead */

    XmFontList	r;
    struct _XmFontListRec entry;

    entry.tag = XtNewString(charset);
    entry.type = XmFONT_IS_FONT;
    entry.font = (XtPointer)font;

    r = XmFontListAppendEntry(NULL, &entry);

    XdbDebug(__FILE__, NULL, "XmFontListCreate() => 0x%X\n", r);

    return r;
}

/*
 * this function is "obsolete"; however Xmt uses it.
 */
XmFontList
XmFontListAdd(XmFontList old, XFontStruct *font, XmStringCharSet charset)
{
    XmFontList newFontList;
    struct _XmFontListRec newfont;

    newfont.tag = charset;
    newfont.type = XmFONT_IS_FONT;
    newfont.font = (XtPointer)font;

    newFontList = XmFontListAppendEntry(old, &newfont);

    XdbDebug(__FILE__, NULL, "XmFontListAppendEntry() => 0x%X\n", newFontList);

    return newFontList;
}

XmFontList 
XmFontListCopy(XmFontList fontlist)
{
    XmFontList newFontList;
    int i;

    newFontList = __XmFontListAlloc(__XmFontListNumEntries(fontlist));

    for (i=0; fontlist[i].tag != NULL; i++)
    {
	newFontList[i].tag = XtNewString(fontlist[i].tag);
	newFontList[i].type = fontlist[i].type;
	newFontList[i].font = fontlist[i].font;
    }

    XdbDebug(__FILE__, NULL, "XmFontListCopy(0x%X) => 0x%X\n",
	fontlist, newFontList);

    return newFontList;
}

XmFontListEntry 
XmFontListEntryCreate(char *tag,
		      XmFontType type,
		      XtPointer font)
{
    XmFontListEntry entry = (XmFontListEntry)XtMalloc(sizeof(struct _XmFontListRec));
    
    entry->tag = XtNewString(tag);
    entry->type = type;
    entry->font = font;

    return entry;
}

void 
XmFontListEntryFree(XmFontListEntry *entry)
{
    XdbDebug(__FILE__, NULL, "XmFontListEntryFree(0x%x)\n", entry);

    if (entry)
    {
	XtFree((*entry)->tag);

	/* should we close the Font? */
    }
}

XtPointer
XmFontListEntryGetFont(XmFontListEntry entry,
		       XmFontType *type_return)
{
    if (entry == NULL) {
	XdbDebug(__FILE__, NULL, "XmFontListEntryGetFont(NULL)\n");
	if (type_return)
	    *type_return = XmFONT_IS_FONT;
	return NULL;
    }

    XdbDebug(__FILE__, NULL, "XmFontListEntryGetFont()\n");

    if (type_return)
	*type_return = entry->type;
    return entry->font;
}

char *
XmFontListEntryGetTag(XmFontListEntry entry)
{
    return entry->tag;
}

XmFontListEntry 
XmFontListEntryLoad(Display *display,
		    char *font_name,
		    XmFontType type,
		    char *tag)
{
    XmFontListEntry entry = (XmFontListEntry)XtMalloc(sizeof(struct _XmFontListRec));
    XrmValue fromString, toFont, cvtArg;
    Boolean success = False;

    XdbDebug(__FILE__, NULL, "XmFontListEntryLoad(%s)\n", font_name);

    fromString.addr = font_name;
    fromString.size = strlen(font_name) + 1;

    cvtArg.addr = (XtPointer)&display;
    cvtArg.size = sizeof(Display *);

    switch (type)
    {
    case XmFONT_IS_FONT:
	toFont.addr = (XtPointer)&(entry->font);
	toFont.size = sizeof(XFontStruct*);

	success = XtCallConverter(display, XtCvtStringToFontStruct, &cvtArg, (Cardinal)1, &fromString, &toFont, NULL);
	break;
    case XmFONT_IS_FONTSET:
	toFont.addr = (XtPointer)&(entry->font);
	toFont.size = sizeof(XFontSet);

	success = XtCallConverter(display, XtCvtStringToFontSet, &cvtArg, (Cardinal)1, &fromString, &toFont, NULL);
	break;
    }

    if (!success)
	return NULL;
    else 
    {
	entry->tag = XtNewString(tag);
	entry->type = type;

	return entry;
    }
}

void 
XmFontListFree(XmFontList list)
{
    XdbDebug(__FILE__, NULL, "XmFontListFree(0x%X)\n", list);

    __XmFontListDealloc(list);
}

void 
XmFontListFreeFontContext(XmFontContext context)
{
    XdbDebug(__FILE__, NULL, "XmFontListFreeFontContext(0x%X)\n", context);
    XtFree((char*)context);
}

Boolean 
XmFontListInitFontContext(XmFontContext *context,
			  XmFontList fontlist)
{
    if (fontlist && context)
    {
	*context = (XmFontContext)XtMalloc(sizeof(struct _XmFontListContextRec));
	(*context)->fontlist = fontlist;
	(*context)->current_entry = -1;

	return TRUE;
    }
    else
	return FALSE;
}

XmFontListEntry 
XmFontListNextEntry(XmFontContext context)
{
    context->current_entry++;

    if (context->current_entry < __XmFontListNumEntries(context->fontlist))
	return &context->fontlist[context->current_entry];
    else
	return NULL;
}

Boolean
XmFontListGetNextFont(XmFontContext context, XmStringCharSet *charset, XFontStruct **font)
{
    context->current_entry++;

    if (context->current_entry < __XmFontListNumEntries(context->fontlist))
    {
	if (context->fontlist[context->current_entry].type == XmFONT_IS_FONT) {
	    *font = (XFontStruct*)(context->fontlist
					[context->current_entry].font);
	    *charset = XtNewString(context->fontlist
					[context->current_entry].tag);
	}
	else
	{
	    XFontStruct **foo;
	    char **bar;

	    if (XFontsOfFontSet((XFontSet)(context->fontlist
						[context->current_entry].font),
					   &foo,
					   &bar) < 1)
		*font = NULL;
	    else
		*font = foo[0];
	    *charset = XtNewString(context->fontlist
					[context->current_entry].tag);
	}
	return True;
    }
    else
	return False;
}

XmFontList 
XmFontListRemoveEntry(XmFontList oldlist,
		      XmFontListEntry entry)
{    
    XmFontList newFontList;
    int i,j;

    newFontList = __XmFontListAlloc(__XmFontListNumEntries(oldlist)-1);

    j=0;

    for (i=0; oldlist[i].tag != NULL; i++)
    {
	if (!(!strcmp(entry->tag, oldlist[i].tag) &&
	      entry->type == oldlist[i].type &&
	      entry->font == oldlist[i].font))
	{
	    newFontList[j].tag = XtNewString(oldlist[i].tag);
	    newFontList[j].type = oldlist[i].type;
	    newFontList[j].font = oldlist[i].font;
	    
	    j++;
	}
    }

    __XmFontListDealloc(oldlist);

    return newFontList;
}
