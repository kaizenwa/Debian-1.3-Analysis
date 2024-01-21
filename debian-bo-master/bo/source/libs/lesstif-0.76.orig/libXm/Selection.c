/**
 *
 * $Id: Selection.c,v 1.3 1996/11/28 09:21:54 u27113 Exp $
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

static char rcsid[] = "$Id: Selection.c,v 1.3 1996/11/28 09:21:54 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextFP.h>
#include <Xm/TextSelP.h>
#include <Xm/TextFSelP.h>

#include <XmI/DebugUtil.h>

Boolean
_XmTextFieldConvert(Widget w,
		    Atom *selection,
		    Atom *target,
		    Atom *type,
		    XtPointer *value,
		    unsigned long *length,
		    int *format)
{
    return False;
}

void
_XmTextFieldLoseSelection(Widget w, Atom *selection)
{
}

Boolean
_XmTextConvert(Widget w,
	       Atom *selection,
	       Atom *target,
	       Atom *type,
	       XtPointer *value,
	       unsigned long *length,
	       int *format)
{
    return False;
}

void
_XmTextLoseSelection(Widget w, Atom *selection)
{
}
