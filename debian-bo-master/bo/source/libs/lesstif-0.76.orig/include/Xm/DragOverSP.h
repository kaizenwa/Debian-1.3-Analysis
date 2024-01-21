/**
 *
 * $Id: DragOverSP.h,v 1.3 1996/11/07 07:15:20 u27113 Exp $
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

#ifndef XM_DRAGOVERSP_H
#define XM_DRAGOVERSP_H

#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <Xm/XmP.h>
#include <Xm/DragIconP.h>
#include <Xm/DragOverS.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DOExpose(do) \
	((XtClass(do))->core_class.expose) ((Widget)(do), NULL, NULL)

typedef struct {
    XtPointer extension;
} XmDragOverShellClassPart;

typedef struct _XmDragOverShellClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    WMShellClassPart wm_shell_class;
    VendorShellClassPart vendor_shell_class;
    XmDragOverShellClassPart dragOver_shell_class;
} XmDragOverShellClassRec;

extern XmDragOverShellClassRec xmDragOverShellClassRec;

typedef struct _XmBackingRec {
    Position x, y;
    Pixmap pixmap;
} XmBackingRec, *XmBacking;

typedef struct _XmDragOverBlendRec {
    XmDragIconObject sourceIcon;
    Position sourceX;
    Position sourceY;
    XmDragIconObject mixedIcon;
    GC gc;
} XmDragOverBlendRec, *XmDragOverBlend;
	
typedef struct _XmDragOverShellPart {
    Position hotX;
    Position hotY;
    unsigned char cursorState;
    unsigned char mode;
    unsigned char activeMode;
    
    Position initialX;
    Position initialY;
    
    XmDragIconObject stateIcon;
    XmDragIconObject opIcon;
    
    XmDragOverBlendRec cursorBlend;
    XmDragOverBlendRec rootBlend;
    Pixel cursorForeground;
    Pixel cursorBackground;
    Cursor ncCursor;
    Cursor activeCursor;
    
    XmBackingRec backing;
    Pixmap tmpPix;
    Pixmap tmpBit;
    Boolean isVisible;
} XmDragOverShellPart;

typedef struct _XmDragOverShellRec {
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    WMShellPart wm;
    VendorShellPart vendor;
    XmDragOverShellPart drag;
} XmDragOverShellRec;

#ifdef __cplusplus
}
#endif

#endif /* XM_DRAGOVERSP_H */
