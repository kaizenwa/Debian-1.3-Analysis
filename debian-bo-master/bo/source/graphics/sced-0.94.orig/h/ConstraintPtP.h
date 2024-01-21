/*
**    ScEd: A Constraint Based Scene Editor.
**    Copyright (C) 1994-1995  Stephen Chenney (stephen@cs.su.oz.au)
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
**	ConstraintPtP.h: Private header for constraintPtWidgetClass.
*/

#define PATCHLEVEL 0

#ifndef _ConstraintPtP_h
#define _ConstraintPtP_h

#include <X11/CompositeP.h>
#include <ConstraintPt.h>

typedef struct {
	int	empty;
	} ConstraintPtClassPart;

typedef struct _ConstraintPtClassRec {
	CoreClassPart			core_class;
	CompositeClassPart		composite_class;
	ConstraintPtClassPart	constraintbox_class;
	} ConstraintPtClassRec;

extern ConstraintPtClassRec constraintPtClassRec;

typedef struct {
	/* Resources. */
	Pixel			foreground;
	int				h_space;
	String			label;
	Pixmap			menu_bitmap;
	XtCallbackList	extra_callback;
	String			extra_label;
	Boolean			have_extra_toggle;
	Boolean			extra_state;
	XtCallbackList	title_callback;
	Boolean			title_is_menu;
	String			title;
	String			title_menu_name;
	XtCallbackList	toggle_callback;
	Boolean			toggle_required;
	Boolean			toggle_state;
	} ConstraintPtPart;

typedef struct _ConstraintPtRec {
	CorePart			core;
	CompositePart		composite;
	ConstraintPtPart	constraint_pt;
	} ConstraintPtRec;

#endif /* _ConstraintPtP_h */

