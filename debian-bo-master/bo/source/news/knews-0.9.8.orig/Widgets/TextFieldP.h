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
#ifndef TextFieldP_h
#define TextFieldP_h

#include "TextField.h"
#include "ScrollableP.h"

typedef struct {
    XtPointer	empty;
} TextFieldClassPart;

typedef struct TextFieldClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    ScrollableClassPart	scrollable_class;
    TextFieldClassPart	textfield_class;
} TextFieldClassRec;

extern TextFieldClassRec textFieldClassRec;

typedef struct {
    char	*buf;
    long	len;
} LineBuf;

typedef struct {
    String		buffer; /* pseudo resource */
    Pixel		fg_pixel;
    Pixel		focus_pixel;
    Pixel		highlight_fg;
    Pixel		highlight_bg;
    XFontStruct		*font;
    XtCallbackList      callback;
    XtCallbackList	tab_callback;
    XtCallbackList	focus_callback;
    Widget		focus_root;
    int			pref_chars;
    int			pref_lines;
    Dimension		internal_height;
    Dimension		internal_width;
    Boolean		single_line;
    Boolean		border_in;
    Boolean		display_caret;
    Boolean		focus_hack;
    Boolean		print_focus;	/* for debugging purposes */
    Boolean		echo_off;
    /* private data */
    GC			gc;
    GC			h_gc;
    Atom		curr_sel;
    Time		sel_time;
    LineBuf		*lines;
    long		n_lines;
    long		sel_start_x;
    long		sel_start_y;
    long		sel_stop_x;
    long		sel_stop_y;
    long		multiply;
    long		caret_x;
    long		caret_y;
    long		mark_x;
    long		mark_y;
    int			char_w;
    int			char_h;
    Boolean		waiting_for_sel;
    Boolean		active;
    Boolean		sel_set;
} TextFieldPart;

typedef struct TextFieldRec {
    CorePart		core;
    ShadowPart		shadow;
    ScrollablePart	scrollable;
    TextFieldPart	textfield;
} TextFieldRec;

#endif /* TextFieldP_h */

