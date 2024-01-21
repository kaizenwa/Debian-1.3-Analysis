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
#ifndef ArtTextP_h
#define ArtTextP_h

#include "ArtText.h"
#include "ScrollableP.h"

typedef struct {
    XtPointer	extension;
} ArtTextClassPart;

typedef struct ArtTextClassRec {
    CoreClassPart       core_class;
    ShadowClassPart	shadow_class;
    ScrollableClassPart	scrollable_class;
    ArtTextClassPart	arttext_class;
} ArtTextClassRec;

extern ArtTextClassRec artTextClassRec;

typedef enum {
    LineTypeString,
    LineTypeWString,
    LineTypeSeparator,
    LineTypeClickable,
    LineTypeImage
} LineType;

typedef struct {
    XtCallbackProc	callback;
    void		*client_data;
} CallbackData;

typedef union TSNode {
    struct {
	int		type;
	union TSNode	*next;
    } gen;
    struct {
	int		type;
	union TSNode	*next;
	Pixel		pixel;
	XFontStruct	*font;
	char		*str;
	long		len;
    } str;
    struct {
	int		type;
	union TSNode	*next;
	Pixel		pixel;
	XFontStruct	*font;
	XChar2b		*str;
	long		len;
    } wstr;
    struct {
	int		type;
	union TSNode	*next;
	Pixel		pixel;
	XFontStruct	*font;
	char		*str;
	CallbackData	*data;
    } cli;
    struct {
	int		type;
	union TSNode	*next;
	Pixel		pixel;
	Dimension	height;
	Dimension	margin;
    } sep;
    struct {
	int		type;
	union TSNode	*next;
	Pixmap		pixmap;
	Dimension	width;
	Dimension	height;
	CallbackData	*data;
    } img;
} TSNode;

typedef struct {
    long	y;
    TSNode	*node;
    long	start;
    long	len;
} TSTable;

typedef struct {
    XFontStruct		*font;
    Pixel		highlight_pixel;
    XtCallbackList	url_callback;
    Dimension		margin;
    Dimension		image_margin;
    Dimension		separator_margin;
    Dimension		preferred_lines;
    Dimension		preferred_columns;
    Boolean		wrap_lines;
    /* private data */
    long		first;
    long		lines;
    long		n_alloc;
    int			max_width;
    TSNode		*stream;
    TSNode		*last;
    TSTable		*table;
    GC			gc;
    Font		gc_fid;
    Pixel		gc_fg;
    Atom		curr_sel;
    Time		sel_time;
    long		sel_start_line;
    long		sel_stop_line;
    int			sel_start_offset;
    int			sel_stop_offset;
    Boolean		sel_ok;
    Boolean		extending;
    Boolean		extend_end;
} ArtTextPart;

typedef struct ArtTextRec {
    CorePart    	core;
    ShadowPart		shadow;
    ScrollablePart	scrollable;
    ArtTextPart		arttext;
} ArtTextRec;

#endif /* ArtTextP_h */
