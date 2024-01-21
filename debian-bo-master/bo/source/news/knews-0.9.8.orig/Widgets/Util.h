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
#ifndef Util_h
#define Util_h

#include "Compat.h"

#ifndef XtRLong
#define XtRLong "Long"
#endif
#ifndef XtRJustify
#define XtRJustify "Justify"
#endif

typedef enum {
    JustifyTypeLeft,
    JustifyTypeCenter,
    JustifyTypeRight
} JustifyType;

extern int	MyXWidthToChars(XFontStruct*, const char*, int, int);
extern int	MyXWidthToWChars(XFontStruct*, const XChar2b*, int, int);
extern void	add_WM_DELETE_WINDOW_callback(Widget, XtCallbackProc,
					      XtPointer);
extern int	get_event_xy(XEvent*, int*, int*);
extern Time	get_event_time(XEvent*);
extern int	is_popped_up(Widget);
extern void	popup_under_pointer(Widget, XtGrabKind);
extern Boolean	cvt_string_to_long(Display*,
				   XrmValue*, Cardinal*,
				   XrmValue*, XrmValue*,
				   XtPointer*);
extern Boolean	cvt_string_to_justify(Display*,
				      XrmValue*, Cardinal*,
				      XrmValue*, XrmValue*,
				      XtPointer*);
extern Boolean	cvt_std_sel(Widget, Time, Atom*, Atom*,
			    Atom*, XPointer*, unsigned long*, int*);
extern Pixmap	create_stipple(Screen*);
extern void	release_stipple(Screen*, Pixmap);

extern void	black_and_white(Screen*, Visual*, Pixel*, Pixel*);
extern Pixel	get_black(Widget);
extern Visual	*get_visual(Widget);

extern Atom	intern_atom(Display*, char*);

#endif /* Util_h */
