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
#ifndef ArtText_h
#define ArtText_h

#ifndef XtCMargin
#define XtCMargin "Margin"
#endif
#ifndef XtCPreferredLines
#define XtCPreferredLines "PreferredLines"
#endif
#ifndef XtCPreferredColumns
#define XtCPreferredColumns "PreferredColumns"
#endif
#ifndef XtCWrapLines
#define XtCWrapLines "WrapLines"
#endif

#ifndef XtNhighlightColor
#define XtNhighlightColor "highlightColor"
#endif
#ifndef XtNseparatorMargin
#define XtNseparatorMargin "separatorMargin"
#endif
#ifndef XtNpreferredLines
#define XtNpreferredLines "preferredLines"
#endif
#ifndef XtNpreferredColumns
#define XtNpreferredColumns "preferredColumns"
#endif
#ifndef XtNurlCallback
#define XtNurlCallback "urlCallback"
#endif
#ifndef XtNwrapLines
#define XtNwrapLines "wrapLines"
#endif
#ifndef XtNimageMargin
#define XtNimageMargin "imageMargin"
#endif
#ifndef XtNmargin
#define XtNmargin "margin"
#endif

typedef struct ArtTextClassRec*		ArtTextWidgetClass;
typedef struct ArtTextRec*		ArtTextWidget;

extern WidgetClass artTextWidgetClass;

typedef struct {
    const char	*line;
    int		sel_ok;
    long	start;
    long	stop;
} ArtTextUrlReport;

extern void ArtTextRot13(Widget);
extern void ArtTextClearLines(Widget);
extern void ArtTextAddLine(Widget, const char*, XFontStruct*, Pixel);
extern void ArtTextAddWLine(Widget, XChar2b*, long, XFontStruct*, Pixel);
extern void ArtTextAppendToLast(Widget, const char*);
extern void ArtTextWAppendToLast(Widget, XChar2b*, long);
extern void ArtTextAddSelected(Widget, const char*,
			       XFontStruct*, Pixel, long, long);
extern void ArtTextAddSeparator(Widget, int, int);
extern void ArtTextAddClickable(Widget, const char*, XFontStruct*, Pixel,
				XtCallbackProc, void*);
extern void ArtTextAddImage(Widget, Pixmap, int, int,
			    XtCallbackProc, XtPointer);
extern int  ArtTextDumpToFile(Widget, FILE*);
extern void ArtTextAllocLines(Widget, long);

#endif /* ArtText_h */
