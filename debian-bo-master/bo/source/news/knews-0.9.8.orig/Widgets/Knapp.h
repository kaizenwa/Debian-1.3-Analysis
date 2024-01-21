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
#ifndef Knapp_h
#define Knapp_h

#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif
#ifndef XtCLeftMargin
#define XtCLeftMargin "LeftMargin"
#endif
#ifndef XtCRightMargin
#define XtCRightMargin "RightMargin"
#endif
#ifndef XtCResizable
#define XtCResizable "Resizable"
#endif

#ifndef XtNleftMargin
#define XtNleftMargin "leftMargin"
#endif
#ifndef XtNrightMargin
#define XtNrightMargin "rightMargin"
#endif
#ifndef XtNresizable
#define XtNresizable "resizable"
#endif

typedef struct KnappClassRec*	KnappWidgetClass;
typedef struct KnappRec*	KnappWidget;

extern WidgetClass knappWidgetClass;

extern void	KnappSetLabel(Widget, char*);
extern void	KnappSetActive(Widget, int);
extern void	KnappSetLabelNo(Widget, int, int);
extern int	KnappGetLabelNo(Widget);
extern void	KnappSetSensitive(Widget, int);

#endif /* Knapp_h */
