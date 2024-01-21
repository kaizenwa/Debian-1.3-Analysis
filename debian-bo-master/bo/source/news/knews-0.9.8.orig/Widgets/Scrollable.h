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
#ifndef Scrollable_h
#define Scrollable_h

#ifndef XtCScrBar
#define XtCScrBar "ScrBar"
#endif

#ifndef XtNhBar
#define XtNhBar "hBar"
#endif
#ifndef XtNvBar
#define XtNvBar "vBar"
#endif

typedef struct ScrollableClassRec*	ScrollableWidgetClass;
typedef struct ScrollableRec*		ScrollableWidget;

extern WidgetClass scrollableWidgetClass;

extern void	ScrollableSetHBar(Widget, Widget);
extern void	ScrollableSetVBar(Widget, Widget);
extern void	ScrollableSuspend(Widget);
extern void	ScrollableResume(Widget);
extern void	ScrollableSetHPos(Widget, long);
extern void	ScrollableSetVPos(Widget, long);
extern long	ScrollableGetVPos(Widget);
extern long	ScrollableGetVShown(Widget);
extern long	ScrollableGetVSize(Widget);
extern void	ScrollablePage(Widget, float);

#endif /* Scrollable_h */
