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
#ifndef ScrList_h
#define ScrList_h

#ifndef XtCAllowDnd
#define XtCAllowDnd "AllowDnd"
#endif
#ifndef XtCAtLeastOne
#define XtCAtLeastOne "AtLeastOne"
#endif
#ifndef XtCAtMostOne
#define XtCAtMostOne "AtMostOne"
#endif
#ifndef XtCDepthOne
#define XtCDepthOne "DepthOne"
#endif
#ifndef XtCHighlightColor
#define XtCHighlightColor "HighlightColor"
#endif
#ifndef XtCIndentation
#define XtCIndentation "Indentation"
#endif
#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif
#ifndef XtCInternalItemHeight
#define XtCInternalItemHeight "InternalItemHeight"
#endif
#ifndef XtCInternalItemWidth
#define XtCInternalItemWidth "InternalItemWidth"
#endif
#ifndef XtCInternalWidth
#define XtCInternalWidth "InternalWidth"
#endif
#ifndef XtCNAlloc
#define XtCNAlloc "NAlloc"
#endif
#ifndef XtCPixmapHeight
#define XtCPixmapHeight "PixmapHeight"
#endif
#ifndef XtCPixmapSpacing
#define XtCPixmapSpacing "PixmapSpacing"
#endif
#ifndef XtCPixmapWidth
#define XtCPixmapWidth "PixmapWidth"
#endif
#ifndef XtCPreferredLines
#define XtCPreferredLines "PreferredLines"
#endif
#ifndef XtCPreferredColumns
#define XtCPreferredColumns "PreferredColumns"
#endif
#ifndef XtCRowSpacing
#define XtCRowSpacing "RowSpacing"
#endif
#ifndef XtCUsePixmaps
#define XtCUsePixmaps "UsePixmaps"
#endif
#ifndef XtCMargin
#define XtCMargin "Margin"
#endif
#ifndef XtCPage
#define XtCPage "Page"
#endif

#ifndef XtNallowDnd
#define XtNallowDnd "allowDnd"
#endif
#ifndef XtNatLeastOne
#define XtNatLeastOne "atLeastOne"
#endif
#ifndef XtNatMostOne
#define XtNatMostOne "atMostOne"
#endif
#ifndef XtNsecondCallback
#define XtNsecondCallback "secondCallback"
#endif
#ifndef XtNselectCallback
#define XtNselectCallback "selectCallback"
#endif
#ifndef XtNdndCallback
#define XtNdndCallback "dndCallback"
#endif
#ifndef XtNdndCursor
#define XtNdndCursor "dndCursor"
#endif
#ifndef XtNdepthOne
#define XtNdepthOne "depthOne"
#endif
#ifndef XtNhighlightColor
#define XtNhighlightColor "highlightColor"
#endif
#ifndef XtNindentation
#define XtNindentation "indentation"
#endif
#ifndef XtNinternalItemHeight
#define XtNinternalItemHeight "internalItemHeight"
#endif
#ifndef XtNinternalItemWidth
#define XtNinternalItemWidth "internalItemWidth"
#endif
#ifndef XtNnAlloc
#define XtNnAlloc "nAlloc"
#endif
#ifndef XtNpixmapHeight
#define XtNpixmapHeight "pixmapHeight"
#endif
#ifndef XtNpixmapSpacing
#define XtNpixmapSpacing "pixmapSpacing"
#endif
#ifndef XtNpixmapWidth
#define XtNpixmapWidth "pixmapWidth"
#endif
#ifndef XtNpreferredLines
#define XtNpreferredLines "preferredLines"
#endif
#ifndef XtNpreferredColumns
#define XtNpreferredColumns "preferredColumns"
#endif
#ifndef XtNrowSpacing
#define XtNrowSpacing "rowSpacing"
#endif
#ifndef XtNusePixmaps
#define XtNusePixmaps "usePixmaps"
#endif
#ifndef XtNmarginUp
#define XtNmarginUp "marginUp"
#endif
#ifndef XtNmarginDown
#define XtNmarginDown "marginDown"
#endif
#ifndef XtNpageUp
#define XtNpageUp "pageUp"
#endif
#ifndef XtNpageDown
#define XtNpageDown "pageDown"
#endif

typedef struct ScrListClassRec*		ScrListWidgetClass;
typedef struct ScrListRec*		ScrListWidget;

extern WidgetClass scrListWidgetClass;

extern void	ScrListClearLines(Widget);
extern long	ScrListAddLine(Widget, char*, Pixmap);
extern void	ScrListSetLine(Widget, long, char*, Pixmap);
extern void	ScrListDeleteLine(Widget, long);
extern void	ScrListSetSelected(Widget, long, int);
extern void	ScrListMakeVisible(Widget, long);
extern int	ScrListGetSelected(Widget, long);
extern long	ScrListGetFirstSelected(Widget);
extern long	ScrListGetNextSelected(Widget, long);
extern char    *ScrListGetString(Widget, long);
extern Pixmap	ScrListGetPixmap(Widget, long);
extern void	ScrListPurgePixmap(Widget, Pixmap);
extern long	ScrListEventToIndex(Widget, XEvent*);
extern void	ScrListSetActive(Widget, int);

#endif /* ScrList_h */
