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
#ifndef ArtTree_h
#define ArtTree_h

#include "ArtTreeNode.h"

#ifndef XtCColumnSpacing
#define XtCColumnSpacing "ColumnSpacing"
#endif
#ifndef XtCDashed
#define XtCDashed "Dashed"
#endif
#ifndef XtCDepthOne
#define XtCDepthOne "DepthOne"
#endif
#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif
#ifndef XtCInternalNodeHeight
#define XtCInternalNodeHeight "InternalNodeHeight"
#endif
#ifndef XtCInternalNodeWidth
#define XtCInternalNodeWidth "InternalNodeWidth"
#endif
#ifndef XtCInternalWidth
#define XtCInternalWidth "InternalWidth"
#endif
#ifndef XtCNodeColumns
#define XtCNodeColumns "NodeColumns"
#endif
#ifndef XtCNodeRows
#define XtCNodeRows "NodeRows"
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
#ifndef XtCRowSpacing
#define XtCRowSpacing "RowSpacing"
#endif
#ifndef XtCTree
#define XtCTree "Tree"
#endif
#ifndef XtCWarpPointer
#define XtCWarpPointer "WarpPointer"
#endif
#ifndef XtCVertical
#define XtCVertical "Vertical"
#endif

#ifndef XtNcolumnSpacing
#define XtNcolumnSpacing "columnSpacing"
#endif
#ifndef XtNdepthOne
#define XtNdepthOne "depthOne"
#endif
#ifndef XtNinnerCallback
#define XtNinnerCallback "innerCallback"
#endif
#ifndef XtNinnerColor
#define XtNinnerColor "innerColor"
#endif
#ifndef XtNinnerDashed
#define XtNinnerDashed "innerDashed"
#endif
#ifndef XtNinternalNodeHeight
#define XtNinternalNodeHeight "internalNodeHeight"
#endif
#ifndef XtNinternalNodeWidth
#define XtNinternalNodeWidth "internalNodeWidth"
#endif
#ifndef XtNnodeColumns
#define XtNnodeColumns "nodeColumns"
#endif
#ifndef XtNnodeRows
#define XtNnodeRows "nodeRows"
#endif
#ifndef XtNouterCallback
#define XtNouterCallback "outerCallback"
#endif
#ifndef XtNouterColor
#define XtNouterColor "outerColor"
#endif
#ifndef XtNouterDashed
#define XtNouterDashed "outerDashed"
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
#ifndef XtNrowSpacing
#define XtNrowSpacing "rowSpacing"
#endif
#ifndef XtNrubberColor
#define XtNrubberColor "rubberColor"
#endif
#ifndef XtNselectCallback
#define XtNselectCallback "selectCallback"
#endif
#ifndef XtNtree
#define XtNtree "tree"
#endif
#ifndef XtNwarpPointer
#define XtNwarpPointer "warpPointer"
#endif
#ifndef XtNvertical
#define XtNvertical "vertical"
#endif

typedef struct ArtTreeClassRec*		ArtTreeWidgetClass;
typedef struct ArtTreeRec*		ArtTreeWidget;

extern WidgetClass artTreeWidgetClass;

extern void	ArtTreeSetTree(Widget, ART_TREE_NODE*);
extern void	ArtTreeRedraw(Widget);
extern void	ArtTreeNodeCenter(Widget, ART_TREE_NODE*);
extern void	ArtTreeNodeMakeVisible(Widget, ART_TREE_NODE*);
extern void	ArtTreeNodeSetSelected(Widget, ART_TREE_NODE*, int);
extern void	ArtTreeNodeSetDashed(Widget, ART_TREE_NODE*, int);
extern void	ArtTreeNodeSetInner(Widget, ART_TREE_NODE*, int);
extern void	ArtTreeNodeSetOuter(Widget, ART_TREE_NODE*, int);
extern void	ArtTreeNodeSetPixmap(Widget, ART_TREE_NODE*, Pixmap);
extern Pixmap	ArtTreeNodeGetPixmap(Widget, ART_TREE_NODE*);
extern int	ArtTreeNodeGetSelected(Widget, ART_TREE_NODE*);
extern int	ArtTreeNodeGetDashed(Widget, ART_TREE_NODE*);
extern int	ArtTreeNodeGetInner(Widget, ART_TREE_NODE*);
extern int	ArtTreeNodeGetOuter(Widget, ART_TREE_NODE*);
extern void	ArtTreeNodeNotifyLabel(Widget, ART_TREE_NODE*);
extern void	ArtTreeSetActive(Widget, int);

#endif /* ArtTree_h */
