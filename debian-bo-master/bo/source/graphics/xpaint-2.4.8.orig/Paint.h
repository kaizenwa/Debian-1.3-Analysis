#ifndef _Paint_h
#define _Paint_h

/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |                                                                   | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.  There is no           | */
/* | representations about the suitability of this software for        | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: Paint.h,v 1.8 1996/04/19 08:52:40 torsten Exp $ */

/****************************************************************
 *
 * Paint widget
 *
 ****************************************************************/

/* Resources:

   Name              Class              RepType         Default Value
   ----              -----              -------         -------------
   background        Background         Pixel           XtDefaultBackground
   border            BorderColor        Pixel           XtDefaultForeground
   borderWidth       BorderWidth        Dimension       1
   destroyCallback   Callback           Pointer         NULL
   height            Height             Dimension       0
   mappedWhenManaged MappedWhenManaged  Boolean         True
   sensitive         Sensitive          Boolean         True
   width             Width              Dimension       0
   x                 Position           Position        0
   y                 Position           Position        0

 */

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNpaintResource "paintResource"
#define XtNpattern	 "pattern"
#define XtNgrid	 	 "grid"
#define XtNsnap	 	 "snap"
#define XtNsnapOn 	 "snapOn"
#define XtNlineWidth	 "lineWidth"
#define XtNfatBack	 "fatBack"
#define XtNsizeChanged	 "sizeChanged"
#define XtNundoSize	 "undoSize"
#define XtNzoom	 	 "zoom"
#define XtNpaint	 "paint"
#define XtNzoomX	 "zoomX"
#define XtNzoomY	 "zoomY"
#define XtNdrawWidth	 "drawWidth"
#define XtNdrawHeight	 "drawHeight"
#define XtNcompress	 "compress"
#define XtNdirty	 "dirty"
#define XtNfillRule	 "fillRule"
#define XtNregionCallback	"regionSetCallback"
#define XtNdownX		"downX"
#define XtNdownY		"downY"
#define XtNlineForeground	"lineForeground"
#define XtNlinePattern		"linePattern"
#define XtNlineFillRule		"lineFillRule"
#ifndef XtNreadOnly
#define XtNreadOnly		"readOnly"
#endif
#ifndef XtNcursor
#define XtNcursor		"cursor"
#endif
#define XtNfilename		"filename"
#define XtNmenuwidgets		"menuwidgets"

#define XtCPaintResource "PaintResource"
#define XtCPattern	 "Pattern"
#define XtCGrid		 "Grid"
#define XtCSnap		 "Snap"
#define XtCSnapOn	 "SnapOn"
#define XtCLineWidth	 "LineWidth"
#define XtCFatBack	 "FatBack"
#define XtCSizeChanged	 "SizeChanged"
#define XtCUndoSize	 "UndoSize"
#define XtCZoom	  	 "Zoom"
#define XtCPaint	 "Paint"
#define XtCZoomX	 "ZoomX"
#define XtCZoomY	 "ZoomY"
#define XtCDrawWidth	 "DrawWidth"
#define XtCDrawHeight	 "DrawHeight"
#define XtCCompress	 "Compress"
#define XtCDirty	 "Dirty"
#define XtCFillRule	 "FillRule"
#define XtCRegionCallback	"RegionSetCallback"
#define XtCDownX		"DownX"
#define XtCDownY		"DownY"
#define XtCLineForeground	"LineForeground"
#define XtCLinePattern		"LinePattern"
#define XtCLineFillRule		"LineFillRule"
#ifndef XtCReadOnly
#define XtCReadOnly		"ReadOnly"
#endif
#define XtCfilename		"Filename"
#define XtCmenuwidgets		"Menuwidgets"

#define NMENUWIDGETS	3

/* declare specific PaintWidget class and instance datatypes */

typedef struct _PaintClassRec *PaintClass;
typedef struct _PaintRec *PaintWidget;

/* declare the class constant */

extern WidgetClass paintWidgetClass;

/*
**  Operation callback information
 */
typedef enum {
    opPixmap = 0x01, opWindow = 0x02
} OpSurface;

typedef struct {
    int refCount;
    OpSurface surface;
    Drawable drawable;
    GC first_gc, second_gc, base_gc;
    void *data;
    int isFat;
    int x, y;
    int realX, realY;
    int zoom;
    Pixmap base;
} OpInfo;

typedef float pwMatrix[2][2];

typedef void (*OpEventProc) (Widget, void *, XEvent *, OpInfo *);
void OpRemoveEventHandler(Widget, int, int, Boolean, OpEventProc, void *);
void OpAddEventHandler(Widget, int, int, Boolean, OpEventProc, void *);
void UndoStart(Widget, OpInfo *);
void UndoStartRectangle(Widget, OpInfo *, XRectangle *);
void UndoStartPoint(Widget, OpInfo *, int, int);
void UndoSetRectangle(Widget, XRectangle *);
void UndoGrow(Widget, int, int);

Pixmap PwUndoStart(Widget, XRectangle *);
void PwUndoSetRectangle(Widget, XRectangle *);
void PwUndoAddRectangle(Widget, XRectangle *);
void Undo(Widget);
void Redo(Widget);
void UndoInitialize(PaintWidget pw, int n);

#define PwZoomParent	((int) -10000)

/*
**  Public functions
 */

void PwUpdateFromLast(Widget, XRectangle *);
void PwUpdate(Widget, XRectangle *, Boolean);
void PwUpdateDrawable(Widget, Drawable, XRectangle *);
void PwSetDrawn(Widget, Boolean);
void PwGetPixmap(Widget, Pixmap *, int *, int *);
void PwPutPixmap(Widget w, Pixmap pix);
XRectangle *PwScaleRectangle(Widget, XRectangle *);
XImage *PwGetImage(Widget, XRectangle *);

/*
**  Region routines
 */
void PwRegionSet(Widget, XRectangle *, Pixmap, Pixmap);
void PwRegionSetRawPixmap(Widget, Pixmap);
void PwRegionTear(Widget);
Boolean PwRegionGet(Widget, Pixmap *, Pixmap *);
Pixmap PwGetRawPixmap(Widget);
void PwRegionSetMatrix(Widget, pwMatrix);
void PwRegionAppendMatrix(Widget, pwMatrix);
void PwRegionAddScale(Widget, float *, float *);
void PwRegionSetScale(Widget, float *, float *);
void PwRegionReset(Widget, Boolean);
void PwRegionClear(Widget);
void PwRegionFinish(Widget, Boolean);
Boolean PwRegionOff(Widget w, Boolean flag);

typedef Pixmap(*pwRegionDoneProc) (Widget, XImage *, pwMatrix);

void PwRegionSetDone(Widget, pwRegionDoneProc);

void RegionCrop(PaintWidget paint);
void RegionMove(PaintWidget pw, int dx, int dy);

#endif				/* _Paint_h */
