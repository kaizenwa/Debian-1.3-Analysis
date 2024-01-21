/* $Id: graphic.h,v 1.5 1996/04/19 08:52:25 torsten Exp $ */

typedef void (*GraphicAllProc) (Widget, void *);

/* graphic.c */
void GraphicRemove(Widget paint, XtPointer junk, XtPointer junk2);
void GraphicAdd(Widget paint);
void GraphicAll(GraphicAllProc func, void *data);
void GraphicSetOp(void (*stop) (Widget, void *), void *(*start) (Widget));
void *GraphicGetData(Widget w);
Widget AddPattern(Widget group, Widget paint, Pixmap pix, Pixel pxl);
Widget AddPatternInfo(void *piArg, Pixmap pix, Pixel pxl);
void ChangePattern(void *iArg, Pixmap pix);
void zoomCallback(Widget w, XtPointer paintArg, XtPointer junk2);
void EnableLast(Widget paint);
void EnableRevert(Widget paint);
Widget makeGraphicShell(Widget wid);
Widget graphicCreate(Widget shell, int width, int height, int zoom,
		     Pixmap pix, Colormap cmap);
void *GraphicGetReaderId(Widget paint);
void GraphicOpenFile(Widget w, XtPointer fileArg, XtPointer imageArg);
Widget GraphicOpenFileZoom(Widget w, char *file, XtPointer imageArg, int zoom);
void GraphicCreate(Widget wid, int value);
