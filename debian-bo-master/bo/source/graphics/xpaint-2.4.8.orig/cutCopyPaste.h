/* $Id: cutCopyPaste.h,v 1.3 1996/04/19 09:16:27 torsten Exp $ */

/* cutCopyPaste.c */
void StdCopyCallback(Widget w, XtPointer paintArg, String * nm, XEvent * event);
void StdPasteCallback(Widget w, XtPointer paintArg, XtPointer junk);
void StdClearCallback(Widget w, XtPointer paintArg, XtPointer junk2);
void StdCutCallback(Widget w, XtPointer paintArg, String * nm, XEvent * event);
void StdDuplicateCallback(Widget w, XtPointer paintArg, XtPointer junk2);
void StdSelectAllCallback(Widget w, XtPointer paintArg, XtPointer junk2);
void StdUndoCallback(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRedoCallback(Widget w, XtPointer paintArg, XtPointer junk2);
#ifdef __IMAGE_H__
void ClipboardSetImage(Widget w, Image * image);
#endif
void StdRegionFlipX(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionFlipY(Widget w, XtPointer paintArg, XtPointer junk2);
void StdLastImgProcess(Widget paint);
void StdRegionInvert(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionSharpen(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionSmooth(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionEdge(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionEmbose(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionOilPaint(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionAddNoise(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionSpread(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionBlend(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionPixelize(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionDespeckle(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionNormContrast(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionSolarize(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionQuantize(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionGrey(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionDirFilt(Widget w, XtPointer paintArg, XtPointer junk2);
void StdRegionTilt(Widget w, XtPointer paintArg, XtPointer junk2);
#ifdef __IMAGE_H__
Image *ImgProcessSetup(Widget paint);
Image *ImgProcessFinish(Widget paint, Image * in, Image * (*func) (Image *));
#endif
void ccpAddUndo(Widget w, Widget paint);
void ccpAddRedo(Widget w, Widget paint);
void ccpAddCut(Widget w, Widget paint);
void ccpAddCopy(Widget w, Widget paint);
void ccpAddPaste(Widget w, Widget paint);
void ccpAddClear(Widget w, Widget paint);
void ccpAddDuplicate(Widget w, Widget paint);
void ccpAddSaveRegion(Widget w, Widget paint);
void ccpAddRegionFlipX(Widget w, Widget paint);
void ccpAddRegionFlipY(Widget w, Widget paint);
void ccpAddRegionInvert(Widget w, Widget paint);
void ccpAddRegionSharpen(Widget w, Widget paint);
void ccpAddRegionEdge(Widget w, Widget paint);
void ccpAddRegionEmbose(Widget w, Widget paint);
void ccpAddRegionOilPaint(Widget w, Widget paint);
void ccpAddRegionNoise(Widget w, Widget paint);
void ccpAddRegionBlend(Widget w, Widget paint);
void ccpAddStdPopup(Widget paint);
