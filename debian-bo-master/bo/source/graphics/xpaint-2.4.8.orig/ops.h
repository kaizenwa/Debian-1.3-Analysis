/*
 * Prototypes for all *Op.c files.
 */

/* $Id: ops.h,v 1.5 1996/04/19 08:52:07 torsten Exp $ */

/* arcOp.c */
void *ArcAdd(Widget w);
void ArcRemove(Widget w, void *l);
/* blobOp.c */
void *FreehandAdd(Widget w);
void FreehandRemove(Widget w, void *l);
void *FFreehandAdd(Widget w);
void FFreehandRemove(Widget w, void *l);
/* boxOp.c */
void *BoxAdd(Widget w);
void BoxRemove(Widget w, void *l);
void *FBoxAdd(Widget w);
void FBoxRemove(Widget w, void *l);
void BoxSetStyle(Boolean mode);
Boolean BoxGetStyle(void);
/* brushOp.c */
Boolean EraseGetMode(void);
void EraseSetMode(Boolean mode);
void BrushSetMode(Boolean mode);
void BrushSetParameters(float opacity);
void *BrushAdd(Widget w);
void BrushRemove(Widget w, void *l);
void *EraseAdd(Widget w);
void EraseRemove(Widget w, void *l);
void *SmearAdd(Widget w);
void SmearRemove(Widget w, void *l);
void BrushInit(Widget toplevel);
void BrushSelect(Widget w);
/* circleOp.c */
void *CircleAdd(Widget w);
void CircleRemove(Widget w, void *p);
void *FCircleAdd(Widget w);
void FCircleRemove(Widget w, void *p);
void *OvalAdd(Widget w);
void OvalRemove(Widget w, void *p);
void *FOvalAdd(Widget w);
void FOvalRemove(Widget w, void *p);
void CircleSetStyle(Boolean value);
Boolean CircleGetStyle(void);
/* fillOp.c */
void *FillAdd(Widget w);
void FillRemove(Widget w, void *l);
void FillSetMode(int value);
void FillSetTolerance(int value);
void *TFillAdd(Widget w);
void TFillRemove(Widget w, void *l);
void TFillSetMode(int value);
void TfillSetParameters(float voffset, float hoffset, float pad,
			int angle, int steps);
void *FFillAdd(Widget w);
void FFillRemove(Widget w, void *l);
void FFillSetMode(int value);
/* fontOp.c */
void *FontAdd(Widget w);
void FontRemove(Widget w, void *p);
void FontChanged(Widget w);
/* lineOp.c */
void *LineAdd(Widget w);
void LineRemove(Widget w, void *p);
void *RayAdd(Widget w);
void RayRemove(Widget w, void *p);
/* pencilOp.c */
void *PencilAdd(Widget w);
void PencilRemove(Widget w, void *l);
void *DotPencilAdd(Widget w);
void DotPencilRemove(Widget w, void *l);
/* polyOp.c */
void *PolyAdd(Widget w);
void PolyRemove(Widget w, void *l);
void *FPolyAdd(Widget w);
void FPolyRemove(Widget w, void *p);
void *CLineAdd(Widget w);
void CLineRemove(Widget w, void *p);
/* selectOp.c */
void *SelectBoxAdd(Widget w);
void SelectBoxRemove(Widget w, void *p);
void *LassoAdd(Widget w);
void LassoRemove(Widget w, void *p);
void *SelectPolyAdd(Widget w);
void SelectPolyRemove(Widget w, void *p);
void SelectSetCutMode(int value);
int SelectGetCutMode(void);
void SelectSetShapeMode(int value);
/* sprayOp.c */
void SpraySetParameters(int r, int d, int sp);
Boolean SprayGetStyle(void);
void SpraySetStyle(Boolean flag);
void *SprayAdd(Widget w);
void SprayRemove(Widget w, void *p);
