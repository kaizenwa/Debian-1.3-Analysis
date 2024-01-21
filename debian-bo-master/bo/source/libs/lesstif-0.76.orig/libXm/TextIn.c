/*                   -*-Mode: c; c-file-style: "GNU"-*-              */
/**
 *
 * $Id: TextIn.c,v 1.9 1996/11/28 09:22:10 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

 static char rcsid[] = "$Id: TextIn.c,v 1.9 1996/11/28 09:22:10 u27113 Exp $";

#include <LTconfig.h>
#include <Xm/XmP.h>
#include <XmI/XmI.h>
#include <Xm/TextP.h>
#include <X11/Xfuncs.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#define ANSI_STRING
#else
#include <strings.h>
#endif
#include <stdlib.h>

#include <XmI/DebugUtil.h>

static void Activate();
static void MoveBackwardChar();
static void MoveBackwardParagraph();
static void MoveBackwardWord();
static void RingBell();
static void MoveBeginningOfFile();
static void MoveToLineStart();
static void ClearSelection();
static void CopyClipboard();
static void CopyPrimary();
static void ProcessCopy();
static void CutClipboard();
static void CutPrimary();
static void DeleteForwardChar();
static void DeleteBackwardChar();
static void DeleteForwardWord();
static void DeleteBackwardWord();
static void DeleteCurrentSelection();
static void DeleteToEndOfLine();
static void DeleteToStartOfLine();
static void DeselectAll();
static void DoSelection();
static void MoveEndOfFile();
static void MoveToLineEnd();
static void ExtendEnd();
static void StartExtendSelection();
static void MoveForwardChar();
static void MoveForwardParagraph();
static void MoveForwardWord();
static void DoGrabFocus();
static void InsertString();
static void KeySelection();
static void KillForwardChar();
static void KillForwardWord();
static void KillBackwardChar();
static void KillBackwardWord();
static void KillCurrentSelection();
static void KillToEndOfLine();
static void KillToStartOfLine();
static void MoveDestination();
static void ProcessMove();
static void InsertNewLine();
static void InsertNewLineAndBackup();
static void InsertNewLineAndIndent();
static void MoveNextLine();
static void MoveNextPage();
static void TraverseNextTabGroup();
static void MovePageLeft();
static void MovePageRight();
static void PasteClipboard();
static void TraversePrevTabGroup();
static void MovePreviousLine();
static void MovePreviousPage();
static void ProcessBDrag();
static void ProcessCancel();
static void ProcessDown();
static void ProcessUp();
static void ProcessHome();
static void ProcessReturn();
static void ProcessShiftDown();
static void ProcessShiftUp();
static void ProcessTab();
static void RedrawDisplay();
static void ScrollOneLineDown();
static void ScrollOneLineUp();
static void SecondaryAdjust();
static void SecondaryNotify();
static void ExtendSelection();
static void SelectAll();
static void StartExtendSelection();
static void SelfInsert();
static void SetAnchor();
static void SetCursorPosition();
static void SetSelectionHint();
static void ToggleAddMode();
static void TraverseHome();
static void TraverseDown();
static void TraverseUp();
static void TextFocusIn();
static void TextFocusOut();
static void TextLeave();
static void VoidAction();
static void UnKill();
extern void LineTable(XmTextWidget w);

#define Offset(field) XtOffsetOf(XmTextInnerRec, inner.field)

/* Resources for the TextIn class */
static XtResource input_resources[] =
{
  {
    XmNpendingDelete, XmCPendingDelete, XmRBoolean,
    sizeof(Boolean), Offset(in.pendingdelete),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNselectionArray, XmCSelectionArray, XmRPointer,
    sizeof(XtPointer), Offset(in.sarray),
    XmRImmediate, (XtPointer) NULL
	/* FIXME: Motif has XmRInt, wacky value here */
  },
  {
    XmNselectionArrayCount, XmCSelectionArrayCount, XmRInt,
    sizeof(int), Offset(in.sarraycount),
    XmRImmediate, (XtPointer) 3
	/* FIXME: Motif has XmRInt, (XtPointer)whacko value here */
  },
  {
    XmNselectThreshold, XmCSelectThreshold, XmRInt,
    sizeof(int), Offset(in.threshold),
    XmRImmediate, (XtPointer) 5
  },
};

/*
 * MLM: FIXME -- without these, misc/test1.c won't compile.  Perhaps these
 * should be installed on the subparts?
 */
char _XmTextIn_XmTextEventBindings1[] =
    ":m <Key>osfPrimaryPaste:   cut-primary() \n\
     :a <Key>osfPrimaryPaste:   cut-primary() \n\
     :<Key>osfPrimaryPaste:     copy-primary() \n\
     :m <Key>osfCut:            cut-primary() \n\
     :a <Key>osfCut:            cut-primary() \n\
     :<Key>osfCut:              cut-clipboard() \n\
     :<Key>osfPaste:            paste-clipboard() \n\
     :m <Key>osfCopy:           copy-primary() \n\
     :a <Key>osfCopy:           copy-primary() \n\
     :<Key>osfCopy:             copy-clipboard() \n\
     s c <Key>osfBeginLine:     beginning-of-file(extend) \n\
     :c <Key>osfBeginLine:      beginning-of-file() \n\
     s <Key>osfBeginLine:       beginning-of-line(extend) \n\
     :<Key>osfBeginLine:        beginning-of-line() \n\
     s c <Key>osfEndLine:       end-of-file(extend) \n\
     :c <Key>osfEndLine:        end-of-file() \n\
     s <Key>osfEndLine:         end-of-line(extend) \n\
     :<Key>osfEndLine:          end-of-line() \n\
     s <Key>osfPageLeft:        page-left(extend) \n\
     :<Key>osfPageLeft:         page-left() \n\
     s c <Key>osfPageUp:        page-left(extend) \n\
     :c <Key>osfPageUp:         page-left() \n\
     s <Key>osfPageUp:          previous-page(extend) \n\
     :<Key>osfPageUp:           previous-page() \n\
     s <Key>osfPageRight:       page-right(extend) \n\
     :<Key>osfPageRight:        page-right() \n\
     s c <Key>osfPageDown:      page-right(extend) \n\
     :c <Key>osfPageDown:       page-right() \n\
     s <Key>osfPageDown:        next-page(extend) \n\
     :<Key>osfPageDown:         next-page() \n\
     :<Key>osfClear:            clear-selection() \n\
     :<Key>osfBackSpace:        delete-previous-character() \n\
     s m <Key>osfDelete:        cut-primary() \n\
     s a <Key>osfDelete:        cut-primary() \n\
     s <Key>osfDelete:          cut-clipboard() \n\
     :c <Key>osfDelete:         delete-to-end-of-line() \n\
     :<Key>osfDelete:           delete-next-character() \n";


char _XmTextIn_XmTextEventBindings2[] =
    ":c m <Key>osfInsert:       copy-primary() \n\
     :c a <Key>osfInsert:       copy-primary() \n\
     s <Key>osfInsert:          paste-clipboard() \n\
     :c <Key>osfInsert:         copy-clipboard() \n\
     :s <Key>osfSelect:         key-select() \n\
     :<Key>osfSelect:           set-anchor() \n\
     :<Key>osfActivate:         activate() \n\
     :<Key>osfAddMode:          toggle-add-mode() \n\
     :<Key>osfHelp:             Help() \n\
     :<Key>osfCancel:           process-cancel() \n\
     s c <Key>osfLeft:          backward-word(extend) \n\
     :c <Key>osfLeft:           backward-word() \n\
     s <Key>osfLeft:            key-select(left) \n\
     :<Key>osfLeft:             backward-character() \n\
     s c <Key>osfRight:         forward-word(extend) \n\
     :c <Key>osfRight:          forward-word() \n\
     s <Key>osfRight:           key-select(right) \n\
     :<Key>osfRight:            forward-character() \n\
     s c <Key>osfUp:            backward-paragraph(extend) \n\
     :c <Key>osfUp:             backward-paragraph() \n\
     s <Key>osfUp:              process-shift-up() \n\
     :<Key>osfUp:               process-up() \n\
     s c <Key>osfDown:          forward-paragraph(extend) \n\
     :c <Key>osfDown:           forward-paragraph() \n\
     s <Key>osfDown:            process-shift-down() \n\
     :<Key>osfDown:             process-down() \n\
     c ~m ~a <Key>slash:        select-all() \n\
     c ~m ~a <Key>backslash:    deselect-all() \n\
     s ~m ~a <Key>Tab:          prev-tab-group() \n\
     c ~m ~a <Key>Tab:          next-tab-group() \n\
     ~m ~a <Key>Tab:            process-tab() \n\
     c ~s ~m ~a <Key>Return:    activate() \n\
     ~c ~s ~m ~a <Key>Return:   process-return() \n\
     c ~s ~m ~a <Key>space:     set-anchor() \n\
     c s ~m ~a <Key>space:      key-select() \n\
     s ~c ~m ~a <Key>space:     self-insert() \n\
     <Key>:                     self-insert() \n";


char _XmTextIn_XmTextEventBindings3[] =
    "<Unmap>:                    unmap() \n\
     <EnterWindow>:              enter() \n\
     <LeaveWindow>:              leave() \n\
     <FocusIn>:                  focusIn() \n\
     <FocusOut>:                 focusOut() \n\
     ~c s ~m ~a <Btn1Down>:      extend-start() \n\
     c ~s ~m ~a <Btn1Down>:      move-destination() \n\
     ~c ~s ~m ~a <Btn1Down>:     grab-focus() \n\
     ~c ~m ~a <Btn1Motion>:      extend-adjust() \n\
     ~c ~m ~a <Btn1Up>:          extend-end() \n\
     <Btn2Down>:                 process-bdrag() \n\
     m ~a <Btn2Motion>:          secondary-adjust() \n\
     ~m a <Btn2Motion>:          secondary-adjust() \n\
     ~s <Btn2Up>:                copy-to() \n\
     ~c <Btn2Up>:                move-to() ";

/*
 * MLM: FIXME -- I don't know if this is a bug in the Solaris/IXI/Motif
 * header file (TransltnsP.h), or if there is a fourth TextIn binding.
 * Check this.
 */
#if 0
char _XmTextIn_XmTextEventBindings3[] =
    "<Unmap>:                        unmap() \n\
     <EnterWindow>:                  enter() \n\
     <LeaveWindow>:                  leave() \n\
     <FocusIn>:                      focusIn() \n\
     <FocusOut>:                     focusOut() \n\
     ~c s ~m ~a <Btn1Down>:          extend-start() \n\
     c ~s ~m ~a <Btn1Down>:          move-destination() \n\
     ~c ~s ~m ~a <Btn1Down>:         grab-focus() \n\
     ~c ~m ~a <Btn1Motion>:          extend-adjust() \n\
     ~c ~m ~a <Btn1Up>:              extend-end() \n\
     <Btn2Down>:                     process-bdrag() \n\
     m ~a <Btn2Motion>:              secondary-adjust() \n\
     ~m a <Btn2Motion>:              secondary-adjust() \n\
     ~s <Btn2Up>:                    copy-to() \n\
     ~c <Btn2Up>:                    move-to()";
#endif

/* action table table */

static XtActionsRec ZdefaultTextActionsTable[] =
{
  {"activate", Activate},
  {"backward-character", MoveBackwardChar},
  {"backward-paragraph", MoveBackwardParagraph},
  {"backward-word", MoveBackwardWord},
  {"beep", RingBell},
  {"beginning-of-file", MoveBeginningOfFile},
  {"beginning-of-line", MoveToLineStart},
  {"clear-selection", ClearSelection},
  {"copy-clipboard", CopyClipboard},
  {"copy-primary", CopyPrimary},
  {"copy-to", ProcessCopy},
  {"cut-clipboard", CutClipboard},
  {"cut-primary", CutPrimary},
  {"delete-next-character", DeleteForwardChar},
  {"delete-previous-character", DeleteBackwardChar},
  {"delete-next-word", DeleteForwardWord},
  {"delete-previous-word", DeleteBackwardWord},
  {"delete-selection", DeleteCurrentSelection},
  {"delete-to-end-of-line", DeleteToEndOfLine},
  {"delete-to-start-of-line", DeleteToStartOfLine},
  {"deselect-all", DeselectAll},
  {"do-quick-action", VoidAction},
  {"end-of-file", MoveEndOfFile},
  {"end-of-line", MoveToLineEnd},
  {"enter", _XmPrimitiveEnter},
  {"extend-adjust", ExtendSelection},
  {"extend-end", ExtendEnd},
  {"extend-start", StartExtendSelection},
  {"forward-character", MoveForwardChar},
  {"forward-paragraph", MoveForwardParagraph},
  {"forward-word", MoveForwardWord},
  {"grab-focus", DoGrabFocus},
  {"Help", _XmPrimitiveHelp},
  {"insert-string", InsertString},
  {"key-select", KeySelection},
  {"kill-next-character", KillForwardChar},
  {"kill-next-word", KillForwardWord},
  {"kill-previous-character", KillBackwardChar},
  {"kill-previous-word", KillBackwardWord},
  {"kill-selection", KillCurrentSelection},
  {"kill-to-end-of-line", KillToEndOfLine},
  {"kill-to-start-of-line",KillToStartOfLine},
  {"leave",TextLeave},
  {"move-destination", MoveDestination},
  {"move-to", ProcessMove},
  {"newline", InsertNewLine},
  {"newline-and-backup", InsertNewLineAndBackup},
  {"newline-and-indent", InsertNewLineAndIndent},
  {"next-line", MoveNextLine},
  {"next-page", MoveNextPage},
  {"next-tab-group", TraverseNextTabGroup},
  {"page-left", MovePageLeft},
  {"page-right", MovePageRight},
  {"paste-clipboard", PasteClipboard},
  {"prev-tab-group", TraversePrevTabGroup},
  {"previous-line", MovePreviousLine},
  {"previous-page", MovePreviousPage},
  {"process-bdrag", ProcessBDrag},
  {"process-cancel",ProcessCancel},
  {"process-down", ProcessDown},
  {"process-up", ProcessUp},
  {"process-home", ProcessHome},
  {"process-return", ProcessReturn},
  {"process-shift-down", ProcessShiftDown},
  {"process-shift-up", ProcessShiftUp},
  {"process-tab", ProcessTab},
  {"quick-copy-set", VoidAction},
  {"quick-cut-set", VoidAction},
  {"redraw-display", RedrawDisplay},
  {"scroll-one-line-down", ScrollOneLineDown},
  {"scroll-one-line-up", ScrollOneLineUp},
  {"secondary-adjust", SecondaryAdjust},
  {"secondary-notify", SecondaryNotify},
  {"select-adjust", DoSelection},
  {"select-all", SelectAll},
  {"select-end", DoSelection},
  {"select-start", StartExtendSelection},
  {"self-insert", SelfInsert},
  {"set-anchor", SetAnchor},
  {"set-insertion-point", SetCursorPosition},
  {"set-selection-hint", SetSelectionHint},
  {"toggle-add-mode", ToggleAddMode},
  {"traverse-home", TraverseHome},
  {"traverse-next", TraverseDown},
  {"traverse-prev", TraverseUp},
  {"focusIn", TextFocusIn},
  {"focusOut", TextFocusOut},
  {"unkill", UnKill},
  {"unmap", _XmPrimitiveUnmap},
};


XtPointer _XmdefaultTextActionsTable = ZdefaultTextActionsTable;

Cardinal _XmdefaultTextActionsTableSize = XtNumber(ZdefaultTextActionsTable);

static void
Invalidate(XmTextWidget w, XmTextPosition start, XmTextPosition end,
  long delta)
{
}

static void
InputGetValues(Widget aw, ArgList args, Cardinal num_args)
{
}

static void
InputSetValues(Widget old, Widget request, Widget new,
  ArgList args, Cardinal * num_args)
{
}

static void
InputDestroy(Widget aw)
{
  XmTextWidget w = (XmTextWidget) aw;
  
  DBGW("destroy");

  (*Text_Source(w)->RemoveWidget)(Text_Source(w),w);
  XtFree((char *)Text_Input(w));
}

static void
GetSecResData(XmSecondaryResourceData *secres)
{
}

InputRec inputRec = {
    /* _InputDataRec             */ NULL,
    /* InputInvalidateProc       */ Invalidate,
    /* InputGetValuesProc        */ InputGetValues,
    /* InputSetValuesProc        */ InputSetValues,
    /* XtWidgetProc              */ InputDestroy,
    /* InputGetSecResProc        */ GetSecResData
};

void
_XmTextInputCreate(Widget aw, ArgList args, Cardinal num_args)
{
  XmTextWidget w = (XmTextWidget) aw;
  XmTextInnerWidget iw = (XmTextInnerWidget) w->text.inner_widget;
  InputData i;

  DBGW("_XmTextInputCreate");
  
  Text_Input(w) = (Input) XtMalloc(sizeof(InputRec));
  bcopy(&inputRec, Text_Input(w), sizeof(InputRec));

  i = Text_Input(w)->data = &iw->inner.in;
  XtGetSubresources (aw, iw, 
		     aw->core.name, 
		     aw->core.widget_class->core_class.class_name,
		     input_resources,
		     XtNumber(input_resources),
		     args, num_args);


  if (!Text_Source(w))
    Text_Source(w)=_XmStringSourceCreate(Text_Value(w),False);

  (*Text_Source(w)->AddWidget)(Text_Source(w),w);

  In_SelArray(i)=NULL;
  In_SelArrayIndex(i)=0;
  
  In_LastTime(i)=0;
}


/* Scan type utilities --------------------------------------------------- */

static XmTextScanType
ScanType(XmTextWidget w)
{
  InputData i=Text_InputData(w);
  XmTextScanType type;

  if (In_SelArray(i)) {
    if (In_SelArrayIndex(i)>=In_SelArrayCount(i))
      In_SelArrayIndex(i)=0;
    type=In_SelArray(i)[In_SelArrayIndex(i)];
  }
  else {
    switch (In_SelArrayIndex(i)) {
      case 1:
        type=XmSELECT_WORD;
        break;
      case 2:
        type=XmSELECT_ALL;
        break;
      default:
        type=XmSELECT_POSITION;
        In_SelArrayIndex(i)=0;
        break;
    }
  }

  return type;
}



/* High level text insert and delete routines ------------------------------ */

static void
VerifyBell(XmTextWidget w)
{
  if (Text_VerifyBell(w))
    XBell(XtDisplay((Widget) w), 50);
}

#if 0
static Boolean
DoCursorMove(XmTextWidget w, XEvent * ev, XmTextPosition pos)
{
  Widget aw = (Widget) w;

  if (pos > Text_LastPos(w))
    pos = Text_LastPos(w);

  cbs.doit = True;
  cbs.newInsert = pos;
  if (Text_MotionVerifyCallback(w)) {
    cbs.reason = XmCR_MOVING_INSERT_CURSOR;
    cbs.event = ev;
    cbs.currInsert = Text_CursorPos(w);
    cbs.startPos = cbs.endPos = 0;
    cbs.text = NULL;

    XtCallCallbacks(aw, XmNmotionVerifyCallback, &cbs);

    if (cbs.doit) {
      pos = cbs.newInsert;
    }
  }
 
  if (cbs.doit)
    _XmTextSetCursorPosition(aw, pos);
  else {
    VerifyBell(w);
  } 
  return cbs.doit;
}
#endif
static void
DoDelete(XmTextWidget w, XEvent * ev, XmTextPosition start, XmTextPosition end)
{
  Widget aw = (Widget) w;
  XmTextVerifyCallbackStruct cbs;
  XmTextBlockRec blockrec;

  if (end <= 0)
    end=0;

  blockrec.ptr = NULL;
  blockrec.length = 0;
  blockrec.format = XmFMT_8_BIT;

  cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
  cbs.event = ev;
  cbs.startPos = cbs.newInsert = start;
  cbs.endPos = end;
  cbs.currInsert = Text_CursorPos(w);
  cbs.text = &blockrec;
  cbs.doit = True;

  if (Text_ModifyVerifyCallback(w)) {
    XtCallCallbacks((Widget) w, XmNmodifyVerifyCallback, &cbs);

    if (!cbs.doit) {
      VerifyBell(w);
    }
  }

  if (cbs.doit) {
    XmTextStatus status;

    start=cbs.startPos;
    end=cbs.endPos;
    status = (*Text_Source(w)->Replace)(w, ev, &start, &end, &blockrec, True);
    if (status==EditDone) {
      _XmTextUpdateLineTable((Widget) w, cbs.startPos, 
			     cbs.endPos, &blockrec, True);
      _XmTextSetCursorPosition(aw, cbs.newInsert);
    }
  }

    /* FIXME:  Are we required to free this if it exists?  It can only exist
     * if the user made it, but does Motif think that the user malloced it?
     */
/*   if (blockrec.ptr) XtFree(blockrec.ptr); */
}

static void
DoInsert(XmTextWidget w, XEvent * ev, char *buf, int len)
{
  Widget aw = (Widget) w;
  XmTextVerifyCallbackStruct cbs;
  XmTextBlockRec blockrec;

  if (len <= 0)
    return;
  (*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), off);
  blockrec.ptr = XtMalloc(len+1);
  blockrec.length = len;
  blockrec.format = XmFMT_8_BIT;
  strncpy(blockrec.ptr, buf, len);
  blockrec.ptr[len]='\0';

  cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
  cbs.event = ev;
  cbs.currInsert = cbs.startPos = cbs.endPos = Text_CursorPos(w);
  cbs.newInsert = Text_CursorPos(w)+=blockrec.length;
  cbs.text = &blockrec;
  cbs.doit = True;
  
  if (Text_ModifyVerifyCallback(w)) {
    XtCallCallbacks((Widget) w, XmNmodifyVerifyCallback, &cbs);

    if (!cbs.doit) {
      VerifyBell(w);
    }
  }

  if (cbs.doit) {
    XmTextStatus status;
    XmTextPosition start,end;

    start=cbs.startPos;
    end=cbs.endPos;
    status=(*Text_Source(w)->Replace)(w, ev, &start, 
				       &end, &blockrec, True);

    if (status==EditDone) {
    _XmTextSetCursorPosition(aw, cbs.newInsert);
    }
  }
  XtFree(blockrec.ptr);
}

static void
DoScanType(XmTextWidget w, XEvent *ev, XmTextPosition pos)
{
  InputData i=Text_InputData(w);

  switch (ScanType(w)) {
    case XmSELECT_POSITION:
      _XmTextSetCursorPosition((Widget) w, pos);
      
      In_HighlightPivot(i) = Text_CursorPos(w);
      break;
    default:
#ifdef NEW_STUFF
      In_HighlightPivot(i) = In_HighlightStart(i) = ScanTypeStart(w,pos);
      In_HighlightEnd(i) = ScanTypeEnd(w,pos);

        /* FIXME - what event type should this be? */
      PrimarySelectionComplete(w, ev->xbutton.time);
      Draw(w);
#endif
      break;
  }
}



/* Action Routines -------------------------------------------------------- */

static void
Activate(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextWidget tw = (XmTextWidget) w;
  XmAnyCallbackStruct cb;

  if (Text_EditMode(tw) != XmSINGLE_LINE_EDIT)
    return;

  cb.reason = XmCR_ACTIVATE;
  cb.event = ev;

  XtCallCallbackList(w, tw->text.activate_callback, (XtPointer) & cb);
  if (XmIsManager(XtParent(w))) {
    /* FIXME: Send event to manager. How do we do this ? XSendEvent ? */
  }
}

static void
MoveBackwardChar(XmTextWidget w, XEvent * ev, String * params, Cardinal * num_params)
{
printf("MOVE BACK\n");
  DBGW("MoveBackwardChar");

  if (Text_CursorPos(w) > 0) {
    (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, Text_CursorPos(w) - 1);
  }
}

static void
MoveBackwardParagraph(XmTextWidget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("MoveBackwardParagraph");

  if (Text_CursorPos(w) > 0) {
    XmTextPosition pos;
    
    pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
      XmSELECT_WHITESPACE,XmsdLeft,-1,False);
    pos=(*Text_Source(w)->Scan)(Text_Source(w),pos,
      XmSELECT_PARAGRAPH,XmsdLeft,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
  }
}

static void
MoveBackwardWord(XmTextWidget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("MoweBackwardWord");

  if (Text_CursorPos(w) > 0) {
    XmTextPosition pos;

    pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
      XmSELECT_WHITESPACE,XmsdLeft,-1,False);
    pos=(*Text_Source(w)->Scan)(Text_Source(w),pos,
      XmSELECT_WORD,XmsdLeft,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
  }
}

static void 
RingBell(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
MoveBeginningOfFile(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DBGW("MoveBeginningOfFile");

  if (!Text_Editable(w))
    return;

  if (Text_CursorPos(w) > 0) {
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, 0);
  }
}

static void 
MoveToLineStart(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  XmTextPosition pos;

  DBGW("MoveToLineStart");

  pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
    XmSELECT_LINE,XmsdLeft,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, pos);
}

static void 
ClearSelection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
CopyClipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
CopyPrimary(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessCopy(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessCancel(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
CutClipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
CutPrimary(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
DeleteForwardChar(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DBGW("DeleteForwardChar");

  if (!Text_Editable(w)) {
    VerifyBell((XmTextWidget) w);
  }
  if (Text_CursorPos(w) < Text_LastPos(w))
    DoDelete((XmTextWidget) w, ev, Text_CursorPos(w), Text_CursorPos(w)+1);
}

static void 
DeleteBackwardChar(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DBGW("DeleteBackardChar");

  if (!Text_Editable(w)) {
    VerifyBell((XmTextWidget) w);
  }
  if (Text_CursorPos(w) > 0)
    DoDelete((XmTextWidget) w, ev, Text_CursorPos(w) - 1, Text_CursorPos(w));
}

static void 
DeleteForwardWord(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{

}

static void 
DeleteBackwardWord(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
DeleteCurrentSelection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
DeleteToEndOfLine(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
DeleteToStartOfLine(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
DeselectAll(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}


static void 
MoveToLineEnd(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  XmTextPosition pos;

  DBGW("MoveToLineEnd");

  pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
    XmSELECT_LINE,XmsdRight,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, pos);
}

static void 
MoveEndOfFile(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  XmTextPosition pos;

  DBGW("MoveToEndOfFile");

  pos=Text_LastPos(w);
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, pos);
}


static void 
DoSelection(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  if (ev->type != MotionNotify)
    return;
}


static void 
ExtendEnd(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ExtendSelection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
MoveForwardChar(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("MoveForwardChar");

  if (!Text_Editable(w))
    return;

  if (Text_CursorPos(w) < Text_LastPos(w))
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, Text_CursorPos(w) + 1);
}

static void 
MoveForwardParagraph(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  XmTextPosition pos;

  DBGW("MoveForwardParagraph");

  pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
    XmSELECT_PARAGRAPH,XmsdRight,-1,False);
  pos=(*Text_Source(w)->Scan)(Text_Source(w),pos,
    XmSELECT_WHITESPACE,XmsdRight,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, pos);
}


static void 
MoveForwardWord(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  XmTextPosition pos;

  DBGW("MoveForwardWord");

  pos=(*Text_Source(w)->Scan)(Text_Source(w),Text_CursorPos(w),
    XmSELECT_WORD,XmsdRight,-1,False);
  pos=(*Text_Source(w)->Scan)(Text_Source(w),pos,
    XmSELECT_WHITESPACE,XmsdRight,-1,False);
   (*Text_Output(w)->DrawInsertionPoint)((XmTextWidget) w,
					 Text_CursorPos(w), off); 
    _XmTextSetCursorPosition(w, pos);
}

static void 
DoGrabFocus(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  InputData i=Text_InputData(w);
  XmTextPosition pos;

  DBGW("DoGrabFocus");

  XmProcessTraversal((Widget) w, XmTRAVERSE_CURRENT);

  pos=(*Text_Output(w)->XYToPos)((XmTextWidget) w, ev->xbutton.x, ev->xbutton.y);

  if ((In_LastTime(i)+XtGetMultiClickTime(XtDisplay((Widget) w))) >
    ev->xbutton.time)
    In_SelArrayIndex(i)++;
  else
    In_SelArrayIndex(i)=0;

  DoScanType((XmTextWidget) w,ev,pos);

  In_LastTime(i)=ev->xbutton.time;
}


static void 
InsertString(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
KeySelection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
KillForwardChar(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DeleteForwardChar(w, ev, params, num_params);
}

static void 
KillForwardWord(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DeleteForwardWord(w, ev, params, num_params);
}

static void 
KillBackwardChar(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DeleteBackwardChar(w, ev, params, num_params);
}

static void 
KillBackwardWord(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DeleteBackwardWord(w, ev, params, num_params);
}

static void 
KillCurrentSelection(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
KillToEndOfLine(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  DeleteToEndOfLine(w, ev, params, num_params);
}

static void 
KillToStartOfLine(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
MoveDestination(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
ProcessMove(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
TraverseNextTabGroup(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
}

static void 
MoveNextLine(Widget aw, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextWidget w = (XmTextWidget) aw;
  unsigned int index, maxindex;
  XmTextPosition pos = Text_CursorPos(w);
  
  DBGW("MoveNextLine");

  if (!Text_Editable(w))
    return;

  index = _XmTextGetTableIndex(w, pos);
  if (index == (maxindex = Text_TotalLines(w) - 1))
    pos = Text_LastPos(w);
  else {
    pos += Text_LineTable(w)[index + 1].start_pos - 
      Text_LineTable(w)[index].start_pos;
    if (index <= maxindex - 2 
	&& pos >= Text_LineTable(w)[index + 2].start_pos)
      pos = Text_LineTable(w)[index + 2].start_pos - 1;
    else if (pos > Text_LastPos(w))
      pos = Text_LastPos(w);
  }
  if (pos != Text_CursorPos(w))
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
}


static void 
MoveNextPage(Widget aw, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextWidget w = (XmTextWidget) aw;
  unsigned int index, maxindex;
  XmTextPosition pos = Text_CursorPos(w);
  int delta = Text_LineCount(w) - 1;
  
  DBGW("MoveNextPage");

  if (!Text_Editable(w))
    return;

  index = _XmTextGetTableIndex(w, pos);
  maxindex = Text_TotalLines(w) - 1;
  if (index > maxindex - delta)
    pos = Text_LastPos(w);
  else {
    pos += Text_LineTable(w)[index + delta].start_pos - 
      Text_LineTable(w)[index].start_pos;
    if (index <= maxindex - delta - 1
	&& pos >= Text_LineTable(w)[index + delta + 1].start_pos)
      pos = Text_LineTable(w)[index + delta + 1].start_pos - 1;
    else if (pos > Text_LastPos(w))
      pos = Text_LastPos(w);
  }
  if (pos != Text_CursorPos(w)) {
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
  }
}

static void
InsertNewLine(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  if (!Text_Editable(w))
    return;			/* if we can't edit */
  else if (Text_EditMode(w) == XmSINGLE_LINE_EDIT) {
    Activate(w, ev, params, num_params);
    return;
  }

  DoInsert((XmTextWidget) w, ev, "\n", 1);
}

static void
InsertNewLineAndBackup(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  if ((!Text_Editable(w)) || (Text_EditMode(w) == XmSINGLE_LINE_EDIT))
    return;			/* if we can't edit, or we are in single line only mode. */

  DoInsert((XmTextWidget) w, ev, "\n", 1);
}

static void 
InsertNewLineAndIndent(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  if ((!Text_Editable(w)) || (Text_EditMode(w) == XmSINGLE_LINE_EDIT))
    return;			/* if we can't edit, or we are in single line only mode. */

  DoInsert((XmTextWidget) w, ev, "\n", 1);
}

static void 
MovePageLeft(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
MovePageRight(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
PasteClipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
TraversePrevTabGroup(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}


static void 
MovePreviousLine(Widget aw, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextWidget w = (XmTextWidget) aw;
  unsigned int index;
  XmTextPosition pos = Text_CursorPos(w);
  
  DBGW("MovePreviousLine");

  if (!Text_Editable(w))
    return;

  index =_XmTextGetTableIndex(w, pos);
  if (index == 0)
    pos = Text_FirstPos(w);
  else {
    pos += Text_LineTable(w)[index - 1].start_pos - 
      Text_LineTable(w)[index].start_pos;
    if (pos >= Text_LineTable(w)[index].start_pos)
      pos = Text_LineTable(w)[index].start_pos - 1;
  }
  if(pos != Text_CursorPos(w)) {
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
  }
}


static void 
MovePreviousPage(Widget aw, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextWidget w = (XmTextWidget) aw;
  unsigned int index;
  XmTextPosition pos = Text_CursorPos(w);
  int delta = Text_LineCount(w) - 1;
  
  DBGW("MovePreviousPage");

  if (!Text_Editable(w))
    return;

  index =_XmTextGetTableIndex(w, pos);
  if (index < delta)
    pos = Text_FirstPos(w);
  else {
    pos += Text_LineTable(w)[index - delta].start_pos - 
      Text_LineTable(w)[index].start_pos;
    if (pos >= Text_LineTable(w)[index - delta + 1].start_pos)
      pos = Text_LineTable(w)[index - delta + 1].start_pos - 1;
  }
  if(pos != Text_CursorPos(w)) {
   (*Text_Output(w)->DrawInsertionPoint)(w, Text_CursorPos(w), off); 
    _XmTextSetCursorPosition((Widget) w, pos);
  }
}

static void 
ProcessBDrag(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessDown(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  MoveNextLine(w, ev, params, num_params);
}

static void 
ProcessHome(Widget w, XEvent * ev, String * params,
  Cardinal * num_params)
{
  MoveToLineStart(w, ev, params, num_params);
}

static void 
ProcessReturn(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  InsertNewLine(w, ev, params, num_params);
}

static void 
ProcessShiftDown(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessShiftUp(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessTab(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void 
ProcessUp(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  MovePreviousLine(w, ev, params, num_params);
}

static void
RedrawDisplay(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  xmTextClassRec.core_class.expose(w, NULL, (Region) NULL);
}

static void
ScrollOneLineUp(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
SecondaryAdjust(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
SecondaryNotify(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}


static void
SelectAll(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
StartExtendSelection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("select_start");

  DoGrabFocus(w,ev,params,num_params);
}



static void
SelfInsert(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
#define INSERTCHARBUFSIZ 32
  char buf[INSERTCHARBUFSIZ];
  KeySym keysym;
  int len, status;

  DBGW("SelfInsert");

  if (!Text_Editable(w)) {
    VerifyBell((XmTextWidget) w);
  }

  len = XmImMbLookupString((Widget)w, (XKeyPressedEvent *)ev, buf, INSERTCHARBUFSIZ, &keysym, &status);

  if (XdbInDebug(__FILE__, w)) {
    XdbDebug(__FILE__, w, "XmImMbLookupString => %d\n", len);
    XdbDebug0(__FILE__, w, "\tStatus %s\n",
      (status == XLookupNone) ? "none" :
      (status == XLookupChars) ? "chars" :
      (status == XLookupBoth) ? "both" :
      (status == XLookupKeySym) ? "keysym" :
      (status == XBufferOverflow) ? "overflow" : "????");
    if (status == XLookupBoth || status == XLookupKeySym)
      XdbDebug0(__FILE__, w, "\tKeySym 0x%X\n", keysym);
    if (len > 0) {
      int i;

      XdbDebug0(__FILE__, w, "\tBuffer ");
      for (i = 0; i < len; i++)
	XdbDebug(__FILE__, w, " %X", 0xFF & buf[i]);
      XdbDebug0(__FILE__, w, "\n");
    }
  }

  if (len > 0) {		/* FIXME */
    if (status == XLookupBoth || status == XLookupChars)
      DoInsert((XmTextWidget) w, ev, buf, len);
  }
}

static void
SetAnchor(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
SetCursorPosition(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
SetSelectionHint(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
ScrollOneLineDown(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
ToggleAddMode(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
TextLeave(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
TraverseHome(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
TraverseDown(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
TraverseUp(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}



static void
TextFocusIn(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextVerifyCallbackStruct cbs;
  OutputData o=Text_OutputData(w);

  DBGW("TextFocusIn");

#ifdef  I18N
  if (Text_Editable(w))
    XmImSetFocusValues(w, NULL, 0);
#endif

  if (!Out_HasFocus(o)) {
    if (Text_FocusCallback(w)) {
      cbs.reason = XmCR_FOCUS;
      cbs.event = ev;
      cbs.currInsert = cbs.newInsert = Text_CursorPos(w);
      cbs.startPos = cbs.endPos = 0;
      cbs.text = NULL;

      XtCallCallbackList((Widget) w, Text_FocusCallback(w), &cbs);
    }
    
    XtCallActionProc((Widget) w, "PrimitiveFocusIn", ev, params, *num_params);

    Out_HasFocus(o)=True;
  }
}

static void
TextFocusOut(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextVerifyCallbackStruct cbs;
  OutputData o=Text_OutputData(w);

  DBGW("TextFocusOut");

#ifdef  I18N
  if (Text_Editable(w))
    XmImUnsetFocus(w);
#endif

  if (Out_HasFocus(o)) {
    if (Text_LosingFocusCallback(w)) {
      cbs.reason = XmCR_LOSING_FOCUS;
      cbs.event = ev;
      cbs.currInsert = cbs.newInsert = Text_CursorPos(w);
      cbs.startPos = cbs.endPos = 0;
      cbs.text = NULL;

      XtCallCallbackList((Widget) w, Text_LosingFocusCallback(w), &cbs);
    }

    XtCallActionProc((Widget) w, "PrimitiveFocusOut", ev, params, *num_params);

    Out_HasFocus(o)=False;
  }
}

static void
UnKill(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}

static void
VoidAction(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
}




/*
 * Quasi-Public functions -----------------------------------------------------
 */


Widget
_XmTextGetDropReciever(Widget w)
{
  /* FIXME */
  return w;
}

Boolean
_XmTextHasDestination(Widget w)
{
  /* FIXME */
  return False;
}

void
_XmTextInputGetSecResData(XmSecondaryResourceData *secResDataRtn)
{
}

XmTextPosition
_XmTextGetAnchor(XmTextWidget tw)
{
  InputData i=Text_InputData(tw);
  return In_HighlightPivot(i);
}

Boolean
_XmTextSetDestinationSelection(Widget w,
			       XmTextPosition position,
			       Boolean disown,
			       Time set_time)
{
  /* FIXME */
  return False;
}

Boolean
_XmTextSetSel2(XmTextWidget tw,
	       XmTextPosition left,
	       XmTextPosition right,
	       Time set_time)
{
  /* FIXME */
  return False;
}

Boolean
_XmTextGetSel2(XmTextWidget tw,
	       XmTextPosition *left,
	       XmTextPosition *right)
{
  /* FIXME */
  return False;
}
