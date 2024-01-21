/**
 *
 * $Id: TextF.c,v 1.14 1996/11/28 09:22:08 u27113 Exp $
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

static char rcsid[] = "$Id: TextF.c,v 1.14 1996/11/28 09:22:08 u27113 Exp $";

#undef	USE_SHARED_GC

#undef	USE_AVERAGE_WIDTH
#define	TextF_FontMaxWidth(w)	TextF_Font(w)->max_bounds.width

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ScreenP.h>
#include <Xm/TextFP.h>
#include <X11/Xatom.h>
#include <X11/Xfuncs.h>
#include <limits.h>		/* for INT_MAX */
#ifdef HAVE_STRING_H
#include <string.h>
#define ANSI_STRING
#else
#include <strings.h>
#endif
#include <stdlib.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

#define	DBG(a)	XdbDebug(__FILE__, NULL, "%s\n", a)
#define DBGW(a) XdbDebug(__FILE__, (Widget)w, "%s\n", a)
#define DBG1(a,b) XdbDebug(__FILE__, (Widget)w, "%s: %d\n", a, (int)b)
#define DBG2(a,b,c) XdbDebug(__FILE__, (Widget)w, "%s: %d %d\n", a, (int)b,(int)c)

static void _XmTextFieldSetEditable(Widget w, Boolean e);
static void Realize(Widget w, XtValueMask * value_mask, XSetWindowAttributes * attributes);

#define Offset(field) XtOffsetOf(XmTextFieldRec, text.field)
#define OffsetOf(rec,field) XtOffsetOf(XmTextFieldRec, rec.field)

/* Resources for the TextField class */
static XtResource resources[] =
{
  {
    XmNactivateCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(activate_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNlosingFocusCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(losing_focus_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNfocusCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(focus_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNmodifyVerifyCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(modify_verify_callback),
    XtRCallback, (XtPointer) NULL
  },
  {
    XmNmodifyVerifyCallbackWcs, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(wcs_modify_verify_callback),
    XtRCallback, (XtPointer) NULL
  },
  {
    XmNmotionVerifyCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(motion_verify_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNgainPrimaryCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(gain_primary_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNlosePrimaryCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(lose_primary_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNvalueChangedCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(value_changed_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNvalue, XmCValue, XmRString,
    sizeof(String), Offset(value),
    XtRImmediate, (XtPointer) XmUNSPECIFIED
	/* FIXME: Motif has XmRString, wacky value here */
  },
  {
    XmNvalueWcs, XmCValueWcs, XmRValueWcs,
    sizeof(wchar_t *), Offset(wc_value),
    XmRString, (XtPointer) NULL
  },
  {
    XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
    sizeof(Dimension), Offset(margin_height),
    XmRImmediate, (XtPointer) 5
  },
  {
    XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
    sizeof(Dimension), Offset(margin_width),
    XmRImmediate, (XtPointer) 5
  },
  {
    XmNcursorPosition, XmCCursorPosition, XmRTextPosition,
    sizeof(XmTextPosition), Offset(cursor_position),
    XmRImmediate, (XtPointer) 0
  },
  {
    XmNcolumns, XmCColumns, XmRShort,
    sizeof(short), Offset(columns),
    XmRImmediate, (XtPointer) 20
  },
  {
    XmNmaxLength, XmCMaxLength, XmRInt,
    sizeof(int), Offset(max_length),
    XmRImmediate, (XtPointer) INT_MAX
  },
  {
    XmNblinkRate, XmCBlinkRate, XmRInt,
    sizeof(int), Offset(blink_rate),
    XmRImmediate, (XtPointer) 500
  },
  {
    XmNfontList, XmCFontList, XmRFontList,
    sizeof(XmFontList), Offset(font_list),
    XmRImmediate, (XtPointer) XmUNSPECIFIED
  },
  {
    XmNselectionArray, XmCSelectionArray, XmRPointer,
    sizeof(XtPointer), Offset(selection_array),
    XmRImmediate, (XtPointer) NULL
	/* FIXME: Motif has XmRInt, wacky value here */
  },
  {
    XmNselectionArrayCount, XmCSelectionArrayCount, XmRInt,
    sizeof(int), Offset(selection_array_count),
    XmRImmediate, (XtPointer) 3
	/* FIXME: Motif has XmRInt, (XtPointer)whacko value here */
  },
  {
    XmNresizeWidth, XmCResizeWidth, XmRBoolean,
    sizeof(Boolean), Offset(resize_width),
    XmRImmediate, (XtPointer) False
  },
  {
    XmNpendingDelete, XmCPendingDelete, XmRBoolean,
    sizeof(Boolean), Offset(pending_delete),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNeditable, XmCEditable, XmRBoolean,
    sizeof(Boolean), Offset(editable),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNcursorPositionVisible, XmCCursorPositionVisible, XmRBoolean,
    sizeof(Boolean), Offset(cursor_position_visible),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNverifyBell, XmCVerifyBell, XmRBoolean,
    sizeof(Boolean), Offset(verify_bell),
    XmRImmediate, (XtPointer) ((unsigned char) XmUNSPECIFIED)
  },
  {
    XmNselectThreshold, XmCSelectThreshold, XmRInt,
    sizeof(int), Offset(threshold),
    XmRImmediate, (XtPointer) 5
  },
  {
    XmNnavigationType, XmCNavigationType, XmRNavigationType,
    sizeof(XmNavigationType), XtOffsetOf(XmTextFieldRec, primitive.navigation_type),
    XmRImmediate, (XtPointer)XmTAB_GROUP
  },
};

static XmSyntheticResource syn_resources[] =
{
  {
    XmNmarginWidth,
    sizeof(Dimension), Offset(margin_width),
    _XmFromHorizontalPixels, _XmToHorizontalPixels
  },
  {
    XmNmarginHeight,
    sizeof(Dimension), Offset(margin_height),
    _XmFromVerticalPixels, _XmToVerticalPixels
  },
  {
    XmNvalue,
    sizeof(String), Offset(value),
    NULL /* FIXME */ , NULL
  },
  {
    XmNvalueWcs,
    sizeof(wchar_t *), Offset(wc_value),
    NULL /* FIXME */ , NULL
  },
};

#undef Offset
#undef OffsetOf

static void ClassInitialize();
static void ClassPartInitialize(WidgetClass w_class);
static void Initialize(Widget, Widget, ArgList, Cardinal *);
static void Destroy(Widget w);
static void Redisplay(Widget w, XEvent *event, Region region);
static void Resize(Widget w);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal *);
static XtGeometryResult QueryGeometry(Widget w, XtWidgetGeometry * proposed, XtWidgetGeometry * answer);

static void TextSet(XmTextFieldWidget w, char *s);
static void Draw(XmTextFieldWidget w);
static void DrawAll(XmTextFieldWidget w);
static void DrawInsert(XmTextFieldWidget w);
static void MassiveChangeDraw(XmTextFieldWidget w);
static void DrawTextReposition(XmTextFieldWidget);
static void ClearHighlight(XmTextFieldWidget w);
static void DrawHighlight(XmTextFieldWidget w);
static void CursorDraw(XmTextFieldWidget w);
static void CursorErase(XmTextFieldWidget w);
static Boolean CursorPosition(XmTextFieldWidget w);
static Boolean CursorMassiveAdjust(XmTextFieldWidget w);
static void CursorSet(XmTextFieldWidget w, Boolean focus);
static void VerifyBell(XmTextFieldWidget w);
static void PrimarySelectionComplete(XmTextFieldWidget w, Time time);

void _XmTextFieldFocusIn(Widget, XEvent *, String *, Cardinal *);
void _XmTextFieldFocusOut(Widget, XEvent *, String *, Cardinal *);
static void activate(Widget, XEvent *, String *, Cardinal *);
static void backward_character(Widget, XEvent *, String *, Cardinal *);
static void backward_word(Widget, XEvent *, String *, Cardinal *);
static void beep(Widget, XEvent *, String *, Cardinal *);
static void beginning_of_line(Widget, XEvent *, String *, Cardinal *);
static void clear_selection(Widget, XEvent *, String *, Cardinal *);
static void copy_clipboard(Widget, XEvent *, String *, Cardinal *);
static void copy_primary(Widget, XEvent *, String *, Cardinal *);
static void copy_to(Widget, XEvent *, String *, Cardinal *);
static void cut_clipboard(Widget, XEvent *, String *, Cardinal *);
static void cut_primary(Widget, XEvent *, String *, Cardinal *);
static void delete_next_character(Widget, XEvent *, String *, Cardinal *);
static void delete_previous_character(Widget, XEvent *, String *, Cardinal *);
static void delete_next_word(Widget, XEvent *, String *, Cardinal *);
static void delete_previous_word(Widget, XEvent *, String *, Cardinal *);
static void delete_selection(Widget, XEvent *, String *, Cardinal *);
static void delete_to_end_of_line(Widget, XEvent *, String *, Cardinal *);
static void delete_to_start_of_line(Widget, XEvent *, String *, Cardinal *);
static void delete_all(Widget, XEvent *, String *, Cardinal *);
static void deselect_all(Widget, XEvent *, String *, Cardinal *);
static void do_quick_action(Widget, XEvent *, String *, Cardinal *);
static void end_of_line(Widget, XEvent *, String *, Cardinal *);
static void enter(Widget, XEvent *, String *, Cardinal *);
static void extend_adjust(Widget, XEvent *, String *, Cardinal *);
static void extend_end(Widget, XEvent *, String *, Cardinal *);
static void extend_start(Widget, XEvent *, String *, Cardinal *);
static void forward_character(Widget, XEvent *, String *, Cardinal *);
static void forward_word(Widget, XEvent *, String *, Cardinal *);
static void grab_focus(Widget, XEvent *, String *, Cardinal *);
static void Help(Widget, XEvent *, String *, Cardinal *);
static void insert_string(Widget, XEvent *, String *, Cardinal *);
static void key_select(Widget, XEvent *, String *, Cardinal *);
static void kill_next_character(Widget, XEvent *, String *, Cardinal *);
static void kill_next_word(Widget, XEvent *, String *, Cardinal *);
static void kill_previous_character(Widget, XEvent *, String *, Cardinal *);
static void kill_previous_word(Widget, XEvent *, String *, Cardinal *);
static void kill_selection(Widget, XEvent *, String *, Cardinal *);
static void kill_to_end_of_line(Widget, XEvent *, String *, Cardinal *);
static void kill_to_start_of_line(Widget, XEvent *, String *, Cardinal *);
static void leave(Widget, XEvent *, String *, Cardinal *);
static void move_destination(Widget, XEvent *, String *, Cardinal *);
static void move_to(Widget, XEvent *, String *, Cardinal *);
static void next_tab_group(Widget, XEvent *, String *, Cardinal *);
static void page_left(Widget, XEvent *, String *, Cardinal *);
static void page_right(Widget, XEvent *, String *, Cardinal *);
static void paste_clipboard(Widget, XEvent *, String *, Cardinal *);
static void paste_primary(Widget, XEvent *, String *, Cardinal *);
static void prev_tab_group(Widget, XEvent *, String *, Cardinal *);
static void process_bdrag(Widget, XEvent *, String *, Cardinal *);
static void process_cancel(Widget, XEvent *, String *, Cardinal *);
static void process_home(Widget, XEvent *, String *, Cardinal *);
static void process_return(Widget, XEvent *, String *, Cardinal *);
static void process_tab(Widget, XEvent *, String *, Cardinal *);
static void quick_copy_set(Widget, XEvent *, String *, Cardinal *);
static void quick_cut_set(Widget, XEvent *, String *, Cardinal *);
static void redraw_display(Widget, XEvent *, String *, Cardinal *);
static void secondary_adjust(Widget, XEvent *, String *, Cardinal *);
static void secondary_notify(Widget, XEvent *, String *, Cardinal *);
static void secondary_start(Widget, XEvent *, String *, Cardinal *);
static void select_adjust(Widget, XEvent *, String *, Cardinal *);
static void select_all(Widget, XEvent *, String *, Cardinal *);
static void select_end(Widget, XEvent *, String *, Cardinal *);
static void select_start(Widget, XEvent *, String *, Cardinal *);
static void self_insert(Widget, XEvent *, String *, Cardinal *);
static void set_anchor(Widget, XEvent *, String *, Cardinal *);
static void set_insertion_point(Widget, XEvent *, String *, Cardinal *);
static void set_selection_hint(Widget, XEvent *, String *, Cardinal *);
static void toggle_add_mode(Widget, XEvent *, String *, Cardinal *);
static void traverse_home(Widget, XEvent *, String *, Cardinal *);
static void traverse_next(Widget, XEvent *, String *, Cardinal *);
static void traverse_prev(Widget, XEvent *, String *, Cardinal *);
static void unkill(Widget, XEvent *, String *, Cardinal *);
static void unmap(Widget, XEvent *, String *, Cardinal *);

char _XmTextF_EventBindings1[] =
":m <Key>osfPrimaryPaste: cut-primary()\n\
:a <Key>osfPrimaryPaste: cut-primary()\n\
:<Key>osfPrimaryPaste:	copy-primary()\n\
:m <Key>osfCut:		cut-primary()\n\
:a <Key>osfCut:		cut-primary()\n\
:<Key>osfCut:		cut-clipboard()\n\
:<Key>osfPaste:		paste-clipboard()\n\
:m <Key>osfCopy:	copy-primary()\n\
:a <Key>osfCopy:	copy-primary()\n\
:<Key>osfCopy:		copy-clipboard()\n\
s <Key>osfBeginLine:	beginning-of-line(extend)\n\
:<Key>osfBeginLine:	beginning-of-line()\n\
s <Key>osfEndLine:	end-of-line(extend)\n\
:<Key>osfEndLine:	end-of-line()\n\
s <Key>osfPageLeft:	page-left(extend)\n\
:<Key>osfPageLeft:	page-left()\n\
s c<Key>osfPageUp:	page-left(extend)\n\
:c <Key>osfPageUp:	page-left()\n\
s <Key>osfPageRight:	page-right(extend)\n\
:<Key>osfPageRight:	page-right()\n\
s c <Key>osfPageDown:	page-right(extend)\n\
:c <Key>osfPageDown:	page-right()\n\
:<Key>osfClear:		clear-selection()\n\
:<Key>osfBackSpace:	delete-previous-character()\n\
s m <Key>osfDelete:	cut-primary()\n\
s a <Key>osfDelete:	cut-primary()\n\
s <Key>osfDelete:	cut-clipboard()\n\
:c <Key>osfDelete:	delete-to-end-of-line()\n\
:<Key>osfDelete:	delete-next-character()";

char _XmTextF_EventBindings2[] =
":c m <Key>osfInsert:	copy-primary()\n\
:c a <Key>osfInsert:	copy-primary()\n\
s <Key>osfInsert:	paste-clipboard()\n\
:c <Key>osfInsert:	copy-clipboard()\n\
:s <Key>osfSelect:	key-select()\n\
:<Key>osfSelect:	set-anchor()\n\
:<Key>osfActivate:	activate()\n\
:<Key>osfAddMode:	toggle-add-mode()\n\
:<Key>osfHelp:		Help()\n\
:<Key>osfCancel:	process-cancel()\n\
s c <Key>osfLeft:	backward-word(extend)\n\
:c <Key>osfLeft:	backward-word()\n\
s <Key>osfLeft:		key-select(left)\n\
:<Key>osfLeft:		backward-character()\n\
s c <Key>osfRight:	forward-word(extend)\n\
:c <Key>osfRight:	forward-word()\n\
s <Key>osfRight:	key-select(right)\n\
:<Key>osfRight:		forward-character()\n\
:<Key>osfUp:		traverse-prev()\n\
:<Key>osfDown:		traverse-next()\n\
c ~m ~a <Key>slash:	select-all()\n\
c ~m ~a <Key>backslash:	deselect-all()\n\
s ~m ~a <Key>Tab:	prev-tab-group()\n\
~m ~a <Key>Tab:		next-tab-group()\n\
~s ~m ~a <Key>Return:	activate()\n\
c ~s ~m ~a <Key>space:	set-anchor()\n\
c s ~m ~a <Key>space:	key-select()\n\
s ~c ~m ~a <Key>space:	self-insert()\n\
<Key>:			self-insert()";

char _XmTextF_EventBindings3[] =
"<Unmap>:		unmap()\n\
<Enter>:		enter()\n\
<Leave>:		leave()\n\
<FocusIn>:		focusIn()\n\
<FocusOut>:		focusOut()\n\
~c s ~m ~a <Btn1Down>:	extend-start()\n\
c ~s ~m ~a <Btn1Down>:	move-destination()\n\
~c ~s ~m ~a <Btn1Down>:	grab-focus()\n\
~c ~m ~a <Btn1Motion>:	extend-adjust()\n\
~c ~m ~a <Btn1Up>:	extend-end()\n\
<Btn2Down>:		process-bdrag()\n\
m ~a <Btn2Motion>:	secondary-adjust()\n\
~m a <Btn2Motion>:	secondary-adjust()\n\
~s <Btn2Up>:		copy-to()\n\
~c <Btn2Up>:		move-to()";

/* action table table */

static XtActionsRec actions[] =
{
  {"activate", activate},
  {"backward-character", backward_character},
  {"backward-word", backward_word},
  {"beep", beep},
  {"beginning-of-line", beginning_of_line},
  {"clear-selection", clear_selection},
  {"copy-clipboard", copy_clipboard},
  {"copy-primary", copy_primary},
  {"copy-to", copy_to},
  {"cut-clipboard", cut_clipboard},
  {"cut-primary", cut_primary},
  {"delete-next-character", delete_next_character},
  {"delete-previous-character", delete_previous_character},
  {"delete-next-word", delete_next_word},
  {"delete-previous-word", delete_previous_word},
  {"delete-selection", delete_selection},
  {"delete-to-end-of-line", delete_to_end_of_line},
  {"delete-to-start-of-line", delete_to_start_of_line},
  {"delete-all", delete_all},
  {"deselect-all", deselect_all},
  {"do-quick-action", do_quick_action},
  {"end-of-line", end_of_line},
  {"enter", enter},
  {"extend-adjust", extend_adjust},
  {"extend-end", extend_end},
  {"extend-start", extend_start},
  {"forward-character", forward_character},
  {"forward-word", forward_word},
  {"grab-focus", grab_focus},
  {"Help", Help},
  {"insert-string", insert_string},
  {"key-select", key_select},
  {"kill-next_character", kill_next_character},
  {"kill-next-word", kill_next_word},
  {"kill-previous-character", kill_previous_character},
  {"kill-previous-word", kill_previous_word},
  {"kill-selection", kill_selection},
  {"kill-to-end-of-line", kill_to_end_of_line},
  {"kill-to-start-of-line", kill_to_start_of_line},
  {"leave", leave},
  {"move-destination", move_destination},
  {"move-to", move_to},
  {"next-tab-group", next_tab_group},
  {"page-left", page_left},
  {"page-right", page_right},
  {"paste-clipboard", paste_clipboard},
  {"paste-primary", paste_primary},
  {"prev-tab-group", prev_tab_group},
  {"process-bdrag", process_bdrag},
  {"process-cancel", process_cancel},
  {"process-home", process_home},
  {"process-return", process_return},
  {"process-tab", process_tab},
  {"quick-copy-set", quick_copy_set},
  {"quick-cut-set", quick_cut_set},
  {"redraw-display", redraw_display},
  {"secondary-adjust", secondary_adjust},
  {"secondary-notify", secondary_notify},
  {"secondary-start", secondary_start},
  {"select-adjust", select_adjust},
  {"select-all", select_all},
  {"select-end", select_end},
  {"select-start", select_start},
  {"self-insert", self_insert},
  {"set-anchor", set_anchor},
  {"set-insertion-point", set_insertion_point},
  {"set-selection-hint", set_selection_hint},
  {"toggle-add-mode", toggle_add_mode},
  {"traverse-home", traverse_home},
  {"traverse-next", traverse_next},
  {"traverse-prev", traverse_prev},
  {"unkill", unkill},
  {"unmap",unmap},
  {"focusIn", _XmTextFieldFocusIn},
  {"focusOut", _XmTextFieldFocusOut}
};

static XmBaseClassExtRec _XmTextFCoreClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL, /* FIXME */
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ NULL, /* FIXME */
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ {0},  /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmTextFPrimClassExtRec =
{
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmTextFieldClassRec xmTextFieldClassRec =
{
    /* Core class part */
  {
	/* superclass            */ (WidgetClass) & xmPrimitiveClassRec,
	/* class_name            */ "XmTextField",
	/* widget_size           */ sizeof(XmTextFieldRec),
	/* class_initialize      */ ClassInitialize,
	/* class_part_initialize */ ClassPartInitialize,
	/* class_inited          */ FALSE,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ Realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ Destroy,
	/* resize                */ Resize,
	/* expose                */ Redisplay,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmTextF_EventBindings1,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer) & _XmTextFCoreClassExtRec
  },
    /* Primitive Class part */
  {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ NULL,
	/* arm_and_activate_proc */ XmInheritArmAndActivate,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer) & _XmTextFPrimClassExtRec,
  },
    /* TextField Class part */
  {
	/* extension */ NULL
  }
};

WidgetClass xmTextFieldWidgetClass = (WidgetClass) & xmTextFieldClassRec;


/*
 * Common initialization procedures -------------------------------------------
 */

static void
FontSize(XmTextFieldWidget w)
{
  XmFontListEntry entry = NULL;
  XFontStruct *fs;
  int i;

  for (i=0; TextF_FontList(w)[i].tag != NULL; i++)
  {
    if (!strcmp(XmFONTLIST_DEFAULT_TAG, TextF_FontList(w)[i].tag))
    {
      entry = &TextF_FontList(w)[i];
      break;
    }
  }
  if (!entry)
    TextF_FontList(w) = entry = _XmFontListCreateDefault(XtDisplay((Widget)w));

  fs = (XFontStruct *) entry->font;
  TextF_Font(w) = fs;
  TextF_FontHeight(w) = TextF_FontAscent(w) + TextF_FontDescent(w);
  TextF_FontAverageWidth(w) = (fs->max_bounds.width + fs->min_bounds.width) / 2;
  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "FontInit: ascent=%d descent=%d height=%d avewidth=%d\n",
      TextF_FontAscent(w), TextF_FontDescent(w),
      TextF_FontHeight(w), TextF_FontAverageWidth(w));
  }
}

static void
FontInitialize(XmTextFieldWidget w)
{
    /* If FontList is NULL, track up the widget heirarchy for a VendorShell
     * or a BulletinBoard & use that FontList
     */
  if (!TextF_FontList(w)) {
    Widget parent=XtParent(w);
    
    while (parent) {
      if (XmIsBulletinBoard(parent)||XmIsVendorShell(parent)) {
        XtVaGetValues(parent,
          XmNtextFontList, &TextF_FontList(w),
          NULL);
        TextF_FontListCreated(w) = False;
        break;
      }
      parent=XtParent(parent);
    }
  }
  
    /* If the FontList is still NULL or we have an unspecified font, use
     * the default.
     */
  if (!TextF_FontList(w) || (TextF_FontList(w) == (XmFontList) XmUNSPECIFIED)) {
    TextF_FontList(w) = _XmGetDefaultFontList((Widget) w, XmTEXT_FONTLIST);
    TextF_FontListCreated(w) = False; /* we didn't create it, so don't free */
  }
  else
    TextF_FontListCreated(w) = True; /* we created this, so free it when the time comes */

  FontSize(w);
}

static void
GCInitialize(XmTextFieldWidget w)
{
  XGCValues values;
  XtGCMask mask, dynamic, dontcare;

/*
 * DrawGC
 *      Need to change :        ClipRectangles
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.font = TextF_Font(w)->fid;
  values.foreground = Prim_Foreground(w);
  values.background = XtBackground(w);
  values.clip_x_origin = 0;
  values.clip_y_origin = 0;
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCFont | GCForeground | GCBackground;
  dynamic = GCClipMask | GCClipXOrigin | GCClipYOrigin | GCForeground | GCBackground;
  dontcare = 0;
#ifdef	USE_SHARED_GC
  TextF_DrawGC(w) = XtAllocateGC((Widget) w, 0, mask, &values, dynamic, dontcare);
#else
  TextF_DrawGC(w) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);
#endif
  TextF_DrawGCInverted(w)=False;
  
/*
 * Cursor GC
 *      Need to change :        Tile/Stipple Origin, Stipple
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillStippled;
  values.foreground = Prim_Foreground(w);
  values.background = XtBackground(w);
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
  dynamic = GCTileStipXOrigin | GCTileStipYOrigin | GCStipple;
  dontcare = 0;
#ifdef	USE_SHARED_GC
  TextF_CursorGC(w) = XtAllocateGC((Widget) w, 0, mask, &values, dynamic, dontcare);
#else
  TextF_CursorGC(w) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);
#endif

/*
 * CopyGC
 *      At least this one is not changed anywhere :-)
 */
  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.foreground = XtBackground(w);
  values.background = Prim_Foreground(w);
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
  TextF_CopyGC(w) = XtGetGC((Widget) w, mask, &values);
}

static void
GCPixmap(XmTextFieldWidget w)
{
  static char dots[] = {2, 1, 1};
  GC gc;
  int width, width_erase, height;
  Display *display;
  XGCValues values;
  XtGCMask mask;

  values.line_style = LineSolid;
  values.line_width = 0;
  values.fill_style = FillSolid;
  values.foreground = 0;
  values.background = 0;

  display = XtDisplay((Widget) w);
  width = 5;
  height = TextF_FontHeight(w);
  width_erase = 2 * TextF_Font(w)->max_bounds.width;

  if (TextF_CursorIBeam(w))
    XFreePixmap(XtDisplay((Widget)w), TextF_CursorIBeam(w));
  TextF_CursorIBeam(w) = (Pixmap) NULL;
  if (TextF_CursorStipple(w))
    XFreePixmap(XtDisplay((Widget)w), TextF_CursorStipple(w));
  TextF_CursorStipple(w) = (Pixmap) NULL;
  if (TextF_CursorSave(w))
    XFreePixmap(XtDisplay((Widget)w), TextF_CursorSave(w));
  TextF_CursorSave(w) = (Pixmap) NULL;
  TextF_CursorSaveValid(w) = False;

  if (height > 0) {
    TextF_CursorIBeam(w) = XCreatePixmap(display,
      RootWindowOfScreen(XtScreen((Widget) w)),
      width, height, 1);
    TextF_CursorStipple(w) = XCreatePixmap(display,
      RootWindowOfScreen(XtScreen((Widget) w)),
      width, height, 1);
    TextF_CursorSave(w) = XCreatePixmap(display,
      RootWindowOfScreen(XtScreen((Widget) w)),
      width_erase, height, w->core.depth);
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillSolid;
    values.foreground = 0;
    values.background = 0;
    mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
    gc = XCreateGC(display, TextF_CursorIBeam(w), mask, &values);
    XFillRectangle(display, TextF_CursorIBeam(w), gc, 0, 0, width, height);
    XFillRectangle(display, TextF_CursorStipple(w), gc, 0, 0, width, height);
    XSetForeground(display, gc, 1);
    XDrawLine(display, TextF_CursorIBeam(w), gc, 2, 0, 2, height - 2);
    XDrawLine(display, TextF_CursorIBeam(w), gc, 0, 0, 4, 0);
    XDrawLine(display, TextF_CursorIBeam(w), gc, 0, height - 2, 4, height - 2);

    XSetLineAttributes(display, gc, 0, LineOnOffDash, CapRound, JoinRound);
    XSetDashes(display, gc, 0, &dots[1], (int) dots[0]);
    XDrawLine(display, TextF_CursorStipple(w), gc, 2, 0, 2, height - 2);
    XDrawLine(display, TextF_CursorStipple(w), gc, 1, 0, 3, 0);
    XDrawLine(display, TextF_CursorStipple(w), gc, 1, height - 2, 3, height - 2);

    XFreeGC(display, gc);
  }
}

static void
GCClip(XmTextFieldWidget w)
{
  XRectangle clip;

  clip.x = 0;
  clip.y = 0;
  clip.width = TextF_ViewWidth(w);
  clip.height = TextF_ViewHeight(w);

/* These calls change the GCs */
  XSetClipRectangles(XtDisplay((Widget) w), TextF_DrawGC(w),
    TextF_XDraw(w), TextF_YDraw(w), &clip, 1, Unsorted);

  GCPixmap(w);

  CursorSet(w, TextF_HasFocus(w));
}

static void
SizeRecalc(XmTextFieldWidget w)
{
  TextF_ViewWidth(w) = XtWidth(w) -
    2 * Prim_ShadowThickness(w) - 2 * Prim_HighlightThickness(w) -
    2 * TextF_MarginWidth(w);
  TextF_XDraw(w) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
    TextF_MarginWidth(w);

  TextF_YDraw(w) = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
    TextF_MarginHeight(w);
  TextF_YOffset(w) = TextF_YDraw(w) + TextF_FontAscent(w);
  TextF_ViewHeight(w) = TextF_FontHeight(w);
  if (TextF_YDraw(w) + TextF_ViewHeight(w) >
    XtHeight(w) - Prim_HighlightThickness(w) - Prim_ShadowThickness(w)) {
    TextF_ViewHeight(w) = XtHeight(w) - TextF_YDraw(w) -
      Prim_HighlightThickness(w) - Prim_ShadowThickness(w);
  }
  
#ifdef	USE_AVERAGE_WIDTH
  TextF_Columns(w) = (XtWidth(w)
    - 2 * Prim_ShadowThickness(w)
    - 2 * TextF_MarginWidth(w)) / TextF_FontAverageWidth(w);
#else
  TextF_Columns(w) = (XtWidth(w)
    - 2 * Prim_ShadowThickness(w)
    - 2 * TextF_MarginWidth(w)) / TextF_FontMaxWidth(w);
#endif
  
  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w, "SizeRecalc : width: core=%d View=%d shadow=%d margin=%d xdraw=%d\n",
      XtWidth(w), TextF_ViewWidth(w), Prim_ShadowThickness(w),
      TextF_MarginWidth(w), TextF_XDraw(w));
    XdbDebug(__FILE__, (Widget)w, "SizeRecalc : height: core=%d View=%d shadow=%d margin=%d ydraw=%d\n",
      XtHeight(w), TextF_ViewHeight(w), Prim_ShadowThickness(w),
      TextF_MarginHeight(w), TextF_YDraw(w));
    XdbDebug(__FILE__, (Widget)w, "SizeRecalc : YOffset=%d\n", TextF_YOffset(w));
  }
}



/*
 * Methods --------------------------------------------------------------------
 */

static void
ClassInitialize()
{
  /* register type converter for XmStrings */
  DBG("ClassInitialize");

  _XmTextFCoreClassExtRec.record_type = XmQmotif;

  XtSetTypeConverter(XtRString,	/* source type */
    XmRXmString,		/* target type */
    _XmCvtStringToXmString,	/* converter routine */
    NULL,			/* args for converter routine */
    0,				/* number of args to converter routine */
    XtCacheAll,			/* caching instructions */
    NULL);			/* destructor function */
}

static void
ClassPartInitialize(WidgetClass widget_class)
{
  DBG("ClassPartInitialize");

  _XmFastSubclassInit(widget_class, XmTEXT_FIELD_BIT);
}

static void
Initialize(Widget request, Widget tnew, ArgList args, Cardinal * num)
{
  XmTextFieldWidget w = (XmTextFieldWidget) tnew;

  DBGW("Initialize");

    /* deal with the translations -- someone tell me if this is wrong... */
#if 1
  XtAugmentTranslations((Widget) w, XtParseTranslationTable(_XmTextF_EventBindings2));
  XtAugmentTranslations((Widget) w, XtParseTranslationTable(_XmTextF_EventBindings3));
#endif

  w->text.extension = (XtPointer) XtMalloc(sizeof(XmTextFieldPartLesstifExtension));

    /* set up the initial text string */
  TextF_SelectionText(w) = NULL;
  TextF_TextWidth(w) = TextF_OldTextWidth(w) = 0;
  if (TextF_Value(w) != (char *) XmUNSPECIFIED) {
    char *temp;

    TextF_Length(w) = strlen(TextF_Value(w));
    TextF_Alloc(w) = TextF_Length(w) + TF_ALLOC_SIZE;
    temp = (char *) XtMalloc(TextF_Alloc(w));
    strcpy(temp, TextF_Value(w));
    TextF_Value(w) = temp;
  }
  else {
    TextF_Alloc(w) = TF_ALLOC_SIZE;
    TextF_Value(w) = (char *) XtMalloc(TextF_Alloc(w));
    TextF_Length(w) = 0;
  }

    /* Get the font information */
  FontInitialize(w);

  if (XtWidth(request) == (Dimension) 0) { /* the user didn't say how wide */
#ifdef	USE_AVERAGE_WIDTH
    XtWidth(w) = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
      2 * TextF_MarginWidth(w) +
      TextF_Columns(w) * TextF_FontAverageWidth(w));
#else
    XtWidth(w) = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
      2 * TextF_MarginWidth(w) +
      TextF_Columns(w) * TextF_FontMaxWidth(w));
#endif
  } else { /* the user specified a width, so fit the columns to it. */
#ifdef	USE_AVERAGE_WIDTH
    TextF_Columns(w) = (XtWidth(w)
      - 2 * Prim_ShadowThickness(w)
      - 2 * TextF_MarginWidth(w)) / TextF_FontAverageWidth(w);
#else
    TextF_Columns(w) = (XtWidth(w)
      - 2 * Prim_ShadowThickness(w)
      - 2 * TextF_MarginWidth(w)) / TextF_FontMaxWidth(w);
#endif
  }

  if (XtHeight(request) == (Dimension) 0)
    XtHeight(w) = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
      2 * TextF_MarginHeight(w) +
      TextF_FontHeight(w));
  SizeRecalc(w);

    /* initialize cursor position */
  if (TextF_CursorPos(w) > 0) {
    if (TextF_CursorPos(w) > TextF_Length(w)) {
      TextF_CursorPos(w) = TextF_Length(w);
    }
  }
  else {
    TextF_CursorPos(w) = 0;
  }

  TextF_OldCursorX(w) = -1;
  TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
  TextF_OldHighlightStart(w) = TextF_OldHighlightEnd(w) = -1;

  TextF_XOffset(w) = TextF_OldXOffset(w) = 0;

    /* initialize booleans */
  TextF_TimerId(w) = (XtIntervalId) 0;
  TextF_SelectId(w) = (XtIntervalId) 0;
  w->text.last_time=(Time)0;
  w->text.sarray_index=0;
  TextF_Echo(w) = True;
  TextF_AllowSelection(w) = True;
  TextF_HasFocus(w) = False;

    /* initialize pixmaps and graphics contexts */
  TextF_CursorIBeam(w) = (Pixmap) NULL;
  TextF_CursorStipple(w) = (Pixmap) NULL;
  TextF_CursorSave(w) = (Pixmap) NULL;

    /* Use DrawGC as a flag to see if the GCs have been created */
  TextF_DrawGC(w) = NULL;
}

static void
Realize(Widget aw, XtValueMask * value_mask, XSetWindowAttributes * attributes)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  XdbDebug(__FILE__, (Widget)w, "Realize\n");

#define	superclass	(&xmPrimitiveClassRec)
  (*superclass->core_class.realize) (aw, value_mask, attributes);
#undef	superclass

  GCInitialize(w);
  GCClip(w);

  _XmTextFieldSetEditable(aw, TextF_Editable(w));
}

static void
Destroy(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  DBGW("Destroy");

  if (TextF_SelectId(aw))
	XtRemoveTimeOut(TextF_SelectId(aw));
  if (TextF_TimerId(aw))
	XtRemoveTimeOut(TextF_TimerId(aw));

  XtReleaseGC((Widget) w, TextF_DrawGC(w));
  XtReleaseGC((Widget) w, TextF_CursorGC(w));
  XtReleaseGC((Widget) w, TextF_CopyGC(w));
  if (TextF_FontListCreated(w))
    XmFontListFree(TextF_FontList(w));

  if (TextF_SelectionText(w))
    XtFree(TextF_SelectionText(w));
  XtFree(TextF_Value(w));
}

static void
Redisplay(Widget aw, XEvent * event, Region region)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  DBGW("Redisplay");
  if (!XtIsRealized(aw))
    return;

/* Then draw ourselves on top of it */
  DrawAll(w);

  _XmDrawShadows(XtDisplay(w),
    XtWindow(w),
    Prim_TopShadowGC(w),
    Prim_BottomShadowGC(w),
    Prim_HighlightThickness(w), Prim_HighlightThickness(w),
    XtWidth(w) - 2 * Prim_HighlightThickness(w),
    XtHeight(w) - 2 * Prim_HighlightThickness(w),
    Prim_ShadowThickness(w),
    XmSHADOW_IN);
}

static void
Resize(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  XdbDebug(__FILE__, aw, "Resize => wid %d ht %d\n", XtWidth(aw), XtHeight(aw));

  SizeRecalc(w);

  /* Only adjust the GCs if they have already been created! */
  if (TextF_DrawGC(w))
    GCClip(w);

  if (XtIsRealized(aw))
    MassiveChangeDraw(w);
}

static Boolean
SetValues(Widget current, Widget request, Widget reply,
  ArgList args, Cardinal * nargs)
{
  XmTextFieldWidget w = (XmTextFieldWidget) current;
  XmTextFieldWidget new_w = (XmTextFieldWidget) reply;
  Boolean redraw = False;

  DBGW("SetValues");
  if (Prim_Foreground(w) != Prim_Foreground(new_w) ||
    XtBackground(w) != XtBackground(new_w)) {
    XtReleaseGC((Widget) w, TextF_DrawGC(w));
    XtReleaseGC((Widget) w, TextF_CursorGC(w));
    XtReleaseGC((Widget) w, TextF_CopyGC(w));
    GCInitialize(new_w);
    redraw = True;
  }

  if ((TextF_CursorPos(w) != TextF_CursorPos(new_w)) ||
    (TextF_CursorPositionVisible(w) != TextF_CursorPositionVisible(new_w))) {
    redraw = True;
  }
  if (TextF_Value(w) != TextF_Value(new_w)) {
    char *temp;

    redraw = True;
    temp = TextF_Value(new_w); /* save ptr to the new string */
    TextF_Value(new_w) = TextF_Value(w); /* need to get old address for malloc */
    TextSet(new_w, temp); /* XtRealloc is called here on TextF_Value(new_w) */
    TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
    TextF_CursorPos(new_w) = 0;
#ifdef DEBUG
    XdbDebug(__FILE__, reply, "SetValues: %s\n", TextF_Value(new_w));
#endif
  }

  if (TextF_Editable(new_w) != TextF_Editable(new_w)) {
    _XmTextFieldSetEditable((Widget) new_w, TextF_Editable(new_w));
    redraw = True;
  }

  return redraw;
}

static XtGeometryResult
QueryGeometry(Widget aw, XtWidgetGeometry * proposed, XtWidgetGeometry * answer)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  XtWidgetGeometry a;

#define Wants(x)   (proposed->request_mode & x)

  if (proposed->request_mode != 0) {	/* NULL case should not yet end here ! */
    if ((!(Wants(CWWidth))) && (!Wants(CWHeight))) {
      /* If they don't ask width/height, let them have whatever they like */
      if (answer)
	*answer = *proposed;
      XdbDebug(__FILE__, aw, "QueryGeometry: request %s => YES\n",
	XdbWidgetGeometry2String(proposed));
      return XtGeometryYes;
    }
  }

#ifdef	USE_AVERAGE_WIDTH
/* The code below (in #else) uses MAXIMUM values for the font size, where
 * everything else in this source uses the font's average character width.
 * Make it conform ...
 *
 * Danny 31/10/96
 */
  a.request_mode = CWWidth | CWHeight;
  a.width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * TextF_MarginWidth(w) +
    TextF_Columns(w) * TextF_FontAverageWidth(w));

  a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * TextF_MarginHeight(w) +
    TextF_Font(w)->max_bounds.ascent + TextF_Font(w)->max_bounds.descent);
#else
  a.request_mode = CWWidth | CWHeight;
  a.width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * TextF_MarginWidth(w) +
    TextF_Columns(w) * TextF_FontMaxWidth(w));

  a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
    2 * TextF_MarginHeight(w) +
    TextF_Font(w)->max_bounds.ascent + TextF_Font(w)->max_bounds.descent);
#endif

  if (answer)
    *answer = a;

  if (proposed->request_mode == 0) {	/* NULL proposed -> return Width+Height */
      XdbDebug(__FILE__, aw, "QueryGeometry: request %s => %s ALMOST\n",
	XdbWidgetGeometry2String(proposed), XdbWidgetGeometry2String(&a));
    return XtGeometryAlmost;
  }

  if (proposed->width >= answer->width && proposed->height >= answer->height) {
      XdbDebug(__FILE__, aw, "QueryGeometry: request %s => YES\n",
	XdbWidgetGeometry2String(proposed));
    return XtGeometryYes;
  } else if (answer->width == XtWidth(w) && answer->height == XtHeight(w)) {
    if (answer)
      answer->request_mode = 0;
      XdbDebug(__FILE__, aw, "QueryGeometry: request %s => %s NO\n",
	XdbWidgetGeometry2String(proposed));
    return XtGeometryNo;
  } else {
      XdbDebug(__FILE__, aw, "QueryGeometry: request %s => %s ALMOST\n",
	XdbWidgetGeometry2String(proposed), XdbWidgetGeometry2String(&a));
    return XtGeometryAlmost;
  }
}



/*
 * String manipulation procedures ---------------------------------------------
 */

static void
TextSet(XmTextFieldWidget w, char *s)
{
  int len;

  if (s) {
    len = strlen(s);
    if (len > TextF_Alloc(w)) {
      TextF_Alloc(w) += len;
      TextF_Value(w) = XtRealloc(TextF_Value(w), TextF_Alloc(w));
    }
    strcpy(TextF_Value(w), s);
    TextF_Length(w) = len;
    TextF_TextWidth(w) = TextF_OldTextWidth(w) =
      TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w));
    if ((TextF_MaxLength(w) > 0) && (TextF_Length(w) > TextF_MaxLength(w)))
      TextF_MaxLength(w) = TextF_Length(w);
  }
}

static void
TextDelete(XmTextFieldWidget w, int start, int len)
{
  int i;

  if (len > 0) {
    for (i = start + len; i < TextF_Length(w); i++)
      TextF_Value(w)[i - len] = TextF_Value(w)[i];
    TextF_Length(w) -= len;
    TextF_TextWidth(w) = TextF_FontTextWidth(w, TextF_Value(w),
      TextF_Length(w));
    TextF_Value(w)[TextF_Length(w)] = 0;
  }
}

static void
TextDeleteHighlighted(XmTextFieldWidget w)
{
  if (TextF_HighlightStart(w) >= 0) {
    TextDelete(w, TextF_HighlightStart(w),
      TextF_HighlightEnd(w) - TextF_HighlightStart(w));
    TextF_CursorPos(w) = TextF_HighlightStart(w);
    TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
  }
}

/* returns value indicating if the text can be redrawn using the fast
 * method */
static Boolean
TextInsert(XmTextFieldWidget w, char *buf, int len)
{
  int i;
  Boolean fast_insert;

  fast_insert = True;
  if (len > 0) {
    if (TextF_Length(w) + len > TextF_MaxLength(w)) {
      VerifyBell(w);
      fast_insert = False;
    }
    else {
      if (TextF_HighlightStart(w) >= 0) {
	fast_insert = False;
	if (TextF_PendingDelete(w))
	  TextDeleteHighlighted(w);
	else
	  ClearHighlight(w);
      }

      if (TextF_Length(w) + len > TextF_Alloc(w)) {
	i = TF_ALLOC_SIZE;
	if (i < len)
	  i = len;
	TextF_Alloc(w) += i + 1;
	TextF_Value(w) = XtRealloc(TextF_Value(w), TextF_Alloc(w));

        DBG1("TextInsert: Alloced new space\n", TextF_Alloc(w));
      }
      for (i = TextF_Length(w) - 1; i >= TextF_CursorPos(w); i--)
	TextF_Value(w)[i + len] = TextF_Value(w)[i];
      strncpy(&TextF_Value(w)[TextF_CursorPos(w)], buf, len);

      TextF_FastInsertCursorStart(w) = TextF_CursorPos(w);
      TextF_FastInsertTextLen(w) = len;
      TextF_Length(w) += len;

      TextF_TextWidth(w) = TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w));
      TextF_Value(w)[TextF_Length(w)] = 0;
    }
  }
  return fast_insert;
}

static XmTextPosition
TextPixelToSelectionPos(XmTextFieldWidget w, int x)
{
  XmTextPosition i, tot, cur, pos;

  pos = 0;

  x -= (int) TextF_XDraw(w) + TextF_XOffset(w);

/* check if the cursor is before the 1st character */
  if (x <= 0) {
    pos = 0;
  }

/* OK, how 'bout after the last character */
  else if (x > TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w))) {
    pos = TextF_Length(w);
  }

/* must be in between somewhere... */
  else {
    tot = 0;
    pos = -1;
    for (i = 0; i < TextF_Length(w); i++) {
      cur = TextF_FontTextWidth(w, &TextF_Value(w)[i], 1);
      if (x < tot + (cur / 2)) {
	pos = i;
	break;
      }
      tot += cur;
    }
    if (pos < 0)
      pos = TextF_Length(w);
  }
  return pos;
}

static XmTextPosition
TextPixelToPos(XmTextFieldWidget w, int x)
{
  XmTextPosition i, tot, cur, pos;

  pos = 0;

  x -= (int) TextF_XDraw(w) + TextF_XOffset(w);

/* check if the cursor is before the 1st character */
  if (x <= 0) {
    pos = 0;
  }

/* OK, how 'bout after the last character */
  else if (x > TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w))) {
    pos = TextF_Length(w);
  }

/* must be in between somewhere... */
  else {
    tot = 0;
    pos = -1;
    for (i = 0; i < TextF_Length(w); i++) {
      cur = TextF_FontTextWidth(w, &TextF_Value(w)[i], 1);
      if (x < tot + cur) {
	pos = i;
	break;
      }
      tot += cur;
    }
    if (pos < 0)
      pos = TextF_Length(w);
  }
  return pos;
}


/*
 * Private drawing functions --------------------------------------------------
 */

static Boolean
MakePositionVisible(XmTextFieldWidget w, XmTextPosition pos)
{
  int x, start, end;
  Boolean moved;

  moved = False;
  x = TextF_FontTextWidth(w, TextF_Value(w), pos);
  start = -TextF_XOffset(w);
  end = start + TextF_ViewWidth(w);

  if (x < start) {
    TextF_XOffset(w) = -x;
    moved = True;
  }
  else if (x > end) {
    TextF_XOffset(w) = TextF_ViewWidth(w) - x;
    moved = True;
  }

  if (XdbInDebug(__FILE__, (Widget)w)) {
    XdbDebug(__FILE__, (Widget)w,
      "MakePositionVisible: start=%d end=%d x=%d moved=%d\n",
      start, end, x, (int) moved);
  }
  
  return moved;
}

/*
 * Actually draw a range of text onto the widget
 */
static void
DrawText(XmTextFieldWidget w, int start, int end, Boolean highlight)
{
  int x;

  if (!TextF_Echo(w))
    return;

  if (TextF_Length(w) > 0) {
    if (start < 0)
      return;
    else if (end < start) {
      int temp;

      temp = start;
      start = end;
      end = temp;
    }
    if (start > TextF_Length(w))
      return;
    else if (end > TextF_Length(w))
      end = TextF_Length(w);
    x = TextF_XDraw(w) + TextF_XOffset(w) +
      TextF_FontTextWidth(w, TextF_Value(w), start);
    if (highlight && !TextF_DrawGCInverted(w)) {
      XSetForeground(XtDisplay(w),TextF_DrawGC(w),XtBackground(w));
      XSetBackground(XtDisplay(w),TextF_DrawGC(w),Prim_Foreground(w));
      TextF_DrawGCInverted(w)=True;
    }
    else if (!highlight && TextF_DrawGCInverted(w)) {
      XSetForeground(XtDisplay(w),TextF_DrawGC(w),Prim_Foreground(w));
      XSetBackground(XtDisplay(w),TextF_DrawGC(w),XtBackground(w));
      TextF_DrawGCInverted(w)=False;
    }
    XDrawImageString(XtDisplay(w), XtWindow(w), TextF_DrawGC(w),
      x, TextF_YOffset(w),
      &TextF_Value(w)[start], end - start);
  }
}

static void
DrawTextRange(XmTextFieldWidget w, int start, int end)
{
  if (!TextF_Echo(w))
    return;

  if (TextF_Length(w) > 0) {
    if (start < 0)
      return;
    else if (end < start) {
      int temp;

      temp = start;
      start = end;
      end = temp;
    }

/* If there is no highlighting, or the refresh area doesn't cross the */
/* the highlight borders, just redraw it. */
    if (TextF_HighlightStart(w) < 0 ||
      start >= TextF_HighlightEnd(w) ||
      end <= TextF_HighlightStart(w)) {
      DrawText(w, start, end, False);
    }

/* OK, the refresh area crosses one or both highlight borders. */
    else {
      int clip;

      while (start < end) {
	if (start < TextF_HighlightStart(w)) {
	  if (end <= TextF_HighlightStart(w))
	    clip = end;
	  else
	    clip = TextF_HighlightStart(w);
	  DrawText(w, start, clip, False);
	  start = clip;
	}
	else if (start < TextF_HighlightEnd(w)) {
	  if (end <= TextF_HighlightEnd(w))
	    clip = end;
	  else
	    clip = TextF_HighlightEnd(w);
	  DrawText(w, start, clip, True);
	  start = clip;
	}
	else {
	  DrawText(w, start, end, False);
	  start = end;
	}
      }


    }
  }
}

static void
DrawTextReposition(XmTextFieldWidget w)
{
  int xsrc, xdest, width, start, end;

  if (!TextF_Echo(w))
    return;
  if (! XtIsRealized((Widget)w))
	return;

  if (TextF_XOffset(w) < TextF_OldXOffset(w)) {
    xsrc = TextF_OldXOffset(w) - TextF_XOffset(w);
    xdest = 0;
    width = TextF_ViewWidth(w) - xsrc + 1;

    /* Need to redraw some characters at the end. */

    end = TextPixelToPos(w, TextF_XDraw(w) + TextF_ViewWidth(w));
    start = TextPixelToPos(w, TextF_XDraw(w) + TextF_ViewWidth(w) - xsrc);
  }
  else if (TextF_XOffset(w) > TextF_OldXOffset(w)) {
    xsrc = 0;
    xdest = TextF_XOffset(w) - TextF_OldXOffset(w);
    width = TextF_ViewWidth(w) - xdest + 1;

    /* Need to redraw some characters at the beginning. */

    start = TextPixelToPos(w, TextF_XDraw(w));
    end = TextPixelToPos(w, TextF_XDraw(w) + xdest);
  }
  else
    return;

  if (width > 0) {
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "Reposition: xoff=%d old=%d src=%d dest=%d width=%d refresh %d-%d\n",
        TextF_XOffset(w), TextF_OldXOffset(w), xsrc, xdest, width, start, end);
    }
    
    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
      TextF_DrawGC(w),
      TextF_XDraw(w) + xsrc, TextF_YDraw(w),
      (unsigned int) width, (unsigned int) TextF_ViewHeight(w),
      TextF_XDraw(w) + xdest, TextF_YDraw(w));

    /*
     * Erase to the end of the window (if necessary).  This removes any
     * garbage that might be left over from the XCopyArea
     */
    if (xdest < xsrc) {
      XClearArea(XtDisplay(w), XtWindow(w),
	TextF_XDraw(w) + xdest + width, TextF_YDraw(w),
	xsrc - xdest, (unsigned int) TextF_ViewHeight(w),
	False);
      if (XdbInDebug(__FILE__, (Widget)w)) {
        XdbDebug(__FILE__, (Widget)w,
          "Reposition: erasing x=%d y=%d w=%d h=%d\n",
          TextF_XDraw(w) + xdest + width, TextF_YDraw(w),
          xsrc - xdest, (unsigned int) TextF_ViewHeight(w));
      }
      
    }
    if (start == end)
      end++;
    DrawTextRange(w, start, end);
  }
  else {
    /* redraw the entire visible string */
    start = TextPixelToPos(w, TextF_XDraw(w));
    end = TextPixelToPos(w, TextF_XDraw(w) + TextF_ViewWidth(w)) + 1;
    DrawTextRange(w, start, end);
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "Reposition: xoff=%d old=%d refresh %d-%d\n",
        TextF_XOffset(w), TextF_OldXOffset(w), start, end);
    }
    
  }
  TextF_OldXOffset(w) = TextF_XOffset(w);
}

static void
EraseXRange(XmTextFieldWidget w, int start, int end)
{
  int x1, x2;

  x1 = TextF_XOffset(w) + start;
  x2 = TextF_XOffset(w) + end;
  if (x1 < 0)
    x1 = 0;
  if (x2 > TextF_ViewWidth(w))
    x2 = TextF_ViewWidth(w);
  if (x2 > x1) {
    XClearArea(XtDisplay(w), XtWindow(w),
      TextF_XDraw(w) + x1, TextF_YDraw(w),
      x2 - x1 + 1,
      TextF_ViewHeight(w), False);
  }
}

static void
DrawTextWithCopyArea(XmTextFieldWidget w)
{
  int x, insert_width;
  int xsrc, xdest, width;

  if (!TextF_Echo(w))
    return;
  if (! XtIsRealized((Widget)w))
	return;

  x = TextF_XOffset(w);
  insert_width = TextF_FontTextWidth(w, &TextF_Value(w)[TextF_FastInsertCursorStart(w)], TextF_FastInsertTextLen(w));
  if (CursorPosition(w)) {
    /*
     *  if the text is scrolled, then:
     * 1.  the cursor is at the end
     * 2.  the copy will move to the left.
     */
    xsrc = 0;
    width = TextF_OldCursorX(w) + x;
    xdest = TextF_ViewWidth(w) - (x + TextF_OldCursorX(w)) - insert_width;
    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
      TextF_DrawGC(w),
      TextF_XDraw(w) + xsrc, TextF_YDraw(w),
      (unsigned int) width, (unsigned int) TextF_ViewHeight(w),
      TextF_XDraw(w) + xdest, TextF_YDraw(w));
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "DrawTextWCA: x=%d xsrc=%d xdest=%d width=%d\n",
        x, xsrc, xdest, width);
    }
    
  }
  else {

    /*
     * the text hasn't been scrolled, so:
     * 1.  the text left of the cursor won't change
     * 2.  the stuff after the cursor will be moved right.
     */
    xsrc = TextF_FontTextWidth(w, TextF_Value(w), TextF_FastInsertCursorStart(w)) + x;
    width = TextF_ViewWidth(w) - xsrc;
    xdest = xsrc + insert_width;
    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
      TextF_DrawGC(w),
      TextF_XDraw(w) + xsrc, TextF_YDraw(w),
      (unsigned int) width, (unsigned int) TextF_ViewHeight(w),
      TextF_XDraw(w) + xdest, TextF_YDraw(w));
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "DrawTextWCA: x=%d xsrc=%d xdest=%d width=%d\n",
        x, xsrc, xdest, width);
    }
  }
  DrawTextRange(w, TextF_FastInsertCursorStart(w),
    TextF_FastInsertCursorStart(w) + TextF_FastInsertTextLen(w));

  if (TextF_TextWidth(w) < TextF_OldTextWidth(w)) {
    EraseXRange(w, TextF_TextWidth(w), TextF_OldTextWidth(w));
  }
  TextF_OldTextWidth(w) = TextF_TextWidth(w);
  TextF_OldXOffset(w) = TextF_XOffset(w);
}

static void
DrawAllText(XmTextFieldWidget w)
{
  if (!TextF_Echo(w))
    return;

  DrawTextRange(w, 0, TextF_Length(w));
  if (TextF_TextWidth(w) < TextF_OldTextWidth(w)) {
    EraseXRange(w, TextF_TextWidth(w), TextF_OldTextWidth(w));
  }
  TextF_OldTextWidth(w) = TextF_TextWidth(w);
  TextF_OldXOffset(w) = TextF_XOffset(w);
  TextF_OldHighlightStart(w) = TextF_HighlightStart(w);
  TextF_OldHighlightEnd(w) = TextF_HighlightEnd(w);
}


/* Cursor functions -------------------------------------------------------- */

static Boolean
CursorPosition(XmTextFieldWidget w)
{
  if (TextF_CursorPos(w) < 0)
    TextF_CursorPos(w) = 0;
  else if (TextF_CursorPos(w) > TextF_Length(w))
    TextF_CursorPos(w) = TextF_Length(w);
  return MakePositionVisible(w, TextF_CursorPos(w));
}

static Boolean
CursorMassiveAdjust(XmTextFieldWidget w)
{
  int start, end, last;
  Boolean moved;

  moved = False;
  end = TextF_FontTextWidth(w, TextF_Value(w), TextF_CursorPos(w));
  if (TextF_HighlightStart(w) >= 0)
    start = TextF_FontTextWidth(w, TextF_Value(w), TextF_HighlightStart(w));
  else
    start = end;

  if (end < TextF_ViewWidth(w)) {
    if (TextF_XOffset(w) < 0) {
      TextF_XOffset(w) = 0;
      moved = True;
    }
  }
  else if (start >= TextF_XOffset(w) && end < TextF_XOffset(w) + TextF_ViewWidth(w))
    return moved;
  else {
    last = TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w));
    if (start - end > TextF_ViewWidth(w)) {
      if (last - end > TextF_ViewWidth(w))
	TextF_XOffset(w) = TextF_ViewWidth(w) - last;
      else
	TextF_XOffset(w) = TextF_ViewWidth(w) - end;
    }
    else if (end > TextF_ViewWidth(w))
      TextF_XOffset(w) = TextF_ViewWidth(w) - end;
    else
      TextF_XOffset(w) = 0;
    moved = True;
  }
  return moved;
}

static void
CursorSet(XmTextFieldWidget w, Boolean focus)
{
  TextF_HasFocus(w) = focus;

/* These change the GC !! (Danny) */
  if (focus)
    XSetStipple(XtDisplay((Widget) w), TextF_CursorGC(w), TextF_CursorIBeam(w));
  else
    XSetStipple(XtDisplay((Widget) w), TextF_CursorGC(w), TextF_CursorStipple(w));
}

static void
CursorSaveUnderIBeam(XmTextFieldWidget w, int x)
{
  if (! XtIsRealized((Widget)w))
	return;
  /* Make sure that the text is drawn before saving the cursor */
  XFlush(XtDisplay((Widget) w));

  /* save the area under the cursor */
  XFillRectangle(XtDisplay((Widget) w), TextF_CursorSave(w), TextF_CopyGC(w),
    0, 0, 5, TextF_ViewHeight(w));
  XCopyArea(XtDisplay((Widget) w), XtWindow((Widget) w), TextF_CursorSave(w),
    TextF_CursorGC(w),
    x - 2, TextF_YDraw(w),
    5, TextF_ViewHeight(w), 0, 0);

  TextF_CursorSaveValid(w) = True;
}

static void
CursorRestoreUnderIBeam(XmTextFieldWidget w)
{
  int x;

  if (! XtIsRealized((Widget)w))
	return;
    /* Make sure that the text is drawn before erasing the cursor */
  XFlush(XtDisplay((Widget) w));
  
  x = TextF_OldCursorX(w) + TextF_XDraw(w) + TextF_XOffset(w);
  XCopyArea(XtDisplay((Widget) w), TextF_CursorSave(w), XtWindow((Widget) w),
    TextF_CursorGC(w), 0, 0, 5, TextF_ViewHeight(w),
    x - 2, TextF_YDraw(w));
  TextF_CursorSaveValid(w) = False;
}


static void
CursorDrawIBeam(XmTextFieldWidget w, int x)
{
  /* save the area under the cursor */
  CursorSaveUnderIBeam(w, x);

/* These change the GC !! (Danny) */
  XSetTSOrigin(XtDisplay((Widget) w), TextF_CursorGC(w),
    x - 2, TextF_YOffset(w) - TextF_FontAscent(w) + 1);
  XFillRectangle(XtDisplay((Widget) w), XtWindow((Widget) w), TextF_CursorGC(w),
    x - 2, TextF_YDraw(w), 5, TextF_ViewHeight(w));
}

static void
CursorDraw(XmTextFieldWidget w)
{
  int x;

  if (TextF_CursorPositionVisible(w)) {
    x = TextF_FontTextWidth(w, TextF_Value(w), TextF_CursorPos(w));
    TextF_OldCursorPos(w) = TextF_CursorPos(w);
    TextF_OldCursorX(w) = x;
    x += TextF_XDraw(w) + TextF_XOffset(w);

    CursorDrawIBeam(w, x);

    TextF_BlinkOn(w) = True;
  }
}

static void
CursorErase(XmTextFieldWidget w)
{
  if (TextF_CursorSaveValid(w)) {
    CursorRestoreUnderIBeam(w);
  }
  TextF_BlinkOn(w) = False;
}

static void
ClearHighlight(XmTextFieldWidget w)
{
  if (!TextF_Echo(w))
    return;

  if (TextF_HighlightStart(w) >= 0) {
    CursorErase(w);
    DrawText(w, TextF_HighlightStart(w), TextF_HighlightEnd(w), False);
    CursorDraw(w);
    TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
  }
  TextF_OldHighlightStart(w) = TextF_OldHighlightEnd(w) = -1;
}

static void
DrawHighlight(XmTextFieldWidget w)
{
  if (!TextF_Echo(w))
    return;

  if (!XtIsRealized(w)) return;

  if (TextF_OldHighlightStart(w) < 0) {
    DrawText(w, TextF_HighlightStart(w), TextF_HighlightEnd(w),
      True);
  }
  else {
    DrawText(w, TextF_HighlightStart(w), TextF_OldHighlightStart(w),
      (TextF_HighlightStart(w) < TextF_OldHighlightStart(w)));
    DrawText(w, TextF_HighlightEnd(w), TextF_OldHighlightEnd(w),
      (TextF_HighlightEnd(w) > TextF_OldHighlightEnd(w)));
  }
  TextF_OldHighlightStart(w) = TextF_HighlightStart(w);
  TextF_OldHighlightEnd(w) = TextF_HighlightEnd(w);
}

/*
 * Special redraw function after a text insertion
 */
static void
DrawInsert(XmTextFieldWidget w)
{
  if (!XtIsRealized(w)) return;

  /*  CursorErase must be called before this */
  DrawTextWithCopyArea(w);
  CursorDraw(w);
}

/*
 * Special redraw function after a cursor reposition (i.e.: no text changes)
 */
static void
DrawMove(XmTextFieldWidget w)
{
  if (!XtIsRealized(w)) return;

  CursorErase(w);
  if (CursorPosition(w))
    DrawTextReposition(w);
  CursorDraw(w);
}

/*
 * Redraw the entire widget, but don't scroll the window much
 */
static void
Draw(XmTextFieldWidget w)
{
  if (!XtIsRealized(w)) return;

  CursorErase(w);
  CursorPosition(w);
  DrawAllText(w);
  CursorDraw(w);
}

/*
 * Clear window & redraw the entire widget
 */
static void
DrawAll(XmTextFieldWidget w)
{
  if (!XtIsRealized(w)) return;

  XClearArea(XtDisplay(w), XtWindow(w),
    Prim_HighlightThickness(w),
    Prim_HighlightThickness(w),
    XtWidth(w) - 2 * Prim_HighlightThickness(w),
    XtHeight(w) - 2 * Prim_HighlightThickness(w),
    False);
  CursorPosition(w);
  DrawAllText(w);
  CursorDraw(w);
}

/*
 * Like Draw(), but has different rules about scrolling the window to
 * put the cursor in a good place
 */
static void
MassiveChangeDraw(XmTextFieldWidget w)
{
  if (!XtIsRealized(w)) return;

  CursorErase(w);
  CursorMassiveAdjust(w);
  DrawAllText(w);
  CursorDraw(w);
}



/*
 * text search procedures -----------------------------------------------------
 */

static Boolean
InWord(char s)
{
  switch (s) {
  case ' ':
  case '\t':
  case '\n':
  case '!':
  case '?':
  case '.':
  case ',':
    return False;
  default:
    return True;
  }
}

static XmTextPosition
WordEnd(XmTextFieldWidget w, XmTextPosition pos)
{
  while ((pos < TextF_Length(w)) && InWord(TextF_Value(w)[pos]))
    pos++;
  return pos;
}

static XmTextPosition
WordStart(XmTextFieldWidget w, XmTextPosition pos)
{
  while ((pos > 0) && InWord(TextF_Value(w)[pos-1]))
    pos--;
  return pos;
}

static XmTextPosition
SkipForward(XmTextFieldWidget w, XmTextPosition pos)
{
  while ((pos < TextF_Length(w)) && !InWord(TextF_Value(w)[pos]))
    pos++;
  return pos;
}

static XmTextPosition
SkipBackward(XmTextFieldWidget w, XmTextPosition pos)
{
  while ((pos > 0) && !InWord(TextF_Value(w)[pos-1]))
    pos--;
  return pos;
}

static XmTextScanType
ScanType(XmTextFieldWidget w)
{
  XmTextScanType type;

  if (TextF_SelectionArray(w)) {
    if (w->text.sarray_index>=TextF_SelectionArrayCount(w))
      w->text.sarray_index=0;
    type=TextF_SelectionArray(w)[w->text.sarray_index];
  }
  else {
    switch (w->text.sarray_index) {
      case 1:
        type=XmSELECT_WORD;
        break;
      case 2:
        type=XmSELECT_ALL;
        break;
      default:
        type=XmSELECT_POSITION;
        w->text.sarray_index=0;
        break;
    }
  }

  return type;
}

static XmTextPosition
ScanTypeStart(XmTextFieldWidget w, XmTextPosition pos)
{
  switch (ScanType(w)) {
    case XmSELECT_WORD:
      pos = WordStart(w,pos);
      break;
    case XmSELECT_WHITESPACE:
      pos = SkipBackward(w,pos);
      break;
    case XmSELECT_LINE:
    case XmSELECT_PARAGRAPH:
    case XmSELECT_ALL:
      pos = 0;
      break;
    case XmSELECT_POSITION:
    default:
      break;
  }
  return pos;
}

static XmTextPosition
ScanTypeEnd(XmTextFieldWidget w, XmTextPosition pos)
{
  switch (ScanType(w)) {
    case XmSELECT_WORD:
      pos = WordEnd(w,pos);
      break;
    case XmSELECT_WHITESPACE:
      pos = SkipForward(w,pos);
      break;
    case XmSELECT_LINE:
    case XmSELECT_PARAGRAPH:
    case XmSELECT_ALL:
      pos = TextF_Length(w);
      break;
    case XmSELECT_POSITION:
    default:
      break;
  }
  return pos;
}


/*
 * High level text insert and delete routines ---------------------------------
 */

static void
VerifyBell(XmTextFieldWidget w)
{
  if (TextF_VerifyBell(w))
    XBell(XtDisplay((Widget) w), 50);
}

static Boolean
DoCursorMove(XmTextFieldWidget w, XEvent * ev, XmTextPosition pos, Boolean highlight, Boolean drawit)
{
  XmTextVerifyCallbackStruct cbs;

  if (pos > TextF_Length(w))
    pos = TextF_Length(w);

  cbs.doit = True;
  if (TextF_MotionVerifyCallback(w)) {
    cbs.reason = XmCR_MOVING_INSERT_CURSOR;
    cbs.event = ev;
    cbs.currInsert = TextF_CursorPos(w);
    cbs.newInsert = pos;
    cbs.startPos = cbs.endPos = 0;
    cbs.text = NULL;

    XtCallCallbacks((Widget) w, XmNmotionVerifyCallback, &cbs);

    if (cbs.doit) {
      pos = cbs.newInsert;
    }
  }

  if (cbs.doit) {
    if (highlight)
      ClearHighlight(w);
    TextF_CursorPos(w) = pos;
    if (drawit)
      DrawMove(w);
  }
  else {
    VerifyBell(w);
  }

  return cbs.doit;
}

static void
DoInsert(XmTextFieldWidget w, XEvent * ev, char *buf, int len)
{
  XmTextVerifyCallbackStruct cbs;
  XmTextBlockRec tb;
  Boolean fastdraw;

  if (len <= 0)
    return;

  cbs.doit = True;
  tb.ptr = NULL;
  if (TextF_ModifyVerifyCallback(w)) {
    cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    cbs.event = ev;
    cbs.currInsert = cbs.newInsert = TextF_CursorPos(w);
    cbs.startPos = cbs.endPos = TextF_CursorPos(w);

    tb.ptr = XtMalloc(len);
    tb.length = len;
    tb.format = XmFMT_8_BIT;
    strncpy(tb.ptr, buf, len);

    cbs.text = &tb;
    XtCallCallbacks((Widget) w, XmNmodifyVerifyCallback, &cbs);

    if (cbs.doit) {
      buf = tb.ptr;
      len = tb.length;
    }
    else {
      VerifyBell(w);
    }
  }

  if (len > 0 && cbs.doit) {
    if (TextF_Length(w) + len > TextF_MaxLength(w)) {
      VerifyBell(w);
    }
    else {
      CursorErase(w);
      fastdraw = TextInsert(w, buf, len);

      DoCursorMove(w, ev, TextF_CursorPos(w) + len, True, False);

      if (fastdraw)
	DrawInsert(w);
      else
	Draw(w);

      if (TextF_ValueChangedCallback(w)) {
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.currInsert = cbs.newInsert = TextF_CursorPos(w);
	cbs.startPos = cbs.endPos = TextF_CursorPos(w);

	if (tb.ptr) {
	  tb.ptr = XtMalloc(len);
	  strncpy(tb.ptr, buf, len);
	  XtFree(buf);
	}
	else {
	  tb.ptr = XtMalloc(len);
	  strncpy(tb.ptr, buf, len);
	}
	tb.length = len;
	tb.format = XmFMT_8_BIT;

	cbs.text = &tb;
	XtCallCallbacks((Widget) w, XmNvalueChangedCallback, &cbs);
      }
    }
  }
  if (tb.ptr)
    XtFree(tb.ptr);
}

static void
DoScanType(XmTextFieldWidget w, XEvent *ev, XmTextPosition pos)
{
  switch (ScanType(w)) {
    case XmSELECT_POSITION:
      DoCursorMove(w, ev, pos, True, True);
      
      TextF_HighlightPivot(w) = TextF_CursorPos(w);
      break;
    default:
      TextF_HighlightPivot(w) = TextF_HighlightStart(w) = ScanTypeStart(w,pos);
      TextF_HighlightEnd(w) = ScanTypeEnd(w,pos);

        /* FIXME - what event type should this be? */
      PrimarySelectionComplete(w, ev->xbutton.time);
      Draw(w);
      break;
  }
}



/*
 * Selection and Clipboard utilities ------------------------------------------
 */

static void
ExtendHighlight(XmTextFieldWidget w)
{
  int x, pos;

  if (!TextF_AllowSelection(w))
    return;

  x = w->text.select_pos_x;

  if (x < (int) TextF_XDraw(w)) {
    pos = TextPixelToSelectionPos(w, (int) 0);
    if (pos > 0)
      pos--;
  }
  else if (x > (int) (TextF_XDraw(w) + TextF_ViewWidth(w))) {
    pos = TextPixelToSelectionPos(w, (int) (TextF_XDraw(w) + TextF_ViewWidth(w)));
    if (pos < TextF_Length(w))
      pos++;
  }
  else {
    pos = TextPixelToSelectionPos(w, x);
  }

  if (pos == TextF_CursorPos(w))
    return;
  DoCursorMove(w, NULL, pos, False, True);

  if (pos<TextF_HighlightPivot(w)) {
    pos = TextF_HighlightStart(w) = ScanTypeStart(w,pos);
    TextF_HighlightEnd(w) = ScanTypeEnd(w,TextF_HighlightPivot(w));
  }
  else {
    TextF_HighlightStart(w) = ScanTypeStart(w,TextF_HighlightPivot(w));
    pos = TextF_HighlightEnd(w) = ScanTypeEnd(w,pos);
  }

  CursorErase(w);
  if (MakePositionVisible(w,pos))
    DrawTextReposition(w);
  DrawHighlight(w);
  CursorDraw(w);
}

static void
ExtendTimer(XtPointer client_data, XtIntervalId * idp)
{
  XmTextFieldWidget w = (XmTextFieldWidget) client_data;
  int highlight_time;

  highlight_time = XtGetMultiClickTime(XtDisplay((Widget) w)) / 2;

  ExtendHighlight(w);
  TextF_SelectId(w) = XtAppAddTimeOut(
    XtWidgetToApplicationContext((Widget) w),
    (unsigned long) highlight_time,
    ExtendTimer,
    (XtPointer) w);

}

/* #include <X11/Xmu/Xmu.h> */
/* ARGSUSED */
static Boolean
ConvertSelection(Widget aw, Atom * selection, Atom * target, Atom * type,
  XtPointer * value, unsigned long *length, int *format)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
/*   XSelectionRequestEvent *req = XtGetSelectionRequest(aw, *selection, NULL); */

  /* if (*target == XA_TARGETS(XtDisplay(aw))) { */
/*     Atom *targetP, *std_targets; */
/*     unsigned long std_length; */

/*     XmuConvertStandardSelection(aw, req->time, selection, */
/*       target, type, (XPointer *) & std_targets, */
/*       &std_length, format); */

/*     *value = XtMalloc((unsigned) sizeof(Atom) * (std_length + 1)); */
/*     targetP = *(Atom **) value; */
/*     *length = std_length + 1; */
/*     *targetP++ = XA_STRING; */
/*     bcopy((char *) targetP, (char *) std_targets, sizeof(Atom) * std_length); */
/*     XtFree((char *) std_targets); */
/*     *type = XA_ATOM; */
/*     *format = sizeof(Atom) * 8; */
/*     return True; */
/*   } */
/*   else  */ if (*target == XA_STRING) {
    *length = (long) TextF_SelectionLen(w);
    *value = TextF_SelectionText(w);
    *type = XA_STRING;
    *format = 8;
    return True;
  }
  return False;
}

/* ARGSUSED */
static void
LoseSelection(Widget aw, Atom * selection)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  ClearHighlight(w);
}

/* ARGSUSED */
static void
RequestSelection(Widget aw, XtPointer client, Atom * selection, Atom * type,
  XtPointer value, unsigned long *length, int *format)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if ((!value) || (*length == 0)) {
    DBGW("RequestSelection: no selection available");
  }
  else {
    XEvent *ev;
    XmTextPosition pos;

    ev = (XEvent *) client;
    pos = TextPixelToSelectionPos(w, ev->xbutton.x);
    DBG1("event pos", pos);
    if (XdbInDebug(__FILE__, (Widget)w)) {
      XdbDebug(__FILE__, (Widget)w,
        "RequestSelection: inserting '%s' length=%d at pos: %d\n",
        (char *) value, (int) (*length), pos);
    }
    
    if (DoCursorMove(w, ev, pos, True, True))
      DoInsert(w, ev, (char *) value, (int) (*length));
  }
}

static void
PrimarySelectionComplete(XmTextFieldWidget w, Time time)
{
  int len;

  if (TextF_SelectionText(w))
    XtFree(TextF_SelectionText(w));
  TextF_SelectionText(w) = NULL;
  TextF_SelectionLen(w) = 0;

  len = TextF_HighlightEnd(w) - TextF_HighlightStart(w);
  if (len > 0) {
    TextF_SelectionLen(w) = len;
    TextF_SelectionText(w) = XtMalloc(len);
    strncpy(TextF_SelectionText(w), &TextF_Value(w)[TextF_HighlightStart(w)], len);

    XtOwnSelection((Widget) w, XA_PRIMARY, time,
      ConvertSelection, LoseSelection, NULL);
#ifdef DEBUG
    XChangeProperty(XtDisplay((Widget) w),
      DefaultRootWindow(XtDisplay((Widget) w)),
      XA_CUT_BUFFER0, XA_STRING, 8, PropModeReplace,
      (unsigned char *) TextF_SelectionText(w), len);
#endif
  }
}



/*
 * Action procedures ----------------------------------------------------------
 */

static void
_BlinkCursorCallback(XtPointer client_data, XtIntervalId * idp)
{
  XmTextFieldWidget w = (XmTextFieldWidget) client_data;

  if (TextF_BlinkOn(w))
    CursorErase(w);
  else
    CursorDraw(w);

  TextF_TimerId(w) = XtAppAddTimeOut(
    XtWidgetToApplicationContext((Widget) w),
    TextF_BlinkRate(w),
    _BlinkCursorCallback,
    (XtPointer) w);
}


/* the focus in action routine changes the cursor to be filled,
 * and sets it blinking. */
void
_XmTextFieldFocusIn(Widget w, XEvent * event, String * params, Cardinal * num_params)
{
  XmTextVerifyCallbackStruct cbs;
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("_XmTextFieldFocusIn");

  if (!TextF_HasFocus(tfw)) {
    CursorErase(tfw);
    CursorSet(tfw, True);
    if (TextF_BlinkRate(tfw) > 0 && !TextF_TimerId(tfw)) {
      TextF_TimerId(tfw) = XtAppAddTimeOut(
	XtWidgetToApplicationContext((Widget) tfw),
	TextF_BlinkRate(tfw),
	_BlinkCursorCallback,
	(XtPointer) tfw);
    }
    else
      CursorDraw(tfw);

    if (TextF_FocusCallback(tfw)) {
      cbs.reason = XmCR_FOCUS;
      cbs.event = event;
      cbs.currInsert = cbs.newInsert = TextF_CursorPos(tfw);
      cbs.startPos = cbs.endPos = 0;
      cbs.text = NULL;

      XtCallCallbackList((Widget) tfw, TextF_FocusCallback(tfw), &cbs);
    }
    
    XtCallActionProc((Widget) tfw, "PrimitiveFocusIn", event, params, *num_params);

  }
}

/* the focus out action routine changes the cursor to be stippled. */
void
_XmTextFieldFocusOut(Widget w, XEvent * event, String * params, Cardinal * num_params)
{
  XmTextVerifyCallbackStruct cbs;
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("_XmTextFieldFocusOut");

  if (TextF_HasFocus(tfw)) {
    if (TextF_TimerId(tfw)) {
      XtRemoveTimeOut(TextF_TimerId(tfw));
      TextF_TimerId(tfw) = 0;
    }
    CursorErase(tfw);
    CursorSet(tfw, False);
    CursorDraw(tfw);

    if (TextF_LosingFocusCallback(tfw)) {
      cbs.reason = XmCR_LOSING_FOCUS;
      cbs.event = event;
      cbs.currInsert = cbs.newInsert = TextF_CursorPos(tfw);
      cbs.startPos = cbs.endPos = 0;
      cbs.text = NULL;

      XtCallCallbackList((Widget) tfw, TextF_LosingFocusCallback(tfw), &cbs);
    }

    XtCallActionProc((Widget) tfw, "PrimitiveFocusOut", event, params, *num_params);
  }
}

static void
activate(Widget w, XEvent * event, String * params, Cardinal * num_params)
{
  XmAnyCallbackStruct cbs;

  DBGW("activate");


  cbs.reason = XmCR_ACTIVATE;
  cbs.event = event;

  XtCallCallbackList(w, TextF_ActivateCallback((XmTextFieldWidget)w), &cbs);
}

static void
backward_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("backward_character");


  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) > 0) {
    XmTextPosition pos;

    pos = TextF_CursorPos(tfw) - 1;
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
backward_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("backward_word");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) > 0) {
    XmTextPosition pos;

    pos = WordStart(tfw,SkipBackward(tfw,TextF_CursorPos(tfw)));
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
beep(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("beep");
  XBell(XtDisplay(w), 50);
}

static void
beginning_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("beginning_of_line");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) > 0) {
    XmTextPosition pos;

    pos = 0;
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
clear_selection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("clear_selection");
  ClearHighlight(tfw);
}

static void
copy_clipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("copy_clipboard");
}

static void
copy_primary(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("copy_primary");
  /* paste from the primary into text field at the cursor
   * position */
}

static void
copy_to(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("copy_to");

  if (!TextF_AllowSelection(tfw))
    return;

/*   _XmTextFieldFocusIn(w,ev,params,num_params); */

  XtGetSelectionValue(w, XA_PRIMARY, XA_STRING,
    RequestSelection,
    (XtPointer) ev, ev->xbutton.time);
}

static void
cut_clipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("cut_clipboard");
}

static void
cut_primary(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("cut_primary");
}

static void
delete_next_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_next_character");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_HighlightStart(tfw) >= 0 && TextF_PendingDelete(tfw)) {
    TextDeleteHighlighted(tfw);
    MassiveChangeDraw(tfw);
  }
  else if (TextF_CursorPos(tfw) < TextF_Length(tfw)) {
    ClearHighlight(tfw);
    TextDelete(tfw, TextF_CursorPos(tfw), 1);
    Draw(tfw);
  }
}

static void
delete_previous_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_previous_character");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_HighlightStart(tfw) >= 0 && TextF_PendingDelete(tfw)) {
    TextDeleteHighlighted(tfw);
    MassiveChangeDraw(tfw);
  }
  else if (TextF_CursorPos(tfw) > 0) {
    ClearHighlight(tfw);
    TextDelete(tfw, TextF_CursorPos(tfw) - 1, 1);
    TextF_CursorPos(tfw)--;
    Draw(tfw);
  }
}

static void
delete_next_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_next_word");

  if (!TextF_Editable(tfw))
    return;
  
  TextF_HighlightStart(tfw) = TextF_CursorPos(tfw);
  TextF_HighlightEnd(tfw) = SkipForward(tfw,WordEnd(tfw,TextF_CursorPos(tfw)));
  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
delete_previous_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_previous_word");

  if (!TextF_Editable(tfw))
    return;

  TextF_HighlightStart(tfw) =  WordStart(tfw,SkipBackward(tfw,TextF_CursorPos(tfw)));
  TextF_HighlightEnd(tfw) = TextF_CursorPos(tfw);
  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
delete_selection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("delete_selection");

  if (!TextF_Editable(tfw))
    return;

  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
delete_to_end_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_to_end_of_line");

  if (!TextF_Editable(tfw))
    return;

  TextF_HighlightStart(tfw) = TextF_CursorPos(tfw);
  TextF_HighlightEnd(tfw) = TextF_Length(tfw);
  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
delete_to_start_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_to_start_of_line");

  if (!TextF_Editable(tfw))
    return;

  TextF_HighlightStart(tfw) = 0;
  TextF_HighlightEnd(tfw) = TextF_CursorPos(tfw);
  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
delete_all(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("delete_all");


  if (!TextF_Editable(tfw))
    return;

  TextF_HighlightStart(tfw) = 0;
  TextF_HighlightEnd(tfw) = TextF_Length(w);
  TextDeleteHighlighted(tfw);
  MassiveChangeDraw(tfw);
}

static void
deselect_all(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("deselect_all");
}

static void
do_quick_action(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("do_quick_action");
}

static void
end_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("end_of_line");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) < TextF_Length(tfw)) {
    XmTextPosition pos;

    pos = TextF_Length(tfw);
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
enter(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("enter");
  /* do some textfield specific stuff */

  XtCallActionProc(w, "PrimitiveEnter", ev, params, *num_params);

  if (TextF_Editable(tfw))
    XmImSetFocusValues(w, NULL, 0);
}

/*
 * extend-adjust : selects text from the anchor to the pointer position and
 *      deselects text outside that range.
 */
static void
extend_adjust(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("extend_adjust");

  if (!TextF_AllowSelection(tfw))
    return;

  tfw->text.select_pos_x = ev->xbutton.x;

  if (ev->xbutton.x < TextF_XDraw(tfw) || ev->xbutton.x > TextF_XDraw(tfw) + TextF_ViewWidth(tfw)) {
    if (TextF_SelectId(tfw))
      ExtendHighlight(tfw);
    else
      ExtendTimer((XtPointer) tfw, (XtIntervalId) 0);
  }
  else {
    if (TextF_SelectId(tfw)) {
      XtRemoveTimeOut(TextF_SelectId(tfw));
      TextF_SelectId(tfw) = (XtIntervalId) 0;
    }
    ExtendHighlight(tfw);
  }
}

/*
 * extend-end() : moves the insertion cursor to the position of the pointer
 */
static void
extend_end(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("extend_end");

  if (!TextF_AllowSelection(tfw))
    return;

  if (TextF_SelectId(tfw)) {
    XtRemoveTimeOut(TextF_SelectId(tfw));
    TextF_SelectId(tfw) = (XtIntervalId) 0;
  }
  PrimarySelectionComplete(tfw, ev->xbutton.time);
}

/*
 * extend-start() : adjusts the anchor using te balance-beam method. Selects text
 *      from the anchor to the pointer position and deselects text outside that range.
 */
static void
extend_start(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  int pos;

  DBGW("extend_start");

  XmProcessTraversal(w, XmTRAVERSE_CURRENT);

  if (!TextF_AllowSelection(tfw))
    return;

  pos = TextPixelToSelectionPos(tfw, ev->xbutton.x);
  DoCursorMove(tfw, ev, pos, False, True);

  if (TextF_HighlightStart(tfw) < 0) {
    TextF_HighlightStart(tfw) =
      TextF_HighlightEnd(tfw) =
      TextF_HighlightPivot(tfw) = TextF_CursorPos(tfw);
  }
  if (TextF_CursorPos(tfw) < TextF_HighlightPivot(tfw)) {
    TextF_HighlightStart(tfw) = TextF_CursorPos(tfw);
  }
  else {
    TextF_HighlightEnd(tfw) = TextF_CursorPos(tfw);
  }
}

static void
forward_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("forward_character");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) < TextF_Length(tfw)) {
    XmTextPosition pos;

    pos = TextF_CursorPos(tfw) + 1;
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
forward_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("forward_word");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) < TextF_Length(tfw)) {
    XmTextPosition pos;
    
    pos=SkipForward(tfw,WordEnd(tfw,TextF_CursorPos(tfw)));
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
grab_focus(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  XmTextPosition pos;

  DBGW("grab_focus");

  XmProcessTraversal((Widget) tfw, XmTRAVERSE_CURRENT);

  if (!TextF_AllowSelection(tfw))
    return;

  pos = TextPixelToSelectionPos(tfw, ev->xbutton.x);

  if ((tfw->text.last_time+XtGetMultiClickTime(XtDisplay((Widget) tfw))) >
    ev->xbutton.time)
    tfw->text.sarray_index++;
  else tfw->text.sarray_index=0;
  
  DoScanType(tfw,ev,pos);
  
  tfw->text.last_time=ev->xbutton.time;
}

static void
Help(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("help");
}

static void
insert_string(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("insert_string");
}

static void
key_select(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("key_select");
}

static void
kill_next_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("kill_next_character");
  delete_next_character((Widget)tfw, ev, params, num_params);
}

static void
kill_next_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("kill_next_word");
  delete_next_word((Widget)tfw, ev, params, num_params);
}

static void
kill_previous_character(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("kill_previous_character");
  delete_previous_character((Widget)tfw, ev, params, num_params);
}

static void
kill_previous_word(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("kill_previous_word");
  delete_previous_word((Widget)tfw, ev, params, num_params);
}

static void
kill_selection(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("kill_selection");
}

static void
kill_to_end_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("kill_to_end_of_line");
}

static void
kill_to_start_of_line(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("kill_to_start_of_line");
}

static void
leave(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("leave");
  /* do some textfield specific stuff */

  XtCallActionProc((Widget) tfw, "PrimitiveLeave", ev, params, *num_params);

  if (TextF_Editable(tfw))
    XmImUnsetFocus((Widget) tfw);
}

static void
move_destination(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("move_destination");
}

static void
move_to(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("move_to");
}

static void
next_tab_group(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("next_tab_group");
  XtCallActionProc(w, "PrimitiveNextTabGroup", ev, params, *num_params);
}

static void
page_left(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("page_left");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) > 0) {
    XmTextPosition pos;

    pos = TextPixelToPos(tfw, TextF_XDraw(tfw) - TextF_ViewWidth(tfw));
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
page_right(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("page_right");

  if (!TextF_Editable(tfw))
    return;

  if (TextF_CursorPos(tfw) < TextF_Length(tfw)) {
    XmTextPosition pos;

    pos = TextPixelToPos(tfw, TextF_XDraw(tfw) + TextF_ViewWidth(tfw));
    DoCursorMove(tfw, ev, pos, True, True);
  }
}

static void
paste_clipboard(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("paste_clipboard");
}

static void
paste_primary(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("paste_primary");
}

static void
prev_tab_group(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("prev_tab_group");
  XtCallActionProc(w, "PrimitivePrevTabGroup", ev, params, *num_params);
}

static void
process_bdrag(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("process_bdrag");
}

static void
process_cancel(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("process_cancel");
}

static void
process_home(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("process_home");
}

static void
process_return(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("process_return");
}

static void
process_tab(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("process_tab");
}

static void
quick_copy_set(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("quick_copy_set");
}

static void
quick_cut_set(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("quick_cut_set");
}

static void
redraw_display(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("redraw_display");
  Draw((XmTextFieldWidget)w);
}

static void
secondary_adjust(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("secondary_adjust");
}

static void
secondary_notify(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("secondary_notify");
}

static void
secondary_start(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("secondary_start");
}

static void
select_adjust(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("select_adjust");
}

static void
select_all(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("select_all");

  TextF_HighlightPivot(tfw) = TextF_HighlightStart(tfw) = 0;
  TextF_HighlightEnd(tfw) = TextF_Length(tfw);

  /* FIXME - what event type should this be? */
  PrimarySelectionComplete(tfw, ev->xbutton.time);
  Draw(tfw);
}

static void
select_end(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("select_end");
}

static void
select_start(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("select_start");

  grab_focus((Widget)tfw,ev,params,num_params);
}

static void
self_insert(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
#define INSERTCHARBUFSIZ 32
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  char buf[INSERTCHARBUFSIZ];
  KeySym keysym;
  int len, status;

  DBGW("self_insert");

  if (!TextF_Editable(tfw)) {
    if (TextF_VerifyBell(tfw))
      beep((Widget)tfw, ev, params, num_params);
    return;
  }

  len = XmImMbLookupString((Widget)tfw, (XKeyPressedEvent *)ev, buf, INSERTCHARBUFSIZ, &keysym, &status);

  if (XdbInDebug(__FILE__, (Widget)tfw)) {
    XdbDebug(__FILE__, (Widget)tfw, "XmImMbLookupString => %d\n", len);
    XdbDebug0(__FILE__, (Widget)tfw, "\tStatus %s\n",
      (status == XLookupNone) ? "none" :
      (status == XLookupChars) ? "chars" :
      (status == XLookupBoth) ? "both" :
      (status == XLookupKeySym) ? "keysym" :
      (status == XBufferOverflow) ? "overflow" : "????");
    if (status == XLookupBoth || status == XLookupKeySym)
      XdbDebug0(__FILE__, (Widget)tfw, "\tKeySym 0x%X\n", keysym);
    if (len > 0) {
      int i;

      XdbDebug0(__FILE__, (Widget)tfw, "\tBuffer ");
      for (i = 0; i < len; i++)
	XdbDebug(__FILE__, (Widget)tfw, " %X", 0xFF & buf[i]);
      XdbDebug0(__FILE__, (Widget)tfw, "\n");
    }
  }

  if (len > 0) {		/* FIX ME */
    if (status == XLookupBoth || status == XLookupChars)
      DoInsert(tfw, ev, buf, len);
  }
}

static void
set_anchor(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("set_anchor");
}

static void
set_insertion_point(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("set_insertion_point");
}

static void
set_selection_hint(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("set_selection_hint");
}

static void
toggle_add_mode(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("toggle_add_mode");
}

static void
traverse_home(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;
  DBGW("traverse_home");
  XmProcessTraversal((Widget) tfw, XmTRAVERSE_HOME);
}

static void
traverse_next(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("traverse_next");
  XmProcessTraversal((Widget) tfw, XmTRAVERSE_NEXT);
}

static void
traverse_prev(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  XmTextFieldWidget tfw = (XmTextFieldWidget) w;

  DBGW("traverse_prev");
  XmProcessTraversal((Widget) tfw, XmTRAVERSE_PREV);
}

static void
unkill(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("unkill");
}

static void
unmap(Widget w, XEvent * ev, String * params, Cardinal * num_params)
{
  DBGW("unmap");
}


/*
 * Public functions -----------------------------------------------------------
 */

Widget
XmCreateTextField(Widget parent, char *name, Arg * arglist, Cardinal argCount)
{
  return XtCreateWidget(name, xmTextFieldWidgetClass, parent, arglist, argCount);
}

void
XmTextFieldClearSelection(Widget aw, Time time)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

}

Boolean
XmTextFieldCopy(Widget aw, Time time)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return False;

  return False;
}

Boolean
XmTextFieldCut(Widget aw, Time time)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return False;

  return False;
}

int
XmTextFieldGetBaseline(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_YOffset(w);
}

XmTextPosition
XmTextFieldGetCursorPosition(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_CursorPos(w);
}

Boolean
XmTextFieldGetEditable(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_Editable(w);
}

XmTextPosition
XmTextFieldGetInsertionPosition(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_CursorPos(w);
}

XmTextPosition
XmTextFieldGetLastPosition(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_Length(w);
}

int
XmTextFieldGetMaxLength(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextF_MaxLength(w);
}

char *
XmTextFieldGetSelection(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return NULL;

  return NULL;
}

Boolean
XmTextFieldGetSelectionPosition(Widget aw, XmTextPosition * left, XmTextPosition * right)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return False;

  return False;
}

wchar_t *
XmTextFieldGetSelectionWcs(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return NULL;

  return NULL;
}

char *
XmTextFieldGetString(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  char *ret;

  if (!XmIsTextField(w)) {
    ret = XtMalloc(1);
    *ret = '\0';
    return ret;
  }

  ret = XtMalloc(TextF_Length(w) + 1);
  strncpy(ret, TextF_Value(w), TextF_Length(w));
  ret[TextF_Length(w)] = '\0';
  return ret;
}

wchar_t *
XmTextFieldGetStringWcs(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return NULL;

  return NULL;
}

int
XmTextFieldGetSubstring(Widget aw, XmTextPosition start, int num_chars, int buffer_size, char *buffer)
{
  int len;
  int retval = XmCOPY_SUCCEEDED;
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(aw))
    return XmCOPY_FAILED;

  if (start < 0 || start > TextF_Length(w))
    return XmCOPY_FAILED;

/* FIXME */
  if (num_chars > buffer_size)
    len = num_chars;
  else {
    len = buffer_size - 1;
    retval = XmCOPY_TRUNCATED;
  }

  if (start + len > TextF_Length(w))
    len = TextF_Length(w) - start;
  strncpy(buffer, TextF_Value(w) + start, len);
  buffer[len] = '\0';

  return retval;
}

int
XmTextFieldGetSubstringWcs(Widget aw, XmTextPosition start, int num_chars, int buffer_size, wchar_t * buffer)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return XmCOPY_FAILED;

  return 0;
}

void
XmTextFieldInsert(Widget aw, XmTextPosition pos, char *str)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  XmTextPosition len;

  if (!XmIsTextField(w))
    return;

  if (str && ((len = strlen(str)) > 0) && pos >= 0 && pos <= TextF_Length(w)) {
    TextF_HighlightStart(w) = TextF_HighlightEnd(w) = pos;
    TextInsert(w, str, len);
    MassiveChangeDraw(w);
  }
}

void
XmTextFieldInsertWcs(Widget aw, XmTextPosition position, wchar_t * wcstring)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;
}

Boolean
XmTextFieldPaste(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return False;

  return False;
}

Boolean
XmTextFieldPosToXY(Widget aw, XmTextPosition position, Position * x, Position * y)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  int pixel;

  if (!XmIsTextField(w))
    return False;

  if (position < 0 || position > TextF_Length(w))
    return False;

  pixel = TextF_XOffset(w) + TextF_FontTextWidth(w, TextF_Value(w), position);
  if (pixel < TextF_XDraw(w) || pixel > (TextF_XDraw(w) + TextF_ViewWidth(w)))
    return False;

  *x = (Position) pixel;
  *y = (Position) TextF_YOffset(w);
  return True;
}

Boolean
XmTextFieldRemove(Widget aw)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return False;

  return False;
}

void
XmTextFieldReplace(Widget aw, XmTextPosition first, XmTextPosition last, char *str)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  XmTextPosition len;

  if (!XmIsTextField(w))
    return;

  if (str) {
    len = strlen(str);
    if (last > TextF_Length(w))
      last = TextF_Length(w);
    if (first <= last) {
      TextF_HighlightStart(w) = first;
      TextF_HighlightEnd(w) = last;
      TextDeleteHighlighted(w);
      TextInsert(w, str, len);
      MassiveChangeDraw(w);
    }
  }
}

void
XmTextFieldReplaceWcs(Widget aw, XmTextPosition from_pos, XmTextPosition to_pos, wchar_t * wcstring)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

}

void
XmTextFieldSetAddMode(Widget aw, Boolean state)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

}

void
XmTextFieldSetCursorPosition(Widget aw, XmTextPosition pos)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

  if (pos >= 0 && pos <= TextF_Length(w)) {
    TextF_CursorPos(w) = pos;
    MassiveChangeDraw(w);
  }
}

static void
_XmTextFieldSetEditable(Widget w, Boolean e)
{
  if (!XtIsRealized(w))
    return;

  if (e) {			/* Becomes editable */
    Arg args[10];
    int nargs;

    XmImRegister(w, 0);

    nargs = 0;
    XtSetArg(args[nargs], XmNbackground, XtBackground(w));
    nargs++;
    XtSetArg(args[nargs], XmNforeground, Prim_Foreground(w));
    nargs++;
    XmImSetValues(w, args, nargs);
  }
  else {			/* Becomes un-editable */
    XmImUnregister(w);
  }
}

void
XmTextFieldSetEditable(Widget aw, Boolean editable)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

  if (TextF_Editable(w) != editable)
    _XmTextFieldSetEditable(aw, editable);

  TextF_Editable(w) = editable;
}

void
XmTextFieldSetHighlight(Widget aw, XmTextPosition left, XmTextPosition right, XmHighlightMode mode)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

}

void
XmTextFieldSetInsertionPosition(Widget aw, XmTextPosition pos)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

  if (pos >= 0 && pos <= TextF_Length(w)) {
    TextF_CursorPos(w) = pos;
    MassiveChangeDraw(w);
  }
}

void
XmTextFieldSetMaxLength(Widget aw, int max_length)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

  TextF_MaxLength(w) = max_length;

  if (TextF_Length(w) > max_length) {
    TextF_Length(w) = max_length;
    TextF_Value(w)[max_length] = '\0';
    if (TextF_CursorPos(w) > max_length)
      TextF_CursorPos(w) = max_length;
    MassiveChangeDraw(w);
  }
}

/* ARGSUSED */
void
XmTextFieldSetSelection(Widget aw, XmTextPosition start, XmTextPosition end, Time time)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

  if (end < start) {
    XmTextPosition temp;

    temp = start;
    start = end;
    end = temp;
  }
  if (start < 0)
    start = 0;
  if (end > TextF_Length(w))
    end = TextF_Length(w);
  TextF_HighlightStart(w) = start;
  TextF_HighlightEnd(w) = TextF_CursorPos(w) = end;
  MassiveChangeDraw(w);
}

void
XmTextFieldSetString(Widget aw, char *str)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;
  XmTextPosition len = 0;

  if (!XmIsTextField(w))
    return;

  if (str) {
    len = strlen(str);
    TextF_HighlightStart(w) = 0;
    TextF_HighlightEnd(w) = TextF_Length(w);
    TextDeleteHighlighted(w);
    TextInsert(w, str, len);
    TextF_CursorPos(w) = 0;
    MassiveChangeDraw(w);
  }
  if (TextF_ValueChangedCallback(w)) {
	XmTextVerifyCallbackStruct      cbs;
	XmTextBlockRec                  tb;

	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.currInsert = cbs.newInsert = 0;     /* FIX ME */
	cbs.startPos = cbs.endPos = 0;          /* FIX ME */

	tb.ptr = str;
	tb.length = len;
	tb.format = XmFMT_8_BIT;

	cbs.text = &tb;
	XtCallCallbacks(aw, XmNvalueChangedCallback, &cbs);
  }
}

void
XmTextFieldSetStringWcs(Widget w, wchar_t *wc_value)
{
}

void
XmTextFieldShowPosition(Widget aw, XmTextPosition position)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return;

}

XmTextPosition
XmTextFieldXYToPos(Widget aw, Position x, Position y)
{
  XmTextFieldWidget w = (XmTextFieldWidget) aw;

  if (!XmIsTextField(w))
    return 0;

  return TextPixelToSelectionPos(w, x);
}

