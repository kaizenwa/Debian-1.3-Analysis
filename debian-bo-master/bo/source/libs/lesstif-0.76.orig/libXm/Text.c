/*                   -*-Mode: c; c-file-style: "GNU"-*-              */
/**
 *
 * $Id: Text.c,v 1.22 1997/01/07 02:35:41 miers Exp $
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

static char rcsid[] = "$Id: Text.c,v 1.22 1997/01/07 02:35:41 miers Exp $";

#define	DO_MOSAIC_HACK
#define I18N

#include <LTconfig.h>
#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextOutP.h>
#include <Xm/TransltnsP.h>
#include <Xm/ScrolledW.h>
#include <XmI/XmI.h>
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

#ifdef	DO_MOSAIC_HACK
#include <Xm/TextF.h>
#endif

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

#define _XmMin(a,b)  ((a) < (b)) ? (a) : (b)
#define _XmMax(a,b)  ((a) > (b)) ? (a) : (b)

/* Forward Declarations */

static void ClassInitialize();
static void ClassPartInitialize(WidgetClass class);
static void Initialize(Widget request, Widget new, ArgList args, Cardinal * num_args);
static void Destroy(Widget w);
static void Resize(Widget w);
static void Realize(Widget w, XtValueMask * value_mask, XSetWindowAttributes * attributes);
static void DoExpose(Widget w, XEvent * event, Region region);
static XtGeometryResult QueryGeometry(Widget w, XtWidgetGeometry * proposed, XtWidgetGeometry * answer);
static void _XmTextSetEditable(Widget w, Boolean e);

static Boolean SetValues(Widget current, Widget request, Widget new, ArgList args, Cardinal * num_args);

static void RefigureLines(XmTextWidget w);
static void InitializeLineTable(XmTextWidget w);
static Cardinal GetSecResData(WidgetClass wc, XmSecondaryResourceData ** resdata);
static OutputCreateProc output_create = _XmTextOutputCreate;
static InputCreateProc input_create = _XmTextInputCreate;
void _XmTextInvalidate(XmTextWidget w, XmTextPosition position, 
		       XmTextPosition topos, long delta);
void _XmTextExportValue(Widget w, int offset, XtArgVal *value);

#define Offset(_name) XtOffsetOf(XmTextRec, text._name)

/* Resources for the Text  class */
static XtResource resources[] =
{
  {
    XmNsource, XmCSource, XmRPointer,
    sizeof(XmTextSource), Offset(source),
    XmRPointer, (XtPointer) NULL	/* FIXME: Motif has a wacky value here */
  },
  {
    XmNactivateCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(activate_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNfocusCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(focus_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNlosingFocusCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(losing_focus_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNvalueChangedCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(value_changed_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNmodifyVerifyCallback, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(modify_verify_callback),
    XmRCallback, (XtPointer) NULL
  },
  {
    XmNmodifyVerifyCallbackWcs, XmCCallback, XmRCallback,
    sizeof(XtCallbackList), Offset(wcs_modify_verify_callback),
    XmRCallback, (XtPointer) NULL
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
    XmNvalue, XmCValue, XmRString,
    sizeof(String), Offset(value),
    XmRString, (XtPointer) NULL	/* FIXME: Motif has a wacky value here */
  },
  {
    XmNvalueWcs, XmCValueWcs, XmRValueWcs,
    sizeof(wchar_t *), Offset(wc_value),
    XmRString, (XtPointer) NULL
  },
  {
    XmNmaxLength, XmCMaxLength, XmRInt,
    sizeof(int), Offset(max_length),
    XmRImmediate, (XtPointer) INT_MAX
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
    XmNoutputCreate, XmCOutputCreate, XmRFunction,
    sizeof(OutputCreateProc), Offset(output_create),
    XmRFunction, (XtPointer) &output_create
  },
  {
    XmNinputCreate, XmCInputCreate, XmRFunction,
    sizeof(InputCreateProc), Offset(input_create),
    XmRFunction, (XtPointer) &input_create
  },
  {
    XmNtopCharacter, XmCTopCharacter, XmRTextPosition,
    sizeof(XmTextPosition), Offset(top_character),
    XmRImmediate, (XtPointer) 0
  },
  {
    XmNcursorPosition, XmCCursorPosition, XmRTextPosition,
    sizeof(XmTextPosition), Offset(cursor_position),
    XmRImmediate, (XtPointer) 0
  },
  {
    XmNeditMode, XmCEditMode, XmREditMode,
    sizeof(int), Offset(edit_mode),
    XmRImmediate, (XtPointer) XmSINGLE_LINE_EDIT
  },
  {
    XmNautoShowCursorPosition, XmCAutoShowCursorPosition, XmRBoolean,
    sizeof(Boolean), Offset(auto_show_cursor_position),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNeditable, XmCEditable, XmRBoolean,
    sizeof(Boolean), Offset(editable),
    XmRImmediate, (XtPointer) True
  },
  {
    XmNverifyBell, XmCVerifyBell, XmRBoolean,
    sizeof(Boolean), Offset(verify_bell),
    XtRImmediate, (XtPointer) ((unsigned char) XmUNSPECIFIED)
  },
  {
    XmNnavigationType, XmCNavigationType, XmRNavigationType,
    sizeof(XmNavigationType), XtOffsetOf(XmTextRec, primitive.navigation_type),
    XmRImmediate, (XtPointer) XmTAB_GROUP
  }
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
    _XmTextExportValue, NULL
  }
};
#undef Offset


static char defaultTranslations[] =
"Ctrl<Key>1:    backward-word()\n" \
"Ctrl<Key>2:    backward-paragraph()\n" \
"Ctrl<Key>3:    beginning-of-line()\n" \
"Ctrl<Key>4:    forward-word()\n" \
"Ctrl<Key>6:	end-of-line()\n" \
"Ctrl<Key>5:    forward-paragraph()\n" \
"Ctrl<Key>A:    beginning-of-line()\n" \
"Ctrl<Key>B:    backward-character()\n" \
"Ctrl<Key>C:    paste-clipboard()\n" \
"Ctrl<Key>D:    delete-next-character()\n" \
"Ctrl<Key>E:	end-of-line()\n" \
"Ctrl<Key>F:    forward-character()\n" \
"Ctrl<Key>H:    delete-previous-character()\n" \
"Ctrl<Key>J:	newline-and-indent()\n" \
"Ctrl<Key>K:    kill-to-end-of-line()\n" \
"Ctrl<Key>L:    redraw-display()\n" \
"Ctrl<Key>M:	newline()\n" \
"Ctrl<Key>N:	next-line()\n" \
"Ctrl<Key>O:	newline-and-backup()\n" \
"Ctrl<Key>P:	previous-line()\n" \
"Ctrl<Key>V:	next-page()\n" \
"Ctrl<Key>W:    kill-selection()\n" \
"Meta<Key>B:    backward-word()\n" \
"Meta<Key>F:    forward-word()\n" \
"Meta<Key>V:    previous-page()\n" \
":Meta<Key>d:	delete-next-word()\n" \
":Meta<Key>h:	delete-previous-word()\n" \
"<Key>Left:     backward-character()\n" \
"<Key>Right:    forward-character()\n" \
"<Key>Up:	previous-line()\n" \
"<Key>Down:	next-line()\n" \
"<Key>End:      end-of-line()\n" \
"<Key>Home:     beginning-of-line()\n" \
"<Key>Return:   process-return()\n" \
"<Key>BackSpace: delete-previous-character()\n" \
"<Key>Delete:   delete-next-character()\n" \
"<Key>:         self-insert()\n" \
"<EnterWindow>:	enter-window()\n" \
"<LeaveWindow>:	leave-window()\n" \
"<Key>osfActivate:	activate()\n" \
"<FocusIn>:	focusIn()\n" \
"<FocusOut>:	focusOut()\n" \
"<Btn1Down>:	select-start()\n" \
"<Btn1Motion>:	extend-adjust()\n" \
"<Btn1Up>:	select-end()\n" \
"<Btn2Down>:	copy-primary()\n";

char _XmText_EventBindings1[] =
"<Key>F2:    backward-word()\n" \
"<Key>F3:    backward-paragraph()\n" \
"<Key>F4:    beginning-of-line()\n" \
"<Key>F5:    forward-word()\n" \
"<Key>F6:    forward-paragraph()\n" \
"<Key>F7:	end-of-line()\n" \
"Ctrl<Key>5:    forward-paragraph()\n" \
"Ctrl<Key>A:    beginning-of-line()\n" \
"Ctrl<Key>B:    backward-character()\n" \
"Ctrl<Key>C:    paste-clipboard()\n" \
"Ctrl<Key>D:    delete-next-character()\n" \
"Ctrl<Key>E:	end-of-line()\n" \
"Ctrl<Key>F:    forward-character()\n" \
"Ctrl<Key>H:    delete-previous-character()\n" \
"Ctrl<Key>J:	newline-and-indent()\n" \
"Ctrl<Key>K:    kill-to-end-of-line()\n" \
"Ctrl<Key>L:    redraw-display()\n" \
"Ctrl<Key>M:	newline()\n" \
"Ctrl<Key>N:	next-line()\n" \
"Ctrl<Key>O:	newline-and-backup()\n" \
"Ctrl<Key>P:	previous-line()\n" \
"Ctrl<Key>V:	next-page()\n" \
"Ctrl<Key>W:    kill-selection()\n" \
"Meta<Key>B:    backward-word()\n" \
"Meta<Key>F:    forward-word()\n" \
"Meta<Key>V:    previous-page()\n" \
":Meta<Key>d:	delete-next-word()\n" \
":Meta<Key>h:	delete-previous-word()\n" \
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
s c<Key>osfPageUp:	previous-page(extend)\n\
:c <Key>osfPageUp:	previous-page()\n\
s <Key>osfPageRight:	page-right(extend)\n\
:<Key>osfPageRight:	page-right()\n\
s c <Key>osfPageDown:	next-page(extend)\n\
:c <Key>osfPageDown:	next-page()\n\
:<Key>osfClear:		clear-selection()\n\
:<Key>osfBackSpace:	delete-previous-character()\n\
s m <Key>osfDelete:	cut-primary()\n\
s a <Key>osfDelete:	cut-primary()\n\
s <Key>osfDelete:	cut-clipboard()\n\
:c <Key>osfDelete:	delete-to-end-of-line()\n\
:<Key>osfDelete:	delete-next-character()";

char _XmText_EventBindings2[] =
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
:<Key>osfUp:		previous-line()\n\
:<Key>osfDown:		next-line()\n\
c ~m ~a <Key>slash:	select-all()\n\
c ~m ~a <Key>backslash:	deselect-all()\n\
s ~m ~a <Key>Tab:	prev-tab-group()\n\
~m ~a <Key>Tab:		next-tab-group()\n\
~s ~m ~a <Key>Return:	newline()\n\
c ~s ~m ~a <Key>space:	set-anchor()\n\
c s ~m ~a <Key>space:	key-select()\n\
s ~c ~m ~a <Key>space:	self-insert()\n\
<Key>:			self-insert()";

char _XmText_EventBindings3[] =
"<Unmap>:		unmap()\n\
<Enter>:		enter-window()\n\
<Leave>:		leave-window()\n\
<FocusIn>:		focus-in()\n\
<FocusOut>:		focus-out()\n\
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




static XmBaseClassExtRec textBaseClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook, 
    /* initialize_posthook       */ XmInheritInitializePosthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ GetSecResData,
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ XmInheritGetValuesPrehook,
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
    /* class_part_init_prehook   */ XmInheritClassPartInitPrehook,
    /* class_part_init_posthook  */ XmInheritClassPartInitPosthook,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmTextPrimClassExtRec =
{
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ _XmTextGetBaselines,
    /* widget_display_rect */ _XmTextGetDisplayRect,
    /* widget_margins      */ _XmTextMarginsProc
};

XmTextClassRec xmTextClassRec =
{
    /* Core class part */
  {
	/* superclass            */ (WidgetClass) & xmPrimitiveClassRec,
	/* class_name            */ "XmText",
	/* widget_size           */ sizeof(XmTextRec),
	/* class_initialize      */ ClassInitialize,
	/* class_part_initialize */ ClassPartInitialize,
	/* class_inited          */ FALSE,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ Realize,
	/* actions               */ NULL, /* set in ClassInitialize */
	/* num_actions           */ 0, /* set in ClassInitialize */
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ Destroy,
	/* resize                */ Resize,
	/* expose                */ DoExpose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer) & textBaseClassExtRec
  },
    /* Primitive Class part */
  {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ NULL,
	/* arm_and_activate_proc */ XmInheritArmAndActivate,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer) & _XmTextPrimClassExtRec,
  },
    /* Text Class part */
  {
	/* extension */ NULL
  }
};

WidgetClass xmTextWidgetClass = (WidgetClass) & xmTextClassRec;



/* Inner Widget--------------------------------------------------------------
 * the base for the Output and Input objects
 */




static void
InnerInitialize(Widget request, Widget new, ArgList args, Cardinal * num_args)
{
  XmTextInnerWidget iw = (XmTextInnerWidget) new;
  XmTextWidget w = (XmTextWidget)XtParent(iw);

  DBGW("InnerInitialize");

}

static void
InnerDestroy(Widget w)
{
    /* Destroy Output and Input */
}

static Boolean
InnerSetValues(Widget old, Widget request, Widget new,
  ArgList args, Cardinal * num_args)
{
  Boolean refresh_needed = False;

  XdbDebug(__FILE__, new, "XmTextInner %s SetValues\n", XtName(new));

  return refresh_needed;
}

XmTextInnerClassRec xmTextInnerClassRec =
{
    /* core_class fields */
  {
    /* superclass               */      (WidgetClass) (&objectClassRec),
    /* class_name               */      "XmTextInner",
    /* widget_size              */      sizeof(XmTextInnerRec),
    /* class_initialize         */      NULL,
    /* class_part_initialize    */      NULL,
    /* class_inited             */      FALSE,
    /* initialize               */      InnerInitialize,
    /* initialize_hook          */      NULL,
    /* obj1                     */      NULL,
    /* obj2                     */      NULL,
    /* obj3                     */      0,
    /* resources                */      NULL,
    /* num_resources            */      0,
    /* xrm_class                */      NULLQUARK,
    /* obj4                     */      FALSE,
    /* obj5                     */      FALSE,
    /* obj6                     */      FALSE,
    /* obj7                     */      FALSE,
    /* destroy                  */      InnerDestroy,
    /* obj8                     */      NULL,
    /* obj9                     */      NULL,
    /* set_values               */      InnerSetValues,
    /* set_values_hook          */      NULL,
    /* obj10                    */      NULL,
    /* get_values_hook          */      NULL,
    /* obj11                    */      NULL,
    /* version                  */      XtVersion,
    /* callback_private         */      NULL,
    /* obj12                    */      NULL,
    /* obj13                    */      NULL,
    /* obj14                    */      NULL,
    /* extension                */      NULL
  },
    /* TextInner Class part */
  {
        /* extension */ NULL
  }
};

WidgetClass xmTextInnerObjectClass = (WidgetClass)&xmTextInnerClassRec;


/* Text-----------------------------------------------------------------------
 *
 * Widget methods
 */



extern Boolean _XmCvtStringToXmString(Display *, XrmValue *, Cardinal *, XrmValue *, XrmValue *, XtPointer *);
extern XmFontList _XmFontListCreateDefault(Display *);

static void
ClassInitialize()
{
  int len1 = strlen (_XmTextIn_XmTextEventBindings1);
  int len2 = strlen (_XmTextIn_XmTextEventBindings2);
  int len3 = strlen (_XmTextIn_XmTextEventBindings3);
  char *buf = XtMalloc ((unsigned)(len1 + len2 + len3 + 1));
  char *cp = buf;

  textBaseClassExtRec.record_type = XmQmotif;
  xmTextClassRec.core_class.actions = _XmdefaultTextActionsTable;
  xmTextClassRec.core_class.num_actions = _XmdefaultTextActionsTableSize;
  (void) strcpy( cp, _XmTextIn_XmTextEventBindings1); cp += len1;
  (void) strcpy( cp, _XmTextIn_XmTextEventBindings2); cp += len2;
  (void) strcpy( cp, _XmTextIn_XmTextEventBindings3);
  xmTextClassRec.core_class.tm_table = buf;
}

static void
ClassPartInitialize(WidgetClass widget_class)
{
  _XmFastSubclassInit(widget_class, XmTEXT_BIT);
}

static void
Initialize(Widget request,
  Widget new,
  ArgList args,
  Cardinal * num_args)
{
  XmTextWidget w = (XmTextWidget) new;
#if 0
  XtAugmentTranslations((Widget) w, XtParseTranslationTable(_XmText_EventBindings2));
  XtAugmentTranslations((Widget) w, XtParseTranslationTable(_XmText_EventBindings3));
#endif
  w->text.inner_widget = XtCreateWidget( "inner", xmTextInnerObjectClass,
                                  new, args, *num_args );
  /* initialize the inner to sane values before fetching resources */
  bzero((char *)(w->text.inner_widget) + XtOffsetOf(XmTextInnerRec, inner),
	sizeof(XmTextInnerPart));

  Text_TableSize(w)=Text_TotalLines(w)=0;
  Text_LineTable(w)=NULL;
  Text_Line(w) = NULL;
  Text_LineMax(w) = Text_LineCount(w) = 0;
  Text_FirstPos(w) = Text_LastPos(w) = Text_TopPos(w) = Text_CursorPos(w) = 0;

  (*Text_OutputCreate(w))((Widget)w,args,*num_args);

  (*Text_InputCreate(w))((Widget)w,args,*num_args);
  InitializeLineTable(w);
}

static void
Destroy(Widget w)
{
  (*Text_Output(w)->destroy)(w);
  (*Text_Input(w)->destroy)(w);
}

#define	CHECK_CLASS(w)	XmTextCheckClass(w, __FILE__, __LINE__)

static void
XmTextCheckClass(Widget w, char *f, int l)
{
  if (!XtIsSubclass(w, xmTextWidgetClass)) {
    XdbDebug(__FILE__, w, "Don't take LessTif for a fool !\n");
    XdbDebug(__FILE__, w,
      "XmTextField != XmText. Widget %s misused at %s %d\n",
      XtName(w), f, l);
  }
}

static Boolean
SetValues(Widget old,
  Widget request,
  Widget new,
  ArgList args,
  Cardinal * num_args)
{
  Boolean refresh_needed = False;
  XmTextWidget nw = (XmTextWidget) new;
  XmTextWidget ow = (XmTextWidget) old;

  XdbDebug(__FILE__, new, "XmText %s SetValues\n", XtName(new));

  if (Text_Value(nw) != Text_Value(ow)) {
    XmTextSetString(new, Text_Value(new));
    refresh_needed = True;
  }

  if (Text_Editable(nw) != Text_Editable(ow)) {
    _XmTextSetEditable(new, Text_Editable(nw));
    refresh_needed = True;
  }
  /* FIX ME - lots more cases */

  return refresh_needed;
}

static void
Realize(Widget aw, XtValueMask * value_mask, XSetWindowAttributes * attributes)
{
  XmTextWidget w=(XmTextWidget)aw;
  
#define superclass (&xmPrimitiveClassRec)
  (*superclass->core_class.realize) (aw, value_mask, attributes);
#undef superclass

  RefigureLines(w);
  (*Text_Output(w)->realize)(aw, value_mask, attributes);
  _XmTextSetEditable(aw, Text_Editable(w));
  XmTextShowPosition(aw, Text_CursorPos(w));
}


static void
DoExpose(Widget aw, XEvent * event, Region region)
{
  XmTextWidget w=(XmTextWidget)aw;
  
  if (!XtIsRealized(w))
    return;
  if (Text_NeedsRefigureLines(w))
      RefigureLines(w);
  (*Text_Output(w)->expose)(aw, event, region);
  

}

static void
Resize(Widget aw)
{
  XmTextWidget w = (XmTextWidget) aw;

  DBGW("resize");

/* Resize can get called from XmScrolledWindowSetAreas before we actually made it
   through our initialize method.
   Make sure not to die because of this
 */
  if (Text_Input(w) == NULL || Text_Output(w) == NULL)
	return;

  (*Text_Output(w)->resize)(aw, True);
  RefigureLines(w);
}

static XtGeometryResult
QueryGeometry(Widget w, XtWidgetGeometry * proposed, XtWidgetGeometry * answer)
{
  XmTextWidget tw = (XmTextWidget) w;
  XtWidgetGeometry a;

#define Wants(x)   (proposed->request_mode & x)
  XdbDebug(__FILE__, w, "QueryGeometry proposed width=%d height=%d mode=%X\n",
	   proposed->width, proposed->height, proposed->request_mode);

  if (proposed->request_mode != 0) {	/* NULL case should not yet end here ! */
    if ((!(Wants(CWWidth))) && (!Wants(CWHeight))) {
      /* If they don't ask width/height, let them have whatever they like */
      if (answer)
	*answer = *proposed;
      return XtGeometryYes;
    }
  }

  a.request_mode = CWWidth | CWHeight;
  a.width = XtWidth(tw);
  a.height = XtHeight(tw);

  if (answer)
    *answer = a;

  if (proposed->request_mode == 0)	/* NULL proposed -> return Width+Height */
    return XtGeometryAlmost;

  if (proposed->width >= answer->width && proposed->height >= answer->height)
    return XtGeometryYes;
  else if (answer->width == XtWidth(tw) && answer->height == XtHeight(tw)) {
    if (answer)
      answer->request_mode = 0;
    return XtGeometryNo;
  }
  else
    return XtGeometryAlmost;
}



/* Drawing Routines ------------------------------------------------------- */
static void
Redisplay(XmTextWidget w)
{
  if (! XtIsRealized((Widget)w))
	return;

  Text_NeedsRedisplay(w) = False;
  if (Text_NeedsRefigureLines(w))
    RefigureLines(w);
  (*Text_Output(w)->expose) ((Widget) w, NULL, NULL);
}

static
Cardinal GetSecResData(WidgetClass wc,	XmSecondaryResourceData ** resdata)
{
  return 0;
}

/* Line Table Routines ---------------------------------------------------- */

static void
LineIncrease(XmTextWidget w,LineNum num)
{
  if (num>Text_LineMax(w)) {
    LineNum start=Text_LineMax(w);
    int i;
    
    Text_Line(w)=(Line)XtRealloc((char *)Text_Line(w),sizeof(LineRec)*num);
    Text_LineMax(w)=num;

    for (i=start; i<num; i++) {
      Line line=&Text_Line(w)[i];

      line->extra=NULL;
    }
  }
}

/*
 * Set up the entire line table, starting with the first visible position.
 *
 * Note that there is 1 extra line in the table.  This is so we can find
 * out what the last position in any line is by looking at the first
 * position of the next line.  Oh, the limitations of Motif compatibility!
 */

static void
RefigureLines(XmTextWidget w)
{
  XmTextPosition pos,next;
  LineTableExtra extra;
  Boolean more;
  Line line;
  LineNum count;

  Text_NeedsRefigureLines(w) = False;

  pos=Text_TopPos(w);
  count=0;
  XdbDebug(__FILE__, (Widget) w, "RefigureLines: pos=%d max=%d\n",pos,Text_LineMax(w));
  
  do {
    if (count>=Text_LineMax(w))
      LineIncrease(w,count+16);
    
    line=&Text_Line(w)[count];
    if (line->extra) XtFree((char *)line->extra);
    extra = NULL;
    more = (*Text_Output(w)->MeasureLine)((XmTextWidget)w, count,
      pos, &next, &extra);
    line->start = pos;
    line->changed = False;
    line->changed_position = 0;
    line->past_end = !more;
    line->extra = extra;

    if (more) {
      pos = next;
      count++;
    }
  } while (more);

  if (Text_EditMode(w) == XmSINGLE_LINE_EDIT) {
    count=1;
  }
  Text_TopLine(w) = _XmTextGetTableIndex(w, Text_TopPos(w));
  Text_LineCount(w) = count;
  if (next == PASTENDPOS)
    Text_BottomPos(w) = Text_Line(w)[count].start;
  else
    Text_BottomPos(w) = Text_Line(w)[count].start - 1;
  
  if (XdbInDebug(__FILE__, (Widget)w)) {
    int i;

    for (i=0; i<Text_LineCount(w); i++) {
      Line line=&Text_Line(w)[i];
      
      if (line->extra)
	XdbDebug(__FILE__, (Widget)w,
		 "RefigureLines: line[%d]: start=%d width=%d\n",
		 i, line->start, line->extra->width);
      else
	XdbDebug(__FILE__, (Widget)w,
		 "RefigureLines: line[%d]: start=%d width=NA\n",
		 i, line->start);
    }
  } 
  XdbDebug(__FILE__, (Widget) w,
	   "RefigureLines: bottompos=%d\n", Text_BottomPos(w));
}

static LineNum
LineTableFindNum(XmTextWidget w, XmTextPosition pos)
{
  LineNum i;
  
  if (pos<Text_TopPos(w)) return NOLINE;
  
  for (i=0; i<Text_LineCount(w); i++) {
    Line line=&Text_Line(w)[i];
    Line next=&Text_Line(w)[i+1];

    if (pos>=line->start && pos <next->start) {
      return i;
    }
  }
  
  return NOLINE;
}

/* Line-table functions (for the whole textstring)------------------------ */

static void
InitializeLineTable(XmTextWidget w)
{
#define NR_LINES 16
  XmTextPosition start = Text_FirstPos(w);
  XmTextPosition  end = Text_LastPos(w);
  XmTextLineTable line;
  unsigned int index;
  unsigned int size;

  index = 0;
  size = NR_LINES;
  Text_LineTable(w) = (XmTextLineTable) XtMalloc(sizeof(XmTextLineTableRec) * size);
  while(end >= start) {
    if (index >= size) {
      size += NR_LINES;
      Text_LineTable(w) = (XmTextLineTable) 
	XtRealloc((char *)Text_LineTable(w), sizeof(XmTextLineTableRec) * size);
    }

    line = Text_LineTable(w) + index; index ++;
    line->start_pos = start;
    line->virt_line = False;
    start = (*Text_Source(w)->Scan)(Text_Source(w),start,
				     XmSELECT_LINE,XmsdRight,-1,False) + 1;
  }
  Text_TotalLines(w) = index;
  Text_TableSize(w) = size;
#undef NR_LINES

}
unsigned int 
_XmTextGetTableIndex(XmTextWidget w, XmTextPosition pos)
{
  XmTextLineTable current = Text_LineTable(w) + Text_TotalLines(w) - 1;
  unsigned int i = Text_TotalLines(w) - 1;

  if (Text_LineTable(w) == NULL)
	return 0;
  for(; current->start_pos > pos; current--, i--);
  XdbDebug(__FILE__, (Widget) w, "XmTextGetTableIndex Pos %d Index %d\n", pos, i);
  return i;
}

void
_XmTextUpdateLineTable(Widget w,
			    XmTextPosition start,
			    XmTextPosition end,
			    XmTextBlock block,
			    Boolean update)
{
#define NR_LINES 16
  XmTextWidget tw = (XmTextWidget) w;
  unsigned int index;
  unsigned int size = Text_TableSize(w);
  XmTextLineTable line;
  Text_NeedsRefigureLines(w) = True;
  Text_NeedsRedisplay(w) = True;
  index = _XmTextGetTableIndex(tw, start);
  start = Text_LineTable(tw)[index].start_pos;
  end = Text_LastPos(w);
  while(end >= start) {
    if (index >= size) {
      size += NR_LINES;
      Text_LineTable(w) = (XmTextLineTable) 
	XtRealloc((char *)Text_LineTable(w), sizeof(XmTextLineTableRec) * size);
    }

    line = Text_LineTable(w) + index; index ++;
    line->start_pos = start;
    line->virt_line = False;
    start = (*Text_Source(w)->Scan)(Text_Source(w), start,
				     XmSELECT_LINE, XmsdRight, -1, False) + 1;
  }
  Text_TotalLines(w) = index;
  Text_TableSize(w) = size;
#undef NR_LINES
}


/* Quasi-Public Functions ------------------------------------------------ */
void
_XmTextLineInfo(XmTextWidget w, LineNum line,
		XmTextPosition *startpos, LineTableExtra *extra)
{
  *startpos = Text_Line(w)[line].start;
  *extra =Text_Line(w)[line].extra;
}

void
_XmTextMarkRedraw(XmTextWidget w, XmTextPosition left,
		   XmTextPosition right)
{
}

LineNum
_XmTextPosToLine(XmTextWidget w, XmTextPosition position)
{
  return _XmTextGetTableIndex(w, position) + 1;
}

void
_XmTextInsert(Widget w, XmTextPosition position, char *string, XEvent * evp)
{
	XmTextStatus	st;
	XmTextWidget	tw = (XmTextWidget)w;
	XmTextBlockRec	block;
	XmTextPosition	startret, endret;

	CHECK_CLASS(w);
	XdbDebug(__FILE__, w, "_XmTextInsert\n");

	if (string == NULL)
		return;		/* Can I do this ? FIX ME */

	block.ptr = string;
	block.length = strlen(string);
	block.format = XmFMT_8_BIT;

	startret = position;
	endret = position;

	st = (*Text_Source(w)->Replace)(tw,
		/* event */			evp,
		/* return start position */	&startret,
		/* return end position */	&endret,
		/* text block */		&block,
		/* call callback ? */		False);	/* FIX ME ?? */

	RefigureLines(tw);
	if (XtIsRealized(w))
		Redisplay(tw);
}

void
_XmTextSetCursorPosition(Widget w, XmTextPosition position)
{
  XmTextWidget tw = (XmTextWidget) w;
  XmTextVerifyCallbackStruct cbs;
  cbs.doit = True;
  cbs.newInsert = position;

  XdbDebug(__FILE__, (Widget) w, "_XmTextSetCursorPosition Pos %d \n",
	   position);
  if (Text_CursorPos(w) != position) {
    if (Text_MotionVerifyCallback(w)) {
      cbs.reason = XmCR_MOVING_INSERT_CURSOR;
      cbs.event = NULL;
      cbs.currInsert = Text_CursorPos(w);
      cbs.startPos = cbs.endPos = 0;
      cbs.text = NULL;

      XtCallCallbacks(w, XmNmotionVerifyCallback, &cbs);
    }
  }
  if (cbs.doit) {
    (*Text_Output(w)->DrawInsertionPoint)(tw, Text_CursorPos(w), off); 
    Text_CursorPos(w)=cbs.newInsert;
    if (Text_AutoShowCursorPosition(w))
      XmTextShowPosition(w, Text_CursorPos(w));
    else 
      _XmTextMovingCursorPosition(tw, Text_CursorPos(w));
    (*Text_Output(w)->DrawInsertionPoint)(tw, Text_CursorPos(w), on); 
  }
}


static void
_XmTextSetEditable(Widget w, Boolean e)
{
  if (!XtIsRealized(w))
    return;

#ifdef  I18N
  if (e) {			/* becomes editable */
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
  _XmStringSourceSetEditable(Text_Source(w),e);
}
#endif

void
_XmTextDisableRedisplay(XmTextWidget w, Boolean losesbackingstore)
{
  Text_DisableDepth(w)++;
}

void
_XmTextEnableRedisplay(XmTextWidget w)
{
  Text_DisableDepth(w)--;
  if (Text_DisableDepth(w) == 0)
    if (Text_NeedsRedisplay(w))
	Redisplay(w);
}

void
_XmTextInvalidate(XmTextWidget w, XmTextPosition position, 
		       XmTextPosition topos, long delta)
{
  int i;
  for (i = 0; i < Text_LineCount(w) && Text_Line(w)[i].start <= position; i++);
  if (i >= Text_LineCount(w))
    return;
  Text_Line(w)[i - 1].changed = True;
  Text_Line(w)[i - 1].changed_position = position;
  (*Text_Output(w)->Invalidate) (w, position, topos, delta);
  (*Text_Input(w)->Invalidate) (w, position, topos, delta);
}
/* Public Functions ------------------------------------------------------ */

Widget
XmCreateText(Widget parent,char *name,Arg * arglist,Cardinal argCount)
{
  return XtCreateWidget(name, xmTextWidgetClass, parent, arglist, argCount);
}

Widget
XmCreateScrolledText(Widget parent, char *name,Arg * arglist,Cardinal argcount)
{
  Widget sw, w;
  char *sname;
  int i;
  Arg *al;

  if (name == NULL) name = "";
  sname = XtMalloc(strlen(name) + 3);
  strcpy(sname, name);
  strcat(sname, "SW");
  al = (Arg *)XtCalloc(argcount + 4, sizeof(Arg));
  for (i=0; i < argcount; i++)
    {
      al[i].name = arglist[i].name;
      al[i].value = arglist[i].value;
    }
  
  XtSetArg(al[i], XmNscrollingPolicy, XmAPPLICATION_DEFINED); i++;
  XtSetArg(al[i], XmNvisualPolicy, XmVARIABLE); i++;
  XtSetArg(al[i], XmNscrollBarDisplayPolicy, XmSTATIC); i++;
  XtSetArg(al[i], XmNshadowThickness, 0); i++;
  
  sw = XtCreateManagedWidget(sname, xmScrolledWindowWidgetClass, parent, 
			     al, i);
  XtFree((XtPointer)sname);

  i=argcount;
  XtSetArg(al[i], XmNeditMode, XmMULTI_LINE_EDIT); i++;

  w = XtCreateWidget(name, xmTextWidgetClass, sw, al, i);
  XtAddCallback(w, XmNdestroyCallback,
		    _XmDestroyParentCallback, 
		    (XtPointer)w);
  
  XtFree((XtPointer)al);

  return w;
}

void
XmTextClearSelection(Widget w, Time time)
{
}

Boolean
XmTextCopy(Widget w, Time time)
{
  return False;
}

Boolean
XmTextCut(Widget w, Time time)
{
  return False;
}

Boolean
XmTextFindString(Widget w, XmTextPosition start, char *string,
  XmTextDirection direction, XmTextPosition * position)
{
  return False;
}

int
XmTextGetBaseline(Widget w)
{
  return 0;
}


XmTextPosition
XmTextGetCursorPosition(Widget w)
{
  return Text_CursorPos(w);
}

Boolean
XmTextGetEditable(Widget w)
{
  return Text_Editable(w);
}

XmTextPosition
XmTextGetInsertionPosition(Widget w)
{
  return Text_CursorPos(w);
}

XmTextPosition
XmTextGetLastPosition(Widget w)
{
  return Text_LastPos(w);
}

int
XmTextGetMaxLength(Widget w)
{
  return _XmStringSourceGetMaxLength(Text_Source(w));
}

char *
XmTextGetSelection(Widget w)
{
  return NULL;
}

Boolean
XmTextGetSelectionPosition(Widget w, XmTextPosition * left, XmTextPosition * right)
{
  return False;
}

wchar_t *
XmTextGetSelectionWcs(Widget w)
{
  return NULL;
}


char *
XmTextGetString(Widget w)
{
  CHECK_CLASS(w);

  if (XmIsText(w))
        return _XmStringSourceGetValue(Text_Source(w),False);
  else if (XmIsTextField(w))
        return XmTextFieldGetString(w);
  else
        return NULL;
}

wchar_t *
XmTextGetStringWcs(Widget w)
{
  return (wchar_t *)_XmStringSourceGetValue(Text_Source(w),True);
}

/*
 * XmTextGetSubstring()
 * 
 * Gets a substring of the text 
 */
int
XmTextGetSubstring(Widget w, XmTextPosition start, int num_chars, int buffer_size, char *buffer)
{
  int len;
  int retval = XmCOPY_SUCCEEDED;

  if (num_chars > buffer_size)
    len = num_chars;
  else {
    len = buffer_size - 1;
    retval = XmCOPY_TRUNCATED;
  }
/* FIXME */
  
  return retval;
}

int
XmTextGetSubstringWcs(Widget w, XmTextPosition start, int num_chars, int buffer_size, wchar_t * buffer)
{
  return 0;
}

XmTextPosition
XmTextGetTopCharacter(Widget w)
{
  return Text_TopPos(w);
}

void
XmTextInsert(Widget w, XmTextPosition position, char *string)
{
  CHECK_CLASS(w);
  _XmTextInsert(w, position, string, NULL);
}


void
XmTextInsertWcs(Widget w, XmTextPosition position, wchar_t * wcstring)
{
}

Boolean
XmTextPaste(Widget w)
{
  return False;
}

Boolean
XmTextPosToXY(Widget w, XmTextPosition position, Position * x, Position * y)
{
  CHECK_CLASS(w);
  return (*Text_Output(w)->PosToXY)((XmTextWidget) w, position, x, y);
}

Boolean
XmTextRemove(Widget w)
{
  return False;
}

void
XmTextReplace(Widget w, XmTextPosition from_pos, XmTextPosition to_pos, char *value)
{
	XmTextStatus	st;
	XmTextWidget	tw = (XmTextWidget)w;
	XmTextBlockRec	block;
	XmTextPosition	startret, endret;

	CHECK_CLASS(w);
	XdbDebug(__FILE__, w, "XmTextReplace\n");

	block.ptr = value;
	block.length = value ? strlen(value) : 0;
	block.format = XmFMT_8_BIT;

	startret = from_pos;
	endret = to_pos;

	st = (*Text_Source(w)->Replace)(tw,
		/* event */			NULL,
		/* return start position */	&startret,
		/* return end position */	&endret,
		/* text block */		&block,
		/* call callback ? */		False);	/* FIX ME ?? */

	RefigureLines(tw);
	if (XtIsRealized(w))
		Redisplay(tw);
}

void
XmTextReplaceWcs(Widget w, XmTextPosition from_pos, XmTextPosition to_pos, wchar_t * wcstring)
{
	XdbDebug(__FILE__, w, "XmTextReplaceWcs is not implemented\n");
}

void
XmTextSetAddMode(Widget w, Boolean state)
{
}

void
XmTextSetCursorPosition(Widget w, XmTextPosition position)
{
  _XmTextSetCursorPosition(w, position);
}


void
XmTextSetEditable(Widget w, Boolean editable)
{
  if (!XmIsText(w))
    return;

  if (Text_Editable(w) != editable)
    _XmTextSetEditable(w, editable);

  Text_Editable(w) = editable;
}

void
XmTextSetHighlight(Widget w, XmTextPosition left, XmTextPosition right, XmHighlightMode mode)
{
}

void
XmTextSetInsertionPosition(Widget w, XmTextPosition position)
{
    if (XtIsSubclass(w, xmTextFieldWidgetClass))
	XmTextFieldSetCursorPosition(w, position);
    else if (XtIsSubclass(w, xmTextWidgetClass))
	_XmTextSetCursorPosition(w, position);
    else
	abort();
}

void
XmTextSetMaxLength(Widget w, int max_length)
{
  CHECK_CLASS(w);

  _XmStringSourceSetMaxLength(Text_Source(w),max_length);
}

void
XmTextSetSelection(Widget w, XmTextPosition first, XmTextPosition last, Time time)
{
}

void
XmTextSetString(Widget w, char *value)
{
  XmTextWidget	tw = (XmTextWidget)w;

  CHECK_CLASS(w);

#ifdef	DO_MOSAIC_HACK
/* This is a hack for Mosaic to work ! */
  if (XtIsSubclass(w, xmTextFieldWidgetClass))
    XmTextFieldSetString(w, value);
  else
#endif
  {
	_XmStringSourceSetValue(tw, value);
	Text_LastPos(w) = (value ? strlen(value) : 0);
	InitializeLineTable(tw);
	RefigureLines(tw);
	(*Text_Output(tw)->Invalidate) (tw, 0, 0, 0);
	if (XtIsRealized(w))
		Redisplay(tw);
  }
  
}

void
XmTextDisableRedisplay(Widget w)
{
  XdbDebug(__FILE__, w, "XmTextDisableRedisplay\n");
}

void
XmTextEnableRedisplay(Widget w)
{
  XdbDebug(__FILE__, w, "XmTextEnableRedisplay\n");
}


XmTextSource
XmTextGetSource(Widget w)
{
  XdbDebug(__FILE__, w, "XmTextGetSource\n");

  return Text_Source(w);
}

void
XmTextSetSource(Widget w, XmTextSource s, XmTextPosition top, XmTextPosition curs)
{
  XdbDebug(__FILE__, w, "XmTextSetSource\n");
}

void
XmTextScroll(Widget aw, int n)
{
  XmTextWidget w = (XmTextWidget) aw;
  int top_index, index;
  top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
  if (n < 0 )
    index = _XmMax(n + top_index, 0);
  else
    index = _XmMin(n + top_index, Text_TotalLines(w) - 1) - Text_LineCount(w) + 1;
  XdbDebug(__FILE__, aw, "XmTextScroll index=%d n=%d\n", index, n);
  XmTextSetTopCharacter(aw, Text_LineTable(w)[index].start_pos);
}

void
XmTextSetStringWcs(Widget w, wchar_t * wcstring)
{
}

void
XmTextSetTopCharacter(Widget w, XmTextPosition top_character)
{
  XdbDebug(__FILE__, w, "XmTextSetTopCharacter top_character=%d\n",
	   top_character);
  if (Text_EditMode(w) == XmSINGLE_LINE_EDIT) {
    /* Scroll horizontally until top_character is leftmost character */
  }
  else {
    Text_TopPos(w) = top_character;
    RefigureLines((XmTextWidget) w);
  }
}
void
XmTextShowPosition(Widget aw, XmTextPosition position)
{
  XmTextWidget w = (XmTextWidget) aw;
  unsigned int top_index, index, bottom_index;

  XdbDebug(__FILE__, aw, "XmTextShowPosition pos=%d\n", position);
  if (XtIsSubclass(aw, xmTextFieldWidgetClass))
    XmTextFieldShowPosition(aw, position);
  else if  (XtIsSubclass(aw, xmTextWidgetClass)) {
    bottom_index = _XmTextGetTableIndex(w, Text_BottomPos(w));
    top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
    index = _XmTextGetTableIndex(w, position);
    if (index < top_index)
      ;
    else if (bottom_index < index) 
	index += top_index - bottom_index;
    else
      index = top_index;
    Text_TopPos(w) = Text_LineTable(w)[index].start_pos; 
    (*Text_Output(w)->MakePositionVisible)(w, position);
    if (Text_NeedsRedisplay(w))
      Redisplay(w);
  }
}

XmTextPosition
XmTextXYToPos(Widget w, Position x, Position y)
{
  XdbDebug(__FILE__, w, "XmTextXYToPos x=%d y=%d\n", x, y);
  if (XtIsSubclass(w, xmTextFieldWidgetClass))
    return XmTextFieldXYToPos(w, x, y);
  else if  (XtIsSubclass(w, xmTextWidgetClass))
    return (*Text_Output(w)->XYToPos)((XmTextWidget) w, x, y);
  else return 0;
}


void
_XmTextExportValue(Widget w, int offset, XtArgVal *value)
{
    *value = (XtArgVal)XmTextGetString(w);
    XdbDebug(__FILE__, w, "_XmTextExportValue: value '%s'\n", (char *)*value);
}
