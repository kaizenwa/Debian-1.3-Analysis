/**
 *
 * $Id: MacrosI.h,v 1.8 1996/12/18 07:24:58 u27113 Exp $
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

#ifndef MACROS_I_H
#define MACROS_I_H

/*
 * Core shorthand
 */
#define CoreBeingDestroyed(w) \
    (((Widget)(w))->core.being_destroyed)

#define CoreConstraints(w) \
    (((Widget)(w))->core.constraints)

/*
 * Shell shorthand
 */
#define	Shell_PopdownCallback(x) \
    (((ShellWidget)(x))->shell.popdown_callback)

/*
 * one that Motif should have but doesn't
 */
#define XmIsMenuBar(w) \
    (XtIsSubclass(w, xmRowColumnWidgetClass) && RC_Type(w) == XmMENU_BAR)

/*
 * In the first set of macros, we have changed the names from what Motif has.
 */
/*
 * LABEL GADGET OVERRIDE MACROS
 */
#define LabG_AcceleratorText	LabG__acceleratorText
#define LabG_Label		LabG__label

/* 
 * PUSH BUTTON GADGET OVERRIDE MACROS
 */
#define PBG_DefaultButtonShadow PBG_DefaultButtonShadowThickness
#define PBG_FillGC		PBG_FillGc
#define PBG_BackgroundGC	PBG_BackgroundGc

/*
 * TOGGLE BUTTON GADGET OVERRIDE MACROS
 */
#define TBG_ArmCallback			TBG_ArmCB
#define TBG_ValueChangedCallback	TBG_ValueChangedCB
#define TBG_DisarmCallback		TBG_DisarmCB

/*
 * In the second set, these are macros that we have that Motif doesn't.
 */

/*
 * PRIMITIVE ACCESS MACROS
 */
#define Prim_Foreground(w) \
    (((XmPrimitiveWidget)(w))->primitive.foreground)

#define Prim_HighlightThickness(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_thickness)

#define Prim_TopShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_GC)

#define Prim_TopShadowColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_color)

#define Prim_TopShadowPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_pixmap)

#define Prim_BottomShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_GC)

#define Prim_BottomShadowColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_color)

#define Prim_BottomShadowPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_pixmap)

#define Prim_HighlightGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_GC)

#define Prim_HighlightColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_color)

#define Prim_HighlightPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_pixmap)

#define Prim_HighlightOnEnter(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_on_enter)

#define Prim_HighlightDrawn(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_drawn)

#define Prim_Highlighted(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlighted)

#define Prim_TraversalOn(w) \
    (((XmPrimitiveWidget)(w))->primitive.traversal_on)

#define Prim_NavigationType(w) \
    (((XmPrimitiveWidget)(w))->primitive.navigation_type)

#define Prim_UnitType(w) \
    (((XmPrimitiveWidget)(w))->primitive.unit_type)

/*
 * MANAGER/GADGET ACCESS MACROS
 */
#define XmParentForeground(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.foreground)

#define XmParentBackground(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->core.background_pixel)

#define XmParentBottomShadowColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.bottom_shadow_color)

#define XmParentHighlightColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.highlight_color)

#define XmParentTopShadowColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.top_shadow_color)

/*
 * MANAGER ACCESS MACROS
 */
#define MGR_InitialFocus(w) \
        (((XmManagerWidget)(w))->manager.initial_focus)
    
#define MGR_BackgroundGC(m) \
	(((XmManagerWidget)(m))->manager.background_GC)

#define MGR_Foreground(m) \
	(((XmManagerWidget)(m))->manager.foreground)

#define MGR_TopShadowGC(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_GC)

#define MGR_TopShadowColor(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_color)

#define MGR_TopShadowPixmap(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_pixmap)

#define MGR_BottomShadowGC(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_GC)

#define MGR_BottomShadowColor(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_color)

#define MGR_BottomShadowPixmap(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_pixmap)

#define MGR_HighlightGC(m) \
	(((XmManagerWidget)(m))->manager.highlight_GC)

#define MGR_HighlightColor(m) \
	(((XmManagerWidget)(m))->manager.highlight_color)

#define MGR_HighlightPixmap(m) \
	(((XmManagerWidget)(m))->manager.highlight_pixmap)

#define MGR_HelpCallbackList(m) \
	(((XmManagerWidget)(m))->manager.help_callback)

#define MGR_EventHandlerAdded(m) \
	(((XmManagerWidget)(m))->manager.event_handler_added)

#define MGR_HighlightedWidget(m) \
	(((XmManagerWidget)(m))->manager.highlighted_widget)

#define MGR_ActiveChild(m) \
	(((XmManagerWidget)(m))->manager.active_child)

#define MGR_SelectedGadget(m) \
	(((XmManagerWidget)(m))->manager.selected_gadget)

#define MGR_NavigationType(m) \
	(((XmManagerWidget)(m))->manager.navigation_type)

#define MGR_TraversalOn(m) \
	(((XmManagerWidget)(m))->manager.traversal_on)

#define MGR_UnitType(m) \
	(((XmManagerWidget)(m))->manager.unit_type)

#define MGR_StringDirection(m) \
	(((XmManagerWidget)(m))->manager.string_direction)

/* a couple of #defines to make composite stuff easier */

#define MGR_NumChildren(m) \
	(((XmManagerWidget)(m))->composite.num_children)

#define MGR_Children(m) \
	(((XmManagerWidget)(m))->composite.children)

/*
 * GADGET ACCESS MACROS
 */
#define G_HelpCallback(g) \
       (((XmGadget)(g))->gadget.help_callback)

#define G_UserData(g) \
       (((XmGadget)(g))->gadget.have_traversal)

#define G_TraversalOn(g) \
       (((XmGadget)(g))->gadget.traversal_on)

#define G_HighlightOnEnter(g) \
       (((XmGadget)(g))->gadget.highlight_on_enter)

#define G_HaveTraversal(g) \
       (((XmGadget)(g))->gadget.have_traversal)

#define G_UnitType(g) \
       (((XmGadget)(g))->gadget.unit_type)

#define G_NavigationType(g) \
       (((XmGadget)(g))->gadget.navigation_type)

#define G_HighlightDrawn(g) \
       (((XmGadget)(g))->gadget.highlight_drawn)

#define G_Highlighted(g) \
       (((XmGadget)(g))->gadget.highlighted)

#define G_Visible(g) \
       (((XmGadget)(g))->gadget.visible)

#define G_EventMask(g) \
       (((XmGadget)(g))->gadget.event_mask)

/*
 * EXT OBJECT ACCESS MACROS
 */
#define ExtObj_LogicalParent(w) \
    (((XmExtRec *)w)->ext.logicalParent)

#define ExtObj_ExtensionType(w) \
    (((XmExtRec *)w)->ext.extensionType)

/*
 * VENDOR SHELL EXT ACCESS MACROS
 */
#define VSEP_DefaultFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.default_font_list)

#define VSEP_FocusPolicy(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_policy)

#define VSEP_FocusData(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_data)

#define VSEP_DeleteResponse(o) \
	(((XmVendorShellExtObject)o)->vendor.delete_response)

#define VSEP_UnitType(o) \
	(((XmVendorShellExtObject)o)->vendor.unit_type)

#define VSEP_MwmHints(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_hints)

#define VSEP_MwmInfo(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_info)

#define VSEP_MwmMenu(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_menu)

#define VSEP_FocusMovedCallback(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_moved_callback)

#define VSEP_OldManaged(o) \
	(((XmVendorShellExtObject)o)->vendor.old_managed)

#define VSEP_XAtMap(o) \
	(((XmVendorShellExtObject)o)->vendor.xAtMap)

#define VSEP_YAtMap(o) \
	(((XmVendorShellExtObject)o)->vendor.yAtMap)

#define VSEP_XOffset(o) \
	(((XmVendorShellExtObject)o)->vendor.xOffset)

#define VSEP_YOffset(o) \
	(((XmVendorShellExtObject)o)->vendor.yOffset)

#define VSEP_LastOffsetSerial(o) \
	(((XmVendorShellExtObject)o)->vendor.lastOffsetSerial)

#define VSEP_LastMapRequest(o) \
	(((XmVendorShellExtObject)o)->vendor.lastMapRequest)

#define VSEP_ExternalReposition(o) \
	(((XmVendorShellExtObject)o)->vendor.externalReposition)

#define VSEP_MapStyle(o) \
	(((XmVendorShellExtObject)o)->vendor.mapStyle)

#define VSEP_RealizeCallback(o) \
	(((XmVendorShellExtObject)o)->vendor.realize_callback)

#define VSEP_GrabKind(o) \
	(((XmVendorShellExtObject)o)->vendor.grab_kind)

#define VSEP_AudibleWarning(o) \
	(((XmVendorShellExtObject)o)->vendor.audible_warning)

#define VSEP_LabelFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.label_font_list)

#define VSEP_ButtonFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.button_font_list)

#define VSEP_TextFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.text_font_list)

#define VSEP_InputMethodString(o) \
	(((XmVendorShellExtObject)o)->vendor.input_method_string)

#define VSEP_PreeditTypeString(o) \
	(((XmVendorShellExtObject)o)->vendor.preedit_type_string)

#define VSEP_LightThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.light_threshold)

#define VSEP_DarkThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.dark_threshold)

#define VSEP_ForegroundThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.foreground_threshold)

#define VSEP_ImHeight(o) \
	(((XmVendorShellExtObject)o)->vendor.im_height)

#define VSEP_ImInfo(o) \
	(((XmVendorShellExtObject)o)->vendor.im_info)

#define VSEP_ImHeightSet(o) \
	(((XmVendorShellExtObject)o)->vendor.im_vs_height_set)

/*
 * ARROW BUTTON WIDGET ACCESS MACROS
 */
#define AB_ArmCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.arm_callback)

#define AB_ActivateCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.activate_callback)

#define AB_DisarmCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.disarm_callback)

#define AB_ArrowGC(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.arrow_GC)

#define AB_InsensitiveGC(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.insensitive_GC)

#define AB_Direction(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.direction)

#define AB_Armed(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.selected)

#define AB_Timer(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.timer)

#define AB_MultiClick(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.multiClick)

#define AB_ClickCount(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.click_count)

#define AB_ArmTimeStamp(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.armTimeStamp)

/*
 * ARROW BUTTON GADGET ACCESS MACROS
 */
#define ABG_ActivateCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.activate_callback)

#define ABG_ArmCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.arm_callback)

#define ABG_DisarmCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.disarm_callback)

#define ABG_ArrowGC(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.arrow_GC)

#define ABG_InsensitiveGC(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.insensitive_GC)

#define ABG_Direction(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.direction)

#define ABG_Armed(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.selected)

#define ABG_Timer(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.timer)

#define ABG_MultiClick(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.multiClick)

#define ABG_ClickCount(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.click_count)

#define ABG_ArmTimeStamp(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.armTimeStamp)

/*
 * BULLETINBOARD WIDGET ACCESS MACROS
 */
#define BB_OldWidth(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_width)

#define BB_OldHeight(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_height)

#define BB_OldShadowThickness(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_shadow_thickness)

#define BB_AutoUnmanage(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.auto_unmanage)

#define BB_FocusCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.focus_callback)

#define BB_MapCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.map_callback)

#define BB_UnmapCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.unmap_callback)

#define BB_DefaultPosition(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.default_position)

#define BB_ShadowType(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.shadow_type)

#define BB_NoResize(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.no_resize)

#define BB_DialogStyle(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dialog_style)

#define BB_DialogTitle(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dialog_title)

#define BB_AllowOverlap(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.allow_overlap)

#define BB_GeoCache(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.geo_cache)

/*
 * COMMAND WIDGET ACCESS MACROS
 */
#define Com_Command(w) \
     (((XmCommandRec *)(w))->selection_box.selection_label_string)

#define Com_HistoryItems(w) \
     (((XmCommandRec *)(w))->selection_box.list_items)

#define Com_HistoryItemCount(w) \
     (((XmCommandRec *)(w))->selection_box.list_item_count)

#define Com_HistoryMaxItems(w) \
     (((XmCommandRec *)(w))->command.history_max_items)

#define Com_HistoryVisibleItemCount(w) \
     (((XmCommandRec *)(w))->selection_box.list_visible_item_count)

#define Com_PromptString(w) \
     (((XmCommandRec *)(w))->selection_box.selection_label_string)

#define	Com_Error(w)	\
     (((XmCommandRec *)(w))->command.error)

/*
 * CASCADE BUTTON WIDGET ACCESS MACROS
 */
#define CB_Timer(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.timer)

#define CB_MapDelay(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.map_delay)

/*
 * DRAWING AREA WIDGET ACCESS MACROS
 */
#define DA_MarginWidth(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.margin_width)

#define DA_MarginHeight(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.margin_height)

#define DA_ResizePolicy(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.resize_policy)

#define DA_ExposeCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.expose_callback)

#define DA_InputCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.input_callback)

#define DA_ResizeCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.resize_callback)

/*
 * DRAWN BUTTON ACCESS MACROS
 */
#define DB_PushButtonEnabled(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.pushbutton_enabled)

#define DB_ShadowType(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.shadow_type)

#define DB_ActivateCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.activate_callback)

#define DB_ArmCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.arm_callback)

#define DB_DisarmCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.disarm_callback)

#define DB_ExposeCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.expose_callback)

#define DB_ResizeCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.resize_callback)

#define DB_Armed(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.armed)

#define DB_OldWidth(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_width)

#define DB_OldHeight(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_height)

#define DB_OldShadowThickness(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_shadow_thickness)

#define DB_OldHighlightThickness(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_highlight_thickness)

#define DB_Timer(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.timer)

#define DB_MultiClick(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.multiClick)

#define DB_ClickCount(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.click_count)

#define DB_ArmTimeStamp(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.armTimeStamp)

/*
 * FILE SELECTIONBOX WIDGET ACCESS MACROS
 */
#define FS_DirSpec(w) \
    SB_TextString(w)

#define FS_FileListLabelString(w) \
    SB_ListLabelString(w)

/*
 * FRAME WIDGET ACCESS MACROS
 */
#define Frame_MarginWidth(w) \
    (((XmFrameWidget)w)->frame.margin_width)

#define Frame_MarginHeight(w) \
    (((XmFrameWidget)w)->frame.margin_height)

#define Frame_ShadowType(w) \
    (((XmFrameWidget)w)->frame.shadow_type)

#define Frame_OldWidth(w) \
    (((XmFrameWidget)w)->frame.old_width)

#define Frame_OldHeight(w) \
    (((XmFrameWidget)w)->frame.old_height)

#define Frame_OldShadowThickness(w) \
    (((XmFrameWidget)w)->frame.old_shadow_thickness)

#define Frame_OldShadowX(w) \
    (((XmFrameWidget)w)->frame.old_shadow_x)

#define Frame_OldShadowY(w) \
    (((XmFrameWidget)w)->frame.old_shadow_y)

#define Frame_WorkArea(w) \
    (((XmFrameWidget)w)->frame.work_area)

#define Frame_TitleArea(w) \
    (((XmFrameWidget)w)->frame.title_area)

#define Frame_ProcessingConstraints(w) \
    (((XmFrameWidget)w)->frame.processing_constraints)

/* constraint macros */
#define FrameC_ChildType(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_type)

#define FrameC_ChildHAlignment(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_h_alignment)

#define FrameC_ChildHSpacing(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_h_spacing)

#define FrameC_ChildVAlignment(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_v_alignment)

/*
 * LABEL WIDGET ACCESS MACROS
 */
#define Lab_RecomputeSize(w) \
       (((XmLabelWidget)(w))->label.recompute_size)

#define Lab_Label(w) \
       (((XmLabelWidget)(w))->label._label)

#define Lab_MnemonicCharset(w) \
       (((XmLabelWidget)(w))->label.mnemonicCharset)

#define Lab_Alignment(w) \
       (((XmLabelWidget)(w))->label.alignment)

#define Lab_Pixmap(w) \
       (((XmLabelWidget)(w))->label.pixmap)

#define Lab_PixmapInsensitive(w) \
       (((XmLabelWidget)(w))->label.pixmap_insen)

#define Lab_LabelType(w) \
       (((XmLabelWidget)(w))->label.label_type)

#define Lab_NormalGC(w) \
       (((XmLabelWidget)(w))->label.normal_GC)

#define Lab_InsensitiveGC(w) \
       (((XmLabelWidget)(w))->label.insensitive_GC)

#define Lab_StringDirection(w) \
       (((XmLabelWidget)(w))->label.string_direction)

#define Lab_AccTextRect(w) \
       (((XmLabelWidget)(w))->label.acc_TextRect)

#define Lab_SkipCallback(w) \
       (((XmLabelWidget)(w))->label.skipCallback)

/*
 * LIST WIDGET ACCESS MACROS
 */
#define List_InternalList(w) \
    (((XmListWidget)(w))->list.InternalList)

#define List_Event(w) \
    (((XmListWidget)(w))->list.Event)

#define List_LastItem(w) \
    (((XmListWidget)(w))->list.LastItem)

#define List_FontHeight(w) \
    (((XmListWidget)(w))->list.FontHeight)

#define List_AutoSelect(w) \
    (((XmListWidget)(w))->list.AutoSelect)

#define List_DidSelection(w) \
    (((XmListWidget)(w))->list.DidSelection)

#define List_FromSetSB(w) \
    (((XmListWidget)(w))->list.FromSetSB)

#define List_FromSetNewSize(w) \
    (((XmListWidget)(w))->list.FromSetNewSize)

#define List_LeaveDir(w) \
    (((XmListWidget)(w))->list.LeaveDir)

#define List_BrowseCallback(w) \
    (((XmListWidget)(w))->list.BrowseCallback)

#define List_ExtendCallback(w) \
    (((XmListWidget)(w))->list.ExtendCallback)

#define List_DefaultCallback(w) \
    (((XmListWidget)(w))->list.DefaultCallback)

#define List_ClickInterval(w) \
    (((XmListWidget)(w))->list.ClickInterval)

#define List_DragID(w) \
    (((XmListWidget)(w))->list.DragID)

#define List_Font(w) \
    (((XmListWidget)(w))->list.font)

#define List_ItemCount(w) \
    (((XmListWidget)(w))->list.itemCount)

#define List_SelectedItemCount(w) \
    (((XmListWidget)(w))->list.selectedItemCount)

#define List_VisibleItemCount(w) \
    (((XmListWidget)(w))->list.visibleItemCount)

#define List_LastSetVizCount(w) \
    (((XmListWidget)(w))->list.LastSetVizCount)

#define List_Items(w) \
    (((XmListWidget)(w))->list.items)

#define List_SelectedItems(w) \
    (((XmListWidget)(w))->list.selectedItems)

#define List_SelectedIndices(w) \
    (((XmListWidget)(w))->list.selectedIndices)

#define List_MarginHeight(w) \
    (((XmListWidget)(w))->list.margin_height)

#define List_MarginWidth(w) \
    (((XmListWidget)(w))->list.margin_width)

#define List_SizePolicy(w) \
    (((XmListWidget)(w))->list.SizePolicy)

#define List_ItemSpacing(w) \
    (((XmListWidget)(w))->list.ItemSpacing)

#define List_Spacing(w) \
    (((XmListWidget)(w))->list.spacing)

#define List_MultipleCallback(w) \
    (((XmListWidget)(w))->list.MultipleCallback)

#define List_SingleCallback(w) \
    (((XmListWidget)(w))->list.SingleCallback)

#define List_SBDisplayPolicy(w) \
    (((XmListWidget)(w))->list.ScrollBarDisplayPolicy)

#define List_SelectionPolicy(w) \
    (((XmListWidget)(w))->list.SelectionPolicy)

#define List_StrDir(w) \
    (((XmListWidget)(w))->list.StrDir)

#define List_TopPosition(w) \
    (((XmListWidget)(w))->list.top_position)

#define List_AddMode(w) \
    (((XmListWidget)(w))->list.AddMode)

#define List_NormalGC(w) \
    (((XmListWidget)(w))->list.NormalGC)

#define List_InsensitiveGC(w) \
    (((XmListWidget)(w))->list.InsensitiveGC)

#define List_InverseGC(w) \
    (((XmListWidget)(w))->list.InverseGC)

#define List_HighlightGC(w) \
    (((XmListWidget)(w))->list.HighlightGC)

#define List_DashTile(w) \
    (((XmListWidget)(w))->list.DashTile)

#define List_HighlightThickness(w) \
    (((XmListWidget)(w))->list.HighlightThickness)

#define List_LastHLItem(w) \
    (((XmListWidget)(w))->list.LastHLItem)

#define List_StartItem(w) \
    (((XmListWidget)(w))->list.StartItem)

#define List_OldStartItem(w) \
    (((XmListWidget)(w))->list.OldStartItem)

#define List_EndItem(w) \
    (((XmListWidget)(w))->list.EndItem)

#define List_OldEndItem(w) \
    (((XmListWidget)(w))->list.OldEndItem)

#define List_BaseX(w) \
    (((XmListWidget)(w))->list.BaseX)

#define List_BaseY(w) \
    (((XmListWidget)(w))->list.BaseY)

#define List_MouseMoved(w) \
    (((XmListWidget)(w))->list.MouseMoved)

#define List_AppendInProgress(w) \
    (((XmListWidget)(w))->list.AppendInProgress)

#define List_Traversing(w) \
    (((XmListWidget)(w))->list.Traversing)

#define List_KbdSelection(w) \
    (((XmListWidget)(w))->list.KbdSelection)

#define List_DownCount(w) \
    (((XmListWidget)(w))->list.DownCount)

#define List_DownTime(w) \
    (((XmListWidget)(w))->list.DownTime)

#define List_CurrentKbdItem(w) \
    (((XmListWidget)(w))->list.CurrentKbdItem)

#define List_SelectionType(w) \
    (((XmListWidget)(w))->list.SelectionType)

#define List_MaxWidth(w) \
    (((XmListWidget)(w))->list.MaxWidth)

#define List_CharWidth(w) \
    (((XmListWidget)(w))->list.CharWidth)

#define List_XOrigin(w) \
    (((XmListWidget)(w))->list.XOrigin)

#define List_MaxItemHeight(w) \
    (((XmListWidget)(w))->list.MaxItemHeight)

#define List_IsScrolledList(w) \
    (((XmListWidget)(w))->list.Mom != NULL)

#define List_HSB(w) \
    (((XmListWidget)(w))->list.hScrollBar)

#define List_Hmax(w) \
    (((XmListWidget)(w))->list.hmax)

#define List_Horigin(w) \
    (((XmListWidget)(w))->list.hOrigin)

#define List_Hextent(w) \
    (((XmListWidget)(w))->list.hExtent)

#define List_Hmin(w) \
    (((XmListWidget)(w))->list.hmin)

#define List_Mom(w) \
    (((XmListWidget)(w))->list.Mom)

#define List_VSB(w) \
    (((XmListWidget)(w))->list.vScrollBar)

#define List_Vmax(w) \
    (((XmListWidget)(w))->list.vmax)

#define List_Vmin(w) \
    (((XmListWidget)(w))->list.vmin)

#define List_Vorigin(w) \
    (((XmListWidget)(w))->list.vOrigin)

#define List_Vextent(w) \
    (((XmListWidget)(w))->list.vExtent)

/*
 * MAINWINDOW WIDGET ACCESS MACROS
 */
#define MW_AreaWidth(w) \
    (((XmMainWindowWidget)w)->mwindow.AreaWidth)

#define MW_AreaHeight(w) \
    (((XmMainWindowWidget)w)->mwindow.AreaHeight)

#define MW_MarginWidth(w) \
    (((XmMainWindowWidget)w)->mwindow.margin_width)

#define MW_MarginHeight(w) \
    (((XmMainWindowWidget)w)->mwindow.margin_height)

#define MW_CommandWindow(w) \
    (((XmMainWindowWidget)w)->mwindow.CommandWindow)

#define MW_MenuBar(w) \
    (((XmMainWindowWidget)w)->mwindow.MenuBar)

#define MW_MessageWindow(w) \
    (((XmMainWindowWidget)w)->mwindow.Message)

#define MW_CommandLoc(w) \
    (((XmMainWindowWidget)w)->mwindow.CommandLoc)

#define MW_Sep1(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep1)

#define MW_Sep2(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep2)

#define MW_Sep3(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep3)

#define MW_ShowSep(w) \
    (((XmMainWindowWidget)w)->mwindow.ShowSep)

/*
 * MESSAGE BOX WIDGET ACCESS MACROS
 */
#define	MB_DialogType(w) \
        (((XmMessageBoxWidget)w)->message_box.dialog_type)

#define	MB_DefaultType(w) \
        (((XmMessageBoxWidget)w)->message_box.default_type)

#define	MB_MessageString(w) \
        (((XmMessageBoxWidget)w)->message_box.message_string)

#define	MB_OKLabelString(w) \
        (((XmMessageBoxWidget)w)->message_box.ok_label_string)

#define	MB_OKButton(w) \
        (((XmMessageBoxWidget)w)->message_box.ok_button)

#define	MB_OKCall(w) \
        (((XmMessageBoxWidget)w)->message_box.ok_callback)

#define	MB_CancelLabelString(w) \
        (((XmMessageBoxWidget)w)->message_box.cancel_label_string)

#define	MB_CancelCall(w) \
        (((XmMessageBoxWidget)w)->message_box.cancel_callback)

#define	MB_HelpLabelString(w) \
        (((XmMessageBoxWidget)w)->message_box.help_label_string)

#define	MB_HelpButton(w) \
        (((XmMessageBoxWidget)w)->message_box.help_button)

#define	MB_Symbol(w) \
        (((XmMessageBoxWidget)w)->message_box.symbol_wid)

#define	MB_SymbolPixmap(w) \
        (((XmMessageBoxWidget)w)->message_box.symbol_pixmap)

#define	MB_Separator(w) \
        (((XmMessageBoxWidget)w)->message_box.separator)

#define	MB_MessageAlignment(w) \
        (((XmMessageBoxWidget)w)->message_box.message_alignment)

#define	MB_Message(w) \
        (((XmMessageBoxWidget)w)->message_box.message_wid)

#define	MB_MessageString(w) \
        (((XmMessageBoxWidget)w)->message_box.message_string)

#define	MB_MinimizeButtons(w) \
        (((XmMessageBoxWidget)w)->message_box.minimize_buttons)

/*
 * MENUSHELL WIDGET ACCESS MACROS
 */
#define MS_LabelFontList(w) \
	(((XmMenuShellWidget)w)->menu_shell.label_font_list)

#define MS_ButtonFontList(w) \
	(((XmMenuShellWidget)w)->menu_shell.button_font_list)

#define MS_DefaultFontList(w) \
	(((XmMenuShellWidget)w)->menu_shell.default_font_list)

#define MS_FocusData(w) \
	(((XmMenuShellWidget)w)->menu_shell.focus_data)

#define MS_PrivateShell(w) \
	(((XmMenuShellWidget)w)->menu_shell.private_shell)

/*
 * PANEDWINDOW WIDGET ACCESS MACROS
 */
#define PW_RefigureMode(w) \
    (((XmPanedWindowWidget)w)->paned_window.refiguremode)

#define PW_SeparatorOn(w) \
    (((XmPanedWindowWidget)w)->paned_window.separator_on)

#define PW_MarginWidth(w) \
    (((XmPanedWindowWidget)w)->paned_window.margin_width)

#define PW_MarginHeight(w) \
    (((XmPanedWindowWidget)w)->paned_window.margin_height)

#define PW_Spacing(w) \
    (((XmPanedWindowWidget)w)->paned_window.spacing)

#define PW_SashWidth(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_width)

#define PW_SashHeight(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_height)

#define PW_SashShadowThickness(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_shadow_thickness)

#define PW_SashIndent(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_indent)

#define PW_StartY(w) \
    (((XmPanedWindowWidget)w)->paned_window.starty)

#define PW_IncrementCount(w) \
    (((XmPanedWindowWidget)w)->paned_window.increment_count)

#define PW_PaneCount(w) \
    (((XmPanedWindowWidget)w)->paned_window.pane_count)

#define PW_NumSlots(w) \
    (((XmPanedWindowWidget)w)->paned_window.num_slots)

#define PW_NumManagedChildren(w) \
    (((XmPanedWindowWidget)w)->paned_window.num_managed_children)

#define PW_RecursivelyCalled(w) \
    (((XmPanedWindowWidget)w)->paned_window.recursively_called)

#define PW_ResizeAtRealize(w) \
    (((XmPanedWindowWidget)w)->paned_window.resize_at_realize)

#define PW_TopPane(w) \
    (((XmPanedWindowWidget)w)->paned_window.top_pane)

#define PW_BottomPane(w) \
    (((XmPanedWindowWidget)w)->paned_window.bottom_pane)

#define PW_FlipGC(w) \
    (((XmPanedWindowWidget)w)->paned_window.flipgc)

#define PW_ManagedChildren(w) \
    (((XmPanedWindowWidget)w)->paned_window.managed_children)


/*
 * constraint shorthand
 */
#define PWC_Position(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.position)

#define PWC_DHeight(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dheight)

#define PWC_DY(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dy)

#define PWC_OldDY(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.olddy)

#define PWC_PaneMaximum(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.max)

#define PWC_PaneMinimum(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.min)

#define PWC_IsPane(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.isPane)

#define PWC_AllowResize(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.allow_resize)

#define PWC_SkipAdjust(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.skip_adjust)

#define PWC_Sash(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.sash)

#define PWC_Separator(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.separator)

#define PWC_PositionIndex(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.position_index)

/*
 * PUSH BUTTON WIDGET ACCESS MACROS
 */
#define PB_ActivateCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.activate_callback)

#define PB_ArmCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_callback)

#define PB_DisarmCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.disarm_callback)

#define PB_Armed(w) \
    (((XmPushButtonWidget)(w))->pushbutton.armed)

#define PB_ClickCount(w) \
    (((XmPushButtonWidget)(w))->pushbutton.click_count)

#define PB_Compatible(w) \
    (((XmPushButtonWidget)(w))->pushbutton.compatible)

#define PB_ShowAsDefault(w) \
    (((XmPushButtonWidget)(w))->pushbutton.show_as_default)

#define PB_DefaultButtonShadow(w) \
    (((XmPushButtonWidget)(w))->pushbutton.default_button_shadow_thickness)

#define PB_FillOnArm(w) \
    (((XmPushButtonWidget)(w))->pushbutton.fill_on_arm)

#define PB_ArmColor(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_color)

#define PB_FillGC(w) \
    (((XmPushButtonWidget)(w))->pushbutton.fill_gc)

#define PB_BackgroundGC(w) \
    (((XmPushButtonWidget)(w))->pushbutton.background_gc)

#define PB_Timer(w) \
    (((XmPushButtonWidget)(w))->pushbutton.timer)

#define PB_ArmTimeStamp(w) \
    (((XmPushButtonWidget)(w))->pushbutton.armTimeStamp)

#define PB_ArmPixmap(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_pixmap)

#define PB_UnarmPixmap(w) \
    (((XmPushButtonWidget)(w))->pushbutton.unarm_pixmap)

#define PB_MultiClick(w) \
    (((XmPushButtonWidget)(w))->pushbutton.multiClick)

/*
 * PROTOCOL OBJECT ACCESS MACROS
 */
#define Protocol_PreHook(w) \
    (((XmProtocol)(w))->protocol.pre_hook)

#define Protocol_PostHook(w) \
    (((XmProtocol)(w))->protocol.post_hook)

#define Protocol_Callbacks(w) \
    (((XmProtocol)(w))->protocol.callbacks)

#define Protocol_Atom(w) \
    (((XmProtocol)(w))->protocol.atom)

#define Protocol_Active(w) \
    (((XmProtocol)(w))->protocol.active)

/*
 * ROWCOLUMN WIDGET ACCESS MACROS
 */
#define RC_LastSelectToplevel(m) \
    (((XmRowColumnWidget)(m))->row_column.lastSelectToplevel)

#define RC_TearOffActivatedCallback(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_activated_callback)

#define RC_TearOffDeactivatedCallback(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_deactivated_callback)

#define RC_constraint_IndexPosition(w) \
    (((XmRowColumnConstraintRec *)(w)->core.constraints)->row_column.position_index)

#define RC_PostFromList(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromList)

#define RC_PostFromCount(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromCount)

#define RC_PostFromListSize(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromListSize)

#define RC_PopupTimer(w) \
    (((XmRowColumnWidget)(w))->row_column.popup_timeout_timer)

#define	RC_IsAligned(w)	\
    (((XmRowColumnWidget)(w))->row_column.do_alignment)
/*
 * SASH WIDGET ACCESS MACROS
 */
#define Sash_SashAction(w) \
    (((XmSashWidget)w)->sash.sash_action)

#define Sash_HasFocus(w) \
    (((XmSashWidget)w)->sash.has_focus)

/*
 * SCREEN WIDGET ACCESS MACROS
 */
#define Screen_Parent(w) \
    (((XmScreen)w)->desktop.parent)

#define Screen_Children(w) \
    (((XmScreen)w)->desktop.children)

#define Screen_NumChildren(w) \
    (((XmScreen)w)->desktop.num_children)

#define Screen_NumSlots(w) \
    (((XmScreen)w)->desktop.num_slots)

#define Screen_MwmPresent(w) \
    (((XmScreen)w)->screen.mwmPresent)

#define Screen_NumReparented(w) \
    (((XmScreen)w)->screen.numReparented)

#define Screen_DarkThreshold(w) \
    (((XmScreen)w)->screen.darkThreshold)

#define Screen_ForegroundThreshold(w) \
    (((XmScreen)w)->screen.foregroundThreshold)

#define Screen_LightThreshold(w) \
    (((XmScreen)w)->screen.lightThreshold)

#define Screen_DefaultNoneCursorIcon(w) \
    (((XmScreen)w)->screen.defaultNoneCursorIcon)

#define Screen_DefaultValidCursorIcon(w) \
    (((XmScreen)w)->screen.defaultValidCursorIcon)

#define Screen_DefaultInvalidCursorIcon(w) \
    (((XmScreen)w)->screen.defaultInvalidCursorIcon)

#define Screen_DefaultMoveCursorIcon(w) \
    (((XmScreen)w)->screen.defaultMoveCursorIcon)

#define Screen_DefaultCopyCursorIcon(w) \
    (((XmScreen)w)->screen.defaultCopyCursorIcon)

#define Screen_DefaultLinkCursorIcon(w) \
    (((XmScreen)w)->screen.defaultLinkCursorIcon)

#define Screen_DefaultSourceCursorIcon(w) \
    (((XmScreen)w)->screen.defaultSourceCursorIcon)

#define Screen_NullCursor(w) \
    (((XmScreen)w)->screen.nullCursor)

#define Screen_CursorCache(w) \
    (((XmScreen)w)->screen.cursorCache)

#define Screen_MaxCursorWidth(w) \
    (((XmScreen)w)->screen.maxCursorWidth)

#define Screen_MaxCursorHeight(w) \
    (((XmScreen)w)->screen.maxCursorHeight)

#define Screen_MenuCursor(w) \
    (((XmScreen)w)->screen.menuCursor)

#define Screen_UnpostBehavior(w) \
    (((XmScreen)w)->screen.unpostBehavior)

#define Screen_FontStruct(w) \
    (((XmScreen)w)->screen.font_struct)

#define Screen_HorizUnit(w) \
    (((XmScreen)w)->screen.h_unit)

#define Screen_VertUnit(w) \
    (((XmScreen)w)->screen.v_unit)

#define Screen_ScratchPixmaps(w) \
    (((XmScreen)w)->screen.scratchPixmaps)

#define Screen_MoveOpaque(w) \
    (((XmScreen)w)->screen.moveOpaque)

#define Screen_StateCursorIcon(w) \
    (((XmScreen)w)->screen.xmStateCursorIcon)

#define Screen_MoveCursorIcon(w) \
    (((XmScreen)w)->screen.xmMoveCursorIcon)

#define Screen_CopyCursorIcon(w) \
    (((XmScreen)w)->screen.xmCopyCursorIcon)

#define Screen_LinkCursorIcon(w) \
    (((XmScreen)w)->screen.xmLinkCursorIcon)

#define Screen_SourceCursorIcon(w) \
    (((XmScreen)w)->screen.xmSourceCursorIcon)

#define Screen_ImageGC(w) \
    (((XmScreen)w)->screen.imageGC)

#define Screen_ImageGCDepth(w) \
    (((XmScreen)w)->screen.imageGCDepth)

#define Screen_ImageForeground(w) \
    (((XmScreen)w)->screen.imageForeground)

#define Screen_ImageBackground(w) \
    (((XmScreen)w)->screen.imageBackground)

#define Screen_ScreenInfo(w) \
    (((XmScreen)w)->screen.screenInfo)

/*
 * SCALE WIDGET ACCESS MACROS
 */
#define Scale_HighlightThickness(x) \
    (((XmScaleWidget)(x))->scale.highlight_thickness)

#define Scale_HighlightOnEnter(x) \
    (((XmScaleWidget)(x))->scale.highlight_on_enter)

#define Scale_ForegroundGC(x) \
    (((XmScaleWidget)(x))->scale.foreground_GC)

#define Scale_Orientation(x) \
    (((XmScaleWidget)(x))->scale.orientation)

#define Scale_ProcessingDirection(x) \
    (((XmScaleWidget)(x))->scale.processing_direction)

#define Scale_DecimalPoints(x) \
    (((XmScaleWidget)(x))->scale.decimal_points)

#define Scale_ScaleWidth(x) \
    (((XmScaleWidget)(x))->scale.scale_width)

#define Scale_ScaleHeight(x) \
    (((XmScaleWidget)(x))->scale.scale_height)

#define Scale_Minimum(x) \
    (((XmScaleWidget)(x))->scale.minimum)

#define Scale_Maximum(x) \
    (((XmScaleWidget)(x))->scale.maximum)

#define Scale_Value(x) \
    (((XmScaleWidget)(x))->scale.value)

#define Scale_LastValue(x) \
    (((XmScaleWidget)(x))->scale.last_value)

#define Scale_ShowValue(x) \
    (((XmScaleWidget)(x))->scale.show_value)

#define Scale_ShowValueX(x) \
    (((XmScaleWidget)(x))->scale.show_value_x)

#define Scale_ShowValueY(x) \
    (((XmScaleWidget)(x))->scale.show_value_y)

#define Scale_ShowValueWidth(x) \
    (((XmScaleWidget)(x))->scale.show_value_width)

#define Scale_ShowValueHeight(x) \
    (((XmScaleWidget)(x))->scale.show_value_height)

#define Scale_FontList(x) \
    (((XmScaleWidget)(x))->scale.font_list)

#define Scale_FontStruct(x) \
    (((XmScaleWidget)(x))->scale.font_struct)

#define Scale_ScaleMultiple(x) \
    (((XmScaleWidget)(x))->scale.scale_multiple)

#define Scale_Title(x) \
    (((XmScaleWidget)(x))->scale.title)

#define Scale_SliderSize(x) \
    (((XmScaleWidget)(x))->scale.slider_size)

#define Scale_ValueChangedCallback(x) \
    (((XmScaleWidget)(x))->scale.value_changed_callback)

#define Scale_DragCallback(x) \
    (((XmScaleWidget)(x))->scale.drag_callback)

/*
 * SELECTIONBOX WIDGET ACCESS MACROS
 */
#define SB_DialogType(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.dialog_type)

#define SB_ListItems(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.list_items)

#define SB_ChildPlacement(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.child_placement)

#define SB_OkCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.ok_callback)

#define SB_CancelCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.cancel_callback)

#define SB_ApplyCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.apply_callback)

#define SB_NoMatchCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.no_match_callback)

#define SB_ListLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.list_label_string)

#define SB_TextString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.text_string)

#define SB_OkLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.ok_label_string)

#define SB_CancelLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.cancel_label_string)

#define SB_ApplyLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.apply_label_string)

#define SB_HelpLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.help_label_string)

#define SB_SelectionLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.selection_label_string)

/*
 * SCROLLBAR WIDGET ACCESS MACROS
 */
#define SCB_Value(w) \
    (((XmScrollBarWidget)(w))->scrollBar.value)

#define SCB_Minimum(w) \
    (((XmScrollBarWidget)(w))->scrollBar.minimum)

#define SCB_Maximum(w) \
    (((XmScrollBarWidget)(w))->scrollBar.maximum)

#define SCB_SliderSize(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_size)

#define SCB_Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.orientation)

#define SCB_ProcessingDirection(w) \
    (((XmScrollBarWidget)(w))->scrollBar.processing_direction)

#define SCB_ShowArrows(w) \
    (((XmScrollBarWidget)(w))->scrollBar.show_arrows)

#define SCB_Increment(w) \
    (((XmScrollBarWidget)(w))->scrollBar.increment)

#define SCB_PageIncrement(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_increment)

#define SCB_InitialDelay(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_delay)

#define SCB_RepeatDelay(w) \
    (((XmScrollBarWidget)(w))->scrollBar.repeat_delay)

#define SCB_ValueChangedCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.value_changed_callback)

#define SCB_IncrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.increment_callback)

#define SCB_DecrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.decrement_callback)

#define SCB_PageIncrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_increment_callback)

#define SCB_PageDecrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_decrement_callback)

#define SCB_ToTopCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.to_top_callback)

#define SCB_ToBottomCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.to_bottom_callback)

#define SCB_DragCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.drag_callback)

#define SCB_ForegroundGC(w) \
    (((XmScrollBarWidget)(w))->scrollBar.foreground_GC)

#define SCB_TroughColor(w) \
    (((XmScrollBarWidget)(w))->scrollBar.trough_color)

#define SCB_Pixmap(w) \
    (((XmScrollBarWidget)(w))->scrollBar.pixmap)

#define SCB_SlidingOn(w) \
    (((XmScrollBarWidget)(w))->scrollBar.sliding_on)

#define SCB_EtchedSlider(w) \
    (((XmScrollBarWidget)(w))->scrollBar.etched_slider)

#define SCB_SavedValue(w) \
    (((XmScrollBarWidget)(w))->scrollBar.saved_value)

#define SCB_Flags(w) \
    (((XmScrollBarWidget)(w))->scrollBar.flags)

#define SCB_ChangeType(w) \
    (((XmScrollBarWidget)(w))->scrollBar.change_type)

#define SCB_Timer(w) \
    (((XmScrollBarWidget)(w))->scrollBar.timer)

#define SCB_InitialX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_x)

#define SCB_InitialY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_y)

#define SCB_SeparationX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.separation_x)

#define SCB_SeparationY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.separation_y)

#define SCB_SliderX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_x)

#define SCB_SliderY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_y)

#define SCB_SliderWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_width)

#define SCB_SliderHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_height)

#define SCB_SliderAreaX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_x)

#define SCB_SliderAreaY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_y)

#define SCB_SliderAreaWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_width)

#define SCB_SliderAreaHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_height)

#define SCB_Arrow1X(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_x)

#define SCB_Arrow1Y(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_y)

#define SCB_Arrow1Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_orientation)

#define SCB_Arrow1Selected(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_selected)

#define SCB_Arrow2X(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_x)

#define SCB_Arrow2Y(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_y)

#define SCB_Arrow2Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_orientation)

#define SCB_Arrow2Selected(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_selected)

#define SCB_ArrowWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow_width)

#define SCB_ArrowHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow_height)

#define SCB_UnavailableGC(w) \
    (((XmScrollBarWidget)(w))->scrollBar.unavailable_GC)

#define SCB_Offset(w) \
    (((XmScrollBarWidget)(w))->scrollBar.offset)

/*
 * SEPARATOR WIDGET ACCESS MACROS
 */
#define SEP_Margin(w) \
    (((XmSeparatorWidget)(w))->separator.margin)

#define SEP_Orientation(w) \
    (((XmSeparatorWidget)(w))->separator.orientation)

#define SEP_SeparatorType(w) \
    (((XmSeparatorWidget)(w))->separator.separator_type)

#define SEP_SeparatorGC(w) \
    (((XmSeparatorWidget)(w))->separator.separator_GC)

/*
 * SCROLLEDWINDOW WIDGET ACCESS MACROS
 */
#define SW_VSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmin)

#define SW_VSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmax)

#define SW_VSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.vOrigin)

#define SW_VSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.vExtent)

#define SW_HSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmin)

#define SW_HSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmax)

#define SW_HSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.hOrigin)

#define SW_HSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.hExtent)

#define SW_HSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbX)

#define SW_HSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbY)

#define SW_HSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbWidth)

#define SW_HSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbHeight)

#define SW_VSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbX)

#define SW_VSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbY)

#define SW_VSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbWidth)

#define SW_VSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbHeight)

#define SW_WorkWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenWidth)

#define SW_WorkHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenHeight)

#define SW_ClipWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaWidth)

#define SW_ClipHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaHeight)

#define SW_MarginWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.WidthPad)

#define SW_MarginHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.HeightPad)

#define SW_ClipX(w) \
    (((XmScrolledWindowWidget)w)->swindow.XOffset)

#define SW_ClipY(w) \
    (((XmScrolledWindowWidget)w)->swindow.YOffset)

#define SW_Spacing(w) \
    (((XmScrolledWindowWidget)w)->swindow.pad)

#define SW_HasHSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasHSB)

#define SW_HasVSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasVSB)

#define SW_InInit(w) \
    (((XmScrolledWindowWidget)w)->swindow.InInit)

#define SW_FromResize(w) \
    (((XmScrolledWindowWidget)w)->swindow.FromResize)

#define SW_VisualPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.VisualPolicy)

#define SW_ScrollPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollPolicy)

#define SW_ScrollBarPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollBarPolicy)

#define SW_Placement(w) \
    (((XmScrolledWindowWidget)w)->swindow.Placement)

#define SW_HSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hScrollBar)

#define SW_VSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.vScrollBar)

#define SW_ClipWindow(w) \
    (((XmScrolledWindowWidget)w)->swindow.ClipWindow)

#define SW_WorkWindow(w) \
    (((XmScrolledWindowWidget)w)->swindow.WorkWindow)

#define SW_TraverseObscuredCallback(w) \
    (((XmScrolledWindowWidget)w)->swindow.traverseObscuredCallback)

/*
 * TOGGLE BUTTON WIDGET ACCESS MACROS
 */
#define TB_IndType(w) \
    (((XmToggleButtonWidget)(w))->toggle.ind_type)

#define TB_Visible(w) \
    (((XmToggleButtonWidget)(w))->toggle.visible)

#define TB_Spacing(w) \
    (((XmToggleButtonWidget)(w))->toggle.spacing)

#define TB_IndicatorDim(w) \
    (((XmToggleButtonWidget)(w))->toggle.indicator_dim)

#define TB_IndicatorSet(w) \
    (((XmToggleButtonWidget)(w))->toggle.indicator_set)

#define TB_OnPixmap(w) \
    (((XmToggleButtonWidget)(w))->toggle.on_pixmap)

#define TB_InsenPixmap(w) \
    (((XmToggleButtonWidget)(w))->toggle.insen_pixmap)

#define TB_Set(w) \
    (((XmToggleButtonWidget)(w))->toggle.set)

#define TB_VisualSet(w) \
    (((XmToggleButtonWidget)(w))->toggle.visual_set)

#define TB_IndOn(w) \
    (((XmToggleButtonWidget)(w))->toggle.ind_on)

#define TB_FillOnSelect(w) \
    (((XmToggleButtonWidget)(w))->toggle.fill_on_select)

#define TB_SelectColor(w) \
    (((XmToggleButtonWidget)(w))->toggle.select_color)

#define TB_SelectGC(w) \
    (((XmToggleButtonWidget)(w))->toggle.select_GC)

#define TB_BackgroundGC(w) \
    (((XmToggleButtonWidget)(w))->toggle.background_gc)

#define TB_ArmCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.arm_CB)

#define TB_ValueChangedCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.value_changed_CB)

#define TB_DisarmCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.disarm_CB)

#define TB_Armed(w) \
    (((XmToggleButtonWidget)(w))->toggle.Armed)

/*
 * TEAROFF BUTTON WIDGET ACCESS MACROS
 */
#define TOB_Margin(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.margin)

#define TOB_Orientation(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.orientation)

#define TOB_SeparatorType(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.separator_type)

#define TOB_SeparatorGC(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.separator_GC)


/*
 * DRAG CONTEXT ACCESS MACROS
 */

#define DC_ActiveProtocolStyle(w) \
   (((XmDragContext)(w))->drag.activeProtocolStyle)

#define DC_SrcWidget(w) \
   (((XmDragContext)(w))->drag.sourceWidget)

#define DC_SrcWindow(w) \
   (((XmDragContext)(w))->drag.srcWindow)

#define DC_DragStartTime(w) \
   (((XmDragContext)(w))->drag.dragStartTime)

#define DC_DragFinishTime(w) \
   (((XmDragContext)(w))->drag.dragFinishTime)

#define DC_LastChangeTime(w) \
   (((XmDragContext)(w))->drag.lastChangeTime)


/*
 * XmDISPLAY ACCESS MACROS
 */
#define Display_DragInitiatorProtocolStyle(w) \
   (((XmDisplay)(w))->display.dragInitiatorProtocolStyle)

#define Display_DragReceiverProtocolStyle(w) \
   (((XmDisplay)(w))->display.dragReceiverProtocolStyle)

#define Display_UserGrabbed(w) \
   (((XmDisplay)(w))->display.userGrabbed)

#define Display_DragContextClass(w) \
   (((XmDisplay)(w))->display.dragContextClass)

#define Display_DropTransferClass(w) \
   (((XmDisplay)(w))->display.dropTransferClass)

#define Display_DropSiteManagerClass(w) \
   (((XmDisplay)(w))->display.dropSiteManagerClass)

#define Display_ActiveDC(w) \
   (((XmDisplay)(w))->display.activeDC)

#define Display_DSM(w) \
   (((XmDisplay)(w))->display.dsm)

#define Display_LastDragTime(w) \
   (((XmDisplay)(w))->display.lastDragTime)

#define Display_ProxyWindow(w) \
   (((XmDisplay)(w))->display.proxyWindow)

#define Display_Modals(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_NumModals(w) \
   (((XmDisplay)(w))->display.numModals)

#define Display_MaxModals(w) \
   (((XmDisplay)(w))->display.maxModals)

#define Display_Modals(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_XmImInfo(w) \
   (((XmDisplay)(w))->display.xmim_info)

#define Display_BindingsString(w) \
   (((XmDisplay)(w))->display.bindingsString)

#define Display_Bindings(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_LastKeyEvent(w) \
   (((XmDisplay)(w))->display.lastKeyEvent)

#define Display_KeycodeTag(w) \
   (((XmDisplay)(w))->display.keycode_tag)

#define Display_ShellCount(w) \
   (((XmDisplay)(w))->display.shellCount)

#define Display_DisplayInfo(w) \
   (((XmDisplay)(w))->display.displayInfo)

#endif /* MacrosI.h */
