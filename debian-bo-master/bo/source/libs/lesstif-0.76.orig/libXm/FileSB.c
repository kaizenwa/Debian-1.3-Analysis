/**
 *
 * $Id: FileSB.c,v 1.19 1996/12/15 20:49:17 jonf Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
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

static char rcsid[] = "$Id: FileSB.c,v 1.19 1996/12/15 20:49:17 jonf Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/List.h>
#include <Xm/SelectioBP.h>
#include <Xm/FileSBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/XmosP.h>
#include <Xm/TextF.h>
#include <Xm/DialogS.h>
#include <X11/Xfuncs.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XmGeoMatrix geoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref);
static Boolean noGeoRequest(XmGeoMatrix _geoSpec);

static void defaultDirSearchProc(Widget widget, XtPointer search_data);
static void defaultFileSearchProc(Widget widget, XtPointer search_data);
static void defaultQualifySearchDataProc(Widget widget, XtPointer input_data, XtPointer output_data);

static void _XmFileSelectionSearch(Widget w);
static void _XmFsbButton(Widget w, XtPointer client, XtPointer call);

/* Extern */
Boolean _XmSelectionBoxMatch(XmSelectionBoxWidget w);

/*
 * Resources for the FileSelection Box class
 */
#define Offset(field) XtOffsetOf(XmFileSelectionBoxRec, file_selection_box.field)
#define SBOffset(field) XtOffsetOf(XmFileSelectionBoxRec, selection_box.field)
static XtResource resources[] = {
    {
	XmNdirectory, XmCDirectory, XmRXmString,
	sizeof(XmString), Offset(directory),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNpattern, XmCPattern, XmRXmString,
	sizeof(XmString), Offset(pattern),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdirListLabelString, XmCDirListLabelString, XmRXmString,
	sizeof(XmString), Offset(dir_list_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNdirListItems, XmCDirListItems, XmRXmStringTable,
	sizeof(XmStringTable), Offset(dir_list_items),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdirListItemCount, XmCDirListItemCount, XmRInt,
	sizeof(int), Offset(dir_list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNfilterLabelString, XmCFilterLabelString, XmRXmString,
	sizeof(XmString), Offset(filter_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNdirMask, XmCDirMask, XmRXmString,
	sizeof(XmString), Offset(dir_mask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNnoMatchString, XmCNoMatchString, XmRXmString,
	sizeof(XmString), Offset(no_match_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNqualifySearchDataProc, XmCQualifySearchDataProc, XmRProc,
	sizeof(XmQualifyProc), Offset(qualify_search_data_proc),
	XmRImmediate, (XtPointer)defaultQualifySearchDataProc
    },
    {
	XmNdirSearchProc, XmCDirSearchProc, XmRProc,
	sizeof(XmSearchProc), Offset(dir_search_proc),
	XmRImmediate, (XtPointer)defaultDirSearchProc
    },
    {
	XmNfileSearchProc, XmCFileSearchProc, XmRProc,
	sizeof(XmSearchProc), Offset(file_search_proc),
	XmRImmediate, (XtPointer)defaultFileSearchProc
    },
    {
	XmNfileTypeMask, XmCFileTypeMask, XmRFileTypeMask,
	sizeof(unsigned char), Offset(file_type_mask),
	XmRImmediate, (XtPointer)XmFILE_REGULAR
    },
    {
	XmNlistUpdated, XmCListUpdated, XmRBoolean,
	sizeof(Boolean), Offset(list_updated),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdirectoryValid, XmCDirectoryValid, XmRBoolean,
	sizeof(Boolean), Offset(directory_valid),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdirSpec, XmCDirSpec, XmRXmString,
	sizeof(XmString), SBOffset(text_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNautoUnmanage, XmCAutoUnmanage, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmFileSelectionBoxRec, bulletin_board.auto_unmanage),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNfileListLabelString, XmCFileListLabelString, XmRXmString,
	sizeof(XmString), SBOffset(list_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNapplyLabelString, XmCApplyLabelString, XmRXmString,
	sizeof(XmString), SBOffset(apply_label_string),
	XmRString, (XtPointer)"Filter"
    },
    {
	XmNdialogType, XmCDialogType, XmRSelectionType,
	sizeof(unsigned char), SBOffset(dialog_type),
	XmRImmediate, (XtPointer)XmDIALOG_FILE_SELECTION
    },
    {
	XmNfileListItems, XmCItems, XmRXmStringTable,
	sizeof(XmStringTable), SBOffset(list_items),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfileListItemCount, XmCItemCount, XmRInt,
	sizeof(int), SBOffset(list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    }
};

/* Synthetic resources */
void FSBGetDirListItemCount(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetDirListItems(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetDirListLabelString(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetDirMask(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetDirectory(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetFilterLabelString(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetListItemCount(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetListItems(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetNoMatchString(Widget wid, int resource_offset, XtArgVal *value);
void FSBGetPattern(Widget wid, int resource_offset, XtArgVal *value);

static XmSyntheticResource syn_resources[] = {
    {
	XmNdirectory,
	sizeof(XmString), Offset(directory),
	FSBGetDirectory, NULL
    },
    {
	XmNdirListLabelString,
	sizeof(XmString), Offset(directory),
	FSBGetDirListLabelString, NULL
    },
    {
	XmNdirListItems,
	sizeof(XmStringTable), Offset(dir_list_items),
	FSBGetDirListItems, NULL
    },
    {
	XmNdirListItemCount,
	sizeof(int), Offset(dir_list_item_count),
	FSBGetDirListItemCount, NULL
    },
    {
	XmNfilterLabelString,
	sizeof(XmString), Offset(filter_label_string),
	FSBGetFilterLabelString, NULL
    },
    {
	XmNdirMask,
	sizeof(XmString), Offset(dir_mask),
	FSBGetDirMask, NULL
    },
    {
	XmNdirSpec,
	sizeof(XmString), SBOffset(text_string),
	_XmExportXmString, NULL
    },
    {
	XmNfileListLabelString,
	sizeof(XmString), SBOffset(list_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNfileListItems,
	sizeof(XmStringTable), SBOffset(list_items),
	FSBGetListItems, NULL
    },
    {
	XmNfileListItemCount,
	sizeof(XmStringTable), SBOffset(list_item_count),
	FSBGetListItemCount, NULL
    },
    {
	XmNnoMatchString,
	sizeof(XmString), Offset(no_match_string),
	FSBGetNoMatchString, NULL
    },
    {
	XmNpattern,
	sizeof(XmString), Offset(pattern),
	FSBGetPattern, NULL
    }
};

static XmBaseClassExtRec _XmFileSBCoreClassExtRec = {
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
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL, /* FIXME */
    /* class_part_init_posthook  */ NULL, /* FIXME */
    /* ext_resources             */ NULL, /* FIXME */
    /* compiled_ext_resources    */ NULL, /* FIXME */
    /* num_ext_resources         */ 0, /* FIXME */
    /* use_sub_resources         */ FALSE, /* FIXME */
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static XmManagerClassExtRec _XmFileSBMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmFileSelectionBoxClassRec xmFileSelectionBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmSelectionBoxClassRec,
        /* class_name            */ "XmFileSelectionBox",
	/* widget_size           */ sizeof(XmFileSelectionBoxRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmFileSBCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  
        /* subresource_count */ 0,     
        /* constraint_size   */ 0,     
        /* initialize        */ NULL,  
        /* destroy           */ NULL,  
        /* set_values        */ NULL,  
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
       /* translations                 */ XmInheritTranslations,
       /* syn_resources                */ syn_resources,
       /* num_syn_resources            */ XtNumber(syn_resources),
       /* syn_constraint_resources     */ NULL,
       /* num_syn_constraint_resources */ 0,
       /* parent_process               */ XmInheritParentProcess,
       /* extension                    */ (XtPointer)&_XmFileSBMClassExtRec
    },
    /* XmBulletinBoard class part */
    {
	/* always_install_accelerators  */ False,
	/* geo_matrix_create            */ geoMatrixCreate,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL,
    },
    /* XmSelectionBox part */
    {
	/* extension */ NULL,
    },
    /* XmFileSelectionBox part */
    {
	/* extension */ NULL,
    }
};

WidgetClass xmFileSelectionBoxWidgetClass = (WidgetClass)&xmFileSelectionBoxClassRec;

#define DEFAULT_NUM_VIS_ITEMS	XmFSB_MAX_WIDGETS_VERT   /* FIXME -- can someone confirm this? */
#define DEFAULT_LIST_WIDTH	100

static void 
class_initialize()
{
    _XmFileSBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmFILE_SELECTION_BOX_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Arg argl[7];
    int argc;
    XmString files = NULL;

    SB_DialogType(new_w) = XmDIALOG_FILE_SELECTION;
    XtManageChild(SB_ApplyButton(new_w));
    XtManageChild(SB_HelpButton(new_w));

    if (FS_DirListItemCount(new_w) == XmUNSPECIFIED)
	FS_DirListItemCount(new_w) = 0;

    /*
     * dir list label
     */
    if (FS_DirListLabelString(new_w) == (XmString)XmUNSPECIFIED)
	FS_DirListLabelString(new_w) =
		_XmOSGetLocalizedString(NULL, /* FIXME */
					new_w,
					XmNdirListLabelString,
					(String)"Directories");
    else if (FS_DirListLabelString(new_w) != NULL)
        FS_DirListLabelString(new_w) = XmStringCopy(FS_DirListLabelString(new_w));

    FS_DirListLabel(new_w) = _XmBB_CreateLabelG(new_w,
					      FS_DirListLabelString(new_w),
					      "dirListLabel");
    argc = 0;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING); argc++;
    XtSetValues(FS_DirListLabel(new_w), argl, argc);
    XtManageChild(FS_DirListLabel(new_w));

    /*
     * dir list
     */
    argc = 0;
    XtSetArg(argl[argc], XmNvisibleItemCount, SB_ListVisibleItemCount(new_w));
	argc++;
    XtSetArg(argl[argc], XmNselectionPolicy, XmBROWSE_SELECT); argc++;
    XtSetArg(argl[argc], XmNscrollBarDisplayPolicy, XmSTATIC); argc++;
    XtSetArg(argl[argc], XmNlistSizePolicy, XmCONSTANT); argc++;
    FS_DirList(new_w) = XmCreateScrolledList(new_w, "dirList", argl, argc);
    XtManageChild(FS_DirList(new_w));
    XtAddCallback(FS_DirList(new_w), XmNdefaultActionCallback, _XmFsbButton, NULL);

    /*
     * file list label
     */
    if (FS_FileListLabelString(new_w) == (XmString)XmUNSPECIFIED)
	files = _XmOSGetLocalizedString(NULL, /* FIXME */
				    new_w,
				    XmNfileListLabelString,
				    "Files");
    else if (FS_FileListLabelString(new_w) != NULL) {
	XmStringFree(FS_FileListLabelString(new_w));
	files = _XmOSGetLocalizedString(NULL, /* FIXME */
				    new_w,
				    XmNfileListLabelString,
				    "Files");
    }
    FS_FileListLabelString(new_w) = files;
    argc = 0;
    XtSetArg(argl[argc], XmNlabelString, FS_FileListLabelString(new_w)); argc++;
    XtSetValues(SB_ListLabel(new_w), argl, argc);

    /*
     * filter label
     */
    if (FS_FilterLabelString(new_w) == (XmString)XmUNSPECIFIED)
	FS_FilterLabelString(new_w) =
		_XmOSGetLocalizedString(NULL, /* FIXME */
					new_w,
					XmNfilterLabelString,
					"Filter");
    else
	FS_FilterLabelString(new_w) = XmStringCopy(FS_FilterLabelString(new_w));

    FS_FilterLabel(new_w) = _XmBB_CreateLabelG(new_w,
					     FS_FilterLabelString(new_w),
					     "filterListLabel");
    argc = 0;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING); argc++;
    XtSetValues(FS_FilterLabel(new_w), argl, argc);
    XtManageChild(FS_FilterLabel(new_w));

    /*
     * filter text
     */
    argc = 0;
    FS_FilterText(new_w) = XmCreateTextField(new_w, "filterText", argl, argc);
    XtManageChild(FS_FilterText(new_w));
    XtAddCallback(FS_FilterText(new_w), XmNactivateCallback, _XmFsbButton, NULL);

/*
 * Override callback on child widgets - need our own version of _XmSbButton -> _XmFsbButton
 */
    if (SB_List(new_w)) {
	XtRemoveAllCallbacks(SB_List(new_w), XmNdefaultActionCallback);
	XtAddCallback(SB_List(new_w), XmNdefaultActionCallback, _XmFsbButton, NULL);
    }
    if (SB_OkButton(new_w)) {
	XtRemoveAllCallbacks(SB_OkButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_OkButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (SB_ApplyButton(new_w)) {
	XtRemoveAllCallbacks(SB_ApplyButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_ApplyButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (BB_CancelButton(new_w)) {
	XtRemoveAllCallbacks(BB_CancelButton(new_w), XmNactivateCallback);
	XtAddCallback(BB_CancelButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (SB_HelpButton(new_w)) {
	XtRemoveAllCallbacks(SB_HelpButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_HelpButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }

    if (FS_NoMatchString(new_w) == (XmString)XmUNSPECIFIED)
        FS_NoMatchString(new_w) = XmStringCreateLocalized(" [ .. ] ");
    else if (FS_NoMatchString(new_w) != NULL)
        FS_NoMatchString(new_w) = XmStringCopy(FS_NoMatchString(new_w));

    _XmFileSelectionSearch(new_w);
}

static void
destroy(Widget w)
{
    XtDestroyWidget(FS_DirListLabel(w));
    XmStringFree(FS_DirListLabelString(w));
    XtDestroyWidget(FS_DirList(w));
    XtDestroyWidget(FS_FilterLabel(w));
    XmStringFree(FS_FilterLabelString(w));
    XtDestroyWidget(FS_FilterText(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Arg                         al[10];
    int                         ac;
    Boolean                    refresh = False;
    
    XdbDebug(__FILE__, new_w, "XmSelectionBox %s SetValues\n", XtName(new_w));

    /* this is required */
    BB_InSetValues(new_w) = True;

    /* dir list */
    if (FS_DirListItems(new_w) != FS_DirListItems(old) ||
	FS_DirListItemCount(new_w) != FS_DirListItemCount(old)) {
	ac = 0;
	/* FIXME: the items should be copied */
	XtSetArg(al[ac], XmNitems, FS_DirListItems(new_w)); ac++;
	XtSetArg(al[ac], XmNitemCount, FS_DirListItemCount(new_w)); ac++;
	XtSetValues(FS_DirList(new_w), al, ac);
	refresh = True;
    }
    
    /* dir list label string */
    if (FS_DirListLabelString(new_w) != FS_DirListLabelString(old)) {
	ac = 0;
	FS_DirListLabelString(new_w) = XmStringCopy(FS_DirListLabelString(new_w));
	XmStringFree(FS_DirListLabelString(old));
	XtSetArg(al[ac], XmNlabelString, FS_FilterLabelString(new_w)); ac++;
	XtSetValues(FS_DirListLabel(new_w), al, ac);
	refresh = True;
    }
    
    /* label string */
    if (FS_FilterLabelString(new_w) != FS_FilterLabelString(old)) {
	ac = 0;
	FS_FilterLabelString(new_w) = XmStringCopy(FS_FilterLabelString(new_w));
	XmStringFree(FS_FilterLabelString(old));
	XtSetArg(al[ac], XmNlabelString, FS_FilterLabelString(new_w)); ac++;
	XtSetValues(FS_FilterLabel(new_w), al, ac);
	refresh = True;
    }
    
    if (FS_Directory(new_w) != FS_Directory(old) ||
	FS_DirMask(new_w) != FS_DirMask(old) ||
	FS_Pattern(new_w) != FS_Pattern(old) ||
	FS_FileTypeMask(new_w) != FS_FileTypeMask(old) ||
	FS_QualifySearchDataProc(new_w) != FS_QualifySearchDataProc(old) ||
	FS_DirSearchProc(new_w) != FS_DirSearchProc(old) ||
	FS_FileSearchProc(new_w) != FS_FileSearchProc(old))
	_XmFileSelectionSearch(new_w);

    /* the next 8 lines are required for every BB subclass that uses
       the GeoMatrix */
    BB_InSetValues(new_w) = False;

    if (refresh && (XtClass(new_w) == xmFileSelectionBoxWidgetClass))
    {
	_XmBulletinBoardSizeUpdate(new_w);
	return False;
    }

    return refresh;
}

static XmGeoMatrix
geoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
    XmGeoMatrix geoSpec;
    register XmGeoRowLayout	layoutPtr;
    register XmKidGeometry boxPtr;
    Cardinal numKids;
    Boolean newRow;
    int nrows, i, nextras;
    Widget *extras = NULL;

    numKids = MGR_NumChildren(_w);

    nextras = 0;
    extras = NULL;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
		? MGR_Children(_w)[i] != XtParent(SB_List(_w))
		: True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w) &&
	    MGR_Children(_w)[i] != FS_FilterLabel(_w) &&
	    MGR_Children(_w)[i] != FS_FilterText(_w) &&
	    MGR_Children(_w)[i] != FS_DirListLabel(_w) &&
	    (FS_DirList(_w)
		? MGR_Children(_w)[i] != XtParent(FS_DirList(_w))
		: True))
	{
	    nextras++;
	}
    }

    if (nextras)
	extras = (Widget *)XtMalloc(sizeof(Widget) * nextras);

    nextras = 0;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
		? MGR_Children(_w)[i] != XtParent(SB_List(_w))
		: True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w) &&
	    MGR_Children(_w)[i] != FS_FilterLabel(_w) &&
	    MGR_Children(_w)[i] != FS_FilterText(_w) &&
	    MGR_Children(_w)[i] != FS_DirListLabel(_w) &&
	    (FS_DirList(_w)
		? MGR_Children(_w)[i] != XtParent(FS_DirList(_w))
		: True))
	{
	    extras[nextras] = MGR_Children(_w)[i];
	    nextras++;
	}
    }

    nrows = 0;

    numKids = MGR_NumChildren(_w);

    /* note the starting from one.  The zero'th child is the "work area" */
    if (nextras > 0) {
	for (i = 1; i < nextras; i++) {
	    if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
		nrows++;
	}
	if (extras[0] && XtIsManaged(extras[0]))
	    nrows++;
    }

    if (FS_FilterLabel(_w) && XtIsManaged(FS_FilterLabel(_w)))
	nrows++;

    if (FS_FilterText(_w) && XtIsManaged(FS_FilterText(_w)))
	nrows++;

    if ((FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w))) ||
	(SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w))))
	nrows++;

    if ((FS_DirList(_w) && XtIsManaged(FS_DirList(_w))) ||
	(SB_List(_w) && XtIsManaged(SB_List(_w))))
	nrows++;

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)))
	nrows++;

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)))
	nrows++;

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)))
	nrows++;

    if ((BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w))) ||
        (SB_OkButton(_w)     && XtIsManaged(SB_OkButton(_w))) ||
        (SB_ApplyButton(_w)  && XtIsManaged(SB_ApplyButton(_w))) ||
        (SB_HelpButton(_w)   && XtIsManaged(SB_HelpButton(_w))))
	nrows++;
    else {
	for (i = 1; i < nextras; i++) {
	    if (extras[i] && XtIsManaged(extras[i]) &&
		(XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])))
	    {
		nrows++;
		break;
	    }
	}
    }

    geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
    geoSpec->composite = (Widget)_w;
    geoSpec->instigator = (Widget)_from;
    if (_pref)
	geoSpec->instig_request = *_pref;
    geoSpec->margin_w = BB_MarginWidth(_w) + MGR_ShadowThickness(_w);
    geoSpec->margin_h = BB_MarginHeight(_w) + MGR_ShadowThickness(_w);
    geoSpec->no_geo_request = noGeoRequest;

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    for (i = 1; i < nextras; i++) {
	if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
	{
	    layoutPtr->fix_up = _XmMenuBarFix;
	    layoutPtr->space_above = 0;
	    boxPtr += 2;
	    layoutPtr++;
	    break;
	}
    }

    if (SB_ChildPlacement(_w) == XmPLACE_TOP && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	    layoutPtr->stretch_height = 1;
	    layoutPtr->fill_mode = XmGEO_EXPAND;
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    layoutPtr++;
	    boxPtr += 2;
	    nrows++;
    }

    if (FS_FilterLabel(_w) && XtIsManaged(FS_FilterLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_FilterLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
    }

    if (FS_FilterText(_w) && XtIsManaged(FS_FilterText(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_FilterText(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->space_above = 0; /* BB_MarginHeight(_w); */
	layoutPtr++;
	boxPtr += 2;
    }

    newRow = FALSE;
    if (FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_DirListLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = TRUE;
	boxPtr++;
    }

    if (SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_ListLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = TRUE;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    newRow = FALSE;
    if (FS_DirList(_w) && XtIsManaged(FS_DirList(_w)) &&
	_XmGeoSetupKid(boxPtr, XtParent(FS_DirList(_w))))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above =
		(FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w)))
			? BB_MarginHeight(_w)
			: 0;
	layoutPtr->min_height = 40;
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = TRUE;
	boxPtr++;
    }

    if (SB_List(_w) && XtIsManaged(SB_List(_w)) &&
	_XmGeoSetupKid(boxPtr, XtParent(SB_List(_w))))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above =
		(SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)))
			? BB_MarginHeight(_w)
			: 0;
	layoutPtr->min_height = 40;
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = TRUE;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    if (SB_ChildPlacement(_w) == XmPLACE_ABOVE_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	    layoutPtr->stretch_height = 1;
	    layoutPtr->fill_mode = XmGEO_EXPAND;
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    layoutPtr++;
	    boxPtr += 2;
	    nrows++;
    }

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_SelectionLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
    }

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_Text(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_height = 1;
	layoutPtr->even_width = 0;
	layoutPtr->space_above = 0; /* BB_MarginHeight(_w); */
	layoutPtr++;
	boxPtr += 2;
    }

    if (SB_ChildPlacement(_w) == XmPLACE_BELOW_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	    layoutPtr->stretch_height = 1;
	    layoutPtr->fill_mode = XmGEO_EXPAND;
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    layoutPtr++;
	    boxPtr += 2;
	    nrows++;
    }

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)) &&
	_XmGeoSetupKid( boxPtr, SB_Separator(_w)))
    {
	layoutPtr->fix_up = _XmSeparatorFix;
	layoutPtr->space_above = BB_MarginHeight(_w);
	boxPtr += 2;
	layoutPtr++;
    }

    newRow = False;
    if (SB_OkButton(_w)     && XtIsManaged(SB_OkButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_OkButton(_w))) {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    for (i = 1; i < nextras; i++)
    {
	if (extras[i] && XtIsManaged(extras[i]) &&
	    (XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])) &&
	    _XmGeoSetupKid(boxPtr++, extras[i]))
	{
	    _XmBulletinBoardSetDefaultShadow(extras[i]);
	    layoutPtr->fill_mode = XmGEO_CENTER;
	    layoutPtr->fit_mode = XmGEO_WRAP;
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    newRow = True;
	}
    }
    if (SB_ApplyButton(_w)  && XtIsManaged(SB_ApplyButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_ApplyButton(_w))) {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, BB_CancelButton(_w))) {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (SB_HelpButton(_w)   && XtIsManaged(SB_HelpButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_HelpButton(_w))) {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    layoutPtr->space_above = 0; /* BB_MarginHeight(_w); */
    layoutPtr->end = TRUE;
    if (nextras)
	XtFree((char *)extras);
    return(geoSpec);
}

static Boolean
noGeoRequest(XmGeoMatrix _geoSpec)
{
	if(BB_InSetValues(_geoSpec->composite) && 
		(XtClass(_geoSpec->composite) == xmFileSelectionBoxWidgetClass))
			return(TRUE);

	return( FALSE);
}

/******************************* SEARCHING **********************************/

static void 
defaultDirSearchProc(Widget widget,
		     XtPointer data)
{
    String *entries, dir, pat;
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)data;
    unsigned int numents, numalloc, argc, i, max;
    Arg argl[5];
    XmString *table;
    
/*
 * Guys, there are memory leaks here. Results from XmStringGetLtoR must be
 * freed by the caller, which you cannot do on "" or "*".
 *
 * FIX ME later on ;-)
 * Danny 25/5/1996
 */
    if (!XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir)) {
	dir = XtNewString("");
    }
    if (!XmStringGetLtoR(cbs->pattern, XmFONTLIST_DEFAULT_TAG, &pat)) {
	pat = XtNewString("*");
    }
    entries = NULL;
    numents = numalloc = 0;
/*
 * I think pattern should always be "*" for directories.
 * It's what happens if you run testXm/filesb/test1 in its directory.
 * At startup the widget shows the "." ".." and "CVS" directories.
 * After appending "*.c" to the filter, the directory list is NOT changed,
 * but the file list is.
 *
 * Danny 25/5/1996
 */
    pat = XtNewString("*");
    _XmOSGetDirEntries(dir, pat, XmFILE_DIRECTORY, False, True,
		       &entries, &numents, &numalloc);
    
    max = 64;
    table = (XmString *)XtCalloc(max, sizeof(XmString));
    for (i = 0; i < numents; i++) {
	if (i == max) {
	    max *= 2;
	    table = (XmString *)XtRealloc((char *)table, max * sizeof(XmString));
	}
	table[i] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
    }
    
    argc = 0;
    XtSetArg(argl[argc], XmNitems, table); argc++;
    XtSetArg(argl[argc], XmNitemCount, numents); argc++;
    XtSetValues(FS_DirList(widget), argl, argc);
    FS_DirListItems(widget) = table;
    FS_DirListItemCount(widget) = numents;
    
    FS_DirectoryValid(widget) = True;
    FS_ListUpdated(widget) = True;
}

static void
defaultFileSearchProc(Widget widget,
		      XtPointer data)
{
    String *entries, dir, pat;
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)data;
    unsigned int numents, numalloc, argc, i, max;
    Arg argl[5];
    XmString *table;

    XdbDebug(__FILE__, widget, "defaultFileSearchProc\n");
    
    if (!XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir)) {
	dir = XtNewString("");
	XdbDebug(__FILE__, widget, "defaultFileSearchProc: empty directory\n");
    } else {
	XdbDebug(__FILE__, widget, "defaultFileSearchProc: directory '%s'\n", dir);
    }

    if (!XmStringGetLtoR(cbs->pattern, XmFONTLIST_DEFAULT_TAG, &pat)) {
	pat = XtNewString("*");
	XdbDebug(__FILE__, widget, "defaultFileSearchProc: default pattern '*'\n");
    } else {
	XdbDebug(__FILE__, widget, "defaultFileSearchProc: pattern '%s'\n", pat);
    }

    entries = NULL;
    numents = numalloc = 0;
    _XmOSBuildFileList(dir, pat, FS_FileTypeMask(widget),
		       &entries, &numents, &numalloc);
    
    max = 64;
    table = (XmString *)XtCalloc(max, sizeof(XmString));
    for (i = 0; i < numents; i++) {
	if (i == max) {
	    max *= 2;
	    table = (XmString *)XtRealloc((char *)table, max * sizeof(XmString));
	}
	table[i] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
    }
    
    argc = 0;
    XtSetArg(argl[argc], XmNitems, table); argc++;
    XtSetArg(argl[argc], XmNitemCount, numents); argc++;
    XtSetValues(SB_List(widget), argl, argc);
    SB_ListItems(widget) = table;
    SB_ListItemCount(widget) = numents;
    
    FS_ListUpdated(widget) = True;
}


/*
 * dir part.  Must return an allocated string, even if of zero length.
 */
static void
getDirPart(Widget w,
	   XmFileSelectionBoxCallbackStruct *in,
	   String *directory) {
    String dir, tmp, tmp2;

    dir = XtMalloc(1);
    *dir = 0;

    /*
     * if dir isn't null, we want it
     */
    if (in->dir != NULL) {
	if (XmStringGetLtoR(in->dir, XmFONTLIST_DEFAULT_TAG, &tmp)) {
	    dir=XtRealloc(dir, strlen(tmp) + 1);
	    bcopy(tmp, dir, strlen(tmp));
	    dir[strlen(tmp)] = 0;
	    XtFree(tmp);
	}
	*directory = dir;
	return;
    }

    /*
     * else if the mask isn't null, get the dir from it
     */
    if (in->mask != NULL && in->mask != (XmString)XmUNSPECIFIED) {
	if (XmStringGetLtoR(in->mask, XmFONTLIST_DEFAULT_TAG, &tmp)) {
	    tmp2 = _XmOSFindPatternPart(tmp);
	    if (tmp2 != tmp) {
		dir = XtRealloc(dir, tmp2 - tmp + 1);
		bcopy(tmp, dir, tmp2 - tmp);
		dir[tmp2-tmp] = 0;
	    }
	    XtFree(tmp);
	}
	*directory = dir;
	return;
    }

    /*
     * else it comes from XmNdirectory
     */
    if (FS_Directory(w) != NULL && FS_Directory(w) != (XmString)XmUNSPECIFIED &&
	XmStringGetLtoR(FS_Directory(w),
			XmFONTLIST_DEFAULT_TAG, &tmp)) {
	dir = XtRealloc(dir, strlen(tmp) + 1);
	bcopy(tmp, dir, strlen(tmp));
	dir[strlen(tmp)] = 0;
	XtFree(tmp);
    }
    *directory = dir;
}

/*
 * pattern part.  Must return an allocated string, even if of zero length.
 */
static void
getPatPart(Widget w,
	   XmFileSelectionBoxCallbackStruct *in,
	   String *pattern) {
    String pat, tmp, tmp2;

    pat = XtMalloc(1);
    *pat = 0;

    /*
     * if we have an input pattern, use it
     */
    if (in->pattern != NULL && in->pattern != (XmString)XmUNSPECIFIED) {
	if (XmStringGetLtoR(in->pattern, XmFONTLIST_DEFAULT_TAG, &tmp)) {
	    pat = XtRealloc(pat, strlen(tmp) + 1);
	    bcopy(tmp, pat, strlen(tmp));
	    pat[strlen(tmp)] = 0;
	    XtFree(tmp);
	}
	*pattern = pat;
	return;
    }

    /*
     * else if we have a mask, get the pattern from it
     */
    if (in->mask != NULL && in->mask != (XmString)XmUNSPECIFIED) {
	if (XmStringGetLtoR(in->mask, XmFONTLIST_DEFAULT_TAG, &tmp)) {
	    tmp2 = _XmOSFindPatternPart(tmp);
	    if (tmp2 != tmp) {
		pat = XtRealloc(pat, strlen(tmp2) + 1);
		bcopy(tmp2, pat, strlen(tmp2));
		pat[strlen(tmp2)] = 0;
	    }
	    XtFree(tmp);
	}
	*pattern = pat;
	return;
    }

    /*
     * else it comes from XmNpattern
     */
    if (FS_Pattern(w) != NULL && FS_Pattern(w) != (XmString)XmUNSPECIFIED &&
	XmStringGetLtoR(FS_Pattern(w),
			XmFONTLIST_DEFAULT_TAG, &tmp)) {
	pat = XtRealloc(pat, strlen(tmp) + 1);
	bcopy(tmp, pat, strlen(tmp));
	pat[strlen(tmp)] = 0;
    }

    *pattern = pat;
}

static void 
defaultQualifySearchDataProc(Widget w,
			     XtPointer input_data,
			     XtPointer output_data)
{
    String dir = NULL, pat = NULL, directory = NULL, pattern = NULL;
    XmFileSelectionBoxCallbackStruct *in =
			(XmFileSelectionBoxCallbackStruct *)input_data;
    XmFileSelectionBoxCallbackStruct *out =
			(XmFileSelectionBoxCallbackStruct *)output_data;

    /*
     * heuristic as by OSF/Motif Programmer's Reference Release 1.2.
     * What a pain in the a**
     */

    /*
     * first, if we have both dir and pattern we're ok.
     */
    if (in->dir != NULL && in->pattern != NULL) {
	out->dir = XmStringCopy(in->dir);
	out->dir_length = XmStringLength(out->dir);

	out->pattern = XmStringCopy(in->pattern);
	out->pattern_length = XmStringLength(out->pattern);
    }
    /*
     * otherwise, get the pieces and qualify them
     */
    else {
	getDirPart(w, in, &dir);
	getPatPart(w, in, &pat);

	_XmOSQualifyFileSpec(dir, pat, &directory, &pattern);

	/*
	 * the subroutines MUST ensure this works, or not return
	 */
	XtFree(dir);
	XtFree(pat);

	out->dir = XmStringCreateLtoR(directory, XmFONTLIST_DEFAULT_TAG);
	out->dir_length = XmStringLength(out->dir);

	out->pattern = XmStringCreateLtoR(pattern, XmFONTLIST_DEFAULT_TAG);
	out->pattern_length = XmStringLength(out->pattern);

        XtFree(directory);
        XtFree(pattern);
    }

    out->mask = XmStringConcat(out->dir, out->pattern);
    out->mask_length = XmStringLength(out->mask);
 
/* This can hardly be right. Oh well. Danny 4/10/96 FIX ME */
{
    char	*s = NULL;
    XmStringGetLtoR(out->mask, XmFONTLIST_DEFAULT_TAG, &s);
    if (s) {
	extern void _XmOSGetDotDot(String);

	XmStringFree(out->mask);
	_XmOSGetDotDot(s);
	out->mask = XmStringCreateLtoR(s, XmFONTLIST_DEFAULT_TAG);
	XtFree(s);
    }
}
/* End */

    if (in->value == NULL || in->value == (XmString)XmUNSPECIFIED) {
	if (FS_DirSpec(w) && FS_DirSpec(w) != (XmString)XmUNSPECIFIED)
	    out->value = XmStringCopy(FS_DirSpec(w));
	else
	    out->value = XmStringConcat(out->dir, out->pattern);
    }
    else
	out->value = XmStringCopy(in->value);
    out->length = XmStringLength(out->value);
    out->reason = in->reason;
    out->event = in->event;
}

/*
 * search procedure.  Algorithm per
 * OSF/M*tif Programmer's Reference Release 1.2, 1-446
 */
static void
_XmFileSelectionSearch(Widget w)
{
    XmFileSelectionBoxCallbackStruct in, out;
    Arg argl[5];
    int argc;

    XdbDebug(__FILE__, w, "_XmFileSelectionSearch()\n");
    
    /*
     * setup the callback structures
     */
    in.reason = 0;
    in.event = NULL;
    in.value = NULL;
    in.length = 0;
    in.mask = FS_DirMask(w);
    if (in.mask && in.mask != (XmString)XmUNSPECIFIED)
	in.mask_length = XmStringLength(FS_DirMask(w));
    in.dir = FS_Directory(w);
    if (in.dir && in.dir != (XmString)XmUNSPECIFIED)
	in.dir_length = XmStringLength(FS_Directory(w));
    in.pattern = FS_Pattern(w);
    if (in.pattern && in.pattern != (XmString)XmUNSPECIFIED)
	in.pattern_length = XmStringLength(FS_Pattern(w));
    
    /*
     * qualification
     */
    (*FS_QualifySearchDataProc(w))(w, &in, &out);
    
    /*
     * dir search
     */
    FS_DirectoryValid(w) = False;
    FS_ListUpdated(w) = False;
    (*FS_DirSearchProc(w))(w, &out);
    
    /*
     * file search
     */
    if (FS_DirectoryValid(w)) {
	char *value;
	
	FS_ListUpdated(w) = False;
	(*FS_FileSearchProc(w))(w, &out);
	if (FS_ListUpdated(w) && SB_ListItemCount(w) == 0) {
	    argc = 0;
	    XtSetArg(argl[argc], XmNitems, FS_NoMatchString(w)); argc++;
	    XtSetValues(SB_List(w), argl, argc);
	    argc = 0;
	    value = XtMalloc(1);
	    *value = 0;
	    XtSetArg(argl[argc], XmNvalue, value); argc++;
	    XtSetValues(SB_Text(w), argl, argc);
	}
	else if (FS_ListUpdated(w)) {
	    FS_DirSpec(w) = out.dir;
	    XmStringGetLtoR(out.dir, XmFONTLIST_DEFAULT_TAG, &value);
	    
	    argc = 0;
	    XtSetArg(argl[argc], XmNvalue, value); argc++;
	    XtSetValues(SB_Text(w), argl, argc);
	    XtFree(value);
	}

	FS_DirMask(w) = out.mask;	/* Shouldn't we free previous contents first ? FIX ME */
	
	XmStringGetLtoR(FS_DirMask(w), XmFONTLIST_DEFAULT_TAG, &value);
	argc = 0;
	XtSetArg(argl[argc], XmNvalue, value); argc++;
	XtSetValues(FS_FilterText(w), argl, argc);
	XtFree(value);
	
	FS_Directory(w) = out.dir;
	FS_Pattern(w) = out.pattern;
    }

#if 1
/* Scroll to the right. This is not right, this may not be the right place either.
   Oh well.
   FIX ME */
   {
      Widget  sl = FS_DirList(w);
      /* We are directly referencing into the scrolled list to get a
       * scrollbar, but we have a pointer to the list part not the
       * scrolled window part (from XmCreateScrolledList).  Use XtParent 
       * to get the scrolled window.
       */
      Widget  hsb = (Widget)SW_HSB(XtParent(sl));
      if (hsb && XtIsManaged(hsb))
		  XmListSetHorizPos(sl, SCB_Maximum(hsb) - SCB_SliderSize(hsb));
	  /*  XtVaSetValues(hsb, XmNvalue, SW_HSBMaximum(sl), NULL);*/

      sl = SB_List(w);
      hsb = (Widget)SW_HSB(XtParent(sl));
      if (hsb && XtIsManaged(hsb))
		  XmListSetHorizPos(sl, SCB_Maximum(hsb) - SCB_SliderSize(hsb));
	  /*  XtVaSetValues(hsb, XmNvalue, SW_HSBMaximum(sl), NULL);*/
   }
	
#endif
}

/************************** END WIDGET METHODS ****************************/

/************************** EXTERNAL FUNCTIONS ****************************/
Widget
XmCreateFileSelectionBox(Widget parent,
			 char *name,
			 Arg *argList,
			 Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmFileSelectionBoxWidgetClass,
			  parent,
			  argList,
			  argcount);
}

Widget 
XmCreateFileSelectionDialog(Widget parent, 
			    char *name,
			    Arg *argList,
			    Cardinal argcount)
{
    Widget      d, ret;
    char        *s;
    Arg         *al;
    int		ac, i;

    s = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    ac = 0;
    XtSetArg(al[ac], XmNallowShellResize, True); ac++;
    for (i=0; i<argcount; i++) {
	XtSetArg(al[ac], argList[i].name, argList[i].value); ac++;
    }

    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, al, ac);
    XtFree(s);

    XtSetArg(al[ac], XmNdialogType, XmDIALOG_FILE_SELECTION); ac++;
    ret = XtCreateWidget(name, xmFileSelectionBoxWidgetClass, d, al, ac);
    XtFree((XtPointer)al);

    return ret;
}

Widget
XmFileSelectionBoxGetChild(Widget w,
			   unsigned char child)
{
    switch(child) {
    case XmDIALOG_APPLY_BUTTON:
	return SB_ApplyButton(w);
    case XmDIALOG_CANCEL_BUTTON:
	return SB_CancelButton(w);
    case XmDIALOG_OK_BUTTON:
	return SB_OkButton(w);
    case XmDIALOG_FILTER_TEXT:
	return FS_FilterText(w);
    case XmDIALOG_HELP_BUTTON:
	return SB_HelpButton(w);
    case XmDIALOG_LIST:
	return SB_List(w);
    case XmDIALOG_SEPARATOR:
	return SB_Separator(w);
    case XmDIALOG_TEXT:
	return SB_Text(w);
    case XmDIALOG_LIST_LABEL:
	return SB_ListLabel(w);
    case XmDIALOG_SELECTION_LABEL:
	return SB_SelectionLabel(w);
    case XmDIALOG_FILTER_LABEL:
	return FS_FilterLabel(w);
    case XmDIALOG_DIR_LIST:
	return FS_DirList(w);
    case XmDIALOG_DIR_LIST_LABEL:
	return FS_DirListLabel(w);

/* Still have to treat these */
    case XmDIALOG_MESSAGE_LABEL:
    case XmDIALOG_DEFAULT_BUTTON:
    case XmDIALOG_SYMBOL_LABEL:
              return BB_DefaultButton(w);
    }
    return NULL;
}

void
XmFileSelectionDoSearch(Widget w,
			XmString dirmask)
{
}

/*
 * Stolen from SelectionBox.c
 *
 * We need a private routine for this here, because we need to pass the
 * callback structures specific to FileSelectionBox not SelectionBox.
 *
 * This routine is called for any button child of selectionbox for the
 * XmNactivateCallback. Make mapping to XmNokCallback etc.
 * Also called for double-click in list.
 */
static void
_XmFsbButton(Widget w, XtPointer client, XtPointer call)
{
	XmFileSelectionBoxWidget		sb = (XmFileSelectionBoxWidget) XtParent(w);
	XmAnyCallbackStruct			*a = (XmAnyCallbackStruct *) call;
	XmFileSelectionBoxCallbackStruct	cbs;

	cbs.event = a->event;
	cbs.value = (XmString)0;
	cbs.length = 0;

	if (XmIsList(w)) {	/* The lists are grandchildren... fix sb */
	    sb = (XmFileSelectionBoxWidget)XtParent(sb);
	}

	if (w == SB_OkButton(sb) || w == SB_List(sb)) {
	    if (w == SB_OkButton(sb)) {
		char	*s = XmTextFieldGetString(SB_Text(sb));

		XdbDebug(__FILE__, w, "_XmFsbButton OK '%s'\n", s);

		cbs.value = XmStringCreateSimple(s);
		XtFree(s);
	    } else {
		XmListCallbackStruct	*ls = (XmListCallbackStruct *) call;
		cbs.value = ls->item;

		XdbDebug(__FILE__, w, "_XmFsbButton LIST '%s'\n",
			XdbXmString2String(cbs.value));
	    }
	    cbs.length = XmStringLength(cbs.value);

		if (SB_TextString(sb))
			XmStringFree(SB_TextString(sb));
		SB_TextString(sb) = XmStringCopy(cbs.value);

	    if (SB_MustMatch(sb)) {
		if (_XmSelectionBoxMatch((XmSelectionBoxWidget)sb)) {
		    cbs.reason = XmCR_OK;
		    XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);
		} else {
		    cbs.reason = XmCR_NO_MATCH;
		    XtCallCallbackList((Widget)sb, SB_NoMatchCallback(sb), &cbs);
		}
	    } else {
		cbs.reason = XmCR_OK;
		XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);
	    }
	} else if (w == BB_CancelButton(sb)) {
		cbs.reason = XmCR_CANCEL;
		XdbDebug(__FILE__, w, "_XmFsbButton CANCEL\n");
		XtCallCallbackList((Widget)sb, SB_CancelCallback(sb), &cbs);
	} else if (w == FS_FilterText(sb) || w == SB_ApplyButton(sb)) {
		/* There is no apply button - this is Filter */
		XdbDebug(__FILE__, w, "_XmFsbButton FILTER\n");
		FS_Pattern(sb) = XmStringCreateSimple(XmTextFieldGetString(FS_FilterText((Widget)sb)));
		_XmFileSelectionSearch((Widget)sb);
	} else if (w == SB_HelpButton(sb)) {
		cbs.reason = XmCR_HELP;
		XdbDebug(__FILE__, w, "_XmFsbButton HELP\n");
		XtCallCallbackList((Widget)sb, sb->manager.help_callback, &cbs);
	} else if (w == FS_DirList(sb)) {
		/* Somebody selected a directory */
		XmListCallbackStruct	*ls = (XmListCallbackStruct *) call;

		XdbDebug(__FILE__, w, "_XmFsbButton DIRLIST\n");

		FS_Directory(sb) = ls->item;
		_XmFileSelectionSearch((Widget)sb);
	} else {
		XdbDebug(__FILE__, w, "_XmFsbButton : untreated case !!\n");
	}
}

/*
 * I assume that the following can be used in synthetic resources :
 *	FSBGetDirListItemCount
 *	FSBGetDirListItems
 *	FSBGetDirListLabelString
 *	FSBGetDirMask
 *	FSBGetDirectory
 *	FSBGetFilterLabelString
 *	FSBGetListItemCount
 *	FSBGetListItems
 *	FSBGetNoMatchString
 *	FSBGetPattern
 *
 * Rules for the copy versus reallocate question have been taken from Kenton Lee's
 * article "Avoiding Motif Memory Leaks" in his column "Advanced Application Development"
 * in The X Advisor, March 1996.
 * The article can be located under http://landru.unx.com/DD/advisor/ .
 * Other articles by Ken are available at http://www.rahul.net/kenton/bib.html or
 * http://www.rahul.net/kenton/index.shtml .
 */	
void FSBGetDirListItemCount(Widget wid, int resource_offset, XtArgVal *value)
{
	*value = FS_DirListItemCount(wid);
}

void FSBGetDirListItems(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is copied in SetValues, but not in GetValues */
	*value = (XtArgVal)FS_DirListItems(wid);
}

void FSBGetDirListLabelString(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is always a copy */
	*value = (XtArgVal)XmStringCopy(FS_DirListLabelString(wid));
}

void FSBGetDirMask(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is always a copy */
	*value = (XtArgVal)XmStringCopy(FS_DirMask(wid));
}

void FSBGetDirectory(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is always a copy */
	*value = (XtArgVal)XmStringCopy(FS_Directory(wid));
}

void FSBGetFilterLabelString(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is always a copy */
	*value = (XtArgVal)XmStringCopy(FS_FilterLabelString(wid));
}

void FSBGetListItemCount(Widget wid, int resource_offset, XtArgVal *value)
{
	*value = (XtArgVal)SB_ListItemCount(wid);
}

void FSBGetListItems(Widget wid, int resource_offset, XtArgVal *value)
{
/* According to Ken this is copied in SetValues, but not in GetValues */
	*value = (XtArgVal)SB_ListItems(wid);
}

void FSBGetNoMatchString(Widget wid, int resource_offset, XtArgVal *value)
{
/* FIX ME : a copy or the real thing ? */
	*value = (XtArgVal)FS_NoMatchString(wid);
}

void FSBGetPattern(Widget wid, int resource_offset, XtArgVal *value)
{
/* FIX ME : a copy or the real thing ? */
	*value = (XtArgVal)FS_Pattern(wid);
}
