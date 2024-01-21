/**
 *
 * $Id: ResConvert.c,v 1.9 1996/12/17 03:24:04 miers Exp $
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

static char rcsid[] = "$Id: ResConvert.c,v 1.9 1996/12/17 03:24:04 miers Exp $";

#include <stdio.h>
#include <stdlib.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <ctype.h>

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/RepType.h>
#include <Xm/BulletinBP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSEP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/XmosP.h>
#include <X11/Xfuncs.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

Boolean _XmCvtStringToHorizontalPosition(Display *, XrmValue *, Cardinal *,
					 XrmValue *, XrmValue *, XtPointer *);
Boolean _XmCvtStringToVerticalPosition(Display *, XrmValue *, Cardinal *,
				       XrmValue *, XrmValue *, XtPointer *);
#ifdef NONSTANDARD_EXTENSIONS
static Boolean _XmeCvtStringToPixmap(Display *, XrmValue *, Cardinal *,
				       XrmValue *, XrmValue *, XtPointer *);
#endif

/* please be careful if you're editting these arrays, as
   the strings have to be in the _same_ order as the
   enumerated constants in the include files.  Failure to comply
   will bring about the death of your firstborn.  Have
   a nice day. */

static char *
multi_click[] = {
    "multiclick_discard",
    "multiclick_keep"
};

static char *
packing_styles[] = {
    "no_packing",
    "pack_tight",
    "pack_column",
    "pack_none"
};

static char *
focus_policies[] = {
    "explicit",
    "pointer"
};

static char *
label_types[] = {
    "pixmap",
    "string"
};
static unsigned char label_type_values[] = {
    XmPIXMAP,
    XmSTRING
};

static char *
vertical_alignments[] = {
    "alignment_baseline_top",
    "alignment_baseline_bottom",
    "alignment_widget_top",
    "alignment_widget_bottom",
    "alignment_contents_top",
    "alignment_contents_bottom",
    "alignment_left",
    "alignment_right",
};
static unsigned char
vertical_alignment_values[] = {
    XmALIGNMENT_BASELINE_TOP,
    XmALIGNMENT_BASELINE_BOTTOM,
    XmALIGNMENT_WIDGET_TOP,
    XmALIGNMENT_WIDGET_BOTTOM,
    XmALIGNMENT_CONTENTS_TOP,
    XmALIGNMENT_CONTENTS_BOTTOM,
    0, /* I can't find these two anywhere in Motif */
    0 /* I can't find these two anywhere in Motif */
};

static char *
horizontal_alignments[] = {
    "alignment_beginning",
    "alignment_center",
    "alignment_end"
};
static unsigned char
horizontal_alignment_values[] = {
    XmALIGNMENT_BEGINNING,
    XmALIGNMENT_CENTER,
    XmALIGNMENT_END
};

static char *
arrow_directions[] = {
    "arrow_up",
    "arrow_down",
    "arrow_left",
    "arrow_right"
};

static char *
attachments[] = {
    "attach_none",
    "attach_form",
    "attach_opposite_form",
    "attach_widget",
    "attach_opposite_widget",
    "attach_position",
    "attach_self"
};

static char *
audible_warnings[] = {
    "none",
    "bell"
};

static char *
frame_child_types[] = {
    "frame_generic_child",
    "frame_workarea_child",
    "frame_title_child"
};

static char *
delete_responses[] = {
    "destroy",
    "unmap",
    "do_nothing"
};

static char *
navigation_types[] = {
    "none",
    "tab_group",
    "sticky_tab_group",
    "exclusive_tab_group"
};

static char *
orientations[] = {
    "vertical",
    "horizontal",
};
static unsigned char
orientation_values[] = {
    XmVERTICAL,
    XmHORIZONTAL
};

static char *
protocol_styles[] = {
    "drag_none",
    "drag_drop_only",
    "drag_prefer_preregister",
    "drag_preregister",
    "drag_prefer_dynamic",
    "drag_dynamic",
    "drag_prefer_receiver"
};

static char *
scrollbar_placement[] = {
    "bottom_right",
    "top_right",
    "bottom_left",
    "top_left"
};

static char *
scrolling_policy[] = {
    "automatic",
    "application_defined"
};

static char *
scrollbar_policy[] = {
    "static",
    "as_needed"
};

static char *
edit_mode[] = {
    "multi_line_edit",
    "single_line_edit"
};

static char *
unit_types[] = {
    "pixels",
    "100th_millimeters",
    "1000th_inches",
    "100th_points",
    "100th_font_units"
};

static char *
dialog_types[] = {
    "dialog_template",
    "dialog_error",
    "dialog_information",
    "dialog_message",
    "dialog_question",
    "dialog_warning",
    "dialog_working"
};

static char *
dialog_styles[] = {
    "dialog_modeless",
    "dialog_primary_application_modal",
    "dialog_full_application_modal",
    "dialog_system_modal"
};

static char *
shadow_types[] = {
    "shadow_in",
    "shadow_out",
    "shadow_etched_in",
    "shadow_etched_out"
};
static unsigned char
shadow_type_values[] = {
    XmSHADOW_IN,
    XmSHADOW_OUT,
    XmSHADOW_ETCHED_IN,
    XmSHADOW_ETCHED_OUT
};

static char *
separator_types[] = {
    "no_line",
    "single_line",
    "double_line",
    "single_dashed_line",
    "double_dashed_line",
    "shadow_etched_in",
    "shadow_etched_out",
    "shadow_etched_in_dash",
    "shadow_etched_out_dash",
    "invalid_separator_type"
};

static char *
row_column_types[] = {
    "work_area",
    "menu_bar",
    "menu_pulldown",
    "menu_popup",
    "menu_option"
};

static unsigned char
row_column_type_values[] = {
    XmWORK_AREA,
    XmMENU_BAR,
    XmMENU_PULLDOWN,
    XmMENU_POPUP,
    XmMENU_OPTION
};

static char *
indicator_types[] = {
    "n_of_many",
    "one_of_many"
};

static char *
resize_policies[] = {
    "resize_none",
    "resize_grow",
    "resize_any"
};

static char *
extension_types[] = {
    "cache_extension",
    "desktop_extension",
    "shell_extension",
    "protocol_extension",
    "default_extension"
};

static unsigned char
extension_type_values[] = {
    XmCACHE_EXTENSION,
    XmDESKTOP_EXTENSION,
    XmSHELL_EXTENSION,
    XmPROTOCOL_EXTENSION,
    XmDEFAULT_EXTENSION
};

static char *
icon_attachments[] = {
    "attach_north_west",
    "attach_north",
    "attach_north_east",
    "attach_east",
    "attach_south_east",
    "attach_south",
    "attach_south_west",
    "attach_west",
    "attach_center",
    "attach_hot"
};

static char *
transfer_statuses[] = {
    "transfer_failure",
    "transfer_success"
};

static char *
file_types[] = {
    "file_directory",
    "file_regular",
    "file_any_type"
};
static unsigned char
file_type_values[] = {
    XmFILE_DIRECTORY,
    XmFILE_REGULAR,
    XmFILE_ANY_TYPE
};

static char *
string_directions[] = {
    "string_direction_l_to_r",
    "string_direction_r_to_l"
};

static char *
command_locations[] = {
    "command_above_workspace",
    "command_below_workspace"
};

static char *
default_button_types[] = {
    "dialog_none",
    "dialog_cancel_button",
    "dialog_ok_button",
    "dialog_help_button"
};
static unsigned char
default_button_type_values[] = {
    XmDIALOG_NONE,
    XmDIALOG_CANCEL_BUTTON,
    XmDIALOG_OK_BUTTON,
    XmDIALOG_HELP_BUTTON
};

static char *
processing_directions[] = {
    "max_on_top",
    "max_on_bottom",
    "max_on_left",
    "max_on_right"
};

static char *
unpost_behaviours[] = {
    "unpost",
    "unpost_and_replay"
};

static char *
visual_policies[] = {
    "variable",
    "constant"
};

static char *
child_placements[] = {
    "place_top",
    "place_above_selection",
    "place_below_selection"
};

void
XmRegisterConverters()
{
    /* don't really know the difference between the two, so 
       we'll just call the other from here for now. */

    _XmRegisterConverters();
}

void
_XmRegisterConverters()
{
#define REPTYPE_REGISTER(reptype,array) \
    XmRepTypeRegister((reptype), \
                     (array), \
                     NULL, XtNumber((array)));
#define REPTYPE_REGISTER_WITH_VALUES(reptype,array,values) \
    XmRepTypeRegister((reptype), \
                     (array), \
                     (values), XtNumber((array)));

#ifdef NONSTANDARD_EXTENSIONS
    /* this is needed for the string to pixmap converter */
    static XtConvertArgRec args[] = {
      {
         XtBaseOffset,
         (XtPointer)XtOffsetOf(WidgetRec, core.screen),
         sizeof(Screen *)
      }
    };
#endif

    REPTYPE_REGISTER(XmRMultiClick, multi_click);
    REPTYPE_REGISTER(XmRPacking, packing_styles);
    REPTYPE_REGISTER(XmRKeyboardFocusPolicy, focus_policies);
    REPTYPE_REGISTER(XmRArrowDirection, arrow_directions);
    REPTYPE_REGISTER(XmRAttachment, attachments);
    REPTYPE_REGISTER(XmRAudibleWarning, audible_warnings);
    REPTYPE_REGISTER(XmRChildType, frame_child_types);
    REPTYPE_REGISTER(XmRDeleteResponse, delete_responses);
    REPTYPE_REGISTER(XmRNavigationType, navigation_types);
    REPTYPE_REGISTER(XmRScrollBarPlacement, scrollbar_placement);
    REPTYPE_REGISTER(XmRScrollingPolicy, scrolling_policy);
    REPTYPE_REGISTER(XmRScrollBarDisplayPolicy, scrollbar_policy);
    REPTYPE_REGISTER(XmREditMode, edit_mode);
    REPTYPE_REGISTER(XmRDragInitiatorProtocolStyle, protocol_styles);
    REPTYPE_REGISTER(XmRDragReceiverProtocolStyle, protocol_styles);
    REPTYPE_REGISTER(XmRUnitType, unit_types);
    REPTYPE_REGISTER(XmRDialogType, dialog_types);
    REPTYPE_REGISTER(XmRSelectionType, dialog_types);
    REPTYPE_REGISTER(XmRDialogStyle, dialog_styles);
    REPTYPE_REGISTER(XmRSeparatorType, separator_types);
    REPTYPE_REGISTER(XmRIndicatorType, indicator_types);
    REPTYPE_REGISTER(XmRResizePolicy, resize_policies);
    REPTYPE_REGISTER(XmRIconAttachment, icon_attachments);
    REPTYPE_REGISTER(XmRTransferStatus, transfer_statuses);
    REPTYPE_REGISTER(XmRStringDirection, string_directions);
    REPTYPE_REGISTER(XmRCommandWindowLocation, command_locations);
    REPTYPE_REGISTER(XmRProcessingDirection, processing_directions);
    REPTYPE_REGISTER(XmRUnpostBehavior, unpost_behaviours);
    REPTYPE_REGISTER(XmRVisualPolicy, visual_policies);
    REPTYPE_REGISTER(XmRChildPlacement, child_placements);

    REPTYPE_REGISTER_WITH_VALUES(XmRLabelType, label_types, label_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRAlignment, horizontal_alignments,
				 horizontal_alignment_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRChildHorizontalAlignment, horizontal_alignments,
				 horizontal_alignment_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRChildVerticalAlignment, vertical_alignments,
				 vertical_alignment_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRShadowType, shadow_types,
				 shadow_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRRowColumnType, row_column_types,
				 row_column_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRExtensionType, extension_types,
				 extension_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRFileTypeMask, file_types,
				 file_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmRDefaultButtonType, default_button_types,
				 default_button_type_values);
    REPTYPE_REGISTER_WITH_VALUES(XmROrientation, orientations,
				 orientation_values);

#undef REPTYPE_REGISTER
#undef REPTYPE_REGISTER_WITH_VALUES

    /* now we install the other toolkit converters */

    XtSetTypeConverter(XtRString,     /* source type */
		       XmRXmString,   /* target type */
		       _XmCvtStringToXmString, /* converter routine */
		       NULL,          /* args for converter routine */
		       0,             /* number of args to converter routine */
		       XtCacheAll,    /* caching instructions */
		       NULL);         /* destructor function */

    XtSetTypeConverter(XtRString,     /* source type */
		       XmRFontList,   /* target type */
		       _XmCvtStringToFontlist, /* converter routine */
		       NULL,          /* args for converter routine */
		       0,             /* number of args to converter routine */
		       XtCacheNone,    /* caching instructions */
		       NULL);         /* destructor function */
		       
    XtSetTypeConverter(XtRString,              /* source type */
		       XmRHorizontalDimension, /* target type */
		       _XmCvtStringToDimension,/* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRShellHorizDim,       /* target type */
		       _XmCvtStringToDimension,/* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRHorizontalInt,       /* target type */
		       _XmCvtStringToPosition, /* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRHorizontalPosition,  /* target type */
		       _XmCvtStringToHorizontalPosition, /* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRVerticalPosition,    /* target type */
		       _XmCvtStringToVerticalPosition, /* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRShellVertDim,        /* target type */
		       _XmCvtStringToDimension,/* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRVerticalDimension,   /* target type */
		       _XmCvtStringToDimension,/* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,              /* source type */
		       XmRVerticalInt,         /* target type */
		       _XmCvtStringToPosition, /* converter routine */
		       NULL,                   /* args for converter routine */
		       0,                      /* number of args to converter routine */
		       XtCacheAll,             /* caching instructions */
		       NULL);                  /* destructor function */

    XtSetTypeConverter(XtRString,		/* source type */
			XmRKeySym,		/* target type */
			_XmCvtStringToKeySym,	/* converter routine */
			NULL,			/* args for converter routine */
			0,			/* number of args to converter routine */
			XtCacheAll,		/* caching instructions */
			NULL);			/* destructor function */

#if 0
    XtSetTypeConverter(XtRString,         /* source type */
		       XmRNavigation,     /* target type */
		       _XmCvtStringToXmNavigation, /* converter routine */
		       NULL,              /* args for converter routine */
		       0,                 /* number of args to converter routine */
		       XtCacheAll,        /* caching instructions */
		       NULL);             /* destructor function */
#endif

    XtSetTypeConverter(XtRString,         /* source type */
		       XmRXmStringTable,          /* target type */
		       _XmCvtStringToStringTable, /* converter routine */
		       NULL,              /* args for converter routine */
		       0,                 /* number of args to converter routine */
		       XtCacheAll,        /* caching instructions */
		       _XmDestroyStringTable);    /* destructor function */

#ifdef NONSTANDARD_EXTENSIONS
   XtSetTypeConverter(XmRString,  /* source type */
                      XmRPixmap,  /* target type */
                      _XmeCvtStringToPixmap, /* converter routine */
                      args,       /* args for converter routine */
                      XtNumber(args), /* number of args for converter */
                      XtCacheNone, /* caching instructions */
                      NULL);      /* destructor function */

   XtSetTypeConverter(XmRString,  /* source type */
                      XmRXmBackgroundPixmap,  /* target type */
                      _XmeCvtStringToPixmap, /* converter routine */
                      args,       /* args for converter routine */
                      XtNumber(args), /* number of args for converter */
                      XtCacheNone, /* caching instructions */
                      NULL);      /* destructor function */
#endif
}

/*
 * error convenience
 */
void
#ifdef __STDC__
_XmError(Widget w, char *fmt, ...) {
    va_list arg_list;
    char buf[256];

    va_start(arg_list, fmt);
#else
_XmError(w, fmt, va_alist)
        Widget w;
	char *fmt;
	va_dcl
{
    char buf[256];
    va_list arg_list;

    va_start(arg_list);
#endif
    if (w) {
	vsprintf(buf, fmt, arg_list);
	va_end(arg_list);

	XtAppError(XtWidgetToApplicationContext(w), buf);
    }
    else {
	vsprintf(buf, fmt, arg_list);
	va_end(arg_list);

	XtError(buf);
    }
}

/*
 * error convenience
 */
void
#ifdef __STDC__
_XmWarning(Widget w, char *fmt, ...) {
    va_list arg_list;
    char buf[256];

    va_start(arg_list, fmt);
#else
_XmWarning(Widget w, fmt, va_list)
	Widget w;
	char *fmt;
	va_dcl
{
    char buf[256];

    va_start(arg_list);
#endif
    if (w) {
	sprintf(buf, "\n    Name: %s\n    Class: %s\n    ", XtName(w), XtClass(w)->core_class.class_name);

	vsprintf(buf + strlen(buf), fmt, arg_list);
	va_end(arg_list);

	strcat(buf, "\n");

	XtAppWarning(XtWidgetToApplicationContext(w), buf);
    }
    else {
	buf[0] = '\0';
	vsprintf(buf + strlen(buf), fmt, arg_list);
	va_end(arg_list);

	XtWarning(buf);
    }
}

/* Motif 2.* version of the above */
void
#ifdef __STDC__
XmeWarning(Widget w, char *fmt, ...) {
    va_list arg_list;
    char buf[256];

    va_start(arg_list, fmt);
#else
XmeWarning(Widget w, fmt, va_list)
	Widget w;
	char *fmt;
	va_dcl
{
    char buf[256];

    va_start(arg_list);
#endif
    if (w) {
	sprintf(buf, "\n    Name: %s\n    Class: %s\n    ", XtName(w), XtClass(w)->core_class.class_name);

	vsprintf(buf + strlen(buf), fmt, arg_list);
	va_end(arg_list);

	strcat(buf, "\n");

	XtAppWarning(XtWidgetToApplicationContext(w), buf);
    }
    else {
	buf[0] = '\0';
	vsprintf(buf + strlen(buf), fmt, arg_list);
	va_end(arg_list);

	XtWarning(buf);
    }
}

void
XmCvtStringToUnitType(XrmValuePtr args, Cardinal *num_args,
		      XrmValue *from_val, XrmValue *to_val)
{
}

char *
XmRegisterSegmentEncoding(char *fontlist_tag, char *ct_encoding)
{
    return NULL;
}

char *
XmMapSegmentEncoding(char *fontlist_tag)
{
    return NULL;
}

XmString
XmCvtCTToXmString(char *text)
{
    return XmStringCreate(text, XmFONTLIST_DEFAULT_TAG);
}

Boolean
XmCvtTextToXmString(Display *display, XrmValuePtr args,
		    Cardinal *num_args, XrmValue *from_val, XrmValue *to_val,
		    XtPointer *converter_data)
{
    return False;
}

char *
XmCvtXmStringToCT(XmString string)
{
    char *text = (char *) NULL;
    XmStringContext context = NULL;
    char *pbuf = (char *) NULL, *pstr;
    XmStringCharSet tag;
    XmStringDirection direction;
    Boolean separator;

    if(XmStringInitContext(&context,string) == TRUE)
    {
        while(XmStringGetNextSegment(context, 
                                     &text, 
                                     &tag, 
                                     &direction, 
                                     &separator) == TRUE)
        {
            if(pbuf == NULL)
            {
                /* allocate space for the buffer and init it */
                pbuf = XtMalloc(strlen(text)+2);
                *pbuf = '\0';
            }
            else
            {
                /* allocate more space for the buffer */
                pbuf = XtRealloc(pbuf, sizeof(pbuf)+strlen(text)+2);
            }
   
            /* reset the pointer pstr to point back at pbuf */
            pstr = pbuf;
            
            pstr += (strlen(strcat(pstr, text)));
            if(separator == TRUE)
            {  /* add newline */
               *pstr++ = '\n';
               *pstr = '\0';
            }

            /* free the text segment we just grabbed */
            XtFree(text);
        }
        text = pbuf;
    }  

    return text;
}

Boolean
XmCvtXmStringToText(Display *display, XrmValuePtr args,
		    Cardinal *num_args, XrmValue *from_val, XrmValue *to_val,
		    XtPointer *converter_data)
{
    return False;
}

/*
 * Compare two strings to see if they're equivalent. This one is used
 * whenever comparing motif constants with resource string values. The
 * motif constants must have the Xm prefix stripped off and must be
 * in lower case.
 */
Boolean
_XmStringsAreEqual(char *in_str,
		   char *text_str)
{
    if (in_str[0] && (tolower(in_str[0]) == 'x') &&
	in_str[1] && (tolower(in_str[1]) == 'm')) {
	in_str += 2; /* skip the Xm prefix */
    }

    while (*in_str) {
	if (tolower(*in_str) != *text_str)
	    return False;
	in_str++; text_str++;
    }

    return *text_str ? False : True;
}

/* Motif 2.* version of the above */
/* Note names differ more than _Xm -> Xme */
Boolean
XmeNamesAreEqual(char *in_str,
		   char *text_str)
{
	return _XmStringsAreEqual(in_str, text_str);
}

/*
 * Fixed this thing to make a COPY of the fontlist it returns.
 * Danny 16/4/1996
 */
XmFontList
_XmGetDefaultFontList(Widget w,
		      unsigned char fontListType)
{
    static XmFontList labelFontList = NULL;
    static XmFontList buttonFontList = NULL;
    static XmFontList textFontList = NULL;
    Widget par = w;
    XmFontList fontlist;
    XmVendorShellExtObject ve = NULL;

    fontlist = NULL;
    switch (fontListType)
    {
    case XmTEXT_FONTLIST:
	while ((par = XtParent(par)) != NULL) {
	    if (XmIsBulletinBoard(par) && BB_TextFontList(par) != NULL) {
		fontlist = BB_TextFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par)) {
		/* Danny, I found out the hard way that this function can
		 * get called before the VSEP child is valid, but the context
		 * has been set.  It was with that UIL compiler you sent.
		 * Thus the next three ve checks have been converted.
		 * - MLM
		 */
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_TextFontList(ve) != NULL) {
		    fontlist = VSEP_TextFontList(ve);
		    break;
		}
	    }
	}
	if (fontlist)
	    return XmFontListCopy(fontlist);

	if (!textFontList)
	{
	    XmFontListEntry newEntry = XmFontListEntryLoad(XtDisplay(w),
							   XmDEFAULT_FONT,
							   XmFONT_IS_FONT,
							   XmFONTLIST_DEFAULT_TAG);
	    textFontList = XmFontListAppendEntry(NULL, newEntry);
	}
	return XmFontListCopy(textFontList);
	break;

    case XmBUTTON_FONTLIST:
	while ((par = XtParent(par)) != NULL) {
	    if (XmIsBulletinBoard(par) && BB_ButtonFontList(par) != NULL) {
		fontlist = BB_ButtonFontList(par);
		break;
	    }
	    else if (XmIsMenuShell(par) && MS_ButtonFontList(par) != NULL) {
		fontlist = MS_ButtonFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par)) {
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_ButtonFontList(ve) != NULL) {
		    fontlist = VSEP_LabelFontList(ve);
		    break;
		}
	    }
	}
	if (fontlist)
	    return XmFontListCopy(fontlist);

	if (!buttonFontList)
	{
	    XmFontListEntry newEntry = XmFontListEntryLoad(XtDisplay(w),
							   XmDEFAULT_FONT,
							   XmFONT_IS_FONT,
							   XmFONTLIST_DEFAULT_TAG);
	    buttonFontList = XmFontListAppendEntry(NULL, newEntry);
	}
	    
	return XmFontListCopy(buttonFontList);
	break;
    default:
    case XmLABEL_FONTLIST:
	while ((par = XtParent(par)) != NULL) {
	    if (XmIsBulletinBoard(par) && BB_LabelFontList(par) != NULL) {
		fontlist = BB_LabelFontList(par);
		break;
	    }
	    else if (XmIsMenuShell(par) && MS_LabelFontList(par) != NULL) {
		fontlist = MS_LabelFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par)) {
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_LabelFontList(ve) != NULL) {
		    fontlist = VSEP_LabelFontList(ve);
		    break;
		}
	    }
	}
	if (fontlist)
	    return XmFontListCopy(fontlist);

	if (!labelFontList)
	{
	    XmFontListEntry newEntry = XmFontListEntryLoad(XtDisplay(w),
							   XmDEFAULT_FONT,
							   XmFONT_IS_FONT,
							   XmFONTLIST_DEFAULT_TAG);
	    labelFontList = XmFontListAppendEntry(NULL, newEntry);
	}
	return XmFontListCopy(labelFontList);

	break;
    }
}

/* resource converters, of various types */

extern char *
_XmConvertCSToString(XmString cs)
{
    return NULL;
}

extern Boolean
_XmCvtXmStringToCT(XrmValue *from,
		   XrmValue *to)
{
    return False;
}

/*
 * One to convert from String (char *) to XmString
 */
Boolean
_XmCvtStringToXmString(Display *display,
		       XrmValue *args,
		       Cardinal *num_args,
		       XrmValue *from,
		       XrmValue *to,
		       XtPointer *converter_data)
{    
    static XmString newString = NULL;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToXmString",
		     "XtToolkitError", "String to XmString conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);
    newString = XmStringCreateLtoR(from->addr, XmFONTLIST_DEFAULT_TAG);

   if (newString)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&newString;
               to->size = sizeof(XmString);
       } else {
               if (to->size >= sizeof(XmString)) {
                       *((XmString *)to->addr) = newString;
                       to->size = sizeof(XmString);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRXmString);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRXmString);

    XdbDebug(__FILE__, NULL, "_XmCvtStringToXmString(%s) => %p\n", from->addr, to->addr);

    return True;
}

Boolean
_XmCvtStringToStringTable (Display *display, XrmValuePtr args,
                           Cardinal *num_args, XrmValue *from_val, 
                           XrmValue *to_val, XtPointer *converter_data)
{
  int len = from_val->size;
  int i, start, item, num_items;
  XmStringTable string_table;

  /* Copy the data so it can be stomped on below */
  char* str = (char*)XtMalloc (from_val->size + 1);
  bcopy (from_val->addr, str, from_val->size);

  /* Find the number of strings */
  for (num_items = 0, i = 0; i < len; num_items++)
    {
      while (i < len && isspace (str[i])) i++;
      if (i==len) break;
      while (i < len && str[i++] != ',')
	;
    }

  string_table = (XmStringTable)XtMalloc (sizeof (XmString) * num_items);

  /* Put the strings in the string table */
  for (i = 0, item = 0; item < num_items; item++)
    {
      /* Skip whitespace */
      while (i < len && isspace (str[i])) i++;
      if (i == len) break;

      /* Null terminate the string */
      start = i;
      while (i < len && str[i] != ',') i++;
      str[i] = '\0';

      /* Create the XmString */
      string_table[item] = XmStringCreateSimple (&str[start]);
      i++;
    }

  XtFree ((XtPointer)str);

  to_val->addr = (XtPointer)string_table;
  to_val->size = sizeof (XmString) * num_items;

  return True;
}

void
_XmDestroyStringTable (XtAppContext app, XrmValue* to, 
		       XtPointer converer_data, XrmValue* args, 
		       Cardinal* num_args)
{
  int i;
  XmStringTable strings = (XmStringTable)to->addr;
  for (i = 0; i < to->size / sizeof (XmString); i++)
    XmStringFree (strings[i]);
}

/* This function is only used by the _XmCvtStringToFontlist resource
   converter */
XmFontList
__XmFontListResourceAddEntry(Display *display,
			     char *full_entry,
			     XmFontList oldFontList)
{
    XmFontList newFontList;
    char *eq, *end, *fn, *ft, *tag, *font;
	
    
    eq = strchr(full_entry, (int)'=');
    
    if (eq) /* there was an equal sign, the left side is the font,
	       and the right side the tag. */
    {
	/* set the equal sign to a null so we can XtNewString the font */
	*eq = 0;
	fn = font = XtNewString(full_entry);
	
	/* set it back again, and XtNewString the tag */
	*eq = '=';
	ft = tag = XtNewString(eq + 1);

	end = font + strlen(font);
	while (isspace(*font) && font < end)
	    font++;
	while (isspace(*end) && end > font)
	    end--;
	if (end != font + strlen(font)) {
	    end++;
	    *end = 0;
	}
	end = tag + strlen(tag);
	while (isspace(*tag) && tag < end)
	    tag++;
	while (isspace(*end) && end > tag)
	    end--;
	if (end != tag + strlen(tag)) {
	    end++;
	    *end = 0;
	}
	newFontList = XmFontListAppendEntry(oldFontList, 
					    XmFontListEntryLoad(display, 
								font,
								XmFONT_IS_FONT,
								tag));
	
	XtFree(ft);
	XtFree(fn);
    }
    else {
	fn = font = XtNewString(full_entry);

	end = font + strlen(font);
	while (isspace(*font) && font < end)
	    font++;
	while (isspace(*end) && end > font)
	    end--;
	if (end != font + strlen(font)) {
	    end++;
	    *end = 0;
	}
	newFontList = XmFontListAppendEntry(oldFontList, 
					    XmFontListEntryLoad(display, 
								full_entry,
								XmFONT_IS_FONT,
								XmFONTLIST_DEFAULT_TAG));
	XtFree(fn);
    }

    XdbDebug(__FILE__, NULL, "__XmFontListResourceAddEntry() => 0x%X\n", newFontList);
    return newFontList;
}
			     
/*
 * This thing should do reference counting, or make copies
 * Danny 16/4/1996
 */
Boolean
_XmCvtStringToFontlist(Display *display,
		       XrmValue *args,
		       Cardinal *num_args,
		       XrmValue *from,
		       XrmValue *to,
		       XtPointer *converter_data)
{    
    XmFontList newFontList = NULL;
    char *p, *end, *font, *fn, *buf;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToFontlist",
		     "XtToolkitError", "String to Fontlist conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    /* there is an implicit rule here.  strtok() modifies the passed in buffer
     * at least on Linux, so if a comma is found, a \0 is put there.  This is
     * bad, as this memory didn't come from us, but from the Intrinsics, and
     * may get used again (especially if it's in the resource cache)! */

    buf = XtNewString(from->addr);

    p = strtok(buf, ",");

    if (p) /* if there was actually a comma character in the string. */
    {
	do {
	    fn = font = XtNewString(p);

	    end = font + strlen(font);
	    while (isspace(*font) && font < end)
		font++;
	    while (isspace(*end) && end > font)
		end--;
	    if (end != font + strlen(font)) {
		end++;
		*end = 0;
	    }

	    newFontList = __XmFontListResourceAddEntry(display, font, newFontList);

	    XtFree(fn);
	    p = strtok(NULL, ",");

	} while (p != NULL );

	XtFree(buf);
    }
    else {
	fn = font = buf;

	end = font + strlen(font);
	while (isspace(*font) && font < end)
	    font++;
	while (isspace(*end) && end > font)
	    end--;
	if (end != font + strlen(font)) {
	    end++;
	    *end = 0;
	}
	newFontList = __XmFontListResourceAddEntry(display, font, newFontList);

	XtFree(buf);
    }

    if (newFontList) {
	if (to->addr == NULL) {
	    to->addr = (XtPointer)&newFontList;
	    to->size = sizeof(XmFontList);
	}
	else {
	    if (to->size >= sizeof(XmFontList)) {
		*((XmFontList *)to->addr) = newFontList;
		to->size = sizeof(XmFontList);
	    }
	    else
		XtDisplayStringConversionWarning(display, (char*)from->addr,
						 XmRFontList);
	}
    }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr,
					 XmRFontList);

    return True;
}


/* One from String to XmNavigationType */
Boolean
_XmCvtStringToXmNavigation(Display *display,
			   XrmValue *args,
			   Cardinal *num_args,
			   XrmValue *from,
			   XrmValue *to,
			   XtPointer *converter_data)
{
    static XmNavigationType navType;

    navType = 10;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToXmNavigation",
		     "XtToolkitError", "String to XmNavigation conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    if (!strcmp((char*)from->addr, "NONE"))
	navType = XmNONE;
    else if (!strcmp((char*)from->addr, "TAB_GROUP"))
	navType = XmTAB_GROUP;
    else if (!strcmp((char*)from->addr, "STICKY_TAB_GROUP"))
	navType = XmSTICKY_TAB_GROUP;
    else if (!strcmp((char*)from->addr, "EXCLUSIVE_TAB_GROUP"))
	navType = XmEXCLUSIVE_TAB_GROUP;

    if (navType != 10)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&navType;
               to->size = sizeof(XmNavigationType);
       } else {
               if (to->size >= sizeof(XmNavigationType)) {
                       *((XmNavigationType *)to->addr) = navType;
                       to->size = sizeof(XmNavigationType);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRNavigationType);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRNavigationType);

    return True;
}

/* One from String to {Horizontal,Vertical}Dimension */
Boolean
_XmCvtStringToDimension(Display *display,
			XrmValue *args,
			Cardinal *num_args,
			XrmValue *from,
			XrmValue *to,
			XtPointer *converter_data)
{
    static Dimension dim;

    dim = (Dimension)XmUNSPECIFIED;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToDimension",
		     "XtToolkitError", "String to XmRDimension conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    dim = (Dimension)atoi((char*)from->addr);

    if (dim != (Dimension)XmUNSPECIFIED)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&dim;
               to->size = sizeof(Dimension);
       } else {
               if (to->size >= sizeof(Dimension)) {
                       *((Dimension *)to->addr) = dim;
                       to->size = sizeof(Dimension);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalDimension);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalDimension);

    return True;
}


Boolean
_XmCvtStringToKeySym(Display *d, XrmValue *args, Cardinal *nargs,
		XrmValue *from, XrmValue *to, XtPointer *data)
{
	static KeySym	k;

	k = XStringToKeysym((char *)from->addr);

	if (k != NoSymbol) {
	    if (to->addr == NULL) {
		to->addr = (XtPointer)&k;
		to->size = sizeof(k);
		return True;
	    } else {
		if (to->size >= sizeof(KeySym)) {
			*((KeySym *)to->addr) = k;
			to->size = sizeof(k);
			return True;
		} else {
		    XtDisplayStringConversionWarning(d, (char*)from->addr, XmRKeySym);
		    XdbDebug(__FILE__, NULL, "_XmCvtStringToKeySym fail at line %d\n", __LINE__);
		    return False;
		}
	    }
	}
	XtDisplayStringConversionWarning(d, (char*)from->addr, XmRKeySym);
	XdbDebug(__FILE__, NULL, "_XmCvtStringToKeySym fail at line %d\n", __LINE__);
	return False;
}

Boolean
_XmCvtStringToPosition(Display *display,
		       XrmValue *args,
		       Cardinal *num_args,
		       XrmValue *from,
		       XrmValue *to,
		       XtPointer *converter_data)
{
    static Position pos;

    pos = (Position)XmUNSPECIFIED;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToPosition",
		     "XtToolkitError", "String to XmRPosition conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    pos = (Position)atoi((char*)from->addr);

    if (pos != (Position)XmUNSPECIFIED)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&pos;
               to->size = sizeof(Position);
       } else {
               if (to->size >= sizeof(Position)) {
                       *((Position *)to->addr) = pos;
                       to->size = sizeof(Position);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalPosition);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalPosition);

    return True;
}


Boolean
_XmCvtStringToVerticalPosition(Display *display,
			       XrmValue *args,
			       Cardinal *num_args,
			       XrmValue *from,
			       XrmValue *to,
			       XtPointer *converter_data)
{
    static Position pos;

    pos = (Position)XmUNSPECIFIED;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToVerticalPosition",
		     "XtToolkitError", "String to XmRVerticalPosition conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    pos = (Position)atoi((char*)from->addr); /* FIX ME: error checks */

    if (pos != (Position)XmUNSPECIFIED)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&pos;
               to->size = sizeof(Position);
       } else {
               if (to->size >= sizeof(Position)) {
                       *((Position *)to->addr) = pos;
                       to->size = sizeof(Position);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRVerticalPosition);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRVerticalPosition);

    return True;
}


Boolean
_XmCvtStringToHorizontalPosition(Display *display,
				 XrmValue *args,
				 Cardinal *num_args,
				 XrmValue *from,
				 XrmValue *to,
				 XtPointer *converter_data)
{
    static Position pos;

    pos = (Position)XmUNSPECIFIED;

    if (*num_args != 0)
	XtWarningMsg("wrongParameters", "cvtStringToPosition",
		     "XtToolkitError", "String to XmRHorizontalPosition conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *) NULL);

    pos = (Position)atoi((char*)from->addr); /* FIX ME: error checks */

    if (pos != (Position)XmUNSPECIFIED)
       if (to->addr == NULL) {
               to->addr = (XtPointer)&pos;
               to->size = sizeof(Position);
       } else {
               if (to->size >= sizeof(Position)) {
                       *((Position *)to->addr) = pos;
                       to->size = sizeof(Position);
               } else {
                       XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalPosition);
               }
       }
    else
	XtDisplayStringConversionWarning(display, (char*)from->addr, XmRHorizontalPosition);

    return True;
}

#ifdef NONSTANDARD_EXTENSIONS
/*
 * _XmeCvtStringToPixmap is not part of the OSF/Motif API.
 * This routine is used to convert a string to a pixmap.
 * This is done by calling the _XmeGetPixmap routine with the
 * supplied string.  
 */
static Boolean
_XmeCvtStringToPixmap(Display *dpy,
                     XrmValue *args,
                     Cardinal *num_args,
                     XrmValue *from,
                     XrmValue *to,
                     XtPointer *converter_data)
{
   static Pixmap _pmap;
   Screen *screen;
   char *name;
   Pixmap _XmeGetPixmap(Screen *, char *);

   if (*num_args != 1)
   {
      XtWarningMsg("wrongParameters",
                 "cvtStringToPixmap",
                 "XtToolkitError",
                 "String to Pixmap conversion needs screen argument",
                 (String *) NULL,
                 (Cardinal *) NULL);
   }             

   /* get arguments */
   screen = *((Screen **) args[0].addr);
   name = (char *)from->addr;
                 
   /* over kill check */
   if (name == NULL
       || strcmp(name,"None") == 0
       || strcmp(name,"XmUNSPECIFIED_PIXMAP") == 0)
   {             
      _pmap = XmUNSPECIFIED_PIXMAP;
   }             
   else {        
      _pmap = _XmeGetPixmap(screen, name);
   }             
                 
   if (to->addr == NULL)
   {             
      to->addr = (XPointer)&_pmap;
      to->size = sizeof(Pixmap);
   }             
   else          
   {  
      if (to->size >= sizeof(Pixmap))
      {          
         *((Pixmap *)to->addr) = _pmap;
         to->size = sizeof(Pixmap);
      }          
      else       
      {  
         XtDisplayStringConversionWarning(dpy, (char*)from->addr, XmRPixmap);
      }          
   }             
      
   converter_data = converter_data; /* keeps compiler happy */
                 
   return True;  
}             
#endif
