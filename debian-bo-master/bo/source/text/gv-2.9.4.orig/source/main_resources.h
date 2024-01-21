/*
**
** main_resources.h
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

#ifndef _GV_MAIN_RESOURCES_H_ 
#define _GV_MAIN_RESOURCES_H_


/* Application resources */

typedef struct _AppResources {
    Boolean	auto_center;		/* whether to automatically center the page */
    int		minimum_magstep;	/* smallest magstep allowed */
    int		maximum_magstep;	/* largest magstep allowed */
    int		magstep;		/* default magstep */
    String      default_orientation;	/* default orientation */
    String      fallback_orientation;	/* fallback orientation */
    String	page;			/* first page to show */
    String	default_pagemedia;	/* default page media */
    String	fallback_pagemedia;	/* fallback page media */
#   ifdef USE_SWAP_LANDSCAPE
       Boolean	swap_landscape;		/* Landscape comment maps to Seascape */
#   endif
    String	scratch_dir;		/* temporary directory */
    String	default_save_dir;	/* default directory for saving */
    Boolean	confirm_print;		/* popup dialog on print attempt */
    String	version;		/* ghostview version identifier*/
    String	maximum_width;		/* maximum width of the application*/
    String	maximum_height;		/* maximum height of the application*/
    int		minimum_width;		/* minimum width of the application*/
    int		minimum_height;		/* minimum height of the application*/
    Boolean	auto_resize;		/* should we try to fit the window size to the page size */
    Pixmap	document_bitmap;
    Pixmap	selected_bitmap;
    String	locator_format;
    Boolean	antialias;
    String	mag_menu_entries;
    Pixel	highlight_pixel;
    Boolean	reverse_scrolling;
    Boolean	respect_dsc;
} AppResources;


#ifndef _GV_MAIN_C_
extern
#endif
AppResources app_res;


/*-------------------------------------------------------------*/

#ifdef _GV_MAIN_C_

#define GV_DEFAULT_VERSION "?"

static XtResource resources[] = {
    {"autoCenter", "AutoCenter", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, auto_center), XtRImmediate, (XtPointer)True},
    {"minimumMagstep", "MinimumMagstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, minimum_magstep), XtRImmediate, (XtPointer)-11},
    {"maximumMagstep", "MaximumMagstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, maximum_magstep), XtRImmediate, (XtPointer)5},
    {"magstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, magstep), XtRImmediate, (XtPointer)0},
    {"orientation", "Orientation", XtRString, sizeof(String),
     XtOffsetOf(AppResources, default_orientation), XtRImmediate, "Automatic"},
    {"fallbackOrientation", "FallbackOrientation", XtRString, sizeof(String),
     XtOffsetOf(AppResources, fallback_orientation), XtRImmediate, "Portrait"},
    {"page", "Page", XtRString, sizeof(String),
     XtOffsetOf(AppResources, page), XtRImmediate, NULL},
    {"pageMedia", "PageMedia", XtRString, sizeof(String),
     XtOffsetOf(AppResources, default_pagemedia), XtRImmediate, "Automatic"},
    {"fallbackPageMedia", "FallbackPageMedia", XtRString, sizeof(String),
     XtOffsetOf(AppResources, fallback_pagemedia), XtRImmediate, "A4"},
#ifdef USE_SWAP_LANDSCAPE
    {"swapLandscape", "SwapLandscape", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, swap_landscape), XtRImmediate, (XtPointer)False},
#endif
    {"scratchDir", "ScratchDir", XtRString, sizeof(String),
     XtOffsetOf(AppResources, scratch_dir), XtRImmediate,"?"},
    {"defaultSaveDir", "DefaultSaveDir", XtRString, sizeof(String),
     XtOffsetOf(AppResources, default_save_dir), XtRImmediate,"?"},
    {"confirmPrint", "ConfirmPrint", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, confirm_print), XtRImmediate, (XtPointer)True},
    {"version", "Version", XtRString, sizeof(String),
     XtOffsetOf(AppResources, version), XtRImmediate,GV_DEFAULT_VERSION},
    {"autoResize", "AutoResize", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, auto_resize), XtRImmediate, (XtPointer)True},
    {"maximumWidth", "MaximumWidth", XtRString, sizeof(String),
     XtOffsetOf(AppResources, maximum_width), XtRImmediate, (XtPointer)"screen-20"},
    {"maximumHeight", "MaximumHeight", XtRString, sizeof(String),
     XtOffsetOf(AppResources, maximum_height), XtRImmediate, (XtPointer)"screen-44"},
    {"minimumWidth", "MinimumWidth", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, minimum_width), XtRImmediate, (XtPointer)400},
    {"minimumHeight", "MinimumHeight", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, minimum_height), XtRImmediate, (XtPointer)400},
    {"selectedBitmap", "SelectedBitmap", XtRBitmap, sizeof(Pixmap),
     XtOffsetOf(AppResources,selected_bitmap), XtRImmediate, (XtPointer)None},
    {"documentBitmap", "DocumentBitmap", XtRBitmap, sizeof(Pixmap),
     XtOffsetOf(AppResources,document_bitmap), XtRImmediate, (XtPointer)None},
    {"locatorFormat", "LocatorFormat", XtRString, sizeof(String),
     XtOffsetOf(AppResources,locator_format),   XtRImmediate, (XtPointer)"%d x %d"},
    {"antialias", "Antialias", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, antialias), XtRImmediate, (XtPointer)False},
    {"magMenuEntries", "MagMenuEntries", XtRString, sizeof(String),
     XtOffsetOf(AppResources,mag_menu_entries), XtRImmediate, (XtPointer)NULL},
    {"highlightPixel", "HighlightPixel", XtRPixel, sizeof(Pixel),
     XtOffsetOf(AppResources,highlight_pixel), XtRString, (XtPointer)XtDefaultBackground }, 
    {"reverseScrolling", "ReverseScrolling", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, reverse_scrolling), XtRImmediate, (XtPointer)False},
    {"respectDSC", "RespectDSC", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, respect_dsc), XtRImmediate, (XtPointer)True},
};

static XrmOptionDescRec options[] = {
 { "-monochrome"	, "*Ghostview.palette"		, XrmoptionNoArg	, "Monochrome"	},
 { "-grayscale"		, "*Ghostview.palette"		, XrmoptionNoArg	, "Grayscale"	},
 { "-color"		, "*Ghostview.palette"		, XrmoptionNoArg	, "Color"	},
 { "-page"		, ".page"			, XrmoptionSepArg	, NULL		},
 { "-center"		, ".autoCenter"			, XrmoptionNoArg	, "True"	},
 { "-nocenter"		, ".autoCenter"			, XrmoptionNoArg	, "False"	},
 { "-xdpi"		, "*Ghostview.xdpi"		, XrmoptionSepArg	, NULL		},
 { "-ydpi"		, "*Ghostview.ydpi"		, XrmoptionSepArg	, NULL		},
 { "-dpi"		, "*Ghostview.Resolution"	, XrmoptionSepArg	, NULL		},
 { "-resolution"	, "*Ghostview.Resolution"	, XrmoptionSepArg	, NULL		},
 { "-letter"            , ".pageMedia"                  , XrmoptionNoArg        , "Letter"      },
 { "-tabloid"           , ".pageMedia"                  , XrmoptionNoArg        , "Tabloid"     },
 { "-ledger"            , ".pageMedia"                  , XrmoptionNoArg        , "Ledger"      },
 { "-legal"             , ".pageMedia"                  , XrmoptionNoArg        , "Legal"       },
 { "-statement"         , ".pageMedia"                  , XrmoptionNoArg        , "Statement"   },
 { "-executive"         , ".pageMedia"                  , XrmoptionNoArg        , "Executive"   },
 { "-a0"                , ".pageMedia"                  , XrmoptionNoArg        , "A0"          },
 { "-a1"                , ".pageMedia"                  , XrmoptionNoArg        , "A1"          },
 { "-a2"                , ".pageMedia"                  , XrmoptionNoArg        , "A2"          },
 { "-a3"                , ".pageMedia"                  , XrmoptionNoArg        , "A3"          },
 { "-a4"                , ".pageMedia"                  , XrmoptionNoArg        , "A4"          },
 { "-a5"                , ".pageMedia"                  , XrmoptionNoArg        , "A5"          },
 { "-b4"                , ".pageMedia"                  , XrmoptionNoArg        , "B4"          },
 { "-b5"                , ".pageMedia"                  , XrmoptionNoArg        , "B5"          },
 { "-folio"             , ".pageMedia"                  , XrmoptionNoArg        , "Folio"       },
 { "-quarto"            , ".pageMedia"                  , XrmoptionNoArg        , "Quarto"      },
 { "-10x14"             , ".pageMedia"                  , XrmoptionNoArg        , "10x14"       },
 { "-portrait"          , ".orientation"                , XrmoptionNoArg        , "portrait"    },
 { "-landscape"         , ".orientation"                , XrmoptionNoArg        , "landscape"   },
 { "-seascape"          , ".orientation"                , XrmoptionNoArg        , "seascape"    },
 { "-upsidedown"        , ".orientation"                , XrmoptionNoArg        , "upsidedown"  },
 { "-magstep"		, ".magstep"			, XrmoptionSepArg	, NULL		},
 { "-resize"		, ".autoResize"			, XrmoptionNoArg	, "True"	},
 { "-noresize"		, ".autoResize"			, XrmoptionNoArg	, "False"	},
#ifdef USE_SWAP_LANDSCAPE
 { "-swap"		, ".swapLandscape"		, XrmoptionNoArg	, "True"	},
 { "-noswap"		, ".swapLandscape"		, XrmoptionNoArg	, "False"	},
#endif
 { "-antialias"		, ".antialias"			, XrmoptionNoArg	, "True"	},
 { "-noantialias"	, ".antialias"			, XrmoptionNoArg	, "False"	},
 { "-dsc"		, ".respectDSC"			, XrmoptionNoArg	, "True"	},
 { "-nodsc"		, ".respectDSC"			, XrmoptionNoArg	, "False"	},
};

#endif /* _GV_MAIN_C_ */
/*-------------------------------------------------------------*/

#endif /* _GV_MAIN_RESOURCES_H_ */
