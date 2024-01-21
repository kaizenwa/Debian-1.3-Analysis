/*
 * tixInt.h --
 *
 *	Defines internal data types and functions used by the Tix library.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
#ifndef _TIX_INT_H_
#define _TIX_INT_H_

#ifndef _TIX_H_
#include <tix.h>
#endif

/*----------------------------------------------------------------------
 *
 * 		Tix Display Item Types
 *
 *----------------------------------------------------------------------
 */

#define TIX_DITEM_NONE      	 	0
#define TIX_DITEM_TEXT      	 	1
#define TIX_DITEM_IMAGETEXT 	 	2
#define TIX_DITEM_WINDOW 	 	3
#define TIX_DITEM_IMAGE 	 	4

/* The flags for drawing */

#define TIX_DITEM_NORMAL_BG		(0x1 <<  0)
#define TIX_DITEM_ACTIVE_BG		(0x1 <<  1)
#define TIX_DITEM_SELECTED_BG		(0x1 <<  2)
#define TIX_DITEM_DISABLED_BG		(0x1 <<  3)
#define TIX_DITEM_NORMAL_FG		(0x1 <<  4)
#define TIX_DITEM_ACTIVE_FG		(0x1 <<  5)
#define TIX_DITEM_SELECTED_FG		(0x1 <<  6)
#define TIX_DITEM_DISABLED_FG		(0x1 <<  7)
#define TIX_DITEM_FONT			(0x1 <<  8)
#define TIX_DITEM_PADX			(0x1 <<  9)
#define TIX_DITEM_PADY			(0x1 << 10)

#if  0
    /* %bordercolor not used */
#define TIX_DITEM_BORDER_COLOR		(0x1 << 11)
#define TIX_DITEM_BORDER_WIDTH		(0x1 << 12)
#define TIX_DITEM_RELIEF		(0x1 << 13)
#define TIX_DITEM_BOTTOM		(0x1 << 14)
#define TIX_DITEM_RIGHT			(0x1 << 15)
#endif

#define TIX_DONT_CALL_CONFIG		TK_CONFIG_USER_BIT

/* These values are used ONLY for indexing the color array in
 * Tix_StyleTemplate  */

#define TIX_DITEM_NORMAL		0
#define TIX_DITEM_ACTIVE		1
#define TIX_DITEM_SELECTED		2
#define TIX_DITEM_DISABLED		3

/* Flags for MultiInfo */
#define TIX_CONFIG_INFO			1
#define TIX_CONFIG_VALUE		2

typedef union  Tix_DItem 		Tix_DItem;
typedef union  Tix_DItemStyle 		Tix_DItemStyle;
typedef struct Tix_DItemInfo 		Tix_DItemInfo;
typedef struct Tix_DispData 		Tix_DispData;
typedef struct Tix_StyleTemplate	Tix_StyleTemplate;

typedef struct Tix_DItemLink {
    Tix_DItem *iPtr;
    struct Tix_DItemLink * next;
}Tix_DItemLink ;

typedef void 		Tix_DItemCalculateSizeProc  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
typedef char * 		Tix_DItemComponentProc _ANSI_ARGS_((
			    Tix_DItem * iPtr, int x, int y));
typedef int 		Tix_DItemConfigureProc _ANSI_ARGS_((
			    Tix_DItem * iPtr, int argc, char ** argv,
			    int flags));
typedef Tix_DItem * 	Tix_DItemCreateProc _ANSI_ARGS_((
			    Tix_DispData * ddPtr,
			    Tix_DItemInfo * diTypePtr));
typedef void		Tix_DItemDisplayProc  _ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flag));
typedef void 		Tix_DItemFreeProc  _ANSI_ARGS_((Tix_DItem * diPtr));
typedef void		Tix_DItemSizeChangedProc  _ANSI_ARGS_((
			    Tix_DItem * iPtr));

typedef void		Tix_DItemStyleChangedProc  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
typedef void		Tix_DItemLostStyleProc  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
typedef int 		Tix_DItemStyleConfigureProc _ANSI_ARGS_((
			    Tix_DItemStyle* style, int argc, char ** argv,
			    int flags));
typedef Tix_DItemStyle*	Tix_DItemStyleCreateProc _ANSI_ARGS_((
			    Tcl_Interp * interp, Tk_Window tkwin,
			    Tix_DItemInfo * diTypePtr, char * name));
typedef void		Tix_DItemStyleFreeProc _ANSI_ARGS_((
			    Tix_DItemStyle* style));
typedef void		Tix_DItemStyleSetTemplateProc _ANSI_ARGS_((
			    Tix_DItemStyle* style,
			    Tix_StyleTemplate * tmplPtr));


/*----------------------------------------------------------------------
 * Tix_DItemInfo --
 *
 *	This structure is used to register a new display item (call
 *	Tix_AddDItemType).
 *----------------------------------------------------------------------
 */
struct Tix_DItemInfo {
    char * name;
    int type;

    /*
     * These info are to communicate with the items
     */
    Tix_DItemCreateProc * createProc;
    Tix_DItemConfigureProc * configureProc;
    Tix_DItemCalculateSizeProc * calculateSizeProc;
    Tix_DItemComponentProc * componentProc;
    Tix_DItemDisplayProc * displayProc;
    Tix_DItemFreeProc * freeProc;
    Tix_DItemStyleChangedProc *styleChangedProc;
    Tix_DItemLostStyleProc * lostStyleProc;

    /*
     * These info are to communicate with the styles
     */
    Tix_DItemStyleCreateProc * styleCreateProc;
    Tix_DItemStyleConfigureProc * styleConfigureProc;
    Tix_DItemStyleFreeProc * styleFreeProc;
    Tix_DItemStyleSetTemplateProc * styleSetTemplateProc;

    Tk_ConfigSpec * itemConfigSpecs;
    Tk_ConfigSpec * styleConfigSpecs;
    struct Tix_DItemInfo * next;
};

/*----------------------------------------------------------------------
 * Tix_DispData --
 *
 *	Information needed by the display types to display the item in
 *	an X drawable.
 *----------------------------------------------------------------------
 */
struct Tix_DispData {
    Display * display;
    Tcl_Interp * interp;
    Tk_Window tkwin;
    Tix_DItemSizeChangedProc * sizeChangedProc;
};

/*----------------------------------------------------------------------
 * Tix_StyleTemplate --
 *
 *	A StyleTemplate is used to set the values of the default styles
 *	associated with a widget
 *----------------------------------------------------------------------
 */
struct Tix_StyleTemplate {
    int flags;			/* determines which field is valid */

    struct {
	XColor * bg;
	XColor * fg;
    } colors[4]; 		/* colors for the four basic modes*/

    int pad[2];
#if 0
    /* %bordercolor not used */
    XColor * borderColor;
    Tix_Relief relief;
    int borderWidth;
#endif
    XFontStruct *fontPtr;
};

/*----------------------------------------------------------------------
 *
 *
 * 			Display Item Types
 *
 *
 *----------------------------------------------------------------------
 */

/* Display Styles */
typedef struct TixBaseStyle 		TixBaseStyle;
typedef struct TixImageTextStyle 	TixImageTextStyle;
typedef struct TixImageStyle 		TixImageStyle;
typedef struct TixTextStyle 		TixTextStyle;
typedef struct TixWindowStyle 		TixWindowStyle;

typedef struct TixBaseItem 		TixBaseItem;
typedef struct TixColorStyle		TixColorStyle;
typedef struct TixImageTextItem 	TixImageTextItem;
typedef struct TixImageItem 		TixImageItem;
typedef struct TixTextItem 		TixTextItem;
typedef struct TixWindowItem 		TixWindowItem;

/*----------------------------------------------------------------------
 * TixBaseItem --
 *
 * This is the abstract base class for all display items. All display items
 * should have the data members defined in the BaseItem structure
 *
 */
#define ITEM_COMMON_MEMBERS \
    Tix_DItemInfo * diTypePtr; \
    Tix_DispData * ddPtr; \
    ClientData clientData; \
    int size[2]			/* Size of this element */ \

struct TixBaseItem {
    ITEM_COMMON_MEMBERS;
    TixBaseStyle * stylePtr;
};

/*----------------------------------------------------------------------
 * TixBaseStyle --
 *
 * This is the abstract base class for all display styles. All display items
 * should have the data members defined in the BaseStyle structure
 *
 */
/* This Part is initialized by tixDiStyle.c */

#define STYLE_COMMON_MEMBERS \
    Tcl_Command styleCmd;	/* Token for style's command. */ \
    Tix_DItemLink * items; \
    int refCount; \
    int flags; \
    Tcl_Interp *interp;		/* Interpreter associated with style. */ \
    Tk_Window tkwin;		/* window associated with this style */ \
    Tix_DItemInfo * diTypePtr; \
    Tk_Anchor anchor; \
    char * name; 		/* Name of this style */ \
    int pad[2]			/* paddings */ 


#if 0
    Tix_Relief relief
    /* %bordercolor not used */
    int borderWidth; 
    XColor * borderColor;	/* color of the border when it is displayed 
				 * in "flat border" mode 
				 */ 
    GC borderGC
#endif

#define STYLE_COLOR_MEMBERS \
    struct { \
	XColor * bg; \
	XColor * fg; \
	GC foreGC;   \
	GC backGC;   \
    } colors[4] 		/* colors and GC's for the four basic modes*/

struct TixBaseStyle {
    STYLE_COMMON_MEMBERS;
};

#define TIX_STYLE_DELETED 1
#define TIX_STYLE_DEFAULT 2

/*
 * Abstract type for all styles that have a color element
 */
struct TixColorStyle {
    STYLE_COMMON_MEMBERS;
    STYLE_COLOR_MEMBERS;
};

/*----------------------------------------------------------------------
 * ImageTextItem --
 *
 *	Display an image together with a text string
 *----------------------------------------------------------------------
 */
struct TixImageTextItem {
    ITEM_COMMON_MEMBERS;

    TixImageTextStyle *stylePtr;
	/*-------------------------*/
	/*       Bitmap            */
	/*-------------------------*/
    Pixmap bitmap;
    int bitmapW, bitmapH;	/* Size of bitmap */

	/*-------------------------*/
	/*       Image             */
	/*-------------------------*/
    char *imageString;		/* Name of image to display (malloc'ed), or
				 * NULL.  If non-NULL, bitmap, text, and
				 * textVarName are ignored. */
    Tk_Image image;
    int imageW, imageH;		/* Size of image */

	/*-------------------------*/
	/*       Text             */
	/*-------------------------*/

    char * text;		/* Show descriptive text */
    size_t numChars;		/* Size of text */
    int textW, textH;
    int wrapLength;
    Tk_Justify justify;		/* Justification to use for multi-line text. */
    int underline;		/* Index of character to underline.  < 0 means
				 * don't underline anything. */

    int showImage, showText;
};

struct TixImageTextStyle {
    STYLE_COMMON_MEMBERS;
    STYLE_COLOR_MEMBERS;
    int wrapLength;
    Tk_Justify justify;		/* Justification to use for multi-line text. */
    XFontStruct *fontPtr;
    int gap;			/* Gap between text and image */
};

/*----------------------------------------------------------------------
 * ImageItem --
 *
 *	Displays an image
 *----------------------------------------------------------------------
 */
struct TixImageItem {
    ITEM_COMMON_MEMBERS;

    TixImageStyle *stylePtr;

	/*-------------------------*/
	/*       Image             */
	/*-------------------------*/
    char *imageString;		/* Name of image to display (malloc'ed), or
				 * NULL.  If non-NULL, bitmap, text, and
				 * textVarName are ignored. */
    Tk_Image image;
    int imageW, imageH;		/* Size of image */
};

struct TixImageStyle {
    STYLE_COMMON_MEMBERS;
    STYLE_COLOR_MEMBERS;
};
/*----------------------------------------------------------------------
 * TextItem --
 *
 *	Displays a text string.
 *----------------------------------------------------------------------
 */
struct TixTextItem {
    ITEM_COMMON_MEMBERS;

    TixTextStyle *stylePtr;
	/*-------------------------*/
	/*       Text             */
	/*-------------------------*/

    char * text;		/* Show descriptive text */
    size_t numChars;		/* Size of text */
    int textW, textH;
    int underline;		/* Index of character to underline.  < 0 means
				 * don't underline anything. */
};

struct TixTextStyle {
    STYLE_COMMON_MEMBERS;
    STYLE_COLOR_MEMBERS;
    int wrapLength;
    Tk_Justify justify;		/* Justification to use for multi-line text. */
    XFontStruct *fontPtr;
};

/*----------------------------------------------------------------------
 * WindowItem --
 *
 *	Displays a window.
 *----------------------------------------------------------------------
 */
struct TixWindowItem {
    ITEM_COMMON_MEMBERS;
    TixWindowStyle *stylePtr;
    Tk_Window tkwin;
    struct TixWindowItem * next;
    int serial;
};

struct TixWindowStyle {
    STYLE_COMMON_MEMBERS;
};

/*----------------------------------------------------------------------
 * Tix_DItem and Tix_DItemStyle --
 *
 *	These unions just make it easy to address the internals of the
 *	structures of the display items and styles. If you create a new
 *	display item, you will need to do you type casting yourself.
 *----------------------------------------------------------------------
 */
union Tix_DItem {
    TixBaseItem 	base;
    TixImageTextItem 	imagetext;
    TixTextItem 	text;
    TixWindowItem 	window;
    TixImageItem 	image;
};

union Tix_DItemStyle {
    TixBaseStyle 	base;
    TixColorStyle 	color;
    TixImageTextStyle 	imagetext;
    TixTextStyle 	text;
    TixWindowStyle 	window;
    TixImageStyle 	image;
};

#define Tix_DItemType(x)     	((x)->base.diTypePtr->type)
#define Tix_DItemTypeName(x)    ((x)->base.diTypePtr->name)
#define Tix_DItemWidth(x) 	((x)->base.size[0])
#define Tix_DItemHeight(x)	((x)->base.size[1])
#define Tix_DItemConfigSpecs(x) ((x)->base.diTypePtr->itemConfigSpecs)
#define Tix_DItemPadX(x)     	((x)->base.stylePtr->pad[0])
#define Tix_DItemPadY(x)     	((x)->base.stylePtr->pad[1])

#define TIX_WIDTH  0
#define TIX_HEIGHT 1

/*----------------------------------------------------------------------
 * Tix_ArgumentList --
 * 
 *	This data structure is used to split command arguments for
 *	the display item types
 *----------------------------------------------------------------------
 */
#define FIXED_SIZE 4
typedef struct {
    int argc;
    char ** argv;
} Tix_Argument;

typedef struct {
    Tix_Argument * arg;
    int numLists;
    Tix_Argument preAlloc[FIXED_SIZE];
} Tix_ArgumentList;

/*----------------------------------------------------------------------
 * Tix_ScrollInfo --
 * 
 *	This data structure encapsulates all the necessary operations
 *	for scrolling widgets
 *----------------------------------------------------------------------
 */
#define TIX_SCROLL_INT		1
#define TIX_SCROLL_DOUBLE	2

/* abstract type */
typedef struct Tix_ScrollInfo {
    int type;		/* TIX_SCROLL_INT or TIX_SCROLL_DOUBLE */
    char * command;
} Tix_ScrollInfo;

typedef struct Tix_IntScrollInfo {
    int type;		/* TIX_SCROLL_INT */
    char * command;

    int total;		/* total size (width or height) of the widget*/
    int window;		/* visible size */
    int offset;		/* The top/left side of the scrolled widget */
    int unit;		/* How much should we scroll when the user
			 * press the arrow on a scrollbar? */

} Tix_IntScrollInfo;

typedef struct Tix_DoubleScrollInfo {
    int type;		/* TIX_SCROLL_DOUBLE */
    char * command;

    double total;	/* total size (width or height) of the widget*/
    double window;	/* visible size */
    double offset;	/* The top/left side of the scrolled widget */
    double unit;	/* How much should we scroll when the user
			 * press the arrow on a scrollbar? */
} Tix_DoubleScrollInfo;

/*----------------------------------------------------------------------
 *
 * 		Global variables
 *
 * Should be used only in the Tix library. Some systems don't support
 * exporting of global variables from shared libraries.
 *
 *----------------------------------------------------------------------
 */
EXTERN Tk_Uid tixNormalUid;
EXTERN Tk_Uid tixDisabledUid;
EXTERN Tcl_HashTable tixSpecTable;
EXTERN Tcl_HashTable tixClassTable;

#define FLAG_READONLY	0
#define FLAG_STATIC	1
#define FLAG_FORCECALL	2
/*----------------------------------------------------------------------
 *
 * 		Internal procedures
 *
 *----------------------------------------------------------------------
 */

EXTERN void		Tix_AddDItemType _ANSI_ARGS_((
			    Tix_DItemInfo * diTypePtr));
EXTERN int		Tix_ConfigureInfo2 _ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin,
			    char *entRec, Tk_ConfigSpec *entConfigSpecs,
			    Tix_DItem * iPtr, char *argvName, int flags));
EXTERN void 		Tix_DItemCalculateSize _ANSI_ARGS_((
			    Tix_DItem * iPtr));
EXTERN char *		Tix_DItemComponent _ANSI_ARGS_((Tix_DItem * diPtr,
			    int x, int y));
EXTERN int 		Tix_DItemConfigure _ANSI_ARGS_((
			    Tix_DItem * diPtr, int argc,
			    char ** argv, int flags));
EXTERN Tix_DItem * 	Tix_DItemCreate _ANSI_ARGS_((Tix_DispData * ddPtr,
			    char * type));
EXTERN void		Tix_DItemDrawBackground _ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flags));
EXTERN void 		Tix_DItemDisplay _ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flag));
EXTERN void 		Tix_DItemFree _ANSI_ARGS_((
			    Tix_DItem * iPtr));
EXTERN void		TixDItemStyleChanged _ANSI_ARGS_((
			    Tix_DItemInfo * diTypePtr,
			    Tix_DItemStyle * stylePtr));
EXTERN void		TixDItemStyleFree  _ANSI_ARGS_((Tix_DItem *iPtr, 
			    Tix_DItemStyle * stylePtr));
EXTERN void		TixDItemGetAnchor _ANSI_ARGS_((Tk_Anchor anchor,
			    int x, int y, int cav_w, int cav_h,
			    int width, int height, int * x_ret, int * y_ret));
EXTERN void 		TixGetColorDItemGC _ANSI_ARGS_((
			    Tix_DItem * iPtr, GC * backGC_ret,
			    GC * foreGC_ret, int flags));
EXTERN Tix_DItemStyle*	TixGetDefaultDItemStyle _ANSI_ARGS_((
			    Tix_DispData * ddPtr, Tix_DItemInfo * diTypePtr,
			    Tix_DItem *iPtr, Tix_DItemStyle* oldStylePtr));
EXTERN Tix_DItemInfo *	Tix_GetDItemType _ANSI_ARGS_((
			    Tcl_Interp * interp, char *type));
EXTERN void		Tix_GetScrollFractions _ANSI_ARGS_((
			    Tix_ScrollInfo * siPtr,
			    double * first_ret, double * last_ret));
EXTERN void 		Tix_InitScrollInfo  _ANSI_ARGS_((
			    Tix_ScrollInfo * siPtr, int type));
EXTERN int 		Tix_MultiConfigureInfo _ANSI_ARGS_((
			    Tcl_Interp * interp,
			    Tk_Window tkwin, Tk_ConfigSpec **specsList,
			    int numLists, char **widgRecList, char *argvName,
			    int flags, int request));
EXTERN void 		Tix_SetDefaultStyleTemplate _ANSI_ARGS_((
			    Tk_Window tkwin, Tix_StyleTemplate * tmplPtr));
EXTERN int		Tix_SetScrollBarView _ANSI_ARGS_((
			    Tcl_Interp *interp, Tix_ScrollInfo * siPtr,
			    int argc, char **argv, int compat));
EXTERN void		Tix_SetWindowItemSerial _ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_DItem * iPtr,
			    int serial));
EXTERN int 		Tix_SplitConfig _ANSI_ARGS_((Tcl_Interp * interp,
			    Tk_Window tkwin, Tk_ConfigSpec  ** specsList,
			    int numLists, int argc, char ** argv,
			    Tix_ArgumentList * argListPtr));
EXTERN void		Tix_UnmapInvisibleWindowItems _ANSI_ARGS_((
			    Tix_LinkList * lPtr, int serial));
EXTERN void		Tix_UpdateScrollBar  _ANSI_ARGS_((
			    Tcl_Interp *interp, Tix_ScrollInfo * siPtr));
EXTERN int 		Tix_WidgetConfigure2 _ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin, char * entRec,
			    Tk_ConfigSpec *entConfigSpecs,
			    Tix_DItem * iPtr, int argc, char ** argv,
			    int flags, int forced, int * sizeChanged_ret));
EXTERN void		Tix_WindowItemListRemove  _ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_DItem * iPtr));

#endif /* _TIX_INT_H_ */
