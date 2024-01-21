/*
 * bltInt.h --
 *
 * Copyright 1993-1996 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#ifndef _BLT_INT_H
#define _BLT_INT_H

#include <tcl.h>
#include <tk.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "bltConfig.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif /* HAVE_ERRNO_H */

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif /* HAVE_MEMORY_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif /* HAVE_MALLOC_H */

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include "bltList.h"

#ifndef M_PI
#define M_PI    	3.14159265358979323846
#endif /* M_PI */

#ifndef M_SQRT2
#define M_SQRT2		1.41421356237309504880
#endif /* M_SQRT2 */

#ifndef M_SQRT1_2
#define M_SQRT1_2	0.70710678118654752440
#endif /* M_SQRT1_2 */

#ifndef SHRT_MAX
#define SHRT_MAX	0x7FFF
#endif /* SHRT_MAX */

#ifndef USHRT_MAX
#define	USHRT_MAX	0xFFFF
#endif /* USHRT_MAX */

#ifndef HAVE_FLOAT_H
/*
 * ----------------------------------------------------------------------
 *
 * DBL_MIN, DBL_MAX --
 *
 * 	DBL_MAX and DBL_MIN are the largest and smaller double
 * 	precision numbers that can be represented by the floating
 * 	point hardware. If the compiler is ANSI, they can be found in
 * 	float.h.  Otherwise, we use HUGE_VAL or HUGE to determine
 * 	them.
 *
 * ---------------------------------------------------------------------- */
/*
 * Don't want to include __infinity (definition of HUGE_VAL (SC1.x))
 */
#ifdef sun
#define DBL_MAX		1.7976931348623157E+308
#define DBL_MIN		2.2250738585072014E-308
#define DBL_EPSILON	2.2204460492503131e-16
#else
#ifndef DBL_EPSILON
#define DBL_EPSILON	BLT_DBL_EPSILON
#endif
#ifdef HUGE_VAL
#define DBL_MAX		HUGE_VAL
#define DBL_MIN		(1/HUGE_VAL)
#else
#ifdef HUGE
#define DBL_MAX		HUGE
#define DBL_MIN		(1/HUGE)
#else
/*
 * Punt: Assume values simple and relatively small
 */
#define DBL_MAX		3.40282347E+38
#define DBL_MIN		1.17549435E-38
#endif /*HUGE*/
#endif /*HUGE_VAL*/
#endif /*sun*/
#endif /*!HAVE_FLOAT_H*/

#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif

#undef BLT_MIN
#define BLT_MIN(a,b)	(((a)<(b))?(a):(b))

#undef BLT_MAX
#define BLT_MAX(a,b)	(((a)>(b))?(a):(b))

/*
 * ----------------------------------------------------------------------
 *
 *  	The following are macros replacing math library functions:
 *  	"fabs", "fmod", "abs", "rint", and "exp10".
 *
 *  	Although many of these routines may be in your math library,
 *  	they aren't used in libtcl.a or libtk.a.  This makes it
 *  	difficult to dynamically load the BLT library as a shared
 *  	object unless the math library is also shared (which isn't
 *  	true on several systems).  We can avoid the problem by
 *  	replacing the "exotic" math routines with macros.
 *
 * ----------------------------------------------------------------------
 */
#undef BLT_ABS
#define BLT_ABS(x) 	(((x)<0)?(-(x)):(x))

#undef BLT_EXP10
#define BLT_EXP10(x)	(pow(10.0,(x)))

#undef BLT_FABS
#define BLT_FABS(x) 	(((x)<0.0)?(-(x)):(x))

#undef BLT_FMOD
#define BLT_FMOD(x,y) 	((x)-(((int)((x)/(y)))*y))

#undef BLT_RND
#define BLT_RND(x) 	((int)((x) + (((x)<0.0) ? -0.5 : 0.5)))

#define TRUE 	1
#define FALSE 	0

#ifdef hpux
#define BLACK		"black"
#define WHITE		"white"
#define BISQUE1		"bisque1"
#define BISQUE2		"bisque2"
#define BISQUE3     	"bisque3"
#define ANTIQUEWHITE1   "antiquewhite1"
#define GRAY		"grey"
#define LIGHTBLUE2	"lightblue2"
#define RED		"red"
#define PINK		"pink"
#define BLUE		"blue"
#define NAVYBLUE	"navyblue"
#define GRAY85		"gray85"
#define GRAY92		"gray92"
#define GRAY77		"gray92"
#define GRAY64		"gray64"
#define MAROON		"maroon"
#else
#define BLACK		"#000000"
#define WHITE		"#ffffff"
#define BISQUE1		"#ffe4c4"
#define BISQUE2		"#eed5b7"
#define BISQUE3     	"#cdb79e"
#define ANTIQUEWHITE1	"#ffefdb"
#define GRAY		"#b0b0b0"
#define LIGHTBLUE2	"#b2dfee"
#define GRAY85		"#d9d9d9"
#define GRAY92		"#ececec"
#define GRAY77		"#c3c3c3"
#define GRAY64		"#a3a3a3"
#define MAROON		"#b03060"
#define RED		"#ff0000"
#define BLUE		"#0000ff"
#define GREEN		"#00ff00"
#endif

#if (TK_MAJOR_VERSION == 3)
#define STD_NORMAL_BG_COLOR	BISQUE1
#define STD_ACTIVE_BG_COLOR	BISQUE2
#define STD_SELECT_BG_COLOR	LIGHTBLUE2
#define STD_DISABLE_FG_COLOR	GRAY64
#else
#define STD_NORMAL_BG_COLOR	GRAY85
#define STD_ACTIVE_BG_COLOR	GRAY92
#define STD_SELECT_BG_COLOR	GRAY77
#define STD_DISABLE_FG_COLOR	GRAY64
#endif

#define STD_INDICATOR_COLOR	MAROON

#define STD_NORMAL_FG_COLOR	BLACK
#define STD_NORMAL_BG_MONO	WHITE
#define STD_NORMAL_FG_MONO	BLACK
#define STD_ACTIVE_BG_MONO	BLACK
#define STD_ACTIVE_FG_COLOR	BLACK
#define STD_ACTIVE_FG_MONO	WHITE
#define STD_SELECT_FG_COLOR	BLACK
#define STD_SELECT_FG_MONO	WHITE
#define STD_SELECT_BG_MONO	BLACK
#define STD_SELECT_BORDERWIDTH	"2"
#define STD_BORDERWIDTH 	"2"

#define STD_FONT_HUGE		"*-Helvetica-Bold-R-Normal-*-18-180-*"
#define STD_FONT_LARGE		"*-Helvetica-Bold-R-Normal-*-14-140-*"
#define STD_FONT		"*-Helvetica-Bold-R-Normal-*-12-120-*"
#define STD_FONT_SMALL		"*-Helvetica-Bold-R-Normal-*-10-100-*"

#define TEXTHEIGHT(f) 	((f)->ascent + (f)->descent)
#define LINEWIDTH(w)	(((w) > 1) ? (w) : 0)
/*
 * FLAGS passed to TkMeasureChars:
 */
#define TK_WHOLE_WORDS           1
#define TK_AT_LEAST_ONE          2
#define TK_PARTIAL_OK            4
#define TK_NEWLINES_NOT_SPECIAL  8
#define TK_IGNORE_TABS          16

#define NO_FLAGS		0

#define NO_TED			1

#undef VARARGS
#ifdef __cplusplus
#define ANYARGS (...)
#define VARARGS(first)  (first, ...)
#else
#define ANYARGS ()
#define VARARGS(first) ()
#endif

#define Panic(mesg)	panic("%s:%d %s", __FILE__, __LINE__, (mesg));

/*
 * ----------------------------------------------------------------------
 *
 * Blt_CmdSpec --
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *name;			/* Name of command */
    char *version;		/* Version string of command */
    Tcl_CmdProc *cmdProc;
    Tcl_CmdDeleteProc *cmdDeleteProc;
    ClientData clientData;
} Blt_CmdSpec;

/*
 * ----------------------------------------------------------------------
 *
 * Blt_OperProc --
 *
 * 	Generic function prototype of CmdOptions.
 *
 * ----------------------------------------------------------------------
 */
typedef int (*Blt_OperProc) _ANSI_ARGS_(ANYARGS);

/*
 * ----------------------------------------------------------------------
 *
 * Blt_OperSpec --
 *
 * 	Structure to specify a CmdOption for a Tcl command.  This is 
 *	passed in the Blt_LookupOperation procedure to search for a 
 *	Blt_OperProc procedure.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *name;			/* Name of minor command */
    int minChars;		/* Minimum # characters to disambiguate */
    Blt_OperProc proc;
    int minArgs;		/* Minimum # args required */
    int maxArgs;		/* Maximum # args required */
    char *usage;		/* Usage message */

} Blt_OperSpec;

typedef enum {
    BLT_OPER_ARG0,		/* Operation name is the first argument */
    BLT_OPER_ARG1,		/* Operation name is the second argument */
    BLT_OPER_ARG2,		/* Operation name is the third argument */
    BLT_OPER_ARG3,		/* Operation name is the fourth argument */
    BLT_OPER_ARG4,		/* Operation name is the fifth argument */

} Blt_OperIndex;

extern Blt_OperProc Blt_LookupOperation _ANSI_ARGS_((Tcl_Interp *interp,
	int numSpecs, Blt_OperSpec *specArr, Blt_OperIndex argIndex, 
	int numArgs, char **argArr));

/*
 * ----------------------------------------------------------------------
 *
 * Pad --
 *
 * 	Structure to specify vertical and horizontal padding.
 *	This allows padding to be specified on a per side basis.
 *	Vertically, side1 and side2 refer to top and bottom sides.
 *	Horizontally, they refer to left and right sides.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    int side1, side2;
} Pad;

#define padLeft  	padX.side1
#define padRight  	padX.side2
#define padTop		padY.side1
#define padBottom	padY.side2
#define PADDING(x)	((x).side1 + (x).side2)

/*
 * ----------------------------------------------------------------------
 *
 * The following enumerated values are used as bit flags.
 *
 *
 * ----------------------------------------------------------------------
 */
typedef enum {
    FILL_NONE,			/* No filling is specified */
    FILL_X,			/* Fill horizontally */
    FILL_Y,			/* Fill vertically */
    FILL_BOTH			/* Fill both vertically and horizontally */
} Fill;

/*
 * ----------------------------------------------------------------------
 *
 * Dashes --
 *
 * 	List of dash values (maximum 11 based upon PostScript limit).
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    unsigned char valueList[12];
    int numValues;
} Dashes;

/*
 * ----------------------------------------------------------------------
 *
 * TextAttributes --
 *
 * 	Represents a convenient structure to hold text attributes
 *	which determine how a text string is to be displayed on the
 *	window, or drawn with PostScript commands.  The alternative
 *	is to pass lots of parameters to the drawing and printing
 *	routines. This seems like a more efficient and less cumbersome
 *	way of passing parameters.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    XColor *bgColorPtr;		/* Text background color */
    XColor *fgColorPtr;		/* Text foreground color */
    double theta;		/* Rotation of text in degrees. */
    XFontStruct *fontPtr;	/* Font to use to draw text */
    Tk_Justify justify;		/* Justification of the text string. This
				 * only matters if the text is composed
				 * of multiple lines. */
    Tk_Anchor anchor;		/* Indicates how the text is anchored around
				 * its x and y coordinates. */
    GC textGC;			/* GC used to draw the text */
    GC fillGC;			/* GC used to fill the background of the
				 * text. bgColorPtr must be set too. */
    int regionWidth;		/* Width of the region where the text will
				 * be drawn. Used for justification. */
    Pad padX, padY;		/* Specifies padding of around text region */
} TextAttributes;

/*
 * -------------------------------------------------------------------
 *
 * Coordinate --
 *
 *	Represents a single coordinate.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    double x, y;
} Coordinate;


typedef enum {
    BLT_VECTOR_NOTIFY_UPDATE=1, /* The vector's values has been updated */
    BLT_VECTOR_NOTIFY_DESTROY=2	/* The vector has been destroyed and the client
				 * should no longer use its data (calling 
				 * Blt_FreeVectorId) */
} Blt_VectorNotify;

typedef void (Blt_VectorChangedProc) _ANSI_ARGS_((Tcl_Interp *interp,
	ClientData clientData, Blt_VectorNotify notify));

typedef struct Blt_VectorId *Blt_VectorId;
typedef struct {
    double *valueArr;		/* Array of values (possibly malloc-ed) */
    int numValues;		/* Number of values in the array */
    int arraySize;		/* Size of the allocated space */
    double min, max;		/* Minimum and maximum values in the vector */
    int reserved;		/* Reserved for future use */
} Blt_Vector;

extern Blt_VectorId Blt_AllocVectorId _ANSI_ARGS_((Tcl_Interp *interp, 
	char *vecName));
extern void Blt_SetVectorChangedProc _ANSI_ARGS_((Blt_VectorId clientId,
	Blt_VectorChangedProc * proc, ClientData clientData));
extern void Blt_FreeVectorId _ANSI_ARGS_((Blt_VectorId clientId));
extern int Blt_GetVectorById _ANSI_ARGS_((Tcl_Interp *interp,
	Blt_VectorId clientId, Blt_Vector *vecPtr));
extern char *Blt_NameOfVectorId _ANSI_ARGS_((Blt_VectorId clientId));
extern int Blt_VectorNotifyPending _ANSI_ARGS_((Blt_VectorId clientId));

extern int Blt_CreateVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	int size, Blt_Vector *vecPtr));
extern int Blt_GetVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	Blt_Vector *vecPtr));
extern int Blt_VectorExists _ANSI_ARGS_((Tcl_Interp *interp, char *vecName));
extern int Blt_ResetVector _ANSI_ARGS_((Tcl_Interp *interp,  char *vecName,
	Blt_Vector *vecPtr, Tcl_FreeProc *freeProc));
extern int Blt_ResizeVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	int newSize));
extern int Blt_DeleteVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName));


/*
 * ----------------------------------------------------------------------
 *
 * 	X11/Xosdefs.h requires XNOSTDHDRS be set for some systems.
 *	This is a guess.  If I can't find STDC headers or unistd.h,
 *	assume that this is non-POSIX and non-STDC environment.
 *	(needed for Encore Umax 3.4 ?)
 *
 * ----------------------------------------------------------------------
 */
#if !defined(STDC_HEADERS) && !defined(HAVE_UNISTD_H)
#define XNOSTDHDRS 	1
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	Assume we need to declare free if there's no stdlib.h or malloc.h
 *
 * ----------------------------------------------------------------------
 */
#if !defined(HAVE_STDLIB_H) && !defined(HAVE_MALLOC_H)
extern void free _ANSI_ARGS_((void *));
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	If strerror isn't in the C library (we'll get it from
 *	libtcl.a) assume that we also need a forward declaration.
 *
 * ----------------------------------------------------------------------
 */
#ifndef HAVE_STRERROR
extern char *strerror _ANSI_ARGS_((int));
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	Make sure we have a declaration for strdup and strcasecmp
 *
 * ----------------------------------------------------------------------
 */
#ifdef NO_DECL_STRDUP
extern char *strdup _ANSI_ARGS_((CONST char *s));
#endif

#ifdef NO_DECL_STRCASECMP
extern int strcasecmp _ANSI_ARGS_((CONST char *s1, CONST char *s2));
#endif

extern GC Blt_GetUnsharedGC _ANSI_ARGS_((Tk_Window tkwin, unsigned long gcMask,
	XGCValues *valuePtr));

extern int Blt_GetLength _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *string, int *valuePtr));

extern char *Blt_NameOfFill _ANSI_ARGS_((Fill fill));

extern int Blt_GetXYPosition _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	XPoint *pointPtr));

extern void Blt_AppendDouble _ANSI_ARGS_((Tcl_Interp *interp, double value));
extern void Blt_AppendInt _ANSI_ARGS_((Tcl_Interp *interp, int value));

extern int Blt_InitCmd _ANSI_ARGS_((Tcl_Interp *interp, Blt_CmdSpec *specPtr));
extern int Blt_InitCmds _ANSI_ARGS_((Tcl_Interp *interp, Blt_CmdSpec *specPtr,
	int numCmds));
extern int Blt_ConfigModified _ANSI_ARGS_(VARARGS(Tk_ConfigSpec *specs));

extern void Blt_MakeInputOnlyWindowExist _ANSI_ARGS_((Tk_Window tkwin));

extern void Blt_GetBoundingBox _ANSI_ARGS_((int width, int height,
	double theta, int *widthPtr, int *heightPtr, XPoint *pointArr));

extern void Blt_InitTextAttrs _ANSI_ARGS_((TextAttributes *attrPtr,
	XColor *fgColorPtr, XColor *bgColorPtr, XFontStruct *fontPtr,
	double theta, Tk_Anchor anchor, Tk_Justify justify));

extern void Blt_DrawText _ANSI_ARGS_((Tk_Window tkwin, Drawable draw,
	char *text, TextAttributes *attrPtr, int x, int y));

extern void Blt_PrintJustified _ANSI_ARGS_((Tcl_Interp *interp, char *buffer,
	TextAttributes *attrPtr, char *text, int x, int y, int length));

extern Pixmap Blt_CreateTextBitmap _ANSI_ARGS_((Tk_Window tkwin, char *textStr,
	TextAttributes *attrPtr, int *bmWidthPtr, int *bmHeightPtr));

extern Pixmap Blt_RotateBitmap _ANSI_ARGS_((Display *display, Drawable draw,
	GC gc, Pixmap bitmap, int width, int height, double theta,
	int *rotWPtr, int *rotHPtr));

extern Coordinate Blt_TranslateBoxCoords _ANSI_ARGS_((double x, double y, 
	int width, int height, Tk_Anchor anchor));

#if (TK_MAJOR_VERSION == 4)
typedef int *Blt_Tile;		/* Opaque type for tiles */

typedef void (Blt_TileChangedProc) _ANSI_ARGS_((ClientData clientData,
	Blt_Tile tile, GC *gcPtr));

extern Blt_Tile Blt_GetTile _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *imageName));
extern void Blt_FreeTile _ANSI_ARGS_((Blt_Tile tile));
extern char *Blt_NameOfTile _ANSI_ARGS_((Blt_Tile tile));
extern void Blt_SetTileChangedProc _ANSI_ARGS_((Blt_Tile tile,
	Blt_TileChangedProc * changeProc, ClientData clientData, GC *gcPtr));
extern Pixmap Blt_PixmapOfTile _ANSI_ARGS_((Blt_Tile tile));
extern void Blt_SizeOfTile _ANSI_ARGS_((Blt_Tile tile, int *widthPtr,
	int *heightPtr));
extern void Blt_SetTileOrigin _ANSI_ARGS_((Tk_Window tkwin, GC gc, int x,
	int y));
#endif /* TK_MAJOR_VERSION == 4 */

/* 
 * Backward compatibility section.
 */
#if (TK_MAJOR_VERSION == 3)
#define Tk_GetPixmap 		XCreatePixmap
#define Tk_FreePixmap 		XFreePixmap
#define Tk_ConfigureValue 	Blt_ConfigureValue
extern int Blt_ConfigureValue _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	Tk_ConfigSpec *specs, char *widRec, char *option, int flags));
#endif /* TK_MAJOR_VERSION == 3 */
#if (TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION >= 1)
typedef char *FreeProcData;
#else
#define Tk_Cursor		Cursor
typedef ClientData FreeProcData;
#endif

extern void panic _ANSI_ARGS_(VARARGS(char *fmt));

/* 
 * [incr tcl] 2.0 Namespaces
 *
 * BLT will automatically use namespaces if you compile with with the
 * itcl-2.x versions of Tcl and Tk headers and libraries.  BLT will
 * reside in its own namespace called "blt". All the normal BLT
 * commands and variables will reside there.
 * 
 * If you have [incr Tcl] 2.x, try to use it instead of the vanilla
 * Tcl and Tk headers and libraries.  Itcl namespaces are the reason
 * why the prefix "blt_" is no longer in front of every command.
 * There's no reason to "uglify" code when the right solution is
 * available.  Nuff said.
 *
 * But if you insist, you can disable the automatic selection of itcl
 * by uncommenting the following #undef statement.
 *
 */

#undef ITCL_NAMESPACES

#endif /*_BLT_INT_H*/




