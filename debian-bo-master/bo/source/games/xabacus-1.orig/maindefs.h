#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* name of the application when iconified & on window*/
#define APPNAME     "xabacus"
#define DEMOWINAME  "xabacus-demo"
#define DEMOCOL		6

#define KEYBUFFERSIZE   10
/* size of windows*/
#define SMALL   1
#define OK      0

#define MAXCOLORS   4          /* the number of colors (GC's) required*/
#define COLORSTRLEN	32			/* length of a color name */

#define BDWINCOLOR  "white" /* border color for windows*/

/* attributes of topLevel window for bogus WM's*/
#define INITPOSX    15
#define INITPOSY    15

/* initial attributes of the abacus, in pixels*/
#define BEADHEIGHT	20
#define BEADWIDTH	30
#define FRAMEWIDTH	10		/* thickness of the frame*/

#define COLGAP		2				/* gap between 2 cols*/
#define ROWGAP		2				/* gap between 2 cols*/

#define NTOPROWS	3				/*(2 beads, 1 gap on top-deck)*/
#define NBOTROWS	6				/*(5 beads, 1 gap on top-deck)*/

#define MAXCOLS		100				/* maximum number of columns*/
#define NCOLS		13				/* number of columns*/
#define NROWS		(NTOPROWS+NBOTROWS)

#define MIDFRAME	(FRAMEWIDTH+(NTOPROWS*BEADHEIGHT)+((NTOPROWS+1)*ROWGAP))

/* width/height of window depends on attributes of the abacus*/
#define INITWIDTH	((2*FRAMEWIDTH)+(NCOLS*BEADWIDTH)+((NCOLS+1)*COLGAP))
#define INITHEIGHT	((3*FRAMEWIDTH)+(NROWS*BEADHEIGHT)+((NROWS+1)*ROWGAP))

#define DEMOINITWIDTH	INITWIDTH
#define DEMOINITHEIGHT	100

#define MINWIDTH	INITWIDTH         /* minimum size of application */
#define MINHEIGHT	INITHEIGHT         

enum { FRAME,BACKGROUND,BEADS,RAILS};	/* for referring to the gc's*/

/*return TRUE if row 'r' in col 'c' is occupied by a bead*/
#define RowOccupied(r,c)    ((c & 1<<(r))?1:0)

