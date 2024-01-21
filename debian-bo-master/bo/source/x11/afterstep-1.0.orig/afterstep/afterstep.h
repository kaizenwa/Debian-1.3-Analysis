/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 * 
 * modified later
 * by Bo Yang
 *
 * later again
 * by Frank Fejes
 ****************************************************************************/
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 * afterstep include file
 ***********************************************************************/

#ifndef _FVWM_
#define _FVWM_

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#ifndef WithdrawnState
#define WithdrawnState 0
#endif

/* use PanFrames! this replaces the 3 pixel margin with PanFrame windows
it should not be an option, once it works right. HEDU 2/2/94 */
#define PAN_FRAME_THICKNESS 2	/* or just 1 ? */


/* the maximum number of mouse buttons afterstep knows about */
/* don't think that upping this to 5 will make everything
 * hunky-dory with 5 button mouses (it's better now - HR) */
#define MAX_BUTTONS 3

#include <X11/Intrinsic.h>

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#define SIGNAL_RETURN return 0
#else
#define SIGNAL_T void
#define SIGNAL_RETURN return
#endif

#define BW 1			/* border width */
#define BOUNDARY_WIDTH 7    	/* border width */
#define CORNER_WIDTH 16    	/* border width */

#define NS_TITLE_HEIGHT 21
#define NS_HANDLE_HEIGHT 8

# define HEIGHT_EXTRA 4		/* Extra height for texts in popus */
# define HEIGHT_EXTRA_TITLE 4	/* Extra height for underlining title */
# define HEIGHT_SEPARATOR 4	/* Height of separator lines */

#define SCROLL_REGION 2         /* region around screen edge that */
                                /* triggers scrolling */

#ifndef TRUE                    
#define TRUE	1
#define FALSE	0
#endif

#define NULLSTR ((char *) NULL)

/* contexts for button presses */
#define C_NO_CONTEXT	0
#define C_WINDOW	1
#define C_TITLE		2
#define C_ICON		4
#define C_ROOT		8
#define C_FRAME		16
#define C_SIDEBAR       32
#define C_L1            64
#define C_L2           128
#define C_L3           256
#define C_L4           512
#define C_L5          1024
#define C_R1          2048
#define C_R2          4096
#define C_R3          8192
#define C_R4         16384
#define C_R5         32768
#define C_RALL       (C_R1|C_R2|C_R3|C_R4|C_R5)
#define C_LALL       (C_L1|C_L2|C_L3|C_L4|C_L5)
#define C_ALL   (C_WINDOW|C_TITLE|C_ICON|C_ROOT|C_FRAME|C_SIDEBAR|\
                 C_L1|C_L2|C_L3|C_L4|C_L5|C_R1|C_R2|C_R3|C_R4|C_R5)

typedef struct MyFont
{
  char *name;			/* name of the font */
  XFontStruct *font;		/* font structure */
  int height;			/* height of the font */
  int y;			/* Y coordinate to draw characters */
} MyFont;

typedef struct ColorPair
{
  Pixel fore;
  Pixel back;
} ColorPair;


/* for each window that is on the display, one of these structures
 * is allocated and linked into a list 
 */
typedef struct ASWindow
{
    struct ASWindow *next;	/* next afterstep window */
    struct ASWindow *prev;	/* prev afterstep window */
    Window w;			/* the child window */
    int old_bw;			/* border width before reparenting */
    Window frame;		/* the frame window */
    Window Parent;              /* Ugly Ugly Ugly - it looks like you
				 * HAVE to reparent the app window into
				 * a window whose size = app window,
				 * or else you can't keep xv and matlab
				 * happy at the same time! */
    Window title_w;		/* the title bar window */
    Window side;
    Window corners[2];          /* Corner pieces */
    int nr_left_buttons;
    int nr_right_buttons;
    Window left_w[5];
    Window right_w[5];
    Window icon_pixmap_w;	/* the icon window */
#ifndef NO_PAGER
    Window pager_view;
#endif
#ifdef SHAPE
    int wShaped;               /* is this a shaped window */
#endif
    int frame_x;		/* x position of frame */
    int frame_y;		/* y position of frame */
    int frame_width;		/* width of frame */
    int frame_height;		/* height of frame */
    int boundary_width;
    int boundary_height;
    int corner_width;
    int bw;
    int title_x;
    int title_y;
    int title_height;		/* height of the title bar */
    int title_width;		/* width of the title bar */
    int button_height;		/* height of the buttons on title bar */
    int icon_x_loc;		/* icon window x coordinate */
    int icon_y_loc;		/* icon window y coordiante */
    int icon_p_width;		/* width of the icon pixmap window */
    int icon_p_height;		/* height of the icon pixmap window */
    int icon_pm_width;		/* width of the icon pixmap  */
    int icon_pm_height;		/* height of the icon pixmap  */
    Pixmap iconPixmap;		/* pixmap for the icon */
    int iconDepth;		/* Drawable depth for the icon */
    Pixmap icon_maskPixmap;	/* pixmap for the icon mask */
    char *name;			/* name of the window */
    char *icon_name;		/* name of the icon */
    XWindowAttributes attr;	/* the child window attributes */
    XSizeHints hints;		/* normal hints */
    XWMHints *wmhints;		/* WM hints */
    XClassHint class;
    int Desk;                   /* Tells which desktop this window is on */
    int FocusDesk;		/* Where (if at all) was it focussed */
    int DeIconifyDesk;          /* Desk to deiconify to, for StubbornIcons */
    Window transientfor;

    unsigned long flags;
    char *icon_bitmap_file;

    int orig_x;                 /* unmaximized x coordinate */
    int orig_y;                 /* unmaximized y coordinate */
    int orig_wd;                /* unmaximized window width */
    int orig_ht;                /* unmaximized window height */

    int xdiff,ydiff;            /* used to restore window position on exit*/
    int *mwm_hints;
    int functions;
    Window *cmap_windows;       /* Colormap windows property */
    int number_cmap_windows;    /* Should generally be 0 */
    long focus_sequence;
    long circulate_sequence;
    Pixel ReliefPixel;
    Pixel ShadowPixel;
    Pixel TextPixel;
    Pixel BackPixel;
    unsigned long buttons;
    Bool focus_var;		/* to see if focus will work this way */
#ifdef ENABLE_TEXTURE
    int    bp_width, bp_height;	/* size of background pixmap */
    Pixmap backPixmap;		/* for titlebar background */
    Pixmap backPixmap2;		/* unfocused window titlebar */
    Pixmap backPixmap3;		/* unfocused sticky titlebar */
#endif
} ASWindow;


#ifdef ENABLE_TEXTURE
#define TITLE_OLD		0	/* old (NEXTSTEP 3) style titlebar */
#define TITLE_NEXT4		1	/* NEXTSTEP 4 style titlebar */

typedef struct {
    int		Tfrom[3], Tto[3];	/* title bar rgb colors */
    int		Ifrom[3], Ito[3];	/* menu item rgb colors */
    int		Mfrom[3], Mto[3];	/* menu title rgb colors */
    int		Ufrom[3], Uto[3];	/* unfocused item rgb colors */
    int		Sfrom[3], Sto[3];	/* unfoc. sticky titlebar */
    int		Tmaxcols, Imaxcols;	/* number of colors reserved */
    int		Umaxcols, Mmaxcols;
    int		Smaxcols;
    int		Ttype, Itype, Utype;	/* texture type */
    int		Mtype, Stype;		/* sticky texture type */
    unsigned long flags;		/* misc. flags */
	int 	TGfrom[3], TGto[3];	/* gradient for the title text */
} TextureInfo;
extern TextureInfo  Textures;
/* flags */
#define TexturedHandle		(1<<0)
#define TitlebarNoPush		(1<<1)
#define GradientText		(1<<2)
/* icon background (aka button) flags */
#define IconNoBorder		(1<<0)
#endif
/***************************************************************************
 * window flags definitions 
 ***************************************************************************/
#define STICKY         1 /* Does window stick to glass? */
#define ONTOP          2 /* does window stay on top */
#define BORDER         4 /* Is this decorated with border*/
#define TITLE          8 /* Is this decorated with title */
#define MAPPED        16 /* is it mapped? */
#define ICONIFIED     32 /* is it an icon now? */
#define TRANSIENT     64 /* is it a transient window? */
#define RAISED       128 /* if its a sticky window, does it need to be raised*/
#define VISIBLE      256 /* is the window fully visible */
#define ICON_OURS    512 /* is the icon window supplied by the app? */
#define XPM_FLAG    1024 /* is the icon window an xpm? */
#define PIXMAP_OURS 2048 /* is the icon pixmap ours to free? */
#define SHAPED_ICON 4096 /* is the icon shaped? */
#define MAXIMIZED   8192 /* is the window maximized? */
#define DoesWmTakeFocus		16384
#define DoesWmDeleteWindow	32768
/* has the icon been moved by the user? */
#define ICON_MOVED              65536 
/* was the icon unmapped, even though the window is still iconified
 * (Transients) */
#define ICON_UNMAPPED          131072 
#define WINDOWLISTSKIP         262144
#define SUPPRESSICON           524288
#define CIRCULATESKIP         1048576
#define STARTICONIC           2097152
/* Sent an XMapWindow, but didn't receive a MapNotify yet.*/
#define MAP_PENDING           4194304
#define SHADED			(1<<23)
/* flags to suppress/enable title bar buttons */
#define BUTTON1     1
#define BUTTON2     2
#define BUTTON3     4
#define BUTTON4     8
#define BUTTON5    16
#define BUTTON6    32
#define BUTTON7    64
#define BUTTON8   128
#define BUTTON9   256
#define BUTTON10  512

#include <stdlib.h>
extern void Reborder(void);
extern void SigDone(int);
extern void Restart(int nonsense);
extern void Done(int, char *);

extern Display *dpy;

extern XClassHint NoClass;

extern XContext ASContext;

extern Window JunkRoot, JunkChild;
extern int JunkX, JunkY;
extern unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

#ifdef PAN_FRAMES
extern void checkPanFrames();
extern void raisePanFrames();
#endif

extern Atom _XA_MIT_PRIORITY_COLORS;
extern Atom _XA_WM_CHANGE_STATE;
extern Atom _XA_WM_STATE;
extern Atom _XA_WM_COLORMAP_WINDOWS;
extern Atom _XA_WM_PROTOCOLS;
extern Atom _XA_WM_TAKE_FOCUS;
extern Atom _XA_WM_SAVE_YOURSELF;
extern Atom _XA_WM_DELETE_WINDOW;
extern Atom _XA_WM_DESKTOP;
extern Atom _XA_FVWM_STICKS_TO_GLASS;
extern Atom _XA_FVWM_CLIENT;

#endif /* _FVWM_ */



