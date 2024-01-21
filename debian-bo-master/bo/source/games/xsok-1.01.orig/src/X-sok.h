/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module X-sok.h				     */
/*									     */
/*	This file is included by all sources for the X interface.	     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include "xsok.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>

#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>	
#include <X11/Xaw/Label.h>	
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Toggle.h>

/* in X11R3, XSize_t was int, since R4 we seem to have a mixture of int  */
/* and unsigned int! (complain!)					 */
typedef unsigned int XSize_t;   /* type used by X for width and height   */
                                /* this is not consistent used by X11R5  */

extern struct graphic {
    boolean autolayout;         /* automatic new layout at resize events */
    XSize_t width;              /* the width of the table window         */
    XSize_t height;             /* the height of the table window        */
} graphic;

extern Display *dpy;
extern Window table;
extern Widget toplevel;

#define DX	32	/* size of one square. we could even read this from the xpm file */
#define DY	32



/* prototypes. some of them may be in xsok.h already */
/* Xaw-help.c */
#ifdef ONLINE_HELP
void create_help(void);
void popup_help(void);
void popdown_help(Widget, XtPointer, XtPointer);
#endif

/* Xaw-main.c */
void show_message(const char *str, ...);
void SetTitle(void);
void cmd_LeaveSok(void);
void cmd_Confirm(void);
void cmd_Cancel(void);
void request_confirm(void (*)(void), const char *);
#ifdef SOUND
int checksound(void);
#endif
int main(int argc, char *argv[]);
void Force_Resize(XSize_t, XSize_t);

/* X-widget.c */
void AskWidgetForResize(XSize_t, XSize_t);

/* X-events.c */
void refresh_screen(void);
void button_press(XButtonPressedEvent *);
void key_press(XKeyPressedEvent *);
void cmd_Resize(void);
void resize_event(XSize_t, XSize_t);

/* X-gfx.c */
void NewLevel(int);
void init_layout(void);
void init_gfx(const char *);
/* void dotPaint(int, int, int, int); */
void doPaint(int, int, int, int);
void redraw_table(XExposeEvent *);
