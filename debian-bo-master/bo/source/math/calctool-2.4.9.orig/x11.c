
/*  @(#)x11.c 1.14 89/12/21
 *
 *  These are the X11 dependent graphics routines used by calctool.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
/* 
   this will be NOT required 
   #include <sys/bsdtypes.h> 
*/
#include "calctool.h"
#include "color.h"
#include "extern.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>

#define  XGETSIZEHINTS  (void) XGetSizeHints

#define  BIGFONT               "helvetica-bold-14"
#define  DEFFONT               "fixed"
#define  NORMALFONT            "fixed"
#define  SMALLFONT             "6x10"

#define  CALCTOOL_BORDER_WIDTH  2

#define  FRAME_MASK  (ButtonPressMask | ButtonReleaseMask | KeyPressMask | \
                      EnterWindowMask | LeaveWindowMask | ExposureMask)

short help_cursor_array[16] = {
#include "help.cursor"
} ;

short icon_image[] = {
#include "calctool.icon"
} ;

short cicon_image[] = {
#include "calctool.color.icon"
} ;

Atom protocol_atom, kill_atom ;
Cursor help_cursor, main_cursor ;
Display *dpy ;
GC gc ;
Pixmap calctool_icon, help_pixmap ;
Pixmap load_icon() ;
Window frame, rframe ;
XColor BGcolor, FGcolor, current_col ;
XEvent event ;
XFontStruct *bfont, *font, *nfont, *sfont ;
XGCValues gc_val ;
XSizeHints size ;
XWMHints wm_hints ;

unsigned long gc_mask ;
int screen ;
unsigned int scr_depth ;
unsigned long backgnd, foregnd ;
unsigned long palette[CALC_COLORSIZE] ;


clear_canvas(ctype, color)
enum can_type ctype ;
int color ;
{
  int x, y ;
  unsigned int width, height, bwidth, depth ;
  Window root, window ;

       if (ctype == KEYCANVAS) window = frame ;
  else if (ctype == REGCANVAS) window = rframe ;
  XGetGeometry(dpy, window, &root, &x, &y, &width, &height, &bwidth, &depth) ;
  if (iscolor) gc_val.foreground = palette[color] ;
  else
    {
      if (color == WHITE) gc_val.foreground = backgnd ;
      else gc_val.foreground = foregnd ;
    }
  gc_val.function = GXcopy ;
  XChangeGC(dpy, gc, GCForeground | GCFunction, &gc_val) ;
  XFillRectangle(dpy, window, gc, x, y, width, height) ;
}


close_frame()
{
  XEvent event ;

  event.xclient.type = ClientMessage ;
  event.xclient.display = dpy ;
  event.xclient.window = frame ;
  event.xclient.message_type = XInternAtom(dpy, "WM_CHANGE_STATE", False) ;
  event.xclient.format = 32 ;
  event.xclient.data.l[0] = IconicState ;
  XSendEvent(dpy, DefaultRootWindow(dpy), False,
	      SubstructureRedirectMask | SubstructureNotifyMask, &event) ;
  XFlush(dpy) ;
}


color_area(x, y, width, height, color)
int x, y, width, height, color ;
{
  if (iscolor) gc_val.foreground = palette[color] ;
  else
    {
      if (color == WHITE) gc_val.foreground = backgnd ;
      else gc_val.foreground = foregnd ;
    }
  gc_val.function = GXcopy ;
  XChangeGC(dpy, gc, GCForeground | GCFunction, &gc_val) ;
  XFillRectangle(dpy, frame, gc, x, y,
                 (unsigned int) width, (unsigned int) height) ;
}


create_menu(mtype)    /* Create popup menu for right button press. */
enum menu_type mtype ;
{
}


destroy_frame()
{
  XDestroyWindow(dpy, frame) ;
  XDestroyWindow(dpy, rframe) ;
  exit(0) ;
}


do_menu(mtype)      /* Popup appropriate menu and get value. */
enum menu_type mtype ;
{
}


drawline(x1, y1, x2, y2)
int x1, y1, x2, y2 ;
{
  if (iscolor) gc_val.foreground = palette[BLACK] ;
  else gc_val.foreground = foregnd ;
  gc_val.function = GXcopy ;
  XChangeGC(dpy, gc, GCForeground | GCFunction, &gc_val) ;
  XDrawLine(dpy, frame, gc, x1, y1, x2, y2) ;
}


draw_regs()
{
  XMapWindow(dpy, rframe) ;
}


drawtext(x, y, ctype, fontno, color, str)
enum font_type fontno ;
enum can_type ctype ;
int x, y, color ;
char *str ;
{
  Window window ;

       if (fontno == SFONT) font = sfont ;
  else if (fontno == NFONT) font = nfont ;
  else if (fontno == BFONT) font = bfont ;
       if (ctype == KEYCANVAS) window = frame ;
  else if (ctype == REGCANVAS) window = rframe ;

  if (ctype == KEYCANVAS && y == items[(int) DISPLAYITEM].y) x += 100 ;
  if (iscolor) gc_val.foreground = palette[color] ;
  else
    {
      if (color == WHITE) gc_val.foreground = backgnd ;
      else gc_val.foreground = foregnd ;
    }
  gc_val.font = font->fid ;
  gc_val.function = GXcopy ;
  XChangeGC(dpy, gc, GCFont | GCForeground | GCFunction, &gc_val) ;
  XDrawString(dpy, window, gc, x, y, str, strlen(str)) ;
}


get_display()         /* GET function key was pressed. */
{
}


XFontStruct *
get_font(name)
char *name ;
{
  XFontStruct *font ;

  if (!(font = XLoadQueryFont(dpy, name)))
    if (!(font = XLoadQueryFont(dpy, DEFFONT)))
      {
        perror("couldn't get the default font.") ;
        exit(1) ;
      }
  return(font) ;
}


get_next_event()
{
  XClientMessageEvent *ev ;
  XKeyPressedEvent *key_event ;
  char chs[2] ;

  if (!XCheckMaskEvent(dpy, ExposureMask, &event))
    XNextEvent(dpy, &event) ;

  switch (event.type)
    {
      case ClientMessage    : /* Catch ICCCM kill from WM. */

                              ev = (XClientMessageEvent *) &event ;
                              if (ev->message_type == protocol_atom &&
                                  ev->data.l[0] == kill_atom)
                                exit(0) ;
                              return(LASTEVENTPLUSONE) ;

      case Expose           : return(process_expose(&event)) ;

      case EnterNotify      : return(ENTER_WINDOW) ;

      case LeaveNotify      : return(EXIT_WINDOW) ;

      case KeyPress         : key_event = (XKeyPressedEvent *) &event ;
                              curx = key_event->x ;
                              cury = key_event->y ;
                              (void) XLookupString(key_event, chs, 1,
                                                   (KeySym *)  NULL,
                                                   (XComposeStatus *) NULL) ;
                              cur_ch = chs[0] ;
                              return(KEYBOARD) ;

      case ButtonPress      : curx = event.xbutton.x ;
                              cury = event.xbutton.y ;
                              if (event.xbutton.button == Button1)
                                return(LEFT_DOWN) ;
                              else if (event.xbutton.button == Button2)
                                return(MIDDLE_DOWN) ;
                              else if (event.xbutton.button == Button3)
                                return(RIGHT_DOWN) ;

      case ButtonRelease    : curx = event.xbutton.x ;
                              cury = event.xbutton.y ;
                              if (event.xbutton.button == Button1)
                                return(LEFT_UP) ;
                              else if (event.xbutton.button == Button2)
                                return(MIDDLE_UP) ;
                              else if (event.xbutton.button == Button3)
                                return(RIGHT_UP) ;

      default               : return(LASTEVENTPLUSONE) ;
   }
/*NOTREACHED*/
}


handle_selection()
{
}


init_fonts()
{
  bfont = get_font(BIGFONT) ;
  nfont = get_font(NORMALFONT) ;
  nfont_width = 6 ;
  sfont = get_font(SMALLFONT) ;
}


init_ws_type()
{
  if ((dpy = XOpenDisplay(x11_display)) == NULL)
    {
      FPRINTF(stderr,"%s: Couldn't open display %s\n", progname,
              (getenv ("DISPLAY") ? getenv("DISPLAY") : x11_display)) ;
      exit(1) ;
    }

  screen = DefaultScreen(dpy) ;

  if (!geometry)
    STRCPY(geometry, XGetDefault(dpy, progname, "Geometry")) ;

  foregnd = BlackPixel(dpy, screen) ;
  backgnd = WhitePixel(dpy, screen) ;
  scr_depth = DefaultDepth(dpy, screen) ;
  gtype = X11 ;
  return 0 ;
}


load_colors()      /* Create and load calctool color map. */
{
  u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;
  int i, numcolors ;

  iscolor = 0 ;
  if (DisplayCells(dpy, screen) > 2)
    {
      calc_colorsetup(red, green, blue) ;
      iscolor = 1 ;
      numcolors = 0 ;
      for (i = 0; i < CALC_COLORSIZE; i++)
        {
          current_col.flags = DoRed | DoGreen | DoBlue ;
          current_col.red = (unsigned short) (red[i] << 8) ;
          current_col.green = (unsigned short) (green[i] << 8) ;
          current_col.blue = (unsigned short) (blue[i] << 8) ;
          if (XAllocColor(dpy, DefaultColormap(dpy, screen), &current_col) == True)
            palette[numcolors++] = current_col.pixel ;
        }
      if (numcolors < 2)
        {
          FPRINTF(stderr, "%s: cannot allocate colors.\n", progname) ;
          exit(1) ;
        }
    }
}


Cursor
load_cursor(sbuf)
short sbuf[16] ;
{
  char cbuf[32] ;
  int i ;

  for (i = 0; i < 16; i++)
    {
      cbuf[i*2+0] = revtable[(sbuf[i] >> 8) & 0xFF] ;
      cbuf[i*2+1] = revtable[sbuf[i] & 0xFF] ;
    }
  help_pixmap = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen), cbuf,
                                     16, 16, foregnd, backgnd, 1) ;
  return(XCreatePixmapCursor(dpy, help_pixmap, help_pixmap,
                                    &FGcolor, &BGcolor, 0, 0)) ;
}


Pixmap
load_icon(sbuf)
short sbuf[] ;
{
  GC pix_gc ;
  Pixmap pixmap ;
  XImage *image ;
  char cbuf[512*8] ;
  int i ;

  if (iscolor)
    {
      for (i = 0; i < (256*8); i++)
        {
          cbuf[i*2+0] = palette[(sbuf[i] >> 8) & 0xFF] ;
          cbuf[i*2+1] = palette[sbuf[i] & 0xFF] ;
        }
      pix_gc = DefaultGC(dpy, screen) ;
      image = XCreateImage(dpy, DefaultVisual(dpy, screen),
                           scr_depth, ZPixmap, 0, cbuf, 64, 64, 8, 64) ;
      pixmap = XCreatePixmap(dpy, RootWindow(dpy, screen),
                             ICONWIDTH, (unsigned) image->height, scr_depth) ;
      XPutImage(dpy, pixmap, pix_gc, image, 0, 0, 0, 0,
                ICONWIDTH, (unsigned) image->height) ;
    }
  else
    {
      for (i = 0; i < 256; i++)
        {
          cbuf[i*2+0] = revtable[(sbuf[i] >> 8) & 0xFF] ;
          cbuf[i*2+1] = revtable[sbuf[i] & 0xFF] ;
        }
      pixmap = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen), cbuf,
                                     64, 64, foregnd, backgnd, scr_depth) ;
    }
  return(pixmap) ;
}


make_frames(argc, argv)
int argc ;
char *argv[] ;
{
  unsigned int h, w ;       /* Window dimensions. */
  int flags ;
  int x, y ;                /* Window position. */
           
  load_colors() ;
  if (iscolor) calctool_icon = load_icon(cicon_image) ;
  else calctool_icon = load_icon(icon_image) ;

  size.flags = PMinSize | PMaxSize | PPosition | PSize ;
  size.x = 0 ;
  size.y = 0 ;
  size.max_width = size.min_width = size.width = TWIDTH ;
  size.max_height = size.min_height = size.height = THEIGHT + DISPLAY ;

  if (strlen(geometry))
    {
      flags = XParseGeometry(geometry, &x, &y, &w, &h) ;
      if (XValue & flags)
        {
          if (XNegative & flags)
            x = DisplayWidth(dpy, screen) + x - size.width ;
            size.flags |= USPosition ;
            size.x = x ;
        }
      if (YValue & flags)
        {
          if (YNegative & flags)
            y = DisplayHeight(dpy, screen) + y - size.height ;
            size.flags |= USPosition ;
            size.y = y ;
        }      
    }

  frame = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
                              size.x, size.y, size.width, size.height,
                              CALCTOOL_BORDER_WIDTH, foregnd, backgnd) ;

  rframe = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
                               size.x + TWIDTH + 15, size.y,
                               size.width, 200,
                               CALCTOOL_BORDER_WIDTH, foregnd, backgnd) ;

  protocol_atom = XInternAtom(dpy, "WM_PROTOCOLS", False) ;
  kill_atom = XInternAtom(dpy, "WM_DELETE_WINDOW", False) ;

  XSetStandardProperties(dpy, frame, "calctool", NULL, calctool_icon,
                         argv, argc, &size) ;

  wm_hints.icon_x = ix ;
  wm_hints.icon_y = iy ;
  wm_hints.input = True ;
  wm_hints.icon_pixmap = calctool_icon ;
  wm_hints.flags = IconPositionHint | InputHint | IconPixmapHint ;
  if (iconic)
    {
      wm_hints.initial_state = IconicState ;
      wm_hints.flags |= StateHint ;
    }
  XSetWMHints(dpy, frame, &wm_hints) ;

  gc_mask = GCFont | GCForeground | GCBackground | GCGraphicsExposures ;
  gc_val.font = nfont->fid ;
  gc_val.foreground = foregnd ;
  gc_val.background = backgnd ;
  gc_val.graphics_exposures = False ;
  gc = XCreateGC(dpy, RootWindow(dpy, screen), gc_mask, &gc_val) ;
  XSetFunction(dpy, gc, GXcopy) ;

  main_cursor = XCreateFontCursor(dpy, XC_top_left_arrow) ;
  FGcolor.red = FGcolor.green = FGcolor.blue = 0 ;
  BGcolor.red = BGcolor.green = BGcolor.blue = 0xffff ;
  help_cursor = load_cursor(help_cursor_array) ;
}


make_icon() {}        /* Null routine - icon created in make_frame. */


make_items()
{
  XSelectInput(dpy, frame, FRAME_MASK) ;
  XMapWindow(dpy, frame) ;

  XSelectInput(dpy, rframe, ExposureMask) ;
}


make_subframes() {}          /* Null routine, see the make_frame routine. */


process_expose(event)
XExposeEvent *event ;
{
  int doframe, dorframe ;

  doframe = dorframe = 0 ;
  do
    {
      if (event->count == 0)
        {
               if (event->window == frame) doframe++ ;
          else if (event->window == rframe) dorframe++ ;
        }
    }
  while (XCheckMaskEvent(dpy, ExposureMask, event)) ;
 
  if (dorframe && rstate) make_registers() ;
  if (doframe) return(CFRAME_REPAINT) ;
}


set_cursor(type)
int type ;
{
  switch (type)
    {
      case HELPCURSOR : XDefineCursor(dpy, frame, help_cursor) ;
                        break ;
      case MAINCURSOR : XDefineCursor(dpy, frame, main_cursor) ;
    }
}


start_tool()
{
  while (1)
    process_event(get_next_event()) ;
}


toggle_reg_canvas()
{
  rstate = !rstate ;
  if (rstate) XMapWindow(dpy, rframe) ;
  else XUnmapWindow(dpy, rframe) ;
}
