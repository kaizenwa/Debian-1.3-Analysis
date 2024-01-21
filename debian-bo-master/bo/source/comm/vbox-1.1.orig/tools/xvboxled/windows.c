/* 
   windows.c
   contains some general, rather low-level window handling functions.
*/

#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "windows.h"

/*----------------------------------------------------------------------*/
/* Create a window. Arguments are:                                      */
/* <win> - pointer to a window data structure;                          */
/* <x><y><w><h> - Position & dimensions of the Window                   */
/* <label> - Window name (appears in title bar if window has one)       */
/* <bg><fg> - background/foreground color                               */
/* <font> - the window default font                                     */
/* The window is _not_ mapped!                                          */
/*----------------------------------------------------------------------*/
void create_window (Win *win, Window parent, int x, int y, int w, int h, 
                    char *label, unsigned long bg, unsigned long fg, 
                    XFontStruct *font) {
  XGCValues gcvalues;
  unsigned long gcvaluemask = 0;
  
  strncpy(win->name,label,40);
  win->x = x; 
  win->y = y; 
  win->w = w; 
  win->h = h; 
  win->window = XCreateSimpleWindow(display,parent,x,y,w,h,0,0,bg);

  win->hints.flags = PPosition | PSize;
  win->hints.x = x;
  win->hints.y = y;
  win->hints.width = w;
  win->hints.height = h;
  XSetStandardProperties(display,win->window,label,NULL,None,0,0,&win->hints);

  win->gc = XCreateGC(display,win->window,gcvaluemask,&gcvalues);
  win->font = font;
  XSetFont(display, win->gc, font->fid);
  XSetForeground(display,win->gc,fg);
  XSetBackground(display,win->gc,bg);
}

/*----------------------------------------------------------------------*/
/* Map the window of the Win structure after checking if the window is  */
/* not already mapped. The <raised> argument decides if the window is   */
/* to be mapped normally with XMapWindow() (raised = 0) or as raised    */
/* with XMapRaised() (raised != 0).                                     */
/* Returns 1 if the window was not mapped before the call and 0 if it   */
/* was mapped (so nothing had to be done).                              */
/*----------------------------------------------------------------------*/
int map_window (Win *win, int raised) {
  XWindowAttributes attr;
  XGetWindowAttributes(display,win->window,&attr);
  if (attr.map_state == IsUnmapped) {
    if (raised) XMapRaised(display,win->window);
    else XMapWindow(display,win->window);
    return(1);
  }
  return(0);
}


/*----------------------------------------------------------------------*/
/* Pops all exposure events for <window> from the event queue           */
/*----------------------------------------------------------------------*/
void discard_exposures (Window window) {
  XEvent e;
  while (XCheckTypedWindowEvent(display,window,Expose,&e));
}
