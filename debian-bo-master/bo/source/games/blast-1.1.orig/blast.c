#ifndef lint
static char SccsId[] = "@(#)blast.c    V1.1    9/9/92";
#endif

/* blast.c
   Written by Daniel Dee  7/23/92

   Copyright (c) 1992 Daniel Dee
   This program is free software; you may
   redistribute it and/or modify it under the terms of
   the GNU General Public License as published by the
   Free Software Foundation; either version, or
   any later version.

   email: ddee@vicorp.com
   V.I. Corporation
   47 Pleasant Street
   Northampton, MA 01060
*/

#include <stdio.h>

#ifndef VMS
/* Header files for Unix version */
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/extensions/shape.h>

#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#else
/* Header files for VMS version */
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Intrinsic.h>
#include <decw$include/Xatom.h>
#include <decw$include/keysym.h>
#endif /* VMS */

#define INITIAL_MAXWIN 100
#define AUDIO_DEVICE "/dev/audio"
#define FILENAME "shooting.au"

static void BlastWindow();
static void SaveWindow();
static Window GetWindowToBlast();
static void RepairWindows();
static void MakeNoise();
static FILE* OpenSoundFile();
static void CloseSoundFile();

typedef struct 
{
  Window window;
  Bool  bshaped;  
  XRectangle *bound;
  int bcount;
  int border;  
  Bool cshaped;  
  XRectangle *clip;
  int ccount;
  int corder;  
}
WinList;

main( argc, argv )
int argc;
char *argv[];
{
  Display *display;
  Window window, root;
  long screen;  
  GC gc;
  XEvent event;
  XGCValues gcvalues;   
  Pixmap shape_mask;
  WinList *winlist;
  int numwin = 0;  
  int maxwin = INITIAL_MAXWIN;
  FILE *fp, *afp;  

  /* Open display connection and create a window */
  if( (display = XOpenDisplay( "" )) == (Display*)NULL ) 
  {
    fprintf( stderr, "Invalid display\n" );
    exit(0);
  }

  winlist = (WinList*)malloc( maxwin * sizeof(WinList) );
  screen = DefaultScreen(display);
  root = RootWindow( display, screen );  

  gcvalues.foreground = 1;
  gcvalues.background = 0;
  shape_mask = XCreatePixmap( display, root,
			     DisplayWidth(display,screen), 
			     DisplayHeight(display,screen), 1);
  gc = XCreateGC(display, shape_mask, GCForeground | GCBackground, &gcvalues);

  afp = OpenSoundFile();  

  /* Main event loop */
  for(;;)
  {
    window = GetWindowToBlast(display, screen, &event, winlist, 
			      &numwin, &maxwin);
    switch( event.xbutton.button )
    {
    case Button1:
      BlastWindow( display, window, gc, screen, shape_mask, afp );
      break;
    case Button2:
      RepairWindows( display, root, winlist, numwin );
      break;
    case Button3:
      CloseSoundFile( afp );
      exit(0);      
    }
  }
}


static void BlastWindow( display, window, gc, screen, shape_mask, afp )
Display *display;
Window window;
GC gc;
long screen;
Pixmap shape_mask;
FILE *afp;
{
  int x, y, root_x, root_y;
  unsigned int bwidth, dummy;
  unsigned int key_buttons;
  Window root, child;

  XGetGeometry( display, window, &root, &x, &y,
	       &dummy, &dummy, &bwidth, &dummy );  

#if 0
  window = XmuClientWindow (display, window);
#endif

  XSetForeground( display, gc, 1 );
  XFillRectangle( display, shape_mask, gc, 0, 0, 
		 DisplayWidth(display,screen),
		 DisplayHeight(display,screen) );
  
  XQueryPointer(display, window, &root, &child, &root_x, 
		&root_y, &x, &y, &key_buttons );

  XSetForeground( display, gc, 0 );
  XFillArc( display, shape_mask, gc, x-25, y-25, 50, 50, 0, 360 * 64);
  XShapeCombineMask(display,window,ShapeBounding, -bwidth, -bwidth, 
		    shape_mask, 
		    ShapeIntersect );
  MakeNoise(afp, FILENAME);  
}


static Window GetWindowToBlast(display, screen, event, winlist, numwin, maxwin)
Display *display;
long screen;
XEvent *event;
WinList *winlist;
int *numwin, *maxwin;
{
  int status;
  Cursor cursor;
  Window target_win = None, root = RootWindow(display,screen);
  int buttons = 0;

  /* Make the target cursor */
  cursor = XCreateFontCursor(display, XC_crosshair);

  /* Grab the pointer using target cursor, letting it room all over */
  status = XGrabPointer(display, root, False,
			ButtonPressMask|ButtonReleaseMask, GrabModeSync,
			GrabModeAsync, root, cursor, CurrentTime);
#if 0
  if (status != GrabSuccess) Fatal_Error("Can't grab the mouse.");
#endif

  /* Let the user select a window... */
  while ((target_win == None) || (buttons != 0)) {
    /* allow one more event */
    XAllowEvents(display, SyncPointer, CurrentTime);
    XWindowEvent(display, root, ButtonPressMask|ButtonReleaseMask, event);
    switch (event->type) 
    {
    case ButtonPress:
      if (target_win == None) 
      {
	target_win = event->xbutton.subwindow; /* window selected */
	if (target_win == None) target_win = root;
      }
      buttons++;
      break;
    case ButtonRelease:
      if (buttons > 0) /* there may have been some down before we started */
	buttons--;
       break;
    }
  } 

  XUngrabPointer(display, CurrentTime);      /* Done with pointer */
  SaveWindow( display, target_win, winlist, numwin, maxwin );  
  return(target_win);
}

static void RepairWindows( display, window, winlist, numwin )
Display *display;
Window window;
WinList *winlist;
int numwin;
{
  Window root, parent, *children;
  int i, num_children;  
#ifdef REPAIR_EVERYTHING_IN_SIGHT
  XQueryTree( display, window, &root, &parent, &children, &num_children );
  for( i=0; i < num_children; i++ )
    XShapeCombineMask(display, children[i], ShapeBounding, 0, 0, 
		      None, ShapeSet );
  XFree( *children );
#endif
  for( i=0; i < numwin; i++ )
  {    
    if( winlist[i].bshaped )
      XShapeCombineRectangles( display, winlist[i].window, ShapeBounding,
			    0, 0, winlist[i].bound, winlist[i].bcount,
			    ShapeSet, winlist[i].border );
    else
      XShapeCombineMask(display, winlist[i].window, ShapeBounding, 0, 0, 
			None, ShapeSet );
      
    if( winlist[i].cshaped )
      XShapeCombineRectangles( display, winlist[i].window, ShapeClip,
			    0, 0, winlist[i].clip, winlist[i].ccount,
			    ShapeSet, winlist[i].corder );
    else
      XShapeCombineMask(display, winlist[i].window, ShapeClip, 0, 0, 
			None, ShapeSet );
  } 
}

static void SaveWindow( display, window, winlist, numwin, maxwin )
Display *display;
Window window;
WinList *winlist;
int *numwin, *maxwin;
{
  int i, count, order, dummy;  
  Bool bshaped, cshaped;
  unsigned int udummy;
  
  for( i=0; i < *numwin; i++ )
    if( window == winlist[i].window )
      return;
  winlist[*numwin].window = window;  

  XShapeQueryExtents(display, window, &bshaped, 
		     &dummy, &dummy, &udummy, &udummy,
		     &cshaped, &dummy, &dummy, &udummy, &udummy);
  
  if( winlist[*numwin].bshaped = bshaped )
  {
    winlist[*numwin].bound = XShapeGetRectangles( display, window, 
					     ShapeBounding, &count, &order );
    winlist[*numwin].bcount = count;
    winlist[*numwin].border = order;  
  }
  
  if( winlist[*numwin].cshaped = cshaped )
  {    
    winlist[*numwin].clip = XShapeGetRectangles( display, window, ShapeClip, 
					    &count, &order );
    winlist[*numwin].ccount = count;
    winlist[*numwin].corder = order;  
  }
  
  (*numwin)++;

  if( *numwin >= *maxwin )
  {    
    winlist = (WinList*)realloc( winlist, 2 * (*maxwin) * sizeof(WinList) );
    *maxwin *= 2;
  }  
  
}

static void MakeNoise( afp, filename )
FILE *afp;
String filename;
{
  unsigned char buf[1024*64];
  int cnt;
  FILE *fp;  
#ifdef sun4  
  fp = (FILE*)open( filename, O_RDONLY, 0 );  
  while( (cnt = read(fp, (char*)buf, sizeof(buf))) > 0 )
    write( afp, (char*)buf, cnt );
  close(fp);  
#endif
}

static FILE* OpenSoundFile()
{
#ifdef sun4
  return (FILE*)open( AUDIO_DEVICE, O_WRONLY | O_NDELAY );  
#endif
}

static void CloseSoundFile( afp )
FILE *afp;
{
#ifdef sun4
  close( afp );
#endif
}
