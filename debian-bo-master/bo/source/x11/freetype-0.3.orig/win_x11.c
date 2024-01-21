/****************************************************************************}
{*                                                                          *}
{*  win_x11 : X11R6 Windowed Display support.                               *}
{*                                                                          *}
{*  This utility component is in charge of managing the display window      *}
{*  used by the X11 FreeType viewer. Note that the Vio pointer is not       *}
{*  in VRAM anymore. It should thus be used to debug the rasterizer.        *}
{*                                                                          *}
{****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "win_x11.h"

#if 0  /* XIMAGE not yet supported by gray-scale routines */
/* this code works on Linux; IMHO, it'll work elsewhere too
   but it needs some testing -- peak@kerberos.troja.mff.cuni.cz */
#define USE_XIMAGE
#endif

  extern Process_Input(char);
  /* This function comes from zoom.c, the glyph viewer */

  char  WindowName[256];

  static Window win;
  static GC gcblack;
  static GC gcwhite;
  static GC gccol[5];

  static Display  *display;
  static char     *displayname="";
  static XEvent   event;
#ifdef USE_XIMAGE
  static XImage *image;
#endif
  static Colormap colormap;
  static int depth;
  static Bool gray;

  /* window size */

  static int x = 0;
  static int y = 0;
  static int w = 640;
  static int h = 480;

  char*  Vio;

  long  VioBufOfs;
  int   Vio_ScanLineWidth;

  /* Restores Screen to its original state */

  void  RestoreScreen()
  {
    XUnmapWindow( display, win );
    XCloseDisplay( display );
  }

  /* Sets graphics mode */

  void  x11init()
  {
    int screen_num;
    XTextProperty xtp;
    XSizeHints xsh;
    XColor color[5];
    unsigned short colors[5] = { 0, 24, 40, 50, 63 }; 
    int i;

    XrmInitialize();

    if(!(display = XOpenDisplay( displayname )))
    {
      fprintf( stderr,"Cannot open Display\n" );
      exit(1);
    }

    Vio = (PByte) malloc(1024*64);

    if ( !Vio )
    {
      fprintf( stderr,"Cannot malloc Display mem\n" );
      exit( 1 );
    }

    /*  Vio_ScanLineWidth = 80; */
    /* Don't forget to change this for the 256 colors modes */

    screen_num=DefaultScreen( display );
    win = XCreateSimpleWindow( display, RootWindow(display, screen_num),
                               x, y, w, h, 10,
                               BlackPixel(display, screen_num),
                               WhitePixel(display,screen_num) );

    XMapWindow( display, win );
    XSelectInput( display, win, KeyPressMask );
    colormap = DefaultColormap( display, screen_num );
    depth = DefaultDepth( display, screen_num );
 
    gcblack = XCreateGC( display, RootWindow(display, screen_num), 0L, NULL );

    XSetForeground( display, gcblack, BlackPixel(display, screen_num) );
    XSetBackground( display, gcblack, WhitePixel(display, screen_num) );

    gcwhite = XCreateGC( display, RootWindow(display, screen_num), 0L, NULL );

    XSetForeground( display, gcwhite, WhitePixel(display, screen_num) );
    XSetBackground( display, gcwhite, BlackPixel(display, screen_num) );

    /* Allocate Colors */

    for ( i = 0; i < 5; i++ )
    {
      XGCValues values;

      color[i].red =
      color[i].green =
      color[i].blue = 65535 - (colors[i] * 65535) / 63;
      if ( !XAllocColor(display, colormap, &color[i]) )
      {
        fprintf( stderr, "Cannot allocate Color\n" );
        exit( 1 );
      }
      values.foreground = color[i].pixel;
      gccol[i] = XCreateGC( display, RootWindow(display,screen_num),
                            GCForeground, &values );
    }

#ifdef USE_XIMAGE
    image = XCreateImage( display, DefaultVisual(display, 0),
                          1, XYBitmap, 0, Vio, 640, 800, 8, 0 );
    if ( !image )
    {
      fprintf( stderr,"Cannot create image\n" );
      exit( 1 );
    }
    /* is this portable? I should consult my Xlib manual -- peak */
    image->byte_order = MSBFirst;
    image->bitmap_bit_order = MSBFirst;
#endif /* USE_XIMAGE */

    /* Make Window Manager happy :-) */
    xtp.value = "FreeType";
    xtp.encoding = 31;
    xtp.format = 8;
    xtp.nitems = strlen( xtp.value );

    xsh.x = x;
    xsh.y = y;
    xsh.width  = w;
    xsh.height = h;
    xsh.flags = (PPosition | PSize);
    xsh.flags = 0;

    XSetWMProperties( display, win, &xtp, &xtp, NULL, 0, &xsh, NULL, NULL );

    XFlush( display );
  }


  void  SetGraphScreen( int mode )
  {
    if ( mode == FS_Graphics_Gray )
    {
      gray = 1;
      Vio_ScanLineWidth = 320;
      x = 0;
      y = 0;
      w = 320;
      h = 200;

    }
    else if ( mode == FS_Graphics_Mono )
    {
      gray = 0;
      Vio_ScanLineWidth = 80;
      x = 0;
      y = 0;
      w = 640;
      h = 480;
    }
    else
    {
      fprintf( stderr, "mode %d not supported\n", mode );
      exit( 1 );
    }
    x11init();
  }

  void  Display_Bitmap_On_Screen( PByte buffer, Int line, Int col )
  {
    int    y,z,b;
    char*  target;

    XClearWindow( display, win );

    /* this displays the index in the window title */
    XStoreName( display, win, WindowName );

    target = Vio + ( line-1 ) * Vio_ScanLineWidth;

    for ( y = 0; y < line; y++ )
    {
      memcpy( target, buffer, col );
      target -= Vio_ScanLineWidth;
      buffer += col;
    }

#ifndef USE_XIMAGE
    for ( y = 0; y < line; y++ )
    {
      for ( z = 0; z < col; z++ )
      {
        char c=Vio[y*Vio_ScanLineWidth+z];
        if (gray)
        { 
          switch (c)
          {
          case 0: 
            XDrawPoint(display,win,gccol[0],z,y);
            break;
          case 23:
            XDrawPoint(display,win,gccol[1],z,y);
            break;
          case 27:
            XDrawPoint(display,win,gccol[2],z,y);
            break;
          case 29:
            XDrawPoint(display,win,gccol[3],z,y);
            break;
          case 31:
            XDrawPoint(display,win,gccol[4],z,y);
            break;
          default:
            printf("%d\n",c);
          }
        }
        else
          for ( b = 7; b >=0 ; b-- )
          {
            if ( (1 << b) & c )
              XDrawPoint( display, win, gcblack, z*8 + 7 - b, y );
          }
      }
    }
#else /* USE_XIMAGE */
    /* much faster! */
    XPutImage( display, win, gcblack, image, 0, 0, 0, 0, col*8, line );
#endif

    XFlush( display );
  }


  /* This routine is called from zoom.c to manage the viewer's events  */
  /* The viewer will probably split into two distinct versions, as     */
  /* procedural and event-based programming are too relatively distant */
  /* concepts..                                                        */
  /*                                                   - David         */

  void  X11_events()
  {
    int count,i;
    char buf[10];
    int bsize= sizeof(buf);
    KeySym key;
    XComposeStatus compose;
  
    while(1)
    {
      XNextEvent(display,&event);

      switch(event.type)
      {
      case KeyPress:
        count = XLookupString( (XKeyEvent*)&event, buf, bsize,
                               &key, &compose );
        i = 0;

        while (count--)
        {
          if ( Process_Input (buf[i]) ) return;
          i++;
        }
        break;
  
      case MappingNotify:
        XRefreshKeyboardMapping( (XMappingEvent*) &event );
        break;
      }
    }
  }
