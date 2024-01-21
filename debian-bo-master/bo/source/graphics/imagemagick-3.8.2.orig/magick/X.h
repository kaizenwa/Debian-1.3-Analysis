#undef False
#undef True
#define XLIB_ILLEGAL_ACCESS  1
#if !defined(macintosh)
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#else
#include <Xos.h>
#include <Xlib.h>
#include <Xutil.h>
#include <Xresource.h>
#include <Xproto.h>
#include <Xatom.h>
#include <cursorfont.h>
#include <keysym.h>
#endif
#ifdef HasShape
#include <X11/extensions/shape.h>
#endif
#ifdef HasSharedMemory
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#endif
#undef index
#if defined(_AIX) || defined(hpux9)
#define XFD_SET  int
#else
#define XFD_SET  fd_set
#endif

/*
  Default colors declarations.
*/
#define BackgroundColor  "#bdbdbd"  /* gray */
#define BorderColor  "#bdbdbd"  /* gray */
#define ForegroundColor  "#000"  /* black */
#define MatteColor  "#bdbdbd"  /* gray */
#define Pen0Color  "#bdbdbd"  /* gray */
#define Pen1Color  "#000000"  /* black */
#define Pen2Color  "#0000ff"  /* blue */
#define Pen3Color  "#00ffff"  /* cyan */
#define Pen4Color  "#00ff00"  /* green */
#define Pen5Color  "#bdbdbd"  /* gray */
#define Pen6Color  "#ff0000"  /* red */
#define Pen7Color  "#ff00ff"  /* magenta */
#define Pen8Color  "#ffff00"  /* yellow */
#define Pen9Color  "#ffffff"  /* white */
/*
  Define declarations.
*/
#define MaxNumberFonts  11
#define MaxNumberPens  11 
#define SuspendTime  50
#define XGammaPixel(map,gamma,color,dx)  (unsigned long) (map->base_pixel+ \
  ((gamma[(color)->red].red*map->red_max+(1 << (dx-1)))/((1 << dx)-1))* \
    map->red_mult+ \
  ((gamma[(color)->green].green*map->green_max+(1 << (dx-1)))/((1 << dx)-1))* \
    map->green_mult+ \
  ((gamma[(color)->blue].blue*map->blue_max+(1 << (dx-1)))/((1 << dx)-1))* \
    map->blue_mult)
#define XStandardPixel(map,color,dx)  (unsigned long) (map->base_pixel+ \
  (((color).red*map->red_max+(1 << (dx-1)))/((1 << dx)-1))*map->red_mult+ \
  (((color).green*map->green_max+(1 << (dx-1)))/((1 << dx)-1))*map->green_mult+\
  (((color).blue*map->blue_max+(1 << (dx-1)))/((1 << dx)-1))*map->blue_mult)

/*
  Enumeration declarations.
*/
typedef enum
{
  ForegroundStencil,
  BackgroundStencil,
  OpaqueStencil,
  TransparentStencil
} AnnotationStencil;

typedef enum
{
  UndefinedMode,
  FrameMode,
  UnframeMode,
  ConcatenateMode
} MontageMode;

typedef enum
{
  UndefinedColormap,
  PrivateColormap,
  SharedColormap
} XColormapType;

/*
  Typedef declarations.
*/
typedef struct _DiversityPacket
{
  Quantum
    red,
    green,
    blue;

  unsigned short
    index;

  unsigned long
    count;
} DiversityPacket;

typedef struct _XAnnotateInfo
{
  int
    x,
    y;

  unsigned int
    width,
    height;

  AnnotationStencil
    stencil;

  double
    degrees;

  XFontStruct
    *font_info;

  char
    *text,
    geometry[MaxTextExtent];

  struct _XAnnotateInfo
    *previous,
    *next;
} XAnnotateInfo;

typedef struct _XDrawInfo
{
  int
    x,
    y;

  unsigned int
    width,
    height;

  char
    geometry[MaxTextExtent];

  double
    degrees;

  AnnotationStencil
    stencil;

  PrimitiveType
    primitive;

  Pixmap
    stipple;

  unsigned int
    line_width;

  XSegment
    line_info;

  RectangleInfo
    rectangle_info;

  unsigned int
    number_coordinates;

  XPoint
    *coordinate_info;
} XDrawInfo;

typedef struct _XMontageInfo
{
  char
    filename[MaxTextExtent];

  unsigned int
    frame,
    shadow;

  CompositeOperator
    compose;

  unsigned int
    pointsize;
 
  char
    *tile,
    *texture;
} XMontageInfo;

typedef struct _XPixelInfo
{
  unsigned int
    colors;

  unsigned long
    *pixels;

  XColor
    foreground_color,
    background_color,
    border_color,
    matte_color,
    highlight_color,
    shadow_color,
    depth_color,
    trough_color,
    box_color,
    pen_color,
    pen_colors[MaxNumberPens],
    *gamma_map;

  unsigned short
    box_index,
    pen_index;

  GC
    annotate_context,
    highlight_context,
    widget_context;
} XPixelInfo;

typedef struct _XResourceInfo
{
  XrmDatabase
    resource_database;

  ImageInfo
    *image_info;

  unsigned int
    close_server,
    backdrop;

  char
    *background_color,
    *border_color;

  unsigned int
    border_width;

  char
    *browse_command,
    *client_name;

  XColormapType
    colormap;

  unsigned int
    color_recovery;

  ColorspaceType
    colorspace;

  unsigned int
    confirm_exit,
    debug,
    delay;

  char
    *display_gamma;

  unsigned int
    display_warnings,
    dither;

  char
    *editor_command,
    *font,
    *font_name[MaxNumberFonts],
    *foreground_color;

  unsigned int
    gamma_correct;

  int
    gravity;

  char
    home_directory[MaxTextExtent],
    *icon_geometry;

  unsigned int
    iconic,
    immutable;

  char
    *image_geometry,
    *launch_command;

  unsigned int
    magnify;

  char
    *map_type,
    *matte_color;

  unsigned int
    monochrome;

  char
    *name;

  unsigned int
    number_colors,
    pause;

  char
    *pen_colors[MaxNumberPens],
    *print_command;

  int
    quantum;

  char
    *server_name,
    *text_font,
    *title;

  unsigned int
    tree_depth,
    update,
    undo_cache,
    use_pixmap,
    use_shared_memory;

  char
    *visual_type,
    *window_group,
    *window_id,
    *write_filename;
} XResourceInfo;

typedef struct _XWindowInfo
{
  Window
    id;

  int
    screen;

  Window
    root;

  Visual
    *visual;

  int
    class,
    depth;

  XVisualInfo
    *visual_info;

  XStandardColormap
    *map_info;

  XPixelInfo
    *pixel_info;

  XFontStruct
    *font_info;

  GC
    annotate_context,
    highlight_context,
    widget_context;

  Cursor
    cursor,
    busy_cursor;

  char
    *name,
    *geometry,
    *icon_name,
    *icon_geometry,
    *crop_geometry;

  unsigned int
    data;

  unsigned long
    flags;

  int
    x,
    y;

  unsigned int
    width,
    height,
    min_width,
    min_height,
    width_inc,
    height_inc,
    border_width,
    use_pixmap,
    immutable,
    shape;

  XImage
    *ximage,
    *matte_image;

  Pixmap
    highlight_stipple,
    shadow_stipple,
    pixmap,
    matte_pixmap,
    *pixmaps;

  int
    mask;

  XSetWindowAttributes
    attributes;

  XWindowChanges
    window_changes;

  int
    shared_memory;

#ifdef HasSharedMemory
  XShmSegmentInfo
    segment_info[4];
#endif

  unsigned int
    orphan,
    mapped,
    stasis;
} XWindowInfo;

typedef struct _XWindows
{
  XWindowInfo
    context,
    group_leader,
    backdrop,
    icon,
    image,
    info,
    magnify,
    pan,
    command,
    widget,
    popup;

  Atom
    wm_protocols,
    wm_delete_window,
    wm_take_focus,
    im_protocols,
    im_update_widget,
    im_update_colormap,
    im_update_signature,
    im_former_image,
    im_retain_colors,
    im_next_image,
    im_exit,
    dnd_protocols;
} XWindows;

/*
  X utilities routines.
*/
extern char
  *XGetResourceClass(XrmDatabase,const char *,const char *,char *),
  *XGetResourceInstance(XrmDatabase,const char *,const char *,char *),
  *XVisualClassName(const int);

extern Cursor
  XMakeCursor(Display *,Window,Colormap,char *,char *);

extern Image
  *XMontageImages(const XResourceInfo *,XMontageInfo *,Image *),
  *ReadXImage(ImageInfo *,const unsigned int,const unsigned int,
    const unsigned int,unsigned int),
  *XGetWindowImage(Display *,const Window,const unsigned int,
    const unsigned int);

extern int
  Latin1Compare(const char *,const char *),
  XError(Display *,XErrorEvent *);

extern unsigned int
  IsTrue(const char *),
  XAnnotateImage(Display *,const XPixelInfo *,const XAnnotateInfo *,Image *),
  XDrawImage(Display *,const XPixelInfo *,const XDrawInfo *,Image *),
  XGetWindowColor(Display *,char *),
  XMakeImage(Display *,const XResourceInfo *,XWindowInfo *,Image *,
    unsigned int,unsigned int),
  XMakePixmap(Display *,const XResourceInfo *,XWindowInfo *),
  XQueryColorDatabase(const char *,XColor *);

extern void
  XBestIconSize(Display *,XWindowInfo *,Image *),
  XBestPixel(Display *,const Colormap,XColor *,unsigned int,XColor *),
  XCheckRefreshWindows(Display *,XWindows *),
  XClientMessage(Display *,const Window,const Atom,const Atom,const Time),
  XConstrainWindowPosition(Display *,XWindowInfo *),
  XDelay(Display *,const unsigned long),
  XDestroyWindowColors(Display *,Window),
  XDisplayImageInfo(Display *,const XResourceInfo *,XWindows *,Image *,Image *),
  XFreeResources(Display *,XVisualInfo *,XStandardColormap *,XPixelInfo *,
    XFontStruct *,XResourceInfo *,XWindowInfo *),
  XFreeStandardColormap(Display *,const XVisualInfo *,XStandardColormap *,
    XPixelInfo *),
  XGetAnnotateInfo(XAnnotateInfo *),
  XGetMapInfo(const XVisualInfo *,const Colormap,XStandardColormap *),
  XGetMontageInfo(XMontageInfo *),
  XGetPixelInfo(Display *,const XVisualInfo *,const XStandardColormap *,
    const XResourceInfo *,Image *,XPixelInfo *),
  XGetResourceInfo(XrmDatabase,char *,XResourceInfo *),
  XGetWindowInfo(Display *,XVisualInfo *,XStandardColormap *,XPixelInfo *,
    XFontStruct *,XResourceInfo *,XWindowInfo *),
  XHighlightEllipse(Display *,Window,GC,const RectangleInfo *),
  XHighlightLine(Display *,Window,GC,const XSegment *),
  XHighlightRectangle(Display *,Window,GC,const RectangleInfo *),
  XMakeMagnifyImage(Display *,XWindows *),
  XMakeStandardColormap(Display *,XVisualInfo *,XResourceInfo *,Image *,
    XStandardColormap *,XPixelInfo *),
  XMakeWindow(Display *,Window,char **,int,XClassHint *,XWMHints *,
    XWindowInfo *),
  XQueryPosition(Display *,const Window,int *,int *),
  XRefreshWindow(Display *,const XWindowInfo *,const XEvent *),
  XRetainWindowColors(Display *,const Window),
  XUserPreferences(XResourceInfo *),
  XSetCursorState(Display *,XWindows *,const unsigned int);

extern Window
  XClientWindow(Display *,Window),
  XGetSubwindow(Display *,Window,int,int),
  XSelectWindow(Display *,RectangleInfo *),
  XWindowByID(Display *,const Window,const unsigned long),
  XWindowByName(Display *,const Window,const char *),
  XWindowByProperty(Display *,const Window,const Atom);

extern XFontStruct
  *XBestFont(Display *,const XResourceInfo *,const unsigned int);

extern XrmDatabase
  XGetResourceDatabase(Display *,char *);

extern XVisualInfo
  *XBestVisualInfo(Display *,XStandardColormap *,XResourceInfo *);

/*
  Variable declarations
*/
extern char
  *client_name;

/*
  Invoke pre-X11R6 ICCCM routines if XlibSpecificationRelease is not 6.
*/
#if XlibSpecificationRelease < 6
#ifndef PRE_R6_ICCCM
#define PRE_R6_ICCCM
#endif
#endif
/*
  Invoke pre-X11R5 ICCCM routines if XlibSpecificationRelease is not defined.
*/
#ifndef XlibSpecificationRelease
#define PRE_R5_ICCCM
#endif
/*
  Invoke pre-X11R4 ICCCM routines if PWinGravity is not defined.
*/
#ifndef PWinGravity
#define PRE_R4_ICCCM
#endif
#include "PreRvIcccm.h"
