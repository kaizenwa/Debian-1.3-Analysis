/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              AAA   N   N  IIIII  M   M   AAA   TTTTT  EEEEE                 %
%             A   A  NN  N    I    MM MM  A   A    T    E                     %
%             AAAAA  N N N    I    M M M  AAAAA    T    EEE                   %
%             A   A  N  NN    I    M   M  A   A    T    E                     %
%             A   A  N   N  IIIII  M   M  A   A    T    EEEEE                 %
%                                                                             %
%                                                                             %
%          Animate Machine Independent File Format Image via X11.             %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. du Pont de Nemours    %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. du Pont de Nemours and Company makes no representations %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. du Pont de Nemours and Company disclaims all warranties with regard  %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. du Pont de Nemours and Company be     %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Animate displays a sequence of images in the MIFF format on any
%  workstation display running an X server.  Animate first determines the
%  hardware capabilities of the workstation.  If the number of unique
%  colors in an image is less than or equal to the number the workstation
%  can support, the image is displayed in an X window.  Otherwise the
%  number of colors in the image is first reduced to match the color
%  resolution of the workstation before it is displayed.
%
%  This means that a continuous-tone 24 bits-per-pixel image can display on a
%  8 bit pseudo-color device or monochrome device.  In most instances the
%  reduced color image closely resembles the original.  Alternatively, a
%  monochrome or pseudo-color image can display on a continuous-tone 24
%  bits-per-pixel device.
%
%  The Animate program command syntax is:
%
%  Usage: animate [options ...] file [ [options ...] file ...]
%
%  Where options include:
%    -backdrop            display image centered on a backdrop
%    -colormap type       Shared or Private
%    -colors value        preferred number of colors in the image
%    -colorspace type     GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV
%    -crop geometry       preferred size and location of the cropped image
%    -delay milliseconds  display the next image after pausing
%    -density geometry    vertical and horizontal density of the image
%    -display server      display image to this X server
%    -dither              apply Floyd/Steinberg error diffusion to image
%    -gamma value         level of gamma correction
%    -geometry geometry   preferred size and location of the Image window
%    -interlace type      None, Line, Plane, or Partition
%    -map type            display image using this Standard Colormap
%    -matte               store matte channel if the image has one
%    -monochrome          transform image to black and white
%    -scene value         image scene number
%    -size geometry       width and height of image
%    -treedepth value     depth of the color classification tree
%    -verbose             print detailed information about the image
%    -visual type         display image using this visual type
%    -window id           display image to background of this window
%
%  In addition to those listed above, you can specify these standard X
%  resources as command line options:  -background, -bordercolor,
%  -borderwidth, -font, -foreground, -iconGeometry, -iconic, -name,
%  -mattecolor, -shared_memory, or -title.
%
%  Change '-' to '+' in any option above to reverse its effect.  For
%  example, specify +matte to suppress any image matte information.
%
%  By default, the image format of `file' is determined by its magic
%  number.  To specify a particular image format, precede the filename
%  with an image format name and a colon (i.e. ps:image) or specify the
%  image type as the filename suffix (i.e. image.ps).  Specify 'file' as
%  '-' for standard input or output.
%
%  Buttons:
%    Press any button to map or unmap the Command widget.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "animate.h"
#include "version.h"

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U s a g e                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Usage displays the program command syntax.
%
%  The format of the Usage routine is:
%
%      Usage(client_name)
%
%  A description of each parameter follows:
%
%    o client_name: a character string representing the name of the client
%      program.
%
%
*/
static void Usage(const char *client_name)
{
  char
    **p;

  static char
    *buttons[]=
    {
      "Press any button to map or unmap the Command widget",
      (char *) NULL
    },
    *options[]=
    {
      "-backdrop            display image centered on a backdrop",
      "-colormap type       Shared or Private",
      "-colors value        preferred number of colors in the image",
      "-colorspace type     GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV",
      "-crop geometry       preferred size and location of the cropped image",
      "-delay milliseconds  display the next image after pausing",
      "-density geometry    vertical and horizontal density of the image",
      "-display server      display image to this X server",
      "-dither              apply Floyd/Steinberg error diffusion to image",
      "-gamma value         level of gamma correction",
      "-geometry geometry   preferred size and location of the Image window",
      "-interlace type      None, Line, Plane, or Partition",
      "-matte               store matte channel if the image has one",
      "-map type            display image using this Standard Colormap",
      "-monochrome          transform image to black and white",
      "-scene value         image scene number",
      "-size geometry       width and height of image",
      "-treedepth value     depth of the color classification tree",
      "-verbose             print detailed information about the image",
      "-visual type         display image using this visual type",
      "-window id          display image to background of this window",
      (char *) NULL
    };

  (void) printf("Version: %s\n\n",Version);
  (void) printf(
    "Usage: %s [-options ...] file [ [-options ...] file ...]\n",client_name);
  (void) printf("\nWhere options include: \n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  (void) printf(
    "\nIn addition to those listed above, you can specify these standard X\n");
  (void) printf(
    "resources as command line options:  -background, -bordercolor,\n");
  (void) printf(
    "-borderwidth, -font, -foreground, -iconGeometry, -iconic, -name,\n");
  (void) printf("-mattecolor, -shared_memory, or -title.\n");
  (void) printf(
    "\nChange '-' to '+' in any option above to reverse its effect.  For\n");
  (void) printf(
    "example, specify +matte to suppress any image matte information.\n");
  (void) printf(
    "\nBy default, the image format of `file' is determined by its magic\n");
  (void) printf(
    "number.  To specify a particular image format, precede the filename\n");
  (void) printf(
    "with an image format name and a colon (i.e. ps:image) or specify the\n");
  (void) printf(
    "image type as the filename suffix (i.e. image.ps).  Specify 'file' as\n");
  (void) printf("'-' for standard input or output.\n");
  (void) printf("\nButtons: \n");
  for (p=buttons; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X A n i m a t e B a c k g r o u n d I m a g e                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XAnimateBackgroundImage animates an image sequence in the
%  background of a window.
%
%  The format of the XAnimateBackgroundImage routine is:
%
%      XAnimateBackgroundImage(display,resource_info,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%
*/

static int SceneCompare(const void *x,const void *y)
{
  Image
    **image_1,
    **image_2;

  image_1=(Image **) x;
  image_2=(Image **) y;
  return((int) (*image_1)->scene-(int) (*image_2)->scene);
}

static void XAnimateBackgroundImage(Display *display,
  XResourceInfo *resource_info,Image *image)
{
  char
    geometry[MaxTextExtent],
    visual_type[MaxTextExtent];

  Image
    **images;

  int
    i,
    scene;

  unsigned int
    height,
    number_scenes,
    sans,
    status,
    width;

  Window
    root_window;

  XEvent
    event;

  XGCValues
    context_values;

  XPixelInfo
    pixel_info,
    scene_info;

  XResourceInfo
    resources;

  XStandardColormap
    *map_info;

  XVisualInfo
    *visual_info;

  XWindowAttributes
    window_attributes;

  XWindowInfo
    window_info;

  /*
    Determine target window.
  */
  resources=(*resource_info);
  window_info.id=(Window) NULL;
  root_window=XRootWindow(display,XDefaultScreen(display));
  if (Latin1Compare(resources.window_id,"root") == 0)
    window_info.id=root_window;
  else
    {
      if (isdigit(*resources.window_id))
        window_info.id=XWindowByID(display,root_window,
          (Window) strtol((char *) resources.window_id,(char **) NULL,0));
      if (window_info.id == (Window) NULL)
        window_info.id=
          XWindowByName(display,root_window,resources.window_id);
    }
  if (window_info.id == (Window) NULL)
    Error("No window with specified id exists",resources.window_id);
  /*
    Determine window visual id.
  */
  window_attributes.width=XDisplayWidth(display,XDefaultScreen(display));
  window_attributes.height=XDisplayHeight(display,XDefaultScreen(display));
  (void) strcpy(visual_type,"default");
  status=XGetWindowAttributes(display,window_info.id,&window_attributes);
  if (status != False)
    (void) sprintf(visual_type,"0x%lx",
      XVisualIDFromVisual(window_attributes.visual));
  /*
    Allocate standard colormap.
  */
  map_info=XAllocStandardColormap();
  if (map_info == (XStandardColormap *) NULL)
    Error("Unable to create standard colormap","Memory allocation failed");
  map_info->colormap=(Colormap) NULL;
  pixel_info.pixels=(unsigned long *) NULL;
  pixel_info.gamma_map=(XColor *) NULL;
  /*
    Initialize visual info.
  */
  resources.map_type=(char *) NULL;
  resources.visual_type=visual_type;
  visual_info=XBestVisualInfo(display,map_info,&resources);
  if (visual_info == (XVisualInfo *) NULL)
    Error("Unable to get visual",resources.visual_type);
  /*
    Initialize window info.
  */
  window_info.ximage=(XImage *) NULL;
  window_info.matte_image=(XImage *) NULL;
  window_info.pixmap=(Pixmap) NULL;
  window_info.matte_pixmap=(Pixmap) NULL;
  window_info.shared_memory=False;
  /*
    Free previous root colors.
  */
  if (window_info.id == root_window)
    XDestroyWindowColors(display,root_window);
  if (visual_info == (XVisualInfo *) NULL)
    Error("Unable to get visual",resources.visual_type);
  if (resources.map_type == (char *) NULL)
    if ((visual_info->class != TrueColor) &&
        (visual_info->class != DirectColor))
      {
        Image
          *next_image;

        unsigned int
          global_colormap;

        /*
          Determine if the sequence of images has the identical colormap.
        */
        global_colormap=True;
        next_image=image;
        for ( ; next_image != (Image *) NULL; next_image=next_image->next)
        {
          next_image->matte=False;
          if ((next_image->class == DirectClass) ||
              (next_image->colors != image->colors) ||
              (next_image->colors > visual_info->colormap_size))
            {
              global_colormap=False;
              continue;
            }
          for (i=0; i < image->colors; i++)
            if (!ColorMatch(next_image->colormap[i],image->colormap[i],0))
              {
                global_colormap=False;
                break;
              }
        }
        if (!global_colormap)
          MapImages(image,(Image *) NULL,resources.dither);
      }
  /*
    Sort images by increasing scene number.
  */
  images=ListToGroupImage(image,&number_scenes);
  if (images == (Image **) NULL)
    Error("Unable to animate images","Memory allocation failed");
  i=0;
  for (scene=0; scene < number_scenes; scene++)
    i+=images[scene]->scene;
  if (i > 0)
    qsort((void *) images,number_scenes,sizeof(Image *),
      (int (*)(const void *, const void *)) SceneCompare);
  /*
    Initialize Standard Colormap.
  */
  resources.colormap=SharedColormap;
  XMakeStandardColormap(display,visual_info,&resources,images[0],map_info,
    &pixel_info);
  /*
    Graphic context superclass.
  */
  context_values.background=pixel_info.background_color.pixel;
  context_values.foreground=pixel_info.foreground_color.pixel;
  pixel_info.annotate_context=XCreateGC(display,window_info.id,GCBackground |
    GCForeground,&context_values);
  if (pixel_info.annotate_context == (GC) NULL)
    Error("Unable to create graphic context",(char *) NULL);
  /*
    Initialize Image window attributes.
  */
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,(XFontStruct *) NULL,
    &resources,&window_info);
  /*
    Create the X image.
  */
  window_info.width=images[0]->columns;
  window_info.height=images[0]->rows;
  (void) sprintf(geometry,"%ux%u>",window_attributes.width,
    window_attributes.height);
  (void) ParseImageGeometry(geometry,&window_info.x,&window_info.y,
    &window_info.width,&window_info.height);
  status=XMakeImage(display,&resources,&window_info,images[0],window_info.width,
    window_info.height);
  if (status == False)
    Error("Unable to create X image",(char *) NULL);
  window_info.x=0;
  window_info.y=0;
  if (resources.debug)
    {
      (void) fprintf(stderr,"Image: %s[%u] %ux%u ",images[0]->filename,
        images[0]->scene,images[0]->columns,images[0]->rows);
      if (images[0]->colors != 0)
        (void) fprintf(stderr,"%uc ",images[0]->colors);
      (void) fprintf(stderr,"%s\n",images[0]->magick);
    }
  /*
    Adjust image dimensions as specified by backdrop or geometry options.
  */
  width=window_info.width;
  height=window_info.height;
  if (resources.backdrop)
    {
      /*
        Center image on window.
      */
      window_info.x=(window_attributes.width >> 1)-
        (window_info.ximage->width >> 1);
      window_info.y=(window_attributes.height >> 1)-
        (window_info.ximage->height >> 1);
      width=window_attributes.width;
      height=window_attributes.height;
    }
  if (resources.image_geometry != (char *) NULL)
    {
      char
        default_geometry[MaxTextExtent];

      int
        flags,
        gravity;

      XSizeHints
        *size_hints;

      /*
        User specified geometry.
      */
      size_hints=XAllocSizeHints();
      if (size_hints == (XSizeHints *) NULL)
        Error("Unable to display on window","Memory allocation failed");
      size_hints->flags=(long) NULL;
      (void) sprintf(default_geometry,"%ux%u",width,height);
      flags=XWMGeometry(display,visual_info->screen,resources.image_geometry,
        default_geometry,window_info.border_width,size_hints,&window_info.x,
        &window_info.y,(int *) &width,(int *) &height,&gravity);
      if (flags & (XValue | YValue))
        {
          width=window_attributes.width;
          height=window_attributes.height;
        }
      XFree((void *) size_hints);
    }
  /*
    Create the X pixmap.
  */
  window_info.pixmap=
    XCreatePixmap(display,window_info.id,width,height,window_info.depth);
  if (window_info.pixmap == (Pixmap) NULL)
    Error("Unable to create X pixmap",(char *) NULL);
  /*
    Display pixmap on the window.
  */
  if ((width > window_info.width) || (height > window_info.height))
    XFillRectangle(display,window_info.pixmap,window_info.annotate_context,
      0,0,width,height);
  XPutImage(display,window_info.pixmap,window_info.annotate_context,
    window_info.ximage,0,0,window_info.x,window_info.y,window_info.width,
    window_info.height);
  XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
  XClearWindow(display,window_info.id);
  /*
    Initialize image pixmaps structure.
  */
  window_info.pixmaps=(Pixmap *) malloc(number_scenes*sizeof(Pixmap));
  if (window_info.pixmaps == (Pixmap *) NULL)
    Error("Unable to animate images","Memory allocation failed");
  window_info.pixmaps[0]=window_info.pixmap;
  scene_info.pixels=(unsigned long *) NULL;
  scene_info.gamma_map=(XColor *) NULL;
  for (scene=1; scene < number_scenes; scene++)
  {
    /*
      Create X image.
    */
    window_info.pixmap=(Pixmap) NULL;
    if ((resources.map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      if (images[scene]->class == PseudoClass)
        {
          /*
            Get pixel info for this scene.
          */
          XGetPixelInfo(display,visual_info,map_info,&resources,images[scene],
            &scene_info);
          window_info.pixel_info=(&scene_info);
        }
    status=XMakeImage(display,&resources,&window_info,images[scene],
      images[scene]->columns,images[scene]->rows);
    if (status == False)
      Error("Unable to create X image",(char *) NULL);
    if (resources.debug)
      {
        (void) fprintf(stderr,"Image: [%u] %s %ux%u ",images[scene]->scene,
          images[scene]->filename,images[scene]->columns,images[scene]->rows);
        if (images[scene]->colors != 0)
          (void) fprintf(stderr,"%uc ",images[scene]->colors);
        (void) fprintf(stderr,"%s\n",images[scene]->magick);
      }
    /*
      Create the X pixmap.
    */
    window_info.pixmap=
      XCreatePixmap(display,window_info.id,width,height,window_info.depth);
    if (window_info.pixmap == (Pixmap) NULL)
      Error("Unable to create X pixmap",(char *) NULL);
    /*
      Display pixmap on the window.
    */
    if ((width > window_info.width) || (height > window_info.height))
      XFillRectangle(display,window_info.pixmap,window_info.annotate_context,
        0,0,width,height);
    XPutImage(display,window_info.pixmap,window_info.annotate_context,
      window_info.ximage,0,0,window_info.x,window_info.y,window_info.width,
      window_info.height);
    XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
    XClearWindow(display,window_info.id);
    window_info.pixmaps[scene]=window_info.pixmap;
    if (images[scene]->matte)
      XClearWindow(display,window_info.id);
    if (resources.delay != 0)
      XDelay(display,(unsigned long) resources.delay);
    else
      XDelay(display,(unsigned long) image->delay*10);
  }
  window_info.pixel_info=(&pixel_info);
  /*
    Display pixmap on the window.
  */
  XSelectInput(display,window_info.id,StructureNotifyMask);
  event.type=Expose;
  do
  {
    for (scene=0; scene < number_scenes; scene++)
    {
      if (XEventsQueued(display,QueuedAfterFlush) > 0)
        {
          XNextEvent(display,&event);
          if (event.type == DestroyNotify)
            break;
        }
      window_info.pixmap=window_info.pixmaps[scene];
      XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
      XClearWindow(display,window_info.id);
      XSync(display,False);
      if (resources.delay != 0)
        XDelay(display,(unsigned long) resources.delay);
      else
        XDelay(display,(unsigned long) images[scene]->delay*10);
    }
  } while (event.type != DestroyNotify);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X A n i m a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XAnimateImages displays an image via X11.
%
%  The format of the XAnimateImages routine is:
%
%      XAnimateImages(display,resource_info,argv,argc,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o argv: Specifies the application's argument list.
%
%    o argc: Specifies the number of arguments.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%
*/
static Image *XAnimateImages(Display *display,XResourceInfo *resource_info,
  char **argv,const int argc,Image *image)
{
#define MagickMenus  3
#define MaxWindows  8
#define MagickTitle  "Commands"

  static char
    *CommandMenu[]=
    {
      "Animate",
      "Speed",
      "Direction",
      "Image Info",
      "Help",
      "Quit",
      (char *) NULL
    },
    *AnimateMenu[]=
    {
      "Open",
      "Play",
      "Step",
      "Repeat",
      "AutoReverse",
      (char *) NULL
    },
    *SpeedMenu[]=
    {
      "Faster",
      "Slower",
      (char *) NULL
    },
    *DirectionMenu[]=
    {
      "Forward",
      "Reverse",
      (char *) NULL
    };

  static char
    **Menus[MagickMenus]=
    {
      AnimateMenu,
      SpeedMenu,
      DirectionMenu
    };

  static CommandType
    CommandMenus[]=
    {
      NullCommand,
      NullCommand,
      NullCommand,
      InfoCommand,
      HelpCommand,
      QuitCommand
    },
    CommandTypes[]=
    {
      OpenCommand,
      PlayCommand,
      StepCommand,
      RepeatCommand,
      AutoReverseCommand
    },
    SpeedCommands[]=
    {
      FasterCommand,
      SlowerCommand
    },
    DirectionCommands[]=
    {
      ForwardCommand,
      ReverseCommand
    };

  static CommandType
    *Commands[MagickMenus]=
    {
      CommandTypes,
      SpeedCommands,
      DirectionCommands
    };

  static unsigned char
    HighlightBitmap[] = {0xaa,0x55,0xaa,0x55,0xaa,0x55,0xaa,0x55},
    ShadowBitmap[] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

  char
    command[MaxTextExtent],
    image_signature[MaxTextExtent],
    resource_name[MaxTextExtent];

  CommandType
    command_type;

  Image
    *displayed_image,
    **images,
    *loaded_image;

  int
    entry,
    id,
    scene,
    status;

  KeySym
    key_symbol;

  MonitorHandler
    handler;

  register char
    *p;

  register int
    i;

  static char
    working_directory[MaxTextExtent];

  static Window
    root_window;

  static XClassHint
    *class_hints;

  static XFontStruct
    *font_info;

  static XPixelInfo
    icon_pixel,
    pixel_info,
    scene_info;

  static XResourceInfo
    icon_resources;

  static XStandardColormap
    *icon_map,
    *map_info;

  static XVisualInfo
    *icon_visual,
    *visual_info = (XVisualInfo *) NULL;

  static XWindowInfo
    *magick_windows[MaxWindows];

  static XWMHints
    *manager_hints;

  static unsigned int
    number_windows;

  struct stat
    file_info;

  unsigned int
    context_mask,
    number_scenes,
    sans,
    state;

  XEvent
    event;

  XGCValues
    context_values;

  XTextProperty
    window_name;

  XWindowChanges
    window_changes;

  if (visual_info != (XVisualInfo *) NULL)
    (void) chdir(working_directory);
  else
    {
      /*
        Allocate standard colormap.
      */
      if (resource_info->debug)
        {
          XSynchronize(display,True);
          (void) fprintf(stderr,"Version: %s\n",Version);
        }
      map_info=XAllocStandardColormap();
      icon_map=XAllocStandardColormap();
      if ((map_info == (XStandardColormap *) NULL) ||
          (icon_map == (XStandardColormap *) NULL))
        Error("Unable to create standard colormap","Memory allocation failed");
      map_info->colormap=(Colormap) NULL;
      icon_map->colormap=(Colormap) NULL;
      pixel_info.pixels=(unsigned long *) NULL;
      pixel_info.gamma_map=(XColor *) NULL;
      pixel_info.annotate_context=(GC) NULL;
      pixel_info.highlight_context=(GC) NULL;
      pixel_info.widget_context=(GC) NULL;
      font_info=(XFontStruct *) NULL;
      icon_pixel.annotate_context=(GC) NULL;
      icon_pixel.pixels=(unsigned long *) NULL;
      icon_pixel.gamma_map=(XColor *) NULL;
      /*
        Allocate visual.
      */
      icon_resources=(*resource_info);
      icon_resources.map_type=(char *) NULL;
      icon_resources.visual_type="default";
      icon_resources.colormap=SharedColormap;
      visual_info=XBestVisualInfo(display,map_info,resource_info);
      icon_visual=XBestVisualInfo(display,icon_map,&icon_resources);
      if ((visual_info == (XVisualInfo *) NULL) ||
          (icon_visual == (XVisualInfo *) NULL))
        Error("Unable to get visual",resource_info->visual_type);
      if (resource_info->debug)
        {
          (void) fprintf(stderr,"Visual:\n");
          (void) fprintf(stderr,"  visual id: 0x%lx\n",visual_info->visualid);
          (void) fprintf(stderr,"  class: %s\n",
            XVisualClassName(visual_info->class));
          (void) fprintf(stderr,"  depth: %d planes\n",visual_info->depth);
          (void) fprintf(stderr,"  size of colormap: %d entries\n",
            visual_info->colormap_size);
          (void) fprintf(stderr,"  red, green, blue masks: 0x%lx 0x%lx 0x%lx\n",
            visual_info->red_mask,visual_info->green_mask,
            visual_info->blue_mask);
          (void) fprintf(stderr,"  significant bits in color: %d bits\n",
            visual_info->bits_per_rgb);
        }
      /*
        Allocate atoms.
      */
      windows=(XWindows *) malloc(sizeof(XWindows));
      if (windows == (XWindows *) NULL)
        Error("Unable to create X windows","Memory allocation failed");
      windows->wm_protocols=XInternAtom(display,"WM_PROTOCOLS",False);
      windows->wm_delete_window=XInternAtom(display,"WM_DELETE_WINDOW",False);
      windows->wm_take_focus=XInternAtom(display,"WM_TAKE_FOCUS",False);
      windows->im_protocols=XInternAtom(display,"IM_PROTOCOLS",False);
      windows->im_update_widget=XInternAtom(display,"IM_UPDATE_WIDGET",False);
      windows->im_update_colormap=
        XInternAtom(display,"IM_UPDATE_COLORMAP",False);
      windows->im_update_signature=
        XInternAtom(display,"IM_UPDATE_SIGNATURE",False);
      *image_signature='\0';
      windows->im_former_image=XInternAtom(display,"IM_FORMER_IMAGE",False);
      windows->im_next_image=XInternAtom(display,"IM_NEXT_IMAGE",False);
      windows->im_retain_colors=XInternAtom(display,"IM_RETAIN_COLORS",False);
      windows->im_exit=XInternAtom(display,"IM_EXIT",False);
      windows->dnd_protocols=XInternAtom(display,"DndProtocol",False);
      if (resource_info->debug)
        {
          (void) fprintf(stderr,"Protocols:\n");
          (void) fprintf(stderr,"  Window Manager: 0x%lx\n",
            windows->wm_protocols);
          (void) fprintf(stderr,"    delete window: 0x%lx\n",
            windows->wm_delete_window);
          (void) fprintf(stderr,"    take focus: 0x%lx\n",
            windows->wm_take_focus);
          (void) fprintf(stderr,"  ImageMagick: 0x%lx\n",
            windows->im_protocols);
          (void) fprintf(stderr,"    update widget: 0x%lx\n",
            windows->im_update_widget);
          (void) fprintf(stderr,"    update colormap: 0x%lx\n",
            windows->im_update_colormap);
          (void) fprintf(stderr,"    update signature: 0x%lx\n",
            windows->im_update_signature);
          (void) fprintf(stderr,"    former image: 0x%lx\n",
            windows->im_former_image);
          (void) fprintf(stderr,"    next image: 0x%lx\n",
            windows->im_next_image);
          (void) fprintf(stderr,"    retain colors: 0x%lx\n",
            windows->im_retain_colors);
          (void) fprintf(stderr,"    exit: 0x%lx\n",windows->im_exit);
          (void) fprintf(stderr,"  Drag and Drop: 0x%lx\n",
            windows->dnd_protocols);
        }
      /*
        Allocate class and manager hints.
      */
      class_hints=XAllocClassHint();
      manager_hints=XAllocWMHints();
      if ((class_hints == (XClassHint *) NULL) ||
          (manager_hints == (XWMHints *) NULL))
        Error("Unable to allocate X hints",(char *) NULL);
      /*
        Determine group leader if we have one.
      */
      root_window=XRootWindow(display,visual_info->screen);
      windows->group_leader.id=(Window) NULL;
      if (resource_info->window_group != (char *) NULL)
        {
          if (isdigit(*resource_info->window_group))
            windows->group_leader.id=XWindowByID(display,root_window,(Window)
              strtol((char *) resource_info->window_group,(char **) NULL,0));
          if (windows->group_leader.id == (Window) NULL)
            windows->group_leader.id=
              XWindowByName(display,root_window,resource_info->window_group);
        }
      /*
        Initialize window id's.
      */
      number_windows=0;
      magick_windows[number_windows++]=(&windows->icon);
      magick_windows[number_windows++]=(&windows->backdrop);
      magick_windows[number_windows++]=(&windows->image);
      magick_windows[number_windows++]=(&windows->info);
      magick_windows[number_windows++]=(&windows->command);
      magick_windows[number_windows++]=(&windows->widget);
      magick_windows[number_windows++]=(&windows->popup);
      for (i=0; i < number_windows; i++)
        magick_windows[i]->id=(Window) NULL;
    }
  if (resource_info->map_type == (char *) NULL)
    if ((visual_info->class != TrueColor) &&
        (visual_info->class != DirectColor))
      {
        Image
          *next_image;

        unsigned int
          global_colormap;

        /*
          Determine if the sequence of images has the identical colormap.
        */
        global_colormap=True;
        next_image=image;
        for ( ; next_image != (Image *) NULL; next_image=next_image->next)
        {
          next_image->matte=False;
          if ((next_image->class == DirectClass) ||
              (next_image->colors != image->colors) ||
              (next_image->colors > visual_info->colormap_size))
            {
              global_colormap=False;
              continue;
            }
          for (i=0; i < image->colors; i++)
            if (!ColorMatch(next_image->colormap[i],image->colormap[i],0))
              {
                global_colormap=False;
                break;
              }
        }
        if (!global_colormap)
          MapImages(image,(Image *) NULL,resource_info->dither);
      }
  /*
    Sort images by increasing scene number.
  */
  images=ListToGroupImage(image,&number_scenes);
  if (images == (Image **) NULL)
    Error("Unable to animate images","Memory allocation failed");
  i=0;
  for (scene=0; scene < number_scenes; scene++)
    i+=images[scene]->scene;
  if (i > 0)
    qsort((void *) images,number_scenes,sizeof(Image *),
      (int (*)(const void *, const void *)) SceneCompare);
  /*
    Initialize Standard Colormap.
  */
  loaded_image=(Image *) NULL;
  displayed_image=image;
  if (resource_info->debug)
    {
      (void) fprintf(stderr,"Image: %s[%u] %ux%u ",displayed_image->filename,
        displayed_image->scene,displayed_image->columns,displayed_image->rows);
      if (displayed_image->colors != 0)
        (void) fprintf(stderr,"%uc ",displayed_image->colors);
      (void) fprintf(stderr,"%s\n",displayed_image->magick);
    }
  XMakeStandardColormap(display,visual_info,resource_info,displayed_image,
    map_info,&pixel_info);
  /*
    Initialize font info.
  */
  if (font_info != (XFontStruct *) NULL)
    XFreeFont(display,font_info);
  font_info=XBestFont(display,resource_info,False);
  if (font_info == (XFontStruct *) NULL)
    Error("Unable to load font",resource_info->font);
  /*
    Initialize graphic context.
  */
  windows->context.id=(Window) NULL;
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->context);
  class_hints->res_name="superclass";
  class_hints->res_class="Display";
  manager_hints->flags=InputHint | StateHint;
  manager_hints->input=False;
  manager_hints->initial_state=WithdrawnState;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->context);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (context)\n",windows->context.id);
  context_values.background=pixel_info.background_color.pixel;
  context_values.font=font_info->fid;
  context_values.foreground=pixel_info.foreground_color.pixel;
  context_values.graphics_exposures=False;
  context_mask=GCBackground | GCFont | GCForeground | GCGraphicsExposures;
  if (pixel_info.annotate_context != (GC) NULL)
    XFreeGC(display,pixel_info.annotate_context);
  pixel_info.annotate_context=
    XCreateGC(display,windows->context.id,context_mask,&context_values);
  if (pixel_info.annotate_context == (GC) NULL)
    Error("Unable to create graphic context",(char *) NULL);
  context_values.background=pixel_info.depth_color.pixel;
  if (pixel_info.widget_context != (GC) NULL)
    XFreeGC(display,pixel_info.widget_context);
  pixel_info.widget_context=
    XCreateGC(display,windows->context.id,context_mask,&context_values);
  if (pixel_info.widget_context == (GC) NULL)
    Error("Unable to create graphic context",(char *) NULL);
  context_values.background=pixel_info.foreground_color.pixel;
  context_values.foreground=pixel_info.background_color.pixel;
  context_values.plane_mask=
    context_values.background ^ context_values.foreground;
  if (pixel_info.highlight_context != (GC) NULL)
    XFreeGC(display,pixel_info.highlight_context);
  pixel_info.highlight_context=XCreateGC(display,windows->context.id,
    context_mask | GCPlaneMask,&context_values);
  if (pixel_info.highlight_context == (GC) NULL)
    Error("Unable to create graphic context",(char *) NULL);
  XDestroyWindow(display,windows->context.id);
  /*
    Initialize icon window.
  */
  XGetWindowInfo(display,icon_visual,icon_map,&icon_pixel,(XFontStruct *) NULL,
    &icon_resources,&windows->icon);
  windows->icon.geometry=resource_info->icon_geometry;
  XBestIconSize(display,&windows->icon,displayed_image);
  windows->icon.attributes.colormap=
    XDefaultColormap(display,icon_visual->screen);
  windows->icon.attributes.event_mask=ExposureMask | StructureNotifyMask;
  class_hints->res_name="icon";
  manager_hints->flags=InputHint | StateHint;
  manager_hints->input=False;
  manager_hints->initial_state=IconicState;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->icon);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (icon)\n",windows->icon.id);
  /*
    Initialize graphic context for icon window.
  */
  if (icon_pixel.annotate_context != (GC) NULL)
    XFreeGC(display,icon_pixel.annotate_context);
  context_values.background=icon_pixel.background_color.pixel;
  context_values.foreground=icon_pixel.foreground_color.pixel;
  icon_pixel.annotate_context=XCreateGC(display,windows->icon.id,
    GCBackground | GCForeground,&context_values);
  if (icon_pixel.annotate_context == (GC) NULL)
    Error("Unable to create graphic context",(char *) NULL);
  windows->icon.annotate_context=icon_pixel.annotate_context;
  /*
    Initialize Image window.
  */
  if (windows->image.id != (Window) NULL)
    {
      free((char *) windows->image.name);
      free((char *) windows->image.icon_name);
    }
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->image);
  windows->image.shape=True;
  windows->image.name=(char *) malloc(MaxTextExtent*sizeof(char));
  windows->image.icon_name=(char *) malloc(MaxTextExtent*sizeof(char));
  if ((windows->image.name == NULL) || (windows->image.icon_name == NULL))
    Error("Unable to create Image window","Memory allocation failed");
  if (resource_info->title != (char *) NULL)
    {
      /*
        User specified window name.
      */
      (void) strcpy(windows->image.name,resource_info->title);
      (void) strcpy(windows->image.icon_name,resource_info->title);
    }
  else
    {
      /*
        Window name is the base of the filename.
      */
      p=displayed_image->filename+Extent(displayed_image->filename)-1;
      while ((p > displayed_image->filename) && (*(p-1) != *BasenameSeparator))
        p--;
      (void) sprintf(windows->image.name,"ImageMagick: %s[%u of %u]",p,
        displayed_image->scene,number_scenes);
      (void) strcpy(windows->image.icon_name,p);
    }
  if (resource_info->immutable)
    windows->image.immutable=True;
  windows->image.shape=True;
  windows->image.geometry=resource_info->image_geometry;
  windows->image.width=displayed_image->columns;
  if (windows->image.width > XDisplayWidth(display,visual_info->screen))
    windows->image.width=XDisplayWidth(display,visual_info->screen);
  windows->image.height=displayed_image->rows;
  if (windows->image.height > XDisplayHeight(display,visual_info->screen))
    windows->image.height=XDisplayHeight(display,visual_info->screen);
  windows->image.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | OwnerGrabButtonMask |
    StructureNotifyMask;
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->backdrop);
  if ((resource_info->backdrop) || (windows->backdrop.id != (Window) NULL))
    {
      /*
        Initialize backdrop window.
      */
      windows->backdrop.x=0;
      windows->backdrop.y=0;
      windows->backdrop.name="ImageMagick Backdrop";
      windows->backdrop.flags=USSize | USPosition;
      windows->backdrop.width=XDisplayWidth(display,visual_info->screen);
      windows->backdrop.height=XDisplayHeight(display,visual_info->screen);
      windows->backdrop.border_width=0;
      windows->backdrop.immutable=True;
      windows->backdrop.attributes.do_not_propagate_mask=ButtonPressMask |
        ButtonReleaseMask;
      windows->backdrop.attributes.event_mask=ButtonPressMask | KeyPressMask |
        StructureNotifyMask;
      windows->backdrop.attributes.override_redirect=True;
      class_hints->res_name="backdrop";
      manager_hints->flags=IconWindowHint | InputHint | StateHint;
      manager_hints->icon_window=windows->icon.id;
      manager_hints->input=True;
      manager_hints->initial_state=
        resource_info->iconic ? IconicState : NormalState;
      XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
        &windows->backdrop);
      if (resource_info->debug)
        (void) fprintf(stderr,"Window id: 0x%lx (backdrop)\n",
          windows->backdrop.id);
      XMapWindow(display,windows->backdrop.id);
      XClearWindow(display,windows->backdrop.id);
      if (windows->image.id != (Window) NULL)
        {
          XDestroyWindow(display,windows->image.id);
          windows->image.id=(Window) NULL;
        }
      /*
        Position image in the center the backdrop.
      */
      windows->image.flags|=USPosition;
      windows->image.x=(XDisplayWidth(display,visual_info->screen) >> 1)-
        (windows->image.width >> 1);
      windows->image.y=(XDisplayHeight(display,visual_info->screen) >> 1)-
        (windows->image.height >> 1);
    }
  if (resource_info->name == (char *) NULL)
    class_hints->res_name=resource_info->client_name;
  else
    class_hints->res_name=resource_info->name;
  manager_hints->flags=IconWindowHint | InputHint | StateHint;
  manager_hints->icon_window=windows->icon.id;
  manager_hints->input=True;
  manager_hints->initial_state=
    resource_info->iconic ? IconicState : NormalState;
  if (windows->group_leader.id != (Window) NULL)
    {
      /*
        Follow the leader.
      */
      manager_hints->flags|=WindowGroupHint;
      manager_hints->window_group=windows->group_leader.id;
      XSelectInput(display,windows->group_leader.id,StructureNotifyMask);
      if (resource_info->debug)
        (void) fprintf(stderr,"Window id: 0x%lx (group leader)\n",
          windows->group_leader.id);
    }
  XMakeWindow(display,
    (Window) (resource_info->backdrop ? windows->backdrop.id : root_window),
    argv,argc,class_hints,manager_hints,&windows->image);
  if (windows->group_leader.id != (Window) NULL)
    XSetTransientForHint(display,windows->image.id,windows->group_leader.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (image)\n",windows->image.id);
  /*
    Initialize X image structure.
  */
  windows->image.x=0;
  windows->image.y=0;
  status=XMakeImage(display,resource_info,&windows->image,displayed_image,
    displayed_image->columns,displayed_image->rows);
  if (status == False)
    Error("Unable to create X image",(char *) NULL);
  if (!windows->image.mapped || (windows->backdrop.id != (Window) NULL))
    XMapWindow(display,windows->image.id);
  if (windows->image.mapped)
    XRefreshWindow(display,&windows->image,(XEvent *) NULL);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_signature,CurrentTime);
  /*
    Initialize Info widget.
  */
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->info);
  windows->info.name="Info";
  windows->info.icon_name="Info";
  windows->info.border_width=1;
  windows->info.x=2;
  windows->info.y=2;
  windows->info.flags|=PPosition;
  windows->info.attributes.win_gravity=UnmapGravity;
  windows->info.attributes.event_mask=
    ButtonPressMask | ExposureMask | StructureNotifyMask;
  class_hints->res_name="info";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=False;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,windows->image.id,argv,argc,class_hints,manager_hints,
    &windows->info);
  windows->info.highlight_stipple=XCreateBitmapFromData(display,
    windows->info.id,(char *) HighlightBitmap,HighlightWidth,HighlightHeight);
  windows->info.shadow_stipple=XCreateBitmapFromData(display,
    windows->info.id,(char *) ShadowBitmap,ShadowWidth,ShadowHeight);
  XSetTransientForHint(display,windows->info.id,windows->image.id);
  if (windows->image.mapped)
    XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (info)\n",windows->info.id);
  /*
    Initialize Command widget.
  */
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->command);
  windows->command.data=MagickMenus;
  (void) XCommandWidget(display,windows,CommandMenu,(XEvent *) NULL);
  (void) sprintf(resource_name,"%s.command",resource_info->client_name);
  windows->command.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  windows->command.name=MagickTitle;
  windows->command.border_width=0;
  windows->command.flags|=PPosition;
  windows->command.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | LeaveWindowMask |
    OwnerGrabButtonMask | StructureNotifyMask;
  class_hints->res_name="command";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=False;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->command);
  windows->command.highlight_stipple=windows->info.highlight_stipple;
  windows->command.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->command.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (command)\n",windows->command.id);
  /*
    Initialize Widget window.
  */
  if (windows->widget.id != (Window) NULL)
    free((char *) windows->widget.name);
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->widget);
  (void) sprintf(resource_name,"%s.widget",resource_info->client_name);
  windows->widget.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  windows->widget.name=(char *) malloc(MaxTextExtent*sizeof(char));
  if (windows->widget.name == NULL)
    Error("Unable to create Image window","Memory allocation failed");
  *windows->widget.name='\0';
  windows->widget.border_width=0;
  windows->widget.flags|=PPosition;
  windows->widget.attributes.backing_store=WhenMapped;
  windows->widget.attributes.save_under=True;
  windows->widget.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | OwnerGrabButtonMask |
    StructureNotifyMask;
  class_hints->res_name="widget";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->widget);
  windows->widget.highlight_stipple=windows->info.highlight_stipple;
  windows->widget.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->widget.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (widget)\n",windows->widget.id);
  /*
    Initialize popup window.
  */
  if (windows->popup.id != (Window) NULL)
    free((char *) windows->popup.name);
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,font_info,
    resource_info,&windows->popup);
  windows->popup.name=(char *) malloc(MaxTextExtent*sizeof(char));
  if (windows->popup.name == NULL)
    Error("Unable to create Image window","Memory allocation failed");
  *windows->popup.name='\0';
  windows->popup.border_width=0;
  windows->popup.flags|=PPosition;
  windows->popup.attributes.backing_store=WhenMapped;
  windows->popup.attributes.save_under=True;
  windows->popup.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | StructureNotifyMask;
  class_hints->res_name="popup";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->popup);
  windows->popup.highlight_stipple=windows->info.highlight_stipple;
  windows->popup.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->popup.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (pop up)\n",windows->popup.id);
  /*
    Set out progress and warning handlers.
  */
  (void) SetMonitorHandler(XProgressMonitor);
  (void) SetWarningHandler(XWarning);
  if (!resource_info->display_warnings)
    (void) SetWarningHandler((ErrorHandler) NULL);
  /*
    Initialize image pixmaps structure.
  */
  XMapWindow(display,windows->image.id);
  windows->image.pixmaps=(Pixmap *) malloc(number_scenes*sizeof(Pixmap));
  if (windows->image.pixmaps == (Pixmap *) NULL)
    Error("Unable to animate images","Memory allocation failed");
  windows->image.pixmaps[0]=windows->image.pixmap;
  scene_info.pixels=(unsigned long *) NULL;
  scene_info.gamma_map=(XColor *) NULL;
  for (scene=1; scene < number_scenes; scene++)
  {
    /*
      Create X image.
    */
    windows->image.pixmap=(Pixmap) NULL;
    if ((resource_info->map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      if (images[scene]->class == PseudoClass)
        {
          /*
            Get pixel info for this scene.
          */
          XGetPixelInfo(display,visual_info,map_info,resource_info,
            images[scene],&scene_info);
          windows->image.pixel_info=(&scene_info);
        }
    status=XMakeImage(display,resource_info,&windows->image,images[scene],
      images[scene]->columns,images[scene]->rows);
    if (status == False)
      Error("Unable to create X image",(char *) NULL);
    if (resource_info->debug)
      {
        (void) fprintf(stderr,"Image: [%u] %s %ux%u ",images[scene]->scene,
          images[scene]->filename,images[scene]->columns,images[scene]->rows);
        if (images[scene]->colors != 0)
          (void) fprintf(stderr,"%uc ",images[scene]->colors);
        (void) fprintf(stderr,"%s\n",images[scene]->magick);
      }
    /*
      Window name is the base of the filename.
    */
    p=images[scene]->filename+Extent(images[scene]->filename)-1;
    while ((p > images[scene]->filename) && (*(p-1) != '/'))
      p--;
    (void) sprintf(windows->image.name,"ImageMagick: %s[%u of %u]",p,
      scene,number_scenes);
    status=XStringListToTextProperty(&windows->image.name,1,&window_name);
    if (status != 0)
      {
        XSetWMName(display,windows->image.id,&window_name);
        XFree((void *) window_name.value);
      }
    windows->image.pixmaps[scene]=windows->image.pixmap;
    if (images[scene]->matte)
      XClearWindow(display,windows->image.id);
    event.xexpose.x=0;
    event.xexpose.y=0;
    event.xexpose.width=images[scene]->columns;
    event.xexpose.height=images[scene]->rows;
    if (images[scene]->page != (char *) NULL)
      (void) XParseGeometry(images[scene]->page,&event.xexpose.x,
        &event.xexpose.y,&sans,&sans);
    windows->image.x=(-event.xexpose.x);
    windows->image.y=(-event.xexpose.y);
    XRefreshWindow(display,&windows->image,&event);
    if (resource_info->delay != 0)
      XDelay(display,(unsigned long) resource_info->delay);
    else
      XDelay(display,(unsigned long) image->delay*10);
  }
  windows->image.pixel_info=(&pixel_info);
  if (windows->command.mapped)
    XMapRaised(display,windows->command.id);
  /*
    Respond to events.
  */
  loaded_image=(Image *) NULL;
  scene=0;
  image=images[0];
  state=ForwardAnimationState | RepeatAnimationState;
  (void) XMagickCommand(display,resource_info,windows,PlayCommand,&image,
    &state);
  do
  {
    if (XEventsQueued(display,QueuedAfterFlush) == 0)
      if ((state & PlayAnimationState) || (state & StepAnimationState))
        {
          /*
            Copy X pixmap to Image window.
          */
          image=images[scene];
          windows->image.ximage->width=image->columns;
          windows->image.ximage->height=image->rows;
          windows->image.pixmap=windows->image.pixmaps[scene];
          if (image->matte)
            XClearWindow(display,windows->image.id);
          event.xexpose.x=0;
          event.xexpose.y=0;
          event.xexpose.width=image->columns;
          event.xexpose.height=image->rows;
          if (image->page != (char *) NULL)
            (void) XParseGeometry(image->page,&event.xexpose.x,&event.xexpose.y,
              &sans,&sans);
          windows->image.x=(-event.xexpose.x);
          windows->image.y=(-event.xexpose.y);
          XRefreshWindow(display,&windows->image,&event);
          XSync(display,False);
          state&=(~StepAnimationState);
          if (resource_info->delay != 0)
            XDelay(display,(unsigned long) resource_info->delay);
          else
            XDelay(display,(unsigned long) image->delay*10);
          if (state & ForwardAnimationState)
            {
              /*
                Forward animation:  increment scene number.
              */
              scene++;
              if (scene == number_scenes)
                if (state & AutoReverseAnimationState)
                  {
                    state&=(~ForwardAnimationState);
                    scene--;
                  }
                else
                  {
                    if (!(state & RepeatAnimationState))
                      state&=(~PlayAnimationState);
                    scene=0;
                    sleep(resource_info->pause);
                  }
            }
          else
            {
              /*
                Reverse animation:  decrement scene number.
              */
              scene--;
              if (scene < 0)
                if (state & AutoReverseAnimationState)
                  {
                    state|=ForwardAnimationState;
                    scene=0;
                    sleep(resource_info->pause);
                  }
                else
                  {
                    if (!(state & RepeatAnimationState))
                      state&=(~PlayAnimationState);
                    scene=number_scenes-1;
                  }
            }
          continue;
        }
    /*
      Handle a window event.
    */
    XNextEvent(display,&event);
    if (event.xany.window == windows->command.id)
      {
        int
          id;

        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,CommandMenu,&event);
        if (id < 0)
          continue;
        (void) strcpy(command,CommandMenu[id]);
        command_type=CommandMenus[id];
        if (id < MagickMenus)
          {
            int
              entry;

            /*
              Select a command from a pop-up menu.
            */
            entry=XMenuWidget(display,windows,CommandMenu[id],Menus[id],
              command);
            if (entry < 0)
              continue;
            (void) strcpy(command,Menus[id][entry]);
            command_type=Commands[id][entry];
          }
        if (command_type != NullCommand)
          loaded_image=XMagickCommand(display,resource_info,windows,
            command_type,&image,&state);
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Press: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        if ((event.xbutton.button == Button3) &&
            (event.xbutton.state & Mod1Mask))
          {
            /*
              Convert Alt-Button3 to Button2.
            */
            event.xbutton.button=Button2;
            event.xbutton.state&=(~Mod1Mask);
          }
        if (event.xbutton.window == windows->backdrop.id)
          {
            XSetInputFocus(display,event.xbutton.window,RevertToParent,
              event.xbutton.time);
            break;
          }
        if (event.xbutton.window == windows->image.id)
          {
            /*
              Map/unmap Command widget.
            */
            if (windows->command.mapped)
              XWithdrawWindow(display,windows->command.id,
                windows->command.screen);
            else
              {
                (void) XCommandWidget(display,windows,CommandMenu,
                  (XEvent *) NULL);
                XMapRaised(display,windows->command.id);
              }
          }
        break;
      }
      case ButtonRelease:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Release: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        break;
      }
      case ClientMessage:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Client Message: 0x%lx 0x%lx %d 0x%lx\n",
            event.xclient.window,event.xclient.message_type,
            event.xclient.format,(unsigned long) event.xclient.data.l[0]);
        if (event.xclient.message_type == windows->im_protocols)
          {
            if (*event.xclient.data.l == windows->im_update_colormap)
              {
                /*
                  Update graphic context and window colormap.
                */
                for (i=0; i < number_windows; i++)
                {
                  if (magick_windows[i]->id == windows->icon.id)
                    continue;
                  context_values.background=pixel_info.background_color.pixel;
                  context_values.foreground=pixel_info.foreground_color.pixel;
                  XChangeGC(display,magick_windows[i]->annotate_context,
                    context_mask,&context_values);
                  XChangeGC(display,magick_windows[i]->widget_context,
                    context_mask,&context_values);
                  context_values.background=pixel_info.foreground_color.pixel;
                  context_values.foreground=pixel_info.background_color.pixel;
                  context_values.plane_mask=
                    context_values.background ^ context_values.foreground;
                  XChangeGC(display,magick_windows[i]->highlight_context,
                    context_mask | GCPlaneMask,&context_values);
                  magick_windows[i]->attributes.background_pixel=
                    pixel_info.background_color.pixel;
                  magick_windows[i]->attributes.border_pixel=
                    pixel_info.border_color.pixel;
                  magick_windows[i]->attributes.colormap=map_info->colormap;
                  XChangeWindowAttributes(display,magick_windows[i]->id,
                    magick_windows[i]->mask,&magick_windows[i]->attributes);
                }
                if (windows->backdrop.id != (Window) NULL)
                  XInstallColormap(display,map_info->colormap);
                break;
              }
            break;
          }
        if (event.xclient.message_type == windows->dnd_protocols)
          {
            Atom
              selection,
              type;

            int
              format,
              status;

            unsigned char
              *data;

            unsigned long
              after,
              length;

            /*
              Display image named by the Drag-and-Drop selection.
            */
            if ((int) (*event.xclient.data.l) != 2)
              break;
            selection=XInternAtom(display,"DndSelection",False);
            status=XGetWindowProperty(display,root_window,selection,0L,2047L,
              False,(Atom) AnyPropertyType,&type,&format,&length,&after,&data);
            if ((status != Success) || (length == 0))
              break;
            (void) strcpy(resource_info->image_info->filename,(char *) data);
            loaded_image=ReadImage(resource_info->image_info);
            if (loaded_image != (Image *) NULL)
              state|=ExitState;
            XFree((void *) data);
            break;
          }
        /*
          If client window delete message, exit.
        */
        if (event.xclient.message_type != windows->wm_protocols)
          break;
        if (*event.xclient.data.l == windows->wm_take_focus)
          {
            XSetInputFocus(display,event.xclient.window,RevertToParent,
              event.xclient.data.l[1]);
            break;
          }
        if (*event.xclient.data.l != windows->wm_delete_window)
          break;
        XWithdrawWindow(display,event.xclient.window,visual_info->screen);
        if (event.xclient.window == windows->image.id)
          {
            state|=ExitState;
            break;
          }
        break;
      }
      case ConfigureNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Configure Notify: 0x%lx %dx%d+%d+%d %d\n",
            event.xconfigure.window,event.xconfigure.width,
            event.xconfigure.height,event.xconfigure.x,event.xconfigure.y,
            event.xconfigure.send_event);
        if (event.xconfigure.window == windows->image.id)
          {
            if (event.xconfigure.send_event != 0)
              if (windows->command.geometry == (char *) NULL)
                if (!windows->command.mapped)
                  {
                    XWindowChanges
                      window_changes;

                     windows->command.x=
                        event.xconfigure.x-windows->command.width-25;
                      windows->command.y=event.xconfigure.y;
                      XConstrainWindowPosition(display,&windows->command);
                      window_changes.x=windows->command.x;
                      window_changes.y=windows->command.y;
                      XReconfigureWMWindow(display,windows->command.id,
                        windows->command.screen,CWX | CWY,&window_changes);
                  }
            /*
              Image window has a new configuration.
            */
            windows->image.width=event.xconfigure.width;
            windows->image.height=event.xconfigure.height;
            break;
          }
        if (event.xconfigure.window == windows->icon.id)
          {
            /*
              Icon window has a new configuration.
            */
            windows->icon.width=event.xconfigure.width;
            windows->icon.height=event.xconfigure.height;
            break;
          }
        break;
      }
      case DestroyNotify:
      {
        /*
          Group leader has exited.
        */
        if (event.xdestroywindow.window == windows->group_leader.id)
          {
            state|=ExitState;
            break;
          }
        break;
      }
      case EnterNotify:
      {
        /*
          Selectively install colormap.
        */
        if (map_info->colormap != XDefaultColormap(display,visual_info->screen))
          if (event.xcrossing.mode != NotifyUngrab)
            XInductColormap(display,map_info->colormap);
        break;
      }
      case Expose:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Expose: 0x%lx %dx%d+%d+%d\n",
            event.xexpose.window,event.xexpose.width,event.xexpose.height,
            event.xexpose.x,event.xexpose.y);
        /*
          Repaint windows that are now exposed.
        */
        if (event.xexpose.window == windows->image.id)
          {
            windows->image.pixmap=windows->image.pixmaps[scene];
            XRefreshWindow(display,&windows->image,&event);
            break;
          }
        if (event.xexpose.window == windows->icon.id)
          if (event.xexpose.count == 0)
            {
              XRefreshWindow(display,&windows->icon,&event);
              break;
            }
        break;
      }
      case KeyPress:
      {
        static char
          command[MaxTextExtent];

        static int
          length;

        static KeySym
          key_symbol;

        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if (resource_info->debug)
          (void) fprintf(stderr,"Key press: 0x%lx (%c)\n",key_symbol,*command);
        command_type=NullCommand;
        switch (key_symbol)
        {
          case XK_o:
          {
            if (!(state & ControlMask))
              break;
            command_type=OpenCommand;
          }
          case XK_space:
          {
            command_type=StepCommand;
            break;
          }
          case XK_less:
          {
            command_type=FasterCommand;
            break;
          }
          case XK_greater:
          {
            command_type=SlowerCommand;
            break;
          }
          case XK_question:
          {
            command_type=InfoCommand;
            break;
          }
          case XK_q:
          case XK_Cancel:
          {
            if (!(state & ControlMask))
              break;
            command_type=QuitCommand;
            break;
          }
          default:
            break;
        }
        if (command_type != NullCommand)
          loaded_image=XMagickCommand(display,resource_info,windows,
            command_type,&image,&state);
        break;
      }
      case KeyRelease:
      {
        /*
          Respond to a user key release.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        if (resource_info->debug)
          (void) fprintf(stderr,"Key release: 0x%lx (%c)\n",key_symbol,
            *command);
        break;
      }
      case LeaveNotify:
      {
        /*
          Selectively uninstall colormap.
        */
        if (map_info->colormap != XDefaultColormap(display,visual_info->screen))
          if (event.xcrossing.mode != NotifyUngrab)
            XUninductColormap(display,map_info->colormap);
        break;
      }
      case MapNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Map Notify: 0x%lx\n",event.xmap.window);
        if (event.xmap.window == windows->backdrop.id)
          {
            XSetInputFocus(display,event.xmap.window,RevertToParent,
              CurrentTime);
            windows->backdrop.mapped=True;
            break;
          }
        if (event.xmap.window == windows->image.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XInstallColormap(display,map_info->colormap);
            if (strcmp(images[0]->magick,"LOGO") == 0)
              loaded_image=XMagickCommand(display,resource_info,windows,
                OpenCommand,&image,&state);
            windows->image.mapped=True;
            break;
          }
        if (event.xmap.window == windows->info.id)
          {
            windows->info.mapped=True;
            break;
          }
        if (event.xmap.window == windows->icon.id)
          {
            /*
              Create an icon image.
            */
            XMakeStandardColormap(display,icon_visual,&icon_resources,
              displayed_image,icon_map,&icon_pixel);
            (void) XMakeImage(display,&icon_resources,&windows->icon,
              displayed_image,windows->icon.width,windows->icon.height);
            XSetWindowBackgroundPixmap(display,windows->icon.id,
              windows->icon.pixmap);
            XClearWindow(display,windows->icon.id);
            XWithdrawWindow(display,windows->info.id,windows->info.screen);
            windows->icon.mapped=True;
            break;
          }
        if (event.xmap.window == windows->command.id)
          {
            windows->command.mapped=True;
            break;
          }
        if (event.xmap.window == windows->popup.id)
          {
            windows->popup.mapped=True;
            break;
          }
        if (event.xmap.window == windows->widget.id)
          {
            windows->widget.mapped=True;
            break;
          }
        break;
      }
      case MappingNotify:
      {
        XRefreshKeyboardMapping(&event.xmapping);
        break;
      }
      case NoExpose:
        break;
      case ReparentNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Reparent Notify: 0x%lx=>0x%lx\n",
            event.xreparent.parent,event.xreparent.window);
        break;
      }
      case UnmapNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Unmap Notify: 0x%lx\n",event.xunmap.window);
        if (event.xunmap.window == windows->backdrop.id)
          {
            windows->backdrop.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->image.id)
          {
            windows->image.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->info.id)
          {
            windows->info.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->icon.id)
          {
            if (map_info->colormap == icon_map->colormap)
              XConfigureImageColormap(display,resource_info,windows,
                displayed_image);
            XFreeStandardColormap(display,icon_visual,icon_map,&icon_pixel);
            windows->icon.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->command.id)
          {
            windows->command.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->popup.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XSetInputFocus(display,windows->image.id,RevertToParent,
                CurrentTime);
            windows->popup.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->widget.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XSetInputFocus(display,windows->image.id,RevertToParent,
                CurrentTime);
            windows->widget.mapped=False;
            break;
          }
        break;
      }
      default:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Event type: %d\n",event.type);
        break;
      }
    }
  }
  while (!(state & ExitState));
  /*
    Withdraw windows.
  */
  if (windows->info.mapped)
    XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (windows->command.mapped)
    XWithdrawWindow(display,windows->command.id,windows->command.screen);
  if (!resource_info->backdrop)
    if (windows->backdrop.mapped)
      {
        XWithdrawWindow(display,windows->backdrop.id,windows->backdrop.screen);
        XDestroyWindow(display,windows->backdrop.id);
        windows->backdrop.id=(Window) NULL;
        XWithdrawWindow(display,windows->image.id,windows->image.screen);
        XDestroyWindow(display,windows->image.id);
        windows->image.id=(Window) NULL;
      }
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  for (scene=1; scene < number_scenes; scene++)
    if (windows->image.pixmaps[scene] != (Pixmap) NULL)
      XFreePixmap(display,windows->image.pixmaps[scene]);
  /*
    Change to home directory.
  */
  (void) getcwd(working_directory,MaxTextExtent-1);
  (void) chdir(resource_info->home_directory);
  return(loaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X C o n f i g u r e I m a g e C o l o r m a p                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XConfigureImageColormap creates a new X colormap.
%
%  The format of the XConfigureImageColormap routine is:
%
%    XConfigureImageColormap(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static void XConfigureImageColormap(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image *image)
{
  Colormap
    colormap;

  /*
    Make standard colormap.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  XMakeStandardColormap(display,windows->image.visual_info,resource_info,
    image,windows->image.map_info,windows->image.pixel_info);
  colormap=windows->image.map_info->colormap;
  XSetWindowColormap(display,windows->image.id,colormap);
  XSetWindowColormap(display,windows->command.id,colormap);
  XSetWindowColormap(display,windows->widget.id,colormap);
  XSetCursorState(display,windows,False);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_colormap,CurrentTime);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X M a g i c k C o m m a n d                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XMagickCommand makes a transform to the image or Image window as
%  specified by a user menu button or keyboard command.
%
%  The format of the XMagickCommand routine is:
%
%    XMagickCommand(display,resource_info,windows,image,command_type,state);
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  XMagickCommand
%      may transform the image and return a new image pointer.
%
%    o state: Specifies an unsigned int;  XMagickCommand may return a
%      modified state.
%
%
*/
static Image *XMagickCommand(Display *display,XResourceInfo *resource_info,
  XWindows *windows,const CommandType command_type,Image **image,
  unsigned int *state)
{
#define LoadImageText  "  Loading images...  "

  Image
    *loaded_image;

  int
    status;

  XTextProperty
    window_name;

  /*
    Process user command.
  */
  loaded_image=(Image *) NULL;
  switch (command_type)
  {
    case OpenCommand:
    {
      char
        **filelist;
    
      Image
        *image,
        *next_image;
    
      ImageInfo
        local_info;
    
      int
        number_files;
    
      MonitorHandler
        handler;

      register int
        i;
    
      static char
        filename[MaxTextExtent] = "\0",
        filenames[MaxTextExtent] = "*";
    
      /*
        Request file name from user.
      */
      XFileBrowserWidget(display,windows,"Animate",filenames);
      if (*filenames == '\0')
        return((Image *) NULL);
      /*
        Expand the filenames.
      */
      filelist=(char **) malloc(sizeof(char *));
      if (filelist == (char **) NULL)
        {
          Warning("Memory allocation error",(char *) NULL);
          return((Image *) NULL);
        }
      number_files=1;
      filelist[0]=filenames;
      ExpandFilenames(&number_files,&filelist);
      if (number_files == 0)
        {
          Warning("No image files were found",filenames);
          return((Image *) NULL);
        }
      local_info=(*resource_info->image_info);
      image=(Image *) NULL;
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      for (i=0; i < number_files; i++)
      {
        if (number_files > 5)
          handler=SetMonitorHandler((MonitorHandler) NULL);
        local_info.filename=filelist[i];
        *local_info.magick='\0';
        next_image=ReadImage(&local_info);
        if (filelist[i] != filenames)
          free((char *) filelist[i]);
        if (next_image != (Image *) NULL)
          if (image == (Image *) NULL)
            image=next_image;
          else
            {
              image->next=next_image;
              image->next->previous=image;
              image=image->next;
            }
        if (number_files <= 5)
          continue;
        (void) SetMonitorHandler(handler);
        ProgressMonitor(LoadImageText,i,number_files);
      }
      if (image == (Image *) NULL)
        {
          XSetCursorState(display,windows,False);
          Warning("No images were loaded",filenames);
          return((Image *) NULL);
        }
      while (image->previous != (Image *) NULL)
        image=image->previous;
      loaded_image=image;
      *state|=ExitState;
      break;
    }
    case PlayCommand:
    {
      *state|=PlayAnimationState;
      *state&=(~AutoReverseAnimationState);
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      /*
        Window name is the base of the filename.
      */
      (void) sprintf(windows->image.name,"ImageMagick: %s",
        ClientName((*image)->filename));
      if (resource_info->title != (char *) NULL)
        (void) strcpy(windows->image.name,resource_info->title);
      status=XStringListToTextProperty(&windows->image.name,1,&window_name);
      if (status == 0)
        break;
      XSetWMName(display,windows->image.id,&window_name);
      XFree((void *) window_name.value);
      break;
    }
    case StepCommand:
    {
      register char
        *p;

      *state|=StepAnimationState;
      *state&=(~PlayAnimationState);
      if (resource_info->title != (char *) NULL)
        break;
      /*
        Window name is the base of the filename.
      */
      p=(*image)->filename+Extent((*image)->filename)-1;
      while ((p > (*image)->filename) && (*(p-1) != '/'))
        p--;
      (void) sprintf(windows->image.name,"%s [%u]",p,(*image)->scene);
      status=XStringListToTextProperty(&windows->image.name,1,&window_name);
      if (status == 0)
        break;
      XSetWMName(display,windows->image.id,&window_name);
      XInfoWidget(display,windows,windows->image.name);
      XFree((void *) window_name.value);
      break;
    }
    case RepeatCommand:
    {
      *state|=RepeatAnimationState;
      *state&=(~AutoReverseAnimationState);
      *state|=PlayAnimationState;
      break;
    }
    case AutoReverseCommand:
    {
      *state|=AutoReverseAnimationState;
      *state&=(~RepeatAnimationState);
      *state|=PlayAnimationState;
      break;
    }
    case SlowerCommand:
    {
      resource_info->delay<<=1;
      if (resource_info->delay == 0)
        resource_info->delay=1;
      break;
    }
    case FasterCommand:
    {
      resource_info->delay>>=1;
      break;
    }
    case ForwardCommand:
    {
      *state=ForwardAnimationState;
      *state&=(~AutoReverseAnimationState);
      break;
    }
    case ReverseCommand:
    {
      *state&=(~ForwardAnimationState);
      *state&=(~AutoReverseAnimationState);
      break;
    }
    case InfoCommand:
    {
      XDisplayImageInfo(display,resource_info,windows,(Image *) NULL,*image);
      break;
    }
    case HelpCommand:
    {
      XTextViewWidget(display,resource_info,windows,False,
        "Help Viewer - Animate",ImageMagickHelp);
      break;
    }
    case QuitCommand:
    {
      /*
        Exit program
      */
      if (!resource_info->confirm_exit)
        *state|=ExitState;
      else
        {
          int
            status;

          /*
            Confirm program exit.
          */
          status=XConfirmWidget(display,windows,"Do you really want to exit",
            resource_info->client_name);
          if (status > 0)
            *state|=ExitState;
        }
      break;
    }
    default:
      break;
  }
  return(loaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X P r o g r e s s M o n i t o r                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XProgressMonitor displays the progress a task is making in
%  completing a task.
%
%  The format of the XProgressMonitor routine is:
%
%      XProgressMonitor(task,quantum,span)
%
%  A description of each parameter follows:
%
%    o task: Identifies the task in progress.
%
%    o quantum: Specifies the quantum position within the span which represents
%      how much progress has been made in completing a task.
%
%    o span: Specifies the span relative to completing a task.
%
%
*/
static void XProgressMonitor(char *task,const unsigned int quantum,
  const unsigned int span)
{
  XMonitorWidget(display,windows,task,quantum,span);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X W a r n i n g                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function XWarning displays a warning message in a Notice widget.
%
%  The format of the XWarning routine is:
%
%      XWarning(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
static void XWarning(const char *message,const char *qualifier)
{
  char
    text[MaxTextExtent];

  if (message == (char *) NULL)
    return;
  (void) strcpy(text,message);
  (void) strcat(text,":");
  XNoticeWidget(display,windows,text,qualifier);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%    M a i n                                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char **argv)
{
  char
    *client_name,
    *option,
    *resource_value,
    *server_name;

  Image
    *image,
    *loaded_image;

  ImageInfo
    image_info;

  int
    i,
    x,
    y;

  unsigned int
    first_scene,
    last_scene,
    scene;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  /*
    Set defaults.
  */
  ReadCommandlLine(argc,&argv);
  client_name=ClientName(*argv);
  display=(Display *) NULL;
  first_scene=0;
  GetImageInfo(&image_info);
  image=(Image *) NULL;
  last_scene=0;
  server_name=(char *) NULL;
  /*
    Check for server name specified on the command line.
  */
  ExpandFilenames(&argc,&argv);
  for (i=1; i < argc; i++)
  {
    /*
      Check command line for server name.
    */
    option=argv[i];
    if ((Extent(option) == 1) || ((*option != '-') && (*option != '+')))
      continue;
    if (strcmp("display",option+1) == 0)
      {
        /*
          User specified server name.
        */
        i++;
        if (i == argc)
          Error("Missing server name on -display",(char *) NULL);
        server_name=argv[i];
        break;
      }
    if (strncmp("help",option+1,2) == 0)
      Usage(client_name);
  }
  /*
    Open X server connection.
  */
  display=XOpenDisplay(server_name);
  if (display == (Display *) NULL)
    Error("Unable to connect to X server",XDisplayName(server_name));
  /*
    Set our forgiving error handler.
  */
  XSetErrorHandler(XError);
  /*
    Get user defaults from X resource database.
  */
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource_info);
  resource_info.image_info=(&image_info);
  resource_value=XGetResourceClass(resource_database,client_name,"delay","60");
  resource_info.delay=0;
  resource_info.pause=0;
  (void) XParseGeometry(resource_value,&x,&y,&resource_info.delay,
    &resource_info.pause);
  image_info.density=
    XGetResourceClass(resource_database,client_name,"density","85");
  resource_value=
    XGetResourceClass(resource_database,client_name,"interlace","plane");
  image_info.interlace=UndefinedInterlace;
  if (Latin1Compare("none",resource_value) == 0)
    image_info.interlace=NoneInterlace;
  if (Latin1Compare("line",resource_value) == 0)
    image_info.interlace=LineInterlace;
  if (Latin1Compare("plane",resource_value) == 0)
    image_info.interlace=PlaneInterlace;
  if (Latin1Compare("partition",resource_value) == 0)
    image_info.interlace=PartitionInterlace;
  if (image_info.interlace == UndefinedInterlace)
    Warning("Unrecognized interlace type",resource_value);
  resource_value=
    XGetResourceClass(resource_database,client_name,"verbose","False");
  image_info.verbose=IsTrue(resource_value);
  /*
    Parse command line.
  */
  for (i=1; i <= argc; i++)
  {
    if (i < argc)
      option=argv[i];
    else
      if (image != (Image *) NULL)
        break;
      else
        if (isatty(STDIN_FILENO))
          option="logo:";
        else
          option="-";
    if ((Extent(option) > 1) && ((*option == '-') || (*option == '+')))
      switch (*(option+1))
      {
        case 'b':
        {
          if (strncmp("backdrop",option+1,5) == 0)
            {
              resource_info.backdrop=(*option == '-');
              break;
            }
          if (strncmp("background",option+1,5) == 0)
            {
              resource_info.background_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -background",(char *) NULL);
                  resource_info.background_color=argv[i];
                }
              break;
            }
          if (strncmp("bordercolor",option+1,7) == 0)
            {
              resource_info.border_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -bordercolor",(char *) NULL);
                  resource_info.border_color=argv[i];
                }
              break;
            }
          if (strncmp("borderwidth",option+1,7) == 0)
            {
              resource_info.border_width=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing width on -borderwidth",(char *) NULL);
                  resource_info.border_width=atoi(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("colormap",option+1,6) == 0)
            {
              resource_info.colormap=PrivateColormap;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -colormap",(char *) NULL);
                  option=argv[i];
                  resource_info.colormap=UndefinedColormap;
                  if (Latin1Compare("private",option) == 0)
                    resource_info.colormap=PrivateColormap;
                  if (Latin1Compare("shared",option) == 0)
                    resource_info.colormap=SharedColormap;
                  if (resource_info.colormap == UndefinedColormap)
                    Error("Invalid colormap type on -colormap",option);
                }
              break;
            }
          if (strncmp("colors",option+1,7) == 0)
            {
              resource_info.number_colors=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing colors on -colors",(char *) NULL);
                  resource_info.number_colors=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              resource_info.colorspace=RGBColorspace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -colorspace",(char *) NULL);
                  option=argv[i];
                  resource_info.colorspace=UndefinedColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    {
                      resource_info.colorspace=GRAYColorspace;
                      resource_info.number_colors=256;
                      resource_info.tree_depth=8;
                    }
                  if (Latin1Compare("ohta",option) == 0)
                    resource_info.colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    resource_info.colorspace=RGBColorspace;
                  if (Latin1Compare("transparent",option) == 0)
                    resource_info.colorspace=TransparentColorspace;
                  if (Latin1Compare("xyz",option) == 0)
                    resource_info.colorspace=XYZColorspace;
                  if (Latin1Compare("ycbcr",option) == 0)
                    resource_info.colorspace=YCbCrColorspace;
                  if (Latin1Compare("yiq",option) == 0)
                    resource_info.colorspace=YIQColorspace;
                  if (Latin1Compare("ypbpr",option) == 0)
                    resource_info.colorspace=YPbPrColorspace;
                  if (Latin1Compare("yuv",option) == 0)
                    resource_info.colorspace=YUVColorspace;
                  if (resource_info.colorspace == UndefinedColorspace)
                    Error("Invalid colorspace type on -colorspace",option);
                }
              break;
            }
          if (strncmp("crop",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -crop",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'd':
        {
          if (strncmp("debug",option+1,3) == 0)
            {
              resource_info.debug=(*option == '-');
              break;
            }
          if (strncmp("delay",option+1,3) == 0)
            {
              resource_info.delay=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing seconds on -delay",(char *) NULL);
                  (void) XParseGeometry(argv[i],&x,&y,&resource_info.delay,
                    &resource_info.pause);
                }
              break;
            }
          if (strncmp("density",option+1,3) == 0)
            {
              image_info.density=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -density",(char *) NULL);
                  image_info.density=argv[i];
                }
              break;
            }
          if (strcmp("display",option+1) == 0)
            {
              server_name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing server name on -display",(char *) NULL);
                  server_name=argv[i];
                }
              break;
            }
          if (strncmp("dither",option+1,3) == 0)
            {
              resource_info.dither=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'f':
        {
          if (strncmp("font",option+1,3) == 0)
            {
              resource_info.font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing font name on -font",(char *) NULL);
                  resource_info.font=argv[i];
                }
              break;
            }
          if (strncmp("foreground",option+1,3) == 0)
            {
              resource_info.foreground_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing foreground on -foreground",(char *) NULL);
                  resource_info.foreground_color=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'g':
        {
          if (strncmp("gamma",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing value on -gamma",(char *) NULL);
                }
              break;
            }
          if (strncmp("geometry",option+1,2) == 0)
            {
              resource_info.image_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -geometry",(char *) NULL);
                  resource_info.image_geometry=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'h':
        {
          if (strncmp("help",option+1,2) == 0)
            {
              Usage(client_name);
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'i':
        {
          if (strncmp("iconGeometry",option+1,5) == 0)
            {
              resource_info.icon_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -iconGeometry",(char *) NULL);
                  resource_info.icon_geometry=argv[i];
                }
              break;
            }
          if (strncmp("iconic",option+1,5) == 0)
            {
              resource_info.iconic=(*option == '-');
              break;
            }
          if (strncmp("interlace",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -interlace",(char *) NULL);
                  option=argv[i];
                  image_info.interlace=UndefinedInterlace;
                  if (Latin1Compare("none",option) == 0)
                    image_info.interlace=NoneInterlace;
                  if (Latin1Compare("line",option) == 0)
                    image_info.interlace=LineInterlace;
                  if (Latin1Compare("plane",option) == 0)
                    image_info.interlace=PlaneInterlace;
                  if (Latin1Compare("partition",option) == 0)
                    image_info.interlace=PartitionInterlace;
                  if (image_info.interlace == UndefinedInterlace)
                    Error("Invalid interlace type on -interlace",option);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'm':
        {
          if (strncmp("map",option+1,3) == 0)
            {
              argv[i]="+sans";
              resource_info.map_type=(char *) NULL;
              if (*option == '-')
                {
                  argv[i]="-sans";
                  i++;
                  if (i == argc)
                    Error("Missing map type on -map",(char *) NULL);
                  resource_info.map_type=argv[i];
                }
              break;
            }
          if (strcmp("matte",option+1) == 0)
            break;
          if (strncmp("mattecolor",option+1,6) == 0)
            {
              resource_info.matte_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -mattecolor",(char *) NULL);
                  resource_info.matte_color=argv[i];
                }
              break;
            }
          if (strncmp("monochrome",option+1,2) == 0)
            {
              resource_info.monochrome=(*option == '-');
              if (resource_info.monochrome)
                {
                  resource_info.number_colors=2;
                  resource_info.tree_depth=8;
                  resource_info.colorspace=GRAYColorspace;
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'n':
        {
          resource_info.name=(char *) NULL;
          if (*option == '-')
            {
              i++;
              if (i == argc)
                Error("Missing name on -name",(char *) NULL);
              resource_info.name=argv[i];
            }
          break;
        }
        case 's':
        {
          if (strncmp("scene",option+1,3) == 0)
            {
              first_scene=0;
              last_scene=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing scene number on -scene",(char *) NULL);
                  first_scene=atoi(argv[i]);
                  last_scene=first_scene;
                  (void) sscanf(argv[i],"%u-%u",&first_scene,&last_scene);
                }
              break;
            }
          if (strncmp("shared_memory",option+1,4) == 0)
            {
              resource_info.use_shared_memory=(*option == '-');
              break;
            }
          if (strncmp("size",option+1,2) == 0)
            {
              image_info.size=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -size",(char *) NULL);
                  image_info.size=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("text_font",option+1,3) == 0)
            {
              resource_info.text_font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing font name on -text_font",(char *) NULL);
                  resource_info.text_font=argv[i];
                }
              break;
            }
          if (strncmp("title",option+1,2) == 0)
            {
              resource_info.title=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing title on -title",(char *) NULL);
                  resource_info.title=argv[i];
                }
              break;
            }
          if (strncmp("treedepth",option+1,3) == 0)
            {
              resource_info.tree_depth=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing depth on -treedepth",(char *) NULL);
                  resource_info.tree_depth=atoi(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'v':
        {
          if (strncmp("verbose",option+1,2) == 0)
            {
              image_info.verbose=(*option == '-');
              break;
            }
          if (strncmp("visual",option+1,2) == 0)
            {
              resource_info.visual_type=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing visual class on -visual",(char *) NULL);
                  resource_info.visual_type=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'w':
        {
          if (strncmp("window",option+1,2) == 0)
            {
              resource_info.window_id=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing id, name, or 'root' on -window",
                      (char *) NULL);
                  resource_info.window_id=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case '?':
        {
          Usage(client_name);
          break;
        }
        default:
        {
          Error("Unrecognized option",option);
          break;
        }
      }
    else
      for (scene=first_scene; scene <= last_scene ; scene++)
      {
        Image
          *next_image;

        /*
          Option is a file name: begin by reading image from specified file.
        */
        (void) strcpy(image_info.filename,option);
        if (first_scene != last_scene)
          {
            char
              filename[MaxTextExtent];

            /*
              Form filename for multi-part images.
            */
            (void) sprintf(filename,image_info.filename,scene);
            if (strcmp(filename,image_info.filename) == 0)
              (void) sprintf(filename,"%s[%u]",image_info.filename,scene);
            (void) strcpy(image_info.filename,filename);
          }
        image_info.server_name=resource_info.server_name;
        image_info.font=resource_info.font;
        image_info.dither=resource_info.dither;
        image_info.monochrome=resource_info.monochrome;
        next_image=ReadImage(&image_info);
        if (next_image == (Image *) NULL)
          if (*option == '-')
            break;
          else
            continue;
        MogrifyImages(&image_info,i,argv,&next_image);
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            /*
              Link image into image list.
            */
            next_image->previous=image;
            image->next=next_image;
            while (image->next != (Image *) NULL)
              image=image->next;
          }
      }
  }
  if (image == (Image *) NULL)
    Error("Missing an image file name",(char *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  if (resource_info.window_id != (char *) NULL)
    XAnimateBackgroundImage(display,&resource_info,image);
  else
    {
      /*
        Animate image to X server.
      */
      loaded_image=XAnimateImages(display,&resource_info,argv,argc,image);
      DestroyImages(image);
      while (loaded_image != (Image *) NULL)
      {
        image=loaded_image;
        MogrifyImage(&image_info,argc-1,argv,&image);
        loaded_image=XAnimateImages(display,&resource_info,argv,argc,image);
        DestroyImages(image);
      }
    }
  exit(0);
  return(False);
}
