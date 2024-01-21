/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  init.c                                                  *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "init.h"
#include "images.h"
#include "sound.h"
#include "screens.h"
#include "tools.h"
#include "game.h"
#include <signal.h>

int sound_process_id=0;

void OpenAll(int argc, char *argv[])
{
  InitCounter();
  InitSound();
  InitSoundProcess();

  INIT_RND();

  signal(SIGINT, CloseAll);
  signal(SIGTERM, CloseAll);

  InitDisplay(argc, argv);
  InitColor();
  InitWindow(argc, argv);
  InitGfx();

  LoadPlayerInfo();
  DrawMainMenu();

  XMapWindow(display, window);
  XFlush(display);
}

void InitSound()
{
  int i;

  if (sound_status==SOUND_OFF)
    return;

  if (access(sound_device_name,W_OK)<0)
  {
    fprintf(stderr,"%s: cannot access sound device - no sounds\n",progname);
    sound_status=SOUND_OFF;
    return;
  }

  if ((sound_device=open(sound_device_name,O_WRONLY))<0)
  {
    fprintf(stderr,"%s: cannot open sound device - no sounds\n",progname);
    sound_status=SOUND_OFF;
    return;
  }

  close(sound_device);
  sound_status=SOUND_AVAILABLE;

#ifdef VOXWARE
  sound_loops_allowed = TRUE;
  sound_loops_on = TRUE;
#endif

  for(i=0;i<NUM_SOUNDS;i++)
  {
    if (!LoadSound(i))
    {
      sound_status=SOUND_OFF;
      return;
    }
  }
}

void InitSoundProcess()
{
  if (sound_status==SOUND_OFF)
    return;

  if (pipe(sound_pipe)<0)
  {
    fprintf(stderr,"%s: cannot create pipe - no sounds\n",progname);
    sound_status=SOUND_OFF;
    return;
  }

  if ((sound_process_id=fork())<0)
  {       
    fprintf(stderr,"%s: cannot create child process - no sounds\n",progname);
    sound_status=SOUND_OFF;
    return;
  }

  if (!sound_process_id)	/* we are child */
    SoundServer();
  else				/* we are parent */
    close(sound_pipe[0]);	/* no reading from pipe needed */
}

void InitDisplay(int argc, char *argv[])
{
  char *display_name = NULL;
  int i;

  /* get X server to connect to, if given as an argument */
  for (i=1;i<argc-1;i++)
  {
    char *dispstr="-display";
    int len=MAX(strlen(dispstr),strlen(argv[i]));

    if (len<4)
      continue;
    else if (!strncmp(argv[i],dispstr,len))
    {
      display_name=argv[i+1];
      break;
    }
  }

  /* connect to X server */
  if (!(display=XOpenDisplay(display_name)))
  {
    fprintf(stderr,"%s: cannot connect to X server %s\n", 
	    progname, XDisplayName(display_name));
    exit(-1);
  }
  
  screen = DefaultScreen(display);
  cmap = DefaultColormap(display, screen);
}

void InitColor()
{
  unsigned long plane_mask[3];
  unsigned long color[3];

  pen_fg = WhitePixel(display,screen);
  pen_bg = BlackPixel(display,screen);

  if (XAllocColorCells(display, cmap, False, plane_mask, 0, color, 3))
  {
    XColor ray_color, magicolor;

    ray_color.pixel = pen_ray = color[0];
    ray_color.flags = DoRed | DoGreen | DoBlue;
    ray_color.red = 0x0000;
    ray_color.green = 0x0000;
    ray_color.blue = 0xFFFF;
    XStoreColor(display, cmap, &ray_color);

    magicolor.pixel = pen_magicolor[0] = color[1];
    magicolor.flags = DoRed | DoGreen | DoBlue;
    magicolor.red = 0x0000;
    magicolor.green = 0xAFFF;
    magicolor.blue = 0x0000;
    XStoreColor(display, cmap, &magicolor);

    magicolor.pixel = pen_magicolor[1] = color[2];
    magicolor.flags = DoRed | DoGreen | DoBlue;
    magicolor.red = 0x0000;
    magicolor.green = 0xFFFF;
    magicolor.blue = 0x0000;
    XStoreColor(display, cmap, &magicolor);

    color_status = DYNAMIC_COLORS;
  }
  else		/* if visual has no read/write colors */
  {
    XColor ray_color;

    ray_color.flags = DoRed | DoGreen | DoBlue;
    ray_color.red = 0x0000;
    ray_color.green = 0x0000;
    ray_color.blue = 0xFFFF;
    XAllocColor(display, cmap, &ray_color);
    pen_ray = ray_color.pixel;

    color_status = STATIC_COLORS;
  }
}

void InitWindow(int argc, char *argv[])
{
  unsigned int border_width = 4;
  Pixmap icon_pixmap, iconmask_pixmap;
  unsigned int icon_width,icon_height;
  int icon_hot_x,icon_hot_y;
  char icon_filename[256];
  XSizeHints size_hints;
  XWMHints wm_hints;
  XClassHint class_hints;
  XTextProperty windowName, iconName;
  XGCValues gc_values;
  unsigned long gc_valuemask;
  char *window_name = "Mirror Magic II - McDuffin's Revenge";
  char *icon_name = "MirrorMagic";
  long window_event_mask;
  int i;

  width = WIN_XSIZE;
  height = WIN_YSIZE;

  window = XCreateSimpleWindow(display, RootWindow(display, screen),
			    WIN_XPOS, WIN_YPOS, width, height, border_width,
			    pen_fg, pen_bg);

  sprintf(icon_filename,"%s/%s",GFX_PATH,icon_pic.picture_filename);
  XReadBitmapFile(display,window,icon_filename,
		  &icon_width,&icon_height,
		  &icon_pixmap,&icon_hot_x,&icon_hot_y);
  if (!icon_pixmap)
  {
    fprintf(stderr, "%s: cannot read icon bitmap file '%s'.\n",
	    progname,icon_filename);
    exit(-1);
  }

  sprintf(icon_filename,"%s/%s",GFX_PATH,icon_pic.picturemask_filename);
  XReadBitmapFile(display,window,icon_filename,
		  &icon_width,&icon_height,
		  &iconmask_pixmap,&icon_hot_x,&icon_hot_y);
  if (!iconmask_pixmap)
  {
    fprintf(stderr, "%s: cannot read icon bitmap file '%s'.\n",
	    progname,icon_filename);
    exit(-1);
  }

  size_hints.flags = PSize | PMinSize | PMaxSize;
  size_hints.width  = size_hints.min_width  = size_hints.max_width  = width;
  size_hints.height = size_hints.min_height = size_hints.max_height = height;

  if (!XStringListToTextProperty(&window_name, 1, &windowName))
  {
    fprintf(stderr, "%s: structure allocation for windowName failed.\n",
	    progname);
    exit(-1);
  }

  if (!XStringListToTextProperty(&icon_name, 1, &iconName))
  {
    fprintf(stderr, "%s: structure allocation for iconName failed.\n",
	    progname);
    exit(-1);
  }

  wm_hints.initial_state = NormalState;
  wm_hints.input = True;
  wm_hints.icon_pixmap = icon_pixmap;
  wm_hints.icon_mask = iconmask_pixmap;
  wm_hints.flags = StateHint | IconPixmapHint | IconMaskHint | InputHint;

  class_hints.res_name = progname;
  class_hints.res_class = "MirrorMagic";

  XSetWMProperties(display, window, &windowName, &iconName, 
		   argv, argc, &size_hints, &wm_hints, 
		   &class_hints);

  XFree(windowName.value);
  XFree(iconName.value);

  /* Select event types wanted */
  window_event_mask = ExposureMask | ButtonPressMask | ButtonReleaseMask |
                      ButtonMotionMask | KeyPressMask | StructureNotifyMask;
  XSelectInput(display, window, window_event_mask);

  /* create GC for normal drawing */
  gc_values.graphics_exposures = False;
  gc_values.foreground = pen_bg;
  gc_values.background = pen_bg;
  gc_valuemask = GCGraphicsExposures | GCForeground | GCBackground;
  gc = XCreateGC(display, window, gc_valuemask, &gc_values);

  /* create GCs for line drawing (black and white) */
  for(i=0;i<2;i++)
  {
    gc_values.graphics_exposures = False;
    gc_values.foreground = (i ? pen_ray : pen_bg);
    gc_values.background = pen_bg;
    gc_values.line_width = 4;
    gc_values.line_style = LineSolid;
    gc_values.cap_style = CapRound;
    gc_values.join_style = JoinRound;

    gc_valuemask = GCGraphicsExposures | GCForeground | GCBackground |
                   GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle;
    line_gc[i] = XCreateGC(display, window, gc_valuemask, &gc_values);
  }
}

void InitGfx()
{
  int i,j;
  int xpm_err;
  unsigned int width,height;
  int hot_x,hot_y;
  XGCValues clip_gc_values;
  unsigned long clip_gc_valuemask;
  char filename[300];
  Pixmap shapemask;


  for(i=0;i<NUM_PICTURES;i++)
  {
    if (pic[i].picture_filename)
    {
      xpm_att[i].valuemask = XpmCloseness;
      xpm_att[i].closeness = 20000;

      sprintf(filename,"%s/%s",GFX_PATH,pic[i].picture_filename);

#if 0
      printf("XpmReadFileToPixmap(%s)\n...",filename);
#endif

      xpm_err = XpmReadFileToPixmap(display,window,filename,
				    &pix[i],&shapemask,&xpm_att[i]);
      switch(xpm_err)
      {
	case XpmOpenFailed:
          fprintf(stderr,"Xpm file open failed on '%s' !\n",filename);
	  CloseAll();
	  exit(-1);
	case XpmFileInvalid:
	  fprintf(stderr,"Invalid Xpm file '%s'!\n",filename);
	  CloseAll();
	  exit(-1);
	case XpmNoMemory:
	  fprintf(stderr,"Not enough memory !\n");	
	  CloseAll();
	  exit(1);
	case XpmColorFailed:
	  fprintf(stderr,"Can`t get any colors...\n");
	  CloseAll();
	  exit(-1);
	default:
	  break;
      }

      if (!pix[i])
      {
	fprintf(stderr, "%s: cannot read Xpm file '%s'.\n",
		progname,filename);
	CloseAll();
	exit(-1);
      }
    }
    if (pic[i].picturemask_filename)
    {
      sprintf(filename,"%s/%s",GFX_PATH,pic[i].picturemask_filename);

#if 0
      printf("XReadBitmapFile(%s)\n...",filename);
#endif

      switch(XReadBitmapFile(display,window,filename,
			     &width,&height,
			     &clipmask[i],&hot_x,&hot_y))
      {
	case BitmapSuccess:
          break;
	case BitmapOpenFailed:
	  fprintf(stderr,"Bitmap file open failed on '%s' !\n",filename);
	  CloseAll();
	  exit(-1);
	  break;
	case BitmapFileInvalid:
	  fprintf(stderr,"Bitmap file invalid: '%s' !\n",filename);
	  CloseAll();
	  exit(-1);
	  break;
	case BitmapNoMemory:
	  fprintf(stderr,"No memory for file '%s' !\n",filename);
	  CloseAll();
	  exit(-1);
	  break;
	default:
	  break;
      }

      clip_gc_values.graphics_exposures = False;
      clip_gc_values.foreground = pen_fg;
      clip_gc_values.background = pen_bg;
      clip_gc_values.clip_mask = clipmask[i];
      clip_gc_valuemask =
	GCGraphicsExposures | GCForeground | GCBackground | GCClipMask;
      clip_gc[i] = XCreateGC(display,window,clip_gc_valuemask,&clip_gc_values);

      if (!clipmask[i])
      {
	fprintf(stderr, "%s: cannot read X11 bitmap file '%s'.\n",
		progname,filename);
	CloseAll();
	exit(-1);
      }
    }
  }

  pix[DB_BACK] = XCreatePixmap(display, window,
			       WIN_XSIZE,WIN_YSIZE,
			       XDefaultDepth(display,screen));
  pix[DB_DOOR] = XCreatePixmap(display, window,
			       4*DXSIZE,DYSIZE,
			       XDefaultDepth(display,screen));

  imagemask = XGetImage(display, clipmask[BACK],
			0,0,640,400,AllPlanes,XYPixmap);

  if (color_status==DYNAMIC_COLORS)
  {
    XSetForeground(display,clip_gc[MAGICOLOR],pen_magicolor[0]);
    XSetClipOrigin(display,clip_gc[MAGICOLOR],392,0);
    XFillRectangle(display,pix[BACK],clip_gc[MAGICOLOR], 392,0, 248,72);

    XSetForeground(display,clip_gc[MAGICOLOR],pen_magicolor[1]);
    XSetClipOrigin(display,clip_gc[MAGICOLOR],392,-72);
    XFillRectangle(display,pix[BACK],clip_gc[MAGICOLOR], 392,0, 248,72);

    XSetClipOrigin(display,clip_gc[MAGICOLOR],300,-144);
    XFillRectangle(display,pix[DOOR],clip_gc[MAGICOLOR], 300,0, 100,100);
  }

  XCopyArea(display,pix[BACK],pix[DB_BACK],gc,0,0,WIN_XSIZE,WIN_YSIZE,0,0);
  XFillRectangle(display,pix[DB_BACK],gc,SX-2,SY-2,SXSIZE+4,SYSIZE+4);
  drawto = pix[DB_BACK];

  for(i=0;i<FIELDX;i++)
    for(j=0;j<FIELDY;j++)
      redraw[i][j]=0;
  redraw_tiles=0;
  redraw_mask=REDRAW_ALL;
}

void CloseAll()
{
  int i;

  StopSounds();
  FreeSounds();

  if (sound_process_id)
    kill(sound_process_id, SIGTERM);

  for(i=0;i<NUM_PIXMAPS;i++)
  {
    if (i<NUM_PICTURES)
    {
      if (clipmask[i])
	XFreePixmap(display,clipmask[i]);
      if (clip_gc[i])
	XFreeGC(display, clip_gc[i]);
      if (pix[i])
      {
	XFreeColors(display,DefaultColormap(display,screen),
		    xpm_att[i].pixels,xpm_att[i].npixels,0);
	XpmFreeAttributes(&xpm_att[i]);
      }
    }

    if (pix[i])
      XFreePixmap(display,pix[i]);
  }

  XFreeGC(display, gc);
  XCloseDisplay(display);

  exit(0);
}
