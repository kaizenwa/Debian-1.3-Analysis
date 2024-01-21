// Program to display an image with the appropriate mask.
// Steve Hardt 2/7/93
// Modified 5/4/94 to work without Motif.
// hardts@mit.edu

// Include Files
extern "C"
{
// C includes
#include <stdio.h>
#include <stdlib.h> // For exit().
#include <time.h>

// Unix includes
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

// X includes
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
}
#include <assert.h>
#include <iostream.h>


// Defines
#define FILENAME_LENGTH 100
#define TIME_UNIT 1 // sec
#define INITIAL_WIDTH 1
#define INITIAL_HEIGHT 1



class State {
 public:
  State(int,char **);
  Display *getDpy() {return dpy;}
  int canDraw() {return pixmap != 0;}
  void reloadImage();
  void checkQuit(XEvent &);
  void checkFiles();
  void draw();


 private:
  Display *dpy;
  Window drawing;
  Screen *scr_ptr;
  int scr_num;
  Window root;
  int depth;
  Colormap cmap;
  GC gc;
  Pixmap pixmap,mask; // 0 if not yet defined.
  int pixmapWidth,pixmapHeight;

  char pixmapName[FILENAME_LENGTH];
  char maskSet;
  char maskName[FILENAME_LENGTH];

  time_t pixmapTime;
  time_t maskTime;
};



// Functions
State::State(int argc,char **argv)
{
  // Parse command line.
  if (argc < 4)
    {
      cout << "Usage: " << argv[0] << " <foreground> <background> <pixmap> " 
	<< "[mask]" << endl;
      exit (-1);
    }

  strcpy(pixmapName,argv[3]);
  if (argc >= 5)
    {
      maskSet = True;
      strcpy(maskName,argv[4]);
    }
  else
    maskSet = False;

  
  // Open display, create window.
  dpy = XOpenDisplay(NULL);

  if (! dpy)
    cout << "Could not open display." << endl;

  scr_ptr = DefaultScreenOfDisplay(dpy);
  scr_num = DefaultScreen(dpy);
  root = RootWindowOfScreen(scr_ptr);
  depth = DefaultDepthOfScreen(scr_ptr);
  cmap = DefaultColormap(dpy,scr_num);

  XColor color,dummy;
  unsigned long bgPixel = WhitePixel(dpy,scr_num);
  if (XAllocNamedColor(dpy,cmap,"antiquewhite",&color,&dummy))
    {
      bgPixel = color.pixel;
    }

  drawing = XCreateSimpleWindow(dpy, root, 
				0, 0, INITIAL_WIDTH, INITIAL_HEIGHT, 
				0, 
				BlackPixel(dpy,scr_num), 
				bgPixel);
  
  
  XSizeHints size_hints;
  size_hints.flags = PPosition | PSize ;

  XTextProperty windowName, iconName;
  char *window_name = "xshow";  // Will appear on window.
  char *icon_name = "xshow";
  assert(XStringListToTextProperty(&window_name,1,&windowName));
  assert(XStringListToTextProperty(&icon_name,1,&iconName)); 
  
  XWMHints wm_hints;
  wm_hints.initial_state = NormalState;
  wm_hints.input = True;
  wm_hints.flags = StateHint | InputHint;

  XClassHint class_hints;
  class_hints.res_name = argv[0];
  class_hints.res_class = "XShow";

  XSetWMProperties(dpy,drawing,&windowName,&iconName,
		   argv,argc,&size_hints,&wm_hints,&class_hints);

  XSelectInput(dpy,drawing,ExposureMask | KeyPressMask | ButtonPressMask);

  XMapWindow(dpy,drawing);
  

  // Create GC.
  XColor actual,database;
  XGCValues values;
  if (! XAllocNamedColor(dpy,cmap,argv[1],&actual,&database))
    {
      cout << "Could not allocate foreground color" << endl;
      exit (-1);
    }
  values.foreground = actual.pixel;

  if (! XAllocNamedColor(dpy,cmap,argv[2],&actual,&database))
    {
      cout << "Could not allocate background color" << endl;
      exit (-1);
    }
  values.background = actual.pixel;
  gc = XCreateGC(dpy,root,GCForeground|GCBackground,&values);

  // Set previous times for pixmap and mask.
  struct stat pixmapStat;
  stat(pixmapName,&pixmapStat);
  pixmapTime = pixmapStat.st_mtime;

  if (maskSet)
    {
      struct stat maskStat;
      stat(maskName,&maskStat);
      maskTime = maskStat.st_mtime;
    }

  pixmap = mask = 0;
}



void State::reloadImage()
{
  Pixmap pixmapTmp,maskTmp;
  unsigned int pixmapWidthTmp,pixmapHeightTmp,
  maskWidthTmp,maskHeightTmp;
  int x_hot,y_hot;

  if (XReadBitmapFile(dpy,root,pixmapName,
		      &pixmapWidthTmp,&pixmapHeightTmp,&pixmapTmp,&x_hot,&y_hot) != 
      BitmapSuccess)
    {
      cout << pixmapName << ":  failed to read pixmap." << endl;
      return;
    }

  if (maskSet)
    {
      if (XReadBitmapFile(dpy,root,maskName,
			  &maskWidthTmp,&maskHeightTmp,&maskTmp,&x_hot,&y_hot) != 
	  BitmapSuccess)
	{
	  XFreePixmap(dpy,pixmapTmp);
	  cout << maskName << ":  failed to read mask." << endl;
	  return;
	}

      if ((pixmapWidthTmp != maskWidthTmp) || (pixmapHeightTmp != maskHeightTmp))
	{
	  XFreePixmap(dpy,pixmapTmp);
	  XFreePixmap(dpy,maskTmp);
	  cout << pixmapName << ":  pixmap and mask have different sizes." 
	    << endl;
	  return;
	}
    }


  // Replace old pixmap and mask with new.
  if (pixmap)
    XFreePixmap(dpy,pixmap);
  pixmap = pixmapTmp;
  pixmapWidth = pixmapWidthTmp;
  pixmapHeight = pixmapHeightTmp;
  if (maskSet)
    {
      if (mask)
	XFreePixmap(dpy,mask);
      mask = maskTmp;
    }

  XWindowChanges changes;
  changes.width = pixmapWidth;
  changes.height = pixmapHeight;
  XReconfigureWMWindow(dpy,drawing,scr_num,CWWidth | CWHeight,&changes);
  
  if (maskSet)
    cout << pixmapName << " with mask " << maskName << " reloaded." << endl;
  else
    cout << pixmapName << " reloaded." << endl;

  draw();
}



void State::checkQuit(XEvent &event)
{
  if (event.type == KeyPress)
    {
      KeySym keysym;
      XLookupString(&event.xkey,NULL,0,&keysym,NULL);
      
      if (keysym == XK_q)
	exit(0);
    }
}



void State::checkFiles()
{
  struct stat pixmapStat;
  stat(pixmapName,&pixmapStat);

  struct stat maskStat;
  if (maskSet)
    stat(maskName,&maskStat);
  
  if ((pixmapStat.st_mtime != pixmapTime) ||
      (maskSet && (maskStat.st_mtime != maskTime)))
    {
      pixmapTime = pixmapStat.st_mtime;
      if (maskSet)
	maskTime = maskStat.st_mtime;
      reloadImage();
    }
}



void State::draw()
{
  if (pixmap)
    {
      if (maskSet)
	assert(mask);

      XClearWindow(dpy,drawing);
      if (maskSet)
	XSetClipMask(dpy,gc,mask);
      XCopyPlane(dpy,pixmap,drawing,gc,
		 0,0,pixmapWidth,pixmapHeight,0,0,1UL);
      if (maskSet)
	XSetClipMask(dpy,gc,None);
    }
}



main (int argc, char **argv)
{
  State state(argc,argv);

  while (True) 
    {
      XEvent event;
      if (XCheckMaskEvent(state.getDpy(),
			  ExposureMask | KeyPressMask | ButtonPressMask,
			  &event))
	{
	  switch (event.type) {
	  case KeyPress:
	    state.checkQuit(event);
	    // No "break;" so will reload image.
	      
	    case ButtonPress:
	      state.reloadImage();
	    break;

	  case Expose:
	    if (state.canDraw())
	      state.draw();
	    else
	      state.reloadImage();
	    break;
	    
	  default:
	    break;
	  };
	}
      else
	state.checkFiles();

      sleep(TIME_UNIT);
    }
}
