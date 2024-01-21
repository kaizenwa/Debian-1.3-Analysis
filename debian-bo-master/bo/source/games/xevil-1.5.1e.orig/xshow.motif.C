// Program to display an image with the appropriate mask.
// Steve Hardt 2/7/93


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

// X includes
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
}
#include <iostream.h>


// Defines
#define FILENAME_LENGTH 100
#define TIME_UNIT 1000


// Structures
struct State {
  State(XtAppContext,Widget,int,char **);
  void reloadImage();

  XtAppContext app;
  Widget drawing;
  Display *dpy;
  Screen *scr_ptr;
  int scr_num;
  Window root;
  int depth;
  Colormap cmap;
  GC gc;

  char pixmapName[FILENAME_LENGTH];
  Boolean maskSet;
  char maskName[FILENAME_LENGTH];
};



// Functions
void State::reloadImage()
{
  unsigned int pixmapWidth,pixmapHeight,
  maskWidth,maskHeight;
  int x_hot,y_hot;
  Pixmap pixmap,mask;

  if (XReadBitmapFile(dpy,root,pixmapName,
		  &pixmapWidth,&pixmapHeight,&pixmap,&x_hot,&y_hot) != 
     BitmapSuccess)
    {
      cout << "Failed to read pixmap." << endl;
      return;
    }

  if (maskSet)
    {
      if (XReadBitmapFile(dpy,root,maskName,
			  &maskWidth,&maskHeight,&mask,&x_hot,&y_hot) != 
	  BitmapSuccess)
	{
	  XFreePixmap(dpy,pixmap);
	  cout << "Failed to read mask." << endl;
	  return;
	}

      if ((pixmapWidth != maskWidth) || (pixmapHeight != maskHeight))
	{
	  XFreePixmap(dpy,pixmap);
	  XFreePixmap(dpy,mask);
	  cout << "Pixmap and mask are not of the same size." << endl;
	  return;
	}

      XSetClipMask(dpy,gc,mask);
    }

  XClearWindow(dpy,XtWindow(drawing));

  XtVaSetValues(drawing,
		XmNwidth,pixmapWidth,
		XmNheight,pixmapHeight,
		NULL);
  
  XCopyPlane(dpy,pixmap,XtWindow(drawing),gc,
	     0,0,pixmapWidth,pixmapHeight,0,0,1UL);

  XSetClipMask(dpy,gc,None);
  XFreePixmap(dpy,pixmap);
  if (maskSet)
    XFreePixmap(dpy,mask);
  
  if (maskSet)
    cout << pixmapName << " with mask " << maskName << " reloaded." << endl;
  else
    cout << pixmapName << " reloaded." << endl;
}



 State::State(XtAppContext aContext,Widget d,int argc,char **argv)
{
  app = aContext;
  drawing = d;
  dpy = XtDisplay(d);
  scr_ptr = DefaultScreenOfDisplay(dpy);
  scr_num = DefaultScreen(dpy);
  root = RootWindowOfScreen(scr_ptr);
  depth = DefaultDepthOfScreen(scr_ptr);
  cmap = DefaultColormap(dpy,scr_num);

  if (argc < 4)
    {
      cout << "Usage: " << argv[0] << " <foreground> <background> <pixmap> " <<
	"[mask]" << endl;
      exit (-1);
    }
  
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
  
  strcpy(pixmapName,argv[3]);
  if (argc >= 5)
    {
      maskSet = True;
      strcpy(maskName,argv[4]);
    }
  else
    maskSet = False;

  gc = XCreateGC(dpy,root,GCForeground|GCBackground,&values);
}



void reloadImageCB (Widget, State *state, XmDrawingAreaCallbackStruct *cbs)
{
  // Implement quit function.
  if ((cbs->reason == XmCR_INPUT) &&
      (cbs->event->xany.type == KeyPress))
    {
      KeySym keysym;
      Modifiers dummy;
      XtTranslateKeycode(state->dpy,cbs->event->xkey.keycode,
			 cbs->event->xkey.state,&dummy,&keysym);

      if (keysym == XK_q)
	exit(0);
    }

  state->reloadImage();
}



// Reload image if either pixmap or mask has been modified 
void checkImageTO(State *state,XtIntervalId)
{
  static Boolean already;
  static time_t pixmapTime;
  static time_t maskTime;

  struct stat pixmapStat;
  struct stat maskStat;

  stat(state->pixmapName,&pixmapStat);
  if (state->maskSet)
    stat(state->maskName,&maskStat);
  
  if ((! already) ||
      difftime(pixmapStat.st_mtime,pixmapTime) ||
      (state->maskSet && difftime(maskStat.st_mtime,maskTime)))
    {
      pixmapTime = pixmapStat.st_mtime;
      if (state->maskSet)
	maskTime = maskStat.st_mtime;
      state->reloadImage();
      already = True;
    }

  XtAppAddTimeOut(state->app,TIME_UNIT,(XtTimerCallbackProc)checkImageTO,
	       (XtPointer)state);
}



main (int argc, char **argv)
{
  XtAppContext app;

  Widget toplevel = 
    XtVaAppInitialize(&app,"xbmshow",
		      NULL,0,
		      (int *)(&argc),argv,
		      NULL,
		      XmNallowShellResize,True,
		      NULL);
  XtVaSetValues(toplevel,
		XmNtitle, "xbmshow",
		NULL);

  Widget drawing = XtVaCreateManagedWidget("drawing",
					   xmDrawingAreaWidgetClass, toplevel,
					   NULL);
  
  State state(app,drawing,argc,argv);
  
  XtAddCallback(drawing,XmNinputCallback,(XtCallbackProc)reloadImageCB,
		(XtPointer)(&state));
  XtAddCallback(drawing,XmNexposeCallback,(XtCallbackProc)reloadImageCB,
		(XtPointer)(&state));
  XtAppAddTimeOut(app,TIME_UNIT,(XtTimerCallbackProc)checkImageTO,
	       (XtPointer)(&state));

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
}
