#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/xpm.h>
#include <X11/extensions/shape.h>
#include <time.h>
#include <X11/Xatom.h>

#include "clk.xpm"
#include "led.xpm"
#include "month.xpm"
#include "date.xpm"
#include "weekday.xpm"
#include "xpm/mask.xbm"
#include "xpm/mask.xpm"

int SHOWAMPM=0;  /* default value is 24h format */
int ONLYSHAPE=0; /* default value is noshape */
int ITBLINKS=1;  /* default is blinking */
int ICONIFIED=0; /* default is not iconified */
/* led positions *************************************************************/
int twelve[5]  = {5, 14, 24, 28, 37};
int twfour[5]  = {4,  8, 17, 22, 31};
/* with shape */
int ws_posx[11] = {0,0,0,0,0,40, 17, 17, 22, 27, 15};
int ws_posy[4]  = {3, 21, 30, 45};

/* no shape */
int ns_posx[11] = {5,5,5,5,5,45, 21, 21, 26, 31, 19};
int ns_posy[4]  = {7, 25, 34, 49};

int posx[11];
int posy[4];

/* X11 Variablen *************************************************************/
Display *dpy;	  /* welches DISPLAY */
Window Root;      /* Hintergrund-Drawable */
int screen;
int x_fd;
int d_depth;
XSizeHints mysizehints;
XWMHints mywmhints;
Pixel back_pix, fore_pix;
GC NormalGC;
Window iconwin, win;       /* My home is my window */
char *ProgName;
char *Geometry;
char *LedColor = "LightSeaGreen";
char Execute[] = "echo no program has been specified >/dev/console";
char *ERR_colorcells = "not enough free color cells\n";
char *ampers = " &";
/* XPM Variablen *************************************************************/
typedef struct _XpmIcon {
    Pixmap pixmap;
    Pixmap mask;
    XpmAttributes attributes;
}        XpmIcon;

XpmIcon asclock, led, month, date, weekday;
XpmIcon visible;
time_t actualtime;
long actualmin;

/* lokale Funktionen *********************************************************/
#define MW_EVENTS   (ExposureMask | ButtonPressMask | StructureNotifyMask)
#define FALSE 0
void GetXPM(void);
Pixel GetColor(char *name);
void RedrawWindow( XpmIcon *v);
void InsertTime();
/*****************************************************************************/
/*****************************************************************************/
static char *help_message[] = {
"where options include:",
"    -12                     12 hour format",
"    -24                     24 hour format",
"    -exe <program>          program to start on click",
"    -led <color>            color of the led",
"    -position [+|-]x[+|-]y  position of asclock",
"    -shape                  without groundplate",
"    -noblink                don't blink",
"    -iconic                 start up as icon",
NULL
};

void usage()
{
  char **cpp;

  fprintf(stderr,"usage:  %s [-options ...] \n", ProgName);
  for (cpp = help_message; *cpp; cpp++) {
    fprintf(stderr, "%s\n", *cpp);
  }
  fprintf(stderr,"\n");
  exit(1);
}
int main(int argc,char *argv[])
{
  int i;
  unsigned int borderwidth ;
  char *display_name = NULL; 
  char *wname = "asclock";
  XGCValues gcv;
  unsigned long gcm;
  XEvent Event;
  XTextProperty name;
  XClassHint classHint;
  Pixmap pixmask;
  ProgName = argv[0];
  Geometry = "";
  
  /* Parse command line options */
  ProgName = argv[0];

  for(i=1;i<argc;i++) {
    char *arg= argv[i];

    if (arg[0] == '-') {
      switch(arg[1]) {
      case '1':
	SHOWAMPM=1;
	continue;
      case '2':
	SHOWAMPM=0;
	continue;
      case 'e':
	if(++i >=argc) usage();
	strcpy(&Execute[0], argv[i]);
	strcat(&Execute[0], " &");
	continue;
      case 's':
	ONLYSHAPE=1;
	continue;
      case 'p':
	if(++i >=argc) usage();
	Geometry = argv[i];
	continue;
      case 'i':
	ICONIFIED=1;
	continue;
      case 'l':
	if(++i >=argc) usage();
	LedColor = argv[i];
	continue;
      case 'n':
        ITBLINKS = 0;
        continue;
      default:
	usage();
      }
    }
  }
  /* init led position */
  for(i=0;i<4;i++)
    posy[i] = ONLYSHAPE ? ws_posy[i] : ns_posy[i];
  for(i=0;i<11;i++)
    posx[i] = ONLYSHAPE ? ws_posx[i] : ns_posx[i]; 
  for(i=0;i<5;i++)
    posx[i] += SHOWAMPM ? twfour[i] : twelve[i];

  /* Open the display */
  if (!(dpy = XOpenDisplay(display_name)))  
    { 
      fprintf(stderr,"asclock: can't open display %s\n", 
	      XDisplayName(display_name)); 
      exit (1); 
    } 
  screen= DefaultScreen(dpy);
  Root = RootWindow(dpy, screen);
  d_depth = DefaultDepth(dpy, screen);
  x_fd = XConnectionNumber(dpy);
  
  /* Icon Daten nach XImage konvertieren */
  GetXPM();
  
  /* Create a window to hold the banner */
  mysizehints.flags= USSize|USPosition;
  mysizehints.x = 0;
  mysizehints.y = 0;

  back_pix = GetColor("white");
  fore_pix = GetColor("black");

  XWMGeometry(dpy, screen, Geometry, NULL, (borderwidth =1), &mysizehints,
	      &mysizehints.x,&mysizehints.y,&mysizehints.width,&mysizehints.height, &i); 

  mysizehints.width = asclock.attributes.width;
  mysizehints.height= asclock.attributes.height;

  win = XCreateSimpleWindow(dpy,Root,mysizehints.x,mysizehints.y,
			    mysizehints.width,mysizehints.height,
			    borderwidth,fore_pix,back_pix);
  iconwin = XCreateSimpleWindow(dpy,win,mysizehints.x,mysizehints.y,
				mysizehints.width,mysizehints.height,
				borderwidth,fore_pix,back_pix);



  /* Hints aktivieren */
  XSetWMNormalHints(dpy, win, &mysizehints);
  classHint.res_name =  "asclock";
  classHint.res_class = " ASClock";
  XSetClassHint(dpy, win, &classHint);

  XSelectInput(dpy,win,MW_EVENTS);
  XSelectInput(dpy,iconwin,MW_EVENTS);
  
  if (XStringListToTextProperty(&wname, 1, &name) ==0) {
    fprintf(stderr, "asclock: can't allocate window name\n");
    exit(-1);
  }
  XSetWMName(dpy, win, &name);
  
  /* Create a GC for drawing */
  gcm = GCForeground|GCBackground|GCGraphicsExposures;
  gcv.foreground = fore_pix;
  gcv.background = back_pix;
  gcv.graphics_exposures = FALSE;
  NormalGC = XCreateGC(dpy, Root, gcm, &gcv);  

  if (ONLYSHAPE) { /* try to make shaped window here */
    pixmask = XCreateBitmapFromData(dpy, win, mask_bits, mask_width, 
				    mask_height);
    XShapeCombineMask(dpy, win, ShapeBounding, 0, 0, pixmask, ShapeSet);
    XShapeCombineMask(dpy, iconwin, ShapeBounding, 0, 0, pixmask, ShapeSet);
  }
  
  mywmhints.initial_state = (ICONIFIED ? IconicState : NormalState);
  mywmhints.icon_window = iconwin;
  mywmhints.icon_x = mysizehints.x;
  mywmhints.icon_y = mysizehints.y;
  mywmhints.flags = StateHint | IconWindowHint | IconPositionHint;
  XSetWMHints(dpy, win, &mywmhints); 

  XMapWindow(dpy,win);

  InsertTime();
  RedrawWindow(&visible);
  while(1)
    {
      if (actualtime != time(0))
	{
	  actualtime = time(0);
	  if(actualmin != actualtime / 60)
	    {
	      InsertTime();
	    }
	  if (ITBLINKS)
	    if (actualtime % 2)
	      /* Sekunden Doppelpunkt ein */
	      XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
			90,0,3,11,posx[2], posy[0]);
	    else
	      /* Sekunden Doppelpunkt aus */
	      XCopyArea(dpy, asclock.pixmap, visible.pixmap, NormalGC,
			27,6,3,11,posx[2], posy[0]);
	  
	  RedrawWindow(&visible);

	}
      
      /* read a packet */
      while (XPending(dpy))
	{
	  XNextEvent(dpy,&Event);
	  switch(Event.type)
	    {
	    case Expose:
	      if(Event.xexpose.count == 0 )
		RedrawWindow(&visible);
	      break;
	    case ButtonPress:
	      system(Execute);
	      break;
	    case DestroyNotify:
              XFreeGC(dpy, NormalGC);
              XDestroyWindow(dpy, win);
	      XDestroyWindow(dpy, iconwin);
              XCloseDisplay(dpy);
	      exit(0); 
	    default:
	      break;      
	    }
	}
      XFlush(dpy);
#ifdef SYSV
      poll((struct poll *) 0, (size_t) 0, 50);
#else
      usleep(50000L);			/* 5/100 sec */
#endif
    }
  return 0;
}
/****************************************************************************/
void nocolor(char *a, char *b)
{
 fprintf(stderr,"asclock: can't %s %s\n", a,b);
}
/****************************************************************************/
/* Konvertiere XPMIcons nach XImage */
void GetXPM(void)
{
  static char **clock_xpm;
  XColor col;
  XWindowAttributes attributes;
  char led1[22];
  char led2[22];
  int ret;

  clock_xpm =ONLYSHAPE ? mask_xpm : clk_xpm;

  /* for the colormap */
  XGetWindowAttributes(dpy,Root,&attributes);

  /* get user-defined color */
  if (!XParseColor (dpy, attributes.colormap, LedColor, &col)) 
    {
      nocolor("parse",LedColor);
    }

  sprintf(&led1[0], ".      c #%4X%4X%4X", col.red, col.green, col.blue);
  for(ret=10;ret<22;ret++)
    if(led1[ret]==' ') led1[ret]='0';
  led_xpm[2] = &led1[0];

  col.red   = (col.red  /10) *3;
  col.green = (col.green/10) *3;
  col.blue  = (col.blue /10) *3;
  sprintf(&led2[0], "X      c #%4X%4X%4X", col.red, col.green, col.blue);
  for(ret=10;ret<22;ret++)
    if(led2[ret]==' ') led2[ret]='0';
  led_xpm[3] = &led2[0];

  asclock.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, clock_xpm, &asclock.pixmap, 
				&asclock.mask, &asclock.attributes);
  if(ret != XpmSuccess)
    {fprintf(stderr, ERR_colorcells);exit(1);}
  visible.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, clk_xpm, &visible.pixmap, 
				&visible.mask, &visible.attributes);

  led.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, led_xpm, &led.pixmap, 
				&led.mask, &led.attributes);
  if(ret != XpmSuccess)
    {fprintf(stderr, ERR_colorcells);exit(1);}

  month.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, month_xpm, &month.pixmap, 
				&month.mask, &month.attributes);
  if(ret != XpmSuccess)
    {fprintf(stderr, ERR_colorcells);exit(1);}

  date.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, date_xpm, &date.pixmap, 
				&date.mask, &date.attributes);
  if(ret != XpmSuccess)
    {fprintf(stderr, ERR_colorcells);exit(1);}

  weekday.attributes.valuemask |= (XpmReturnPixels | XpmReturnExtensions);
  ret = XpmCreatePixmapFromData(dpy, Root, weekday_xpm, &weekday.pixmap, 
				&weekday.mask, &weekday.attributes);
  if(ret != XpmSuccess)
    {fprintf(stderr, ERR_colorcells);exit(1);}
}
/****************************************************************************/
/* Removes expose events for a specific window from the queue */
int flush_expose (Window w)
{
  XEvent dummy;
  int i=0;
  
  while (XCheckTypedWindowEvent (dpy, w, Expose, &dummy))i++;
  return i;
}

/****************************************************************************/
/* Draws the icon window */
void RedrawWindow( XpmIcon *v)
{
  flush_expose (iconwin);
  XCopyArea(dpy,v->pixmap,iconwin,NormalGC,
	    0,0,v->attributes.width, v->attributes.height,0,0);
  flush_expose (win);
  XCopyArea(dpy,v->pixmap,win,NormalGC,
	    0,0,v->attributes.width, v->attributes.height,0,0);

}
/****************************************************************************/
Pixel GetColor(char *name)
{
  XColor color;
  XWindowAttributes attributes;

  XGetWindowAttributes(dpy,Root,&attributes);
  color.pixel = 0;
   if (!XParseColor (dpy, attributes.colormap, name, &color)) 
     {
       nocolor("parse",name);
     }
   else if(!XAllocColor (dpy, attributes.colormap, &color)) 
     {
       nocolor("alloc",name);
     }
  return color.pixel;
}
/****************************************************************************/
static struct tm *clk;

void Twelve()
{
  int thishour;
  /* Stunde ohne am/pm */
  thishour = clk->tm_hour % 12;
  if (thishour == 0 )
    thishour = 12;

  if (clk->tm_hour >= 12)
    {
      /* PM */
      XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
		107,5,11,6,posx[5],posy[0]+5);
    }
  else
    /* AM */
    XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	      94,5,12,6,posx[5],posy[0]+5);
  
  if (thishour>9)
    XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	      13,0,5,11,posx[0], posy[0]);

  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(thishour % 10),0,9,11,posx[1], posy[0]);
  
  /* Minute, drawn first, so am/pm won't be overwritten */
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_min / 10),0,9,11,posx[3],posy[0]);
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_min % 10),0,9,11,posx[4],posy[0]);

  
}

void TwentyFour()
{
  /* Stunde ohne am/pm */
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_hour / 10),0,9,11,posx[0],posy[0]);
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_hour % 10),0,9,11,posx[1], posy[0]);

  /* Minute */
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_min / 10),0,9,11,posx[3],posy[0]);
  XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	    9*(clk->tm_min % 10),0,9,11,posx[4],posy[0]);
  
}
/****************************************************************************/
void InsertTime()
{
  int thismonth, thisweekday, thisdate;

  /* Zeit auslesen */
  actualtime = time(0);
  actualmin = actualtime / 60;

  clk = localtime(&actualtime);

  /* leere asclock holen */
  XCopyArea(dpy, asclock.pixmap, visible.pixmap, NormalGC,
	    0,0,mysizehints.width,mysizehints.height,0,0);

  if (SHOWAMPM)
  {
    Twelve();
  }
  else
    TwentyFour();

  /* Monat  */ 
  XCopyArea(dpy, month.pixmap, visible.pixmap, NormalGC,
	    0,6*(clk->tm_mon ),22,6,posx[10],posy[3]);

  /* Datum */
  if (clk->tm_mday>9)
    {
      XCopyArea(dpy, date.pixmap, visible.pixmap, NormalGC,
		9*((clk->tm_mday / 10 +9) % 10),0,9,13,posx[7],posy[2]);
      XCopyArea(dpy, date.pixmap, visible.pixmap, NormalGC,
		9*((clk->tm_mday % 10 +9) % 10),0,9,13,posx[9],posy[2]);      
    }
  else
    XCopyArea(dpy, date.pixmap, visible.pixmap, NormalGC,
	      9*(clk->tm_mday -1),0,9,13,posx[8], posy[2]);
    
  /* Wochentag */
  XCopyArea(dpy, weekday.pixmap, visible.pixmap, NormalGC,
            0,6*((clk->tm_wday +6) % 7),21,7,posx[6], posy[1]); 

  if (! ITBLINKS ) 
    /* Sekunden Doppelpunkt ein */
     XCopyArea(dpy, led.pixmap, visible.pixmap, NormalGC,
	      90,0,3,11,posx[2], posy[0]);
    
}













