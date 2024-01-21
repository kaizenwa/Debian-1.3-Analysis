/*
 *   A multi-player version of Tetris for the X Window System
 *
 *   Copyright (C) 1996 Roger Espel Llima <roger.espel.llima@pobox.com>
 *
 *   Started: 10 Oct 1996
 *   Version: 1.01
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation. See the file COPYING for details.
 *
 */

#define VERSION "1.01"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/time.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#define DEFAULTPORT 19503

#define PROT_VERS_1 1
#define PROT_VERS_2 0
#define PROT_VERS_3 0

#define OP_NICK 1
#define OP_PLAY 2
#define OP_FALL 3
#define OP_DRAW 4
#define OP_LOST 5
#define OP_GONE 6
#define OP_CLEAR 7
#define OP_NEW 8
#define OP_LINES 9
#define OP_GROW 10
#define OP_MODE 11
#define OP_LEVEL 12
#define OP_BOT 13
#define OP_KILL 14
#define OP_PAUSE 15
#define OP_CONT 16
#define OP_VERSION 17
#define OP_BADVERS 18
#define OP_MSG 19
#define OP_YOUARE 20
#define OP_LINESTO 21


static int shape[7][4][8] = {

    /*  The I   ++++++++  */
 {
    {  1, 3, 2, 3, 3, 3, 4, 3 },
    {  3, 1, 3, 2, 3, 3, 3, 4 },
    {  1, 3, 2, 3, 3, 3, 4, 3 },
    {  3, 1, 3, 2, 3, 3, 3, 4 }
 },
    /*  The O      ++++
                   ++++   */
 {
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 },
    {  2, 3, 3, 3, 2, 2, 3, 2 }
 },

    /*  The T   ++++++
                  ++      */
 {
    {  3, 2, 2, 3, 3, 3, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 3 },
    {  2, 2, 3, 2, 4, 2, 3, 3 },
    {  2, 3, 3, 2, 3, 3, 3, 4 }
 },

    /*  The L   ++++++
                ++        */
 {
    {  2, 2, 2, 3, 3, 3, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 2 },
    {  2, 2, 3, 2, 4, 2, 4, 3 },
    {  2, 4, 3, 2, 3, 3, 3, 4 }
 },

    /*  The J   ++++++
                    ++    */
 {
    {  2, 3, 3, 3, 4, 2, 4, 3 },
    {  3, 2, 3, 3, 3, 4, 4, 4 },
    {  2, 2, 3, 2, 4, 2, 2, 3 },
    {  2, 2, 3, 2, 3, 3, 3, 4 }
 },

    /*  The S     ++++
                ++++      */
 {
    {  2, 2, 3, 2, 3, 3, 4, 3 },
    {  3, 3, 3, 4, 4, 2, 4, 3 },
    {  2, 2, 3, 2, 3, 3, 4, 3 },
    {  3, 3, 3, 4, 4, 2, 4, 3 }
 },

    /*  The Z   ++++
                  ++++    */
 {
    {  2, 3, 3, 2, 3, 3, 4, 2 },
    {  3, 2, 3, 3, 4, 3, 4, 4 },
    {  2, 3, 3, 2, 3, 3, 4, 2 },
    {  3, 2, 3, 3, 4, 3, 4, 4 }
 }
    
};

int bw = 0;
int funmode = 0;
int flashy = 0;
int standalone = 0;
int sqrsize = 16;
int hscroll = 10;
int paused = 0;
int visiblemsgs = 0, msgsclosed = 0;
int verbose = 0;

/* X stuff */
char *disp = NULL;
Display *display;
int depth;
int scn;
Window win;
GC gc;
XFontStruct *font_info;
int height, width, color[10][3], white, black, x_initted = 0;

int keysspecified = 0;
KeySym quit_key = XK_q, start_key = XK_s, pause_key = XK_p, bot_key = XK_b;
KeySym text_key = XK_t, togglenext_key = XK_Return, mode_key = XK_n;
KeySym verbose_key = XK_v, clear_key = XK_c, warp_key = XK_Tab;
KeySym down_key = XK_comma;
KeySym level_keys[10] = 
  { XK_0, XK_1, XK_2, XK_3, XK_4, XK_5, XK_6, XK_7, XK_8, XK_9 };
KeySym keys[6] = { XK_J, XK_j, XK_k, XK_l, XK_L, XK_space };

int warp_next = 1;

#define MAXNICKLEN 14

/* this needs to be a fixed-width font */
#define FONT "-misc-fixed-bold-r-normal--13-*-*-*-*-*-*-*"
#define FONTWIDTH 7
#define FONTHEIGHT 13

/* position of all buttons & areas */

#define XBTNS 20
#define XLVLS 19
#define XSPEEDTXT 21
#define XNEXT 15
#define XNEXTTXT 16
#define XNICK (XMARGIN + (WPITIN-WNICK) / 2) /* relative to u->x0 */
#define XMSGS 30
#define XSCROLL (XPIT + LEFTSIDE)
#define XMSGENTRY (XPIT + LEFTSIDE)

#define YQUIT 13
#define YPLAY 40
#define YPAUSE 67
#define YBOT 94
#define YSPEEDTXT 140
#define YLVLS 148
#define YMODE 235
#define YNEXTTXT 279
#define YNEXT 285
#define YNICK (16 + YMARGIN + (HSQR * ROWS))
#define YMSGS YNICK
#define YSCROLL (HEIGHT0 + 10)
#define YMSGENTRY (YSCROLL + HSCROLL * FONTHEIGHT + 17)
#define YVERBOSE (YSCROLL + 20)
#define YCLEAR (YSCROLL + 55)

#define XMARGIN 8
#define YMARGIN 12

/* width & height of buttons & areas */

#define LEFTSIDE (46 + 4 * WSQR) /* size of the whole left-side buttons area */

#define WIDTH0 ((WSQR * COLS) + XMARGIN + 22)
#define HEIGHT0 (52 + YMARGIN + (HSQR * ROWS))

#define WSQR sqrsize
#define HSQR sqrsize

#define WBTNS 70
#define HBTNS 20
#define WMSGS 50
#define WNICKCHARS 16

#define WNICK (WNICKCHARS * FONTWIDTH + 8)
#define WL 20
#define HL 20
#define WNEXT (4 * WSQR + 16)
#define HNEXT (2 * HSQR + 12)
#define WSCROLL 50 /* for 2 users */
#define HSCROLL hscroll
#define WSCROLLRESIZE (X_EACH / FONTWIDTH)
#define HSCROLLRESIZE (HSCROLL * FONTHEIGHT + HTEXTENTRIES + 45)
#define WMSGENTRY (WSCROLL * FONTWIDTH + 8)
#define HTEXTENTRIES 23

/* don't change these: */

#define COLS 10
#define ROWS 20

#define X_EACH (WIDTH0 + 10)
#define XPIT (XMARGIN + 2)
#define YPIT (YMARGIN + 2)
#define WPITIN (COLS * WSQR)
#define HPITIN (ROWS * HSQR)
#define WPITOUT (COLS * WSQR + 4)
#define HPITOUT (ROWS * HSQR + 4)

#define ALIGN_LEFT 0
#define ALIGN_CENTER 1
#define ALIGN_RIGHT 2

typedef void (*v_v)(void);
typedef void (*v_vtp)(struct timeval *, void *);
typedef void (*v_si)(char *, int);

struct button {
  int x, y, w, h;
  int down, flash;
  char *txt;
  int align;
  v_v callback;
  struct button *next;
};

struct button *button0 = NULL; 

struct toggle {
  int x, y, w, h;
  char *txt;
  v_v callback;
  int down;
  int align;
  struct toggle *next;
  struct toggle *master;
};

struct toggle *tog0 = NULL;

struct textentry {
  int x, y, w, h;
  int cols; /* in chars */
  char *txt;
  int maxlen;
  int offset;
  int align;
  int cursor;
  int next_clears;
  v_si callback;
  struct textentry *next;
};

struct textentry *text0 = NULL;

struct textscroll {
  int x, y, w, h;
  int rows, cols; /* in chars */
  char **lines, **plines;
  int nextpline, nextline;
  int color;
  struct textentry *redirect;
  struct textscroll *next;
};

struct textscroll *scroll0 = NULL;

struct timer {
  struct timeval tv;
  v_vtp action;
  void *data;
  struct timer *next;
};

struct timer *timer0 = NULL;

struct lostfalldata {
  struct user *u;
  int counter;
};

struct flashlinedata {
  struct user *u;
  int counter;
  char lines[20];
  int nlines, color;
  int oldpit[20][10];
};

/* server stuff */

#define BUFLEN 1024
int sfd;
int port = 0;
unsigned char realbuf[512], readbuf[BUFLEN], *buf = realbuf + 4;
int nread = 0;
char *serverhost = NULL;

struct textentry *nickentry, *msgentry;
struct button *modebutton, *pausebutton, *quitbutton, *playbutton, *botbutton;
struct button *textbutton = NULL, *verbosebutton, *clearbutton;
struct toggle *leveltogs[10];
struct textscroll *msgscroll;
int scrollcols; /* for 1 user */

struct user {
  unsigned char number;
  int x0;
  char nick[MAXNICKLEN+2];
  struct user *next;
  int pit[20][10];
  char haslost, flashing;
  char replaybuf[BUFLEN];
  int replaylen;
};

struct user me;

int delays[10] = { 1500000, 1200000, 900000, 700000, 500000, 350000, 200000,
		   120000, 80000 };

/* #define textwidth(text)  XTextWidth(font_info, text, strlen(text)) */
#define textwidth(text) (FONTWIDTH * strlen(text))

#define WINNAME "xtris v" VERSION

int oldpit[20][10], oldpittmp[20][10];
#define EMPTY	7
#define GRAY    8
#define TEXT	9
int piece, rotation, x, y, next, shownext = 1, level = 5;
int playing = 0, got_play = 0, interrupt = 0, addlines = 0, addsquares = 0;


/* like memcpy, but guaranteed to handle overlap when s <= t */
void copydown(char *s, char *t, int n) {
  for (; n; n--)
    *(s++) = *(t++);
}

void fatal(char *s) {
  fprintf(stderr, "%s.\n", s);
  exit(1);
}

void *mmalloc(int n) {
  void *r;

  r = malloc(n);
  if (r == NULL)
    fatal("Out of memory");
  return r;
}

void u_sleep(int i) {
  struct timeval tm;
  tm.tv_sec = i / 1000000;
  tm.tv_usec = i % 1000000;
  select(0, NULL, NULL, NULL, &tm);
}

void sendbuf(int len) {
  if (!standalone) {
    realbuf[0] = realbuf[1] = realbuf[2] = 0;
    realbuf[3] = (unsigned char)len;
    buf[0] = 0;
    write(sfd, realbuf, 4 + len);
  }
}

void getkeyresource(char *r, KeySym *key) {
  char *s;
  KeySym k;

  s = XGetDefault(display, "xtris", r);
  if (s != NULL) {
    k = XStringToKeysym(s);
    if (k != NoSymbol)
      *key = k;
    else
      fprintf(stderr, "warning: keysym not found '%s'\n", s);
  }
}

int resourcetrue(char *r) {
  char *s;

  s = XGetDefault(display, "xtris", r);
  if (s && (strcmp(s, "on") == 0 ||
	    strcmp(s, "yes") == 0 ||
	    strcmp(s, "true") == 0))
    return 1;
  
  return 0;
}

void getnumresource(char *r, int *i) {
  char *s;

  s = XGetDefault(display, "xtris", r);
  if (s)
    *i = atoi(s);
}

void getresources() {
  if (!keysspecified) {
    getkeyresource("quitKey", &quit_key);
    getkeyresource("startKey", &start_key);
    getkeyresource("pauseKey", &pause_key);
    getkeyresource("botKey", &bot_key);
    getkeyresource("textKey", &text_key);
    getkeyresource("togglenextKey", &togglenext_key);
    getkeyresource("clearKey", &clear_key);
    getkeyresource("verboseKey", &verbose_key);
    getkeyresource("warpKey", &warp_key);
    getkeyresource("modeKey", &mode_key);

    getkeyresource("leftBorderKey", &keys[0]);
    getkeyresource("leftKey", &keys[1]);
    getkeyresource("rotateKey", &keys[2]);
    getkeyresource("rightKey", &keys[3]);
    getkeyresource("rightBorderKey", &keys[4]);
    getkeyresource("downKey", &down_key);
    getkeyresource("dropKey", &keys[5]);
  }

  if (resourcetrue("big"))
    sqrsize = 20;
  
  if (resourcetrue("flashy"))
    flashy = 1;

  if (!flashy && resourcetrue("bw"))
    bw = 1;
  
  if (resourcetrue("verbose"))
    verbose = 1;
  
  getnumresource("scrollLines", &hscroll);
  if (hscroll < 3 || hscroll > 30)
    hscroll = 10;
}

int GetColor(char *s) {
  Colormap cmap;
  XColor c, xc;

  cmap = DefaultColormap(display, scn);
  if (!XAllocNamedColor(display, cmap, s, &c, &xc))
    fatal("Can't allocate colors");
  return(c.pixel);
}

void getcolors(char *resource, char *light, char *normal, char *dark,
	       int *color) {
  Colormap cmap;
  XColor c, xc, nc;
  char *s;

  cmap = DefaultColormap(display, scn);

  s = XGetDefault(display, "xtris", resource);
  if (s != NULL) {
    if (!XAllocNamedColor(display, cmap, s, &c, &xc))
      fatal("Can't allocate colors");
    color[1] = c.pixel;
    nc = c;
    nc.red = nc.red >= 48000 ? 65535 : nc.red + nc.red / 3;
    nc.blue = nc.blue >= 48000 ? 65535 : nc.blue + nc.blue / 3;
    nc.green = nc.green >= 48000 ? 65535 : nc.green + nc.green / 3;
    if (!XAllocColor(display, cmap, &nc))
      fatal("Can't allocate colors");
    color[0] = nc.pixel;
    nc = c;
    nc.red = nc.red - nc.red / 3;
    nc.blue = nc.blue - nc.blue / 3;
    nc.green = nc.green - nc.green / 3;
    if (!XAllocColor(display, cmap, &nc))
      fatal("Can't allocate colors");
    color[2] = nc.pixel;
  } else {
    if (!XAllocNamedColor(display, cmap, light, &c, &xc))
      fatal("Can't allocate colors");
    color[0] = c.pixel;
    if (!XAllocNamedColor(display, cmap, normal, &c, &xc))
      fatal("Can't allocate colors");
    color[1] = c.pixel;
    if (!XAllocNamedColor(display, cmap, dark, &c, &xc))
      fatal("Can't allocate colors");
    color[2] = c.pixel;
  }
}

void init_X() {
  XSizeHints szh;
  XTextProperty windowName;
  XGCValues values;
  XEvent xev;
  char *win_name;
  int n;

  display = XOpenDisplay(disp);
  if (display == NULL)
    fatal("Can't open X display");

  getresources();

  if ((font_info = XLoadQueryFont(display, FONT)) == NULL)
    fatal("Can't load font " FONT);

  scn = DefaultScreen(display);
  depth = DefaultDepth(display, scn);
  width = WIDTH0 + LEFTSIDE;
  height = HEIGHT0;
  white = WhitePixel(display, scn);
  black = BlackPixel(display, scn);

  if (depth > 2) {
    getcolors("background", "#e1e1e1", "#b2b2b2", "#656565", color[EMPTY]);

    color[TEXT][0] = white;
    color[TEXT][1] = color[EMPTY][1];
    color[TEXT][2] = GetColor("#404040");
  } else {
    color[EMPTY][0] = black;
    color[EMPTY][1] = white;
    color[EMPTY][2] = black;
    color[TEXT][0] = black;
    color[TEXT][1] = white;
    color[TEXT][2] = black;
  }

  color[GRAY][0] = color[EMPTY][0];
  color[GRAY][1] = color[EMPTY][1];
  color[GRAY][2] = color[EMPTY][2];

  win = XCreateSimpleWindow(display, RootWindow(display, scn), 0, 0, width, height, 4, WhitePixel(display, scn), color[7][1]);
  szh.flags = PMinSize | PMaxSize | PSize | PResizeInc;
  szh.min_width = width;
  szh.min_height = height;
  szh.max_width = width;
  szh.max_height = height;
  szh.width_inc = 1;
  szh.height_inc = 1;
  win_name = mmalloc(strlen(WINNAME) + 2);
  strcpy(win_name, WINNAME);

  if (XStringListToTextProperty(&win_name, 1, &windowName) == (Status)0)
    fatal("X barfed while setting window name");
  XSetWMProperties(display, win, &windowName, NULL, NULL, 0, &szh, NULL, NULL);

  XSelectInput(display, win, ExposureMask | KeyPressMask | ButtonPressMask | StructureNotifyMask | Button1MotionMask | VisibilityChangeMask);
  
  gc = XCreateGC(display, win, (unsigned long)0, &values);
  XSetFont(display, gc, font_info->fid);
  XSetForeground(display, gc, BlackPixel(display, scn));
  XMapWindow(display, win);
  
  /* wait until window gets actually mapped */
  do {
    XNextEvent(display, &xev);
  } while (xev.type != MapNotify);

  if (depth > 4 && !bw) {
    if (flashy) {

      color[0][0] = GetColor("#fa0000");
      color[0][1] = GetColor("#bc0000");     /* dark red */
      color[0][2] = GetColor("#7d0000");   

      color[1][0] = GetColor("#00fa00");   
      color[1][1] = GetColor("#00bc00");     /* green */
      color[1][2] = GetColor("#007d00");   

      color[2][0] = GetColor("#0000fa");   
      color[2][1] = GetColor("#0000bc");     /* dark blue */
      color[2][2] = GetColor("#00007c");   

      color[3][0] = GetColor("#fafa00");   
      color[3][1] = GetColor("#bcbc00");     /* yellow */
      color[3][2] = GetColor("#7d7d00");   

      color[4][0] = GetColor("#00fafa");   
      color[4][1] = GetColor("#00bcbc");     /* light blue-green */
      color[4][2] = GetColor("#007d7d");   

      color[5][0] = GetColor("#fa00fa");   
      color[5][1] = GetColor("#bc00bc");     /* mauve */
      color[5][2] = GetColor("#7d007d");   
    
      color[6][0] = GetColor("#bc00fa");   
      color[6][1] = GetColor("#7d00bc");     /* dark violetish blue */
      color[6][2] = GetColor("#1f007d");   

    } else {

      /* dark red */
      getcolors("brick0", "#fa0000", "#bc0000", "#7d0000", color[0]);

      /* green */
      getcolors("brick1", "#00f000", "#00a800", "#007000", color[1]);

      /* blue */
      getcolors("brick2", "#4080f0", "#0060d8", "#00407c", color[2]);

      /* brownish */
      getcolors("brick3", "#d0b000", "#968000", "#706000", color[3]);

      /* dark blue-green */
      getcolors("brick4", "#00b0b0", "#008080", "#006060", color[4]);

      /* mauve */
      getcolors("brick5", "#fa00fa", "#bc00bc", "#7d007d", color[5]);
      
      /* dark violetish blue */
      getcolors("brick6", "#bc00fa", "#7d00bc", "#1f007d", color[6]);

    }
  } else if (depth > 4 && bw) {
      color[0][0] = GetColor("#e0e0e0");
      color[0][1] = GetColor("#606060");
      color[0][2] = black;
    for (n=1; n<7; n++) {
      color[n][0] = color[0][0];
      color[n][1] = color[0][1];
      color[n][2] = black;
    }
  } else {
    for (n=0; n<7; n++) {
      color[n][0] = white;
      color[n][1] = black;
      color[n][2] = black;
    }
  }
}

void SetColor(int color) {
  XGCValues values;

  values.foreground = color;
  XChangeGC(display, gc, GCForeground, &values);
}

void DrawSqr(struct user *u, int type, int x, int y) {
  if (!u) {
    /* fixed position for "next"-type squares */
    x = (x * WSQR) + XNEXT + 6;
    y = (y * HSQR) + YNEXT + 5;
  } else {
    x = (x * WSQR) + XPIT + u->x0;
    y = (y * HSQR) + YPIT;
  }
  type &= 0xff;
  if (type == EMPTY) {
    SetColor(color[EMPTY][1]);
    XFillRectangle(display, win, gc, x, y, WSQR, HSQR);
  } else {
    SetColor(color[type][1]);
    XFillRectangle(display, win, gc, x+2, y+2, WSQR-4, HSQR-4);
    SetColor(color[type][0]);
    XDrawLine(display, win, gc, x, y, x, y+HSQR-1);
    XDrawLine(display, win, gc, x, y, x+WSQR-1, y);
    XDrawLine(display, win, gc, x+1, y, x+1, y+HSQR-1);
    XDrawLine(display, win, gc, x, y+1, x+WSQR-1, y+1);
    SetColor(color[type][2]);
    XDrawLine(display, win, gc, x+WSQR-2, y+2, x+WSQR-2, y+HSQR-2);
    XDrawLine(display, win, gc, x+WSQR-1, y+1, x+WSQR-1, y+HSQR-1);
    XDrawLine(display, win, gc, x, y+HSQR-1, x+WSQR-1, y+HSQR-1);
    XDrawLine(display, win, gc, x+1, y+HSQR-2, x+WSQR-2, y+HSQR-2);
  }
}

void DrawLine(struct user *u, int type, int y) {
  int x;

  x = u->x0 + XPIT;
  y = (y * HSQR) + YPIT;

  SetColor(color[type][1]);
  XFillRectangle(display, win, gc, x+2, y+2, WPITIN-4, HSQR-4);
  SetColor(color[type][0]);
  XDrawLine(display, win, gc, x, y, x, y+HSQR-1);
  XDrawLine(display, win, gc, x, y, x+WPITIN-1, y);
  XDrawLine(display, win, gc, x+1, y, x+1, y+HSQR-1);
  XDrawLine(display, win, gc, x, y+1, x+WPITIN-1, y+1);
  SetColor(color[type][2]);
  XDrawLine(display, win, gc, x+WPITIN-2, y+2, x+WPITIN-2, y+HSQR-2);
  XDrawLine(display, win, gc, x+WPITIN-1, y+1, x+WPITIN-1, y+HSQR-1);
  XDrawLine(display, win, gc, x, y+HSQR-1, x+WPITIN-1, y+HSQR-1);
  XDrawLine(display, win, gc, x+1, y+HSQR-2, x+WPITIN-2, y+HSQR-2);
}

void DrawButton(int updown, int bar, int x, int y, int w, int h, char *text, int align, int reverse) {
  int u, l, xx;

  if (x > width || y > height)
    return;
  if (updown) {
    u = 2;
    l = 0;
  } else {
    u = 0;
    l = 2;
  }
  if (bar) {
    SetColor(color[EMPTY][1]);
    XFillRectangle(display, win, gc, x+2, y+2, w-4, h-4);
  }
  SetColor(color[EMPTY][u]);
  XDrawLine(display, win, gc, x, y, x, y+h-1);
  XDrawLine(display, win, gc, x, y, x+w-1, y);
  XDrawLine(display, win, gc, x+1, y, x+1, y+h-1);
  XDrawLine(display, win, gc, x, y+1, x+w-1, y+1);
  SetColor(color[EMPTY][l]);
  XDrawLine(display, win, gc, x+w-2, y+2, x+w-2, y+h-2);
  XDrawLine(display, win, gc, x+w-1, y+1, x+w-1, y+h-1);
  XDrawLine(display, win, gc, x, y+h-1, x+w-1, y+h-1);
  XDrawLine(display, win, gc, x+1, y+h-2, x+w-2, y+h-2);
  if (text) {
    if (reverse)
      SetColor(color[TEXT][u]);
    else
      SetColor(color[TEXT][l]);
    switch (align) {
      case ALIGN_LEFT:
	xx = x + 4;
	break;
      case ALIGN_CENTER:
	xx = x + ((w - textwidth(text)) >> 1);
	break;
      default:
	xx = x + w - textwidth(text) - 4;
	break;
    }
    XDrawString(display, win, gc, xx, y+FONTHEIGHT+1 , text, strlen(text));
  }
}

void WriteXY(int x, int y, char *txt) {
  XDrawString(display, win, gc, x, y, txt, strlen(txt));
}

void UpdateWinSize() {
  XSizeHints szh;

  XResizeWindow(display, win, width, height);
  szh.flags = PMinSize | PMaxSize | PSize | PResizeInc;
  szh.min_width = width;
  szh.min_height = height;
  szh.max_width = width;
  szh.max_height = height;
  szh.width_inc = 1;
  szh.height_inc = 1;
  XSetWMProperties(display, win, NULL, NULL, NULL, 0, &szh, NULL, NULL);
}

struct button *DeclareButton(int x, int y, int w, int h, int down, int flash,
			     char *txt, int align, v_v callback) {
  struct button *b;

  b = mmalloc(sizeof (struct button));
  b->x = x;
  b->y = y;
  b->w = w;
  b->h = h;
  b->down = down;
  b->flash = flash;
  b->txt = txt;
  b->align = align;
  b->callback = callback;
  b->next = button0;
  button0 = b;

  return b;
}

void RemoveButton(struct button *b) {
  struct button *bb;

  if (b == button0) {
    button0 = button0->next;
  } else {
    for (bb=button0; bb->next; bb=bb->next) {
      if (bb->next == b) {
	bb->next = bb->next->next;
	break;
      }
    }
  }

  SetColor(color[EMPTY][1]);
  XFillRectangle(display, win, gc, b->x, b->y, b->w, b->h);
  XFlush(display);
}

struct toggle *DeclareToggle(int x, int y, int w, int h, char *txt, int align,
			     v_v callback, struct toggle *master) {
  struct toggle *t;

  t = mmalloc(sizeof (struct toggle));
  t->x = x;
  t->y = y;
  t->w = w;
  t->h = h;
  t->txt = txt;
  t->align = align;
  t->callback = callback;
  t->next = tog0;
  t->down = (master == NULL);
  t->master = master == NULL ? t : master;
  tog0 = t;

  return t;
}

struct textentry *DeclareTextEntry(int x, int y, int cols, int maxlen,
				   char *txt, int cursor, v_si callback) {
  struct textentry *t;
  char *s;

  s = mmalloc(5 + maxlen);
  t = mmalloc(sizeof(struct textentry));
  t->x = x;
  t->y = y;
  t->w = cols * FONTWIDTH + 8;
  t->cols = cols;
  t->h = HTEXTENTRIES;
  t->txt = s;
  t->next_clears = 0;
  t->offset = 0;
  t->cursor = cursor;
  strncpy(s, txt, maxlen);
  t->maxlen = maxlen;
  t->callback = callback;
  t->next = text0;
  text0 = t;

  return t;
}

struct textscroll *DeclareTextScroll(int x, int y, int cols, int rows, 
				     int color, struct textentry *redirect) {
  struct textscroll *t;
  int i;

  t = mmalloc(sizeof(struct textscroll));
  t->x = x;
  t->y = y;
  t->w = cols * FONTWIDTH + 8;
  t->h = rows * FONTHEIGHT + 8;
  t->rows = rows;
  t->cols = cols;
  t->lines = mmalloc(rows * sizeof(char *));
  t->plines = mmalloc(rows * sizeof(char *));
  for (i=0; i<rows; i++)
    t->lines[i] = t->plines[i] = NULL;
  t->nextpline = t->nextline = 0;
  t->color = color;
  t->next = scroll0;
  t->redirect = redirect;
  scroll0 = t;

  return t;
}

void DrawScrollTextLine(struct textscroll *t, int line) {
  SetColor(t->color);
  if (t->plines[line])
    WriteXY(t->x + 4, t->y + 14 + FONTHEIGHT * line, t->plines[line]);
}

void DrawScrollText(struct textscroll *t) {
  int i;

  DrawButton(1, 1, t->x, t->y, t->w, t->h, NULL, ALIGN_LEFT, 0);
  for (i=0; i<t->rows; i++)
    DrawScrollTextLine(t, i);
}

void AddScrollPline(struct textscroll *t, char *s, int update) {
  char *ss;
  int i;

  ss = mmalloc(1 + strlen(s));
  strcpy(ss, s);

  if (t->nextpline >= t->rows) {
    if (t->plines[0])
      free(t->plines[0]);
    for (i=0; i<t->rows-1; i++)
      t->plines[i] = t->plines[i+1];
    t->plines[t->rows - 1] = ss;
    if (update)
      DrawScrollText(t);
  } else {
    t->plines[t->nextpline] = ss;
    if (update)
      DrawScrollTextLine(t, t->nextpline);
    t->nextpline++;
  }
  if (update)
    XFlush(display);
}

void AddLineToPlines(struct textscroll *t, char *s, int update) {
  char *p, *w, *lww, *lws;

  lww = w = p = mmalloc(2 + t->cols);

  while (*s) {
    if (w - p >= t->cols - 1) {
      if (*s == ' ')
	lww = w;
      else if (lww > p + 7) {
	w = lww;
	s = lws;
      }
      *w = 0;
      AddScrollPline(t, p, update);
      w = p;
      *w++ = ' ';
      *w++ = ' ';
      *w++ = ' ';
      *w++ = ' ';
      *w++ = ' ';
      *w++ = ' ';
      lww = w;
    }
    *w++ = *s;
    if (*s++ == ' ') {
      lws = s;
      lww = w;
    }
  }
  if (w > p) {
    *w = 0;
    AddScrollPline(t, p, update);
  }
  free(p);
}

void RecalcPlines(struct textscroll *t) {
  int i;

  for (i=0; i<t->rows; i++) {
    if (t->plines) {
      free(t->plines[i]);
      t->plines[i] = NULL;
    }
  }
  t->nextpline = 0;
  for (i=0; i<t->rows; i++)
    if (t->lines[i] != NULL)
      AddLineToPlines(t, t->lines[i], 0);
}

void ClearScrollArea(struct textscroll *t) {
  int i;

  for (i=0; i<t->rows; i++) {
    if (t->plines) {
      free(t->plines[i]);
      t->plines[i] = NULL;
    }
    if (t->lines) {
      free(t->lines[i]);
      t->lines[i] = NULL;
    }
  }
  t->nextpline = 0;
  t->nextline = 0;
  DrawButton(1, 1, t->x, t->y, t->w, t->h, NULL, ALIGN_LEFT, 0);
}

void SetTextScrollWidth(struct textscroll *t, int cols) {
  t->cols = cols;
  t->w = cols * FONTWIDTH + 8;
  RecalcPlines(t);
}

void SetTextEntryWidth(struct textentry *t, int cols) {
  int len;

  len = strlen(t->txt);
  t->cols = cols;
  t->w = cols * FONTWIDTH + 8;
  if (len - t->offset >= t->cols)
    t->offset = len - t->cols + 3;
}

void AddScrollLine(struct textscroll *t, char *s) {
  char *ss;
  int i;

  ss = mmalloc(1 + strlen(s));
  strcpy(ss, s);

  if (t->nextline >= t->rows) {
    if (t->lines[0])
      free(t->lines[0]);
    for (i=0; i<t->rows-1; i++)
      t->lines[i] = t->lines[i+1];
    t->lines[t->rows - 1] = ss;
  } else {
    t->lines[t->nextline] = ss;
    t->nextline++;
  }
  AddLineToPlines(t, s, 1);
}

void RedrawButtons() {
  struct button *b;

  for (b=button0; b; b=b->next)
    DrawButton(b->down, 1, b->x, b->y, b->w, b->h, b->txt, b->align, 0);
}

void RedrawToggles() {
  struct toggle *t;
  
  for (t=tog0; t; t=t->next)
    DrawButton(t->down, 1, t->x, t->y, t->w, t->h, t->txt, t->align, 0);
}

void RedrawTextEntryArea(struct textentry *t) {
  int l;

  DrawButton(1, 1, t->x, t->y, t->w, t->h, t->txt + t->offset, t->align, t->next_clears ? 0 : 1);
  if (t->cursor) {
    l = strlen(t->txt) - t->offset;
    SetColor(color[TEXT][2]);
    XDrawLine(display, win, gc, t->x + FONTWIDTH * l + 6, t->y + 4, t->x + FONTWIDTH * l + 6, t->y + 16);
  }
  XFlush(display);
}

void RedrawTextEntries() {
  struct textentry *t;

  for (t=text0; t; t=t->next)
    RedrawTextEntryArea(t);
}

void RedrawTextScrolls() {
  struct textscroll *t;

  for (t=scroll0; t; t=t->next)
    DrawScrollText(t);
}

void ToggleDown(struct toggle *t) {
  struct toggle *tt;

  for (tt=tog0; tt; tt=tt->next) {
    if (tt != t && tt->down && tt->master == t->master) {
      tt->down = 0;
      DrawButton(0, 0, tt->x, tt->y, tt->w, tt->h, tt->txt, tt->align, 0);
      XFlush(display);
    }
  }
  if (!t->down) {
    t->down = 1;
    DrawButton(1, 0, t->x, t->y, t->w, t->h, t->txt, t->align, 0);
    XFlush(display);
  }
}

void ChangeButtonAppearance(struct button *b, int down, char *txt, int align) {
/* down == -1 => no change;  txt == NULL => no change; align == -1 => no change */

  if (down >= 0)
    b->down = down;
  if (txt != NULL)
    b->txt = txt;
  if (align >= 0)
    b->align = align;
  DrawButton(b->down, 1, b->x, b->y, b->w, b->h, b->txt, b->align, 0);
  XFlush(display);
}

void ChangeTextEntryArea(struct textentry *t, char *txt, int align, int cursor) {
/* txt == NULL => no change;  align == -1 => no change;  cursor == -1 => no change */
  if (align >= 0)
    t->align = align;
  if (txt != NULL)
    strncpy(t->txt, txt, t->maxlen);
  if (cursor >= 0)
    t->cursor = cursor;
  RedrawTextEntryArea(t);
}

void FlashButton(struct button *b) {
  DrawButton(!b->down, 0, b->x, b->y, b->w, b->h, b->txt, b->align, 0);
  XFlush(display);
  u_sleep(120000);
  DrawButton(b->down, 0, b->x, b->y, b->w, b->h, b->txt, b->align, 0);
  XFlush(display);
}

void SimulateButtonPress(struct button *b) {
  if (b->flash)
    FlashButton(b);
  b->callback();
}

void SimulateTextEntry(struct textentry *t, char *txt) {
  strncpy(t->txt, txt, t->maxlen);
  t->callback(t->txt, 0);
  t->next_clears = 1;
}

void DrawFrame() {
  struct user *u;

  /* the Next square */
  DrawButton(1, 0, XNEXT, YNEXT, WNEXT, HNEXT, NULL, ALIGN_LEFT, 0);
  SetColor(color[TEXT][0]);
  WriteXY(XNEXTTXT, YNEXTTXT, "Next:");

  WriteXY(XSPEEDTXT, YSPEEDTXT, "Speed:");

  RedrawButtons();
  RedrawToggles();
  RedrawTextEntries();
  RedrawTextScrolls();

  for (u=&me; u; u=u->next) {
    DrawButton(1, 0, XMARGIN + u->x0, YMARGIN, WPITOUT, HPITOUT, NULL, ALIGN_LEFT, 0);
    if (u != &me)  /* me.nick is actually a textarea */
      DrawButton(1, 0, XNICK + u->x0, YNICK, WNICK, HTEXTENTRIES, u->nick, ALIGN_CENTER, 0);
  }
}

/* a random number generator loosely based on RC5;
   assumes ints are at least 32 bit */

unsigned int my_rand() {
  static unsigned int s = 0, t = 0, k = 12345678;
  int i;

  if (s == 0 && t == 0) {
    s = (unsigned int)getpid();
    t = (unsigned int)time(NULL);
  }
  for (i=0; i<12; i++) {
    s = (((s^t) << (t&31)) | ((s^t) >> (31 - (t&31)))) + k;
    k += s + t;
    t = (((t^s) << (s&31)) | ((t^s) >> (31 - (s&31)))) + k;
    k += s + t;
  }
  return s;
}

int fits(int piece, int rotation, int x, int y) {
  int xx, yy, i;

  for (i=0; i<4; i++) {
    xx = x + shape[piece][rotation][2*i];
    yy = y - shape[piece][rotation][2*i+1];
    if (xx < 0 || xx > 9 || yy > 19)
      return 0;
    if (yy >= 0 && me.pit[yy][xx] != EMPTY)
      return 0;
  }
  return 1;
}

void put(int piece, int rotation, int x, int y, int color) {
  int xx, yy, i;

  for (i=0; i<4; i++) {
    xx = x + shape[piece][rotation][2*i];
    yy = y - shape[piece][rotation][2*i+1];
    if (yy >= 0)
      me.pit[yy][xx] = color;
  }
}

void drawnext() {
  int i, xx, yy;

  if (shownext)
    for (i=0; i<4; i++) {
      xx = shape[next][0][2*i];
      yy = 3 - shape[next][0][2*i+1];
      DrawSqr(NULL, next, xx-1, yy);
    }
}

void clearnext() {
  SetColor(color[EMPTY][1]);
  XFillRectangle(display, win, gc, XNEXT+3, YNEXT+3, WNEXT-6, HNEXT-6);
}

#define remove(p, r, x, y) put(p, r, x, y, EMPTY)

void refresh() {
  int x, y;
  int i = 2;

  buf[1] = OP_DRAW;

  for (x=0; x<10; x++)
    for (y=0; y<20; y++)
      if (me.pit[y][x] != oldpit[y][x]) {
	DrawSqr(&me, me.pit[y][x], x, y);
	if (me.pit[y][x] != (oldpit[y][x] & 0xff)) {
	  buf[i] = x;
	  buf[i+1] = y;
	  buf[i+2] = me.pit[y][x];
	  i += 3;
	  if (i > 200) {
	    sendbuf(i);
	    i = 2;
	    buf[1] = OP_DRAW;
	  }
	}
	oldpit[y][x] = me.pit[y][x];
      }
  if (i > 2)
    sendbuf(i);
}

void refreshme(int n) {
  int x, y;

  for (x=0; x<10; x++)
    for (y=0; y<n; y++)
      if (me.pit[y][x] != oldpit[y][x]) {
	DrawSqr(&me, me.pit[y][x], x, y);
	oldpit[y][x] = me.pit[y][x];
      }
}

void refreshuser(struct user *u) {
  int x, y, n = 0;

  for (x=0; x<10; x++)
    for (y=0; y<20; y++)
      if (u->pit[y][x] != oldpittmp[y][x]) {
	DrawSqr(u, u->pit[y][x], x, y);
	n++;
      }
  if (n)
    XFlush(display);
}

void redrawall() {
  int x, y;
  struct user *u;

  DrawFrame();
  for (u=&me; u; u=u->next)
    for (x=0; x<10; x++)
      for (y=0; y<20; y++)
	DrawSqr(u, u->pit[y][x], x, y);
  if (playing)
    drawnext();
  XFlush(display);
}

void copyline(struct user *u, int i, int j) {
  int n;

  for (n=0; n<10; n++)
    u->pit[j][n] = u->pit[i][n];
}

void emptyline(struct user *u, int i) {
  int n;

  for (n=0; n<10; n++)
    u->pit[i][n] = EMPTY;
}

void falline(struct user *u, int i) {
  int x;
  for (x = i-1; x>=0; x--)
    copyline(u, x, x+1);
  emptyline(u, 0);
}

void growline(struct user *u) {
  int n;
  for (n=0; n<19; n++)
    copyline(u, n+1, n);
  emptyline(u, 19);
}

void fallines() {
  int n, c, x, i, fallen = 0, nextcol;

  buf[1] = OP_FALL;
  nextcol = my_rand() % 7;
  for (n=0; n<20; n++) {
    c = 0;
    for (x=0; x<10; x++)
      if (me.pit[n][x] != EMPTY)
	c++;
    if (c == 10) {
      DrawLine(&me, nextcol, n);
      falline(&me, n);
      for (i=0; i<10; i++)
	oldpit[n][i] |= 0x100;
      buf[2 + fallen++] = (unsigned char)n;
    }
  }
  if (fallen) {
    sendbuf(2 + fallen);
    XFlush(display);
    u_sleep(80000);
    nextcol = (nextcol+1) % 7;
    for (i=2; i<fallen+2; i++)
      DrawLine(&me, nextcol, buf[i]);
    XFlush(display);
    u_sleep(80000);
    nextcol = (nextcol+1) % 7;
    for (i=2; i<fallen+2; i++)
      DrawLine(&me, nextcol, buf[i]);
    u_sleep(80000);
    XFlush(display);
    refreshme(20);
    XFlush(display);
  }
  if (fallen > 1 || (fallen > 0 && funmode)) {
    buf[1] = OP_LINES;
    buf[2] = (unsigned char)fallen;
    sendbuf(3);
  }
}

int newlines() {
  int i = addlines, j, k, r = 1;

  if (playing) {
    while (i > 0) {
      for (j=0; j<10; j++)
	if (me.pit[0][j] != EMPTY)
	  r = 0;
      growline(&me);
      buf[1] = OP_GROW;
      sendbuf(2);
      k = 0;
      for (j=0; j<10; j++)
      if (my_rand() % 7 < 3) {
        me.pit[19][j] = my_rand() % 7;
	k++;
      }
      if (k == 10)
	me.pit[19][my_rand() % 10] = EMPTY;
      i--;
    }
    if (addlines > 0) {
      refreshme(20 - addlines);
      refresh();
      XFlush(display);
    }
    addlines = 0;
  }
  return r;
}

int newsquares() {
  int n = addsquares, i, j, k, r = 1;

  if (playing) {
    while (n > 0) {
      for (i=0; i<10; i++)
	if (me.pit[0][i] != EMPTY)
	  r = 0;
      for (i=0; i<20; i++) {
	k = 0;
	for (j=0; j<10; j++) {
	  if (me.pit[i][j] != EMPTY)
	    k++;
	}
	if (k)
	  break;
      }
      if (k == 0 || i == 0) {
	i = 19;
	j = my_rand() % 10;
      } else {
	j = my_rand() % k;
	for (k=0; k<10; k++) {
	  if (me.pit[i][k] != EMPTY)
	    if (j == 0) {
	      me.pit[i-1][k] = GRAY;
	      break;
	    } else j--;
	}
      }
      n--;
    }
    if (addsquares > 0) {
      refresh();
      XFlush(display);
    }
    addsquares = 0;
  }
  return r;
}

#define sysmsg(s) msgfrom(NULL, s)

void msgfrom(struct user *u, char *s) {
  static char msgbuf[300];

  if (u)
    sprintf(msgbuf, "(%d) (%s)  %s", u->number, u->nick, s);
  else
    sprintf(msgbuf, "** %s", s);
  if (u || verbose) {
    AddScrollLine(msgscroll, msgbuf);
    if (!visiblemsgs && !msgsclosed && me.next != NULL) {
      height += HSCROLLRESIZE;
      visiblemsgs = 1;
      UpdateWinSize();
      redrawall();
    }
  }
}

void c_clear() {
  ClearScrollArea(msgscroll);
}

void c_msgentry(char *s, int shift) {
  buf[1] = OP_MSG;
  strcpy(&buf[2], s);
  sendbuf(strlen(s) + 2);
  msgfrom(&me, s);
  ChangeTextEntryArea(msgentry, "", ALIGN_LEFT, 1);
  if (shift) {
    XWarpPointer(display, None, win, 0, 0, 0, 0, 3, 3);
    warp_next = 1;
  }
}

void c_msgs() {
  if (visiblemsgs) {
    height -= HSCROLLRESIZE;
    visiblemsgs = 0;
    msgsclosed = 1;
  } else {
    height += HSCROLLRESIZE;
    visiblemsgs = 1;
  }
  UpdateWinSize();
  redrawall();
}

void c_quit() {
  exit(0);
}

void c_play() {
  if (standalone) {
    got_play = interrupt = 1;
  } else {
    buf[1] = OP_PLAY;
    sendbuf(2);
  }
}

void c_pause() {
  if (standalone) {
    paused = !paused;
    if (paused)
      ChangeButtonAppearance(pausebutton, 1, NULL, -1);
    else
      ChangeButtonAppearance(pausebutton, 0, NULL, -1);
  } else {
    buf[1] = paused ? OP_CONT : OP_PAUSE;
    sendbuf(2);
  }
}

void c_verbose() {
  verbose = !verbose;
  if (verbose)
    ChangeButtonAppearance(verbosebutton, 1, NULL, -1);
  else
    ChangeButtonAppearance(verbosebutton, 0, NULL, -1);
}

void c_bot() {
  switch (fork()) {
    case 0:
      close(sfd);
      close(ConnectionNumber(display)); /* ugh! */
#ifdef XTRISPATH
      execl(XTRISPATH "/xtbot", "xtbot", "-quiet", serverhost, NULL);
#else
      execlp("xtbot", "xtbot", "-quiet", serverhost, NULL);
#endif
      fprintf(stderr, "Can't start bot '%s'.\n",

#ifdef XTRISPATH
	XTRISPATH "/xtbot");
#else
	"xtbot");
#endif
      _exit(1);
    
    case -1:
      fatal("fork() failed");
    
    default:
      return;
  }
}

void c_setnick(char *s, int shift) {
  static char msgbuf[300];

  strncpy(me.nick, s, MAXNICKLEN);
  buf[1] = OP_NICK;
  memcpy(&buf[2], me.nick, strlen(me.nick));
  sendbuf(2 + strlen(me.nick));
  ChangeTextEntryArea(nickentry, s, ALIGN_CENTER, 0);
  sprintf(msgbuf, "you set your nick to %s", s);
  sysmsg(msgbuf);
  if (shift) {
    XWarpPointer(display, None, win, 0, 0, 0, 0, 3, 3);
    warp_next = 1;
  }
}

void setlevel(int n) {
  if (n < 0)
    n = 0;
  if (n > 9)
    n = 9;
  level = n;
  ToggleDown(leveltogs[n]);
}

void setmode(int n) {
  funmode = (n != 0);
  if (funmode)
    ChangeButtonAppearance(modebutton, -1, "fuN", -1);
  else
    ChangeButtonAppearance(modebutton, -1, "Normal", -1);
}

void sendlevel(int n) {
  if (standalone)
    setlevel(n);
  else {
    buf[1] = OP_LEVEL;
    buf[2] = n;
    sendbuf(3);
  }
}

void sendmode(int n) {
  if (standalone)
    setmode(n);
  else {
    buf[1] = OP_MODE;
    buf[2] = n;
    sendbuf(3);
  }
}

void c_speed1() {
  sendlevel(1);
}

void c_speed2() {
  sendlevel(2);
}

void c_speed3() {
  sendlevel(3);
}

void c_speed4() {
  sendlevel(4);
}

void c_speed5() {
  sendlevel(5);
}

void c_speed6() {
  sendlevel(6);
}

void c_speed7() {
  sendlevel(7);
}

void c_speed8() {
  sendlevel(8);
}

void c_speed9() {
  sendlevel(9);
}

void c_mode() {
  sendmode(funmode ? 0 : 1);
  if (!standalone) {
    if (funmode)
      ChangeButtonAppearance(modebutton, -1, "fuN", -1);
    else
      ChangeButtonAppearance(modebutton, -1, "Normal", -1);
  }
}

void textentrykey(struct textentry *t, int keysym, unsigned int mask) {
  int len, keep = 1;

  len = strlen(t->txt);

  if (!(mask & ControlMask) && (mask & Mod1Mask) && (keysym <= 0x80)) {
    mask &= ~Mod1Mask;
    keysym += 128;
  }

  if (mask & ControlMask) {
    if (keysym == 'u' || keysym == 'U') {
      t->txt[0] = 0;
      t->offset = 0;
      t->next_clears = 0;
      RedrawTextEntryArea(t);
    }
  } else if (keysym == XK_Delete || keysym == XK_BackSpace) {
    if (t->next_clears) {
      t->txt[0] = 0;
      t->offset = 0;
      len = 0;
      keep = 0;
    }
    if (len > 0) {
      t->txt[len - 1] = 0;
      if (len < t->offset + 2 && t->offset > 0) {
	t->offset -= t->cols - 3;
	if (t->offset < 0)
	  t->offset = 0;
	keep = 0;
      }
      len--;
    }
    t->next_clears = 0;
    t->cursor = 1;
    if (keep) {
      SetColor(color[EMPTY][1]);
      XFillRectangle(display, win, gc, t->x + FONTWIDTH * (len - t->offset) + 4, t->y + 3, FONTWIDTH + 3, HTEXTENTRIES - 6);
      SetColor(color[TEXT][2]);
      XDrawLine(display, win, gc, t->x + FONTWIDTH * (len - t->offset) + 6, t->y + 4, t->x + FONTWIDTH * (len - t->offset) + 6, t->y + 16);
      XFlush(display);
    } else
      RedrawTextEntryArea(t);
  } else if (keysym == XK_Return) {
    t->next_clears = 1;
    t->callback(t->txt, (mask & ShiftMask ? 1 : 0));
    return;
  } else if ((keysym >= ' ' && keysym <= 0x7f) ||
	     (keysym >= 0xa1 && keysym <= 0xff)) {
    t->align = ALIGN_LEFT;
    if (t->next_clears) {
      t->txt[0] = 0;
      t->offset = 0;
      len = 0;
      keep = 0;
    }
    t->next_clears = 0;
    t->cursor = 1;
    if (len < t->maxlen) {
      t->txt[len] = keysym;
      t->txt[len + 1] = 0;
      if (len - t->offset >= t->cols - 1) {
	t->offset += t->cols - 3;
	keep = 0;
      }
      if (keep) {
	SetColor(color[EMPTY][1]);
	XFillRectangle(display, win, gc, t->x + FONTWIDTH * (len - t->offset) + 4, t->y + 3, FONTWIDTH + 3, HTEXTENTRIES - 6);
	SetColor(color[TEXT][2]);
	XDrawString(display, win, gc, t->x + FONTWIDTH * (len - t->offset) + 4, t->y + FONTHEIGHT + 1, &t->txt[len], 1);
	XDrawLine(display, win, gc, t->x + FONTWIDTH * (len - t->offset + 1) + 6, t->y + 4, t->x + FONTWIDTH * (len - t->offset + 1) + 6, t->y + 16);
	XFlush(display);
      } else
	RedrawTextEntryArea(t);
    }
  }
}

void doevent(XEvent *xev) {
  char kbuf[16];
  KeySym keysym;
  XComposeStatus compose;
  Window root, child;
  int px, py, wx, wy, rx, ry;
  int i;
  unsigned int mask;
  struct button *b;
  struct toggle *t;
  struct textentry *te;
  struct textscroll *ts;
  struct user *u;

  switch(xev->type) {
    case Expose:
      if (xev->xexpose.count == 0)
	redrawall();
      break;
    case ButtonPress:
      XQueryPointer(display, win, &root, &child, &px, &py, &wx, &wy, &mask);
      XTranslateCoordinates(display, win, root, 0, 0, &rx, &ry, &child);
      px -= rx;
      py -= ry;
      for (b=button0; b; b=b->next) {
	if (px >= b->x && px <= b->x + b->w && py >= b->y && 
	    py <= b->y + b->h) {
	  SimulateButtonPress(b);
	  break;
	}
      }
      for (t=tog0; t; t=t->next) {
	if (px >= t->x && px <= t->x + t->w && py >= t->y && 
	    py <= t->y + t->h)  {
	  if (!t->down) {
	    t->callback();
	    ToggleDown(t);
	  }
	  break;
	}
      }
      for (u=me.next; u; u=u->next) {
	if (px >= u->x0 + XNICK && px <= u->x0 + XNICK + WNICK &&
	    py >= YNICK && py <= YNICK + HTEXTENTRIES) {
	  buf[1] = OP_KILL;
	  buf[2] = u->number;
	  sendbuf(3);
	  break;
	}
      }
      break;

    case KeyPress:
      XLookupString((XKeyEvent *)xev, kbuf, 16, &keysym, &compose);
      XQueryPointer(display, win, &root, &child, &px, &py, &wx, &wy, &mask);
      XTranslateCoordinates(display, win, root, 0, 0, &rx, &ry, &child);
      px -= rx;
      py -= ry;

      if (!x_initted)
	return;

      /* this one goes before the text entry areas! */
      if (keysym == warp_key) {
	if (warp_next == 1 && !visiblemsgs)
	  warp_next = 2;
	switch (warp_next) {
	  case 0:
	    XWarpPointer(display, None, win, 0, 0, 0, 0, 3, 3);
	    break;
	  case 1:
	    XWarpPointer(display, None, win, 0, 0, 0, 0, XSCROLL + scrollcols * FONTWIDTH, YMSGENTRY + HTEXTENTRIES - 3);
	    break;
	  case 2:
	    XWarpPointer(display, None, win, 0, 0, 0, 0, XNICK + me.x0 + WNICKCHARS * FONTWIDTH, YNICK + HTEXTENTRIES - 3);
	  break;
	}
	warp_next = (warp_next + 1) % 3;
	break;
      }

      for (te=text0; te; te=te->next) {
	if (px >= te->x && px <= te->x + te->w &&
	    py >= te->y && py <= te->y + te->h) {
	  textentrykey(te, (int)keysym, mask);
	  return;
	}
      }
      for (ts=scroll0; ts; ts=ts->next) {
	if (ts->redirect &&
	    px >= ts->x && px <= ts->x + ts->w &&
	    py >= ts->y && py <= ts->y + ts->h) {
	  textentrykey(ts->redirect, (int)keysym, mask);
	  return;
	}
      }

      if (keysym == quit_key) {
	SimulateButtonPress(quitbutton);
      }

      if (keysym == start_key) {
	SimulateButtonPress(playbutton);
	break;
      }

      if (keysym == pause_key) {
	SimulateButtonPress(pausebutton);
	break;
      }

      if (keysym == bot_key && botbutton != NULL) {
	SimulateButtonPress(botbutton);
	break;
      }

      if (keysym == text_key && textbutton != NULL) {
	SimulateButtonPress(textbutton);
	break;
      }

      if (keysym == mode_key) {
	SimulateButtonPress(modebutton);
	break;
      }

      if (visiblemsgs && keysym == verbose_key) {
	SimulateButtonPress(verbosebutton);
	break;
      }

      if (visiblemsgs && keysym == clear_key) {
	SimulateButtonPress(clearbutton);
	break;
      }

      for (i=0; i<10; i++) {
	if (keysym == level_keys[i]) {
	  sendlevel(i);
	  return;
	}
      }

      if (!playing || paused)
	break;

      if (keysym == keys[5] || 
	  (keysym == XK_Down && (mask&ShiftMask) != 0)) {
	remove(piece, rotation, x, y);
	while(fits(piece, rotation, x, ++y));
	y--;
	put(piece, rotation, x, y, piece);
	refresh();
	XFlush(display);
      } else if (keysym == keys[1] || 
		  (keysym == XK_Left && (mask&ShiftMask) == 0)) {
	remove(piece, rotation, x, y);
	if (fits(piece, rotation, --x, y)) {
	  put(piece, rotation, x, y, piece);
	  refresh();
	  XFlush(display);
	} else put(piece, rotation, ++x, y, piece);
      } else if (keysym == keys[3] || 
		  (keysym == XK_Right && (mask&ShiftMask) == 0)) {
	remove(piece, rotation, x, y);
	if (fits(piece, rotation, ++x, y)) {
	  put(piece, rotation, x, y, piece);
	  refresh();
	  XFlush(display);
	} else put(piece, rotation, --x, y, piece);
      } else if (keysym == keys[2] || 
		 ((keys[2]&0xdf) >= 'A' && (keys[2]&0xdf) <= 'Z' &&
		   keysym == (keys[2]^0x20)) ||
		 (keysym == XK_Up)) {
	remove(piece, rotation, x, y);
	rotation = (rotation + 1) & 3;
	if (fits(piece, rotation, x, y)) {
	  put(piece, rotation, x, y, piece);
	  refresh();
	  XFlush(display);
	} else {
	  rotation = (rotation + 3) & 3;
	  put(piece, rotation, x, y, piece);
	}
      } else if (keysym == keys[0] ||
		 (keysym == XK_Left && (mask&ShiftMask) != 0)) {
	remove(piece, rotation, x, y);
	while (fits(piece, rotation, --x, y));
	x++;
	put(piece, rotation, x, y, piece);
	refresh();
	XFlush(display);
      } else if (keysym == keys[4] ||
		 (keysym == XK_Right && (mask&ShiftMask) != 0)) {
	remove(piece, rotation, x, y);
	while (fits(piece, rotation, ++x, y));
	x--;
	put(piece, rotation, x, y, piece);
	refresh();
	XFlush(display);
      } else if (keysym == down_key || 
		 (keysym == XK_Down && (mask&ShiftMask) == 0)) {
	remove(piece, rotation, x, y);
	if (fits(piece, rotation, x, ++y)) {
	  put(piece, rotation, x, y, piece);
	  refresh();
	  XFlush(display);
	} else put(piece, rotation, x, --y, piece);
      } else if (keysym == togglenext_key) {
	if (shownext) {
	  clearnext();
	  shownext = 0;
	} else {
	  shownext = 1;
	  drawnext();
	}
      }
      break;
  }
}

struct user *finduser(unsigned char c) {
  struct user *u = &me;

  for (; u; u=u->next)
    if (u->number == c)
      return u;
  
  fatal("Protocol error: reference to non-existing user");
  return NULL; /* so that gcc -Wall doesn't complain */
}

#define XFD (ConnectionNumber(display))

void doserverstuff();

#define tvgeq(a, b) ((a).tv_sec > (b).tv_sec ||  \
		      ((a).tv_sec == (b).tv_sec && (a).tv_usec >= (b).tv_usec))

#define tvnormal(a) do { \
		      while ((a).tv_usec >= 1000000) { \
		        (a).tv_usec -= 1000000;  \
			(a).tv_sec++;  \
		      }  \
		    } while(0)


void tvdiff(struct timeval *a, struct timeval *b, struct timeval *r) {
  if (a->tv_usec >= b->tv_usec) {
    r->tv_usec = a->tv_usec - b->tv_usec;
    r->tv_sec = a->tv_sec - b->tv_sec;
  } else {
    r->tv_usec = a->tv_usec + 1000000 - b->tv_usec;
    r->tv_sec = a->tv_sec - b->tv_sec - 1;
  }
}

void dofirsttimer() {
  struct timer *tm;

  tm = timer0;
  timer0 = timer0->next;
  tm->action(&tm->tv, tm->data);
  free(tm);
}

void addtimer(struct timeval *tv, v_vtp action, void *data) {
  struct timer *tm, *ltm = NULL;

  tm = mmalloc(sizeof(struct timer));
  tm->action = action;
  tm->data = data;
  tm->tv = *tv;
  if (!timer0 || tvgeq(timer0->tv, *tv)) {
    tm->next = timer0;
    timer0 = tm;
  } else {
    ltm = timer0;
    while (ltm->next && tvgeq(ltm->next->tv, *tv))
      ltm = ltm->next;
    tm->next = ltm->next;
    ltm->next = tm;
  }
}

void waitfor(int delay) {
/* wait while reading and responding to X events and reading server stuff */
 
  struct timeval tv, till, now;
  int r = 0;
  fd_set rfds;
  XEvent xev;

  interrupt = 0;
  gettimeofday(&now, NULL);
  till = now;
  till.tv_usec += delay;
  tvnormal(till);

  while (XCheckWindowEvent(display, win, ExposureMask|KeyPressMask|ButtonPressMask|StructureNotifyMask, &xev)) {
    doevent(&xev);
    gettimeofday(&now, NULL);
  }

  do {
    gettimeofday(&now, NULL);

    if (timer0) {
      while (timer0) {
	if (tvgeq(now, timer0->tv))
	  dofirsttimer();
	else
	  break;
      }
    }

    if (tvgeq(now, till))
      break;
    else
      tvdiff(&till, &now, &tv);

    if (timer0 && tvgeq(till, timer0->tv))
      tvdiff(&timer0->tv, &now, &tv);

    FD_ZERO(&rfds);
    FD_SET(XFD, &rfds);
    if (!standalone)
      FD_SET(sfd, &rfds);

    r = select(XFD > sfd ? XFD + 1 : sfd + 1, &rfds, NULL, NULL, &tv);
    if (r < 0 && errno != EINTR) {
      perror("select");
      fatal("fatal: select() failed");
    }
    if (r < 0)
      FD_ZERO(&rfds);

    if (FD_ISSET(XFD, &rfds)) {
      while (XCheckWindowEvent(display, win, ExposureMask|KeyPressMask|ButtonPressMask|StructureNotifyMask, &xev))
	doevent(&xev);
    }

    if (FD_ISSET(sfd, &rfds))
      doserverstuff();

    if (timer0) {
      while (timer0) {
	if (tvgeq(now, timer0->tv))
	  dofirsttimer();
	else
	  break;
      }
    }
  } while (!interrupt);
}

void lostfallstep(struct timeval *tm, struct lostfalldata *lfd) {
  struct timeval ntm;
  int i;

  if (lfd->u->haslost) {
    memcpy(&oldpittmp[0][0], &(lfd->u->pit[0][0]), 200*sizeof(int));
    if (lfd->counter < 20) {
      for (i=0; i<10; i++)
	lfd->u->pit[19 - lfd->counter][i] = GRAY;
    } else {
      for (i=0; i<10; i++)
	lfd->u->pit[lfd->counter - 20][i] = EMPTY;
    }  
    refreshuser(lfd->u);
    if (++lfd->counter >= 40)
      free(lfd);
    else {
      ntm = *tm;
      ntm.tv_usec += 50000;
      tvnormal(ntm);
      addtimer(&ntm, (v_vtp)lostfallstep, (void *)lfd);
    }
  } else free(lfd);
}

void handlemessages();

void do_replay(struct user *u) {
  if (nread + u->replaylen > BUFLEN)
    fatal("Internal error: read buffer overflow");
  if (u->replaylen) {
    memcpy(readbuf+nread, u->replaybuf, u->replaylen);
    nread += u->replaylen;
    u->replaylen = 0;
    handlemessages();
  }
}

void flashlinestep(struct timeval *tm, struct flashlinedata *fld) {
  struct timeval ntm;
  int i;

  if (fld->u->flashing) {
    fld->color = (fld->color+1) % 7;
    for (i=0; i<fld->nlines; i++)
      DrawLine(fld->u, fld->color, fld->lines[i]);
    XFlush(display);
    if (++fld->counter < 3) {
      ntm = *tm;
      ntm.tv_usec += 80000;
      tvnormal(ntm);
      addtimer(&ntm, (v_vtp)flashlinestep, (void *)fld);
    } else {
      memcpy(&oldpittmp[0][0], &fld->oldpit, 200*sizeof(int));
      fld->u->flashing = 0;
      refreshuser(fld->u);
      do_replay(fld->u);
      free(fld);
    }
  } else {
    memcpy(&oldpittmp[0][0], &fld->oldpit, 200*sizeof(int));
    fld->u->flashing = 0;
    refreshuser(fld->u);
    do_replay(fld->u);
    free(fld);
  }
}

void falldown(struct user *u) {
  struct lostfalldata *lfd;
  struct timeval tv;

  u->haslost = 1;
  lfd = mmalloc(sizeof(struct lostfalldata));
  lfd->u = u;
  lfd->counter = 0;
  gettimeofday(&tv, NULL);
  tv.tv_usec += 50000;
  tvnormal(tv);
  addtimer(&tv, (v_vtp)lostfallstep, (void *)lfd);
}

int checkstore(struct user *u, int len) {
  unsigned char *s;

  if (u->flashing) {
    if (u->replaylen + len + 4 >= BUFLEN)
      fatal("Internal error: replay buffer overflow");
    s = u->replaybuf + u->replaylen;
    *s++ = 0;
    *s++ = 0;
    *s++ = 0;
    *s++ = len;
    memcpy(s, buf, len);
    u->replaylen += len + 4;
    return 0;
  } else
    return 1;
}

char isyou;

char *getid(unsigned char c) {
  static char rs[32];
  struct user *u = &me;

  sprintf(rs, "(unknown)(%d)", c);
  if (c == 0) {
    strcpy(rs, "the server");
  } else if (c == me.number) {
    strcpy(rs, "you");
    isyou = 1;
  } else {
    isyou = 0;
    for (; u; u=u->next)
      if (u->number == c)
	sprintf(rs, "client %d (%s)", u->number, u->nick);
  }
  return rs;
}

void handlemessages() {
  unsigned int len;
  struct user *u, *v = NULL;
  int i, j, x0 = -1;
  unsigned char *c, *s;
  struct flashlinedata *fld;
  struct timeval tv;
  static char msgbuf[300];

  while (nread >= 4 && nread >= 4 + readbuf[3]) {
    len = readbuf[3];
    if (readbuf[0] || readbuf[1] || readbuf[2])
      fatal("Wrong server line length");
    memcpy(buf, &readbuf[4], len);
    nread -= 4 + len;
    copydown(readbuf, readbuf + 4 + len, nread);

    switch(buf[1]) {
      case OP_YOUARE:
	me.number = buf[0];
	break;

      case OP_NEW:
	for (u=&me; u; u=u->next) {
	  if (u->number == buf[0])
	    fatal("fatal: duplicate player id");
	  else {
	    v = u;
	    if (u->x0 > x0)
	      x0 = u->x0;
	  }
	}
	v->next = u = mmalloc(sizeof(struct user));
	u->number = buf[0];
	u->nick[0] = 0;
	u->next = NULL;
	for (i=0; i<10; i++)
	  for (j=0; j<20; j++)
	    u->pit[j][i] = EMPTY;
	u->x0 = x0 + X_EACH;
	u->haslost = u->flashing = u->replaylen = 0;
	width += X_EACH;

	if (!textbutton)
	  textbutton = DeclareButton(XMSGS, YMSGS, WMSGS, HBTNS, 0, 1, "Text", ALIGN_CENTER, c_msgs);
	
	scrollcols += WSCROLLRESIZE;
	SetTextScrollWidth(msgscroll, scrollcols);
	SetTextEntryWidth(msgentry, scrollcols);

	sprintf(msgbuf, "new client connected (%d)", u->number);
	sysmsg(msgbuf);

	UpdateWinSize();
	redrawall();
	break;
      
      case OP_GONE:
	u = finduser(buf[0]);
	sprintf(msgbuf, "%s disconnected", getid(buf[0]));
	sysmsg(msgbuf);

	if (checkstore(u, len)) {
	  for (v=&me; v; v=v->next) {
	    if (v->x0 > u->x0)
	      v->x0 -= X_EACH;
	    if (v->next == u)
	      v->next = u->next;
	  }
	  width -= X_EACH;

	  if (me.next == NULL && textbutton != NULL) {
	    RemoveButton(textbutton);
	    textbutton = NULL;
	  }

	  scrollcols -= WSCROLLRESIZE;
	  SetTextScrollWidth(msgscroll, scrollcols);
	  SetTextEntryWidth(msgentry, scrollcols);

	  if (visiblemsgs && me.next == NULL) {
	    height -= HSCROLLRESIZE;
	    visiblemsgs = 0;
	  }

	  UpdateWinSize();
	  redrawall();

	  free(u);
	}
	break;

      case OP_BADVERS:
	{
	  static char tmpbuf[128];

	  sprintf(tmpbuf, "Protocol version mismatch: server expects %d.%d.x instead of %d.%d.%d", buf[2], buf[3], PROT_VERS_1, PROT_VERS_2, PROT_VERS_3);
	  fatal(tmpbuf);
	}
	break;
      
      case OP_PLAY:
	s = getid(buf[0]);
	sprintf(msgbuf, "%s start%s game", s, isyou ? "" : "s");
	sysmsg(msgbuf);
	got_play = interrupt = 1;
	break;

      case OP_PAUSE:
	s = getid(buf[0]);
	sprintf(msgbuf, "%s pause%s game", s, isyou ? "" : "s");
	sysmsg(msgbuf);
	paused = interrupt = 1;
	ChangeButtonAppearance(pausebutton, 1, NULL, -1);
	break;

      case OP_CONT:
	s = getid(buf[0]);
	sprintf(msgbuf, "%s continue%s game", s, isyou ? "" : "s");
	sysmsg(msgbuf);
	paused = 0;
	interrupt = 1;
	ChangeButtonAppearance(pausebutton, 0, NULL, -1);
	break;

      case OP_LINES:
	u = finduser(buf[0]);
	sprintf(msgbuf, "%s sends the wrong number of lines (ignored)", getid(buf[0]));
	if (funmode) {
	  switch(buf[2]) {
	    case 1:
	      addsquares++;
	      sprintf(msgbuf, "%s sends you one square", getid(buf[0]));
	      break;
	    case 2:
	      addlines++;
	      sprintf(msgbuf, "%s sends you one line", getid(buf[0]));
	      break;
	    case 3:
	      addlines += 3;
	      sprintf(msgbuf, "%s sends you 3 lines", getid(buf[0]));
	      break;
	    case 4:
	      sprintf(msgbuf, "%s sends you 5 lines", getid(buf[0]));
	      addlines += 5;
	      break;
	  }
	} else {
	  if (buf[2] >= 2 && buf[2] <= 4) {
	    sprintf(msgbuf, "%s sends you %d lines", getid(buf[0]), (int)buf[2]);
	    addlines += buf[2];
	  }
	}
	sysmsg(msgbuf);
	break;

      case OP_LINESTO:
	strcpy(msgbuf, getid(buf[0]));
	strcat(msgbuf, isyou ? " send " : " sends ");
	s = msgbuf + strlen(msgbuf);
	strcpy(s, "the wrong number of lines (ignored)");
	if (funmode) {
	  switch(buf[2]) {
	    case 1:
	      sprintf(s, "one square to %s", getid(buf[3]));
	      break;
	    case 2:
	      sprintf(s, "one line to %s", getid(buf[3]));
	      break;
	    case 3:
	      sprintf(s, "3 lines to %s", getid(buf[3]));
	      break;
	    case 4:
	      sprintf(s, "5 lines to %s", getid(buf[3]));
	      break;
	  }
	} else {
	  if (buf[2] >= 2 && buf[2] <= 4)
	    sprintf(s, "%d lines to %s", (int)buf[2], getid(buf[3]));
	}
	sysmsg(msgbuf);
	break;

      case OP_NICK:
	u = finduser(buf[0]);
	buf[len] = 0;
	sprintf(msgbuf, "%s calls itself %s", getid(buf[0]), &buf[2]);
	sysmsg(msgbuf);
	strncpy(u->nick, &buf[2], MAXNICKLEN);
	DrawButton(1, 1, XNICK + u->x0, YNICK, WNICK, HTEXTENTRIES, u->nick, ALIGN_CENTER, 0);
	XFlush(display);
	break;

      case OP_MSG:
	u = finduser(buf[0]);
	buf[len] = 0;
	msgfrom(u, &buf[2]);
	break;
      
      case OP_DRAW:
	u = finduser(buf[0]);
	if (checkstore(u, len)) {
	  len -= 2;
	  c = &buf[2];
	  while (len >= 3) {
	    DrawSqr(u, c[2], c[0], c[1]);
	    u->pit[c[1]][c[0]] = c[2];
	    c += 3;
	    len -= 3;
	  }
	  if (len != 0)
	    fatal("fatal: crap at the end of an OP_DRAW");
	  XFlush(display);
	}
	break;

      case OP_FALL:
	if (len > 6 && len < 3)
	  fatal("fatal: wrong OP_FALL command");
	u = finduser(buf[0]);

	if (checkstore(u, len)) {
	  fld = mmalloc(sizeof(struct flashlinedata));
	  memcpy(fld->lines, &buf[2], len-2);
	  fld->counter = 0;
	  fld->u = u;
	  fld->nlines = len - 2;
	  fld->color = i = my_rand() % 7;
	  u->flashing = 1;
	  for (j=2; j<len; j++)
	    DrawLine(u, i, buf[j]);
	  XFlush(display);
	  gettimeofday(&tv, NULL);
	  tv.tv_usec += 80000;
	  tvnormal(tv);
	  addtimer(&tv, (v_vtp)flashlinestep, (void *)fld);

	  memcpy(&fld->oldpit[0][0], &(u->pit[0][0]), 200*sizeof(int));
	  for (j=2; j<len; j++) {
	    falline(u, buf[j]);
	    for (i=0; i<10; i++)
	      fld->oldpit[buf[j]][i] |= 0x100;
	  }
	}
	break;
      
      case OP_GROW:
	u = finduser(buf[0]);
	if (checkstore(u, len)) {
	  memcpy(&oldpittmp[0][0], &(u->pit[0][0]), 200*sizeof(int));
	  growline(u);
	  refreshuser(u);
	}
	break;
      
      case OP_LOST:
        u = finduser(buf[0]);
	sprintf(msgbuf, "%s has lost", getid(buf[0]));
	sysmsg(msgbuf);
	if (checkstore(u, len)) {
	  falldown(u);
	}
	break;
      
      case OP_CLEAR:
	u = finduser(buf[0]);
	if (checkstore(u, len)) {
	  for (i=0; i<10; i++)
	    for (j=0; j<20; j++)
	      u->pit[j][i] = EMPTY;
	  SetColor(color[EMPTY][1]);
	  XFillRectangle(display, win, gc, u->x0 + XPIT, YPIT, WPITIN, HPITIN);
	  XFlush(display);
	}
	break;
      
      case OP_LEVEL:
	s = getid(buf[0]);
	sprintf(msgbuf, "%s set%s level %d", s, isyou ? "" : "s", (int)buf[2]);
	sysmsg(msgbuf);
	setlevel(buf[2]);
	break;

      case OP_MODE:
	s = getid(buf[0]);
	sprintf(msgbuf, "%s set%s %s mode", s, isyou ? "" : "s", buf[2] ? "fun" : "normal");
	sysmsg(msgbuf);
	setmode(buf[2]);
	break;
    }
  }
}

void doserverstuff() {
  int r;

  r = read(sfd, readbuf + nread, BUFLEN - nread);
  if (r < 0)
    fatal("Error reading from server");
  if (r == 0)
    fatal("Connection to server lost");
  nread += r;

  handlemessages();
}

void startserver() {
  switch (fork()) {
    case 0:
#ifdef XTRISPATH
      execl(XTRISPATH "/xtserv", "xtserv", "-once", NULL);
#else
      execlp("xtserv", "xtserv", "-once", NULL);
#endif
      fprintf(stderr, "Can't start server '%s'.\n",

#ifdef XTRISPATH
	XTRISPATH "/xtserv");
#else
	"xtserv");
#endif
      _exit(1);
    
    case -1:
      fatal("fork() failed");
    
    default:
      return;
  }
}

void connect2server(char *h) {
  struct hostent *hp;
  struct sockaddr_in s;
  struct protoent *tcpproto;
  int on = 1, i;

  if (h) {
    if ((s.sin_addr.s_addr = inet_addr(h)) == -1) {
      hp = gethostbyname(h);
      if (!hp)
	fatal("Host not found");
      s.sin_addr = *(struct in_addr *)(hp->h_addr_list[0]);
    }
  } else s.sin_addr.s_addr = inet_addr("127.0.0.1");
  s.sin_port = htons(port);
  s.sin_family = AF_INET;
  sfd = socket(PF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    fatal("Out of file descriptors");
  if ((tcpproto = getprotobyname("tcp")) != NULL)
    setsockopt(sfd, tcpproto->p_proto, TCP_NODELAY, (char *)&on, sizeof(int));

  if (connect(sfd, (struct sockaddr *)&s, sizeof(s)) < 0) {
    if (!h) {
      printf("No xtris server on localhost - starting up one ...\n");
      startserver();
      for (i=0; i<6; i++) {
	u_sleep(500000);
	close(sfd);
	sfd = socket(PF_INET, SOCK_STREAM, 0);
	if (sfd < 0)
	  fatal("Out of file descriptors");
	setsockopt(sfd, tcpproto->p_proto, TCP_NODELAY, (char *)&on, sizeof(int));
	if (connect(sfd, (struct sockaddr *)&s, sizeof(s)) >= 0)
	  break;
      }
      if (i==6)
	fatal("Can't connect to server");
    } else fatal("Can't connect to server");
  }
}

void sigchld() {
#ifdef NeXT
  while (wait3(NULL, WNOHANG|WUNTRACED, NULL) > 0);
#else
  while (waitpid(0, NULL, WNOHANG|WUNTRACED) > 0);
#endif
}

int main(int argc, char *argv[]) {
  int i, j;
  char *s;

#ifndef NeXT
  struct sigaction sact;
#endif

  struct user *u;
  struct toggle *t;

#ifdef NeXT
  signal(SIGPIPE, SIG_IGN);
  signal(SIGCHLD, sigchld);
#else
  sact.sa_handler = SIG_IGN;
  sigemptyset(&sact.sa_mask);
  sact.sa_flags = 0;
  sigaction(SIGPIPE, &sact, NULL);

  sact.sa_handler = sigchld;
  sigaction(SIGCHLD, &sact, NULL);
#endif

  while (argc >= 2) {
    if (strcmp(argv[1], "-bw") == 0) {
      argv++;
      argc--;
      bw = 1;
    } else if (strcmp(argv[1], "-h") == 0 ||
	       strcmp(argv[1], "-help") == 0) {
      printf(

"Use: xtris [options] [server.name [port]]\n"
"Options:   -display machine:0   --  set the display\n"
"           -k keys              --  set keys (default is \"JjklL \")\n"
"           -s                   --  standalone mode\n"
"           -bw                  --  force black & white\n"
"           -big                 --  bigger squares\n"
"           -flashy              --  use flashier colors\n"

);

      exit(0);
    } else if (strcmp(argv[1], "-big") == 0) {
      sqrsize = 20;
      argv++;
      argc--;
    } else if (strcmp(argv[1], "-display") == 0) {
      if (argc < 3)
	fatal("Missing argument for -display");
      disp = argv[2];
      argv += 2;
      argc -= 2;
    } else if (strcmp(argv[1], "-s") == 0) {
      standalone = 1;
      argv++;
      argc--;
    } else if (strcmp(argv[1], "-k") == 0) {
      if (argc < 3)
	fatal("Missing argument for -k");
      if (strlen(argv[2]) < 6)
	fatal("List of keys too short: must be 6 characters");
      for (i=0; i<6; i++)
	keys[i] = (KeySym)argv[2][i];
      keysspecified = 1;
      argv += 2;
      argc -= 2;
    } else if (strcmp(argv[1], "-flashy") == 0) {
      argv++;
      argc--;
      flashy = 1;
    } else if (argv[1][0] == '-') {
      fatal("Unrecognized option, try \"xtris -help\"");
    } else if (serverhost == NULL) {
      serverhost = argv[1];
      argv++;
      argc--;
    } else if (port != 0) {
      fatal("Too many arguments, try \"xtris -help\"");
    } else {
      port = atoi(argv[1]);
      if (port < 1024)
	fatal("Bad port number");
      argv++;
      argc--;
    }
  }

  me.number = 0;
  me.x0 = LEFTSIDE;

  if ((s = getenv("XTRISNAME")) || (s = getenv("NETTRISNAME")) || 
      (s = getlogin()))
    strncpy(me.nick, s, MAXNICKLEN);
  else
    strcpy(me.nick, "unknown");

  me.next = NULL;

  if (port == 0)
    port = DEFAULTPORT;

  if (!standalone)
    connect2server(serverhost); /* possibly NULL */

  scrollcols = WSCROLL - WSCROLLRESIZE;

  init_X();

  quitbutton = DeclareButton(XBTNS, YQUIT, WBTNS, HBTNS, 0, 1, "Quit", ALIGN_CENTER, c_quit);
  playbutton = DeclareButton(XBTNS, YPLAY, WBTNS, HBTNS, 0, 1, "Start", ALIGN_CENTER, c_play);
  pausebutton = DeclareButton(XBTNS, YPAUSE, WBTNS, HBTNS, 0, 0, "Pause", ALIGN_CENTER, c_pause);
  modebutton = DeclareButton(XBTNS, YMODE, WBTNS, HBTNS, 0, 1, "Normal", ALIGN_CENTER, c_mode);
  verbosebutton = DeclareButton(XBTNS, YVERBOSE, WBTNS, HBTNS, verbose, 0, "Verbose", ALIGN_CENTER, c_verbose);
  clearbutton = DeclareButton(XBTNS, YCLEAR, WBTNS, HBTNS, 0, 1, "Clear", ALIGN_CENTER, c_clear);

  if (!standalone) {
    botbutton = DeclareButton(XBTNS, YBOT, WBTNS, HBTNS, 0, 1, "Bot", ALIGN_CENTER, c_bot);
  }

  t = leveltogs[5] = DeclareToggle(XLVLS+26, YLVLS+26, WL, HL, "5", ALIGN_CENTER, c_speed5, NULL);
  leveltogs[4] = DeclareToggle(XLVLS, YLVLS+26, WL, HL, "4", ALIGN_CENTER, c_speed4, t);
  leveltogs[6] = DeclareToggle(XLVLS+52, YLVLS+26, WL, HL, "6", ALIGN_CENTER, c_speed6, t);
  leveltogs[1] = DeclareToggle(XLVLS, YLVLS, WL, HL, "1", ALIGN_CENTER, c_speed1, t);
  leveltogs[2] = DeclareToggle(XLVLS+26, YLVLS, WL, HL, "2", ALIGN_CENTER, c_speed2, t);
  leveltogs[3] = DeclareToggle(XLVLS+52, YLVLS, WL, HL, "3", ALIGN_CENTER, c_speed3, t);
  leveltogs[7] = DeclareToggle(XLVLS, YLVLS+52, WL, HL, "7", ALIGN_CENTER, c_speed7, t);
  leveltogs[8] = DeclareToggle(XLVLS+26, YLVLS+52, WL, HL, "8", ALIGN_CENTER, c_speed8, t);
  leveltogs[9] = DeclareToggle(XLVLS+52, YLVLS+52, WL, HL, "9", ALIGN_CENTER, c_speed9, t);

  msgentry = DeclareTextEntry(XMSGENTRY, YMSGENTRY, scrollcols, 240, "", 1, c_msgentry);

  msgscroll = DeclareTextScroll(XSCROLL, YSCROLL, WSCROLL, HSCROLL, color[TEXT][2], msgentry);

  nickentry = DeclareTextEntry(XNICK + me.x0, YNICK, WNICKCHARS, MAXNICKLEN, "", 0, c_setnick);
  SimulateTextEntry(nickentry, me.nick);

  x_initted = 1;

  buf[1] = OP_VERSION;
  buf[2] = PROT_VERS_1;
  buf[3] = PROT_VERS_2;
  buf[4] = PROT_VERS_3;
  sendbuf(5);

  DrawFrame();

  for (i=0; i<20; i++)
    for (j=0; j<10; j++)
      me.pit[i][j] = oldpit[i][j] = EMPTY;
  XFlush(display);

  while(1) {
    playing = 0;
    do {
      waitfor(1000000);
    } while(!got_play);

  start_game:
    buf[1] = OP_CLEAR;
    sendbuf(2);
    got_play = 0;
    addlines = 0;
    addsquares = 0;
    playing = 1;
    for (u=&me; u; u=u->next) {
      u->haslost = 0;
      u->flashing = 0;
      u->replaylen = 0;
    }
    for (i=0; i<20; i++)
      for (j=0; j<10; j++)
	me.pit[i][j] = oldpit[i][j] = EMPTY;
    SetColor(color[EMPTY][1]);
    XFillRectangle(display, win, gc, me.x0 + XPIT, YPIT, WPITIN, HPITIN);
    clearnext();

    if (funmode) {
      for (i=10; i<20; i++) {
	me.pit[i][0] = my_rand() % 7;
	me.pit[i][9] = my_rand() % 7;
      }
      for (i=0; i<24; i++)
	me.pit[15 + my_rand() % 5][my_rand() % 10] = my_rand() % 7;
      refresh();
    }

    XFlush(display);

    next = my_rand() % 7;
    drawnext();
    while (fits(piece = next, rotation = 0, x = 2, y = 3)) {
      clearnext();
      next = my_rand() % 7;
      drawnext();
      while(fits(piece, rotation, x, y)) {
	put(piece, rotation, x, y, piece);
	refresh();
	XFlush(display);
	while (paused)
	  waitfor(1000000);
	waitfor(delays[level-1]);
	while (paused)
	  waitfor(1000000);
	if (got_play)
	  goto start_game;
	remove(piece, rotation, x, y);
	y++;
      }
      put(piece, rotation, x, --y, piece);
      fallines();
      if (!newlines() || !newsquares())
	break;
    }
    buf[1] = OP_LOST;
    sendbuf(2);
    sysmsg("you have lost");
    clearnext();
    falldown(&me);
  }
}

