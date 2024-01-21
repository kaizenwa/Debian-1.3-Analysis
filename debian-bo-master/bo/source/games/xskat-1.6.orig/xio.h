
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#undef EXTERN
#ifdef XIO_C
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN char spnames[3][2][10];
EXTERN int charw[3],charh[3];
EXTERN int lost[3];
EXTERN Display *dpy[3];
EXTERN Colormap cmap[3];
EXTERN Window win[3];
EXTERN unsigned long bpix[3],wpix[3],rpix[3],fgpix[3],bgpix[3],btpix[3];
EXTERN GC gc[3],gcbck[3],gcxor[3];
EXTERN XFontStruct *dfont[3];
EXTERN Pixmap bwcards[3],colcards[3],bck[3];
EXTERN Cursor cursor[3][2];
EXTERN struct
{
  int x,y,w,h;
  int col[3],plan[3];
  int com1x,com2x,com1y[3],com2y[3];
  int playx,playy;
  int skatx,skaty;
  int stichx,stichy;
  int cbox1x,cbox2x,cboxy;
  int pboxx,pboxy;
  int cardx;
} desk;
EXTERN XColor color[16]
#ifdef XIO_C
=
{
  {0,0xffff,0xffff,0xffff},
  {0,0xffff,0x0000,0x0000},
  {0,0x0000,0xffff,0x0000},
  {0,0xffff,0xffff,0x0000},
  {0,0x0000,0x0000,0xffff},
  {0,0xffff,0xd999,0xd999},
  {0,0x0000,0x0000,0x0000},
  {0,0xcccc,0xcccc,0xcccc},
  {0,0x8000,0x8000,0x8000},
  {0,0x8000,0x0000,0x0000},
  {0,0x0000,0x8000,0x0000},
  {0,0x8000,0x8000,0x0000},
  {0,0x0000,0x0000,0x8000},
  {0,0x8000,0x0000,0x8000},
  {0,0x0000,0x8000,0x8000},
  {0,0x0000,0x0000,0x0000}
}
#endif
;

EXTERN int cnts[]
#ifdef XIO_C
=
{
  0,2,22,26,30,34,52,68,82
}
#endif
;

EXTERN int bigs[]
#ifdef XIO_C
=
{
  24,39,
  12,8, 36,8, 24,18, 12,29, 36,29, 12,49, 36,49, 24,60, 12,70, 36,70,
  12,8, 36,70,
  12,8, 36,70,
  9,12, 41,66,
  12,8, 36,8, 12,29, 36,29, 24,39, 12,49, 36,49, 12,70, 36,70,
  12,8, 36,8, 24,23, 12,39, 36,39, 24,55, 12,70, 36,70,
  12,8, 36,8, 24,23, 12,39, 36,39, 12,70, 36,70
}
#endif
;

EXTERN int smls[]
#ifdef XIO_C
=
{
  5,11, 52,11, 5,76, 52,76
}
#endif
;

EXTERN int smlc[]
#ifdef XIO_C
=
{
  5,4, 51,4, 5,84, 51,84
}
#endif
;
