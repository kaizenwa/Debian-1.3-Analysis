#include "aconfig.h"
#ifdef OS2VIO_DRIVER
#define INCL_VIO
#define INCL_KBD
#define INCL_MOU
#include <os2.h>
#include <sys/hw.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "gif.h"
#include "ui.h"
#include "ui_os2viofont.h"

struct ui_driver os2vio_driver;

static int mode = -1;

static int currentbuff = 0;
static int ncolors = 0;
static int width=320, height=200;

static int page_size = 64000, no_pages = 5;

static int buttons = 0, keys = 0;
char *buffers[2];

PVIOPHYSBUF phys_buf;
VIOMODEINFO orig_mode, vio_mode;
PCH ScrPtr;

static HMOU hMou;
static USHORT mouwait = MOU_NOWAIT;
static MOUEVENTINFO mouEvent;
USHORT oldDeviceStatus;

int vio_init()
{
  USHORT mask = 0x7F;
  USHORT status;

  if (MouOpen(NULL,&hMou) && MouOpen("POINTER$", &hMou))
    hMou = 0;

  MouFlushQue(hMou);
  MouSetEventMask(&mask, hMou);

  MouGetDevStatus(&oldDeviceStatus, hMou);

  /* Turn off mouse pointer drawing, BEFORE switching to graphics mode! */
  status = 0x100;
  MouSetDevStatus(&status, hMou);

  orig_mode.cb=sizeof(VIOMODEINFO);
  VioGetMode((PVIOMODEINFO) &orig_mode,(HVIO) 0);

  memcpy((void *) &vio_mode, (const void *) &orig_mode, sizeof(VIOMODEINFO));
  vio_mode.fbType = 3;
  vio_mode.color = 8;
  vio_mode.col = 40;
  vio_mode.row = 25;
  vio_mode.hres = width;
  vio_mode.vres = height;
  vio_mode.fmt_ID = 0;
  vio_mode.attrib = 0;

  if (VioSetMode(&vio_mode, (HVIO) 0)) {
    printf("VioSetMode error\n");
    return 0;
  }

  phys_buf = malloc(sizeof(VIOPHYSBUF));
  phys_buf->pBuf=(unsigned char *) 0xA0000;
  phys_buf->cb=page_size;
  if (VioGetPhysBuf(phys_buf, 0)) {
    printf("VioGetPhysBuf error\n");
    return 0;
  }

  ScrPtr=MAKEP(phys_buf->asel[0],0);
  memset(ScrPtr, 0, page_size);

  return 1;
}

static void vio_get_size(int *wi, int *he)
{
  *wi = width; *he = height;
}

#define ABS(a) (((a)<0) ? -(a) : (a))

static int vga_drawline(int x1, int y1, int x2, int y2, int c)
{
  int dx = x2 - x1;
  int dy = y2 - y1;
  int ax = ABS(dx) << 1;
  int ay = ABS(dy) << 1;
  int sx = (dx >= 0) ? 1 : -1;
  int sy = (dy >= 0) ? 1 : -1;
  
  int x = x1;
  int y = y1;
  
  if (ax > ay) {
    int d = ay - (ax >> 1);
    while (x != x2) {
      ScrPtr[y * width + x] = c;
      
      if (d > 0 || (d == 0 && sx == 1)) {
	y += sy;
	d -= ax;
      }
      x += sx;
      d += ay;
    }
  } else {
    int d = ax - (ay >> 1);
    while (y != y2) {
      ScrPtr[y * width + x] = c;
      
      if (d > 0 || (d == 0 && sy == 1)) {
	x += sx;
	d -= ay;
      }
      y += sy;
      d += ax;
    }
  }
  ScrPtr[y * width + x] = c;
  
  return 0;
  
}

static void draw_mouse(int x, int y, int clear)
{
  PTRLOC ptrLoc;
  static int oldx, oldy;
  int buffstart, i;
  char status;

  VioScrLock(1, &status, (HVIO) 0);
  if (x < 3)
    x = 3;
  if (y < 3)
    y = 3;
  if (x > width - 5)
    x = width - 5;
  if (y > height - 5)
    y = height - 5;

  if ((oldx != x || oldy != y) && clear) {
    /* copy rectangle over mouse pointer */
    buffstart = (oldy - 3) * width + (oldx - 3);
    for (i = 0; i < 8; i++) {
      memcpy((PCH)(ScrPtr+buffstart),
	     (char *)(buffstart+buffers[currentbuff]), 8);
      buffstart += width;
    }
  }

  vga_drawline(x - 3, y - 3, x + 3, y + 3, 255);
  vga_drawline(x - 3, y + 3, x + 3, y - 3, 255);
  vga_drawline(x + 1 - 3, y - 3, x + 1 + 3, y + 3, 0);
  vga_drawline(x + 1 - 3, y + 3, x + 1 + 3, y - 3, 0);

  oldx = x;
  oldy = y;
  VioScrUnLock((HVIO)0);
}

/* check for cursor keys
 *
 * returns status
 */
static int check_ckeys(char c)
{
  if (c == 75) { /* left */
    keys |= 1;
  } else {
    keys &= ~1;
  }
  if (c == 77) { /* right */
    keys |= 2;
  } else {
    keys &= ~2;
  }
  if (c == 72) { /* up */
    keys |= 4;
  } else {
    keys &= ~4;
  }
  if (c == 80) { /* down */
    keys |= 8;
  } else {
    keys &= ~8;
  }
  return keys;
}

static int check_keys(char c)
{
  return 1;  /* not handled */
}

static void check_buttons(USHORT b)
{
  buttons = 0;
  if (b == 1) {
    buttons = 0;
    return;
  }

  if (b & 0x06) {
    buttons |= BUTTON1;
  } else {
    buttons &= ~BUTTON1;
  };
  if (b & 0x18) {
    buttons |= BUTTON2;
  } else {
    buttons &= ~BUTTON2;
  };
  if (b & 0x60) {
    buttons |= BUTTON3;
  } else {
    buttons &= ~BUTTON3;
  };

}

static void vio_processevents(int wait, int *x, int *y, int *b, int *k)
{
  KBDKEYINFO    kbdkeyinfo;
  PTRLOC ptrLoc;

  *k = 0;
  if (wait) {
    while(1) {

      /* 'Reset' params */
      MouGetPtrPos(&ptrLoc, hMou);
      *x = ptrLoc.col; *y = ptrLoc.row;
      *b = buttons;

      /* check for key events */
      KbdCharIn(&kbdkeyinfo, IO_NOWAIT, 0);
      if (kbdkeyinfo.fbStatus!=0) {
	if (check_keys(kbdkeyinfo.chChar))
	  ui_key(kbdkeyinfo.chChar);
        *k = check_ckeys(kbdkeyinfo.chScan);
        return;
      }

      /* check for mouse events */
      MouReadEventQue(&mouEvent, &mouwait, hMou);
      if (!(mouEvent.fs==0 && mouEvent.row==0 && mouEvent.col==0)) {
        MouGetPtrPos(&ptrLoc, hMou);
        *x = ptrLoc.col; *y = ptrLoc.row;
        check_buttons(mouEvent.fs);
        *b = buttons;
        draw_mouse(ptrLoc.col, ptrLoc.row, 1);
        return;
      }
      if (buttons!=0) {
        *b = buttons;
        return;
      }
      DosSleep(1);
    }
  } else {
    KbdCharIn(&kbdkeyinfo, IO_NOWAIT, 0);
    if (kbdkeyinfo.fbStatus!=0) {
      if (check_keys(kbdkeyinfo.chChar))
        ui_key(kbdkeyinfo.chChar);
      *k = check_ckeys(kbdkeyinfo.chScan);
    }
    MouReadEventQue(&mouEvent, &mouwait, hMou);
    if (!(mouEvent.fs==0 && mouEvent.row==0 && mouEvent.col==0)) {
      MouGetPtrPos(&ptrLoc, hMou);
      *x = ptrLoc.col; *y = ptrLoc.row;
      check_buttons(mouEvent.fs);
      *b = buttons;
      draw_mouse(ptrLoc.col, ptrLoc.row, 1);
    }
  }

  return;
}

static void vio_getmouse(int *x, int *y, int *b)
{
  PTRLOC ptrLoc;

  MouGetPtrPos(&ptrLoc, hMou);
  *x = ptrLoc.col; *y = ptrLoc.row;
  check_buttons(mouEvent.fs);
  *b = buttons;
  draw_mouse(ptrLoc.col, ptrLoc.row, 1);
}

static void vio_uninitialise()
{
  MouSetDevStatus(&oldDeviceStatus, hMou);
  MouClose(hMou);
  VioSetMode(&orig_mode, (HVIO) 0);
}

static void vio_rotate(int direction) {
    int i;
    zoom_context *c;
    c=ui_getcontext();
    for(i=1;i<=c->num_colors;i++) {
	  _portaccess(0x03c8, 0x03c9);
	  _outp8(0x03c8, i);
  	_outp8(0x03c9, c->cmap[0][i] / 4);
  	_outp8(0x03c9, c->cmap[1][i] / 4);
  	_outp8(0x03c9, c->cmap[2][i] / 4);
    }
}

static int vio_set_color(int r, int g, int b, int init) 
{
  if(init)
    ncolors = 1;
  if(ncolors == 255)
    return (-1);
  _portaccess(0x03c8, 0x03c9);
  _outp8(0x03c8, ncolors);
  _outp8(0x03c9, r/4);
  _outp8(0x03c9, g/4);
  _outp8(0x03c9, b/4);
  return (ncolors++);
}

static void vio_print(int x, int y, char *text)
{
  char c, t, status;
  int i, idx, cx, cy;

  if (y > height - os2vio_driver.textheight)
    return;

  VioScrLock(1, &status, (HVIO) 0);
  while((c=*text++)!='\0') {
    cy = y / os2vio_driver.textheight * 8;
    if (cy >= 200) {
	VioScrUnLock((HVIO)0);
	return;
      }
    for (i=0; i < os2vio_driver.textheight; i++) {
      cx = x * 8;
      if (cx >= 320) {
	VioScrUnLock((HVIO)0);
	return;
      }
      t = font8[c*8+i];
      for (idx = 0; idx < 8; idx++) {
	if (t & 0x80)
	  ScrPtr[cy*width+cx++] = 255;
	else
	  ScrPtr[cy*width+cx++] = 0;
	t = t << 1;
      }
      cy++;
    }
    x++;
  }	
  VioScrUnLock((HVIO)0);
}

static void vio_display()
{
  PTRLOC ptrLoc;
  char status;

  /* lock screen, if possible */
  VioScrLock(1, &status, (HVIO) 0);
  memcpy(ScrPtr, buffers[currentbuff], page_size);

  MouGetPtrPos(&ptrLoc, hMou);
  draw_mouse(ptrLoc.col, ptrLoc.row, 0);
  VioScrUnLock((HVIO)0);
}

int vio_alloc(char **b1, char **b2)
{
  *b1 = buffers[0] = (char *)malloc(width*height);
  *b2 = buffers[1] = (char *)malloc(width*height);
  currentbuff = 0;

  return(width);
}

void vio_free(char *b1, char *b2) 
{
  free(buffers[0]);
  free(buffers[1]);
}

static void vio_flip_buffers() 
{
  currentbuff ^= 1;
}

static char *helptext[] =
{
  "OS/2 VIO DRIVER VERSION 1.0            ", 
  "=========================              ", 
  " I really like this driver, because    ",
  " I much prefer full screen zooming     ",
  " instead of small 320x320 window on    ",
  " the OS/2 Desktop.                     ",
  " This XaoS driver is for OS/2 VIO mode ",
  " and is fully featured.                ",
  " The following problems can ocour:     ",
  " o It doesn't start                    ",
  "    Thats can not be true. You can see ",
  "    this text!                         ",
  "                                       ",
  " As can be seen, I managed to get the  ",
  " MouAPI working in VIO fullscreen...   ",
  " What a fight is was..                 ",
  "                                       ", 
  "      OS/2 VIO driver was done by      ", 
  "      Thomas A. K. Kjaer  (C) 1996     ", 
  "      takjaer@daimi.aau.dk             "
};

#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
  {NULL, 0, NULL, NULL}
};

struct ui_driver os2vio_driver = 
{
  "OS2VIO", 
  vio_init, 
  vio_get_size, 
  vio_processevents, 
  vio_getmouse, 
  vio_uninitialise, 
  vio_set_color, 
  vio_print, 
  vio_display, 
  vio_alloc, 
  vio_free, 
  vio_flip_buffers, 
  vio_rotate,
  256, 
  8, 
  helptext, 
  UGLYTEXTSIZE, 
  params,
  FULLSCREEN | UPDATE_AFTER_RESIZE | PALETTE_ROTATION | ROTATE_INSIDE_CALCULATION
};

#endif
