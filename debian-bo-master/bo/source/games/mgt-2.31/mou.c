/*****************************************************************************
 * PROJECT:  Mouse routines with 'real' graphic cursor in text mode.
 *****************************************************************************
 * MODULE:  MOU.C
 *****************************************************************************
 * DESCRIPTION:
 *   Main file for the mouse routines.
 *
 *****************************************************************************
 * MODIFICATION NOTES:
 *    Date     Version Author Comment
 * 15-Mar-1991	 1.04	 dk   Added function to set mouse position.  From a
 *			      suggestion by Gary Tepel.
 * 17-Jan-1991	 1.02	 ta   Diffs sent to me from Tony Acero for middle
 *			      mouse button support.
 * 10-Jan-1991	 1.01	 dk   Made points variable global throughout module.
 * 07-Jan-1991	 1.00	 dk   Fixed bugs and set up for release to Usenet.
 * 26-Oct-1990	 0.00	 dk   Initial file.
 *****************************************************************************
 *
 * DISCLAIMER:
 *
 * Programmers may incorporate any or all code into their programs,
 * giving proper credit within the source. Publication of the
 * source routines is permitted so long as proper credit is given
 * to Dave Kirsch.
 *
 * Copyright (C) 1990, 1991 by Dave Kirsch.  You may use this program, or
 * code or tables extracted from it, as desired without restriction.
 * I can not and will not be held responsible for any damage caused from
 * the use of this software.
 *
 * Author: a563@mindlink.UUCP  -or-  Zoid@cc.sfu.ca
 *
 *****************************************************************************
 * This source works with Turbo C 2.0 and MSC 6.0 and above.
 *****************************************************************************/

#ifdef __TURBOC__
#pragma inline
#endif

#include <stdio.h>
#include <dos.h>
#include <stdlib.h>
#include <string.h>

#include "mou.h"

#define HEIGHT 16

int vgapresent=0;
word mousehidden = 0;           /* Is the mouse on? Additive flag */
mouseboolean mouseinstalled = FALSE; /* Is the mouse installed? */

volatile word mousex=0, mousey=0; /* Character position of mouse */

#ifndef __TURBOC__
STATIC word _based(_segname("_CODE")) DGroupSeg;
#endif

STATIC volatile word mbufin = 0, mbufout = 0; /* Mouse buffer pointers */

STATIC word mousefreeze = 0;           /* Is mouse frozen in place? */

STATIC MOUINFOREC mbuf[MOUSEBUFFERSIZE]; /* Mouse buffer */

/* Save information for non EGA/VGA */
STATIC word oldword;
STATIC word newword;
STATIC mouseboolean saved = FALSE;
STATIC word oldmx, oldmy;

/* Save information for EGA/VGA displays */
  mouseboolean egavga = FALSE; /* Do we have an EGA/VGA adapter? */
STATIC byte savechars[3][3]; /* The saved characters we overwrote */
STATIC dword mousecursormask[HEIGHT] =  {
  0x00000000L,  /*0000000000000000*/
  0x40000000L,  /*0100000000000000*/
  0x60000000L,  /*0110000000000000*/
  0x70000000L,  /*0111000000000000*/
  0x78000000L,  /*0111100000000000*/
  0x7c000000L,  /*0111110000000000*/
  0x7e000000L,  /*0111111000000000*/
  0x7f000000L,  /*0111111100000000*/
  0x7f800000L,  /*0111111110000000*/
  0x7f000000L,  /*0111111100000000*/
  0x7c000000L,  /*0111110000000000*/
  0x46000000L,  /*0100011000000000*/
  0x06000000L,  /*0000011000000000*/
  0x03000000L,  /*0000001100000000*/
  0x03000000L,  /*0000001100000000*/
  0x00000000L   /*0000000000000000*/
};

STATIC dword mousescreenmask[HEIGHT] =  {
  0x3fffffffL,  /*0011111111111111*/
  0x1fffffffL,  /*0001111111111111*/
  0x0fffffffL,  /*0000111111111111*/
  0x07ffffffL,  /*0000011111111111*/
  0x03ffffffL,  /*0000001111111111*/
  0x01ffffffL,  /*0000000111111111*/
  0x00ffffffL,  /*0000000011111111*/
  0x007fffffL,  /*0000000001111111*/
  0x003fffffL,  /*0000000000111111*/
  0x007fffffL,  /*0000000001111111*/
  0x01ffffffL,  /*0000000111111111*/
  0x10ffffffL,  /*0001000011111111*/
  0xb0ffffffL,  /*1011000011111111*/
  0xf87fffffL,  /*1111100001111111*/
  0xf87fffffL,  /*1111100001111111*/
  0xfcffffffL   /*1111110011111111*/
};

STATIC byte chardefs[32 * 9]; /* 9 character definitons. */
STATIC word mousepx, mousepy; /* Mouse pixel coordinates */

STATIC mouseboolean conditionalhidemouse = FALSE;
STATIC word conx1, cony1, conx2, cony2;

STATIC word vseg; /* Segment of video ram. */
STATIC word mcols, mrows;
STATIC byte savevmode;
STATIC word points;

STATIC word maxx, maxy;

STATIC mouseboolean desqview = FALSE;

#define POKEATTRIB(x, y, a) pokeb(vseg, (y) * (mcols * 2) + ((x) << 1) + 1, a)
#define PEEKATTRIB(x, y)    peekb(vseg, (y) * (mcols * 2) + ((x) << 1) + 1)

#define POINTS *((byte far *) 0x00000485)
#define COLS *((byte far *) 0x0040004AL)
#define ROWS *((byte far *) 0x00400084L)

#define DEFCHAR 0xd0

/*********************************************************************/
/* Mon 07-Jan-1991 - dk                                              */
/*                                                                   */
/*  Plot the cursor on the screen, save background, draw grid, etc.  */
/*                                                                   */
/*********************************************************************/
PRIVATE void LOCAL FAST plotegavgacursor(word func)
{
word off;
word width, height, i, j;
word disp;
word x = 0, y = 0;
static word lsavex = 0, lsavey = 0;

  switch (func) {
    case 0 : /* erase grid, put back save info */
      x = lsavex;
      y = lsavey;
      break;
    case 1 : /* draw grid */
      x = mousex;
      y = mousey;
      break;
    case 2 : /* save grid */
      lsavex = x = mousex;
      lsavey = y = mousey;
      break;
  }

  width = mcols - x;
  if (width > 3)
    width = 3;
  height = mrows - y;
  if (height > 3)
    height = 3;

  off = y * (mcols * 2) + x * 2;
  disp = (mcols * 2) - width * 2;

  switch (func) {
    case 0 : /* erase grid, put back save info */
      for (i = 0; i < height; i++, off += disp)
        for (j = 0; j < width; j++, off += 2)
          pokeb(vseg, off, savechars[i][j]);
      break;
    case 1 : /* draw grid. */
      for (i = 0; i < height; i++, off += disp)
        for (j = 0; j < width; j++, off += 2)
          pokeb(vseg, off, DEFCHAR + i * 3 + j);
      break;
    case 2 : /* save grid. */
      for (i = 0; i < height; i++, off += disp)
        for (j = 0; j < width; j++, off += 2)
          savechars[i][j] = peekb(vseg, off);
      break;
  }
}

PRIVATE void LOCAL FAST drawegavgacursor(void)
{
word off;
word i, j;
word s1, s2, s3;
dword *defs;
dword *masks;
word shift;
dword addmask;

  plotegavgacursor(2); /* Save current grid that is there. */

  /* Time for some assembler.  Program the EGA/VGA Sequencer and Graphics
     Controller for direct access to the character definition tables.
     Then read in the definitions for the characters we are changing, AND
     the screen mask, then OR the cursor mask to them.  Then copy those
     defintions into the location of the mouse cursor defintions
     and set the Sequencer and Graphics Controller back to normal <whew!>.
  */

  /* Program the Sequencer */

  asm pushf; /* Disable interrupts */
  asm cli;
  asm mov dx, 3c4h; /* Sequencer port address */
  asm mov ax, 0704h; /* Sequential addressing */
  asm out dx, ax;

  /* Program the Graphics Controller */
  asm mov dx, 3ceh; /* Graphics Controller port address */
  asm mov ax, 0204h; /* Select map 2 for CPU reads */
  asm out dx, ax;
  asm mov ax, 0005h; /* Disable odd-even addressing */
  asm out dx, ax;
  asm mov ax, 0406h; /* Map starts at A000:0000 (64K mode) */
  asm out dx, ax;
  asm popf;

  /* Ok, now we have direct access to the character defintion tables, copy
     over the defintions for the characters we are changing */

  off = 0;
  for (i = 0; i < 9; i += 3) { /* Grid is three characters high. */
    s1 = ((byte *)savechars)[i    ] * 32;
    s2 = ((byte *)savechars)[i + 1] * 32;
    s3 = ((byte *)savechars)[i + 2] * 32;
    for (j = 0; j < points; j++) {
      off++; /* 4th byte, that we don't need. */
      chardefs[off++] = peekb(0xa000, s3++);
      chardefs[off++] = peekb(0xa000, s2++);
      chardefs[off++] = peekb(0xa000, s1++);
    }
  }

  /* Ok, we've got the defintions for the characters that we are drawing the
     cursor on.  AND the screen mask and OR the cursor mask to them, thereby
     'drawing' the cursor.  Since the cursor is 16 pixels wide and 16 pixels
     high, we have to save a 3 by 3 character grid where the mouse cursor is
     going.  We use dwords (32 bits) to do the bit AND and OR.  This could
     be made alot faster on a 386 by using 32 bit registers. */

  shift = mousepx % 8;
  addmask = 0xff000000L << (8 - shift);

  masks = mousescreenmask;
  defs = ((dword *)chardefs) + mousepy % points;
  for (i = 0; i < HEIGHT; i++)
    *defs++ &= (*masks++ >> shift) | addmask;

  masks = mousecursormask;
  defs = ((dword *)chardefs) + mousepy % points;
  for (i = 0; i < HEIGHT; i++)
    *defs++ |= *masks++ >> shift;

  /* Ok, Everything is setup, now copy the modifed character definitions
     to their new location. */

  asm mov dx, 3c4h; /* Sequencer port address */
  asm mov ax, 0402h; /* CPU writes only to map 2 */
  asm out dx, ax;

  off = 0;
  for (i = 0; i < 9; i += 3) { /* Grid is three characters high. */
    s1 = (DEFCHAR + i    ) * 32;
    s2 = (DEFCHAR + i + 1) * 32;
    s3 = (DEFCHAR + i + 2) * 32;
    for (j = 0; j < points; j++) {
      off++; /* 4th byte, that we don't need. */
      pokeb(0xa000, s3++, chardefs[off++]);
      pokeb(0xa000, s2++, chardefs[off++]);
      pokeb(0xa000, s1++, chardefs[off++]);
    }
  }

  /* Ok, put the Sequencer and Graphics Controller back to normal */

  /* Program the Sequencer */
  asm pushf; /* Disable interrupts */
  asm cli;
  asm mov dx, 3c4h; /* Sequencer port address */
  asm mov ax, 0302h; /* CPU writes to maps 0 and 1 */
  asm out dx, ax;
  asm mov ax, 0304h; /* Odd-even addressing */
  asm out dx, ax;

  /* Program the Graphics Controller */
  asm mov dx, 3ceh; /* Graphics Controller port address */
  asm mov ax, 0004h; /* Select map 0 for CPU reads */
  asm out dx, ax;
  asm mov ax, 1005h; /* Enable odd-even addressing */
  asm out dx, ax;
  asm sub ax, ax;
  asm mov es, ax; /* Segment 0 */
  asm mov ax, 0e06h; /* Map starts at B800:0000 */
  asm mov bl, 7;
  asm cmp es:[49h], bl; /* Get current video mode */
  asm jne notmono;
  asm mov ax, 0806h; /* Map starts at B000:0000 */
notmono:
  asm out dx, ax;
  asm popf;

  /* Ok, now put the bytes on the screen */

  plotegavgacursor(1); /* Plot the new grid on the screen. */
}

/*******************************************************/
/* 27-Oct-1990 - dk                                    */
/*                                                     */
/*  This function checks for the presense of EGA/VGA.  */
/*                                                     */
/*******************************************************/
 mouseboolean isegavga(void)
{
  asm mov ax, 1a00h; /* ROM BIOS video function 1ah -- Read Display Code */
  asm int 10h;
  asm cmp ah, 1ah; /* Is this call supported? */
  asm je checkega; /* Not supported */
  asm cmp bl, 7; /* VGA w/monochrome display? */
  asm je isvga; /* Yup. */
  asm cmp bl, 8; /* VGA w/color display? */
  asm jne checkega; /* Nope */
isvga:
  vgapresent=1;
isega:
  return TRUE; /* EGA/VGA is installed */
checkega:
  asm mov ah, 12h; /* EGA BIOS function */
  asm mov bl, 10h;
  asm int 10h;
  asm cmp bl, 10h; /* Is EGA BIOS present? */
  asm jne isega; /* There is an EGA in the system. */
  return FALSE; /* Not EGA or VGA in system. */
}

/************************************************/
/* 26-Oct-1990 - dk                             */
/*                                              */
/*  Mouse handler -- called from mouse driver.  */
/*                                              */
/************************************************/
PRIVATE void far mousehandler(void)
/* This function is called whenever a button is pressed.  Do not call this
   function directly!!
*/
{
register word conditionmask;

  /* Get our data segment */
  asm push ds
#ifdef __TURBOC__
  asm push ax
  asm mov ax, DGROUP
  asm mov ds, ax
  asm pop ax
#else
  asm mov ds, cs:DGroupSeg
#endif

  asm mov conditionmask,ax

  if (!mousefreeze) {
    /* save mouse info passed to us from driver */
    asm mov mousex, cx
    asm mov mousey, dx
    asm mov mousepx, cx
    asm mov mousepy, dx

    mousex /= 8; /* Characters are 8 pixels wide */
    mousey /= points; /* Scale mousey down */

    /* See if the mouse has moved. */
    if (conditionmask & MOUSEMOVE) {
      if (saved) {
        if (egavga)
          plotegavgacursor(0);
        else
          POKEATTRIB(oldmx, oldmy, oldword);
        saved = FALSE;
      }

      if (!mousehidden && conditionalhidemouse) /* Check to see if we need to hide */
        if (mousex >= conx1 && mousex <= conx2 &&
            mousey >= cony1 && mousey <= cony2) {
          mousehidden++;
          conditionalhidemouse = FALSE;
        }

      if (!mousehidden) {
        if (egavga)
          drawegavgacursor();
        else {
          oldword = PEEKATTRIB(mousex, mousey);
	  asm mov ax, oldword;	/* Prepare to rotate attrib byte */
          asm and al, 0f7h; /* Clear high bit */
          asm mov cl, 4   /* We want to rotate 4 bits */
          asm rol al, cl  /* Rotate it */
	  asm mov newword, ax;

          POKEATTRIB(mousex, mousey, newword); /* Write out new mouse cursor */
        }

        oldmx = mousex;
        oldmy = mousey;
        saved = TRUE;

      }
    }
  }

  /* Now, see if a mouse button was whacked */
  if (conditionmask & ~MOUSEMOVE)
    if (((mbufin + 1) % MOUSEBUFFERSIZE) == mbufout) { /* Buffer full? */
#ifdef __TURBOC__
      sound(1760); /* Make some noise. */
      delay(10);
      nosound();
#endif
    } else {
      mbuf[mbufin].buttonstat = conditionmask & ~MOUSEMOVE;
      mbuf[mbufin].cx = mousex;
      mbuf[mbufin].cy = mousey;
      mbuf[mbufin].shiftstate = peekb(0, 0x417); /* Get shift byte */
      mbufin = (mbufin + 1) % MOUSEBUFFERSIZE;
    }

  asm pop ds;
}

/************************************/
/* 26-Oct-1990 - dk                 */
/*                                  */
/*  Initialize the mouse routines.  */
/*                                  */
/************************************/
void FAST MOUinit(void)
{
extern int graphicalCursor;
byte v;

#ifndef __TURBOC__
  asm mov cs:DGroupSeg,ds   /* save for interrupt handler to use */
#endif

  asm sub ax,ax;    /* Mouse driver function 0 -- reset and detect */
  asm int 33h
  asm mov mouseinstalled, AX;

  if (mouseinstalled) { /* If a mouse is installed then activate driver */

    mousefreeze++; /* Make sure handler doesn't do things, yet */

    asm mov ax,0F00h;
    asm int 10h;
    asm mov v,al;

    if (v == 7) {
      vseg = 0xb000u;
    } else {
      vseg = 0xb800u;
      v = 3;
    }

    if (ROWS == 0) { /* No value, assume 80x25. */
      mrows = 25;
      mcols = 80;
      points = 8;
    } else {
      mrows = ROWS + 1;
      mcols = COLS;
      points = POINTS;
    }

    /* Check to see if we are running in DESQview.  If so, don't try to
       use the 'true' EGA/VGA cursor (DV doesn't like it at ALL). */

    asm mov ax, 2b01h;
    asm mov cx, 4445h;
    asm mov dx, 5351h;
    asm int 21h;

    asm cmp al, 0ffh;
    asm je notdv;

    desqview = TRUE;

notdv:

    /* Do we have an EGA or VGA?  If so, and we are not in monochrome mode
       and we are not in DESQview then setup to draw a 'true' mouse cursor
       on an EGA/VGA */
    egavga = isegavga() && vseg != 0xb000u && !desqview && graphicalCursor;

    if (egavga) {
      /* We are going to use our 'true' mouse cursor and we need pixel
         resolution, not character resolution from the mouse driver
         (In text mode, the mouse driver only returns coordinates in multiples
         of 8, which we don't want.  We want multiples of 1, i.e. pixel
         resolution).  To get the mouse driver to return coordinates in pixel
         resolution, we 'trick' it into thinking it's in graphics mode by
         setting the low memory byte indicating mode to mode 6 (CGA 640x200x2).
         Then we reset the mouse driver.  The mouse driver will get the video
         mode then act as if it was in graphics mode, not text mode. */
      savevmode = peekb(0x40, 0x49);
      pokeb(0x40, 0x49, 6); /* Does this work ?!?!?!?!? */

      /* Reset driver for change in video mode to take effect. */
      asm sub ax,ax
      asm int 33h

      /* Now that we've tricked the mouse driver into a grapics mode thereby
         causing it to give us pixel resolutions, put the old mode back. */
      pokeb(0x40, 0x49, savevmode);
    }

    /* Set up max x and y ranges. */

    maxx = mcols * 8 - 1; /* Pixels horizontally */
    maxy = mrows * points - 1; /* Pixels vertically */

    asm mov dx,maxx     /* Pixels horizontally */
    asm mov ax,7        /* mouse driver function 7 -- set max x range */
    asm sub cx,cx       /* Minimum range */
    asm int 33h

    asm mov dx,maxy     /* Pixels veritcally */
    asm mov ax,8        /* mouse driver function 8 -- set max y range */
    asm sub cx,cx       /* Minimum range */
    asm int 33h

    /* Now install user routine */

    asm mov ax,cs
    asm mov es,ax
    asm mov dx, offset mousehandler
    /* Setup up bits for calling routine */
#ifdef __TURBOC__
    _CX = LEFTBPRESS | LEFTBRELEASE | RIGHTBPRESS | RIGHTBRELEASE | MIDBPRESS |
	  MIDBRELEASE | MOUSEMOVE;
#else
    asm mov cx, LEFTBPRESS | LEFTBRELEASE | RIGHTBPRESS | RIGHTBRELEASE | MIDBPRESS | MIDBRELEASE | MOUSEMOVE;
#endif
    asm mov ax,12       /* Function 12 -- set user routine */
    asm int 33h

    mousex = mousey = mousepx = mousepy = 0;
    asm mov cx,mousepx	 /* xcoord */
    asm mov dx,mousepy	 /* ycoord */
    asm mov ax,4    /* mouse driver function 4 -- set mouse position */
    asm int 33h

    MOUshow(); /* Call it twice just to make sure */

    mousefreeze--; /* Handler can get into business, now */
  }
}

/****************************/
/* 26-Oct-1990 - dk         */
/*                          */
/*  Hide the mouse cursor.  */
/*                          */
/****************************/
void FAST MOUhide(void)
/* This function turns off the mouse cursor, the mouse still responds
   to button presses */
{
  if (!mouseinstalled)
    return;

  mousefreeze++; /* don't have the handler doing weird things */

  mousehidden++; /* indicate it's hidden now */

  if (saved) {
    if (egavga)
      plotegavgacursor(0);
    else
      POKEATTRIB(oldmx, oldmy, oldword);
    saved = FALSE;
  }

  mousefreeze--; /* reactivate handler */
}

/****************************/
/* 26-Oct-1990 - dk         */
/*                          */
/*  Show the mouse cursor.  */
/*                          */
/****************************/
void FAST MOUshow(void)
{
  if (!mouseinstalled)
    return;

  mousefreeze++; /* don't have the handler doing weird things */

  /* Just in case we were in a conditionalhide */
  if (conditionalhidemouse) {
    /* We were about to conditional hide, but we didn't, don't reactive
       mouse cursor. */
    conditionalhidemouse = FALSE;
    mousefreeze--; /* Reactivate handler */
    return;
  }

  if (mousehidden)
    mousehidden--;
  else {
    mousefreeze--; /* Reactivate handler */
    return;  /* It isn't hidden! */
  }

  if (mousehidden) {
    mousefreeze--; /* reactivate handler */
    return; /* still hidden! */
  }

  /* Draw mouse cursor */

  if (egavga)
    drawegavgacursor();
  else {
    oldword = PEEKATTRIB(mousex, mousey);
    asm mov ax, oldword;  /* Prepare to rotate attrib byte */
    asm and al, 0f7h; /* Clear high bit */
    asm mov cl, 4   /* We want to rotate 4 bits */
    asm rol al, cl  /* Rotate it */
    asm mov newword, ax;

    POKEATTRIB(mousex, mousey, newword); /* Write out new mouse cursor */
  }

  oldmx = mousex;
  oldmy = mousey;
  saved = TRUE;

  mousefreeze--; /* Reactivate handler */
}

/*************************************************************/
/* 27-Oct-1990 - dk                                          */
/*                                                           */
/*  Returns true if there is something in the mouse buffer.  */
/*                                                           */
/*************************************************************/
mouseboolean FAST MOUcheck(void)
{
  return mbufin != mbufout;
}

/**************************************************************/
/* 26-Oct-1990 - dk                                           */
/*                                                            */
/*  Take a copy of the mouse event at the head of the queue.  */
/*                                                            */
/**************************************************************/
void FAST MOUpreview(MOUINFOREC *mouinforec)
{
  if (!mouseinstalled)
    return;

  if (mbufin != mbufout) /* if something is in buffer */
    *mouinforec = mbuf[mbufout];
  else {
    /* Nothing to pull, just report mouse position */
    mouinforec -> cx = mousex;
    mouinforec -> cy = mousey;
    mouinforec -> buttonstat = 0;
    mouinforec -> shiftstate = peekb(0, 0x417);
  }
}

/****************************************************************/
/* 26-Oct-1990 - dk                                             */
/*                                                              */
/*  Get (and remove) the mouse event at the head of the queue.  */
/*                                                              */
/****************************************************************/
void FAST MOUget(MOUINFOREC *mouinforec)
{
  if (!mouseinstalled)
    return;

  if (mbufin != mbufout) { /* if something is in buffer */
    if (mouinforec != NULL)
      *mouinforec = mbuf[mbufout];
    mbufout = (mbufout + 1) % MOUSEBUFFERSIZE;
  } else {
    /* Nothing to pull, just report mouse position */
    mouinforec -> cx = mousex;
    mouinforec -> cy = mousey;
    mouinforec -> buttonstat = 0;
    mouinforec -> shiftstate = peekb(0, 0x417);
  }
}

/**************************************/
/* 26-Oct-1990 - dk                   */
/*                                    */
/*  Deinitialize the mouse routines.  */
/*                                    */
/**************************************/
void FAST MOUdeinit(void)
{
  if (!mouseinstalled)
    return;

  MOUhide();

  asm sub ax,ax
  asm int 33h
}

/**************************************************/
/* 26-Oct-1990 - dk                               */
/*                                                */
/*  Returns the bits for the button status info.  */
/*                                                */
/**************************************************/
word FAST MOUbuttonstatus(void)
{
word buts;

  if (!mouseinstalled)
    return 0;

  asm mov ax,3
  asm int 33h
  asm mov buts,bx
  return buts;
}

/************************************************************************/
/* 26-Oct-1990 - dk                                                     */
/*                                                                      */
/*  Hide the mouse *if* it enters a certain screen area, automatically. */
/*                                                                      */
/************************************************************************/
void FAST MOUconditionalhide(int x1, int y1, int x2, int y2)
{
  if (!mouseinstalled)
    return;

  mousefreeze++; /* hold the handler */

  if (mousehidden) {
    mousefreeze--; /* reactivate handler */
    return; /* already hidden! */
  }

  conditionalhidemouse = TRUE;

  x1 -= 2;
  if (x1 < 0)
    x1 = 0;
  y1 -= 2;
  if (y1 < 0)
    y1 = 0;
  x2 += 2;
  y2 += 2;

  conx1 = x1;
  cony1 = y1;
  conx2 = x2;
  cony2 = y2;

  if (mousex >= conx1 && mousex <= conx2 &&
      mousey >= cony1 && mousey <= cony2) {
    conditionalhidemouse = FALSE; /* We've already hidden it */
    MOUhide(); /* turn it off now if it's there. */
  }

  mousefreeze--;
}

/*********************************************************/
/* Fri 15-Mar-1991 - dk 				 */
/*							 */
/*  Set the mouse cursor to a specific screen position.  */
/*							 */
/*********************************************************/
void FAST MOUsetpos(word x, word y)
{
  if (!mouseinstalled)
    return;

  mousefreeze++;

  MOUhide();

  mousex = x;
  mousey = y;
  mousepx = mousex * 8;
  mousepy = mousey * points;
  asm mov cx, mousepx	/* xcoord */
  asm mov dx, mousepy	/* ycoord */
  asm mov ax, 4 	/* mouse driver function 4 -- set mouse position */
  asm int 33h

  MOUshow();

  mousefreeze--;
}
