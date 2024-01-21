/*
 * gpm-xterm.h - pseudo client for non-Linux xterm only mouse support.
 *               This code has been extracted from libgpm-0.18 and then
 *               took its own way.
 *
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
 * Copyright 1994        miguel@roxanne.nuclecu.unam.mx (Miguel de Icaza)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#ifndef _GPM_XTERM_H_
#define _GPM_XTERM_H_

/*....................................... Xtermish stuff */
#define GPM_XTERM_ON \
  printf("%c[?1001s",27), fflush(stdout), /* save old hilit tracking */ \
  printf("%c[?1000h",27), fflush(stdout) /* enable mouse tracking */

#define GPM_XTERM_OFF \
  printf("%c[?10001",27), fflush(stdout), /* disable mouse tracking */ \
  printf("%c[?1001r",27), fflush(stdout) /* restore old hilittracking */

/*....................................... Cfg buttons */

#define GPM_B_LEFT      4
#define GPM_B_MIDDLE    2
#define GPM_B_RIGHT     1

/*....................................... The event types */

enum Gpm_Etype {
  GPM_MOVE=1,
  GPM_DRAG=2,   /* exactly one in four is active at a time */
  GPM_DOWN=4,
  GPM_UP=  8,


#define GPM_BARE_EVENTS(type) ((type)&(0xF|GPM_ENTER|GPM_LEAVE))

  GPM_SINGLE=16,            /* at most one in three is set */
  GPM_DOUBLE=32,
  GPM_TRIPLE=64,            /* WARNING: I depend on the values */

  GPM_MFLAG=128,            /* motion during click? */
  GPM_HARD=256,             /* if set in the defaultMask, force an already
			       used event to pass over to another handler */
  GPM_ENTER=512,            /* enter event, user in Roi's */
  GPM_LEAVE=1024            /* leave event, used in Roi's */
};

/*....................................... The event data structure */

enum Gpm_Margin {GPM_TOP=1, GPM_BOT=2, GPM_LFT=4, GPM_RGT=8};


typedef struct Gpm_Event {
  unsigned char buttons, modifiers;  /* try to be a multiple of 4 */
  unsigned short vc;
  short dx, dy, x, y;
  enum Gpm_Etype type;
  int clicks;
  enum Gpm_Margin margin;
}              Gpm_Event;

/*....................................... The connection data structure */

#define GPM_MAGIC 0x47706D4C /* "GpmL" */
typedef struct Gpm_Connect {
  unsigned short eventMask, defaultMask;
  unsigned short minMod, maxMod;
  int pid;
  int vc;
}              Gpm_Connect;


/*....................................... Global variables for the client */

extern int gpm_flag, gpm_ctlfd, gpm_fd, gpm_hflag, gpm_morekeys;

typedef int Gpm_Handler(Gpm_Event *event, void *clientdata);

extern Gpm_Handler *gpm_handler;
extern void *gpm_data;

extern int Gpm_Open(Gpm_Connect *, int);
extern int Gpm_Close(void);
extern int Gpm_Getc(FILE *);
#define    Gpm_Getchar() Gpm_Getc(stdin)
extern int Gpm_Repeat(int millisec);

/* #include <curses.h> Hmm... risky... */

extern int Gpm_Wgetch();
#define Gpm_Getch() (Gpm_Wgetch(NULL))

/* disable the functions available in libgpm but not here */
#define    Gpm_FitValuesM(x, y, margin)
#define    Gpm_FitValues(x,y)
#define    Gpm_FitEvent(ePtr)

#define Gpm_DrawPointer(x,y,fd)
#define GPM_DRAWPOINTER(ePtr)

/* This material comes from gpmCfg.h */
#define SELECT_TIME 60


#endif /* _GPM_XTERM_H_ */
