/*
 * mice.c - mouse definitions for gpm-Linux
 *
 * Copyright 1993        ajh@gec-mrc.co.uk (Andrew Haylett)
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
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

/*
 * This file is part of the mouse server. The information herein
 * is kept aside from the rest of the server to ease fixing mouse-type
 * issues. Each mouse type is expected to fill the `buttons', `dx' and `dy'
 * fields of the Gpm_Event structure and nothing more.
 *
 * Absolute-pointing devices (support by Marc Meis), are expecting to
 * fit `x' and `y' as well. Unfortunately, to do it the window size must
 * be accessed. The global variable "win" is available for that use.
 *
 * The `data' parameter points to a byte-array with event data, as read
 * by the mouse device. The mouse device should return a fixed number of
 * bytes to signal an event, and that exact number is read by the server
 * before calling one of these functions.
 *
 * The conversion function defined here should return 0 on success and -1
 * on failure.
 *
 * Refer to the definition of Gpm_Type to probe further.
 */

#include <termios.h>
#include <fcntl.h>
#include <termios.h>

#include <sys/types.h>
#include <sys/stat.h> /* stat() */
#include <sys/time.h> /* select() */
#include <unistd.h>
#include <linux/fs.h> /* MAJOR */
#include <errno.h>

#include "gpmInt.h"
#include "kmouse.h"     /* protocol id's */

/*========================================================================*/
/*
 * Ok, here we are: first, provide the functions.
 * The return value is the number of unprocessed bytes
 */

static int M_ms(Gpm_Event *state,  unsigned char *data)
{
  /*
   * some devices report a change of middle-button state by
   * repeating the current button state  (patch by Mark Lord)
   */
  static unsigned char prev=0;

#if 1 /* original patch */

  if (data[0] == 0x40 && !(prev|data[1]|data[2]))
    state->buttons = 2;           /* third button on MS compatible mouse */
  else
    state->buttons= ((data[0] & 0x20) >> 3) | ((data[0] & 0x10) >> 4);
  prev = state->buttons;
  state->dx=      (char)(((data[0] & 0x03) << 6) | (data[1] & 0x3F));
  state->dy=      (char)(((data[0] & 0x0C) << 4) | (data[2] & 0x3F));

#else /* my own trial to keep track of all the buttons simultaneously */

  state->buttons= ((data[0] & 0x20) >> 3) | ((data[0] & 0x10) >> 4);
  state->dx=      (char)(((data[0] & 0x03) << 6) | (data[1] & 0x3F));
  state->dy=      (char)(((data[0] & 0x0C) << 4) | (data[2] & 0x3F));

  /* Allow motion *and* button change (Michael Plass) */

  if ((state->dx==0) && (state->dx==0) 
      && (state->buttons == (prev&~GPM_B_MIDDLE)))
    state->buttons = prev^GPM_B_MIDDLE;  /* no move or change: toggle middle */
  else
    state->buttons |= prev&GPM_B_MIDDLE;    /* change: preserve middle */
  prev=state->buttons;


#endif

  return 0;
}

static int M_bare(Gpm_Event *state,  unsigned char *data)
{
  /* a bare ms protocol */
  state->buttons= ((data[0] & 0x20) >> 3) | ((data[0] & 0x10) >> 4);
  state->dx=      (char)(((data[0] & 0x03) << 6) | (data[1] & 0x3F));
  state->dy=      (char)(((data[0] & 0x0C) << 4) | (data[2] & 0x3F));
  return 0;
}

static int M_sun(Gpm_Event *state,  unsigned char *data)
{
  state->buttons= (~data[0]) & 0x07;
  state->dx=      (char)(data[1]);
  state->dy=     -(char)(data[2]);
  return 0;
}

static int M_msc(Gpm_Event *state,  unsigned char *data)
{
  state->buttons= (~data[0]) & 0x07;
  state->dx=      (char)(data[1]) + (char)(data[3]);
  state->dy=     -((char)(data[2]) + (char)(data[4]));
  return 0;
}

static int M_mm(Gpm_Event *state,  unsigned char *data)
{
  state->buttons= data[0] & 0x07;
  state->dx=      (data[0] & 0x10) ?   data[1] : - data[1];
  state->dy=      (data[0] & 0x08) ? - data[2] :   data[2];
  return 0;
}

static int M_logi(Gpm_Event *state,  unsigned char *data) /* equal to mm */
{
  state->buttons= data[0] & 0x07;
  state->dx=      (data[0] & 0x10) ?   data[1] : - data[1];
  state->dy=      (data[0] & 0x08) ? - data[2] :   data[2];
  return 0;
}

static int M_bm(Gpm_Event *state,  unsigned char *data) /* equal to sun */
{
  state->buttons= (~data[0]) & 0x07;
  state->dx=      (char)data[1];
  state->dy=     -(char)data[2];
  return 0;
}

static int M_ps2(Gpm_Event *state,  unsigned char *data)
{
  static int tap_active=0; /* there exist glidepoint ps2 mice */

  state->buttons=
    !!(data[0]&1) * GPM_B_LEFT +
    !!(data[0]&2) * GPM_B_RIGHT +
    !!(data[0]&4) * GPM_B_MIDDLE;

  if (data[0]==0 && opt_glidepoint_tap) /* by default this is false */
    state->buttons = tap_active = opt_glidepoint_tap;
  else if (tap_active)
    if (data[0]==8)
      state->buttons = tap_active = 0;
    else state->buttons = tap_active;

  state->dx=   (data[0] & 0x10) ? data[1]-256 : data[1];
  state->dy= -((data[0] & 0x20) ? data[2]-256 : data[2]);
  return 0;
}

#define GPM_B_BOTH (GPM_B_LEFT|GPM_B_RIGHT)
static int M_mman(Gpm_Event *state,  unsigned char *data)
{
  /*
   * the damned MouseMan has 3/4 bytes packets. The extra byte 
   * is only there if the middle button is active.
   * I get the extra byte as a packet with magic numbers in it.
   * and then switch to 4-byte mode.
   */
  static unsigned char prev=0;
  static Gpm_Type *mytype=mice; /* it is the first */
  unsigned char b;

  if (data[1]==GPM_EXTRA_MAGIC_1 && data[2]==GPM_EXTRA_MAGIC_2)
    {
    /* got unexpected fourth byte */
    LOG(("Extra byte = %02x",*data));
    if ((b=(*data>>4)) > 0x3) return -1;  /* just a sanity check */
    state->dx=state->dy=0;
    
    mytype->packetlen=4;
    mytype->getextra=0;
    }
  else
    {
    /* got 3/4, as expected */
    
    /* motion is independent of packetlen... */
    state->dx= (char)(((data[0] & 0x03) << 6) | (data[1] & 0x3F));
    state->dy= (char)(((data[0] & 0x0C) << 4) | (data[2] & 0x3F));
    
    prev= ((data[0] & 0x20) >> 3) | ((data[0] & 0x10) >> 4);
    if (mytype->packetlen==4) b=data[3]>>4;
    }
	  
  if(mytype->packetlen==4) 
    {
    if(b == 0) 
      {
      mytype->packetlen=3;
      mytype->getextra=1;
      }
    else
      {
      if (b & 0x2) prev |= GPM_B_MIDDLE;
      if (b & 0x1) prev |= opt_glidepoint_tap;
      }
    }
  state->buttons=prev;

  /* This "chord-middle" behaviour was reported by David A. van Leeuwen */
  if ( ((prev^state->buttons) & GPM_B_BOTH)==GPM_B_BOTH )
    state->buttons = state->buttons ? GPM_B_MIDDLE : 0;
  prev=state->buttons;
  
  return 0;
}

/* wacom tablet, in native mode */
static int M_wacom(Gpm_Event *state, unsigned char *data)
{
  static int ox=-1, oy, threshold=8;
  int x, y, z;

  if (data[0]==0xa0) /* leave -- but sometimes it's not decoded */
    { ox=-1; state->buttons=0; state->dx=state->dy=0; return 0;}

  
  x = (data[1]<<7) | (data[2]&0x7f);
  y = (data[4]<<7) | (data[5]&0x7f);
  z = data[6]-64 + 128*(data[6]<64);
  if (ox==-1 || abs(x-ox)>128 || abs(y-oy)>128) { ox=x; oy=y;} /* enter */

  state->buttons = (GPM_B_MIDDLE * !!(data[3]&0x10)); /* the b. means middle */
  if (!state->buttons)
    if (data[3]&0x20) state->buttons = (z>threshold) * GPM_B_RIGHT;
    else              state->buttons = (z>threshold) * GPM_B_LEFT;

  if (z>threshold) threshold=1;
  else threshold=8;

  state->dx = (x-ox)/5; 
  state->dy = (y-oy)/5; /* divide by 5, 'cause it's too much sensitive */

  ox=x; oy=y;
  return 0;
}

/* ncr pen support (Marc Meis) */

#define NCR_LEFT_X     40
#define NCR_RIGHT_X    2000

#define NCR_BOTTOM_Y   25
#define NCR_TOP_Y      1490

#define NCR_DELTA_X    (NCR_RIGHT_X - NCR_LEFT_X)
#define NCR_DELTA_Y    (NCR_TOP_Y - NCR_BOTTOM_Y)

static int M_ncr(Gpm_Event *state,  unsigned char *data)
{
  int x,y;

  state->buttons= (data[0]&1)*GPM_B_LEFT +
                !!(data[0]&2)*GPM_B_RIGHT;

  state->dx = (char)data[1]; /* currently unused */
  state->dy = (char)data[2];

  x = ((int)data[3] << 8) + (int)data[4];
  y = ((int)data[5] << 8) + (int)data[6];

  /* these formulaes may look curious, but this is the way it works!!! */

  state->x = x < NCR_LEFT_X
             ? 0
             : x > NCR_RIGHT_X
               ? win.ws_col+1
               : (long)(x-NCR_LEFT_X) * (long)(win.ws_col-1) / NCR_DELTA_X+2;

  state->y = y < NCR_BOTTOM_Y
             ? win.ws_row + 1
             : y > NCR_TOP_Y
	       ? 0
	       : (long)(NCR_TOP_Y-y) * (long)win.ws_row / NCR_DELTA_Y + 1;

  return 0;
}


/*========================================================================*/
/* Then, mice should be initialized */

static int setspeed(int fd, int old, int new, unsigned short flags)
{
struct termios tty;
char *c;

  tcgetattr(fd, &tty);
    
  tty.c_iflag = IGNBRK | IGNPAR;
  tty.c_oflag = 0;
  tty.c_lflag = 0;
  tty.c_line = 0;
  tty.c_cc[VTIME] = 0;
  tty.c_cc[VMIN] = 1;

  switch (old)
    {
    case 9600:	tty.c_cflag = flags | B9600; break;
    case 4800:	tty.c_cflag = flags | B4800; break;
    case 2400:	tty.c_cflag = flags | B2400; break;
    case 1200:
    default:	tty.c_cflag = flags | B1200; break;
    }

  tcsetattr(fd, TCSAFLUSH, &tty);

  switch (new)
    {
    case 9600:	c = "*q";  tty.c_cflag = flags | B9600; break;
    case 4800:	c = "*p";  tty.c_cflag = flags | B4800; break;
    case 2400:	c = "*o";  tty.c_cflag = flags | B2400; break;
    case 1200:
    default:	c = "*n";  tty.c_cflag = flags | B1200; break;
    }

  write(fd, c, 2);
  usleep(100000);
  tcsetattr(fd, TCSAFLUSH, &tty);
  return 0;
}



static struct {
    int sample; char code[2];
    }
  sampletab[]={
    {  0,"O"},
    { 15,"J"},
    { 27,"K"},
    { 42,"L"},
    { 60,"R"},
    { 85,"M"},
    {125,"Q"},
    {1E9,"N"},
};

static Gpm_Type *I_serial(int fd, unsigned short flags, struct Gpm_Type *type)
{
int i; unsigned char c;
fd_set set; struct timeval timeout={0,0}; /* used when not debugging */


  /* change from any available speed to the chosen one */
  for (i=9600; i>=1200; i/=2)
    setspeed(fd, i, opt_baud, flags);

#if 0 /* actually, I never understood this... */
  /* configure the sample rate */
  for (i=0;opt_sample<=sampletab[i].sample;i++)
    ;
  write(fd,sampletab[i].code,1);
#endif

#ifndef DEBUG
  /*
   * flush any pending input (thanks, Miguel)
   * moreover, damned mouseman's can be detected here (though it isn't)
   */
  FD_ZERO(&set);
  for(i=0; /* always */ ; i++)
	{
	FD_SET(fd,&set);
	switch(select(fd+1,&set,(fd_set *)NULL,(fd_set *)NULL,&timeout/* zero */))
	  {
	  case 1:  if (read(fd,&c,1)==0) break;
	  case -1: continue;
	  }
	break;
	}

  if (type->fun==M_ms && i==2 && c==0x33) /* Aha.. a mouseman... */
	{
	LOG(("MouseMan detected"));
	return mice; /* it is the first */
	}
#endif

  /*
   * reset the MouseMan/TrackMan to use the 3/4 byte protocol
   * (Stephen Lee, sl14@crux1.cit.cornell.edu)
   */
  if (type->fun==M_mman)
	{
	setspeed(fd, 1200, 1200, flags);
	write(fd, "*X", 2);
	setspeed(fd, 1200, opt_baud, flags);
	}

  return type;
}

static Gpm_Type *I_logi(int fd, unsigned short flags, struct Gpm_Type *type)
{
int i;
struct stat buf;
int busmouse;

  /* is this a serial- or a bus- mouse? */
  if (fstat(fd,&buf)==-1) oops("fstat()");
  i=MAJOR(buf.st_rdev);
  if (stat("/dev/ttyS0",&buf)==-1) oops("stat()");
  busmouse=(i != MAJOR(buf.st_rdev));

  /* fix the howmany field, so that serial mice have 1, while busmice have 3 */
  type->howmany = busmouse ? 3 : 1;

  /* change from any available speed to the chosen one */
  for (i=9600; i>=1200; i/=2)
    setspeed(fd, i, opt_baud, flags);

  /* this stuff is peculiar of logitech mice, also for the serial ones */
  write(fd, "S", 1);
  setspeed(fd, opt_baud, opt_baud,CS8 |PARENB |PARODD |CREAD |CLOCAL |HUPCL);

  /* configure the sample rate */
  for (i=0;opt_sample<=sampletab[i].sample;i++)
    ;
  write(fd,sampletab[i].code,1);
  return type;
}

static Gpm_Type *I_wacom(int fd, unsigned short flags, struct Gpm_Type *type)
{
  int i;

  if (opt_baud == 1200) opt_baud=9600; /* default to 9600 */
  /* change from any available speed to the chosen one */
  for (i=9600; i>=1200; i/=2)
    setspeed(fd, i, opt_baud, flags);
  return type;
}


/*========================================================================*/
/* Finally, the table */
#define STD_FLG (CREAD|CLOCAL|HUPCL)

/*
 * Note that mman must be the first, and ms the second (I use this info
 * in mouse-test.c, as a quick and dirty hack
 */

 /*
  * For those who are trying to add a new type, here some docs:
  *
  * The first three strings are the name, an help line, a long name (if any)
  * Then come the function: the decoder and the initializazion function
  * The PROTO_* field is an id label, from kmouse.h, and then come the
  *     flags to pass to the serial port.
  * Follows an array of four bytes: it is the protocol-identification, based
  *     on the first two bytes of a packet: if
  *     "((byte0 & proto[0]) == proto[1]) && ((byte1 & proto[2]) == proto[3])"
  *     then we are at the beginning of a packet.
  * The last numbers are: bytes-per-packet bytes-to-read() does-get-extra-byte
  *     and is-absolute-coordinates. The last two are booleans
  */

Gpm_Type mice[]={
  {"mman", "The \"MouseMan\" and similar devices (3/4 bytes per packet).",
           "Mouseman", M_mman, I_serial, PROTO_MMAN, CS7 | STD_FLG, /* first */
                                {0x40, 0x40, 0x40, 0x00}, 3, 1, 1, 0},
  {"ms",   "The original micro$oft protocol, with a middle-button extension.",
           "", M_ms, I_serial, PROTO_MS, CS7 | STD_FLG,
                                {0x40, 0x40, 0x40, 0x00}, 3, 1, 0, 0},
  {"bare", "The ms protocol, unextended. Necessary for some 2-buttons mice.",
           "Microsoft", M_bare, I_serial, PROTO_BARE, CS7 | STD_FLG,
                                {0x40, 0x40, 0x40, 0x00}, 3, 1, 0, 0},
  {"msc",  "Mouse-Systems-Compatible. Used in most 3-button mice (5-bytes).",
           "MouseSystems", M_msc, I_serial, PROTO_MSC, CS8 | CSTOPB | STD_FLG,
                                {0xf8, 0x80, 0x00, 0x00}, 5, 1, 0, 0},
  {"sun",  "'msc' protocol, but only 3 bytes per packet.",
           "", M_sun, I_serial, PROTO_SUN, CS8 | CSTOPB | STD_FLG,
                                {0xf8, 0x80, 0x00, 0x00}, 3, 1, 0, 0},
  {"mm",   "MM series. Probably an old protocol...",
           "MMSeries", M_mm, I_serial, PROTO_MM, CS8 | PARENB|PARODD | STD_FLG,
                                {0xe0, 0x80, 0x80, 0x00}, 3, 1, 0, 0},
  {"logi", "Used in some Logitech devices (only serial).",
           "Logitech", M_logi, I_logi, PROTO_LOGI, CS8 | CSTOPB | STD_FLG,
                                {0xe0, 0x80, 0x80, 0x00}, 3, 3, 0, 0},
  {"bm",   "Micro$oft busmice and compatible devices.",
           "BusMouse", M_bm, NULL,  PROTO_SUN, STD_FLG, /* bm is sun */
                                {0xf8, 0x80, 0x00, 0x00}, 3, 3, 0, 0},
  {"ps2",  "Busmice of the ps/2 series. Most busmice, actually.",
           "PS/2", M_ps2, NULL,  PROTO_PS2, STD_FLG,
                                {0xc0, 0x00, 0x00, 0x00}, 3, 1, 0, 0},
  {"ncr",  "Ncr3125pen, found on some laptops",
           "", M_ncr, NULL, PROTO_NCR, STD_FLG,
                                {0x08, 0x08, 0x00, 0x00}, 7, 7, 0, 1},
  {"wacom",  "Wacom tablet",
           "", M_wacom, I_wacom, PROTO_WACOM, STD_FLG,
                                {0x80, 0x80, 0x80, 0x00}, 7, 7, 0, 0},

  {"",     "",
           "", NULL, NULL, PROTO_UNKNOWN, 0,
                                {0x00, 0x00, 0x00, 0x00}, 0, 0, 0, 0}
};

/*------------------------------------------------------------------------*/
/* and the help */

int M_listTypes(void)
{
Gpm_Type *type;

 printf("\n" GPM_NAME " " GPM_RELEASE ", " GPM_DATE "\n");
 printf("Available mouse types are:\n\n");
 printf("  name   synonym         description\n\n");
 for (type=mice; type->fun; type++)
   printf("  %-6s %-12s %s\n",type->name, type->syn, type->desc);

 putchar('\n');

return 1; /* to exit() */
}






