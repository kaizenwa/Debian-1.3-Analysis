/*
 * XLife Copyright 1989 Jon Bennett jb7m+@andrew.cmu.edu, jcrb@cs.cmu.edu
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#ifdef __GNUCC__
; /* bogus semi-colon to appease the GNU god */
#endif __GNUCC__
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>

#ifndef SMALLFONT
#define NORMALFONT "9x15"
#define BOLDFONT "9x15bold"
#define FONTHEIGHT 15
#define FONTWIDTH 9
#else
#define NORMALFONT "8x13"
#define BOLDFONT "8x13bold"
#define FONTHEIGHT 13
#define FONTWIDTH 8
#endif

#define BORDERWIDTH 0

#define MAINWIN 0

#define LIFEWIN 1	/* the viewport on the Life universe */

#define INPUTWIN 2	/* the command-input window */
#define INPUTXOFF 2
#define INPUTH 20
#define INPUTYOFF FONTHEIGHT
#define INPUTLEN 125
#define INPUTTEXTLEN (INPUTLEN - INPUTFROMLEN)
#define ICOORDS(x,y) (INPUTXOFF + (x) * FONTWIDTH), (INPUTYOFF + (y) * FONTHEIGHT)

#define RULEWIN	3	/* the rule window */
#define RULEXOFF 2
#define RULEYOFF FONTHEIGHT
#define CWRULES(x,y) (RULEXOFF + (x) * FONTWIDTH), (RULEYOFF + (y) * FONTHEIGHT)
#define RULEW (FONTWIDTH * 16)

#define COORDWIN 4	/* the coordinate-display window */
#define COORDXOFF 2
#define COORDYOFF FONTHEIGHT
#define CWCOORDS(x,y) (COORDXOFF + (x) * FONTWIDTH), (COORDYOFF + (y) * FONTHEIGHT)
#define COORDW 120

#define HELPWIN 5	/* the help window */

/* mouse coordinates to life-universe coordinates */
#define XPOS(x,xpos) (((unsigned)(x) >> (scale - 1)) + xpos) 
#define YPOS(y,ypos) (((unsigned)(y) >> (scale - 1)) + ypos)

/* life-universe coordinates to mouse coordinates */
#define RXPOS(x,xpos) (((x) - xpos) << (scale - 1))
#define RYPOS(y,ypos) (((y) - ypos) << (scale - 1))

/* mouse coordinates to cell coords (relative to top left of screen) */
#define MOUSE2CELL(n)	((n) & (0xffffffff << (scale-1)))

/* this magic number is roughly halfway through the range of unsigned long */
#define STARTX 2100000000
#define STARTY 2100000000

/* the program's run states */
#define STOP 	0x0	/* single-stepping */
#define HIDE	0x1	/* evolving, no per-generation display */
#define RUN	0x2	/* evolving with per-generation display */

/* time-delays for generation evolution */
#define DELAY_FAST 0
#define DELAY_MED 250
#define DELAY_SLOW 500

#define DEADTIME	10	/* nuke a tile when it's been dead this long */

#define PATCH_LOG	"new.transitions"

#define LOADEXT		".l"
#define LOADEXTLEN	2

typedef int	bool;
#define TRUE	1
#define FALSE	0

/*
 * If you don't want n-state capability, define this to 1 in the Makefile
 * to save some storage and reduce the program size.
 */
#ifndef STATEBITS
#define STATEBITS	3
#endif /* STATEBITS */

#define MAXSTATES	(1<<STATEBITS)
#define BADSTATE	0xff	/* out-of-band cell-state value */

#define is_state(c)	isdigit(c)	/* is char a valid state indicator? */
#define stoi(c)		((c) - '0')	/* state character to int */
#define itos(n)		('0' + (n))	/* int to state character */

#if STATEBITS <= 8
typedef unsigned char	cell_t;		/* allows 256 states per cell */
#elif STATEBITS <= 16
typedef short	cell_t;			/* allows 65536 states per cell */
#else
typedef int	cell_t;			/* are you serious? */
#endif

/*
 * These typedefs might change on 64-bit machines like the DEC Alpha to
 * support *really large* universes.  Ideally, cellcount_t should be
 * twice the length of coord_t, since it might hold coord_t's max squared.
 * It's hardly likely it will ever go that high, though...
 */
typedef unsigned long	coord_t;	/* hold world coordinates */
typedef unsigned long	cellcount_t;	/* hold a cell count */

#define PATNAMESIZ 100 /* It's an arbitrary limit, but probably enough */

typedef struct lq
{
    long loadtime;
    char patname[PATNAMESIZ];
    int relpath;
    coord_t hotx,hoty;
    int rxx,rxy,ryx,ryy;
    struct lq  *next;
}
LoadReq;

/* definitions for geometric transformations */ 

/* Get around "%" bogosity, and compute mod *correctly*. */
#define mod(m,n) (((m)>=0)?((m)%(n)):((n)-1-(-(m)+(n)-1)%(n)))

#define max(x, y)	(((x) > (y)) ? (x) : (y))
#define min(x, y)	(((x) > (y)) ? (y) : (x))
#define abs(x)	  	(((x) > (y)) ? ((x) - (y)) : ((y) - (x)))

/* compute coordinate transform for rotations */
/* yes, macros are equivalent, but this makes the meaning clearer (maybe) */
#define tx(x,y,rxx,rxy) ((rxx)*(x)+(rxy)*(y))
#define ty(x,y,ryx,ryy) ((ryx)*(x)+(ryy)*(y))

#ifdef SVR4
#define srandom(n)	srand48(n);
#define random()	lrand48()
#endif

#ifndef GLOBAL
#define GLOBAL extern
#endif /* GLOBAL */

#include "data.h"
