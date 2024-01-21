// "utils.h" Misc. utilities. to be included by ALL files.

/*    Copyright (C) 1994,1995,1996  Steve Hardt

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Steve Hardt 
    hardts@mit.edu (valid until Nov. 1996)
    hardts@netscape.com
    hardts@alum.mit.edu
    http://graphics.lcs.mit.edu/~hardts/xevil.html
*/

#ifndef UTILS_H
#define UTILS_H

#ifndef NO_PRAGMAS
#pragma interface
#endif



// Include Files
#ifndef MATH_H_IS_CC
extern "C"
{
#endif
#include <math.h>
#ifndef MATH_H_IS_CC
}
#endif

extern "C"
{
#include <stdlib.h>
#include <time.h> // For seed to srandom and clock().
#include <X11/X.h>
#include <X11/Xlib.h>
}
#include <assert.h>



// Defines
#ifndef MSEC_PER_CLOCK
#define MSEC_PER_CLOCK (1.0e3 / CLOCKS_PER_SEC) 
#endif

#ifndef max
#define max(a,b)               (a<b ? b : a)
#endif
#ifndef min
#define min(a,b)               (a>b ? b : a)
#endif

typedef char Boolean;
typedef unsigned long Pixel;

typedef int ColorNum;



#ifdef RANDOM_NEEDS_PROTOTYPES
extern "C" {
long random();
void srandom(int);
}
#endif 



// Data Structures
#define Xvars_BACKGROUND "light grey"
class Xvars {
public:
  enum {DISPLAYS_MAX = 6,DISPLAY_NAME_LENGTH = 80,
	HUMAN_COLORS_NUM = 6};

  Pixel allocNamedColor(int displayNum,const char *name,
			Pixel def = (Pixel)-1);
  /* EFFECTS: Tries to allocate the named color using the current values
     of the Xvars.  Returns the Pixel if successful.  Otherwise, returns 
     default.  If called with only 2 arguments, default 
     is white.  If black and white display, returns default. */
  
  int dpyMax; // Total number of allocated displays.
  Display *dpy[DISPLAYS_MAX];
  Screen *scr_ptr[DISPLAYS_MAX];
  int scr_num[DISPLAYS_MAX];
  Window root[DISPLAYS_MAX];
  Visual *visual[DISPLAYS_MAX];
  int depth[DISPLAYS_MAX];
  Colormap cmap[DISPLAYS_MAX];
  Pixel white[DISPLAYS_MAX],black[DISPLAYS_MAX]; /* background/foreground. */
  Pixel red[DISPLAYS_MAX],green[DISPLAYS_MAX]; /* Both default to black. */
  Pixel humanColors[DISPLAYS_MAX][HUMAN_COLORS_NUM];
  GC gc[DISPLAYS_MAX]; 
  /* Default gc except foreground is black, background is white, no
     graphics exposures, font is set, stipple is suitable for drawing
     insensitive areas, fill style is FillSolid */
  XFontStruct *font[DISPLAYS_MAX];

  static const char *humanColorNames[HUMAN_COLORS_NUM];
};



class Timer {
 public:
  Timer() {remaining = maxx = 0;}
  // Starts out ready.
  Timer(int t) {assert (t >= 0); maxx = t; remaining = 0;} 
  Boolean ready() {return remaining == 0;}
  void set() {remaining = maxx;}
  void set(int time) {remaining = time;}
  void reset() {remaining = 0;}
  void clock() {if (remaining) remaining--;}

 private:
  int remaining;
  int maxx;
};



class Utils {
 public:
  static inline Boolean coinFlip() 
#ifdef USE_RANDOM
    {return (Boolean)(random() % 2);}
#else
  { return (Boolean)(rand() % 2); }
#endif
  /* EFFECTS: Randomly returns True or False; */

  static inline int choose(int x) 
#ifdef USE_RANDOM
    { 
      assert (x > 0);
      return (int)(random() % x);
    }
#else
  { 
    assert (x > 0);
    return rand() % x;
  }
#endif
  /* EFFECTS: Randomly return a number from 0 to x - 1. */
  
  static void insertionSort(int arry[],int numElements);

  static void randomList(int arry[],int numElements);
  /* EFFECTS: Fills arry the first numElements of arry with the numbers in 
     {0..(numElements-1)} in a random order. */

  static int minimum(int arry[],int size);
  /* EFFECTS: Return the minimum value in the array arry of size size. */

  static int minimum(int arry[],Boolean oks[],int size);
  /* EFFECTS: Return the minimum value in arry which is ok or -1 if none. */
};
#endif
