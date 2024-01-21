/* 
Copyright notice:

This is mine.  I'm only letting you use it.  Period.  Feel free to rip off
any of the code you see fit, but have the courtesy to give me credit.
Otherwise great hairy beasties will rip your eyes out and eat your flesh
when you least expect it.

Jonny Goldman <jonathan@think.com>

Wed May  8 1991
*/

/* score.c -- Print the score. */

#include "vaders.h"

#define SCORELABEL	10
#define SCOREPOS	(SCORELABEL+15)
#define HILABEL		(SCOREPOS+20)
#define HIPOS		(HILABEL+15)

PaintScore()
{
  char scorestring[8];
  XDrawImageString(dpy, labelwindow, scoregc, 0, SCORELABEL, "Score", 5);
  sprintf(scorestring, "%6d ", score);
  XDrawImageString(dpy, labelwindow, scoregc, 0, SCOREPOS, scorestring, 7);
  if (nextbonus && score >= nextbonus) {
    basesleft++;
    ShowBase(basesleft-1, basegc);
    bases = basesleft;
    nextbonus = 0;
  }
  if (score > hiscore)
    hiscore = score;
    sprintf(scorestring, "%6d ", hiscore);
    XDrawImageString(dpy, labelwindow, scoregc, 0, HILABEL, " High", 5);
    XDrawImageString(dpy, labelwindow, scoregc, 0, HIPOS, scorestring, 7);
}

InitScore()
{
    score = 0;
    if (hiscore == 0) hiscore = -1;
    basesleft = 3;
    nextbonus = 1500;
}
