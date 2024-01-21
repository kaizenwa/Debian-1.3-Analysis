
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef FONT5X5_H
#define FONT5X5_H

#include "thrust_types.h"

extern byte font[256][6];

#define gcenter(y,s) (printgs(160-(gstrlen(s)>>1), (y), (s)))

extern byte chcolor, chpaper, chflag;

#ifdef __STDC__
void printgc(int x, int y, byte ch);
int printgs(int x, int y, char *string);
int readgs(int x, int y, char string[], int maxc, int maxp, char flag);
int gstrlen(char *string);
#endif

#endif /* FONT5X5_H */
