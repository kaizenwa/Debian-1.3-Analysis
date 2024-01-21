
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef INIT_H
#define INIT_H

#include "thrust_types.h"

#ifdef __STDC__
void turnship(void);
void makeshieldedship(void);
void makefuelmap(byte *fuelmap);
int initmem(void);
void inithardware(int argc, char **argv);
void initscreen(int round);
void initgame(int round, int reset, int xblock, int yblock);
int initsoundIt(void);
void restoresoundIt(void);
void restorehardware(void);
void restoremem(void);
#endif

#endif /* INIT_H */
