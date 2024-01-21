
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef GRAPHICS_H
#define GRAPHICS_H

#include "fast_gr.h"

#define maxlenx ((word)BBILDX*12) /*max 48 skärmar a 41*23 tecken a */
#define maxleny ((word)BBILDY*4) /* 8*8 pixels a 8 bitar, dvs. 3274752 bytes.*/

#ifdef __STDC__
void writeblock(word bx, word by, byte block);
void drawteleline(int round, int x1, int y1, int x2, int y2, int j, int k);
void drawteleport(void);
void swap(int *pa, int *pb);
void drawlinev  (int x1, int y1, int x2, int y2, byte color, byte *storage);
void undrawlinev(int x1, int y1, int x2, int y2,             byte *storage);
void drawlineh  (int x1, int y1, int x2, int y2, byte color, byte *storage);
void undrawlineh(int x1, int y1, int x2, int y2,             byte *storage);
void drawline   (int x1, int y1, int x2, int y2, byte color, byte *storage);
void undrawline (int x1, int y1, int x2, int y2,             byte *storage);
void drawbullets(void);
void undrawbullets(void);
void drawfragments(void);
void undrawfragments(void);
void drawspacestationblip(void);
void drawload(int flag);
word drawshuttle(void);
void undrawshuttle(void);
void drawfuellines(void);
void undrawfuellines(void);
#endif

#endif /* GRAPHICS_H */
