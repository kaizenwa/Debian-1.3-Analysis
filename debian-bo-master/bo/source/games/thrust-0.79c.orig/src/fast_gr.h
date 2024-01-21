
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef FAST_GR_H
#define FAST_GR_H

#include "thrust_types.h"

#define BBILDX	(41)
#define BBILDY	(23)
#define PBILDX	(BBILDX*8)
#define PBILDY	(BBILDY*8)
#define PUSEX	(PBILDX-8)
#define PUSEY	(PBILDY-8)

#ifdef __STDC__
void putscr(int x, int y);
void savegraphics(int x, int y);
void putblock(int x, int y, byte *source);
void drawfuel(int fuel);
void drawship(word bx, word by, byte *ship, byte *storage);
void undrawship(word bx, word by, byte *storage);
void drawsquare(word bx, word by,
		byte *object, byte *storage,
		byte deltax, byte deltay);
void undrawsquare(word bx, word by,
		  byte *storage,
		  byte deltax, byte deltay);
word testcrash(byte *object, byte *storage, word len, byte shield);
void setmargin(byte color, byte flag);
#endif

extern byte *bild;

#endif /* FAST_GR_H */
