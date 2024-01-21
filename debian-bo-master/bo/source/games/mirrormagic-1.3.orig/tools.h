/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  tools.h                                                 *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#ifndef TOOLS_H
#define TOOLS_H

#include "main.h"
#include "misc.h"

#include <sys/time.h>

void BackToFront();
void ClearWindow();
void DrawText(int, int, char *, int, int);
void DrawTextExt(Drawable, GC, int, int, char *, int, int);
void DrawGraphic(int, int, int);
void DrawGraphicExt(Drawable, GC, int, int, int);
void DrawElement(int, int, int);
void DrawWalls(int, int, int);
void DrawWalls2(int, int, int, int, int);
void DrawMicroElement(int, int, int);
void DrawMicroWalls(int, int, int);
void DrawMicroLevel(int, int);
BOOL AreYouSure(char *, int);
void OpenDoor(int);
void CloseDoor(int);
void MoveDoor(int, int);
long mainCounter(int);
long Counter(void);
long Counter2(void);
void InitCounter(void);
void WaitCounter(long);
void WaitCounter2(long);
void Delay(long);
BOOL DelayReached(long *, int);
unsigned long ReadPixel(Drawable, int, int);
void SetRGB(unsigned long, unsigned short, unsigned short, unsigned short);
int el2gfx(int);

#endif
