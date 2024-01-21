/***********************************************************
*  Rocks'n'Diamonds -- McDuffin Strikes Back!              *
*----------------------------------------------------------*
*  ©1995 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.owl.de                      *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  misc.h                                                  *
*                                                          *
*  Letzte Aenderung: 15.06.1995                            *
***********************************************************/

#ifndef MISC_H
#define MISC_H

#include "main.h"

/* values for cartoon figures */
#define DWARF_XSIZE	40
#define DWARF_YSIZE	48
#define DWARF_X		6
#define DWARF_Y		290
#define DWARF_FRAMES	8
#define DWARF_FPS	10
#define DWARF_STEPSIZE	4
#define JUMPER_XSIZE	48
#define JUMPER_YSIZE	56
#define JUMPER_X	6
#define JUMPER_Y	338
#define JUMPER_FRAMES	8
#define JUMPER_FPS	10
#define JUMPER_STEPSIZE	4
#define CLOWN_XSIZE	80
#define CLOWN_YSIZE	110
#define CLOWN_X		442
#define CLOWN_Y		284
#define CLOWN_FRAMES	1
#define CLOWN_FPS	10
#define CLOWN_STEPSIZE	4
#define BIRD_XSIZE	32
#define BIRD_YSIZE	30
#define BIRD1_X		8
#define BIRD1_Y		232
#define BIRD2_X		(BIRD1_X+8*BIRD_XSIZE)
#define BIRD2_Y		(BIRD1_Y)
#define BIRD_FRAMES	8
#define BIRD_FPS	20
#define BIRD_STEPSIZE	4

#define ANIMDIR_LEFT	1
#define ANIMDIR_RIGHT	2
#define ANIMDIR_UP	4
#define ANIMDIR_DOWN	8

#define ANIMPOS_ANY	0
#define ANIMPOS_LEFT	1
#define ANIMPOS_RIGHT	2
#define ANIMPOS_UP	4
#define ANIMPOS_DOWN	8
#define ANIMPOS_UPPER	16

#define ANIM_START	0
#define ANIM_CONTINUE	1
#define ANIM_STOP	2

struct AnimInfo
{
  int width, height;
  int src_x, src_y;
  int frames;
  int frames_per_second;
  int stepsize;
  BOOL pingpong;
  int direction;
  int position;
};

void microsleep(unsigned long);
unsigned long be2long(unsigned long *);
char *int2str(int, int);
int RND(int);
void INIT_RND(void);
char *GetLoginName(void);

void InitAnimation(void);
void StopAnimation(void);
void DoAnimation(void);
void HandleAnimation(int);
BOOL AnimateToon(int, BOOL);
void DrawAnim(int, int, int, int, int, int, int, int);
#endif
