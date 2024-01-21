/*                                */
/* xemeraldia   ------ graphics.h */
/*                                */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <unistd.h>

#define BLOCK_VARIETY  6
#define BLOCK_WIDTH  32
#define BLOCK_HEIGHT 32
#define CRUSH_ANIME_FRAMES  5

#define BOARD_WIDTH  7
#define BOARD_HEIGHT 13

#define WIN_WIDTH   BLOCK_WIDTH * (BOARD_WIDTH + 1)
#define WIN_HEIGHT  BLOCK_HEIGHT * (BOARD_HEIGHT + 1)
#define DIFF_X       BLOCK_WIDTH / 2
#define DIFF_Y       BLOCK_HEIGHT / 2

#define MAX_DELAY   920


extern GC      draw_gc, delete_gc;
extern Pixmap  board_pix, block[BLOCK_VARIETY * 2 + 1];
extern Pixmap  crush[CRUSH_ANIME_FRAMES];
extern Pixmap  star;
extern Widget  board_w, quit, start, nextItem_w, score_disp, level_disp;
extern Widget  score_frame, score_text, high_sc_w;
extern int     colored;
extern XtIntervalId  timer;

typedef struct {
  Boolean  usescorefile;
  String   scorefile;
  Pixel    block1pixel;
  Pixel    block2pixel;
  Pixel    block3pixel;
  Pixel    block4pixel;
  Pixel    block5pixel;
  Pixel    block6pixel;
  Pixel    starpixel;
} AppData, *AppDataPtr;

extern AppData app_data;

/* In graphics.c */
void  RedrawBoard (), RedrawNextItem (), deleteBlocks (), delete_1_block ();
void  printNextItem (), printBlock (), crack_1_block (), crushAnimate ();
void  printScore (), printItem (), printLevel ();
void  clearScreen (), clearNextItem (), startTimer (), stopTimer ();
void  showTmpScore (), Done ();

/* In move.c */
void  MoveLeft (), MoveRight (), Rotation (), CCRotation ();
void  MoveDown (), moveItem ();
int   CanItemMove (), CanStarMove ();

/* In games.c */
void  StartGame (), addScore (), makeNext ();

/* In crack.c */
void  DropItem ();

/* In score.c */
void  update_highscore_table (), read_high_scores (), write_high_scores ();
void  PrintHighScores ();

#ifndef HIGH_SCORE_TABLE
#define HIGH_SCORE_TABLE ".xemeraldia.scores"
#endif

#if defined(sgi) || defined(SYSV)
void  usleep ();
#endif
