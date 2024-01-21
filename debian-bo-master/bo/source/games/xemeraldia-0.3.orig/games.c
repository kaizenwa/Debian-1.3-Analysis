/*                             */
/* xemeraldia   -----  games.c */
/*                             */

#include "graphics.h"
#include "games.h"
#include <stdlib.h>

#if defined(SYSV) || defined(SVR4)
#define srandom srand48
#define random lrand48
#endif

static void  decideMaxRnd (), decideNextItem (), pauseProc (), endGame ();
static void  initGame (), restart ();

struct Board     board[BOARD_WIDTH + 2][BOARD_HEIGHT + 1];
struct DropItem  drop_i, next_i;

static int  max_rnd, star_gauge;

int  gameover_flag = TRUE;
long sc, blocks, tmp_blocks, chain_step;
int  movedown_counter;
int  paused = False;
int  star_comes;

int  iRot_vx[4][3] = {{1,  0,  0}, {0,  0,  1}, {0,  1,  1}, { 1,  1,  0}};
int  iRot_vy[4][3] = {{0,  0, -1}, {0, -1, -1}, {-1, -1, 0}, {-1,  0,  0}};
int  iCrChk[4][2] = {{0, 1}, {0, 2}, {0, 2}, {1, 2}};


static void  initGame ()
{
  int  x, y;

  sc = 0;
  blocks = 0;
  star_gauge = 0;
  printScore ();
  printLevel ();
  for (y = 0; y <= BOARD_HEIGHT; y++)
    {
      board[0][y].blk = OBSTACLE;
      board[BOARD_WIDTH + 1][y].blk = OBSTACLE;
      board[0][y].chk = OBSTACLE;
      board[BOARD_WIDTH + 1][y].chk = OBSTACLE;
      for (x = 1; x <= BOARD_WIDTH; x++)
	{
	  board[x][y].blk = EMPTY;
	  board[x][y].sub = EMPTY;
	  board[x][y].chk = EMPTY;
	}
    }
  for (x = 0; x <= BOARD_WIDTH + 1; x++)
    {
      board[x][BOARD_HEIGHT].blk = OBSTACLE;
      board[x][BOARD_HEIGHT].chk = OBSTACLE;
    }

  decideNextItem ();
}


static void  decideNextItem ()
{
  int  reduce_blocks;

  decideMaxRnd ();
  if ((blocks > 200) && ((random () % max_rnd + 1) == LUCKY_NUMBER))
    {
      next_i.col[0] = STAR;
    }
  else 
    {
      if (blocks < 200)  reduce_blocks = 2;
      else if (blocks < 600)  reduce_blocks = 1;
      else  reduce_blocks = 0;

      next_i.col[0] = random () % (BLOCK_VARIETY - reduce_blocks) + 1;
      do
	{
	  next_i.col[1] = random () % (BLOCK_VARIETY - reduce_blocks) + 1;
	} while  (next_i.col[1] == next_i.col[0]);
      do
	{
	  next_i.col[2] = random () % (BLOCK_VARIETY - reduce_blocks) + 1;
	} while  ((next_i.col[2] == next_i.col[0])
		  || (next_i.col[2] == next_i.col[1]));

      next_i.rot = 0;
    }
}


static void  restart ()
{
  XtVaSetValues (start, XtNlabel, "Pause", NULL);
  paused = False;
  RedrawBoard (board_w);
  RedrawNextItem (nextItem_w);
  XSync (XtDisplay (board_w), FALSE);
  startTimer ();
}


void  StartGame ()
{
  if (gameover_flag)
    {
      XtVaSetValues (start, XtNlabel, "Pause", NULL);
      gameover_flag = FALSE;
      srandom (time (0));
      initGame ();
      clearScreen (board_w);
      RedrawBoard (board_w);
      makeNext ();
      printItem ();
      startTimer ();
    }
  else
    {
      if (! paused)
	pauseProc ();
      else
	restart ();
    }
}


void  addScore (sc_x, sc_y)
     int  sc_x, sc_y;
{
  long tmp_sc;

  if (tmp_blocks > 0)
    {
      if (chain_step == 0)
	tmp_sc = 1000;
      else
	tmp_sc = tmp_blocks * 10
	  * chain_step * (chain_step + 3) * (tmp_blocks / 3 + 1);
      showTmpScore (tmp_sc, sc_x, sc_y, chain_step);
      usleep (200000);
      RedrawBoard (board_w);
      sc += tmp_sc;
      blocks += tmp_blocks;
    }
}


void  makeNext ()
{
  int  i;

  if (next_i.col[0] == STAR)
    {
      drop_i.col[0] = EMPTY;
      star_comes = TRUE;
      drop_i.x = 4;
      drop_i.y = 0;
    }
  else
    {
      star_comes = FALSE;
      for (i = 0; i < 3; i++)
	drop_i.col[i] = next_i.col[i];
      drop_i.rot = next_i.rot;
      drop_i.x = 4;
      drop_i.y = 1;
    }
  movedown_counter = 0;

  decideNextItem ();
  printNextItem ();

  if (! star_comes)
    {
      if (! ((board[drop_i.x + iRot_vx[drop_i.rot][0]]
	         [drop_i.y + iRot_vy[drop_i.rot][0]].blk == EMPTY) &&
	   (board[drop_i.x + iRot_vx[drop_i.rot][1]]
	         [drop_i.y + iRot_vy[drop_i.rot][1]].blk == EMPTY) &&
	   (board[drop_i.x + iRot_vx[drop_i.rot][2]]
                 [drop_i.y + iRot_vy[drop_i.rot][2]].blk == EMPTY)))
	endGame ();
    }
  else
    {
      if (! (board[drop_i.x][drop_i.y].blk == EMPTY))
	endGame ();
    }
}


static void  pauseProc ()
{
  XtVaSetValues (start, XtNlabel, "Start", NULL);
  paused = True;
  stopTimer ();
  RedrawBoard (board_w);
  clearNextItem ();
  XDrawImageString (XtDisplay (board_w), XtWindow (board_w), draw_gc, 90, 220,
		    "PAUSE!", 6);
}


static void  endGame ()
{
  XtVaSetValues (start, XtNlabel, "NewGame", NULL);
  gameover_flag = TRUE;
  if (timer)
    stopTimer ();
  XDrawImageString (XtDisplay (board_w), board_pix, draw_gc, 28, 220,
		    "<<< GAME OVER >>>", 17);
  RedrawBoard (board_w);

  update_highscore_table ();
  PrintHighScores ();
}


static void  decideMaxRnd ()
{
  int  x, y, danger_blocks = 0;

  for (y = 0; y <= 4; y++)
    for (x = 1; x <= BOARD_WIDTH; x++)
      if (board[x][y].blk != EMPTY)
	danger_blocks++;
  if (danger_blocks < 6)  max_rnd = 120 - (blocks / 200) / 2;
  else if (danger_blocks < 12)  max_rnd = 60 - (blocks / 200) / 4;
  else if (danger_blocks < 20)  max_rnd = 10 - (blocks / 200) / 16;
  else  max_rnd = 5;
  star_gauge += danger_blocks;
  if (star_gauge > 100)
    {
      max_rnd = 4;
      star_gauge = 0;
    }
}
