/*                              */
/* xemeraldia     ----- games.h */
/*                              */

#include <time.h>
#include <stdio.h>

#define OBSTACLE  32
#define EMPTY     0
#define STAR      44

#define LUCKY_NUMBER  1

#define MOVE_LEFT    -1
#define MOVE_RIGHT    1
#define MOVE_DOWN     1

#define CRACKED       1
#define DELETE        2
#define NEW_CRACKED   5

#define CHECKED       10

#define CLOCKWIZE           1
#define COUNTER_CLOCKWIZE  -1


extern int   gameover_flag;
extern long  sc, blocks, tmp_blocks, chain_step;
extern int   paused;
extern int   star_comes;
extern int   movedown_counter;
extern char *name, *programname;

extern int  iRot_vx[4][3];
extern int  iRot_vy[4][3];
extern int  iCrChk[4][2];

struct Board
{
  int  blk;   /* blocks */
  int  sub;   /* sub    */
  int  chk;   /* check  */
};
extern struct Board board[BOARD_WIDTH + 2][BOARD_HEIGHT + 1];

struct DropItem {
  int  col[3];
  int  rot;
  int  x, y;
};
extern struct DropItem  drop_i, next_i;
