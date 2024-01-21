/*                             */
/* xemeraldia   -----  crack.c */
/*                             */

#include "graphics.h"
#include "games.h"

static void  fallDownBlocks (), crushBlocks (), flushCheckedBlocks ();
static void  checkCrack (), clearCheckBoard (), setCollisionPoint ();
static void  checkShock (), checkSameColorBlocks (), crackBlocks ();
static void  starComes ();

struct  CollisionPoint {
  int  x, y;
};
struct CollisionPoint  coll_p[BOARD_WIDTH * BOARD_HEIGHT];

int  coll_num;
static int  sc_x, sc_y;


void  DropItem ()
{
  if (! gameover_flag)
    {
      if (((!star_comes) &&(! CanItemMove (0, MOVE_DOWN, drop_i.rot)))
	  || ((star_comes) && (! CanStarMove (0, MOVE_DOWN))))
	{
	  chain_step = 0;
	  coll_num = 0;
	  if (star_comes)
	    starComes ();
	  else
	    setCollisionPoint ();
	  do
	    {
	      chain_step++;
	      tmp_blocks = 0;
	      checkCrack ();
	      addScore (sc_x, sc_y);
	      coll_num = 0;
	      fallDownBlocks ();
	    } while (coll_num !=0);
	  if (chain_step > 0)
	    {
	      printScore ();
	      printLevel ();
	    }
	  makeNext ();
	  printItem ();
	}
      else
	moveItem (0, MOVE_DOWN);
      
      startTimer ();
    }
}


static void  setCollisionPoint ()
{
  int  i;

  for (i = 0; i < 2; i++)
    {
      if (board[drop_i.x + iRot_vx[drop_i.rot][iCrChk[drop_i.rot][i]]]
	  [drop_i.y + iRot_vy[drop_i.rot][iCrChk[drop_i.rot][i]] + 1].blk
	  != EMPTY)
	{
	  coll_num++;
	  coll_p[coll_num].x = drop_i.x
	    + iRot_vx[drop_i.rot][iCrChk[drop_i.rot][i]];
	  coll_p[coll_num].y = drop_i.y
	    + iRot_vy[drop_i.rot][iCrChk[drop_i.rot][i]];
	}
      else 
	fallDownBlocks ();
    }
}


static void  checkCrack ()
{
  int  i;

  for (i = 1; i <= coll_num; i++)
    {
      clearCheckBoard ();
      if (board[coll_p[i].x][coll_p[i].y].blk
	  == board[coll_p[i].x][coll_p[i].y + 1].blk)
	{
	  if (board[coll_p[i].x][coll_p[i].y].sub == EMPTY)
	    board[coll_p[i].x][coll_p[i].y].sub = NEW_CRACKED;
	  else
	    {
	      board[coll_p[i].x][coll_p[i].y].sub = DELETE;
	      sc_x = coll_p[i].x; sc_y = coll_p[i].y;
	    }
	  if (board[coll_p[i].x][coll_p[i].y + 1].sub == EMPTY)
	    board[coll_p[i].x][coll_p[i].y + 1].sub = NEW_CRACKED;
	  else
	    {
	      board[coll_p[i].x][coll_p[i].y + 1].sub = DELETE;
	      sc_x = coll_p[i].x; sc_y = coll_p[i].y;
	    }

	  board[coll_p[i].x][coll_p[i].y].chk = CHECKED;
	  board[coll_p[i].x][coll_p[i].y + 1].chk = CHECKED;
	  flushCheckedBlocks ();
	  checkShock (coll_p[i].x, coll_p[i].y,
		      board[coll_p[i].x][coll_p[i].y].blk);
	  checkShock (coll_p[i].x, coll_p[i].y + 1,
		      board[coll_p[i].x][coll_p[i].y].blk);
	  crushBlocks ();
	  crackBlocks (i);
	}
    }
}


static void  clearCheckBoard ()
{
  int  x, y;

  for (y = 0; y <= BOARD_HEIGHT - 1; y++)
    for (x = 1; x <= BOARD_WIDTH; x++)
      board[x][y].chk = EMPTY;
}  


static void  checkShock (x, y, color)
     int  x, y, color;
{
  int  ix, iy;
  
  board[x][y].chk = CHECKED;
  
  for (ix = -1; ix <= 1; ix++)
    for (iy = -1; iy <= 1; iy++)
      if ((ix != 0) || (iy != 0))
	{
	  if ((board[x + ix][y + iy].blk == color)
	      && (board[x + ix][y + iy].chk != CHECKED))
	    {
	      if (board[x + ix][y + iy].sub == EMPTY)
		board[x + ix][y + iy].sub = NEW_CRACKED;
	      else
		if ((board[x + ix][y + iy].sub == CRACKED)
		    ||(board[x + ix][y + iy].sub == NEW_CRACKED))
		  {
		    board[x + ix][y + iy].sub = DELETE;
		    sc_x = x + ix; sc_y = y + iy;
		  }
	      flushCheckedBlocks ();
	      checkShock (x + ix, y + iy, color);
	    }
	}
}


static void  crackBlocks (collision_times)
      int  collision_times;
{
  int  x, y, k, h;

  for (x = 1; x <= BOARD_WIDTH; x++)
    for (y = 0; y <= BOARD_HEIGHT; y++)
      {
	if (board[x][y].sub == NEW_CRACKED)
	  {
	    board[x][y].sub = CRACKED;
	    crack_1_block (x, y);
	  }
	else
	  if (board[x][y].sub == DELETE)
	    {
	      delete_1_block (x, y);
	      board[x][y].blk = EMPTY;
	      board[x][y].sub = EMPTY;
	      tmp_blocks++;
	      for (k = collision_times + 1; k <= coll_num; k++)
		{
		  if ((coll_p[k].x == x) && (coll_p[k].y == y))
		    {
		      for (h = k; h < coll_num; h++)
			{
			  coll_p[k].x = coll_p[k + 1].x;
			  coll_p[k].y = coll_p[k + 1].y;
			}
		      coll_num--;
		      k--;
		    }
		}
	    }
      }
}


static void  fallDownBlocks ()
{
  int  x, y, wy = BOARD_HEIGHT - 1, wy1, wy2;
  int  lines[BOARD_HEIGHT + 2], sub_lines[BOARD_HEIGHT + 2];

  for (x = 1; x <= BOARD_WIDTH; x++)
    {
      while (wy != -1) 
	{
	  if (board[x][wy].blk == EMPTY)
	    {
	      wy1 = wy;
	      while ((wy != -1) && (board[x][wy].blk == EMPTY)) wy--;
	      if (wy != -1)
		{
		  wy2 = wy - 1;
		  for (y = 0; y <= wy2 + 1; y++)
		    {
		      lines[y] = board[x][y].blk;
		      sub_lines[y] = board[x][y].sub;
		      delete_1_block (x, y);
		      board[x][y].blk = EMPTY;
		      board[x][y].sub = EMPTY;
		    }
		  for (y = wy1 - wy2 - 1; y <= wy1; y++)
		    {
		      board[x][y].blk = lines[y - wy1 + wy2 + 1];
		      board[x][y].sub = sub_lines[y - wy1 + wy2 + 1];
		      if (board[x][y].blk != EMPTY)
			printBlock (x, y, board[x][y].blk);
		    }
		  coll_num++;
		  coll_p[coll_num].x = x;
		  coll_p[coll_num].y = wy1;
		  wy = wy1;
		}
	    }
	  else wy--;
	}
      wy = BOARD_HEIGHT - 1;
    }
}


static void  flushCheckedBlocks ()
{
  int  x, y;
  
  for (x = 1; x <= BOARD_WIDTH; x++)
    for (y = 0; y <= BOARD_HEIGHT - 1; y++)
      {
	if ((board[x][y].sub == NEW_CRACKED)|| (board[x][y].sub == DELETE))
	  delete_1_block (x, y); 
      }
  XSync (XtDisplay (board_w), FALSE);
  usleep (28000);
  for (x = 1; x <= BOARD_WIDTH; x++)
    for (y = 0; y <= BOARD_HEIGHT - 1; y++)
      {
	if ((board[x][y].sub == NEW_CRACKED)|| (board[x][y].sub == DELETE))
	  printBlock (x, y, board[x][y].blk);
      }
  XSync (XtDisplay (board_w), FALSE);
  usleep (28000);
}

static void  crushBlocks ()
{
  int  x, y, i;
  
  for (i = 0; i < CRUSH_ANIME_FRAMES; i++)
    {
      for (x = 1; x <= BOARD_WIDTH; x++)
	for (y = 0; y <= BOARD_HEIGHT - 1; y++)
	  {
	    if (board[x][y].sub == DELETE)  crushAnimate (x, y, i);
	  }
      XSync (XtDisplay (board_w), FALSE);
      usleep (23000);
      for (x = 1; x <= BOARD_WIDTH; x++)
	for (y = 0; y <= BOARD_HEIGHT; y++)
	  {
	    if (board[x][y].sub == DELETE)  crushAnimate (x, y, i);
	  }
      XSync (XtDisplay (board_w), FALSE);
      usleep (23000);
    }
}


static void  starComes ()
{
  int  selected_color = board[drop_i.x][drop_i.y + 1].blk;

  board[drop_i.x][drop_i.y].sub = DELETE;
  if ((selected_color >= 1) && (selected_color <= BLOCK_VARIETY))
    checkSameColorBlocks (selected_color);

  crushBlocks ();
  crackBlocks (0);
  XSync (XtDisplay (board_w), FALSE);
  usleep (200000);

  coll_num = 0;
  fallDownBlocks ();
  addScore (drop_i.x, drop_i.y + 1);
}


static void  checkSameColorBlocks (selected_color)
      int  selected_color;
{
  int  x, y;

  for (y = 0; y <= BOARD_HEIGHT - 1; y++)
    for (x = 1; x <= BOARD_WIDTH ; x++)
      {
	if (board[x][y].blk == selected_color)
	  {
	    board[x][y].sub = DELETE;
	    board[x][y].chk = CHECKED;
	    flushCheckedBlocks ();
	  }
      }
}  
